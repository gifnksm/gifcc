#include "gifcc.h"
#include <limits.h>
#include <string.h>
#include <strings.h>

#define DISPATCH_BASE(tokenizer, func, ...)                                    \
  ((tokenizer)->type == TKNZR_BASE)                                            \
      ? func##_base(&(tokenizer)->base, ##__VA_ARGS__)                         \
      : func((tokenizer)->adapter.inner, ##__VA_ARGS__)

#define DISPATCH(tokenizer, func, ...)                                         \
  ((tokenizer)->type == TKNZR_BASE)                                            \
      ? func##_base(&(tokenizer)->base, ##__VA_ARGS__)                         \
      : func##_adapter(&(tokenizer)->adapter, ##__VA_ARGS__)

typedef struct Base {
  PpTokenizer *pp_tokenizer;
  Vector *tokens;
} Base;

typedef struct Adapter {
  Tokenizer *inner;
  tknzr_filter_fun_t *filter;
  void *arg;
  Vector *tokens;
} Adapter;

struct Tokenizer {
  enum { TKNZR_BASE, TKNZR_ADAPTER } type;
  union {
    Base base;
    Adapter adapter;
  };
};

typedef struct {
  tokenizer_listener_fun_t *fun;
  void *arg;
} Listener;

static void convert_number(Token *token);
static void convert_ident(Token *token);
static Number read_float(Token *token);
static Number read_integer(Token *token);

Tokenizer *new_tokenizer(PpTokenizer *pp_tokenizer) {
  Tokenizer *tokenizer = NEW(Tokenizer);
  tokenizer->type = TKNZR_BASE;
  tokenizer->base = (Base){
      .pp_tokenizer = pp_tokenizer,
      .tokens = new_vector(),
  };
  return tokenizer;
}

Tokenizer *new_filtered_tokenizer(Tokenizer *base, tknzr_filter_fun_t *filter,
                                  void *arg) {
  Tokenizer *tokenizer = NEW(Tokenizer);
  tokenizer->type = TKNZR_ADAPTER;
  tokenizer->adapter = (Adapter){
      .inner = base,
      .filter = filter,
      .arg = arg,
      .tokens = new_vector(),
  };
  return tokenizer;
}

void consume_all_tokens(Tokenizer *tokenizer) {
  Token *token = NULL;
  do {
    token = tknzr_pop(tokenizer);
  } while (token->ty != TK_EOF);
}

static void tknzr_succ_base(Base *tokenizer) {
  if (vec_len(tokenizer->tokens) > 0) {
    vec_remove(tokenizer->tokens, 0);
  } else {
    pp_tknzr_succ(tokenizer->pp_tokenizer);
  }
}
static void tknzr_succ_adapter(Adapter *tokenizer) {
  if (vec_len(tokenizer->tokens) > 0) {
    vec_remove(tokenizer->tokens, 0);
  } else {
    tokenizer->filter(tokenizer->arg, tokenizer->inner, tokenizer->tokens);
    if (vec_len(tokenizer->tokens) > 0) {
      vec_remove(tokenizer->tokens, 0);
    }
  }
}
void tknzr_succ(Tokenizer *tokenizer) { DISPATCH(tokenizer, tknzr_succ); }

Token *tknzr_peek(Tokenizer *tokenizer) {
  return tknzr_peek_ahead(tokenizer, 0);
}

static Token *tknzr_peek_ahead_base(Base *tokenizer, int n) {
  while (vec_len(tokenizer->tokens) <= n) {
    Token *token = pp_tknzr_pop(tokenizer->pp_tokenizer);
    if (token == NULL) {
      return NULL;
    }

    // Translation phase #6:
    // * concatenate adjacent string literal tokens
    if (token->ty == TK_STR) {
      Token *next;
      while ((next = pp_tknzr_consume(tokenizer->pp_tokenizer, TK_STR)) !=
             NULL) {
        token->str = format("%s%s", token->str, next->str);
        token->range = range_join(token->range, next->range);
      }
    }

    // Translation phase #7:
    // * convert pp tokens into tokens
    if (token->ty == TK_PP_NUM) {
      convert_number(token);
    }
    if (token->ty == TK_PP_IDENT) {
      convert_ident(token);
    }

    vec_push(tokenizer->tokens, token);
  }
  return vec_get(tokenizer->tokens, n);
}
static Token *tknzr_peek_ahead_adapter(Adapter *tokenizer, int n) {
  while (vec_len(tokenizer->tokens) <= n) {
    int len = vec_len(tokenizer->tokens);
    tokenizer->filter(tokenizer->arg, tokenizer->inner, tokenizer->tokens);
    if (vec_len(tokenizer->tokens) == len) {
      return NULL;
    }
  }
  return vec_get(tokenizer->tokens, n);
}
Token *tknzr_peek_ahead(Tokenizer *tokenizer, int n) {
  return DISPATCH(tokenizer, tknzr_peek_ahead, n);
}
Token *tknzr_pop(Tokenizer *tokenizer) {
  Token *token = tknzr_peek(tokenizer);
  tknzr_succ(tokenizer);
  return token;
}

Token *tknzr_consume(Tokenizer *tokenizer, int ty) {
  if (tknzr_peek(tokenizer)->ty != ty) {
    return NULL;
  }
  return tknzr_pop(tokenizer);
}

Token *tknzr_consume2(Tokenizer *tokenizer, int ty1, int ty2) {
  if (tknzr_peek(tokenizer)->ty == ty1 &&
      tknzr_peek_ahead(tokenizer, 1)->ty == ty2) {
    (void)tknzr_pop(tokenizer);
    return tknzr_pop(tokenizer);
  }
  return false;
}

Token *tknzr_expect(Tokenizer *tokenizer, int ty) {
  Token *token = tknzr_pop(tokenizer);
  if (token->ty != ty) {
    range_error(token->range, "%s expected, but found %s",
                token_kind_to_str(ty), token_kind_to_str(token->ty));
  }
  return token;
}

const Reader *tknzr_get_reader_base(const Base *tokenizer) {
  return pp_tknzr_get_reader(tokenizer->pp_tokenizer);
}
const Reader *tknzr_get_reader(const Tokenizer *tokenizer) {
  return DISPATCH_BASE(tokenizer, tknzr_get_reader);
}

static void convert_number(Token *token) {
  assert(token->ty == TK_PP_NUM);
  bool is_float =
      strpbrk(token->pp_num, ".pP") ||
      (strncasecmp(token->pp_num, "0x", 2) && strpbrk(token->pp_num, "eE"));

  Number num;
  if (is_float) {
    num = read_float(token);
  } else {
    num = read_integer(token);
  }

  token->ty = TK_NUM;
  token->num = num;
}

static void convert_ident(Token *token) {
  assert(token->ty == TK_PP_IDENT);
  const char *ident = token->pp_ident;
  token->pp_ident = NULL;

  for (int i = 0; LONG_IDENT_TOKENS[i].str != NULL; i++) {
    const LongToken *tk = &LONG_IDENT_TOKENS[i];
    if (strcmp(ident, tk->str) == 0) {
      token->ty = tk->kind;
      return;
    }
  }

  token->ty = TK_IDENT;
  token->ident = ident;
}

static Number read_float(Token *token) {
  assert(token->ty == TK_PP_NUM);
  char *suffix = NULL;
  long double val = strtold(token->pp_num, &suffix);
  type_t type = TY_VOID;
  if (strcmp(suffix, "") == 0) {
    type = TY_DOUBLE;
  } else if (strcasecmp(suffix, "l") == 0) {
    type = TY_LDOUBLE;
  } else if (strcasecmp(suffix, "f") == 0) {
    type = TY_FLOAT;
  } else {
    range_error(token->range, "不正なサフィックスです");
  }
  return new_number_float(type, val);
}

static Number read_integer(Token *token) {
  typedef struct Suffix {
    const char *suffix;
    type_t type;
  } Suffix;
  Suffix SUFFIX[] = {
      // unsigned long long int
      {"ull", TY_U_LLONG},
      {"llu", TY_U_LLONG},
      {"uLL", TY_U_LLONG},
      {"LLu", TY_U_LLONG},
      {"Ull", TY_U_LLONG},
      {"llU", TY_U_LLONG},
      {"ULL", TY_U_LLONG},
      {"LLU", TY_U_LLONG},
      // unsigned long int
      {"ul", TY_U_LONG},
      {"lu", TY_U_LONG},
      {"uL", TY_U_LONG},
      {"Lu", TY_U_LONG},
      {"Ul", TY_U_LONG},
      {"lU", TY_U_LONG},
      {"UL", TY_U_LONG},
      {"LU", TY_U_LONG},
      // unsigned int
      {"u", TY_U_INT},
      {"U", TY_U_INT},
      // signed long long int
      {"ll", TY_S_LLONG},
      {"LL", TY_S_LLONG},
      // signed long int
      {"l", TY_S_LONG},
      {"L", TY_S_LONG},
      // stub
      {NULL, TY_VOID},
  };

  assert(token->ty == TK_PP_NUM);
  char *suffix = NULL;
  unsigned long long val;
  if (strncasecmp("0b", token->pp_num, 2) == 0) {
    // NonStandard/GNU: binary prefix integer literal
    val = strtoull(&token->pp_num[2], &suffix, 2);
  } else {
    val = strtoull(token->pp_num, &suffix, 0);
  }

  bool isbase10 = token->pp_num[0] != '0';

  type_t ty = TY_VOID;
  if (strcmp(suffix, "") == 0) {
    // no suffix
    if (val <= INT_MAX) {
      ty = TY_S_INT;
    } else if (!isbase10 && val <= UINT_MAX) {
      ty = TY_U_INT;
    } else if (val <= LONG_MAX) {
      ty = TY_S_LONG;
    } else if (!isbase10 && val <= ULONG_MAX) {
      ty = TY_U_LONG;
    } else if (val <= LLONG_MAX) {
      ty = TY_S_LLONG;
    } else {
      assert(val <= ULLONG_MAX);
      ty = TY_U_LLONG;
    }
  } else {
    for (int i = 0; SUFFIX[i].suffix != NULL; i++) {
      if (strcmp(SUFFIX[i].suffix, suffix) == 0) {
        ty = SUFFIX[i].type;
        break;
      }
    }
    if (ty == TY_VOID) {
      range_error(token->range, "不正な整数のサフィックスです");
    }
  }

  return new_number(ty, val);
}
