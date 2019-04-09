#include "gifcc.h"
#include <limits.h>
#include <string.h>
#include <strings.h>

static void convert_number(Token *token);
static void convert_ident(Token *token);
static Number read_float(Token *token);
static Number read_integer(Token *token);

static char phase2_pop_fun(void *arg UNUSED, Reader *reader) {
  // Translation phase #2
  // * backslash character (\) immediately followed by a new-line character is
  //   deleted
  reader_consume_str(reader, "\\\n");
  return reader_pop(reader);
}

Reader *phase2_filter(Reader *reader) {
  return new_filtered_reader(reader, phase2_pop_fun, NULL);
}

static bool phase6_next(void *arg, Vector *output) {
  TokenStream *ts = arg;

  // Translation phase #6:
  // * concatenate adjacent string literal tokens
  Token *token = ts_pop(ts);
  if (token == NULL) {
    return false;
  }

  if (token->ty == TK_STR) {
    Token *next;
    while ((next = ts_consume(ts, TK_STR)) != NULL) {
      token->str = format("%s%s", token->str, next->str);
      token->range = range_join(token->range, next->range);
    }
  }

  vec_push(output, token);
  return true;
}

TokenStream *phase6_filter(TokenStream *ts) {
  return new_token_stream(phase6_next, ts);
}

static bool phase7_next(void *arg, Vector *output) {
  TokenStream *ts = arg;

  // Translation phase #7:
  // * convert pp tokens into tokens
  Token *token = ts_pop(ts);
  if (token == NULL) {
    return false;
  }
  if (token->ty == TK_PP_NUM) {
    convert_number(token);
  }
  if (token->ty == TK_PP_IDENT) {
    convert_ident(token);
  }

  vec_push(output, token);
  return true;
}

TokenStream *phase7_filter(TokenStream *ts) {
  return new_token_stream(phase7_next, ts);
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