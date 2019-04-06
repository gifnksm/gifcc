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
    vec_push(tokenizer->tokens, token);
  }
  return vec_get(tokenizer->tokens, n);
}
static Token *tknzr_peek_ahead_adapter(Adapter *tokenizer, int n) {
  while (vec_len(tokenizer->tokens) <= n) {
    int old_len = vec_len(tokenizer->tokens);
    tokenizer->filter(tokenizer->arg, tokenizer->inner, tokenizer->tokens);
    if (vec_len(tokenizer->tokens) == old_len) {
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
