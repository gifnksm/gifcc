#include "gifcc.h"

struct Tokenizer {
  PpTokenizer *pp_tokenizer;
  Vector *tokens;
  Vector *listeners;
};

typedef struct {
  tokenizer_listener_fun_t *fun;
  void *arg;
} Listener;

Tokenizer *new_tokenizer(PpTokenizer *pp_tokenizer) {
  Tokenizer *tokenizer = NEW(Tokenizer);
  tokenizer->pp_tokenizer = pp_tokenizer;
  tokenizer->tokens = new_vector();
  return tokenizer;
}

void consume_all_tokens(Tokenizer *tokenizer) {
  Token *token = NULL;
  do {
    token = tknzr_pop(tokenizer);
  } while (token->ty != TK_EOF);
}

void tknzr_add_listener(Tokenizer *tokenizer, tokenizer_listener_fun_t *fun,
                        void *arg) {
  if (tokenizer->listeners == NULL) {
    tokenizer->listeners = new_vector();
  }
  Listener *listener = NEW(Listener);
  listener->fun = fun;
  listener->arg = arg;
  vec_push(tokenizer->listeners, listener);
}

void tknzr_succ(Tokenizer *tokenizer) {
  assert(vec_len(tokenizer->tokens) > 0);
  vec_remove(tokenizer->tokens, 0);
}

Token *tknzr_peek(Tokenizer *tokenizer) {
  return tknzr_peek_ahead(tokenizer, 0);
}

Token *tknzr_peek_ahead(Tokenizer *tokenizer, int n) {
  while (vec_len(tokenizer->tokens) <= n) {
    Token *token = pp_tknzr_pop(tokenizer->pp_tokenizer);
    if (token == NULL) {
      return NULL;
    }

    if (tokenizer->listeners != NULL) {
      for (int i = 0; i < vec_len(tokenizer->listeners); i++) {
        Listener *listener = vec_get(tokenizer->listeners, i);
        listener->fun(listener->arg, token);
      }
    }
    vec_push(tokenizer->tokens, token);
  }
  return vec_get(tokenizer->tokens, n);
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

const Reader *tknzr_get_reader(const Tokenizer *tokenizer) {
  return pp_tknzr_get_reader(tokenizer->pp_tokenizer);
}
