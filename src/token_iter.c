#include "gifcc.h"

typedef struct TokenIterator {
  ts_next_fn_t *next;
  void *arg;
  TokenVector *tokens;
} TokenIterator;

TokenIterator *new_token_iterator(ts_next_fn_t *next, void *arg) {
  TokenIterator *ts = NEW(TokenIterator);
  *ts = (TokenIterator){
      .next = next,
      .arg = arg,
      .tokens = NEW_VECTOR(TokenVector),
  };
  return ts;
}

static bool next_vec(void *arg, TokenVector *output) {
  TokenVector *input = arg;
  if (VEC_LEN(input) == 0) {
    return false;
  }
  VEC_APPEND(output, input);
  VEC_CLEAR(input);
  return true;
}

TokenIterator *token_iterator_from_vec(TokenVector *tokens) {
  return new_token_iterator(next_vec, tokens);
}

void consume_all_token_iterator(TokenIterator *ts) {
  Token *token = NULL;
  do {
    token = ts_pop(ts);
  } while (token->ty != TK_EOF);
}

Token *ts_pop(TokenIterator *ts) {
  while (VEC_LEN(ts->tokens) == 0) {
    if (!ts->next(ts->arg, ts->tokens)) {
      return NULL;
    }
  }

  return VEC_REMOVE(ts->tokens, 0);
}

void ts_succ(TokenIterator *ts) { (void)ts_pop(ts); }

Token *ts_peek(TokenIterator *ts) { return ts_peek_ahead(ts, 0); }
Token *ts_peek_ahead(TokenIterator *ts, int n) {
  while (VEC_LEN(ts->tokens) <= n) {
    if (!ts->next(ts->arg, ts->tokens)) {
      return NULL;
    }
  }
  return VEC_GET(ts->tokens, n);
}

Token *ts_consume(TokenIterator *ts, int ty) {
  Token *tk = ts_peek(ts);
  if (tk == NULL || tk->ty != ty) {
    return NULL;
  }
  ts_succ(ts);
  return tk;
}

Token *ts_consume2(TokenIterator *ts, int ty1, int ty2) {
  Token *tk1 = ts_peek(ts);
  if (tk1->ty != ty1) {
    return NULL;
  }
  Token *tk2 = ts_peek_ahead(ts, 1);
  if (tk2->ty != ty2) {
    return NULL;
  }
  ts_succ(ts);
  ts_succ(ts);
  return tk2;
}

Token *ts_expect(TokenIterator *ts, int ty) {
  Token *tk = ts_pop(ts);
  if (tk->ty != ty) {
    range_error(tk->range, "%s expected, but found %s", token_kind_to_str(ty),
                token_kind_to_str(tk->ty));
  }
  return tk;
}
