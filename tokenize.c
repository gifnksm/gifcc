#include "gifcc.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

// トークナイズした結果のトークン列はこのベクタに保存する
static Vector *tokens = NULL;

static Token *new_token(int ty, char *input);
static Token *new_token_num(char *input, int val);
static Token *new_token_ident(char *input, char *name);

// pos番目のtokenを取得する
Token *get_token(int pos) { return tokens->data[pos]; }

// pが指している文字列をトークンに分割してtokensに保存する
void tokenize(char *p) {
  tokens = new_vector();
  while (*p) {
    // 空白文字をスキップ
    if (isspace(*p)) {
      p++;
      continue;
    }

    switch (*p) {
    case '=': {
      if (*(p + 1) == '=') {
        vec_push(tokens, new_token(TK_EQEQ, p));
        p += 2;
        continue;
      }
      vec_push(tokens, new_token(*p, p));
      p++;
      continue;
    }
    case '!': {
      if (*(p + 1) == '=') {
        vec_push(tokens, new_token(TK_NOTEQ, p));
        p += 2;
        continue;
      }
      goto ERROR;
    }
    case '<': {
      if (*(p + 1) == '<') {
        vec_push(tokens, new_token(TK_LSHIFT, p));
        p += 2;
        continue;
      }
      if (*(p + 1) == '=') {
        vec_push(tokens, new_token(TK_LTEQ, p));
        p += 2;
        continue;
      }
      vec_push(tokens, new_token(*p, p));
      p++;
      continue;
    }
    case '>': {
      if (*(p + 1) == '>') {
        vec_push(tokens, new_token(TK_RSHIFT, p));
        p += 2;
        continue;
      }
      if (*(p + 1) == '=') {
        vec_push(tokens, new_token(TK_GTEQ, p));
        p += 2;
        continue;
      }
      vec_push(tokens, new_token(*p, p));
      p++;
      continue;
    }
    case '&': {
      if (*(p + 1) == '&') {
        vec_push(tokens, new_token(TK_LOGAND, p));
        p += 2;
        continue;
      }
      vec_push(tokens, new_token(*p, p));
      p++;
      continue;
    }
    case '|': {
      if (*(p + 1) == '|') {
        vec_push(tokens, new_token(TK_LOGOR, p));
        p += 2;
        continue;
      }
      vec_push(tokens, new_token(*p, p));
      p++;
      continue;
    }
    case '+':
    case '-':
    case '*':
    case '/':
    case '(':
    case ')':
    case ';':
    case '^': {
      vec_push(tokens, new_token(*p, p));
      p++;
      continue;
    }
    }

    if (('a' <= *p && *p <= 'z') || ('A' <= *p && *p <= 'Z') || *p == '_') {
      char *q = p;
      while (('a' <= *q && *q <= 'z') || ('A' <= *q && *q <= 'Z') ||
             *q == '_' || ('0' <= *q && *q <= '9')) {
        q++;
      }
      vec_push(tokens, new_token_ident(p, strndup(p, q - p)));
      p = q;
      continue;
    }

    if (isdigit(*p)) {
      vec_push(tokens, new_token_num(p, strtol(p, &p, 10)));
      continue;
    }

  ERROR:
    error("トークナイズできません: %s\n", p);
  }

  vec_push(tokens, new_token(TK_EOF, p));
}

static Token *new_token(int ty, char *input) {
  Token *token = malloc(sizeof(Token));
  token->ty = ty;
  token->input = input;
  return token;
}

static Token *new_token_num(char *input, int val) {
  Token *token = malloc(sizeof(Token));
  token->ty = TK_NUM;
  token->input = input;
  token->val = val;
  return token;
}

static Token *new_token_ident(char *input, char *name) {
  Token *token = malloc(sizeof(Token));
  token->ty = TK_IDENT;
  token->input = input;
  token->name = name;
  return token;
}
