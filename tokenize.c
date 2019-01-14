#include "gifcc.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

// トークナイズした結果のトークン列はこのベクタに保存する
static Vector *tokens = NULL;

static Token *new_token(int ty, char *input);
static Token *new_token_num(char *input, int val);
static Token *new_token_ident(char *input, char *name);
static Token *punctuator(char **input);
static Token *identifier_or_keyword(char **input);
static Token *constant(char **input);

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

    Token *token = NULL;
    if ((token = punctuator(&p)) != NULL) {
      goto SKIP;
    }
    if ((token = identifier_or_keyword(&p)) != NULL) {
      goto SKIP;
    }
    if ((token = constant(&p)) != NULL) {
      goto SKIP;
    }
  SKIP:
    vec_push(tokens, token);
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
  if (strcmp(name, "if") == 0) {
    token->ty = TK_IF;
  } else if (strcmp(name, "else") == 0) {
    token->ty = TK_ELSE;
  } else if (strcmp(name, "switch") == 0) {
    token->ty = TK_SWITCH;
  } else if (strcmp(name, "case") == 0) {
    token->ty = TK_CASE;
  } else if (strcmp(name, "default") == 0) {
    token->ty = TK_DEFAULT;
  } else if (strcmp(name, "while") == 0) {
    token->ty = TK_WHILE;
  } else if (strcmp(name, "do") == 0) {
    token->ty = TK_DO;
  } else if (strcmp(name, "for") == 0) {
    token->ty = TK_FOR;
  } else if (strcmp(name, "goto") == 0) {
    token->ty = TK_GOTO;
  } else if (strcmp(name, "break") == 0) {
    token->ty = TK_BREAK;
  } else if (strcmp(name, "continue") == 0) {
    token->ty = TK_CONTINUE;
  } else {
    token->ty = TK_IDENT;
  }
  token->input = input;
  token->name = name;
  return token;
}

static Token *punctuator(char **input) {
  Token *token = NULL;
  char *p = *input;
  switch (*p) {
  case '=': {
    if (*(p + 1) == '=') {
      token = new_token(TK_EQEQ, p);
      p += 2;
      break;
    }
    token = new_token(*p, p);
    p++;
    break;
  }
  case '!': {
    if (*(p + 1) == '=') {
      token = new_token(TK_NOTEQ, p);
      p += 2;
      break;
    }
    token = new_token(*p, p);
    p++;
    break;
  }
  case '<': {
    if (*(p + 1) == '<') {
      if (*(p + 2) == '=') {
        token = new_token(TK_LSHIFT_ASSIGN, p);
        p += 3;
        break;
      }
      token = new_token(TK_LSHIFT, p);
      p += 2;
      break;
    }
    if (*(p + 1) == '=') {
      token = new_token(TK_LTEQ, p);
      p += 2;
      break;
    }
    token = new_token(*p, p);
    p++;
    break;
  }
  case '>': {
    if (*(p + 1) == '>') {
      if (*(p + 2) == '=') {
        token = new_token(TK_RSHIFT_ASSIGN, p);
        p += 3;
        break;
      }
      token = new_token(TK_RSHIFT, p);
      p += 2;
      break;
    }
    if (*(p + 1) == '=') {
      token = new_token(TK_GTEQ, p);
      p += 2;
      break;
    }
    token = new_token(*p, p);
    p++;
    break;
  }
  case '&': {
    if (*(p + 1) == '&') {
      token = new_token(TK_LOGAND, p);
      p += 2;
      break;
    }
    if (*(p + 1) == '=') {
      token = new_token(TK_AND_ASSIGN, p);
      p += 2;
      break;
    }
    token = new_token(*p, p);
    p++;
    break;
  }
  case '|': {
    if (*(p + 1) == '|') {
      token = new_token(TK_LOGOR, p);
      p += 2;
      break;
    }
    if (*(p + 1) == '=') {
      token = new_token(TK_OR_ASSIGN, p);
      p += 2;
      break;
    }
    token = new_token(*p, p);
    p++;
    break;
  }
  case '^': {
    if (*(p + 1) == '=') {
      token = new_token(TK_XOR_ASSIGN, p);
      p += 2;
      break;
    }
    token = new_token(*p, p);
    p++;
    break;
  }
  case '+': {
    if (*(p + 1) == '+') {
      token = new_token(TK_INC, p);
      p += 2;
      break;
    }
    if (*(p + 1) == '=') {
      token = new_token(TK_ADD_ASSIGN, p);
      p += 2;
      break;
    }
    token = new_token(*p, p);
    p++;
    break;
  }
  case '-': {
    if (*(p + 1) == '-') {
      token = new_token(TK_DEC, p);
      p += 2;
      break;
    }
    if (*(p + 1) == '=') {
      token = new_token(TK_SUB_ASSIGN, p);
      p += 2;
      break;
    }
    token = new_token(*p, p);
    p++;
    break;
  }
  case '*': {
    if (*(p + 1) == '=') {
      token = new_token(TK_MUL_ASSIGN, p);
      p += 2;
      break;
    }
    token = new_token(*p, p);
    p++;
    break;
  }
  case '/': {
    if (*(p + 1) == '=') {
      token = new_token(TK_DIV_ASSIGN, p);
      p += 2;
      break;
    }
    token = new_token(*p, p);
    p++;
    break;
  }
  case '%': {
    if (*(p + 1) == '=') {
      token = new_token(TK_MOD_ASSIGN, p);
      p += 2;
      break;
    }
    token = new_token(*p, p);
    p++;
    break;
  }
  case '(':
  case ')':
  case ';':
  case '?':
  case ':':
  case '~':
  case '{':
  case '}':
  case ',': {
    token = new_token(*p, p);
    p++;
    break;
  }
  }

  *input = p;
  return token;
}

static Token *identifier_or_keyword(char **input) {
  Token *token = NULL;
  char *p = *input;
  if (('a' <= *p && *p <= 'z') || ('A' <= *p && *p <= 'Z') || *p == '_') {
    char *q = p;
    while (('a' <= *q && *q <= 'z') || ('A' <= *q && *q <= 'Z') || *q == '_' ||
           ('0' <= *q && *q <= '9')) {
      q++;
    }
    token = new_token_ident(p, strndup(p, q - p));
    p = q;
  }

  *input = p;
  return token;
}

static Token *constant(char **input) {
  Token *token = NULL;
  char *p = *input;

  if (isdigit(*p)) {
    token = new_token_num(p, strtol(p, &p, 10));
  }

  *input = p;
  return token;
}
