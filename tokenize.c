#include "gifcc.h"
#include <assert.h>
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
static Token *integer_constant(char **input);
static Token *hexadecimal_constant(char **input);
static Token *octal_constant(char **input);
static Token *decimal_constant(char **input);
static Token *character_constant(char **input);
static int c_char(char **input);

static inline int hex(int c) {
  assert(isxdigit(c));
  if ('0' <= c && c <= '9') {
    return c - '0';
  }
  if ('a' <= c && c <= 'f') {
    return (c - 'a') + 0xa;
  }
  assert('A' <= c && c <= 'F');
  return (c - 'A') + 0xa;
}

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

    error("トークナイズできません: %s", p);

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

  if ((token = integer_constant(&p)) != NULL) {
    goto SKIP;
  }
  if ((token = character_constant(&p)) != NULL) {
    goto SKIP;
  }

SKIP:
  *input = p;
  return token;
}

static Token *integer_constant(char **input) {
  Token *token = NULL;
  char *p = *input;
  if ((token = hexadecimal_constant(&p)) != NULL) {
    goto SKIP;
  }
  if ((token = octal_constant(&p)) != NULL) {
    goto SKIP;
  }
  if ((token = decimal_constant(&p)) != NULL) {
    goto SKIP;
  }

SKIP:
  *input = p;
  return token;
}

static Token *hexadecimal_constant(char **input) {
  Token *token = NULL;
  char *p = *input;
  if ((*p != '0') || ((*(p + 1) != 'x') && (*(p + 1)) != 'X')) {
    return NULL;
  }
  p += 2;

  int val = 0;

  char *q = p;
  for (; isxdigit(*q); q++) {
    val = val * 0x10 + hex(*q);
  }

  token = new_token_num(p, val);
  p = q;

  *input = p;
  return token;
}

static Token *octal_constant(char **input) {
  Token *token = NULL;
  char *p = *input;
  if (*p != '0') {
    return NULL;
  }
  p += 1;

  int val = 0;

  char *q = p;
  for (; '0' <= *q && *q <= '7'; q++) {
    val = val * 010 + (*q - '0');
  }

  token = new_token_num(p, val);
  p = q;

  *input = p;
  return token;
}

static Token *decimal_constant(char **input) {
  Token *token = NULL;
  char *p = *input;
  if (!isdigit(*p)) {
    return NULL;
  }
  int val = 0;

  char *q = p;
  for (; isdigit(*q); q++) {
    val = val * 10 + (*q - '0');
  }

  token = new_token_num(p, val);
  p = q;

  *input = p;
  return token;
}

static Token *character_constant(char **input) {
  Token *token = NULL;
  char *p = *input;
  if (*p != '\'') {
    return NULL;
  }
  p++;

  token = new_token_num(p, c_char(&p));

  if (*p != '\'') {
    error("'\\'' がありません: %s", *input);
  }
  p++;

  *input = p;
  return token;
}

static int c_char(char **input) {
  int val = 0;
  char *p = *input;
  if (*p == '\'') {
    error("空の文字リテラルです: %s", p);
  }
  if (*p == '\n' || *p == '\r') {
    error("改行文字を文字リテラル中に含めることはできません: %s", p);
  }

  if (*p == '\\') {
    switch (*(p + 1)) {
    case '\'':
    case '\"':
    case '\?':
    case '\\': {
      val = (*(p + 1));
      p += 2;
      goto SKIP;
    }
    case 'a': {
      val = '\a';
      p += 2;
      goto SKIP;
    }
    case 'b': {
      val = '\b';
      p += 2;
      goto SKIP;
    }
    case 'f': {
      val = '\f';
      p += 2;
      goto SKIP;
    }
    case 'n': {
      val = '\n';
      p += 2;
      goto SKIP;
    }
    case 'r': {
      val = '\r';
      p += 2;
      goto SKIP;
    }
    case 't': {
      val = '\t';
      p += 2;
      goto SKIP;
    }
    case 'v': {
      val = '\v';
      p += 2;
      goto SKIP;
    }

    case 'x': {
      char *q = p + 2;
      for (; isxdigit(*q); q++) {
        val = val * 0x10 + hex(*q);
      }
      if (q == p + 2) {
        error("空の16進文字リテラルです: %s", p);
      }
      p = q;
      goto SKIP;
    }
    }

    char *q = p + 1;
    for (int i = 0; (i < 3) && ('0' <= *q && *q <= '7'); i++, q++) {
      val = 010 * val + (*q - '0');
    }
    if (q - p >= 2) {
      p = q;
      goto SKIP;
    }

    error("不明なエスケープシーケンスです: %s", p);
  }

  val = *p;
  p++;
  goto SKIP;

SKIP:
  *input = p;
  return val;
}
