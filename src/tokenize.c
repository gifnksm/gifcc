#include "gifcc.h"
#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

struct Tokenizer {
  const char *input;
  Token *current;
  Token *next;
  bool read_eof;
};

static Token *read_token(const char **p, bool *read_eof);
static Token *new_token(int ty, const char *input);
static Token *new_token_num(const char *input, int val);
static Token *new_token_ident(const char *input, char *name);
static Token *punctuator(const char **input);
static Token *identifier_or_keyword(const char **input);
static Token *constant(const char **input);
static Token *integer_constant(const char **input);
static Token *hexadecimal_constant(const char **input);
static Token *octal_constant(const char **input);
static Token *decimal_constant(const char **input);
static Token *character_constant(const char **input);
static Token *string_literal(const char **input);
static int c_char(const char **input);

Tokenizer *new_tokenizer(const char *input) {
  Tokenizer *tokenizer = malloc(sizeof(Tokenizer));
  tokenizer->input = input;
  tokenizer->read_eof = false;
  tokenizer->current = read_token(&tokenizer->input, &tokenizer->read_eof);
  tokenizer->next = read_token(&tokenizer->input, &tokenizer->read_eof);
  return tokenizer;
}

void token_succ(Tokenizer *tokenizer) {
  tokenizer->current = tokenizer->next;
  tokenizer->next = read_token(&tokenizer->input, &tokenizer->read_eof);
}

Token *token_peek(Tokenizer *tokenizer) { return tokenizer->current; }

Token *token_peek_ahead(Tokenizer *tokenizer, int n) {
  assert(n == 1);
  return tokenizer->next;
}

Token *token_pop(Tokenizer *tokenizer) {
  Token *token = tokenizer->current;
  token_succ(tokenizer);
  return token;
}

Token *token_consume(Tokenizer *tokenizer, int ty) {
  if (token_peek(tokenizer)->ty != ty) {
    return NULL;
  }
  return token_pop(tokenizer);
}

bool token_consume2(Tokenizer *tokenizer, int ty1, int ty2) {
  if (token_peek(tokenizer)->ty == ty1 &&
      token_peek_ahead(tokenizer, 1)->ty == ty2) {
    (void)token_pop(tokenizer);
    (void)token_pop(tokenizer);
    return true;
  }
  return false;
}

Token *token_expect(Tokenizer *tokenizer, int ty) {
  Token *token = token_pop(tokenizer);
  if (token->ty != ty) {
    if (ty <= 255) {
      error("'%c' がありません: %s", ty, token->input);
    }
    switch (ty) {
    case TK_IDENT:
      error("'識別子' がありません: %s", token->input);
    case TK_WHILE:
      error("'while' がありません: %s", token->input);
    case TK_INT:
      error("'int' がありません: %s", token->input);
    default:
      error("%d がありません: %s", ty, token->input);
    }
  }
  return token;
}

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

static Token *read_token(const char **p, bool *read_eof) {
  while (**p != '\0') {
    // 空白文字をスキップ
    if (isspace(**p)) {
      (*p)++;
      continue;
    }

    Token *token = NULL;
    if ((token = punctuator(p)) != NULL) {
      return token;
    }
    if ((token = identifier_or_keyword(p)) != NULL) {
      return token;
    }
    if ((token = constant(p)) != NULL) {
      return token;
    }
    if ((token = string_literal(p)) != NULL) {
      return token;
    }

    error("トークナイズできません: %s", *p);
  }

  if (!*read_eof) {
    *read_eof = true;
    return new_token(TK_EOF, *p);
  }
  return NULL;
}

static Token *new_token(int ty, const char *input) {
  Token *token = malloc(sizeof(Token));
  token->ty = ty;
  token->input = input;
  return token;
}

static Token *new_token_num(const char *input, int val) {
  Token *token = malloc(sizeof(Token));
  token->ty = TK_NUM;
  token->input = input;
  token->val = val;
  return token;
}

static Token *new_token_ident(const char *input, char *name) {
  Token *token = malloc(sizeof(Token));
  if (strcmp(name, "void") == 0) {
    token->ty = TK_VOID;
  } else if (strcmp(name, "int") == 0) {
    token->ty = TK_INT;
  } else if (strcmp(name, "char") == 0) {
    token->ty = TK_CHAR;
  } else if (strcmp(name, "if") == 0) {
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
  } else if (strcmp(name, "return") == 0) {
    token->ty = TK_RETURN;
  } else {
    token->ty = TK_IDENT;
  }
  token->input = input;
  token->name = name;
  return token;
}

static Token *new_token_str(const char *input, char *str) {
  Token *token = malloc(sizeof(Token));
  token->ty = TK_STR;
  token->input = input;
  token->str = str;
  return token;
}

static Token *punctuator(const char **input) {
  Token *token = NULL;
  const char *p = *input;
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
  case '[':
  case ']':
  case ',': {
    token = new_token(*p, p);
    p++;
    break;
  }
  }

  *input = p;
  return token;
}

static Token *identifier_or_keyword(const char **input) {
  Token *token = NULL;
  const char *p = *input;
  if (('a' <= *p && *p <= 'z') || ('A' <= *p && *p <= 'Z') || *p == '_') {
    const char *q = p;
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

static Token *constant(const char **input) {
  Token *token = NULL;
  const char *p = *input;

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

static Token *integer_constant(const char **input) {
  Token *token = NULL;
  const char *p = *input;
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

static Token *hexadecimal_constant(const char **input) {
  Token *token = NULL;
  const char *p = *input;
  if ((*p != '0') || ((*(p + 1) != 'x') && (*(p + 1)) != 'X')) {
    return NULL;
  }
  p += 2;

  int val = 0;

  const char *q = p;
  for (; isxdigit(*q); q++) {
    val = val * 0x10 + hex(*q);
  }

  token = new_token_num(p, val);
  p = q;

  *input = p;
  return token;
}

static Token *octal_constant(const char **input) {
  Token *token = NULL;
  const char *p = *input;
  if (*p != '0') {
    return NULL;
  }
  p += 1;

  int val = 0;

  const char *q = p;
  for (; '0' <= *q && *q <= '7'; q++) {
    val = val * 010 + (*q - '0');
  }

  token = new_token_num(p, val);
  p = q;

  *input = p;
  return token;
}

static Token *decimal_constant(const char **input) {
  Token *token = NULL;
  const char *p = *input;
  if (!isdigit(*p)) {
    return NULL;
  }
  int val = 0;

  const char *q = p;
  for (; isdigit(*q); q++) {
    val = val * 10 + (*q - '0');
  }

  token = new_token_num(p, val);
  p = q;

  *input = p;
  return token;
}

static Token *character_constant(const char **input) {
  Token *token = NULL;
  const char *p = *input;
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

static Token *string_literal(const char **input) {
  Token *token = NULL;
  const char *p = *input;
  if (*p != '"') {
    return NULL;
  }
  const char *q = p;
  q++;
  int alloc = 0;
  int len = 0;
  char *str = NULL;
  while (*q != '"' && *q != '\0') {
    if (len == alloc) {
      alloc = (alloc == 0) ? 8 : alloc * 2;
      str = realloc(str, alloc);
    }
    str[len] = c_char(&q);
    len++;
  }
  if (len == alloc) {
    alloc = (alloc == 0) ? 8 : alloc * 2;
    str = realloc(str, alloc);
  }
  str[len] = '\0';

  if (*q != '"') {
    error("文字列の終端がありません: %s", p);
  }
  q++;
  token = new_token_str(p, str);
  p = q;

  *input = p;
  return token;
}

static int c_char(const char **input) {
  int val = 0;
  const char *p = *input;
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
      const char *q = p + 2;
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

    const char *q = p + 1;
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