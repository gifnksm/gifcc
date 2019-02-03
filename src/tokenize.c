#include "gifcc.h"
#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

struct Tokenizer {
  Reader *reader;
  Token *current;
  Token *next;
  bool read_eof;
};

static Token *read_token(Reader *reader, bool *read_eof);
static Token *new_token(int ty, Reader *reader);
static Token *new_token_num(Reader *reader, int val);
static Token *new_token_ident(Reader *reader, char *name);
static Token *new_token_str(Reader *reader, char *str);
static Token *punctuator(Reader *reader);
static Token *identifier_or_keyword(Reader *reader);
static Token *constant(Reader *reader);
static Token *integer_constant(Reader *reader);
static Token *hexadecimal_constant(Reader *reader);
static Token *octal_constant(Reader *reader);
static Token *decimal_constant(Reader *reader);
static Token *character_constant(Reader *reader);
static Token *string_literal(Reader *reader);
static char c_char(Reader *reader);

Tokenizer *new_tokenizer(Reader *reader) {
  Tokenizer *tokenizer = malloc(sizeof(Tokenizer));
  tokenizer->reader = reader;
  tokenizer->read_eof = false;
  tokenizer->current = read_token(tokenizer->reader, &tokenizer->read_eof);
  tokenizer->next = read_token(tokenizer->reader, &tokenizer->read_eof);
  return tokenizer;
}

void token_succ(Tokenizer *tokenizer) {
  tokenizer->current = tokenizer->next;
  tokenizer->next = read_token(tokenizer->reader, &tokenizer->read_eof);
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

static Token *read_token(Reader *reader, bool *read_eof) {
  char ch;
  while ((ch = reader_peek(reader)) != '\0') {
    // 空白文字をスキップ
    if (isspace(ch)) {
      reader_succ(reader);
      continue;
    }

    // コメントをスキップ
    if (ch == '/') {
      char ch2 = reader_peek_ahead(reader, 1);
      if (ch2 == '/') {
        reader_succ_n(reader, 2);
        while (true) {
          char ch = reader_peek(reader);
          if (ch == '\n' || ch == '\0') {
            break;
          }
          reader_succ(reader);
        }
        continue;
      }
      if (ch2 == '*') {
        reader_succ_n(reader, 2);
        while (true) {
          if (reader_peek(reader) == '*' &&
              reader_peek_ahead(reader, 1) == '/') {
            reader_succ_n(reader, 2);
            break;
          }
          if (reader_peek(reader) == '\0') {
            break;
          }
          reader_succ(reader);
        }
        continue;
      }
    }

    Token *token = NULL;
    if ((token = punctuator(reader)) != NULL) {
      return token;
    }
    if ((token = identifier_or_keyword(reader)) != NULL) {
      return token;
    }
    if ((token = constant(reader)) != NULL) {
      return token;
    }
    if ((token = string_literal(reader)) != NULL) {
      return token;
    }

    error("トークナイズできません: %s", reader_rest(reader));
  }

  if (!*read_eof) {
    *read_eof = true;
    return new_token(TK_EOF, reader);
  }
  return NULL;
}

static Token *new_token(int ty, Reader *reader) {
  Token *token = malloc(sizeof(Token));
  token->ty = ty;
  token->input = reader_rest(reader);
  return token;
}

static Token *new_token_num(Reader *reader, int val) {
  Token *token = malloc(sizeof(Token));
  token->ty = TK_NUM;
  token->input = reader_rest(reader);
  token->val = val;
  return token;
}

static Token *new_token_ident(Reader *reader, char *name) {
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
  token->input = reader_rest(reader);
  token->name = name;
  return token;
}

static Token *new_token_str(Reader *reader, char *str) {
  Token *token = malloc(sizeof(Token));
  token->ty = TK_STR;
  token->input = reader_rest(reader);
  token->str = str;
  return token;
}

static Token *punctuator(Reader *reader) {
  Token *token = NULL;
  switch (reader_peek(reader)) {
  case '=': {
    if (reader_peek_ahead(reader, 1) == '=') {
      token = new_token(TK_EQEQ, reader);
      reader_succ_n(reader, 2);
      break;
    }
    token = new_token(reader_peek(reader), reader);
    reader_succ(reader);
    break;
  }
  case '!': {
    if (reader_peek_ahead(reader, 1) == '=') {
      token = new_token(TK_NOTEQ, reader);
      reader_succ_n(reader, 2);
      break;
    }
    token = new_token(reader_peek(reader), reader);
    reader_succ(reader);
    break;
  }
  case '<': {
    if (reader_peek_ahead(reader, 1) == '<') {
      if (reader_peek_ahead(reader, 2) == '=') {
        token = new_token(TK_LSHIFT_ASSIGN, reader);
        reader_succ_n(reader, 3);
        break;
      }
      token = new_token(TK_LSHIFT, reader);
      reader_succ_n(reader, 2);
      break;
    }
    if (reader_peek_ahead(reader, 1) == '=') {
      token = new_token(TK_LTEQ, reader);
      reader_succ_n(reader, 2);
      break;
    }
    token = new_token(reader_peek(reader), reader);
    reader_succ(reader);
    break;
  }
  case '>': {
    if (reader_peek_ahead(reader, 1) == '>') {
      if (reader_peek_ahead(reader, 2) == '=') {
        token = new_token(TK_RSHIFT_ASSIGN, reader);
        reader_succ_n(reader, 3);
        break;
      }
      token = new_token(TK_RSHIFT, reader);
      reader_succ_n(reader, 2);
      break;
    }
    if (reader_peek_ahead(reader, 1) == '=') {
      token = new_token(TK_GTEQ, reader);
      reader_succ_n(reader, 2);
      break;
    }
    token = new_token(reader_peek(reader), reader);
    reader_succ(reader);
    break;
  }
  case '&': {
    if (reader_peek_ahead(reader, 1) == '&') {
      token = new_token(TK_LOGAND, reader);
      reader_succ_n(reader, 2);
      break;
    }
    if (reader_peek_ahead(reader, 1) == '=') {
      token = new_token(TK_AND_ASSIGN, reader);
      reader_succ_n(reader, 2);
      break;
    }
    token = new_token(reader_peek(reader), reader);
    reader_succ(reader);
    break;
  }
  case '|': {
    if (reader_peek_ahead(reader, 1) == '|') {
      token = new_token(TK_LOGOR, reader);
      reader_succ_n(reader, 2);
      break;
    }
    if (reader_peek_ahead(reader, 1) == '=') {
      token = new_token(TK_OR_ASSIGN, reader);
      reader_succ_n(reader, 2);
      break;
    }
    token = new_token(reader_peek(reader), reader);
    reader_succ(reader);
    break;
  }
  case '^': {
    if (reader_peek_ahead(reader, 1) == '=') {
      token = new_token(TK_XOR_ASSIGN, reader);
      reader_succ_n(reader, 2);
      break;
    }
    token = new_token(reader_peek(reader), reader);
    reader_succ(reader);
    break;
  }
  case '+': {
    if (reader_peek_ahead(reader, 1) == '+') {
      token = new_token(TK_INC, reader);
      reader_succ_n(reader, 2);
      break;
    }
    if (reader_peek_ahead(reader, 1) == '=') {
      token = new_token(TK_ADD_ASSIGN, reader);
      reader_succ_n(reader, 2);
      break;
    }
    token = new_token(reader_peek(reader), reader);
    reader_succ(reader);
    break;
  }
  case '-': {
    if (reader_peek_ahead(reader, 1) == '-') {
      token = new_token(TK_DEC, reader);
      reader_succ_n(reader, 2);
      break;
    }
    if (reader_peek_ahead(reader, 1) == '=') {
      token = new_token(TK_SUB_ASSIGN, reader);
      reader_succ_n(reader, 2);
      break;
    }
    token = new_token(reader_peek(reader), reader);
    reader_succ(reader);
    break;
  }
  case '*': {
    if (reader_peek_ahead(reader, 1) == '=') {
      token = new_token(TK_MUL_ASSIGN, reader);
      reader_succ_n(reader, 2);
      break;
    }
    token = new_token(reader_peek(reader), reader);
    reader_succ(reader);
    break;
  }
  case '/': {
    if (reader_peek_ahead(reader, 1) == '=') {
      token = new_token(TK_DIV_ASSIGN, reader);
      reader_succ_n(reader, 2);
      break;
    }
    token = new_token(reader_peek(reader), reader);
    reader_succ(reader);
    break;
  }
  case '%': {
    if (reader_peek_ahead(reader, 1) == '=') {
      token = new_token(TK_MOD_ASSIGN, reader);
      reader_succ_n(reader, 2);
      break;
    }
    token = new_token(reader_peek(reader), reader);
    reader_succ(reader);
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
    token = new_token(reader_peek(reader), reader);
    reader_succ(reader);
    break;
  }
  }

  return token;
}

static Token *identifier_or_keyword(Reader *reader) {
  char ch = reader_peek(reader);
  if ((ch < 'a' || 'z' < ch) && (ch < 'A' || 'Z' < ch) && ch != '_') {
    return NULL;
  }

  String *str = new_string();
  str_push(str, ch);
  reader_succ(reader);

  while (true) {
    ch = reader_peek(reader);
    if ((ch < 'a' || 'z' < ch) && (ch < 'A' || 'Z' < ch) && ch != '_' &&
        (ch < '0' || '9' < ch)) {
      break;
    }
    str_push(str, ch);
    reader_succ(reader);
  }
  str_push(str, '\0');

  return new_token_ident(reader, str->data);
}

static Token *constant(Reader *reader) {
  Token *token = NULL;

  if ((token = integer_constant(reader)) != NULL) {
    return token;
  }
  if ((token = character_constant(reader)) != NULL) {
    return token;
  }

  return NULL;
}

static Token *integer_constant(Reader *reader) {
  Token *token = NULL;
  if ((token = hexadecimal_constant(reader)) != NULL) {
    return token;
  }
  if ((token = octal_constant(reader)) != NULL) {
    return token;
  }
  if ((token = decimal_constant(reader)) != NULL) {
    return token;
  }
  return NULL;
}

static Token *hexadecimal_constant(Reader *reader) {
  char ch0 = reader_peek(reader);
  char ch1 = reader_peek_ahead(reader, 1);
  if ((ch0 != '0') || (ch1 != 'x' && ch1 != 'X')) {
    return NULL;
  }
  reader_succ_n(reader, 2);

  int val = 0;

  while (true) {
    char ch = reader_peek(reader);
    if (!isxdigit(ch)) {
      break;
    }
    val = val * 0x10 + hex(ch);
    reader_succ(reader);
  }

  return new_token_num(reader, val);
}

static Token *octal_constant(Reader *reader) {
  if (reader_peek(reader) != '0') {
    return NULL;
  }
  reader_succ(reader);

  int val = 0;

  while (true) {
    char ch = reader_peek(reader);
    if (ch < '0' || '7' < ch) {
      break;
    }
    val = val * 010 + (ch - '0');
    reader_succ(reader);
  }

  return new_token_num(reader, val);
}

static Token *decimal_constant(Reader *reader) {
  if (!isdigit(reader_peek(reader))) {
    return NULL;
  }
  int val = 0;

  while (true) {
    char ch = reader_peek(reader);
    if (!isdigit(ch)) {
      break;
    }
    val = val * 10 + (ch - '0');
    reader_succ(reader);
  }

  return new_token_num(reader, val);
}

static Token *character_constant(Reader *reader) {
  Token *token = NULL;
  if (reader_peek(reader) != '\'') {
    return NULL;
  }
  reader_succ(reader);

  token = new_token_num(reader, c_char(reader));

  if (reader_peek(reader) != '\'') {
    error("'\\'' がありません: %s", reader_rest(reader));
  }
  reader_succ(reader);

  return token;
}

static Token *string_literal(Reader *reader) {
  Token *token = NULL;
  if (reader_peek(reader) != '"') {
    return NULL;
  }
  reader_succ(reader);

  String *str = new_string();
  while (true) {
    char ch = reader_peek(reader);
    if (ch == '"' || ch == '\0') {
      break;
    }
    str_push(str, c_char(reader));
  }
  str_push(str, '\0');

  if (reader_peek(reader) != '"') {
    error("文字列の終端がありません: %s", reader_rest(reader));
  }
  reader_succ(reader);
  token = new_token_str(reader, str->data);

  return token;
}

static char c_char(Reader *reader) {
  int val = 0;
  char ch = reader_peek(reader);
  if (ch == '\'') {
    error("空の文字リテラルです: %s", reader_rest(reader));
  }
  if (ch == '\n' || ch == '\r') {
    error("改行文字を文字リテラル中に含めることはできません: %s",
          reader_rest(reader));
  }

  if (ch == '\\') {
    const char *p = reader_rest(reader);
    char ch1 = reader_peek_ahead(reader, 1);
    switch (ch1) {
    case '\'':
    case '\"':
    case '\?':
    case '\\': {
      reader_succ_n(reader, 2);
      return ch1;
    }
    case 'a': {
      reader_succ_n(reader, 2);
      return '\a';
    }
    case 'b': {
      reader_succ_n(reader, 2);
      return '\b';
    }
    case 'f': {
      reader_succ_n(reader, 2);
      return '\f';
    }
    case 'n': {
      reader_succ_n(reader, 2);
      return '\n';
    }
    case 'r': {
      reader_succ_n(reader, 2);
      return '\r';
    }
    case 't': {
      reader_succ_n(reader, 2);
      return '\t';
    }
    case 'v': {
      reader_succ_n(reader, 2);
      return '\v';
    }

    case 'x': {
      const char *q = p + 2;
      for (; isxdigit(*q); q++) {
        val = val * 0x10 + hex(*q);
      }
      if (q == p + 2) {
        error("空の16進文字リテラルです: %s", p);
      }
      reader_succ_n(reader, q - p);
      return val;
    }
    }

    const char *q = p + 1;
    for (int i = 0; (i < 3) && ('0' <= *q && *q <= '7'); i++, q++) {
      val = 010 * val + (*q - '0');
    }
    if (q - p >= 2) {
      reader_succ_n(reader, q - p);
      return val;
    }

    error("不明なエスケープシーケンスです: %s", p);
  }

  reader_succ(reader);
  return ch;
}
