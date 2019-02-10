#include "gifcc.h"
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

struct Tokenizer {
  Reader *reader;
  Token *current;
  Token *next;
  bool read_eof;
};

typedef struct {
  char *str;
  int kind;
} LongToken;

static const LongToken LONG_IDENT_TOKENS[] = {
    {"void", TK_VOID},     {"int", TK_INT},
    {"short", TK_SHORT},   {"long", TK_LONG},
    {"char", TK_CHAR},     {"if", TK_IF},
    {"else", TK_ELSE},     {"switch", TK_SWITCH},
    {"case", TK_CASE},     {"default", TK_DEFAULT},
    {"while", TK_WHILE},   {"do", TK_DO},
    {"for", TK_FOR},       {"goto", TK_GOTO},
    {"break", TK_BREAK},   {"continue", TK_CONTINUE},
    {"return", TK_RETURN}, {"struct", TK_STRUCT},
    {"union", TK_UNION},   {"typedef", TK_TYPEDEF},
    {"sizeof", TK_SIZEOF}, {NULL, '\0'},
};
static const LongToken LONG_PUNCT_TOKENS[] = {
    {"==", TK_EQEQ},       {"!=", TK_NOTEQ},      {"<<=", TK_LSHIFT_ASSIGN},
    {"<<", TK_LSHIFT},     {"<=", TK_LTEQ},       {">>=", TK_RSHIFT_ASSIGN},
    {">>", TK_RSHIFT},     {">=", TK_GTEQ},       {"&&", TK_LOGAND},
    {"&=", TK_AND_ASSIGN}, {"||", TK_LOGOR},      {"|=", TK_OR_ASSIGN},
    {"^=", TK_XOR_ASSIGN}, {"++", TK_INC},        {"+=", TK_ADD_ASSIGN},
    {"--", TK_DEC},        {"-=", TK_SUB_ASSIGN}, {"*=", TK_MUL_ASSIGN},
    {"/=", TK_DIV_ASSIGN}, {"%=", TK_MOD_ASSIGN}, {"->", TK_ARROW},
    {NULL, '\0'},
};
static const char *SHORT_PUNCT_TOKENS = "=!<>&|^+-*/%();?:~{}[],.";

static Token *read_token(Reader *reader, bool *read_eof);
static Token *new_token(int ty);
static Token *new_token_num(int val);
static Token *new_token_ident(char *name);
static Token *new_token_str(char *str);
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

Token *token_consume2(Tokenizer *tokenizer, int ty1, int ty2) {
  if (token_peek(tokenizer)->ty == ty1 &&
      token_peek_ahead(tokenizer, 1)->ty == ty2) {
    (void)token_pop(tokenizer);
    return token_pop(tokenizer);
  }
  return false;
}

Token *token_expect(Tokenizer *tokenizer, int ty) {
  Token *token = token_pop(tokenizer);
  if (token->ty != ty) {
    range_error(token->range, "%sがありません", token_kind_to_str(ty));
  }
  return token;
}

static const char *quote(const char *s) {
  int len = strlen(s);
  char *str = calloc(len + 3, sizeof(char));
  strcat(str, "`");
  strcat(str, s);
  strcat(str, "`");
  return str;
}

const char *token_kind_to_str(int kind) {
  if (kind <= 255) {
    char str[] = " ";
    str[0] = kind;
    return quote(str);
  }

  switch (kind) {
  case TK_IDENT:
    return "IDENT";
  case TK_NUM:
    return "NUM";
  case TK_STR:
    return "STR";
  case TK_EOF:
    return "EOF";
  default: {
    const LongToken *ltss[] = {LONG_IDENT_TOKENS, LONG_PUNCT_TOKENS, NULL};
    for (int j = 0; ltss[j] != NULL; j++) {
      const LongToken *lts = ltss[j];
      for (int i = 0; lts[i].str != NULL; i++) {
        const LongToken *tk = &lts[i];
        if (tk->kind == kind) {
          return quote(tk->str);
        }
      }
    }
    abort();
  }
  }
}

const Reader *token_get_reader(const Tokenizer *tokenizer) {
  return tokenizer->reader;
}

static inline bool is_ident_head(int c) {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}
static inline bool is_ident_tail(int c) {
  return is_ident_head(c) || ('0' <= c && c <= '9');
}

static inline bool is_hex_digit(int c) { return isxdigit(c) != 0; }
static inline int hex2num(int c) {
  assert(is_hex_digit(c));
  if ('0' <= c && c <= '9') {
    return c - '0';
  }
  if ('a' <= c && c <= 'f') {
    return (c - 'a') + 0xa;
  }
  assert('A' <= c && c <= 'F');
  return (c - 'A') + 0xa;
}
static inline bool is_oct_digit(int c) { return '0' <= c && c <= '7'; }
static inline int oct2num(int c) {
  assert(is_oct_digit(c));
  return c - '0';
}
static inline bool is_dec_digit(int c) { return isdigit(c) != 0; }
static inline int dec2num(int c) {
  assert(is_dec_digit(c));
  return c - '0';
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
    if (reader_consume_str(reader, "//")) {
      while (true) {
        char ch = reader_peek(reader);
        if (ch == '\n' || ch == '\0') {
          break;
        }
        reader_succ(reader);
      }
      continue;
    }
    if (reader_consume_str(reader, "/*")) {
      while (true) {
        if (reader_consume_str(reader, "*/")) {
          break;
        }
        if (reader_peek(reader) == '\0') {
          reader_error_here(reader, "コメントの終端文字列 `*/` がありません");
        }
        reader_succ(reader);
      }
      continue;
    }

    int start = reader_get_offset(reader);
    Token *token = NULL;
    if ((token = punctuator(reader)) == NULL &&
        (token = identifier_or_keyword(reader)) == NULL &&
        (token = constant(reader)) == NULL &&
        (token = string_literal(reader)) == NULL) {
      reader_error_here(reader, "トークナイズできません: `%c`",
                        reader_peek(reader));
    }

    int end = reader_get_offset(reader);
    token->range.reader = reader;
    token->range.start = start;
    token->range.len = end - start;
    return token;
  }

  if (!*read_eof) {
    *read_eof = true;
    Token *token = new_token(TK_EOF);
    token->range.reader = reader;
    token->range.start = reader_get_offset(reader);
    token->range.len = 0;
    return token;
  }

  return NULL;
}

static Token *new_token(int ty) {
  Token *token = malloc(sizeof(Token));
  token->ty = ty;
  return token;
}

static Token *new_token_num(int val) {
  Token *token = malloc(sizeof(Token));
  token->ty = TK_NUM;
  token->val = val;
  return token;
}

static Token *new_token_ident(char *name) {
  Token *token = malloc(sizeof(Token));
  token->ty = TK_IDENT;
  for (int i = 0; LONG_IDENT_TOKENS[i].str != NULL; i++) {
    const LongToken *tk = &LONG_IDENT_TOKENS[i];
    if (strcmp(name, tk->str) == 0) {
      token->ty = tk->kind;
      break;
    }
  }
  token->name = name;
  return token;
}

static Token *new_token_str(char *str) {
  Token *token = malloc(sizeof(Token));
  token->ty = TK_STR;
  token->str = str;
  return token;
}

static Token *punctuator(Reader *reader) {
  for (int i = 0; LONG_PUNCT_TOKENS[i].str != NULL; i++) {
    const LongToken *tk = &LONG_PUNCT_TOKENS[i];
    if (reader_consume_str(reader, tk->str)) {
      return new_token(tk->kind);
    }
  }
  for (int i = 0; SHORT_PUNCT_TOKENS[i] != '\0'; i++) {
    char tk = SHORT_PUNCT_TOKENS[i];
    if (reader_consume(reader, tk)) {
      return new_token(tk);
    }
  }

  return NULL;
}

static Token *identifier_or_keyword(Reader *reader) {
  char ch = reader_peek(reader);
  if (!is_ident_head(ch)) {
    return NULL;
  }

  String *str = new_string();
  str_push(str, ch);
  reader_succ(reader);

  while (true) {
    ch = reader_peek(reader);
    if (!is_ident_tail(ch)) {
      break;
    }
    str_push(str, ch);
    reader_succ(reader);
  }
  str_push(str, '\0');

  return new_token_ident(str->data);
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
  int start = reader_get_offset(reader);
  if (!reader_consume_str(reader, "0x") && !reader_consume_str(reader, "0X")) {
    return NULL;
  }

  int val = 0;

  if (!is_hex_digit(reader_peek(reader))) {
    reader_error_offset(reader, start, "空の16進文字リテラルです");
  }

  while (true) {
    char ch = reader_peek(reader);
    if (!is_hex_digit(ch)) {
      break;
    }
    val = val * 0x10 + hex2num(ch);
    reader_succ(reader);
  }

  return new_token_num(val);
}

static Token *octal_constant(Reader *reader) {
  if (!reader_consume(reader, '0')) {
    return NULL;
  }

  int val = 0;

  while (true) {
    char ch = reader_peek(reader);
    if (!is_oct_digit(ch)) {
      break;
    }
    val = val * 010 + oct2num(ch);
    reader_succ(reader);
  }

  return new_token_num(val);
}

static Token *decimal_constant(Reader *reader) {
  if (!isdigit(reader_peek(reader))) {
    return NULL;
  }
  int val = 0;

  while (true) {
    char ch = reader_peek(reader);
    if (!is_dec_digit(ch)) {
      break;
    }
    val = val * 10 + dec2num(ch);
    reader_succ(reader);
  }

  return new_token_num(val);
}

static Token *character_constant(Reader *reader) {
  int start = reader_get_offset(reader);
  if (!reader_consume(reader, '\'')) {
    return NULL;
  }
  char ch;
  ch = reader_peek(reader);
  if (ch == '\'' || ch == '\0') {
    reader_error_offset(reader, start, "空の文字リテラルです");
  }
  ch = c_char(reader);
  reader_expect(reader, '\'');
  return new_token_num(ch);
}

static Token *string_literal(Reader *reader) {
  if (!reader_consume(reader, '"')) {
    return NULL;
  }

  String *str = new_string();
  while (true) {
    char ch = reader_peek(reader);
    if (ch == '"' || ch == '\0') {
      break;
    }
    str_push(str, c_char(reader));
  }
  str_push(str, '\0');

  reader_expect(reader, '"');
  return new_token_str(str->data);
}

static char c_char(Reader *reader) {
  int start = reader_get_offset(reader);

  if (reader_consume(reader, '\\')) {
    const char ESCAPE_CHARS[] = {'\'', '"', '?', '\\', '\0'};
    for (int i = 0; ESCAPE_CHARS[i] != '\0'; i++) {
      if (reader_consume(reader, ESCAPE_CHARS[i])) {
        return ESCAPE_CHARS[i];
      }
    }

    typedef struct {
      char raw;
      char meta;
    } MetaChar;

    MetaChar META_CHARS[] = {
        {'a', '\a'}, {'b', '\b'}, {'f', '\f'}, {'n', '\n'},
        {'r', '\r'}, {'t', '\t'}, {'v', '\v'}, {'\0', '\0'},
    };
    for (int i = 0; META_CHARS[i].raw != '\0'; i++) {
      if (reader_consume(reader, META_CHARS[i].raw)) {
        return META_CHARS[i].meta;
      }
    }

    if (reader_consume(reader, 'x')) {
      if (!is_hex_digit(reader_peek(reader))) {
        reader_error_offset(reader, start, "空の16進文字リテラルです");
      }
      int val = 0;
      while (true) {
        char ch = reader_peek(reader);
        if (!is_hex_digit(ch)) {
          break;
        }
        val = val * 0x10 + hex2num(ch);
        reader_succ(reader);
      }
      return val;
    }

    if (is_oct_digit(reader_peek(reader))) {
      int val = 0;
      while (true) {
        char ch = reader_peek(reader);
        if (!is_oct_digit(ch)) {
          break;
        }
        val = val * 010 + oct2num(ch);
        reader_succ(reader);
      }
      return val;
    }

    reader_error_offset(reader, start, "不明なエスケープシーケンスです");
  }

  if (reader_consume(reader, '\n')) {
    reader_error_here(reader,
                      "改行文字を文字リテラル中に含めることはできません");
  }

  return reader_pop(reader);
}
