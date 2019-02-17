#include "gifcc.h"
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

struct Tokenizer {
  Reader *reader;
  Vector *tokens;
  bool read_eof;
  Map *define_map;
};

typedef struct {
  char *str;
  int kind;
} LongToken;

static const LongToken LONG_IDENT_TOKENS[] = {
    {"void", TK_VOID},         {"int", TK_INT},
    {"short", TK_SHORT},       {"long", TK_LONG},
    {"char", TK_CHAR},         {"signed", TK_SIGNED},
    {"unsigned", TK_UNSIGNED}, {"if", TK_IF},
    {"else", TK_ELSE},         {"switch", TK_SWITCH},
    {"case", TK_CASE},         {"default", TK_DEFAULT},
    {"while", TK_WHILE},       {"do", TK_DO},
    {"for", TK_FOR},           {"goto", TK_GOTO},
    {"break", TK_BREAK},       {"continue", TK_CONTINUE},
    {"return", TK_RETURN},     {"struct", TK_STRUCT},
    {"union", TK_UNION},       {"enum", TK_ENUM},
    {"sizeof", TK_SIZEOF},     {"typedef", TK_TYPEDEF},
    {"extern", TK_EXTERN},     {"static", TK_STATIC},
    {"const", TK_CONST},       {NULL, '\0'},
};
static const LongToken LONG_PUNCT_TOKENS[] = {
    {"==", TK_EQEQ},       {"!=", TK_NOTEQ},      {"<<=", TK_LSHIFT_ASSIGN},
    {"<<", TK_LSHIFT},     {"<=", TK_LTEQ},       {">>=", TK_RSHIFT_ASSIGN},
    {">>", TK_RSHIFT},     {">=", TK_GTEQ},       {"&&", TK_LOGAND},
    {"&=", TK_AND_ASSIGN}, {"||", TK_LOGOR},      {"|=", TK_OR_ASSIGN},
    {"^=", TK_XOR_ASSIGN}, {"++", TK_INC},        {"+=", TK_ADD_ASSIGN},
    {"--", TK_DEC},        {"-=", TK_SUB_ASSIGN}, {"*=", TK_MUL_ASSIGN},
    {"/=", TK_DIV_ASSIGN}, {"%=", TK_MOD_ASSIGN}, {"->", TK_ARROW},
    {"...", TK_ELIPSIS},   {NULL, '\0'},
};
static const char *SHORT_PUNCT_TOKENS = "=!<>&|^+-*/%();?:~{}[],.#";

static bool read_token(Tokenizer *tokenizer);
static Token *new_token(int ty, Range range);
static Token *new_token_num(Number val, Range range);
static Token *new_token_ident(char *name, Range range);
static Token *new_token_str(char *str, Range range);
static bool pp_directive(Reader *reader, Map *define_map);
static void do_include(Reader *reader, const char *path, Range range);
static bool read_normal_token(Reader *reader, Vector *tokens);
static bool punctuator(Reader *reader, Vector *tokens);
static bool identifier_or_keyword(Reader *reader, Vector *tokens);
static bool constant(Reader *reader, Vector *tokens);
static bool integer_constant(Reader *reader, Vector *tokens);
static bool hexadecimal_constant(Reader *reader, Vector *tokens);
static bool octal_constant(Reader *reader, Vector *tokens);
static bool decimal_constant(Reader *reader, Vector *tokens);
static bool character_constant(Reader *reader, Vector *tokens);
static bool string_literal(Reader *reader, Vector *tokens);
static char c_char(Reader *reader);

Tokenizer *new_tokenizer(Reader *reader) {
  Tokenizer *tokenizer = malloc(sizeof(Tokenizer));
  tokenizer->reader = reader;
  tokenizer->read_eof = false;
  tokenizer->define_map = new_map();
  tokenizer->tokens = new_vector();
  return tokenizer;
}

void token_succ(Tokenizer *tokenizer) {
  assert(vec_len(tokenizer->tokens) > 0);
  vec_remove(tokenizer->tokens, 0);
}

Token *token_peek(Tokenizer *tokenizer) {
  return token_peek_ahead(tokenizer, 0);
}

Token *token_peek_ahead(Tokenizer *tokenizer, int n) {
  while (vec_len(tokenizer->tokens) <= n) {
    if (!read_token(tokenizer)) {
      return NULL;
    }
  }
  return vec_get(tokenizer->tokens, n);
}

Token *token_pop(Tokenizer *tokenizer) {
  Token *token = token_peek(tokenizer);
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

static bool skip_space(Reader *reader) {
  bool skipped = false;
  while (true) {
    char ch = reader_peek(reader);
    if (!isspace(ch) || ch == '\n') {
      return skipped;
    }
    skipped = true;
    reader_succ(reader);
  }
}

static bool skip_comment(Reader *reader) {
  bool skipped = false;

  while (true) {
    if (reader_consume_str(reader, "//")) {
      while (true) {
        char ch = reader_peek(reader);
        if (ch == '\n' || ch == '\0') {
          break;
        }
        reader_succ(reader);
      }
      skipped = true;
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
      skipped = true;
      continue;
    }
    break;
  }

  return skipped;
}

static bool skip_space_or_comment(Reader *reader) {
  bool skipped = false;
  while (true) {
    if (skip_space(reader)) {
      skipped = true;
      continue;
    }
    if (skip_comment(reader)) {
      skipped = true;
      continue;
    }
    break;
  }
  return skipped;
}

static String *read_identifier(Reader *reader) {
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

  return str;
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
static bool read_token(Tokenizer *tokenizer) {
  char ch;
  while ((ch = reader_peek(tokenizer->reader)) != '\0') {
    if (skip_space_or_comment(tokenizer->reader)) {
      continue;
    }
    if (ch == '\n') {
      reader_succ(tokenizer->reader);
      continue;
    }

    if (pp_directive(tokenizer->reader, tokenizer->define_map)) {
      continue;
    }

    if (!read_normal_token(tokenizer->reader, tokenizer->tokens)) {
      reader_error_here(tokenizer->reader, "トークナイズできません: `%c`",
                        reader_peek(tokenizer->reader));
    }

    return true;
  }

  if (!tokenizer->read_eof) {
    tokenizer->read_eof = true;
    int start = reader_get_offset(tokenizer->reader);
    int end = reader_get_offset(tokenizer->reader);
    Token *token =
        new_token(TK_EOF, range_from_reader(tokenizer->reader, start, end));
    vec_push(tokenizer->tokens, token);
    return true;
  }

  return false;
}

static Token *new_token(int ty, Range range) {
  Token *token = NEW(Token);
  token->ty = ty;
  token->range = range;
  return token;
}

static Token *new_token_num(Number val, Range range) {
  Token *token = new_token(TK_NUM, range);
  token->num_val = val;
  return token;
}

static Token *new_token_ident(char *name, Range range) {
  Token *token = new_token(TK_IDENT, range);

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

static Token *new_token_str(char *str, Range range) {
  Token *token = new_token(TK_STR, range);
  token->str = str;
  return token;
}

static bool pp_directive(Reader *reader, Map *define_map) {
  int start = reader_get_offset(reader);
  if (!reader_consume(reader, '#')) {
    return false;
  }
  skip_space_or_comment(reader);

  if (reader_consume_str(reader, "include")) {
    skip_space_or_comment(reader);

    if (reader_consume(reader, '<')) {
      String *str = new_string();
      while ((reader_peek(reader) != '>') && (reader_peek(reader) != '\0')) {
        char ch = reader_pop(reader);
        str_push(str, ch);
      }
      reader_expect(reader, '>');
      int end = reader_get_offset(reader);

      skip_space_or_comment(reader);
      reader_expect(reader, '\n');

      do_include(reader, str_get_raw(str),
                 range_from_reader(reader, start, end));

      return true;
    }

    reader_error_here(reader, "識別子がありません");
  }

  if (reader_consume_str(reader, "define")) {
    skip_space_or_comment(reader);
    String *ident = read_identifier(reader);
    if (ident == NULL) {
      reader_error_here(reader, "識別子がありません");
    }
    skip_space_or_comment(reader);
    Vector *tokens = new_vector();
    while (reader_peek(reader) != '\n' && reader_peek(reader) != '\0') {
      if (!read_normal_token(reader, tokens)) {
        reader_error_here(reader, "トークナイズできません: `%c`",
                          reader_peek(reader));
      }
      skip_space_or_comment(reader);
    }
    reader_expect(reader, '\n');

    map_put(define_map, str_get_raw(ident), tokens);

    return true;
  }

  while ((reader_peek(reader) != '\n') && (reader_peek(reader) != '\0')) {
    reader_succ(reader);
  }
  int end = reader_get_offset(reader);
  reader_expect(reader, '\n');
  range_warn(range_from_reader(reader, start, end), "不明なディレクティブです");

  return true;
}

static void do_include(Reader *reader, const char *path, Range range) {
  char *abs_path = NULL;
  alloc_printf(&abs_path, "%s/%s", GIFCC_INCLUDE, path);

  FILE *fp = fopen(abs_path, "r");
  if (fp == NULL) {
    range_error(range, "ファイルが開けませんでした: %s", abs_path);
  }
  reader_add_file(reader, fp, path);
}

static bool read_normal_token(Reader *reader, Vector *tokens) {
  return punctuator(reader, tokens) || identifier_or_keyword(reader, tokens) ||
         constant(reader, tokens) || string_literal(reader, tokens);
}

static bool punctuator(Reader *reader, Vector *tokens) {
  int kind;
  int start = reader_get_offset(reader);

  for (int i = 0; LONG_PUNCT_TOKENS[i].str != NULL; i++) {
    const LongToken *tk = &LONG_PUNCT_TOKENS[i];
    if (reader_consume_str(reader, tk->str)) {
      kind = tk->kind;
      goto Hit;
    }
  }

  for (int i = 0; SHORT_PUNCT_TOKENS[i] != '\0'; i++) {
    char tk = SHORT_PUNCT_TOKENS[i];
    if (reader_consume(reader, tk)) {
      kind = tk;
      goto Hit;
    }
  }

  return false;

Hit:;
  int end = reader_get_offset(reader);
  vec_push(tokens, new_token(kind, range_from_reader(reader, start, end)));
  return true;
}

static bool identifier_or_keyword(Reader *reader, Vector *tokens) {
  int start = reader_get_offset(reader);
  String *str = read_identifier(reader);
  if (str == NULL) {
    return false;
  }
  int end = reader_get_offset(reader);
  vec_push(tokens, new_token_ident(str_get_raw(str),
                                   range_from_reader(reader, start, end)));
  return true;
}

static bool constant(Reader *reader, Vector *tokens) {
  return integer_constant(reader, tokens) || character_constant(reader, tokens);
}

static bool integer_constant(Reader *reader, Vector *tokens) {
  return hexadecimal_constant(reader, tokens) ||
         octal_constant(reader, tokens) || decimal_constant(reader, tokens);
}

static bool hexadecimal_constant(Reader *reader, Vector *tokens) {
  int start = reader_get_offset(reader);

  if (!reader_consume_str(reader, "0x") && !reader_consume_str(reader, "0X")) {
    return false;
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

  int end = reader_get_offset(reader);
  vec_push(tokens, new_token_num((Number){.type = TY_S_INT, .s_int_val = val},
                                 range_from_reader(reader, start, end)));
  return true;
}

static bool octal_constant(Reader *reader, Vector *tokens) {
  int start = reader_get_offset(reader);

  if (!reader_consume(reader, '0')) {
    return false;
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

  int end = reader_get_offset(reader);
  vec_push(tokens, new_token_num((Number){.type = TY_S_INT, .s_int_val = val},
                                 range_from_reader(reader, start, end)));
  return true;
}

static bool decimal_constant(Reader *reader, Vector *tokens) {
  int start = reader_get_offset(reader);

  if (!isdigit(reader_peek(reader))) {
    return false;
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

  int end = reader_get_offset(reader);
  vec_push(tokens, new_token_num((Number){.type = TY_S_INT, .s_int_val = val},
                                 range_from_reader(reader, start, end)));
  return true;
}

static bool character_constant(Reader *reader, Vector *tokens) {
  int start = reader_get_offset(reader);
  if (!reader_consume(reader, '\'')) {
    return false;
  }
  char ch;
  ch = reader_peek(reader);
  if (ch == '\'' || ch == '\0') {
    reader_error_offset(reader, start, "空の文字リテラルです");
  }
  ch = c_char(reader);
  reader_expect(reader, '\'');

  int end = reader_get_offset(reader);
  vec_push(tokens, new_token_num((Number){.type = TY_S_INT, .s_int_val = ch},
                                 range_from_reader(reader, start, end)));
  return true;
}

static bool string_literal(Reader *reader, Vector *tokens) {
  int start = reader_get_offset(reader);
  if (!reader_consume(reader, '"')) {
    return false;
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
  int end = reader_get_offset(reader);
  vec_push(tokens, new_token_str(str_get_raw(str),
                                 range_from_reader(reader, start, end)));
  return true;
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
