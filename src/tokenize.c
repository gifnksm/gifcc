#include "gifcc.h"
#include <assert.h>
#include <ctype.h>
#include <libgen.h>
#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

struct Tokenizer {
  Reader *reader;
  Vector *tokens;
  bool read_eof;
  Map *define_map;
  Vector *pp_cond_stack;
};

typedef struct {
  char *str;
  int kind;
} LongToken;

typedef struct Cond {
  bool once_fullfilled;
  bool fullfilled;
} Cond;

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
static Token *token_clone(Token *token);
static Token *new_token_num(Number val, Range range);
static Token *new_token_int_from_str(const char *str, const char *suffix,
                                     int base, Range range);
static Token *new_token_ident(char *name, Range range);
static Token *new_token_str(char *str, Range range);
static bool pp_directive(Reader *reader, Map *define_map,
                         Vector *pp_cond_stack);
static bool pp_cond_fullfilled(Vector *pp_cond_stack);
static bool pp_read_if_cond(Reader *reader, Map *define_map);
static void pp_if(Vector *pp_cond_stack, bool fullfilled);
static void pp_elif(Vector *pp_cond_stack, bool fullfilled, Range range);
static void pp_else(Vector *pp_cond_stack, Range range);
static void pp_endif(Vector *pp_cond_stack, Range range);
static void do_include(Reader *reader, int offset, const char *path,
                       Range range, bool include_sourcedir);
static bool try_include(Reader *reader, const char *base_path,
                        const char *rel_path);
static bool read_normal_token(Reader *reader, Map *define_map, Vector *tokens);
static bool punctuator(Reader *reader, Vector *tokens);
static bool identifier_or_keyword(Reader *reader, Map *define_map,
                                  Vector *tokens);
static bool constant(Reader *reader, Vector *tokens);
static bool integer_constant(Reader *reader, Vector *tokens);
static bool hexadecimal_constant(Reader *reader, Vector *tokens);
static bool octal_constant(Reader *reader, Vector *tokens);
static bool decimal_constant(Reader *reader, Vector *tokens);
static bool character_constant(Reader *reader, Vector *tokens);
static bool string_literal(Reader *reader, Vector *tokens);
static char c_char(Reader *reader);

Tokenizer *new_tokenizer(Reader *reader) {
  Tokenizer *tokenizer = NEW(Tokenizer);
  tokenizer->reader = reader;
  tokenizer->read_eof = false;
  tokenizer->define_map = new_map();
  tokenizer->tokens = new_vector();
  tokenizer->pp_cond_stack = new_vector();
  return tokenizer;
}

static Tokenizer *tokenizer_from_tokens(Vector *tokens) {
  Tokenizer *tokenizer = NEW(Tokenizer);
  tokenizer->tokens = tokens;
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

static void skip_to_eol(Reader *reader) {
  while (true) {
    char ch = reader_peek(reader);
    if (ch == '\n' || ch == '\0') {
      break;
    }
    reader_succ(reader);
  }
  return;
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

    if (pp_directive(tokenizer->reader, tokenizer->define_map,
                     tokenizer->pp_cond_stack)) {
      continue;
    }

    Vector *tokens = pp_cond_fullfilled(tokenizer->pp_cond_stack)
                         ? tokenizer->tokens
                         : new_vector();

    if (!read_normal_token(tokenizer->reader, tokenizer->define_map, tokens)) {
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

static Token *token_clone(Token *token) {
  Token *cloned = NEW(Token);
  *cloned = *token;
  return cloned;
}

static Token *new_token_num(Number val, Range range) {
  Token *token = new_token(TK_NUM, range);
  token->num_val = val;
  return token;
}

static Token *new_token_int_from_str(const char *str, const char *suffix,
                                     int base, Range range) {
  typedef struct Suffix {
    const char *suffix;
    type_t type;
  } Suffix;
  Suffix SUFFIX[] = {
      // unsigned long long int
      {"ull", TY_U_LLONG},
      {"llu", TY_U_LLONG},
      {"uLL", TY_U_LLONG},
      {"LLu", TY_U_LLONG},
      {"Ull", TY_U_LLONG},
      {"llU", TY_U_LLONG},
      {"ULL", TY_U_LLONG},
      {"LLU", TY_U_LLONG},
      // unsigned long int
      {"ul", TY_U_LONG},
      {"lu", TY_U_LONG},
      {"uL", TY_U_LONG},
      {"Lu", TY_U_LONG},
      {"Ul", TY_U_LONG},
      {"lU", TY_U_LONG},
      {"UL", TY_U_LONG},
      {"LU", TY_U_LONG},
      // unsigned int
      {"u", TY_U_INT},
      {"U", TY_U_INT},
      // signed long long int
      {"ll", TY_S_LLONG},
      {"LL", TY_S_LLONG},
      // signed long int
      {"l", TY_S_LONG},
      {"L", TY_S_LONG},
      // stub
      {NULL, TY_VOID},
  };

  char *endptr = NULL;
  unsigned long long val = strtoull(str, &endptr, base);
  if (*endptr != '\0') {
    range_warn(range, "オーバーフローしました");
  }
  type_t ty = TY_VOID;
  if (strcmp(suffix, "") == 0) {
    // no suffix
    if (val <= INT_MAX) {
      ty = TY_S_INT;
    } else if (base != 10 && val <= UINT_MAX) {
      ty = TY_U_INT;
    } else if (val <= LONG_MAX) {
      ty = TY_S_LONG;
    } else if (base != 10 && val <= ULONG_MAX) {
      ty = TY_U_LONG;
    } else if (val <= LLONG_MAX) {
      ty = TY_S_LLONG;
    } else {
      assert(val <= ULLONG_MAX);
      ty = TY_U_LLONG;
    }
  } else {
    for (int i = 0; SUFFIX[i].suffix != NULL; i++) {
      if (strcmp(SUFFIX[i].suffix, suffix) == 0) {
        ty = SUFFIX[i].type;
        break;
      }
    }
    if (ty == TY_VOID) {
      range_error(range, "不正な整数のサフィックスです");
    }
  }

  return new_token_num(new_number(ty, val), range);
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

static bool pp_directive(Reader *reader, Map *define_map,
                         Vector *pp_cond_stack) {
  int start = reader_get_offset(reader);
  if (!reader_consume(reader, '#')) {
    return false;
  }
  skip_space_or_comment(reader);

  String *directive = read_identifier(reader);
  if (directive == NULL) {
    skip_to_eol(reader);
    int end = reader_get_offset(reader);
    reader_expect(reader, '\n');
    range_warn(range_from_reader(reader, start, end),
               "不明なディレクティブです");
  }
  const char *directive_raw = str_get_raw(directive);

  if (strcmp(directive_raw, "if") == 0) {
    bool fullfilled = pp_read_if_cond(reader, define_map);
    reader_expect(reader, '\n');
    pp_if(pp_cond_stack, fullfilled);
    return true;
  }

  if (strcmp(directive_raw, "elif") == 0) {
    bool fullfilled = pp_read_if_cond(reader, define_map);
    int end = reader_get_offset(reader);
    reader_expect(reader, '\n');
    pp_elif(pp_cond_stack, fullfilled, range_from_reader(reader, start, end));
    return true;
  }

  if (strcmp(directive_raw, "ifdef") == 0) {
    skip_space_or_comment(reader);
    String *ident = read_identifier(reader);
    if (ident == NULL) {
      reader_error_here(reader, "識別子がありません");
    }
    skip_space_or_comment(reader);
    reader_expect(reader, '\n');

    bool defined = map_get(define_map, str_get_raw(ident)) != NULL;
    pp_if(pp_cond_stack, defined);
    return true;
  }

  if (strcmp(directive_raw, "ifndef") == 0) {
    skip_space_or_comment(reader);
    String *ident = read_identifier(reader);
    if (ident == NULL) {
      reader_error_here(reader, "識別子がありません");
    }
    skip_space_or_comment(reader);
    reader_expect(reader, '\n');

    bool defined = map_get(define_map, str_get_raw(ident)) != NULL;
    pp_if(pp_cond_stack, !defined);
    return true;
  }

  if (strcmp(directive_raw, "else") == 0) {
    int end = reader_get_offset(reader);
    skip_space_or_comment(reader);
    reader_expect(reader, '\n');

    pp_else(pp_cond_stack, range_from_reader(reader, start, end));
    return true;
  }

  if (strcmp(directive_raw, "endif") == 0) {
    int end = reader_get_offset(reader);
    skip_space_or_comment(reader);
    reader_expect(reader, '\n');

    pp_endif(pp_cond_stack, range_from_reader(reader, start, end));
    return true;
  }

  if (!pp_cond_fullfilled(pp_cond_stack)) {
    skip_to_eol(reader);
    return true;
  }

  if (strcmp(directive_raw, "include") == 0) {
    skip_space_or_comment(reader);

    if (reader_consume(reader, '<')) {
      String *str = new_string();
      while ((reader_peek(reader) != '>') && (reader_peek(reader) != '\0')) {
        char ch = reader_pop(reader);
        str_push(str, ch);
      }
      str_push(str, '\0');
      reader_expect(reader, '>');
      int end = reader_get_offset(reader);

      skip_space_or_comment(reader);
      reader_expect(reader, '\n');

      do_include(reader, start, str_get_raw(str),
                 range_from_reader(reader, start, end), false);

      return true;
    }

    if (reader_consume(reader, '"')) {
      String *str = new_string();
      while ((reader_peek(reader) != '"') && (reader_peek(reader) != '\0')) {
        char ch = reader_pop(reader);
        str_push(str, ch);
      }
      str_push(str, '\0');
      reader_expect(reader, '"');
      int end = reader_get_offset(reader);

      skip_space_or_comment(reader);
      reader_expect(reader, '\n');

      do_include(reader, start, str_get_raw(str),
                 range_from_reader(reader, start, end), true);
      return true;
    }

    reader_error_here(reader, "\"FILENAME\" または <FILENAME> がありません");
  }

  if (strcmp(directive_raw, "define") == 0) {
    skip_space_or_comment(reader);
    String *ident = read_identifier(reader);
    if (ident == NULL) {
      reader_error_here(reader, "識別子がありません");
    }
    if (reader_peek(reader) == '(') {
      reader_error_here(reader, "関数マクロは未サポートです");
    }
    skip_space_or_comment(reader);
    Vector *tokens = new_vector();
    while (reader_peek(reader) != '\n' && reader_peek(reader) != '\0') {
      if (!read_normal_token(reader, define_map, tokens)) {
        reader_error_here(reader, "トークナイズできません: `%c`",
                          reader_peek(reader));
      }
      skip_space_or_comment(reader);
    }
    reader_expect(reader, '\n');

    map_put(define_map, str_get_raw(ident), tokens);

    return true;
  }

  if (strcmp(directive_raw, "undef") == 0) {
    skip_space_or_comment(reader);
    String *ident = read_identifier(reader);
    if (ident == NULL) {
      reader_error_here(reader, "識別子がありません");
    }
    int end = reader_get_offset(reader);
    skip_space_or_comment(reader);
    reader_expect(reader, '\n');
    if (!map_remove(define_map, str_get_raw(ident))) {
      range_warn(range_from_reader(reader, start, end),
                 "未定義のマクロに対する#undefです");
    }
    return true;
  }

  if (strcmp(directive_raw, "error") == 0) {
    skip_space_or_comment(reader);

    String *str = new_string();
    while ((reader_peek(reader) != '\n') && (reader_peek(reader) != '\0')) {
      str_push(str, reader_peek(reader));
      reader_succ(reader);
    }
    str_push(str, '\0');
    int end = reader_get_offset(reader);
    reader_expect(reader, '\n');
    range_error(range_from_reader(reader, start, end), "#error %s",
                str_get_raw(str));
  }

  skip_to_eol(reader);
  int end = reader_get_offset(reader);
  reader_expect(reader, '\n');
  range_warn(range_from_reader(reader, start, end), "不明なディレクティブです");

  return true;
}

static bool pp_cond_fullfilled(Vector *pp_cond_stack) {
  if (vec_len(pp_cond_stack) > 0) {
    Cond *cond = vec_last(pp_cond_stack);
    return cond->fullfilled;
  }
  return true;
}

static bool pp_read_if_cond(Reader *reader, Map *define_map) {
  Vector *tokens = new_vector();

  enum { NORMAL, DEFINE, OPEN_PAREN, IDENT_IN_PAREN } state = NORMAL;
  Range def_start = {};

  // read (normal) tokens until next line break
  while (true) {
    skip_space_or_comment(reader);
    char ch = reader_peek(reader);
    if (ch == '\n' || ch == '\0') {
      int here_offset = reader_get_offset(reader);
      Range here = range_from_reader(reader, here_offset, here_offset);
      switch (state) {
      case NORMAL:
        break;
      case DEFINE:
      case OPEN_PAREN:
        range_error(here, "識別子がありません");
      case IDENT_IN_PAREN:
        range_error(here, "`)`がありません");
      }
      break;
    }

    // passing NULL prevents macro expansion
    if (!read_normal_token(reader, state == NORMAL ? define_map : NULL,
                           tokens)) {
      reader_error_here(reader, "トークナイズできません: `%c`",
                        reader_peek(reader));
    }
    if (vec_len(tokens) == 0) {
      continue;
    }

    Token *token = vec_last(tokens);
    switch (state) {
    case NORMAL:
      if (token->ty == TK_IDENT && strcmp(token->name, "defined") == 0) {
        state = DEFINE;
        def_start = token->range;
        vec_pop(tokens);
      }
      break;
    case DEFINE: {
      if (token->ty == '(') {
        state = OPEN_PAREN;
        vec_pop(tokens);
      } else {
        // Assume Only idents and keywords have name
        if (token->name == NULL) {
          range_error(token->range, "識別子がありません");
        }
        bool defined = map_get(define_map, token->name);
        Token *num_token = new_token_num(new_number_int(defined),
                                         range_join(def_start, token->range));
        vec_pop(tokens);
        vec_push(tokens, num_token);
        state = NORMAL;
      }
      break;
    }
    case OPEN_PAREN: {
      // Assume Only idents and keywords have name
      if (token->name == NULL) {
        range_error(token->range, "識別子がありません");
      }
      bool defined = map_get(define_map, token->name);
      Token *num_token = new_token_num(new_number_int(defined),
                                       range_join(def_start, token->range));
      vec_pop(tokens);
      vec_push(tokens, num_token);
      state = IDENT_IN_PAREN;
      break;
    }
    case IDENT_IN_PAREN: {
      // Assume Only idents and keywords have name
      if (token->ty != ')') {
        range_error(token->range, "`)`がありません");
      }
      vec_pop(tokens);
      state = NORMAL;
      break;
    }
    }
  }

  int here_offset = reader_get_offset(reader);
  Range here = range_from_reader(reader, here_offset, here_offset);

  Token *eof_token = new_token(TK_EOF, here);
  vec_push(tokens, eof_token);

  Scope *scope = new_pp_scope();
  Tokenizer *tokenizer = tokenizer_from_tokens(tokens);
  Expr *expr = constant_expression(tokenizer, scope);
  if (token_peek(tokenizer)->ty != TK_EOF) {
    range_error(token_peek(tokenizer)->range, "改行がありません");
  }
  if (expr->ty != EX_NUM) {
    range_error(expr->range, "定数式ではありません: %d", expr->ty);
  }

  int val;
  SET_NUMBER_VAL(val, &expr->num_val);
  return val != 0;
}

static void pp_if(Vector *pp_cond_stack, bool fullfilled) {
  bool current_cond_fullfilled = true;
  if (vec_len(pp_cond_stack) > 0) {
    Cond *current_cond = vec_last(pp_cond_stack);
    current_cond_fullfilled = current_cond->fullfilled;
  }

  Cond *cond = NEW(Cond);
  cond->fullfilled = current_cond_fullfilled && fullfilled;
  cond->once_fullfilled = fullfilled;
  vec_push(pp_cond_stack, cond);
}

static void pp_elif(Vector *pp_cond_stack, bool fullfilled, Range range) {
  bool outer_cond_fullfilled = true;
  if (vec_len(pp_cond_stack) > 1) {
    Cond *outer_cond = vec_rget(pp_cond_stack, 1);
    outer_cond_fullfilled = outer_cond->fullfilled;
  }

  if (vec_len(pp_cond_stack) <= 0) {
    range_error(range, "#if, #ifdef, #ifndefがありません");
  }
  Cond *cond = vec_last(pp_cond_stack);
  if (cond->once_fullfilled) {
    fullfilled = false;
  }
  cond->fullfilled = outer_cond_fullfilled && fullfilled;
  cond->once_fullfilled |= fullfilled;
}

static void pp_else(Vector *pp_cond_stack, Range range) {
  bool outer_cond_fullfilled = true;
  if (vec_len(pp_cond_stack) > 1) {
    Cond *outer_cond = vec_rget(pp_cond_stack, 1);
    outer_cond_fullfilled = outer_cond->fullfilled;
  }
  if (vec_len(pp_cond_stack) <= 0) {
    range_error(range, "#if, #ifdef, #ifndefがありません");
  }
  Cond *cond = vec_last(pp_cond_stack);
  bool fullfilled = !cond->once_fullfilled;
  cond->fullfilled = outer_cond_fullfilled && fullfilled;
  cond->once_fullfilled |= fullfilled;
}

static void pp_endif(Vector *pp_cond_stack, Range range) {
  if (vec_len(pp_cond_stack) <= 0) {
    range_error(range, "#if, #ifdef, #ifndefがありません");
  }
  vec_pop(pp_cond_stack);
}

static void do_include(Reader *reader, int offset, const char *path,
                       Range range, bool include_sourcedir) {
  if (include_sourcedir) {
    const char *sourcepath;
    reader_get_position(reader, offset, &sourcepath, NULL, NULL);
    char *sourcedir = strdup(sourcepath);
    sourcedir = dirname(sourcedir);
    if (try_include(reader, sourcedir, path)) {
      return;
    }
  }

  const char *DIRECTRIES[] = {
      GIFCC_INCLUDE,
      "/usr/local/include",
      "/usr/include",
      NULL,
  };

  for (int i = 0; DIRECTRIES[i] != NULL; i++) {
    if (try_include(reader, DIRECTRIES[i], path)) {
      return;
    }
  }

  range_error(range, "ファイルが見つかりませんでした: %s", path);
}

static bool try_include(Reader *reader, const char *base_path,
                        const char *rel_path) {
  char *abs_path = NULL;
  alloc_printf(&abs_path, "%s/%s", base_path, rel_path);

  FILE *fp = fopen(abs_path, "r");
  if (fp != NULL) {
    reader_add_file(reader, fp, rel_path);
    return true;
  }
  return false;
}

static bool read_normal_token(Reader *reader, Map *define_map, Vector *tokens) {
  return punctuator(reader, tokens) || constant(reader, tokens) ||
         string_literal(reader, tokens) ||
         identifier_or_keyword(reader, define_map, tokens);
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

static bool identifier_or_keyword(Reader *reader, Map *define_map,
                                  Vector *tokens) {
  int start = reader_get_offset(reader);
  String *str = read_identifier(reader);
  if (str == NULL) {
    return false;
  }
  int end = reader_get_offset(reader);
  Range range = range_from_reader(reader, start, end);
  char *raw_str = str_get_raw(str);

  Vector *defined = define_map != NULL ? map_get(define_map, raw_str) : NULL;
  if (defined != NULL) {
    for (int i = 0; i < vec_len(defined); i++) {
      Token *pptoken = vec_get(defined, i);
      Token *token = token_clone(pptoken);
      token->range.expanded_from = NEW(Range);
      *token->range.expanded_from = range;
      vec_push(tokens, token);
    }
  } else {
    vec_push(tokens, new_token_ident(str_get_raw(str), range));
  }

  return true;
}

static bool constant(Reader *reader, Vector *tokens) {
  return integer_constant(reader, tokens) || character_constant(reader, tokens);
}

static bool integer_constant(Reader *reader, Vector *tokens) {
  return hexadecimal_constant(reader, tokens) ||
         octal_constant(reader, tokens) || decimal_constant(reader, tokens);
}

static String *read_string_suffix(Reader *reader) {
  String *str = new_string();
  while (true) {
    char ch = reader_peek(reader);
    if ((ch < 'a' || 'z' < ch) && (ch < 'A' || 'Z' < ch)) {
      break;
    }
    str_push(str, ch);
    reader_succ(reader);
  }
  str_push(str, '\0');
  return str;
}

static bool hexadecimal_constant(Reader *reader, Vector *tokens) {
  int start = reader_get_offset(reader);

  if (!reader_consume_str(reader, "0x") && !reader_consume_str(reader, "0X")) {
    return false;
  }

  if (!is_hex_digit(reader_peek(reader))) {
    reader_error_offset(reader, start, "空の16進文字リテラルです");
  }

  String *str = new_string();
  while (true) {
    char ch = reader_peek(reader);
    if (!is_hex_digit(ch)) {
      break;
    }
    str_push(str, ch);
    reader_succ(reader);
  }
  str_push(str, '\0');

  String *suffix = read_string_suffix(reader);

  int end = reader_get_offset(reader);
  vec_push(tokens,
           new_token_int_from_str(str_get_raw(str), str_get_raw(suffix), 16,
                                  range_from_reader(reader, start, end)));

  return true;
}

static bool octal_constant(Reader *reader, Vector *tokens) {
  int start = reader_get_offset(reader);

  if (!reader_consume(reader, '0')) {
    return false;
  }

  String *str = new_string();
  while (true) {
    char ch = reader_peek(reader);
    if (!is_oct_digit(ch)) {
      break;
    }
    str_push(str, ch);
    reader_succ(reader);
  }
  str_push(str, '\0');

  String *suffix = read_string_suffix(reader);

  int end = reader_get_offset(reader);
  vec_push(tokens,
           new_token_int_from_str(str_get_raw(str), str_get_raw(suffix), 8,
                                  range_from_reader(reader, start, end)));
  return true;
}

static bool decimal_constant(Reader *reader, Vector *tokens) {
  int start = reader_get_offset(reader);

  if (!isdigit(reader_peek(reader))) {
    return false;
  }

  String *str = new_string();
  while (true) {
    char ch = reader_peek(reader);
    if (!is_dec_digit(ch)) {
      break;
    }
    str_push(str, ch);
    reader_succ(reader);
  }
  str_push(str, '\0');

  String *suffix = read_string_suffix(reader);

  int end = reader_get_offset(reader);
  vec_push(tokens,
           new_token_int_from_str(str_get_raw(str), str_get_raw(suffix), 10,
                                  range_from_reader(reader, start, end)));
  return true;
}

static bool character_constant(Reader *reader, Vector *tokens) {
  int start = reader_get_offset(reader);
  bool is_wide;
  if (reader_consume_str(reader, "L\'")) {
    is_wide = true;
  } else if (reader_consume(reader, '\'')) {
    is_wide = false;
  } else {
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
  if (is_wide) {
    vec_push(tokens, new_token_num(new_number_wchar_t(ch),
                                   range_from_reader(reader, start, end)));
  } else {
    vec_push(tokens, new_token_num(new_number_int(ch),
                                   range_from_reader(reader, start, end)));
  }
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
