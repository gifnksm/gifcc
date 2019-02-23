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

typedef enum {
  MACRO_OBJ,
  MACRO_FUNC,
} macro_t;

typedef struct Macro {
  macro_t kind;
  Vector *params;
  Vector *replacement;
} Macro;

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
    {"...", TK_ELIPSIS},   {"##", TK_HASH_HASH},  {NULL, '\0'},
};
static const char *SHORT_PUNCT_TOKENS = "=!<>&|^+-*/%();?:~{}[],.#";

static bool read_token(Tokenizer *tokenizer, Token **token, bool skip_eol,
                       bool check_pp_cond);
static Token *new_token(int ty, Range range);
static Token *token_clone(Token *token);
static Token *new_token_num(Number num, Range range);
static Token *new_token_int_from_str(const char *str, const char *suffix,
                                     int base, Range range);
static Token *new_token_ident(char *ident, Range range);
static Token *new_token_str(char *str, Range range);
static Macro *new_macro(Vector *params, Vector *replacement);
static bool pp_directive(Tokenizer *tokenizer);
static bool pp_cond_fullfilled(Vector *pp_cond_stack);
static bool pp_read_if_cond(Tokenizer *tokenizer);
static Vector *pp_convert_defined(Map *define_map, Vector *tokens);
static Vector *pp_expand_macros(Map *define_map, Vector *tokens);
static void pp_if(Vector *pp_cond_stack, bool fullfilled);
static void pp_elif(Vector *pp_cond_stack, bool fullfilled, Range range);
static void pp_else(Vector *pp_cond_stack, Range range);
static void pp_endif(Vector *pp_cond_stack, Range range);
static void do_include(Reader *reader, int offset, const char *path,
                       Range range, bool include_sourcedir);
static bool try_include(Reader *reader, const char *base_path,
                        const char *rel_path);
static Token *read_normal_token(Reader *reader);
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
  Tokenizer *tokenizer = NEW(Tokenizer);
  tokenizer->reader = reader;
  tokenizer->define_map = new_map();
  tokenizer->tokens = new_vector();
  tokenizer->pp_cond_stack = new_vector();
  return tokenizer;
}

static Tokenizer *tokenizer_from_tokens(Map *define_map, Vector *tokens) {
  Tokenizer *tokenizer = NEW(Tokenizer);
  tokenizer->define_map = define_map;
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
    while (true) {
      Token *token = NULL;
      if (!read_token(tokenizer, &token, true, true)) {
        return NULL;
      }
      if (token == NULL) {
        continue;
      }

      Vector *tokens = new_vector();
      vec_push(tokens, token);
      if (token->ident != NULL) {
        // read function macro arguments
        Macro *def = map_get(tokenizer->define_map, token->ident);
        if (def != NULL && def->kind == MACRO_FUNC) {
          int nest = 0;
          do {
            if (!read_token(tokenizer, &token, true, true)) {
              break;
            }
            if (token->ty == '(') {
              nest++;
            }
            if (token->ty == ')') {
              nest--;
            }
            vec_push(tokens, token);
          } while (nest > 0);
        }
      }

      // expand macros
      tokens = pp_expand_macros(tokenizer->define_map, tokens);

      // concatenate adjacent string literal
      Token *last = NULL;
      while (vec_len(tokens) > 0) {
        Token *next = vec_remove(tokens, 0);
        if (vec_len(tokenizer->tokens) > 0) {
          last = vec_last(tokenizer->tokens);
        }
        if (last == NULL || last->ty != TK_STR || next->ty != TK_STR) {
          vec_push(tokenizer->tokens, next);
          last = next;
          continue;
        }

        size_t size = strlen(last->str) + 1;
        size_t extra_size = strlen(next->str);
        if (extra_size > 0) {
          last->str = realloc(last->str, size + extra_size);
          strncat(last->str, next->str, extra_size + 1);
        }
        last->range = range_join(last->range, next->range);
      }

      if (last == NULL || last->ty != TK_STR) {
        break;
      }
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

static bool read_token(Tokenizer *tokenizer, Token **token, bool skip_eol,
                       bool check_pp_cond) {
  char ch;
  while ((ch = reader_peek(tokenizer->reader)) != '\0') {
    if (skip_space_or_comment(tokenizer->reader)) {
      continue;
    }
    if (ch == '\n') {
      if (skip_eol) {
        reader_succ(tokenizer->reader);
        continue;
      }
      *token = NULL;
      return false;
    }

    if (pp_directive(tokenizer)) {
      continue;
    }

    Token *read_token = read_normal_token(tokenizer->reader);
    if (read_token == NULL) {
      reader_error_here(tokenizer->reader, "トークナイズできません: `%c`",
                        reader_peek(tokenizer->reader));
    }
    if (!check_pp_cond || pp_cond_fullfilled(tokenizer->pp_cond_stack)) {
      *token = read_token;
    } else {
      *token = NULL;
    }

    return true;
  }

  int start = reader_get_offset(tokenizer->reader);
  int end = reader_get_offset(tokenizer->reader);
  *token = new_token(TK_EOF, range_from_reader(tokenizer->reader, start, end));
  return true;
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

static Token *new_token_num(Number num, Range range) {
  Token *token = new_token(TK_NUM, range);
  token->num = num;
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

static Token *new_token_ident(char *ident, Range range) {
  Token *token = new_token(TK_IDENT, range);

  for (int i = 0; LONG_IDENT_TOKENS[i].str != NULL; i++) {
    const LongToken *tk = &LONG_IDENT_TOKENS[i];
    if (strcmp(ident, tk->str) == 0) {
      token->ty = tk->kind;
      break;
    }
  }
  token->ident = ident;
  return token;
}

static Token *new_token_str(char *str, Range range) {
  Token *token = new_token(TK_STR, range);
  token->str = str;
  return token;
}

static Macro *new_macro(Vector *params, Vector *replacement) {
  Macro *macro = NEW(Macro);
  if (params != NULL) {
    macro->kind = MACRO_FUNC;
    macro->params = params;
  } else {
    macro->kind = MACRO_OBJ;
  }
  macro->replacement = replacement;
  return macro;
}

static bool pp_directive(Tokenizer *tokenizer) {
  int start = reader_get_offset(tokenizer->reader);
  if (!reader_consume(tokenizer->reader, '#')) {
    return false;
  }
  skip_space_or_comment(tokenizer->reader);

  String *directive = read_identifier(tokenizer->reader);
  if (directive == NULL) {
    skip_to_eol(tokenizer->reader);
    int end = reader_get_offset(tokenizer->reader);
    reader_expect(tokenizer->reader, '\n');
    range_warn(range_from_reader(tokenizer->reader, start, end),
               "不明なディレクティブです");
  }
  const char *directive_raw = str_get_raw(directive);

  if (strcmp(directive_raw, "if") == 0) {
    bool fullfilled = pp_read_if_cond(tokenizer);
    reader_expect(tokenizer->reader, '\n');
    pp_if(tokenizer->pp_cond_stack, fullfilled);
    return true;
  }

  if (strcmp(directive_raw, "elif") == 0) {
    bool fullfilled = pp_read_if_cond(tokenizer);
    int end = reader_get_offset(tokenizer->reader);
    reader_expect(tokenizer->reader, '\n');
    pp_elif(tokenizer->pp_cond_stack, fullfilled,
            range_from_reader(tokenizer->reader, start, end));
    return true;
  }

  if (strcmp(directive_raw, "ifdef") == 0) {
    skip_space_or_comment(tokenizer->reader);
    String *ident = read_identifier(tokenizer->reader);
    if (ident == NULL) {
      reader_error_here(tokenizer->reader, "識別子がありません");
    }
    skip_space_or_comment(tokenizer->reader);
    reader_expect(tokenizer->reader, '\n');

    bool defined = map_get(tokenizer->define_map, str_get_raw(ident)) != NULL;
    pp_if(tokenizer->pp_cond_stack, defined);
    return true;
  }

  if (strcmp(directive_raw, "ifndef") == 0) {
    skip_space_or_comment(tokenizer->reader);
    String *ident = read_identifier(tokenizer->reader);
    if (ident == NULL) {
      reader_error_here(tokenizer->reader, "識別子がありません");
    }
    skip_space_or_comment(tokenizer->reader);
    reader_expect(tokenizer->reader, '\n');

    bool defined = map_get(tokenizer->define_map, str_get_raw(ident)) != NULL;
    pp_if(tokenizer->pp_cond_stack, !defined);
    return true;
  }

  if (strcmp(directive_raw, "else") == 0) {
    int end = reader_get_offset(tokenizer->reader);
    skip_space_or_comment(tokenizer->reader);
    reader_expect(tokenizer->reader, '\n');

    pp_else(tokenizer->pp_cond_stack,
            range_from_reader(tokenizer->reader, start, end));
    return true;
  }

  if (strcmp(directive_raw, "endif") == 0) {
    int end = reader_get_offset(tokenizer->reader);
    skip_space_or_comment(tokenizer->reader);
    reader_expect(tokenizer->reader, '\n');

    pp_endif(tokenizer->pp_cond_stack,
             range_from_reader(tokenizer->reader, start, end));
    return true;
  }

  if (!pp_cond_fullfilled(tokenizer->pp_cond_stack)) {
    skip_to_eol(tokenizer->reader);
    return true;
  }

  if (strcmp(directive_raw, "include") == 0) {
    skip_space_or_comment(tokenizer->reader);

    if (reader_consume(tokenizer->reader, '<')) {
      String *str = new_string();
      while ((reader_peek(tokenizer->reader) != '>') &&
             (reader_peek(tokenizer->reader) != '\0')) {
        char ch = reader_pop(tokenizer->reader);
        str_push(str, ch);
      }
      str_push(str, '\0');
      reader_expect(tokenizer->reader, '>');
      int end = reader_get_offset(tokenizer->reader);

      skip_space_or_comment(tokenizer->reader);
      reader_expect(tokenizer->reader, '\n');

      do_include(tokenizer->reader, start, str_get_raw(str),
                 range_from_reader(tokenizer->reader, start, end), false);

      return true;
    }

    if (reader_consume(tokenizer->reader, '"')) {
      String *str = new_string();
      while ((reader_peek(tokenizer->reader) != '"') &&
             (reader_peek(tokenizer->reader) != '\0')) {
        char ch = reader_pop(tokenizer->reader);
        str_push(str, ch);
      }
      str_push(str, '\0');
      reader_expect(tokenizer->reader, '"');
      int end = reader_get_offset(tokenizer->reader);

      skip_space_or_comment(tokenizer->reader);
      reader_expect(tokenizer->reader, '\n');

      do_include(tokenizer->reader, start, str_get_raw(str),
                 range_from_reader(tokenizer->reader, start, end), true);
      return true;
    }

    reader_error_here(tokenizer->reader,
                      "\"FILENAME\" または <FILENAME> がありません");
  }

  if (strcmp(directive_raw, "define") == 0) {
    skip_space_or_comment(tokenizer->reader);
    String *ident = read_identifier(tokenizer->reader);
    if (ident == NULL) {
      reader_error_here(tokenizer->reader, "識別子がありません");
    }
    Vector *params = NULL;
    if (reader_consume(tokenizer->reader, '(')) {
      params = new_vector();
      while (reader_peek(tokenizer->reader) != ')') {
        skip_space_or_comment(tokenizer->reader);
        if (reader_consume_str(tokenizer->reader, "...")) {
          vec_push(params, "__VA_ARGS__");
          break;
        }
        String *ident = read_identifier(tokenizer->reader);
        if (ident == NULL) {
          reader_error_here(tokenizer->reader, "識別子がありません");
        }
        vec_push(params, str_get_raw(ident));
        skip_space_or_comment(tokenizer->reader);
        if (!reader_consume(tokenizer->reader, ',')) {
          break;
        }
      }
      skip_space_or_comment(tokenizer->reader);
      reader_expect(tokenizer->reader, ')');
    }

    Vector *tokens = new_vector();
    while (true) {
      Token *token = NULL;
      if (!read_token(tokenizer, &token, false, true)) {
        char ch = reader_peek(tokenizer->reader);
        if (ch == '\n' || ch == '\0') {
          break;
        }
        reader_error_here(tokenizer->reader, "トークナイズできません: `%c`",
                          reader_peek(tokenizer->reader));
      }
      vec_push(tokens, token);
    }
    reader_expect(tokenizer->reader, '\n');

    map_put(tokenizer->define_map, str_get_raw(ident),
            new_macro(params, tokens));

    return true;
  }

  if (strcmp(directive_raw, "undef") == 0) {
    skip_space_or_comment(tokenizer->reader);
    String *ident = read_identifier(tokenizer->reader);
    if (ident == NULL) {
      reader_error_here(tokenizer->reader, "識別子がありません");
    }
    skip_space_or_comment(tokenizer->reader);
    reader_expect(tokenizer->reader, '\n');
    map_remove(tokenizer->define_map, str_get_raw(ident));
    return true;
  }

  if (strcmp(directive_raw, "error") == 0) {
    skip_space_or_comment(tokenizer->reader);

    String *str = new_string();
    while ((reader_peek(tokenizer->reader) != '\n') &&
           (reader_peek(tokenizer->reader) != '\0')) {
      str_push(str, reader_peek(tokenizer->reader));
      reader_succ(tokenizer->reader);
    }
    str_push(str, '\0');
    int end = reader_get_offset(tokenizer->reader);
    reader_expect(tokenizer->reader, '\n');
    range_error(range_from_reader(tokenizer->reader, start, end), "#error %s",
                str_get_raw(str));
  }

  skip_to_eol(tokenizer->reader);
  int end = reader_get_offset(tokenizer->reader);
  reader_expect(tokenizer->reader, '\n');
  range_warn(range_from_reader(tokenizer->reader, start, end),
             "不明なディレクティブです");

  return true;
}

static bool pp_cond_fullfilled(Vector *pp_cond_stack) {
  if (vec_len(pp_cond_stack) > 0) {
    Cond *cond = vec_last(pp_cond_stack);
    return cond->fullfilled;
  }
  return true;
}

static bool pp_read_if_cond(Tokenizer *tokenizer) {
  Vector *tokens = new_vector();

  // read tokens until next line break
  while (true) {
    Token *token = NULL;
    if (!read_token(tokenizer, &token, false, false)) {
      char ch = reader_peek(tokenizer->reader);
      if (ch == '\n' || ch == '\0') {
        break;
      }
      reader_error_here(tokenizer->reader, "トークナイズできません: `%c`",
                        reader_peek(tokenizer->reader));
    }
    if (token != NULL) {
      vec_push(tokens, token);
    }
  }
  int here_offset = reader_get_offset(tokenizer->reader);
  Range here = range_from_reader(tokenizer->reader, here_offset, here_offset);
  Token *eof_token = new_token(TK_EOF, here);
  vec_push(tokens, eof_token);

  // convert `defined(IDENT)` or `defined INDET` into 0 or 1
  tokens = pp_convert_defined(tokenizer->define_map, tokens);

  // expand macros
  tokens = pp_expand_macros(tokenizer->define_map, tokens);

  for (int i = 0; i < vec_len(tokens); i++) {
    // replace all ident tokens (including keyword ident) into '0'
    Token *tk = vec_get(tokens, i);
    if (tk->ident != NULL) {
      // Assume Only idents and keywords have non-null `ident`
      tk->ident = NULL;
      tk->ty = TK_NUM;
      tk->num = new_number_int(0);
    }
  }

  Scope *scope = new_pp_scope();
  Tokenizer *sub_tokenizer = tokenizer_from_tokens(NULL, tokens);
  Expr *expr = constant_expression(sub_tokenizer, scope);
  if (token_peek(sub_tokenizer)->ty != TK_EOF) {
    range_error(token_peek(sub_tokenizer)->range, "改行がありません");
  }

  assert(expr->ty == EX_NUM);
  int val;
  SET_NUMBER_VAL(val, &expr->num);
  return val != 0;
}

static Vector *pp_convert_defined(Map *define_map, Vector *tokens) {
  Vector *converted = new_vector();
  while (vec_len(tokens) > 0) {
    Token *token = vec_remove(tokens, 0);
    if (token->ty != TK_IDENT || strcmp(token->ident, "defined") != 0) {
      vec_push(converted, token);
      continue;
    }
    Range def_start = token->range;

    bool has_paren = false;
    token = vec_remove(tokens, 0);
    if (token->ty == '(') {
      has_paren = true;
      token = vec_remove(tokens, 0);
    }

    // Assume Only idents and keywords have non-null `ident`
    if (token->ident == NULL) {
      range_error(token->range, "識別子がありません");
    }
    bool defined = map_get(define_map, token->ident) != NULL;
    Token *num_token = new_token_num(new_number_int(defined),
                                     range_join(def_start, token->range));
    if (has_paren) {
      token = vec_remove(tokens, 0);
      if (token->ty != ')') {
        range_error(token->range, "`)` がありません");
      }
    }
    vec_push(converted, num_token);
  }
  return converted;
}

static Vector *pp_expand_macros(Map *define_map, Vector *tokens) {
  Vector *expanded = new_vector();
  while (vec_len(tokens) > 0) {
    Token *token = vec_remove(tokens, 0);
    Macro *defined =
        token->ident != NULL ? map_get(define_map, token->ident) : NULL;
    if (defined != NULL) {
      for (int i = 0; i < vec_len(defined->replacement); i++) {
        Token *pptoken = vec_get(defined->replacement, i);
        Token *ex_token = token_clone(pptoken);
        ex_token->range.expanded_from = NEW(Range);
        *ex_token->range.expanded_from = token->range;
        vec_push(expanded, ex_token);
      }
    } else {
      vec_push(expanded, token);
    }
  }
  return expanded;
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

static Token *read_normal_token(Reader *reader) {
  Token *token;
  if ((token = punctuator(reader)) != NULL ||
      (token = constant(reader)) != NULL ||
      (token = string_literal(reader)) != NULL ||
      (token = identifier_or_keyword(reader)) != NULL) {
    return token;
  }
  return NULL;
}

static Token *punctuator(Reader *reader) {
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

  return NULL;

Hit:;
  int end = reader_get_offset(reader);
  return new_token(kind, range_from_reader(reader, start, end));
}

static Token *identifier_or_keyword(Reader *reader) {
  int start = reader_get_offset(reader);
  String *str = read_identifier(reader);
  if (str == NULL) {
    return NULL;
  }
  int end = reader_get_offset(reader);
  Range range = range_from_reader(reader, start, end);
  return new_token_ident(str_get_raw(str), range);
}

static Token *constant(Reader *reader) {
  Token *token;
  if ((token = integer_constant(reader)) != NULL ||
      (token = character_constant(reader)) != NULL) {
    return token;
  }
  return NULL;
}

static Token *integer_constant(Reader *reader) {
  Token *token;
  if ((token = hexadecimal_constant(reader)) != NULL ||
      (token = octal_constant(reader)) != NULL ||
      (token = decimal_constant(reader)) != NULL) {
    return token;
  }
  return NULL;
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

static Token *hexadecimal_constant(Reader *reader) {
  int start = reader_get_offset(reader);

  if (!reader_consume_str(reader, "0x") && !reader_consume_str(reader, "0X")) {
    return NULL;
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
  return new_token_int_from_str(str_get_raw(str), str_get_raw(suffix), 16,
                                range_from_reader(reader, start, end));
}

static Token *octal_constant(Reader *reader) {
  int start = reader_get_offset(reader);

  if (!reader_consume(reader, '0')) {
    return NULL;
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
  return new_token_int_from_str(str_get_raw(str), str_get_raw(suffix), 8,
                                range_from_reader(reader, start, end));
}

static Token *decimal_constant(Reader *reader) {
  int start = reader_get_offset(reader);

  if (!isdigit(reader_peek(reader))) {
    return NULL;
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
  return new_token_int_from_str(str_get_raw(str), str_get_raw(suffix), 10,
                                range_from_reader(reader, start, end));
}

static Token *character_constant(Reader *reader) {
  int start = reader_get_offset(reader);
  bool is_wide;
  if (reader_consume_str(reader, "L\'")) {
    is_wide = true;
  } else if (reader_consume(reader, '\'')) {
    is_wide = false;
  } else {
    return NULL;
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
    return new_token_num(new_number_wchar_t(ch),
                         range_from_reader(reader, start, end));
  }
  return new_token_num(new_number_int(ch),
                       range_from_reader(reader, start, end));
}

static Token *string_literal(Reader *reader) {
  int start = reader_get_offset(reader);
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
  int end = reader_get_offset(reader);
  return new_token_str(str_get_raw(str), range_from_reader(reader, start, end));
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
