#include "gifcc.h"
#include <assert.h>
#include <ctype.h>
#include <libgen.h>
#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

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
  MACRO_OBJ_SPECIAL,
} macro_t;

typedef struct Macro {
  macro_t kind;
  Vector *params;
  bool has_varargs;
  Vector *replacement;
  Vector *(*replacement_func)(Tokenizer *);
} Macro;

static const LongToken LONG_IDENT_TOKENS[] = {
    {"void", TK_VOID},
    {"int", TK_INT},
    {"short", TK_SHORT},
    {"long", TK_LONG},
    {"char", TK_CHAR},
    {"signed", TK_SIGNED},
    {"unsigned", TK_UNSIGNED},
    {"_Bool", TK_BOOL},
    {"if", TK_IF},
    {"else", TK_ELSE},
    {"switch", TK_SWITCH},
    {"case", TK_CASE},
    {"default", TK_DEFAULT},
    {"while", TK_WHILE},
    {"do", TK_DO},
    {"for", TK_FOR},
    {"goto", TK_GOTO},
    {"break", TK_BREAK},
    {"continue", TK_CONTINUE},
    {"return", TK_RETURN},
    {"struct", TK_STRUCT},
    {"union", TK_UNION},
    {"enum", TK_ENUM},
    {"sizeof", TK_SIZEOF},
    {"typedef", TK_TYPEDEF},
    {"extern", TK_EXTERN},
    {"static", TK_STATIC},
    {"const", TK_CONST},
    {"restrict", TK_RESTRICT},
    {"volatile", TK_VOLATILE},
    {NULL, '\0'},
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

static bool do_read_token(Tokenizer *tokenizer, Token **token, bool skip_eol,
                          bool check_pp_cond);
static Token *new_token(int ty, Range range);
static Token *token_clone(Token *token);
static Token *new_token_num(const char *num, Range range);
static Token *new_token_char(Number val, Range range);
static Token *new_token_ident(char *ident, Range range);
static Token *new_token_str(const char *str, Range range);
static Macro *new_obj_macro(Vector *replacement);
static Macro *new_obj_special_macro(Vector *(*replacement_func)(Tokenizer *));
static Macro *new_func_macro(Vector *params, bool has_varargs,
                             Vector *replacement);
static bool pp_directive(Tokenizer *tokenizer);
static bool pp_cond_fullfilled(Vector *pp_cond_stack);
static bool pp_read_if_cond(Tokenizer *tokenizer);
static Vector *pp_convert_defined(Map *define_map, Vector *tokens);
static Vector *pp_expand_macros(Tokenizer *tokenizer, Vector *tokens,
                                bool (*reader)(Tokenizer *, Token **));
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
static Token *character_constant(Reader *reader);
static Token *string_literal(Reader *reader);
static char c_char(Reader *reader);

static void set_predefined_num_macro(const Reader *reader, Map *map, char *name,
                                     const char *num) {
  Vector *replacement = new_vector();
  vec_push(replacement, new_token_num(num, range_builtin(reader)));
  map_put(map, name, new_obj_macro(replacement));
}

static void
set_predefined_special_macro(Map *map, char *name,
                             Vector *(*replacement_func)(Tokenizer *)) {
  map_put(map, name, new_obj_special_macro(replacement_func));
}

static Vector *macro_date(Tokenizer *tokenizer __attribute__((unused))) {
  time_t now = time(NULL);
  struct tm now_tm;
  char buf[30];
  localtime_r(&now, &now_tm);
  strftime(buf, sizeof(buf), "%b %e %Y", &now_tm);

  Vector *rep = new_vector();
  vec_push(rep, new_token_str(strdup(buf), range_builtin(tokenizer->reader)));
  return rep;
}

static Vector *macro_time(Tokenizer *tokenizer __attribute__((unused))) {
  time_t now = time(NULL);
  struct tm now_tm;
  char buf[30];
  localtime_r(&now, &now_tm);
  strftime(buf, sizeof(buf), "%T", &now_tm);

  Vector *rep = new_vector();
  vec_push(rep, new_token_str(strdup(buf), range_builtin(tokenizer->reader)));
  return rep;
}

static Vector *macro_file(Tokenizer *tokenizer) {
  const char *filename;
  int offset = reader_get_offset(tokenizer->reader);
  reader_get_position(tokenizer->reader, offset, &filename, NULL, NULL);

  Vector *rep = new_vector();
  vec_push(rep, new_token_str(filename, range_builtin(tokenizer->reader)));
  return rep;
}

static Vector *macro_line(Tokenizer *tokenizer) {
  int line;
  int offset = reader_get_offset(tokenizer->reader);
  reader_get_position(tokenizer->reader, offset, NULL, &line, NULL);

  char *buf;
  alloc_printf(&buf, "%d", line);

  Vector *rep = new_vector();
  vec_push(rep, new_token_num(buf, range_builtin(tokenizer->reader)));
  return rep;
}

static void initialize_predefined_macro(const Reader *reader, Map *map) {
  set_predefined_special_macro(map, "__DATE__", macro_date);
  set_predefined_special_macro(map, "__TIME__", macro_time);
  set_predefined_special_macro(map, "__FILE__", macro_file);
  set_predefined_special_macro(map, "__LINE__", macro_line);

  set_predefined_num_macro(reader, map, "__STDC__", "1");
  set_predefined_num_macro(reader, map, "__STDC_HOSTED__", "1");
  set_predefined_num_macro(reader, map, "__STDC_VERSION__", "201112L");
  set_predefined_num_macro(reader, map, "__LP64__", "1");
  set_predefined_num_macro(reader, map, "__x86_64__", "1");
}

Tokenizer *new_tokenizer(Reader *reader) {
  Tokenizer *tokenizer = NEW(Tokenizer);
  tokenizer->reader = reader;
  tokenizer->define_map = new_map();
  tokenizer->tokens = new_vector();
  tokenizer->pp_cond_stack = new_vector();

  initialize_predefined_macro(reader, tokenizer->define_map);

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

static bool read_token(Tokenizer *tokenizer, Token **token) {
  return do_read_token(tokenizer, token, true, true);
}

Token *token_peek_ahead(Tokenizer *tokenizer, int n) {
  while (vec_len(tokenizer->tokens) <= n) {
    while (true) {
      Token *token = NULL;
      if (!read_token(tokenizer, &token)) {
        return NULL;
      }
      if (token == NULL) {
        continue;
      }

      // expand macros
      Vector *tokens = new_vector();
      vec_push(tokens, token);
      tokens = pp_expand_macros(tokenizer, tokens, read_token);

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

        char *buf;
        alloc_printf(&buf, "%s%s", last->str, next->str);
        last->str = buf;
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
  case TK_CHARCONST:
    return "CHARCONST";
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

static const char *pp_token_to_str(const Token *token) {
  if (token->ty <= 255) {
    char *name;
    alloc_printf(&name, "%c", token->ty);
    return name;
  }
  if (token->ident != NULL) {
    return token->ident;
  }
  if (token->ty == TK_NUM) {
    return token->str;
  }
  for (int i = 0; LONG_PUNCT_TOKENS[i].str != NULL; i++) {
    if (LONG_PUNCT_TOKENS[i].kind == token->ty) {
      return LONG_PUNCT_TOKENS[i].str;
    }
  }
  range_error(token->range, "結合できないトークンです");
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

static bool do_read_token(Tokenizer *tokenizer, Token **token, bool skip_eol,
                          bool check_pp_cond) {
  char ch;
  while ((ch = reader_peek(tokenizer->reader)) != '\0') {
    bool is_sol = reader_is_sol(tokenizer->reader);
    if (skip_space_or_comment(tokenizer->reader)) {
      ch = reader_peek(tokenizer->reader);
      if (ch == '\0') {
        break;
      }
    }
    if (ch == '\n') {
      if (skip_eol) {
        reader_succ(tokenizer->reader);
        continue;
      }
      *token = NULL;
      return false;
    }

    if (is_sol && pp_directive(tokenizer)) {
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
  if (token->pp_hideset != NULL) {
    cloned->pp_hideset = new_map();
    for (int i = 0; i < map_size(token->pp_hideset); i++) {
      char *key = map_get_by_index(token->pp_hideset, i, NULL);
      map_put(cloned->pp_hideset, key, key);
    }
  }
  return cloned;
}

static Token *new_token_num(const char *num, Range range) {
  Token *token = new_token(TK_NUM, range);
  token->num = num;
  return token;
}

static Token *new_token_char(Number val, Range range) {
  Token *token = new_token(TK_CHARCONST, range);
  token->char_val = val;
  return token;
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

static Token *new_token_str(const char *str, Range range) {
  Token *token = new_token(TK_STR, range);
  token->str = str;
  return token;
}

static Macro *new_obj_macro(Vector *replacement) {
  Macro *macro = NEW(Macro);
  macro->kind = MACRO_OBJ;
  macro->replacement = replacement;
  return macro;
}

static Macro *new_obj_special_macro(Vector *(*replacement_func)(Tokenizer *)) {
  Macro *macro = NEW(Macro);
  macro->kind = MACRO_OBJ_SPECIAL;
  macro->replacement_func = replacement_func;
  return macro;
}

static Macro *new_func_macro(Vector *params, bool has_varargs,
                             Vector *replacement) {
  Macro *macro = NEW(Macro);
  macro->kind = MACRO_FUNC;
  macro->params = params;
  macro->has_varargs = has_varargs;
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
    bool has_varargs = false;
    Vector *params = NULL;
    if (reader_consume(tokenizer->reader, '(')) {
      params = new_vector();
      while (reader_peek(tokenizer->reader) != ')') {
        skip_space_or_comment(tokenizer->reader);
        if (reader_consume_str(tokenizer->reader, "...")) {
          has_varargs = true;
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
      if (!do_read_token(tokenizer, &token, false, true)) {
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

    Macro *macro;
    if (params != NULL) {
      macro = new_func_macro(params, has_varargs, tokens);
    } else {
      macro = new_obj_macro(tokens);
    }
    map_put(tokenizer->define_map, str_get_raw(ident), macro);

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
    if (!do_read_token(tokenizer, &token, false, false)) {
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
  tokens = pp_expand_macros(tokenizer, tokens, NULL);

  for (int i = 0; i < vec_len(tokens); i++) {
    // replace all ident tokens (including keyword ident) into '0'
    Token *tk = vec_get(tokens, i);
    if (tk->ident != NULL) {
      // Assume Only idents and keywords have non-null `ident`
      tk->ident = NULL;
      tk->ty = TK_NUM;
      tk->num = strdup("0");
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
    char *num;
    alloc_printf(&num, "%d", defined);
    Token *num_token = new_token_num(num, range_join(def_start, token->range));
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

static bool pp_is_token_in_hideset(Token *token) {
  if (token->ident == NULL) {
    return false;
  }
  if (token->pp_hideset == NULL) {
    return false;
  }
  return map_get(token->pp_hideset, token->ident) != NULL;
}

static Map *pp_hideset_intersection(Token *a, Token *b) {
  Map *map = new_map();
  if (a->pp_hideset == NULL || b->pp_hideset == NULL) {
    return map;
  }
  for (int i = 0; i < map_size(a->pp_hideset); i++) {
    char *key = map_get_by_index(a->pp_hideset, i, NULL);
    if (map_get(b->pp_hideset, key) != NULL) {
      map_put(map, key, key);
    }
  }
  return map;
}

static Vector *pp_read_macro_func_arg(Tokenizer *tokenizer, Macro *macro,
                                      Vector *tokens, Token **rparen,
                                      bool (*reader)(Tokenizer *, Token **)) {
  assert(macro->kind == MACRO_FUNC);
  assert(((Token *)vec_first(tokens))->ty == '(');

  Range range = ((Token *)vec_first(tokens))->range;

  Vector *arguments = new_vector();
  Vector *current_arg = NULL;
  int nest = 0;
  do {
    Token *token;
    if (vec_len(tokens) == 0 && reader != NULL) {
      reader(tokenizer, &token);
    } else {
      token = vec_remove(tokens, 0);
    }
    range = range_join(range, token->range);
    if (token->ty == TK_EOF) {
      range_error(token->range, "`)` がありません");
    }
    if (token->ty == '(') {
      nest++;
      if (nest == 1) {
        continue;
      }
    }
    if (token->ty == ')') {
      nest--;
      if (nest == 0) {
        *rparen = token;
        break;
      }
    }

    assert(nest > 0);
    if (nest == 1 && token->ty == ',' &&
        (!macro->has_varargs ||
         vec_len(arguments) < vec_len(macro->params) - 1)) {
      if (current_arg == NULL) {
        current_arg = new_vector();
      }
      vec_push(arguments, current_arg);
      current_arg = new_vector();
      continue;
    }

    if (current_arg == NULL) {
      current_arg = new_vector();
    }
    vec_push(current_arg, token);
  } while (nest > 0);

  if (current_arg != NULL) {
    vec_push(arguments, current_arg);
  }
  if (macro->has_varargs && vec_len(arguments) == vec_len(macro->params) - 1) {
    vec_push(arguments, new_vector());
  }
  if (vec_len(arguments) == 0 && vec_len(macro->params) == 1) {
    vec_push(arguments, new_vector());
  }

  if (vec_len(macro->params) != vec_len(arguments)) {
    range_error(range,
                "関数マクロの引数の個数が一致しません, 仮引数: %d, 引数: %d",
                vec_len(macro->params), vec_len(arguments));
  }

  return arguments;
}

static Vector *pp_hsadd(Range *expanded_from, Map *hideset, Vector *output) {
  for (int i = 0; i < vec_len(output); i++) {
    Token *token = token_clone(vec_get(output, i));
    vec_set(output, i, token);
    Range *range = &token->range;
    while (range->expanded_from != NULL &&
           range->expanded_from != expanded_from) {
      range = range->expanded_from;
    }
    range->expanded_from = expanded_from;

    for (int j = 0; j < map_size(hideset); j++) {
      char *key = map_get_by_index(hideset, j, NULL);
      if (token->pp_hideset == NULL) {
        token->pp_hideset = new_map();
      }
      map_put(token->pp_hideset, key, key);
    }
  }
  return output;
}

static Vector *pp_get_func_arg(Vector *params, Vector *arguments,
                               Token *token) {
  if (token->ident == NULL || params == NULL) {
    return NULL;
  }
  int i;
  for (i = 0; i < vec_len(params); i++) {
    char *key = vec_get(params, i);
    if (strcmp(key, token->ident) == 0) {
      Vector *arg = vec_get(arguments, i);
      assert(arg != NULL);
      return arg;
    }
  }
  return NULL;
}

static Token *pp_stringize(Vector *arg, Range range) {
  String *str = new_string();
  for (int i = 0; i < vec_len(arg); i++) {
    Token *token = vec_get(arg, i);
    if (i == 0) {
      range = token->range;
    } else {
      str_push(str, ' ');
      range = range_join(range, token->range);
    }
    if (token->ty < 256) {
      str_push(str, token->ty);
      continue;
    }
    switch (token->ty) {
    case TK_NUM:
      str_append(str, token->num);
      break;
    case TK_IDENT:
      str_append(str, token->ident);
      break;
    case TK_STR:
    case TK_CHARCONST:
      str_append(str, reader_get_source(token->range));
      break;
    default:
      if (token->ident != NULL) {
        str_append(str, token->ident);
        break;
      }
      for (int i = 0; LONG_PUNCT_TOKENS[i].str != NULL; i++) {
        if (LONG_PUNCT_TOKENS[i].kind == token->ty) {
          str_append(str, LONG_PUNCT_TOKENS[i].str);
          break;
        }
      }
      break;
    }
  }
  str_push(str, '\0');
  return new_token_str(str_get_raw(str), range);
}

static void pp_glue(Vector *ls, Vector *rs) {
  Token *l = vec_pop(ls);
  Token *r = vec_remove(rs, 0);

  String *str = new_string();
  str_append(str, pp_token_to_str(l));
  str_append(str, pp_token_to_str(r));
  str_push(str, '\0');

  Range range = range_join(l->range, r->range);
  Token *token = NULL;
  char *str_raw = str_get_raw(str);
  if (isdigit(str_raw[0])) {
    token = new_token_num(str_raw, range);
  } else if (is_ident_head(str_raw[0])) {
    token = new_token_ident(str_raw, range);
  } else {
    for (int i = 0; LONG_PUNCT_TOKENS[i].str != NULL; i++) {
      if (strcmp(LONG_PUNCT_TOKENS[i].str, str_raw) == 0) {
        token = new_token(LONG_PUNCT_TOKENS[i].kind, range);
        break;
      }
    }
  }
  if (token == NULL) {
    range_error(range, "結合できないトークンです: %s", str_raw);
  }

  vec_push(ls, token);
  vec_append(ls, rs);
}

static Vector *pp_subst_macros(Tokenizer *tokenizer, Range *expanded_from,
                               Vector *input, Vector *params, Vector *arguments,
                               Map *hideset, Vector *output) {
  while (vec_len(input) > 0) {
    Token *token = vec_remove(input, 0);
    if (token->ty == '#') {
      Token *ident = vec_remove(input, 0);
      Vector *arg = pp_get_func_arg(params, arguments, ident);
      if (arg == NULL) {
        range_error(ident->range,
                    "`#` の後がマクロのパラメーターではありません");
      }
      vec_push(output, pp_stringize(arg, ident->range));
      continue;
    }

    if (token->ty == TK_HASH_HASH) {
      Token *ident = vec_remove(input, 0);
      Vector *arg = pp_get_func_arg(params, arguments, ident);
      if (arg != NULL) {
        if (vec_len(arg) == 0) {
          continue;
        }
        pp_glue(output, vec_clone(arg));
        continue;
      }

      Vector *is = new_vector();
      vec_push(is, ident);
      pp_glue(output, is);
      continue;
    }

    Vector *arg = pp_get_func_arg(params, arguments, token);
    if (arg != NULL) {
      Token *hash_hash = vec_len(input) > 1 ? vec_get(input, 0) : NULL;
      if (hash_hash != NULL && hash_hash->ty == TK_HASH_HASH) {
        if (vec_len(arg) == 0) {
          Token *next = vec_get(input, 0);
          Vector *arg2 = pp_get_func_arg(params, arguments, next);
          if (arg2 != NULL) {
            vec_remove(input, 0);
            vec_append(output, vec_clone(arg2));
            continue;
          }
          continue;
        }
        vec_append(output, vec_clone(arg));
        continue;
      }
      vec_append(output, pp_expand_macros(tokenizer, vec_clone(arg), NULL));
      continue;
    }
    vec_push(output, token);
  }

  return pp_hsadd(expanded_from, hideset, output);
}

static Vector *pp_expand_macros(Tokenizer *tokenizer, Vector *tokens,
                                bool (*reader)(Tokenizer *, Token **)) {
  Vector *expanded = new_vector();
  while (vec_len(tokens) > 0) {
    Token *ident = vec_remove(tokens, 0);
    if (ident->ident == NULL || pp_is_token_in_hideset(ident)) {
      vec_push(expanded, ident);
      continue;
    }
    Macro *macro = map_get(tokenizer->define_map, ident->ident);
    if (macro == NULL) {
      vec_push(expanded, ident);
      continue;
    }

    if (macro->kind == MACRO_OBJ || macro->kind == MACRO_OBJ_SPECIAL) {
      Range *expanded_from = NEW(Range);
      *expanded_from = ident->range;
      Map *hideset = new_map();
      map_put(hideset, ident->ident, ident->ident);
      Vector *replacement = macro->kind == MACRO_OBJ
                                ? vec_clone(macro->replacement)
                                : macro->replacement_func(tokenizer);
      Vector *exp_tokens = pp_expand_macros(
          tokenizer,
          pp_subst_macros(tokenizer, expanded_from, replacement, NULL, NULL,
                          hideset, new_vector()),
          reader);
      vec_append(expanded, exp_tokens);
      continue;
    }

    assert(macro->kind == MACRO_FUNC);
    if (vec_len(tokens) == 0 && reader != NULL) {
      Token *token = NULL;
      reader(tokenizer, &token);
      vec_push(tokens, token);
    }
    if (vec_len(tokens) == 0 || ((Token *)vec_first(tokens))->ty != '(') {
      vec_push(expanded, ident);
      continue;
    }

    Token *rparen = NULL;
    Vector *arguments =
        pp_read_macro_func_arg(tokenizer, macro, tokens, &rparen, reader);
    Map *hideset = pp_hideset_intersection(ident, rparen);
    map_put(hideset, ident->ident, ident->ident);
    Range *expanded_from = NEW(Range);
    *expanded_from = range_join(ident->range, rparen->range);
    Vector *exp_tokens = pp_expand_macros(
        tokenizer,
        pp_subst_macros(tokenizer, expanded_from, vec_clone(macro->replacement),
                        macro->params, arguments, hideset, new_vector()),
        reader);
    vec_append(expanded, exp_tokens);
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
      "/usr/include/linux",
      "/usr/include/x86_64-linux-gnu",
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
  if (!isdigit(reader_peek(reader))) {
    return NULL;
  }

  int start = reader_get_offset(reader);

  String *str = new_string();
  char ch = reader_pop(reader);
  str_push(str, ch);

  while (true) {
    ch = reader_peek(reader);
    if (!isdigit(ch) && !is_ident_head(ch)) {
      break;
    }
    str_push(str, ch);
    reader_succ(reader);
  }

  int end = reader_get_offset(reader);

  str_push(str, '\0');
  return new_token_num(str_get_raw(str), range_from_reader(reader, start, end));
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
    return new_token_char(new_number_wchar_t(ch),
                          range_from_reader(reader, start, end));
  }
  return new_token_char(new_number_int(ch),
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
