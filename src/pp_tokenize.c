#include "gifcc.h"
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <libgen.h>
#include <limits.h>
#include <string.h>
#include <time.h>

struct PpTokenizer {
  Reader *reader;
  CharIterator *cs;
  Map *define_map;
  Vector *pp_cond_stack;
};

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
  Vector *(*replacement_func)(PpTokenizer *);
} Macro;

static bool read_token(PpTokenizer *tokenizer, Token **token);
static bool do_read_token(PpTokenizer *tokenizer, Token **token, bool skip_eol,
                          bool check_pp_cond);
static Macro *new_obj_macro(Vector *replacement);
static Macro *new_obj_special_macro(Vector *(*replacement_func)(PpTokenizer *));
static Macro *new_func_macro(Vector *params, bool has_varargs,
                             Vector *replacement);
static bool pp_directive(PpTokenizer *tokenizer);
static bool pp_cond_fullfilled(Vector *pp_cond_stack);
static bool pp_outer_cond_fullfilled(Vector *pp_cond_stack);
static bool pp_read_if_cond(PpTokenizer *tokenizer);
static Vector *pp_convert_defined(Map *define_map, Vector *tokens);
static Vector *pp_expand_macros(PpTokenizer *tokenizer, Vector *tokens,
                                bool (*reader)(PpTokenizer *, Token **));
static void pp_if(Vector *pp_cond_stack, bool fullfilled);
static void pp_elif(Vector *pp_cond_stack, bool fullfilled, const Range *range);
static void pp_else(Vector *pp_cond_stack, const Range *range);
static void pp_endif(Vector *pp_cond_stack, const Range *range);
static void do_include(Reader *reader, int offset, const char *path,
                       const Range *range, bool include_sourcedir);
static bool try_include(Reader *reader, const char *base_path,
                        const char *rel_path);
static Token *read_normal_token(CharIterator *cs);
static Token *punctuator(CharIterator *cs);
static Token *identifier_or_keyword(CharIterator *cs);
static Token *constant(CharIterator *cs);
static Token *number_constant(CharIterator *cs);
static Token *character_constant(CharIterator *cs);
static Token *string_literal(CharIterator *cs);
static Char c_char(CharIterator *cs);

static void set_predefined_num_macro(const Reader *reader, Map *map, char *name,
                                     const char *num) {
  Vector *replacement = new_vector();
  vec_push(replacement, new_token_pp_num(num, range_builtin(reader)));
  map_put(map, name, new_obj_macro(replacement));
}

static void
set_predefined_special_macro(Map *map, char *name,
                             Vector *(*replacement_func)(PpTokenizer *)) {
  map_put(map, name, new_obj_special_macro(replacement_func));
}

static Vector *macro_date(PpTokenizer *tokenizer __attribute__((unused))) {
  static char buf[30] = "";
  if (buf[0] == '\0') {
    time_t now;
    struct tm now_tm;
    const char *env = getenv("GIFCC_TIME");
    if (env != NULL) {
      now = atol(env);
    } else {
      now = time(NULL);
    }
    localtime_r(&now, &now_tm);
    strftime(buf, sizeof(buf), "%b %e %Y", &now_tm);
  }

  Vector *rep = new_vector();
  vec_push(rep, new_token_str(strdup(buf), range_builtin(tokenizer->reader)));
  return rep;
}

static Vector *macro_time(PpTokenizer *tokenizer __attribute__((unused))) {
  static char buf[30] = "";
  if (buf[0] == '\0') {
    time_t now;
    struct tm now_tm;
    const char *env = getenv("GIFCC_TIME");
    if (env != NULL) {
      now = atol(env);
    } else {
      now = time(NULL);
    }
    localtime_r(&now, &now_tm);
    strftime(buf, sizeof(buf), "%T", &now_tm);
  }

  Vector *rep = new_vector();
  vec_push(rep, new_token_str(strdup(buf), range_builtin(tokenizer->reader)));
  return rep;
}

static Vector *macro_file(PpTokenizer *tokenizer) {
  const char *filename;
  Char c = cs_peek(tokenizer->cs);
  reader_get_position(c.reader, c.start, &filename, NULL, NULL);

  Vector *rep = new_vector();
  vec_push(rep, new_token_str(filename, range_builtin(tokenizer->reader)));
  return rep;
}

static Vector *macro_line(PpTokenizer *tokenizer) {
  int line;
  Char c = cs_peek(tokenizer->cs);
  reader_get_position(c.reader, c.start, NULL, &line, NULL);

  Vector *rep = new_vector();
  vec_push(rep, new_token_pp_num(format("%d", line),
                                 range_builtin(tokenizer->reader)));
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

static bool next(void *arg, Vector *output) {
  PpTokenizer *tokenizer = arg;

  while (true) {
    Token *token = NULL;
    if (!read_token(tokenizer, &token)) {
      return false;
    }
    if (token == NULL) {
      continue;
    }

    // expand macros
    Vector *tokens = new_vector();
    vec_push(tokens, token);
    tokens = pp_expand_macros(tokenizer, tokens, read_token);

    vec_append(output, tokens);
    return true;
  }
}

TokenIterator *new_pp_tokenizer(CharIterator *cs, Reader *reader) {
  PpTokenizer *tokenizer = NEW(PpTokenizer);
  tokenizer->cs = cs;
  tokenizer->reader = reader;
  tokenizer->define_map = new_map();
  tokenizer->pp_cond_stack = new_vector();

  initialize_predefined_macro(reader, tokenizer->define_map);

  TokenIterator *ts = new_token_iterator(next, tokenizer);

  return ts;
}

static bool read_token(PpTokenizer *tokenizer, Token **token) {
  return do_read_token(tokenizer, token, true, true);
}

static inline bool is_ident_head(int c) {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}
static inline bool is_ident_tail(int c) {
  return is_ident_head(c) || ('0' <= c && c <= '9');
}

static bool skip_space(CharIterator *cs) {
  bool skipped = false;
  while (true) {
    Char ch = cs_peek(cs);
    if (!isspace(ch.val) || ch.val == '\n') {
      return skipped;
    }
    skipped = true;
    cs_succ(cs);
  }
}

static bool skip_comment(CharIterator *cs) {
  bool skipped = false;

  while (true) {
    if (cs_consume_str(cs, "//", NULL, NULL, NULL)) {
      while (true) {
        Char ch = cs_peek(cs);
        if (ch.val == '\n' || ch.val == '\0') {
          break;
        }
        cs_succ(cs);
      }
      skipped = true;
      continue;
    }
    if (cs_consume_str(cs, "/*", NULL, NULL, NULL)) {
      while (true) {
        if (cs_consume_str(cs, "*/", NULL, NULL, NULL)) {
          break;
        }
        if (cs_peek(cs).val == '\0') {
          reader_error_here(cs_peek(cs).reader,
                            "コメントの終端文字列 `*/` がありません");
        }
        cs_succ(cs);
      }
      skipped = true;
      continue;
    }
    break;
  }

  return skipped;
}

static bool skip_space_or_comment(CharIterator *cs) {
  bool skipped = false;
  while (true) {
    if (skip_space(cs)) {
      skipped = true;
      continue;
    }
    if (skip_comment(cs)) {
      skipped = true;
      continue;
    }
    break;
  }
  return skipped;
}

static void skip_to_eol(CharIterator *cs) {
  while (true) {
    Char ch = cs_peek(cs);
    if (ch.val == '\n' || ch.val == '\0') {
      break;
    }
    cs_succ(cs);
  }
  return;
}

static String *read_identifier(CharIterator *cs, const Reader **reader,
                               int *start, int *end) {
  Char ch = cs_peek(cs);
  Char cstart = ch;
  if (!is_ident_head(ch.val)) {
    return NULL;
  }

  String *str = new_string();
  str_push(str, ch.val);
  cs_succ(cs);

  while (true) {
    ch = cs_peek(cs);
    if (!is_ident_tail(ch.val)) {
      break;
    }
    str_push(str, ch.val);
    cs_succ(cs);
  }
  str_push(str, '\0');

  if (reader != NULL) {
    *reader = cstart.reader;
  }
  if (start != NULL) {
    *start = cstart.start;
  }
  if (end != NULL) {
    *end = ch.end;
  }

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

static bool do_read_token(PpTokenizer *tokenizer, Token **token, bool skip_eol,
                          bool check_pp_cond) {
  Char ch;
  while ((ch = cs_peek(tokenizer->cs)).val != '\0') {
    bool is_sol = cs_is_sol(tokenizer->cs);
    if (skip_space_or_comment(tokenizer->cs)) {
      ch = cs_peek(tokenizer->cs);
      if (ch.val == '\0') {
        break;
      }
    }
    if (ch.val == '\n') {
      if (skip_eol) {
        cs_succ(tokenizer->cs);
        continue;
      }
      *token = NULL;
      return false;
    }

    if (is_sol && pp_directive(tokenizer)) {
      continue;
    }

    Token *read_token = read_normal_token(tokenizer->cs);
    if (read_token == NULL) {
      reader_error_here(tokenizer->reader, "トークナイズできません: `%c`",
                        cs_peek(tokenizer->cs).val);
    }
    if (!check_pp_cond || pp_cond_fullfilled(tokenizer->pp_cond_stack)) {
      *token = read_token;
    } else {
      *token = NULL;
    }

    return true;
  }

  *token = new_token(TK_EOF, range_from_reader(ch.reader, ch.start, ch.end));
  return true;
}

static Macro *new_obj_macro(Vector *replacement) {
  Macro *macro = NEW(Macro);
  macro->kind = MACRO_OBJ;
  macro->replacement = replacement;
  return macro;
}

static Macro *
new_obj_special_macro(Vector *(*replacement_func)(PpTokenizer *)) {
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

static Vector *read_tokens_to_eol(PpTokenizer *tokenizer) {
  Vector *tokens = new_vector();
  while (true) {
    Token *token = NULL;
    if (!do_read_token(tokenizer, &token, false, false)) {
      Char ch = cs_peek(tokenizer->cs);
      if (ch.val == '\n' || ch.val == '\0') {
        break;
      }
      reader_error_here(tokenizer->reader, "トークナイズできません: `%c`",
                        ch.val);
    }
    vec_push(tokens, token);
  }
  return tokens;
}

static bool pp_directive(PpTokenizer *tokenizer) {
  const Reader *reader;
  int start;
  if (!cs_consume(tokenizer->cs, '#', &reader, &start, NULL)) {
    return false;
  }
  skip_space_or_comment(tokenizer->cs);

  String *directive = read_identifier(tokenizer->cs, NULL, NULL, NULL);
  if (directive == NULL) {
    skip_to_eol(tokenizer->cs);
    Char end = cs_peek(tokenizer->cs);
    (void)cs_expect(tokenizer->cs, '\n');
    range_warn(range_from_reader(end.reader, start, end.start),
               "不明なディレクティブです");
  }
  const char *directive_raw = str_get_raw(directive);

  if (strcmp(directive_raw, "if") == 0) {
    bool fullfilled = true;
    if (pp_cond_fullfilled(tokenizer->pp_cond_stack)) {
      fullfilled = pp_read_if_cond(tokenizer);
    } else {
      skip_to_eol(tokenizer->cs);
    }
    (void)cs_expect(tokenizer->cs, '\n');
    pp_if(tokenizer->pp_cond_stack, fullfilled);
    return true;
  }

  if (strcmp(directive_raw, "elif") == 0) {
    bool fullfilled = true;
    if (pp_outer_cond_fullfilled(tokenizer->pp_cond_stack)) {
      fullfilled = pp_read_if_cond(tokenizer);
    } else {
      skip_to_eol(tokenizer->cs);
    }
    Char end = cs_peek(tokenizer->cs);
    (void)cs_expect(tokenizer->cs, '\n');
    pp_elif(tokenizer->pp_cond_stack, fullfilled,
            range_from_reader(end.reader, start, end.start));
    return true;
  }

  if (strcmp(directive_raw, "ifdef") == 0) {
    skip_space_or_comment(tokenizer->cs);
    String *ident = read_identifier(tokenizer->cs, NULL, NULL, NULL);
    if (ident == NULL) {
      reader_error_here(tokenizer->reader, "識別子がありません");
    }
    skip_space_or_comment(tokenizer->cs);
    (void)cs_expect(tokenizer->cs, '\n');

    bool defined = map_get(tokenizer->define_map, str_get_raw(ident)) != NULL;
    pp_if(tokenizer->pp_cond_stack, defined);
    return true;
  }

  if (strcmp(directive_raw, "ifndef") == 0) {
    skip_space_or_comment(tokenizer->cs);
    String *ident = read_identifier(tokenizer->cs, NULL, NULL, NULL);
    if (ident == NULL) {
      reader_error_here(tokenizer->reader, "識別子がありません");
    }
    skip_space_or_comment(tokenizer->cs);
    cs_expect(tokenizer->cs, '\n');

    bool defined = map_get(tokenizer->define_map, str_get_raw(ident)) != NULL;
    pp_if(tokenizer->pp_cond_stack, !defined);
    return true;
  }

  if (strcmp(directive_raw, "else") == 0) {
    Char end = cs_peek(tokenizer->cs);
    skip_space_or_comment(tokenizer->cs);
    (void)cs_expect(tokenizer->cs, '\n');

    pp_else(tokenizer->pp_cond_stack,
            range_from_reader(end.reader, start, end.start));
    return true;
  }

  if (strcmp(directive_raw, "endif") == 0) {
    Char end = cs_peek(tokenizer->cs);
    skip_space_or_comment(tokenizer->cs);
    (void)cs_expect(tokenizer->cs, '\n');

    pp_endif(tokenizer->pp_cond_stack,
             range_from_reader(end.reader, start, end.start));
    return true;
  }

  if (!pp_cond_fullfilled(tokenizer->pp_cond_stack)) {
    skip_to_eol(tokenizer->cs);
    return true;
  }

  if (strcmp(directive_raw, "include") == 0) {
    skip_space_or_comment(tokenizer->cs);

    if (cs_consume(tokenizer->cs, '<', NULL, NULL, NULL)) {
      String *str = new_string();
      while ((cs_peek(tokenizer->cs).val != '>') &&
             (cs_peek(tokenizer->cs).val != '\0')) {
        Char ch;
        cs_pop(tokenizer->cs, &ch);
        str_push(str, ch.val);
      }
      str_push(str, '\0');
      Char end = cs_expect(tokenizer->cs, '>');

      skip_space_or_comment(tokenizer->cs);
      (void)cs_expect(tokenizer->cs, '\n');

      do_include(tokenizer->reader, start, str_get_raw(str),
                 range_from_reader(end.reader, start, end.end), false);

      return true;
    }

    if (cs_consume(tokenizer->cs, '"', NULL, NULL, NULL)) {
      String *str = new_string();
      while ((cs_peek(tokenizer->cs).val != '"') &&
             (cs_peek(tokenizer->cs).val != '\0')) {
        Char ch;
        cs_pop(tokenizer->cs, &ch);
        str_push(str, ch.val);
      }
      str_push(str, '\0');
      Char end = cs_expect(tokenizer->cs, '"');

      skip_space_or_comment(tokenizer->cs);
      (void)cs_expect(tokenizer->cs, '\n');

      do_include(tokenizer->reader, start, str_get_raw(str),
                 range_from_reader(end.reader, start, end.end), true);
      return true;
    }

    reader_error_here(tokenizer->reader,
                      "\"FILENAME\" または <FILENAME> がありません");
  }

  if (strcmp(directive_raw, "define") == 0) {
    skip_space_or_comment(tokenizer->cs);
    String *ident = read_identifier(tokenizer->cs, NULL, NULL, NULL);
    if (ident == NULL) {
      reader_error_here(tokenizer->reader, "識別子がありません");
    }
    bool has_varargs = false;
    Vector *params = NULL;
    if (cs_consume(tokenizer->cs, '(', NULL, NULL, NULL)) {
      params = new_vector();
      while (cs_peek(tokenizer->cs).val != ')') {
        skip_space_or_comment(tokenizer->cs);
        if (cs_consume_str(tokenizer->cs, "...", NULL, NULL, NULL)) {
          has_varargs = true;
          vec_push(params, "__VA_ARGS__");
          break;
        }
        String *ident = read_identifier(tokenizer->cs, NULL, NULL, NULL);
        if (ident == NULL) {
          reader_error_here(tokenizer->reader, "識別子がありません");
        }
        vec_push(params, str_get_raw(ident));
        skip_space_or_comment(tokenizer->cs);
        if (!cs_consume(tokenizer->cs, ',', NULL, NULL, NULL)) {
          break;
        }
      }
      skip_space_or_comment(tokenizer->cs);
      (void)cs_expect(tokenizer->cs, ')');
    }

    Vector *tokens = read_tokens_to_eol(tokenizer);
    (void)cs_expect(tokenizer->cs, '\n');
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
    skip_space_or_comment(tokenizer->cs);
    String *ident = read_identifier(tokenizer->cs, NULL, NULL, NULL);
    if (ident == NULL) {
      reader_error_here(tokenizer->reader, "識別子がありません");
    }
    skip_space_or_comment(tokenizer->cs);
    (void)cs_expect(tokenizer->cs, '\n');
    map_remove(tokenizer->define_map, str_get_raw(ident));
    return true;
  }

  if (strcmp(directive_raw, "error") == 0) {
    skip_space_or_comment(tokenizer->cs);

    String *str = new_string();
    while ((cs_peek(tokenizer->cs).val != '\n') &&
           (cs_peek(tokenizer->cs).val != '\0')) {
      str_push(str, cs_peek(tokenizer->cs).val);
      cs_succ(tokenizer->cs);
    }
    str_push(str, '\0');
    Char end = cs_expect(tokenizer->cs, '\n');
    range_error(range_from_reader(end.reader, start, end.start), "#error %s",
                str_get_raw(str));
  }

  if (strcmp(directive_raw, "line") == 0) {
    skip_space_or_comment(tokenizer->cs);
    Vector *tokens = read_tokens_to_eol(tokenizer);
    tokens = pp_expand_macros(tokenizer, tokens, NULL);
    if (vec_len(tokens) == 0) {
      reader_error_here(tokenizer->reader,
                        "#line directive requires a positive integer argument");
    }
    Token *num = vec_get(tokens, 0);
    if (num->ty != TK_PP_NUM) {
      range_error(num->range,
                  "#line directive requires a positive integer argument");
    }
    char *endptr;
    unsigned long long val;
    errno = 0;
    val = strtoull(num->pp_num, &endptr, 10);
    if ((val == ULLONG_MAX && errno == ERANGE) || val > INT_MAX ||
        strcmp(endptr, "") != 0) {
      range_error(num->range,
                  "#line directive requires a positive integer argument");
    }
    int line = val - 1;
    const char *filename = NULL;
    if (vec_len(tokens) > 1) {
      Token *str = vec_get(tokens, 1);
      if (str->ty != TK_STR) {
        range_error(str->range, "invalid filename for #line directive");
      }
      filename = str->str;
    }
    if (vec_len(tokens) > 2) {
      Token *token = vec_get(tokens, 2);
      range_warn(token->range, "extra tokens at end of #line directive");
    }

    (void)cs_expect(tokenizer->cs, '\n');

    reader_set_position(tokenizer->reader, &line, filename);
    return true;
  }

  skip_to_eol(tokenizer->cs);
  Char end = cs_expect(tokenizer->cs, '\n');
  range_warn(range_from_reader(end.reader, start, end.start),
             "不明なディレクティブです: %s", directive_raw);

  return true;
}

static bool pp_cond_fullfilled(Vector *pp_cond_stack) {
  if (vec_len(pp_cond_stack) > 0) {
    Cond *cond = vec_last(pp_cond_stack);
    return cond->fullfilled;
  }
  return true;
}

static bool pp_outer_cond_fullfilled(Vector *pp_cond_stack) {
  if (vec_len(pp_cond_stack) > 1) {
    Cond *outer_cond = vec_rget(pp_cond_stack, 1);
    return outer_cond->fullfilled;
  }
  return true;
}

static bool pp_read_if_cond(PpTokenizer *tokenizer) {
  Vector *tokens = read_tokens_to_eol(tokenizer);
  int here_offset = cs_peek(tokenizer->cs).start;
  const Range *here =
      range_from_reader(tokenizer->reader, here_offset, here_offset);
  Token *eof_token = new_token(TK_EOF, here);
  vec_push(tokens, eof_token);

  // convert `defined(IDENT)` or `defined INDET` into 0 or 1
  tokens = pp_convert_defined(tokenizer->define_map, tokens);

  // expand macros
  tokens = pp_expand_macros(tokenizer, tokens, NULL);

  for (int i = 0; i < vec_len(tokens); i++) {
    // replace all ident tokens (including keyword ident) into '0'
    Token *tk = vec_get(tokens, i);
    if (tk->ty == TK_PP_IDENT) {
      tk->pp_ident = NULL;
      tk->ty = TK_PP_NUM;
      tk->pp_num = strdup("0");
    }
  }

  TokenIterator *sub_ts = token_iterator_from_vec(tokens);
  sub_ts = phase6_filter(sub_ts);
  sub_ts = phase7_filter(sub_ts);
  Scope *scope = new_pp_scope(tokenizer->reader);
  Number num = integer_constant_expression(sub_ts, scope);
  if (ts_peek(sub_ts)->ty != TK_EOF) {
    range_error(ts_peek(sub_ts)->range, "改行がありません");
  }

  int val;
  SET_NUMBER_VAL(val, &num);
  return val != 0;
}

static Vector *pp_convert_defined(Map *define_map, Vector *tokens) {
  Vector *converted = new_vector();
  while (vec_len(tokens) > 0) {
    Token *token = vec_remove(tokens, 0);
    if (token->ty != TK_PP_IDENT || strcmp(token->pp_ident, "defined") != 0) {
      vec_push(converted, token);
      continue;
    }
    const Range *def_start = token->range;

    bool has_paren = false;
    token = vec_remove(tokens, 0);
    if (token->ty == '(') {
      has_paren = true;
      token = vec_remove(tokens, 0);
    }

    if (token->ty != TK_PP_IDENT) {
      range_error(token->range, "識別子がありません");
    }
    bool defined = map_get(define_map, token->pp_ident) != NULL;
    char *num = format("%d", defined);
    Token *num_token =
        new_token_pp_num(num, range_join(def_start, token->range));
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
  assert(token->ty == TK_PP_IDENT);
  if (token->pp_hideset == NULL) {
    return false;
  }
  return set_contains(token->pp_hideset, token->pp_ident);
}

static Set *pp_hideset_intersection(Token *a, Token *b) {
  if (a->pp_hideset == NULL || b->pp_hideset == NULL) {
    return new_set();
  }
  return set_intersection(a->pp_hideset, b->pp_hideset);
}

static Vector *pp_read_macro_func_arg(PpTokenizer *tokenizer, Macro *macro,
                                      Vector *tokens, Token **rparen,
                                      bool (*reader)(PpTokenizer *, Token **)) {
  assert(macro->kind == MACRO_FUNC);
  assert(((Token *)vec_first(tokens))->ty == '(');

  const Range *range = ((Token *)vec_first(tokens))->range;

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

static Vector *pp_hsadd(const Range *expanded_from, Set *hideset,
                        Vector *output) {
  for (int i = 0; i < vec_len(output); i++) {
    Token *token = token_clone(vec_get(output, i), expanded_from);
    vec_set(output, i, token);

    for (int j = 0; j < set_size(hideset); j++) {
      const char *key = set_get_by_index(hideset, j);
      if (token->pp_hideset == NULL) {
        token->pp_hideset = new_set();
      }
      set_insert(token->pp_hideset, key);
    }
  }
  return output;
}

static Vector *pp_get_func_arg(Vector *params, Vector *arguments,
                               Token *token) {
  if (token->ty != TK_PP_IDENT || params == NULL) {
    return NULL;
  }
  int i;
  for (i = 0; i < vec_len(params); i++) {
    char *key = vec_get(params, i);
    if (strcmp(key, token->pp_ident) == 0) {
      Vector *arg = vec_get(arguments, i);
      assert(arg != NULL);
      return arg;
    }
  }
  return NULL;
}

static Token *pp_stringize(Vector *arg, const Range *range) {
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
    case TK_PP_NUM:
      str_append(str, token->pp_num);
      break;
    case TK_PP_IDENT:
      str_append(str, token->pp_ident);
      break;
    case TK_STR:
    case TK_CHARCONST:
      str_append(str, reader_get_source(token->range));
      break;
    default:
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
  str_append(str, token_to_str(l));
  str_append(str, token_to_str(r));
  str_push(str, '\0');

  const Range *range = range_join(l->range, r->range);
  Token *token = NULL;
  char *str_raw = str_get_raw(str);
  if (isdigit(str_raw[0])) {
    token = new_token_pp_num(str_raw, range);
  } else if (is_ident_head(str_raw[0])) {
    token = new_token_pp_ident(str_raw, range);
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

static Vector *pp_subst_macros(PpTokenizer *tokenizer,
                               const Range *expanded_from, Vector *input,
                               Vector *params, Vector *arguments, Set *hideset,
                               Vector *output) {
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
        // NonStandard/GNU: , ## __VA_ARGS__
        if (ident->ty == TK_PP_IDENT &&
            strcmp(ident->pp_ident, "__VA_ARGS__") == 0 &&
            vec_len(output) > 0 && ((Token *)vec_last(output))->ty == ',') {
          if (vec_len(arg) == 0) {
            vec_pop(output);
          } else {
            vec_append(output, vec_clone(arg));
          }
          continue;
        }

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

static Vector *pp_expand_macros(PpTokenizer *tokenizer, Vector *tokens,
                                bool (*reader)(PpTokenizer *, Token **)) {
  Vector *expanded = new_vector();
  while (vec_len(tokens) > 0) {
    Token *ident = vec_remove(tokens, 0);
    if (ident->ty != TK_PP_IDENT) {
      vec_push(expanded, ident);
      continue;
    }
    if (pp_is_token_in_hideset(ident)) {
      vec_push(expanded, ident);
      continue;
    }

    Macro *macro = map_get(tokenizer->define_map, ident->pp_ident);
    if (macro == NULL) {
      vec_push(expanded, ident);
      continue;
    }

    if (macro->kind == MACRO_OBJ || macro->kind == MACRO_OBJ_SPECIAL) {
      const Range *expanded_from = ident->range;
      Set *hideset = new_set();
      set_insert(hideset, ident->pp_ident);
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
    Set *hideset = pp_hideset_intersection(ident, rparen);
    set_insert(hideset, ident->pp_ident);
    const Range *expanded_from = range_join(ident->range, rparen->range);
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
  Cond *cond = NEW(Cond);
  cond->fullfilled = pp_cond_fullfilled(pp_cond_stack) && fullfilled;
  cond->once_fullfilled = fullfilled;
  vec_push(pp_cond_stack, cond);
}

static void pp_elif(Vector *pp_cond_stack, bool fullfilled,
                    const Range *range) {
  if (vec_len(pp_cond_stack) <= 0) {
    range_error(range, "#if, #ifdef, #ifndefがありません");
  }
  Cond *cond = vec_last(pp_cond_stack);
  if (cond->once_fullfilled) {
    fullfilled = false;
  }
  cond->fullfilled = pp_outer_cond_fullfilled(pp_cond_stack) && fullfilled;
  cond->once_fullfilled |= fullfilled;
}

static void pp_else(Vector *pp_cond_stack, const Range *range) {
  if (vec_len(pp_cond_stack) <= 0) {
    range_error(range, "#if, #ifdef, #ifndefがありません");
  }
  Cond *cond = vec_last(pp_cond_stack);
  bool fullfilled = !cond->once_fullfilled;
  cond->fullfilled = pp_outer_cond_fullfilled(pp_cond_stack) && fullfilled;
  cond->once_fullfilled |= fullfilled;
}

static void pp_endif(Vector *pp_cond_stack, const Range *range) {
  if (vec_len(pp_cond_stack) <= 0) {
    range_error(range, "#if, #ifdef, #ifndefがありません");
  }
  vec_pop(pp_cond_stack);
}

static void do_include(Reader *reader, int offset, const char *path,
                       const Range *range, bool include_sourcedir) {
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
  char *abs_path = format("%s/%s", base_path, rel_path);
  FILE *fp = fopen(abs_path, "r");
  if (fp != NULL) {
    reader_add_file(reader, fp, abs_path);
    return true;
  }
  return false;
}

static Token *read_normal_token(CharIterator *cs) {
  Token *token;
  if ((token = punctuator(cs)) != NULL || (token = constant(cs)) != NULL ||
      (token = string_literal(cs)) != NULL ||
      (token = identifier_or_keyword(cs)) != NULL) {
    return token;
  }
  return NULL;
}

static Token *punctuator(CharIterator *cs) {
  int kind, start, end;
  const Reader *reader;

  for (int i = 0; LONG_PUNCT_TOKENS[i].str != NULL; i++) {
    const LongToken *tk = &LONG_PUNCT_TOKENS[i];
    if (cs_consume_str(cs, tk->str, &reader, &start, &end)) {
      kind = tk->kind;
      goto Hit;
    }
  }

  for (int i = 0; SHORT_PUNCT_TOKENS[i] != '\0'; i++) {
    char tk = SHORT_PUNCT_TOKENS[i];
    if (cs_consume(cs, tk, &reader, &start, &end)) {
      kind = tk;
      goto Hit;
    }
  }

  return NULL;

Hit:;
  return new_token(kind, range_from_reader(reader, start, end));
}

static Token *identifier_or_keyword(CharIterator *cs) {
  int start, end;
  const Reader *reader;

  String *str = read_identifier(cs, &reader, &start, &end);
  if (str == NULL) {
    return NULL;
  }
  const Range *range = range_from_reader(reader, start, end);
  return new_token_pp_ident(str_get_raw(str), range);
}

static Token *constant(CharIterator *cs) {
  Token *token;
  if ((token = number_constant(cs)) != NULL ||
      (token = character_constant(cs)) != NULL) {
    return token;
  }
  return NULL;
}

static Token *number_constant(CharIterator *cs) {
  if (!isdigit(cs_peek(cs).val)) {
    return NULL;
  }

  String *str = new_string();
  Char ch;
  cs_pop(cs, &ch);
  str_push(str, ch.val);
  int start = ch.start;

  Char last = ch;
  while (true) {
    ch = cs_peek(cs);
    bool is_float = strchr("eEpP", last.val) && strchr("+-", ch.val);
    if (!isdigit(ch.val) && !is_ident_head(ch.val) && ch.val != '.' &&
        !is_float) {
      break;
    }
    str_push(str, ch.val);
    cs_succ(cs);
    last = ch;
  }

  int end = last.end;

  str_push(str, '\0');
  return new_token_pp_num(str_get_raw(str),
                          range_from_reader(ch.reader, start, end));
}

static Token *character_constant(CharIterator *cs) {
  int start;
  bool is_wide;
  if (cs_consume_str(cs, "L\'", NULL, &start, NULL)) {
    is_wide = true;
  } else if (cs_consume(cs, '\'', NULL, &start, NULL)) {
    is_wide = false;
  } else {
    return NULL;
  }
  Char ch;
  ch = cs_peek(cs);
  if (ch.val == '\'' || ch.val == '\0') {
    reader_error_offset(ch.reader, start, "空の文字リテラルです");
  }
  ch = c_char(cs);
  int end = cs_expect(cs, '\'').end;

  if (is_wide) {
    return new_token_char(new_number_wchar_t(ch.val),
                          range_from_reader(ch.reader, start, end));
  }
  return new_token_char(new_number_int(ch.val),
                        range_from_reader(ch.reader, start, end));
}

static Token *string_literal(CharIterator *cs) {
  const Reader *reader;
  int start;
  if (!cs_consume(cs, '"', &reader, &start, NULL)) {
    return NULL;
  }

  String *str = new_string();
  while (true) {
    Char ch = cs_peek(cs);
    if (ch.val == '"' || ch.val == '\0') {
      break;
    }
    str_push(str, c_char(cs).val);
  }
  str_push(str, '\0');

  int end = cs_expect(cs, '"').end;
  return new_token_str(str_get_raw(str), range_from_reader(reader, start, end));
}

static Char c_char(CharIterator *cs) {
  const Reader *reader;
  int start;

  if (cs_consume(cs, '\\', &reader, &start, NULL)) {
    const char ESCAPE_CHARS[] = {'\'', '"', '?', '\\', '\0'};
    for (int i = 0; ESCAPE_CHARS[i] != '\0'; i++) {
      int end;
      if (cs_consume(cs, ESCAPE_CHARS[i], NULL, NULL, &end)) {
        return (Char){
            .val = ESCAPE_CHARS[i],
            .start = start,
            .end = end,
            .reader = reader,
        };
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
      int end;
      if (cs_consume(cs, META_CHARS[i].raw, NULL, NULL, &end)) {
        return (Char){
            .val = META_CHARS[i].meta,
            .start = start,
            .end = end,
            .reader = reader,
        };
      }
    }

    if (cs_consume(cs, 'x', NULL, NULL, NULL)) {
      int end = cs_peek(cs).end;
      if (!is_hex_digit(cs_peek(cs).val)) {
        reader_error_offset(reader, start, "空の16進文字リテラルです");
      }
      int val = 0;
      while (true) {
        Char ch = cs_peek(cs);
        if (!is_hex_digit(ch.val)) {
          break;
        }
        val = val * 0x10 + hex2num(ch.val);
        cs_succ(cs);
      }
      return (Char){
          .val = val,
          .start = start,
          .end = end,
          .reader = reader,
      };
    }

    if (is_oct_digit(cs_peek(cs).val)) {
      int val = 0;
      int end = cs_peek(cs).end;
      while (true) {
        Char ch = cs_peek(cs);
        if (!is_oct_digit(ch.val)) {
          break;
        }
        end = ch.end;
        val = val * 010 + oct2num(ch.val);
        cs_succ(cs);
      }
      return (Char){
          .val = val,
          .start = start,
          .end = end,
          .reader = reader,
      };
    }

    reader_error_offset(reader, start, "不明なエスケープシーケンスです");
  }

  if (cs_consume(cs, '\n', NULL, NULL, NULL)) {
    reader_error_here(reader,
                      "改行文字を文字リテラル中に含めることはできません");
  }

  Char ch;
  cs_pop(cs, &ch);
  return ch;
}
