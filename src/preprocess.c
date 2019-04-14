#include "gifcc.h"
#include <ctype.h>
#include <errno.h>
#include <libgen.h>
#include <limits.h>
#include <time.h>

typedef struct Preprocessor Preprocessor;
typedef Vector *special_macro_handler_t(Preprocessor *, Token *);

typedef struct Cond {
  bool once_fullfilled;
  bool fullfilled;
  const Range *range;
} Cond;
typedef DEFINE_VECTOR(CondVector, Cond *) CondVector;

typedef enum {
  MACRO_OBJ,
  MACRO_FUNC,
  MACRO_OBJ_SPECIAL,
} macro_t;

typedef struct Macro {
  macro_t kind;
  StrVector *params;
  bool has_varargs;
  Vector *replacement;
  special_macro_handler_t *replacement_func;
} Macro;

typedef struct Preprocessor {
  Reader *reader;
  Map *define_map;
  CondVector *cond_stack;
} Preprocessor;

typedef struct ConditionalInclusionArg {
  Preprocessor *pp;
  TokenIterator *ts;
} ConditionalInclusionArg;

typedef struct ControlArg {
  Preprocessor *pp;
  TokenIterator *ts;
} ControlArg;

typedef struct ExpandMacroArg {
  Preprocessor *pp;
  TokenIterator *ts;
} ExpandMacroArg;

static Macro *new_obj_macro(Vector *replacement);
static Macro *new_obj_special_macro(special_macro_handler_t *replacement_func);
static Macro *new_func_macro(StrVector *params, bool has_varargs,
                             Vector *replacement);
static void initialize_predefined_macro(Preprocessor *pp,
                                        const Range *builtin_range);
static void set_predefined_num_macro(Preprocessor *pp,
                                     const Range *builtin_range,
                                     const char *name, const char *num);
static void
set_predefined_special_macro(Preprocessor *pp, const char *name,
                             special_macro_handler_t *replacement_func);
static Vector *macro_date(Preprocessor *pp, Token *token);
static Vector *macro_time(Preprocessor *pp, Token *token);
static Vector *macro_file(Preprocessor *pp, Token *token);
static Vector *macro_line(Preprocessor *pp, Token *token);

static bool conditional_inclusion(void *arg, Vector *output);
static bool pp_cond_fullfilled(CondVector *cond_stack);
static bool pp_outer_cond_fullfilled(CondVector *cond_stack);
static bool read_pp_if_cond(Preprocessor *pp, Vector *tokens,
                            const Range *range);
static Vector *pp_convert_defined(Map *define_map, Vector *tokens);
static void pp_if(CondVector *cond_stack, bool fullfilled, const Range *range);
static void pp_elif(CondVector *cond_stack, bool fullfilled,
                    const Range *range);
static void pp_else(CondVector *cond_stack, const Range *range);
static void pp_endif(CondVector *cond_stack, const Range *range);

static bool control_directive(void *arg, Vector *output);
static void do_include(const Range *range, const char *path,
                       bool include_sourcedir);
static bool try_include(Reader *reader, const char *base_path,
                        const char *rel_path);

static bool expand_macro(void *arg, Vector *output);

static Vector *pp_expand_macros(Preprocessor *pp, Vector *tokens,
                                TokenIterator *ts);
static Vector *pp_subst_macros(Preprocessor *pp, const Range *expanded_from,
                               Vector *input, StrVector *params,
                               Vector *arguments, Set *hideset, Vector *output);
static Vector *pp_get_func_arg(StrVector *params, Vector *arguments,
                               Token *token);
static Vector *pp_read_macro_func_arg(Macro *macro, Vector *tokens,
                                      Token **rparen, TokenIterator *ts);
static bool pp_is_token_in_hideset(Token *token);
static Vector *pp_hsadd(const Range *expanded_from, Set *hideset,
                        Vector *output);
static Set *pp_hideset_intersection(Token *a, Token *b);
static Token *pp_stringize(Vector *arg, bool quote);
static const char *token_to_str(const Token *token);
static void pp_glue(Vector *ls, Vector *rs);

TokenIterator *new_preprocessor(TokenIterator *ts, Reader *reader) {
  Preprocessor *pp = NEW(Preprocessor);
  *pp = (Preprocessor){
      .reader = reader,
      .define_map = new_map(),
      .cond_stack = NEW_VECTOR(CondVector),
  };

  const Range *builtin_range = range_builtin(reader);
  initialize_predefined_macro(pp, builtin_range);

  ConditionalInclusionArg *cond_arg = NEW(ConditionalInclusionArg);
  cond_arg->pp = pp;
  cond_arg->ts = ts;
  ts = new_token_iterator(conditional_inclusion, cond_arg);

  ControlArg *ctrl_arg = NEW(ControlArg);
  ctrl_arg->pp = pp;
  ctrl_arg->ts = ts;
  ts = new_token_iterator(control_directive, ctrl_arg);

  ExpandMacroArg *expand_arg = NEW(ExpandMacroArg);
  expand_arg->pp = pp;
  expand_arg->ts = ts;
  ts = new_token_iterator(expand_macro, expand_arg);

  return ts;
}

static Macro *new_obj_macro(Vector *replacement) {
  Macro *macro = NEW(Macro);
  macro->kind = MACRO_OBJ;
  macro->replacement = replacement;
  return macro;
}

static Macro *new_obj_special_macro(special_macro_handler_t *replacement_func) {
  Macro *macro = NEW(Macro);
  macro->kind = MACRO_OBJ_SPECIAL;
  macro->replacement_func = replacement_func;
  return macro;
}

static Macro *new_func_macro(StrVector *params, bool has_varargs,
                             Vector *replacement) {
  Macro *macro = NEW(Macro);
  macro->kind = MACRO_FUNC;
  macro->params = params;
  macro->has_varargs = has_varargs;
  macro->replacement = replacement;
  return macro;
}

static void initialize_predefined_macro(Preprocessor *pp,
                                        const Range *builtin_range) {
  set_predefined_special_macro(pp, "__DATE__", macro_date);
  set_predefined_special_macro(pp, "__TIME__", macro_time);
  set_predefined_special_macro(pp, "__FILE__", macro_file);
  set_predefined_special_macro(pp, "__LINE__", macro_line);

  set_predefined_num_macro(pp, builtin_range, "__STDC__", "1");
  set_predefined_num_macro(pp, builtin_range, "__STDC_HOSTED__", "1");
  set_predefined_num_macro(pp, builtin_range, "__STDC_VERSION__", "201112L");
  set_predefined_num_macro(pp, builtin_range, "__LP64__", "1");
  set_predefined_num_macro(pp, builtin_range, "__x86_64__", "1");
}

static void set_predefined_num_macro(Preprocessor *pp,
                                     const Range *builtin_range,
                                     const char *name, const char *num) {
  Vector *replacement = new_vector();
  vec_push(replacement, new_token_pp_num(num, builtin_range));
  map_put(pp->define_map, name, new_obj_macro(replacement));
}

static void
set_predefined_special_macro(Preprocessor *pp, const char *name,
                             special_macro_handler_t *replacement_func) {
  map_put(pp->define_map, name, new_obj_special_macro(replacement_func));
}

static Vector *macro_date(Preprocessor *pp,
                          Token *token __attribute__((unused))) {
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
    strftime(buf, sizeof(buf), "\"%b %e %Y\"", &now_tm);
  }

  Vector *rep = new_vector();
  vec_push(rep, new_token_pp_str(strdup(buf), range_builtin(pp->reader)));
  return rep;
}

static Vector *macro_time(Preprocessor *pp,
                          Token *token __attribute__((unused))) {
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
    strftime(buf, sizeof(buf), "\"%T\"", &now_tm);
  }

  Vector *rep = new_vector();
  vec_push(rep, new_token_pp_str(strdup(buf), range_builtin(pp->reader)));
  return rep;
}

static Vector *macro_file(Preprocessor *pp, Token *token) {
  const char *filename;
  range_get_start(token->range, &filename, NULL, NULL);

  Vector *rep = new_vector();
  vec_push(rep, new_token_pp_str(format("\"%s\"", filename),
                                 range_builtin(pp->reader)));
  return rep;
}

static Vector *macro_line(Preprocessor *pp, Token *token) {
  int line;
  range_get_start(token->range, NULL, &line, NULL);

  Vector *rep = new_vector();
  vec_push(rep,
           new_token_pp_num(format("%d", line), range_builtin(pp->reader)));
  return rep;
}

static bool conditional_inclusion(void *arg, Vector *output) {
  ConditionalInclusionArg *cond_arg = arg;
  Preprocessor *pp = cond_arg->pp;
  TokenIterator *ts = cond_arg->ts;

  while (true) {
    Token *token = ts_pop(ts);
    if (token == NULL) {
      return false;
    }

    switch (token->ty) {
    case TK_PP_IF: {
      Vector *tokens = token->pp_if.tokens;
      bool fullfilled = true;
      if (pp_cond_fullfilled(pp->cond_stack)) {
        fullfilled = read_pp_if_cond(pp, tokens, token->range);
      }
      pp_if(pp->cond_stack, fullfilled, token->range);
      continue;
    }
    case TK_PP_ELIF: {
      Vector *tokens = token->pp_elif.tokens;
      bool fullfilled = true;
      if (pp_outer_cond_fullfilled(pp->cond_stack)) {
        fullfilled = read_pp_if_cond(pp, tokens, token->range);
      }
      pp_elif(pp->cond_stack, fullfilled, token->range);
      continue;
    }
    case TK_PP_IFDEF: {
      const char *ident = token->pp_ifdef.ident;
      bool defined = map_get(pp->define_map, ident) != NULL;
      pp_if(pp->cond_stack, defined, token->range);
      continue;
    }
    case TK_PP_IFNDEF: {
      const char *ident = token->pp_ifndef.ident;
      bool defined = map_get(pp->define_map, ident) != NULL;
      pp_if(pp->cond_stack, !defined, token->range);
      continue;
    }
    case TK_PP_ELSE: {
      pp_else(pp->cond_stack, token->range);
      continue;
    }
    case TK_PP_ENDIF: {
      pp_endif(pp->cond_stack, token->range);
      continue;
    }
    case TK_EOF: {
      if (VEC_LEN(pp->cond_stack) > 0) {
        Cond *cond = VEC_LAST(pp->cond_stack);
        range_error(cond->range, "Unterminated conditional directive");
      }
      break;
    }
    default: {
      if (!pp_cond_fullfilled(pp->cond_stack)) {
        continue;
      }
      break;
    }
    }

    vec_push(output, token);
    return true;
  }
}

static bool pp_cond_fullfilled(CondVector *cond_stack) {
  if (VEC_LEN(cond_stack) > 0) {
    Cond *cond = VEC_LAST(cond_stack);
    return cond->fullfilled;
  }
  return true;
}

static bool pp_outer_cond_fullfilled(CondVector *cond_stack) {
  if (VEC_LEN(cond_stack) > 1) {
    Cond *outer_cond = VEC_RGET(cond_stack, 1);
    return outer_cond->fullfilled;
  }
  return true;
}

static bool read_pp_if_cond(Preprocessor *pp, Vector *tokens,
                            const Range *range) {
  if (vec_len(tokens) == 0) {
    range_error(range, "expected value in expression");
  }
  Token *last = vec_last(tokens);
  const Range *here = last->range;
  const Reader *reader = range_get_reader(here);
  Token *eof_token = new_token(TK_EOF, here);
  vec_push(tokens, eof_token);

  // convert `defined(IDENT)` or `defined INDET` into 0 or 1
  tokens = pp_convert_defined(pp->define_map, tokens);

  // expand macros
  tokens = pp_expand_macros(pp, tokens, NULL);

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
  sub_ts = phase5_filter(sub_ts);
  sub_ts = phase6_filter(sub_ts);
  sub_ts = phase7_filter(sub_ts);
  Scope *scope = new_pp_scope(reader);
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

static void pp_if(CondVector *cond_stack, bool fullfilled, const Range *range) {
  Cond *cond = NEW(Cond);
  cond->fullfilled = pp_cond_fullfilled(cond_stack) && fullfilled;
  cond->once_fullfilled = fullfilled;
  cond->range = range;
  VEC_PUSH(cond_stack, cond);
}

static void pp_elif(CondVector *cond_stack, bool fullfilled,
                    const Range *range) {
  if (VEC_LEN(cond_stack) <= 0) {
    range_error(range, "#elif without #if, #ifdef, #ifndef");
  }
  Cond *cond = VEC_LAST(cond_stack);
  if (cond->once_fullfilled) {
    fullfilled = false;
  }
  cond->fullfilled = pp_outer_cond_fullfilled(cond_stack) && fullfilled;
  cond->once_fullfilled |= fullfilled;
}

static void pp_else(CondVector *cond_stack, const Range *range) {
  if (VEC_LEN(cond_stack) <= 0) {
    range_error(range, "#else without #if, #ifdef, #ifndef");
  }
  Cond *cond = VEC_LAST(cond_stack);
  bool fullfilled = !cond->once_fullfilled;
  cond->fullfilled = pp_outer_cond_fullfilled(cond_stack) && fullfilled;
  cond->once_fullfilled |= fullfilled;
}

static void pp_endif(CondVector *cond_stack, const Range *range) {
  if (VEC_LEN(cond_stack) <= 0) {
    range_error(range, "#endif without #if, #ifdef, #ifndef");
  }
  VEC_POP(cond_stack);
}

static bool control_directive(void *arg, Vector *output) {
  ControlArg *ctrl_arg = arg;
  Preprocessor *pp = ctrl_arg->pp;
  TokenIterator *ts = ctrl_arg->ts;

  while (true) {
    Token *token = ts_pop(ts);
    if (token == NULL) {
      return false;
    }

    switch (token->ty) {
    case TK_PP_NULL: {
      continue;
    }
    case TK_PP_INCLUDE: {
      Vector *tokens = token->pp_include.tokens;
      tokens = pp_expand_macros(pp, tokens, NULL);
      if (vec_len(tokens) == 0) {
        range_error(token->range, "expected \"FILENAME\" or <FILENAME>");
      }
      Token *filename = pp_stringize(tokens, false);
      int len = strlen(filename->pp_str);
      if ((filename->pp_str[0] == '<' && filename->pp_str[len - 1] == '>') ||
          (filename->pp_str[0] == '"' && filename->pp_str[len - 1] == '"')) {
        char *path = strdup(filename->pp_str);
        path[0] = '\0';
        path[len - 1] = '\0';
        path = &path[1];
        bool include_sourcedir = filename->pp_str[0] == '"';

        do_include(token->range, path, include_sourcedir);
      } else {
        range_error(filename->range, "expected \"FILENAME\" or <FILENAME>");
      }
      continue;
    }
    case TK_PP_DEFINE: {
      const char *ident = token->pp_define.ident;
      StrVector *params = token->pp_define.params;
      bool has_varargs = token->pp_define.has_varargs;
      if (has_varargs) {
        VEC_PUSH(params, "__VA_ARGS__");
      }
      Vector *replacements = token->pp_define.replacements;
      Macro *macro;
      if (params != NULL) {
        macro = new_func_macro(params, has_varargs, replacements);
      } else {
        macro = new_obj_macro(replacements);
      }
      map_put(pp->define_map, ident, macro);
      continue;
    }
    case TK_PP_UNDEF: {
      const char *ident = token->pp_undef.ident;
      map_remove(pp->define_map, ident);
      continue;
    }
    case TK_PP_ERROR: {
      const char *message = token->pp_error.message;
      range_error(token->range, "#error %s", message);
      continue;
    }
    case TK_PP_LINE: {
      Vector *tokens = token->pp_line.tokens;
      tokens = pp_expand_macros(pp, tokens, NULL);
      if (vec_len(tokens) == 0) {
        range_error(token->range,
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
      int line = val;
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

      reader_set_position(pp->reader, &line, filename);
      continue;
    }
    case TK_PP_UNKNOWN: {
      range_error(token->range, "invalid preprocessor directive");
    }
    default:
      break;
    }

    vec_push(output, token);
    return true;
  }
}

static void do_include(const Range *range, const char *path,
                       bool include_sourcedir) {
  Reader *reader = (Reader *)range_get_reader(range);
  if (include_sourcedir) {
    const char *sourcepath;
    range_get_start(range, &sourcepath, NULL, NULL);
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

static bool expand_macro(void *arg, Vector *output) {
  ExpandMacroArg *expand_arg = arg;
  Preprocessor *pp = expand_arg->pp;
  TokenIterator *ts = expand_arg->ts;

  Token *token = ts_pop(ts);
  if (token == NULL) {
    return false;
  }

  Vector *tokens = new_vector();
  vec_push(tokens, token);
  tokens = pp_expand_macros(pp, tokens, ts);
  vec_append(output, tokens);
  return true;
}

static Vector *pp_expand_macros(Preprocessor *pp, Vector *tokens,
                                TokenIterator *ts) {
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

    Macro *macro = map_get(pp->define_map, ident->pp_ident);
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
                                : macro->replacement_func(pp, ident);
      Vector *exp_tokens =
          pp_expand_macros(pp,
                           pp_subst_macros(pp, expanded_from, replacement, NULL,
                                           NULL, hideset, new_vector()),
                           ts);
      vec_append(expanded, exp_tokens);
      continue;
    }

    assert(macro->kind == MACRO_FUNC);
    if (vec_len(tokens) == 0 && ts != NULL) {
      Token *token = ts_pop(ts);
      vec_push(tokens, token);
    }
    if (vec_len(tokens) == 0 || ((Token *)vec_first(tokens))->ty != '(') {
      vec_push(expanded, ident);
      continue;
    }

    Token *rparen = NULL;
    Vector *arguments = pp_read_macro_func_arg(macro, tokens, &rparen, ts);
    Set *hideset = pp_hideset_intersection(ident, rparen);
    set_insert(hideset, ident->pp_ident);
    const Range *expanded_from = range_join(ident->range, rparen->range);
    Vector *exp_tokens = pp_expand_macros(
        pp,
        pp_subst_macros(pp, expanded_from, vec_clone(macro->replacement),
                        macro->params, arguments, hideset, new_vector()),
        ts);
    vec_append(expanded, exp_tokens);
  }
  return expanded;
}

static Vector *pp_subst_macros(Preprocessor *pp, const Range *expanded_from,
                               Vector *input, StrVector *params,
                               Vector *arguments, Set *hideset,
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
      vec_append(output, pp_expand_macros(pp, vec_clone(arg), NULL));
      continue;
    }
    vec_push(output, token);
  }

  return pp_hsadd(expanded_from, hideset, output);
}

static Vector *pp_get_func_arg(StrVector *params, Vector *arguments,
                               Token *token) {
  if (token->ty != TK_PP_IDENT || params == NULL) {
    return NULL;
  }
  int i;
  for (i = 0; i < VEC_LEN(params); i++) {
    const char *key = VEC_GET(params, i);
    if (strcmp(key, token->pp_ident) == 0) {
      Vector *arg = vec_get(arguments, i);
      assert(arg != NULL);
      return arg;
    }
  }
  return NULL;
}

static Vector *pp_read_macro_func_arg(Macro *macro, Vector *tokens,
                                      Token **rparen, TokenIterator *ts) {
  assert(macro->kind == MACRO_FUNC);
  assert(((Token *)vec_first(tokens))->ty == '(');

  const Range *range = ((Token *)vec_first(tokens))->range;

  Vector *arguments = new_vector();
  Vector *current_arg = NULL;
  int nest = 0;
  do {
    Token *token;
    if (vec_len(tokens) == 0 && ts != NULL) {
      token = ts_pop(ts);
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
         vec_len(arguments) < VEC_LEN(macro->params) - 1)) {
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
  if (macro->has_varargs && vec_len(arguments) == VEC_LEN(macro->params) - 1) {
    vec_push(arguments, new_vector());
  }
  if (vec_len(arguments) == 0 && VEC_LEN(macro->params) == 1) {
    vec_push(arguments, new_vector());
  }

  if (VEC_LEN(macro->params) != vec_len(arguments)) {
    range_error(range,
                "関数マクロの引数の個数が一致しません, 仮引数: %d, 引数: %d",
                VEC_LEN(macro->params), vec_len(arguments));
  }

  return arguments;
}

static bool pp_is_token_in_hideset(Token *token) {
  assert(token->ty == TK_PP_IDENT);
  if (token->pp_hideset == NULL) {
    return false;
  }
  return set_contains(token->pp_hideset, token->pp_ident);
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

static Set *pp_hideset_intersection(Token *a, Token *b) {
  if (a->pp_hideset == NULL || b->pp_hideset == NULL) {
    return new_set();
  }
  return set_intersection(a->pp_hideset, b->pp_hideset);
}

static Token *pp_stringize(Vector *arg, bool quote) {
  const Range *range = NULL;
  String *str = new_string();
  if (quote) {
    str_push(str, '"');
  }
  for (int i = 0; i < vec_len(arg); i++) {
    Token *token = vec_get(arg, i);
    range = range_join(range, token->range);
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
    case TK_PP_CHAR:
      str_append(str, token->pp_char);
      break;
    case TK_PP_STR:
      str_append(str, token->pp_str);
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
  if (quote) {
    str_push(str, '"');
  }
  str_push(str, '\0');
  return new_token_pp_str(str_get_raw(str), range);
}

static const char *token_to_str(const Token *token) {
  if (token->ty <= 255) {
    return format("%c", token->ty);
  }
  if (token->ty == TK_PP_IDENT) {
    return token->pp_ident;
  }
  if (token->ty == TK_PP_NUM) {
    return token->pp_num;
  }
  for (int i = 0; LONG_PUNCT_TOKENS[i].str != NULL; i++) {
    if (LONG_PUNCT_TOKENS[i].kind == token->ty) {
      return LONG_PUNCT_TOKENS[i].str;
    }
  }
  range_error(token->range, "結合できないトークンです");
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
