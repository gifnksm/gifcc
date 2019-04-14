#include "gifcc.h"
#include <ctype.h>
#include <errno.h>
#include <libgen.h>
#include <limits.h>

typedef struct Cond {
  bool once_fullfilled;
  bool fullfilled;
  const Range *range;
} Cond;
typedef DEFINE_VECTOR(CondVector, Cond *) CondVector;

typedef DEFINE_VECTOR(TokenListVector, TokenVector *) TokenListVector;

typedef struct ConditionalInclusionArg {
  CondVector *cond_stack;
  Map *define_map;
  TokenIterator *ts;
} ConditionalInclusionArg;

typedef struct ControlArg {
  Reader *reader;
  Map *define_map;
  TokenIterator *ts;
} ControlArg;

typedef struct ExpandMacroArg {
  Map *define_map;
  TokenIterator *ts;
} ExpandMacroArg;

static bool conditional_inclusion(void *arg, TokenVector *output);
static bool pp_cond_fullfilled(CondVector *cond_stack);
static bool pp_outer_cond_fullfilled(CondVector *cond_stack);
static bool read_pp_if_cond(Map *define_map, TokenVector *tokens,
                            const Range *range);
static TokenVector *pp_convert_defined(Map *define_map, TokenVector *tokens);
static void pp_if(CondVector *cond_stack, bool fullfilled, const Range *range);
static void pp_elif(CondVector *cond_stack, bool fullfilled,
                    const Range *range);
static void pp_else(CondVector *cond_stack, const Range *range);
static void pp_endif(CondVector *cond_stack, const Range *range);

static bool control_directive(void *arg, TokenVector *output);
static void do_include(const Range *range, const char *path,
                       bool include_sourcedir);
static bool try_include(Reader *reader, const char *base_path,
                        const char *rel_path);

static bool expand_macro(void *arg, TokenVector *output);

static TokenVector *pp_expand_macros(Map *define_map, TokenVector *tokens,
                                     TokenIterator *ts);
static TokenVector *pp_subst_macros(Map *define_map, const Range *expanded_from,
                                    TokenVector *input, StrVector *params,
                                    TokenListVector *arguments, Set *hideset,
                                    TokenVector *output);
static TokenVector *pp_get_func_arg(StrVector *params,
                                    TokenListVector *arguments, Token *token);
static TokenListVector *pp_read_macro_func_arg(Macro *macro,
                                               TokenVector *tokens,
                                               Token **rparen,
                                               TokenIterator *ts);
static bool pp_is_token_in_hideset(Token *token);
static TokenVector *pp_hsadd(const Range *expanded_from, Set *hideset,
                             TokenVector *output);
static Set *pp_hideset_intersection(Token *a, Token *b);
static Token *pp_stringize(TokenVector *arg, bool quote);
static const char *token_to_str(const Token *token);
static void pp_glue(TokenVector *ls, TokenVector *rs);

TokenIterator *new_preprocessor(TokenIterator *ts, Reader *reader) {
  Map *define_map = new_map();

  const Range *builtin_range = range_builtin(reader);
  initialize_predefined_macro(define_map, builtin_range);

  ConditionalInclusionArg *cond_arg = NEW(ConditionalInclusionArg);
  cond_arg->cond_stack = NEW_VECTOR(CondVector);
  cond_arg->define_map = define_map;
  cond_arg->ts = ts;
  ts = new_token_iterator(conditional_inclusion, cond_arg);

  ControlArg *ctrl_arg = NEW(ControlArg);
  ctrl_arg->reader = reader;
  ctrl_arg->define_map = define_map;
  ctrl_arg->ts = ts;
  ts = new_token_iterator(control_directive, ctrl_arg);

  ExpandMacroArg *expand_arg = NEW(ExpandMacroArg);
  expand_arg->define_map = define_map;
  expand_arg->ts = ts;
  ts = new_token_iterator(expand_macro, expand_arg);

  return ts;
}

static bool conditional_inclusion(void *arg, TokenVector *output) {
  ConditionalInclusionArg *cond_arg = arg;
  CondVector *cond_stack = cond_arg->cond_stack;
  Map *define_map = cond_arg->define_map;
  TokenIterator *ts = cond_arg->ts;

  while (true) {
    Token *token = ts_pop(ts);
    if (token == NULL) {
      return false;
    }

    switch (token->ty) {
    case TK_PP_IF: {
      TokenVector *tokens = token->pp_if.tokens;
      bool fullfilled = true;
      if (pp_cond_fullfilled(cond_stack)) {
        fullfilled = read_pp_if_cond(define_map, tokens, token->range);
      }
      pp_if(cond_stack, fullfilled, token->range);
      continue;
    }
    case TK_PP_ELIF: {
      TokenVector *tokens = token->pp_elif.tokens;
      bool fullfilled = true;
      if (pp_outer_cond_fullfilled(cond_stack)) {
        fullfilled = read_pp_if_cond(define_map, tokens, token->range);
      }
      pp_elif(cond_stack, fullfilled, token->range);
      continue;
    }
    case TK_PP_IFDEF: {
      const char *ident = token->pp_ifdef.ident;
      bool defined = map_get(define_map, ident) != NULL;
      pp_if(cond_stack, defined, token->range);
      continue;
    }
    case TK_PP_IFNDEF: {
      const char *ident = token->pp_ifndef.ident;
      bool defined = map_get(define_map, ident) != NULL;
      pp_if(cond_stack, !defined, token->range);
      continue;
    }
    case TK_PP_ELSE: {
      pp_else(cond_stack, token->range);
      continue;
    }
    case TK_PP_ENDIF: {
      pp_endif(cond_stack, token->range);
      continue;
    }
    case TK_EOF: {
      if (VEC_LEN(cond_stack) > 0) {
        Cond *cond = VEC_LAST(cond_stack);
        range_error(cond->range, "Unterminated conditional directive");
      }
      break;
    }
    default: {
      if (!pp_cond_fullfilled(cond_stack)) {
        continue;
      }
      break;
    }
    }

    VEC_PUSH(output, token);
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

static bool read_pp_if_cond(Map *define_map, TokenVector *tokens,
                            const Range *range) {
  if (VEC_LEN(tokens) == 0) {
    range_error(range, "expected value in expression");
  }
  Token *last = VEC_LAST(tokens);
  const Range *here = last->range;
  const Reader *reader = range_get_reader(here);
  Token *eof_token = new_token(TK_EOF, here);
  VEC_PUSH(tokens, eof_token);

  // convert `defined(IDENT)` or `defined INDET` into 0 or 1
  tokens = pp_convert_defined(define_map, tokens);

  // expand macros
  tokens = pp_expand_macros(define_map, tokens, NULL);

  for (int i = 0; i < VEC_LEN(tokens); i++) {
    // replace all ident tokens (including keyword ident) into '0'
    Token *tk = VEC_GET(tokens, i);
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

static TokenVector *pp_convert_defined(Map *define_map, TokenVector *tokens) {
  TokenVector *converted = NEW_VECTOR(TokenVector);
  while (VEC_LEN(tokens) > 0) {
    Token *token = VEC_REMOVE(tokens, 0);
    if (token->ty != TK_PP_IDENT || strcmp(token->pp_ident, "defined") != 0) {
      VEC_PUSH(converted, token);
      continue;
    }
    const Range *def_start = token->range;

    bool has_paren = false;
    token = VEC_REMOVE(tokens, 0);
    if (token->ty == '(') {
      has_paren = true;
      token = VEC_REMOVE(tokens, 0);
    }

    if (token->ty != TK_PP_IDENT) {
      range_error(token->range, "識別子がありません");
    }
    bool defined = map_get(define_map, token->pp_ident) != NULL;
    char *num = format("%d", defined);
    Token *num_token =
        new_token_pp_num(num, range_join(def_start, token->range));
    if (has_paren) {
      token = VEC_REMOVE(tokens, 0);
      if (token->ty != ')') {
        range_error(token->range, "`)` がありません");
      }
    }
    VEC_PUSH(converted, num_token);
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

static bool control_directive(void *arg, TokenVector *output) {
  ControlArg *ctrl_arg = arg;
  Reader *reader = ctrl_arg->reader;
  Map *define_map = ctrl_arg->define_map;
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
      TokenVector *tokens = token->pp_include.tokens;
      tokens = pp_expand_macros(define_map, tokens, NULL);
      if (VEC_LEN(tokens) == 0) {
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
      TokenVector *replacements = token->pp_define.replacements;
      Macro *macro;
      if (params != NULL) {
        macro = new_func_macro(params, has_varargs, replacements);
      } else {
        macro = new_obj_macro(replacements);
      }
      map_put(define_map, ident, macro);
      continue;
    }
    case TK_PP_UNDEF: {
      const char *ident = token->pp_undef.ident;
      map_remove(define_map, ident);
      continue;
    }
    case TK_PP_ERROR: {
      const char *message = token->pp_error.message;
      range_error(token->range, "#error %s", message);
      continue;
    }
    case TK_PP_LINE: {
      TokenVector *tokens = token->pp_line.tokens;
      tokens = pp_expand_macros(define_map, tokens, NULL);
      if (VEC_LEN(tokens) == 0) {
        range_error(token->range,
                    "#line directive requires a positive integer argument");
      }
      Token *num = VEC_GET(tokens, 0);
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
      if (VEC_LEN(tokens) > 1) {
        Token *str = VEC_GET(tokens, 1);
        if (str->ty != TK_STR) {
          range_error(str->range, "invalid filename for #line directive");
        }
        filename = str->str;
      }
      if (VEC_LEN(tokens) > 2) {
        Token *token = VEC_GET(tokens, 2);
        range_warn(token->range, "extra tokens at end of #line directive");
      }

      reader_set_position(reader, &line, filename);
      continue;
    }
    case TK_PP_UNKNOWN: {
      range_error(token->range, "invalid preprocessor directive");
    }
    default:
      break;
    }

    VEC_PUSH(output, token);
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

static bool expand_macro(void *arg, TokenVector *output) {
  ExpandMacroArg *expand_arg = arg;
  Map *define_map = expand_arg->define_map;
  TokenIterator *ts = expand_arg->ts;

  Token *token = ts_pop(ts);
  if (token == NULL) {
    return false;
  }

  TokenVector *tokens = NEW_VECTOR(TokenVector);
  VEC_PUSH(tokens, token);
  tokens = pp_expand_macros(define_map, tokens, ts);
  VEC_APPEND(output, tokens);
  return true;
}

static TokenVector *pp_expand_macros(Map *define_map, TokenVector *tokens,
                                     TokenIterator *ts) {
  TokenVector *expanded = NEW_VECTOR(TokenVector);
  while (VEC_LEN(tokens) > 0) {
    Token *ident = VEC_REMOVE(tokens, 0);
    if (ident->ty != TK_PP_IDENT) {
      VEC_PUSH(expanded, ident);
      continue;
    }
    if (pp_is_token_in_hideset(ident)) {
      VEC_PUSH(expanded, ident);
      continue;
    }

    Macro *macro = map_get(define_map, ident->pp_ident);
    if (macro == NULL) {
      VEC_PUSH(expanded, ident);
      continue;
    }

    if (macro->kind == MACRO_OBJ || macro->kind == MACRO_OBJ_SPECIAL) {
      const Range *expanded_from = ident->range;
      Set *hideset = new_set();
      set_insert(hideset, ident->pp_ident);
      TokenVector *replacement = macro->kind == MACRO_OBJ
                                     ? VEC_CLONE(macro->replacement)
                                     : macro->replacement_func(ident);
      TokenVector *exp_tokens = pp_expand_macros(
          define_map,
          pp_subst_macros(define_map, expanded_from, replacement, NULL, NULL,
                          hideset, NEW_VECTOR(TokenVector)),
          ts);
      VEC_APPEND(expanded, exp_tokens);
      continue;
    }

    assert(macro->kind == MACRO_FUNC);
    if (VEC_LEN(tokens) == 0 && ts != NULL) {
      Token *token = ts_pop(ts);
      VEC_PUSH(tokens, token);
    }
    if (VEC_LEN(tokens) == 0 || (VEC_FIRST(tokens))->ty != '(') {
      VEC_PUSH(expanded, ident);
      continue;
    }

    Token *rparen = NULL;
    TokenListVector *arguments =
        pp_read_macro_func_arg(macro, tokens, &rparen, ts);
    Set *hideset = pp_hideset_intersection(ident, rparen);
    set_insert(hideset, ident->pp_ident);
    const Range *expanded_from = range_join(ident->range, rparen->range);
    TokenVector *exp_tokens = pp_expand_macros(
        define_map,
        pp_subst_macros(define_map, expanded_from,
                        VEC_CLONE(macro->replacement), macro->params, arguments,
                        hideset, NEW_VECTOR(TokenVector)),
        ts);
    VEC_APPEND(expanded, exp_tokens);
  }
  return expanded;
}

static TokenVector *pp_subst_macros(Map *define_map, const Range *expanded_from,
                                    TokenVector *input, StrVector *params,
                                    TokenListVector *arguments, Set *hideset,
                                    TokenVector *output) {
  while (VEC_LEN(input) > 0) {
    Token *token = VEC_REMOVE(input, 0);
    if (token->ty == '#') {
      Token *ident = VEC_REMOVE(input, 0);
      TokenVector *arg = pp_get_func_arg(params, arguments, ident);
      if (arg == NULL) {
        range_error(ident->range,
                    "`#` の後がマクロのパラメーターではありません");
      }
      VEC_PUSH(output, pp_stringize(arg, ident->range));
      continue;
    }

    if (token->ty == TK_HASH_HASH) {
      Token *ident = VEC_REMOVE(input, 0);
      TokenVector *arg = pp_get_func_arg(params, arguments, ident);
      if (arg != NULL) {
        // NonStandard/GNU: , ## __VA_ARGS__
        if (ident->ty == TK_PP_IDENT &&
            strcmp(ident->pp_ident, "__VA_ARGS__") == 0 &&
            VEC_LEN(output) > 0 && (VEC_LAST(output))->ty == ',') {
          if (VEC_LEN(arg) == 0) {
            VEC_POP(output);
          } else {
            VEC_APPEND(output, VEC_CLONE(arg));
          }
          continue;
        }

        if (VEC_LEN(arg) == 0) {
          continue;
        }

        pp_glue(output, VEC_CLONE(arg));
        continue;
      }

      TokenVector *is = NEW_VECTOR(TokenVector);
      VEC_PUSH(is, ident);
      pp_glue(output, is);
      continue;
    }

    TokenVector *arg = pp_get_func_arg(params, arguments, token);
    if (arg != NULL) {
      Token *hash_hash = VEC_LEN(input) > 1 ? VEC_GET(input, 0) : NULL;
      if (hash_hash != NULL && hash_hash->ty == TK_HASH_HASH) {
        if (VEC_LEN(arg) == 0) {
          Token *next = VEC_GET(input, 0);
          TokenVector *arg2 = pp_get_func_arg(params, arguments, next);
          if (arg2 != NULL) {
            VEC_REMOVE(input, 0);
            VEC_APPEND(output, VEC_CLONE(arg2));
            continue;
          }
          continue;
        }
        VEC_APPEND(output, VEC_CLONE(arg));
        continue;
      }
      VEC_APPEND(output, pp_expand_macros(define_map, VEC_CLONE(arg), NULL));
      continue;
    }
    VEC_PUSH(output, token);
  }

  return pp_hsadd(expanded_from, hideset, output);
}

static TokenVector *pp_get_func_arg(StrVector *params,
                                    TokenListVector *arguments, Token *token) {
  if (token->ty != TK_PP_IDENT || params == NULL) {
    return NULL;
  }
  int i;
  for (i = 0; i < VEC_LEN(params); i++) {
    const char *key = VEC_GET(params, i);
    if (strcmp(key, token->pp_ident) == 0) {
      TokenVector *arg = VEC_GET(arguments, i);
      assert(arg != NULL);
      return arg;
    }
  }
  return NULL;
}

static TokenListVector *pp_read_macro_func_arg(Macro *macro,
                                               TokenVector *tokens,
                                               Token **rparen,
                                               TokenIterator *ts) {
  assert(macro->kind == MACRO_FUNC);
  assert(VEC_FIRST(tokens)->ty == '(');

  const Range *range = VEC_FIRST(tokens)->range;

  TokenListVector *arguments = NEW_VECTOR(TokenListVector);
  TokenVector *current_arg = NULL;
  int nest = 0;
  do {
    Token *token;
    if (VEC_LEN(tokens) == 0 && ts != NULL) {
      token = ts_pop(ts);
    } else {
      token = VEC_REMOVE(tokens, 0);
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
         VEC_LEN(arguments) < VEC_LEN(macro->params) - 1)) {
      if (current_arg == NULL) {
        current_arg = NEW_VECTOR(TokenVector);
      }
      VEC_PUSH(arguments, current_arg);
      current_arg = NEW_VECTOR(TokenVector);
      continue;
    }

    if (current_arg == NULL) {
      current_arg = NEW_VECTOR(TokenVector);
    }
    VEC_PUSH(current_arg, token);
  } while (nest > 0);

  if (current_arg != NULL) {
    VEC_PUSH(arguments, current_arg);
  }
  if (macro->has_varargs && VEC_LEN(arguments) == VEC_LEN(macro->params) - 1) {
    VEC_PUSH(arguments, NEW_VECTOR(TokenVector));
  }
  if (VEC_LEN(arguments) == 0 && VEC_LEN(macro->params) == 1) {
    VEC_PUSH(arguments, NEW_VECTOR(TokenVector));
  }

  if (VEC_LEN(macro->params) != VEC_LEN(arguments)) {
    range_error(range,
                "関数マクロの引数の個数が一致しません, 仮引数: %d, 引数: %d",
                VEC_LEN(macro->params), VEC_LEN(arguments));
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

static TokenVector *pp_hsadd(const Range *expanded_from, Set *hideset,
                             TokenVector *output) {
  for (int i = 0; i < VEC_LEN(output); i++) {
    Token *token = token_clone(VEC_GET(output, i), expanded_from);
    VEC_SET(output, i, token);

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

static Token *pp_stringize(TokenVector *arg, bool quote) {
  const Range *range = NULL;
  String *str = new_string();
  if (quote) {
    str_push(str, '"');
  }
  for (int i = 0; i < VEC_LEN(arg); i++) {
    Token *token = VEC_GET(arg, i);
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

static void pp_glue(TokenVector *ls, TokenVector *rs) {
  Token *l = VEC_POP(ls);
  Token *r = VEC_REMOVE(rs, 0);

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

  VEC_PUSH(ls, token);
  VEC_APPEND(ls, rs);
}
