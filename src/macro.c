#include "gifcc.h"
#include <time.h>

static void set_predefined_num_macro(Map *define_map,
                                     const Range *builtin_range,
                                     const char *name, const char *num);
static void
set_predefined_special_macro(Map *define_map, const char *name,
                             special_macro_handler_t *replacement_func);
static Vector *macro_date(Token *token);
static Vector *macro_time(Token *token);
static Vector *macro_file(Token *token);
static Vector *macro_line(Token *token);

Macro *new_obj_macro(Vector *replacement) {
  Macro *macro = NEW(Macro);
  macro->kind = MACRO_OBJ;
  macro->replacement = replacement;
  return macro;
}

Macro *new_obj_special_macro(special_macro_handler_t *replacement_func) {
  Macro *macro = NEW(Macro);
  macro->kind = MACRO_OBJ_SPECIAL;
  macro->replacement_func = replacement_func;
  return macro;
}

Macro *new_func_macro(StrVector *params, bool has_varargs,
                      Vector *replacement) {
  Macro *macro = NEW(Macro);
  macro->kind = MACRO_FUNC;
  macro->params = params;
  macro->has_varargs = has_varargs;
  macro->replacement = replacement;
  return macro;
}

void initialize_predefined_macro(Map *define_map, const Range *builtin_range) {
  set_predefined_special_macro(define_map, "__DATE__", macro_date);
  set_predefined_special_macro(define_map, "__TIME__", macro_time);
  set_predefined_special_macro(define_map, "__FILE__", macro_file);
  set_predefined_special_macro(define_map, "__LINE__", macro_line);

  set_predefined_num_macro(define_map, builtin_range, "__STDC__", "1");
  set_predefined_num_macro(define_map, builtin_range, "__STDC_HOSTED__", "1");
  set_predefined_num_macro(define_map, builtin_range, "__STDC_VERSION__",
                           "201112L");
  set_predefined_num_macro(define_map, builtin_range, "__LP64__", "1");
  set_predefined_num_macro(define_map, builtin_range, "__x86_64__", "1");
}

static void set_predefined_num_macro(Map *define_map,
                                     const Range *builtin_range,
                                     const char *name, const char *num) {
  Vector *replacement = new_vector();
  vec_push(replacement, new_token_pp_num(num, builtin_range));
  map_put(define_map, name, new_obj_macro(replacement));
}

static void
set_predefined_special_macro(Map *define_map, const char *name,
                             special_macro_handler_t *replacement_func) {
  map_put(define_map, name, new_obj_special_macro(replacement_func));
}

static Vector *macro_date(Token *token) {
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
  vec_push(rep,
           new_token_pp_str(strdup(buf),
                            range_builtin(range_get_reader(token->range))));
  return rep;
}

static Vector *macro_time(Token *token) {
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
  vec_push(rep,
           new_token_pp_str(strdup(buf),
                            range_builtin(range_get_reader(token->range))));
  return rep;
}

static Vector *macro_file(Token *token) {
  const char *filename;
  range_get_start(token->range, &filename, NULL, NULL);

  Vector *rep = new_vector();
  vec_push(rep,
           new_token_pp_str(format("\"%s\"", filename),
                            range_builtin(range_get_reader(token->range))));
  return rep;
}

static Vector *macro_line(Token *token) {
  int line;
  range_get_start(token->range, NULL, &line, NULL);

  Vector *rep = new_vector();
  vec_push(rep,
           new_token_pp_num(format("%d", line),
                            range_builtin(range_get_reader(token->range))));
  return rep;
}
