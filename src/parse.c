#include "gifcc.h"
#include <assert.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct Decl {
  Type *type;
  StackVar *stack_var;
} Decl;

typedef struct Scope {
  Map *decl_map;
  Map *typedef_map;
  Map *tag_map;
  struct Scope *outer;
} Scope;

typedef struct GlobalCtxt {
  Tokenizer *tokenizer;
  Map *var_map;
  Vector *str_list;
  Scope *scope;
} GlobalCtxt;

typedef struct FuncCtxt {
  GlobalCtxt *global;
  Tokenizer *tokenizer;
  char *name;
  int stack_size;
  Vector *switches;
  Map *label_map;
} FuncCtxt;

typedef struct LocalCtxt {
  GlobalCtxt *global;
  FuncCtxt *func;
  struct LocalCtxt *outer;
  Tokenizer *tokenizer;
  Scope *scope;
} LocalCtxt;

static GlobalCtxt *new_global_ctxt(Tokenizer *tokenizer);
static FuncCtxt *new_func_ctxt(GlobalCtxt *gcx, char *name);
static LocalCtxt *new_root_local_ctxt(FuncCtxt *fcx);
static LocalCtxt *new_inner_local_ctxt(LocalCtxt *outer);
static Scope *new_scope(Scope *outer);
static StackVar *register_stack(LocalCtxt *lcx, char *name, Type *type,
                                Range range);
static bool register_member(Tokenizer *tokenizer, Type *type, char *member_name,
                            Type *member_type, Range range);
static bool register_decl(Scope *scope, char *name, Type *type, StackVar *svar);
static Decl *get_decl(Scope *scope, char *name);
static bool register_tag(Scope *scope, char *tag, Type *type);
static Type *get_tag(Scope *scope, char *tag);
static bool register_typedef(Scope *scope, char *name, Type *type);
static Type *get_typedef(Scope *scope, char *name);
static bool is_sametype(Type *ty1, Type *ty2);
static bool is_integer_type(Type *ty);
static bool is_arith_type(Type *ty);
static bool is_ptr_type(Type *ty);
static bool is_array_type(Type *ty);
static Type *integer_promoted(LocalCtxt *lcx, Expr **e);
static Type *arith_converted(LocalCtxt *lcx, Expr **e1, Expr **e2);
static bool token_is_typename(Scope *scope, Token *token);
static noreturn void binop_type_error_raw(LocalCtxt *lcx, int ty, Expr *lhs,
                                          Expr *rhs, const char *dbg_file,
                                          int dbg_line);
static Type *new_type(int ty);
static Type *new_type_ptr(Type *base_type);
static Type *new_type_array(Type *base_type, int len);
static Type *new_type_func(Type *ret_type, Vector *func_param);
static Type *new_type_struct(int tk, char *tag);
static Type *new_type_anon_struct(int tk);
static Expr *coerce_array2ptr(LocalCtxt *lcx, Expr *expr);
static Expr *new_expr(int ty, Type *val_type, Range range);
static Expr *new_expr_num(int val, Range range);
static Expr *new_expr_ident(LocalCtxt *lcx, char *name, Range range);
static Expr *new_expr_str(LocalCtxt *lcx, char *val, Range range);
static Expr *new_expr_call(LocalCtxt *lcx, Expr *callee, Vector *argument,
                           Range range);
static Expr *new_expr_postfix(LocalCtxt *lcx, int ty, Expr *operand,
                              Range range);
static Expr *new_expr_cast(LocalCtxt *lcx, Type *val_type, Expr *operand,
                           Range range);
static Expr *new_expr_unary(LocalCtxt *lcx, int ty, Expr *operand, Range range);
static Expr *new_expr_binop(LocalCtxt *lcx, int ty, Expr *lhs, Expr *rhs,
                            Range range);
static Expr *new_expr_cond(LocalCtxt *lcx, Expr *cond, Expr *then_expr,
                           Expr *else_expr, Range range);
static Expr *new_expr_index(LocalCtxt *lcx, Expr *array, Expr *index,
                            Range range);
static Expr *new_expr_dot(LocalCtxt *lcx, Expr *operand, char *name,
                          Range range);
static Expr *new_expr_arrow(LocalCtxt *lcx, Expr *operand, char *name,
                            Range range);
static Stmt *new_stmt(int ty, Range range);
static Stmt *new_stmt_expr(Expr *expr, Range range);
static Stmt *new_stmt_if(Expr *cond, Stmt *then_stmt, Stmt *else_stmt,
                         Range range);
static Stmt *new_stmt_switch(Expr *cond, Stmt *body, Range range);
static Stmt *new_stmt_case(Expr *expr, Range range);
static Stmt *new_stmt_default(Range range);
static Stmt *new_stmt_label(FuncCtxt *fcx, char *name, Range range);
static Stmt *new_stmt_while(Expr *cond, Stmt *body, Range range);
static Stmt *new_stmt_do_while(Expr *cond, Stmt *body, Range range);
static Stmt *new_stmt_for(Expr *init, Expr *cond, Expr *inc, Stmt *body,
                          Range range);
static Stmt *new_stmt_goto(char *name, Range range);
static Stmt *new_stmt_return(Expr *expr, Range range);
static Stmt *new_stmt_compound(Vector *stmts, Range range);

// expression
static Expr *primary_expression(LocalCtxt *lcx);
static Expr *postfix_expression(LocalCtxt *lcx);
static Vector *argument_expression_list(LocalCtxt *lcx);
static Expr *unary_expression(LocalCtxt *lcx);
static Expr *cast_expression(LocalCtxt *lcx);
static Expr *multiplicative_expression(LocalCtxt *lcx);
static Expr *additive_expression(LocalCtxt *lcx);
static Expr *shift_expression(LocalCtxt *lcx);
static Expr *relational_expression(LocalCtxt *lcx);
static Expr *equality_expression(LocalCtxt *lcx);
static Expr *and_expression(LocalCtxt *lcx);
static Expr *exclusive_or_expression(LocalCtxt *lcx);
static Expr *inclusive_or_expression(LocalCtxt *lcx);
static Expr *logical_and_expression(LocalCtxt *lcx);
static Expr *logical_or_expression(LocalCtxt *lcx);
static Expr *conditional_expression(LocalCtxt *lcx);
static Expr *assignment_expression(LocalCtxt *lcx);
static Expr *expression(LocalCtxt *lcx);
static Expr *constant_expression(LocalCtxt *lcx);

// declaration
static void declaration(LocalCtxt *lcx);
static Type *type_specifier(Scope *scope, Tokenizer *tokenizer);
static void struct_declaration(Scope *scope, Tokenizer *tokenizer, Type *type);
static Type *struct_or_union_specifier(Scope *scope, Tokenizer *tokenizer,
                                       Token *token);
static Type *type_name(Scope *scope, Tokenizer *tokenizer);
static void declarator(Scope *scope, Tokenizer *tokenizer, Type *base_type,
                       char **name, Type **type, Range *range);
static void direct_declarator(Scope *scope, Tokenizer *tokenizer,
                              Type *base_type, char **name, Type **type,
                              Range *range);

// statement
static Stmt *statement(LocalCtxt *lcx);
static Stmt *compound_statement(LocalCtxt *lcx);

// top-level
static Function *function_definition(GlobalCtxt *gcx, Type *type, char *name,
                                     Range start);
static GlobalVar *new_global_variable(Type *type, char *name, Range range);
static TranslationUnit *translation_unit(Tokenizer *tokenizer);

static Stmt null_stmt = {
    .ty = ST_NULL,
};

TranslationUnit *parse(Reader *reader) {
  Tokenizer *tokenizer = new_tokenizer(reader);
  return translation_unit(tokenizer);
}

noreturn void expr_error_raw(const Reader *reader, const Expr *expr,
                             const char *dbg_file, int dbg_line, char *fmt,
                             ...) {
  va_list ap;
  va_start(ap, fmt);
  reader_error_range_raw_v(reader, expr->range, dbg_file, dbg_line, fmt, ap);
}

noreturn void stmt_error_raw(const Reader *reader, const Stmt *stmt,
                             const char *dbg_file, int dbg_line, char *fmt,
                             ...) {
  va_list ap;
  va_start(ap, fmt);
  reader_error_range_raw_v(reader, stmt->range, dbg_file, dbg_line, fmt, ap);
}

#define local_error(lcx, range, fmt, ...)                                      \
  local_error_raw((lcx), (range), __FILE__, __LINE__, (fmt), ##__VA_ARGS__)
static noreturn void local_error_raw(const LocalCtxt *lcx, Range range,
                                     const char *dbg_file, int dbg_line,
                                     char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  reader_error_range_raw_v(token_get_reader(lcx->tokenizer), range, dbg_file,
                           dbg_line, fmt, ap);
}

static GlobalCtxt *new_global_ctxt(Tokenizer *tokenizer) {
  GlobalCtxt *gcx = malloc(sizeof(GlobalCtxt));
  gcx->tokenizer = tokenizer;
  gcx->var_map = new_map();
  gcx->str_list = new_vector();
  gcx->scope = new_scope(NULL);

  return gcx;
}

static FuncCtxt *new_func_ctxt(GlobalCtxt *gcx, char *name) {
  FuncCtxt *fcx = malloc(sizeof(FuncCtxt));

  fcx->global = gcx;
  fcx->tokenizer = gcx->tokenizer;
  fcx->name = name;
  fcx->stack_size = 0;
  fcx->switches = new_vector();
  fcx->label_map = new_map();

  return fcx;
}

static LocalCtxt *new_root_local_ctxt(FuncCtxt *fcx) {
  LocalCtxt *lcx = malloc(sizeof(LocalCtxt));

  lcx->global = fcx->global;
  lcx->func = fcx;
  lcx->outer = NULL;
  lcx->tokenizer = fcx->tokenizer;
  lcx->scope = new_scope(fcx->global->scope);

  return lcx;
}

static LocalCtxt *new_inner_local_ctxt(LocalCtxt *outer) {
  LocalCtxt *inner = malloc(sizeof(LocalCtxt));

  inner->global = outer->global;
  inner->func = outer->func;
  inner->outer = outer;
  inner->tokenizer = outer->tokenizer;
  inner->scope = new_scope(outer->scope);

  return inner;
}

static Scope *new_scope(Scope *outer) {
  Scope *scope = malloc(sizeof(Scope));
  scope->decl_map = new_map();
  scope->typedef_map = new_map();
  scope->tag_map = new_map();
  scope->outer = outer;
  return scope;
}

static StackVar *register_stack(LocalCtxt *lcx, char *name, Type *type,
                                Range range) {
  if (type->ty == TY_VOID) {
    local_error(lcx, range, "void型の変数は定義できません: %s", name);
  }

  FuncCtxt *fcx = lcx->func;

  StackVar *var = malloc(sizeof(StackVar));
  fcx->stack_size = align(fcx->stack_size, get_val_align(type));
  var->offset = fcx->stack_size;
  var->type = type;
  var->range = range;
  fcx->stack_size += get_val_size(type);

  if (!register_decl(lcx->scope, name, type, var)) {
    local_error(lcx, range, "同じ名前のローカル変数が複数あります: %s", name);
  }

  return var;
}

static bool register_member(Tokenizer *tokenizer, Type *type, char *member_name,
                            Type *member_type, Range range) {
  assert(type->ty == TY_STRUCT || type->ty == TY_UNION);

  if (member_type->ty == TY_VOID) {
    reader_error_range(token_get_reader(tokenizer), range,
                       "void型のメンバーです: %s", member_name);
  }
  if (map_get(type->members, member_name)) {
    return false;
  }

  Member *member = malloc(sizeof(Member));
  member->name = member_name;
  member->type = member_type;

  if (type->ty == TY_STRUCT) {
    type->member_size = align(type->member_size, get_val_align(member_type));
    member->offset = type->member_size;
    type->member_size += get_val_size(member_type);
  } else {
    if (get_val_size(member_type) > type->member_size) {
      type->member_size = get_val_size(member_type);
    }
    member->offset = 0;
  }

  if (type->member_align < get_val_align(member_type)) {
    type->member_align = get_val_align(member_type);
  }

  map_put(type->members, member_name, member);
  return true;
}

static bool register_decl(Scope *scope, char *name, Type *type,
                          StackVar *svar) {
  if (map_get(scope->decl_map, name)) {
    return false;
  }
  Decl *decl = malloc(sizeof(Decl));
  decl->type = type;
  decl->stack_var = svar;
  map_put(scope->decl_map, name, decl);
  return true;
}

static Decl *get_decl(Scope *scope, char *name) {
  while (scope != NULL) {
    Decl *decl = map_get(scope->decl_map, name);
    if (decl) {
      return decl;
    }
    scope = scope->outer;
  }
  return NULL;
}

static bool register_tag(Scope *scope, char *tag, Type *type) {
  if (map_get(scope->tag_map, tag)) {
    return false;
  }
  map_put(scope->tag_map, tag, type);
  return true;
}

static Type *get_tag(Scope *scope, char *tag) {
  while (scope != NULL) {
    Type *type = map_get(scope->tag_map, tag);
    if (type) {
      return type;
    }
    scope = scope->outer;
  }
  return NULL;
}

static bool register_typedef(Scope *scope, char *name, Type *type) {
  if (map_get(scope->typedef_map, name)) {
    return false;
  }
  map_put(scope->typedef_map, name, type);
  return true;
}

static Type *get_typedef(Scope *scope, char *name) {
  while (scope != NULL) {
    Type *type = map_get(scope->typedef_map, name);
    if (type) {
      return type;
    }
    scope = scope->outer;
  }
  return NULL;
}

static bool is_sametype(Type *ty1, Type *ty2) {
  if (ty1->ty != ty2->ty) {
    return false;
  }
  if (ty1->ty == TY_PTR) {
    return is_sametype(ty1->ptrof, ty2->ptrof);
  }
  return true;
}

static bool is_integer_type(Type *ty) {
  return ty->ty == TY_INT || ty->ty == TY_CHAR;
}
static bool is_arith_type(Type *ty) { return is_integer_type(ty); }
static bool is_ptr_type(Type *ty) { return ty->ty == TY_PTR; }
static bool is_array_type(Type *ty) { return ty->ty == TY_ARRAY; }
static bool is_func_type(Type *ty) { return ty->ty == TY_FUNC; }
static Type *integer_promoted(LocalCtxt *lcx, Expr **e) {
  if (!is_integer_type((*e)->val_type)) {
    return NULL;
  }
  // CHAR は INT へ昇格する
  if ((*e)->val_type->ty == TY_CHAR) {
    *e = new_expr_cast(lcx, new_type(TY_INT), *e, (*e)->range);
  }
  return (*e)->val_type;
}
static Type *arith_converted(LocalCtxt *lcx, Expr **e1, Expr **e2) {
  if (!is_arith_type((*e1)->val_type) || !is_arith_type((*e2)->val_type)) {
    return NULL;
  }
  Type *ty1 = integer_promoted(lcx, e2);
  Type *ty2 = integer_promoted(lcx, e1);
  assert(is_sametype(ty1, ty2));
  return ty1;
}

static bool token_is_typename(Scope *scope, Token *token) {
  switch (token->ty) {
  case TK_VOID:
  case TK_INT:
  case TK_CHAR:
  case TK_STRUCT:
  case TK_UNION:
    return true;
  case TK_IDENT:
    return get_typedef(scope, token->name);
  default:
    return false;
  }
}

int get_val_size(Type *ty) {
  switch (ty->ty) {
  case TY_VOID:
    return sizeof(void);
  case TY_CHAR:
    return sizeof(char);
  case TY_INT:
    return sizeof(int);
  case TY_PTR:
    return sizeof(void *);
  case TY_ARRAY:
    return get_val_size(ty->ptrof) * ty->array_len;
  case TY_FUNC:
    error("関数型の値サイズを取得しようとしました");
  case TY_STRUCT:
  case TY_UNION:
    if (ty->members == NULL) {
      error("不完全型の値のサイズを取得しようとしました");
    }
    return align(ty->member_size, ty->member_align);
  }
  error("不明な型のサイズを取得しようとしました");
}

int get_val_align(Type *ty) {
  switch (ty->ty) {
  case TY_VOID:
    return alignof(void);
  case TY_CHAR:
    return alignof(char);
  case TY_INT:
    return alignof(int);
  case TY_PTR:
    return alignof(void *);
  case TY_ARRAY:
    return get_val_align(ty->ptrof);
  case TY_FUNC:
    error("関数型の値アラインメントを取得しようとしました");
  case TY_STRUCT:
  case TY_UNION:
    return ty->member_align;
  }
  error("不明な型の値アラインメントを取得しようとしました");
}

#define binop_type_error(lcx, ty, lhs, rhs)                                    \
  binop_type_error_raw((lcx), (ty), (lhs), (rhs), __FILE__, __LINE__)
static noreturn void binop_type_error_raw(LocalCtxt *lcx, int ty, Expr *lhs,
                                          Expr *rhs, const char *dbg_file,
                                          int dbg_line) {
  local_error_raw(lcx, range_join(lhs->range, rhs->range), dbg_file, dbg_line,
                  "不正な型の値に対する演算です: 演算=%d(%c), 左辺=%d, 右辺=%d",
                  ty, ty, lhs->val_type->ty, rhs->val_type->ty);
}

static Type *new_type(int ty) {
  Type *type = malloc(sizeof(Type));
  type->ty = ty;
  return type;
}

static Type *new_type_ptr(Type *base_type) {
  Type *ptrtype = malloc(sizeof(Type));
  ptrtype->ty = TY_PTR;
  ptrtype->ptrof = base_type;
  return ptrtype;
}

static Type *new_type_array(Type *base_type, int len) {
  Type *ptrtype = malloc(sizeof(Type));
  ptrtype->ty = TY_ARRAY;
  ptrtype->ptrof = base_type;
  ptrtype->array_len = len;
  return ptrtype;
}

static Type *new_type_func(Type *ret_type, Vector *func_param) {
  Type *funtype = malloc(sizeof(Type));
  funtype->ty = TY_FUNC;
  funtype->func_ret = ret_type;
  funtype->func_param = func_param;
  return funtype;
}

static Type *new_type_struct(int tk, char *tag) {
  assert(tk == TK_STRUCT || tk == TK_UNION);
  Type *type = malloc(sizeof(Type));
  type->ty = tk == TK_STRUCT ? TY_STRUCT : TY_UNION;
  type->tag = tag;
  type->members = new_map();
  type->member_size = 0;
  type->member_align = 0;
  return type;
}

static Type *new_type_anon_struct(int tk) {
  assert(tk == TK_STRUCT || tk == TK_UNION);
  Type *type = malloc(sizeof(Type));
  type->ty = tk == TK_STRUCT ? TY_STRUCT : TY_UNION;
  type->members = NULL;
  return type;
}

static Expr *coerce_array2ptr(LocalCtxt *lcx, Expr *expr) {
  if (is_array_type(expr->val_type)) {
    return new_expr_unary(lcx, '&', expr, expr->range);
  }
  return expr;
}

static Expr *new_expr(int ty, Type *val_type, Range range) {
  Expr *expr = malloc(sizeof(Expr));
  expr->ty = ty;
  expr->val_type = val_type;
  expr->range = range;
  return expr;
}

static Expr *new_expr_num(int val, Range range) {
  Expr *expr = new_expr(EX_NUM, new_type(TY_INT), range);
  expr->val = val;
  return expr;
}

static Expr *new_expr_ident(LocalCtxt *lcx, char *name, Range range) {
  int ty;
  Type *type;
  if (strcmp(name, "__func__") == 0) {
    if (lcx->func == NULL) {
      local_error(lcx, range, "関数外で__func__が使用されました");
    }
    return new_expr_str(lcx, lcx->func->name, range);
  }
  Decl *decl = get_decl(lcx->scope, name);
  StackVar *svar;
  if (decl != NULL) {
    if (decl->stack_var != NULL) {
      ty = EX_STACK_VAR;
      type = decl->type;
      svar = decl->stack_var;
    } else {
      ty = EX_GLOBAL_VAR;
      type = decl->type;
      svar = NULL;
    }
  } else {
    // 未知の識別子はint型のグローバル変数として扱う
    ty = EX_GLOBAL_VAR;
    type = new_type(TY_INT);
    svar = NULL;
  }
  Expr *expr = new_expr(ty, type, range);
  expr->name = name;
  expr->stack_var = svar;
  return expr;
}

static Expr *new_expr_str(LocalCtxt *lcx, char *val, Range range) {
  Type *type = new_type_ptr(new_type(TY_CHAR));
  Expr *expr = new_expr(EX_STR, type, range);

  expr->name = make_label();
  StringLiteral *str = malloc(sizeof(StringLiteral));
  str->name = expr->name;
  str->val = val;
  vec_push(lcx->global->str_list, str);
  return expr;
}

static Expr *new_expr_call(LocalCtxt *lcx, Expr *callee, Vector *argument,
                           Range range) {
  callee = coerce_array2ptr(lcx, callee);
  if (argument != NULL) {
    for (int i = 0; i < argument->len; i++) {
      argument->data[i] = coerce_array2ptr(lcx, argument->data[i]);
    }
  }

  Type *ret_type;
  if (is_func_type(callee->val_type)) {
    ret_type = callee->val_type->func_ret;
  } else {
    // TODO: 定義のない関数の戻り値はintを仮定する
    ret_type = new_type(TY_INT);
  }
  Expr *expr = new_expr(EX_CALL, ret_type, range);
  expr->callee = callee;
  expr->argument = argument;
  return expr;
}

static Expr *new_expr_postfix(LocalCtxt *lcx, int ty, Expr *operand,
                              Range range) {
  operand = coerce_array2ptr(lcx, operand);

  Expr *expr = new_expr(ty, operand->val_type, range);
  expr->lhs = operand;
  expr->rhs = NULL;
  return expr;
}

static Expr *new_expr_cast(LocalCtxt *lcx, Type *val_type, Expr *operand,
                           Range range) {
  operand = coerce_array2ptr(lcx, operand);

  if (is_sametype(operand->val_type, val_type)) {
    return operand;
  }

  if (operand->ty == EX_NUM) {
    switch (val_type->ty) {
    case TY_VOID:
      return operand;
    case TY_INT:
      operand->val_type = val_type;
      operand->val = (int)operand->val;
      return operand;
    case TY_CHAR:
      operand->val_type = val_type;
      operand->val = (char)operand->val;
      return operand;
    case TY_PTR:
    case TY_ARRAY:
    case TY_FUNC:
    case TY_STRUCT:
    case TY_UNION:
      break;
    }
  }

  Expr *expr = new_expr(EX_CAST, val_type, range);
  expr->expr = operand;
  return expr;
}

static Expr *new_expr_unary(LocalCtxt *lcx, int ty, Expr *operand,
                            Range range) {
  if (ty != '&') {
    // & 以外は array は ptr とみなす
    operand = coerce_array2ptr(lcx, operand);
  }

  Type *val_type;
  if (ty == '&') {
    if (is_array_type(operand->val_type)) {
      val_type = new_type_ptr(operand->val_type->ptrof);
    } else {
      val_type = new_type_ptr(operand->val_type);
    }
  } else if (ty == '*') {
    if (operand->val_type->ty != TY_PTR) {
      local_error(lcx, range, "ポインタ型でない値に対するデリファレンスです");
    }
    val_type = operand->val_type->ptrof;
  } else {
    val_type = operand->val_type;
  }
  Expr *expr = new_expr(ty, val_type, range);
  expr->lhs = NULL;
  expr->rhs = operand;
  return expr;
}

static Expr *new_expr_binop(LocalCtxt *lcx, int ty, Expr *lhs, Expr *rhs,
                            Range range) {
  lhs = coerce_array2ptr(lcx, lhs);
  rhs = coerce_array2ptr(lcx, rhs);

  Type *val_type;
  switch (ty) {
  // multiplicative
  case '*':
  case '/':
  case '%':
    val_type = arith_converted(lcx, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(lcx, ty, lhs, rhs);
    }
    assert(val_type->ty == TY_INT);
    if (lhs->ty == EX_NUM && rhs->ty == EX_NUM) {
      switch (ty) {
      case '*':
        lhs->val *= rhs->val;
        return lhs;
      case '/':
        lhs->val /= rhs->val;
        return lhs;
      case '%':
        lhs->val %= rhs->val;
        return lhs;
      }
      assert(false);
    }
    break;
  // additive
  case '+':
    if (is_ptr_type(lhs->val_type)) {
      if (is_ptr_type(rhs->val_type) || !is_integer_type(rhs->val_type)) {
        binop_type_error(lcx, ty, lhs, rhs);
      }
      rhs = new_expr_binop(
          lcx, '*', rhs,
          new_expr_num(get_val_size(lhs->val_type->ptrof), range), range);
      val_type = lhs->val_type;
      break;
    }

    if (is_ptr_type(rhs->val_type)) {
      if (!is_integer_type(lhs->val_type)) {
        binop_type_error(lcx, ty, lhs, rhs);
      }
      lhs = new_expr_binop(
          lcx, '*', lhs,
          new_expr_num(get_val_size(rhs->val_type->ptrof), range), range);
      val_type = rhs->val_type;
      break;
    }

    val_type = arith_converted(lcx, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(lcx, ty, lhs, rhs);
    }
    assert(val_type->ty == TY_INT);
    if (lhs->ty == EX_NUM && rhs->ty == EX_NUM) {
      lhs->val += rhs->val;
      return lhs;
    }
    break;
  case '-':
    if (is_ptr_type(lhs->val_type)) {
      if (is_ptr_type(rhs->val_type)) {
        if (!is_sametype(lhs->val_type, rhs->val_type)) {
          binop_type_error(lcx, ty, lhs, rhs);
        }
        Expr *sub = new_expr('-', new_type(TY_INT), range);
        sub->lhs = lhs;
        sub->rhs = rhs;
        return new_expr_binop(
            lcx, '/', sub,
            new_expr_num(get_val_size(lhs->val_type->ptrof), range), range);
      }
      if (is_integer_type(rhs->val_type)) {
        rhs = new_expr_binop(
            lcx, '*', rhs,
            new_expr_num(get_val_size(lhs->val_type->ptrof), range), range);
        val_type = lhs->val_type;
        break;
      }
      binop_type_error(lcx, ty, lhs, rhs);
    }

    if (is_ptr_type(rhs->val_type)) {
      binop_type_error(lcx, ty, lhs, rhs);
    }

    val_type = arith_converted(lcx, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(lcx, ty, lhs, rhs);
    }
    assert(val_type->ty == TY_INT);
    if (lhs->ty == EX_NUM && rhs->ty == EX_NUM) {
      lhs->val -= rhs->val;
      return lhs;
    }
    break;
  // shift
  case EX_LSHIFT:
  case EX_RSHIFT:
    if (!is_integer_type(lhs->val_type) || !is_integer_type(rhs->val_type)) {
      binop_type_error(lcx, ty, lhs, rhs);
    }
    val_type = integer_promoted(lcx, &lhs);
    if (val_type == NULL) {
      binop_type_error(lcx, ty, lhs, rhs);
    }
    assert(val_type->ty == TY_INT);
    if (lhs->ty == EX_NUM && rhs->ty == EX_NUM) {
      switch (ty) {
      case EX_LSHIFT:
        lhs->val <<= rhs->val;
        return lhs;
      case EX_RSHIFT:
        lhs->val >>= rhs->val;
        return lhs;
      }
      assert(false);
    }
    break;

  case '<':
  case '>':
  case EX_LTEQ:
  case EX_GTEQ:
  case EX_EQEQ:
  case EX_NOTEQ:
    val_type = new_type(TY_INT);
    break;
  // and
  case '&':
  case '^':
  case '|':
    if (!is_integer_type(lhs->val_type) || !is_integer_type(rhs->val_type)) {
      binop_type_error(lcx, ty, lhs, rhs);
    }
    val_type = arith_converted(lcx, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(lcx, ty, lhs, rhs);
    }
    assert(val_type->ty == TY_INT);
    if (lhs->ty == EX_NUM && rhs->ty == EX_NUM) {
      switch (ty) {
      case '&':
        lhs->val &= rhs->val;
        return lhs;
      case '^':
        lhs->val ^= rhs->val;
        return lhs;
      case '|':
        lhs->val |= rhs->val;
        return lhs;
      }
      assert(false);
    }
    break;
  case EX_LOGAND:
  case EX_LOGOR:
    val_type = new_type(TY_INT);
    break;
  case '=':
    val_type = lhs->val_type;
    break;
  case ',':
    val_type = lhs->val_type;
    break;
  default:
    assert(false);
  }

  Expr *expr = new_expr(ty, val_type, range);
  expr->lhs = lhs;
  expr->rhs = rhs;
  return expr;
}

static Expr *new_expr_cond(LocalCtxt *lcx, Expr *cond, Expr *then_expr,
                           Expr *else_expr, Range range) {
  cond = coerce_array2ptr(lcx, cond);
  then_expr = coerce_array2ptr(lcx, then_expr);
  else_expr = coerce_array2ptr(lcx, else_expr);

  if (!is_sametype(then_expr->val_type, else_expr->val_type)) {
    local_error(lcx, range, "条件演算子の両辺の型が異なります: %d, %d",
                then_expr->val_type->ty, else_expr->val_type->ty);
  }
  Expr *expr = new_expr(EX_COND, then_expr->val_type, range);
  expr->cond = cond;
  expr->lhs = then_expr;
  expr->rhs = else_expr;
  return expr;
}

static Expr *new_expr_index(LocalCtxt *lcx, Expr *array, Expr *index,
                            Range range) {
  array = coerce_array2ptr(lcx, array);
  index = coerce_array2ptr(lcx, index);

  return new_expr_unary(lcx, '*', new_expr_binop(lcx, '+', array, index, range),
                        range);
}

static Expr *new_expr_dot(LocalCtxt *lcx, Expr *operand, char *name,
                          Range range) {
  if (operand->val_type->ty != TY_STRUCT && operand->val_type->ty != TY_UNION) {
    local_error(lcx, range, "構造体または共用体以外のメンバへのアクセスです");
  }
  return new_expr_arrow(lcx, new_expr_unary(lcx, '&', operand, range), name,
                        range);
}

static Expr *new_expr_arrow(LocalCtxt *lcx, Expr *operand, char *name,
                            Range range) {
  if (operand->val_type->ty != TY_PTR ||
      (operand->val_type->ptrof->ty != TY_STRUCT &&
       operand->val_type->ptrof->ty != TY_UNION)) {
    local_error(lcx, range, "構造体または共用体以外のメンバへのアクセスです");
  }
  Member *member = operand->val_type->ptrof->members
                       ? map_get(operand->val_type->ptrof->members, name)
                       : NULL;
  if (member == NULL) {
    local_error(lcx, range, "存在しないメンバへのアクセスです: %s", name);
  }
  Expr *expr = new_expr('+', new_type_ptr(member->type), range);
  expr->lhs = operand;
  expr->rhs = new_expr_num(member->offset, range);
  return new_expr_unary(lcx, '*', expr, range);
}

static Stmt *new_stmt(int ty, Range range) {
  Stmt *stmt = malloc(sizeof(Stmt));
  stmt->ty = ty;
  stmt->range = range;
  return stmt;
}

static Stmt *new_stmt_expr(Expr *expr, Range range) {
  Stmt *stmt = new_stmt(ST_EXPR, range);
  stmt->expr = expr;
  return stmt;
}

static Stmt *new_stmt_if(Expr *cond, Stmt *then_stmt, Stmt *else_stmt,
                         Range range) {
  Stmt *stmt = new_stmt(ST_IF, range);
  stmt->cond = cond;
  stmt->then_stmt = then_stmt;
  stmt->else_stmt = else_stmt;
  return stmt;
}

static Stmt *new_stmt_switch(Expr *cond, Stmt *body, Range range) {
  Stmt *stmt = new_stmt(ST_SWITCH, range);
  stmt->cond = cond;
  stmt->body = body;
  stmt->cases = new_vector();
  stmt->default_case = NULL;
  return stmt;
}

static Stmt *new_stmt_case(Expr *expr, Range range) {
  Stmt *stmt = new_stmt(ST_CASE, range);
  stmt->expr = expr;
  stmt->label = make_label();
  return stmt;
}

static Stmt *new_stmt_default(Range range) {
  Stmt *stmt = new_stmt(ST_DEFAULT, range);
  stmt->label = make_label();
  return stmt;
}

static Stmt *new_stmt_label(FuncCtxt *fcx, char *name, Range range) {
  Stmt *stmt = new_stmt(ST_LABEL, range);
  stmt->name = name;
  stmt->label = make_label();
  map_put(fcx->label_map, stmt->name, stmt->label);
  return stmt;
}

static Stmt *new_stmt_while(Expr *cond, Stmt *body, Range range) {
  Stmt *stmt = new_stmt(ST_WHILE, range);
  stmt->cond = cond;
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_do_while(Expr *cond, Stmt *body, Range range) {
  Stmt *stmt = new_stmt(ST_DO_WHILE, range);
  stmt->cond = cond;
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_for(Expr *init, Expr *cond, Expr *inc, Stmt *body,
                          Range range) {
  Stmt *stmt = new_stmt(ST_FOR, range);
  stmt->init = init;
  stmt->cond = cond;
  stmt->inc = inc;
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_goto(char *name, Range range) {
  Stmt *stmt = new_stmt(ST_GOTO, range);
  stmt->name = name;
  return stmt;
}

static Stmt *new_stmt_return(Expr *expr, Range range) {
  Stmt *stmt = new_stmt(ST_RETURN, range);
  stmt->expr = expr;
  return stmt;
}

static Stmt *new_stmt_compound(Vector *stmts, Range range) {
  Stmt *stmt = new_stmt(ST_COMPOUND, range);
  stmt->stmts = stmts;
  return stmt;
}

static Expr *primary_expression(LocalCtxt *lcx) {
  Token *token = NULL;
  if ((token = token_consume(lcx->tokenizer, TK_NUM)) != NULL) {
    return new_expr_num(token->val, token->range);
  }
  if ((token = token_consume(lcx->tokenizer, TK_IDENT)) != NULL) {
    return new_expr_ident(lcx, token->name, token->range);
  }
  if ((token = token_consume(lcx->tokenizer, TK_STR)) != NULL) {
    return new_expr_str(lcx, token->str, token->range);
  }
  if (token_consume(lcx->tokenizer, '(')) {
    Expr *expr = expression(lcx);
    token_expect(lcx->tokenizer, ')');
    return expr;
  }
  token_error(lcx->tokenizer, "数値でも開きカッコでもないトークンです");
}

static Expr *postfix_expression(LocalCtxt *lcx) {
  Token *token;
  Expr *expr = primary_expression(lcx);
  while (true) {
    if (token_consume(lcx->tokenizer, '[')) {
      Expr *operand = expression(lcx);
      Token *end = token_expect(lcx->tokenizer, ']');
      expr = new_expr_index(lcx, expr, operand,
                            range_join(expr->range, end->range));
    } else if (token_consume(lcx->tokenizer, '.')) {
      Token *member = token_expect(lcx->tokenizer, TK_IDENT);
      expr = new_expr_dot(lcx, expr, member->name,
                          range_join(expr->range, member->range));
    } else if (token_consume(lcx->tokenizer, TK_ARROW)) {
      Token *member = token_expect(lcx->tokenizer, TK_IDENT);
      expr = new_expr_arrow(lcx, expr, member->name,
                            range_join(expr->range, member->range));
    } else if (token_consume(lcx->tokenizer, '(')) {
      Vector *argument = NULL;
      if (token_peek(lcx->tokenizer)->ty != ')') {
        argument = argument_expression_list(lcx);
      }
      Token *end = token_expect(lcx->tokenizer, ')');
      expr = new_expr_call(lcx, expr, argument,
                           range_join(expr->range, end->range));
    } else if ((token = token_consume(lcx->tokenizer, TK_INC)) != NULL) {
      int val;
      if (is_ptr_type(expr->val_type)) {
        val = get_val_size(expr->val_type->ptrof);
      } else {
        val = 1;
      }
      expr = new_expr_postfix(lcx, EX_INC, expr,
                              range_join(expr->range, token->range));
      expr->val = val;
    } else if ((token = token_consume(lcx->tokenizer, TK_DEC)) != NULL) {
      int val;
      if (is_ptr_type(expr->val_type)) {
        val = get_val_size(expr->val_type->ptrof);
      } else {
        val = 1;
      }
      expr = new_expr_postfix(lcx, EX_DEC, expr, token->range);
      expr->val = val;
    } else {
      return expr;
    }
  }
}

static Vector *argument_expression_list(LocalCtxt *lcx) {
  Vector *argument = new_vector();
  while (true) {
    vec_push(argument, assignment_expression(lcx));
    if (!token_consume(lcx->tokenizer, ',')) {
      break;
    }
  }
  return argument;
}

static Expr *unary_expression(LocalCtxt *lcx) {
  const int OPS[] = {'&', '*', '+', '-', '~', '!', '\0'};
  for (int i = 0; OPS[i] != '\0'; i++) {
    int op = OPS[i];
    Token *token;
    if ((token = token_consume(lcx->tokenizer, op)) != NULL) {
      Expr *operand = cast_expression(lcx);
      return new_expr_unary(lcx, op, operand,
                            range_join(token->range, operand->range));
    }
  }

  Token *token;
  if ((token = token_consume(lcx->tokenizer, TK_INC)) != NULL) {
    Expr *expr = cast_expression(lcx);
    int val;
    if (is_ptr_type(expr->val_type)) {
      val = get_val_size(expr->val_type->ptrof);
    } else {
      val = 1;
    }
    expr = new_expr_unary(lcx, EX_INC, expr,
                          range_join(token->range, expr->range));
    expr->val = val;
    return expr;
  }
  if ((token = token_consume(lcx->tokenizer, TK_DEC)) != NULL) {
    Expr *expr = cast_expression(lcx);
    int val;
    if (is_ptr_type(expr->val_type)) {
      val = get_val_size(expr->val_type->ptrof);
    } else {
      val = 1;
    }
    expr = new_expr_unary(lcx, EX_DEC, expr,
                          range_join(token->range, expr->range));
    expr->val = val;
    return expr;
  }

  if ((token = token_consume(lcx->tokenizer, TK_SIZEOF)) != NULL) {
    if (token_peek(lcx->tokenizer)->ty != '(' ||
        !token_is_typename(lcx->scope, token_peek_ahead(lcx->tokenizer, 1))) {
      Expr *expr = unary_expression(lcx);
      return new_expr_num(get_val_size(expr->val_type),
                          range_join(token->range, expr->range));
    } else {
      token_expect(lcx->tokenizer, '(');
      Type *type = type_name(lcx->scope, lcx->tokenizer);
      Token *end = token_expect(lcx->tokenizer, ')');
      return new_expr_num(get_val_size(type),
                          range_join(token->range, end->range));
    }
  }
  return postfix_expression(lcx);
}

static Expr *cast_expression(LocalCtxt *lcx) {
  Token *token = token_peek(lcx->tokenizer);
  if (token->ty == '(' &&
      token_is_typename(lcx->scope, token_peek_ahead(lcx->tokenizer, 1))) {
    Range start = token->range;
    token_succ(lcx->tokenizer);
    Type *val_type = type_name(lcx->scope, lcx->tokenizer);
    token_expect(lcx->tokenizer, ')');
    Expr *operand = cast_expression(lcx);
    return new_expr_cast(lcx, val_type, operand,
                         range_join(start, operand->range));
  }
  return unary_expression(lcx);
}

static Expr *binary_expression(LocalCtxt *lcx, const int *tks, const int *exs,
                               Expr *(*op_parser)(LocalCtxt *lcx)) {
  Expr *expr = op_parser(lcx);
  bool found;
  do {
    found = false;
    for (int i = 0; tks[i] != '\0'; i++) {
      int tk = tks[i];
      int ex = exs[i];
      Token *token;
      if ((token = token_consume(lcx->tokenizer, tk)) != NULL) {
        Expr *operand = op_parser(lcx);
        expr = new_expr_binop(lcx, ex, expr, operand,
                              range_join(expr->range, operand->range));
        found = true;
        break;
      }
    }
  } while (found);
  return expr;
}

static Expr *multiplicative_expression(LocalCtxt *lcx) {
  const int OPS[] = {'*', '/', '%', '\0'};
  return binary_expression(lcx, OPS, OPS, cast_expression);
}

static Expr *additive_expression(LocalCtxt *lcx) {
  const int OPS[] = {'+', '-', '\0'};
  return binary_expression(lcx, OPS, OPS, multiplicative_expression);
}

static Expr *shift_expression(LocalCtxt *lcx) {
  const int TKS[] = {TK_LSHIFT, TK_RSHIFT, '\0'};
  const int EXS[] = {EX_LSHIFT, EX_RSHIFT, '\0'};
  return binary_expression(lcx, TKS, EXS, additive_expression);
}

static Expr *relational_expression(LocalCtxt *lcx) {
  const int TKS[] = {'<', '>', TK_LTEQ, TK_GTEQ, '\0'};
  const int EXS[] = {'<', '>', EX_LTEQ, EX_GTEQ, '\0'};
  return binary_expression(lcx, TKS, EXS, shift_expression);
}

static Expr *equality_expression(LocalCtxt *lcx) {
  const int TKS[] = {TK_EQEQ, TK_NOTEQ, '\0'};
  const int EXS[] = {EX_EQEQ, EX_NOTEQ, '\0'};
  return binary_expression(lcx, TKS, EXS, relational_expression);
}

static Expr *and_expression(LocalCtxt *lcx) {
  const int OPS[] = {'&', '\0'};
  return binary_expression(lcx, OPS, OPS, equality_expression);
}

static Expr *exclusive_or_expression(LocalCtxt *lcx) {
  const int OPS[] = {'^', '\0'};
  return binary_expression(lcx, OPS, OPS, and_expression);
}
static Expr *inclusive_or_expression(LocalCtxt *lcx) {
  const int OPS[] = {'|', '\0'};
  return binary_expression(lcx, OPS, OPS, exclusive_or_expression);
}
static Expr *logical_and_expression(LocalCtxt *lcx) {
  const int TKS[] = {TK_LOGAND, '\0'};
  const int EXS[] = {EX_LOGAND, '\0'};
  return binary_expression(lcx, TKS, EXS, inclusive_or_expression);
}
static Expr *logical_or_expression(LocalCtxt *lcx) {
  const int TKS[] = {TK_LOGOR, '\0'};
  const int EXS[] = {EX_LOGOR, '\0'};
  return binary_expression(lcx, TKS, EXS, logical_and_expression);
}
static Expr *conditional_expression(LocalCtxt *lcx) {
  Expr *cond = logical_or_expression(lcx);
  if (token_consume(lcx->tokenizer, '?')) {
    Expr *then_expr = expression(lcx);
    token_expect(lcx->tokenizer, ':');
    Expr *else_expr = conditional_expression(lcx);
    return new_expr_cond(lcx, cond, then_expr, else_expr,
                         range_join(cond->range, else_expr->range));
  }
  return cond;
}

static Expr *assignment_expression(LocalCtxt *lcx) {
  Expr *lhs = conditional_expression(lcx);
  if (token_consume(lcx->tokenizer, '=')) {
    Expr *rhs = assignment_expression(lcx);
    return new_expr_binop(lcx, '=', lhs, rhs,
                          range_join(lhs->range, rhs->range));
  }
  if (token_consume(lcx->tokenizer, TK_MUL_ASSIGN)) {
    Expr *rhs = assignment_expression(lcx);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(lcx, '=', lhs,
                          new_expr_binop(lcx, '*', lhs, rhs, range), range);
  }
  if (token_consume(lcx->tokenizer, TK_DIV_ASSIGN)) {
    Expr *rhs = assignment_expression(lcx);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(lcx, '=', lhs,
                          new_expr_binop(lcx, '/', lhs, rhs, range), range);
  }
  if (token_consume(lcx->tokenizer, TK_MOD_ASSIGN)) {
    Expr *rhs = assignment_expression(lcx);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(lcx, '=', lhs,
                          new_expr_binop(lcx, '%', lhs, rhs, range), range);
  }
  if (token_consume(lcx->tokenizer, TK_ADD_ASSIGN)) {
    Expr *rhs = assignment_expression(lcx);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(lcx, '=', lhs,
                          new_expr_binop(lcx, '+', lhs, rhs, range), range);
  }
  if (token_consume(lcx->tokenizer, TK_SUB_ASSIGN)) {
    Expr *rhs = assignment_expression(lcx);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(lcx, '=', lhs,
                          new_expr_binop(lcx, '-', lhs, rhs, range), range);
  }
  if (token_consume(lcx->tokenizer, TK_LSHIFT_ASSIGN)) {
    Expr *rhs = assignment_expression(lcx);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(
        lcx, '=', lhs, new_expr_binop(lcx, EX_LSHIFT, lhs, rhs, range), range);
  }
  if (token_consume(lcx->tokenizer, TK_RSHIFT_ASSIGN)) {
    Expr *rhs = assignment_expression(lcx);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(
        lcx, '=', lhs, new_expr_binop(lcx, EX_RSHIFT, lhs, rhs, range), range);
  }
  if (token_consume(lcx->tokenizer, TK_AND_ASSIGN)) {
    Expr *rhs = assignment_expression(lcx);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(lcx, '=', lhs,
                          new_expr_binop(lcx, '&', lhs, rhs, range), range);
  }
  if (token_consume(lcx->tokenizer, TK_OR_ASSIGN)) {
    Expr *rhs = assignment_expression(lcx);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(lcx, '=', lhs,
                          new_expr_binop(lcx, '|', lhs, rhs, range), range);
  }
  if (token_consume(lcx->tokenizer, TK_XOR_ASSIGN)) {
    Expr *rhs = assignment_expression(lcx);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(lcx, '=', lhs,
                          new_expr_binop(lcx, '^', lhs, rhs, range), range);
  }
  return lhs;
}

static Expr *expression(LocalCtxt *lcx) {
  const int OPS[] = {',', '\0'};
  return binary_expression(lcx, OPS, OPS, assignment_expression);
}

static Expr *constant_expression(LocalCtxt *lcx) {
  return conditional_expression(lcx);
}

static void declaration(LocalCtxt *lcx) {
  bool is_typedef = token_consume(lcx->tokenizer, TK_TYPEDEF);
  Type *base_type = type_specifier(lcx->scope, lcx->tokenizer);
  if (token_consume(lcx->tokenizer, ';')) {
    return;
  }
  char *name;
  Type *type;
  Range range;
  declarator(lcx->scope, lcx->tokenizer, base_type, &name, &type, &range);
  if (is_typedef) {
    register_typedef(lcx->scope, name, type);
  } else {
    (void)register_stack(lcx, name, type, range);
  }
  while (token_consume(lcx->tokenizer, ',')) {
    declarator(lcx->scope, lcx->tokenizer, base_type, &name, &type, &range);
    if (is_typedef) {
      register_typedef(lcx->scope, name, type);
    } else {
      (void)register_stack(lcx, name, type, range);
    }
  }
  token_expect(lcx->tokenizer, ';');
}

static Type *type_specifier(Scope *scope, Tokenizer *tokenizer) {
  Token *token = token_pop(tokenizer);
  switch (token->ty) {
  case TK_CHAR:
    return new_type(TY_CHAR);
  case TK_INT:
    return new_type(TY_INT);
  case TK_VOID:
    return new_type(TY_VOID);
  case TK_STRUCT:
  case TK_UNION:
    return struct_or_union_specifier(scope, tokenizer, token);
  case TK_IDENT:
    return get_typedef(scope, token->name);
  default:
    token_error_with(tokenizer, token, "型名がありません");
  }
}

static void struct_declaration(Scope *scope, Tokenizer *tokenizer, Type *type) {
  assert(type->ty == TY_STRUCT || type->ty == TY_UNION);
  Type *base_type = type_specifier(scope, tokenizer);
  char *member_name;
  Type *member_type;
  Range range;
  declarator(scope, tokenizer, base_type, &member_name, &member_type, &range);
  if (!register_member(tokenizer, type, member_name, member_type, range)) {
    reader_error_range(token_get_reader(tokenizer), range,
                       "同じ名前のメンバ変数が複数あります: %s", member_name);
  }

  while (token_consume(tokenizer, ',')) {
    declarator(scope, tokenizer, base_type, &member_name, &member_type, &range);
    if (!register_member(tokenizer, type, member_name, member_type, range)) {
      reader_error_range(token_get_reader(tokenizer), range,
                         "同じ名前のメンバ変数が複数あります: %s", member_name);
    }
  }
  token_expect(tokenizer, ';');
}

static Type *struct_or_union_specifier(Scope *scope, Tokenizer *tokenizer,
                                       Token *token) {
  assert(token->ty == TK_STRUCT || token->ty == TK_UNION);
  Token *tag = token_consume(tokenizer, TK_IDENT);
  if (tag == NULL && token_peek(tokenizer)->ty != '{') {
    token_error_with(tokenizer, token,
                     "構造体または共用体のタグまたは `{` がありません");
  }

  if (token_consume(tokenizer, '{')) {
    Type *type = new_type_struct(token->ty, tag ? tag->name : NULL);
    if (tag != NULL) {
      if (!register_tag(scope, tag->name, type)) {
        token_error_with(tokenizer, tag,
                         "同じタグ名の構造体または共用体の多重定義です: %s",
                         tag->name);
      }
    }
    while (token_peek(tokenizer)->ty != '}') {
      struct_declaration(scope, tokenizer, type);
    }
    token_expect(tokenizer, '}');
    return type;
  }

  Type *type = get_tag(scope, tag->name);
  if (type != NULL) {
    return type;
  }

  return new_type_anon_struct(token->ty);
}

static Type *type_name(Scope *scope, Tokenizer *tokenizer) {
  Type *type = type_specifier(scope, tokenizer);
  while (token_consume(tokenizer, '*')) {
    type = new_type_ptr(type);
  }
  while (token_consume(tokenizer, '[')) {
    type = new_type_array(type, token_expect(tokenizer, TK_NUM)->val);
    token_expect(tokenizer, ']');
  }
  return type;
}

static void declarator(Scope *scope, Tokenizer *tokenizer, Type *base_type,
                       char **name, Type **type, Range *range) {
  Range start = token_peek(tokenizer)->range;
  while (token_consume(tokenizer, '*')) {
    base_type = new_type_ptr(base_type);
  }
  Range end;
  direct_declarator(scope, tokenizer, base_type, name, type, &end);
  *range = range_join(start, end);
}

static void direct_declarator(Scope *scope, Tokenizer *tokenizer,
                              Type *base_type, char **name, Type **type,
                              Range *range) {
  Type *placeholder = malloc(sizeof(Type));
  *range = token_peek(tokenizer)->range;

  {
    Token *token;
    if ((token = token_consume(tokenizer, TK_IDENT)) != NULL) {
      *name = token->name;
      *type = placeholder;
      *range = range_join(*range, token->range);
    } else if (token_consume(tokenizer, '(')) {
      Range mid;
      declarator(scope, tokenizer, placeholder, name, type, &mid);
      Token *token = token_expect(tokenizer, ')');
      *range = range_join(*range, token->range);
    } else {
      *name = NULL;
      *type = base_type;
      *range = *range;
    }
  }

  while (true) {
    if (token_consume(tokenizer, '[')) {
      Type *inner = malloc(sizeof(Type));
      *placeholder =
          *new_type_array(inner, token_expect(tokenizer, TK_NUM)->val);
      placeholder = inner;
      Token *end = token_expect(tokenizer, ']');
      *range = range_join(*range, end->range);
      continue;
    }

    if (token_consume(tokenizer, '(')) {
      Vector *params = new_vector();
      Token *end;
      if ((end = token_consume(tokenizer, ')')) ||
          (end = token_consume2(tokenizer, TK_VOID, ')'))) {
        // do nothing
      } else {
        while (true) {
          Type *base_type = type_specifier(scope, tokenizer);
          Param *param = malloc(sizeof(Param));
          declarator(scope, tokenizer, base_type, &param->name, &param->type,
                     &param->range);
          vec_push(params, param);
          if (token_peek(tokenizer)->ty == ')') {
            break;
          }
          token_expect(tokenizer, ',');
        }
        end = token_expect(tokenizer, ')');
      }

      *range = range_join(*range, end->range);

      Type *inner = malloc(sizeof(Type));
      *placeholder = *new_type_func(inner, params);
      placeholder = inner;
      continue;
    }

    break;
  }

  *placeholder = *base_type;
}

static Stmt *statement(LocalCtxt *lcx) {
  Token *start = token_peek(lcx->tokenizer);
  switch (start->ty) {
  case TK_IF: {
    token_succ(lcx->tokenizer);
    token_expect(lcx->tokenizer, '(');
    Expr *cond = expression(lcx);
    token_expect(lcx->tokenizer, ')');
    Stmt *then_stmt = statement(lcx);
    Stmt *else_stmt = &null_stmt;
    Range range;
    if (token_consume(lcx->tokenizer, TK_ELSE)) {
      else_stmt = statement(lcx);
      range = range_join(start->range, else_stmt->range);
    } else {
      range = range_join(start->range, then_stmt->range);
    }
    return new_stmt_if(cond, then_stmt, else_stmt, range);
  }
  case TK_SWITCH: {
    token_succ(lcx->tokenizer);
    token_expect(lcx->tokenizer, '(');
    Expr *cond = expression(lcx);
    token_expect(lcx->tokenizer, ')');
    Stmt *stmt = new_stmt_switch(cond, NULL, start->range);
    vec_push(lcx->func->switches, stmt);
    stmt->body = statement(lcx);
    stmt->range = range_join(stmt->range, stmt->body->range);
    vec_pop(lcx->func->switches);
    return stmt;
  }
  case TK_CASE: {
    token_succ(lcx->tokenizer);
    Expr *expr = constant_expression(lcx);
    Token *end = token_expect(lcx->tokenizer, ':');
    Stmt *stmt = new_stmt_case(expr, range_join(start->range, end->range));
    if (lcx->func->switches->len <= 0) {
      local_error(lcx, stmt->range, "switch文中でない箇所にcase文があります");
    }
    Stmt *switch_stmt = lcx->func->switches->data[lcx->func->switches->len - 1];
    vec_push(switch_stmt->cases, stmt);
    return stmt;
  }
  case TK_DEFAULT: {
    token_succ(lcx->tokenizer);
    Token *end = token_expect(lcx->tokenizer, ':');
    Stmt *stmt = new_stmt_default(range_join(start->range, end->range));
    if (lcx->func->switches->len <= 0) {
      local_error(lcx, stmt->range, "switch文中でない箇所にcase文があります");
    }
    Stmt *switch_expr = lcx->func->switches->data[lcx->func->switches->len - 1];
    switch_expr->default_case = stmt;
    return stmt;
  }
  case TK_WHILE: {
    token_succ(lcx->tokenizer);
    token_expect(lcx->tokenizer, '(');
    Expr *cond = expression(lcx);
    token_expect(lcx->tokenizer, ')');
    Stmt *body = statement(lcx);
    return new_stmt_while(cond, body, range_join(start->range, body->range));
  }
  case TK_DO: {
    token_succ(lcx->tokenizer);
    Stmt *body = statement(lcx);
    token_expect(lcx->tokenizer, TK_WHILE);
    token_expect(lcx->tokenizer, '(');
    Expr *cond = expression(lcx);
    token_expect(lcx->tokenizer, ')');
    Token *end = token_expect(lcx->tokenizer, ';');
    return new_stmt_do_while(cond, body, range_join(start->range, end->range));
  }
  case TK_FOR: {
    token_succ(lcx->tokenizer);
    Expr *init = NULL;
    Expr *cond = NULL;
    Expr *inc = NULL;
    token_expect(lcx->tokenizer, '(');
    if (token_peek(lcx->tokenizer)->ty != ';') {
      init = expression(lcx);
    }
    token_expect(lcx->tokenizer, ';');
    if (token_peek(lcx->tokenizer)->ty != ';') {
      cond = expression(lcx);
    }
    token_expect(lcx->tokenizer, ';');
    if (token_peek(lcx->tokenizer)->ty != ')') {
      inc = expression(lcx);
    }
    token_expect(lcx->tokenizer, ')');
    Stmt *body = statement(lcx);
    return new_stmt_for(init, cond, inc, body,
                        range_join(start->range, body->range));
  }
  case TK_GOTO: {
    token_succ(lcx->tokenizer);
    char *name = token_expect(lcx->tokenizer, TK_IDENT)->name;
    Token *end = token_expect(lcx->tokenizer, ';');
    return new_stmt_goto(name, range_join(start->range, end->range));
  }
  case TK_BREAK: {
    token_succ(lcx->tokenizer);
    Token *end = token_expect(lcx->tokenizer, ';');
    return new_stmt(ST_BREAK, range_join(start->range, end->range));
  }
  case TK_CONTINUE: {
    token_succ(lcx->tokenizer);
    Token *end = token_expect(lcx->tokenizer, ';');
    return new_stmt(ST_CONTINUE, range_join(start->range, end->range));
  }
  case TK_RETURN: {
    token_succ(lcx->tokenizer);
    Expr *expr = NULL;
    if (token_peek(lcx->tokenizer)->ty != ';') {
      expr = expression(lcx);
    }
    Token *end = token_expect(lcx->tokenizer, ';');
    return new_stmt_return(expr, range_join(start->range, end->range));
  }
  case '{': {
    LocalCtxt *inner = new_inner_local_ctxt(lcx);
    return compound_statement(inner);
  }
  case ';': {
    token_succ(lcx->tokenizer);
    return &null_stmt;
  }
  case TK_IDENT: {
    if (token_peek_ahead(lcx->tokenizer, 1)->ty == ':') {
      Token *ident = token_pop(lcx->tokenizer);
      Token *end = token_pop(lcx->tokenizer);
      Stmt *stmt = new_stmt_label(lcx->func, ident->name,
                                  range_join(start->range, end->range));
      return stmt;
    }
  }
  // fall through
  default: {
    Expr *expr = expression(lcx);
    Token *end = token_expect(lcx->tokenizer, ';');
    return new_stmt_expr(expr, range_join(expr->range, end->range));
  }
  }
}

static Stmt *compound_statement(LocalCtxt *lcx) {
  Token *start = token_expect(lcx->tokenizer, '{');

  Range range = start->range;
  Vector *stmts = new_vector();
  while (!token_consume(lcx->tokenizer, '}')) {
    Token *token = token_peek(lcx->tokenizer);
    if (token_is_typename(lcx->scope, token) || token->ty == TK_TYPEDEF) {
      declaration(lcx);
      continue;
    }
    Stmt *s = statement(lcx);
    range = range_join(range, s->range);
    vec_push(stmts, s);
  }
  return new_stmt_compound(stmts, range);
}

static Function *function_definition(GlobalCtxt *gcx, Type *type, char *name,
                                     Range start) {
  FuncCtxt *fcx = new_func_ctxt(gcx, name);
  LocalCtxt *lcx = new_root_local_ctxt(fcx);
  for (int i = 0; i < type->func_param->len; i++) {
    Param *param = type->func_param->data[i];
    param->stack_var =
        register_stack(lcx, param->name, param->type, param->range);
  }

  Stmt *body = compound_statement(lcx);

  Function *func = malloc(sizeof(Function));
  func->name = name;
  func->type = type;
  func->range = range_join(start, body->range);
  func->stack_size = fcx->stack_size;
  func->label_map = fcx->label_map;
  func->body = body;

  return func;
}

static GlobalVar *new_global_variable(Type *type, char *name, Range range) {
  GlobalVar *gvar = malloc(sizeof(GlobalVar));
  gvar->type = type;
  gvar->name = name;
  gvar->range = range;
  return gvar;
}

static TranslationUnit *translation_unit(Tokenizer *tokenizer) {
  GlobalCtxt *gcx = new_global_ctxt(tokenizer);

  Vector *func_list = new_vector();
  Vector *gvar_list = new_vector();

  while (token_peek(tokenizer)->ty != TK_EOF) {
    bool is_typedef = token_consume(tokenizer, TK_TYPEDEF);
    Type *base_type = type_specifier(gcx->scope, gcx->tokenizer);
    if (token_consume(gcx->tokenizer, ';')) {
      continue;
    }
    char *name;
    Type *type;
    Range range;
    declarator(gcx->scope, gcx->tokenizer, base_type, &name, &type, &range);

    (void)register_decl(gcx->scope, name, type, NULL);

    if (is_func_type(type) && token_peek(gcx->tokenizer)->ty == '{') {
      vec_push(func_list, function_definition(gcx, type, name, range));
      continue;
    }
    if (is_typedef) {
      register_typedef(gcx->scope, name, type);
    } else {
      if (!is_func_type(type)) {
        vec_push(gvar_list, new_global_variable(type, name, range));
      }
    }

    while (token_consume(gcx->tokenizer, ',')) {
      declarator(gcx->scope, gcx->tokenizer, base_type, &name, &type, &range);
      (void)register_decl(gcx->scope, name, type, NULL);

      if (is_typedef) {
        register_typedef(gcx->scope, name, type);
      } else {
        if (!is_func_type(type)) {
          vec_push(gvar_list, new_global_variable(type, name, range));
        }
      }
    }
    token_expect(gcx->tokenizer, ';');
  }

  TranslationUnit *tunit = malloc(sizeof(TranslationUnit));
  tunit->func_list = func_list;
  tunit->gvar_list = gvar_list;
  tunit->str_list = gcx->str_list;
  return tunit;
}
