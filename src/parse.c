#include "gifcc.h"
#include <assert.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct GlobalCtxt {
  Map *var_map;
  Vector *str_list;
} GlobalCtxt;

typedef struct FuncCtxt {
  char *name;
  int stack_size;
  Vector *switches;
  Map *label_map;
} FuncCtxt;

typedef struct Decl {
  Type *type;
  StackVar *stack_var;
} Decl;

typedef struct Scope {
  Map *decl_map;
  Map *typedef_map;
  Map *tag_map;

  GlobalCtxt *global_ctxt;
  FuncCtxt *func_ctxt;
  struct Scope *outer;
} Scope;

static GlobalCtxt *new_global_ctxt(void);
static FuncCtxt *new_func_ctxt(char *name);
static Scope *new_scope(GlobalCtxt *gcx, FuncCtxt *fcx, Scope *outer);
static Scope *new_global_scope(GlobalCtxt *gcx);
static Scope *new_func_scope(Scope *global, FuncCtxt *fcx);
static Scope *new_inner_scope(Scope *outer);
static StackVar *register_stack(Scope *scope, char *name, Type *type,
                                Range range);
static bool register_member(Type *type, char *member_name, Type *member_type,
                            Range range);
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
static Type *integer_promoted(Scope *scope, Expr **e);
static Type *arith_converted(Scope *scope, Expr **e1, Expr **e2);
static bool token_is_typename(Scope *scope, Token *token);
static noreturn void binop_type_error_raw(int ty, Expr *lhs, Expr *rhs,
                                          const char *dbg_file, int dbg_line);
static Type *new_type(int ty);
static Type *new_type_ptr(Type *base_type);
static Type *new_type_array(Type *base_type, int len);
static Type *new_type_func(Type *ret_type, Vector *func_param);
static Type *new_type_struct(int tk, char *tag);
static Type *new_type_anon_struct(int tk);
static Expr *coerce_array2ptr(Scope *scope, Expr *expr);
static Expr *new_expr(int ty, Type *val_type, Range range);
static Expr *new_expr_num(int val, Range range);
static Expr *new_expr_ident(Scope *scope, char *name, Range range);
static Expr *new_expr_str(Scope *scope, char *val, Range range);
static Expr *new_expr_call(Scope *scope, Expr *callee, Vector *argument,
                           Range range);
static Expr *new_expr_postfix(Scope *scope, int ty, Expr *operand, Range range);
static Expr *new_expr_cast(Scope *scope, Type *val_type, Expr *operand,
                           Range range);
static Expr *new_expr_unary(Scope *scope, int ty, Expr *operand, Range range);
static Expr *new_expr_binop(Scope *scope, int ty, Expr *lhs, Expr *rhs,
                            Range range);
static Expr *new_expr_cond(Scope *scope, Expr *cond, Expr *then_expr,
                           Expr *else_expr, Range range);
static Expr *new_expr_index(Scope *scope, Expr *array, Expr *index,
                            Range range);
static Expr *new_expr_dot(Scope *scope, Expr *operand, char *name, Range range);
static Expr *new_expr_arrow(Scope *scope, Expr *operand, char *name,
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
static Expr *primary_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *postfix_expression(Tokenizer *tokenizer, Scope *scope);
static Vector *argument_expression_list(Tokenizer *tokenizer, Scope *scope);
static Expr *unary_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *cast_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *binary_expression(Tokenizer *tokenizer, Scope *scope,
                               const int *tks, const int *exs,
                               Expr *(*op_parser)(Tokenizer *, Scope *));
static Expr *multiplicative_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *additive_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *shift_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *relational_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *equality_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *and_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *exclusive_or_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *inclusive_or_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *logical_and_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *logical_or_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *conditional_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *assignment_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *expression(Tokenizer *tokenizer, Scope *scope);
static Expr *constant_expression(Tokenizer *tokenizer, Scope *scope);

// declaration
static void declaration(Tokenizer *tokenizer, Scope *scope, Vector *stmts);
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
static Stmt *statement(Tokenizer *tokenizer, Scope *scope);
static Stmt *compound_statement(Tokenizer *tokenizer, Scope *scope);

// top-level
static Function *function_definition(Tokenizer *tokenizer, Scope *global_scope,
                                     Type *type, char *name, Range start);
static GlobalVar *new_global_variable(Type *type, char *name, Range range,
                                      Expr *init);
static TranslationUnit *translation_unit(Tokenizer *tokenizer);

static Stmt null_stmt = {
    .ty = ST_NULL,
};

TranslationUnit *parse(Reader *reader) {
  Tokenizer *tokenizer = new_tokenizer(reader);
  return translation_unit(tokenizer);
}

static GlobalCtxt *new_global_ctxt(void) {
  GlobalCtxt *gcx = malloc(sizeof(GlobalCtxt));
  gcx->var_map = new_map();
  gcx->str_list = new_vector();

  return gcx;
}

static FuncCtxt *new_func_ctxt(char *name) {
  FuncCtxt *fcx = malloc(sizeof(FuncCtxt));

  fcx->name = name;
  fcx->stack_size = 0;
  fcx->switches = new_vector();
  fcx->label_map = new_map();

  return fcx;
}

static Scope *new_scope(GlobalCtxt *gcx, FuncCtxt *fcx, Scope *outer) {
  Scope *scope = malloc(sizeof(Scope));
  scope->decl_map = new_map();
  scope->typedef_map = new_map();
  scope->tag_map = new_map();

  scope->global_ctxt = gcx;
  scope->func_ctxt = fcx;
  scope->outer = outer;
  return scope;
}

static Scope *new_global_scope(GlobalCtxt *gcx) {
  return new_scope(gcx, NULL, NULL);
}

static Scope *new_func_scope(Scope *global, FuncCtxt *fcx) {
  return new_scope(global->global_ctxt, fcx, global);
}

static Scope *new_inner_scope(Scope *outer) {
  return new_scope(outer->global_ctxt, outer->func_ctxt, outer);
}

static StackVar *register_stack(Scope *scope, char *name, Type *type,
                                Range range) {
  if (type->ty == TY_VOID) {
    range_error(range, "void型の変数は定義できません: %s", name);
  }

  FuncCtxt *fcx = scope->func_ctxt;

  StackVar *var = malloc(sizeof(StackVar));
  fcx->stack_size = align(fcx->stack_size, get_val_align(type));
  var->offset = fcx->stack_size;
  var->type = type;
  var->range = range;
  fcx->stack_size += get_val_size(type);

  if (!register_decl(scope, name, type, var)) {
    range_error(range, "同じ名前のローカル変数が複数あります: %s", name);
  }

  return var;
}

static bool register_member(Type *type, char *member_name, Type *member_type,
                            Range range) {
  assert(type->ty == TY_STRUCT || type->ty == TY_UNION);

  if (member_type->ty == TY_VOID) {
    range_error(range, "void型のメンバーです: %s", member_name);
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
static Type *integer_promoted(Scope *scope, Expr **e) {
  if (!is_integer_type((*e)->val_type)) {
    return NULL;
  }
  // CHAR は INT へ昇格する
  if ((*e)->val_type->ty == TY_CHAR) {
    *e = new_expr_cast(scope, new_type(TY_INT), *e, (*e)->range);
  }
  return (*e)->val_type;
}
static Type *arith_converted(Scope *scope, Expr **e1, Expr **e2) {
  if (!is_arith_type((*e1)->val_type) || !is_arith_type((*e2)->val_type)) {
    return NULL;
  }
  Type *ty1 = integer_promoted(scope, e2);
  Type *ty2 = integer_promoted(scope, e1);
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

#define binop_type_error(ty, lhs, rhs)                                         \
  binop_type_error_raw((ty), (lhs), (rhs), __FILE__, __LINE__)
static noreturn void binop_type_error_raw(int ty, Expr *lhs, Expr *rhs,
                                          const char *dbg_file, int dbg_line) {
  range_error_raw(range_join(lhs->range, rhs->range), dbg_file, dbg_line,
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

static Expr *coerce_array2ptr(Scope *scope, Expr *expr) {
  if (is_array_type(expr->val_type)) {
    return new_expr_unary(scope, '&', expr, expr->range);
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

static Expr *new_expr_ident(Scope *scope, char *name, Range range) {
  int ty;
  Type *type;
  if (strcmp(name, "__func__") == 0) {
    if (scope->func_ctxt == NULL) {
      range_error(range, "関数外で__func__が使用されました");
    }
    return new_expr_str(scope, scope->func_ctxt->name, range);
  }
  Decl *decl = get_decl(scope, name);
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

static Expr *new_expr_str(Scope *scope, char *val, Range range) {
  Type *type = new_type_ptr(new_type(TY_CHAR));
  Expr *expr = new_expr(EX_STR, type, range);

  expr->name = make_label();
  StringLiteral *str = malloc(sizeof(StringLiteral));
  str->name = expr->name;
  str->val = val;
  vec_push(scope->global_ctxt->str_list, str);
  return expr;
}

static Expr *new_expr_call(Scope *scope, Expr *callee, Vector *argument,
                           Range range) {
  callee = coerce_array2ptr(scope, callee);
  if (argument != NULL) {
    for (int i = 0; i < argument->len; i++) {
      argument->data[i] = coerce_array2ptr(scope, argument->data[i]);
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

static Expr *new_expr_postfix(Scope *scope, int ty, Expr *operand,
                              Range range) {
  operand = coerce_array2ptr(scope, operand);

  Expr *expr = new_expr(ty, operand->val_type, range);
  expr->lhs = operand;
  expr->rhs = NULL;
  if (ty == EX_INC || ty == EX_DEC) {
    if (is_ptr_type(operand->val_type)) {
      expr->val = get_val_size(operand->val_type->ptrof);
    } else {
      expr->val = 1;
    }
  }
  return expr;
}

static Expr *new_expr_cast(Scope *scope, Type *val_type, Expr *operand,
                           Range range) {
  operand = coerce_array2ptr(scope, operand);

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

static Expr *new_expr_unary(Scope *scope, int ty, Expr *operand, Range range) {
  if (ty != '&') {
    // & 以外は array は ptr とみなす
    operand = coerce_array2ptr(scope, operand);
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
      range_error(range, "ポインタ型でない値に対するデリファレンスです");
    }
    val_type = operand->val_type->ptrof;
  } else {
    val_type = operand->val_type;
  }
  Expr *expr = new_expr(ty, val_type, range);
  expr->lhs = NULL;
  expr->rhs = operand;
  if (ty == EX_INC || ty == EX_DEC) {
    if (is_ptr_type(operand->val_type)) {
      expr->val = get_val_size(expr->val_type->ptrof);
    } else {
      expr->val = 1;
    }
  }
  return expr;
}

static Expr *new_expr_binop(Scope *scope, int ty, Expr *lhs, Expr *rhs,
                            Range range) {
  lhs = coerce_array2ptr(scope, lhs);
  rhs = coerce_array2ptr(scope, rhs);

  Type *val_type;
  switch (ty) {
  // multiplicative
  case '*':
  case '/':
  case '%':
    val_type = arith_converted(scope, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(ty, lhs, rhs);
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
        binop_type_error(ty, lhs, rhs);
      }
      rhs = new_expr_binop(
          scope, '*', rhs,
          new_expr_num(get_val_size(lhs->val_type->ptrof), range), range);
      val_type = lhs->val_type;
      break;
    }

    if (is_ptr_type(rhs->val_type)) {
      if (!is_integer_type(lhs->val_type)) {
        binop_type_error(ty, lhs, rhs);
      }
      lhs = new_expr_binop(
          scope, '*', lhs,
          new_expr_num(get_val_size(rhs->val_type->ptrof), range), range);
      val_type = rhs->val_type;
      break;
    }

    val_type = arith_converted(scope, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(ty, lhs, rhs);
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
          binop_type_error(ty, lhs, rhs);
        }
        Expr *sub = new_expr('-', new_type(TY_INT), range);
        sub->lhs = lhs;
        sub->rhs = rhs;
        return new_expr_binop(
            scope, '/', sub,
            new_expr_num(get_val_size(lhs->val_type->ptrof), range), range);
      }
      if (is_integer_type(rhs->val_type)) {
        rhs = new_expr_binop(
            scope, '*', rhs,
            new_expr_num(get_val_size(lhs->val_type->ptrof), range), range);
        val_type = lhs->val_type;
        break;
      }
      binop_type_error(ty, lhs, rhs);
    }

    if (is_ptr_type(rhs->val_type)) {
      binop_type_error(ty, lhs, rhs);
    }

    val_type = arith_converted(scope, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(ty, lhs, rhs);
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
      binop_type_error(ty, lhs, rhs);
    }
    val_type = integer_promoted(scope, &lhs);
    if (val_type == NULL) {
      binop_type_error(ty, lhs, rhs);
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
      binop_type_error(ty, lhs, rhs);
    }
    val_type = arith_converted(scope, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(ty, lhs, rhs);
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

static Expr *new_expr_cond(Scope *scope, Expr *cond, Expr *then_expr,
                           Expr *else_expr, Range range) {
  cond = coerce_array2ptr(scope, cond);
  then_expr = coerce_array2ptr(scope, then_expr);
  else_expr = coerce_array2ptr(scope, else_expr);

  if (!is_sametype(then_expr->val_type, else_expr->val_type)) {
    range_error(range, "条件演算子の両辺の型が異なります: %d, %d",
                then_expr->val_type->ty, else_expr->val_type->ty);
  }
  Expr *expr = new_expr(EX_COND, then_expr->val_type, range);
  expr->cond = cond;
  expr->lhs = then_expr;
  expr->rhs = else_expr;
  return expr;
}

static Expr *new_expr_index(Scope *scope, Expr *array, Expr *index,
                            Range range) {
  array = coerce_array2ptr(scope, array);
  index = coerce_array2ptr(scope, index);

  return new_expr_unary(scope, '*',
                        new_expr_binop(scope, '+', array, index, range), range);
}

static Expr *new_expr_dot(Scope *scope, Expr *operand, char *name,
                          Range range) {
  if (operand->val_type->ty != TY_STRUCT && operand->val_type->ty != TY_UNION) {
    range_error(range, "構造体または共用体以外のメンバへのアクセスです");
  }
  return new_expr_arrow(scope, new_expr_unary(scope, '&', operand, range), name,
                        range);
}

static Expr *new_expr_arrow(Scope *scope, Expr *operand, char *name,
                            Range range) {
  if (operand->val_type->ty != TY_PTR ||
      (operand->val_type->ptrof->ty != TY_STRUCT &&
       operand->val_type->ptrof->ty != TY_UNION)) {
    range_error(range, "構造体または共用体以外のメンバへのアクセスです");
  }
  Member *member = operand->val_type->ptrof->members
                       ? map_get(operand->val_type->ptrof->members, name)
                       : NULL;
  if (member == NULL) {
    range_error(range, "存在しないメンバへのアクセスです: %s", name);
  }
  Expr *expr = new_expr('+', new_type_ptr(member->type), range);
  expr->lhs = operand;
  expr->rhs = new_expr_num(member->offset, range);
  return new_expr_unary(scope, '*', expr, range);
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

static Expr *primary_expression(Tokenizer *tokenizer, Scope *scope) {
  Token *token = NULL;
  if ((token = token_consume(tokenizer, TK_NUM)) != NULL) {
    return new_expr_num(token->val, token->range);
  }
  if ((token = token_consume(tokenizer, TK_IDENT)) != NULL) {
    return new_expr_ident(scope, token->name, token->range);
  }
  if ((token = token_consume(tokenizer, TK_STR)) != NULL) {
    return new_expr_str(scope, token->str, token->range);
  }
  if (token_consume(tokenizer, '(')) {
    Expr *expr = expression(tokenizer, scope);
    token_expect(tokenizer, ')');
    return expr;
  }
  range_error(token_peek(tokenizer)->range,
              "数値でも開きカッコでもないトークンです");
}

static Expr *postfix_expression(Tokenizer *tokenizer, Scope *scope) {
  Token *token;
  Expr *expr = primary_expression(tokenizer, scope);
  while (true) {
    if (token_consume(tokenizer, '[')) {
      Expr *operand = expression(tokenizer, scope);
      Token *end = token_expect(tokenizer, ']');
      expr = new_expr_index(scope, expr, operand,
                            range_join(expr->range, end->range));
    } else if (token_consume(tokenizer, '.')) {
      Token *member = token_expect(tokenizer, TK_IDENT);
      expr = new_expr_dot(scope, expr, member->name,
                          range_join(expr->range, member->range));
    } else if (token_consume(tokenizer, TK_ARROW)) {
      Token *member = token_expect(tokenizer, TK_IDENT);
      expr = new_expr_arrow(scope, expr, member->name,
                            range_join(expr->range, member->range));
    } else if (token_consume(tokenizer, '(')) {
      Vector *argument = NULL;
      if (token_peek(tokenizer)->ty != ')') {
        argument = argument_expression_list(tokenizer, scope);
      }
      Token *end = token_expect(tokenizer, ')');
      expr = new_expr_call(scope, expr, argument,
                           range_join(expr->range, end->range));
    } else if ((token = token_consume(tokenizer, TK_INC)) != NULL) {
      expr = new_expr_postfix(scope, EX_INC, expr,
                              range_join(expr->range, token->range));
    } else if ((token = token_consume(tokenizer, TK_DEC)) != NULL) {
      expr = new_expr_postfix(scope, EX_DEC, expr, token->range);
    } else {
      return expr;
    }
  }
}

static Vector *argument_expression_list(Tokenizer *tokenizer, Scope *scope) {
  Vector *argument = new_vector();
  while (true) {
    vec_push(argument, assignment_expression(tokenizer, scope));
    if (!token_consume(tokenizer, ',')) {
      break;
    }
  }
  return argument;
}

static Expr *unary_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {'&', '*', '+', '-', '~', '!', TK_INC, TK_DEC, '\0'};
  const int EXS[] = {'&', '*', '+', '-', '~', '!', EX_INC, EX_DEC, '\0'};
  for (int i = 0; TKS[i] != '\0'; i++) {
    int tk = TKS[i];
    int ex = EXS[i];
    Token *token;
    if ((token = token_consume(tokenizer, tk)) != NULL) {
      Expr *operand = cast_expression(tokenizer, scope);
      return new_expr_unary(scope, ex, operand,
                            range_join(token->range, operand->range));
    }
  }

  Token *token;
  if ((token = token_consume(tokenizer, TK_SIZEOF)) != NULL) {
    if (token_peek(tokenizer)->ty != '(' ||
        !token_is_typename(scope, token_peek_ahead(tokenizer, 1))) {
      Expr *expr = unary_expression(tokenizer, scope);
      return new_expr_num(get_val_size(expr->val_type),
                          range_join(token->range, expr->range));
    } else {
      token_expect(tokenizer, '(');
      Type *type = type_name(scope, tokenizer);
      Token *end = token_expect(tokenizer, ')');
      return new_expr_num(get_val_size(type),
                          range_join(token->range, end->range));
    }
  }
  return postfix_expression(tokenizer, scope);
}

static Expr *cast_expression(Tokenizer *tokenizer, Scope *scope) {
  Token *token = token_peek(tokenizer);
  if (token->ty == '(' &&
      token_is_typename(scope, token_peek_ahead(tokenizer, 1))) {
    Range start = token->range;
    token_succ(tokenizer);
    Type *val_type = type_name(scope, tokenizer);
    token_expect(tokenizer, ')');
    Expr *operand = cast_expression(tokenizer, scope);
    return new_expr_cast(scope, val_type, operand,
                         range_join(start, operand->range));
  }
  return unary_expression(tokenizer, scope);
}

static Expr *binary_expression(Tokenizer *tokenizer, Scope *scope,
                               const int *tks, const int *exs,
                               Expr *(*op_parser)(Tokenizer *, Scope *)) {
  Expr *expr = op_parser(tokenizer, scope);
  bool found;
  do {
    found = false;
    for (int i = 0; tks[i] != '\0'; i++) {
      int tk = tks[i];
      int ex = exs[i];
      Token *token;
      if ((token = token_consume(tokenizer, tk)) != NULL) {
        Expr *operand = op_parser(tokenizer, scope);
        expr = new_expr_binop(scope, ex, expr, operand,
                              range_join(expr->range, operand->range));
        found = true;
        break;
      }
    }
  } while (found);
  return expr;
}

static Expr *multiplicative_expression(Tokenizer *tokenizer, Scope *scope) {
  const int OPS[] = {'*', '/', '%', '\0'};
  return binary_expression(tokenizer, scope, OPS, OPS, cast_expression);
}
static Expr *additive_expression(Tokenizer *tokenizer, Scope *scope) {
  const int OPS[] = {'+', '-', '\0'};
  return binary_expression(tokenizer, scope, OPS, OPS,
                           multiplicative_expression);
}
static Expr *shift_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {TK_LSHIFT, TK_RSHIFT, '\0'};
  const int EXS[] = {EX_LSHIFT, EX_RSHIFT, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, additive_expression);
}
static Expr *relational_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {'<', '>', TK_LTEQ, TK_GTEQ, '\0'};
  const int EXS[] = {'<', '>', EX_LTEQ, EX_GTEQ, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, shift_expression);
}
static Expr *equality_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {TK_EQEQ, TK_NOTEQ, '\0'};
  const int EXS[] = {EX_EQEQ, EX_NOTEQ, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, relational_expression);
}
static Expr *and_expression(Tokenizer *tokenizer, Scope *scope) {
  const int OPS[] = {'&', '\0'};
  return binary_expression(tokenizer, scope, OPS, OPS, equality_expression);
}
static Expr *exclusive_or_expression(Tokenizer *tokenizer, Scope *scope) {
  const int OPS[] = {'^', '\0'};
  return binary_expression(tokenizer, scope, OPS, OPS, and_expression);
}
static Expr *inclusive_or_expression(Tokenizer *tokenizer, Scope *scope) {
  const int OPS[] = {'|', '\0'};
  return binary_expression(tokenizer, scope, OPS, OPS, exclusive_or_expression);
}
static Expr *logical_and_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {TK_LOGAND, '\0'};
  const int EXS[] = {EX_LOGAND, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, inclusive_or_expression);
}
static Expr *logical_or_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {TK_LOGOR, '\0'};
  const int EXS[] = {EX_LOGOR, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, logical_and_expression);
}
static Expr *conditional_expression(Tokenizer *tokenizer, Scope *scope) {
  Expr *cond = logical_or_expression(tokenizer, scope);
  if (token_consume(tokenizer, '?')) {
    Expr *then_expr = expression(tokenizer, scope);
    token_expect(tokenizer, ':');
    Expr *else_expr = conditional_expression(tokenizer, scope);
    return new_expr_cond(scope, cond, then_expr, else_expr,
                         range_join(cond->range, else_expr->range));
  }
  return cond;
}

static Expr *assignment_expression(Tokenizer *tokenizer, Scope *scope) {
  Expr *lhs = conditional_expression(tokenizer, scope);
  if (token_consume(tokenizer, '=')) {
    Expr *rhs = assignment_expression(tokenizer, scope);
    return new_expr_binop(scope, '=', lhs, rhs,
                          range_join(lhs->range, rhs->range));
  }

  typedef struct {
    int token_ty;
    int expr_ty;
  } Pair;

  const Pair PAIRS[] = {
      {TK_MUL_ASSIGN, '*'},          {TK_DIV_ASSIGN, '/'},
      {TK_MOD_ASSIGN, '%'},          {TK_ADD_ASSIGN, '+'},
      {TK_SUB_ASSIGN, '-'},          {TK_SUB_ASSIGN, '-'},
      {TK_LSHIFT_ASSIGN, EX_LSHIFT}, {TK_RSHIFT_ASSIGN, EX_RSHIFT},
      {TK_AND_ASSIGN, '&'},          {TK_OR_ASSIGN, '|'},
      {TK_XOR_ASSIGN, '^'},          {TK_EOF, '\0'},
  };

  for (int i = 0; PAIRS[i].token_ty != TK_EOF; i++) {
    const Pair *p = &PAIRS[i];
    if (token_consume(tokenizer, p->token_ty)) {
      Expr *rhs = assignment_expression(tokenizer, scope);
      Range range = range_join(lhs->range, rhs->range);
      return new_expr_binop(scope, '=', lhs,
                            new_expr_binop(scope, p->expr_ty, lhs, rhs, range),
                            range);
    }
  }

  return lhs;
}

static Expr *expression(Tokenizer *tokenizer, Scope *scope) {
  const int OPS[] = {',', '\0'};
  return binary_expression(tokenizer, scope, OPS, OPS, assignment_expression);
}

static Expr *constant_expression(Tokenizer *tokenizer, Scope *scope) {
  return conditional_expression(tokenizer, scope);
}

static void declaration(Tokenizer *tokenizer, Scope *scope, Vector *stmts) {
  bool is_typedef = token_consume(tokenizer, TK_TYPEDEF);
  Type *base_type = type_specifier(scope, tokenizer);
  if (token_consume(tokenizer, ';')) {
    return;
  }
  char *name;
  Type *type;
  Range range;
  declarator(scope, tokenizer, base_type, &name, &type, &range);
  if (is_typedef) {
    register_typedef(scope, name, type);
  } else {
    (void)register_stack(scope, name, type, range);
  }
  if (token_consume(tokenizer, '=')) {
    Expr *rval = assignment_expression(tokenizer, scope);
    Expr *ident = new_expr_ident(scope, name, range);
    Expr *expr =
        new_expr_binop(scope, '=', ident, rval, range_join(range, rval->range));
    Stmt *s = new_stmt_expr(expr, expr->range);
    vec_push(stmts, s);
  }
  while (token_consume(tokenizer, ',')) {
    declarator(scope, tokenizer, base_type, &name, &type, &range);
    if (is_typedef) {
      register_typedef(scope, name, type);
    } else {
      (void)register_stack(scope, name, type, range);
    }
    if (token_consume(tokenizer, '=')) {
      Expr *rval = assignment_expression(tokenizer, scope);
      Expr *ident = new_expr_ident(scope, name, range);
      Expr *expr = new_expr_binop(scope, '=', ident, rval,
                                  range_join(range, rval->range));
      Stmt *s = new_stmt_expr(expr, expr->range);
      vec_push(stmts, s);
    }
  }
  token_expect(tokenizer, ';');
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
  case TK_IDENT: {
    Type *type = get_typedef(scope, token->name);
    if (type != NULL) {
      return type;
    }
  }
    // fallthrough
  default:
    range_error(token->range, "型名がありません");
  }
}

static void struct_declaration(Scope *scope, Tokenizer *tokenizer, Type *type) {
  assert(type->ty == TY_STRUCT || type->ty == TY_UNION);
  Type *base_type = type_specifier(scope, tokenizer);
  char *member_name;
  Type *member_type;
  Range range;
  declarator(scope, tokenizer, base_type, &member_name, &member_type, &range);
  if (!register_member(type, member_name, member_type, range)) {
    range_error(range, "同じ名前のメンバ変数が複数あります: %s", member_name);
  }

  while (token_consume(tokenizer, ',')) {
    declarator(scope, tokenizer, base_type, &member_name, &member_type, &range);
    if (!register_member(type, member_name, member_type, range)) {
      range_error(range, "同じ名前のメンバ変数が複数あります: %s", member_name);
    }
  }
  token_expect(tokenizer, ';');
}

static Type *struct_or_union_specifier(Scope *scope, Tokenizer *tokenizer,
                                       Token *token) {
  assert(token->ty == TK_STRUCT || token->ty == TK_UNION);
  Token *tag = token_consume(tokenizer, TK_IDENT);
  if (tag == NULL && token_peek(tokenizer)->ty != '{') {
    range_error(token->range,
                "構造体または共用体のタグまたは `{` がありません");
  }

  if (token_consume(tokenizer, '{')) {
    Type *type = new_type_struct(token->ty, tag ? tag->name : NULL);
    if (tag != NULL) {
      if (!register_tag(scope, tag->name, type)) {
        range_error(tag->range,
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

static Stmt *statement(Tokenizer *tokenizer, Scope *scope) {
  Token *start = token_peek(tokenizer);
  switch (start->ty) {
  case TK_IF: {
    token_succ(tokenizer);
    token_expect(tokenizer, '(');
    Expr *cond = expression(tokenizer, scope);
    token_expect(tokenizer, ')');
    Stmt *then_stmt = statement(tokenizer, scope);
    Stmt *else_stmt = &null_stmt;
    Range range;
    if (token_consume(tokenizer, TK_ELSE)) {
      else_stmt = statement(tokenizer, scope);
      range = range_join(start->range, else_stmt->range);
    } else {
      range = range_join(start->range, then_stmt->range);
    }
    return new_stmt_if(cond, then_stmt, else_stmt, range);
  }
  case TK_SWITCH: {
    token_succ(tokenizer);
    token_expect(tokenizer, '(');
    Expr *cond = expression(tokenizer, scope);
    token_expect(tokenizer, ')');
    Stmt *stmt = new_stmt_switch(cond, NULL, start->range);
    vec_push(scope->func_ctxt->switches, stmt);
    stmt->body = statement(tokenizer, scope);
    stmt->range = range_join(stmt->range, stmt->body->range);
    vec_pop(scope->func_ctxt->switches);
    return stmt;
  }
  case TK_CASE: {
    token_succ(tokenizer);
    Expr *expr = constant_expression(tokenizer, scope);
    Token *end = token_expect(tokenizer, ':');
    Stmt *stmt = new_stmt_case(expr, range_join(start->range, end->range));
    if (scope->func_ctxt->switches->len <= 0) {
      range_error(stmt->range, "switch文中でない箇所にcase文があります");
    }
    Stmt *switch_stmt =
        scope->func_ctxt->switches->data[scope->func_ctxt->switches->len - 1];
    vec_push(switch_stmt->cases, stmt);
    return stmt;
  }
  case TK_DEFAULT: {
    token_succ(tokenizer);
    Token *end = token_expect(tokenizer, ':');
    Stmt *stmt = new_stmt_default(range_join(start->range, end->range));
    if (scope->func_ctxt->switches->len <= 0) {
      range_error(stmt->range, "switch文中でない箇所にcase文があります");
    }
    Stmt *switch_expr =
        scope->func_ctxt->switches->data[scope->func_ctxt->switches->len - 1];
    switch_expr->default_case = stmt;
    return stmt;
  }
  case TK_WHILE: {
    token_succ(tokenizer);
    token_expect(tokenizer, '(');
    Expr *cond = expression(tokenizer, scope);
    token_expect(tokenizer, ')');
    Stmt *body = statement(tokenizer, scope);
    return new_stmt_while(cond, body, range_join(start->range, body->range));
  }
  case TK_DO: {
    token_succ(tokenizer);
    Stmt *body = statement(tokenizer, scope);
    token_expect(tokenizer, TK_WHILE);
    token_expect(tokenizer, '(');
    Expr *cond = expression(tokenizer, scope);
    token_expect(tokenizer, ')');
    Token *end = token_expect(tokenizer, ';');
    return new_stmt_do_while(cond, body, range_join(start->range, end->range));
  }
  case TK_FOR: {
    token_succ(tokenizer);
    Expr *init = NULL;
    Expr *cond = NULL;
    Expr *inc = NULL;
    token_expect(tokenizer, '(');
    if (token_peek(tokenizer)->ty != ';') {
      init = expression(tokenizer, scope);
    }
    token_expect(tokenizer, ';');
    if (token_peek(tokenizer)->ty != ';') {
      cond = expression(tokenizer, scope);
    }
    token_expect(tokenizer, ';');
    if (token_peek(tokenizer)->ty != ')') {
      inc = expression(tokenizer, scope);
    }
    token_expect(tokenizer, ')');
    Stmt *body = statement(tokenizer, scope);
    return new_stmt_for(init, cond, inc, body,
                        range_join(start->range, body->range));
  }
  case TK_GOTO: {
    token_succ(tokenizer);
    char *name = token_expect(tokenizer, TK_IDENT)->name;
    Token *end = token_expect(tokenizer, ';');
    return new_stmt_goto(name, range_join(start->range, end->range));
  }
  case TK_BREAK: {
    token_succ(tokenizer);
    Token *end = token_expect(tokenizer, ';');
    return new_stmt(ST_BREAK, range_join(start->range, end->range));
  }
  case TK_CONTINUE: {
    token_succ(tokenizer);
    Token *end = token_expect(tokenizer, ';');
    return new_stmt(ST_CONTINUE, range_join(start->range, end->range));
  }
  case TK_RETURN: {
    token_succ(tokenizer);
    Expr *expr = NULL;
    if (token_peek(tokenizer)->ty != ';') {
      expr = expression(tokenizer, scope);
    }
    Token *end = token_expect(tokenizer, ';');
    return new_stmt_return(expr, range_join(start->range, end->range));
  }
  case '{': {
    Scope *inner = new_inner_scope(scope);
    return compound_statement(tokenizer, inner);
  }
  case ';': {
    Token *end = token_pop(tokenizer);
    return new_stmt(ST_NULL, end->range);
  }
  case TK_IDENT: {
    if (token_peek_ahead(tokenizer, 1)->ty == ':') {
      Token *ident = token_pop(tokenizer);
      Token *end = token_pop(tokenizer);
      Stmt *stmt = new_stmt_label(scope->func_ctxt, ident->name,
                                  range_join(start->range, end->range));
      return stmt;
    }
  }
  // fall through
  default: {
    Expr *expr = expression(tokenizer, scope);
    Token *end = token_expect(tokenizer, ';');
    return new_stmt_expr(expr, range_join(expr->range, end->range));
  }
  }
}

static Stmt *compound_statement(Tokenizer *tokenizer, Scope *scope) {
  Token *start = token_expect(tokenizer, '{');

  Range range = start->range;
  Vector *stmts = new_vector();
  while (!token_consume(tokenizer, '}')) {
    Token *token = token_peek(tokenizer);
    if (token_is_typename(scope, token) || token->ty == TK_TYPEDEF) {
      declaration(tokenizer, scope, stmts);
      continue;
    }
    Stmt *s = statement(tokenizer, scope);
    range = range_join(range, s->range);
    vec_push(stmts, s);
  }
  return new_stmt_compound(stmts, range);
}

static Function *function_definition(Tokenizer *tokenizer, Scope *global_scope,
                                     Type *type, char *name, Range start) {
  FuncCtxt *fcx = new_func_ctxt(name);
  Scope *scope = new_func_scope(global_scope, fcx);
  for (int i = 0; i < type->func_param->len; i++) {
    Param *param = type->func_param->data[i];
    param->stack_var =
        register_stack(scope, param->name, param->type, param->range);
  }

  Stmt *body = compound_statement(tokenizer, scope);

  Function *func = malloc(sizeof(Function));
  func->name = name;
  func->type = type;
  func->range = range_join(start, body->range);
  func->stack_size = fcx->stack_size;
  func->label_map = fcx->label_map;
  func->body = body;

  return func;
}

static GlobalVar *new_global_variable(Type *type, char *name, Range range,
                                      Expr *init) {
  GlobalVar *gvar = malloc(sizeof(GlobalVar));
  gvar->type = type;
  gvar->name = name;
  gvar->range = range;
  gvar->init = init;
  return gvar;
}

static TranslationUnit *translation_unit(Tokenizer *tokenizer) {
  GlobalCtxt *gcx = new_global_ctxt();
  Scope *scope = new_global_scope(gcx);

  Vector *func_list = new_vector();
  Vector *gvar_list = new_vector();

  while (token_peek(tokenizer)->ty != TK_EOF) {
    bool is_typedef = token_consume(tokenizer, TK_TYPEDEF);
    Type *base_type = type_specifier(scope, tokenizer);
    if (token_consume(tokenizer, ';')) {
      continue;
    }
    char *name;
    Type *type;
    Range range;
    declarator(scope, tokenizer, base_type, &name, &type, &range);

    (void)register_decl(scope, name, type, NULL);

    if (is_func_type(type) && token_peek(tokenizer)->ty == '{') {
      vec_push(func_list,
               function_definition(tokenizer, scope, type, name, range));
      continue;
    }
    if (is_typedef) {
      register_typedef(scope, name, type);
    } else {
      if (!is_func_type(type)) {
        Expr *init = NULL;
        if (token_consume(tokenizer, '=')) {
          init = assignment_expression(tokenizer, scope);
        }
        vec_push(gvar_list, new_global_variable(type, name, range, init));
      }
    }

    while (token_consume(tokenizer, ',')) {
      declarator(scope, tokenizer, base_type, &name, &type, &range);
      (void)register_decl(scope, name, type, NULL);

      if (is_typedef) {
        register_typedef(scope, name, type);
      } else {
        if (!is_func_type(type)) {
          Expr *init = NULL;
          if (token_consume(tokenizer, '=')) {
            init = assignment_expression(tokenizer, scope);
          }
          vec_push(gvar_list, new_global_variable(type, name, range, init));
        }
      }
    }
    token_expect(tokenizer, ';');
  }

  TranslationUnit *tunit = malloc(sizeof(TranslationUnit));
  tunit->func_list = func_list;
  tunit->gvar_list = gvar_list;
  tunit->str_list = gcx->str_list;
  return tunit;
}
