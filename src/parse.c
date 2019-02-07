#include "gifcc.h"
#include <assert.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct DeclItem {
  Type *type;
  StackVar *stack_var;
} DeclItem;

typedef struct Decl {
  Map *decl;
  Map *def;
  Map *struct_tag;
  struct Decl *outer;
} Decl;

typedef struct GlobalCtxt {
  Tokenizer *tokenizer;
  Map *var_map;
  Vector *str_list;
  Decl *decl;
} GlobalCtxt;

typedef struct FuncCtxt {
  GlobalCtxt *global;
  Tokenizer *tokenizer;
  char *name;
  int stack_size;
  Vector *switches;
  Map *label_map;
} FuncCtxt;

typedef struct ScopeCtxt {
  GlobalCtxt *global;
  FuncCtxt *func;
  struct ScopeCtxt *outer;
  Tokenizer *tokenizer;
  Decl *decl;
} ScopeCtxt;

static GlobalCtxt *new_global_ctxt(Tokenizer *tokenizer);
static FuncCtxt *new_func_ctxt(GlobalCtxt *gctxt, char *name);
static ScopeCtxt *new_root_scope_ctxt(FuncCtxt *fctxt);
static ScopeCtxt *new_inner_scope_ctxt(ScopeCtxt *outer);
static Decl *new_decl(Decl *outer);
static StackVar *register_stack(ScopeCtxt *sctxt, char *name, Type *type,
                                Range range);
static bool register_member(Tokenizer *tokenizer, Type *type, char *member_name,
                            Type *member_type, Range range);
static bool register_decl_item(Decl *decl, char *name, Type *type,
                               StackVar *svar);
static bool register_struct_decl(Decl *decl, char *tag, Type *type);
static Type *get_struct_decl(Decl *decl, char *tag);
static bool register_typedef(Decl *decl, char *name, Type *type);
static Type *get_typedef(Decl *decl, char *name);
static bool is_sametype(Type *ty1, Type *ty2);
static bool is_integer_type(Type *ty);
static bool is_arith_type(Type *ty);
static bool is_ptr_type(Type *ty);
static bool is_array_type(Type *ty);
static Type *integer_promoted(ScopeCtxt *sctxt, Expr **e);
static Type *arith_converted(ScopeCtxt *sctxt, Expr **e1, Expr **e2);
static bool token_is_typename(Decl *decl, Token *token);
static noreturn void binop_type_error_raw(ScopeCtxt *sctxt, int ty, Expr *lhs,
                                          Expr *rhs, const char *dbg_file,
                                          int dbg_line);
static Type *new_type(int ty);
static Type *new_type_ptr(Type *base_type);
static Type *new_type_array(Type *base_type, int len);
static Type *new_type_func(Type *ret_type, Vector *func_param);
static Type *new_type_struct(char *tag);
static Type *new_type_anon_struct(void);
static Expr *coerce_array2ptr(ScopeCtxt *sctxt, Expr *expr);
static Expr *new_expr(int ty, Type *val_type, Range range);
static Expr *new_expr_num(int val, Range range);
static Expr *new_expr_ident(ScopeCtxt *sctxt, char *name, Range range);
static Expr *new_expr_str(ScopeCtxt *sctxt, char *val, Range range);
static Expr *new_expr_call(ScopeCtxt *sctxt, Expr *callee, Vector *argument,
                           Range range);
static Expr *new_expr_postfix(ScopeCtxt *sctxt, int ty, Expr *operand,
                              Range range);
static Expr *new_expr_cast(ScopeCtxt *sctxt, Type *val_type, Expr *operand,
                           Range range);
static Expr *new_expr_unary(ScopeCtxt *sctxt, int ty, Expr *operand,
                            Range range);
static Expr *new_expr_binop(ScopeCtxt *sctxt, int ty, Expr *lhs, Expr *rhs,
                            Range range);
static Expr *new_expr_cond(ScopeCtxt *sctxt, Expr *cond, Expr *then_expr,
                           Expr *else_expr, Range range);
static Expr *new_expr_index(ScopeCtxt *sctxt, Expr *array, Expr *index,
                            Range range);
static Expr *new_expr_dot(ScopeCtxt *sctxt, Expr *operand, char *name,
                          Range range);
static Expr *new_expr_arrow(ScopeCtxt *sctxt, Expr *operand, char *name,
                            Range range);
static Stmt *new_stmt(int ty, Range range);
static Stmt *new_stmt_expr(Expr *expr, Range range);
static Stmt *new_stmt_if(Expr *cond, Stmt *then_stmt, Stmt *else_stmt,
                         Range range);
static Stmt *new_stmt_switch(Expr *cond, Stmt *body, Range range);
static Stmt *new_stmt_case(Expr *expr, Range range);
static Stmt *new_stmt_default(Range range);
static Stmt *new_stmt_label(FuncCtxt *fctxt, char *name, Range range);
static Stmt *new_stmt_while(Expr *cond, Stmt *body, Range range);
static Stmt *new_stmt_do_while(Expr *cond, Stmt *body, Range range);
static Stmt *new_stmt_for(Expr *init, Expr *cond, Expr *inc, Stmt *body,
                          Range range);
static Stmt *new_stmt_goto(char *name, Range range);
static Stmt *new_stmt_return(Expr *expr, Range range);
static Stmt *new_stmt_compound(Vector *stmts, Range range);

// expression
static Expr *primary_expression(ScopeCtxt *sctxt);
static Expr *postfix_expression(ScopeCtxt *sctxt);
static Vector *argument_expression_list(ScopeCtxt *sctxt);
static Expr *unary_expression(ScopeCtxt *sctxt);
static Expr *cast_expression(ScopeCtxt *sctxt);
static Expr *multiplicative_expression(ScopeCtxt *sctxt);
static Expr *additive_expression(ScopeCtxt *sctxt);
static Expr *shift_expression(ScopeCtxt *sctxt);
static Expr *relational_expression(ScopeCtxt *sctxt);
static Expr *equality_expression(ScopeCtxt *sctxt);
static Expr *and_expression(ScopeCtxt *sctxt);
static Expr *exclusive_or_expression(ScopeCtxt *sctxt);
static Expr *inclusive_or_expression(ScopeCtxt *sctxt);
static Expr *logical_and_expression(ScopeCtxt *sctxt);
static Expr *logical_or_expression(ScopeCtxt *sctxt);
static Expr *conditional_expression(ScopeCtxt *sctxt);
static Expr *assignment_expression(ScopeCtxt *sctxt);
static Expr *expression(ScopeCtxt *sctxt);
static Expr *constant_expression(ScopeCtxt *sctxt);

// declaration
static void declaration(ScopeCtxt *sctxt);
static Type *type_specifier(Decl *decl, Tokenizer *tokenizer);
static void struct_declaration(Decl *decl, Tokenizer *tokenizer, Type *type);
static Type *struct_or_union_specifier(Decl *decl, Tokenizer *tokenizer,
                                       Token *token);
static Type *type_name(Decl *decl, Tokenizer *tokenizer);
static void declarator(Decl *decl, Tokenizer *tokenizer, Type *base_type,
                       char **name, Type **type, Range *range);
static void direct_declarator(Decl *decl, Tokenizer *tokenizer, Type *base_type,
                              char **name, Type **type, Range *range);

// statement
static Stmt *statement(ScopeCtxt *sctxt);
static Stmt *compound_statement(ScopeCtxt *sctxt);

// top-level
static Function *function_definition(GlobalCtxt *gctxt, Type *type, char *name,
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

#define scope_error(sctxt, range, fmt, ...)                                    \
  scope_error_raw((sctxt), (range), __FILE__, __LINE__, (fmt), ##__VA_ARGS__)
static noreturn void scope_error_raw(const ScopeCtxt *sctxt, Range range,
                                     const char *dbg_file, int dbg_line,
                                     char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  reader_error_range_raw_v(token_get_reader(sctxt->tokenizer), range, dbg_file,
                           dbg_line, fmt, ap);
}

static GlobalCtxt *new_global_ctxt(Tokenizer *tokenizer) {
  GlobalCtxt *gctxt = malloc(sizeof(GlobalCtxt));
  gctxt->tokenizer = tokenizer;
  gctxt->var_map = new_map();
  gctxt->str_list = new_vector();
  gctxt->decl = new_decl(NULL);

  return gctxt;
}

static FuncCtxt *new_func_ctxt(GlobalCtxt *gctxt, char *name) {
  FuncCtxt *fctxt = malloc(sizeof(FuncCtxt));

  fctxt->global = gctxt;
  fctxt->tokenizer = gctxt->tokenizer;
  fctxt->name = name;
  fctxt->stack_size = 0;
  fctxt->switches = new_vector();
  fctxt->label_map = new_map();

  return fctxt;
}

static ScopeCtxt *new_root_scope_ctxt(FuncCtxt *fctxt) {
  ScopeCtxt *sctxt = malloc(sizeof(ScopeCtxt));

  sctxt->global = fctxt->global;
  sctxt->func = fctxt;
  sctxt->outer = NULL;
  sctxt->tokenizer = fctxt->tokenizer;
  sctxt->decl = new_decl(fctxt->global->decl);

  return sctxt;
}

static ScopeCtxt *new_inner_scope_ctxt(ScopeCtxt *outer) {
  ScopeCtxt *inner = malloc(sizeof(ScopeCtxt));

  inner->global = outer->global;
  inner->func = outer->func;
  inner->outer = outer;
  inner->tokenizer = outer->tokenizer;
  inner->decl = new_decl(outer->decl);

  return inner;
}

static Decl *new_decl(Decl *outer) {
  Decl *decl = malloc(sizeof(Decl));
  decl->decl = new_map();
  decl->def = new_map();
  decl->struct_tag = new_map();
  decl->outer = outer;
  return decl;
}

static StackVar *register_stack(ScopeCtxt *sctxt, char *name, Type *type,
                                Range range) {
  if (type->ty == TY_VOID) {
    scope_error(sctxt, range, "void型の変数は定義できません: %s", name);
  }

  FuncCtxt *fctxt = sctxt->func;

  StackVar *var = malloc(sizeof(StackVar));
  fctxt->stack_size = align(fctxt->stack_size, get_val_align(type));
  var->offset = fctxt->stack_size;
  var->type = type;
  var->range = range;
  fctxt->stack_size += get_val_size(type);

  if (!register_decl_item(sctxt->decl, name, type, var)) {
    scope_error(sctxt, range, "同じ名前のローカル変数が複数あります: %s", name);
  }

  return var;
}

static bool register_member(Tokenizer *tokenizer, Type *type, char *member_name,
                            Type *member_type, Range range) {
  assert(type->ty == TY_STRUCT);
  if (member_type->ty == TY_VOID) {
    reader_error_range(token_get_reader(tokenizer), range,
                       "void型の構造体メンバーです: %s", member_name);
  }
  if (map_get(type->members, member_name)) {
    return false;
  }

  Member *member = malloc(sizeof(Member));
  member->name = member_name;
  member->type = member_type;

  type->member_size = align(type->member_size, get_val_align(member_type));
  member->offset = type->member_size;
  type->member_size += get_val_size(member_type);

  if (type->member_align < get_val_align(member_type)) {
    type->member_align = get_val_align(member_type);
  }

  map_put(type->members, member_name, member);
  return true;
}

static bool register_decl_item(Decl *decl, char *name, Type *type,
                               StackVar *svar) {
  if (map_get(decl->decl, name)) {
    return false;
  }
  DeclItem *item = malloc(sizeof(DeclItem));
  item->type = type;
  item->stack_var = svar;
  map_put(decl->decl, name, item);
  return true;
}

static DeclItem *get_decl_item(Decl *decl, char *name) {
  while (decl != NULL) {
    DeclItem *item = map_get(decl->decl, name);
    if (item) {
      return item;
    }
    decl = decl->outer;
  }
  return NULL;
}

static bool register_struct_decl(Decl *decl, char *tag, Type *type) {
  if (map_get(decl->struct_tag, tag)) {
    return false;
  }
  map_put(decl->struct_tag, tag, type);
  return true;
}

static Type *get_struct_decl(Decl *decl, char *tag) {
  while (decl != NULL) {
    Type *type = map_get(decl->struct_tag, tag);
    if (type) {
      return type;
    }
    decl = decl->outer;
  }
  return NULL;
}

static bool register_typedef(Decl *decl, char *name, Type *type) {
  if (map_get(decl->def, name)) {
    return false;
  }
  map_put(decl->def, name, type);
  return true;
}

static Type *get_typedef(Decl *decl, char *name) {
  while (decl != NULL) {
    Type *type = map_get(decl->def, name);
    if (type) {
      return type;
    }
    decl = decl->outer;
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
static Type *integer_promoted(ScopeCtxt *sctxt, Expr **e) {
  if (!is_integer_type((*e)->val_type)) {
    return NULL;
  }
  // CHAR は INT へ昇格する
  if ((*e)->val_type->ty == TY_CHAR) {
    *e = new_expr_cast(sctxt, new_type(TY_INT), *e, (*e)->range);
  }
  return (*e)->val_type;
}
static Type *arith_converted(ScopeCtxt *sctxt, Expr **e1, Expr **e2) {
  if (!is_arith_type((*e1)->val_type) || !is_arith_type((*e2)->val_type)) {
    return NULL;
  }
  Type *ty1 = integer_promoted(sctxt, e2);
  Type *ty2 = integer_promoted(sctxt, e1);
  assert(is_sametype(ty1, ty2));
  return ty1;
}

static bool token_is_typename(Decl *decl, Token *token) {
  switch (token->ty) {
  case TK_VOID:
  case TK_INT:
  case TK_CHAR:
  case TK_STRUCT:
    return true;
  case TK_IDENT:
    return get_typedef(decl, token->name);
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
    return ty->member_align;
  }
  error("不明な型の値アラインメントを取得しようとしました");
}

#define binop_type_error(sctxt, ty, lhs, rhs)                                  \
  binop_type_error_raw((sctxt), (ty), (lhs), (rhs), __FILE__, __LINE__)
static noreturn void binop_type_error_raw(ScopeCtxt *sctxt, int ty, Expr *lhs,
                                          Expr *rhs, const char *dbg_file,
                                          int dbg_line) {
  scope_error_raw(sctxt, range_join(lhs->range, rhs->range), dbg_file, dbg_line,
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

static Type *new_type_struct(char *tag) {
  Type *type = malloc(sizeof(Type));
  type->ty = TY_STRUCT;
  type->tag = tag;
  type->members = new_map();
  type->member_size = 0;
  type->member_align = 0;
  return type;
}

static Type *new_type_anon_struct(void) {
  Type *type = malloc(sizeof(Type));
  type->ty = TY_STRUCT;
  type->members = NULL;
  return type;
}

static Expr *coerce_array2ptr(ScopeCtxt *sctxt, Expr *expr) {
  if (is_array_type(expr->val_type)) {
    return new_expr_unary(sctxt, '&', expr, expr->range);
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

static Expr *new_expr_ident(ScopeCtxt *sctxt, char *name, Range range) {
  int ty;
  Type *type;
  if (strcmp(name, "__func__") == 0) {
    if (sctxt->func == NULL) {
      scope_error(sctxt, range, "関数外で__func__が使用されました");
    }
    return new_expr_str(sctxt, sctxt->func->name, range);
  }
  DeclItem *item = get_decl_item(sctxt->decl, name);
  StackVar *svar;
  if (item != NULL) {
    if (item->stack_var != NULL) {
      ty = EX_STACK_VAR;
      type = item->type;
      svar = item->stack_var;
    } else {
      ty = EX_GLOBAL_VAR;
      type = item->type;
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

static Expr *new_expr_str(ScopeCtxt *sctxt, char *val, Range range) {
  Type *type = new_type_ptr(new_type(TY_CHAR));
  Expr *expr = new_expr(EX_STR, type, range);

  expr->name = make_label();
  StringLiteral *str = malloc(sizeof(StringLiteral));
  str->name = expr->name;
  str->val = val;
  vec_push(sctxt->global->str_list, str);
  return expr;
}

static Expr *new_expr_call(ScopeCtxt *sctxt, Expr *callee, Vector *argument,
                           Range range) {
  callee = coerce_array2ptr(sctxt, callee);
  if (argument != NULL) {
    for (int i = 0; i < argument->len; i++) {
      argument->data[i] = coerce_array2ptr(sctxt, argument->data[i]);
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

static Expr *new_expr_postfix(ScopeCtxt *sctxt, int ty, Expr *operand,
                              Range range) {
  operand = coerce_array2ptr(sctxt, operand);

  Expr *expr = new_expr(ty, operand->val_type, range);
  expr->lhs = operand;
  expr->rhs = NULL;
  return expr;
}

static Expr *new_expr_cast(ScopeCtxt *sctxt, Type *val_type, Expr *operand,
                           Range range) {
  operand = coerce_array2ptr(sctxt, operand);

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
      break;
    }
  }

  Expr *expr = new_expr(EX_CAST, val_type, range);
  expr->expr = operand;
  return expr;
}

static Expr *new_expr_unary(ScopeCtxt *sctxt, int ty, Expr *operand,
                            Range range) {
  if (ty != '&') {
    // & 以外は array は ptr とみなす
    operand = coerce_array2ptr(sctxt, operand);
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
      scope_error(sctxt, range, "ポインタ型でない値に対するデリファレンスです");
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

static Expr *new_expr_binop(ScopeCtxt *sctxt, int ty, Expr *lhs, Expr *rhs,
                            Range range) {
  lhs = coerce_array2ptr(sctxt, lhs);
  rhs = coerce_array2ptr(sctxt, rhs);

  Type *val_type;
  switch (ty) {
  // multiplicative
  case '*':
  case '/':
  case '%':
    val_type = arith_converted(sctxt, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(sctxt, ty, lhs, rhs);
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
        binop_type_error(sctxt, ty, lhs, rhs);
      }
      rhs = new_expr_binop(
          sctxt, '*', rhs,
          new_expr_num(get_val_size(lhs->val_type->ptrof), range), range);
      val_type = lhs->val_type;
      break;
    }

    if (is_ptr_type(rhs->val_type)) {
      if (!is_integer_type(lhs->val_type)) {
        binop_type_error(sctxt, ty, lhs, rhs);
      }
      lhs = new_expr_binop(
          sctxt, '*', lhs,
          new_expr_num(get_val_size(rhs->val_type->ptrof), range), range);
      val_type = rhs->val_type;
      break;
    }

    val_type = arith_converted(sctxt, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(sctxt, ty, lhs, rhs);
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
          binop_type_error(sctxt, ty, lhs, rhs);
        }
        Expr *sub = new_expr('-', new_type(TY_INT), range);
        sub->lhs = lhs;
        sub->rhs = rhs;
        return new_expr_binop(
            sctxt, '/', sub,
            new_expr_num(get_val_size(lhs->val_type->ptrof), range), range);
      }
      if (is_integer_type(rhs->val_type)) {
        rhs = new_expr_binop(
            sctxt, '*', rhs,
            new_expr_num(get_val_size(lhs->val_type->ptrof), range), range);
        val_type = lhs->val_type;
        break;
      }
      binop_type_error(sctxt, ty, lhs, rhs);
    }

    if (is_ptr_type(rhs->val_type)) {
      binop_type_error(sctxt, ty, lhs, rhs);
    }

    val_type = arith_converted(sctxt, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(sctxt, ty, lhs, rhs);
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
      binop_type_error(sctxt, ty, lhs, rhs);
    }
    val_type = integer_promoted(sctxt, &lhs);
    if (val_type == NULL) {
      binop_type_error(sctxt, ty, lhs, rhs);
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
      binop_type_error(sctxt, ty, lhs, rhs);
    }
    val_type = arith_converted(sctxt, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(sctxt, ty, lhs, rhs);
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

static Expr *new_expr_cond(ScopeCtxt *sctxt, Expr *cond, Expr *then_expr,
                           Expr *else_expr, Range range) {
  cond = coerce_array2ptr(sctxt, cond);
  then_expr = coerce_array2ptr(sctxt, then_expr);
  else_expr = coerce_array2ptr(sctxt, else_expr);

  if (!is_sametype(then_expr->val_type, else_expr->val_type)) {
    scope_error(sctxt, range, "条件演算子の両辺の型が異なります: %d, %d",
                then_expr->val_type->ty, else_expr->val_type->ty);
  }
  Expr *expr = new_expr(EX_COND, then_expr->val_type, range);
  expr->cond = cond;
  expr->lhs = then_expr;
  expr->rhs = else_expr;
  return expr;
}

static Expr *new_expr_index(ScopeCtxt *sctxt, Expr *array, Expr *index,
                            Range range) {
  array = coerce_array2ptr(sctxt, array);
  index = coerce_array2ptr(sctxt, index);

  return new_expr_unary(sctxt, '*',
                        new_expr_binop(sctxt, '+', array, index, range), range);
}

static Expr *new_expr_dot(ScopeCtxt *sctxt, Expr *operand, char *name,
                          Range range) {
  if (operand->val_type->ty != TY_STRUCT) {
    scope_error(sctxt, range, "構造体以外のメンバへのアクセスです");
  }
  return new_expr_arrow(sctxt, new_expr_unary(sctxt, '&', operand, range), name,
                        range);
}

static Expr *new_expr_arrow(ScopeCtxt *sctxt, Expr *operand, char *name,
                            Range range) {
  if (operand->val_type->ty != TY_PTR ||
      operand->val_type->ptrof->ty != TY_STRUCT) {
    scope_error(sctxt, range, "構造体以外のメンバへのアクセスです");
  }
  Member *member = operand->val_type->ptrof->members
                       ? map_get(operand->val_type->ptrof->members, name)
                       : NULL;
  if (member == NULL) {
    scope_error(sctxt, range, "存在しないメンバへのアクセスです: %s", name);
  }
  Expr *expr = new_expr('+', new_type_ptr(member->type), range);
  expr->lhs = operand;
  expr->rhs = new_expr_num(member->offset, range);
  return new_expr_unary(sctxt, '*', expr, range);
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

static Stmt *new_stmt_label(FuncCtxt *fctxt, char *name, Range range) {
  Stmt *stmt = new_stmt(ST_LABEL, range);
  stmt->name = name;
  stmt->label = make_label();
  map_put(fctxt->label_map, stmt->name, stmt->label);
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

static Expr *primary_expression(ScopeCtxt *sctxt) {
  Token *token = NULL;
  if ((token = token_consume(sctxt->tokenizer, TK_NUM)) != NULL) {
    return new_expr_num(token->val, token->range);
  }
  if ((token = token_consume(sctxt->tokenizer, TK_IDENT)) != NULL) {
    return new_expr_ident(sctxt, token->name, token->range);
  }
  if ((token = token_consume(sctxt->tokenizer, TK_STR)) != NULL) {
    return new_expr_str(sctxt, token->str, token->range);
  }
  if (token_consume(sctxt->tokenizer, '(')) {
    Expr *expr = expression(sctxt);
    token_expect(sctxt->tokenizer, ')');
    return expr;
  }
  token_error(sctxt->tokenizer, "数値でも開きカッコでもないトークンです");
}

static Expr *postfix_expression(ScopeCtxt *sctxt) {
  Token *token;
  Expr *expr = primary_expression(sctxt);
  while (true) {
    if (token_consume(sctxt->tokenizer, '[')) {
      Expr *operand = expression(sctxt);
      Token *end = token_expect(sctxt->tokenizer, ']');
      expr = new_expr_index(sctxt, expr, operand,
                            range_join(expr->range, end->range));
    } else if (token_consume(sctxt->tokenizer, '.')) {
      Token *member = token_expect(sctxt->tokenizer, TK_IDENT);
      expr = new_expr_dot(sctxt, expr, member->name,
                          range_join(expr->range, member->range));
    } else if (token_consume(sctxt->tokenizer, TK_ARROW)) {
      Token *member = token_expect(sctxt->tokenizer, TK_IDENT);
      expr = new_expr_arrow(sctxt, expr, member->name,
                            range_join(expr->range, member->range));
    } else if (token_consume(sctxt->tokenizer, '(')) {
      Vector *argument = NULL;
      if (token_peek(sctxt->tokenizer)->ty != ')') {
        argument = argument_expression_list(sctxt);
      }
      Token *end = token_expect(sctxt->tokenizer, ')');
      expr = new_expr_call(sctxt, expr, argument,
                           range_join(expr->range, end->range));
    } else if ((token = token_consume(sctxt->tokenizer, TK_INC)) != NULL) {
      int val;
      if (is_ptr_type(expr->val_type)) {
        val = get_val_size(expr->val_type->ptrof);
      } else {
        val = 1;
      }
      expr = new_expr_postfix(sctxt, EX_INC, expr,
                              range_join(expr->range, token->range));
      expr->val = val;
    } else if ((token = token_consume(sctxt->tokenizer, TK_DEC)) != NULL) {
      int val;
      if (is_ptr_type(expr->val_type)) {
        val = get_val_size(expr->val_type->ptrof);
      } else {
        val = 1;
      }
      expr = new_expr_postfix(sctxt, EX_DEC, expr, token->range);
      expr->val = val;
    } else {
      return expr;
    }
  }
}

static Vector *argument_expression_list(ScopeCtxt *sctxt) {
  Vector *argument = new_vector();
  while (true) {
    vec_push(argument, assignment_expression(sctxt));
    if (!token_consume(sctxt->tokenizer, ',')) {
      break;
    }
  }
  return argument;
}

static Expr *unary_expression(ScopeCtxt *sctxt) {
  const int OPS[] = {'&', '*', '+', '-', '~', '!', '\0'};
  for (int i = 0; OPS[i] != '\0'; i++) {
    int op = OPS[i];
    Token *token;
    if ((token = token_consume(sctxt->tokenizer, op)) != NULL) {
      Expr *operand = cast_expression(sctxt);
      return new_expr_unary(sctxt, op, operand,
                            range_join(token->range, operand->range));
    }
  }

  Token *token;
  if ((token = token_consume(sctxt->tokenizer, TK_INC)) != NULL) {
    Expr *expr = cast_expression(sctxt);
    int val;
    if (is_ptr_type(expr->val_type)) {
      val = get_val_size(expr->val_type->ptrof);
    } else {
      val = 1;
    }
    expr = new_expr_unary(sctxt, EX_INC, expr,
                          range_join(token->range, expr->range));
    expr->val = val;
    return expr;
  }
  if ((token = token_consume(sctxt->tokenizer, TK_DEC)) != NULL) {
    Expr *expr = cast_expression(sctxt);
    int val;
    if (is_ptr_type(expr->val_type)) {
      val = get_val_size(expr->val_type->ptrof);
    } else {
      val = 1;
    }
    expr = new_expr_unary(sctxt, EX_DEC, expr,
                          range_join(token->range, expr->range));
    expr->val = val;
    return expr;
  }
  return postfix_expression(sctxt);
}

static Expr *cast_expression(ScopeCtxt *sctxt) {
  Token *token = token_peek(sctxt->tokenizer);
  if (token->ty == '(' &&
      token_is_typename(sctxt->decl, token_peek_ahead(sctxt->tokenizer, 1))) {
    Range start = token->range;
    token_succ(sctxt->tokenizer);
    Type *val_type = type_name(sctxt->decl, sctxt->tokenizer);
    token_expect(sctxt->tokenizer, ')');
    Expr *operand = cast_expression(sctxt);
    return new_expr_cast(sctxt, val_type, operand,
                         range_join(start, operand->range));
  }
  return unary_expression(sctxt);
}

static Expr *binary_expression(ScopeCtxt *sctxt, const int *tks, const int *exs,
                               Expr *(*op_parser)(ScopeCtxt *sctxt)) {
  Expr *expr = op_parser(sctxt);
  bool found;
  do {
    found = false;
    for (int i = 0; tks[i] != '\0'; i++) {
      int tk = tks[i];
      int ex = exs[i];
      Token *token;
      if ((token = token_consume(sctxt->tokenizer, tk)) != NULL) {
        Expr *operand = op_parser(sctxt);
        expr = new_expr_binop(sctxt, ex, expr, operand,
                              range_join(expr->range, operand->range));
        found = true;
        break;
      }
    }
  } while (found);
  return expr;
}

static Expr *multiplicative_expression(ScopeCtxt *sctxt) {
  const int OPS[] = {'*', '/', '%', '\0'};
  return binary_expression(sctxt, OPS, OPS, cast_expression);
}

static Expr *additive_expression(ScopeCtxt *sctxt) {
  const int OPS[] = {'+', '-', '\0'};
  return binary_expression(sctxt, OPS, OPS, multiplicative_expression);
}

static Expr *shift_expression(ScopeCtxt *sctxt) {
  const int TKS[] = {TK_LSHIFT, TK_RSHIFT, '\0'};
  const int EXS[] = {EX_LSHIFT, EX_RSHIFT, '\0'};
  return binary_expression(sctxt, TKS, EXS, additive_expression);
}

static Expr *relational_expression(ScopeCtxt *sctxt) {
  const int TKS[] = {'<', '>', TK_LTEQ, TK_GTEQ, '\0'};
  const int EXS[] = {'<', '>', EX_LTEQ, EX_GTEQ, '\0'};
  return binary_expression(sctxt, TKS, EXS, shift_expression);
}

static Expr *equality_expression(ScopeCtxt *sctxt) {
  const int TKS[] = {TK_EQEQ, TK_NOTEQ, '\0'};
  const int EXS[] = {EX_EQEQ, EX_NOTEQ, '\0'};
  return binary_expression(sctxt, TKS, EXS, relational_expression);
}

static Expr *and_expression(ScopeCtxt *sctxt) {
  const int OPS[] = {'&', '\0'};
  return binary_expression(sctxt, OPS, OPS, equality_expression);
}

static Expr *exclusive_or_expression(ScopeCtxt *sctxt) {
  const int OPS[] = {'^', '\0'};
  return binary_expression(sctxt, OPS, OPS, and_expression);
}
static Expr *inclusive_or_expression(ScopeCtxt *sctxt) {
  const int OPS[] = {'|', '\0'};
  return binary_expression(sctxt, OPS, OPS, exclusive_or_expression);
}
static Expr *logical_and_expression(ScopeCtxt *sctxt) {
  const int TKS[] = {TK_LOGAND, '\0'};
  const int EXS[] = {EX_LOGAND, '\0'};
  return binary_expression(sctxt, TKS, EXS, inclusive_or_expression);
}
static Expr *logical_or_expression(ScopeCtxt *sctxt) {
  const int TKS[] = {TK_LOGOR, '\0'};
  const int EXS[] = {EX_LOGOR, '\0'};
  return binary_expression(sctxt, TKS, EXS, logical_and_expression);
}
static Expr *conditional_expression(ScopeCtxt *sctxt) {
  Expr *cond = logical_or_expression(sctxt);
  if (token_consume(sctxt->tokenizer, '?')) {
    Expr *then_expr = expression(sctxt);
    token_expect(sctxt->tokenizer, ':');
    Expr *else_expr = conditional_expression(sctxt);
    return new_expr_cond(sctxt, cond, then_expr, else_expr,
                         range_join(cond->range, else_expr->range));
  }
  return cond;
}

static Expr *assignment_expression(ScopeCtxt *sctxt) {
  Expr *lhs = conditional_expression(sctxt);
  if (token_consume(sctxt->tokenizer, '=')) {
    Expr *rhs = assignment_expression(sctxt);
    return new_expr_binop(sctxt, '=', lhs, rhs,
                          range_join(lhs->range, rhs->range));
  }
  if (token_consume(sctxt->tokenizer, TK_MUL_ASSIGN)) {
    Expr *rhs = assignment_expression(sctxt);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(sctxt, '=', lhs,
                          new_expr_binop(sctxt, '*', lhs, rhs, range), range);
  }
  if (token_consume(sctxt->tokenizer, TK_DIV_ASSIGN)) {
    Expr *rhs = assignment_expression(sctxt);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(sctxt, '=', lhs,
                          new_expr_binop(sctxt, '/', lhs, rhs, range), range);
  }
  if (token_consume(sctxt->tokenizer, TK_MOD_ASSIGN)) {
    Expr *rhs = assignment_expression(sctxt);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(sctxt, '=', lhs,
                          new_expr_binop(sctxt, '%', lhs, rhs, range), range);
  }
  if (token_consume(sctxt->tokenizer, TK_ADD_ASSIGN)) {
    Expr *rhs = assignment_expression(sctxt);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(sctxt, '=', lhs,
                          new_expr_binop(sctxt, '+', lhs, rhs, range), range);
  }
  if (token_consume(sctxt->tokenizer, TK_SUB_ASSIGN)) {
    Expr *rhs = assignment_expression(sctxt);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(sctxt, '=', lhs,
                          new_expr_binop(sctxt, '-', lhs, rhs, range), range);
  }
  if (token_consume(sctxt->tokenizer, TK_LSHIFT_ASSIGN)) {
    Expr *rhs = assignment_expression(sctxt);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(sctxt, '=', lhs,
                          new_expr_binop(sctxt, EX_LSHIFT, lhs, rhs, range),
                          range);
  }
  if (token_consume(sctxt->tokenizer, TK_RSHIFT_ASSIGN)) {
    Expr *rhs = assignment_expression(sctxt);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(sctxt, '=', lhs,
                          new_expr_binop(sctxt, EX_RSHIFT, lhs, rhs, range),
                          range);
  }
  if (token_consume(sctxt->tokenizer, TK_AND_ASSIGN)) {
    Expr *rhs = assignment_expression(sctxt);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(sctxt, '=', lhs,
                          new_expr_binop(sctxt, '&', lhs, rhs, range), range);
  }
  if (token_consume(sctxt->tokenizer, TK_OR_ASSIGN)) {
    Expr *rhs = assignment_expression(sctxt);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(sctxt, '=', lhs,
                          new_expr_binop(sctxt, '|', lhs, rhs, range), range);
  }
  if (token_consume(sctxt->tokenizer, TK_XOR_ASSIGN)) {
    Expr *rhs = assignment_expression(sctxt);
    Range range = range_join(lhs->range, rhs->range);
    return new_expr_binop(sctxt, '=', lhs,
                          new_expr_binop(sctxt, '^', lhs, rhs, range), range);
  }
  return lhs;
}

static Expr *expression(ScopeCtxt *sctxt) {
  const int OPS[] = {',', '\0'};
  return binary_expression(sctxt, OPS, OPS, assignment_expression);
}

static Expr *constant_expression(ScopeCtxt *sctxt) {
  return conditional_expression(sctxt);
}

static void declaration(ScopeCtxt *sctxt) {
  bool is_typedef = token_consume(sctxt->tokenizer, TK_TYPEDEF);
  Type *base_type = type_specifier(sctxt->decl, sctxt->tokenizer);
  if (token_consume(sctxt->tokenizer, ';')) {
    return;
  }
  char *name;
  Type *type;
  Range range;
  declarator(sctxt->decl, sctxt->tokenizer, base_type, &name, &type, &range);
  if (is_typedef) {
    register_typedef(sctxt->decl, name, type);
  } else {
    (void)register_stack(sctxt, name, type, range);
  }
  while (token_consume(sctxt->tokenizer, ',')) {
    declarator(sctxt->decl, sctxt->tokenizer, base_type, &name, &type, &range);
    if (is_typedef) {
      register_typedef(sctxt->decl, name, type);
    } else {
      (void)register_stack(sctxt, name, type, range);
    }
  }
  token_expect(sctxt->tokenizer, ';');
}

static Type *type_specifier(Decl *decl, Tokenizer *tokenizer) {
  Token *token = token_pop(tokenizer);
  switch (token->ty) {
  case TK_CHAR:
    return new_type(TY_CHAR);
  case TK_INT:
    return new_type(TY_INT);
  case TK_VOID:
    return new_type(TY_VOID);
  case TK_STRUCT:
    return struct_or_union_specifier(decl, tokenizer, token);
  case TK_IDENT:
    return get_typedef(decl, token->name);
  default:
    token_error_with(tokenizer, token, "型名がありません");
  }
}

static void struct_declaration(Decl *decl, Tokenizer *tokenizer, Type *type) {
  assert(type->ty == TY_STRUCT);
  Type *base_type = type_specifier(decl, tokenizer);
  char *member_name;
  Type *member_type;
  Range range;
  declarator(decl, tokenizer, base_type, &member_name, &member_type, &range);
  if (!register_member(tokenizer, type, member_name, member_type, range)) {
    reader_error_range(token_get_reader(tokenizer), range,
                       "同じ名前のメンバ変数が複数あります: %s", member_name);
  }

  while (token_consume(tokenizer, ',')) {
    declarator(decl, tokenizer, base_type, &member_name, &member_type, &range);
    if (!register_member(tokenizer, type, member_name, member_type, range)) {
      reader_error_range(token_get_reader(tokenizer), range,
                         "同じ名前のメンバ変数が複数あります: %s", member_name);
    }
  }
  token_expect(tokenizer, ';');
}

static Type *struct_or_union_specifier(Decl *decl, Tokenizer *tokenizer,
                                       Token *token) {
  Token *tag = token_consume(tokenizer, TK_IDENT);
  if (tag == NULL && token_peek(tokenizer)->ty != '{') {
    token_error_with(tokenizer, token, "構造体のタグまたは `{` がありません");
  }

  if (token_consume(tokenizer, '{')) {
    Type *type = new_type_struct(tag ? tag->name : NULL);
    if (tag != NULL) {
      if (!register_struct_decl(decl, tag->name, type)) {
        token_error_with(tokenizer, tag, "同じタグ名の構造体の多重定義です: %s",
                         tag->name);
      }
    }
    while (token_peek(tokenizer)->ty != '}') {
      struct_declaration(decl, tokenizer, type);
    }
    token_expect(tokenizer, '}');
    return type;
  }

  Type *type = get_struct_decl(decl, tag->name);
  if (type != NULL) {
    return type;
  }

  return new_type_anon_struct();
}

static Type *type_name(Decl *decl, Tokenizer *tokenizer) {
  Type *type = type_specifier(decl, tokenizer);
  while (token_consume(tokenizer, '*')) {
    type = new_type_ptr(type);
  }
  while (token_consume(tokenizer, '[')) {
    type = new_type_array(type, token_expect(tokenizer, TK_NUM)->val);
    token_expect(tokenizer, ']');
  }
  return type;
}

static void declarator(Decl *decl, Tokenizer *tokenizer, Type *base_type,
                       char **name, Type **type, Range *range) {
  Range start = token_peek(tokenizer)->range;
  while (token_consume(tokenizer, '*')) {
    base_type = new_type_ptr(base_type);
  }
  Range end;
  direct_declarator(decl, tokenizer, base_type, name, type, &end);
  *range = range_join(start, end);
}

static void direct_declarator(Decl *decl, Tokenizer *tokenizer, Type *base_type,
                              char **name, Type **type, Range *range) {
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
      declarator(decl, tokenizer, placeholder, name, type, &mid);
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
          Type *base_type = type_specifier(decl, tokenizer);
          Param *param = malloc(sizeof(Param));
          declarator(decl, tokenizer, base_type, &param->name, &param->type,
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

static Stmt *statement(ScopeCtxt *sctxt) {
  Token *start = token_peek(sctxt->tokenizer);
  switch (start->ty) {
  case TK_IF: {
    token_succ(sctxt->tokenizer);
    token_expect(sctxt->tokenizer, '(');
    Expr *cond = expression(sctxt);
    token_expect(sctxt->tokenizer, ')');
    Stmt *then_stmt = statement(sctxt);
    Stmt *else_stmt = &null_stmt;
    Range range;
    if (token_consume(sctxt->tokenizer, TK_ELSE)) {
      else_stmt = statement(sctxt);
      range = range_join(start->range, else_stmt->range);
    } else {
      range = range_join(start->range, then_stmt->range);
    }
    return new_stmt_if(cond, then_stmt, else_stmt, range);
  }
  case TK_SWITCH: {
    token_succ(sctxt->tokenizer);
    token_expect(sctxt->tokenizer, '(');
    Expr *cond = expression(sctxt);
    token_expect(sctxt->tokenizer, ')');
    Stmt *stmt = new_stmt_switch(cond, NULL, start->range);
    vec_push(sctxt->func->switches, stmt);
    stmt->body = statement(sctxt);
    stmt->range = range_join(stmt->range, stmt->body->range);
    vec_pop(sctxt->func->switches);
    return stmt;
  }
  case TK_CASE: {
    token_succ(sctxt->tokenizer);
    Expr *expr = constant_expression(sctxt);
    Token *end = token_expect(sctxt->tokenizer, ':');
    Stmt *stmt = new_stmt_case(expr, range_join(start->range, end->range));
    if (sctxt->func->switches->len <= 0) {
      scope_error(sctxt, stmt->range, "switch文中でない箇所にcase文があります");
    }
    Stmt *switch_stmt =
        sctxt->func->switches->data[sctxt->func->switches->len - 1];
    vec_push(switch_stmt->cases, stmt);
    return stmt;
  }
  case TK_DEFAULT: {
    token_succ(sctxt->tokenizer);
    Token *end = token_expect(sctxt->tokenizer, ':');
    Stmt *stmt = new_stmt_default(range_join(start->range, end->range));
    if (sctxt->func->switches->len <= 0) {
      scope_error(sctxt, stmt->range, "switch文中でない箇所にcase文があります");
    }
    Stmt *switch_expr =
        sctxt->func->switches->data[sctxt->func->switches->len - 1];
    switch_expr->default_case = stmt;
    return stmt;
  }
  case TK_WHILE: {
    token_succ(sctxt->tokenizer);
    token_expect(sctxt->tokenizer, '(');
    Expr *cond = expression(sctxt);
    token_expect(sctxt->tokenizer, ')');
    Stmt *body = statement(sctxt);
    return new_stmt_while(cond, body, range_join(start->range, body->range));
  }
  case TK_DO: {
    token_succ(sctxt->tokenizer);
    Stmt *body = statement(sctxt);
    token_expect(sctxt->tokenizer, TK_WHILE);
    token_expect(sctxt->tokenizer, '(');
    Expr *cond = expression(sctxt);
    token_expect(sctxt->tokenizer, ')');
    Token *end = token_expect(sctxt->tokenizer, ';');
    return new_stmt_do_while(cond, body, range_join(start->range, end->range));
  }
  case TK_FOR: {
    token_succ(sctxt->tokenizer);
    Expr *init = NULL;
    Expr *cond = NULL;
    Expr *inc = NULL;
    token_expect(sctxt->tokenizer, '(');
    if (token_peek(sctxt->tokenizer)->ty != ';') {
      init = expression(sctxt);
    }
    token_expect(sctxt->tokenizer, ';');
    if (token_peek(sctxt->tokenizer)->ty != ';') {
      cond = expression(sctxt);
    }
    token_expect(sctxt->tokenizer, ';');
    if (token_peek(sctxt->tokenizer)->ty != ')') {
      inc = expression(sctxt);
    }
    token_expect(sctxt->tokenizer, ')');
    Stmt *body = statement(sctxt);
    return new_stmt_for(init, cond, inc, body,
                        range_join(start->range, body->range));
  }
  case TK_GOTO: {
    token_succ(sctxt->tokenizer);
    char *name = token_expect(sctxt->tokenizer, TK_IDENT)->name;
    Token *end = token_expect(sctxt->tokenizer, ';');
    return new_stmt_goto(name, range_join(start->range, end->range));
  }
  case TK_BREAK: {
    token_succ(sctxt->tokenizer);
    Token *end = token_expect(sctxt->tokenizer, ';');
    return new_stmt(ST_BREAK, range_join(start->range, end->range));
  }
  case TK_CONTINUE: {
    token_succ(sctxt->tokenizer);
    Token *end = token_expect(sctxt->tokenizer, ';');
    return new_stmt(ST_CONTINUE, range_join(start->range, end->range));
  }
  case TK_RETURN: {
    token_succ(sctxt->tokenizer);
    Expr *expr = NULL;
    if (token_peek(sctxt->tokenizer)->ty != ';') {
      expr = expression(sctxt);
    }
    Token *end = token_expect(sctxt->tokenizer, ';');
    return new_stmt_return(expr, range_join(start->range, end->range));
  }
  case '{': {
    ScopeCtxt *inner = new_inner_scope_ctxt(sctxt);
    return compound_statement(inner);
  }
  case ';': {
    token_succ(sctxt->tokenizer);
    return &null_stmt;
  }
  case TK_IDENT: {
    if (token_peek_ahead(sctxt->tokenizer, 1)->ty == ':') {
      Token *ident = token_pop(sctxt->tokenizer);
      Token *end = token_pop(sctxt->tokenizer);
      Stmt *stmt = new_stmt_label(sctxt->func, ident->name,
                                  range_join(start->range, end->range));
      return stmt;
    }
  }
  // fall through
  default: {
    Expr *expr = expression(sctxt);
    Token *end = token_expect(sctxt->tokenizer, ';');
    return new_stmt_expr(expr, range_join(expr->range, end->range));
  }
  }
}

static Stmt *compound_statement(ScopeCtxt *sctxt) {
  Token *start = token_expect(sctxt->tokenizer, '{');

  Range range = start->range;
  Vector *stmts = new_vector();
  while (!token_consume(sctxt->tokenizer, '}')) {
    Token *token = token_peek(sctxt->tokenizer);
    if (token_is_typename(sctxt->decl, token) || token->ty == TK_TYPEDEF) {
      declaration(sctxt);
      continue;
    }
    Stmt *s = statement(sctxt);
    range = range_join(range, s->range);
    vec_push(stmts, s);
  }
  return new_stmt_compound(stmts, range);
}

static Function *function_definition(GlobalCtxt *gctxt, Type *type, char *name,
                                     Range start) {
  FuncCtxt *fctxt = new_func_ctxt(gctxt, name);
  ScopeCtxt *sctxt = new_root_scope_ctxt(fctxt);
  for (int i = 0; i < type->func_param->len; i++) {
    Param *param = type->func_param->data[i];
    param->stack_var =
        register_stack(sctxt, param->name, param->type, param->range);
  }

  Stmt *body = compound_statement(sctxt);

  Function *func = malloc(sizeof(Function));
  func->name = name;
  func->type = type;
  func->range = range_join(start, body->range);
  func->stack_size = fctxt->stack_size;
  func->label_map = fctxt->label_map;
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
  GlobalCtxt *gctxt = new_global_ctxt(tokenizer);

  Vector *func_list = new_vector();
  Vector *gvar_list = new_vector();

  while (token_peek(tokenizer)->ty != TK_EOF) {
    bool is_typedef = token_consume(tokenizer, TK_TYPEDEF);
    Type *base_type = type_specifier(gctxt->decl, gctxt->tokenizer);
    if (token_consume(gctxt->tokenizer, ';')) {
      continue;
    }
    char *name;
    Type *type;
    Range range;
    declarator(gctxt->decl, gctxt->tokenizer, base_type, &name, &type, &range);

    (void)register_decl_item(gctxt->decl, name, type, NULL);

    if (is_func_type(type) && token_peek(gctxt->tokenizer)->ty == '{') {
      vec_push(func_list, function_definition(gctxt, type, name, range));
      continue;
    }
    if (is_typedef) {
      register_typedef(gctxt->decl, name, type);
    } else {
      if (!is_func_type(type)) {
        vec_push(gvar_list, new_global_variable(type, name, range));
      }
    }

    while (token_consume(gctxt->tokenizer, ',')) {
      declarator(gctxt->decl, gctxt->tokenizer, base_type, &name, &type,
                 &range);
      (void)register_decl_item(gctxt->decl, name, type, NULL);

      if (is_typedef) {
        register_typedef(gctxt->decl, name, type);
      } else {
        if (!is_func_type(type)) {
          vec_push(gvar_list, new_global_variable(type, name, range));
        }
      }
    }
    token_expect(gctxt->tokenizer, ';');
  }

  TranslationUnit *tunit = malloc(sizeof(TranslationUnit));
  tunit->func_list = func_list;
  tunit->gvar_list = gvar_list;
  tunit->str_list = gctxt->str_list;
  return tunit;
}
