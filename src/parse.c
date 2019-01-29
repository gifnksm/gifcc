#include "gifcc.h"
#include <assert.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct GlobalCtxt {
  Tokenizer *tokenizer;
  Map *var_map;
  Vector *str_list;
} GlobalCtxt;

typedef struct FuncCtxt {
  GlobalCtxt *global;
  Tokenizer *tokenizer;
  int stack_size;
  Vector *switches;
  Map *label_map;
} FuncCtxt;

typedef struct ScopeCtxt {
  GlobalCtxt *global;
  FuncCtxt *func;
  struct ScopeCtxt *outer;
  Tokenizer *tokenizer;
  Map *stack_map;
} ScopeCtxt;

static GlobalCtxt *new_global_ctxt(Tokenizer *tokenizer);
static FuncCtxt *new_func_ctxt(GlobalCtxt *gctxt);
static ScopeCtxt *new_root_scope_ctxt(FuncCtxt *fctxt);
static ScopeCtxt *new_inner_scope_ctxt(ScopeCtxt *outer);
static StackVar *register_stack(ScopeCtxt *sctxt, char *name, Type *type);
static StackVar *get_stack(ScopeCtxt *sctxt, char *name);
static bool register_global(GlobalCtxt *gctxt, char *name, Type *type);
static GlobalVar *get_global(GlobalCtxt *gctxt, char *name);
static bool is_sametype(Type *ty1, Type *ty2);
static bool is_integer_type(Type *ty);
static bool is_arith_type(Type *ty);
static bool is_ptr_type(Type *ty);
static bool is_array_type(Type *ty);
static Type *integer_promoted(Expr **e);
static Type *arith_converted(Expr **e1, Expr **e2);
static bool token_is_typename(Token *token);
static noreturn void binop_type_error(int ty, Expr *lhs, Expr *rhs);
static Type *new_type(int ty);
static Type *new_type_ptr(Type *base_type);
static Type *new_type_array(Type *base_type, int len);
static Type *new_type_func(Type *ret_type, Vector *func_param);
static Expr *coerce_array2ptr(Expr *expr);
static Expr *new_expr(int ty, Type *val_type);
static Expr *new_expr_num(int val);
static Expr *new_expr_ident(ScopeCtxt *sctxt, char *name);
static Expr *new_expr_str(ScopeCtxt *sctxt, char *val);
static Expr *new_expr_call(Expr *callee, Vector *argument);
static Expr *new_expr_postfix(int ty, Expr *operand);
static Expr *new_expr_cast(Type *val_type, Expr *operand);
static Expr *new_expr_unary(int ty, Expr *operand);
static Expr *new_expr_binop(int ty, Expr *lhs, Expr *rhs);
static Expr *new_expr_cond(Expr *cond, Expr *then_expr, Expr *else_expr);
static Expr *new_expr_index(Expr *array, Expr *index);
static Stmt *new_stmt(int ty);
static Stmt *new_stmt_expr(Expr *expr);
static Stmt *new_stmt_if(Expr *cond, Stmt *then_stmt, Stmt *else_stmt);
static Stmt *new_stmt_switch(Expr *cond, Stmt *body);
static Stmt *new_stmt_case(Expr *expr);
static Stmt *new_stmt_default(void);
static Stmt *new_stmt_label(FuncCtxt *fctxt, char *name);
static Stmt *new_stmt_while(Expr *cond, Stmt *body);
static Stmt *new_stmt_do_while(Expr *cond, Stmt *body);
static Stmt *new_stmt_for(Expr *init, Expr *cond, Expr *inc, Stmt *body);
static Stmt *new_stmt_goto(char *name);
static Stmt *new_stmt_return(Expr *expr);

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
static Type *type_specifier(Tokenizer *tokenizer);
static Type *type_name(Tokenizer *tokenizer);
static void declarator(Tokenizer *tokenizer, Type *base_type, char **name,
                       Type **type);
static void direct_declarator(Tokenizer *tokenizer, Type *base_type,
                              char **name, Type **type);

// statement
static Stmt *statement(ScopeCtxt *sctxt);
static Stmt *compound_statement(ScopeCtxt *sctxt);

// top-level
static Function *function_definition(GlobalCtxt *gctxt, Type *type, char *name);
static GlobalVar *global_variable(GlobalCtxt *gctxt, Type *type, char *name);
static TranslationUnit *translation_unit(Tokenizer *tokenizer);

static Stmt null_stmt = {
    .ty = ST_NULL,
};

TranslationUnit *parse(const char *input) {
  Tokenizer *tokenizer = new_tokenizer(input);
  return translation_unit(tokenizer);
}

static GlobalCtxt *new_global_ctxt(Tokenizer *tokenizer) {
  GlobalCtxt *gctxt = malloc(sizeof(GlobalCtxt));
  gctxt->tokenizer = tokenizer;
  gctxt->var_map = new_map();
  gctxt->str_list = new_vector();

  return gctxt;
}

static FuncCtxt *new_func_ctxt(GlobalCtxt *gctxt) {
  FuncCtxt *fctxt = malloc(sizeof(FuncCtxt));

  fctxt->global = gctxt;
  fctxt->tokenizer = gctxt->tokenizer;
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
  sctxt->stack_map = new_map();

  return sctxt;
}

static ScopeCtxt *new_inner_scope_ctxt(ScopeCtxt *outer) {
  ScopeCtxt *inner = malloc(sizeof(ScopeCtxt));

  inner->global = outer->global;
  inner->func = outer->func;
  inner->outer = outer;
  inner->tokenizer = outer->tokenizer;
  inner->stack_map = new_map();

  return inner;
}

static StackVar *register_stack(ScopeCtxt *sctxt, char *name, Type *type) {
  if (type->ty == TY_VOID) {
    error("void型の変数は定義できません: %s", name);
  }
  if (map_get(sctxt->stack_map, name)) {
    error("同じ名前のローカル変数が複数あります: %s", name);
    return NULL;
  }

  FuncCtxt *fctxt = sctxt->func;

  StackVar *var = malloc(sizeof(StackVar));
  fctxt->stack_size = align(fctxt->stack_size, get_val_align(type));
  var->offset = fctxt->stack_size;
  var->type = type;
  map_put(sctxt->stack_map, name, var);
  fctxt->stack_size += get_val_size(type);

  return var;
}

static StackVar *get_stack(ScopeCtxt *sctxt, char *name) {
  while (sctxt != NULL) {
    StackVar *var = map_get(sctxt->stack_map, name);
    if (var != NULL) {
      return var;
    }
    sctxt = sctxt->outer;
  }
  return NULL;
}

static bool register_global(GlobalCtxt *gctxt, char *name, Type *type) {
  if (type->ty == TY_VOID) {
    error("void型の変数は定義できません");
  }
  if (map_get(gctxt->var_map, name)) {
    return false;
  }

  GlobalVar *var = malloc(sizeof(GlobalVar));
  var->type = type;
  var->name = name;
  map_put(gctxt->var_map, name, var);
  return true;
}

static GlobalVar *get_global(GlobalCtxt *gctxt, char *name) {
  return map_get(gctxt->var_map, name);
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
static Type *integer_promoted(Expr **e) {
  if (!is_integer_type((*e)->val_type)) {
    return NULL;
  }
  // CHAR は INT へ昇格する
  if ((*e)->val_type->ty == TY_CHAR) {
    *e = new_expr_cast(new_type(TY_INT), *e);
  }
  return (*e)->val_type;
}
static Type *arith_converted(Expr **e1, Expr **e2) {
  if (!is_arith_type((*e1)->val_type) || !is_arith_type((*e2)->val_type)) {
    return NULL;
  }
  Type *ty1 = integer_promoted(e2);
  Type *ty2 = integer_promoted(e1);
  assert(is_sametype(ty1, ty2));
  return ty1;
}

static bool token_is_typename(Token *token) {
  switch (token->ty) {
  case TK_VOID:
  case TK_INT:
  case TK_CHAR:
    return true;
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
  }
  error("不明な型の値アラインメントを取得しようとしました");
}

static noreturn void binop_type_error(int ty, Expr *lhs, Expr *rhs) {
  error("不正な型の値に対する演算です: 演算=%d, 左辺=%d, 右辺=%d", ty,
        lhs->val_type->ty, rhs->val_type->ty);
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

static Expr *coerce_array2ptr(Expr *expr) {
  if (is_array_type(expr->val_type)) {
    return new_expr_unary('&', expr);
  }
  return expr;
}

static Expr *new_expr(int ty, Type *val_type) {
  Expr *expr = malloc(sizeof(Expr));
  expr->ty = ty;
  expr->val_type = val_type;
  return expr;
}

static Expr *new_expr_num(int val) {
  Expr *expr = new_expr(EX_NUM, new_type(TY_INT));
  expr->val = val;
  return expr;
}

static Expr *new_expr_ident(ScopeCtxt *sctxt, char *name) {
  int ty;
  Type *type;
  StackVar *svar = get_stack(sctxt, name);
  GlobalVar *gvar = get_global(sctxt->global, name);
  if (svar != NULL) {
    ty = EX_STACK_VAR;
    type = svar->type;
  } else if (gvar != NULL) {
    ty = EX_GLOBAL_VAR;
    type = gvar->type;
  } else {
    // 未知の識別子はint型のグローバル変数として扱う
    ty = EX_GLOBAL_VAR;
    type = new_type(TY_INT);
  }
  Expr *expr = new_expr(ty, type);
  expr->name = name;
  if (svar != NULL) {
    expr->stack_var = svar;
  } else {
    expr->global_var = gvar;
  }
  return expr;
}

static Expr *new_expr_str(ScopeCtxt *sctxt, char *val) {
  Type *type = new_type_ptr(new_type(TY_CHAR));
  Expr *expr = new_expr(EX_STR, type);

  expr->name = make_label();
  StringLiteral *str = malloc(sizeof(StringLiteral));
  str->name = expr->name;
  str->val = val;
  vec_push(sctxt->global->str_list, str);
  return expr;
}

static Expr *new_expr_call(Expr *callee, Vector *argument) {
  callee = coerce_array2ptr(callee);
  if (argument != NULL) {
    for (int i = 0; i < argument->len; i++) {
      argument->data[i] = coerce_array2ptr(argument->data[i]);
    }
  }

  Type *ret_type;
  if (is_func_type(callee->val_type)) {
    ret_type = callee->val_type->func_ret;
  } else {
    // TODO: 定義のない関数の戻り値はintを仮定する
    ret_type = new_type(TY_INT);
  }
  Expr *expr = new_expr(EX_CALL, ret_type);
  expr->callee = callee;
  expr->argument = argument;
  return expr;
}

static Expr *new_expr_postfix(int ty, Expr *operand) {
  operand = coerce_array2ptr(operand);

  Expr *expr = new_expr(ty, operand->val_type);
  expr->lhs = operand;
  expr->rhs = NULL;
  return expr;
}

static Expr *new_expr_cast(Type *val_type, Expr *operand) {
  operand = coerce_array2ptr(operand);

  if (is_sametype(operand->val_type, val_type)) {
    return operand;
  }

  Expr *expr = new_expr(EX_CAST, val_type);
  expr->expr = operand;
  return expr;
}

static Expr *new_expr_unary(int ty, Expr *operand) {
  if (ty != '&') {
    // & 以外は array は ptr とみなす
    operand = coerce_array2ptr(operand);
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
      error("ポインタ型でない値に対するデリファレンスです");
    }
    val_type = operand->val_type->ptrof;
  } else {
    val_type = operand->val_type;
  }
  Expr *expr = new_expr(ty, val_type);
  expr->lhs = NULL;
  expr->rhs = operand;
  return expr;
}

static Expr *new_expr_binop(int ty, Expr *lhs, Expr *rhs) {
  lhs = coerce_array2ptr(lhs);
  rhs = coerce_array2ptr(rhs);

  Type *val_type;
  switch (ty) {
  // multiplicative
  case '*':
  case '/':
  case '%':
    val_type = arith_converted(&lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(ty, lhs, rhs);
    }
    break;
  // additive
  case '+':
    if (is_ptr_type(lhs->val_type)) {
      if (is_ptr_type(rhs->val_type) || !is_integer_type(rhs->val_type)) {
        binop_type_error(ty, lhs, rhs);
      }
      rhs = new_expr_binop('*', rhs,
                           new_expr_num(get_val_size(lhs->val_type->ptrof)));
      val_type = lhs->val_type;
      break;
    }

    if (is_ptr_type(rhs->val_type)) {
      if (!is_integer_type(lhs->val_type)) {
        binop_type_error(ty, lhs, rhs);
      }
      lhs = new_expr_binop('*', lhs,
                           new_expr_num(get_val_size(rhs->val_type->ptrof)));
      val_type = rhs->val_type;
      break;
    }

    val_type = arith_converted(&lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(ty, lhs, rhs);
    }
    break;
  case '-':
    if (is_ptr_type(lhs->val_type)) {
      if (is_ptr_type(rhs->val_type)) {
        if (!is_sametype(lhs->val_type, rhs->val_type)) {
          binop_type_error(ty, lhs, rhs);
        }
        Expr *sub = new_expr('-', new_type(TY_INT));
        sub->lhs = lhs;
        sub->rhs = rhs;
        return new_expr_binop('/', sub,
                              new_expr_num(get_val_size(lhs->val_type->ptrof)));
      }
      if (is_integer_type(rhs->val_type)) {
        rhs = new_expr_binop('*', rhs,
                             new_expr_num(get_val_size(lhs->val_type->ptrof)));
        val_type = lhs->val_type;
        break;
      }
      binop_type_error(ty, lhs, rhs);
    }

    if (is_ptr_type(rhs->val_type)) {
      binop_type_error(ty, lhs, rhs);
    }

    val_type = arith_converted(&lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(ty, lhs, rhs);
    }
    break;
  // shift
  case EX_LSHIFT:
  case EX_RSHIFT:
    if (!is_integer_type(lhs->val_type) || !is_integer_type(rhs->val_type)) {
      binop_type_error(ty, lhs, rhs);
    }
    val_type = integer_promoted(&lhs);
    if (val_type == NULL) {
      binop_type_error(ty, lhs, rhs);
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
    val_type = arith_converted(&lhs, &rhs);
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

  Expr *expr = new_expr(ty, val_type);
  expr->lhs = lhs;
  expr->rhs = rhs;
  return expr;
}

static Expr *new_expr_cond(Expr *cond, Expr *then_expr, Expr *else_expr) {
  cond = coerce_array2ptr(cond);
  then_expr = coerce_array2ptr(then_expr);
  else_expr = coerce_array2ptr(else_expr);

  if (!is_sametype(then_expr->val_type, else_expr->val_type)) {
    error("条件演算子の両辺の型が異なります: %d, %d", then_expr->val_type->ty,
          else_expr->val_type->ty);
  }
  Expr *expr = new_expr(EX_COND, then_expr->val_type);
  expr->cond = cond;
  expr->lhs = then_expr;
  expr->rhs = else_expr;
  return expr;
}

static Expr *new_expr_index(Expr *array, Expr *index) {
  array = coerce_array2ptr(array);
  index = coerce_array2ptr(index);

  return new_expr_unary('*', new_expr_binop('+', array, index));
}

static Stmt *new_stmt(int ty) {
  Stmt *stmt = malloc(sizeof(Stmt));
  stmt->ty = ty;
  return stmt;
}

static Stmt *new_stmt_expr(Expr *expr) {
  Stmt *stmt = new_stmt(ST_EXPR);
  stmt->expr = expr;
  return stmt;
}

static Stmt *new_stmt_if(Expr *cond, Stmt *then_stmt, Stmt *else_stmt) {
  Stmt *stmt = new_stmt(ST_IF);
  stmt->cond = cond;
  stmt->then_stmt = then_stmt;
  stmt->else_stmt = else_stmt;
  return stmt;
}

static Stmt *new_stmt_switch(Expr *cond, Stmt *body) {
  Stmt *stmt = new_stmt(ST_SWITCH);
  stmt->cond = cond;
  stmt->body = body;
  stmt->cases = new_vector();
  stmt->default_case = NULL;
  return stmt;
}

static Stmt *new_stmt_case(Expr *expr) {
  Stmt *stmt = new_stmt(ST_CASE);
  stmt->expr = expr;
  stmt->label = make_label();
  return stmt;
}

static Stmt *new_stmt_default(void) {
  Stmt *stmt = new_stmt(ST_DEFAULT);
  stmt->label = make_label();
  return stmt;
}

static Stmt *new_stmt_label(FuncCtxt *fctxt, char *name) {
  Stmt *stmt = new_stmt(ST_LABEL);
  stmt->name = name;
  stmt->label = make_label();
  map_put(fctxt->label_map, stmt->name, stmt->label);
  return stmt;
}

static Stmt *new_stmt_while(Expr *cond, Stmt *body) {
  Stmt *stmt = new_stmt(ST_WHILE);
  stmt->cond = cond;
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_do_while(Expr *cond, Stmt *body) {
  Stmt *stmt = new_stmt(ST_DO_WHILE);
  stmt->cond = cond;
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_for(Expr *init, Expr *cond, Expr *inc, Stmt *body) {
  Stmt *stmt = new_stmt(ST_FOR);
  stmt->init = init;
  stmt->cond = cond;
  stmt->inc = inc;
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_goto(char *name) {
  Stmt *stmt = new_stmt(ST_GOTO);
  stmt->name = name;
  return stmt;
}

static Stmt *new_stmt_return(Expr *expr) {
  Stmt *stmt = new_stmt(ST_RETURN);
  stmt->expr = expr;
  return stmt;
}

static Expr *primary_expression(ScopeCtxt *sctxt) {
  Token *token = NULL;
  if ((token = token_consume(sctxt->tokenizer, TK_NUM)) != NULL) {
    return new_expr_num(token->val);
  }
  if ((token = token_consume(sctxt->tokenizer, TK_IDENT)) != NULL) {
    return new_expr_ident(sctxt, token->name);
  }
  if ((token = token_consume(sctxt->tokenizer, TK_STR)) != NULL) {
    return new_expr_str(sctxt, token->str);
  }
  if (token_consume(sctxt->tokenizer, '(')) {
    Expr *expr = expression(sctxt);
    token_expect(sctxt->tokenizer, ')');
    return expr;
  }
  error("数値でも開きカッコでもないトークンです: %s",
        token_peek(sctxt->tokenizer)->input);
}

static Expr *postfix_expression(ScopeCtxt *sctxt) {
  Expr *expr = primary_expression(sctxt);
  while (true) {
    if (token_consume(sctxt->tokenizer, '[')) {
      Expr *operand = expression(sctxt);
      token_expect(sctxt->tokenizer, ']');
      expr = new_expr_index(expr, operand);
    } else if (token_consume(sctxt->tokenizer, '(')) {
      Vector *argument = NULL;
      if (token_peek(sctxt->tokenizer)->ty != ')') {
        argument = argument_expression_list(sctxt);
      }
      token_expect(sctxt->tokenizer, ')');
      expr = new_expr_call(expr, argument);
    } else if (token_consume(sctxt->tokenizer, TK_INC)) {
      int val;
      if (is_ptr_type(expr->val_type)) {
        val = get_val_size(expr->val_type->ptrof);
      } else {
        val = 1;
      }
      expr = new_expr_postfix(EX_INC, expr);
      expr->val = val;
    } else if (token_consume(sctxt->tokenizer, TK_DEC)) {
      int val;
      if (is_ptr_type(expr->val_type)) {
        val = get_val_size(expr->val_type->ptrof);
      } else {
        val = 1;
      }
      expr = new_expr_postfix(EX_DEC, expr);
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
  if (token_consume(sctxt->tokenizer, '&')) {
    return new_expr_unary('&', cast_expression(sctxt));
  }
  if (token_consume(sctxt->tokenizer, '*')) {
    return new_expr_unary('*', cast_expression(sctxt));
  }
  if (token_consume(sctxt->tokenizer, '+')) {
    return new_expr_unary('+', cast_expression(sctxt));
  }
  if (token_consume(sctxt->tokenizer, '-')) {
    return new_expr_unary('-', cast_expression(sctxt));
  }
  if (token_consume(sctxt->tokenizer, '~')) {
    return new_expr_unary('~', cast_expression(sctxt));
  }
  if (token_consume(sctxt->tokenizer, '!')) {
    return new_expr_unary('!', cast_expression(sctxt));
  }
  if (token_consume(sctxt->tokenizer, TK_INC)) {
    Expr *expr = cast_expression(sctxt);
    int val;
    if (is_ptr_type(expr->val_type)) {
      val = get_val_size(expr->val_type->ptrof);
    } else {
      val = 1;
    }
    expr = new_expr_unary(EX_INC, expr);
    expr->val = val;
    return expr;
  }
  if (token_consume(sctxt->tokenizer, TK_DEC)) {
    Expr *expr = cast_expression(sctxt);
    int val;
    if (is_ptr_type(expr->val_type)) {
      val = get_val_size(expr->val_type->ptrof);
    } else {
      val = 1;
    }
    expr = new_expr_unary(EX_DEC, expr);
    expr->val = val;
    return expr;
  }
  return postfix_expression(sctxt);
}

static Expr *cast_expression(ScopeCtxt *sctxt) {
  if (token_peek(sctxt->tokenizer)->ty == '(' &&
      token_is_typename(token_peek_ahead(sctxt->tokenizer, 1))) {
    token_succ(sctxt->tokenizer);
    Type *val_type = type_name(sctxt->tokenizer);
    token_expect(sctxt->tokenizer, ')');
    return new_expr_cast(val_type, cast_expression(sctxt));
  }
  return unary_expression(sctxt);
}

static Expr *multiplicative_expression(ScopeCtxt *sctxt) {
  Expr *expr = cast_expression(sctxt);
  while (true) {
    if (token_consume(sctxt->tokenizer, '*')) {
      expr = new_expr_binop('*', expr, cast_expression(sctxt));
    } else if (token_consume(sctxt->tokenizer, '/')) {
      expr = new_expr_binop('/', expr, cast_expression(sctxt));
    } else if (token_consume(sctxt->tokenizer, '%')) {
      expr = new_expr_binop('%', expr, cast_expression(sctxt));
    } else {
      return expr;
    }
  }
}

static Expr *additive_expression(ScopeCtxt *sctxt) {
  Expr *expr = multiplicative_expression(sctxt);
  while (true) {
    if (token_consume(sctxt->tokenizer, '+')) {
      expr = new_expr_binop('+', expr, multiplicative_expression(sctxt));
    } else if (token_consume(sctxt->tokenizer, '-')) {
      expr = new_expr_binop('-', expr, multiplicative_expression(sctxt));
    } else {
      return expr;
    }
  }
}

static Expr *shift_expression(ScopeCtxt *sctxt) {
  Expr *expr = additive_expression(sctxt);
  while (true) {
    if (token_consume(sctxt->tokenizer, TK_LSHIFT)) {
      expr = new_expr_binop(EX_LSHIFT, expr, additive_expression(sctxt));
    } else if (token_consume(sctxt->tokenizer, TK_RSHIFT)) {
      expr = new_expr_binop(EX_RSHIFT, expr, additive_expression(sctxt));
    } else {
      return expr;
    }
  }
  return expr;
}

static Expr *relational_expression(ScopeCtxt *sctxt) {
  Expr *expr = shift_expression(sctxt);
  while (true) {
    if (token_consume(sctxt->tokenizer, '<')) {
      expr = new_expr_binop('<', expr, shift_expression(sctxt));
    } else if (token_consume(sctxt->tokenizer, '>')) {
      expr = new_expr_binop('>', expr, shift_expression(sctxt));
    } else if (token_consume(sctxt->tokenizer, TK_LTEQ)) {
      expr = new_expr_binop(EX_LTEQ, expr, shift_expression(sctxt));
    } else if (token_consume(sctxt->tokenizer, TK_GTEQ)) {
      expr = new_expr_binop(EX_GTEQ, expr, shift_expression(sctxt));
    } else {
      return expr;
    }
  }
  return expr;
}

static Expr *equality_expression(ScopeCtxt *sctxt) {
  Expr *expr = relational_expression(sctxt);
  while (true) {
    if (token_consume(sctxt->tokenizer, TK_EQEQ)) {
      expr = new_expr_binop(EX_EQEQ, expr, relational_expression(sctxt));
    } else if (token_consume(sctxt->tokenizer, TK_NOTEQ)) {
      expr = new_expr_binop(EX_NOTEQ, expr, relational_expression(sctxt));
    } else {
      return expr;
    }
  }
}

static Expr *and_expression(ScopeCtxt *sctxt) {
  Expr *expr = equality_expression(sctxt);
  while (true) {
    if (token_consume(sctxt->tokenizer, '&')) {
      expr = new_expr_binop('&', expr, equality_expression(sctxt));
    } else {
      return expr;
    }
  }
  return expr;
}

static Expr *exclusive_or_expression(ScopeCtxt *sctxt) {
  Expr *expr = and_expression(sctxt);
  while (true) {
    if (token_consume(sctxt->tokenizer, '^')) {
      expr = new_expr_binop('^', expr, and_expression(sctxt));
    } else {
      return expr;
    }
  }
  return expr;
}
static Expr *inclusive_or_expression(ScopeCtxt *sctxt) {
  Expr *expr = exclusive_or_expression(sctxt);
  while (true) {
    if (token_consume(sctxt->tokenizer, '|')) {
      expr = new_expr_binop('|', expr, exclusive_or_expression(sctxt));
    } else {
      return expr;
    }
  }
  return expr;
}
static Expr *logical_and_expression(ScopeCtxt *sctxt) {
  Expr *expr = inclusive_or_expression(sctxt);
  while (true) {
    if (token_consume(sctxt->tokenizer, TK_LOGAND)) {
      expr = new_expr_binop(EX_LOGAND, expr, inclusive_or_expression(sctxt));
    } else {
      return expr;
    }
  }
  return expr;
}
static Expr *logical_or_expression(ScopeCtxt *sctxt) {
  Expr *expr = logical_and_expression(sctxt);
  while (true) {
    if (token_consume(sctxt->tokenizer, TK_LOGOR)) {
      expr = new_expr_binop(EX_LOGOR, expr, logical_and_expression(sctxt));
    } else {
      return expr;
    }
  }
  return expr;
}
static Expr *conditional_expression(ScopeCtxt *sctxt) {
  Expr *cond = logical_or_expression(sctxt);
  if (token_consume(sctxt->tokenizer, '?')) {
    Expr *then_expr = expression(sctxt);
    token_expect(sctxt->tokenizer, ':');
    Expr *else_expr = conditional_expression(sctxt);
    return new_expr_cond(cond, then_expr, else_expr);
  }
  return cond;
}

static Expr *assignment_expression(ScopeCtxt *sctxt) {
  Expr *lhs = conditional_expression(sctxt);
  if (token_consume(sctxt->tokenizer, '=')) {
    return new_expr_binop('=', lhs, assignment_expression(sctxt));
  }
  if (token_consume(sctxt->tokenizer, TK_MUL_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('*', lhs, assignment_expression(sctxt)));
  }
  if (token_consume(sctxt->tokenizer, TK_DIV_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('/', lhs, assignment_expression(sctxt)));
  }
  if (token_consume(sctxt->tokenizer, TK_MOD_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('%', lhs, assignment_expression(sctxt)));
  }
  if (token_consume(sctxt->tokenizer, TK_ADD_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('+', lhs, assignment_expression(sctxt)));
  }
  if (token_consume(sctxt->tokenizer, TK_SUB_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('-', lhs, assignment_expression(sctxt)));
  }
  if (token_consume(sctxt->tokenizer, TK_LSHIFT_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop(EX_LSHIFT, lhs, assignment_expression(sctxt)));
  }
  if (token_consume(sctxt->tokenizer, TK_RSHIFT_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop(EX_RSHIFT, lhs, assignment_expression(sctxt)));
  }
  if (token_consume(sctxt->tokenizer, TK_AND_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('&', lhs, assignment_expression(sctxt)));
  }
  if (token_consume(sctxt->tokenizer, TK_OR_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('|', lhs, assignment_expression(sctxt)));
  }
  if (token_consume(sctxt->tokenizer, TK_XOR_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('^', lhs, assignment_expression(sctxt)));
  }
  return lhs;
}

static Expr *expression(ScopeCtxt *sctxt) {
  Expr *expr = assignment_expression(sctxt);
  while (true) {
    if (token_consume(sctxt->tokenizer, ',')) {
      expr = new_expr_binop(',', expr, assignment_expression(sctxt));
    } else {
      return expr;
    }
  }
  return expr;
}

static Expr *constant_expression(ScopeCtxt *sctxt) {
  return conditional_expression(sctxt);
}

static void declaration(ScopeCtxt *sctxt) {
  Type *base_type = type_specifier(sctxt->tokenizer);
  char *name;
  Type *type;
  declarator(sctxt->tokenizer, base_type, &name, &type);
  token_expect(sctxt->tokenizer, ';');
  (void)register_stack(sctxt, name, type);
}

static Type *type_specifier(Tokenizer *tokenizer) {
  Token *token = token_pop(tokenizer);
  switch (token->ty) {
  case TK_CHAR:
    return new_type(TY_CHAR);
  case TK_INT:
    return new_type(TY_INT);
  case TK_VOID:
    return new_type(TY_VOID);
  default:
    error("型名がありません: %s", token->input);
  }
}

static Type *type_name(Tokenizer *tokenizer) {
  Type *type = type_specifier(tokenizer);
  while (token_consume(tokenizer, '*')) {
    type = new_type_ptr(type);
  }
  while (token_consume(tokenizer, '[')) {
    type = new_type_array(type, token_expect(tokenizer, TK_NUM)->val);
    token_expect(tokenizer, ']');
  }
  return type;
}

static void declarator(Tokenizer *tokenizer, Type *base_type, char **name,
                       Type **type) {
  while (token_consume(tokenizer, '*')) {
    base_type = new_type_ptr(base_type);
  }
  direct_declarator(tokenizer, base_type, name, type);
}

static void direct_declarator(Tokenizer *tokenizer, Type *base_type,
                              char **name, Type **type) {
  Type *placeholder = malloc(sizeof(Type));
  Token *token;
  if ((token = token_consume(tokenizer, TK_IDENT)) != NULL) {
    *name = token->name;
    *type = placeholder;
  } else if (token_consume(tokenizer, '(')) {
    declarator(tokenizer, placeholder, name, type);
    token_expect(tokenizer, ')');
  } else {
    error("識別子でも括弧でもありません: %s", token_peek(tokenizer)->input);
  }

  while (true) {
    if (token_consume(tokenizer, '[')) {
      Type *inner = malloc(sizeof(Type));
      *placeholder =
          *new_type_array(inner, token_expect(tokenizer, TK_NUM)->val);
      placeholder = inner;
      token_expect(tokenizer, ']');
      continue;
    }

    if (token_consume(tokenizer, '(')) {
      Vector *params = new_vector();
      if (token_consume(tokenizer, ')') ||
          token_consume2(tokenizer, TK_VOID, ')')) {
        // do nothing
      } else {
        while (true) {
          Type *base_type = type_specifier(tokenizer);
          Param *param = malloc(sizeof(Param));
          declarator(tokenizer, base_type, &param->name, &param->type);
          vec_push(params, param);
          if (token_peek(tokenizer)->ty == ')') {
            break;
          }
          token_expect(tokenizer, ',');
        }
        token_expect(tokenizer, ')');
      }

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
  switch (token_peek(sctxt->tokenizer)->ty) {
  case TK_IF: {
    token_succ(sctxt->tokenizer);
    token_expect(sctxt->tokenizer, '(');
    Expr *cond = expression(sctxt);
    token_expect(sctxt->tokenizer, ')');
    Stmt *then_stmt = statement(sctxt);
    Stmt *else_stmt = &null_stmt;
    if (token_consume(sctxt->tokenizer, TK_ELSE)) {
      else_stmt = statement(sctxt);
    }
    return new_stmt_if(cond, then_stmt, else_stmt);
  }
  case TK_SWITCH: {
    token_succ(sctxt->tokenizer);
    token_expect(sctxt->tokenizer, '(');
    Expr *cond = expression(sctxt);
    token_expect(sctxt->tokenizer, ')');
    Stmt *stmt = new_stmt_switch(cond, NULL);
    vec_push(sctxt->func->switches, stmt);
    stmt->body = statement(sctxt);
    vec_pop(sctxt->func->switches);
    return stmt;
  }
  case TK_CASE: {
    token_succ(sctxt->tokenizer);
    Expr *expr = constant_expression(sctxt);
    token_expect(sctxt->tokenizer, ':');
    Stmt *stmt = new_stmt_case(expr);
    if (sctxt->func->switches->len <= 0) {
      error("switch文中でない箇所にcase文があります");
    }
    Stmt *switch_stmt =
        sctxt->func->switches->data[sctxt->func->switches->len - 1];
    vec_push(switch_stmt->cases, stmt);
    return stmt;
  }
  case TK_DEFAULT: {
    token_succ(sctxt->tokenizer);
    token_expect(sctxt->tokenizer, ':');
    Stmt *stmt = new_stmt_default();
    if (sctxt->func->switches->len <= 0) {
      error("switch文中でない箇所にcase文があります");
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
    return new_stmt_while(cond, body);
  }
  case TK_DO: {
    token_succ(sctxt->tokenizer);
    Stmt *body = statement(sctxt);
    token_expect(sctxt->tokenizer, TK_WHILE);
    token_expect(sctxt->tokenizer, '(');
    Expr *cond = expression(sctxt);
    token_expect(sctxt->tokenizer, ')');
    token_expect(sctxt->tokenizer, ';');
    return new_stmt_do_while(cond, body);
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
    return new_stmt_for(init, cond, inc, body);
  }
  case TK_GOTO: {
    token_succ(sctxt->tokenizer);
    char *name = token_expect(sctxt->tokenizer, TK_IDENT)->name;
    token_expect(sctxt->tokenizer, ';');
    return new_stmt_goto(name);
  }
  case TK_BREAK: {
    token_succ(sctxt->tokenizer);
    token_expect(sctxt->tokenizer, ';');
    return new_stmt(ST_BREAK);
  }
  case TK_CONTINUE: {
    token_succ(sctxt->tokenizer);
    token_expect(sctxt->tokenizer, ';');
    return new_stmt(ST_CONTINUE);
  }
  case TK_RETURN: {
    token_succ(sctxt->tokenizer);
    Expr *expr = NULL;
    if (token_peek(sctxt->tokenizer)->ty != ';') {
      expr = expression(sctxt);
    }
    token_expect(sctxt->tokenizer, ';');
    return new_stmt_return(expr);
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
      Stmt *stmt =
          new_stmt_label(sctxt->func, token_pop(sctxt->tokenizer)->name);
      token_succ(sctxt->tokenizer);
      return stmt;
    }
  }
  // fall through
  default: {
    Expr *expr = expression(sctxt);
    token_expect(sctxt->tokenizer, ';');
    return new_stmt_expr(expr);
  }
  }
}

static Stmt *compound_statement(ScopeCtxt *sctxt) {
  token_expect(sctxt->tokenizer, '{');

  Stmt *stmt = new_stmt(ST_COMPOUND);
  stmt->stmts = new_vector();
  while (!token_consume(sctxt->tokenizer, '}')) {
    if (token_is_typename(token_peek(sctxt->tokenizer))) {
      declaration(sctxt);
      continue;
    }
    vec_push(stmt->stmts, statement(sctxt));
  }
  return stmt;
}

static Function *function_definition(GlobalCtxt *gctxt, Type *type,
                                     char *name) {
  FuncCtxt *fctxt = new_func_ctxt(gctxt);
  ScopeCtxt *sctxt = new_root_scope_ctxt(fctxt);
  if (!register_global(gctxt, name, type)) {
    error("同じ名前の関数またはグローバル変数が複数回定義されました: %s", name);
  }
  for (int i = 0; i < type->func_param->len; i++) {
    Param *param = type->func_param->data[i];
    param->stack_var = register_stack(sctxt, param->name, param->type);
  }

  Stmt *body = compound_statement(sctxt);

  Function *func = malloc(sizeof(Function));
  func->name = name;
  func->type = type;
  func->stack_size = fctxt->stack_size;
  func->label_map = fctxt->label_map;
  func->body = body;

  return func;
}

static GlobalVar *global_variable(GlobalCtxt *gctxt, Type *type, char *name) {
  token_expect(gctxt->tokenizer, ';');

  if (!register_global(gctxt, name, type)) {
    error("同じ名前の関数またはグローバル変数が複数回定義されました: %s", name);
  }

  return get_global(gctxt, name);
}

static TranslationUnit *translation_unit(Tokenizer *tokenizer) {
  GlobalCtxt *gctxt = new_global_ctxt(tokenizer);

  Vector *func_list = new_vector();
  Vector *gvar_list = new_vector();

  while (token_peek(tokenizer)->ty != TK_EOF) {
    Type *base_type = type_specifier(gctxt->tokenizer);
    char *name;
    Type *type;
    declarator(gctxt->tokenizer, base_type, &name, &type);

    if (is_func_type(type)) {
      vec_push(func_list, function_definition(gctxt, type, name));
      continue;
    }

    vec_push(gvar_list, global_variable(gctxt, type, name));
  }

  TranslationUnit *tunit = malloc(sizeof(TranslationUnit));
  tunit->func_list = func_list;
  tunit->gvar_list = gvar_list;
  tunit->str_list = gctxt->str_list;
  return tunit;
}
