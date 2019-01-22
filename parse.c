#include "gifcc.h"
#include <assert.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// 関数パース用情報
typedef struct FuncCtxt {
  Tokenizer *tokenizer;
  int stack_size;
  Map *stack_map;
  Vector *switches;
  Map *label_map;
} FuncCtxt;

static FuncCtxt *new_func_ctxt(Tokenizer *tokenizer);
static bool register_stack(FuncCtxt *fctxt, char *name, Type *type);
static StackVar *get_stack(FuncCtxt *fctxt, char *name);
static bool is_sametype(Type *ty1, Type *ty2);
static bool is_integer_type(Type *ty);
static bool is_arith_type(Type *ty);
static bool is_ptr_type(Type *ty);
static bool is_array_type(Type *ty);
static Type *integer_promoted(Type *ty);
static Type *arith_converted(Type *ty1, Type *ty2);
static bool token_is_typename(Token *token);
static void __attribute__((noreturn))
binop_type_error(int ty, Expr *lhs, Expr *rhs);
static Type *new_type(int ty);
static Type *new_type_ptr(Type *base_type);
static Type *new_type_array(Type *base_type, int len);
static Expr *coerce_array2ptr(Expr *expr);
static Expr *new_expr(int ty, Type *val_type);
static Expr *new_expr_num(int val);
static Expr *new_expr_ident(FuncCtxt *fctxt, char *name);
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
static Expr *primary_expression(FuncCtxt *fctxt);
static Expr *postfix_expression(FuncCtxt *fctxt);
static Vector *argument_expression_list(FuncCtxt *fctxt);
static Expr *unary_expression(FuncCtxt *fctxt);
static Expr *cast_expression(FuncCtxt *fctxt);
static Expr *multiplicative_expression(FuncCtxt *fctxt);
static Expr *additive_expression(FuncCtxt *fctxt);
static Expr *shift_expression(FuncCtxt *fctxt);
static Expr *relational_expression(FuncCtxt *fctxt);
static Expr *equality_expression(FuncCtxt *fctxt);
static Expr *and_expression(FuncCtxt *fctxt);
static Expr *exclusive_or_expression(FuncCtxt *fctxt);
static Expr *inclusive_or_expression(FuncCtxt *fctxt);
static Expr *logical_and_expression(FuncCtxt *fctxt);
static Expr *logical_or_expression(FuncCtxt *fctxt);
static Expr *conditional_expression(FuncCtxt *fctxt);
static Expr *assignment_expression(FuncCtxt *fctxt);
static Expr *expression(FuncCtxt *fctxt);
static Expr *constant_expression(FuncCtxt *fctxt);

// declaration
static void declaration(FuncCtxt *fctxt);
static Type *type_specifier(FuncCtxt *fctxt);
static Type *type_name(FuncCtxt *fctxt);
static void declarator(FuncCtxt *fctxt, Type *base_type, char **name,
                       Type **type);

// statement
static Stmt *statement(FuncCtxt *fctxt);
static Stmt *compound_statement(FuncCtxt *fctxt);

// top-level
static Function *function_declaration(Tokenizer *tokenizer);
static Vector *translation_unit(Tokenizer *tokenizer);

static Stmt null_stmt = {
    .ty = ST_NULL,
};

Vector *parse(const char *input) {
  Tokenizer *tokenizer = new_tokenizer(input);
  return translation_unit(tokenizer);
}

static FuncCtxt *new_func_ctxt(Tokenizer *tokenizer) {
  FuncCtxt *fctxt = malloc(sizeof(FuncCtxt));

  fctxt->tokenizer = tokenizer;
  fctxt->stack_size = 0;
  fctxt->stack_map = new_map();
  fctxt->switches = new_vector();

  fctxt->label_map = new_map();

  return fctxt;
}

static bool register_stack(FuncCtxt *fctxt, char *name, Type *type) {
  if (map_get(fctxt->stack_map, name)) {
    return false;
  }

  StackVar *var = malloc(sizeof(StackVar));
  fctxt->stack_size = align(fctxt->stack_size, get_val_align(type));
  var->offset = fctxt->stack_size;
  var->type = type;
  map_put(fctxt->stack_map, name, var);
  fctxt->stack_size += get_val_size(type);

  return true;
}

static StackVar *get_stack(FuncCtxt *fctxt, char *name) {
  return map_get(fctxt->stack_map, name);
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

static bool is_integer_type(Type *ty) { return ty->ty == TY_INT; }
static bool is_arith_type(Type *ty) { return is_integer_type(ty); }
static bool is_ptr_type(Type *ty) { return ty->ty == TY_PTR; }
static bool is_array_type(Type *ty) { return ty->ty == TY_ARRAY; }
static Type *integer_promoted(Type *ty) { return ty; }
static Type *arith_converted(Type *ty1, Type *ty2) {
  if (!is_arith_type(ty1) || !is_arith_type(ty2)) {
    return NULL;
  }
  return ty1;
}

static bool token_is_typename(Token *token) {
  if (token->ty == TK_INT) {
    return true;
  }
  return false;
}

int get_val_size(Type *ty) {
  switch (ty->ty) {
  case TY_INT:
    return sizeof(int);
  case TY_PTR:
    return sizeof(void *);
  case TY_ARRAY:
    return get_val_size(ty->ptrof) * ty->array_len;
  default:
    assert(false);
    break;
  }
}

int get_val_align(Type *ty) {
  switch (ty->ty) {
  case TY_INT:
    return alignof(int);
  case TY_PTR:
    return alignof(void *);
  case TY_ARRAY:
    return get_val_align(ty->ptrof);
  default:
    assert(false);
    break;
  }
}

static void __attribute__((noreturn))
binop_type_error(int ty, Expr *lhs, Expr *rhs) {
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
  base_type = ptrtype;
  return ptrtype;
}

static Type *new_type_array(Type *base_type, int len) {
  Type *ptrtype = malloc(sizeof(Type));
  ptrtype->ty = TY_ARRAY;
  ptrtype->ptrof = base_type;
  ptrtype->array_len = len;
  base_type = ptrtype;
  return ptrtype;
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

static Expr *new_expr_ident(FuncCtxt *fctxt, char *name) {
  StackVar *var = get_stack(fctxt, name);
  // 未知の識別子はint型として扱う
  Type *type = var != NULL ? var->type : new_type(TY_INT);
  Expr *expr = new_expr(EX_IDENT, type);
  expr->name = name;
  return expr;
}

static Expr *new_expr_call(Expr *callee, Vector *argument) {
  callee = coerce_array2ptr(callee);

  // TODO: 関数の戻り値はintを仮定する
  Expr *expr = new_expr(EX_CALL, new_type(TY_INT));
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
    val_type = arith_converted(lhs->val_type, rhs->val_type);
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

    val_type = arith_converted(lhs->val_type, rhs->val_type);
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

    val_type = arith_converted(lhs->val_type, rhs->val_type);
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
    val_type = integer_promoted(lhs->val_type);
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
    val_type = arith_converted(lhs->val_type, rhs->val_type);
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

static Expr *primary_expression(FuncCtxt *fctxt) {
  if (token_peek(fctxt->tokenizer)->ty == TK_NUM) {
    return new_expr_num(token_pop(fctxt->tokenizer)->val);
  }
  if (token_peek(fctxt->tokenizer)->ty == TK_IDENT) {
    return new_expr_ident(fctxt, token_pop(fctxt->tokenizer)->name);
  }
  if (token_consume(fctxt->tokenizer, '(')) {
    Expr *expr = expression(fctxt);
    token_expect(fctxt->tokenizer, ')');
    return expr;
  }
  error("数値でも開きカッコでもないトークンです: %s",
        token_peek(fctxt->tokenizer)->input);
}

static Expr *postfix_expression(FuncCtxt *fctxt) {
  Expr *expr = primary_expression(fctxt);
  while (true) {
    if (token_consume(fctxt->tokenizer, '[')) {
      Expr *operand = expression(fctxt);
      token_expect(fctxt->tokenizer, ']');
      expr = new_expr_index(expr, operand);
    } else if (token_consume(fctxt->tokenizer, '(')) {
      Vector *argument = NULL;
      if (token_peek(fctxt->tokenizer)->ty != ')') {
        argument = argument_expression_list(fctxt);
      }
      token_expect(fctxt->tokenizer, ')');
      expr = new_expr_call(expr, argument);
    } else if (token_consume(fctxt->tokenizer, TK_INC)) {
      int val;
      if (is_ptr_type(expr->val_type)) {
        val = get_val_size(expr->val_type->ptrof);
      } else {
        val = 1;
      }
      expr = new_expr_postfix(EX_INC, expr);
      expr->val = val;
    } else if (token_consume(fctxt->tokenizer, TK_DEC)) {
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

static Vector *argument_expression_list(FuncCtxt *fctxt) {
  Vector *argument = new_vector();
  while (true) {
    vec_push(argument, assignment_expression(fctxt));
    if (!token_consume(fctxt->tokenizer, ',')) {
      break;
    }
  }
  return argument;
}

static Expr *unary_expression(FuncCtxt *fctxt) {
  if (token_consume(fctxt->tokenizer, '&')) {
    return new_expr_unary('&', cast_expression(fctxt));
  }
  if (token_consume(fctxt->tokenizer, '*')) {
    return new_expr_unary('*', cast_expression(fctxt));
  }
  if (token_consume(fctxt->tokenizer, '+')) {
    return new_expr_unary('+', cast_expression(fctxt));
  }
  if (token_consume(fctxt->tokenizer, '-')) {
    return new_expr_unary('-', cast_expression(fctxt));
  }
  if (token_consume(fctxt->tokenizer, '~')) {
    return new_expr_unary('~', cast_expression(fctxt));
  }
  if (token_consume(fctxt->tokenizer, '!')) {
    return new_expr_unary('!', cast_expression(fctxt));
  }
  if (token_consume(fctxt->tokenizer, TK_INC)) {
    Expr *expr = cast_expression(fctxt);
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
  if (token_consume(fctxt->tokenizer, TK_DEC)) {
    Expr *expr = cast_expression(fctxt);
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
  return postfix_expression(fctxt);
}

static Expr *cast_expression(FuncCtxt *fctxt) {
  if (token_peek(fctxt->tokenizer)->ty == '(' &&
      token_is_typename(token_peek_ahead(fctxt->tokenizer, 1))) {
    token_succ(fctxt->tokenizer);
    Type *val_type = type_name(fctxt);
    token_expect(fctxt->tokenizer, ')');
    return new_expr_cast(val_type, cast_expression(fctxt));
  }
  return unary_expression(fctxt);
}

static Expr *multiplicative_expression(FuncCtxt *fctxt) {
  Expr *expr = cast_expression(fctxt);
  while (true) {
    if (token_consume(fctxt->tokenizer, '*')) {
      expr = new_expr_binop('*', expr, cast_expression(fctxt));
    } else if (token_consume(fctxt->tokenizer, '/')) {
      expr = new_expr_binop('/', expr, cast_expression(fctxt));
    } else if (token_consume(fctxt->tokenizer, '%')) {
      expr = new_expr_binop('%', expr, cast_expression(fctxt));
    } else {
      return expr;
    }
  }
}

static Expr *additive_expression(FuncCtxt *fctxt) {
  Expr *expr = multiplicative_expression(fctxt);
  while (true) {
    if (token_consume(fctxt->tokenizer, '+')) {
      expr = new_expr_binop('+', expr, multiplicative_expression(fctxt));
    } else if (token_consume(fctxt->tokenizer, '-')) {
      expr = new_expr_binop('-', expr, multiplicative_expression(fctxt));
    } else {
      return expr;
    }
  }
}

static Expr *shift_expression(FuncCtxt *fctxt) {
  Expr *expr = additive_expression(fctxt);
  while (true) {
    if (token_consume(fctxt->tokenizer, TK_LSHIFT)) {
      expr = new_expr_binop(EX_LSHIFT, expr, additive_expression(fctxt));
    } else if (token_consume(fctxt->tokenizer, TK_RSHIFT)) {
      expr = new_expr_binop(EX_RSHIFT, expr, additive_expression(fctxt));
    } else {
      return expr;
    }
  }
  return expr;
}

static Expr *relational_expression(FuncCtxt *fctxt) {
  Expr *expr = shift_expression(fctxt);
  while (true) {
    if (token_consume(fctxt->tokenizer, '<')) {
      expr = new_expr_binop('<', expr, shift_expression(fctxt));
    } else if (token_consume(fctxt->tokenizer, '>')) {
      expr = new_expr_binop('>', expr, shift_expression(fctxt));
    } else if (token_consume(fctxt->tokenizer, TK_LTEQ)) {
      expr = new_expr_binop(EX_LTEQ, expr, shift_expression(fctxt));
    } else if (token_consume(fctxt->tokenizer, TK_GTEQ)) {
      expr = new_expr_binop(EX_GTEQ, expr, shift_expression(fctxt));
    } else {
      return expr;
    }
  }
  return expr;
}

static Expr *equality_expression(FuncCtxt *fctxt) {
  Expr *expr = relational_expression(fctxt);
  while (true) {
    if (token_consume(fctxt->tokenizer, TK_EQEQ)) {
      expr = new_expr_binop(EX_EQEQ, expr, relational_expression(fctxt));
    } else if (token_consume(fctxt->tokenizer, TK_NOTEQ)) {
      expr = new_expr_binop(EX_NOTEQ, expr, relational_expression(fctxt));
    } else {
      return expr;
    }
  }
}

static Expr *and_expression(FuncCtxt *fctxt) {
  Expr *expr = equality_expression(fctxt);
  while (true) {
    if (token_consume(fctxt->tokenizer, '&')) {
      expr = new_expr_binop('&', expr, equality_expression(fctxt));
    } else {
      return expr;
    }
  }
  return expr;
}

static Expr *exclusive_or_expression(FuncCtxt *fctxt) {
  Expr *expr = and_expression(fctxt);
  while (true) {
    if (token_consume(fctxt->tokenizer, '^')) {
      expr = new_expr_binop('^', expr, and_expression(fctxt));
    } else {
      return expr;
    }
  }
  return expr;
}
static Expr *inclusive_or_expression(FuncCtxt *fctxt) {
  Expr *expr = exclusive_or_expression(fctxt);
  while (true) {
    if (token_consume(fctxt->tokenizer, '|')) {
      expr = new_expr_binop('|', expr, exclusive_or_expression(fctxt));
    } else {
      return expr;
    }
  }
  return expr;
}
static Expr *logical_and_expression(FuncCtxt *fctxt) {
  Expr *expr = inclusive_or_expression(fctxt);
  while (true) {
    if (token_consume(fctxt->tokenizer, TK_LOGAND)) {
      expr = new_expr_binop(EX_LOGAND, expr, inclusive_or_expression(fctxt));
    } else {
      return expr;
    }
  }
  return expr;
}
static Expr *logical_or_expression(FuncCtxt *fctxt) {
  Expr *expr = logical_and_expression(fctxt);
  while (true) {
    if (token_consume(fctxt->tokenizer, TK_LOGOR)) {
      expr = new_expr_binop(EX_LOGOR, expr, logical_and_expression(fctxt));
    } else {
      return expr;
    }
  }
  return expr;
}
static Expr *conditional_expression(FuncCtxt *fctxt) {
  Expr *cond = logical_or_expression(fctxt);
  if (token_consume(fctxt->tokenizer, '?')) {
    Expr *then_expr = expression(fctxt);
    token_expect(fctxt->tokenizer, ':');
    Expr *else_expr = conditional_expression(fctxt);
    return new_expr_cond(cond, then_expr, else_expr);
  }
  return cond;
}

static Expr *assignment_expression(FuncCtxt *fctxt) {
  Expr *lhs = conditional_expression(fctxt);
  if (token_consume(fctxt->tokenizer, '=')) {
    return new_expr_binop('=', lhs, assignment_expression(fctxt));
  }
  if (token_consume(fctxt->tokenizer, TK_MUL_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('*', lhs, assignment_expression(fctxt)));
  }
  if (token_consume(fctxt->tokenizer, TK_DIV_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('/', lhs, assignment_expression(fctxt)));
  }
  if (token_consume(fctxt->tokenizer, TK_MOD_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('%', lhs, assignment_expression(fctxt)));
  }
  if (token_consume(fctxt->tokenizer, TK_ADD_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('+', lhs, assignment_expression(fctxt)));
  }
  if (token_consume(fctxt->tokenizer, TK_SUB_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('-', lhs, assignment_expression(fctxt)));
  }
  if (token_consume(fctxt->tokenizer, TK_LSHIFT_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop(EX_LSHIFT, lhs, assignment_expression(fctxt)));
  }
  if (token_consume(fctxt->tokenizer, TK_RSHIFT_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop(EX_RSHIFT, lhs, assignment_expression(fctxt)));
  }
  if (token_consume(fctxt->tokenizer, TK_AND_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('&', lhs, assignment_expression(fctxt)));
  }
  if (token_consume(fctxt->tokenizer, TK_OR_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('|', lhs, assignment_expression(fctxt)));
  }
  if (token_consume(fctxt->tokenizer, TK_XOR_ASSIGN)) {
    return new_expr_binop(
        '=', lhs, new_expr_binop('^', lhs, assignment_expression(fctxt)));
  }
  return lhs;
}

static Expr *expression(FuncCtxt *fctxt) {
  Expr *expr = assignment_expression(fctxt);
  while (true) {
    if (token_consume(fctxt->tokenizer, ',')) {
      expr = new_expr_binop(',', expr, assignment_expression(fctxt));
    } else {
      return expr;
    }
  }
  return expr;
}

static Expr *constant_expression(FuncCtxt *fctxt) {
  return conditional_expression(fctxt);
}

static void declaration(FuncCtxt *fctxt) {
  Type *base_type = type_specifier(fctxt);
  char *name;
  Type *type;
  declarator(fctxt, base_type, &name, &type);
  token_expect(fctxt->tokenizer, ';');

  if (!register_stack(fctxt, name, type)) {
    error("同じ名前の変数が複数回宣言されました: %s", name);
  }
}

static Type *type_specifier(FuncCtxt *fctxt) {
  token_expect(fctxt->tokenizer, TK_INT);
  return new_type(TY_INT);
}

static Type *type_name(FuncCtxt *fctxt) {
  Type *type = type_specifier(fctxt);
  while (token_consume(fctxt->tokenizer, '*')) {
    type = new_type_ptr(type);
  }
  if (token_consume(fctxt->tokenizer, '[')) {
    type = new_type_array(type, token_expect(fctxt->tokenizer, TK_INT)->val);
    token_expect(fctxt->tokenizer, ']');
  }
  return type;
}

static void declarator(FuncCtxt *fctxt, Type *base_type, char **name,
                       Type **type) {
  while (token_consume(fctxt->tokenizer, '*')) {
    base_type = new_type_ptr(base_type);
  }
  Token *ident = token_expect(fctxt->tokenizer, TK_IDENT);
  if (token_consume(fctxt->tokenizer, '[')) {
    base_type =
        new_type_array(base_type, token_expect(fctxt->tokenizer, TK_NUM)->val);
    token_expect(fctxt->tokenizer, ']');
  }
  *name = ident->name;
  *type = base_type;
}

static Stmt *statement(FuncCtxt *fctxt) {
  switch (token_peek(fctxt->tokenizer)->ty) {
  case TK_IF: {
    token_succ(fctxt->tokenizer);
    token_expect(fctxt->tokenizer, '(');
    Expr *cond = expression(fctxt);
    token_expect(fctxt->tokenizer, ')');
    Stmt *then_stmt = statement(fctxt);
    Stmt *else_stmt = &null_stmt;
    if (token_consume(fctxt->tokenizer, TK_ELSE)) {
      else_stmt = statement(fctxt);
    }
    return new_stmt_if(cond, then_stmt, else_stmt);
  }
  case TK_SWITCH: {
    token_succ(fctxt->tokenizer);
    token_expect(fctxt->tokenizer, '(');
    Expr *cond = expression(fctxt);
    token_expect(fctxt->tokenizer, ')');
    Stmt *stmt = new_stmt_switch(cond, NULL);
    vec_push(fctxt->switches, stmt);
    stmt->body = statement(fctxt);
    vec_pop(fctxt->switches);
    return stmt;
  }
  case TK_CASE: {
    token_succ(fctxt->tokenizer);
    Expr *expr = constant_expression(fctxt);
    token_expect(fctxt->tokenizer, ':');
    Stmt *stmt = new_stmt_case(expr);
    if (fctxt->switches->len <= 0) {
      error("switch文中でない箇所にcase文があります");
    }
    Stmt *switch_stmt = fctxt->switches->data[fctxt->switches->len - 1];
    vec_push(switch_stmt->cases, stmt);
    return stmt;
  }
  case TK_DEFAULT: {
    token_succ(fctxt->tokenizer);
    token_expect(fctxt->tokenizer, ':');
    Stmt *stmt = new_stmt_default();
    if (fctxt->switches->len <= 0) {
      error("switch文中でない箇所にcase文があります");
    }
    Stmt *switch_expr = fctxt->switches->data[fctxt->switches->len - 1];
    switch_expr->default_case = stmt;
    return stmt;
  }
  case TK_WHILE: {
    token_succ(fctxt->tokenizer);
    token_expect(fctxt->tokenizer, '(');
    Expr *cond = expression(fctxt);
    token_expect(fctxt->tokenizer, ')');
    Stmt *body = statement(fctxt);
    return new_stmt_while(cond, body);
  }
  case TK_DO: {
    token_succ(fctxt->tokenizer);
    Stmt *body = statement(fctxt);
    token_expect(fctxt->tokenizer, TK_WHILE);
    token_expect(fctxt->tokenizer, '(');
    Expr *cond = expression(fctxt);
    token_expect(fctxt->tokenizer, ')');
    token_expect(fctxt->tokenizer, ';');
    return new_stmt_do_while(cond, body);
  }
  case TK_FOR: {
    token_succ(fctxt->tokenizer);
    Expr *init = NULL;
    Expr *cond = NULL;
    Expr *inc = NULL;
    token_expect(fctxt->tokenizer, '(');
    if (token_peek(fctxt->tokenizer)->ty != ';') {
      init = expression(fctxt);
    }
    token_expect(fctxt->tokenizer, ';');
    if (token_peek(fctxt->tokenizer)->ty != ';') {
      cond = expression(fctxt);
    }
    token_expect(fctxt->tokenizer, ';');
    if (token_peek(fctxt->tokenizer)->ty != ')') {
      inc = expression(fctxt);
    }
    token_expect(fctxt->tokenizer, ')');
    Stmt *body = statement(fctxt);
    return new_stmt_for(init, cond, inc, body);
  }
  case TK_GOTO: {
    token_succ(fctxt->tokenizer);
    char *name = token_expect(fctxt->tokenizer, TK_IDENT)->name;
    token_expect(fctxt->tokenizer, ';');
    return new_stmt_goto(name);
  }
  case TK_BREAK: {
    token_succ(fctxt->tokenizer);
    token_expect(fctxt->tokenizer, ';');
    return new_stmt(ST_BREAK);
  }
  case TK_CONTINUE: {
    token_succ(fctxt->tokenizer);
    token_expect(fctxt->tokenizer, ';');
    return new_stmt(ST_CONTINUE);
  }
  case TK_RETURN: {
    token_succ(fctxt->tokenizer);
    Expr *expr = NULL;
    if (token_peek(fctxt->tokenizer)->ty != ';') {
      expr = expression(fctxt);
    }
    token_expect(fctxt->tokenizer, ';');
    return new_stmt_return(expr);
  }
  case '{': {
    return compound_statement(fctxt);
  }
  case ';': {
    token_succ(fctxt->tokenizer);
    return &null_stmt;
  }
  case TK_IDENT: {
    if (token_peek_ahead(fctxt->tokenizer, 1)->ty == ':') {
      Stmt *stmt = new_stmt_label(fctxt, token_pop(fctxt->tokenizer)->name);
      token_succ(fctxt->tokenizer);
      return stmt;
    }
  }
  // fall through
  default: {
    Expr *expr = expression(fctxt);
    token_expect(fctxt->tokenizer, ';');
    return new_stmt_expr(expr);
  }
  }
}

static Stmt *compound_statement(FuncCtxt *fctxt) {
  token_expect(fctxt->tokenizer, '{');

  Stmt *stmt = new_stmt(ST_COMPOUND);
  stmt->stmts = new_vector();
  while (!token_consume(fctxt->tokenizer, '}')) {
    if (token_peek(fctxt->tokenizer)->ty == TK_INT) {
      declaration(fctxt);
      continue;
    }
    vec_push(stmt->stmts, statement(fctxt));
  }
  return stmt;
}

static Function *function_declaration(Tokenizer *tokenizer) {
  FuncCtxt *fctxt = new_func_ctxt(tokenizer);
  Vector *params = new_vector();

  token_expect(fctxt->tokenizer, TK_INT);
  Token *name = token_expect(fctxt->tokenizer, TK_IDENT);
  token_expect(fctxt->tokenizer, '(');
  if (token_peek(fctxt->tokenizer)->ty != ')' &&
      !token_consume(fctxt->tokenizer, TK_VOID)) {
    while (true) {
      Type *base_type = type_specifier(fctxt);
      char *name;
      Type *type;
      declarator(fctxt, base_type, &name, &type);
      if (!register_stack(fctxt, name, type)) {
        error("同じ名前の引数が複数個あります: %s\n", name);
      }
      vec_push(params, name);
      if (token_peek(fctxt->tokenizer)->ty == ')') {
        break;
      }
      token_expect(fctxt->tokenizer, ',');
    }
  }
  token_expect(fctxt->tokenizer, ')');

  Stmt *body = compound_statement(fctxt);

  Function *func = malloc(sizeof(Function));
  func->name = name->name;
  func->stack_size = fctxt->stack_size;
  func->stack_map = fctxt->stack_map;
  func->label_map = fctxt->label_map;
  func->params = params;
  func->body = body;

  return func;
}

static Vector *translation_unit(Tokenizer *tokenizer) {
  Vector *func_list = new_vector();
  while (token_peek(tokenizer)->ty != TK_EOF) {
    vec_push(func_list, function_declaration(tokenizer));
  }
  return func_list;
}
