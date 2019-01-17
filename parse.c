#include "gifcc.h"
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// expression
static Expr *primary_expression(void);
static Expr *postfix_expression(void);
static Vector *argument_expression_list(void);
static Expr *unary_expression(void);
static Expr *cast_expression(void);
static Expr *multiplicative_expression(void);
static Expr *additive_expression(void);
static Expr *shift_expression(void);
static Expr *relational_expression(void);
static Expr *equality_expression(void);
static Expr *and_expression(void);
static Expr *exclusive_or_expression(void);
static Expr *inclusive_or_expression(void);
static Expr *logical_and_expression(void);
static Expr *logical_or_expression(void);
static Expr *conditional_expression(void);
static Expr *assignment_expression(void);
static Expr *expression(void);
static Expr *constant_expression(void);
static void declaration(void);
static Type *type_specifier(void);
static void declarator(Type *base_type, char **name, Type **type);
static Stmt *statement(void);
static Stmt *compound_statement(void);
static Function *function_declaration(void);

static int pos = 0;
static int stack_size = 0;
static Map *stack_map;
static Stmt null_stmt = {
    .ty = ST_NULL,
};
static Vector *switches = NULL;
static Map *label_map;

static bool register_stack(char *name, Type *type) {
  if (map_get(stack_map, name)) {
    return false;
  }

  StackVar *var = malloc(sizeof(StackVar));
  var->offset = stack_size;
  var->type = type;
  map_put(stack_map, name, var);
  stack_size += 8;

  return true;
}

static StackVar *get_stack(char *name) { return map_get(stack_map, name); }

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
static Type *integer_promoted(Type *ty) { return ty; }
static Type *arith_converted(Type *ty1, Type *ty2) {
  if (!is_arith_type(ty1) || !is_arith_type(ty2)) {
    return NULL;
  }
  return ty1;
}

static int get_val_size(Type *ty) {
  switch (ty->ty) {
  case TY_INT:
    return 4;
  case TY_PTR:
    return 8;
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

static Expr *new_expr_ident(char *name) {
  StackVar *var = get_stack(name);
  // 未知の識別子はint型として扱う
  Type *type = var != NULL ? var->type : new_type(TY_INT);
  Expr *expr = new_expr(EX_IDENT, type);
  expr->name = name;
  return expr;
}

static Expr *new_expr_call(Expr *callee, Vector *argument) {
  // TODO: 関数の戻り値はintを仮定する
  Expr *expr = new_expr(EX_CALL, new_type(TY_INT));
  expr->callee = callee;
  expr->argument = argument;
  return expr;
}

static Expr *new_expr_postfix(int ty, Expr *operand) {
  Expr *expr = new_expr(ty, operand->val_type);
  expr->lhs = operand;
  expr->rhs = NULL;
  return expr;
}

static Expr *new_expr_unary(int ty, Expr *operand) {
  Type *val_type;
  if (ty == '&') {
    val_type = new_type_ptr(operand->val_type);
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
  case EX_ADD_ASSIGN:
    if (is_ptr_type(lhs->val_type) || is_ptr_type(rhs->val_type)) {
      return new_expr_binop('=', lhs, new_expr_binop('+', lhs, rhs));
    }
    val_type = lhs->val_type;
    break;
  case EX_SUB_ASSIGN:
    if (is_ptr_type(lhs->val_type) || is_ptr_type(rhs->val_type)) {
      return new_expr_binop('=', lhs, new_expr_binop('-', lhs, rhs));
    }
    val_type = lhs->val_type;
    break;
  case EX_MUL_ASSIGN:
  case EX_DIV_ASSIGN:
  case EX_MOD_ASSIGN:
  case EX_LSHIFT_ASSIGN:
  case EX_RSHIFT_ASSIGN:
  case EX_AND_ASSIGN:
  case EX_OR_ASSIGN:
  case EX_XOR_ASSIGN:
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

static Stmt *new_stmt_label(char *name) {
  Stmt *stmt = new_stmt(ST_LABEL);
  stmt->name = name;
  stmt->label = make_label();
  map_put(label_map, stmt->name, stmt->label);
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

static bool consume(int ty) {
  if (get_token(pos)->ty != ty) {
    return false;
  }
  pos++;
  return true;
}

static Token *expect(int ty) {
  Token *token = get_token(pos);
  if (!consume(ty)) {
    if (ty <= 255) {
      error("'%c' がありません: %s", ty, get_token(pos)->input);
    } else {
      assert(ty == TK_WHILE);
      error("'while' がありません: %s", get_token(pos)->input);
    }
  }
  return token;
}

Vector *translation_unit(void) {
  Vector *func_list = new_vector();
  while (get_token(pos)->ty != TK_EOF) {
    vec_push(func_list, function_declaration());
  }
  return func_list;
}

static Expr *primary_expression(void) {
  if (get_token(pos)->ty == TK_NUM) {
    return new_expr_num(get_token(pos++)->val);
  }
  if (get_token(pos)->ty == TK_IDENT) {
    return new_expr_ident(get_token(pos++)->name);
  }
  if (consume('(')) {
    Expr *expr = expression();
    expect(')');
    return expr;
  }
  error("数値でも開きカッコでもないトークンです: %s", get_token(pos)->input);
}

static Expr *postfix_expression(void) {
  Expr *expr = primary_expression();
  while (true) {
    if (consume('(')) {
      Vector *argument = NULL;
      if (get_token(pos)->ty != ')') {
        argument = argument_expression_list();
      }
      expect(')');
      expr = new_expr_call(expr, argument);
    } else if (consume(TK_INC)) {
      int val;
      if (is_ptr_type(expr->val_type)) {
        val = get_val_size(expr->val_type->ptrof);
      } else {
        val = 1;
      }
      expr = new_expr_postfix(EX_INC, expr);
      expr->val = val;
    } else if (consume(TK_DEC)) {
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

static Vector *argument_expression_list(void) {
  Vector *argument = new_vector();
  while (true) {
    vec_push(argument, assignment_expression());
    if (!consume(',')) {
      break;
    }
  }
  return argument;
}

static Expr *unary_expression(void) {
  if (consume('&')) {
    return new_expr_unary('&', cast_expression());
  }
  if (consume('*')) {
    return new_expr_unary('*', cast_expression());
  }
  if (consume('+')) {
    return new_expr_unary('+', cast_expression());
  }
  if (consume('-')) {
    return new_expr_unary('-', cast_expression());
  }
  if (consume('~')) {
    return new_expr_unary('~', cast_expression());
  }
  if (consume('!')) {
    return new_expr_unary('!', cast_expression());
  }
  if (consume(TK_INC)) {
    Expr *expr = cast_expression();
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
  if (consume(TK_DEC)) {
    Expr *expr = cast_expression();
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
  return postfix_expression();
}

static Expr *cast_expression(void) { return unary_expression(); }

static Expr *multiplicative_expression(void) {
  Expr *expr = cast_expression();
  while (true) {
    if (consume('*')) {
      expr = new_expr_binop('*', expr, cast_expression());
    } else if (consume('/')) {
      expr = new_expr_binop('/', expr, cast_expression());
    } else if (consume('%')) {
      expr = new_expr_binop('%', expr, cast_expression());
    } else {
      return expr;
    }
  }
}

static Expr *additive_expression(void) {
  Expr *expr = multiplicative_expression();
  while (true) {
    if (consume('+')) {
      expr = new_expr_binop('+', expr, multiplicative_expression());
    } else if (consume('-')) {
      expr = new_expr_binop('-', expr, multiplicative_expression());
    } else {
      return expr;
    }
  }
}

static Expr *shift_expression(void) {
  Expr *expr = additive_expression();
  while (true) {
    if (consume(TK_LSHIFT)) {
      expr = new_expr_binop(EX_LSHIFT, expr, additive_expression());
    } else if (consume(TK_RSHIFT)) {
      expr = new_expr_binop(EX_RSHIFT, expr, additive_expression());
    } else {
      return expr;
    }
  }
  return expr;
}

static Expr *relational_expression(void) {
  Expr *expr = shift_expression();
  while (true) {
    if (consume('<')) {
      expr = new_expr_binop('<', expr, shift_expression());
    } else if (consume('>')) {
      expr = new_expr_binop('>', expr, shift_expression());
    } else if (consume(TK_LTEQ)) {
      expr = new_expr_binop(EX_LTEQ, expr, shift_expression());
    } else if (consume(TK_GTEQ)) {
      expr = new_expr_binop(EX_GTEQ, expr, shift_expression());
    } else {
      return expr;
    }
  }
  return expr;
}

static Expr *equality_expression(void) {
  Expr *expr = relational_expression();
  while (true) {
    if (consume(TK_EQEQ)) {
      expr = new_expr_binop(EX_EQEQ, expr, relational_expression());
    } else if (consume(TK_NOTEQ)) {
      expr = new_expr_binop(EX_NOTEQ, expr, relational_expression());
    } else {
      return expr;
    }
  }
}

static Expr *and_expression(void) {
  Expr *expr = equality_expression();
  while (true) {
    if (consume('&')) {
      expr = new_expr_binop('&', expr, equality_expression());
    } else {
      return expr;
    }
  }
  return expr;
}

static Expr *exclusive_or_expression(void) {
  Expr *expr = and_expression();
  while (true) {
    if (consume('^')) {
      expr = new_expr_binop('^', expr, and_expression());
    } else {
      return expr;
    }
  }
  return expr;
}
static Expr *inclusive_or_expression(void) {
  Expr *expr = exclusive_or_expression();
  while (true) {
    if (consume('|')) {
      expr = new_expr_binop('|', expr, exclusive_or_expression());
    } else {
      return expr;
    }
  }
  return expr;
}
static Expr *logical_and_expression(void) {
  Expr *expr = inclusive_or_expression();
  while (true) {
    if (consume(TK_LOGAND)) {
      expr = new_expr_binop(EX_LOGAND, expr, inclusive_or_expression());
    } else {
      return expr;
    }
  }
  return expr;
}
static Expr *logical_or_expression(void) {
  Expr *expr = logical_and_expression();
  while (true) {
    if (consume(TK_LOGOR)) {
      expr = new_expr_binop(EX_LOGOR, expr, logical_and_expression());
    } else {
      return expr;
    }
  }
  return expr;
}
static Expr *conditional_expression(void) {
  Expr *cond = logical_or_expression();
  if (consume('?')) {
    Expr *then_expr = expression();
    expect(':');
    Expr *else_expr = conditional_expression();
    return new_expr_cond(cond, then_expr, else_expr);
  }
  return cond;
}

static Expr *assignment_expression(void) {
  Expr *lhs = conditional_expression();
  if (consume('=')) {
    return new_expr_binop('=', lhs, assignment_expression());
  }
  if (consume(TK_MUL_ASSIGN)) {
    return new_expr_binop(EX_MUL_ASSIGN, lhs, assignment_expression());
  }
  if (consume(TK_DIV_ASSIGN)) {
    return new_expr_binop(EX_DIV_ASSIGN, lhs, assignment_expression());
  }
  if (consume(TK_MOD_ASSIGN)) {
    return new_expr_binop(EX_MOD_ASSIGN, lhs, assignment_expression());
  }
  if (consume(TK_ADD_ASSIGN)) {
    return new_expr_binop(EX_ADD_ASSIGN, lhs, assignment_expression());
  }
  if (consume(TK_SUB_ASSIGN)) {
    return new_expr_binop(EX_SUB_ASSIGN, lhs, assignment_expression());
  }
  if (consume(TK_LSHIFT_ASSIGN)) {
    return new_expr_binop(EX_LSHIFT_ASSIGN, lhs, assignment_expression());
  }
  if (consume(TK_RSHIFT_ASSIGN)) {
    return new_expr_binop(EX_RSHIFT_ASSIGN, lhs, assignment_expression());
  }
  if (consume(TK_AND_ASSIGN)) {
    return new_expr_binop(EX_AND_ASSIGN, lhs, assignment_expression());
  }
  if (consume(TK_OR_ASSIGN)) {
    return new_expr_binop(EX_OR_ASSIGN, lhs, assignment_expression());
  }
  if (consume(TK_XOR_ASSIGN)) {
    return new_expr_binop(EX_XOR_ASSIGN, lhs, assignment_expression());
  }
  return lhs;
}

static Expr *expression(void) {
  Expr *expr = assignment_expression();
  while (true) {
    if (consume(',')) {
      expr = new_expr_binop(',', expr, assignment_expression());
    } else {
      return expr;
    }
  }
  return expr;
}

static Expr *constant_expression(void) { return conditional_expression(); }

static void declaration(void) {
  Type *base_type = type_specifier();
  char *name;
  Type *type;
  declarator(base_type, &name, &type);
  expect(';');

  if (!register_stack(name, type)) {
    error("同じ名前の変数が複数回宣言されました: %s", name);
  }
}

static Type *type_specifier(void) {
  expect(TK_INT);
  return new_type(TY_INT);
}

static void declarator(Type *base_type, char **name, Type **type) {
  while (consume('*')) {
    base_type = new_type_ptr(base_type);
  }
  *name = expect(TK_IDENT)->name;
  *type = base_type;
}

static Stmt *statement(void) {
  switch (get_token(pos)->ty) {
  case TK_IF: {
    pos++;
    expect('(');
    Expr *cond = expression();
    expect(')');
    Stmt *then_stmt = statement();
    Stmt *else_stmt = &null_stmt;
    if (consume(TK_ELSE)) {
      else_stmt = statement();
    }
    return new_stmt_if(cond, then_stmt, else_stmt);
  }
  case TK_SWITCH: {
    pos++;
    expect('(');
    Expr *cond = expression();
    expect(')');
    Stmt *stmt = new_stmt_switch(cond, NULL);
    vec_push(switches, stmt);
    stmt->body = statement();
    vec_pop(switches);
    return stmt;
  }
  case TK_CASE: {
    pos++;
    Expr *expr = constant_expression();
    expect(':');
    Stmt *stmt = new_stmt_case(expr);
    if (switches->len <= 0) {
      error("switch文中でない箇所にcase文があります");
    }
    Stmt *switch_stmt = switches->data[switches->len - 1];
    vec_push(switch_stmt->cases, stmt);
    return stmt;
  }
  case TK_DEFAULT: {
    pos++;
    expect(':');
    Stmt *stmt = new_stmt_default();
    if (switches->len <= 0) {
      error("switch文中でない箇所にcase文があります");
    }
    Stmt *switch_expr = switches->data[switches->len - 1];
    switch_expr->default_case = stmt;
    return stmt;
  }
  case TK_WHILE: {
    pos++;
    expect('(');
    Expr *cond = expression();
    expect(')');
    Stmt *body = statement();
    return new_stmt_while(cond, body);
  }
  case TK_DO: {
    pos++;
    Stmt *body = statement();
    expect(TK_WHILE);
    expect('(');
    Expr *cond = expression();
    expect(')');
    expect(';');
    return new_stmt_do_while(cond, body);
  }
  case TK_FOR: {
    pos++;
    Expr *init = NULL;
    Expr *cond = NULL;
    Expr *inc = NULL;
    expect('(');
    if (get_token(pos)->ty != ';') {
      init = expression();
    }
    expect(';');
    if (get_token(pos)->ty != ';') {
      cond = expression();
    }
    expect(';');
    if (get_token(pos)->ty != ')') {
      inc = expression();
    }
    expect(')');
    Stmt *body = statement();
    return new_stmt_for(init, cond, inc, body);
  }
  case TK_GOTO: {
    pos++;
    char *name = expect(TK_IDENT)->name;
    expect(';');
    return new_stmt_goto(name);
  }
  case TK_BREAK: {
    pos++;
    expect(';');
    return new_stmt(ST_BREAK);
  }
  case TK_CONTINUE: {
    pos++;
    expect(';');
    return new_stmt(ST_CONTINUE);
  }
  case TK_RETURN: {
    pos++;
    Expr *expr = NULL;
    if (get_token(pos)->ty != ';') {
      expr = expression();
    }
    expect(';');
    return new_stmt_return(expr);
  }
  case '{': {
    return compound_statement();
  }
  case ';': {
    pos++;
    return &null_stmt;
  }
  case TK_IDENT: {
    if (get_token(pos + 1)->ty == ':') {
      Stmt *stmt = new_stmt_label(get_token(pos)->name);
      pos += 2;
      return stmt;
    }
    // fall through
  }
  default: {
    Expr *expr = expression();
    expect(';');
    return new_stmt_expr(expr);
  }
  }
}

static Stmt *compound_statement(void) {
  expect('{');

  Stmt *stmt = new_stmt(ST_COMPOUND);
  stmt->stmts = new_vector();
  while (!consume('}')) {
    if (get_token(pos)->ty == TK_INT) {
      declaration();
      continue;
    }
    vec_push(stmt->stmts, statement());
  }
  return stmt;
}

static Function *function_declaration(void) {
  switches = new_vector();

  stack_size = 0;
  stack_map = new_map();
  label_map = new_map();

  Vector *params = new_vector();

  expect(TK_INT);
  Token *name = expect(TK_IDENT);
  expect('(');
  if (get_token(pos)->ty != ')' && !consume(TK_VOID)) {
    while (true) {
      Type *base_type = type_specifier();
      char *name;
      Type *type;
      declarator(base_type, &name, &type);
      if (!register_stack(name, type)) {
        error("同じ名前の引数が複数個あります: %s\n", name);
      }
      vec_push(params, name);
      if (get_token(pos)->ty == ')') {
        break;
      }
      expect(',');
    }
  }
  expect(')');

  Stmt *body = compound_statement();

  Function *func = malloc(sizeof(Function));
  func->name = name->name;
  func->stack_size = stack_size;
  func->stack_map = stack_map;
  func->label_map = label_map;
  func->params = params;
  func->body = body;

  return func;
}
