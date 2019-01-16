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

static bool register_stack(char *name) {
  if (map_get(stack_map, name)) {
    return false;
  }

  StackVar *var = malloc(sizeof(StackVar));
  var->offset = stack_size;
  map_put(stack_map, name, var);
  stack_size += 8;

  return true;
}

static Expr *new_expr(int ty) {
  Expr *expr = malloc(sizeof(Expr));
  expr->ty = ty;
  return expr;
}

static Expr *new_expr_binop(int ty, Expr *lhs, Expr *rhs) {
  Expr *expr = new_expr(ty);
  expr->lhs = lhs;
  expr->rhs = rhs;
  return expr;
}

static Expr *new_expr_num(int val) {
  Expr *expr = new_expr(EX_NUM);
  expr->val = val;
  return expr;
}

static Expr *new_expr_ident(char *name) {
  Expr *expr = new_expr(EX_IDENT);
  expr->name = name;
  return expr;
}

static Expr *new_expr_call(Expr *callee, Vector *argument) {
  Expr *expr = new_expr(EX_CALL);
  expr->callee = callee;
  expr->argument = argument;
  return expr;
}

static Expr *new_expr_cond(Expr *cond, Expr *then_expr, Expr *else_expr) {
  Expr *expr = new_expr(EX_COND);
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
      return new_expr_binop(EX_INC, expr, NULL);
    } else if (consume(TK_DEC)) {
      return new_expr_binop(EX_DEC, expr, NULL);
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
    return new_expr_binop('&', NULL, cast_expression());
  }
  if (consume('*')) {
    return new_expr_binop('*', NULL, cast_expression());
  }
  if (consume('+')) {
    return new_expr_binop('+', NULL, cast_expression());
  }
  if (consume('-')) {
    return new_expr_binop('-', NULL, cast_expression());
  }
  if (consume('~')) {
    return new_expr_binop('~', NULL, cast_expression());
  }
  if (consume('!')) {
    return new_expr_binop('!', NULL, cast_expression());
  }
  if (consume(TK_INC)) {
    return new_expr_binop(EX_INC, NULL, cast_expression());
  }
  if (consume(TK_DEC)) {
    return new_expr_binop(EX_DEC, NULL, cast_expression());
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
  expect(TK_INT);
  Token *ident = expect(TK_IDENT);
  expect(';');

  if (!register_stack(ident->name)) {
    error("同じ名前の変数が複数回宣言されました: %s", ident->name);
  }
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
      expect(TK_INT);
      Token *arg = expect(TK_IDENT);
      if (!register_stack(arg->name)) {
        error("同じ名前の引数が複数個あります: %s\n", arg->name);
      }
      vec_push(params, arg->name);
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
