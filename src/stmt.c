#include "gifcc.h"

static Stmt *new_stmt(int ty, Type *val_type, const Range *range);

static Stmt *new_stmt(int ty, Type *val_type, const Range *range) {
  Stmt *stmt = NEW(Stmt);
  stmt->ty = ty;
  stmt->val_type = val_type;
  stmt->range = range;
  return stmt;
}

Stmt *new_stmt_null(const Range *range) {
  return new_stmt(ST_NULL, new_type(TY_VOID, EMPTY_TYPE_QUALIFIER), range);
}

Stmt *new_stmt_expr(Expr *expr, const Range *range) {
  Stmt *stmt = new_stmt(ST_EXPR, expr->val_type, range);
  stmt->expr = expr;
  return stmt;
}

Stmt *new_stmt_if(Expr *cond, Stmt *then_stmt, Stmt *else_stmt,
                  const Range *range) {
  Stmt *stmt = new_stmt(ST_IF, new_type(TY_VOID, EMPTY_TYPE_QUALIFIER), range);
  stmt->cond = cond;
  stmt->then_stmt = then_stmt;
  stmt->else_stmt = else_stmt;
  return stmt;
}

Stmt *new_stmt_switch(Expr *cond, Stmt *body, const Range *range) {
  Stmt *stmt =
      new_stmt(ST_SWITCH, new_type(TY_VOID, EMPTY_TYPE_QUALIFIER), range);
  stmt->cond = cond;
  stmt->body = body;
  stmt->cases = NEW_VECTOR(StmtVector);
  stmt->default_case = NULL;
  return stmt;
}

Stmt *new_stmt_case(Number val, Stmt *body, const Range *range) {
  Stmt *stmt =
      new_stmt(ST_CASE, new_type(TY_VOID, EMPTY_TYPE_QUALIFIER), range);
  stmt->case_val = val;
  stmt->asm_label = make_label("case");
  stmt->body = body;
  return stmt;
}

Stmt *new_stmt_default(Stmt *body, const Range *range) {
  Stmt *stmt =
      new_stmt(ST_DEFAULT, new_type(TY_VOID, EMPTY_TYPE_QUALIFIER), range);
  stmt->asm_label = make_label("default");
  stmt->body = body;
  return stmt;
}

Stmt *new_stmt_label(const char *c_label, const char *asm_label, Stmt *body,
                     const Range *range) {
  Stmt *stmt = new_stmt(ST_LABEL, body->val_type, range);
  stmt->c_label = c_label;
  stmt->asm_label = asm_label;
  stmt->body = body;
  return stmt;
}

Stmt *new_stmt_while(Expr *cond, Stmt *body, const Range *range) {
  Stmt *stmt =
      new_stmt(ST_WHILE, new_type(TY_VOID, EMPTY_TYPE_QUALIFIER), range);
  stmt->cond = cond;
  stmt->body = body;
  return stmt;
}

Stmt *new_stmt_do_while(Expr *cond, Stmt *body, const Range *range) {
  Stmt *stmt =
      new_stmt(ST_DO_WHILE, new_type(TY_VOID, EMPTY_TYPE_QUALIFIER), range);
  stmt->cond = cond;
  stmt->body = body;
  return stmt;
}

Stmt *new_stmt_for(Stmt *init, Expr *cond, Expr *inc, Stmt *body,
                   const Range *range) {
  Stmt *stmt = new_stmt(ST_FOR, new_type(TY_VOID, EMPTY_TYPE_QUALIFIER), range);
  stmt->init = init;
  stmt->cond = cond;
  stmt->inc = inc;
  stmt->body = body;
  return stmt;
}

Stmt *new_stmt_goto(const char *c_label, const Range *range) {
  Stmt *stmt =
      new_stmt(ST_GOTO, new_type(TY_VOID, EMPTY_TYPE_QUALIFIER), range);
  stmt->c_label = c_label;
  return stmt;
}

Stmt *new_stmt_break(const Range *range) {
  return new_stmt(ST_BREAK, new_type(TY_VOID, EMPTY_TYPE_QUALIFIER), range);
}

Stmt *new_stmt_continue(const Range *range) {
  return new_stmt(ST_CONTINUE, new_type(TY_VOID, EMPTY_TYPE_QUALIFIER), range);
}

Stmt *new_stmt_return(Type *ret_type, Expr *expr, const Range *range) {
  Stmt *stmt =
      new_stmt(ST_RETURN, new_type(TY_VOID, EMPTY_TYPE_QUALIFIER), range);
  if (expr != NULL) {
    stmt->expr = new_expr_cast(ret_type, expr, expr->range);
  } else {
    stmt->expr = NULL;
  }
  return stmt;
}

Stmt *new_stmt_compound(StmtVector *stmts, const Range *range) {
  Type *type;
  if (VEC_LEN(stmts) != 0) {
    Stmt *last = VEC_LAST(stmts);
    type = last->val_type;
  } else {
    type = new_type(TY_VOID, EMPTY_TYPE_QUALIFIER);
  }
  Stmt *stmt = new_stmt(ST_COMPOUND, type, range);
  stmt->stmts = stmts;
  return stmt;
}

Stmt *new_stmt_decl(StackVarDeclVector *decl, const Range *range) {
  Stmt *stmt =
      new_stmt(ST_DECL, new_type(TY_VOID, EMPTY_TYPE_QUALIFIER), range);
  stmt->decl = decl;
  return stmt;
}
