#include "gifcc.h"

static void walk_expr(Expr *expr) {
  switch (expr->ty) {
  case EX_NUM:
    return;
  case EX_STACK_VAR:
    return;
  case EX_GLOBAL_VAR:
    return;
  case EX_STR:
    return;

  case EX_PRE_INC:
    walk_expr(expr->unop.operand);
    return;
  case EX_PRE_DEC:
    walk_expr(expr->unop.operand);
    return;
  case EX_ADDRESS:
    walk_expr(expr->unop.operand);
    return;
  case EX_INDIRECT:
    walk_expr(expr->unop.operand);
    return;
  case EX_PLUS:
    walk_expr(expr->unop.operand);
    return;
  case EX_MINUS:
    walk_expr(expr->unop.operand);
    return;
  case EX_NOT:
    walk_expr(expr->unop.operand);
    return;
  case EX_LOG_NOT:
    walk_expr(expr->unop.operand);
    return;
  case EX_CAST:
    walk_expr(expr->unop.operand);
    return;

  case EX_CALL:
    walk_expr(expr->call.callee);
    if (expr->call.argument != NULL) {
      for (int i = 0; i < vec_len(expr->call.argument); i++) {
        walk_expr(vec_get(expr->call.argument, i));
      }
    }
    return;
  case EX_POST_INC:
    walk_expr(expr->unop.operand);
    return;
  case EX_POST_DEC:
    walk_expr(expr->unop.operand);
    return;

  case EX_ADD:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_SUB:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_MUL:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_DIV:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_MOD:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_EQEQ:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_NOTEQ:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_LT:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_GT:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_LTEQ:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_GTEQ:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_LSHIFT:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_RSHIFT:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_AND:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_XOR:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_OR:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_LOG_AND:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_LOG_OR:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_ASSIGN:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_COMMA:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  case EX_COND:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    return;
  }
}

static void walk_stmt(Stmt *stmt) {
  switch (stmt->ty) {
  case ST_EXPR:
    walk_expr(stmt->expr);
    return;
  case ST_COMPOUND:
    for (int i = 0; i < vec_len(stmt->stmts); i++) {
      walk_stmt(vec_get(stmt->stmts, i));
    }
    return;
  case ST_IF:
    walk_expr(stmt->cond);
    walk_stmt(stmt->then_stmt);
    walk_stmt(stmt->else_stmt);
    return;
  case ST_SWITCH:
    walk_expr(stmt->cond);
    for (int i = 0; i < vec_len(stmt->cases); i++) {
      walk_stmt(vec_get(stmt->cases, i));
    }
    if (stmt->default_case != NULL) {
      walk_stmt(stmt->default_case);
    }
    return;
  case ST_CASE:
    walk_expr(stmt->expr);
    walk_stmt(stmt->body);
    return;
  case ST_DEFAULT:
    walk_stmt(stmt->body);
    return;
  case ST_LABEL:
    walk_stmt(stmt->body);
    return;
  case ST_WHILE:
    walk_expr(stmt->cond);
    walk_stmt(stmt->body);
    return;
  case ST_DO_WHILE:
    walk_expr(stmt->cond);
    walk_stmt(stmt->body);
    return;
  case ST_FOR:
    if (stmt->init != NULL) {
      walk_expr(stmt->init);
    }
    if (stmt->cond != NULL) {
      walk_expr(stmt->cond);
    }
    if (stmt->inc != NULL) {
      walk_expr(stmt->inc);
    }
    walk_stmt(stmt->body);
    return;
  case ST_GOTO:
    return;
  case ST_BREAK:
    return;
  case ST_CONTINUE:
    return;
  case ST_RETURN:
    if (stmt->expr != NULL) {
      walk_expr(stmt->expr);
    }
    return;
  case ST_NULL:
    return;
  }
  assert(false);
}

static void walk_initializer(Initializer *init) {
  if (init == NULL) {
    return;
  }

  if (init->members != NULL) {
    for (int i = 0; i < map_size(init->members); i++) {
      char *name;
      Initializer *meminit = map_get_by_index(init->members, i, &name);
      walk_initializer(meminit);
    }
  }

  if (init->elements != NULL) {
    for (int i = 0; i < vec_len(init->elements); i++) {
      Initializer *eleminit = vec_get(init->elements, i);
      walk_initializer(eleminit);
    }
  }

  if (init->expr != NULL) {
    walk_expr(init->expr);
  }
}

static void walk_func(Function *func) { walk_stmt(func->body); }

static void walk_gvar(GlobalVar *gvar) { walk_initializer(gvar->init); }

static void walk_str(StringLiteral *str __attribute__((unused))) {
  // do nothing
}

void sema_expr(Expr *expr) { walk_expr(expr); }

void sema(TranslationUnit *tunit) {
  for (int i = 0; i < vec_len(tunit->func_list); i++) {
    walk_func(vec_get(tunit->func_list, i));
  }
  for (int i = 0; i < vec_len(tunit->gvar_list); i++) {
    walk_gvar(vec_get(tunit->gvar_list, i));
  }
  for (int i = 0; i < vec_len(tunit->str_list); i++) {
    walk_str(vec_get(tunit->str_list, i));
  }
}
