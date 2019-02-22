#include "gifcc.h"

#define UNARYOP(op, r, a, range)                                               \
  switch ((op)) {                                                              \
  case EX_MINUS:                                                               \
    *(r) = (-(a));                                                             \
    break;                                                                     \
  case EX_NOT:                                                                 \
    *(r) = (~(a));                                                             \
    break;                                                                     \
  default:                                                                     \
    range_error(range, "不正な演算です: %d", op);                              \
    break;                                                                     \
  }

static void eval_unop(Expr *expr) {
  Expr *operand = expr->unop.operand;
  if (operand->ty != EX_NUM) {
    return;
  }

  type_t type = expr->val_type->ty;
  expr_t op = expr->ty;

  expr->ty = EX_NUM;
  expr->num = (Number){.type = type};

  Number nop = operand->num;
  switch (type) {
  case TY_S_INT: {
    signed int a, *r = &expr->num.s_int_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP(op, r, a, expr->range);
    return;
  }

  case TY_S_LONG: {
    signed long a, *r = &expr->num.s_long_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP(op, r, a, expr->range);
    return;
  }

  case TY_S_LLONG: {
    signed long long a, *r = &expr->num.s_llong_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP(op, r, a, expr->range);
    return;
  }

  case TY_U_INT: {
    unsigned int a, *r = &expr->num.u_int_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP(op, r, a, expr->range);
    return;
  }

  case TY_U_LONG: {
    unsigned long a, *r = &expr->num.u_long_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP(op, r, a, expr->range);
    return;
  }

  case TY_U_LLONG: {
    unsigned long long a, *r = &expr->num.u_llong_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP(op, r, a, expr->range);
    return;
  }

  case TY_VOID:
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_S_SHORT:
  case TY_U_CHAR:
  case TY_U_SHORT:
  case TY_PTR:
  case TY_ENUM:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    break;
  }
  range_error(expr->range, "不正な型の演算です: %d, %d, %d", op, type,
              nop.type);
}

static void eval_cast(Expr *expr) {
  Expr *operand = expr->unop.operand;
  if (operand->ty == EX_CAST) {
    expr->unop.operand = operand->unop.operand;
    return;
  }

  if (operand->ty != EX_NUM) {
    return;
  }

  expr->ty = EX_NUM;
  Number *exnum = &expr->num;
  exnum->type = expr->val_type->ty;
  const Number *opnum = &operand->num;
  switch (expr->val_type->ty) {
  case TY_VOID:
    return;
  case TY_CHAR:
    SET_NUMBER_VAL(exnum->char_val, opnum);
    return;
  case TY_S_CHAR:
    SET_NUMBER_VAL(exnum->s_char_val, opnum);
    return;
  case TY_S_SHORT:
    SET_NUMBER_VAL(exnum->s_short_val, opnum);
    return;
  case TY_S_INT:
    SET_NUMBER_VAL(exnum->s_int_val, opnum);
    return;
  case TY_S_LONG:
    SET_NUMBER_VAL(exnum->s_long_val, opnum);
    return;
  case TY_S_LLONG:
    SET_NUMBER_VAL(exnum->s_llong_val, opnum);
    return;
  case TY_U_CHAR:
    SET_NUMBER_VAL(exnum->u_char_val, opnum);
    return;
  case TY_U_SHORT:
    SET_NUMBER_VAL(exnum->u_short_val, opnum);
    return;
  case TY_U_INT:
    SET_NUMBER_VAL(exnum->u_int_val, opnum);
    return;
  case TY_U_LONG:
    SET_NUMBER_VAL(exnum->u_long_val, opnum);
    return;
  case TY_U_LLONG:
    SET_NUMBER_VAL(exnum->u_llong_val, opnum);
    return;
  case TY_PTR:
    SET_NUMBER_VAL(exnum->ptr_val, opnum);
    return;
  case TY_ENUM:
    SET_NUMBER_VAL(exnum->enum_val, opnum);
    return;
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    break;
  }
}

#define BINOP(op, r, a, b, range)                                              \
  switch ((op)) {                                                              \
  case EX_MUL:                                                                 \
    *(r) = ((a) * (b));                                                        \
    break;                                                                     \
  case EX_DIV:                                                                 \
    if (b == 0) {                                                              \
      range_error(range, "ゼロ除算です");                                      \
    }                                                                          \
    *(r) = ((a) / (b));                                                        \
    break;                                                                     \
  case EX_MOD:                                                                 \
    *(r) = ((a) % (b));                                                        \
    break;                                                                     \
  case EX_ADD:                                                                 \
    *(r) = ((a) + (b));                                                        \
    break;                                                                     \
  case EX_SUB:                                                                 \
    *(r) = ((a) - (b));                                                        \
    break;                                                                     \
  case EX_AND:                                                                 \
    *(r) = ((a) & (b));                                                        \
    break;                                                                     \
  case EX_XOR:                                                                 \
    *(r) = ((a) ^ (b));                                                        \
    break;                                                                     \
  case EX_OR:                                                                  \
    *(r) = ((a) | (b));                                                        \
    break;                                                                     \
  default:                                                                     \
    range_error(range, "不正な演算です: %d", op);                              \
    break;                                                                     \
  }

static void eval_binop(Expr *expr) {
  Expr *lhs = expr->binop.lhs;
  Expr *rhs = expr->binop.rhs;
  if (lhs->ty != EX_NUM || rhs->ty != EX_NUM) {
    return;
  }

  Number lnum = lhs->num;
  Number rnum = rhs->num;
  assert(lnum.type == rnum.type);

  type_t type = lnum.type;
  expr_t op = expr->ty;

  expr->ty = EX_NUM;
  expr->num = (Number){.type = type};

  switch (type) {
  case TY_S_INT: {
    signed int a, b, *r = &expr->num.s_int_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP(op, r, a, b, expr->range);
    return;
  }

  case TY_S_LONG: {
    signed long a, b, *r = &expr->num.s_long_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP(op, r, a, b, expr->range);
    return;
  }

  case TY_S_LLONG: {
    signed long long a, b, *r = &expr->num.s_llong_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP(op, r, a, b, expr->range);
    return;
  }

  case TY_U_INT: {
    unsigned int a, b, *r = &expr->num.u_int_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP(op, r, a, b, expr->range);
    return;
  }

  case TY_U_LONG: {
    unsigned long a, b, *r = &expr->num.u_long_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP(op, r, a, b, expr->range);
    return;
  }

  case TY_U_LLONG: {
    unsigned long long a, b, *r = &expr->num.u_llong_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP(op, r, a, b, expr->range);
    return;
  }

  case TY_VOID:
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_S_SHORT:
  case TY_U_CHAR:
  case TY_U_SHORT:
  case TY_PTR:
  case TY_ENUM:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    break;
  }
  range_error(expr->range, "不正な型の演算です: %d, %d, %d, %d", op, type,
              lnum.type, rnum.type);
}

#define BINOP_COMP(op, r, a, b, range)                                         \
  switch ((op)) {                                                              \
  case EX_LT:                                                                  \
    *(r) = ((a) < (b));                                                        \
    break;                                                                     \
  case EX_GT:                                                                  \
    *(r) = ((a) > (b));                                                        \
    break;                                                                     \
  case EX_LTEQ:                                                                \
    *(r) = ((a) <= (b));                                                       \
    break;                                                                     \
  case EX_GTEQ:                                                                \
    *(r) = ((a) >= (b));                                                       \
    break;                                                                     \
  case EX_EQEQ:                                                                \
    *(r) = ((a) == (b));                                                       \
    break;                                                                     \
  case EX_NOTEQ:                                                               \
    *(r) = ((a) != (b));                                                       \
    break;                                                                     \
  default:                                                                     \
    range_error(range, "不正な演算です: %d", op);                              \
    break;                                                                     \
  }

static void eval_binop_comp(Expr *expr) {
  Expr *lhs = expr->binop.lhs;
  Expr *rhs = expr->binop.rhs;
  if (lhs->ty != EX_NUM || rhs->ty != EX_NUM) {
    return;
  }

  Number lnum = lhs->num;
  Number rnum = rhs->num;
  assert(lnum.type == rnum.type);

  type_t type = lnum.type;
  expr_t op = expr->ty;

  expr->ty = EX_NUM;
  expr->num = (Number){.type = type};

  int *r = &expr->num.s_int_val;

  switch (type) {
  case TY_S_INT: {
    signed int a, b;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_COMP(op, r, a, b, expr->range);
    return;
  }

  case TY_S_LONG: {
    signed long a, b;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_COMP(op, r, a, b, expr->range);
    return;
  }

  case TY_S_LLONG: {
    signed long long a, b;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_COMP(op, r, a, b, expr->range);
    return;
  }

  case TY_U_INT: {
    unsigned int a, b;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_COMP(op, r, a, b, expr->range);
    return;
  }

  case TY_U_LONG: {
    unsigned long a, b;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_COMP(op, r, a, b, expr->range);
    return;
  }

  case TY_U_LLONG: {
    unsigned long long a, b;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_COMP(op, r, a, b, expr->range);
    return;
  }

  case TY_VOID:
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_S_SHORT:
  case TY_U_CHAR:
  case TY_U_SHORT:
  case TY_PTR:
  case TY_ENUM:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    break;
  }
  range_error(expr->range, "不正な型の演算です: %d, %d, %d, %d", op, type,
              lnum.type, rnum.type);
}

#define BINOP_SHIFT(op, r, a, b, range)                                        \
  switch ((op)) {                                                              \
  case EX_LSHIFT:                                                              \
    *(r) = ((a) << (b));                                                       \
    break;                                                                     \
  case EX_RSHIFT:                                                              \
    *(r) = ((a) >> (b));                                                       \
    break;                                                                     \
  default:                                                                     \
    range_error(range, "不正な演算です: %d", op);                              \
    break;                                                                     \
  }

static void eval_binop_shift(Expr *expr) {
  Expr *lhs = expr->binop.lhs;
  Expr *rhs = expr->binop.rhs;
  if (lhs->ty != EX_NUM || rhs->ty != EX_NUM) {
    return;
  }

  Number lnum = lhs->num;
  unsigned char width;
  SET_NUMBER_VAL(width, &rhs->num);

  type_t type = expr->val_type->ty;
  expr_t op = expr->ty;

  expr->ty = EX_NUM;
  expr->num = (Number){.type = type};

  switch (type) {
  case TY_S_INT: {
    signed int a, *r = &expr->num.s_int_val;
    SET_NUMBER_VAL(a, &lnum);
    BINOP_SHIFT(op, r, a, width, expr->range);
    return;
  }

  case TY_S_LONG: {
    signed long a, *r = &expr->num.s_long_val;
    SET_NUMBER_VAL(a, &lnum);
    BINOP_SHIFT(op, r, a, width, expr->range);
    return;
  }

  case TY_S_LLONG: {
    signed long long a, *r = &expr->num.s_llong_val;
    SET_NUMBER_VAL(a, &lnum);
    BINOP_SHIFT(op, r, a, width, expr->range);
    return;
  }

  case TY_U_INT: {
    unsigned int a, *r = &expr->num.u_int_val;
    SET_NUMBER_VAL(a, &lnum);
    BINOP_SHIFT(op, r, a, width, expr->range);
    return;
  }

  case TY_U_LONG: {
    unsigned long a, *r = &expr->num.u_long_val;
    SET_NUMBER_VAL(a, &lnum);
    BINOP_SHIFT(op, r, a, width, expr->range);
    return;
  }

  case TY_U_LLONG: {
    unsigned long long a, *r = &expr->num.u_llong_val;
    SET_NUMBER_VAL(a, &lnum);
    BINOP_SHIFT(op, r, a, width, expr->range);
    return;
  }

  case TY_VOID:
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_S_SHORT:
  case TY_U_CHAR:
  case TY_U_SHORT:
  case TY_PTR:
  case TY_ENUM:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    break;
  }
  range_error(expr->range, "不正な型の演算です: %d, %d, %d", op, type,
              lnum.type);
}

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
  case EX_PRE_DEC:
  case EX_ADDRESS:
  case EX_INDIRECT:
    walk_expr(expr->unop.operand);
    return;
  case EX_PLUS: {
    Expr *operand = expr->unop.operand;
    walk_expr(operand);
    *expr = *operand;
    return;
  }
  case EX_MINUS:
  case EX_NOT:
    walk_expr(expr->unop.operand);
    eval_unop(expr);
    return;
  case EX_LOG_NOT:
    assert(false);
    return;
  case EX_CAST:
    walk_expr(expr->unop.operand);
    eval_cast(expr);
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
  case EX_POST_DEC:
    walk_expr(expr->unop.operand);
    return;

  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    eval_binop(expr);
    return;
  case EX_EQEQ:
  case EX_NOTEQ:
  case EX_LT:
  case EX_GT:
  case EX_LTEQ:
  case EX_GTEQ:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    eval_binop_comp(expr);
    return;
  case EX_LSHIFT:
  case EX_RSHIFT:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    eval_binop_shift(expr);
    return;
  case EX_AND:
  case EX_XOR:
  case EX_OR:
    walk_expr(expr->binop.lhs);
    walk_expr(expr->binop.rhs);
    eval_binop(expr);
    return;
  case EX_LOG_AND: {
    bool lhs_is_true = false;
    walk_expr(expr->binop.lhs);
    if (expr->binop.lhs->ty == EX_NUM) {
      int lval;
      SET_NUMBER_VAL(lval, &expr->binop.lhs->num);
      if (lval == 0) {
        expr->ty = EX_NUM;
        expr->num = new_number_int(0);
        return;
      }
      lhs_is_true = true;
    }
    walk_expr(expr->binop.rhs);
    if (expr->binop.rhs->ty == EX_NUM) {
      int rval;
      SET_NUMBER_VAL(rval, &expr->binop.rhs->num);
      if (rval == 0) {
        expr->ty = EX_NUM;
        expr->num = new_number_int(0);
        return;
      }
      if (lhs_is_true) {
        expr->ty = EX_NUM;
        expr->num = new_number_int(1);
        return;
      }
    }
    return;
  }
  case EX_LOG_OR:
    walk_expr(expr->binop.lhs);
    bool lhs_is_false = false;
    if (expr->binop.lhs->ty == EX_NUM) {
      int lval;
      SET_NUMBER_VAL(lval, &expr->binop.lhs->num);
      if (lval != 0) {
        expr->ty = EX_NUM;
        expr->num = new_number_int(1);
        return;
      }
      lhs_is_false = true;
    }
    walk_expr(expr->binop.rhs);
    if (expr->binop.rhs->ty == EX_NUM) {
      int rval;
      SET_NUMBER_VAL(rval, &expr->binop.rhs->num);
      if (rval != 0) {
        expr->ty = EX_NUM;
        expr->num = new_number_int(1);
        return;
      }
      if (lhs_is_false) {
        expr->ty = EX_NUM;
        expr->num = new_number_int(0);
        return;
      }
    }
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
    walk_expr(expr->cond.cond);
    if (expr->cond.cond->ty == EX_NUM) {
      int cond;
      SET_NUMBER_VAL(cond, &expr->cond.cond->num);
      Expr *taken;
      if (cond != 0) {
        taken = expr->cond.then_expr;
      } else {
        taken = expr->cond.else_expr;
      }
      Range range = expr->range;
      walk_expr(taken);
      *expr = *taken;
      expr->range = range;
      return;
    }
    walk_expr(expr->cond.then_expr);
    walk_expr(expr->cond.else_expr);
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