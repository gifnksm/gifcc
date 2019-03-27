#include "gifcc.h"

#include <string.h>

static void walk_stmt(Stmt *stmt);
static void walk_initializer(Initializer *init);

#define UNARYOP_PROLOGUE(op, r, a, range) switch ((op)) {
#define UNARYOP_BODY_INT(op, r, a, range)                                      \
  case EX_NOT:                                                                 \
    *(r) = (~(a));                                                             \
    break;
#define UNARYOP_BODY_ARITH(op, r, a, range)                                    \
  case EX_MINUS:                                                               \
    *(r) = (-(a));                                                             \
    break;
#define UNARYOP_EPILOGUE(op, r, a, range)                                      \
  default:                                                                     \
    range_error(range, "不正な演算です: %d", op);                              \
    break;                                                                     \
    }

#define UNARYOP_INT(...)                                                       \
  UNARYOP_PROLOGUE(__VA_ARGS__)                                                \
  UNARYOP_BODY_INT(__VA_ARGS__)                                                \
  UNARYOP_BODY_ARITH(__VA_ARGS__)                                              \
  UNARYOP_EPILOGUE(__VA_ARGS__)
#define UNARYOP_FLOAT(...)                                                     \
  UNARYOP_PROLOGUE(__VA_ARGS__)                                                \
  UNARYOP_BODY_ARITH(__VA_ARGS__)                                              \
  UNARYOP_EPILOGUE(__VA_ARGS__)

static void replace_expr(Expr *dest, Expr *src) {
  Type *type = dest->val_type;
  const Range *range = dest->range;
  *dest = *src;
  dest->val_type = type;
  dest->range = range;
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
    UNARYOP_INT(op, r, a, expr->range);
    return;
  }

  case TY_S_LONG: {
    signed long a, *r = &expr->num.s_long_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP_INT(op, r, a, expr->range);
    return;
  }

  case TY_S_LLONG: {
    signed long long a, *r = &expr->num.s_llong_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP_INT(op, r, a, expr->range);
    return;
  }

  case TY_U_INT: {
    unsigned int a, *r = &expr->num.u_int_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP_INT(op, r, a, expr->range);
    return;
  }

  case TY_U_LONG: {
    unsigned long a, *r = &expr->num.u_long_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP_INT(op, r, a, expr->range);
    return;
  }

  case TY_U_LLONG: {
    unsigned long long a, *r = &expr->num.u_llong_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP_INT(op, r, a, expr->range);
    return;
  }

  case TY_FLOAT: {
    float a, *r = &expr->num.float_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP_FLOAT(op, r, a, expr->range);
    return;
  }

  case TY_DOUBLE: {
    double a, *r = &expr->num.double_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP_FLOAT(op, r, a, expr->range);
    return;
  }

  case TY_LDOUBLE: {
    long double a, *r = &expr->num.ldouble_val;
    SET_NUMBER_VAL(a, &nop);
    UNARYOP_FLOAT(op, r, a, expr->range);
    return;
  }

  case TY_VOID:
  case TY_BOOL:
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
  case TY_BUILTIN:
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

  if (is_ptr_type(expr->val_type) && is_ptr_type(operand->val_type)) {
    replace_expr(expr, operand);
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
  case TY_BOOL:
    SET_NUMBER_VAL(exnum->bool_val, opnum);
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
  case TY_FLOAT:
    SET_NUMBER_VAL(exnum->float_val, opnum);
    return;
  case TY_DOUBLE:
    SET_NUMBER_VAL(exnum->double_val, opnum);
    return;
  case TY_LDOUBLE:
    SET_NUMBER_VAL(exnum->ldouble_val, opnum);
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
  case TY_BUILTIN:
    break;
  }
}

#define BINOP_PROLOGUE(op, r, a, b, range) switch ((op)) {
#define BINOP_BODY_INT(op, r, a, b, range)                                     \
  case EX_MOD:                                                                 \
    *(r) = ((a) % (b));                                                        \
    break;                                                                     \
  case EX_AND:                                                                 \
    *(r) = ((a) & (b));                                                        \
    break;                                                                     \
  case EX_XOR:                                                                 \
    *(r) = ((a) ^ (b));                                                        \
    break;                                                                     \
  case EX_OR:                                                                  \
    *(r) = ((a) | (b));                                                        \
    break;
#define BINOP_BODY_ARITH(op, r, a, b, range)                                   \
  case EX_MUL:                                                                 \
    *(r) = ((a) * (b));                                                        \
    break;                                                                     \
  case EX_DIV:                                                                 \
    if (b == 0) {                                                              \
      range_error(range, "ゼロ除算です");                                      \
    }                                                                          \
    *(r) = ((a) / (b));                                                        \
    break;                                                                     \
  case EX_ADD:                                                                 \
    *(r) = ((a) + (b));                                                        \
    break;                                                                     \
  case EX_SUB:                                                                 \
    *(r) = ((a) - (b));                                                        \
    break;
#define BINOP_EPILOGUE(op, r, a, b, range)                                     \
  default:                                                                     \
    range_error(range, "不正な演算です: %d", op);                              \
    break;                                                                     \
    }

#define BINOP_INT(...)                                                         \
  BINOP_PROLOGUE(__VA_ARGS__)                                                  \
  BINOP_BODY_INT(__VA_ARGS__)                                                  \
  BINOP_BODY_ARITH(__VA_ARGS__)                                                \
  BINOP_EPILOGUE(__VA_ARGS__)
#define BINOP_FLOAT(...)                                                       \
  BINOP_PROLOGUE(__VA_ARGS__)                                                  \
  BINOP_BODY_ARITH(__VA_ARGS__)                                                \
  BINOP_EPILOGUE(__VA_ARGS__)

static void eval_binop(Expr *expr) {
  Expr *lhs = expr->binop.lhs;
  Expr *rhs = expr->binop.rhs;
  if (expr->ty == EX_ADD || expr->ty == EX_SUB) {
    Expr *gvar = NULL;
    Expr *svar = NULL;
    Expr *offset = NULL;
    if (lhs->ty == EX_NUM && rhs->ty == EX_ADDRESS) {
      Expr *pointee = rhs->unop.operand;
      if (pointee->ty == EX_STACK_VAR) {
        svar = rhs;
        offset = lhs;
      } else if (pointee->ty == EX_GLOBAL_VAR) {
        gvar = rhs;
        offset = lhs;
      } else {
      }
    } else if (rhs->ty == EX_NUM && lhs->ty == EX_ADDRESS) {
      Expr *pointee = lhs->unop.operand;
      if (pointee->ty == EX_STACK_VAR) {
        svar = lhs;
        offset = rhs;
      } else if (pointee->ty == EX_GLOBAL_VAR) {
        gvar = lhs;
        offset = rhs;
      } else {
      }
    } else {
    }

    if (svar != NULL) {
      assert(svar->ty == EX_ADDRESS && svar->unop.operand->ty == EX_STACK_VAR);
      int val;
      SET_NUMBER_VAL(val, &offset->num);
      expr_t ex = expr->ty;
      replace_expr(expr, svar);
      if (ex == EX_ADD) {
        expr->unop.operand->stack_var.offset += val;
      } else {
        assert(ex == EX_SUB);
        expr->unop.operand->stack_var.offset -= val;
      }
      return;
    }

    if (gvar != NULL) {
      assert(gvar->ty == EX_ADDRESS && gvar->unop.operand->ty == EX_GLOBAL_VAR);
      int val;
      SET_NUMBER_VAL(val, &offset->num);
      expr_t ex = expr->ty;
      replace_expr(expr, gvar);
      if (ex == EX_ADD) {
        expr->unop.operand->global_var.offset += val;
      } else {
        assert(ex == EX_SUB);
        expr->unop.operand->global_var.offset -= val;
      }
      return;
    }
  }

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
    BINOP_INT(op, r, a, b, expr->range);
    return;
  }

  case TY_S_LONG: {
    signed long a, b, *r = &expr->num.s_long_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_INT(op, r, a, b, expr->range);
    return;
  }

  case TY_S_LLONG: {
    signed long long a, b, *r = &expr->num.s_llong_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_INT(op, r, a, b, expr->range);
    return;
  }

  case TY_U_INT: {
    unsigned int a, b, *r = &expr->num.u_int_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_INT(op, r, a, b, expr->range);
    return;
  }

  case TY_U_LONG: {
    unsigned long a, b, *r = &expr->num.u_long_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_INT(op, r, a, b, expr->range);
    return;
  }

  case TY_U_LLONG: {
    unsigned long long a, b, *r = &expr->num.u_llong_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_INT(op, r, a, b, expr->range);
    return;
  }

  case TY_FLOAT: {
    float a, b, *r = &expr->num.float_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_FLOAT(op, r, a, b, expr->range);
    return;
  }

  case TY_DOUBLE: {
    double a, b, *r = &expr->num.double_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_FLOAT(op, r, a, b, expr->range);
    return;
  }

  case TY_LDOUBLE: {
    long double a, b, *r = &expr->num.ldouble_val;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_FLOAT(op, r, a, b, expr->range);
    return;
  }

  case TY_VOID:
  case TY_BOOL:
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
  case TY_BUILTIN:
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
  range_assert(expr->range, lnum.type == rnum.type,
               "invalid number types: lhs=<%s:%d> rhs=<%s:%d>, op=%d",
               format_type(lhs->val_type, false), lnum.type,
               format_type(rhs->val_type, false), rnum.type, expr->ty);
  range_assert(expr->range, expr->val_type->ty == TY_S_INT,
               "expr type is not int: %s", format_type(expr->val_type, false));

  type_t type = lnum.type;
  expr_t op = expr->ty;

  expr->ty = EX_NUM;
  expr->num = (Number){.type = TY_S_INT};

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

  case TY_FLOAT: {
    float a, b;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_COMP(op, r, a, b, expr->range);
    return;
  }

  case TY_DOUBLE: {
    double a, b;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_COMP(op, r, a, b, expr->range);
    return;
  }

  case TY_LDOUBLE: {
    long double a, b;
    SET_NUMBER_VAL(a, &lnum);
    SET_NUMBER_VAL(b, &rnum);
    BINOP_COMP(op, r, a, b, expr->range);
    return;
  }

  case TY_VOID:
  case TY_BOOL:
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
  case TY_BUILTIN:
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
  case TY_BOOL:
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_S_SHORT:
  case TY_U_CHAR:
  case TY_U_SHORT:
  case TY_FLOAT:
  case TY_DOUBLE:
  case TY_LDOUBLE:
  case TY_PTR:
  case TY_ENUM:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
  case TY_BUILTIN:
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
  case EX_COMPOUND:
    walk_initializer(expr->compound.init);
    return;
  case EX_STMT:
    walk_stmt(expr->stmt);
    return;

  case EX_PRE_INC:
  case EX_PRE_DEC:
  case EX_INDIRECT:
  case EX_ADDRESS:
    walk_expr(expr->unop.operand);
    return;
  case EX_PLUS: {
    Expr *operand = expr->unop.operand;
    walk_expr(operand);
    replace_expr(expr, operand);
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
  case EX_DOT:
    walk_expr(expr->dot.operand);
    if (expr->dot.operand->ty == EX_GLOBAL_VAR) {
      Member *member = expr->dot.member;
      replace_expr(expr, expr->dot.operand);
      expr->global_var.offset += member->offset;
      return;
    }
    if (expr->dot.operand->ty == EX_STACK_VAR) {
      Member *member = expr->dot.member;
      replace_expr(expr, expr->dot.operand);
      expr->stack_var.offset += member->offset;
      return;
    }
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
  case EX_MUL_ASSIGN:
  case EX_DIV_ASSIGN:
  case EX_MOD_ASSIGN:
  case EX_ADD_ASSIGN:
  case EX_SUB_ASSIGN:
  case EX_LSHIFT_ASSIGN:
  case EX_RSHIFT_ASSIGN:
  case EX_AND_ASSIGN:
  case EX_XOR_ASSIGN:
  case EX_OR_ASSIGN:
    assert(false);
    return;
  case EX_COMMA:
    for (int i = 0; i < vec_len(expr->comma.exprs); i++) {
      Expr *op = vec_get(expr->comma.exprs, i);
      walk_expr(op);
    }
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
      walk_expr(taken);
      replace_expr(expr, taken);
      return;
    }
    walk_expr(expr->cond.then_expr);
    walk_expr(expr->cond.else_expr);
    return;
  case EX_BUILTIN_FUNC:
    return;

  case EX_BUILTIN_VA_START:
    walk_expr(expr->builtin_va_start.ap);
    walk_expr(expr->builtin_va_start.last);
    return;
  case EX_BUILTIN_VA_ARG:
    walk_expr(expr->builtin_va_arg.ap);
    return;
  case EX_BUILTIN_VA_END:
    walk_expr(expr->builtin_va_end.ap);
    return;
  case EX_BUILTIN_VA_COPY:
    walk_expr(expr->builtin_va_copy.dest);
    walk_expr(expr->builtin_va_copy.src);
    return;
  }
  range_error(expr->range, "Unknown expr type %d", expr->ty);
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
  case ST_DECL:
    for (int i = 0; i < vec_len(stmt->decl); i++) {
      StackVarDecl *decl = vec_get(stmt->decl, i);
      walk_initializer(decl->init);
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
    walk_stmt(stmt->body);
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
      walk_stmt(stmt->init);
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
    for (int i = 0; i < vec_len(init->members); i++) {
      MemberInitializer *meminit = vec_get(init->members, i);
      walk_initializer(meminit->init);
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
