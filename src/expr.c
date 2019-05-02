#include "gifcc.h"

static void cast_as(Expr **expr, Type *type);
static Type *integer_promoted(Expr **e);
static Type *arith_converted(Expr **e1, Expr **e2);
static Expr *coerce_array2ptr(Expr *expr);
static Expr *coerce_func2ptr(Expr *expr);
static Expr *coerce_expr2cond(Expr *expr);
static bool is_null_ptr_const(Expr *expr);
static noreturn void binop_type_error_raw(int ty, Expr *lhs, Expr *rhs,
                                          const char *dbg_file, int dbg_line);
static Expr *new_expr(int ty, Type *val_type, const Range *range);

static void cast_as(Expr **expr, Type *type) {
  *expr = new_expr_cast(type, (*expr), (*expr)->range);
}

static Type *integer_promoted(Expr **e) {
  if (!is_integer_type((*e)->val_type)) {
    return NULL;
  }
  // BOOL, S/UCHAR, (S_,U_)SHORT, ENUM は S_INT へ昇格する
  switch ((*e)->val_type->ty) {
  case TY_BOOL:
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_U_CHAR:
  case TY_S_SHORT:
  case TY_U_SHORT:
  case TY_ENUM:
    cast_as(e, new_type(TY_S_INT, EMPTY_TYPE_QUALIFIER));
    break;
  case TY_S_INT:
  case TY_U_INT:
  case TY_S_LONG:
  case TY_U_LONG:
  case TY_S_LLONG:
  case TY_U_LLONG:
  case TY_FLOAT:
  case TY_DOUBLE:
  case TY_LDOUBLE:
  case TY_VOID:
  case TY_PTR:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
  case TY_BUILTIN:
    break;
  }
  return (*e)->val_type;
}

static Type *arith_converted(Expr **e1, Expr **e2) {
  Type *ty1 = (*e1)->val_type;
  Type *ty2 = (*e2)->val_type;
  const Range *r1 = (*e1)->range;
  const Range *r2 = (*e2)->range;

  if (!is_arith_type(ty1) || !is_arith_type(ty2)) {
    return NULL;
  }

  if (is_float_type(ty1) || is_float_type(ty2)) {
    Type *type;
    if (ty1->ty == TY_LDOUBLE || ty2->ty == TY_LDOUBLE) {
      type = new_type(TY_LDOUBLE, EMPTY_TYPE_QUALIFIER);
    } else if (ty1->ty == TY_DOUBLE || ty2->ty == TY_DOUBLE) {
      type = new_type(TY_DOUBLE, EMPTY_TYPE_QUALIFIER);
    } else {
      assert(ty1->ty == TY_FLOAT || ty2->ty == TY_FLOAT);
      type = new_type(TY_FLOAT, EMPTY_TYPE_QUALIFIER);
    }
    cast_as(e1, type);
    cast_as(e2, type);
    return type;
  }

  ty1 = integer_promoted(e1);
  ty2 = integer_promoted(e2);

  bool is_signed1 = is_signed_int_type(ty1, r1);
  bool is_signed2 = is_signed_int_type(ty2, r2);
  int rank1 = get_int_type_rank(ty1, r1);
  int rank2 = get_int_type_rank(ty2, r2);

  if (rank1 < rank2) {
    // revert argument order
    return arith_converted(e2, e1);
  }

  if ((is_signed1 ^ is_signed2) == 0) {
    // both operands have signed type or both have unsigned type
    if (rank1 > rank2) {
      cast_as(e2, ty1);
      return ty1;
    }
    assert(is_same_type(ty1, ty2));
    return ty1;
  }

  if (!is_signed1) {
    // unsigned operand has rank greater or equal to another
    cast_as(e2, ty1);
    return ty1;
  }

  assert(is_signed1 && !is_signed2);
  if (get_val_size(ty1, r1) > get_val_size(ty2, r2)) {
    // signed type can represent all of the value of unsigned type
    cast_as(e2, ty1);
    return ty1;
  }

  Type *ty;
  switch (ty1->ty) {
  case TY_S_INT:
    ty = new_type(TY_U_INT, EMPTY_TYPE_QUALIFIER);
    break;
  case TY_S_LONG:
    ty = new_type(TY_U_LONG, EMPTY_TYPE_QUALIFIER);
    break;
  case TY_S_LLONG:
    ty = new_type(TY_U_LLONG, EMPTY_TYPE_QUALIFIER);
    break;
  default:
    range_internal_error(r1, "Invalid number type: %d", ty1->ty);
  }

  cast_as(e1, ty);
  cast_as(e2, ty);
  return ty;
}

static Expr *coerce_array2ptr(Expr *expr) {
  if (is_array_type(expr->val_type)) {
    return new_expr_unary(EX_ADDRESS, expr, expr->range);
  }
  return expr;
}
static Expr *coerce_func2ptr(Expr *expr) {
  if (is_func_type(expr->val_type)) {
    return new_expr_unary(EX_ADDRESS, expr, expr->range);
  }
  return expr;
}
static Expr *coerce_expr2cond(Expr *expr) {
  Expr *zero = new_expr_num(new_number_int(0), expr->range);
  cast_as(&zero, expr->val_type);
  return new_expr_binop(EX_NOTEQ, expr, zero, expr->range);
}

static bool is_null_ptr_const(Expr *expr) {
  if (!is_integer_type(expr->val_type)) {
    return false;
  }

  // evaluate constant expression
  sema_expr(expr);

  if (expr->ty != EX_NUM) {
    return false;
  }
  int n;
  SET_NUMBER_VAL(n, &expr->num);
  return n == 0;
}

#define binop_type_error(ty, lhs, rhs)                                         \
  binop_type_error_raw((ty), (lhs), (rhs), __FILE__, __LINE__)
static noreturn void binop_type_error_raw(int ty, Expr *lhs, Expr *rhs,
                                          const char *dbg_file, int dbg_line) {
  range_error_raw(range_join(lhs->range, rhs->range), dbg_file, dbg_line,
                  "不正な型の値に対する演算です: 演算=%d(%c), 左辺=%s, 右辺=%s",
                  ty, ty, format_type(lhs->val_type, false),
                  format_type(rhs->val_type, false));
}

static Expr *new_expr(int ty, Type *val_type, const Range *range) {
  Expr *expr = NEW(Expr);
  expr->ty = ty;
  expr->val_type = to_unqualified(val_type);
  expr->range = range;
  return expr;
}

Expr *new_expr_num(Number val, const Range *range) {
  Expr *expr =
      new_expr(EX_NUM, new_type(val.type, EMPTY_TYPE_QUALIFIER), range);
  expr->num = val;
  return expr;
}

Expr *new_expr_stack_var(StackVar *svar, const Range *range) {
  Expr *expr = new_expr(EX_STACK_VAR, svar->type, range);
  expr->stack_var.def = svar;
  expr->stack_var.offset = 0;
  return expr;
}

Expr *new_expr_global_var(Type *val_type, const char *name, GlobalVar *gvar,
                          const Range *range) {
  Expr *expr = new_expr(EX_GLOBAL_VAR, val_type, range);
  expr->global_var.name = name;
  expr->global_var.def = gvar;
  expr->global_var.offset = 0;
  return expr;
}

Expr *new_expr_builtin_func(const char *name, builtin_func_handler_t *handler,
                            const Range *range) {
  Type *type = new_type(TY_BUILTIN, EMPTY_TYPE_QUALIFIER);
  Expr *expr = new_expr(EX_BUILTIN_FUNC, type, range);
  expr->builtin_func.name = name;
  expr->builtin_func.handler = handler;
  return expr;
}

Expr *new_expr_str(StringLiteral *lit, const Range *range) {
  Type *type =
      new_type_array(new_type(TY_CHAR, EMPTY_TYPE_QUALIFIER),
                     new_number_int(strlen(lit->val)), EMPTY_TYPE_QUALIFIER);
  Expr *expr = new_expr(EX_STR, type, range);
  expr->str = lit;
  return expr;
}

Expr *new_expr_stmt(Stmt *stmt, const Range *range) {
  Expr *expr = new_expr(EX_STMT, stmt->val_type, range);
  expr->stmt = stmt;
  return expr;
}

Expr *new_expr_generic(Expr *control, GenericAssociationVector *assoc_list) {
  control = coerce_func2ptr(control);
  control = coerce_array2ptr(control);

  Expr *default_expr = NULL;
  VEC_FOREACH (GenericAssociation *assoc, assoc_list) {
    if (assoc->type == NULL) {
      if (default_expr != NULL) {
        range_error(assoc->range, "duplicate default generic association");
      }
      default_expr = assoc->expr;
      continue;
    }
    if (is_same_type(assoc->type, control->val_type)) {
      return assoc->expr;
    }
  }
  if (default_expr == NULL) {
    range_error(control->range,
                "controlling expression type '%s' not compatible with any "
                "generic association type",
                format_type(control->val_type, false));
  }
  return default_expr;
}

Expr *new_expr_call(Expr *callee, ExprVector *arguments, const Range *range) {
  if (callee->ty == EX_BUILTIN_FUNC) {
    return callee->builtin_func.handler(callee, arguments, range);
  }

  callee = coerce_array2ptr(callee);

  Type *func_type;
  Type *ret_type;
  if (is_func_type(callee->val_type)) {
    func_type = callee->val_type;
  } else if (is_ptr_type(callee->val_type) &&
             is_func_type(callee->val_type->ptr)) {
    func_type = callee->val_type->ptr;
  } else {
    range_warn(range, "未知の関数です");
    func_type = new_type_func(new_type(TY_S_INT, EMPTY_TYPE_QUALIFIER), NULL,
                              false, EMPTY_TYPE_QUALIFIER);
  }

  ret_type = func_type->func.ret;

  int narg = 0;
  if (arguments != NULL) {
    for (int i = 0; i < VEC_LEN(arguments); i++) {
      Expr *arg = VEC_GET(arguments, i);
      arg = coerce_array2ptr(arg);
      arg = coerce_func2ptr(arg);
      VEC_SET(arguments, i, arg);
    }
    narg = VEC_LEN(arguments);
  }

  ParamVector *params = func_type->func.params;
  int nparam = params != NULL ? VEC_LEN(params) : 0;
  if (params != NULL) {
    if ((narg < nparam) || (narg > nparam && !func_type->func.has_varargs)) {
      range_error(range,
                  "関数の引数の個数が一致しません: argument=%d, parameter=%d",
                  narg, nparam);
    }
  }

  for (int i = 0; i < nparam; i++) {
    Param *param = VEC_GET(params, i);
    Expr *arg = VEC_GET(arguments, i);
    cast_as(&arg, param->type);
    VEC_SET(arguments, i, arg);
  }
  // default argument promotion
  for (int i = nparam; i < narg; i++) {
    Expr *arg = VEC_GET(arguments, i);
    if (is_integer_type(arg->val_type)) {
      integer_promoted(&arg);
    } else if (arg->val_type->ty == TY_FLOAT) {
      cast_as(&arg, new_type(TY_DOUBLE, EMPTY_TYPE_QUALIFIER));
    } else {
      // do nothing
    }
    VEC_SET(arguments, i, arg);
  }

  Expr *expr = new_expr(EX_CALL, ret_type, range);
  expr->call.callee = callee;
  expr->call.arguments = arguments;
  return expr;
}

Expr *new_expr_builtin_va_start(Expr *callee __attribute__((unused)),
                                ExprVector *arguments, const Range *range) {
  int narg = arguments != NULL ? VEC_LEN(arguments) : 0;

  if (arguments == NULL || VEC_LEN(arguments) != 2) {
    range_error(range,
                "関数の引数の個数が一致しません: argument=%d, parameter=%d",
                narg, 2);
  }

  Expr *ap = VEC_GET(arguments, 0);
  Expr *last = VEC_GET(arguments, 1);

  ap = coerce_array2ptr(ap);
  ap = coerce_func2ptr(ap);

  Type *type = new_type(TY_VOID, EMPTY_TYPE_QUALIFIER);
  Expr *expr = new_expr(EX_BUILTIN_VA_START, type, range);
  expr->builtin_va_start.ap = ap;
  expr->builtin_va_start.last = last;
  return expr;
}

Expr *new_expr_builtin_va_arg(Expr *callee __attribute__((unused)),
                              ExprVector *arguments __attribute__((unused)),
                              const Range *range) {
  int narg = arguments != NULL ? VEC_LEN(arguments) : 0;

  if (arguments == NULL || VEC_LEN(arguments) != 2) {
    range_error(range,
                "関数の引数の個数が一致しません: argument=%d, parameter=%d",
                narg, 2);
  }

  Expr *ap = VEC_GET(arguments, 0);
  Expr *type_expr = VEC_GET(arguments, 1);

  ap = coerce_array2ptr(ap);
  ap = coerce_func2ptr(ap);

  if (!is_ptr_type(type_expr->val_type)) {
    range_error(range, "ポインタ型ではありません: %s",
                format_type(type_expr->val_type, false));
  }

  Type *type = type_expr->val_type->ptr;
  Expr *expr = new_expr(EX_BUILTIN_VA_ARG, type, range);
  expr->builtin_va_arg.ap = ap;
  return expr;
}

Expr *new_expr_builtin_va_end(Expr *callee __attribute__((unused)),
                              ExprVector *arguments __attribute__((unused)),
                              const Range *range) {
  int narg = arguments != NULL ? VEC_LEN(arguments) : 0;

  if (arguments == NULL || VEC_LEN(arguments) != 1) {
    range_error(range,
                "関数の引数の個数が一致しません: argument=%d, parameter=%d",
                narg, 2);
  }

  Expr *ap = VEC_GET(arguments, 0);

  ap = coerce_array2ptr(ap);
  ap = coerce_func2ptr(ap);

  Type *type = new_type(TY_VOID, EMPTY_TYPE_QUALIFIER);
  Expr *expr = new_expr(EX_BUILTIN_VA_END, type, range);
  expr->builtin_va_end.ap = ap;
  return expr;
}

Expr *new_expr_builtin_va_copy(Expr *callee __attribute__((unused)),
                               ExprVector *arguments __attribute__((unused)),
                               const Range *range) {
  int narg = arguments != NULL ? VEC_LEN(arguments) : 0;

  if (arguments == NULL || VEC_LEN(arguments) != 2) {
    range_error(range,
                "関数の引数の個数が一致しません: argument=%d, parameter=%d",
                narg, 2);
  }

  Expr *dest = VEC_GET(arguments, 0);
  Expr *src = VEC_GET(arguments, 1);

  dest = coerce_array2ptr(dest);
  dest = coerce_func2ptr(dest);

  src = coerce_array2ptr(src);
  src = coerce_func2ptr(src);

  Type *type = new_type(TY_VOID, EMPTY_TYPE_QUALIFIER);
  Expr *expr = new_expr(EX_BUILTIN_VA_COPY, type, range);
  Expr *idx = new_expr_num(new_number_int(0), expr->range);
  expr->builtin_va_copy.dest = new_expr_index(dest, idx, dest->range);
  expr->builtin_va_copy.src = new_expr_index(src, idx, src->range);
  return expr;
}

Expr *new_expr_postfix(int ty, Expr *operand, const Range *range) {
  operand = coerce_array2ptr(operand);
  operand = coerce_func2ptr(operand);

  Expr *expr = new_expr(ty, operand->val_type, range);
  expr->unop.operand = operand;
  return expr;
}

Expr *new_expr_cast(Type *val_type, Expr *operand, const Range *range) {
  operand = coerce_array2ptr(operand);
  operand = coerce_func2ptr(operand);

  Expr *expr = new_expr(EX_CAST, val_type, range);
  if (val_type->ty == TY_BOOL) {
    expr->unop.operand = coerce_expr2cond(operand);
  } else {
    expr->unop.operand = operand;
  }
  return expr;
}

Expr *new_expr_compound(Type *val_type, StackVar *svar, Initializer *init,
                        const Range *range) {
  Expr *expr = new_expr(EX_COMPOUND, val_type, range);
  expr->compound.stack_var = svar;
  expr->compound.init = init;
  return expr;
}

Expr *new_expr_unary(int op, Expr *operand, const Range *range) {
  if (op != EX_ADDRESS) {
    // & 以外は array, func は ptr とみなす
    operand = coerce_array2ptr(operand);
    operand = coerce_func2ptr(operand);
  }

  Type *val_type;
  switch (op) {
  case EX_PRE_INC:
  case EX_PRE_DEC: {
    val_type = operand->val_type;
    break;
  case EX_ADDRESS: {
    if (operand->ty == EX_STR) {
      operand->val_type = new_type_ptr(new_type(TY_CHAR, EMPTY_TYPE_QUALIFIER),
                                       EMPTY_TYPE_QUALIFIER);
      return operand;
    }
    if (is_array_type(operand->val_type)) {
      val_type =
          new_type_ptr(operand->val_type->array.elem, EMPTY_TYPE_QUALIFIER);
    } else {
      val_type = new_type_ptr(operand->val_type, EMPTY_TYPE_QUALIFIER);
    }

    break;
  }
  case EX_INDIRECT: {
    if (!is_ptr_type(operand->val_type)) {
      range_error(range, "ポインタ型でない値に対するデリファレンスです: %s",
                  format_type(operand->val_type, false));
    }
    if (operand->val_type->ptr->ty == TY_FUNC) {
      return operand;
    }
    val_type = operand->val_type->ptr;
    break;
  }
  case EX_PLUS:
  case EX_MINUS:
    if (!is_arith_type(operand->val_type)) {
      range_error(range,
                  "不正な型の値に対する演算です: 算術型ではありません: %s",
                  format_type(operand->val_type, false));
    }
    if (is_integer_type(operand->val_type)) {
      val_type = integer_promoted(&operand);
    } else {
      val_type = operand->val_type;
    }
    break;
  case EX_LOG_NOT: {
    Expr *zero = new_expr_num(new_number_int(0), operand->range);
    cast_as(&zero, operand->val_type);
    return new_expr_binop(EX_EQEQ, operand, zero, operand->range);
  }
  case EX_NOT:
    if (!is_integer_type(operand->val_type)) {
      range_error(range,
                  "不正な型の値に対する演算です: 整数型ではありません: %s",
                  format_type(operand->val_type, false));
    }
    val_type = integer_promoted(&operand);
    break;
  default:
    range_internal_error(range, "invalid unop type: %d", op);
  }
  }

  Expr *expr = new_expr(op, val_type, range);
  expr->unop.operand = operand;
  return expr;
}

Expr *new_expr_binop(int op, Expr *lhs, Expr *rhs, const Range *range) {
  lhs = coerce_array2ptr(lhs);
  lhs = coerce_func2ptr(lhs);
  rhs = coerce_array2ptr(rhs);
  rhs = coerce_func2ptr(rhs);

  Type *val_type;
  switch (op) {
  // multiplicative
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
    val_type = arith_converted(&lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(op, lhs, rhs);
    }
    break;
  // additive
  case EX_ADD:
    if (is_ptr_type(lhs->val_type)) {
      if (is_ptr_type(rhs->val_type) || !is_integer_type(rhs->val_type)) {
        binop_type_error(op, lhs, rhs);
      }

      // ptr + int => ptr + (size * int)
      Expr *size = new_expr_num(
          new_number_size_t(get_val_size(lhs->val_type->ptr, lhs->range)),
          range);
      rhs = new_expr_binop(EX_MUL, rhs, size, range);
      val_type = lhs->val_type;
      break;
    }

    if (is_ptr_type(rhs->val_type)) {
      if (!is_integer_type(lhs->val_type)) {
        binop_type_error(op, lhs, rhs);
      }

      // int + ptr => (size * int) + ptr
      Expr *size = new_expr_num(
          new_number_size_t(get_val_size(rhs->val_type->ptr, rhs->range)),
          range);
      lhs = new_expr_binop(EX_MUL, lhs, size, range);
      val_type = rhs->val_type;
      break;
    }

    // int + int
    val_type = arith_converted(&lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(op, lhs, rhs);
    }
    break;
  case EX_SUB:
    if (is_ptr_type(lhs->val_type)) {
      if (is_ptr_type(rhs->val_type)) {
        if (!is_same_type(lhs->val_type, rhs->val_type)) {
          binop_type_error(op, lhs, rhs);
        }

        // ptr - ptr
        Expr *sub =
            new_expr(EX_SUB, new_type(TY_S_LONG, EMPTY_TYPE_QUALIFIER), range);
        sub->binop.lhs = lhs;
        sub->binop.rhs = rhs;
        Expr *size = new_expr_num(
            new_number_ptrdiff_t(get_val_size(lhs->val_type->ptr, lhs->range)),
            range);
        return new_expr_binop(EX_DIV, sub, size, range);
      }

      if (is_integer_type(rhs->val_type)) {
        // int - int
        Expr *size = new_expr_num(
            new_number_size_t(get_val_size(lhs->val_type->ptr, lhs->range)),
            range);
        rhs = new_expr_binop(EX_MUL, rhs, size, range);
        val_type = lhs->val_type;
        break;
      }
      binop_type_error(op, lhs, rhs);
    }

    if (is_ptr_type(rhs->val_type)) {
      binop_type_error(op, lhs, rhs);
    }

    val_type = arith_converted(&lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(op, lhs, rhs);
    }
    break;
  // shift
  case EX_LSHIFT:
  case EX_RSHIFT:
    if (!is_integer_type(lhs->val_type) || !is_integer_type(rhs->val_type)) {
      binop_type_error(op, lhs, rhs);
    }
    val_type = integer_promoted(&lhs);
    if (val_type == NULL) {
      binop_type_error(op, lhs, rhs);
    }
    break;

  case EX_LT:
  case EX_GT:
  case EX_LTEQ:
  case EX_GTEQ:
  case EX_EQEQ:
  case EX_NOTEQ:
    arith_converted(&lhs, &rhs);
    val_type = new_type(TY_S_INT, EMPTY_TYPE_QUALIFIER);
    break;
  // and
  case EX_AND:
  case EX_XOR:
  case EX_OR:
    if (!is_integer_type(lhs->val_type) || !is_integer_type(rhs->val_type)) {
      binop_type_error(op, lhs, rhs);
    }
    val_type = arith_converted(&lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(op, lhs, rhs);
    }
    break;
  case EX_LOG_AND: {
    lhs = coerce_expr2cond(lhs);
    rhs = coerce_expr2cond(rhs);
    val_type = new_type(TY_S_INT, EMPTY_TYPE_QUALIFIER);
    break;
  }
  case EX_LOG_OR:
    lhs = coerce_expr2cond(lhs);
    rhs = coerce_expr2cond(rhs);
    val_type = new_type(TY_S_INT, EMPTY_TYPE_QUALIFIER);
    break;
  case EX_ASSIGN:
    cast_as(&rhs, lhs->val_type);
    val_type = lhs->val_type;
    break;
  case EX_MUL_ASSIGN:
    return new_expr_binop(EX_ASSIGN, lhs,
                          new_expr_binop(EX_MUL, lhs, rhs, range), range);
  case EX_DIV_ASSIGN:
    return new_expr_binop(EX_ASSIGN, lhs,
                          new_expr_binop(EX_DIV, lhs, rhs, range), range);
  case EX_MOD_ASSIGN:
    return new_expr_binop(EX_ASSIGN, lhs,
                          new_expr_binop(EX_MOD, lhs, rhs, range), range);
  case EX_ADD_ASSIGN:
    return new_expr_binop(EX_ASSIGN, lhs,
                          new_expr_binop(EX_ADD, lhs, rhs, range), range);
  case EX_SUB_ASSIGN:
    return new_expr_binop(EX_ASSIGN, lhs,
                          new_expr_binop(EX_SUB, lhs, rhs, range), range);
  case EX_LSHIFT_ASSIGN:
    return new_expr_binop(EX_ASSIGN, lhs,
                          new_expr_binop(EX_LSHIFT, lhs, rhs, range), range);
  case EX_RSHIFT_ASSIGN:
    return new_expr_binop(EX_ASSIGN, lhs,
                          new_expr_binop(EX_RSHIFT, lhs, rhs, range), range);
  case EX_AND_ASSIGN:
    return new_expr_binop(EX_ASSIGN, lhs,
                          new_expr_binop(EX_AND, lhs, rhs, range), range);
  case EX_XOR_ASSIGN:
    return new_expr_binop(EX_ASSIGN, lhs,
                          new_expr_binop(EX_XOR, lhs, rhs, range), range);
  case EX_OR_ASSIGN:
    return new_expr_binop(EX_ASSIGN, lhs,
                          new_expr_binop(EX_OR, lhs, rhs, range), range);
  case EX_COMMA: {
    if (lhs->ty == EX_COMMA && rhs->ty == EX_COMMA) {
      VEC_APPEND(lhs->comma.exprs, rhs->comma.exprs);
      lhs->range = range;
      lhs->val_type = rhs->val_type;
      return lhs;
    }
    if (lhs->ty == EX_COMMA) {
      VEC_PUSH(lhs->comma.exprs, rhs);
      lhs->range = range;
      lhs->val_type = rhs->val_type;
      return lhs;
    }
    if (rhs->ty == EX_COMMA) {
      VEC_INSERT(rhs->comma.exprs, 0, lhs);
      rhs->range = range;
      return rhs;
    }
    Expr *expr = new_expr(op, rhs->val_type, range);
    expr->comma.exprs = NEW_VECTOR(ExprVector);
    VEC_PUSH(expr->comma.exprs, lhs);
    VEC_PUSH(expr->comma.exprs, rhs);
    return expr;
  }
  default:
    range_internal_error(range, "invalid binop type: %d", op);
  }

  Expr *expr = new_expr(op, val_type, range);
  expr->binop.lhs = lhs;
  expr->binop.rhs = rhs;
  return expr;
}

Expr *new_expr_cond(Expr *cond, Expr *then_expr, Expr *else_expr,
                    const Range *range) {
  cond = coerce_array2ptr(cond);
  cond = coerce_func2ptr(cond);
  then_expr = coerce_array2ptr(then_expr);
  then_expr = coerce_func2ptr(then_expr);
  else_expr = coerce_array2ptr(else_expr);
  else_expr = coerce_func2ptr(else_expr);

  Type *val_type;
  if (is_arith_type(then_expr->val_type) &&
      is_arith_type(else_expr->val_type)) {
    val_type = arith_converted(&then_expr, &else_expr);
  } else if (is_ptr_type(then_expr->val_type) &&
             is_ptr_type(else_expr->val_type)) {
    if (then_expr->val_type->ptr->ty == TY_VOID) {
      val_type = else_expr->val_type;
    } else if (else_expr->val_type->ptr->ty == TY_VOID) {
      val_type = then_expr->val_type;
    } else {
      if (!is_same_type(then_expr->val_type, else_expr->val_type)) {
        range_error(range, "条件演算子の両辺の型が異なります: %s, %s",
                    format_type(then_expr->val_type, false),
                    format_type(else_expr->val_type, false));
      }
      val_type = then_expr->val_type;
    }
  } else if (is_ptr_type(then_expr->val_type) && is_null_ptr_const(else_expr)) {
    val_type = then_expr->val_type;
  } else if (is_ptr_type(else_expr->val_type) && is_null_ptr_const(then_expr)) {
    val_type = else_expr->val_type;
  } else if (then_expr->val_type->ty == TY_VOID ||
             else_expr->val_type->ty == TY_VOID) {
    // NonStandard/GNU?: conditional expressions with only one void side
    val_type = new_type(TY_VOID, EMPTY_TYPE_QUALIFIER);
    cast_as(&then_expr, val_type);
    cast_as(&else_expr, val_type);
  } else {
    if (!is_same_type(then_expr->val_type, else_expr->val_type)) {
      range_error(range, "条件演算子の両辺の型が異なります: %s, %s",
                  format_type(then_expr->val_type, false),
                  format_type(else_expr->val_type, false));
    }
    val_type = then_expr->val_type;
  }

  Expr *expr = new_expr(EX_COND, val_type, range);
  expr->cond.cond = cond;
  expr->cond.then_expr = then_expr;
  expr->cond.else_expr = else_expr;
  return expr;
}

Expr *new_expr_index(Expr *array, Expr *index, const Range *range) {
  array = coerce_array2ptr(array);
  array = coerce_func2ptr(array);
  index = coerce_array2ptr(index);
  index = coerce_func2ptr(index);

  return new_expr_unary(EX_INDIRECT,
                        new_expr_binop(EX_ADD, array, index, range), range);
}

Expr *new_expr_dot(Expr *operand, const char *name, const Range *range) {
  operand = coerce_array2ptr(operand);
  operand = coerce_func2ptr(operand);

  if (operand->val_type->ty != TY_STRUCT && operand->val_type->ty != TY_UNION) {
    range_error(range, "構造体または共用体以外のメンバへのアクセスです");
  }

  StructBody *body = operand->val_type->struct_body;
  Member *member =
      body->member_name_map ? map_get(body->member_name_map, name) : NULL;
  if (member == NULL) {
    range_error(range, "存在しないメンバへのアクセスです: %s %s",
                format_type(operand->val_type, false), name);
  }

  Expr *expr = new_expr(EX_DOT, member->type, range);
  expr->dot.operand = operand;
  expr->dot.members = NEW_VECTOR(MemberVector);
  VEC_PUSH(expr->dot.members, member);
  return expr;
}

Expr *new_expr_arrow(Expr *operand, const char *name, const Range *range) {
  operand = coerce_array2ptr(operand);
  operand = coerce_func2ptr(operand);
  if (operand->val_type->ty != TY_PTR ||
      (operand->val_type->ptr->ty != TY_STRUCT &&
       operand->val_type->ptr->ty != TY_UNION)) {
    range_error(range, "構造体または共用体以外のメンバへのアクセスです: %s",
                format_type(operand->val_type, false));
  }

  StructBody *body = operand->val_type->ptr->struct_body;
  Member *member =
      body->member_name_map ? map_get(body->member_name_map, name) : NULL;
  if (member == NULL) {
    range_error(range, "存在しないメンバへのアクセスです: %s %s",
                format_type(operand->val_type, false), name);
  }

  Expr *expr = new_expr(EX_ARROW, member->type, range);
  expr->arrow.operand = operand;
  expr->arrow.members = NEW_VECTOR(MemberVector);
  VEC_PUSH(expr->arrow.members, member);
  return expr;
}
