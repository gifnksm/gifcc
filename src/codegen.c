#include "gifcc.h"
#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#define INVALID_STACK_POS INT_MIN

typedef enum {
  ARG_CLASS_MEMORY,
  ARG_CLASS_INTEGER,
  ARG_CLASS_SSE,
  ARG_CLASS_X87,
} arg_class_t;

typedef struct {
  int size;
  const char *ptr;
  const char *rax;
  const char *rdi;
  const char *rsi;
  const char *rdx;
  const char *rcx;
  const char *r8;
  const char *r9;
  const char *r10;
  const char *r11;
} Reg;

const Reg Reg8 = {
    .size = 8,
    .ptr = "QWORD PTR",
    .rax = "rax",
    .rdi = "rdi",
    .rsi = "rsi",
    .rdx = "rdx",
    .rcx = "rcx",
    .r8 = "r8",
    .r9 = "r9",
    .r10 = "r10",
    .r11 = "r11",
};
const Reg Reg4 = {
    .size = 4,
    .ptr = "DWORD PTR",
    .rax = "eax",
    .rdi = "edi",
    .rsi = "esi",
    .rdx = "edx",
    .rcx = "ecx",
    .r8 = "r8d",
    .r9 = "r9d",
    .r10 = "r10d",
    .r11 = "r11d",
};
const Reg Reg2 = {
    .size = 2,
    .ptr = "WORD PTR",
    .rax = "ax",
    .rdi = "di",
    .rsi = "si",
    .rdx = "dx",
    .rcx = "cx",
    .r8 = "r8w",
    .r9 = "r9w",
    .r10 = "r10w",
    .r11 = "r11w",
};
const Reg Reg1 = {
    .size = 1,
    .ptr = "BYTE PTR",
    .rax = "al",
    .rdi = "dil",
    .rsi = "sil",
    .rdx = "dl",
    .rcx = "cl",
    .r8 = "r8b",
    .r9 = "r9b",
    .r10 = "r10b",
    .r11 = "r11b",
};

typedef struct {
  const char *ptr;
  const char *add;
  const char *sub;
  const char *mul;
  const char *div;
  const char * xor ;
  const char *comi;
  const char *cvtt_to_si;
  const char *cvt_to_ss;
  const char *cvt_to_sd;
  const char *cvtt_from_si;
} SseOp;

const SseOp SseOpSS = {
    .ptr = "DWORD PTR",
    .add = "addss",
    .sub = "subss",
    .mul = "mulss",
    .div = "divss",
    .xor = "xorps",
    .comi = "comiss",
    .cvtt_to_si = "cvttss2si",
    .cvt_to_ss = NULL,
    .cvt_to_sd = "cvtss2sd",
    .cvtt_from_si = "cvtsi2ss",
};
const SseOp SseOpSD = {
    .ptr = "QWORD PTR",
    .add = "addsd",
    .sub = "subsd",
    .mul = "mulsd",
    .div = "divsd",
    .xor = "xorpd",
    .comi = "comisd",
    .cvtt_to_si = "cvttsd2si",
    .cvt_to_ss = "cvtsd2ss",
    .cvt_to_sd = NULL,
    .cvtt_from_si = "cvtsi2sd",
};

const int NUM_INT_REG = 6;
const int NUM_SSE_REG = 8;

static FILE *output_fp = NULL;
static char *epilogue_label = NULL;
static Function *func_ctxt = NULL;
static Vector *break_labels = NULL;
static Vector *continue_labels = NULL;
static int stack_pos = INVALID_STACK_POS;
static int retval_pos = INVALID_STACK_POS;
static int reg_save_area_pos = INVALID_STACK_POS;
static int reg_gp_offset = INVALID_STACK_POS;
static int reg_fp_offset = INVALID_STACK_POS;
static int overflow_arg_area_offset = INVALID_STACK_POS;

static __attribute__((format(printf, 1, 2))) void emit(const char *fmt, ...);
static void emit_assign(Type *type, const Range *range, Expr *dest, Expr *src);
static void emit_expr(Expr *expr);
static arg_class_t classify_arg_type(const Type *type, const Range *range,
                                     int *num_int, int *num_sse);
static IntVector *classify_arg(const Vector *args, int int_reg_idx);
static void emit_expr_num(Expr *expr);
static void emit_expr_stack_var(Expr *expr);
static void emit_expr_global_var(Expr *expr);
static void emit_expr_str(Expr *expr);
static void emit_expr_compound(Expr *expr);
static void emit_expr_stmt(Expr *expr);
static void emit_expr_call(Expr *expr);
static void emit_expr_dot(Expr *expr);
static void emit_expr_comma(Expr *expr);
static void emit_expr_log_and(Expr *expr);
static void emit_expr_log_or(Expr *expr);
static void emit_expr_cond(Expr *expr);
static void emit_expr_indirect(Expr *expr);
static void emit_expr_minus(Expr *expr);
static void emit_expr_not(Expr *expr);
static void emit_expr_cast(Expr *expr);
static void emit_expr_pre_inc(Expr *expr);
static void emit_expr_pre_dec(Expr *expr);
static void emit_expr_post_inc(Expr *expr);
static void emit_expr_post_dec(Expr *expr);
static void emit_expr_builtin_va_start(Expr *expr);
static void emit_expr_builtin_va_arg(Expr *expr);
static void emit_expr_builtin_va_end(Expr *expr);
static void emit_expr_builtin_va_copy(Expr *expr);
static void emit_expr_binop(Expr *expr);
static void emit_expr_binop_int(Expr *expr);
static void emit_expr_binop_sse(Expr *expr);
static void emit_expr_binop_x87(Expr *expr);
static void emit_stmt(Stmt *stmt, bool leave_value);
static void emit_svar_zero(StackVar *svar);
static void emit_svar_init(StackVar *svar, int offset, Initializer *init,
                           const Range *range);
static void emit_gvar_init(Initializer *init, const Range *range,
                           Vector *gvar_list);
static void emit_gvar(GlobalVar *gvar, Vector *gvar_list);

static bool is_int_reg_type(Type *type) {
  return is_ptr_type(type) || is_integer_type(type);
}
static bool is_sse_reg_type(Type *type) {
  return type->ty == TY_FLOAT || type->ty == TY_DOUBLE;
}
static bool is_x87_reg_type(Type *type) { return type->ty == TY_LDOUBLE; }

static const Reg *get_int_reg_for_copy(int size) {
  assert(size > 0);
  if (size >= 8) {
    return &Reg8;
  }
  if (size >= 4) {
    return &Reg4;
  }
  if (size >= 2) {
    return &Reg2;
  }
  assert(size == 1);
  return &Reg1;
}

static const Reg *get_int_reg_for_size(int size, const Range *range) {
  switch (size) {
  case 8:
    return &Reg8;
  case 4:
    return &Reg4;
  case 2:
    return &Reg2;
  case 1:
    return &Reg1;
  default:
    range_error(range, "サポートしていない型サイズです");
  }
}

static const Reg *get_int_reg(Type *type, const Range *range) {
  assert(is_int_reg_type(type));
  return get_int_reg_for_size(get_val_size(type, range), range);
}

static const char *get_int_arg_reg(const Reg *r, int idx) {
  switch (idx) {
  case 0:
    return r->rdi;
  case 1:
    return r->rsi;
  case 2:
    return r->rdx;
  case 3:
    return r->rcx;
  case 4:
    return r->r8;
  case 5:
    return r->r9;
  default:
    break;
  }
  assert(false);
}

static const SseOp *get_sse_op(Type *type, const Range *range) {
  switch (type->ty) {
  case TY_FLOAT:
    return &SseOpSS;
  case TY_DOUBLE:
    return &SseOpSD;
  default:
    range_error(range, "不正な型です: %s", format_type(type, false));
  }
}

char *make_label(const char *s) {
  static int count = 0;
  return format(".L.%s.%d", s, count++);
}

static char *num2str(Number num, const Range *range) {
  switch (num.type) {
  case TY_BOOL:
    return format("%d", num.bool_val);
  case TY_CHAR:
    return format("0x%02hhx", num.char_val);
  case TY_S_CHAR:
    return format("0x%02hhx", num.s_char_val);
  case TY_S_SHORT:
    return format("0x%04hx", num.s_short_val);
  case TY_S_INT:
    return format("0x%08x", num.s_int_val);
  case TY_S_LONG:
    return format("0x%016lx", num.s_long_val);
  case TY_S_LLONG:
    return format("0x%016llx", num.s_llong_val);
  case TY_U_CHAR:
    return format("0x%02hhx", num.u_char_val);
  case TY_U_SHORT:
    return format("0x%04hx", num.u_short_val);
  case TY_U_INT:
  case TY_FLOAT:
    return format("0x%08x", num.u_int_val);
  case TY_DOUBLE:
  case TY_U_LONG:
    return format("0x%016lx", num.u_long_val);
  case TY_U_LLONG:
    return format("0x%016llx", num.u_llong_val);
  case TY_PTR:
    return format("%" PRIdPTR, num.ptr_val);
  case TY_ENUM:
    return format("0x%08x", num.enum_val);
  case TY_VOID:
    return format("0");
  case TY_LDOUBLE:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
  case TY_BUILTIN:
    break;
  }
  range_error(range, "不正な型の数値です: %d", num.type);
}

static int get_incdec_size(Expr *expr) {
  if (is_ptr_type(expr->val_type)) {
    return get_val_size(expr->val_type->ptr, expr->range);
  }
  return 1;
}

static __attribute__((format(printf, 1, 2))) void emit(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_fp, fmt, ap);
  fprintf(output_fp, "\n");
  va_end(ap);
}

static void emit_stack_sub(int size) {
  stack_pos += size;
  assert(stack_pos >= 0);
  emit("  sub rsp, %d \t# rsp = rbp - %d", size, stack_pos);
}
static void emit_stack_add(int size) {
  stack_pos -= size;
  assert(stack_pos >= 0);
  emit("  add rsp, %d \t# rsp = rbp - %d", size, stack_pos);
}

static void emit_pop(const char *operand) {
  stack_pos -= 8;
  assert(stack_pos >= 0);
  emit("  pop %s \t# rsp = rbp - %d", operand, stack_pos);
}
static void emit_push(const char *operand) {
  stack_pos += 8;
  assert(stack_pos >= 0);
  emit("  push %s \t# rsp = rbp - %d", operand, stack_pos);
}

static void emit_pop_xmm(int n) {
  emit("  movsd xmm%d, [rsp]", n);
  emit_stack_add(8);
}

static void emit_push_xmm(int n) {
  emit_stack_sub(8);
  emit("  movsd [rsp], xmm%d", n);
}

static void emit_push_stack_var(StackVar *svar) {
  int size = get_val_size(svar->type, svar->range);
  emit_stack_sub(align(size, 8));

  int src_offset = svar->offset;
  int copy_size = 0;
  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    emit("  mov %s, [rbp - %d]", r->rax, src_offset - copy_size);
    emit("  mov [rsp + %d], %s", copy_size, r->rax);
    copy_size += r->size;
  }
}

static void emit_lval(Expr *expr) {
  if (expr->ty == EX_STACK_VAR) {
    StackVar *var = expr->stack_var;
    assert(var != NULL);
    emit("  lea rax, [rbp - %d]", var->offset);
    emit_push("rax");
    return;
  }
  if (expr->ty == EX_GLOBAL_VAR) {
    emit("  lea rax, %s[rip]", expr->global_var.name);
    emit_push("rax");
    return;
  }
  if (expr->ty == EX_INDIRECT) {
    emit_expr(expr->unop.operand);
    return;
  }
  if (expr->ty == EX_DOT) {
    emit_lval(expr->dot.operand);
    emit_pop("rax");
    emit("  lea rax, [rax + %d]", expr->dot.member->offset);
    emit_push("rax");
    return;
  }
  if (expr->ty == EX_COMMA) {
    Vector *exprs = expr->comma.exprs;
    for (int i = 0; i < vec_len(exprs); i++) {
      Expr *op = vec_get(exprs, i);
      if (i != vec_len(exprs) - 1) {
        emit_expr(op);
        int op_size = get_val_size(op->val_type, op->range);
        emit_stack_add(align(op_size, 8));
        continue;
      }
      emit_lval(op);
    }
    return;
  }
  if (expr->ty == EX_COMPOUND) {
    StackVar *svar = expr->compound.stack_var;
    Initializer *init = expr->compound.init;
    emit_svar_zero(svar);
    emit_svar_init(svar, 0, init, svar->range);
    emit("  lea rax, [rbp - %d]", svar->offset);
    emit_push("rax");
    return;
  }

  range_error(expr->range, "Invalid lvalue: %d", expr->ty);
}

static void emit_assign(Type *type, const Range *range, Expr *dest, Expr *src) {
  emit_expr(src);
  emit_lval(dest);
  emit_pop("rax");

  int size = get_val_size(type, range);
  int copy_size = 0;
  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    emit("  mov %s, [rsp + %d]", r->rdi, copy_size);
    emit("  mov [rax + %d], %s", copy_size, r->rdi);
    copy_size += r->size;
  }
}

static void emit_expr(Expr *expr) {
  switch (expr->ty) {
  case EX_NUM:
    emit_expr_num(expr);
    return;
  case EX_STACK_VAR:
    emit_expr_stack_var(expr);
    return;
  case EX_GLOBAL_VAR:
    emit_expr_global_var(expr);
    return;
  case EX_STR:
    emit_expr_str(expr);
    return;
  case EX_COMPOUND:
    emit_expr_compound(expr);
    return;
  case EX_STMT:
    emit_expr_stmt(expr);
    return;

  case EX_CALL:
    emit_expr_call(expr);
    return;
  case EX_ASSIGN:
    emit_assign(expr->val_type, expr->range, expr->binop.lhs, expr->binop.rhs);
    return;
  case EX_DOT:
    emit_expr_dot(expr);
    return;
  case EX_COMMA:
    emit_expr_comma(expr);
    return;
  case EX_LOG_AND:
    emit_expr_log_and(expr);
    return;
  case EX_LOG_OR:
    emit_expr_log_or(expr);
    return;
  case EX_COND:
    emit_expr_cond(expr);
    return;
  case EX_ADDRESS:
    emit_lval(expr->unop.operand);
    return;
  case EX_INDIRECT:
    emit_expr_indirect(expr);
    return;
  case EX_MINUS:
    emit_expr_minus(expr);
    return;
  case EX_NOT:
    emit_expr_not(expr);
    return;
  case EX_CAST:
    emit_expr_cast(expr);
    return;
  case EX_PRE_INC:
    emit_expr_pre_inc(expr);
    return;
  case EX_PRE_DEC:
    emit_expr_pre_dec(expr);
    return;
  case EX_POST_INC:
    emit_expr_post_inc(expr);
    return;
  case EX_POST_DEC:
    emit_expr_post_dec(expr);
    return;
  case EX_BUILTIN_VA_START:
    emit_expr_builtin_va_start(expr);
    return;
  case EX_BUILTIN_VA_ARG:
    emit_expr_builtin_va_arg(expr);
    return;
  case EX_BUILTIN_VA_END:
    emit_expr_builtin_va_end(expr);
    return;
  case EX_BUILTIN_VA_COPY:
    emit_expr_builtin_va_copy(expr);
    return;
  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
  case EX_EQEQ:
  case EX_NOTEQ:
  case EX_LT:
  case EX_GT:
  case EX_LTEQ:
  case EX_GTEQ:
  case EX_LSHIFT:
  case EX_RSHIFT:
  case EX_AND:
  case EX_XOR:
  case EX_OR:
    emit_expr_binop(expr);
    return;
  case EX_PLUS:
  case EX_LOG_NOT:
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
  case EX_BUILTIN_FUNC:
    break;
  }
  range_internal_error(expr->range, "Invalid expr type: %d", expr->ty);
}

static arg_class_t classify_arg_type(const Type *type, const Range *range,
                                     int *num_int, int *num_sse) {
  switch (type->ty) {
  case TY_BOOL:
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_S_SHORT:
  case TY_S_INT:
  case TY_S_LONG:
  case TY_S_LLONG:
  case TY_U_CHAR:
  case TY_U_SHORT:
  case TY_U_INT:
  case TY_U_LONG:
  case TY_U_LLONG:
  case TY_PTR:
  case TY_ENUM:
    if (*num_int + 1 <= NUM_INT_REG) {
      (*num_int)++;
      return ARG_CLASS_INTEGER;
    }
    return ARG_CLASS_MEMORY;
  case TY_ARRAY:
  case TY_STRUCT:
  case TY_UNION: {
    int size = get_val_size(type, range);
    if (size > 16) {
      return ARG_CLASS_MEMORY;
    }
    if (size > 8 && *num_int + 2 <= NUM_INT_REG) {
      (*num_int) += 2;
      return ARG_CLASS_INTEGER;
    }
    if (size <= 8 && *num_int + 1 <= NUM_INT_REG) {
      (*num_int)++;
      return ARG_CLASS_INTEGER;
    }
    return ARG_CLASS_MEMORY;

    break;
  }
  case TY_FLOAT:
  case TY_DOUBLE:
    if (*num_sse < NUM_SSE_REG) {
      (*num_sse)++;
      return ARG_CLASS_SSE;
    }
    return ARG_CLASS_MEMORY;
  case TY_LDOUBLE:
    return ARG_CLASS_X87;
  case TY_FUNC:
  case TY_VOID:
  case TY_BUILTIN:
    break;
  }
  range_error(range, "不正な型の引数です: %s", format_type(type, false));
}

static IntVector *classify_arg(const Vector *args, int int_reg_idx) {
  IntVector *class = new_int_vector();
  int num_int_reg = int_reg_idx;
  int num_sse_reg = 0;
  for (int i = 0; i < vec_len(args); i++) {
    Expr *expr = vec_get(args, i);
    int_vec_push(class, classify_arg_type(expr->val_type, expr->range,
                                          &num_int_reg, &num_sse_reg));
  }
  return class;
}

static void emit_expr_num(Expr *expr) {
  assert(expr->ty == EX_NUM);
  if (expr->val_type->ty == TY_VOID) {
    return;
  }

  int size = get_val_size(expr->val_type, expr->range);
  if (size < 4) {
    emit_push(num2str(expr->num, expr->range));
  } else if (size <= 8) {
    const Reg *r = get_int_reg_for_size(size, expr->range);
    emit("  mov %s, %s", r->rax, num2str(expr->num, expr->range));
    emit_push("rax");
  } else {
    assert(is_x87_reg_type(expr->val_type));
    assert(size == 16);
    emit("  mov rax, 0x%016lx", expr->num.bytes[0]);
    emit("  mov [rsp - 16],  rax");
    emit("  mov rax, 0x%016lx", expr->num.bytes[1]);
    emit("  mov [rsp - 8],  rax");
    emit_stack_sub(16);
  }
}

static void emit_expr_stack_var(Expr *expr) {
  assert(expr->ty == EX_STACK_VAR);
  emit_push_stack_var(expr->stack_var);
}

static void emit_expr_global_var(Expr *expr) {
  assert(expr->ty == EX_GLOBAL_VAR);

  const char *name = expr->global_var.name;
  int size = get_val_size(expr->val_type, expr->range);
  emit_stack_sub(align(size, 8));

  int copy_size = 0;

  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    emit("  mov %s, %s[rip + %d]", r->rax, name, copy_size);
    emit("  mov [rsp + %d], %s", copy_size, r->rax);
    copy_size += r->size;
  }
}

static void emit_expr_str(Expr *expr) {
  assert(expr->ty == EX_STR);

  emit("  lea rax, %s[rip]", expr->str->name);
  emit_push("rax");
}

static void emit_expr_compound(Expr *expr) {
  assert(expr->ty == EX_COMPOUND);
  assert(expr->compound.stack_var != NULL);

  StackVar *svar = expr->compound.stack_var;
  Initializer *init = expr->compound.init;
  emit_svar_zero(svar);
  emit_svar_init(svar, 0, init, svar->range);

  emit_push_stack_var(svar);
}

static void emit_expr_stmt(Expr *expr) {
  assert(expr->ty == EX_STMT);

  emit_stmt(expr->stmt, true);
}

static void emit_expr_call(Expr *expr) {
  assert(expr->ty == EX_CALL);

  bool call_direct;
  Type *functype;
  Type *ret_type;
  if (expr->call.callee->val_type->ty == TY_FUNC) {
    call_direct = true;
    functype = expr->call.callee->val_type;
    ret_type = functype->func.ret;
  } else if (is_ptr_type(expr->call.callee->val_type) &&
             is_func_type(expr->call.callee->val_type->ptr)) {
    call_direct = false;
    functype = expr->call.callee->val_type->ptr;
    ret_type = functype->func.ret;
  } else {
    range_error(expr->call.callee->range,
                "関数または関数ポインタ以外を呼び出そうとしました: %s",
                format_type(expr->call.callee->val_type, false));
  }

  int num_vararg_sse_reg = 0;
  int int_reg_idx = 0;
  int sse_reg_idx = 0;
  int ret_size;
  arg_class_t ret_class;
  if (ret_type->ty != TY_VOID) {
    ret_size = get_val_size(ret_type, expr->range);
    int num_int_reg = 0;
    int num_sse_reg = 0;
    ret_class =
        classify_arg_type(ret_type, expr->range, &num_int_reg, &num_sse_reg);
    if (ret_class == ARG_CLASS_MEMORY) {
      // 戻り値をメモリで返す場合は、格納先の領域を獲得しておく
      emit_stack_sub(align(ret_size, 8));
      int_reg_idx++;
    }
  } else {
    ret_size = 0;
    ret_class = ARG_CLASS_INTEGER;
  }
  Vector *arg = expr->call.argument;
  IntVector *arg_class = arg != NULL ? classify_arg(arg, int_reg_idx) : NULL;

  int arg_size = 0;
  if (arg != NULL && vec_len(arg) > 0) {
    for (int i = vec_len(arg) - 1; i >= 0; i--) {
      Expr *expr = vec_get(arg, i);
      int size = get_val_size(expr->val_type, expr->range);
      arg_class_t class = int_vec_get(arg_class, i);
      if (class == ARG_CLASS_MEMORY || class == ARG_CLASS_X87) {
        arg_size += align(size, 8);
      }
    }
  }

  // 呼び出される関数の rbp が 16 byte 境界に整列するようにする
  if ((stack_pos + arg_size) % 16 != 0) {
    assert((stack_pos + arg_size) % 16 == 8);
    arg_size += 8;
    emit_stack_sub(8);
  }

  if (arg != NULL && vec_len(arg) > 0) {
    // メモリ渡しする引数をスタックに積む
    for (int i = vec_len(arg) - 1; i >= 0; i--) {
      arg_class_t class = int_vec_get(arg_class, i);
      if (class == ARG_CLASS_MEMORY || class == ARG_CLASS_X87) {
        emit_expr(vec_get(arg, i));
      }
    }
    // レジスタ渡しする引数をスタックに積む
    for (int i = vec_len(arg) - 1; i >= 0; i--) {
      arg_class_t class = int_vec_get(arg_class, i);
      switch (class) {
      case ARG_CLASS_X87:
      case ARG_CLASS_MEMORY:
        continue;
      case ARG_CLASS_INTEGER:
        emit_expr(vec_get(arg, i));
        continue;
      case ARG_CLASS_SSE:
        emit_expr(vec_get(arg, i));
        continue;
      }
      assert(false);
    }
  }

  if (!call_direct) {
    emit_expr(expr->call.callee);
    emit_pop("r10");
  }

  if (arg && vec_len(arg) > 0) {
    // レジスタ渡しする引数をpopしレジスタにセットする
    for (int i = 0; i < vec_len(arg); i++) {
      Expr *expr = vec_get(arg, i);
      int size = get_val_size(expr->val_type, expr->range);

      arg_class_t class = int_vec_get(arg_class, i);
      switch (class) {
      case ARG_CLASS_X87:
      case ARG_CLASS_MEMORY:
        continue;

      case ARG_CLASS_INTEGER: {
        int num_reg = (size + 7) / 8;
        for (int j = 0; j < num_reg; j++) {
          emit_pop(get_int_arg_reg(&Reg8, int_reg_idx));
          int_reg_idx++;
        }
        continue;
      }
      case ARG_CLASS_SSE: {
        assert(size <= 8);
        emit_pop_xmm(sse_reg_idx);
        sse_reg_idx++;
        if (functype->func.has_varargs && i >= vec_len(functype->func.param)) {
          num_vararg_sse_reg++;
        }
        continue;
      }
      }
      assert(false);
    }
  }
  if (ret_class == ARG_CLASS_MEMORY) {
    // rdiには戻り値の格納先を設定
    emit("  lea rdi, [rsp + %d]", arg_size);
  }
  emit("  mov al, %d", num_vararg_sse_reg);

  range_assert(expr->range, stack_pos % 16 == 0, "stack position mismatch");
  if (call_direct) {
    emit("  call %s", expr->call.callee->global_var.name);
  } else {
    emit("  call r10");
  }
  if (arg_size > 0) {
    emit_stack_add(arg_size);
  }
  if (ret_type->ty != TY_VOID) {
    switch (ret_class) {
    case ARG_CLASS_MEMORY:
      // スタックのトップに戻り値が設定されているはずなので何もしなくて良い
      break;
    case ARG_CLASS_INTEGER:
      if (ret_size > 8) {
        emit_push("rdx");
      }
      emit_push("rax");
      break;
    case ARG_CLASS_SSE:
      assert(ret_size <= 8);
      emit_push_xmm(0);
      break;
    case ARG_CLASS_X87:
      emit_stack_sub(16);
      emit("  fstp TBYTE PTR[rsp]");
      break;
    }
  }
  return;
}

static void emit_expr_dot(Expr *expr) {
  assert(expr->ty == EX_DOT);

  Expr *operand = expr->dot.operand;
  Member *member = expr->dot.member;
  int size = get_val_size(operand->val_type, operand->range);
  int offset = member->offset;
  int mem_size = get_val_size(member->type, expr->range);
  int size_diff = align(size, 8) - align(mem_size, 8);
  emit_expr(expr->dot.operand);

  int copy_size = 0;
  while (mem_size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(mem_size - copy_size);
    emit("  mov %s, [rsp + %d]", r->rax, offset + copy_size);
    emit("  mov [rsp - %d], %s", align(mem_size, 8) - copy_size, r->rax);
    copy_size += r->size;
  }
  copy_size = 0;
  while (mem_size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(mem_size - copy_size);
    emit("  mov %s, [rsp - %d]", r->rax, align(mem_size, 8) - copy_size);
    emit("  mov [rsp + %d], %s", size_diff + copy_size, r->rax);
    copy_size += r->size;
  }
  emit_stack_add(size_diff);
}

static void emit_expr_comma(Expr *expr) {
  assert(expr->ty == EX_COMMA);

  Vector *exprs = expr->comma.exprs;
  for (int i = 0; i < vec_len(exprs); i++) {
    Expr *op = vec_get(exprs, i);
    emit_expr(op);
    if (i != vec_len(exprs) - 1) {
      int op_size = get_val_size(op->val_type, op->range);
      emit_stack_add(align(op_size, 8));
      continue;
    }
  }
}

static void emit_expr_log_and(Expr *expr) {
  assert(expr->ty == EX_LOG_AND);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  char *false_label = make_label("logand.false");
  char *end_label = make_label("logand.end");
  emit_expr(expr->binop.lhs);
  emit_pop("rax");
  emit("  cmp %s, 0", r->rax);
  emit("  je %s", false_label);

  emit_expr(expr->binop.rhs);
  emit_pop("rax");
  emit("  cmp %s, 0", r->rax);
  emit("  je %s", false_label);

  emit_push("1");
  emit("  jmp %s", end_label);
  stack_pos -= 8;

  emit("%s:", false_label);
  emit_push("0");

  emit("%s:", end_label);
}

static void emit_expr_log_or(Expr *expr) {
  assert(expr->ty == EX_LOG_OR);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  char *true_label = make_label("logor.true");
  char *end_label = make_label("logor.end");
  emit_expr(expr->binop.lhs);
  emit_pop("rax");
  emit("  cmp %s, 0", r->rax);
  emit("  jne %s", true_label);

  emit_expr(expr->binop.rhs);
  emit_pop("rax");
  emit("  cmp %s, 0", r->rax);
  emit("  jne %s", true_label);

  emit_push("0");
  emit("  jmp %s", end_label);
  stack_pos -= 8;

  emit("%s:", true_label);
  emit_push("1");

  emit("%s:", end_label);
}

static void emit_expr_cond(Expr *expr) {
  assert(expr->ty == EX_COND);

  const Reg *r = get_int_reg(expr->cond.cond->val_type, expr->cond.cond->range);
  char *else_label = make_label("cond.else");
  char *end_label = make_label("cond.end");
  emit_expr(expr->cond.cond);
  emit_pop("rax");
  emit("  cmp %s, 0", r->rax);
  emit("  je %s", else_label);

  int cond_stack_pos = stack_pos;
  emit_expr(expr->cond.then_expr);
  emit("  jmp %s", end_label);
  int end_stack_pos = stack_pos;

  stack_pos = cond_stack_pos;
  emit("%s:", else_label);
  emit_expr(expr->cond.else_expr);
  assert(stack_pos == end_stack_pos);

  emit("%s:", end_label);
}

static void emit_expr_indirect(Expr *expr) {
  assert(expr->ty == EX_INDIRECT);

  Expr *operand = expr->unop.operand;
  emit_expr(operand);
  emit_pop("rax");

  int size = get_val_size(expr->val_type, expr->range);
  emit_stack_sub(align(size, 8));

  int copy_size = 0;
  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    emit("  mov %s, [rax + %d]", r->rdi, copy_size);
    emit("  mov [rsp + %d], %s", copy_size, r->rdi);
    copy_size += r->size;
  }
}

static void emit_expr_minus(Expr *expr) {
  assert(expr->ty == EX_MINUS);

  emit_expr(expr->unop.operand);

  if (is_int_reg_type(expr->val_type)) {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit("  neg %s [rsp]", r->ptr);
    return;
  }

  if (is_sse_reg_type(expr->val_type)) {
    emit_pop_xmm(0);

    const SseOp *op = get_sse_op(expr->val_type, expr->range);
    if (expr->val_type->ty == TY_DOUBLE) {
      emit("  mov DWORD PTR [rsp - 4], 0x80000000");
      emit("  mov DWORD PTR [rsp - 8], 0x00000000");
    } else {
      assert(expr->val_type->ty == TY_FLOAT);
      emit("  mov DWORD PTR [rsp - 4], 0x00000000");
      emit("  mov DWORD PTR [rsp - 8], 0x80000000");
    }
    emit("  movsd xmm1, QWORD PTR [rsp - 8]");
    emit("  %s xmm0, xmm1", op->xor);
    emit_push_xmm(0);
    return;
  }

  if (is_x87_reg_type(expr->val_type)) {
    emit("  fld TBYTE PTR [rsp]");
    emit("  fchs");
    emit("  fstp TBYTE PTR [rsp]");
    return;
  }

  range_internal_error(expr->range, "Invalid type: %s, op=%d",
                       format_type(expr->val_type, false), expr->ty);
}

static void emit_expr_not(Expr *expr) {
  assert(expr->ty == EX_NOT);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_expr(expr->unop.operand);
  emit("  not %s [rsp]", r->ptr);
}

static void emit_expr_cast(Expr *expr) {
  assert(expr->ty == EX_CAST);

  Expr *operand = expr->unop.operand;
  emit_expr(operand);

  if (expr->val_type->ty == TY_VOID) {
    int size = get_val_size(operand->val_type, operand->range);
    emit_stack_add(align(size, 8));
    return;
  }

  if (is_int_reg_type(operand->val_type) && is_int_reg_type(expr->val_type)) {
    const Reg *to = get_int_reg(expr->val_type, expr->range);
    const Reg *from = get_int_reg(operand->val_type, operand->range);
    int from_size = from->size;
    int to_size = to->size;
    if (to_size > from_size) {
      if (is_signed_int_type(operand->val_type, operand->range)) {
        emit("  movsx %s, %s [rsp]", to->rax, from->ptr);
      } else {
        if (from_size < 4) {
          emit("  movzx %s, %s [rsp]", to->rax, from->ptr);
        } else {
          emit("  mov %s, %s [rsp]", from->rax, from->ptr);
        }
      }
      emit("  mov [rsp], rax");
    }
    return;
  }

  if (is_sse_reg_type(operand->val_type) && is_int_reg_type(expr->val_type)) {
    const SseOp *op = get_sse_op(operand->val_type, operand->range);
    const Reg *to = get_int_reg(expr->val_type, expr->range);
    const Reg *conv = to;
    if (to->size < 4) {
      conv = &Reg4;
    }
    emit_pop_xmm(0);
    emit("  %s %s, xmm0", op->cvtt_to_si, conv->rax);
    emit_push("rax");
    return;
  }

  if (is_int_reg_type(operand->val_type) && is_sse_reg_type(expr->val_type)) {
    const SseOp *op = get_sse_op(expr->val_type, expr->range);
    const Reg *from = get_int_reg(operand->val_type, operand->range);
    const Reg *conv = from;
    emit_pop("rax");
    if (from->size < 4) {
      conv = &Reg4;
      if (is_signed_int_type(operand->val_type, operand->range)) {
        emit("  movsx %s, %s", conv->rax, from->rax);
      } else {
        emit("  movzx %s, %s", conv->rax, from->rax);
      }
    }
    emit("  %s xmm0, %s", op->cvtt_from_si, conv->rax);
    emit_push_xmm(0);
    return;
  }

  if (is_sse_reg_type(operand->val_type) && is_sse_reg_type(expr->val_type)) {
    const SseOp *op = get_sse_op(operand->val_type, expr->range);
    emit_pop_xmm(0);
    if (expr->val_type->ty == TY_FLOAT) {
      emit("  %s xmm0, xmm0", op->cvt_to_ss);
    } else if (expr->val_type->ty == TY_DOUBLE) {
      emit("  %s xmm0, xmm0", op->cvt_to_sd);
    } else {
      goto CastError;
    }
    emit_push_xmm(0);
    return;
  }

  if (is_int_reg_type(operand->val_type) && is_x87_reg_type(expr->val_type)) {
    const Reg *from = get_int_reg(operand->val_type, operand->range);
    const Reg *conv = from;
    if (from->size < 2) {
      conv = &Reg2;
      if (is_signed_int_type(operand->val_type, operand->range)) {
        emit("  movsx %s, %s[rsp]", conv->rax, from->ptr);
      } else {
        emit("  movzx %s, %s[rsp]", conv->rax, from->ptr);
      }
      emit("  mov [rsp], %s", conv->rax);
    }
    emit("  fild %s [rsp]", conv->ptr);
    emit("  fstp TBYTE PTR [rsp - 8]");
    emit("  mov %s [rsp + 4], 0", Reg4.ptr);
    emit_stack_sub(8);
    return;
  }

  if (is_x87_reg_type(operand->val_type) && is_int_reg_type(expr->val_type)) {
    emit("  fld TBYTE PTR [rsp]");
    emit("  fisttp QWORD PTR [rsp + 8]");
    emit_stack_add(8);
    return;
  }

  if (is_sse_reg_type(operand->val_type) && is_x87_reg_type(expr->val_type)) {
    const SseOp *from_op = get_sse_op(operand->val_type, operand->range);
    emit("  fld %s [rsp]", from_op->ptr);
    emit("  fstp TBYTE PTR [rsp - 8]");
    emit("  mov %s [rsp + 4], 0", Reg4.ptr);
    emit_stack_sub(8);
    return;
  }

  if (is_x87_reg_type(operand->val_type) && is_sse_reg_type(expr->val_type)) {
    const SseOp *to_op = get_sse_op(expr->val_type, expr->range);
    emit("  fld TBYTE PTR [rsp]");
    emit("  fstp %s [rsp + 8]", to_op->ptr);
    emit_stack_add(8);
    return;
  }

CastError:
  range_error(expr->range, "不正なキャストです: %s, %s",
              format_type(operand->val_type, false),
              format_type(expr->val_type, false));
}

static void emit_expr_pre_inc(Expr *expr) {
  assert(expr->ty == EX_PRE_INC);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_lval(expr->unop.operand);
  emit("  mov rax, [rsp]");
  emit("  mov %s, [rax]", r->rdi);
  emit("  add %s, %d", r->rdi, get_incdec_size(expr));
  emit("  mov [rax], %s", r->rdi);
  emit("  mov [rsp], rdi");
}

static void emit_expr_pre_dec(Expr *expr) {
  assert(expr->ty == EX_PRE_DEC);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_lval(expr->unop.operand);
  emit("  mov rax, [rsp]");
  emit("  mov %s, [rax]", r->rdi);
  emit("  sub %s, %d", r->rdi, get_incdec_size(expr));
  emit("  mov [rax], %s", r->rdi);
  emit("  mov [rsp], rdi");
}

static void emit_expr_post_inc(Expr *expr) {
  assert(expr->ty == EX_POST_INC);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_lval(expr->unop.operand);
  emit("  mov rax, [rsp]");
  emit("  mov %s, [rax]", r->rdi);
  emit("  mov [rsp], rdi");
  emit("  add %s, %d", r->rdi, get_incdec_size(expr));
  emit("  mov [rax], %s", r->rdi);
}

static void emit_expr_post_dec(Expr *expr) {
  assert(expr->ty == EX_POST_DEC);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_lval(expr->unop.operand);
  emit("  mov rax, [rsp]");
  emit("  mov %s, [rax]", r->rdi);
  emit("  mov [rsp], rdi");
  emit("  sub %s, %d", r->rdi, get_incdec_size(expr));
  emit("  mov [rax], %s", r->rdi);
}

static void emit_expr_builtin_va_start(Expr *expr) {
  assert(expr->ty == EX_BUILTIN_VA_START);

  Expr *ap = expr->builtin_va_start.ap;
  range_assert(expr->range, is_ptr_type(ap->val_type),
               "va_list is not pointer");
  emit_expr(ap);
  emit_pop("rax");
  const char *gp_offset = "[rax]";
  const char *fp_offset = "[rax + 4]";
  const char *overflow_arg_area = "[rax + 8]";
  const char *reg_save_area = "[rax + 16]";
  emit("  mov DWORD PTR %s, %d", gp_offset, reg_gp_offset);
  emit("  mov DWORD PTR %s, %d", fp_offset, reg_fp_offset);
  emit("  lea rdi, [rbp + %d]", overflow_arg_area_offset);
  emit("  mov %s, rdi", overflow_arg_area);
  emit("  lea rdi, [rbp - %d]", reg_save_area_pos);
  emit("  mov %s, rdi", reg_save_area);
}

static void emit_expr_builtin_va_arg(Expr *expr) {
  assert(expr->ty == EX_BUILTIN_VA_ARG);

  int num_int = 0;
  int num_sse = 0;

  Type *type = expr->val_type;

  Expr *ap = expr->builtin_va_arg.ap;
  arg_class_t class = classify_arg_type(type, expr->range, &num_int, &num_sse);

  emit_expr(ap);
  emit_pop("rcx");
  const char *gp_offset = "[rcx]";
  const char *fp_offset = "[rcx + 4]";
  const char *overflow_arg_area = "[rcx + 8]";
  const char *reg_save_area = "[rcx + 16]";
  const char *stack_label = NULL;
  const char *fetch_label = make_label("va_arg.fetch");
  int size = get_val_size(type, expr->range);
  switch (class) {
  case ARG_CLASS_MEMORY:
  case ARG_CLASS_X87:
    // do nothing
    break;
  case ARG_CLASS_INTEGER:
    stack_label = make_label("va_arg.stack");
    emit("  mov %s, %s", Reg4.rax, gp_offset);
    emit("  cmp %s, %d", Reg4.rax, 8 * NUM_INT_REG - align(size, 8));
    emit("  ja %s", stack_label);
    emit("  lea %s, [rax + %d]", Reg4.rdx, align(size, 8));
    emit("  add rax, %s", reg_save_area);
    emit("  mov %s, %s", gp_offset, Reg4.rdx);
    emit("  jmp %s", fetch_label);
    break;
  case ARG_CLASS_SSE:
    stack_label = make_label("va_arg.stack");
    emit("  mov %s, %s", Reg4.rax, fp_offset);
    emit("  cmp %s, %d", Reg4.rax,
         8 * NUM_INT_REG + 16 * NUM_SSE_REG - align(size, 16));
    emit("  ja %s", stack_label);
    emit("  lea %s, [rax + %d]", Reg4.rdx, align(size, 16));
    emit("  add rax, %s", reg_save_area);
    emit("  mov %s, %s", fp_offset, Reg4.rdx);
    emit("  jmp %s", fetch_label);
    break;
    break;
  }
  if (stack_label != NULL) {
    emit("%s:", stack_label);
  }
  emit("  mov rax, %s", overflow_arg_area);
  emit("  lea rdx, [rax + %d]", align(size, 8));
  emit("  mov %s, rdx", overflow_arg_area);

  emit("%s:", fetch_label);
  emit_stack_sub(align(size, 8));
  int copy_size = 0;
  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    emit("  mov %s, [rax + %d]", r->rdx, copy_size);
    emit("  mov [rsp + %d], %s", copy_size, r->rdx);
    copy_size += r->size;
  }
}

static void emit_expr_builtin_va_end(Expr *expr) {
  assert(expr->ty == EX_BUILTIN_VA_END);
  // nothing to do
}
static void emit_expr_builtin_va_copy(Expr *expr) {
  assert(expr->ty == EX_BUILTIN_VA_COPY);

  Type *type = new_type_builtin_va_list(expr->range);
  int size = get_val_size(type, expr->range);
  emit_assign(type, expr->range, expr->builtin_va_copy.dest,
              expr->builtin_va_copy.src);
  emit_stack_add(align(size, 8));
}

static void emit_expr_binop(Expr *expr) {
  const Expr *lhs = expr->binop.lhs;
  if (is_int_reg_type(lhs->val_type)) {
    emit_expr_binop_int(expr);
    return;
  }

  if (is_sse_reg_type(lhs->val_type)) {
    emit_expr_binop_sse(expr);
    return;
  }

  if (is_x87_reg_type(lhs->val_type)) {
    emit_expr_binop_x87(expr);
    return;
  }

  range_internal_error(expr->range, "Invalid type: %s, op=%d",
                       format_type(expr->val_type, false), expr->ty);
}

static void emit_expr_binop_int(Expr *expr) {
  Expr *lhs = expr->binop.lhs;
  Expr *rhs = expr->binop.rhs;
  assert(is_int_reg_type(lhs->val_type));

  const Reg *rex = get_int_reg(expr->val_type, expr->range);
  const Reg *rop = get_int_reg(lhs->val_type, lhs->range);
  emit_expr(lhs);
  emit_expr(rhs);

  switch (expr->ty) {
  case EX_ADD:
    assert(rex->size == rop->size);
    emit_pop("rdi");
    emit("  add [rsp], %s", rop->rdi);
    break;
  case EX_SUB:
    assert(rex->size == rop->size);
    emit_pop("rdi");
    emit("  sub [rsp], %s", rop->rdi);
    break;
  case EX_MUL:
    assert(rex->size == rop->size);
    emit_pop("rdi");
    emit_pop("rax");
    emit("  imul %s, %s", rop->rax, rop->rdi);
    emit_push("rax");
    break;
  case EX_DIV:
    assert(rex->size == rop->size);
    emit_pop("rdi");
    emit_pop("rax");
    emit("  mov %s, 0", rop->rdx);
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit("  idiv %s", rop->rdi);
    } else {
      emit("  div %s", rop->rdi);
    }
    emit_push("rax");
    break;
  case EX_MOD:
    assert(rex->size == rop->size);
    emit_pop("rdi");
    emit_pop("rax");
    emit("  mov %s, 0", rop->rdx);
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit("  idiv %s", rop->rdi);
    } else {
      emit("  div %s", rop->rdi);
    }
    emit("  mov %s, %s", rex->rax, rop->rdx);
    emit_push("rax");
    break;
  case EX_EQEQ:
    emit_pop("rdi");
    emit("  cmp [rsp], %s", rop->rdi);
    emit("  sete al");
    emit("  movzx %s, al", rex->rax);
    emit("  mov [rsp], %s", rex->rax);
    break;
  case EX_NOTEQ:
    emit_pop("rdi");
    emit("  cmp [rsp], %s", rop->rdi);
    emit("  setne al");
    emit("  movzx %s, al", rex->rax);
    emit("  mov [rsp], %s", rex->rax);
    break;
  case EX_LT:
    emit_pop("rdi");
    emit("  cmp [rsp], %s", rop->rdi);
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit("  setl al");
    } else {
      emit("  setb al");
    }
    emit("  movzx %s, al", rex->rax);
    emit("  mov [rsp], %s", rex->rax);
    break;
  case EX_GT:
    emit_pop("rdi");
    emit("  cmp [rsp], %s", rop->rdi);
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit("  setg al");
    } else {
      emit("  seta al");
    }
    emit("  movzx %s, al", rex->rax);
    emit("  mov [rsp], %s", rex->rax);
    break;
  case EX_LTEQ:
    emit_pop("rdi");
    emit("  cmp [rsp], %s", rop->rdi);
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit("  setle al");
    } else {
      emit("  setbe al");
    }
    emit("  movzx %s, al", rex->rax);
    emit("  mov [rsp], %s", rex->rax);
    break;
  case EX_GTEQ:
    emit_pop("rdi");
    emit("  cmp [rsp], %s", rop->rdi);
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit("  setge al");
    } else {
      emit("  setae al");
    }
    emit("  movzx %s, al", rex->rax);
    emit("  mov [rsp], %s", rex->rax);
    break;
  case EX_LSHIFT:
    assert(rex->size == rop->size);
    emit_pop("rcx");
    emit("  shl %s [rsp], cl", rop->ptr);
    break;
  case EX_RSHIFT:
    assert(rex->size == rop->size);
    emit_pop("rcx");
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit("  sar %s [rsp], cl", rop->ptr);
    } else {
      emit("  shr %s [rsp], cl", rop->ptr);
    }
    break;
  case EX_AND:
    assert(rex->size == rop->size);
    emit_pop("rdi");
    emit("  and [rsp], %s", rop->rdi);
    break;
  case EX_XOR:
    assert(rex->size == rop->size);
    emit_pop("rdi");
    emit("  xor [rsp], %s", rop->rdi);
    break;
  case EX_OR:
    assert(rex->size == rop->size);
    emit_pop("rdi");
    emit("  or [rsp], %s", rop->rdi);
    break;
  default:
    range_error(expr->range, "不正な演算子です: %d", expr->ty);
  }

  return;
}

static void emit_expr_binop_sse(Expr *expr) {
  Type *type = expr->binop.lhs->val_type;
  assert(is_sse_reg_type(type));

  const SseOp *op = get_sse_op(type, expr->range);

  emit_expr(expr->binop.lhs);
  emit_expr(expr->binop.rhs);

  emit_pop_xmm(1); // rhs
  emit_pop_xmm(0); // lhs

  switch (expr->ty) {
  case EX_ADD:
    emit("  %s xmm0, xmm1", op->add);
    emit_push_xmm(0);
    break;
  case EX_SUB:
    emit("  %s xmm0, xmm1", op->sub);
    emit_push_xmm(0);
    break;
  case EX_MUL:
    emit("  %s xmm0, xmm1", op->mul);
    emit_push_xmm(0);
    break;
  case EX_DIV:
    emit("  %s xmm0, xmm1", op->div);
    emit_push_xmm(0);
    break;
  case EX_EQEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit("  %s xmm0, xmm1", op->comi);
    emit("  sete al");
    emit("  setnp dil");
    emit("  and al, dil");
    emit("  movzx %s, al", r->rax);
    emit_push("rax");
    break;
  }
  case EX_NOTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit("  %s xmm0, xmm1", op->comi);
    emit("  setne al");
    emit("  setp dil");
    emit("  or al, dil");
    emit("  movzx %s, al", r->rax);
    emit_push("rax");
    break;
  }
  case EX_LT: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit("  %s xmm1, xmm0", op->comi);
    emit("  seta al");
    emit("  movzx %s, al", r->rax);
    emit_push("rax");
    break;
  }
  case EX_GT: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit("  %s xmm0, xmm1", op->comi);
    emit("  seta al");
    emit("  movzx %s, al", r->rax);
    emit_push("rax");
    break;
  }
  case EX_LTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit("  %s xmm1, xmm0", op->comi);
    emit("  setnb al");
    emit("  movzx %s, al", r->rax);
    emit_push("rax");
    break;
  }
  case EX_GTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit("  %s xmm0, xmm1", op->comi);
    emit("  setnb al");
    emit("  movzx %s, al", r->rax);
    emit_push("rax");
    break;
  }
  default:
    range_error(expr->range, "不正な演算子です: %d %s", expr->ty,
                format_type(type, false));
  }
  return;
}

static void emit_expr_binop_x87(Expr *expr) {
  Type *type = expr->binop.lhs->val_type;
  assert(is_x87_reg_type(type));

  emit_expr(expr->binop.lhs);
  emit_expr(expr->binop.rhs);

  switch (expr->ty) {
  case EX_ADD:
    emit("  fld TBYTE PTR [rsp + 16]");
    emit("  fld TBYTE PTR [rsp]");
    emit("  faddp st(1), st");
    emit("  fstp TBYTE PTR [rsp + 16]");
    emit_stack_add(16);
    break;
  case EX_SUB:
    emit("  fld TBYTE PTR [rsp + 16]");
    emit("  fld TBYTE PTR [rsp]");
    emit("  fsubp st(1), st");
    emit("  fstp TBYTE PTR [rsp + 16]");
    emit_stack_add(16);
    break;
  case EX_MUL:
    emit("  fld TBYTE PTR [rsp + 16]");
    emit("  fld TBYTE PTR [rsp]");
    emit("  fmulp st(1), st");
    emit("  fstp TBYTE PTR [rsp + 16]");
    emit_stack_add(16);
    break;
  case EX_DIV:
    emit("  fld TBYTE PTR [rsp + 16]");
    emit("  fld TBYTE PTR [rsp]");
    emit("  fdivp st(1), st");
    emit("  fstp TBYTE PTR [rsp + 16]");
    emit_stack_add(16);
    break;
  case EX_EQEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit("  fld TBYTE PTR [rsp + 16]");
    emit("  fld TBYTE PTR [rsp]");
    emit("  fcomip st, st(1)");
    emit("  fstp st(0)");
    emit("  sete al");
    emit("  setnp dil");
    emit("  and al, dil");
    emit("  movzx %s, al", r->rax);
    emit_stack_add(32);
    emit_push("rax");
    break;
  }
  case EX_NOTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit("  fld TBYTE PTR [rsp + 16]");
    emit("  fld TBYTE PTR [rsp]");
    emit("  fcomip st, st(1)");
    emit("  fstp st(0)");
    emit("  setne al");
    emit("  setp dil");
    emit("  or al, dil");
    emit("  movzx %s, al", r->rax);
    emit_stack_add(32);
    emit_push("rax");
    break;
  }
  case EX_LT: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit("  fld TBYTE PTR [rsp + 16]");
    emit("  fld TBYTE PTR [rsp]");
    emit("  fcomip st, st(1)");
    emit("  fstp st(0)");
    emit("  seta al");
    emit("  movzx %s, al", r->rax);
    emit_stack_add(32);
    emit_push("rax");
    break;
  }
  case EX_GT: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit("  fld TBYTE PTR [rsp]");
    emit("  fld TBYTE PTR [rsp + 16]");
    emit("  fcomip st, st(1)");
    emit("  fstp st(0)");
    emit("  seta al");
    emit("  movzx %s, al", r->rax);
    emit_stack_add(32);
    emit_push("rax");
    break;
  }
  case EX_LTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit("  fld TBYTE PTR [rsp + 16]");
    emit("  fld TBYTE PTR [rsp]");
    emit("  fcomip st, st(1)");
    emit("  fstp st(0)");
    emit("  setnb al");
    emit("  movzx %s, al", r->rax);
    emit_stack_add(32);
    emit_push("rax");
    break;
  }
  case EX_GTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit("  fld TBYTE PTR [rsp]");
    emit("  fld TBYTE PTR [rsp + 16]");
    emit("  fcomip st, st(1)");
    emit("  fstp st(0)");
    emit("  setnb al");
    emit("  movzx %s, al", r->rax);
    emit_stack_add(32);
    emit_push("rax");
    break;
  }
  default:
    range_error(expr->range, "不正な演算子です: %d %s", expr->ty,
                format_type(type, false));
  }
  return;
}

static void emit_stmt(Stmt *stmt, bool leave_value) {
  const char *filename;
  int line;
  int column;
  range_get_start(stmt->range, &filename, &line, &column);
  emit("  # %s:%d:%d", filename, line, column);

  switch (stmt->ty) {
  case ST_NULL: {
    return;
  }
  case ST_EXPR: {
    int base_stack_pos = stack_pos;
    emit_expr(stmt->expr);

    // 式の評価結果としてスタックに一つの値が残っている
    // はずなので、スタックが溢れないようにポップしておく
    if (stmt->expr->val_type->ty != TY_VOID && !leave_value) {
      int size = get_val_size(stmt->expr->val_type, stmt->expr->range);
      emit_stack_add(align(size, 8));
    }
    if (stmt->val_type->ty == TY_VOID || !leave_value) {
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
    } else {
      int size = get_val_size(stmt->val_type, stmt->range);
      range_assert(stmt->range, stack_pos - align(size, 8) == base_stack_pos,
                   "stack position mismatch");
    }
    return;
  }
  case ST_COMPOUND: {
    int base_stack_pos = stack_pos;
    for (int i = 0; i < vec_len(stmt->stmts); i++) {
      bool is_last = i == vec_len(stmt->stmts) - 1;
      emit_stmt(vec_get(stmt->stmts, i), leave_value && is_last);
    }
    if (stmt->val_type->ty == TY_VOID || !leave_value) {
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
    } else {
      int size = get_val_size(stmt->val_type, stmt->range);
      range_assert(stmt->range, stack_pos - align(size, 8) == base_stack_pos,
                   "stack position mismatch");
    }
    return;
  }
  case ST_DECL: {
    for (int i = 0; i < vec_len(stmt->decl); i++) {
      StackVarDecl *decl = vec_get(stmt->decl, i);
      StackVar *svar = decl->stack_var;
      Initializer *init = decl->init;
      emit_svar_zero(svar);
      emit_svar_init(svar, 0, init, svar->range);
    }
    return;
  }
  case ST_IF: {
    int base_stack_pos = stack_pos;
    char *else_label = make_label("if.else");
    char *end_label = make_label("if.end");
    const Reg *r = get_int_reg(stmt->cond->val_type, stmt->cond->range);
    emit_expr(stmt->cond);
    emit_pop("rax");
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");

    emit("  cmp %s, 0", r->rax);
    emit("  je %s", else_label);

    emit_stmt(stmt->then_stmt, false);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    emit("  jmp %s", end_label);

    emit("%s:", else_label);
    emit_stmt(stmt->else_stmt, false);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");

    emit("%s:", end_label);
    return;
  }
  case ST_SWITCH: {
    int base_stack_pos = stack_pos;
    char *end_label = make_label("switch.end");
    const Reg *r = get_int_reg(stmt->cond->val_type, stmt->cond->range);
    emit_expr(stmt->cond);
    for (int i = 0; i < vec_len(stmt->cases); i++) {
      Stmt *case_expr = vec_get(stmt->cases, i);
      emit_expr(case_expr->expr);
      emit_pop("rax");
      emit_pop("rdi");
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
      emit("  cmp %s, %s", r->rax, r->rdi);
      emit("  je %s", case_expr->label);
      emit_push("rdi");
    }
    emit_pop("rdi");
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    if (stmt->default_case) {
      emit("  jmp %s", stmt->default_case->label);
    } else {
      emit("  jmp %s", end_label);
    }
    vec_push(break_labels, end_label);
    emit_stmt(stmt->body, false);
    vec_pop(break_labels);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    emit("%s:", end_label);
    return;
  }
  case ST_CASE:
  case ST_DEFAULT:
  case ST_LABEL: {
    emit("%s:", stmt->label);
    emit_stmt(stmt->body, leave_value);
    return;
  }
  case ST_WHILE: {
    int base_stack_pos = stack_pos;
    char *cond_label = make_label("while.cond");
    char *end_label = make_label("while.end");

    emit("%s:", cond_label);
    const Reg *r = get_int_reg(stmt->cond->val_type, stmt->cond->range);
    emit_expr(stmt->cond);
    emit_pop("rax");
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    emit("  cmp %s, 0", r->rax);
    emit("  je %s", end_label);

    vec_push(break_labels, end_label);
    vec_push(continue_labels, cond_label);
    emit_stmt(stmt->body, false);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    vec_pop(break_labels);
    vec_pop(continue_labels);

    emit("  jmp %s", cond_label);
    emit("%s:", end_label);
    return;
  }
  case ST_DO_WHILE: {
    int base_stack_pos = stack_pos;
    char *loop_label = make_label("do_while.loop");
    char *cond_label = make_label("do_while.cond");
    char *end_label = make_label("do_while.end");
    emit("%s:", loop_label);

    vec_push(break_labels, end_label);
    vec_push(continue_labels, cond_label);
    emit_stmt(stmt->body, false);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    vec_pop(break_labels);
    vec_pop(continue_labels);

    emit("%s:", cond_label);
    const Reg *r = get_int_reg(stmt->cond->val_type, stmt->cond->range);
    emit_expr(stmt->cond);
    emit_pop("rax");
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");

    emit("  cmp %s, 0", r->rax);
    emit("  jne %s", loop_label);

    emit("%s:", end_label);
    return;
  }
  case ST_FOR: {
    int base_stack_pos = stack_pos;
    char *cond_label = make_label("for.cond");
    char *inc_label = make_label("for.inc");
    char *end_label = make_label("for.end");
    if (stmt->init != NULL) {
      emit_stmt(stmt->init, false);
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
    }
    emit("%s:", cond_label);
    if (stmt->cond != NULL) {
      const Reg *r = get_int_reg(stmt->cond->val_type, stmt->cond->range);
      emit_expr(stmt->cond);
      emit_pop("rax");
      emit("  cmp %s, 0", r->rax);
      emit("  je %s", end_label);
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
    }

    vec_push(break_labels, end_label);
    vec_push(continue_labels, inc_label);
    emit_stmt(stmt->body, false);
    vec_pop(break_labels);
    vec_pop(continue_labels);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");

    emit("%s:", inc_label);
    if (stmt->inc != NULL) {
      emit_expr(stmt->inc);
      emit_pop("rax");
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
    }
    emit("  jmp %s", cond_label);
    emit("%s:", end_label);
    return;
  }
  case ST_GOTO: {
    char *label = get_label(func_ctxt, stmt->name);
    if (label == NULL) {
      range_error(stmt->range, "未知のラベルへのgotoです: %s", stmt->name);
    }
    emit("  jmp %s", label);
    return;
  }
  case ST_BREAK: {
    if (vec_len(break_labels) <= 0) {
      range_error(stmt->range,
                  "ループでもswitch文中でもない箇所にbreakがあります");
    }
    emit("  jmp %s", (char *)vec_last(break_labels));
    return;
  }
  case ST_CONTINUE: {
    if (vec_len(continue_labels) <= 0) {
      range_error(stmt->range, "ループ中でない箇所にcontinueがあります");
    }
    emit("  jmp %s", (char *)vec_last(continue_labels));
    return;
  }
  case ST_RETURN: {
    int base_stack_pos = stack_pos;
    if (stmt->expr != NULL) {
      Type *ret_type = func_ctxt->type->func.ret;
      emit_expr(stmt->expr);

      int int_reg_idx = 0;
      int sse_reg_idx = 0;
      int size = get_val_size(ret_type, func_ctxt->range);
      arg_class_t class = classify_arg_type(ret_type, func_ctxt->range,
                                            &int_reg_idx, &sse_reg_idx);
      switch (class) {
      case ARG_CLASS_MEMORY:
        // 戻り値を格納するアドレス
        emit("  mov rax, [rbp - %d]", retval_pos);
        int copy_size = 0;
        while (size - copy_size > 0) {
          const Reg *r = get_int_reg_for_copy(size - copy_size);
          emit("  mov %s, [rsp + %d]", r->rdi, copy_size);
          emit("  mov [rax + %d], %s", copy_size, r->rdi);
          copy_size += r->size;
        }
        emit_stack_add(align(size, 8));
        break;
      case ARG_CLASS_INTEGER:
        emit_pop("rax");
        if (size > 8) {
          emit_pop("rdx");
        }
        break;
      case ARG_CLASS_SSE:
        assert(size <= 8);
        emit_pop_xmm(0);
        break;
      case ARG_CLASS_X87:
        emit("  fld TBYTE PTR [rsp]");
        emit_stack_add(16);
        break;
      }
    }
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    emit("  jmp %s", epilogue_label);
    return;
  }
  }
  range_error(stmt->range, "不正な文です: %d", stmt->ty);
}

static IntVector *classify_param(const Vector *params, int int_reg_idx) {
  IntVector *class = new_int_vector();
  int num_int_reg = int_reg_idx;
  int num_sse_reg = 0;
  for (int i = 0; i < vec_len(params); i++) {
    Param *param = vec_get(params, i);
    int_vec_push(class, classify_arg_type(param->type, param->range,
                                          &num_int_reg, &num_sse_reg));
  }
  return class;
}

static void emit_func(Function *func) {
  epilogue_label = make_label("func.epi");
  func_ctxt = func;
  break_labels = new_vector();
  continue_labels = new_vector();
  retval_pos = INVALID_STACK_POS;
  reg_save_area_pos = INVALID_STACK_POS;
  reg_gp_offset = INVALID_STACK_POS;
  reg_fp_offset = INVALID_STACK_POS;

  if (!func->storage_class.is_static) {
    emit(".global %s", func->name);
  }
  emit("%s:", func->name);

  // プロローグ
  // スタックサイズ分の領域を確保する
  emit("  push rbp");
  emit("  mov rbp, rsp");

  // ローカル変数の領域確保
  int stack_size = 0;
  for (int i = 0; i < vec_len(func->var_list); i++) {
    StackVar *svar = vec_get(func->var_list, i);
    stack_size = align(stack_size, get_val_align(svar->type, svar->range));
    svar->offset = stack_size;
    stack_size += get_val_size(svar->type, svar->range);
  }
  for (int i = 0; i < vec_len(func->var_list); i++) {
    StackVar *svar = vec_get(func->var_list, i);
    svar->offset = align(stack_size, 16) - svar->offset;

    const char *filename;
    int line;
    int column;
    range_get_start(svar->range, &filename, &line, &column);
    emit("  # %s:%d:%d", filename, line, column);
    emit("  # [rbp - %d]: svar %s (size=%d) (%s:%d:%d)", svar->offset,
         svar->name, get_val_size(svar->type, svar->range), filename, line,
         column);
  }

  stack_pos = 0;
  int reg_save_area_size = 0;
  if (func->type->func.has_varargs) {
    reg_save_area_size += 8 * NUM_INT_REG + 16 * NUM_SSE_REG;
  }
  emit_stack_sub(align(stack_size, 16) + align(reg_save_area_size, 16));
  if (func->type->func.has_varargs) {
    reg_save_area_pos = stack_pos;
    emit("  # [rbp - %d]: reg_save_area", stack_pos);

    for (int i = 0; i < NUM_INT_REG; i++) {
      emit("  mov [rbp - %d], %s", stack_pos - 8 * i,
           get_int_arg_reg(&Reg8, i));
    }
    const char *end_label = make_label("skip_float_reg");
    emit("  test al, al");
    emit("  je %s", end_label);
    for (int i = 0; i < NUM_SSE_REG; i++) {
      emit("  movaps [rbp - %d], xmm%d", stack_pos - 8 * NUM_INT_REG - 16 * i,
           i);
    }
    emit("%s:", end_label);
  }

  int arg_stack_offset = 16;
  int ret_stack_offset = 0;
  int int_reg_idx = 0;
  int sse_reg_idx = 0;
  if (func->type->func.ret->ty != TY_VOID) {
    int num_int_reg = 0;
    int num_sse_reg = 0;
    arg_class_t class = classify_arg_type(func->type->func.ret, func->range,
                                          &num_int_reg, &num_sse_reg);
    if (class == ARG_CLASS_MEMORY) {
      // 戻り値をメモリで返す場合は、格納先のポインタをpushしておく
      emit_push("rdi");
      int_reg_idx++;
      ret_stack_offset = 8;
      retval_pos = stack_pos;
    }
  }

  // 引数をスタックへコピー
  if (func->type->func.param != NULL) {
    IntVector *param_class =
        classify_param(func->type->func.param, int_reg_idx);
    for (int i = 0; i < vec_len(func->type->func.param); i++) {
      Param *param = vec_get(func->type->func.param, i);
      StackVar *var = param->stack_var;
      assert(var != NULL);
      arg_class_t class = int_vec_get(param_class, i);
      int size = get_val_size(param->type, param->range);
      int dst_offset = var->offset;

      switch (class) {
      case ARG_CLASS_MEMORY:
      case ARG_CLASS_X87: {
        int copy_size = 0;
        while (size - copy_size > 0) {
          const Reg *r = get_int_reg_for_copy(size - copy_size);
          emit("  mov %s, [rbp + %d]", r->rax, arg_stack_offset + copy_size);
          emit("  mov [rbp - %d], %s", dst_offset - copy_size, r->rax);
          copy_size += r->size;
        }
        arg_stack_offset += align(size, 8);
        continue;
      }
      case ARG_CLASS_INTEGER: {
        int rest_size = size;
        int num_reg = (size + 7) / 8;
        for (int j = 0; j < num_reg; j++) {
          int copy_size = 0;
          while (rest_size > 0) {
            if (rest_size >= 8) {
              emit("  mov [rbp - %d], %s", dst_offset - j * 8 - copy_size,
                   get_int_arg_reg(&Reg8, int_reg_idx));
              rest_size -= 8;
              copy_size += 8;
              break;
            }
            if (rest_size >= 4) {
              emit("  mov [rbp - %d], %s", dst_offset - j * 8 - copy_size,
                   get_int_arg_reg(&Reg4, int_reg_idx));
              rest_size -= 4;
              copy_size += 4;
              if (rest_size > 0) {
                emit("  shr %s, 32", get_int_arg_reg(&Reg8, int_reg_idx));
              }
              continue;
            }
            if (rest_size >= 2) {
              emit("  mov [rbp - %d], %s", dst_offset - j * 8 - copy_size,
                   get_int_arg_reg(&Reg2, int_reg_idx));
              rest_size -= 2;
              copy_size += 2;
              if (rest_size > 0) {
                emit("  shr %s, 16", get_int_arg_reg(&Reg8, int_reg_idx));
              }
              continue;
            }

            assert(rest_size == 1);
            emit("  mov [rbp - %d], %s", dst_offset - j * 8 - copy_size,
                 get_int_arg_reg(&Reg1, int_reg_idx));
            rest_size -= 1;
            copy_size += 1;
            assert(rest_size == 0);
          }
          int_reg_idx++;
          continue;
        }
        continue;
      }
      case ARG_CLASS_SSE: {
        assert(size <= 8);
        if (size == 8) {
          emit("  movsd [rbp - %d], xmm%d", dst_offset, sse_reg_idx);
        } else {
          assert(size == 4);
          emit("  movss [rbp - %d], xmm%d", dst_offset, sse_reg_idx);
        }
        sse_reg_idx++;
        continue;
      }
      }
    }
  }

  reg_gp_offset = 8 * int_reg_idx;
  reg_fp_offset = 8 * NUM_INT_REG + 16 * sse_reg_idx;
  overflow_arg_area_offset = arg_stack_offset;

  emit("  # reg_gp_offset: %d", reg_gp_offset);
  emit("  # reg_fp_offset: %d", reg_fp_offset);
  emit("  # overflow_arg_area: [rbp + %d]", overflow_arg_area_offset);
  emit("  # %s body", func->name);

  emit_stmt(func->body, false);

  // main関数の場合、returnなしに関数末尾まで到達した場合、戻り値は0にする
  if (strcmp(func->name, "main") == 0) {
    emit("mov eax, 0");
  }

  // エピローグ
  // 最後の式の結果がRAXに残っているのでそれが返り値になる
  assert(stack_pos == align(stack_size, 16) + align(reg_save_area_size, 16) +
                          ret_stack_offset);
  emit("%s:", epilogue_label);
  emit("  mov rsp, rbp");
  emit("  pop rbp");
  emit("  ret");
  stack_pos = INVALID_STACK_POS;
}

static void emit_svar_zero(StackVar *svar) {
  int size = get_val_size(svar->type, svar->range);
  int copy_size = 0;
  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    emit("  mov %s [rbp - %d], 0", r->ptr, svar->offset - copy_size);
    copy_size += r->size;
  }
}

static void emit_svar_init(StackVar *svar, int offset, Initializer *init,
                           const Range *range) {
  if (init == NULL) {
    return;
  }

  if (init->expr != NULL) {
    emit_expr(init->expr);
    int size = get_val_size(init->expr->val_type, init->expr->range);
    int copy_size = 0;
    while (size - copy_size > 0) {
      const Reg *r = get_int_reg_for_copy(size - copy_size);
      emit("  mov %s, [rsp + %d]", r->rax, copy_size);
      emit("  mov [rbp - %d], %s", svar->offset - copy_size - offset, r->rax);
      copy_size += r->size;
    }

    emit_stack_add(align(size, 8));
    return;
  }

  if (init->members != NULL) {
    assert(init->type->ty == TY_STRUCT || init->type->ty == TY_UNION);
    assert(vec_len(init->members) <= 1 || init->type->ty == TY_STRUCT);

    for (int i = 0; i < vec_len(init->members); i++) {
      MemberInitializer *meminit = vec_get(init->members, i);
      const Member *member = meminit->member;
      emit_svar_init(svar, offset + member->offset, meminit->init, range);
    }
    return;
  }

  if (init->elements != NULL) {
    assert(init->type->ty == TY_ARRAY);
    int size = get_val_size(init->type->array.elem, range);
    for (int i = 0; i < vec_len(init->elements); i++) {
      Initializer *meminit = vec_get(init->elements, i);
      emit_svar_init(svar, offset + i * size, meminit, range);
    }
    return;
  }

  assert(false);
}

static void emit_gvar_init(Initializer *init, const Range *range,
                           Vector *gvar_list) {
  if (init->expr != NULL) {
    Expr *expr = init->expr;
    if (expr->ty == EX_NUM) {
      int size = get_val_size(expr->val_type, expr->range);
      switch (size) {
      case 1:
        emit("  .byte %s", num2str(expr->num, expr->range));
        break;
      case 2:
        emit("  .word %s", num2str(expr->num, expr->range));
        break;
      case 4:
        emit("  .long %s", num2str(expr->num, expr->range));
        break;
      case 8:
        emit("  .quad %s", num2str(expr->num, expr->range));
        break;
      case 16:
        emit("  .quad 0x%016lx", expr->num.bytes[0]);
        emit("  .quad 0x%016lx", expr->num.bytes[1]);
        break;
      default:
        range_internal_error(range, "invalid value size: %d", size);
      }
    } else if (expr->ty == EX_ADDRESS) {
      Expr *operand = expr->unop.operand;
      if (operand->ty == EX_GLOBAL_VAR) {
        emit("  .quad %s", operand->global_var.name);
        return;
      }
      if (operand->ty == EX_COMPOUND) {
        Expr *compound = operand;
        assert(compound->compound.stack_var == NULL);
        GlobalVar *gvar = NEW(GlobalVar);
        gvar->name = make_label("compound");
        gvar->type = compound->val_type;
        gvar->range = compound->range;
        gvar->storage_class.is_static = true;
        gvar->init = compound->compound.init;
        vec_push(gvar_list, gvar);

        emit("  .quad %s", gvar->name);
        return;
      }
      range_error(
          range,
          "グローバル変数またはコンパウンドリテラル以外へのポインタです");
    } else if (expr->ty == EX_COMPOUND) {
      assert(expr->compound.stack_var == NULL);
      emit_gvar_init(expr->compound.init, expr->range, gvar_list);
    } else if (expr->ty == EX_STR) {
      emit("  .quad %s", expr->str->name);
    } else {
      range_error(range, "数値でもポインタでもありません: %d %s", expr->ty,
                  format_type(expr->val_type, false));
    }
    return;
  }

  if (init->members != NULL) {
    assert(init->type->ty == TY_STRUCT || init->type->ty == TY_UNION);
    assert(vec_len(init->members) <= 1 || init->type->ty == TY_STRUCT);
    StructBody *body = init->type->struct_body;
    int offset = 0;
    for (int i = 0; i < vec_len(init->members); i++) {
      MemberInitializer *meminit = vec_get(init->members, i);
      if (meminit->init == NULL) {
        continue;
      }
      if (i > 0) {
        Member *member = vec_get(body->member_list, i);
        if (offset < member->offset) {
          emit("  .zero %d", member->offset - offset);
          offset = member->offset;
        }
        assert(offset == member->offset);
      }
      emit_gvar_init(meminit->init, range, gvar_list);
      offset += get_val_size(meminit->member->type, range);
    }
    int ty_size = get_val_size(init->type, range);
    if (offset < ty_size) {
      emit("  .zero %d", ty_size - offset);
      offset = ty_size;
    }
    assert(body->has_flex_array || offset == ty_size);
    return;
  }

  if (init->elements != NULL) {
    assert(init->type->ty == TY_ARRAY);
    for (int i = 0; i < vec_len(init->elements); i++) {
      Initializer *meminit = vec_get(init->elements, i);
      if (meminit == NULL) {
        emit("  .zero %d", get_val_size(init->type->array.elem, range));
        continue;
      }
      emit_gvar_init(meminit, range, gvar_list);
    }
    return;
  }

  assert(false);
}

static void emit_gvar(GlobalVar *gvar, Vector *gvar_list) {
  Initializer *init = gvar->init;
  if (init == NULL) {
    emit("  .bss");
    if (!gvar->storage_class.is_static) {
      emit(".global %s", gvar->name);
    }
    emit("%s:", gvar->name);
    emit("  .zero %d", get_val_size(gvar->type, gvar->range));
  } else {
    emit("  .data");
    if (!gvar->storage_class.is_static) {
      emit(".global %s", gvar->name);
    }
    emit("%s:", gvar->name);
    emit_gvar_init(init, gvar->range, gvar_list);
  }
}

static void emit_str(StringLiteral *str) {
  emit("%s:", str->name);
  emit("  .string %s", format_string_literal(str->val));
}

void gen(FILE *fp, TranslationUnit *tunit) {
  output_fp = fp;
  emit(".intel_syntax noprefix");

  emit("  .text");
  for (int i = 0; i < vec_len(tunit->func_list); i++) {
    emit_func(vec_get(tunit->func_list, i));
  }
  while (vec_len(tunit->gvar_list) > 0) {
    GlobalVar *gvar = vec_remove(tunit->gvar_list, 0);
    emit_gvar(gvar, tunit->gvar_list);
  }
  emit("  .section .rodata");
  for (int i = 0; i < vec_len(tunit->str_list); i++) {
    emit_str(vec_get(tunit->str_list, i));
  }
}
