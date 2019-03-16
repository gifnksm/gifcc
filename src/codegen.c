#include "gifcc.h"
#include <assert.h>
#include <inttypes.h>
#include <limits.h>
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
    .comi = "comisd",
    .cvtt_to_si = "cvttsd2si",
    .cvt_to_ss = "cvtsd2ss",
    .cvt_to_sd = NULL,
    .cvtt_from_si = "cvtsi2sd",
};

const int NUM_INT_REG = 6;
const int NUM_SSE_REG = 8;

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

static void emit_assign(Type *type, const Range *range, Expr *dest, Expr *src);
static void emit_expr(Expr *expr);
static arg_class_t classify_arg_type(const Type *type, const Range *range,
                                     int *num_int, int *num_sse);
static IntVector *classify_arg(const Vector *args, int int_reg_idx);
static void emit_expr_num(Expr *expr);
static void emit_expr_stack_var(Expr *expr);
static void emit_expr_global_var(Expr *expr);
static void emit_expr_str(Expr *expr);
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
  char buf[256];
  sprintf(buf, ".L.%s.%d", s, count++);
  return strdup(buf);
}

static char *num2str(Number num, const Range *range) {
  char buf[1024];
  switch (num.type) {
  case TY_BOOL:
    sprintf(buf, "%d", num.bool_val);
    break;
  case TY_CHAR:
    sprintf(buf, "%hhd", num.char_val);
    break;
  case TY_S_CHAR:
    sprintf(buf, "%hhd", num.s_char_val);
    break;
  case TY_S_SHORT:
    sprintf(buf, "%hd", num.s_short_val);
    break;
  case TY_S_INT:
    sprintf(buf, "%d", num.s_int_val);
    break;
  case TY_S_LONG:
    sprintf(buf, "%ld", num.s_long_val);
    break;
  case TY_S_LLONG:
    sprintf(buf, "%lld", num.s_llong_val);
    break;
  case TY_U_CHAR:
    sprintf(buf, "%hhu", num.u_char_val);
    break;
  case TY_U_SHORT:
    sprintf(buf, "%hu", num.u_short_val);
    break;
  case TY_U_INT:
  case TY_FLOAT:
    sprintf(buf, "%u", num.u_int_val);
    break;
  case TY_DOUBLE:
  case TY_U_LONG:
    sprintf(buf, "%lu", num.u_long_val);
    break;
  case TY_U_LLONG:
    sprintf(buf, "%llu", num.u_llong_val);
    break;
  case TY_PTR:
    sprintf(buf, "%" PRIdPTR, num.ptr_val);
    break;
  case TY_ENUM:
    sprintf(buf, "%d", num.enum_val);
    break;
  case TY_VOID:
    sprintf(buf, "0");
    break;
  case TY_LDOUBLE:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
  case TY_BUILTIN:
    range_error(range, "不正な型の数値です: %d", num.type);
  }
  return strdup(buf);
}

static int get_incdec_size(Expr *expr) {
  if (expr->val_type->ty == TY_PTR) {
    return get_val_size(expr->val_type->ptrof, expr->range);
  }
  return 1;
}

static void emit_stack_sub(int size) {
  stack_pos += size;
  assert(stack_pos >= 0);
  printf("  sub rsp, %d \t# rsp = rbp - %d\n", size, stack_pos);
}
static void emit_stack_add(int size) {
  stack_pos -= size;
  assert(stack_pos >= 0);
  printf("  add rsp, %d \t# rsp = rbp - %d\n", size, stack_pos);
}

static void emit_pop(const char *operand) {
  stack_pos -= 8;
  assert(stack_pos >= 0);
  printf("  pop %s \t# rsp = rbp - %d\n", operand, stack_pos);
}
static void emit_push(const char *operand) {
  stack_pos += 8;
  assert(stack_pos >= 0);
  printf("  push %s \t# rsp = rbp - %d\n", operand, stack_pos);
}

static void emit_pop_xmm(int n) {
  printf("  movsd xmm%d, [rsp]\n", n);
  emit_stack_add(8);
}

static void emit_push_xmm(int n) {
  emit_stack_sub(8);
  printf("  movsd [rsp], xmm%d\n", n);
}

static void emit_lval(Expr *expr) {
  if (expr->ty == EX_STACK_VAR) {
    StackVar *var = expr->stack_var;
    assert(var != NULL);
    printf("  lea rax, [rbp - %d]\n", var->offset);
    emit_push("rax");
    return;
  }
  if (expr->ty == EX_GLOBAL_VAR) {
    printf("  lea rax, %s[rip]\n", expr->global_var.name);
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
    printf("  lea rax, [rax + %d]\n", expr->dot.member->offset);
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

  range_error(expr->range, "左辺値が変数ではありません: %d", expr->ty);
}

static void emit_assign(Type *type, const Range *range, Expr *dest, Expr *src) {
  emit_expr(src);
  emit_lval(dest);
  emit_pop("rax");

  int size = get_val_size(type, range);
  int copy_size = 0;
  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    printf("  mov %s, [rsp + %d]\n", r->rdi, copy_size);
    printf("  mov [rax + %d], %s\n", copy_size, r->rdi);
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
  case EX_COMPOUND:
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
    printf("  mov %s, %s\n", r->rax, num2str(expr->num, expr->range));
    emit_push("rax");
  } else {
    assert(is_x87_reg_type(expr->val_type));
    assert(size == 16);
    printf("  mov rax, %lu\n", expr->num.bytes[0]);
    printf("  mov [rsp - 16],  rax\n");
    printf("  mov rax, %lu\n", expr->num.bytes[1]);
    printf("  mov [rsp - 8],  rax\n");
    emit_stack_sub(16);
  }
}

static void emit_expr_stack_var(Expr *expr) {
  assert(expr->ty == EX_STACK_VAR);

  StackVar *var = expr->stack_var;
  int size = get_val_size(expr->val_type, expr->range);
  emit_stack_sub(align(size, 8));

  int src_offset = var->offset;
  int copy_size = 0;
  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    printf("  mov %s, [rbp - %d]\n", r->rax, src_offset - copy_size);
    printf("  mov [rsp + %d], %s\n", copy_size, r->rax);
    copy_size += r->size;
  }
}

static void emit_expr_global_var(Expr *expr) {
  assert(expr->ty == EX_GLOBAL_VAR);

  const char *name = expr->global_var.name;
  int size = get_val_size(expr->val_type, expr->range);
  emit_stack_sub(align(size, 8));

  int copy_size = 0;

  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    printf("  mov %s, %s[rip + %d]\n", r->rax, name, copy_size);
    printf("  mov [rsp + %d], %s\n", copy_size, r->rax);
    copy_size += r->size;
  }
}

static void emit_expr_str(Expr *expr) {
  assert(expr->ty == EX_STR);

  printf("  lea rax, %s[rip]\n", expr->str);
  emit_push("rax");
}

static void emit_expr_call(Expr *expr) {
  assert(expr->ty == EX_CALL);

  bool call_direct;
  Type *functype;
  Type *ret_type;
  if (expr->call.callee->val_type->ty == TY_FUNC) {
    call_direct = true;
    functype = expr->call.callee->val_type;
    ret_type = functype->func_ret;
  } else if (expr->call.callee->val_type->ty == TY_PTR &&
             expr->call.callee->val_type->ptrof->ty == TY_FUNC) {
    call_direct = false;
    functype = expr->call.callee->val_type->ptrof;
    ret_type = functype->func_ret;
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
    if (ret_class == ARG_CLASS_MEMORY || ret_class == ARG_CLASS_X87) {
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
        if (functype->func_has_varargs && i >= vec_len(functype->func_param)) {
          num_vararg_sse_reg++;
        }
        continue;
      }
      }
      assert(false);
    }
  }
  if (ret_class == ARG_CLASS_MEMORY || ret_class == ARG_CLASS_X87) {
    // rdiには戻り値の格納先を設定
    printf("  lea rdi, [rsp + %d]\n", arg_size);
  }
  printf("  mov al, %d\n", num_vararg_sse_reg);

  assert(stack_pos % 16 == 0);
  if (call_direct) {
    printf("  call %s\n", expr->call.callee->global_var.name);
  } else {
    printf("  call r10\n");
  }
  if (arg_size > 0) {
    emit_stack_add(arg_size);
  }
  if (ret_type->ty != TY_VOID) {
    switch (ret_class) {
    case ARG_CLASS_MEMORY:
    case ARG_CLASS_X87:
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
    printf("  mov %s, [rsp + %d]\n", r->rax, offset + copy_size);
    printf("  mov [rsp - %d], %s\n", align(mem_size, 8) - copy_size, r->rax);
    copy_size += r->size;
  }
  copy_size = 0;
  while (mem_size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(mem_size - copy_size);
    printf("  mov %s, [rsp - %d]\n", r->rax, align(mem_size, 8) - copy_size);
    printf("  mov [rsp + %d], %s\n", size_diff + copy_size, r->rax);
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
  printf("  cmp %s, 0\n", r->rax);
  printf("  je %s\n", false_label);

  emit_expr(expr->binop.rhs);
  emit_pop("rax");
  printf("  cmp %s, 0\n", r->rax);
  printf("  je %s\n", false_label);

  emit_push("1");
  printf("  jmp %s\n", end_label);
  stack_pos -= 8;

  printf("%s:\n", false_label);
  emit_push("0");

  printf("%s:\n", end_label);
}

static void emit_expr_log_or(Expr *expr) {
  assert(expr->ty == EX_LOG_OR);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  char *true_label = make_label("logor.true");
  char *end_label = make_label("logor.end");
  emit_expr(expr->binop.lhs);
  emit_pop("rax");
  printf("  cmp %s, 0\n", r->rax);
  printf("  jne %s\n", true_label);

  emit_expr(expr->binop.rhs);
  emit_pop("rax");
  printf("  cmp %s, 0\n", r->rax);
  printf("  jne %s\n", true_label);

  emit_push("0");
  printf("  jmp %s\n", end_label);
  stack_pos -= 8;

  printf("%s:\n", true_label);
  emit_push("1");

  printf("%s:\n", end_label);
}

static void emit_expr_cond(Expr *expr) {
  assert(expr->ty == EX_COND);

  const Reg *r = get_int_reg(expr->cond.cond->val_type, expr->cond.cond->range);
  char *else_label = make_label("cond.else");
  char *end_label = make_label("cond.end");
  emit_expr(expr->cond.cond);
  emit_pop("rax");
  printf("  cmp %s, 0\n", r->rax);
  printf("  je %s\n", else_label);

  int cond_stack_pos = stack_pos;
  emit_expr(expr->cond.then_expr);
  printf("  jmp %s\n", end_label);
  int end_stack_pos = stack_pos;

  stack_pos = cond_stack_pos;
  printf("%s:\n", else_label);
  emit_expr(expr->cond.else_expr);
  assert(stack_pos == end_stack_pos);

  printf("%s:\n", end_label);
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
    printf("  mov %s, [rax + %d]\n", r->rdi, copy_size);
    printf("  mov [rsp + %d], %s\n", copy_size, r->rdi);
    copy_size += r->size;
  }
}

static void emit_expr_minus(Expr *expr) {
  assert(expr->ty == EX_MINUS);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_expr(expr->unop.operand);
  printf("  neg %s [rsp]\n", r->ptr);
}

static void emit_expr_not(Expr *expr) {
  assert(expr->ty == EX_NOT);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_expr(expr->unop.operand);
  printf("  not %s [rsp]\n", r->ptr);
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
    int from_size = get_val_size(operand->val_type, operand->range);
    int to_size = get_val_size(expr->val_type, expr->range);
    if (to_size > from_size) {
      if (is_signed_int_type(operand->val_type, operand->range)) {
        printf("  movsx %s, %s [rsp]\n", to->rax, from->ptr);
      } else {
        if (from_size < 4) {
          printf("  movzx %s, %s [rsp]\n", to->rax, from->ptr);
        } else {
          printf("  mov %s, %s [rsp]\n", from->rax, from->ptr);
        }
      }
      printf("  mov [rsp], rax\n");
    }
    return;
  }

  if (is_sse_reg_type(operand->val_type) && is_int_reg_type(expr->val_type)) {
    const SseOp *op = get_sse_op(operand->val_type, operand->range);
    const Reg *to = get_int_reg(expr->val_type, expr->range);
    emit_pop_xmm(0);
    printf("  %s %s, xmm0\n", op->cvtt_to_si, to->rax);
    emit_push("rax");
    return;
  }

  if (is_int_reg_type(operand->val_type) && is_sse_reg_type(expr->val_type)) {
    const SseOp *op = get_sse_op(expr->val_type, expr->range);
    const Reg *from = get_int_reg(operand->val_type, operand->range);
    emit_pop("rax");
    printf("  %s xmm0, %s\n", op->cvtt_from_si, from->rax);
    emit_push_xmm(0);
    return;
  }

  if (is_sse_reg_type(operand->val_type) && is_sse_reg_type(expr->val_type)) {
    const SseOp *op = get_sse_op(operand->val_type, expr->range);
    emit_pop_xmm(0);
    if (expr->val_type->ty == TY_FLOAT) {
      printf("  %s xmm0, xmm0\n", op->cvt_to_ss);
    } else if (expr->val_type->ty == TY_DOUBLE) {
      printf("  %s xmm0, xmm0\n", op->cvt_to_sd);
    } else {
      goto CastError;
    }
    emit_push_xmm(0);
    return;
  }

  if (is_int_reg_type(operand->val_type) && is_x87_reg_type(expr->val_type)) {
    const Reg *from = get_int_reg(operand->val_type, operand->range);
    printf("  fild %s [rsp]\n", from->ptr);
    printf("  fstp TBYTE PTR [rsp - 8]\n");
    emit_stack_sub(8);
    return;
  }

  if (is_x87_reg_type(operand->val_type) && is_int_reg_type(expr->val_type)) {
    printf("  fld TBYTE PTR [rsp]\n");
    printf("  fisttp QWORD PTR [rsp + 8]\n");
    emit_stack_add(8);
    return;
  }

  if (is_sse_reg_type(operand->val_type) && is_x87_reg_type(expr->val_type)) {
    const SseOp *from_op = get_sse_op(operand->val_type, operand->range);
    printf("  fld %s [rsp]\n", from_op->ptr);
    printf("  fstp TBYTE PTR [rsp - 8]\n");
    emit_stack_sub(8);
    return;
  }

  if (is_x87_reg_type(operand->val_type) && is_sse_reg_type(expr->val_type)) {
    const SseOp *to_op = get_sse_op(expr->val_type, expr->range);
    printf("  fld TBYTE PTR [rsp]\n");
    printf("  fstp %s [rsp + 8]\n", to_op->ptr);
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
  printf("  mov rax, [rsp]\n");
  printf("  mov %s, [rax]\n", r->rdi);
  printf("  add %s, %d\n", r->rdi, get_incdec_size(expr));
  printf("  mov [rax], %s\n", r->rdi);
  printf("  mov [rsp], rdi\n");
}

static void emit_expr_pre_dec(Expr *expr) {
  assert(expr->ty == EX_PRE_DEC);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_lval(expr->unop.operand);
  printf("  mov rax, [rsp]\n");
  printf("  mov %s, [rax]\n", r->rdi);
  printf("  sub %s, %d\n", r->rdi, get_incdec_size(expr));
  printf("  mov [rax], %s\n", r->rdi);
  printf("  mov [rsp], rdi\n");
}

static void emit_expr_post_inc(Expr *expr) {
  assert(expr->ty == EX_POST_INC);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_lval(expr->unop.operand);
  printf("  mov rax, [rsp]\n");
  printf("  mov %s, [rax]\n", r->rdi);
  printf("  mov [rsp], rdi\n");
  printf("  add %s, %d\n", r->rdi, get_incdec_size(expr));
  printf("  mov [rax], %s\n", r->rdi);
}

static void emit_expr_post_dec(Expr *expr) {
  assert(expr->ty == EX_POST_DEC);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_lval(expr->unop.operand);
  printf("  mov rax, [rsp]\n");
  printf("  mov %s, [rax]\n", r->rdi);
  printf("  mov [rsp], rdi\n");
  printf("  sub %s, %d\n", r->rdi, get_incdec_size(expr));
  printf("  mov [rax], %s\n", r->rdi);
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
  printf("  mov DWORD PTR %s, %d\n", gp_offset, reg_gp_offset);
  printf("  mov DWORD PTR %s, %d\n", fp_offset, reg_fp_offset);
  printf("  lea rdi, [rbp + %d]\n", overflow_arg_area_offset);
  printf("  mov %s, rdi\n", overflow_arg_area);
  printf("  lea rdi, [rbp - %d]\n", reg_save_area_pos);
  printf("  mov %s, rdi\n", reg_save_area);
}

static void emit_expr_builtin_va_arg(Expr *expr) {
  assert(expr->ty == EX_BUILTIN_VA_ARG);

  int num_int = 0;
  int num_sse = 0;

  Type *type = expr->builtin_va_arg.type;

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
    printf("  mov %s, %s\n", Reg4.rax, gp_offset);
    printf("  cmp %s, %d\n", Reg4.rax, 8 * NUM_INT_REG - align(size, 8));
    printf("  ja %s\n", stack_label);
    printf("  lea %s, [rax + %d]\n", Reg4.rdx, align(size, 8));
    printf("  add rax, %s\n", reg_save_area);
    printf("  mov %s, %s\n", gp_offset, Reg4.rdx);
    printf("  jmp %s\n", fetch_label);
    break;
  case ARG_CLASS_SSE:
    stack_label = make_label("va_arg.stack");
    printf("  mov %s, %s\n", Reg4.rax, fp_offset);
    printf("  cmp %s, %d\n", Reg4.rax,
           8 * NUM_INT_REG + 16 * NUM_SSE_REG - align(size, 16));
    printf("  ja %s\n", stack_label);
    printf("  lea %s, [rax + %d]\n", Reg4.rdx, align(size, 16));
    printf("  add rax, %s\n", reg_save_area);
    printf("  mov %s, %s\n", fp_offset, Reg4.rdx);
    printf("  jmp %s\n", fetch_label);
    break;
    break;
  }
  if (stack_label != NULL) {
    printf("%s:\n", stack_label);
  }
  printf("  mov rax, %s\n", overflow_arg_area);
  printf("  lea rdx, [rax + %d]\n", align(size, 8));
  printf("  mov %s, rdx\n", overflow_arg_area);

  printf("%s:\n", fetch_label);
  emit_stack_sub(align(size, 8));
  int copy_size = 0;
  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    printf("  mov %s, [rax + %d]\n", r->rdx, copy_size);
    printf("  mov [rsp + %d], %s\n", copy_size, r->rdx);
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
  assert(is_int_reg_type(expr->binop.lhs->val_type));

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_expr(expr->binop.lhs);
  emit_expr(expr->binop.rhs);

  switch (expr->ty) {
  case EX_ADD:
    emit_pop("rdi");
    printf("  add [rsp], %s\n", r->rdi);
    break;
  case EX_SUB:
    emit_pop("rdi");
    printf("  sub [rsp], %s\n", r->rdi);
    break;
  case EX_MUL:
    emit_pop("rdi");
    emit_pop("rax");
    printf("  imul %s, %s\n", r->rax, r->rdi);
    emit_push("rax");
    break;
  case EX_DIV:
    emit_pop("rdi");
    emit_pop("rax");
    printf("  mov %s, 0\n", r->rdx);
    if (is_signed_int_type(expr->binop.lhs->val_type, expr->binop.lhs->range)) {
      printf("  idiv %s\n", r->rdi);
    } else {
      printf("  div %s\n", r->rdi);
    }
    emit_push("rax");
    break;
  case EX_MOD:
    emit_pop("rdi");
    emit_pop("rax");
    printf("  mov %s, 0\n", r->rdx);
    if (is_signed_int_type(expr->binop.lhs->val_type, expr->binop.lhs->range)) {
      printf("  idiv %s\n", r->rdi);
    } else {
      printf("  div %s\n", r->rdi);
    }
    printf("  mov %s, %s\n", r->rax, r->rdx);
    emit_push("rax");
    break;
  case EX_EQEQ:
    emit_pop("rdi");
    printf("  cmp [rsp], %s\n", r->rdi);
    printf("  sete al\n");
    printf("  movzx %s, al\n", r->rax);
    printf("  mov [rsp], %s\n", r->rax);
    break;
  case EX_NOTEQ:
    emit_pop("rdi");
    printf("  cmp [rsp], %s\n", r->rdi);
    printf("  setne al\n");
    printf("  movzx %s, al\n", r->rax);
    printf("  mov [rsp], %s\n", r->rax);
    break;
  case EX_LT:
    emit_pop("rdi");
    printf("  cmp [rsp], %s\n", r->rdi);
    if (is_signed_int_type(expr->binop.lhs->val_type, expr->binop.lhs->range)) {
      printf("  setl al\n");
    } else {
      printf("  setb al\n");
    }
    printf("  movzx %s, al\n", r->rax);
    printf("  mov [rsp], %s\n", r->rax);
    break;
  case EX_GT:
    emit_pop("rdi");
    printf("  cmp [rsp], %s\n", r->rdi);
    if (is_signed_int_type(expr->binop.lhs->val_type, expr->binop.lhs->range)) {
      printf("  setg al\n");
    } else {
      printf("  seta al\n");
    }
    printf("  movzx %s, al\n", r->rax);
    printf("  mov [rsp], %s\n", r->rax);
    break;
  case EX_LTEQ:
    emit_pop("rdi");
    printf("  cmp [rsp], %s\n", r->rdi);
    if (is_signed_int_type(expr->binop.lhs->val_type, expr->binop.lhs->range)) {
      printf("  setle al\n");
    } else {
      printf("  setbe al\n");
    }
    printf("  movzx %s, al\n", r->rax);
    printf("  mov [rsp], %s\n", r->rax);
    break;
  case EX_GTEQ:
    emit_pop("rdi");
    printf("  cmp [rsp], %s\n", r->rdi);
    if (is_signed_int_type(expr->binop.lhs->val_type, expr->binop.lhs->range)) {
      printf("  setge al\n");
    } else {
      printf("  setae al\n");
    }
    printf("  movzx %s, al\n", r->rax);
    printf("  mov [rsp], %s\n", r->rax);
    break;
  case EX_LSHIFT:
    emit_pop("rcx");
    printf("  shl %s [rsp], cl\n", r->ptr);
    break;
  case EX_RSHIFT:
    emit_pop("rcx");
    if (is_signed_int_type(expr->binop.lhs->val_type, expr->binop.lhs->range)) {
      printf("  sar %s [rsp], cl\n", r->ptr);
    } else {
      printf("  shr %s [rsp], cl\n", r->ptr);
    }
    break;
  case EX_AND:
    emit_pop("rdi");
    printf("  and [rsp], %s\n", r->rdi);
    break;
  case EX_XOR:
    emit_pop("rdi");
    printf("  xor [rsp], %s\n", r->rdi);
    break;
  case EX_OR:
    emit_pop("rdi");
    printf("  or [rsp], %s\n", r->rdi);
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
    printf("  %s xmm0, xmm1\n", op->add);
    emit_push_xmm(0);
    break;
  case EX_SUB:
    printf("  %s xmm0, xmm1\n", op->sub);
    emit_push_xmm(0);
    break;
  case EX_MUL:
    printf("  %s xmm0, xmm1\n", op->mul);
    emit_push_xmm(0);
    break;
  case EX_DIV:
    printf("  %s xmm0, xmm1\n", op->div);
    emit_push_xmm(0);
    break;
  case EX_EQEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    printf("  %s xmm0, xmm1\n", op->comi);
    printf("  sete al\n");
    printf("  setnp dil\n");
    printf("  and al, dil\n");
    printf("  movzx %s, al\n", r->rax);
    emit_push("rax");
    break;
  }
  case EX_NOTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    printf("  %s xmm0, xmm1\n", op->comi);
    printf("  setne al\n");
    printf("  setp dil\n");
    printf("  or al, dil\n");
    printf("  movzx %s, al\n", r->rax);
    emit_push("rax");
    break;
  }
  case EX_LT: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    printf("  %s xmm0, xmm1\n", op->comi);
    printf("  setl al\n");
    printf("  movzx %s, al\n", r->rax);
    emit_push("rax");
    break;
  }
  case EX_GT: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    printf("  %s xmm0, xmm1\n", op->comi);
    printf("  setg al\n");
    printf("  movzx %s, al\n", r->rax);
    emit_push("rax");
    break;
  }
  case EX_LTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    printf("  %s xmm0, xmm1\n", op->comi);
    printf("  setle al\n");
    printf("  movzx %s, al\n", r->rax);
    emit_push("rax");
    break;
  }
  case EX_GTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    printf("  %s xmm0, xmm1\n", op->comi);
    printf("  setge al\n");
    printf("  movzx %s, al\n", r->rax);
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

  printf("  fld TBYTE PTR [rsp + 16]\n");
  printf("  fld TBYTE PTR [rsp]\n");

  switch (expr->ty) {
  case EX_ADD:
    printf("  faddp st(1), st\n");
    printf("  fstp TBYTE PTR [rsp + 16]\n");
    emit_stack_add(16);
    break;
  case EX_SUB:
    printf("  fsubp st(1), st\n");
    printf("  fstp TBYTE PTR [rsp + 16]\n");
    emit_stack_add(16);
    break;
  case EX_MUL:
    printf("  fmulp st(1), st\n");
    printf("  fstp TBYTE PTR [rsp + 16]\n");
    emit_stack_add(16);
    break;
  case EX_DIV:
    printf("  fdivp st(1), st\n");
    printf("  fstp TBYTE PTR [rsp + 16]\n");
    emit_stack_add(16);
    break;
  case EX_EQEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    printf("  fcomip st, st(1)\n");
    printf("  fstp st(0)\n");
    printf("  sete al\n");
    printf("  setnp dil\n");
    printf("  and al, dil\n");
    printf("  movzx %s, al\n", r->rax);
    emit_stack_add(32);
    emit_push("rax");
    break;
  }
  case EX_NOTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    printf("  fcomip st, st(1)\n");
    printf("  fstp st(0)\n");
    printf("  setne al\n");
    printf("  setp dil\n");
    printf("  or al, dil\n");
    printf("  movzx %s, al\n", r->rax);
    emit_stack_add(32);
    emit_push("rax");
    break;
  }
  case EX_LT: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    printf("  fcomip st, st(1)\n");
    printf("  fstp st(0)\n");
    printf("  setl al\n");
    printf("  movzx %s, al\n", r->rax);
    emit_stack_add(32);
    emit_push("rax");
    break;
  }
  case EX_GT: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    printf("  fcomip st, st(1)\n");
    printf("  fstp st(0)\n");
    printf("  setg al\n");
    printf("  movzx %s, al\n", r->rax);
    emit_stack_add(32);
    emit_push("rax");
    break;
  }
  case EX_LTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    printf("  fcomip st, st(1)\n");
    printf("  fstp st(0)\n");
    printf("  setle al\n");
    printf("  movzx %s, al\n", r->rax);
    emit_stack_add(32);
    emit_push("rax");
    break;
  }
  case EX_GTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    printf("  fcomip st, st(1)\n");
    printf("  fstp st(0)\n");
    printf("  setge al\n");
    printf("  movzx %s, al\n", r->rax);
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

static void emit_stmt(Stmt *stmt) {
  const char *filename;
  int line;
  int column;
  range_get_start(stmt->range, &filename, &line, &column);
  printf("  # %s:%d:%d\n", filename, line, column);

  switch (stmt->ty) {
  case ST_NULL: {
    return;
  }
  case ST_EXPR: {
    int base_stack_pos = stack_pos;
    emit_expr(stmt->expr);

    // 式の評価結果としてスタックに一つの値が残っている
    // はずなので、スタックが溢れないようにポップしておく
    if (stmt->expr->val_type->ty != TY_VOID) {
      int size = get_val_size(stmt->expr->val_type, stmt->expr->range);
      emit_stack_add(align(size, 8));
    }
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    return;
  }
  case ST_COMPOUND: {
    int base_stack_pos = stack_pos;
    for (int i = 0; i < vec_len(stmt->stmts); i++) {
      emit_stmt(vec_get(stmt->stmts, i));
    }
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
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

    printf("  cmp %s, 0\n", r->rax);
    printf("  je %s\n", else_label);

    emit_stmt(stmt->then_stmt);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    printf("  jmp %s\n", end_label);

    printf("%s:\n", else_label);
    emit_stmt(stmt->else_stmt);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");

    printf("%s:\n", end_label);
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
      printf("  cmp %s, %s\n", r->rax, r->rdi);
      printf("  je %s\n", case_expr->label);
      emit_push("rdi");
    }
    emit_pop("rdi");
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    if (stmt->default_case) {
      printf("  jmp %s\n", stmt->default_case->label);
    } else {
      printf("  jmp %s\n", end_label);
    }
    vec_push(break_labels, end_label);
    emit_stmt(stmt->body);
    vec_pop(break_labels);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    printf("%s:", end_label);
    return;
  }
  case ST_CASE:
  case ST_DEFAULT:
  case ST_LABEL: {
    printf("%s:\n", stmt->label);
    emit_stmt(stmt->body);
    return;
  }
  case ST_WHILE: {
    int base_stack_pos = stack_pos;
    char *cond_label = make_label("while.cond");
    char *end_label = make_label("while.end");

    printf("%s:\n", cond_label);
    const Reg *r = get_int_reg(stmt->cond->val_type, stmt->cond->range);
    emit_expr(stmt->cond);
    emit_pop("rax");
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    printf("  cmp %s, 0\n", r->rax);
    printf("  je %s\n", end_label);

    vec_push(break_labels, end_label);
    vec_push(continue_labels, cond_label);
    emit_stmt(stmt->body);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    vec_pop(break_labels);
    vec_pop(continue_labels);

    printf("  jmp %s\n", cond_label);
    printf("%s:\n", end_label);
    return;
  }
  case ST_DO_WHILE: {
    int base_stack_pos = stack_pos;
    char *loop_label = make_label("do_while.loop");
    char *cond_label = make_label("do_while.cond");
    char *end_label = make_label("do_while.end");
    printf("%s:\n", loop_label);

    vec_push(break_labels, end_label);
    vec_push(continue_labels, cond_label);
    emit_stmt(stmt->body);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    vec_pop(break_labels);
    vec_pop(continue_labels);

    printf("%s:\n", cond_label);
    const Reg *r = get_int_reg(stmt->cond->val_type, stmt->cond->range);
    emit_expr(stmt->cond);
    emit_pop("rax");
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");

    printf("  cmp %s, 0\n", r->rax);
    printf("  jne %s\n", loop_label);

    printf("%s:\n", end_label);
    return;
  }
  case ST_FOR: {
    int base_stack_pos = stack_pos;
    char *cond_label = make_label("for.cond");
    char *inc_label = make_label("for.inc");
    char *end_label = make_label("for.end");
    if (stmt->init != NULL) {
      emit_expr(stmt->init);
      emit_pop("rax");
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
    }
    printf("%s:\n", cond_label);
    if (stmt->cond != NULL) {
      const Reg *r = get_int_reg(stmt->cond->val_type, stmt->cond->range);
      emit_expr(stmt->cond);
      emit_pop("rax");
      printf("  cmp %s, 0\n", r->rax);
      printf("  je %s\n", end_label);
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
    }

    vec_push(break_labels, end_label);
    vec_push(continue_labels, inc_label);
    emit_stmt(stmt->body);
    vec_pop(break_labels);
    vec_pop(continue_labels);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");

    printf("%s:\n", inc_label);
    if (stmt->inc != NULL) {
      emit_expr(stmt->inc);
      emit_pop("rax");
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
    }
    printf("  jmp %s\n", cond_label);
    printf("%s:\n", end_label);
    return;
  }
  case ST_GOTO: {
    char *label = get_label(func_ctxt, stmt->name);
    if (label == NULL) {
      range_error(stmt->range, "未知のラベルへのgotoです: %s", stmt->name);
    }
    printf("  jmp %s\n", label);
    return;
  }
  case ST_BREAK: {
    if (vec_len(break_labels) <= 0) {
      range_error(stmt->range,
                  "ループでもswitch文中でもない箇所にbreakがあります");
    }
    printf("  jmp %s\n", (char *)vec_last(break_labels));
    return;
  }
  case ST_CONTINUE: {
    if (vec_len(continue_labels) <= 0) {
      range_error(stmt->range, "ループ中でない箇所にcontinueがあります");
    }
    printf("  jmp %s\n", (char *)vec_last(continue_labels));
    return;
  }
  case ST_RETURN: {
    int base_stack_pos = stack_pos;
    if (stmt->expr != NULL) {
      Type *ret_type = func_ctxt->type->func_ret;
      emit_expr(stmt->expr);

      int int_reg_idx = 0;
      int sse_reg_idx = 0;
      int size = get_val_size(ret_type, func_ctxt->range);
      arg_class_t class = classify_arg_type(ret_type, func_ctxt->range,
                                            &int_reg_idx, &sse_reg_idx);
      switch (class) {
      case ARG_CLASS_MEMORY:
      case ARG_CLASS_X87:
        // 戻り値を格納するアドレス
        printf("  mov rax, [rbp - %d]\n", retval_pos);
        int copy_size = 0;
        while (size - copy_size > 0) {
          const Reg *r = get_int_reg_for_copy(size - copy_size);
          printf("  mov %s, [rsp + %d]\n", r->rdi, copy_size);
          printf("  mov [rax + %d], %s\n", copy_size, r->rdi);
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
      }
    }
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    printf("  jmp %s\n", epilogue_label);
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
    printf(".global %s\n", func->name);
  }
  printf("%s:\n", func->name);

  // プロローグ
  // スタックサイズ分の領域を確保する
  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");

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
    printf("  # %s:%d:%d\n", filename, line, column);
    printf("  # [rbp - %d]: svar %s (%s:%d:%d)\n", svar->offset, svar->name,
           filename, line, column);
  }

  stack_pos = 0;
  int reg_save_area_size = 0;
  if (func->type->func_has_varargs) {
    reg_save_area_size += 8 * NUM_INT_REG + 16 * NUM_SSE_REG;
  }
  emit_stack_sub(align(stack_size, 16) + align(reg_save_area_size, 16));
  if (func->type->func_has_varargs) {
    reg_save_area_pos = stack_pos;
    printf("  # [rbp - %d]: reg_save_area\n", stack_pos);

    for (int i = 0; i < NUM_INT_REG; i++) {
      printf("  mov [rbp - %d], %s\n", stack_pos - 8 * i,
             get_int_arg_reg(&Reg8, i));
    }
    const char *end_label = make_label("skip_float_reg");
    printf("  test al, al\n");
    printf("  je %s\n", end_label);
    for (int i = 0; i < NUM_SSE_REG; i++) {
      printf("  movaps [rbp - %d], xmm%d\n",
             stack_pos - 8 * NUM_INT_REG - 16 * i, i);
    }
    printf("%s:\n", end_label);
  }

  int arg_stack_offset = 16;
  int ret_stack_offset = 0;
  int int_reg_idx = 0;
  int sse_reg_idx = 0;
  if (func->type->func_ret->ty != TY_VOID) {
    int num_int_reg = 0;
    int num_sse_reg = 0;
    arg_class_t class = classify_arg_type(func->type->func_ret, func->range,
                                          &num_int_reg, &num_sse_reg);
    if (class == ARG_CLASS_MEMORY || class == ARG_CLASS_X87) {
      // 戻り値をメモリで返す場合は、格納先のポインタをpushしておく
      emit_push("rdi");
      int_reg_idx++;
      ret_stack_offset = 8;
      retval_pos = stack_pos;
    }
  }

  // 引数をスタックへコピー
  if (func->type->func_param != NULL) {
    IntVector *param_class =
        classify_param(func->type->func_param, int_reg_idx);
    for (int i = 0; i < vec_len(func->type->func_param); i++) {
      Param *param = vec_get(func->type->func_param, i);
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
          printf("  mov %s, [rbp + %d]\n", r->rax,
                 arg_stack_offset + copy_size);
          printf("  mov [rbp - %d], %s\n", dst_offset - copy_size, r->rax);
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
              printf("  mov [rbp - %d], %s\n", dst_offset - j * 8 - copy_size,
                     get_int_arg_reg(&Reg8, int_reg_idx));
              rest_size -= 8;
              copy_size += 8;
              break;
            }
            if (rest_size >= 4) {
              printf("  mov [rbp - %d], %s\n", dst_offset - j * 8 - copy_size,
                     get_int_arg_reg(&Reg4, int_reg_idx));
              rest_size -= 4;
              copy_size += 4;
              if (rest_size > 0) {
                printf("  shr %s, 32\n", get_int_arg_reg(&Reg8, int_reg_idx));
              }
              continue;
            }
            if (rest_size >= 2) {
              printf("  mov [rbp - %d], %s\n", dst_offset - j * 8 - copy_size,
                     get_int_arg_reg(&Reg2, int_reg_idx));
              rest_size -= 2;
              copy_size += 2;
              if (rest_size > 0) {
                printf("  shr %s, 16\n", get_int_arg_reg(&Reg8, int_reg_idx));
              }
              continue;
            }

            assert(rest_size == 1);
            printf("  mov [rbp - %d], %s\n", dst_offset - j * 8 - copy_size,
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
          printf("  movsd [rbp - %d], xmm%d\n", dst_offset, sse_reg_idx);
        } else {
          assert(size == 4);
          printf("  movss [rbp - %d], xmm%d\n", dst_offset, sse_reg_idx);
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

  printf("  # reg_gp_offset: %d\n", reg_gp_offset);
  printf("  # reg_fp_offset: %d\n", reg_fp_offset);
  printf("  # overflow_arg_area: [rbp + %d]\n", overflow_arg_area_offset);
  printf("  # %s body\n", func->name);

  emit_stmt(func->body);

  // main関数の場合、returnなしに関数末尾まで到達した場合、戻り値は0にする
  if (strcmp(func->name, "main") == 0) {
    printf("mov eax, 0\n");
  }

  // エピローグ
  // 最後の式の結果がRAXに残っているのでそれが返り値になる
  assert(stack_pos == align(stack_size, 16) + align(reg_save_area_size, 16) +
                          ret_stack_offset);
  printf("%s:\n", epilogue_label);
  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
  stack_pos = INVALID_STACK_POS;
}

static void emit_gvar_init(Initializer *init, const Range *range,
                           Vector *gvar_list) {
  if (init->expr != NULL) {
    Expr *expr = init->expr;
    if (expr->ty == EX_NUM) {
      switch (expr->val_type->ty) {
      case TY_BOOL:
        printf("  .byte %d\n", expr->num.bool_val);
        break;
      case TY_CHAR:
        printf("  .byte %hhd\n", expr->num.char_val);
        break;
      case TY_S_CHAR:
        printf("  .byte %hhd\n", expr->num.s_char_val);
        break;
      case TY_S_SHORT:
        printf("  .word %hd\n", expr->num.s_short_val);
        break;
      case TY_S_INT:
        printf("  .long %d\n", expr->num.s_int_val);
        break;
      case TY_S_LONG:
        printf("  .quad %ld\n", expr->num.s_long_val);
        break;
      case TY_S_LLONG:
        printf("  .quad %lld\n", expr->num.s_llong_val);
        break;
      case TY_U_CHAR:
        printf("  .byte %hhu\n", expr->num.u_char_val);
        break;
      case TY_U_SHORT:
        printf("  .word %hu\n", expr->num.u_short_val);
        break;
      case TY_FLOAT:
      case TY_U_INT:
        printf("  .long %u\n", expr->num.u_int_val);
        break;
      case TY_DOUBLE:
      case TY_U_LONG:
        printf("  .quad %lu\n", expr->num.u_long_val);
        break;
      case TY_U_LLONG:
        printf("  .quad %llu\n", expr->num.u_llong_val);
        break;
      case TY_PTR:
        printf("  .quad %" PRIdPTR "\n", expr->num.ptr_val);
        break;
      case TY_ENUM:
        printf("  .long %d\n", expr->num.enum_val);
        break;
      case TY_LDOUBLE:
        printf("  .quad %lu\n", expr->num.bytes[0]);
        printf("  .quad %lu\n", expr->num.bytes[1]);
        break;
      case TY_VOID:
      case TY_ARRAY:
      case TY_FUNC:
      case TY_STRUCT:
      case TY_UNION:
      case TY_BUILTIN:
        range_error(range, "int型ではありません");
      }
    } else if (expr->ty == EX_ADDRESS) {
      if (expr->unop.operand->ty == EX_GLOBAL_VAR) {
        Expr *gvar = expr->unop.operand;
        printf("  .quad %s\n", gvar->global_var.name);
        return;
      }
      if (expr->unop.operand->ty == EX_COMPOUND) {
        Expr *compound = expr->unop.operand;
        GlobalVar *gvar = NEW(GlobalVar);
        gvar->name = make_label("compound");
        gvar->type = compound->val_type;
        gvar->range = compound->range;
        gvar->storage_class.is_static = true;
        gvar->init = compound->compound;
        vec_push(gvar_list, gvar);

        printf("  .quad %s\n", gvar->name);
        return;
      }
      range_error(
          range,
          "グローバル変数またはコンパウンドリテラル以外へのポインタです");
    } else if (expr->ty == EX_COMPOUND) {
      emit_gvar_init(expr->compound, expr->range, gvar_list);
    } else if (expr->ty == EX_STR) {
      printf("  .quad %s\n", expr->str);
    } else {
      range_error(range, "数値でもポインタでもありません: %d", expr->ty);
    }
    return;
  }

  if (init->members != NULL) {
    assert(init->type->ty == TY_STRUCT || init->type->ty == TY_UNION);
    assert(map_size(init->members) <= 1 || init->type->ty == TY_STRUCT);
    StructBody *body = init->type->struct_body;
    int offset = 0;
    for (int i = 0; i < map_size(init->members); i++) {
      Initializer *meminit = map_get_by_index(init->members, i, NULL);
      if (meminit == NULL) {
        continue;
      }
      if (i > 0) {
        Member *member = vec_get(body->member_list, i);
        if (offset < member->offset) {
          printf("  .zero %d\n", member->offset - offset);
          offset = member->offset;
        }
        assert(offset == member->offset);
      }
      emit_gvar_init(meminit, range, gvar_list);
      offset += get_val_size(meminit->type, range);
    }
    int ty_size = get_val_size(init->type, range);
    if (offset < ty_size) {
      printf("  .zero %d\n", ty_size - offset);
      offset = ty_size;
    }
    assert(offset == ty_size);
    return;
  }

  if (init->elements != NULL) {
    assert(init->type->ty == TY_ARRAY);
    for (int i = 0; i < vec_len(init->elements); i++) {
      Initializer *meminit = vec_get(init->elements, i);
      if (meminit == NULL) {
        printf("  .zero %d\n", get_val_size(init->type->ptrof, range));
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
    printf("  .bss\n");
    if (!gvar->storage_class.is_static) {
      printf(".global %s\n", gvar->name);
    }
    printf("%s:\n", gvar->name);
    printf("  .zero %d\n", get_val_size(gvar->type, gvar->range));
  } else {
    printf("  .data\n");
    if (!gvar->storage_class.is_static) {
      printf(".global %s\n", gvar->name);
    }
    printf("%s:\n", gvar->name);
    emit_gvar_init(init, gvar->range, gvar_list);
  }
}

static void emit_str(StringLiteral *str) {
  printf("%s:\n", str->name);
  printf("  .string ");
  print_string_literal(str->val);
  printf("\n");
}

void gen(TranslationUnit *tunit) {
  printf(".intel_syntax noprefix\n");

  printf("  .text\n");
  for (int i = 0; i < vec_len(tunit->func_list); i++) {
    emit_func(vec_get(tunit->func_list, i));
  }
  while (vec_len(tunit->gvar_list) > 0) {
    GlobalVar *gvar = vec_remove(tunit->gvar_list, 0);
    emit_gvar(gvar, tunit->gvar_list);
  }
  printf("  .section .rodata\n");
  for (int i = 0; i < vec_len(tunit->str_list); i++) {
    emit_str(vec_get(tunit->str_list, i));
  }
}
