#include "gifcc.h"
#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#define INVALID_STACK_POS INT_MIN

#define REG(str)                                                               \
  { .type = OP_REG, .reg = (str) }

typedef enum {
  ARG_CLASS_MEMORY,
  ARG_CLASS_INTEGER,
  ARG_CLASS_SSE,
  ARG_CLASS_X87,
} arg_class_t;

typedef DEFINE_VECTOR(ArgClassVector, int) ArgClassVector;

typedef struct {
  int size;
  char suffix;
} RegSize;

typedef struct {
  RegSize size;
  const char *reg;
  int offset;
  const char *symbol;
} Addr;

typedef struct {
  enum { OP_ADDR, OP_REG, OP_NUM, OP_SYMBOL } type;
  Addr addr;
  const char *reg;
  Number num;
  const char *symbol;
} Operand;

typedef struct {
  RegSize s;
  RegSize s_x87;
  Operand rax;
  Operand rdi;
  Operand rsi;
  Operand rdx;
  Operand rcx;
  Operand r8;
  Operand r9;
  Operand r10;
  Operand r11;
} Reg;

static const RegSize RegSizeAuto = {.size = 0, .suffix = '\0'};

const struct {
  Operand rbp;
  Operand rsp;
  Operand rip;
} R = {
    .rbp = REG("rbp"),
    .rsp = REG("rsp"),
    .rip = REG("rip"),
};

const Operand SseReg[] = {
    REG("xmm0"), REG("xmm1"), REG("xmm2"), REG("xmm3"),
    REG("xmm4"), REG("xmm5"), REG("xmm6"), REG("xmm7"),
};
const Operand X87Reg[] = {
    REG("st(0)"), REG("st(1)"), REG("st(2)"), REG("st(3)"),
    REG("st(4)"), REG("st(5)"), REG("st(6)"), REG("st(7)"),
};

const Reg Reg8 = {
    .s = {.size = 8, .suffix = 'q'},
    .s_x87 = {.size = 8, .suffix = 'q'},
    .rax = REG("rax"),
    .rdi = REG("rdi"),
    .rsi = REG("rsi"),
    .rdx = REG("rdx"),
    .rcx = REG("rcx"),
    .r8 = REG("r8"),
    .r9 = REG("r9"),
    .r10 = REG("r10"),
    .r11 = REG("r11"),
};
const Reg Reg4 = {
    .s = {.size = 4, .suffix = 'l'},
    .s_x87 = {.size = 4, .suffix = 'l'},
    .rax = REG("eax"),
    .rdi = REG("edi"),
    .rsi = REG("esi"),
    .rdx = REG("edx"),
    .rcx = REG("ecx"),
    .r8 = REG("r8d"),
    .r9 = REG("r9d"),
    .r10 = REG("r10d"),
    .r11 = REG("r11d"),
};
const Reg Reg2 = {
    .s = {.size = 2, .suffix = 'w'},
    .s_x87 = {.size = 2, .suffix = 's'},
    .rax = REG("ax"),
    .rdi = REG("di"),
    .rsi = REG("si"),
    .rdx = REG("dx"),
    .rcx = REG("cx"),
    .r8 = REG("r8w"),
    .r9 = REG("r9w"),
    .r10 = REG("r10w"),
    .r11 = REG("r11w"),
};
const Reg Reg1 = {
    .s = {.size = 1, .suffix = 'b'},
    .s_x87 = {.size = 1, .suffix = '\0'},
    .rax = REG("al"),
    .rdi = REG("dil"),
    .rsi = REG("sil"),
    .rdx = REG("dl"),
    .rcx = REG("cl"),
    .r8 = REG("r8b"),
    .r9 = REG("r9b"),
    .r10 = REG("r10b"),
    .r11 = REG("r11b"),
};

typedef struct {
  RegSize s;
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
    .s = {.size = 4, .suffix = 's'},
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
    .s = {.size = 8, .suffix = 'l'},
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

const RegSize X87Size = {.size = 10, .suffix = 't'};

const int NUM_INT_REG = 6;
const int NUM_SSE_REG = 8;

static asm_syntax_t asm_syntax;
static FILE *output_fp = NULL;
static char *epilogue_label = NULL;
static Function *func_ctxt = NULL;
static StrVector *break_labels = NULL;
static StrVector *continue_labels = NULL;
static int stack_pos = INVALID_STACK_POS;
static int retval_pos = INVALID_STACK_POS;
static int reg_save_area_pos = INVALID_STACK_POS;
static int reg_gp_offset = INVALID_STACK_POS;
static int reg_fp_offset = INVALID_STACK_POS;
static int overflow_arg_area_offset = INVALID_STACK_POS;

static __attribute__((format(printf, 1, 2))) void emit(const char *fmt, ...);
static __attribute__((format(printf, 1, 2))) void emit_comment(const char *fmt,
                                                               ...);
static void emit_assign(Type *type, const Range *range, Expr *dest, Expr *src);
static void emit_expr(Expr *expr);
static arg_class_t classify_arg_type(const Type *type, const Range *range,
                                     int *num_int, int *num_sse);
static ArgClassVector *classify_arg(const ExprVector *args, int int_reg_idx);
static void emit_expr_num(Expr *expr);
static void emit_expr_stack_var(Expr *expr);
static void emit_expr_global_var(Expr *expr);
static void emit_expr_str(Expr *expr);
static void emit_expr_compound(Expr *expr);
static void emit_expr_stmt(Expr *expr);
static void emit_expr_call(Expr *expr);
static void emit_expr_dot(Expr *expr);
static void emit_expr_arrow(Expr *expr);
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
                           GlobalVarVector *gvar_list);
static void emit_gvar(GlobalVar *gvar, GlobalVarVector *gvar_list);

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

static Operand get_int_arg_reg(const Reg *r, int idx) {
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
  internal_error("Invalid int reg index: %d", idx);
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

static char *num2str(Number num) {
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
  internal_error("Invalid number type: %d", num.type);
}

static Operand sized_addr2(RegSize size, Operand reg, int offset) {
  assert(reg.type == OP_REG);
  return (Operand){.type = OP_ADDR, .addr = {.size = size, reg.reg, offset}};
}
static Operand sized_addr(RegSize size, Operand reg) {
  return sized_addr2(size, reg, 0);
}
static Operand addr2(Operand reg, int offset) {
  return sized_addr2(RegSizeAuto, reg, offset);
}
static Operand addr(Operand reg) { return sized_addr(RegSizeAuto, reg); }
static Operand symbol_addr2(const char *symbol, int offset) {
  return (Operand){.type = OP_ADDR,
                   .addr = {.size = RegSizeAuto,
                            .reg = R.rip.reg,
                            .offset = offset,
                            .symbol = symbol}};
}
static Operand symbol_addr(const char *symbol) {
  return symbol_addr2(symbol, 0);
}

static Operand imm_num(Number num) {
  return (Operand){.type = OP_NUM, .num = num};
}
static Operand imm_int(int n) { return imm_num(new_number_int(n)); }
static Operand imm_symbol(const char *symbol) {
  return (Operand){.type = OP_SYMBOL, .symbol = symbol};
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

static const char *format_operand_intel(Operand opr) {
  switch (opr.type) {
  case OP_ADDR: {
    Addr addr = opr.addr;
    const char *s1 = "", *s2 = "", *s3 = "";
    switch (addr.size.size) {
    case 0:
      break;
    case 1:
      s1 = "BYTE PTR ";
      break;
    case 2:
      s1 = "WORD PTR ";
      break;
    case 4:
      s1 = "DWORD PTR";
      break;
    case 8:
      s1 = "QWORD PTR";
      break;
    case 10:
      s1 = "TBYTE PTR";
      break;
    default:
      assert(false);
    }
    if (addr.symbol != NULL) {
      s2 = addr.symbol;
      s3 = " ";
    }
    if (addr.offset > 0) {
      return format("%s%s%s[%s + %d]", s1, s2, s3, addr.reg, addr.offset);
    }
    if (addr.offset < 0) {
      return format("%s%s%s[%s - %d]", s1, s2, s3, addr.reg, -addr.offset);
    }
    return format("%s%s%s[%s]", s1, s2, s3, addr.reg);
  }
  case OP_REG:
    return opr.reg;
  case OP_NUM:
    return num2str(opr.num);
  case OP_SYMBOL:
    return opr.symbol;
  }
  assert(false);
}

static const char *format_operand_att(Operand opr) {
  switch (opr.type) {
  case OP_ADDR: {
    Addr addr = opr.addr;
    if (addr.offset > 0) {
      if (addr.symbol != NULL) {
        return format("%s+%d(%%%s)", addr.symbol, addr.offset, addr.reg);
      }
      return format("%d(%%%s)", addr.offset, addr.reg);
    }
    if (addr.offset < 0) {
      if (addr.symbol != NULL) {
        return format("%s-%d(%%%s)", addr.symbol, -addr.offset, addr.reg);
      }
      return format("-%d(%%%s)", -addr.offset, addr.reg);
    }
    if (addr.symbol != NULL) {
      return format("%s(%%%s)", addr.symbol, addr.reg);
    }
    return format("(%%%s)", addr.reg);
  }
  case OP_REG:
    return format("%%%s", opr.reg);
  case OP_NUM:
    return format("$%s", num2str(opr.num));
  case OP_SYMBOL:
    return opr.symbol;
  }
  assert(false);
}

static const char *format_operand(Operand opr) {
  if (asm_syntax == ASM_SYNTAX_INTEL) {
    return format_operand_intel(opr);
  }
  return format_operand_att(opr);
}

static const char *operand2suffix(Operand opr) {
  if (opr.type != OP_ADDR) {
    return "";
  }
  Addr addr = opr.addr;
  if (addr.size.suffix == '\0') {
    return "";
  }
  return format("%c", addr.size.suffix);
}

static void emit_op0(const char *op) { fprintf(output_fp, "  %s\n", op); }
static void emit_op1(const char *op, Operand opr1) {
  if (asm_syntax == ASM_SYNTAX_INTEL) {
    fprintf(output_fp, "  %s %s\n", op, format_operand(opr1));
  } else {
    const char *s1 = operand2suffix(opr1);
    fprintf(output_fp, "  %s%s %s\n", op, s1, format_operand(opr1));
  }
}
static void emit_op2(const char *op, Operand opr1, Operand opr2) {
  if (asm_syntax == ASM_SYNTAX_INTEL) {
    fprintf(output_fp, "  %s %s, %s\n", op, format_operand(opr1),
            format_operand(opr2));
  } else {
    const char *s1 = operand2suffix(opr1);
    const char *s2 = operand2suffix(opr2);
    fprintf(output_fp, "  %s%s%s %s, %s\n", op, s2, s1, format_operand(opr2),
            format_operand(opr1));
  }
}

static void emit_label(const char *label) {
  fprintf(output_fp, "%s:\n", label);
}

static __attribute__((format(printf, 1, 2))) void emit_comment(const char *fmt,
                                                               ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(output_fp, "  # ");
  vfprintf(output_fp, fmt, ap);
  fprintf(output_fp, "\n");
  va_end(ap);
}

static void emit_stack_indicator(void) {
  emit_comment("=> %s = %s", format_operand(R.rsp),
               format_operand(addr2(R.rbp, -stack_pos)));
}

static void emit_stack_sub(int size) {
  stack_pos += size;
  assert(stack_pos >= 0);
  emit_op2("sub", R.rsp, imm_int(size));
  emit_stack_indicator();
}
static void emit_stack_add(int size) {
  stack_pos -= size;
  assert(stack_pos >= 0);
  emit_op2("add", R.rsp, imm_int(size));
  emit_stack_indicator();
}

static void emit_pop(Operand operand) {
  stack_pos -= 8;
  assert(stack_pos >= 0);
  emit_op1("pop", operand);
  emit_stack_indicator();
}
static void emit_push(Operand operand) {
  stack_pos += 8;
  assert(stack_pos >= 0);
  emit_op1("push", operand);
  emit_stack_indicator();
}

static void emit_pop_xmm(int n) {
  emit_op2("movsd", SseReg[n], addr(R.rsp));
  emit_stack_add(8);
}

static void emit_push_xmm(int n) {
  emit_stack_sub(8);
  emit_op2("movsd", addr(R.rsp), SseReg[n]);
}

static void emit_push_stack_var(StackVar *svar, Type *type, int offset) {
  int size = get_val_size(type, svar->range);
  emit_stack_sub(align(size, 8));

  int src_offset = svar->offset;
  int copy_size = 0;
  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    emit_op2("mov", r->rax, addr2(R.rbp, -src_offset + copy_size + offset));
    emit_op2("mov", addr2(R.rsp, copy_size), r->rax);
    copy_size += r->s.size;
  }
}

static void emit_lval(Expr *expr, Operand reg) {
  if (expr->ty == EX_STACK_VAR) {
    StackVar *var = expr->stack_var.def;
    assert(var != NULL);
    emit_op2("lea", reg, addr2(R.rbp, -var->offset + expr->stack_var.offset));
    return;
  }
  if (expr->ty == EX_GLOBAL_VAR) {
    emit_op2("lea", reg,
             symbol_addr2(expr->global_var.name, expr->global_var.offset));
    return;
  }
  if (expr->ty == EX_INDIRECT) {
    emit_expr(expr->unop.operand);
    emit_pop(reg);
    return;
  }
  if (expr->ty == EX_DOT) {
    emit_lval(expr->dot.operand, reg);
    int offset = get_members_offset(expr->dot.members);
    emit_op2("lea", reg, addr2(reg, offset));
    return;
  }
  if (expr->ty == EX_ARROW) {
    emit_expr(expr->arrow.operand);
    emit_pop(Reg8.rax);
    int offset = get_members_offset(expr->dot.members);
    emit_op2("lea", reg, addr2(Reg8.rax, offset));
    return;
  }
  if (expr->ty == EX_COMMA) {
    ExprVector *exprs = expr->comma.exprs;
    for (int i = 0; i < VEC_LEN(exprs); i++) {
      Expr *op = VEC_GET(exprs, i);
      if (i != VEC_LEN(exprs) - 1) {
        emit_expr(op);
        int op_size = get_val_size(op->val_type, op->range);
        emit_stack_add(align(op_size, 8));
        continue;
      }
      emit_lval(op, reg);
    }
    return;
  }
  if (expr->ty == EX_COMPOUND) {
    StackVar *svar = expr->compound.stack_var;
    Initializer *init = expr->compound.init;
    emit_svar_zero(svar);
    emit_svar_init(svar, 0, init, svar->range);
    emit_op2("lea", reg, addr2(R.rbp, -svar->offset));
    return;
  }

  range_error(expr->range, "Invalid lvalue: %d", expr->ty);
}

static void emit_assign(Type *type, const Range *range, Expr *dest, Expr *src) {
  emit_expr(src);
  emit_lval(dest, Reg8.rax);

  int size = get_val_size(type, range);
  int copy_size = 0;
  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    emit_op2("mov", r->rdi, addr2(R.rsp, copy_size));
    emit_op2("mov", addr2(Reg8.rax, copy_size), r->rdi);
    copy_size += r->s.size;
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
  case EX_ARROW:
    emit_expr_arrow(expr);
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
    emit_lval(expr->unop.operand, Reg8.rax);
    emit_push(Reg8.rax);
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

static ArgClassVector *classify_arg(const ExprVector *args, int int_reg_idx) {
  ArgClassVector *class = NEW_VECTOR(ArgClassVector);
  int num_int_reg = int_reg_idx;
  int num_sse_reg = 0;
  for (int i = 0; i < VEC_LEN(args); i++) {
    Expr *expr = VEC_GET(args, i);
    VEC_PUSH(class, classify_arg_type(expr->val_type, expr->range, &num_int_reg,
                                      &num_sse_reg));
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
    emit_push(imm_num(expr->num));
  } else if (size <= 8) {
    const Reg *r = get_int_reg_for_size(size, expr->range);
    emit_op2("mov", r->rax, imm_num(expr->num));
    emit_push(Reg8.rax);
  } else {
    assert(is_x87_reg_type(expr->val_type));
    assert(size == 16);
    emit_op2("mov", Reg8.rax, imm_num(new_number_uint64(expr->num.bytes[0])));
    emit_op2("mov", addr2(R.rsp, -16), Reg8.rax);
    emit_op2("mov", Reg8.rax, imm_num(new_number_uint64(expr->num.bytes[1])));
    emit_op2("mov", addr2(R.rsp, -8), Reg8.rax);
    emit_stack_sub(16);
  }
}

static void emit_expr_stack_var(Expr *expr) {
  assert(expr->ty == EX_STACK_VAR);
  emit_push_stack_var(expr->stack_var.def, expr->val_type,
                      expr->stack_var.offset);
}

static void emit_expr_global_var(Expr *expr) {
  assert(expr->ty == EX_GLOBAL_VAR);

  const char *name = expr->global_var.name;
  int size = get_val_size(expr->val_type, expr->range);
  emit_stack_sub(align(size, 8));

  int copy_size = 0;

  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    emit_op2("mov", r->rax,
             symbol_addr2(name, expr->global_var.offset + copy_size));
    emit_op2("mov", addr2(R.rsp, copy_size), r->rax);
    copy_size += r->s.size;
  }
}

static void emit_expr_str(Expr *expr) {
  assert(expr->ty == EX_STR);

  emit_op2("lea", Reg8.rax, symbol_addr(expr->str->name));
  emit_push(Reg8.rax);
}

static void emit_expr_compound(Expr *expr) {
  assert(expr->ty == EX_COMPOUND);
  assert(expr->compound.stack_var != NULL);

  StackVar *svar = expr->compound.stack_var;
  Initializer *init = expr->compound.init;
  emit_svar_zero(svar);
  emit_svar_init(svar, 0, init, svar->range);

  emit_push_stack_var(svar, expr->val_type, 0);
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
  ExprVector *args = expr->call.arguments;
  ArgClassVector *arg_class =
      args != NULL ? classify_arg(args, int_reg_idx) : NULL;

  int arg_size = 0;
  if (args != NULL && VEC_LEN(args) > 0) {
    for (int i = VEC_LEN(args) - 1; i >= 0; i--) {
      Expr *expr = VEC_GET(args, i);
      int size = get_val_size(expr->val_type, expr->range);
      arg_class_t class = VEC_GET(arg_class, i);
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

  if (args != NULL && VEC_LEN(args) > 0) {
    // メモリ渡しする引数をスタックに積む
    for (int i = VEC_LEN(args) - 1; i >= 0; i--) {
      arg_class_t class = VEC_GET(arg_class, i);
      if (class == ARG_CLASS_MEMORY || class == ARG_CLASS_X87) {
        emit_expr(VEC_GET(args, i));
      }
    }
    // レジスタ渡しする引数をスタックに積む
    for (int i = VEC_LEN(args) - 1; i >= 0; i--) {
      arg_class_t class = VEC_GET(arg_class, i);
      switch (class) {
      case ARG_CLASS_X87:
      case ARG_CLASS_MEMORY:
        continue;
      case ARG_CLASS_INTEGER:
        emit_expr(VEC_GET(args, i));
        continue;
      case ARG_CLASS_SSE:
        emit_expr(VEC_GET(args, i));
        continue;
      }
      assert(false);
    }
  }

  if (!call_direct) {
    emit_expr(expr->call.callee);
    emit_pop(Reg8.r10);
  }

  if (args != NULL && VEC_LEN(args) > 0) {
    // レジスタ渡しする引数をpopしレジスタにセットする
    for (int i = 0; i < VEC_LEN(args); i++) {
      Expr *expr = VEC_GET(args, i);
      int size = get_val_size(expr->val_type, expr->range);

      arg_class_t class = VEC_GET(arg_class, i);
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
        if (functype->func.has_varargs && i >= VEC_LEN(functype->func.params)) {
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
    emit_op2("lea", Reg8.rdi, addr2(R.rsp, arg_size));
  }
  emit_op2("mov", Reg1.rax, imm_int(num_vararg_sse_reg));

  range_assert(expr->range, stack_pos % 16 == 0, "stack position mismatch");
  if (call_direct) {
    emit_op1("call", imm_symbol(expr->call.callee->global_var.name));
  } else {
    if (asm_syntax == ASM_SYNTAX_INTEL) {
      emit_op1("call", Reg8.r10);
    } else {
      emit_op1("call *", Reg8.r10);
    }
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
        emit_push(Reg8.rdx);
      }
      emit_push(Reg8.rax);
      break;
    case ARG_CLASS_SSE:
      assert(ret_size <= 8);
      emit_push_xmm(0);
      break;
    case ARG_CLASS_X87:
      emit_stack_sub(16);
      emit_op1("fstp", sized_addr(X87Size, R.rsp));
      break;
    }
  }
  return;
}

static void emit_expr_dot(Expr *expr) {
  assert(expr->ty == EX_DOT);

  Expr *operand = expr->dot.operand;
  int offset = get_members_offset(expr->arrow.members);
  int size = get_val_size(operand->val_type, operand->range);
  int mem_size = get_val_size(expr->val_type, expr->range);
  int size_diff = align(size, 8) - align(mem_size, 8);
  emit_expr(operand);

  int copy_size = 0;
  while (mem_size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(mem_size - copy_size);
    emit_op2("mov", r->rax, addr2(R.rsp, offset + copy_size));
    emit_op2("mov", addr2(R.rsp, -align(mem_size, 8) + copy_size), r->rax);
    copy_size += r->s.size;
  }
  copy_size = 0;
  while (mem_size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(mem_size - copy_size);
    emit_op2("mov", r->rax, addr2(R.rsp, -align(mem_size, 8) + copy_size));
    emit_op2("mov", addr2(R.rsp, size_diff + copy_size), r->rax);
    copy_size += r->s.size;
  }
  emit_stack_add(size_diff);
}

static void emit_expr_arrow(Expr *expr) {
  assert(expr->ty == EX_ARROW);

  Expr *operand = expr->arrow.operand;
  int offset = get_members_offset(expr->arrow.members);
  int mem_size = get_val_size(expr->val_type, expr->range);

  emit_expr(operand);
  emit_pop(Reg8.rax);

  int copy_size = 0;
  while (mem_size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(mem_size - copy_size);
    emit_op2("mov", r->rdi, addr2(Reg8.rax, offset + copy_size));
    emit_op2("mov", addr2(R.rsp, -align(mem_size, 8) + copy_size), r->rdi);
    copy_size += r->s.size;
  }

  emit_stack_sub(align(mem_size, 8));
}

static void emit_expr_comma(Expr *expr) {
  assert(expr->ty == EX_COMMA);

  ExprVector *exprs = expr->comma.exprs;
  for (int i = 0; i < VEC_LEN(exprs); i++) {
    Expr *op = VEC_GET(exprs, i);
    emit_expr(op);
    if (i != VEC_LEN(exprs) - 1) {
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
  emit_pop(Reg8.rax);
  emit_op2("cmp", r->rax, imm_int(0));
  emit_op1("je", imm_symbol(false_label));

  emit_expr(expr->binop.rhs);
  emit_pop(Reg8.rax);
  emit_op2("cmp", r->rax, imm_int(0));
  emit_op1("je", imm_symbol(false_label));

  emit_push(imm_int(1));
  emit_op1("jmp", imm_symbol(end_label));
  stack_pos -= 8;

  emit_label(false_label);
  emit_push(imm_int(0));

  emit_label(end_label);
}

static void emit_expr_log_or(Expr *expr) {
  assert(expr->ty == EX_LOG_OR);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  char *true_label = make_label("logor.true");
  char *end_label = make_label("logor.end");
  emit_expr(expr->binop.lhs);
  emit_pop(Reg8.rax);
  emit_op2("cmp", r->rax, imm_int(0));
  emit_op1("jne", imm_symbol(true_label));

  emit_expr(expr->binop.rhs);
  emit_pop(Reg8.rax);
  emit_op2("cmp", r->rax, imm_int(0));
  emit_op1("jne", imm_symbol(true_label));

  emit_push(imm_int(0));
  emit_op1("jmp", imm_symbol(end_label));
  stack_pos -= 8;

  emit_label(true_label);
  emit_push(imm_int(1));

  emit_label(end_label);
}

static void emit_expr_cond(Expr *expr) {
  assert(expr->ty == EX_COND);

  const Reg *r = get_int_reg(expr->cond.cond->val_type, expr->cond.cond->range);
  char *else_label = make_label("cond.else");
  char *end_label = make_label("cond.end");
  emit_expr(expr->cond.cond);
  emit_pop(Reg8.rax);
  emit_op2("cmp", r->rax, imm_int(0));
  emit_op1("je", imm_symbol(else_label));

  int cond_stack_pos = stack_pos;
  emit_expr(expr->cond.then_expr);
  emit_op1("jmp", imm_symbol(end_label));
#ifndef NDEBUG
  int end_stack_pos = stack_pos;
#endif

  stack_pos = cond_stack_pos;
  emit_label(else_label);
  emit_expr(expr->cond.else_expr);
  range_assert(expr->range, stack_pos == end_stack_pos,
               "stack position mismatch");

  emit_label(end_label);
}

static void emit_expr_indirect(Expr *expr) {
  assert(expr->ty == EX_INDIRECT);

  Expr *operand = expr->unop.operand;
  emit_expr(operand);
  emit_pop(Reg8.rax);

  int size = get_val_size(expr->val_type, expr->range);
  emit_stack_sub(align(size, 8));

  int copy_size = 0;
  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    emit_op2("mov", r->rdi, addr2(Reg8.rax, copy_size));
    emit_op2("mov", addr2(R.rsp, copy_size), r->rdi);
    copy_size += r->s.size;
  }
}

static void emit_expr_minus(Expr *expr) {
  assert(expr->ty == EX_MINUS);

  emit_expr(expr->unop.operand);

  if (is_int_reg_type(expr->val_type)) {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit_op1("neg", sized_addr(r->s, R.rsp));
    return;
  }

  if (is_sse_reg_type(expr->val_type)) {
    emit_pop_xmm(0);

    const SseOp *op = get_sse_op(expr->val_type, expr->range);
    if (expr->val_type->ty == TY_DOUBLE) {
      emit_op2("mov", sized_addr2(Reg4.s, R.rsp, -4),
               imm_num(new_number_uint64(0x80000000)));
      emit_op2("mov", sized_addr2(Reg4.s, R.rsp, -8),
               imm_num(new_number_uint64(0x00000000)));
    } else {
      assert(expr->val_type->ty == TY_FLOAT);
      emit_op2("mov", sized_addr2(Reg4.s, R.rsp, -4),
               imm_num(new_number_uint64(0x00000000)));
      emit_op2("mov", sized_addr2(Reg4.s, R.rsp, -8),
               imm_num(new_number_uint64(0x80000000)));
    }
    emit_op2("movsd", SseReg[1], addr2(R.rsp, -8));
    emit_op2(op->xor, SseReg[0], SseReg[1]);
    emit_push_xmm(0);
    return;
  }

  if (is_x87_reg_type(expr->val_type)) {
    emit_op1("fld", sized_addr(X87Size, R.rsp));
    emit_op0("fchs");
    emit_op1("fstp", sized_addr(X87Size, R.rsp));
    return;
  }

  range_internal_error(expr->range, "Invalid type: %s, op=%d",
                       format_type(expr->val_type, false), expr->ty);
}

static void emit_expr_not(Expr *expr) {
  assert(expr->ty == EX_NOT);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_expr(expr->unop.operand);
  emit_op1("not", sized_addr(r->s, R.rsp));
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
    int from_size = from->s.size;
    int to_size = to->s.size;
    if (to_size > from_size) {
      if (is_signed_int_type(operand->val_type, operand->range)) {
        emit_op2("movsx", to->rax, sized_addr(from->s, R.rsp));
      } else {
        if (from_size < 4) {
          emit_op2("movzx", to->rax, sized_addr(from->s, R.rsp));
        } else {
          emit_op2("mov", from->rax, sized_addr(from->s, R.rsp));
        }
      }
      emit_op2("mov", addr(R.rsp), Reg8.rax);
    }
    return;
  }

  if (is_sse_reg_type(operand->val_type) && is_int_reg_type(expr->val_type)) {
    const SseOp *op = get_sse_op(operand->val_type, operand->range);
    const Reg *to = get_int_reg(expr->val_type, expr->range);
    const Reg *conv = to;
    if (to->s.size < 4) {
      conv = &Reg4;
    }
    emit_pop_xmm(0);
    emit_op2(op->cvtt_to_si, conv->rax, SseReg[0]);
    emit_push(Reg8.rax);
    return;
  }

  if (is_int_reg_type(operand->val_type) && is_sse_reg_type(expr->val_type)) {
    const SseOp *op = get_sse_op(expr->val_type, expr->range);
    const Reg *from = get_int_reg(operand->val_type, operand->range);
    const Reg *conv = from;
    emit_pop(Reg8.rax);
    if (from->s.size < 4) {
      conv = &Reg4;
      if (is_signed_int_type(operand->val_type, operand->range)) {
        emit_op2("movsx", conv->rax, from->rax);
      } else {
        emit_op2("movzx", conv->rax, from->rax);
      }
    }
    emit_op2(op->cvtt_from_si, SseReg[0], conv->rax);
    emit_push_xmm(0);
    return;
  }

  if (is_sse_reg_type(operand->val_type) && is_sse_reg_type(expr->val_type)) {
    const SseOp *op = get_sse_op(operand->val_type, expr->range);
    emit_pop_xmm(0);
    if (expr->val_type->ty == TY_FLOAT) {
      emit_op2(op->cvt_to_ss, SseReg[0], SseReg[0]);
    } else if (expr->val_type->ty == TY_DOUBLE) {
      emit_op2(op->cvt_to_sd, SseReg[0], SseReg[0]);
    } else {
      goto CastError;
    }
    emit_push_xmm(0);
    return;
  }

  if (is_int_reg_type(operand->val_type) && is_x87_reg_type(expr->val_type)) {
    const Reg *from = get_int_reg(operand->val_type, operand->range);
    const Reg *conv = from;
    if (from->s.size < 2) {
      conv = &Reg2;
      if (is_signed_int_type(operand->val_type, operand->range)) {
        emit_op2("movsx", conv->rax, sized_addr(from->s, R.rsp));
      } else {
        emit_op2("movzx", conv->rax, sized_addr(from->s, R.rsp));
      }
      emit_op2("mov", addr(R.rsp), conv->rax);
    }
    emit_op1("fild", sized_addr(conv->s_x87, R.rsp));
    emit_op1("fstp", sized_addr2(X87Size, R.rsp, -8));
    emit_op2("mov", sized_addr2(Reg4.s, R.rsp, 4), imm_int(0));
    emit_stack_sub(8);
    return;
  }

  if (is_x87_reg_type(operand->val_type) && is_int_reg_type(expr->val_type)) {
    emit_op1("fld", sized_addr(X87Size, R.rsp));
    emit_op1("fisttp", sized_addr2(Reg8.s, R.rsp, 8));
    emit_stack_add(8);
    return;
  }

  if (is_sse_reg_type(operand->val_type) && is_x87_reg_type(expr->val_type)) {
    const SseOp *from_op = get_sse_op(operand->val_type, operand->range);
    emit_op1("fld", sized_addr(from_op->s, R.rsp));
    emit_op2("mov", sized_addr(Reg8.s, R.rsp), imm_int(0));
    emit_op1("fstp", sized_addr2(X87Size, R.rsp, -8));
    emit_stack_sub(8);
    return;
  }

  if (is_x87_reg_type(operand->val_type) && is_sse_reg_type(expr->val_type)) {
    const SseOp *to_op = get_sse_op(expr->val_type, expr->range);
    emit_op1("fld", sized_addr(X87Size, R.rsp));
    emit_op1("fstp", sized_addr2(to_op->s, R.rsp, 8));
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
  emit_lval(expr->unop.operand, Reg8.rax);
  emit_op2("mov", r->rdi, addr(Reg8.rax));
  emit_op2("add", r->rdi, imm_int(get_incdec_size(expr)));
  emit_op2("mov", addr(Reg8.rax), r->rdi);
  emit_push(Reg8.rdi);
}

static void emit_expr_pre_dec(Expr *expr) {
  assert(expr->ty == EX_PRE_DEC);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_lval(expr->unop.operand, Reg8.rax);
  emit_op2("mov", r->rdi, addr(Reg8.rax));
  emit_op2("sub", r->rdi, imm_int(get_incdec_size(expr)));
  emit_op2("mov", addr(Reg8.rax), r->rdi);
  emit_push(Reg8.rdi);
}

static void emit_expr_post_inc(Expr *expr) {
  assert(expr->ty == EX_POST_INC);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_lval(expr->unop.operand, Reg8.rax);
  emit_op2("mov", r->rdi, addr(Reg8.rax));
  emit_push(Reg8.rdi);
  emit_op2("add", r->rdi, imm_int(get_incdec_size(expr)));
  emit_op2("mov", addr(Reg8.rax), r->rdi);
}

static void emit_expr_post_dec(Expr *expr) {
  assert(expr->ty == EX_POST_DEC);

  const Reg *r = get_int_reg(expr->val_type, expr->range);
  emit_lval(expr->unop.operand, Reg8.rax);
  emit_op2("mov", r->rdi, addr(Reg8.rax));
  emit_push(Reg8.rdi);
  emit_op2("sub", r->rdi, imm_int(get_incdec_size(expr)));
  emit_op2("mov", addr(Reg8.rax), r->rdi);
}

static void emit_expr_builtin_va_start(Expr *expr) {
  assert(expr->ty == EX_BUILTIN_VA_START);

  Expr *ap = expr->builtin_va_start.ap;
  range_assert(expr->range, is_ptr_type(ap->val_type),
               "va_list is not pointer");
  emit_expr(ap);
  emit_pop(Reg8.rax);
  Operand gp_offset = sized_addr(Reg4.s, Reg8.rax);
  Operand fp_offset = sized_addr2(Reg4.s, Reg8.rax, 4);
  Operand overflow_arg_area = addr2(Reg8.rax, 8);
  Operand reg_save_area = addr2(Reg8.rax, 16);
  emit_op2("mov", gp_offset, imm_int(reg_gp_offset));
  emit_op2("mov", fp_offset, imm_int(reg_fp_offset));
  emit_op2("lea", Reg8.rdi, addr2(R.rbp, overflow_arg_area_offset));
  emit_op2("mov", overflow_arg_area, Reg8.rdi);
  emit_op2("lea", Reg8.rdi, addr2(R.rbp, -reg_save_area_pos));
  emit_op2("mov", reg_save_area, Reg8.rdi);
}

static void emit_expr_builtin_va_arg(Expr *expr) {
  assert(expr->ty == EX_BUILTIN_VA_ARG);

  int num_int = 0;
  int num_sse = 0;

  Type *type = expr->val_type;

  Expr *ap = expr->builtin_va_arg.ap;
  arg_class_t class = classify_arg_type(type, expr->range, &num_int, &num_sse);

  emit_expr(ap);
  emit_pop(Reg8.rcx);
  Operand gp_offset = addr(Reg8.rcx);
  Operand fp_offset = addr2(Reg8.rcx, 4);
  Operand overflow_arg_area = addr2(Reg8.rcx, 8);
  Operand reg_save_area = addr2(Reg8.rcx, 16);
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
    emit_op2("mov", Reg4.rax, gp_offset);
    emit_op2("cmp", Reg4.rax, imm_int(8 * NUM_INT_REG - align(size, 8)));
    emit_op1("ja", imm_symbol(stack_label));
    emit_op2("lea", Reg4.rdx, addr2(Reg8.rax, align(size, 8)));
    emit_op2("add", Reg8.rax, reg_save_area);
    emit_op2("mov", gp_offset, Reg4.rdx);
    emit_op1("jmp", imm_symbol(fetch_label));
    break;
  case ARG_CLASS_SSE:
    stack_label = make_label("va_arg.stack");
    emit_op2("mov", Reg4.rax, fp_offset);
    emit_op2("cmp", Reg4.rax,
             imm_int(8 * NUM_INT_REG + 16 * NUM_SSE_REG - align(size, 16)));
    emit_op1("ja", imm_symbol(stack_label));
    emit_op2("lea", Reg4.rdx, addr2(Reg8.rax, align(size, 16)));
    emit_op2("add", Reg8.rax, reg_save_area);
    emit_op2("mov", fp_offset, Reg4.rdx);
    emit_op1("jmp", imm_symbol(fetch_label));
    break;
  }
  if (stack_label != NULL) {
    emit_label(stack_label);
  }
  emit_op2("mov", Reg8.rax, overflow_arg_area);
  emit_op2("lea", Reg8.rdx, addr2(Reg8.rax, align(size, 8)));
  emit_op2("mov", overflow_arg_area, Reg8.rdx);

  emit_label(fetch_label);
  emit_stack_sub(align(size, 8));
  int copy_size = 0;
  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    emit_op2("mov", r->rdx, addr2(Reg8.rax, copy_size));
    emit_op2("mov", addr2(R.rsp, copy_size), r->rdx);
    copy_size += r->s.size;
  }
}

static void emit_expr_builtin_va_end(Expr *expr) {
  assert(expr->ty == EX_BUILTIN_VA_END);
  // nothing to do
  (void)expr;
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
    assert(rex->s.size == rop->s.size);
    emit_pop(Reg8.rdi);
    emit_op2("add", addr(R.rsp), rop->rdi);
    break;
  case EX_SUB:
    assert(rex->s.size == rop->s.size);
    emit_pop(Reg8.rdi);
    emit_op2("sub", addr(R.rsp), rop->rdi);
    break;
  case EX_MUL:
    assert(rex->s.size == rop->s.size);
    emit_pop(Reg8.rdi);
    emit_pop(Reg8.rax);
    emit_op2("imul", rop->rax, rop->rdi);
    emit_push(Reg8.rax);
    break;
  case EX_DIV:
    assert(rex->s.size == rop->s.size);
    emit_pop(Reg8.rdi);
    emit_pop(Reg8.rax);
    emit_op2("mov", rop->rdx, imm_int(0));
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit_op1("idiv", rop->rdi);
    } else {
      emit_op1("div", rop->rdi);
    }
    emit_push(Reg8.rax);
    break;
  case EX_MOD:
    assert(rex->s.size == rop->s.size);
    emit_pop(Reg8.rdi);
    emit_pop(Reg8.rax);
    emit_op2("mov", rop->rdx, imm_int(0));
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit_op1("idiv", rop->rdi);
    } else {
      emit_op1("div", rop->rdi);
    }
    emit_op2("mov", rex->rax, rop->rdx);
    emit_push(Reg8.rax);
    break;
  case EX_EQEQ:
    emit_pop(Reg8.rdi);
    emit_op2("cmp", addr(R.rsp), rop->rdi);
    emit_op1("sete", Reg1.rax);
    emit_op2("movzx", rex->rax, Reg1.rax);
    emit_op2("mov", addr(R.rsp), rex->rax);
    break;
  case EX_NOTEQ:
    emit_pop(Reg8.rdi);
    emit_op2("cmp", addr(R.rsp), rop->rdi);
    emit_op1("setne", Reg1.rax);
    emit_op2("movzx", rex->rax, Reg1.rax);
    emit_op2("mov", addr(R.rsp), rex->rax);
    break;
  case EX_LT:
    emit_pop(Reg8.rdi);
    emit_op2("cmp", addr(R.rsp), rop->rdi);
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit_op1("setl", Reg1.rax);
    } else {
      emit_op1("setb", Reg1.rax);
    }
    emit_op2("movzx", rex->rax, Reg1.rax);
    emit_op2("mov", addr(R.rsp), rex->rax);
    break;
  case EX_GT:
    emit_pop(Reg8.rdi);
    emit_op2("cmp", addr(R.rsp), rop->rdi);
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit_op1("setg", Reg1.rax);
    } else {
      emit_op1("seta", Reg1.rax);
    }
    emit_op2("movzx", rex->rax, Reg1.rax);
    emit_op2("mov", addr(R.rsp), rex->rax);
    break;
  case EX_LTEQ:
    emit_pop(Reg8.rdi);
    emit_op2("cmp", addr(R.rsp), rop->rdi);
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit_op1("setle", Reg1.rax);
    } else {
      emit_op1("setbe", Reg1.rax);
    }
    emit_op2("movzx", rex->rax, Reg1.rax);
    emit_op2("mov", addr(R.rsp), rex->rax);
    break;
  case EX_GTEQ:
    emit_pop(Reg8.rdi);
    emit_op2("cmp", addr(R.rsp), rop->rdi);
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit_op1("setge", Reg1.rax);
    } else {
      emit_op1("setae", Reg1.rax);
    }
    emit_op2("movzx", rex->rax, Reg1.rax);
    emit_op2("mov", addr(R.rsp), rex->rax);
    break;
  case EX_LSHIFT:
    assert(rex->s.size == rop->s.size);
    emit_pop(Reg8.rcx);
    emit_op2("shl", sized_addr(rop->s, R.rsp), Reg1.rcx);
    break;
  case EX_RSHIFT:
    assert(rex->s.size == rop->s.size);
    emit_pop(Reg8.rcx);
    if (is_signed_int_type(lhs->val_type, lhs->range)) {
      emit_op2("sar", sized_addr(rop->s, R.rsp), Reg1.rcx);
    } else {
      emit_op2("shr", sized_addr(rop->s, R.rsp), Reg1.rcx);
    }
    break;
  case EX_AND:
    assert(rex->s.size == rop->s.size);
    emit_pop(Reg8.rdi);
    emit_op2("and", addr(R.rsp), rop->rdi);
    break;
  case EX_XOR:
    assert(rex->s.size == rop->s.size);
    emit_pop(Reg8.rdi);
    emit_op2("xor", addr(R.rsp), rop->rdi);
    break;
  case EX_OR:
    assert(rex->s.size == rop->s.size);
    emit_pop(Reg8.rdi);
    emit_op2("or", addr(R.rsp), rop->rdi);
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
    emit_op2(op->add, SseReg[0], SseReg[1]);
    emit_push_xmm(0);
    break;
  case EX_SUB:
    emit_op2(op->sub, SseReg[0], SseReg[1]);
    emit_push_xmm(0);
    break;
  case EX_MUL:
    emit_op2(op->mul, SseReg[0], SseReg[1]);
    emit_push_xmm(0);
    break;
  case EX_DIV:
    emit_op2(op->div, SseReg[0], SseReg[1]);
    emit_push_xmm(0);
    break;
  case EX_EQEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit_op2(op->comi, SseReg[0], SseReg[1]);
    emit_op1("sete", Reg1.rax);
    emit_op1("setnp", Reg1.rdi);
    emit_op2("and", Reg1.rax, Reg1.rdi);
    emit_op2("movzx", r->rax, Reg1.rax);
    emit_push(Reg8.rax);
    break;
  }
  case EX_NOTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit_op2(op->comi, SseReg[0], SseReg[1]);
    emit_op1("setne", Reg1.rax);
    emit_op1("setp", Reg1.rdi);
    emit_op2("or", Reg1.rax, Reg1.rdi);
    emit_op2("movzx", r->rax, Reg1.rax);
    emit_push(Reg8.rax);
    break;
  }
  case EX_LT: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit_op2(op->comi, SseReg[1], SseReg[0]);
    emit_op1("seta", Reg1.rax);
    emit_op2("movzx", r->rax, Reg1.rax);
    emit_push(Reg8.rax);
    break;
  }
  case EX_GT: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit_op2(op->comi, SseReg[0], SseReg[1]);
    emit_op1("seta", Reg1.rax);
    emit_op2("movzx", r->rax, Reg1.rax);
    emit_push(Reg8.rax);
    break;
  }
  case EX_LTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit_op2(op->comi, SseReg[1], SseReg[0]);
    emit_op1("setnb", Reg1.rax);
    emit_op2("movzx", r->rax, Reg1.rax);
    emit_push(Reg8.rax);
    break;
  }
  case EX_GTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit_op2(op->comi, SseReg[0], SseReg[1]);
    emit_op1("setnb", Reg1.rax);
    emit_op2("movzx", r->rax, Reg1.rax);
    emit_push(Reg8.rax);
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
    emit_op1("fld", sized_addr2(X87Size, R.rsp, 16));
    emit_op1("fld", sized_addr(X87Size, R.rsp));
    emit_op0("faddp");
    emit_op1("fstp", sized_addr2(X87Size, R.rsp, 16));
    emit_stack_add(16);
    break;
  case EX_SUB:
    emit_op1("fld", sized_addr2(X87Size, R.rsp, 16));
    emit_op1("fld", sized_addr(X87Size, R.rsp));
    emit_op0("fsubp");
    emit_op1("fstp", sized_addr2(X87Size, R.rsp, 16));
    emit_stack_add(16);
    break;
  case EX_MUL:
    emit_op1("fld", sized_addr2(X87Size, R.rsp, 16));
    emit_op1("fld", sized_addr(X87Size, R.rsp));
    emit_op0("fmulp");
    emit_op1("fstp", sized_addr2(X87Size, R.rsp, 16));
    emit_stack_add(16);
    break;
  case EX_DIV:
    emit_op1("fld", sized_addr2(X87Size, R.rsp, 16));
    emit_op1("fld", sized_addr(X87Size, R.rsp));
    if (asm_syntax == ASM_SYNTAX_INTEL) {
      emit_op0("fdivp");
    } else {
      emit_op0("fdivrp");
    }
    emit_op1("fstp", sized_addr2(X87Size, R.rsp, 16));
    emit_stack_add(16);
    break;
  case EX_EQEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit_op1("fld", sized_addr2(X87Size, R.rsp, 16));
    emit_op1("fld", sized_addr(X87Size, R.rsp));
    emit_op2("fcomip", X87Reg[0], X87Reg[1]);
    emit_op1("fstp", X87Reg[0]);
    emit_op1("sete", Reg1.rax);
    emit_op1("setnp", Reg1.rdi);
    emit_op2("and", Reg1.rax, Reg1.rdi);
    emit_op2("movzx", r->rax, Reg1.rax);
    emit_stack_add(32);
    emit_push(Reg8.rax);
    break;
  }
  case EX_NOTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit_op1("fld", sized_addr2(X87Size, R.rsp, 16));
    emit_op1("fld", sized_addr(X87Size, R.rsp));
    emit_op2("fcomip", X87Reg[0], X87Reg[1]);
    emit_op1("fstp", X87Reg[0]);
    emit_op1("setne", Reg1.rax);
    emit_op1("setp", Reg1.rdi);
    emit_op2("or", Reg1.rax, Reg1.rdi);
    emit_op2("movzx", r->rax, Reg1.rax);
    emit_stack_add(32);
    emit_push(Reg8.rax);
    break;
  }
  case EX_LT: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit_op1("fld", sized_addr2(X87Size, R.rsp, 16));
    emit_op1("fld", sized_addr(X87Size, R.rsp));
    emit_op2("fcomip", X87Reg[0], X87Reg[1]);
    emit_op1("fstp", X87Reg[0]);
    emit_op1("seta", Reg1.rax);
    emit_op2("movzx", r->rax, Reg1.rax);
    emit_stack_add(32);
    emit_push(Reg8.rax);
    break;
  }
  case EX_GT: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit_op1("fld", sized_addr(X87Size, R.rsp));
    emit_op1("fld", sized_addr2(X87Size, R.rsp, 16));
    emit_op2("fcomip", X87Reg[0], X87Reg[1]);
    emit_op1("fstp", X87Reg[0]);
    emit_op1("seta", Reg1.rax);
    emit_op2("movzx", r->rax, Reg1.rax);
    emit_stack_add(32);
    emit_push(Reg8.rax);
    break;
  }
  case EX_LTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit_op1("fld", sized_addr2(X87Size, R.rsp, 16));
    emit_op1("fld", sized_addr(X87Size, R.rsp));
    emit_op2("fcomip", X87Reg[0], X87Reg[1]);
    emit_op1("fstp", X87Reg[0]);
    emit_op1("setnb", Reg1.rax);
    emit_op2("movzx", r->rax, Reg1.rax);
    emit_stack_add(32);
    emit_push(Reg8.rax);
    break;
  }
  case EX_GTEQ: {
    const Reg *r = get_int_reg(expr->val_type, expr->range);
    emit_op1("fld", sized_addr(X87Size, R.rsp));
    emit_op1("fld", sized_addr2(X87Size, R.rsp, 16));
    emit_op2("fcomip", X87Reg[0], X87Reg[1]);
    emit_op1("fstp", X87Reg[0]);
    emit_op1("setnb", Reg1.rax);
    emit_op2("movzx", r->rax, Reg1.rax);
    emit_stack_add(32);
    emit_push(Reg8.rax);
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
  emit_comment("%s:%d:%d", filename, line, column);

  switch (stmt->ty) {
  case ST_NULL: {
    return;
  }
  case ST_EXPR: {
#ifndef NDEBUG
    int base_stack_pos = stack_pos;
#endif
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
#ifndef NDEBUG
      int size = get_val_size(stmt->val_type, stmt->range);
#endif
      range_assert(stmt->range, stack_pos - align(size, 8) == base_stack_pos,
                   "stack position mismatch");
    }
    return;
  }
  case ST_COMPOUND: {
#ifndef NDEBUG
    int base_stack_pos = stack_pos;
#endif
    for (int i = 0; i < VEC_LEN(stmt->stmts); i++) {
      bool is_last = i == VEC_LEN(stmt->stmts) - 1;
      emit_stmt(VEC_GET(stmt->stmts, i), leave_value && is_last);
    }
    if (stmt->val_type->ty == TY_VOID || !leave_value) {
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
    } else {
#ifndef NDEBUG
      int size = get_val_size(stmt->val_type, stmt->range);
#endif
      range_assert(stmt->range, stack_pos - align(size, 8) == base_stack_pos,
                   "stack position mismatch");
    }
    return;
  }
  case ST_DECL: {
    for (int i = 0; i < VEC_LEN(stmt->decl); i++) {
      StackVarDecl *decl = VEC_GET(stmt->decl, i);
      StackVar *svar = decl->stack_var;
      Initializer *init = decl->init;
      emit_svar_zero(svar);
      emit_svar_init(svar, 0, init, svar->range);
    }
    return;
  }
  case ST_IF: {
#ifndef NDEBUG
    int base_stack_pos = stack_pos;
#endif
    char *else_label = make_label("if.else");
    char *end_label = make_label("if.end");
    const Reg *r = get_int_reg(stmt->cond->val_type, stmt->cond->range);
    emit_expr(stmt->cond);
    emit_pop(Reg8.rax);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");

    emit_op2("cmp", r->rax, imm_int(0));
    emit_op1("je", imm_symbol(else_label));

    emit_stmt(stmt->then_stmt, false);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    emit_op1("jmp", imm_symbol(end_label));

    emit_label(else_label);
    emit_stmt(stmt->else_stmt, false);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");

    emit_label(end_label);
    return;
  }
  case ST_SWITCH: {
#ifndef NDEBUG
    int base_stack_pos = stack_pos;
#endif
    char *end_label = make_label("switch.end");
    const Reg *r = get_int_reg(stmt->cond->val_type, stmt->cond->range);
    emit_expr(stmt->cond);
    for (int i = 0; i < VEC_LEN(stmt->cases); i++) {
      Stmt *case_stmt = VEC_GET(stmt->cases, i);
      assert(case_stmt->ty == ST_CASE);
      emit_op2("mov", Reg8.rax, imm_num(case_stmt->case_val));
      emit_pop(Reg8.rdi);
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
      emit_op2("cmp", r->rax, r->rdi);
      emit_op1("je", imm_symbol(case_stmt->label));
      emit_push(Reg8.rdi);
    }
    emit_pop(Reg8.rdi);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    if (stmt->default_case) {
      emit_op1("jmp", imm_symbol(stmt->default_case->label));
    } else {
      emit_op1("jmp", imm_symbol(end_label));
    }
    VEC_PUSH(break_labels, end_label);
    emit_stmt(stmt->body, false);
    VEC_POP(break_labels);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    emit_label(end_label);
    return;
  }
  case ST_CASE:
  case ST_DEFAULT:
  case ST_LABEL: {
    emit_label(stmt->label);
    emit_stmt(stmt->body, leave_value);
    return;
  }
  case ST_WHILE: {
#ifndef NDEBUG
    int base_stack_pos = stack_pos;
#endif
    char *cond_label = make_label("while.cond");
    char *end_label = make_label("while.end");

    emit_label(cond_label);
    const Reg *r = get_int_reg(stmt->cond->val_type, stmt->cond->range);
    emit_expr(stmt->cond);
    emit_pop(Reg8.rax);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    emit_op2("cmp", r->rax, imm_int(0));
    emit_op1("je", imm_symbol(end_label));

    VEC_PUSH(break_labels, end_label);
    VEC_PUSH(continue_labels, cond_label);
    emit_stmt(stmt->body, false);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    VEC_POP(break_labels);
    VEC_POP(continue_labels);

    emit_op1("jmp", imm_symbol(cond_label));
    emit_label(end_label);
    return;
  }
  case ST_DO_WHILE: {
#ifndef NDEBUG
    int base_stack_pos = stack_pos;
#endif
    char *loop_label = make_label("do_while.loop");
    char *cond_label = make_label("do_while.cond");
    char *end_label = make_label("do_while.end");
    emit_label(loop_label);

    VEC_PUSH(break_labels, end_label);
    VEC_PUSH(continue_labels, cond_label);
    emit_stmt(stmt->body, false);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    VEC_POP(break_labels);
    VEC_POP(continue_labels);

    emit_label(cond_label);
    const Reg *r = get_int_reg(stmt->cond->val_type, stmt->cond->range);
    emit_expr(stmt->cond);
    emit_pop(Reg8.rax);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");

    emit_op2("cmp", r->rax, imm_int(0));
    emit_op1("jne", imm_symbol(loop_label));

    emit_label(end_label);
    return;
  }
  case ST_FOR: {
#ifndef NDEBUG
    int base_stack_pos = stack_pos;
#endif
    char *cond_label = make_label("for.cond");
    char *inc_label = make_label("for.inc");
    char *end_label = make_label("for.end");
    if (stmt->init != NULL) {
      emit_stmt(stmt->init, false);
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
    }
    emit_label(cond_label);
    if (stmt->cond != NULL) {
      const Reg *r = get_int_reg(stmt->cond->val_type, stmt->cond->range);
      emit_expr(stmt->cond);
      emit_pop(Reg8.rax);
      emit_op2("cmp", r->rax, imm_int(0));
      emit_op1("je", imm_symbol(end_label));
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
    }

    VEC_PUSH(break_labels, end_label);
    VEC_PUSH(continue_labels, inc_label);
    emit_stmt(stmt->body, false);
    VEC_POP(break_labels);
    VEC_POP(continue_labels);
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");

    emit_label(inc_label);
    if (stmt->inc != NULL) {
      emit_expr(stmt->inc);
      emit_pop(Reg8.rax);
      range_assert(stmt->range, stack_pos == base_stack_pos,
                   "stack position mismatch");
    }
    emit_op1("jmp", imm_symbol(cond_label));
    emit_label(end_label);
    return;
  }
  case ST_GOTO: {
    char *label = get_label(func_ctxt, stmt->name);
    if (label == NULL) {
      range_error(stmt->range, "未知のラベルへのgotoです: %s", stmt->name);
    }
    emit_op1("jmp", imm_symbol(label));
    return;
  }
  case ST_BREAK: {
    if (VEC_LEN(break_labels) <= 0) {
      range_error(stmt->range,
                  "ループでもswitch文中でもない箇所にbreakがあります");
    }
    emit_op1("jmp", imm_symbol(VEC_LAST(break_labels)));
    return;
  }
  case ST_CONTINUE: {
    if (VEC_LEN(continue_labels) <= 0) {
      range_error(stmt->range, "ループ中でない箇所にcontinueがあります");
    }
    emit_op1("jmp", imm_symbol(VEC_LAST(continue_labels)));
    return;
  }
  case ST_RETURN: {
#ifndef NDEBUG
    int base_stack_pos = stack_pos;
#endif
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
        emit_op2("mov", Reg8.rax, addr2(R.rbp, -retval_pos));
        int copy_size = 0;
        while (size - copy_size > 0) {
          const Reg *r = get_int_reg_for_copy(size - copy_size);
          emit_op2("mov", r->rdi, addr2(R.rsp, copy_size));
          emit_op2("mov", addr2(Reg8.rax, copy_size), r->rdi);
          copy_size += r->s.size;
        }
        emit_stack_add(align(size, 8));
        break;
      case ARG_CLASS_INTEGER:
        emit_pop(Reg8.rax);
        if (size > 8) {
          emit_pop(Reg8.rdx);
        }
        break;
      case ARG_CLASS_SSE:
        assert(size <= 8);
        emit_pop_xmm(0);
        break;
      case ARG_CLASS_X87:
        emit_op1("fld", sized_addr(X87Size, R.rsp));
        emit_stack_add(16);
        break;
      }
    }
    range_assert(stmt->range, stack_pos == base_stack_pos,
                 "stack position mismatch");
    emit_op1("jmp", imm_symbol(epilogue_label));
    return;
  }
  }
  range_error(stmt->range, "不正な文です: %d", stmt->ty);
}

static ArgClassVector *classify_param(const ParamVector *params,
                                      int int_reg_idx) {
  ArgClassVector *class = NEW_VECTOR(ArgClassVector);
  int num_int_reg = int_reg_idx;
  int num_sse_reg = 0;
  for (int i = 0; i < VEC_LEN(params); i++) {
    Param *param = VEC_GET(params, i);
    VEC_PUSH(class, classify_arg_type(param->type, param->range, &num_int_reg,
                                      &num_sse_reg));
  }
  return class;
}

static void emit_func(Function *func) {
  epilogue_label = make_label("func.epi");
  func_ctxt = func;
  break_labels = NEW_VECTOR(StrVector);
  continue_labels = NEW_VECTOR(StrVector);
  retval_pos = INVALID_STACK_POS;
  reg_save_area_pos = INVALID_STACK_POS;
  reg_gp_offset = INVALID_STACK_POS;
  reg_fp_offset = INVALID_STACK_POS;

  if (!func->storage_class.is_static) {
    emit(".global %s", func->name);
  }
  emit_label(func->name);

  // プロローグ
  // スタックサイズ分の領域を確保する
  emit_op1("push", R.rbp);
  emit_op2("mov", R.rbp, R.rsp);

  // ローカル変数の領域確保
  int stack_size = 0;
  for (int i = 0; i < VEC_LEN(func->var_list); i++) {
    StackVar *svar = VEC_GET(func->var_list, i);
    stack_size = align(stack_size, get_val_align(svar->type, svar->range));
    svar->offset = stack_size;
    stack_size += get_val_size(svar->type, svar->range);
  }
  for (int i = 0; i < VEC_LEN(func->var_list); i++) {
    StackVar *svar = VEC_GET(func->var_list, i);
    svar->offset = align(stack_size, 16) - svar->offset;

    const char *filename;
    int line;
    int column;
    range_get_start(svar->range, &filename, &line, &column);
    emit_comment("%s:%d:%d", filename, line, column);
    emit_comment("%s: svar %s (size=%d) (%s:%d:%d)",
                 format_operand(addr2(R.rbp, -svar->offset)), svar->name,
                 get_val_size(svar->type, svar->range), filename, line, column);
  }

  stack_pos = 0;
  int reg_save_area_size = 0;
  if (func->type->func.has_varargs) {
    reg_save_area_size += 8 * NUM_INT_REG + 16 * NUM_SSE_REG;
  }
  emit_stack_sub(align(stack_size, 16) + align(reg_save_area_size, 16));
  if (func->type->func.has_varargs) {
    reg_save_area_pos = stack_pos;
    emit_comment("%s: reg_save_area", format_operand(addr2(R.rbp, -stack_pos)));

    for (int i = 0; i < NUM_INT_REG; i++) {
      emit_op2("mov", addr2(R.rbp, -stack_pos + 8 * i),
               get_int_arg_reg(&Reg8, i));
    }
    const char *end_label = make_label("skip_float_reg");
    emit_op2("test", Reg1.rax, Reg1.rax);
    emit_op1("je", imm_symbol(end_label));
    for (int i = 0; i < NUM_SSE_REG; i++) {
      emit_op2("movaps", addr2(R.rbp, -stack_pos + 8 * NUM_INT_REG + 16 * i),
               SseReg[i]);
    }
    emit_label(end_label);
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
      emit_push(Reg8.rdi);
      int_reg_idx++;
      ret_stack_offset = 8;
      retval_pos = stack_pos;
    }
  }

  // 引数をスタックへコピー
  if (func->type->func.params != NULL) {
    ArgClassVector *param_class =
        classify_param(func->type->func.params, int_reg_idx);
    for (int i = 0; i < VEC_LEN(func->type->func.params); i++) {
      Param *param = VEC_GET(func->type->func.params, i);
      StackVar *var = param->stack_var;
      assert(var != NULL);
      arg_class_t class = VEC_GET(param_class, i);
      int size = get_val_size(param->type, param->range);
      int dst_offset = var->offset;

      switch (class) {
      case ARG_CLASS_MEMORY:
      case ARG_CLASS_X87: {
        int copy_size = 0;
        while (size - copy_size > 0) {
          const Reg *r = get_int_reg_for_copy(size - copy_size);
          emit_op2("mov", r->rax, addr2(R.rbp, arg_stack_offset + copy_size));
          emit_op2("mov", addr2(R.rbp, -dst_offset + copy_size), r->rax);
          copy_size += r->s.size;
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
              emit_op2("mov", addr2(R.rbp, -dst_offset + j * 8 + copy_size),
                       get_int_arg_reg(&Reg8, int_reg_idx));
              rest_size -= 8;
              copy_size += 8;
              break;
            }
            if (rest_size >= 4) {
              emit_op2("mov", addr2(R.rbp, -dst_offset + j * 8 + copy_size),
                       get_int_arg_reg(&Reg4, int_reg_idx));
              rest_size -= 4;
              copy_size += 4;
              if (rest_size > 0) {
                emit_op2("shr", get_int_arg_reg(&Reg8, int_reg_idx),
                         imm_int(32));
              }
              continue;
            }
            if (rest_size >= 2) {
              emit_op2("mov", addr2(R.rbp, -dst_offset + j * 8 + copy_size),
                       get_int_arg_reg(&Reg2, int_reg_idx));
              rest_size -= 2;
              copy_size += 2;
              if (rest_size > 0) {
                emit_op2("shr", get_int_arg_reg(&Reg8, int_reg_idx),
                         imm_int(16));
              }
              continue;
            }

            assert(rest_size == 1);
            emit_op2("mov", addr2(R.rbp, -dst_offset + j * 8 + copy_size),
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
          emit_op2("movsd", addr2(R.rbp, -dst_offset), SseReg[sse_reg_idx]);
        } else {
          assert(size == 4);
          emit_op2("movss", addr2(R.rbp, -dst_offset), SseReg[sse_reg_idx]);
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

  emit_comment("reg_gp_offset: %d", reg_gp_offset);
  emit_comment("reg_fp_offset: %d", reg_fp_offset);
  emit_comment("overflow_arg_area: %s",
               format_operand(addr2(R.rbp, overflow_arg_area_offset)));
  emit_comment("%s body", func->name);

  emit_stmt(func->body, false);

  // main関数の場合、returnなしに関数末尾まで到達した場合、戻り値は0にする
  if (strcmp(func->name, "main") == 0) {
    emit_op2("mov", Reg4.rax, imm_int(0));
  }

  // エピローグ
  // 最後の式の結果がRAXに残っているのでそれが返り値になる
  assert(stack_pos == align(stack_size, 16) + align(reg_save_area_size, 16) +
                          ret_stack_offset);
  emit_label(epilogue_label);
  emit_op2("mov", R.rsp, R.rbp);
  emit_op1("pop", R.rbp);
  emit_op0("ret");
  stack_pos = INVALID_STACK_POS;
}

static void emit_svar_zero(StackVar *svar) {
  int size = get_val_size(svar->type, svar->range);
  int copy_size = 0;
  while (size - copy_size > 0) {
    const Reg *r = get_int_reg_for_copy(size - copy_size);
    emit_op2("mov", sized_addr2(r->s, R.rbp, -svar->offset + copy_size),
             imm_int(0));
    copy_size += r->s.size;
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
      emit_op2("mov", r->rax, addr2(R.rsp, copy_size));
      emit_op2("mov", addr2(R.rbp, -svar->offset + copy_size + offset), r->rax);
      copy_size += r->s.size;
    }

    emit_stack_add(align(size, 8));
    return;
  }

  if (init->members != NULL) {
    assert(init->type->ty == TY_STRUCT || init->type->ty == TY_UNION);
    assert(VEC_LEN(init->members) <= 1 || init->type->ty == TY_STRUCT);

    for (int i = 0; i < VEC_LEN(init->members); i++) {
      MemberInitializer *meminit = VEC_GET(init->members, i);
      const Member *member = meminit->member;
      emit_svar_init(svar, offset + member->offset, meminit->init, range);
    }
    return;
  }

  if (init->elements != NULL) {
    assert(init->type->ty == TY_ARRAY);
    int size = get_val_size(init->type->array.elem, range);
    for (int i = 0; i < VEC_LEN(init->elements); i++) {
      Initializer *meminit = VEC_GET(init->elements, i);
      emit_svar_init(svar, offset + i * size, meminit, range);
    }
    return;
  }

  assert(false);
}

static void emit_gvar_init(Initializer *init, const Range *range,
                           GlobalVarVector *gvar_list) {
  if (init->expr != NULL) {
    Expr *expr = init->expr;
    if (expr->ty == EX_NUM) {
      int size = get_val_size(expr->val_type, expr->range);
      switch (size) {
      case 1:
        emit("  .byte %s", num2str(expr->num));
        break;
      case 2:
        emit("  .word %s", num2str(expr->num));
        break;
      case 4:
        emit("  .long %s", num2str(expr->num));
        break;
      case 8:
        emit("  .quad %s", num2str(expr->num));
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
        VEC_PUSH(gvar_list, gvar);

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
    assert(VEC_LEN(init->members) <= 1 || init->type->ty == TY_STRUCT);
    StructBody *body = init->type->struct_body;
    int offset = 0;
    for (int i = 0; i < VEC_LEN(init->members); i++) {
      MemberInitializer *meminit = VEC_GET(init->members, i);
      if (meminit->init == NULL) {
        continue;
      }
      if (i > 0) {
        Member *member = VEC_GET(body->members, i);
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
    for (int i = 0; i < VEC_LEN(init->elements); i++) {
      Initializer *meminit = VEC_GET(init->elements, i);
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

static void emit_gvar(GlobalVar *gvar, GlobalVarVector *gvar_list) {
  Initializer *init = gvar->init;
  if (init == NULL) {
    emit(".bss");
    if (!gvar->storage_class.is_static) {
      emit(".global %s", gvar->name);
    }
    emit_label(gvar->name);
    emit("  .zero %d", get_val_size(gvar->type, gvar->range));
  } else {
    emit(".data");
    if (!gvar->storage_class.is_static) {
      emit(".global %s", gvar->name);
    }
    emit_label(gvar->name);
    emit_gvar_init(init, gvar->range, gvar_list);
  }
}

static void emit_str(StringLiteral *str) {
  emit_label(str->name);
  emit("  .string %s", format_string_literal(str->val));
}

void gen(FILE *fp, TranslationUnit *tunit, asm_syntax_t syntax) {
  asm_syntax = syntax;
  output_fp = fp;
  if (asm_syntax == ASM_SYNTAX_INTEL) {
    emit(".intel_syntax noprefix");
  } else {
    emit(".att_syntax prefix");
  }

  emit(".text");
  for (int i = 0; i < VEC_LEN(tunit->func_list); i++) {
    emit_func(VEC_GET(tunit->func_list, i));
  }
  while (VEC_LEN(tunit->gvar_list) > 0) {
    GlobalVar *gvar = VEC_REMOVE(tunit->gvar_list, 0);
    emit_gvar(gvar, tunit->gvar_list);
  }
  emit(".section .rodata");
  for (int i = 0; i < VEC_LEN(tunit->str_list); i++) {
    emit_str(VEC_GET(tunit->str_list, i));
  }
}
