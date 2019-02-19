#include "gifcc.h"
#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

static char *epilogue_label = NULL;
static Function *func_ctxt = NULL;
static Vector *break_labels = NULL;
static Vector *continue_labels = NULL;

static void gen_expr(Expr *expr);

typedef struct {
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

static const Reg *get_int_reg(Type *ty, Range range) {
  switch (get_val_size(ty, range)) {
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

char *make_label(const char *s) {
  static int count = 0;
  char buf[256];
  sprintf(buf, ".L.%s.%d", s, count++);
  return strdup(buf);
}

static void gen_lval(Expr *expr) {
  if (expr->ty == EX_STACK_VAR) {
    StackVar *var = expr->stack_var;
    assert(var != NULL);
    printf("  lea rax, [rbp - %d]\n",
           align(func_ctxt->stack_size, 16) - var->offset);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == EX_GLOBAL_VAR) {
    printf("  lea rax, %s[rip]\n",
           expr->global_var ? expr->global_var->name : expr->name);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == '*' && expr->lhs == NULL) {
    gen_expr(expr->rhs);
    return;
  }

  range_error(expr->range, "左辺値が変数ではありません");
}

static char *num2str(Number num, Range range) {
  char buf[1024];
  switch (num.type) {
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
    sprintf(buf, "%u", num.u_int_val);
    break;
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
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    range_error(range, "不正な型の数値です: %d", num.type);
  }
  return strdup(buf);
}

static void gen_expr(Expr *expr) {
  const Reg *r = get_int_reg(expr->val_type, expr->range);

  if (expr->ty == EX_NUM) {
    if (get_val_size(expr->val_type, expr->range) < 4) {
      printf("  push %s\n", num2str(expr->num_val, expr->range));
    } else {
      printf("  mov %s, %s\n", r->rax, num2str(expr->num_val, expr->range));
      printf("  push rax\n");
    }
    return;
  }

  if (expr->ty == EX_STACK_VAR) {
    StackVar *var = expr->stack_var;
    assert(var != NULL);
    printf("  mov %s, [rbp - %d]\n", r->rax,
           align(func_ctxt->stack_size, 16) - var->offset);
    printf("  push rax\n");
    return;
  }

  if (expr->ty == EX_GLOBAL_VAR) {
    printf("  mov %s, %s[rip]\n", r->rax,
           expr->global_var ? expr->global_var->name : expr->name);
    printf("  push rax\n");
    return;
  }

  if (expr->ty == EX_STR) {
    printf("  lea rax, %s[rip]\n", expr->name);
    printf("  push rax\n");
    return;
  }

  if (expr->ty == EX_CALL) {
    int num_push = 0;
    Vector *arg = expr->argument;
    bool call_direct;
    if (expr->callee->val_type->ty == TY_FUNC) {
      call_direct = true;
    } else if (expr->callee->val_type->ty == TY_PTR &&
               expr->callee->val_type->ptrof->ty == TY_FUNC) {
      call_direct = false;
    } else if (expr->callee->ty == EX_GLOBAL_VAR) {
      call_direct = true;
    } else {
      range_error(expr->callee->range,
                  "関数または関数ポインタ以外を呼び出そうとしました: %d",
                  expr->callee->val_type->ty);
    }
    if (arg && vec_len(arg) > 0) {
      // 引数をスタックに積む
      for (int i = vec_len(arg) - 1; i >= 0; i--) {
        gen_expr(vec_get(arg, i));
      }
    }
    if (!call_direct) {
      gen_expr(expr->callee);
      printf("  pop r10\n");
    }
    if (arg && vec_len(arg) > 0) {
      // レジスタ渡しする引数をpopする
      for (int i = 0; i < vec_len(arg); i++) {
        switch (i) {
        // 0~5番目の引数はレジスタ経由で渡す
        case 0:
          printf("  pop rdi\n");
          break;
        case 1:
          printf("  pop rsi\n");
          break;
        case 2:
          printf("  pop rdx\n");
          break;
        case 3:
          printf("  pop rcx\n");
          break;
        case 4:
          printf("  pop r8\n");
          break;
        case 5:
          printf("  pop r9\n");
          break;
        // 6番目以降の引数はスタック経由で渡すため、pushされたままにする
        default:
          num_push++;
          break;
        }
      }
    }
    printf("  mov al, 0\n");
    if (call_direct) {
      printf("  call %s\n", expr->callee->name);
    } else {
      printf("  call r10\n");
    }
    if (num_push > 0) {
      printf("  add rsp, %d\n", 8 * num_push);
    }
    printf("  push rax\n");
    return;
  }

  if (expr->ty == EX_CAST) {
    Expr *operand = expr->expr;
    gen_expr(operand);
    const Reg *from = get_int_reg(operand->val_type, operand->range);
    int from_size = get_val_size(operand->val_type, operand->range);
    int to_size = get_val_size(expr->val_type, expr->range);
    if (to_size > from_size) {
      printf("  pop rax\n");
      if (is_signed_int_type(operand->val_type, operand->range)) {
        printf("  movsx %s, %s\n", r->rax, from->rax);
      } else {
        if (from_size < 4) {
          printf("  movzx %s, %s\n", r->rax, from->rax);
        } else {
          printf("  mov %s, %s\n", from->rax, from->rax);
        }
      }
      printf("  push rax\n");
    }
    return;
  }

  if (expr->ty == '=') {
    gen_lval(expr->lhs);
    gen_expr(expr->rhs);
    printf("  pop rdi\n");
    printf("  pop rax\n");
    printf("  mov [rax], %s\n", r->rdi);
    printf("  push rdi\n");
    return;
  }

  if (expr->ty == EX_LOGAND) {
    char *false_label = make_label("logand.false");
    char *end_label = make_label("logand.end");
    gen_expr(expr->lhs);
    printf("  pop rax\n");
    printf("  cmp %s, 0\n", r->rax);
    printf("  je %s\n", false_label);
    gen_expr(expr->rhs);
    printf("  pop rax\n");
    printf("  cmp %s, 0\n", r->rax);
    printf("  je %s\n", false_label);
    printf("  push 1\n");
    printf("  jmp %s\n", end_label);
    printf("%s:\n", false_label);
    printf("  push 0\n");
    printf("%s:\n", end_label);
    return;
  }

  if (expr->ty == EX_LOGOR) {
    char *true_label = make_label("logor.true");
    char *end_label = make_label("logor.end");
    gen_expr(expr->lhs);
    printf("  pop rax\n");
    printf("  cmp %s, 0\n", r->rax);
    printf("  jne %s\n", true_label);
    gen_expr(expr->rhs);
    printf("  pop rax\n");
    printf("  cmp %s, 0\n", r->rax);
    printf("  jne %s\n", true_label);
    printf("  push 0\n");
    printf("  jmp %s\n", end_label);
    printf("%s:\n", true_label);
    printf("  push 1\n");
    printf("%s:\n", end_label);
    return;
  }

  if (expr->ty == EX_COND) {
    char *else_label = make_label("cond.else");
    char *end_label = make_label("cond.end");
    gen_expr(expr->cond);
    printf("  pop rax\n");
    printf("  cmp %s, 0\n", r->rax);
    printf("  je %s\n", else_label);
    gen_expr(expr->lhs);
    printf("  jmp %s\n", end_label);
    printf("%s:\n", else_label);
    gen_expr(expr->rhs);
    printf("%s:\n", end_label);
    return;
  }

  if (expr->ty == '&' && expr->lhs == NULL) {
    // 単項の `&`
    gen_lval(expr->rhs);
    return;
  }
  if (expr->ty == '*' && expr->lhs == NULL) {
    // 単項の `*`
    gen_expr(expr->rhs);
    printf("  pop rax\n");
    printf("  mov %s, [rax]\n", r->rax);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == '+' && expr->lhs == NULL) {
    // 単項の `+`
    gen_expr(expr->rhs);
    return;
  }
  if (expr->ty == '-' && expr->lhs == NULL) {
    // 単項の `-`
    gen_expr(expr->rhs);
    printf("  pop rax\n");
    printf("  neg %s\n", r->rax);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == '~') {
    // `~`
    gen_expr(expr->rhs);
    printf("  pop rax\n");
    printf("  not %s\n", r->rax);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == '!') {
    assert(false);
  }
  if (expr->ty == EX_INC) {
    if (expr->lhs == NULL) {
      // 前置の `++`
      gen_lval(expr->rhs);
      printf("  pop rax\n");
      printf("  mov %s, [rax]\n", r->rdi);
      printf("  add %s, %d\n", r->rdi, expr->incdec_size);
      printf("  mov [rax], %s\n", r->rdi);
      printf("  push rdi\n");
      return;
    }
    // 後置の `++`
    gen_lval(expr->lhs);
    printf("  pop rax\n");
    printf("  mov %s, [rax]\n", r->rdi);
    printf("  push rdi\n");
    printf("  add %s, %d\n", r->rdi, expr->incdec_size);
    printf("  mov [rax], %s\n", r->rdi);
    return;
  }

  if (expr->ty == EX_DEC) {
    if (expr->lhs == NULL) {
      // 前置の `--`
      gen_lval(expr->rhs);
      printf("  pop rax\n");
      printf("  mov %s, [rax]\n", r->rdi);
      printf("  sub %s, %d\n", r->rdi, expr->incdec_size);
      printf("  mov [rax], %s\n", r->rdi);
      printf("  push rdi\n");
      return;
    }
    // 後置の `--`
    gen_lval(expr->lhs);
    printf("  pop rax\n");
    printf("  mov %s, [rax]\n", r->rdi);
    printf("  push rdi\n");
    printf("  sub %s, %d\n", r->rdi, expr->incdec_size);
    printf("  mov [rax], %s\n", r->rdi);
    return;
  }

  // 二項演算子
  assert(expr->lhs != NULL);
  assert(expr->rhs != NULL);

  gen_expr(expr->lhs);
  gen_expr(expr->rhs);

  printf("  pop rdi\n");
  printf("  pop rax\n");

  switch (expr->ty) {
  case '+':
    printf("  add %s, %s\n", r->rax, r->rdi);
    break;
  case '-':
    printf("  sub %s, %s\n", r->rax, r->rdi);
    break;
  case '*':
    printf("  imul %s, %s\n", r->rax, r->rdi);
    break;
  case '/':
    if (is_signed_int_type(expr->lhs->val_type, expr->lhs->range)) {
      printf("  mov %s, 0\n", r->rdx);
      printf("  idiv %s\n", r->rdi);
    } else {
      printf("  mov %s, 0\n", r->rdx);
      printf("  div %s\n", r->rdi);
    }
    break;
  case '%':
    printf("  mov %s, 0\n", r->rdx);
    printf("  div %s\n", r->rdi);
    printf("  mov %s, %s\n", r->rax, r->rdx);
    break;
  case EX_EQEQ:
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    printf("  sete al\n");
    printf("  movzb %s, al\n", r->rax);
    break;
  case EX_NOTEQ:
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    printf("  setne al\n");
    printf("  movzb %s, al\n", r->rax);
    break;
  case '<':
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    if (is_signed_int_type(expr->lhs->val_type, expr->lhs->range)) {
      printf("  setl al\n");
    } else {
      printf("  setb al\n");
    }
    printf("  movzb %s, al\n", r->rax);
    break;
  case '>':
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    if (is_signed_int_type(expr->lhs->val_type, expr->lhs->range)) {
      printf("  setg al\n");
    } else {
      printf("  seta al\n");
    }
    printf("  movzb %s, al\n", r->rax);
    break;
  case EX_LTEQ:
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    if (is_signed_int_type(expr->lhs->val_type, expr->lhs->range)) {
      printf("  setle al\n");
    } else {
      printf("  setbe al\n");
    }
    printf("  movzb %s, al\n", r->rax);
    break;
  case EX_GTEQ:
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    if (is_signed_int_type(expr->lhs->val_type, expr->lhs->range)) {
      printf("  setge al\n");
    } else {
      printf("  setae al\n");
    }
    printf("  movzb %s, al\n", r->rax);
    break;
  case EX_LSHIFT:
    printf("  mov %s, %s\n", r->rcx, r->rdi);
    printf("  shl %s, cl\n", r->rax);
    break;
  case EX_RSHIFT:
    printf("  mov %s, %s\n", r->rcx, r->rdi);
    if (is_signed_int_type(expr->lhs->val_type, expr->lhs->range)) {
      printf("  sar %s, cl\n", r->rax);
    } else {
      printf("  shr %s, cl\n", r->rax);
    }
    break;
  case '&':
    printf("  and %s, %s\n", r->rax, r->rdi);
    break;
  case '^':
    printf("  xor %s, %s\n", r->rax, r->rdi);
    break;
  case '|':
    printf("  or %s, %s\n", r->rax, r->rdi);
    break;
  case ',':
    printf("  mov %s, %s\n", r->rax, r->rdi);
    break;
  default:
    assert(false);
  }

  printf("  push rax\n");
}

static void gen_stmt(Stmt *stmt) {
  switch (stmt->ty) {
  case ST_NULL: {
    return;
  }
  case ST_EXPR: {
    gen_expr(stmt->expr);

    // 式の評価結果としてスタックに一つの値が残っている
    // はずなので、スタックが溢れないようにポップしておく
    printf("  pop rax\n");
    return;
  }
  case ST_COMPOUND: {
    for (int i = 0; i < vec_len(stmt->stmts); i++) {
      gen_stmt(vec_get(stmt->stmts, i));
    }
    return;
  }
  case ST_IF: {
    char *else_label = make_label("if.else");
    char *end_label = make_label("if.end");
    gen_expr(stmt->cond);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je %s\n", else_label);
    gen_stmt(stmt->then_stmt);
    printf("  jmp %s\n", end_label);
    printf("%s:\n", else_label);
    gen_stmt(stmt->else_stmt);
    printf("%s:\n", end_label);
    return;
  }
  case ST_SWITCH: {
    char *end_label = make_label("switch.end");
    gen_expr(stmt->cond);
    for (int i = 0; i < vec_len(stmt->cases); i++) {
      Stmt *case_expr = vec_get(stmt->cases, i);
      gen_expr(case_expr->expr);
      printf("  pop rax\n");
      printf("  pop rdi\n");
      printf("  cmp rax, rdi\n");
      printf("  je %s\n", case_expr->label);
      printf("  push rdi\n");
    }
    printf("  pop rdi\n");
    if (stmt->default_case) {
      printf("  jmp %s\n", stmt->default_case->label);
    } else {
      printf("  jmp %s\n", end_label);
    }
    vec_push(break_labels, end_label);
    gen_stmt(stmt->body);
    vec_pop(break_labels);
    printf("%s:", end_label);
    return;
  }
  case ST_CASE:
  case ST_DEFAULT:
  case ST_LABEL: {
    printf("%s:\n", stmt->label);
    gen_stmt(stmt->body);
    return;
  }
  case ST_WHILE: {
    char *cond_label = make_label("while.cond");
    char *end_label = make_label("while.end");
    printf("%s:\n", cond_label);
    gen_expr(stmt->cond);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je %s\n", end_label);

    vec_push(break_labels, end_label);
    vec_push(continue_labels, cond_label);
    gen_stmt(stmt->body);
    vec_pop(break_labels);
    vec_pop(continue_labels);

    printf("  jmp %s\n", cond_label);
    printf("%s:\n", end_label);
    return;
  }
  case ST_DO_WHILE: {
    char *loop_label = make_label("do_while.loop");
    char *cond_label = make_label("do_while.cond");
    char *end_label = make_label("do_while.end");
    printf("%s:\n", loop_label);

    vec_push(break_labels, end_label);
    vec_push(continue_labels, cond_label);
    gen_stmt(stmt->body);
    vec_pop(break_labels);
    vec_pop(continue_labels);

    printf("%s:\n", cond_label);
    gen_expr(stmt->cond);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  jne %s\n", loop_label);
    printf("%s:\n", end_label);
    return;
  }
  case ST_FOR: {
    char *cond_label = make_label("for.cond");
    char *inc_label = make_label("for.inc");
    char *end_label = make_label("for.end");
    if (stmt->init != NULL) {
      gen_expr(stmt->init);
    }
    printf("%s:\n", cond_label);
    if (stmt->cond != NULL) {
      gen_expr(stmt->cond);
      printf("  pop rax\n");
      printf("  cmp rax, 0\n");
      printf("  je %s\n", end_label);
    }

    vec_push(break_labels, end_label);
    vec_push(continue_labels, inc_label);
    gen_stmt(stmt->body);
    vec_pop(break_labels);
    vec_pop(continue_labels);

    printf("%s:\n", inc_label);
    if (stmt->inc != NULL) {
      gen_expr(stmt->inc);
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
    if (stmt->expr != NULL) {
      gen_expr(stmt->expr);
      printf("  pop rax\n");
    }
    printf("  jmp %s\n", epilogue_label);
    return;
  }
  }
  assert(false);
}

static void gen_func(Function *func) {
  epilogue_label = make_label("func.epi");
  func_ctxt = func;
  break_labels = new_vector();
  continue_labels = new_vector();

  if (!func->is_static) {
    printf(".global %s\n", func->name);
  }
  printf("%s:\n", func->name);

  // プロローグ
  // スタックサイズ分の領域を確保する
  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");
  printf("  sub rsp, %d\n", align(func->stack_size, 16));

  // 引数をスタックへコピー
  if (func->type->func_param != NULL) {
    for (int i = 0; i < vec_len(func->type->func_param); i++) {
      Param *param = vec_get(func->type->func_param, i);
      StackVar *var = param->stack_var;
      assert(var != NULL);
      const Reg *r = get_int_reg(var->type, var->range);
      printf("  lea rax, [rbp - %d]\n",
             align(func_ctxt->stack_size, 16) - var->offset);
      switch (i) {
      case 0:
        printf("  mov [rax], %s\n", r->rdi);
        break;
      case 1:
        printf("  mov [rax], %s\n", r->rsi);
        break;
      case 2:
        printf("  mov [rax], %s\n", r->rdx);
        break;
      case 3:
        printf("  mov [rax], %s\n", r->rcx);
        break;
      case 4:
        printf("  mov [rax], %s\n", r->r8);
        break;
      case 5:
        printf("  mov [rax], %s\n", r->r9);
        break;
        // 6番目以降の引数はスタック経由で渡すため、スタックからコピーする
      default:
        printf("  mov %s, [rbp + %d]\n", r->r11, (i - 6) * 8 + 16);
        printf("  mov [rax], %s\n", r->r11);
        break;
      }
    }
  }

  gen_stmt(func->body);

  // エピローグ
  // 最後の式の結果がRAXに残っているのでそれが返り値になる
  printf("%s:\n", epilogue_label);
  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
}

static void gen_gvar_init(Initializer *init, Range range) {
  if (init->expr != NULL) {
    Expr *expr = init->expr;
    if (expr->ty == EX_NUM) {
      switch (expr->val_type->ty) {
      case TY_CHAR:
        printf("  .byte %hhd\n", expr->num_val.char_val);
        break;
      case TY_S_CHAR:
        printf("  .byte %hhd\n", expr->num_val.s_char_val);
        break;
      case TY_S_SHORT:
        printf("  .word %hd\n", expr->num_val.s_short_val);
        break;
      case TY_S_INT:
        printf("  .long %d\n", expr->num_val.s_int_val);
        break;
      case TY_S_LONG:
        printf("  .quad %ld\n", expr->num_val.s_long_val);
        break;
      case TY_S_LLONG:
        printf("  .quad %lld\n", expr->num_val.s_llong_val);
        break;
      case TY_U_CHAR:
        printf("  .byte %hhu\n", expr->num_val.u_char_val);
        break;
      case TY_U_SHORT:
        printf("  .word %hu\n", expr->num_val.u_short_val);
        break;
      case TY_U_INT:
        printf("  .long %u\n", expr->num_val.u_int_val);
        break;
      case TY_U_LONG:
        printf("  .quad %lu\n", expr->num_val.u_long_val);
        break;
      case TY_U_LLONG:
        printf("  .quad %llu\n", expr->num_val.u_llong_val);
        break;
      case TY_PTR:
        printf("  .quad %" PRIdPTR "\n", expr->num_val.ptr_val);
        break;
      case TY_ENUM:
        printf("  .long %d\n", expr->num_val.enum_val);
        break;
      case TY_VOID:
      case TY_ARRAY:
      case TY_FUNC:
      case TY_STRUCT:
      case TY_UNION:
        range_error(range, "int型ではありません");
      }
    } else if (expr->ty == '&' && expr->lhs == NULL && expr->rhs != NULL) {
      if (expr->rhs->ty != EX_GLOBAL_VAR) {
        range_error(range, "グローバル変数以外へのポインタです");
      }
      Expr *gvar = expr->rhs;
      printf("  .quad %s\n",
             gvar->global_var != NULL ? gvar->global_var->name : gvar->name);
    } else {
      range_error(range, "数値でもポインタでもありません");
    }
    return;
  }

  if (init->members != NULL) {
    assert(init->type->ty == TY_STRUCT || init->type->ty == TY_UNION);
    assert(map_size(init->members) <= 1 || init->type->ty == TY_STRUCT);
    int offset = 0;
    for (int i = 0; i < map_size(init->members); i++) {
      Initializer *meminit = map_get_by_index(init->members, i, NULL);
      if (meminit == NULL) {
        continue;
      }
      if (i > 0) {
        Member *member = vec_get(init->type->member_list, i);
        if (offset < member->offset) {
          printf("  .zero %d\n", member->offset - offset);
          offset = member->offset;
        }
        assert(offset == member->offset);
      }
      gen_gvar_init(meminit, range);
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
      gen_gvar_init(meminit, range);
    }
    return;
  }

  assert(false);
}

static void gen_gvar(GlobalVar *gvar) {
  Initializer *init = gvar->init;
  if (init == NULL) {
    printf("  .bss\n");
    if (!gvar->is_static) {
      printf(".global %s\n", gvar->name);
    }
    printf("%s:\n", gvar->name);
    printf("  .zero %d\n", get_val_size(gvar->type, gvar->range));
  } else {
    printf("  .data\n");
    if (!gvar->is_static) {
      printf(".global %s\n", gvar->name);
    }
    printf("%s:\n", gvar->name);
    gen_gvar_init(init, gvar->range);
  }
}

static void gen_str(StringLiteral *str) {
  printf("%s:\n", str->name);
  printf("  .string ");
  print_string_literal(str->val);
  printf("\n");
}

void gen(TranslationUnit *tunit) {
  printf(".intel_syntax noprefix\n");

  printf("  .text\n");
  for (int i = 0; i < vec_len(tunit->func_list); i++) {
    gen_func(vec_get(tunit->func_list, i));
  }
  for (int i = 0; i < vec_len(tunit->gvar_list); i++) {
    gen_gvar(vec_get(tunit->gvar_list, i));
  }
  printf("  .section .rodata\n");
  for (int i = 0; i < vec_len(tunit->str_list); i++) {
    gen_str(vec_get(tunit->str_list, i));
  }
}
