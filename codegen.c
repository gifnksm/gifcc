#include "gifcc.h"
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

static const Reg *get_int_reg(Type *ty) {
  switch (get_val_size(ty)) {
  case 8:
    return &Reg8;
  case 4:
    return &Reg4;
  case 1:
    return &Reg1;
  default:
    error("サポートしていない型サイズです");
  }
}

char *make_label(void) {
  static int count = 0;
  char buf[256];
  sprintf(buf, ".L%d", count++);
  return strdup(buf);
}

static void gen_lval(Expr *expr) {
  if (expr->ty == EX_STACK_VAR) {
    StackVar *var = get_stack_variable(func_ctxt, expr->name);
    if (var == NULL) {
      error("変数が定義されていません: %s", expr->name);
    }
    printf("  mov rax, rbp\n");
    printf("  sub rax, %d\n", var->offset + 8);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == EX_GLOBAL_VAR) {
    printf("  lea rax, %s[rip]\n", expr->name);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == '*' && expr->lhs == NULL) {
    gen_expr(expr->rhs);
    return;
  }
  error("左辺値が変数ではありません");
}

static void gen_expr(Expr *expr) {
  const Reg *r = get_int_reg(expr->val_type);

  if (expr->ty == EX_NUM) {
    printf("  push %d\n", expr->val);
    return;
  }

  if (expr->ty == EX_STACK_VAR) {
    StackVar *var = get_stack_variable(func_ctxt, expr->name);
    if (var == NULL) {
      error("変数が定義されていません: %s", expr->name);
    }
    printf("  mov %s, [rbp - %d]\n", r->rax, var->offset + 8);
    printf("  push rax\n");
    return;
  }

  if (expr->ty == EX_GLOBAL_VAR) {
    printf("  mov %s, %s[rip]\n", r->rax, expr->name);
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
    if (expr->callee->ty != EX_GLOBAL_VAR) {
      error("識別子以外を関数として呼び出そうとしました");
    }
    if (arg && arg->len > 0) {
      for (int i = arg->len - 1; i >= 0; i--) {
        gen_expr(arg->data[i]);
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
    printf("  call %s\n", expr->callee->name);
    if (num_push > 0) {
      printf("  add rsp, %d\n", 8 * num_push);
    }
    printf("  push rax\n");
    return;
  }

  if (expr->ty == EX_CAST) {
    gen_expr(expr->expr);
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
    char *false_label = make_label();
    char *end_label = make_label();
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
    char *true_label = make_label();
    char *end_label = make_label();
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
    char *else_label = make_label();
    char *end_label = make_label();
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
    // `!`
    gen_expr(expr->rhs);
    printf("  pop rax\n");
    printf("  cmp %s, 0\n", r->rax);
    printf("  sete al\n");
    printf("  movzb %s, al\n", r->rax);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == EX_INC) {
    if (expr->lhs == NULL) {
      // 前置の `++`
      gen_lval(expr->rhs);
      printf("  pop rax\n");
      printf("  mov %s, [rax]\n", r->rdi);
      printf("  add %s, %d\n", r->rdi, expr->val);
      printf("  mov [rax], %s\n", r->rdi);
      printf("  push rdi\n");
      return;
    }
    // 後置の `++`
    gen_lval(expr->lhs);
    printf("  pop rax\n");
    printf("  mov %s, [rax]\n", r->rdi);
    printf("  push rdi\n");
    printf("  add %s, %d\n", r->rdi, expr->val);
    printf("  mov [rax], %s\n", r->rdi);
    return;
  }

  if (expr->ty == EX_DEC) {
    if (expr->lhs == NULL) {
      // 前置の `--`
      gen_lval(expr->rhs);
      printf("  pop rax\n");
      printf("  mov %s, [rax]\n", r->rdi);
      printf("  sub %s, %d\n", r->rdi, expr->val);
      printf("  mov [rax], %s\n", r->rdi);
      printf("  push rdi\n");
      return;
    }
    // 後置の `--`
    gen_lval(expr->lhs);
    printf("  pop rax\n");
    printf("  mov %s, [rax]\n", r->rdi);
    printf("  push rdi\n");
    printf("  sub %s, %d\n", r->rdi, expr->val);
    printf("  mov [rax], %s\n", r->rdi);
    return;
  }

  // 二項演算子
  if (expr->lhs == NULL || expr->rhs == NULL) {
    error("lhs, rhsのいずれかまたは両方が空です: %p %p", expr->lhs, expr->rhs);
  }

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
    printf("  mul %s\n", r->rdi);
    break;
  case '/':
    printf("  mov %s, 0\n", r->rdx);
    printf("  div %s\n", r->rdi);
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
    printf("  setl al\n");
    printf("  movzb %s, al\n", r->rax);
    break;
  case '>':
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    printf("  setg al\n");
    printf("  movzb %s, al\n", r->rax);
    break;
  case EX_LTEQ:
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    printf("  setle al\n");
    printf("  movzb %s, al\n", r->rax);
    break;
  case EX_GTEQ:
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    printf("  setge al\n");
    printf("  movzb %s, al\n", r->rax);
    break;
  case EX_LSHIFT:
    printf("  mov %s, %s\n", r->rcx, r->rdi);
    printf("  shl %s, cl\n", r->rax);
    break;
  case EX_RSHIFT:
    printf("  mov %s, %s\n", r->rcx, r->rdi);
    printf("  sar %s, cl\n", r->rax);
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
    error("未知のノード種別です: %d", expr->ty);
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
    for (int i = 0; i < stmt->stmts->len; i++) {
      gen_stmt(stmt->stmts->data[i]);
    }
    return;
  }
  case ST_IF: {
    char *else_label = make_label();
    char *end_label = make_label();
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
    char *end_label = make_label();
    gen_expr(stmt->cond);
    for (int i = 0; i < stmt->cases->len; i++) {
      Stmt *case_expr = stmt->cases->data[i];
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
    return;
  }
  case ST_WHILE: {
    char *cond_label = make_label();
    char *end_label = make_label();
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
    char *loop_label = make_label();
    char *cond_label = make_label();
    char *end_label = make_label();
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
    char *cond_label = make_label();
    char *inc_label = make_label();
    char *end_label = make_label();
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
      error("未知のラベルへのgotoです: %s", stmt->name);
    }
    printf("  jmp %s\n", label);
    return;
  }
  case ST_BREAK: {
    if (break_labels->len <= 0) {
      error("ループでもswitch文中でもない箇所にbreakがあります");
    }
    printf("  jmp %s\n", (char *)break_labels->data[break_labels->len - 1]);
    return;
  }
  case ST_CONTINUE: {
    if (continue_labels->len <= 0) {
      error("ループ中でない箇所にcontinueがあります");
    }
    printf("  jmp %s\n",
           (char *)continue_labels->data[continue_labels->len - 1]);
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
  error("未知のノード種別です: %d", stmt->ty);
}

static void gen_func(Function *func) {
  epilogue_label = make_label();
  func_ctxt = func;
  break_labels = new_vector();
  continue_labels = new_vector();

  printf(".global %s\n", func->name);
  printf("%s:\n", func->name);

  // プロローグ
  // スタックサイズ分の領域を確保する
  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");
  printf("  sub rsp, %d\n", align(func->stack_size, 16));

  // 引数をスタックへコピー
  for (int i = 0; i < func->params->len; i++) {
    char *name = func->params->data[i];
    StackVar *var = get_stack_variable(func, name);
    if (var == NULL) {
      error("変数が定義されていません: %s", name);
    }
    const Reg *r = get_int_reg(var->type);
    printf("  mov rax, rbp\n");
    printf("  sub rax, %d\n", var->offset + 8);
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
      printf("  mov r10, rbp\n");
      printf("  add r10, %d\n", (i - 6) * 8 + 16);
      printf("  mov %s, [r10]\n", r->r11);
      printf("  mov [rax], %s\n", r->r11);
      break;
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

static void gen_gvar(GlobalVar *gvar) {
  printf(".global %s\n", gvar->name);
  printf("%s:\n", gvar->name);
  printf("  .zero %d\n", get_val_size(gvar->type));
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
  for (int i = 0; i < tunit->func_list->len; i++) {
    gen_func(tunit->func_list->data[i]);
  }
  printf("  .bss\n");
  for (int i = 0; i < tunit->gvar_list->len; i++) {
    gen_gvar(tunit->gvar_list->data[i]);
  }
  printf("  .section .rodata\n");
  for (int i = 0; i < tunit->str_list->len; i++) {
    gen_str(tunit->str_list->data[i]);
  }
}
