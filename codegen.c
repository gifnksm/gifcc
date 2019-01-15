#include "gifcc.h"
#include <stdio.h>
#include <string.h>

static char *epilogue_label = NULL;
static Function *func_ctxt = NULL;
static Vector *break_labels = NULL;
static Vector *continue_labels = NULL;

static void gen_expr(Expr *expr);

char *make_label(void) {
  static int count = 0;
  char buf[256];
  sprintf(buf, ".L%d", count++);
  return strdup(buf);
}

static void gen_lval(Expr *expr) {
  if (expr->ty == EX_IDENT) {
    printf("  mov rax, rbp\n");
    printf("  sub rax, %d\n", get_stack_offset(func_ctxt, expr->name) + 8);
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
  if (expr->ty == EX_NUM) {
    printf("  push %d\n", expr->val);
    return;
  }

  if (expr->ty == EX_IDENT) {
    gen_lval(expr);
    printf("  pop rax\n");
    printf("  mov rax, [rax]\n");
    printf("  push rax\n");
    return;
  }

  if (expr->ty == EX_CALL) {
    Vector *arg = expr->argument;
    if (expr->callee->ty != EX_IDENT) {
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
          break;
        }
      }
    }
    printf("  call %s\n", expr->callee->name);
    printf("  push rax\n");
    return;
  }

  if (expr->ty == '=') {
    gen_lval(expr->lhs);
    gen_expr(expr->rhs);
    printf("  pop rdi\n");
    printf("  pop rax\n");
    printf("  mov [rax], rdi\n");
    printf("  push rdi\n");
    return;
  }
  if (expr->ty == EX_MUL_ASSIGN || expr->ty == EX_DIV_ASSIGN ||
      expr->ty == EX_MOD_ASSIGN || expr->ty == EX_ADD_ASSIGN ||
      expr->ty == EX_SUB_ASSIGN || expr->ty == EX_LSHIFT_ASSIGN ||
      expr->ty == EX_RSHIFT_ASSIGN || expr->ty == EX_AND_ASSIGN ||
      expr->ty == EX_OR_ASSIGN || expr->ty == EX_XOR_ASSIGN) {
    gen_lval(expr->lhs);
    gen_expr(expr->rhs);
    printf("  pop rdi\n");
    printf("  pop rsi\n");
    printf("  mov rax, [rsi]\n");
    switch (expr->ty) {
    case EX_MUL_ASSIGN:
      printf("  mul rdi\n");
      break;
    case EX_DIV_ASSIGN:
      printf("  mov rdx, 0\n");
      printf("  div rdi\n");
      break;
    case EX_MOD_ASSIGN:
      printf("  mov rdx, 0\n");
      printf("  div rdi\n");
      printf("  mov rax, rdx\n");
      break;
    case EX_ADD_ASSIGN:
      printf("  add rax, rdi\n");
      break;
    case EX_SUB_ASSIGN:
      printf("  sub rax, rdi\n");
      break;
    case EX_LSHIFT_ASSIGN:
      printf("  mov rcx, rdi\n");
      printf("  shl rax, cl\n");
      break;
    case EX_RSHIFT_ASSIGN:
      printf("  mov rcx, rdi\n");
      printf("  sar rax, cl\n");
      break;
    case EX_AND_ASSIGN:
      printf("  and rax, rdi\n");
      break;
    case EX_OR_ASSIGN:
      printf("  or rax, rdi\n");
      break;
    case EX_XOR_ASSIGN:
      printf("  xor rax, rdi\n");
      break;
    }
    printf("  mov [rsi], rax\n");
    printf("  push rax\n");
    return;
  }
  if (expr->ty == EX_LOGAND) {
    char *false_label = make_label();
    char *end_label = make_label();
    gen_expr(expr->lhs);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je %s\n", false_label);
    gen_expr(expr->rhs);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
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
    printf("  cmp rax, 0\n");
    printf("  jne %s\n", true_label);
    gen_expr(expr->rhs);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
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
    printf("  cmp rax, 0\n");
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
    printf("  mov rax, [rax]");
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
    printf("  neg rax\n");
    printf("  push rax\n");
    return;
  }
  if (expr->ty == '~') {
    // `~`
    gen_expr(expr->rhs);
    printf("  pop rax\n");
    printf("  not rax\n");
    printf("  push rax\n");
    return;
  }
  if (expr->ty == '!') {
    // `!`
    gen_expr(expr->rhs);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  sete al\n");
    printf("  movzb rax, al\n");
    printf("  push rax\n");
    return;
  }
  if (expr->ty == EX_INC) {
    if (expr->lhs == NULL) {
      // 前置の `++`
      gen_lval(expr->rhs);
      printf("  pop rax\n");
      printf("  mov rdi, [rax]\n");
      printf("  add rdi, 1\n");
      printf("  mov [rax], rdi\n");
      printf("  push rdi\n");
      return;
    }
    // 後置の `++`
    gen_lval(expr->lhs);
    printf("  pop rax\n");
    printf("  mov rdi, [rax]\n");
    printf("  push rdi\n");
    printf("  add rdi, 1\n");
    printf("  mov [rax], rdi\n");
    return;
  }

  if (expr->ty == EX_DEC) {
    if (expr->lhs == NULL) {
      // 前置の `--`
      gen_lval(expr->rhs);
      printf("  pop rax\n");
      printf("  mov rdi, [rax]\n");
      printf("  sub rdi, 1\n");
      printf("  mov [rax], rdi\n");
      printf("  push rdi\n");
      return;
    }
    // 後置の `--`
    gen_lval(expr->lhs);
    printf("  pop rax\n");
    printf("  mov rdi, [rax]\n");
    printf("  push rdi\n");
    printf("  sub rdi, 1\n");
    printf("  mov [rax], rdi\n");
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
    printf("  add rax, rdi\n");
    break;
  case '-':
    printf("  sub rax, rdi\n");
    break;
  case '*':
    printf("  mul rdi\n");
    break;
  case '/':
    printf("  mov rdx, 0\n");
    printf("  div rdi\n");
    break;
  case '%':
    printf("  mov rdx, 0\n");
    printf("  div rdi\n");
    printf("  mov rax, rdx\n");
    break;
  case EX_EQEQ:
    printf("  cmp rax, rdi\n");
    printf("  sete al\n");
    printf("  movzb rax, al\n");
    break;
  case EX_NOTEQ:
    printf("  cmp rax, rdi\n");
    printf("  setne al\n");
    printf("  movzb rax, al\n");
    break;
  case '<':
    printf("  cmp rax, rdi\n");
    printf("  setl al\n");
    printf("  movzb rax, al\n");
    break;
  case '>':
    printf("  cmp rax, rdi\n");
    printf("  setg al\n");
    printf("  movzb rax, al\n");
    break;
  case EX_LTEQ:
    printf("  cmp rax, rdi\n");
    printf("  setle al\n");
    printf("  movzb rax, al\n");
    break;
  case EX_GTEQ:
    printf("  cmp rax, rdi\n");
    printf("  setge al\n");
    printf("  movzb rax, al\n");
    break;
  case EX_LSHIFT:
    printf("  mov rcx, rdi\n");
    printf("  shl rax, cl\n");
    break;
  case EX_RSHIFT:
    printf("  mov rcx, rdi\n");
    printf("  sar rax, cl\n");
    break;
  case '&':
    printf("  and rax, rdi\n");
    break;
  case '^':
    printf("  xor rax, rdi\n");
    break;
  case '|':
    printf("  or rax, rdi\n");
    break;
  case ',':
    printf("  mov rax, rdi\n");
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
    break;
  }
  case ST_COMPOUND: {
    for (int i = 0; i < stmt->stmts->len; i++) {
      gen_stmt(stmt->stmts->data[i]);
    }
    break;
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
    break;
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
    break;
  }
  case ST_CASE:
  case ST_DEFAULT:
  case ST_LABEL: {
    printf("%s:\n", stmt->label);
    break;
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
    break;
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
    break;
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
    break;
  }
  case ST_GOTO: {
    char *label = get_label(func_ctxt, stmt->name);
    if (label == NULL) {
      error("未知のラベルへのgotoです: %s", stmt->name);
    }
    printf("  jmp %s\n", label);
    break;
  }
  case ST_BREAK: {
    if (break_labels->len <= 0) {
      error("ループでもswitch文中でもない箇所にbreakがあります");
    }
    printf("  jmp %s\n", (char *)break_labels->data[break_labels->len - 1]);
    break;
  }
  case ST_CONTINUE: {
    if (continue_labels->len <= 0) {
      error("ループ中でない箇所にcontinueがあります");
    }
    printf("  jmp %s\n",
           (char *)continue_labels->data[continue_labels->len - 1]);
    break;
  }
  case ST_RETURN: {
    if (stmt->expr != NULL) {
      gen_expr(stmt->expr);
      printf("  pop rax\n");
    }
    printf("  jmp %s\n", epilogue_label);
    break;
  }
  default: { error("未知のノード種別です: %d", stmt->ty); }
  }
}

void gen(Function *func) {
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
    printf("  mov rax, rbp\n");
    printf("  sub rax, %d\n",
           get_stack_offset(func, func->params->data[i]) + 8);
    switch (i) {
    case 0:
      printf("  mov [rax], rdi\n");
      break;
    case 1:
      printf("  mov [rax], rsi\n");
      break;
    case 2:
      printf("  mov [rax], rdx\n");
      break;
    case 3:
      printf("  mov [rax], rcx\n");
      break;
    case 4:
      printf("  mov [rax], r8\n");
      break;
    case 5:
      printf("  mov [rax], r9\n");
      break;
      // 6番目以降の引数はスタック経由で渡すため、スタックからコピーする
    default:
      printf("  mov r10, rbp\n");
      printf("  add r10, %d\n", (i - 6) * 8 + 16);
      printf("  mov r11, [r10]\n");
      printf("  mov [rax], r11\n");
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
