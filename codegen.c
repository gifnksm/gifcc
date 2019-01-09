#include "gifcc.h"
#include <stdio.h>
#include <string.h>

Vector *break_labels = NULL;
Vector *continue_labels = NULL;

static char *make_label(void) {
  static int count = 0;
  char buf[256];
  sprintf(buf, ".L%d", count++);
  return strdup(buf);
}

static void gen_lval(Node *node) {
  if (node->ty == ND_IDENT) {
    printf("  mov rax, rbp\n");
    printf("  sub rax, %d\n", get_stack_offset(node->name) + 8);
    printf("  push rax\n");
    return;
  }
  error("代入の左辺値が変数ではありません");
}

static void gen_expr(Node *node) {
  if (node->ty == ND_NUM) {
    printf("  push %d\n", node->val);
    return;
  }

  if (node->ty == ND_IDENT) {
    gen_lval(node);
    printf("  pop rax\n");
    printf("  mov rax, [rax]\n");
    printf("  push rax\n");
    return;
  }

  if (node->ty == ND_CALL) {
    Vector *arg = node->argument;
    if (node->callee->ty != ND_IDENT) {
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
          printf("  pop r8d\n");
          break;
        case 5:
          printf("  pop r9d\n");
          break;
        // 6番目以降の引数はスタック経由で渡すため、pushされたままにする
        default:
          break;
        }
      }
    }
    printf("  call %s\n", node->callee->name);
    printf("  push rax\n");
    return;
  }

  if (node->ty == '=') {
    gen_lval(node->lhs);
    gen_expr(node->rhs);
    printf("  pop rdi\n");
    printf("  pop rax\n");
    printf("  mov [rax], rdi\n");
    printf("  push rdi\n");
    return;
  }
  if (node->ty == ND_MUL_ASSIGN || node->ty == ND_DIV_ASSIGN ||
      node->ty == ND_MOD_ASSIGN || node->ty == ND_ADD_ASSIGN ||
      node->ty == ND_SUB_ASSIGN || node->ty == ND_LSHIFT_ASSIGN ||
      node->ty == ND_RSHIFT_ASSIGN || node->ty == ND_AND_ASSIGN ||
      node->ty == ND_OR_ASSIGN || node->ty == ND_XOR_ASSIGN) {
    gen_lval(node->lhs);
    gen_expr(node->rhs);
    printf("  pop rdi\n");
    printf("  pop rsi\n");
    printf("  mov rax, [rsi]\n");
    switch (node->ty) {
    case ND_MUL_ASSIGN:
      printf("  mul rdi\n");
      break;
    case ND_DIV_ASSIGN:
      printf("  mov rdx, 0\n");
      printf("  div rdi\n");
      break;
    case ND_MOD_ASSIGN:
      printf("  mov rdx, 0\n");
      printf("  div rdi\n");
      printf("  mov rax, rdx\n");
      break;
    case ND_ADD_ASSIGN:
      printf("  add rax, rdi\n");
      break;
    case ND_SUB_ASSIGN:
      printf("  sub rax, rdi\n");
      break;
    case ND_LSHIFT_ASSIGN:
      printf("  mov rcx, rdi\n");
      printf("  shl rax, cl\n");
      break;
    case ND_RSHIFT_ASSIGN:
      printf("  mov rcx, rdi\n");
      printf("  sar rax, cl\n");
      break;
    case ND_AND_ASSIGN:
      printf("  and rax, rdi\n");
      break;
    case ND_OR_ASSIGN:
      printf("  or rax, rdi\n");
      break;
    case ND_XOR_ASSIGN:
      printf("  xor rax, rdi\n");
      break;
    }
    printf("  mov [rsi], rax\n");
    printf("  push rax\n");
    return;
  }
  if (node->ty == ND_LOGAND) {
    char *false_label = make_label();
    char *end_label = make_label();
    gen_expr(node->lhs);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je %s\n", false_label);
    gen_expr(node->rhs);
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

  if (node->ty == ND_LOGOR) {
    char *true_label = make_label();
    char *end_label = make_label();
    gen_expr(node->lhs);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  jne %s\n", true_label);
    gen_expr(node->rhs);
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

  if (node->ty == ND_COND) {
    char *else_label = make_label();
    char *end_label = make_label();
    gen_expr(node->cond);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je %s\n", else_label);
    gen_expr(node->then_node);
    printf("  jmp %s\n", end_label);
    printf("%s:\n", else_label);
    gen_expr(node->else_node);
    printf("%s:\n", end_label);
    return;
  }

  if (node->ty == '+' && node->lhs == NULL) {
    // 単項の `+`
    gen_expr(node->rhs);
    return;
  }
  if (node->ty == '-' && node->lhs == NULL) {
    // 単項の `-`
    gen_expr(node->rhs);
    printf("  pop rax\n");
    printf("  neg rax\n");
    printf("  push rax\n");
    return;
  }
  if (node->ty == '~') {
    // `~`
    gen_expr(node->rhs);
    printf("  pop rax\n");
    printf("  not rax\n");
    printf("  push rax\n");
    return;
  }
  if (node->ty == '!') {
    // `!`
    gen_expr(node->rhs);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  sete al\n");
    printf("  movzb rax, al\n");
    printf("  push rax\n");
    return;
  }
  if (node->ty == ND_INC) {
    if (node->lhs == NULL) {
      // 前置の `++`
      gen_lval(node->rhs);
      printf("  pop rax\n");
      printf("  mov rdi, [rax]\n");
      printf("  add rdi, 1\n");
      printf("  mov [rax], rdi\n");
      printf("  push rdi\n");
      return;
    }
    // 後置の `++`
    gen_lval(node->lhs);
    printf("  pop rax\n");
    printf("  mov rdi, [rax]\n");
    printf("  push rdi\n");
    printf("  add rdi, 1\n");
    printf("  mov [rax], rdi\n");
    return;
  }

  if (node->ty == ND_DEC) {
    if (node->lhs == NULL) {
      // 前置の `--`
      gen_lval(node->rhs);
      printf("  pop rax\n");
      printf("  mov rdi, [rax]\n");
      printf("  sub rdi, 1\n");
      printf("  mov [rax], rdi\n");
      printf("  push rdi\n");
      return;
    }
    // 後置の `--`
    gen_lval(node->lhs);
    printf("  pop rax\n");
    printf("  mov rdi, [rax]\n");
    printf("  push rdi\n");
    printf("  sub rdi, 1\n");
    printf("  mov [rax], rdi\n");
    return;
  }

  // 二項演算子
  if (node->lhs == NULL || node->rhs == NULL)
    error("lhs, rhsのいずれかまたは両方が空です: %p %p", node->lhs, node->rhs);

  gen_expr(node->lhs);
  gen_expr(node->rhs);

  printf("  pop rdi\n");
  printf("  pop rax\n");

  switch (node->ty) {
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
  case ND_EQEQ:
    printf("  cmp rax, rdi\n");
    printf("  sete al\n");
    printf("  movzb rax, al\n");
    break;
  case ND_NOTEQ:
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
  case ND_LTEQ:
    printf("  cmp rax, rdi\n");
    printf("  setle al\n");
    printf("  movzb rax, al\n");
    break;
  case ND_GTEQ:
    printf("  cmp rax, rdi\n");
    printf("  setge al\n");
    printf("  movzb rax, al\n");
    break;
  case ND_LSHIFT:
    printf("  mov rcx, rdi\n");
    printf("  shl rax, cl\n");
    break;
  case ND_RSHIFT:
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
  default:
    error("未知のノード種別です: %d", node->ty);
  }

  printf("  push rax\n");
}

static void gen_stmt(Node *stmt) {
  switch (stmt->ty) {
  case ND_NULL: {
    return;
  }
  case ND_EXPR: {
    gen_expr(stmt->expr);

    // 式の評価結果としてスタックに一つの値が残っている
    // はずなので、スタックが溢れないようにポップしておく
    printf("  pop rax\n");
    break;
  }
  case ND_COMPOUND: {
    for (int i = 0; i < stmt->stmts->len; i++)
      gen_stmt(stmt->stmts->data[i]);
    break;
  }
  case ND_IF: {
    char *else_label = make_label();
    char *end_label = make_label();
    gen_expr(stmt->cond);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je %s\n", else_label);
    gen_stmt(stmt->then_node);
    printf("  jmp %s\n", end_label);
    printf("%s:\n", else_label);
    gen_stmt(stmt->else_node);
    printf("%s:\n", end_label);
    break;
  }
  case ND_WHILE: {
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
  case ND_DO_WHILE: {
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
  case ND_FOR: {
    char *cond_label = make_label();
    char *inc_label = make_label();
    char *end_label = make_label();
    if (stmt->init != NULL)
      gen_expr(stmt->init);
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
    if (stmt->inc != NULL)
      gen_expr(stmt->inc);
    printf("  jmp %s\n", cond_label);
    printf("%s:\n", end_label);
    break;
  }
  case ND_BREAK: {
    if (break_labels->len <= 0)
      error("ループでもswitch文中でもない箇所にbreakがあります");
    printf("  jmp %s\n", (char *)break_labels->data[break_labels->len - 1]);
    break;
  }
  case ND_CONTINUE: {
    if (continue_labels->len <= 0)
      error("ループ中でない箇所にcontinueがあります");
    printf("  jmp %s\n",
           (char *)continue_labels->data[continue_labels->len - 1]);
    break;
  }
  default: { error("未知のノード種別です: %d", stmt->ty); }
  }
}

void gen(Node *node) {
  break_labels = new_vector();
  continue_labels = new_vector();
  gen_stmt(node);
}
