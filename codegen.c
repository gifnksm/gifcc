#include "gifcc.h"
#include <stdio.h>

static void gen_lval(Node *node) {
  if (node->ty == ND_IDENT) {
    printf("  mov rax, rbp\n");
    printf("  sub rax, %d\n", get_stack_offset(node->name) + 8);
    printf("  push rax\n");
    return;
  }
  error("代入の左辺値が変数ではありません");
}

void gen(Node *node) {
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
        gen(arg->data[i]);
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
    gen(node->rhs);
    printf("  pop rdi\n");
    printf("  pop rax\n");
    printf("  mov [rax], rdi\n");
    printf("  push rdi\n");
    return;
  }

  gen(node->lhs);
  gen(node->rhs);

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
