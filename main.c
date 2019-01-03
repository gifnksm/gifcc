#include "9cc.h"
#include <getopt.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// エラーを報告するための関数
void error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  va_end(ap);
  exit(1);
}

static void output_token(void) {
  for (int pos = 0; true; pos++) {
    Token *token = get_token(pos);

    if (token->ty <= 255) {
      printf("%03d [%c]\n", pos, token->ty);
      continue;
    }

    switch (token->ty) {
    case TK_NUM:
      printf("%03d NUM %d\n", pos, token->val);
      break;
    case TK_IDENT:
      printf("%03d IDENT %s\n", pos, token->name);
      break;
    case TK_EQEQ:
      printf("%03d [==]\n", pos);
      break;
    case TK_NOTEQ:
      printf("%03d [!=]\n", pos);
      break;
    case TK_EOF:
      printf("%03d EOF\n", pos);
      break;
    }

    if (token->ty == TK_EOF) {
      break;
    }
  }
}

static void dump_node(Node *node, int level);
static void dump_complex_node(Node *node, char *label, int level) {
  printf("%*s(%s\n", 2 * level, "", label);
  if (node->lhs != NULL)
    dump_node(node->lhs, level + 1);
  if (node->rhs != NULL)
    dump_node(node->rhs, level + 1);
  printf("%*s)\n", 2 * level, "");
}

static void dump_node(Node *node, int level) {
  if (node->ty <= 255) {
    dump_complex_node(node, (char[]){'[', node->ty, ']', '\0'}, level + 1);
    return;
  }

  switch (node->ty) {
  case ND_NUM:
    printf("%*s(NUM %d)\n", 2 * level, "", node->val);
    break;
  case ND_IDENT:
    printf("%*s(IDENT %s)\n", 2 * level, "", node->name);
    break;
  case ND_EQEQ:
    dump_complex_node(node, "[==]", level + 1);
    break;
  case ND_NOTEQ:
    dump_complex_node(node, "[!=]", level + 1);
    break;
  }
}

static void output_ast(void) {
  for (int pos = 0; true; pos++) {
    Node *node = get_node(pos);
    if (node == NULL) {
      break;
    }

    dump_node(node, 0);
  }
}

enum {
  OPTVAL_TEST = 256,
  OPTVAL_OUTPUT,
};

typedef enum {
  OUTPUT_ASM,
  OUTPUT_TOKEN,
  OUTPUT_AST,
} output_t;

struct option longopts[] = {
    {"test", no_argument, NULL, OPTVAL_TEST},
    {"output", required_argument, NULL, OPTVAL_OUTPUT},
    {NULL, 0, 0, 0},
};

int main(int argc, char **argv) {
  output_t output_mode = OUTPUT_ASM;
  while (true) {
    int c = getopt_long(argc, argv, "", longopts, NULL);
    if (c == -1)
      break;

    switch (c) {
    case OPTVAL_TEST:
      runtest();
      return 0;
    case OPTVAL_OUTPUT:
      if (strcmp(optarg, "asm") == 0) {
        output_mode = OUTPUT_ASM;
      } else if (strcmp(optarg, "token") == 0) {
        output_mode = OUTPUT_TOKEN;
      } else if (strcmp(optarg, "ast") == 0) {
        output_mode = OUTPUT_AST;
      } else {
        error("不明なオプションの値です: %s", optarg);
        return 1;
      }
      break;
    case '?':
      return 1;
    }
  }

  if (optind != argc - 1) {
    error("引数の個数が正しくありません");
  }

  char *input = argv[optind];

  // トークナイズしてパースする
  // 結果はcodeに保存される
  tokenize(input);
  if (output_mode == OUTPUT_TOKEN) {
    output_token();
    return 0;
  }

  program();
  if (output_mode == OUTPUT_AST) {
    output_ast();
    return 0;
  }

  // アセンブリの前半部分を出力
  printf(".intel_syntax noprefix\n");
  printf(".global main\n");
  printf("main:\n");

  // プロローグ
  // スタックサイズ分の領域を確保する
  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");
  printf("  sub rsp, %d\n", get_stack_size());

  // 先頭の式から順にコード生成
  for (int i = 0; get_node(i); i++) {
    gen(get_node(i));

    // 式の評価結果としてスタックに一つの値が残っている
    // はずなので、スタックが溢れないようにポップしておく
    printf("  pop rax\n");
  }

  // エピローグ
  // 最後の式の結果がRAXに残っているのでそれが返り値になる
  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
  return 0;
}
