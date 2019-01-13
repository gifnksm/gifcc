#include "gifcc.h"
#include <getopt.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// エラーを報告するための関数
void error_raw(const char *file, int line, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "%s:%d:", file, line);
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
    case TK_LTEQ:
      printf("%03d [<=]\n", pos);
      break;
    case TK_GTEQ:
      printf("%03d [>=]\n", pos);
      break;
    case TK_LSHIFT:
      printf("%03d [<<]\n", pos);
      break;
    case TK_RSHIFT:
      printf("%03d [>>]\n", pos);
      break;
    case TK_LOGAND:
      printf("%03d [&&]\n", pos);
      break;
    case TK_LOGOR:
      printf("%03d [||]\n", pos);
      break;
    case TK_EOF:
      printf("%03d EOF\n", pos);
      break;
    case TK_INC:
      printf("%03d [++]\n", pos);
      break;
    case TK_DEC:
      printf("%03d [--]\n", pos);
      break;
    case TK_MUL_ASSIGN:
      printf("%03d [*=]\n", pos);
      break;
    case TK_DIV_ASSIGN:
      printf("%03d [/=]\n", pos);
      break;
    case TK_MOD_ASSIGN:
      printf("%03d [%%=]\n", pos);
      break;
    case TK_ADD_ASSIGN:
      printf("%03d [+=]\n", pos);
      break;
    case TK_SUB_ASSIGN:
      printf("%03d [-=]\n", pos);
      break;
    case TK_LSHIFT_ASSIGN:
      printf("%03d [<<=]\n", pos);
      break;
    case TK_RSHIFT_ASSIGN:
      printf("%03d [>>=]\n", pos);
      break;
    case TK_AND_ASSIGN:
      printf("%03d [&=]\n", pos);
      break;
    case TK_OR_ASSIGN:
      printf("%03d [|=]\n", pos);
      break;
    case TK_XOR_ASSIGN:
      printf("%03d [^=]\n", pos);
      break;
    case TK_IF:
      printf("%03d [IF]\n", pos);
      break;
    case TK_ELSE:
      printf("%03d [ELSE]\n", pos);
      break;
    case TK_WHILE:
      printf("%03d [WHILE]\n", pos);
      break;
    case TK_DO:
      printf("%03d [DO]\n", pos);
      break;
    case TK_FOR:
      printf("%03d [FOR]\n", pos);
      break;
    case TK_BREAK:
      printf("%03d [BREAK]\n", pos);
      break;
    case TK_CONTINUE:
      printf("%03d [CONTINUE]\n", pos);
      break;
    default:
      error("未知のトークンです: %d\n", token->ty);
    }

    if (token->ty == TK_EOF) {
      break;
    }
  }
}

static void dump_node(Node *node, int level);
static void dump_binop_node(Node *node, char *label, int level) {
  printf("%*s(%s\n", 2 * level, "", label);
  if (node->lhs != NULL) {
    dump_node(node->lhs, level + 1);
  } else {
    printf("%*s(NULL)\n", 2 * (level + 1), "");
  }
  if (node->rhs != NULL) {
    dump_node(node->rhs, level + 1);
  } else {
    printf("%*s(NULL)\n", 2 * (level + 1), "");
  }
  printf("%*s)\n", 2 * level, "");
}

static void dump_node(Node *node, int level) {
  if (node->ty <= 255) {
    dump_binop_node(node, (char[]){'[', node->ty, ']', '\0'}, level);
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
    dump_binop_node(node, "[==]", level);
    break;
  case ND_NOTEQ:
    dump_binop_node(node, "[!=]", level);
    break;
  case ND_LTEQ:
    dump_binop_node(node, "[<=]", level);
    break;
  case ND_GTEQ:
    dump_binop_node(node, "[>=]", level);
    break;
  case ND_LSHIFT:
    dump_binop_node(node, "[<<]", level);
    break;
  case ND_RSHIFT:
    dump_binop_node(node, "[>>]", level);
    break;
  case ND_LOGAND:
    dump_binop_node(node, "[&&]", level);
    break;
  case ND_LOGOR:
    dump_binop_node(node, "[||]", level);
    break;
  case ND_COND:
    printf("%*s(COND\n", 2 * level, "");
    dump_node(node->cond, level + 1);
    dump_node(node->then_node, level + 1);
    dump_node(node->else_node, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case ND_INC:
    dump_binop_node(node, "[++]", level);
    break;
  case ND_DEC:
    dump_binop_node(node, "[--]", level);
    break;
  case ND_MUL_ASSIGN:
    dump_binop_node(node, "[*=]", level);
    break;
  case ND_DIV_ASSIGN:
    dump_binop_node(node, "[/=]", level);
    break;
  case ND_MOD_ASSIGN:
    dump_binop_node(node, "[%%=]", level);
    break;
  case ND_ADD_ASSIGN:
    dump_binop_node(node, "[+=]", level);
    break;
  case ND_SUB_ASSIGN:
    dump_binop_node(node, "[-=]", level);
    break;
  case ND_LSHIFT_ASSIGN:
    dump_binop_node(node, "[<<=]", level);
    break;
  case ND_RSHIFT_ASSIGN:
    dump_binop_node(node, "[>>=]", level);
    break;
  case ND_AND_ASSIGN:
    dump_binop_node(node, "[^=]", level);
    break;
  case ND_OR_ASSIGN:
    dump_binop_node(node, "[|=]", level);
    break;
  case ND_XOR_ASSIGN:
    dump_binop_node(node, "[^=]", level);
    break;
  case ND_CALL:
    printf("%*s(CALL\n", 2 * level, "");
    dump_node(node->callee, level + 1);
    if (node->argument != NULL) {
      for (int i = 0; i < node->argument->len; i++) {
        dump_node(node->argument->data[i], level + 1);
      }
    }
    printf("%*s)\n", 2 * level, "");
    break;
  case ND_EXPR:
    printf("%*s(EXPR\n", 2 * level, "");
    dump_node(node->expr, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case ND_COMPOUND:
    printf("%*s(COMPOUND\n", 2 * level, "");
    for (int i = 0; i < node->stmts->len; i++) {
      dump_node(node->stmts->data[i], level + 1);
    }
    printf("%*s)\n", 2 * level, "");
    break;
  case ND_IF:
    printf("%*s(IF\n", 2 * level, "");
    dump_node(node->cond, level + 1);
    dump_node(node->then_node, level + 1);
    dump_node(node->else_node, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case ND_WHILE:
    printf("%*s(WHILE\n", 2 * level, "");
    dump_node(node->cond, level + 1);
    dump_node(node->body, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case ND_DO_WHILE:
    printf("%*s(DO_WHILE\n", 2 * level, "");
    dump_node(node->cond, level + 1);
    dump_node(node->body, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case ND_FOR:
    printf("%*s(FOR\n", 2 * level, "");
    if (node->init != NULL) {
      dump_node(node->init, level + 1);
    } else {
      printf("%*s(NULL)\n", 2 * (level + 1), "");
    }
    if (node->cond != NULL) {
      dump_node(node->cond, level + 1);
    } else {
      printf("%*s(NULL)\n", 2 * (level + 1), "");
    }
    if (node->inc != NULL) {
      dump_node(node->inc, level + 1);
    } else {
      printf("%*s(NULL)\n", 2 * (level + 1), "");
    }
    dump_node(node->body, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case ND_BREAK:
    printf("%*s(BREAK)\n", 2 * level, "");
    break;
  case ND_CONTINUE:
    printf("%*s(CONTINUE)\n", 2 * level, "");
    break;
  case ND_NULL:
    printf("%*s(NULL)\n", 2 * level, "");
    break;
  default:
    error("未知のノードです: %d\n", node->ty);
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
    if (c == -1) {
      break;
    }

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
  }

  // エピローグ
  // 最後の式の結果がRAXに残っているのでそれが返り値になる
  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
  return 0;
}
