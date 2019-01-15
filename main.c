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
    case TK_SWITCH:
      printf("%03d [SWITCH]\n", pos);
      break;
    case TK_CASE:
      printf("%03d [CASE]\n", pos);
      break;
    case TK_DEFAULT:
      printf("%03d [DEFAULT]\n", pos);
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
    case TK_GOTO:
      printf("%03d [GOTO]\n", pos);
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

static void dump_expr(Expr *expr, int level);
static void dump_binop_expr(Expr *expr, char *label, int level) {
  printf("%*s(%s\n", 2 * level, "", label);
  if (expr->lhs != NULL) {
    dump_expr(expr->lhs, level + 1);
  } else {
    printf("%*s(NULL)\n", 2 * (level + 1), "");
  }
  if (expr->rhs != NULL) {
    dump_expr(expr->rhs, level + 1);
  } else {
    printf("%*s(NULL)\n", 2 * (level + 1), "");
  }
  printf("%*s)\n", 2 * level, "");
}

static void dump_expr(Expr *expr, int level) {
  if (expr->ty <= 255) {
    dump_binop_expr(expr, (char[]){'[', expr->ty, ']', '\0'}, level);
    return;
  }

  switch (expr->ty) {
  case EX_NUM:
    printf("%*s(NUM %d)\n", 2 * level, "", expr->val);
    break;
  case EX_IDENT:
    printf("%*s(IDENT %s)\n", 2 * level, "", expr->name);
    break;
  case EX_EQEQ:
    dump_binop_expr(expr, "[==]", level);
    break;
  case EX_NOTEQ:
    dump_binop_expr(expr, "[!=]", level);
    break;
  case EX_LTEQ:
    dump_binop_expr(expr, "[<=]", level);
    break;
  case EX_GTEQ:
    dump_binop_expr(expr, "[>=]", level);
    break;
  case EX_LSHIFT:
    dump_binop_expr(expr, "[<<]", level);
    break;
  case EX_RSHIFT:
    dump_binop_expr(expr, "[>>]", level);
    break;
  case EX_LOGAND:
    dump_binop_expr(expr, "[&&]", level);
    break;
  case EX_LOGOR:
    dump_binop_expr(expr, "[||]", level);
    break;
  case EX_COND:
    printf("%*s(COND\n", 2 * level, "");
    dump_expr(expr->cond, level + 1);
    dump_expr(expr->lhs, level + 1);
    dump_expr(expr->rhs, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case EX_INC:
    dump_binop_expr(expr, "[++]", level);
    break;
  case EX_DEC:
    dump_binop_expr(expr, "[--]", level);
    break;
  case EX_MUL_ASSIGN:
    dump_binop_expr(expr, "[*=]", level);
    break;
  case EX_DIV_ASSIGN:
    dump_binop_expr(expr, "[/=]", level);
    break;
  case EX_MOD_ASSIGN:
    dump_binop_expr(expr, "[%%=]", level);
    break;
  case EX_ADD_ASSIGN:
    dump_binop_expr(expr, "[+=]", level);
    break;
  case EX_SUB_ASSIGN:
    dump_binop_expr(expr, "[-=]", level);
    break;
  case EX_LSHIFT_ASSIGN:
    dump_binop_expr(expr, "[<<=]", level);
    break;
  case EX_RSHIFT_ASSIGN:
    dump_binop_expr(expr, "[>>=]", level);
    break;
  case EX_AND_ASSIGN:
    dump_binop_expr(expr, "[^=]", level);
    break;
  case EX_OR_ASSIGN:
    dump_binop_expr(expr, "[|=]", level);
    break;
  case EX_XOR_ASSIGN:
    dump_binop_expr(expr, "[^=]", level);
    break;
  case EX_CALL:
    printf("%*s(CALL\n", 2 * level, "");
    dump_expr(expr->callee, level + 1);
    if (expr->argument != NULL) {
      for (int i = 0; i < expr->argument->len; i++) {
        dump_expr(expr->argument->data[i], level + 1);
      }
    }
    printf("%*s)\n", 2 * level, "");
    break;
  default:
    error("未知のノードです: %d\n", expr->ty);
  }
}

static void dump_stmt(Stmt *stmt, int level) {
  switch (stmt->ty) {
  case ST_EXPR:
    printf("%*s(EXPR\n", 2 * level, "");
    dump_expr(stmt->expr, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case ST_COMPOUND:
    printf("%*s(COMPOUND\n", 2 * level, "");
    for (int i = 0; i < stmt->stmts->len; i++) {
      dump_stmt(stmt->stmts->data[i], level + 1);
    }
    printf("%*s)\n", 2 * level, "");
    break;
  case ST_IF:
    printf("%*s(IF\n", 2 * level, "");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->then_stmt, level + 1);
    dump_stmt(stmt->else_stmt, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case ST_SWITCH:
    printf("%*s(SWITCH\n", 2 * level, "");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case ST_CASE:
    printf("%*s(CASE\n", 2 * level, "");
    dump_expr(stmt->expr, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case ST_DEFAULT:
    printf("%*s(DEFAULT)\n", 2 * level, "");
    break;
  case ST_LABEL:
    printf("%*s(LABEL %s)\n", 2 * level, "", stmt->name);
    break;
  case ST_WHILE:
    printf("%*s(WHILE\n", 2 * level, "");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case ST_DO_WHILE:
    printf("%*s(DO_WHILE\n", 2 * level, "");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case ST_FOR:
    printf("%*s(FOR\n", 2 * level, "");
    if (stmt->init != NULL) {
      dump_expr(stmt->init, level + 1);
    } else {
      printf("%*s(NULL)\n", 2 * (level + 1), "");
    }
    if (stmt->cond != NULL) {
      dump_expr(stmt->cond, level + 1);
    } else {
      printf("%*s(NULL)\n", 2 * (level + 1), "");
    }
    if (stmt->inc != NULL) {
      dump_expr(stmt->inc, level + 1);
    } else {
      printf("%*s(NULL)\n", 2 * (level + 1), "");
    }
    dump_stmt(stmt->body, level + 1);
    printf("%*s)\n", 2 * level, "");
    break;
  case ST_GOTO:
    printf("%*s(GOTO %s)\n", 2 * level, "", stmt->name);
    break;
  case ST_BREAK:
    printf("%*s(BREAK)\n", 2 * level, "");
    break;
  case ST_CONTINUE:
    printf("%*s(CONTINUE)\n", 2 * level, "");
    break;
  case ST_NULL:
    printf("%*s(NULL)\n", 2 * level, "");
    break;
  default:
    error("未知のノードです: %d\n", stmt->ty);
  }
}

static void output_ast(void) {
  for (int pos = 0; true; pos++) {
    Stmt *stmt = get_stmt(pos);
    if (stmt == NULL) {
      break;
    }

    dump_stmt(stmt, 0);
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
  printf("  sub rsp, %d\n", align(get_stack_size(), 16));

  // 先頭の式から順にコード生成
  for (int i = 0; get_stmt(i); i++) {
    gen(get_stmt(i));
  }

  // エピローグ
  // 最後の式の結果がRAXに残っているのでそれが返り値になる
  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
  return 0;
}
