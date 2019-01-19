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

static void output_token(const char *input) {
  Tokenizer *tokenizer = new_tokenizer(input);
  for (int pos = 0;; pos++) {
    Token *token = token_pop(tokenizer);
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
    case TK_VOID:
      printf("%03d [VOID]\n", pos);
      break;
    case TK_INT:
      printf("%03d [INT]\n", pos);
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
    case TK_RETURN:
      printf("%03d [RETURN]\n", pos);
      break;
    default:
      error("未知のトークンです: %d\n", token->ty);
    }

    if (token->ty == TK_EOF) {
      break;
    }
  }
}

static void dump_indent(int level) { printf("%*s", 2 * level, ""); }
static void dump_type_inner(Type *ty) {
  switch (ty->ty) {
  case TY_INT:
    printf("int");
    break;
  case TY_PTR:
    printf("PTR(");
    dump_type_inner(ty->ptrof);
    printf(")");
    break;
  default:
    error("未知の型です: %d\n", ty->ty);
  }
}
static void dump_type(Type *ty) {
  printf("<");
  dump_type_inner(ty);
  printf(">");
}
static void dump_expr(Expr *expr, int level);
static void dump_binop_expr(Expr *expr, char *label, int level) {
  dump_indent(level);
  dump_type(expr->val_type);
  printf("(%s\n", label);
  if (expr->lhs != NULL) {
    dump_expr(expr->lhs, level + 1);
  } else {
    dump_indent(level + 1);
    printf("<void>(NULL)\n");
  }
  if (expr->rhs != NULL) {
    dump_expr(expr->rhs, level + 1);
  } else {
    dump_indent(level + 1);
    printf("<void>(NULL)\n");
  }
  dump_indent(level);
  printf(")\n");
}

static void dump_binop_expr_incdec(Expr *expr, char *label, int level) {
  dump_indent(level);
  dump_type(expr->val_type);
  printf("(%s %d\n", label, expr->val);
  if (expr->lhs != NULL) {
    dump_expr(expr->lhs, level + 1);
  } else {
    dump_indent(level + 1);
    printf("<void>(NULL)\n");
  }
  if (expr->rhs != NULL) {
    dump_expr(expr->rhs, level + 1);
  } else {
    dump_indent(level + 1);
    printf("<void>(NULL)\n");
  }
  dump_indent(level);
  printf(")\n");
}

static void dump_expr(Expr *expr, int level) {
  if (expr->ty <= 255) {
    dump_binop_expr(expr, (char[]){'[', expr->ty, ']', '\0'}, level);
    return;
  }

  switch (expr->ty) {
  case EX_NUM:
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(NUM %d)\n", expr->val);
    break;
  case EX_IDENT:
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(IDENT %s)\n", expr->name);
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
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(COND\n");
    dump_expr(expr->cond, level + 1);
    dump_expr(expr->lhs, level + 1);
    dump_expr(expr->rhs, level + 1);
    dump_indent(level);
    printf(")\n");
    break;
  case EX_INC:
    dump_binop_expr_incdec(expr, "[++]", level);
    break;
  case EX_DEC:
    dump_binop_expr_incdec(expr, "[--]", level);
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
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(CALL\n");
    dump_expr(expr->callee, level + 1);
    if (expr->argument != NULL) {
      for (int i = 0; i < expr->argument->len; i++) {
        dump_expr(expr->argument->data[i], level + 1);
      }
    }
    dump_indent(level);
    printf(")\n");
    break;
  case EX_CAST:
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(CAST\n");
    dump_expr(expr->expr, level + 1);
    dump_indent(level);
    printf(")\n");
    break;
  default:
    error("未知のノードです: %d\n", expr->ty);
  }
}

static void dump_stmt(Stmt *stmt, int level) {
  switch (stmt->ty) {
  case ST_EXPR:
    dump_indent(level);
    printf("{EXPR\n");
    dump_expr(stmt->expr, level + 1);
    dump_indent(level);
    printf("}\n");
    break;
  case ST_COMPOUND:
    dump_indent(level);
    printf("{COMPOUND\n");
    for (int i = 0; i < stmt->stmts->len; i++) {
      dump_stmt(stmt->stmts->data[i], level + 1);
    }
    dump_indent(level);
    printf("}\n");
    break;
  case ST_IF:
    dump_indent(level);
    printf("{IF\n");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->then_stmt, level + 1);
    dump_stmt(stmt->else_stmt, level + 1);
    dump_indent(level);
    printf("\n");
    break;
  case ST_SWITCH:
    dump_indent(level);
    printf("{SWITCH\n");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_indent(level);
    printf("\n");
    break;
  case ST_CASE:
    dump_indent(level);
    printf("{CASE\n");
    dump_expr(stmt->expr, level + 1);
    dump_indent(level);
    printf("\n");
    break;
  case ST_DEFAULT:
    dump_indent(level);
    printf("{DEFAULT}\n");
    break;
  case ST_LABEL:
    dump_indent(level);
    printf("{LABEL %s}\n", stmt->name);
    break;
  case ST_WHILE:
    dump_indent(level);
    printf("{WHILE\n");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_indent(level);
    printf("\n");
    break;
  case ST_DO_WHILE:
    dump_indent(level);
    printf("{DO_WHILE\n");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_indent(level);
    printf("\n");
    break;
  case ST_FOR:
    dump_indent(level);
    printf("{FOR\n");
    if (stmt->init != NULL) {
      dump_expr(stmt->init, level + 1);
    } else {
      dump_indent(level + 1);
      printf("<void>(NULL)\n");
    }
    if (stmt->cond != NULL) {
      dump_expr(stmt->cond, level + 1);
    } else {
      dump_indent(level + 1);
      printf("<void>(NULL)\n");
    }
    if (stmt->inc != NULL) {
      dump_expr(stmt->inc, level + 1);
    } else {
      dump_indent(level + 1);
      printf("<void>(NULL)\n");
    }
    dump_stmt(stmt->body, level + 1);
    dump_indent(level);
    printf("\n");
    break;
  case ST_GOTO:
    dump_indent(level);
    printf("{GOTO %s}\n", stmt->name);
    break;
  case ST_BREAK:
    dump_indent(level);
    printf("{BREAK}\n");
    break;
  case ST_CONTINUE:
    dump_indent(level);
    printf("{CONTINUE}\n");
    break;
  case ST_RETURN:
    dump_indent(level);
    printf("{RETURN\n");
    dump_expr(stmt->expr, level + 1);
    dump_indent(level);
    printf("\n");
    break;
  case ST_NULL:
    dump_indent(level);
    printf("{NULL}\n");
    break;
  default:
    error("未知のノードです: %d\n", stmt->ty);
  }
}

static void output_ast(Function *func) {
  int level = 0;
  dump_indent(level);
  printf("{FUNCTION %s\n", func->name);
  dump_stmt(func->body, level + 1);
  dump_indent(level);
  printf(")\n");
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

  const char *input = argv[optind];

  if (output_mode == OUTPUT_TOKEN) {
    output_token(input);
    return 0;
  }

  Vector *func_list = parse(input);

  if (output_mode == OUTPUT_AST) {
    for (int i = 0; i < func_list->len; i++) {
      output_ast(func_list->data[i]);
    }
    return 0;
  }

  printf(".intel_syntax noprefix\n");

  for (int i = 0; i < func_list->len; i++) {
    gen(func_list->data[i]);
  }
  return 0;
}
