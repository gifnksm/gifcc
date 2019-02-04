#include "gifcc.h"
#include <assert.h>
#include <getopt.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// エラーを報告するための関数
void error_raw(const char *dbg_file, int dbg_line, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);

  fprintf(stderr, " (DEBUG:%s:%d)\n", dbg_file, dbg_line);
  exit(1);
}

static void output_token(Reader *reader) {
  Tokenizer *tokenizer = new_tokenizer(reader);
  Token *token;
  do {
    token = token_pop(tokenizer);
    int line, column;
    reader_get_position(reader, token->range.start, &line, &column);
    printf("%s:%d:%d:\t%-8s", reader_get_filename(reader), line, column,
           token_kind_to_str(token->ty));
    switch (token->ty) {
    case TK_NUM:
      printf("%-8d", token->val);
      break;
    case TK_IDENT:
      printf("%-8s", token->name);
      break;
    case TK_STR:
      print_string_literal(token->str);
      break;
    default:
      printf("%8s", "");
      break;
    }
    printf("\t// %s\n", reader_get_source(reader, token->range));
  } while (token->ty != TK_EOF);
}

static void dump_indent(int level) { printf("%*s", 2 * level, ""); }
static void dump_type_inner(Type *ty) {
  switch (ty->ty) {
  case TY_VOID:
    printf("void");
    break;
  case TY_INT:
    printf("int");
    break;
  case TY_CHAR:
    printf("char");
    break;
  case TY_PTR:
    printf("PTR(");
    dump_type_inner(ty->ptrof);
    printf(")");
    break;
  case TY_ARRAY:
    printf("ARRAY[%d](", ty->array_len);
    dump_type_inner(ty->ptrof);
    printf(")");
    break;
  case TY_FUNC:
    printf("FUNC ");
    printf("(");
    for (int i = 0; i < ty->func_param->len; i++) {
      if (i > 0) {
        printf(", ");
      }
      Param *param = ty->func_param->data[i];
      dump_type_inner(param->type);
      printf(" %s", param->name);
    }
    printf(")->");
    dump_type_inner(ty->func_ret);
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
    return;
  case EX_STACK_VAR:
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(STACK_VAR %s@%d)\n", expr->name, expr->stack_var->offset);
    return;
  case EX_GLOBAL_VAR:
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(GLOBAL_VAR %s)\n", expr->name);
    return;
  case EX_STR:
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(STR ");
    print_string_literal(expr->name);
    printf(")\n");
    return;
  case EX_EQEQ:
    dump_binop_expr(expr, "[==]", level);
    return;
  case EX_NOTEQ:
    dump_binop_expr(expr, "[!=]", level);
    return;
  case EX_LTEQ:
    dump_binop_expr(expr, "[<=]", level);
    return;
  case EX_GTEQ:
    dump_binop_expr(expr, "[>=]", level);
    return;
  case EX_LSHIFT:
    dump_binop_expr(expr, "[<<]", level);
    return;
  case EX_RSHIFT:
    dump_binop_expr(expr, "[>>]", level);
    return;
  case EX_LOGAND:
    dump_binop_expr(expr, "[&&]", level);
    return;
  case EX_LOGOR:
    dump_binop_expr(expr, "[||]", level);
    return;
  case EX_COND:
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(COND\n");
    dump_expr(expr->cond, level + 1);
    dump_expr(expr->lhs, level + 1);
    dump_expr(expr->rhs, level + 1);
    dump_indent(level);
    printf(")\n");
    return;
  case EX_INC:
    dump_binop_expr_incdec(expr, "[++]", level);
    return;
  case EX_DEC:
    dump_binop_expr_incdec(expr, "[--]", level);
    return;
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
    return;
  case EX_CAST:
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(CAST\n");
    dump_expr(expr->expr, level + 1);
    dump_indent(level);
    printf(")\n");
    return;
  }
  error("未知のノードです: %d\n", expr->ty);
}

static void dump_stmt(Stmt *stmt, int level) {
  switch (stmt->ty) {
  case ST_EXPR:
    dump_indent(level);
    printf("{EXPR\n");
    dump_expr(stmt->expr, level + 1);
    dump_indent(level);
    printf("}\n");
    return;
  case ST_COMPOUND:
    dump_indent(level);
    printf("{COMPOUND\n");
    for (int i = 0; i < stmt->stmts->len; i++) {
      dump_stmt(stmt->stmts->data[i], level + 1);
    }
    dump_indent(level);
    printf("}\n");
    return;
  case ST_IF:
    dump_indent(level);
    printf("{IF\n");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->then_stmt, level + 1);
    dump_stmt(stmt->else_stmt, level + 1);
    dump_indent(level);
    printf("\n");
    return;
  case ST_SWITCH:
    dump_indent(level);
    printf("{SWITCH\n");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_indent(level);
    printf("\n");
    return;
  case ST_CASE:
    dump_indent(level);
    printf("{CASE\n");
    dump_expr(stmt->expr, level + 1);
    dump_indent(level);
    printf("\n");
    return;
  case ST_DEFAULT:
    dump_indent(level);
    printf("{DEFAULT}\n");
    return;
  case ST_LABEL:
    dump_indent(level);
    printf("{LABEL %s}\n", stmt->name);
    return;
  case ST_WHILE:
    dump_indent(level);
    printf("{WHILE\n");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_indent(level);
    printf("\n");
    return;
  case ST_DO_WHILE:
    dump_indent(level);
    printf("{DO_WHILE\n");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_indent(level);
    printf("\n");
    return;
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
    return;
  case ST_GOTO:
    dump_indent(level);
    printf("{GOTO %s}\n", stmt->name);
    return;
  case ST_BREAK:
    dump_indent(level);
    printf("{BREAK}\n");
    return;
  case ST_CONTINUE:
    dump_indent(level);
    printf("{CONTINUE}\n");
    return;
  case ST_RETURN:
    dump_indent(level);
    printf("{RETURN\n");
    dump_expr(stmt->expr, level + 1);
    dump_indent(level);
    printf("\n");
    return;
  case ST_NULL:
    dump_indent(level);
    printf("{NULL}\n");
    return;
  }
  error("未知のノードです: %d\n", stmt->ty);
}

static void output_ast(TranslationUnit *tunit) {
  int level = 0;
  for (int i = 0; i < tunit->func_list->len; i++) {
    Function *func = tunit->func_list->data[i];

    dump_indent(level);
    printf("FUNCTION ");
    dump_type(func->type);
    printf(" %s = {\n", func->name);
    dump_stmt(func->body, level + 1);
    dump_indent(level);
    printf("}\n");
  }
  for (int i = 0; i < tunit->gvar_list->len; i++) {
    GlobalVar *gvar = tunit->gvar_list->data[i];

    dump_indent(level);
    printf("GLOBAL ");
    dump_type(gvar->type);
    printf(" %s\n", gvar->name);
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

  if (optind > argc) {
    error("引数の個数が正しくありません");
  }

  char *filename;
  FILE *file;
  if (optind == argc) {
    filename = "STDIN";
    file = stdin;
  } else {
    filename = argv[optind];
    file = fopen(filename, "r");
    if (file == NULL) {
      error("ファイルが開けませんでした: %s", filename);
    }
  }

  Reader *reader = new_reader(file, filename);

  if (output_mode == OUTPUT_TOKEN) {
    output_token(reader);
    return 0;
  }

  TranslationUnit *tunit = parse(reader);

  if (output_mode == OUTPUT_AST) {
    output_ast(tunit);
    return 0;
  }

  gen(tunit);

  return 0;
}
