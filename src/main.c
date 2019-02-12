#include "gifcc.h"
#include <assert.h>
#include <getopt.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// エラーを報告するための関数
noreturn void error_raw(const char *dbg_file, int dbg_line, const char *fmt,
                        ...) {
  va_list ap;
  va_start(ap, fmt);
  error_raw_v(dbg_file, dbg_line, fmt, ap);
}

noreturn void error_raw_v(const char *dbg_file, int dbg_line, const char *fmt,
                          va_list ap) {
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, " (DEBUG:%s:%d)\n", dbg_file, dbg_line);
  exit(1);
}

static void dump_number(Number num) {
  switch (num.type) {
  case TY_CHAR:
    printf("%hhd", num.char_val);
    break;
  case TY_SCHAR:
    printf("%hhd", num.schar_val);
    break;
  case TY_SHORT:
    printf("%hd", num.short_val);
    break;
  case TY_INT:
    printf("%d", num.int_val);
    break;
  case TY_LONG:
    printf("%ld", num.long_val);
    break;
  case TY_LLONG:
    printf("%lld", num.llong_val);
    break;
  case TY_PTR:
    printf("%" PRIdPTR, num.ptr_val);
    break;
  case TY_VOID:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    assert(false);
  }
}

static void output_token(Reader *reader) {
  Tokenizer *tokenizer = new_tokenizer(reader);
  Token *token;
  do {
    token = token_pop(tokenizer);
    const char *filename;
    int line, column;
    reader_get_position(reader, token->range.start, &filename, &line, &column);
    printf("%s:%d:%d:\t%-8s", filename, line, column,
           token_kind_to_str(token->ty));
    switch (token->ty) {
    case TK_NUM:
      dump_number(token->num_val);
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

static void dump_range_start(Range range) {
  const char *filename;
  int line, column;
  reader_get_position(range.reader, range.start, &filename, &line, &column);
  printf("%s:%d:%d\t", filename, line, column);
}

static void dump_range_end(Range range) {
  const char *filename;
  int line, column;
  reader_get_position(range.reader, range.start + range.len, &filename, &line,
                      &column);
  printf("%s:%d:%d\t", filename, line, column);
}
static void dump_indent(int level) { printf("%*s", 2 * level, ""); }
static void dump_type_inner(Type *ty) {
  switch (ty->ty) {
  case TY_VOID:
    printf("void");
    return;
  case TY_INT:
    printf("int");
    return;
  case TY_SHORT:
    printf("short");
    return;
  case TY_LONG:
    printf("long");
    return;
  case TY_LLONG:
    printf("long long");
    return;
  case TY_CHAR:
    printf("char");
    return;
  case TY_SCHAR:
    printf("signed char");
    return;
  case TY_PTR:
    printf("PTR(");
    if (ty->ptrof->ty == TY_STRUCT && ty->ptrof->tag != NULL) {
      printf("struct %s", ty->ptrof->tag);
    } else if (ty->ptrof->ty == TY_UNION && ty->ptrof->tag != NULL) {
      printf("union %s", ty->ptrof->tag);
    } else {
      dump_type_inner(ty->ptrof);
    }
    printf(")");
    return;
  case TY_ARRAY:
    printf("ARRAY[%d](", ty->array_len);
    dump_type_inner(ty->ptrof);
    printf(")");
    return;
  case TY_FUNC:
    printf("FUNC ");
    printf("(");
    for (int i = 0; i < ty->func_param->len; i++) {
      if (i > 0) {
        printf(", ");
      }
      Param *param = ty->func_param->data[i];
      dump_type_inner(param->type);
      printf(" %s", param->name != NULL ? param->name->name : NULL);
    }
    printf(")->");
    dump_type_inner(ty->func_ret);
    return;
  case TY_STRUCT:
    printf("STRUCT %s", ty->tag);
    printf("(");
    for (int i = 0; i < ty->member_list->len; i++) {
      if (i > 0) {
        printf(", ");
      }
      Member *member = ty->member_list->data[i];
      dump_type_inner(member->type);
      printf(" %s", member->name);
    }
    printf(")");
    return;
  case TY_UNION:
    printf("UNION %s", ty->tag);
    printf("(");
    for (int i = 0; i < ty->member_list->len; i++) {
      if (i > 0) {
        printf(", ");
      }
      Member *member = ty->member_list->data[i];
      dump_type_inner(member->type);
      printf(" %s", member->name);
    }
    printf(")");
    return;
  }
  error("未知の型です: %d\n", ty->ty);
}
static void dump_type(Type *ty) {
  printf("<");
  dump_type_inner(ty);
  printf(">");
}
static void dump_expr(Expr *expr, int level);
static void dump_binop_expr(Expr *expr, char *label, int level) {
  dump_range_start(expr->range);
  dump_indent(level);
  dump_type(expr->val_type);
  printf("(%s\n", label);
  if (expr->lhs != NULL) {
    dump_expr(expr->lhs, level + 1);
  } else {
    dump_range_start(expr->range);
    dump_indent(level + 1);
    printf("<void>(NULL)\n");
  }
  if (expr->rhs != NULL) {
    dump_expr(expr->rhs, level + 1);
  } else {
    dump_range_end(expr->range);
    dump_indent(level + 1);
    printf("<void>(NULL)\n");
  }
  dump_range_end(expr->range);
  dump_indent(level);
  printf(")\n");
}

static void dump_binop_expr_incdec(Expr *expr, char *label, int level) {
  dump_range_start(expr->range);
  dump_indent(level);
  dump_type(expr->val_type);
  printf("(%s %d\n", label, expr->incdec_size);
  if (expr->lhs != NULL) {
    dump_expr(expr->lhs, level + 1);
  } else {
    dump_range_start(expr->range);
    dump_indent(level + 1);
    printf("<void>(NULL)\n");
  }
  if (expr->rhs != NULL) {
    dump_expr(expr->rhs, level + 1);
  } else {
    dump_range_end(expr->range);
    dump_indent(level + 1);
    printf("<void>(NULL)\n");
  }
  dump_range_end(expr->range);
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
    dump_range_start(expr->range);
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(NUM ");
    dump_number(expr->num_val);
    printf(")\n");
    return;
  case EX_STACK_VAR:
    dump_range_start(expr->range);
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(STACK_VAR %s@%d)\n", expr->name, expr->stack_var->offset);
    return;
  case EX_GLOBAL_VAR:
    dump_range_start(expr->range);
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(GLOBAL_VAR %s@%s)\n", expr->name,
           expr->global_var ? expr->global_var->name : NULL);
    return;
  case EX_STR:
    dump_range_start(expr->range);
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
    dump_range_start(expr->range);
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(COND\n");
    dump_expr(expr->cond, level + 1);
    dump_expr(expr->lhs, level + 1);
    dump_expr(expr->rhs, level + 1);
    dump_range_end(expr->range);
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
    dump_range_start(expr->range);
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(CALL\n");
    dump_expr(expr->callee, level + 1);
    if (expr->argument != NULL) {
      for (int i = 0; i < expr->argument->len; i++) {
        dump_expr(expr->argument->data[i], level + 1);
      }
    }
    dump_range_end(expr->range);
    dump_indent(level);
    printf(")\n");
    return;
  case EX_CAST:
    dump_range_start(expr->range);
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(CAST\n");
    dump_expr(expr->expr, level + 1);
    dump_range_end(expr->range);
    dump_indent(level);
    printf(")\n");
    return;
  }
  error("未知のノードです: %d\n", expr->ty);
}

static void dump_stmt(Stmt *stmt, int level) {
  switch (stmt->ty) {
  case ST_EXPR:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{EXPR\n");
    dump_expr(stmt->expr, level + 1);
    dump_range_end(stmt->range);
    dump_indent(level);
    printf("}\n");
    return;
  case ST_COMPOUND:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{COMPOUND\n");
    for (int i = 0; i < stmt->stmts->len; i++) {
      dump_stmt(stmt->stmts->data[i], level + 1);
    }
    dump_range_end(stmt->range);
    dump_indent(level);
    printf("}\n");
    return;
  case ST_IF:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{IF\n");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->then_stmt, level + 1);
    dump_stmt(stmt->else_stmt, level + 1);
    dump_range_end(stmt->range);
    dump_indent(level);
    printf("}\n");
    return;
  case ST_SWITCH:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{SWITCH\n");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_range_end(stmt->range);
    dump_indent(level);
    printf("}\n");
    return;
  case ST_CASE:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{CASE\n");
    dump_expr(stmt->expr, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_range_end(stmt->range);
    dump_indent(level);
    printf("}\n");
    return;
  case ST_DEFAULT:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{DEFAULT\n");
    dump_stmt(stmt->body, level + 1);
    dump_range_end(stmt->range);
    dump_indent(level);
    printf("}\n");
    return;
  case ST_LABEL:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{LABEL %s\n", stmt->name);
    dump_stmt(stmt->body, level + 1);
    dump_range_end(stmt->range);
    dump_indent(level);
    printf("}\n");
    return;
  case ST_WHILE:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{WHILE\n");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_range_end(stmt->range);
    dump_indent(level);
    printf("\n");
    return;
  case ST_DO_WHILE:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{DO_WHILE\n");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_range_end(stmt->range);
    dump_indent(level);
    printf("\n");
    return;
  case ST_FOR:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{FOR\n");
    if (stmt->init != NULL) {
      dump_expr(stmt->init, level + 1);
    } else {
      dump_range_start(stmt->range);
      dump_indent(level + 1);
      printf("<void>(NULL)\n");
    }
    if (stmt->cond != NULL) {
      dump_expr(stmt->cond, level + 1);
    } else {
      dump_range_start(stmt->range);
      dump_indent(level + 1);
      printf("<void>(NULL)\n");
    }
    if (stmt->inc != NULL) {
      dump_expr(stmt->inc, level + 1);
    } else {
      dump_range_start(stmt->range);
      dump_indent(level + 1);
      printf("<void>(NULL)\n");
    }
    dump_stmt(stmt->body, level + 1);
    dump_range_end(stmt->range);
    dump_indent(level);
    printf("\n");
    return;
  case ST_GOTO:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{GOTO %s}\n", stmt->name);
    return;
  case ST_BREAK:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{BREAK}\n");
    return;
  case ST_CONTINUE:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{CONTINUE}\n");
    return;
  case ST_RETURN:
    dump_range_start(stmt->range);
    dump_indent(level);
    if (stmt->expr != NULL) {
      printf("{RETURN\n");
      dump_range_start(stmt->range);
      dump_indent(level + 1);
      printf("<void>(NULL)\n");
      dump_range_end(stmt->range);
      dump_indent(level);
      printf("}\n");
    } else {
      printf("{RETURN}\n");
    }
    return;
  case ST_NULL:
    dump_range_start(stmt->range);
    dump_indent(level);
    printf("{NULL}\n");
    return;
  }
  error("未知のノードです: %d\n", stmt->ty);
}

static void dump_init(Initializer *init, Range range, int level) {
  if (init == NULL) {
    dump_range_start(range);
    dump_indent(level);
    printf("NULL\n");
    return;
  }

  if (init->expr != NULL) {
    dump_expr(init->expr, level);
    return;
  }
  if (init->members != NULL) {
    dump_range_start(range);
    dump_indent(level);
    dump_type(init->type);
    printf("{\n");
    for (int i = 0; i < init->members->keys->len; i++) {
      char *name = init->members->keys->data[i];
      Initializer *val = init->members->vals->data[i];
      dump_range_start(range);
      dump_indent(level + 1);
      printf(".%s = \n", name);
      dump_init(val, range, level + 1);
    }
    dump_range_end(range);
    dump_indent(level);
    printf("}\n");
    return;
  }
  assert(false);
}

static void output_ast(TranslationUnit *tunit) {
  int level = 0;
  for (int i = 0; i < tunit->func_list->len; i++) {
    Function *func = tunit->func_list->data[i];

    dump_range_start(func->range);
    dump_indent(level);
    printf("FUNCTION ");
    dump_type(func->type);
    printf(" %s = {\n", func->name);
    dump_stmt(func->body, level + 1);
    dump_range_end(func->range);
    dump_indent(level);
    printf("}\n");
  }
  for (int i = 0; i < tunit->gvar_list->len; i++) {
    GlobalVar *gvar = tunit->gvar_list->data[i];

    dump_range_start(gvar->range);
    dump_indent(level);
    printf("GLOBAL ");
    dump_type(gvar->type);
    if (gvar->init != NULL) {
      printf(" %s = \n", gvar->name);
      dump_init(gvar->init, gvar->range, level + 1);
      dump_range_end(gvar->range);
      dump_indent(level);
      printf("\n");
    } else {
      printf(" %s\n", gvar->name);
    }
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
