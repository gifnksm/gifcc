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

static void output_token(Reader *reader) {
  Tokenizer *tokenizer = new_tokenizer(reader);
  Token *token;
  do {
    token = token_pop(tokenizer);
    const char *filename;
    int line, column;
    range_get_start(token->range, &filename, &line, &column);
    printf("%s:%d:%d:\t%-8s", filename, line, column,
           token_kind_to_str(token->ty));
    switch (token->ty) {
    case TK_NUM:
      printf("%s", token->num);
      break;
    case TK_CHARCONST:
      printf("%s", format_number(token->char_val));
      break;
    case TK_IDENT:
      printf("%s", token->ident);
      break;
    case TK_STR:
      printf("%s", format_string_literal(token->str));
      break;
    default:
      break;
    }
    printf("\n");
  } while (token->ty != TK_EOF);
}

static void dump_range_start(const Range *range) {
  const char *filename;
  int line, column;
  range_get_start(range, &filename, &line, &column);
  printf("%s:%d:%d\t", filename, line, column);
}
static void dump_range_end(const Range *range) {
  const char *filename;
  int line, column;
  range_get_end(range, &filename, &line, &column);
  printf("%s:%d:%d\t", filename, line, column);
}

static void dump_indent(int level) { printf("%*s", 2 * level, ""); }
static void dump_type(Type *ty) { printf("<%s>", format_type(ty, true)); }

static __attribute__((format(printf, 3, 4))) void
dump_expr_oneline(const Expr *expr, int level, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  dump_range_start(expr->range);
  dump_indent(level);
  dump_type(expr->val_type);
  printf("(");
  vprintf(fmt, ap);
  printf(")\n");
  va_end(ap);
}
static __attribute__((format(printf, 3, 4))) void
dump_expr_start(const Expr *expr, int level, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  dump_range_start(expr->range);
  dump_indent(level);
  dump_type(expr->val_type);
  printf("(");
  vprintf(fmt, ap);
  printf("\n");
  va_end(ap);
}
static void dump_expr_end(const Expr *expr, int level) {
  dump_range_end(expr->range);
  dump_indent(level);
  printf(")\n");
}
static void dump_expr(Expr *expr, int level);
static void dump_init(Initializer *init, const Range *range, int level);
static void dump_unop_expr(Expr *expr, char *label, int level) {
  dump_expr_start(expr, level, "%s", label);
  dump_expr(expr->unop.operand, level + 1);
  dump_expr_end(expr, level);
}
static void dump_binop_expr(Expr *expr, char *label, int level) {
  dump_expr_start(expr, level, "%s", label);
  dump_expr(expr->binop.lhs, level + 1);
  dump_expr(expr->binop.rhs, level + 1);
  dump_expr_end(expr, level);
}
static void dump_expr(Expr *expr, int level) {
  switch (expr->ty) {
  // primary expression
  case EX_NUM:
    dump_expr_oneline(expr, level, "NUM %s", format_number(expr->num));
    return;
  case EX_STACK_VAR:
    dump_expr_oneline(expr, level, "STACK_VAR %s", expr->stack_var->name);
    return;
  case EX_GLOBAL_VAR:
    dump_expr_oneline(expr, level, "GLOBAL_VAR %s", expr->global_var.name);
    return;
  case EX_STR:
    dump_expr_oneline(expr, level, "STR %s", format_string_literal(expr->str));
    return;
  case EX_COMPOUND:
    dump_expr_start(expr, level, "COMPOUND");
    dump_init(expr->compound, expr->range, level + 1);
    dump_expr_end(expr, level);
    return;

  // prefix unary operator
  case EX_PRE_INC:
    dump_unop_expr(expr, "[++(PRE)]", level);
    return;
  case EX_PRE_DEC:
    dump_unop_expr(expr, "[--(PRE)]", level);
    return;
  case EX_ADDRESS:
    dump_unop_expr(expr, "[&]", level);
    return;
  case EX_INDIRECT:
    dump_unop_expr(expr, "[*]", level);
    return;
  case EX_PLUS:
    dump_unop_expr(expr, "[+]", level);
    return;
  case EX_MINUS:
    dump_unop_expr(expr, "[-]", level);
    return;
  case EX_NOT:
    dump_unop_expr(expr, "[~]", level);
    return;
  case EX_LOG_NOT:
    dump_unop_expr(expr, "[!]", level);
    return;
  case EX_CAST:
    dump_unop_expr(expr, "CAST", level);
    return;

  // postfix unary operator
  case EX_CALL:
    dump_expr_start(expr, level, "CALL");
    dump_expr(expr->call.callee, level + 1);
    if (expr->call.argument != NULL) {
      for (int i = 0; i < vec_len(expr->call.argument); i++) {
        dump_expr(vec_get(expr->call.argument, i), level + 1);
      }
    }
    dump_expr_end(expr, level);
    return;
  case EX_POST_INC:
    dump_unop_expr(expr, "[++(POST)]", level);
    return;
  case EX_POST_DEC:
    dump_unop_expr(expr, "[--(POST)]", level);
    return;
  case EX_DOT:
    dump_expr_start(expr, level, "DOT %s", expr->dot.member->name);
    dump_expr(expr->dot.operand, level + 1);
    dump_expr_end(expr, level);
    return;

  // binary operator
  case EX_ADD:
    dump_binop_expr(expr, "[+]", level);
    return;
  case EX_SUB:
    dump_binop_expr(expr, "[-]", level);
    return;
  case EX_MUL:
    dump_binop_expr(expr, "[*]", level);
    return;
  case EX_DIV:
    dump_binop_expr(expr, "[/]", level);
    return;
  case EX_MOD:
    dump_binop_expr(expr, "[%]", level);
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
  case EX_LT:
    dump_binop_expr(expr, "[<]", level);
    return;
  case EX_GT:
    dump_binop_expr(expr, "[>]", level);
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
  case EX_AND:
    dump_binop_expr(expr, "[&]", level);
    return;
  case EX_XOR:
    dump_binop_expr(expr, "[^]", level);
    return;
  case EX_OR:
    dump_binop_expr(expr, "[|]", level);
    return;
  case EX_LOG_AND:
    dump_binop_expr(expr, "[&&]", level);
    return;
  case EX_LOG_OR:
    dump_binop_expr(expr, "[||]", level);
    return;
  case EX_ASSIGN:
    dump_binop_expr(expr, "[=]", level);
    return;
  case EX_MUL_ASSIGN:
    dump_binop_expr(expr, "[*=]", level);
    return;
  case EX_DIV_ASSIGN:
    dump_binop_expr(expr, "[/=]", level);
    return;
  case EX_MOD_ASSIGN:
    dump_binop_expr(expr, "[%=]", level);
    return;
  case EX_ADD_ASSIGN:
    dump_binop_expr(expr, "[+=]", level);
    return;
  case EX_SUB_ASSIGN:
    dump_binop_expr(expr, "[-=]", level);
    return;
  case EX_LSHIFT_ASSIGN:
    dump_binop_expr(expr, "[<<=]", level);
    return;
  case EX_RSHIFT_ASSIGN:
    dump_binop_expr(expr, "[>>=]", level);
    return;
  case EX_AND_ASSIGN:
    dump_binop_expr(expr, "[&=]", level);
    return;
  case EX_XOR_ASSIGN:
    dump_binop_expr(expr, "[^=]", level);
    return;
  case EX_OR_ASSIGN:
    dump_binop_expr(expr, "[|=]", level);
    return;
  case EX_COMMA:
    dump_expr_start(expr, level, "[,]");
    for (int i = 0; i < vec_len(expr->comma.exprs); i++) {
      Expr *op = vec_get(expr->comma.exprs, i);
      dump_expr(op, level + 1);
    }
    dump_expr_end(expr, level);
    return;

  // ternary unary operator
  case EX_COND:
    dump_expr_start(expr, level, "COND");
    dump_expr(expr->cond.cond, level + 1);
    dump_expr(expr->cond.then_expr, level + 1);
    dump_expr(expr->cond.else_expr, level + 1);
    dump_expr_end(expr, level);
    return;

  // compiler builtins
  case EX_BUILTIN_FUNC:
    dump_expr_oneline(expr, level, "BUILTIN_FUNC %s", expr->builtin_func.name);
    return;

  case EX_BUILTIN_VA_START:
    dump_expr_start(expr, level, "BUILTIN_VA_START");
    dump_expr(expr->builtin_va_start.ap, level + 1);
    dump_expr(expr->builtin_va_start.last, level + 1);
    dump_expr_end(expr, level);
    return;
  case EX_BUILTIN_VA_ARG:
    dump_expr_start(expr, level, "BUILTIN_VA_ARG");
    dump_expr(expr->builtin_va_arg.ap, level + 1);
    dump_expr_end(expr, level);
    return;
  case EX_BUILTIN_VA_END:
    dump_expr_start(expr, level, "BUILTIN_VA_END");
    dump_expr(expr->builtin_va_end.ap, level + 1);
    dump_expr_end(expr, level);
    return;
  case EX_BUILTIN_VA_COPY:
    dump_expr_start(expr, level, "BUILTIN_VA_COPY");
    dump_expr(expr->builtin_va_copy.dest, level + 1);
    dump_expr(expr->builtin_va_copy.src, level + 1);
    dump_expr_end(expr, level);
    return;
  }
  error("未知のノードです: %d", expr->ty);
}

static __attribute__((format(printf, 3, 4))) void
dump_stmt_oneline(const Stmt *stmt, int level, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  dump_range_start(stmt->range);
  dump_indent(level);
  printf("{");
  vprintf(fmt, ap);
  printf("}\n");
  va_end(ap);
}
static __attribute__((format(printf, 3, 4))) void
dump_stmt_start(const Stmt *stmt, int level, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  dump_range_start(stmt->range);
  dump_indent(level);
  printf("{");
  vprintf(fmt, ap);
  printf("\n");
  va_end(ap);
}
static void dump_stmt_end(const Stmt *stmt, int level) {
  dump_range_end(stmt->range);
  dump_indent(level);
  printf("}\n");
}

static void dump_stmt(Stmt *stmt, int level) {
  switch (stmt->ty) {
  case ST_EXPR:
    dump_stmt_start(stmt, level, "EXPR");
    dump_expr(stmt->expr, level + 1);
    dump_stmt_end(stmt, level);
    return;
  case ST_COMPOUND:
    dump_stmt_start(stmt, level, "COMPOUND");
    for (int i = 0; i < vec_len(stmt->stmts); i++) {
      dump_stmt(vec_get(stmt->stmts, i), level + 1);
    }
    dump_stmt_end(stmt, level);
    return;
  case ST_IF:
    dump_stmt_start(stmt, level, "IF");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->then_stmt, level + 1);
    dump_stmt(stmt->else_stmt, level + 1);
    dump_stmt_end(stmt, level);
    return;
  case ST_SWITCH:
    dump_stmt_start(stmt, level, "SWITCH");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_stmt_end(stmt, level);
    return;
  case ST_CASE:
    dump_stmt_start(stmt, level, "CASE");
    dump_expr(stmt->expr, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_stmt_end(stmt, level);
    return;
  case ST_DEFAULT:
    dump_stmt_start(stmt, level, "DEFAULT");
    dump_stmt(stmt->body, level + 1);
    dump_stmt_end(stmt, level);
    return;
  case ST_LABEL:
    dump_stmt_start(stmt, level, "LABEL %s", stmt->name);
    dump_stmt(stmt->body, level + 1);
    dump_stmt_end(stmt, level);
    return;
  case ST_WHILE:
    dump_stmt_start(stmt, level, "WHILE");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_stmt_end(stmt, level);
    return;
  case ST_DO_WHILE:
    dump_stmt_start(stmt, level, "DO_WHILE");
    dump_expr(stmt->cond, level + 1);
    dump_stmt(stmt->body, level + 1);
    dump_stmt_end(stmt, level);
    return;
  case ST_FOR:
    dump_stmt_start(stmt, level, "FOR");
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
    dump_stmt_end(stmt, level);
    return;
  case ST_GOTO:
    dump_stmt_oneline(stmt, level, "GOTO %s", stmt->name);
    return;
  case ST_BREAK:
    dump_stmt_oneline(stmt, level, "BREAK");
    return;
  case ST_CONTINUE:
    dump_stmt_oneline(stmt, level, "CONTINUE");
    return;
  case ST_RETURN:
    if (stmt->expr != NULL) {
      dump_stmt_start(stmt, level, "RETURN");
      dump_expr(stmt->expr, level + 1);
      dump_stmt_end(stmt, level);
    } else {
      dump_stmt_oneline(stmt, level, "RETURN");
    }
    return;
  case ST_NULL:
    dump_stmt_oneline(stmt, level, "NULL");
    return;
  }
  error("未知のノードです: %d\n", stmt->ty);
}

static void dump_init(Initializer *init, const Range *range, int level) {
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
    for (int i = 0; i < map_size(init->members); i++) {
      const char *name;
      Initializer *val = map_get_by_index(init->members, i, &name);
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
  if (init->elements != NULL) {
    dump_range_start(range);
    dump_indent(level);
    dump_type(init->type);
    printf("{\n");
    for (int i = 0; i < vec_len(init->elements); i++) {
      Initializer *val = vec_get(init->elements, i);
      dump_range_start(range);
      dump_indent(level + 1);
      printf(".[%d] = \n", i);
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
  for (int i = 0; i < vec_len(tunit->func_list); i++) {
    Function *func = vec_get(tunit->func_list, i);

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
  for (int i = 0; i < vec_len(tunit->gvar_list); i++) {
    GlobalVar *gvar = vec_get(tunit->gvar_list, i);

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
  OUTPUT_SEMA,
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
      } else if (strcmp(optarg, "sema") == 0) {
        output_mode = OUTPUT_SEMA;
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

  Reader *reader = new_reader();
  reader_add_file(reader, file, filename);

  if (output_mode == OUTPUT_TOKEN) {
    output_token(reader);
    return 0;
  }

  TranslationUnit *tunit = parse(reader);
  if (output_mode == OUTPUT_AST) {
    output_ast(tunit);
    return 0;
  }

  sema(tunit);
  if (output_mode == OUTPUT_SEMA) {
    output_ast(tunit);
    return 0;
  }

  gen(tunit);

  return 0;
}
