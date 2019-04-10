#include "gifcc.h"
#include <assert.h>
#include <getopt.h>
#include <inttypes.h>
#include <libgen.h>
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

static const char *trim_filename(const char *filename) {
  if (strncmp(filename, GIFCC_INCLUDE, sizeof(GIFCC_INCLUDE) - 1) == 0) {
    return format("<gifcc>%s", &filename[sizeof(GIFCC_INCLUDE) - 1]);
  }
  return filename;
}

static void dump_token(FILE *fp, const Token *token) {
  const char *filename;
  int line, column;
  range_get_start(token->range, &filename, &line, &column);
  fprintf(fp, "%32s:%4d:%3d:\t%-8s ", trim_filename(filename), line, column,
          token_kind_to_str(token->ty));
  switch (token->ty) {
  case TK_PP_NUM:
    fprintf(fp, "%s", token->pp_num);
    break;
  case TK_PP_IDENT:
    fprintf(fp, "%s", token->pp_ident);
    break;
  case TK_NUM:
    fprintf(fp, "%s", format_number(token->num));
    break;
  case TK_IDENT:
    fprintf(fp, "%s", token->ident);
    break;
  case TK_CHARCONST:
    fprintf(fp, "%s", format_number(token->char_val));
    break;
  case TK_STR:
    fprintf(fp, "%s", format_string_literal(token->str));
    break;
  default:
    break;
  }
  fprintf(fp, "\n");
}

typedef struct {
  FILE *fp;
  TokenIterator *ts;
} TokenFilterArg;

static bool token_filter(void *arg, Vector *output) {
  TokenFilterArg *tfa = arg;
  Token *token = ts_pop(tfa->ts);
  if (token == NULL) {
    return false;
  }
  dump_token(tfa->fp, token);
  vec_push(output, token);
  return true;
}

static void dump_range_start(FILE *fp, const Range *range) {
  const char *filename;
  int line, column;
  range_get_start(range, &filename, &line, &column);
  fprintf(fp, "%32s:%4d:%3d\t", trim_filename(filename), line, column);
}
static void dump_range_end(FILE *fp, const Range *range) {
  const char *filename;
  int line, column;
  range_get_end(range, &filename, &line, &column);
  fprintf(fp, "%32s:%4d:%3d\t", trim_filename(filename), line, column);
}

static void dump_indent(FILE *fp, int level) {
  fprintf(fp, "%*s", 2 * level, "");
}
static void dump_type(FILE *fp, Type *ty) {
  fprintf(fp, "<%s>", format_type(ty, true));
}

static __attribute__((format(printf, 4, 5))) void
dump_expr_oneline(FILE *fp, const Expr *expr, int level, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  dump_range_start(fp, expr->range);
  dump_indent(fp, level);
  dump_type(fp, expr->val_type);
  fprintf(fp, "(");
  vfprintf(fp, fmt, ap);
  fprintf(fp, ")\n");
  va_end(ap);
}
static __attribute__((format(printf, 4, 5))) void
dump_expr_start(FILE *fp, const Expr *expr, int level, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  dump_range_start(fp, expr->range);
  dump_indent(fp, level);
  dump_type(fp, expr->val_type);
  fprintf(fp, "(");
  vfprintf(fp, fmt, ap);
  fprintf(fp, "\n");
  va_end(ap);
}
static void dump_expr_end(FILE *fp, const Expr *expr, int level) {
  dump_range_end(fp, expr->range);
  dump_indent(fp, level);
  fprintf(fp, ")\n");
}
static void dump_expr(FILE *fp, Expr *expr, int level);
static void dump_stmt(FILE *fp, Stmt *stmt, int level);
static void dump_init(FILE *fp, Initializer *init, const Range *range,
                      int level);
static void dump_unop_expr(FILE *fp, Expr *expr, char *label, int level) {
  dump_expr_start(fp, expr, level, "%s", label);
  dump_expr(fp, expr->unop.operand, level + 1);
  dump_expr_end(fp, expr, level);
}
static void dump_binop_expr(FILE *fp, Expr *expr, char *label, int level) {
  dump_expr_start(fp, expr, level, "%s", label);
  dump_expr(fp, expr->binop.lhs, level + 1);
  dump_expr(fp, expr->binop.rhs, level + 1);
  dump_expr_end(fp, expr, level);
}
static void dump_expr(FILE *fp, Expr *expr, int level) {
  switch (expr->ty) {
  // primary expression
  case EX_NUM:
    dump_expr_oneline(fp, expr, level, "NUM %s", format_number(expr->num));
    return;
  case EX_STACK_VAR:
    dump_expr_oneline(fp, expr, level, "STACK_VAR %s+%d",
                      expr->stack_var.def->name, expr->stack_var.offset);
    return;
  case EX_GLOBAL_VAR:
    dump_expr_oneline(fp, expr, level, "GLOBAL_VAR %s+%d",
                      expr->global_var.name, expr->global_var.offset);
    return;
  case EX_STR:
    dump_expr_oneline(fp, expr, level, "STR %s:%s",
                      format_string_literal(expr->str->val), expr->str->name);
    return;
  case EX_COMPOUND:
    dump_expr_start(fp, expr, level, "COMPOUND");
    dump_init(fp, expr->compound.init, expr->range, level + 1);
    dump_expr_end(fp, expr, level);
    return;
  case EX_STMT:
    dump_expr_start(fp, expr, level, "STMT");
    dump_stmt(fp, expr->stmt, level + 1);
    dump_expr_end(fp, expr, level);
    return;

  // prefix unary operator
  case EX_PRE_INC:
    dump_unop_expr(fp, expr, "PRE_INC", level);
    return;
  case EX_PRE_DEC:
    dump_unop_expr(fp, expr, "PRE_DEC", level);
    return;
  case EX_ADDRESS:
    dump_unop_expr(fp, expr, "ADDRESS", level);
    return;
  case EX_INDIRECT:
    dump_unop_expr(fp, expr, "INDIRECT", level);
    return;
  case EX_PLUS:
    dump_unop_expr(fp, expr, "PLUS", level);
    return;
  case EX_MINUS:
    dump_unop_expr(fp, expr, "MINUS", level);
    return;
  case EX_NOT:
    dump_unop_expr(fp, expr, "[~]", level);
    return;
  case EX_LOG_NOT:
    dump_unop_expr(fp, expr, "[!]", level);
    return;
  case EX_CAST:
    dump_unop_expr(fp, expr, "CAST", level);
    return;

  // postfix unary operator
  case EX_CALL:
    dump_expr_start(fp, expr, level, "CALL");
    dump_expr(fp, expr->call.callee, level + 1);
    if (expr->call.argument != NULL) {
      for (int i = 0; i < vec_len(expr->call.argument); i++) {
        dump_expr(fp, vec_get(expr->call.argument, i), level + 1);
      }
    }
    dump_expr_end(fp, expr, level);
    return;
  case EX_POST_INC:
    dump_unop_expr(fp, expr, "POST_INC", level);
    return;
  case EX_POST_DEC:
    dump_unop_expr(fp, expr, "POST_DEC", level);
    return;
  case EX_DOT: {
    String *s = new_string();
    for (int i = 0; i < vec_len(expr->dot.members); i++) {
      Member *m = vec_get(expr->dot.members, i);
      if (i > 0) {
        str_push(s, '.');
      }
      str_append(s, m->name);
    }
    str_push(s, '\0');
    dump_expr_start(fp, expr, level, "DOT %s", str_get_raw(s));
    dump_expr(fp, expr->dot.operand, level + 1);
    dump_expr_end(fp, expr, level);
    return;
  }
  case EX_ARROW: {
    String *s = new_string();
    for (int i = 0; i < vec_len(expr->arrow.members); i++) {
      Member *m = vec_get(expr->arrow.members, i);
      if (i > 0) {
        str_push(s, '.');
      }
      str_append(s, m->name);
    }
    str_push(s, '\0');
    dump_expr_start(fp, expr, level, "ARROW %s", str_get_raw(s));
    dump_expr(fp, expr->arrow.operand, level + 1);
    dump_expr_end(fp, expr, level);
    return;
  }

  // binary operator
  case EX_ADD:
    dump_binop_expr(fp, expr, "[+]", level);
    return;
  case EX_SUB:
    dump_binop_expr(fp, expr, "[-]", level);
    return;
  case EX_MUL:
    dump_binop_expr(fp, expr, "[*]", level);
    return;
  case EX_DIV:
    dump_binop_expr(fp, expr, "[/]", level);
    return;
  case EX_MOD:
    dump_binop_expr(fp, expr, "[%]", level);
    return;
  case EX_EQEQ:
    dump_binop_expr(fp, expr, "[==]", level);
    return;
  case EX_NOTEQ:
    dump_binop_expr(fp, expr, "[!=]", level);
    return;
  case EX_LTEQ:
    dump_binop_expr(fp, expr, "[<=]", level);
    return;
  case EX_LT:
    dump_binop_expr(fp, expr, "[<]", level);
    return;
  case EX_GT:
    dump_binop_expr(fp, expr, "[>]", level);
    return;
  case EX_GTEQ:
    dump_binop_expr(fp, expr, "[>=]", level);
    return;
  case EX_LSHIFT:
    dump_binop_expr(fp, expr, "[<<]", level);
    return;
  case EX_RSHIFT:
    dump_binop_expr(fp, expr, "[>>]", level);
    return;
  case EX_AND:
    dump_binop_expr(fp, expr, "[&]", level);
    return;
  case EX_XOR:
    dump_binop_expr(fp, expr, "[^]", level);
    return;
  case EX_OR:
    dump_binop_expr(fp, expr, "[|]", level);
    return;
  case EX_LOG_AND:
    dump_binop_expr(fp, expr, "[&&]", level);
    return;
  case EX_LOG_OR:
    dump_binop_expr(fp, expr, "[||]", level);
    return;
  case EX_ASSIGN:
    dump_binop_expr(fp, expr, "[=]", level);
    return;
  case EX_MUL_ASSIGN:
    dump_binop_expr(fp, expr, "[*=]", level);
    return;
  case EX_DIV_ASSIGN:
    dump_binop_expr(fp, expr, "[/=]", level);
    return;
  case EX_MOD_ASSIGN:
    dump_binop_expr(fp, expr, "[%=]", level);
    return;
  case EX_ADD_ASSIGN:
    dump_binop_expr(fp, expr, "[+=]", level);
    return;
  case EX_SUB_ASSIGN:
    dump_binop_expr(fp, expr, "[-=]", level);
    return;
  case EX_LSHIFT_ASSIGN:
    dump_binop_expr(fp, expr, "[<<=]", level);
    return;
  case EX_RSHIFT_ASSIGN:
    dump_binop_expr(fp, expr, "[>>=]", level);
    return;
  case EX_AND_ASSIGN:
    dump_binop_expr(fp, expr, "[&=]", level);
    return;
  case EX_XOR_ASSIGN:
    dump_binop_expr(fp, expr, "[^=]", level);
    return;
  case EX_OR_ASSIGN:
    dump_binop_expr(fp, expr, "[|=]", level);
    return;
  case EX_COMMA:
    dump_expr_start(fp, expr, level, "[,]");
    for (int i = 0; i < vec_len(expr->comma.exprs); i++) {
      Expr *op = vec_get(expr->comma.exprs, i);
      dump_expr(fp, op, level + 1);
    }
    dump_expr_end(fp, expr, level);
    return;

  // ternary unary operator
  case EX_COND:
    dump_expr_start(fp, expr, level, "COND");
    dump_expr(fp, expr->cond.cond, level + 1);
    dump_expr(fp, expr->cond.then_expr, level + 1);
    dump_expr(fp, expr->cond.else_expr, level + 1);
    dump_expr_end(fp, expr, level);
    return;

  // compiler builtins
  case EX_BUILTIN_FUNC:
    dump_expr_oneline(fp, expr, level, "BUILTIN_FUNC %s",
                      expr->builtin_func.name);
    return;

  case EX_BUILTIN_VA_START:
    dump_expr_start(fp, expr, level, "BUILTIN_VA_START");
    dump_expr(fp, expr->builtin_va_start.ap, level + 1);
    dump_expr(fp, expr->builtin_va_start.last, level + 1);
    dump_expr_end(fp, expr, level);
    return;
  case EX_BUILTIN_VA_ARG:
    dump_expr_start(fp, expr, level, "BUILTIN_VA_ARG");
    dump_expr(fp, expr->builtin_va_arg.ap, level + 1);
    dump_expr_end(fp, expr, level);
    return;
  case EX_BUILTIN_VA_END:
    dump_expr_start(fp, expr, level, "BUILTIN_VA_END");
    dump_expr(fp, expr->builtin_va_end.ap, level + 1);
    dump_expr_end(fp, expr, level);
    return;
  case EX_BUILTIN_VA_COPY:
    dump_expr_start(fp, expr, level, "BUILTIN_VA_COPY");
    dump_expr(fp, expr->builtin_va_copy.dest, level + 1);
    dump_expr(fp, expr->builtin_va_copy.src, level + 1);
    dump_expr_end(fp, expr, level);
    return;
  }
  error("未知のノードです: %d", expr->ty);
}

static __attribute__((format(printf, 4, 5))) void
dump_stmt_oneline(FILE *fp, const Stmt *stmt, int level, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  dump_range_start(fp, stmt->range);
  dump_indent(fp, level);
  dump_type(fp, stmt->val_type);
  fprintf(fp, "{");
  vfprintf(fp, fmt, ap);
  fprintf(fp, "}\n");
  va_end(ap);
}
static __attribute__((format(printf, 4, 5))) void
dump_stmt_start(FILE *fp, const Stmt *stmt, int level, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  dump_range_start(fp, stmt->range);
  dump_indent(fp, level);
  dump_type(fp, stmt->val_type);
  fprintf(fp, "{");
  vfprintf(fp, fmt, ap);
  fprintf(fp, "\n");
  va_end(ap);
}
static void dump_stmt_end(FILE *fp, const Stmt *stmt, int level) {
  dump_range_end(fp, stmt->range);
  dump_indent(fp, level);
  fprintf(fp, "}\n");
}

static void dump_stmt(FILE *fp, Stmt *stmt, int level) {
  switch (stmt->ty) {
  case ST_EXPR:
    dump_stmt_start(fp, stmt, level, "EXPR");
    dump_expr(fp, stmt->expr, level + 1);
    dump_stmt_end(fp, stmt, level);
    return;
  case ST_COMPOUND:
    dump_stmt_start(fp, stmt, level, "COMPOUND");
    for (int i = 0; i < vec_len(stmt->stmts); i++) {
      dump_stmt(fp, vec_get(stmt->stmts, i), level + 1);
    }
    dump_stmt_end(fp, stmt, level);
    return;
  case ST_DECL:
    dump_stmt_start(fp, stmt, level, "DECL");
    for (int i = 0; i < vec_len(stmt->decl); i++) {
      StackVarDecl *decl = vec_get(stmt->decl, i);
      StackVar *svar = decl->stack_var;
      Initializer *init = decl->init;
      dump_range_start(fp, svar->range);
      dump_indent(fp, level + 1);
      dump_type(fp, svar->type);
      if (decl->init != NULL) {
        fprintf(fp, " %s = \n", svar->name);
        dump_init(fp, init, svar->range, level + 2);
      } else {
        fprintf(fp, " %s = \n", svar->name);
      }
    }
    dump_stmt_end(fp, stmt, level);
    return;
  case ST_IF:
    dump_stmt_start(fp, stmt, level, "IF");
    dump_expr(fp, stmt->cond, level + 1);
    dump_stmt(fp, stmt->then_stmt, level + 1);
    dump_stmt(fp, stmt->else_stmt, level + 1);
    dump_stmt_end(fp, stmt, level);
    return;
  case ST_SWITCH:
    dump_stmt_start(fp, stmt, level, "SWITCH");
    dump_expr(fp, stmt->cond, level + 1);
    dump_stmt(fp, stmt->body, level + 1);
    dump_stmt_end(fp, stmt, level);
    return;
  case ST_CASE:
    dump_stmt_start(fp, stmt, level, "CASE %s", format_number(stmt->case_val));
    dump_stmt(fp, stmt->body, level + 1);
    dump_stmt_end(fp, stmt, level);
    return;
  case ST_DEFAULT:
    dump_stmt_start(fp, stmt, level, "DEFAULT");
    dump_stmt(fp, stmt->body, level + 1);
    dump_stmt_end(fp, stmt, level);
    return;
  case ST_LABEL:
    dump_stmt_start(fp, stmt, level, "LABEL %s", stmt->name);
    dump_stmt(fp, stmt->body, level + 1);
    dump_stmt_end(fp, stmt, level);
    return;
  case ST_WHILE:
    dump_stmt_start(fp, stmt, level, "WHILE");
    dump_expr(fp, stmt->cond, level + 1);
    dump_stmt(fp, stmt->body, level + 1);
    dump_stmt_end(fp, stmt, level);
    return;
  case ST_DO_WHILE:
    dump_stmt_start(fp, stmt, level, "DO_WHILE");
    dump_expr(fp, stmt->cond, level + 1);
    dump_stmt(fp, stmt->body, level + 1);
    dump_stmt_end(fp, stmt, level);
    return;
  case ST_FOR:
    dump_stmt_start(fp, stmt, level, "FOR");
    if (stmt->init != NULL) {
      dump_stmt(fp, stmt->init, level + 1);
    } else {
      dump_range_start(fp, stmt->range);
      dump_indent(fp, level + 1);
      fprintf(fp, "<void>(NULL)\n");
    }
    if (stmt->cond != NULL) {
      dump_expr(fp, stmt->cond, level + 1);
    } else {
      dump_range_start(fp, stmt->range);
      dump_indent(fp, level + 1);
      fprintf(fp, "<void>(NULL)\n");
    }
    if (stmt->inc != NULL) {
      dump_expr(fp, stmt->inc, level + 1);
    } else {
      dump_range_start(fp, stmt->range);
      dump_indent(fp, level + 1);
      fprintf(fp, "<void>(NULL)\n");
    }
    dump_stmt(fp, stmt->body, level + 1);
    dump_stmt_end(fp, stmt, level);
    return;
  case ST_GOTO:
    dump_stmt_oneline(fp, stmt, level, "GOTO %s", stmt->name);
    return;
  case ST_BREAK:
    dump_stmt_oneline(fp, stmt, level, "BREAK");
    return;
  case ST_CONTINUE:
    dump_stmt_oneline(fp, stmt, level, "CONTINUE");
    return;
  case ST_RETURN:
    if (stmt->expr != NULL) {
      dump_stmt_start(fp, stmt, level, "RETURN");
      dump_expr(fp, stmt->expr, level + 1);
      dump_stmt_end(fp, stmt, level);
    } else {
      dump_stmt_oneline(fp, stmt, level, "RETURN");
    }
    return;
  case ST_NULL:
    dump_stmt_oneline(fp, stmt, level, "NULL");
    return;
  }
  error("未知のノードです: %d\n", stmt->ty);
}

static void dump_init(FILE *fp, Initializer *init, const Range *range,
                      int level) {
  if (init == NULL) {
    dump_range_start(fp, range);
    dump_indent(fp, level);
    fprintf(fp, "NULL\n");
    return;
  }

  if (init->expr != NULL) {
    dump_expr(fp, init->expr, level);
    return;
  }
  if (init->members != NULL) {
    dump_range_start(fp, range);
    dump_indent(fp, level);
    dump_type(fp, init->type);
    fprintf(fp, "{\n");
    for (int i = 0; i < vec_len(init->members); i++) {
      MemberInitializer *meminit = vec_get(init->members, i);
      dump_range_start(fp, range);
      dump_indent(fp, level + 1);
      fprintf(fp, ".%s = \n", meminit->member->name);
      dump_init(fp, meminit->init, range, level + 1);
    }
    dump_range_end(fp, range);
    dump_indent(fp, level);
    fprintf(fp, "}\n");
    return;
  }
  if (init->elements != NULL) {
    dump_range_start(fp, range);
    dump_indent(fp, level);
    dump_type(fp, init->type);
    fprintf(fp, "{\n");
    for (int i = 0; i < vec_len(init->elements); i++) {
      Initializer *val = vec_get(init->elements, i);
      dump_range_start(fp, range);
      dump_indent(fp, level + 1);
      fprintf(fp, "[%d] = \n", i);
      dump_init(fp, val, range, level + 1);
    }
    dump_range_end(fp, range);
    dump_indent(fp, level);
    fprintf(fp, "}\n");
    return;
  }
  assert(false);
}

static void output_ast(FILE *fp, TranslationUnit *tunit) {
  int level = 0;
  for (int i = 0; i < vec_len(tunit->func_list); i++) {
    Function *func = vec_get(tunit->func_list, i);

    dump_range_start(fp, func->range);
    dump_indent(fp, level);
    fprintf(fp, "FUNCTION ");
    dump_type(fp, func->type);
    fprintf(fp, " %s = {\n", func->name);
    dump_stmt(fp, func->body, level + 1);
    dump_range_end(fp, func->range);
    dump_indent(fp, level);
    fprintf(fp, "}\n");
  }
  for (int i = 0; i < vec_len(tunit->gvar_list); i++) {
    GlobalVar *gvar = vec_get(tunit->gvar_list, i);

    dump_range_start(fp, gvar->range);
    dump_indent(fp, level);
    fprintf(fp, "GLOBAL ");
    dump_type(fp, gvar->type);
    if (gvar->init != NULL) {
      fprintf(fp, " %s = \n", gvar->name);
      dump_init(fp, gvar->init, gvar->range, level + 1);
      dump_range_end(fp, gvar->range);
      dump_indent(fp, level);
      fprintf(fp, "\n");
    } else {
      fprintf(fp, " %s\n", gvar->name);
    }
  }
}

enum {
  OPTVAL_TEST = 256,
  OPTVAL_EMIT,
  OPTVAL_ASM_SYNTAX,
};

typedef enum {
  EMIT_PP_TOKEN = 0x01,
  EMIT_TOKEN = 0x02,
  EMIT_AST = 0x04,
  EMIT_SEMA = 0x08,
  EMIT_ASM = 0x10,
  EMIT_ALL = 0x1f,
} output_t;

struct option longopts[] = {
    {"test", no_argument, NULL, OPTVAL_TEST},
    {"emit", required_argument, NULL, OPTVAL_EMIT},
    {"output", required_argument, NULL, 'o'},
    {"asm-syntax", required_argument, NULL, OPTVAL_ASM_SYNTAX},
    {NULL, 0, 0, 0},
};

int main(int argc, char **argv) {
  asm_syntax_t asm_syntax = ASM_SYNTAX_INTEL;
  output_t emit_target = 0;
  const char *output = NULL;

  while (true) {
    int c = getopt_long(argc, argv, "o:", longopts, NULL);
    if (c == -1) {
      break;
    }

    switch (c) {
    case OPTVAL_TEST:
      runtest();
      return 0;

    case OPTVAL_EMIT:
      if (strcmp(optarg, "asm") == 0) {
        emit_target |= EMIT_ASM;
      } else if (strcmp(optarg, "pp_token") == 0) {
        emit_target |= EMIT_PP_TOKEN;
      } else if (strcmp(optarg, "token") == 0) {
        emit_target |= EMIT_TOKEN;
      } else if (strcmp(optarg, "ast") == 0) {
        emit_target |= EMIT_AST;
      } else if (strcmp(optarg, "sema") == 0) {
        emit_target |= EMIT_SEMA;
      } else if (strcmp(optarg, "all") == 0) {
        emit_target |= EMIT_ALL;
      } else {
        error("unrecognized option argument: %s", optarg);
      }
      break;

    case OPTVAL_ASM_SYNTAX:
      if (strcmp(optarg, "intel") == 0) {
        asm_syntax = ASM_SYNTAX_INTEL;
      } else if (strcmp(optarg, "att") == 0) {
        asm_syntax = ASM_SYNTAX_ATT;
      } else {
        error("unrecognized option argument: %s", optarg);
      }
      break;

    case 'o':
      output = optarg;
      break;

    case '?':
      return 1;
    }
  }

  if (emit_target == 0) {
    emit_target = EMIT_ASM;
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

  if (output == NULL) {
    output = replace_suffix(basename(strdup(filename)), ".c", ".s");
  }

  Reader *reader = new_reader();
  reader = phase2_filter(reader);
  reader_add_file(reader, file, filename);

  TokenIterator *ts = new_pp_tokenizer(reader);
  if (emit_target & EMIT_PP_TOKEN) {
    emit_target ^= EMIT_PP_TOKEN;
    FILE *fp = open_output_file(replace_suffix(output, ".s", ".pp_token"));
    TokenFilterArg *arg = NEW(TokenFilterArg);
    *arg = (TokenFilterArg){.fp = fp, .ts = ts};
    ts = new_token_iterator(token_filter, arg);
    if (emit_target == 0) {
      // consume all tokens to trigger event listener
      consume_all_token_iterator(ts);
      goto End;
    }
  }

  ts = phase6_filter(ts);
  ts = phase7_filter(ts);
  if (emit_target & EMIT_TOKEN) {
    emit_target ^= EMIT_TOKEN;
    FILE *fp = open_output_file(replace_suffix(output, ".s", ".token"));
    TokenFilterArg *arg = NEW(TokenFilterArg);
    *arg = (TokenFilterArg){.fp = fp, .ts = ts};
    ts = new_token_iterator(token_filter, arg);
    if (emit_target == 0) {
      // consume all tokens to trigger event listener
      consume_all_token_iterator(ts);
      goto End;
    }
  }

  TranslationUnit *tunit = parse(reader, ts);
  if (emit_target & EMIT_AST) {
    emit_target ^= EMIT_AST;
    FILE *fp = open_output_file(replace_suffix(output, ".s", ".ast"));
    output_ast(fp, tunit);
    if (emit_target == 0) {
      goto End;
    }
  }

  sema(tunit);
  if (emit_target & EMIT_SEMA) {
    emit_target ^= EMIT_SEMA;
    FILE *fp = open_output_file(replace_suffix(output, ".s", ".sema"));
    output_ast(fp, tunit);
    if (emit_target == 0) {
      goto End;
    }
  }

  assert(emit_target & EMIT_ASM);
  FILE *fp = open_output_file(output);
  gen(fp, tunit, asm_syntax);

End:
  complete_output_file();
  return 0;
}
