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
  case TY_BOOL:
    printf("%d", num.bool_val);
    break;
  case TY_CHAR:
    printf("%hhd", num.char_val);
    break;
  case TY_S_CHAR:
    printf("%hhd", num.s_char_val);
    break;
  case TY_S_SHORT:
    printf("%hd", num.s_short_val);
    break;
  case TY_S_INT:
    printf("%d", num.s_int_val);
    break;
  case TY_S_LONG:
    printf("%ld", num.s_long_val);
    break;
  case TY_S_LLONG:
    printf("%lld", num.s_llong_val);
    break;
  case TY_U_CHAR:
    printf("%hhu", num.u_char_val);
    break;
  case TY_U_SHORT:
    printf("%hu", num.u_short_val);
    break;
  case TY_U_INT:
    printf("%u", num.u_int_val);
    break;
  case TY_U_LONG:
    printf("%lu", num.u_long_val);
    break;
  case TY_U_LLONG:
    printf("%llu", num.u_llong_val);
    break;
  case TY_PTR:
    printf("%" PRIdPTR, num.ptr_val);
    break;
  case TY_ENUM:
    printf("%d", num.enum_val);
    break;
  case TY_FLOAT:
    printf("%f", num.float_val);
    break;
  case TY_DOUBLE:
    printf("%f", num.double_val);
    break;
  case TY_LDOUBLE:
    printf("%Lf", num.ldouble_val);
    break;
    break;
  case TY_VOID:
    printf("void");
    break;
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
      printf("%s", token->num);
      break;
    case TK_CHARCONST:
      dump_number(token->char_val);
      break;
    case TK_IDENT:
      printf("%s", token->ident);
      break;
    case TK_STR:
      print_string_literal(token->str);
      break;
    default:
      break;
    }
    printf("\n");
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
void dump_type(Type *ty) { printf("<%s>", format_type(ty, true)); }
static void dump_expr(Expr *expr, int level);
static void dump_init(Initializer *init, Range range, int level);
static void dump_unop_expr(Expr *expr, char *label, int level) {
  dump_range_start(expr->range);
  dump_indent(level);
  dump_type(expr->val_type);
  printf("(%s\n", label);
  dump_expr(expr->unop.operand, level + 1);
  dump_range_end(expr->range);
  dump_indent(level);
  printf(")\n");
}
static void dump_binop_expr(Expr *expr, char *label, int level) {
  dump_range_start(expr->range);
  dump_indent(level);
  dump_type(expr->val_type);
  printf("(%s\n", label);
  dump_expr(expr->binop.lhs, level + 1);
  dump_expr(expr->binop.rhs, level + 1);
  dump_range_end(expr->range);
  dump_indent(level);
  printf(")\n");
}
static void dump_expr(Expr *expr, int level) {
  switch (expr->ty) {
  // primary expression
  case EX_NUM:
    dump_range_start(expr->range);
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(NUM ");
    dump_number(expr->num);
    printf(")\n");
    return;
  case EX_STACK_VAR:
    dump_range_start(expr->range);
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(STACK_VAR %s@%d)\n", expr->stack_var->name,
           expr->stack_var->offset);
    return;
  case EX_GLOBAL_VAR:
    dump_range_start(expr->range);
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(GLOBAL_VAR %s)\n", expr->global_var.name);
    return;
  case EX_STR:
    dump_range_start(expr->range);
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(STR ");
    print_string_literal(expr->str);
    printf(")\n");
    return;
  case EX_COMPOUND:
    dump_range_start(expr->range);
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(COMPOUND ");
    dump_init(expr->compound, expr->range, level + 1);
    printf(")\n");
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
    dump_range_start(expr->range);
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(CALL\n");
    dump_expr(expr->call.callee, level + 1);
    if (expr->call.argument != NULL) {
      for (int i = 0; i < vec_len(expr->call.argument); i++) {
        dump_expr(vec_get(expr->call.argument, i), level + 1);
      }
    }
    dump_range_end(expr->range);
    dump_indent(level);
    printf(")\n");
    return;
  case EX_POST_INC:
    dump_unop_expr(expr, "[++(POST)]", level);
    return;
  case EX_POST_DEC:
    dump_unop_expr(expr, "[--(POST)]", level);
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
    dump_binop_expr(expr, "[,]", level);
    return;

  // ternary unary operator
  case EX_COND:
    dump_range_start(expr->range);
    dump_indent(level);
    dump_type(expr->val_type);
    printf("(COND\n");
    dump_expr(expr->cond.cond, level + 1);
    dump_expr(expr->cond.then_expr, level + 1);
    dump_expr(expr->cond.else_expr, level + 1);
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
    for (int i = 0; i < vec_len(stmt->stmts); i++) {
      dump_stmt(vec_get(stmt->stmts, i), level + 1);
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
      dump_expr(stmt->expr, level + 1);
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
    for (int i = 0; i < map_size(init->members); i++) {
      char *name;
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
