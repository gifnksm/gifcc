#include "gifcc.h"
#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

static char *epilogue_label = NULL;
static Function *func_ctxt = NULL;
static Vector *break_labels = NULL;
static Vector *continue_labels = NULL;

static void gen_expr(const Reader *reader, Expr *expr);

typedef struct {
  const char *rax;
  const char *rdi;
  const char *rsi;
  const char *rdx;
  const char *rcx;
  const char *r8;
  const char *r9;
  const char *r10;
  const char *r11;
} Reg;

const Reg Reg8 = {
    .rax = "rax",
    .rdi = "rdi",
    .rsi = "rsi",
    .rdx = "rdx",
    .rcx = "rcx",
    .r8 = "r8",
    .r9 = "r9",
    .r10 = "r10",
    .r11 = "r11",
};
const Reg Reg4 = {
    .rax = "eax",
    .rdi = "edi",
    .rsi = "esi",
    .rdx = "edx",
    .rcx = "ecx",
    .r8 = "r8d",
    .r9 = "r9d",
    .r10 = "r10d",
    .r11 = "r11d",
};
const Reg Reg2 = {
    .rax = "ax",
    .rdi = "di",
    .rsi = "si",
    .rdx = "dx",
    .rcx = "cx",
    .r8 = "r8w",
    .r9 = "r9w",
    .r10 = "r10w",
    .r11 = "r11w",
};
const Reg Reg1 = {
    .rax = "al",
    .rdi = "dil",
    .rsi = "sil",
    .rdx = "dl",
    .rcx = "cl",
    .r8 = "r8b",
    .r9 = "r9b",
    .r10 = "r10b",
    .r11 = "r11b",
};

static const Reg *get_int_reg(Type *ty, Range range) {
  switch (get_val_size(ty, range)) {
  case 8:
    return &Reg8;
  case 4:
    return &Reg4;
  case 2:
    return &Reg2;
  case 1:
    return &Reg1;
  default:
    range_error(range, "サポートしていない型サイズです");
  }
}

char *make_label(void) {
  static int count = 0;
  char buf[256];
  sprintf(buf, ".L%d", count++);
  return strdup(buf);
}

static void gen_lval(const Reader *reader, Expr *expr) {
  if (expr->ty == EX_STACK_VAR) {
    StackVar *var = expr->stack_var;
    assert(var != NULL);
    printf("  lea rax, [rbp - %d]\n",
           align(func_ctxt->stack_size, 16) - var->offset);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == EX_GLOBAL_VAR) {
    printf("  lea rax, %s[rip]\n", expr->name);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == '*' && expr->lhs == NULL) {
    gen_expr(reader, expr->rhs);
    return;
  }

  range_error(expr->range, "左辺値が変数ではありません");
}

static char *num2str(Number num, Range range) {
  char buf[1024];
  switch (num.type) {
  case TY_INT:
    sprintf(buf, "%d", num.int_val);
    break;
  case TY_CHAR:
    sprintf(buf, "%hhd", num.char_val);
    break;
  case TY_SHORT:
    sprintf(buf, "%hd", num.short_val);
    break;
  case TY_LONG:
    sprintf(buf, "%ld", num.long_val);
    break;
  case TY_PTR:
    sprintf(buf, "%" PRIdPTR, num.ptr_val);
    break;
  case TY_VOID:
    sprintf(buf, "0");
    break;
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    range_error(range, "不正な型の数値です: %d", num.type);
  }
  return strdup(buf);
}

static void gen_expr(const Reader *reader, Expr *expr) {
  const Reg *r = get_int_reg(expr->val_type, expr->range);

  if (expr->ty == EX_NUM) {
    printf("  push %s\n", num2str(expr->num_val, expr->range));
    return;
  }

  if (expr->ty == EX_STACK_VAR) {
    StackVar *var = expr->stack_var;
    assert(var != NULL);
    printf("  mov %s, [rbp - %d]\n", r->rax,
           align(func_ctxt->stack_size, 16) - var->offset);
    printf("  push rax\n");
    return;
  }

  if (expr->ty == EX_GLOBAL_VAR) {
    printf("  mov %s, %s[rip]\n", r->rax, expr->name);
    printf("  push rax\n");
    return;
  }

  if (expr->ty == EX_STR) {
    printf("  lea rax, %s[rip]\n", expr->name);
    printf("  push rax\n");
    return;
  }

  if (expr->ty == EX_CALL) {
    int num_push = 0;
    Vector *arg = expr->argument;
    bool call_direct;
    if (expr->callee->val_type->ty == TY_FUNC) {
      call_direct = true;
    } else if (expr->callee->val_type->ty == TY_PTR &&
               expr->callee->val_type->ptrof->ty == TY_FUNC) {
      call_direct = false;
    } else if (expr->callee->ty == EX_GLOBAL_VAR) {
      call_direct = true;
    } else {
      range_error(expr->callee->range,
                  "関数または関数ポインタ以外を呼び出そうとしました: %d",
                  expr->callee->val_type->ty);
    }
    if (arg && arg->len > 0) {
      // 引数をスタックに積む
      for (int i = arg->len - 1; i >= 0; i--) {
        gen_expr(reader, arg->data[i]);
      }
    }
    if (!call_direct) {
      gen_expr(reader, expr->callee);
      printf("  pop r10\n");
    }
    if (arg && arg->len > 0) {
      // レジスタ渡しする引数をpopする
      for (int i = 0; i < arg->len; i++) {
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
          printf("  pop r8\n");
          break;
        case 5:
          printf("  pop r9\n");
          break;
        // 6番目以降の引数はスタック経由で渡すため、pushされたままにする
        default:
          num_push++;
          break;
        }
      }
    }
    printf("  mov al, 0\n");
    if (call_direct) {
      printf("  call %s\n", expr->callee->name);
    } else {
      printf("  call r10\n");
    }
    if (num_push > 0) {
      printf("  add rsp, %d\n", 8 * num_push);
    }
    printf("  push rax\n");
    return;
  }

  if (expr->ty == EX_CAST) {
    Expr *operand = expr->expr;
    gen_expr(reader, operand);
    const Reg *from = get_int_reg(operand->val_type, operand->range);
    if (get_val_size(expr->val_type, expr->range) >
        get_val_size(operand->val_type, operand->range)) {
      printf("  pop rax\n");
      printf("  movsx %s, %s\n", r->rax, from->rax);
      printf("  push rax\n");
    }
    return;
  }

  if (expr->ty == '=') {
    gen_lval(reader, expr->lhs);
    gen_expr(reader, expr->rhs);
    printf("  pop rdi\n");
    printf("  pop rax\n");
    printf("  mov [rax], %s\n", r->rdi);
    printf("  push rdi\n");
    return;
  }
  if (expr->ty == EX_LOGAND) {
    char *false_label = make_label();
    char *end_label = make_label();
    gen_expr(reader, expr->lhs);
    printf("  pop rax\n");
    printf("  cmp %s, 0\n", r->rax);
    printf("  je %s\n", false_label);
    gen_expr(reader, expr->rhs);
    printf("  pop rax\n");
    printf("  cmp %s, 0\n", r->rax);
    printf("  je %s\n", false_label);
    printf("  push 1\n");
    printf("  jmp %s\n", end_label);
    printf("%s:\n", false_label);
    printf("  push 0\n");
    printf("%s:\n", end_label);
    return;
  }

  if (expr->ty == EX_LOGOR) {
    char *true_label = make_label();
    char *end_label = make_label();
    gen_expr(reader, expr->lhs);
    printf("  pop rax\n");
    printf("  cmp %s, 0\n", r->rax);
    printf("  jne %s\n", true_label);
    gen_expr(reader, expr->rhs);
    printf("  pop rax\n");
    printf("  cmp %s, 0\n", r->rax);
    printf("  jne %s\n", true_label);
    printf("  push 0\n");
    printf("  jmp %s\n", end_label);
    printf("%s:\n", true_label);
    printf("  push 1\n");
    printf("%s:\n", end_label);
    return;
  }

  if (expr->ty == EX_COND) {
    char *else_label = make_label();
    char *end_label = make_label();
    gen_expr(reader, expr->cond);
    printf("  pop rax\n");
    printf("  cmp %s, 0\n", r->rax);
    printf("  je %s\n", else_label);
    gen_expr(reader, expr->lhs);
    printf("  jmp %s\n", end_label);
    printf("%s:\n", else_label);
    gen_expr(reader, expr->rhs);
    printf("%s:\n", end_label);
    return;
  }

  if (expr->ty == '&' && expr->lhs == NULL) {
    // 単項の `&`
    gen_lval(reader, expr->rhs);
    return;
  }
  if (expr->ty == '*' && expr->lhs == NULL) {
    // 単項の `*`
    gen_expr(reader, expr->rhs);
    printf("  pop rax\n");
    printf("  mov %s, [rax]\n", r->rax);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == '+' && expr->lhs == NULL) {
    // 単項の `+`
    gen_expr(reader, expr->rhs);
    return;
  }
  if (expr->ty == '-' && expr->lhs == NULL) {
    // 単項の `-`
    gen_expr(reader, expr->rhs);
    printf("  pop rax\n");
    printf("  neg %s\n", r->rax);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == '~') {
    // `~`
    gen_expr(reader, expr->rhs);
    printf("  pop rax\n");
    printf("  not %s\n", r->rax);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == '!') {
    // `!`
    gen_expr(reader, expr->rhs);
    printf("  pop rax\n");
    printf("  cmp %s, 0\n", r->rax);
    printf("  sete al\n");
    printf("  movzb %s, al\n", r->rax);
    printf("  push rax\n");
    return;
  }
  if (expr->ty == EX_INC) {
    if (expr->lhs == NULL) {
      // 前置の `++`
      gen_lval(reader, expr->rhs);
      printf("  pop rax\n");
      printf("  mov %s, [rax]\n", r->rdi);
      printf("  add %s, %d\n", r->rdi, expr->incdec_size);
      printf("  mov [rax], %s\n", r->rdi);
      printf("  push rdi\n");
      return;
    }
    // 後置の `++`
    gen_lval(reader, expr->lhs);
    printf("  pop rax\n");
    printf("  mov %s, [rax]\n", r->rdi);
    printf("  push rdi\n");
    printf("  add %s, %d\n", r->rdi, expr->incdec_size);
    printf("  mov [rax], %s\n", r->rdi);
    return;
  }

  if (expr->ty == EX_DEC) {
    if (expr->lhs == NULL) {
      // 前置の `--`
      gen_lval(reader, expr->rhs);
      printf("  pop rax\n");
      printf("  mov %s, [rax]\n", r->rdi);
      printf("  sub %s, %d\n", r->rdi, expr->incdec_size);
      printf("  mov [rax], %s\n", r->rdi);
      printf("  push rdi\n");
      return;
    }
    // 後置の `--`
    gen_lval(reader, expr->lhs);
    printf("  pop rax\n");
    printf("  mov %s, [rax]\n", r->rdi);
    printf("  push rdi\n");
    printf("  sub %s, %d\n", r->rdi, expr->incdec_size);
    printf("  mov [rax], %s\n", r->rdi);
    return;
  }

  // 二項演算子
  assert(expr->lhs != NULL);
  assert(expr->rhs != NULL);

  gen_expr(reader, expr->lhs);
  gen_expr(reader, expr->rhs);

  printf("  pop rdi\n");
  printf("  pop rax\n");

  switch (expr->ty) {
  case '+':
    printf("  add %s, %s\n", r->rax, r->rdi);
    break;
  case '-':
    printf("  sub %s, %s\n", r->rax, r->rdi);
    break;
  case '*':
    printf("  mul %s\n", r->rdi);
    break;
  case '/':
    printf("  mov %s, 0\n", r->rdx);
    printf("  div %s\n", r->rdi);
    break;
  case '%':
    printf("  mov %s, 0\n", r->rdx);
    printf("  div %s\n", r->rdi);
    printf("  mov %s, %s\n", r->rax, r->rdx);
    break;
  case EX_EQEQ:
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    printf("  sete al\n");
    printf("  movzb %s, al\n", r->rax);
    break;
  case EX_NOTEQ:
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    printf("  setne al\n");
    printf("  movzb %s, al\n", r->rax);
    break;
  case '<':
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    printf("  setl al\n");
    printf("  movzb %s, al\n", r->rax);
    break;
  case '>':
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    printf("  setg al\n");
    printf("  movzb %s, al\n", r->rax);
    break;
  case EX_LTEQ:
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    printf("  setle al\n");
    printf("  movzb %s, al\n", r->rax);
    break;
  case EX_GTEQ:
    printf("  cmp %s, %s\n", r->rax, r->rdi);
    printf("  setge al\n");
    printf("  movzb %s, al\n", r->rax);
    break;
  case EX_LSHIFT:
    printf("  mov %s, %s\n", r->rcx, r->rdi);
    printf("  shl %s, cl\n", r->rax);
    break;
  case EX_RSHIFT:
    printf("  mov %s, %s\n", r->rcx, r->rdi);
    printf("  sar %s, cl\n", r->rax);
    break;
  case '&':
    printf("  and %s, %s\n", r->rax, r->rdi);
    break;
  case '^':
    printf("  xor %s, %s\n", r->rax, r->rdi);
    break;
  case '|':
    printf("  or %s, %s\n", r->rax, r->rdi);
    break;
  case ',':
    printf("  mov %s, %s\n", r->rax, r->rdi);
    break;
  default:
    assert(false);
  }

  printf("  push rax\n");
}

static void gen_stmt(const Reader *reader, Stmt *stmt) {
  switch (stmt->ty) {
  case ST_NULL: {
    return;
  }
  case ST_EXPR: {
    gen_expr(reader, stmt->expr);

    // 式の評価結果としてスタックに一つの値が残っている
    // はずなので、スタックが溢れないようにポップしておく
    printf("  pop rax\n");
    return;
  }
  case ST_COMPOUND: {
    for (int i = 0; i < stmt->stmts->len; i++) {
      gen_stmt(reader, stmt->stmts->data[i]);
    }
    return;
  }
  case ST_IF: {
    char *else_label = make_label();
    char *end_label = make_label();
    gen_expr(reader, stmt->cond);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je %s\n", else_label);
    gen_stmt(reader, stmt->then_stmt);
    printf("  jmp %s\n", end_label);
    printf("%s:\n", else_label);
    gen_stmt(reader, stmt->else_stmt);
    printf("%s:\n", end_label);
    return;
  }
  case ST_SWITCH: {
    char *end_label = make_label();
    gen_expr(reader, stmt->cond);
    for (int i = 0; i < stmt->cases->len; i++) {
      Stmt *case_expr = stmt->cases->data[i];
      gen_expr(reader, case_expr->expr);
      printf("  pop rax\n");
      printf("  pop rdi\n");
      printf("  cmp rax, rdi\n");
      printf("  je %s\n", case_expr->label);
      printf("  push rdi\n");
    }
    printf("  pop rdi\n");
    if (stmt->default_case) {
      printf("  jmp %s\n", stmt->default_case->label);
    } else {
      printf("  jmp %s\n", end_label);
    }
    vec_push(break_labels, end_label);
    gen_stmt(reader, stmt->body);
    vec_pop(break_labels);
    printf("%s:", end_label);
    return;
  }
  case ST_CASE:
  case ST_DEFAULT:
  case ST_LABEL: {
    printf("%s:\n", stmt->label);
    return;
  }
  case ST_WHILE: {
    char *cond_label = make_label();
    char *end_label = make_label();
    printf("%s:\n", cond_label);
    gen_expr(reader, stmt->cond);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je %s\n", end_label);

    vec_push(break_labels, end_label);
    vec_push(continue_labels, cond_label);
    gen_stmt(reader, stmt->body);
    vec_pop(break_labels);
    vec_pop(continue_labels);

    printf("  jmp %s\n", cond_label);
    printf("%s:\n", end_label);
    return;
  }
  case ST_DO_WHILE: {
    char *loop_label = make_label();
    char *cond_label = make_label();
    char *end_label = make_label();
    printf("%s:\n", loop_label);

    vec_push(break_labels, end_label);
    vec_push(continue_labels, cond_label);
    gen_stmt(reader, stmt->body);
    vec_pop(break_labels);
    vec_pop(continue_labels);

    printf("%s:\n", cond_label);
    gen_expr(reader, stmt->cond);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  jne %s\n", loop_label);
    printf("%s:\n", end_label);
    return;
  }
  case ST_FOR: {
    char *cond_label = make_label();
    char *inc_label = make_label();
    char *end_label = make_label();
    if (stmt->init != NULL) {
      gen_expr(reader, stmt->init);
    }
    printf("%s:\n", cond_label);
    if (stmt->cond != NULL) {
      gen_expr(reader, stmt->cond);
      printf("  pop rax\n");
      printf("  cmp rax, 0\n");
      printf("  je %s\n", end_label);
    }

    vec_push(break_labels, end_label);
    vec_push(continue_labels, inc_label);
    gen_stmt(reader, stmt->body);
    vec_pop(break_labels);
    vec_pop(continue_labels);

    printf("%s:\n", inc_label);
    if (stmt->inc != NULL) {
      gen_expr(reader, stmt->inc);
    }
    printf("  jmp %s\n", cond_label);
    printf("%s:\n", end_label);
    return;
  }
  case ST_GOTO: {
    char *label = get_label(func_ctxt, stmt->name);
    if (label == NULL) {
      range_error(stmt->range, "未知のラベルへのgotoです: %s", stmt->name);
    }
    printf("  jmp %s\n", label);
    return;
  }
  case ST_BREAK: {
    if (break_labels->len <= 0) {
      range_error(stmt->range,
                  "ループでもswitch文中でもない箇所にbreakがあります");
    }
    printf("  jmp %s\n", (char *)break_labels->data[break_labels->len - 1]);
    return;
  }
  case ST_CONTINUE: {
    if (continue_labels->len <= 0) {
      range_error(stmt->range, "ループ中でない箇所にcontinueがあります");
    }
    printf("  jmp %s\n",
           (char *)continue_labels->data[continue_labels->len - 1]);
    return;
  }
  case ST_RETURN: {
    if (stmt->expr != NULL) {
      gen_expr(reader, stmt->expr);
      printf("  pop rax\n");
    }
    printf("  jmp %s\n", epilogue_label);
    return;
  }
  }
  assert(false);
}

static void gen_func(const Reader *reader, Function *func) {
  epilogue_label = make_label();
  func_ctxt = func;
  break_labels = new_vector();
  continue_labels = new_vector();

  printf(".global %s\n", func->name);
  printf("%s:\n", func->name);

  // プロローグ
  // スタックサイズ分の領域を確保する
  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");
  printf("  sub rsp, %d\n", align(func->stack_size, 16));

  // 引数をスタックへコピー
  for (int i = 0; i < func->type->func_param->len; i++) {
    Param *param = func->type->func_param->data[i];
    StackVar *var = param->stack_var;
    assert(var != NULL);
    const Reg *r = get_int_reg(var->type, var->range);
    printf("  lea rax, [rbp - %d]\n",
           align(func_ctxt->stack_size, 16) - var->offset);
    switch (i) {
    case 0:
      printf("  mov [rax], %s\n", r->rdi);
      break;
    case 1:
      printf("  mov [rax], %s\n", r->rsi);
      break;
    case 2:
      printf("  mov [rax], %s\n", r->rdx);
      break;
    case 3:
      printf("  mov [rax], %s\n", r->rcx);
      break;
    case 4:
      printf("  mov [rax], %s\n", r->r8);
      break;
    case 5:
      printf("  mov [rax], %s\n", r->r9);
      break;
      // 6番目以降の引数はスタック経由で渡すため、スタックからコピーする
    default:
      printf("  mov %s, [rbp + %d]\n", r->r11, (i - 6) * 8 + 16);
      printf("  mov [rax], %s\n", r->r11);
      break;
    }
  }

  gen_stmt(reader, func->body);

  // エピローグ
  // 最後の式の結果がRAXに残っているのでそれが返り値になる
  printf("%s:\n", epilogue_label);
  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
}

static void gen_gvar(GlobalVar *gvar) {
  Expr *init = gvar->init;
  if (init == NULL) {
    printf("  .bss\n");
    printf(".global %s\n", gvar->name);
    printf("%s:\n", gvar->name);
    printf("  .zero %d\n", get_val_size(gvar->type, gvar->range));
  } else {
    printf("  .data\n");
    printf(".global %s\n", gvar->name);
    printf("%s:\n", gvar->name);
    if (init->ty == EX_NUM) {
      if (init->val_type->ty == TY_INT) {
        printf("  .long %d\n", init->num_val.int_val);
      } else if (init->val_type->ty == TY_LONG) {
        printf("  .quad %ld\n", init->num_val.long_val);
      } else if (init->val_type->ty == TY_PTR) {
        printf("  .quad %" PRIdPTR "\n", init->num_val.ptr_val);
      } else {
        range_error(gvar->range, "int型ではありません");
      }
    } else if (init->ty == '&' && init->lhs == NULL && init->rhs != NULL) {
      if (init->rhs->ty != EX_GLOBAL_VAR) {
        range_error(gvar->range, "グローバル変数以外へのポインタです");
      }
      printf("  .quad %s\n", init->rhs->name);
    } else {
      range_error(gvar->range, "数値でもポインタでもありません");
    }
  }
}

static void gen_str(StringLiteral *str) {
  printf("%s:\n", str->name);
  printf("  .string ");
  print_string_literal(str->val);
  printf("\n");
}

void gen(const Reader *reader, TranslationUnit *tunit) {
  printf(".intel_syntax noprefix\n");

  printf("  .text\n");
  for (int i = 0; i < tunit->func_list->len; i++) {
    gen_func(reader, tunit->func_list->data[i]);
  }
  for (int i = 0; i < tunit->gvar_list->len; i++) {
    gen_gvar(tunit->gvar_list->data[i]);
  }
  printf("  .section .rodata\n");
  for (int i = 0; i < tunit->str_list->len; i++) {
    gen_str(tunit->str_list->data[i]);
  }
}
