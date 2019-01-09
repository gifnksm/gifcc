#include "gifcc.h"
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

// expression
static Node *primary_expression(void);
static Node *postfix_expression(void);
static Vector *argument_expression_list(void);
static Node *unary_expression(void);
static Node *cast_expression(void);
static Node *multiplicative_expression(void);
static Node *additive_expression(void);
static Node *shift_expression(void);
static Node *relational_expression(void);
static Node *equality_expression(void);
static Node *and_expression(void);
static Node *exclusive_or_expression(void);
static Node *inclusive_or_expression(void);
static Node *logical_and_expression(void);
static Node *logical_or_expression(void);
static Node *conditional_expression(void);
static Node *assignment_expression(void);
static Node *expression(void);
static Node *statement(void);
static Node *compound_statement(void);

static int pos = 0;
static Vector *code;
static int stack_size = 0;
static Map *stack_map;
static Node null_stmt = {
    .ty = ND_NULL,
};

static Node *new_node(int ty, Node *lhs, Node *rhs) {
  Node *node = malloc(sizeof(Node));
  node->ty = ty;
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

static Node *new_node_num(int val) {
  Node *node = malloc(sizeof(Node));
  node->ty = ND_NUM;
  node->val = val;
  return node;
}

static Node *new_node_ident(char *name) {
  Node *node = malloc(sizeof(Node));
  node->ty = ND_IDENT;
  node->name = name;
  if (!map_get(stack_map, name)) {
    int *offset = malloc(sizeof(int));
    *offset = stack_size;
    map_put(stack_map, name, offset);
    stack_size += 8;
  }
  return node;
}

static Node *new_node_call(Node *callee, Vector *argument) {
  Node *node = malloc(sizeof(Node));
  node->ty = ND_CALL;
  node->callee = callee;
  node->argument = argument;
  return node;
}

static Node *new_node_cond(Node *cond, Node *then_expr, Node *else_expr) {
  Node *node = malloc(sizeof(Node));
  node->ty = ND_COND;
  node->cond = cond;
  node->then_node = then_expr;
  node->else_node = else_expr;
  return node;
}

static Node *new_node_expr(Node *expr) {
  Node *node = malloc(sizeof(Node));
  node->ty = ND_EXPR;
  node->expr = expr;
  return node;
}

static Node *new_node_if(Node *cond, Node *then_node, Node *else_node) {
  Node *node = malloc(sizeof(Node));
  node->ty = ND_IF;
  node->cond = cond;
  node->then_node = then_node;
  node->else_node = else_node;
  return node;
}

static Node *new_node_while(Node *cond, Node *body) {
  Node *node = malloc(sizeof(Node));
  node->ty = ND_WHILE;
  node->cond = cond;
  node->body = body;
  return node;
}

static Node *new_node_do_while(Node *cond, Node *body) {
  Node *node = malloc(sizeof(Node));
  node->ty = ND_DO_WHILE;
  node->cond = cond;
  node->body = body;
  return node;
}

static Node *new_node_for(Node *init, Node *cond, Node *inc, Node *body) {
  Node *node = malloc(sizeof(Node));
  node->ty = ND_FOR;
  node->init = init;
  node->cond = cond;
  node->inc = inc;
  node->body = body;
  return node;
}

static bool consume(int ty) {
  if (get_token(pos)->ty != ty)
    return false;
  pos++;
  return true;
}

static void expect(int ty) {
  if (!consume(ty)) {
    if (ty <= 255) {
      error("'%c' がありません: %s", ty, get_token(pos)->input);
    } else {
      assert(ty == TK_WHILE);
      error("'while' がありません: %s", get_token(pos)->input);
    }
  }
}

Node *get_node(int pos) { return code->data[pos]; }
int get_stack_size(void) { return stack_size; }
int get_stack_offset(char *name) { return *(int *)map_get(stack_map, name); }

void program(void) {
  code = new_vector();
  stack_map = new_map();

  while (get_token(pos)->ty != TK_EOF) {
    vec_push(code, statement());
  }

  vec_push(code, NULL);
}

static Node *primary_expression(void) {
  if (get_token(pos)->ty == TK_NUM)
    return new_node_num(get_token(pos++)->val);
  if (get_token(pos)->ty == TK_IDENT)
    return new_node_ident(get_token(pos++)->name);
  if (consume('(')) {
    Node *node = expression();
    expect(')');
    return node;
  }
  error("数値でも開きカッコでもないトークンです: %s", get_token(pos)->input);
}

static Node *postfix_expression(void) {
  Node *node = primary_expression();
  while (true) {
    if (consume('(')) {
      Vector *argument = NULL;
      if (get_token(pos)->ty != ')') {
        argument = argument_expression_list();
      }
      expect(')');
      node = new_node_call(node, argument);
    } else if (consume(TK_INC)) {
      return new_node(ND_INC, node, NULL);
    } else if (consume(TK_DEC)) {
      return new_node(ND_DEC, node, NULL);
    } else {
      return node;
    }
  }
}

static Vector *argument_expression_list(void) {
  Vector *argument = new_vector();
  while (true) {
    vec_push(argument, assignment_expression());
    if (!consume(',')) {
      break;
    }
  }
  return argument;
}

static Node *unary_expression(void) {
  if (consume('&'))
    return new_node('&', NULL, cast_expression());
  if (consume('*'))
    return new_node('*', NULL, cast_expression());
  if (consume('+'))
    return new_node('+', NULL, cast_expression());
  if (consume('-'))
    return new_node('-', NULL, cast_expression());
  if (consume('~'))
    return new_node('~', NULL, cast_expression());
  if (consume('!'))
    return new_node('!', NULL, cast_expression());
  if (consume(TK_INC))
    return new_node(ND_INC, NULL, cast_expression());
  if (consume(TK_DEC))
    return new_node(ND_DEC, NULL, cast_expression());
  return postfix_expression();
}

static Node *cast_expression(void) { return unary_expression(); }

static Node *multiplicative_expression(void) {
  Node *node = cast_expression();
  while (true) {
    if (consume('*'))
      node = new_node('*', node, cast_expression());
    else if (consume('/'))
      node = new_node('/', node, cast_expression());
    else if (consume('%'))
      node = new_node('%', node, cast_expression());
    else
      return node;
  }
}

static Node *additive_expression(void) {
  Node *node = multiplicative_expression();
  while (true) {
    if (consume('+'))
      node = new_node('+', node, multiplicative_expression());
    else if (consume('-'))
      node = new_node('-', node, multiplicative_expression());
    else
      return node;
  }
}

static Node *shift_expression(void) {
  Node *node = additive_expression();
  while (true) {
    if (consume(TK_LSHIFT))
      node = new_node(ND_LSHIFT, node, additive_expression());
    else if (consume(TK_RSHIFT))
      node = new_node(ND_RSHIFT, node, additive_expression());
    else
      return node;
  }
  return node;
}

static Node *relational_expression(void) {
  Node *node = shift_expression();
  while (true) {
    if (consume('<'))
      node = new_node('<', node, shift_expression());
    else if (consume('>'))
      node = new_node('>', node, shift_expression());
    else if (consume(TK_LTEQ))
      node = new_node(ND_LTEQ, node, shift_expression());
    else if (consume(TK_GTEQ))
      node = new_node(ND_GTEQ, node, shift_expression());
    else
      return node;
  }
  return node;
}

static Node *equality_expression(void) {
  Node *node = relational_expression();
  while (true) {
    if (consume(TK_EQEQ))
      node = new_node(ND_EQEQ, node, relational_expression());
    else if (consume(TK_NOTEQ))
      node = new_node(ND_NOTEQ, node, relational_expression());
    else
      return node;
  }
}

static Node *and_expression(void) {
  Node *node = equality_expression();
  while (true) {
    if (consume('&'))
      node = new_node('&', node, equality_expression());
    else
      return node;
  }
  return node;
}

static Node *exclusive_or_expression(void) {
  Node *node = and_expression();
  while (true) {
    if (consume('^'))
      node = new_node('^', node, and_expression());
    else
      return node;
  }
  return node;
}
static Node *inclusive_or_expression(void) {
  Node *node = exclusive_or_expression();
  while (true) {
    if (consume('|'))
      node = new_node('|', node, exclusive_or_expression());
    else
      return node;
  }
  return node;
}
static Node *logical_and_expression(void) {
  Node *node = inclusive_or_expression();
  while (true) {
    if (consume(TK_LOGAND))
      node = new_node(ND_LOGAND, node, inclusive_or_expression());
    else
      return node;
  }
  return node;
}
static Node *logical_or_expression(void) {
  Node *node = logical_and_expression();
  while (true) {
    if (consume(TK_LOGOR))
      node = new_node(ND_LOGOR, node, logical_and_expression());
    else
      return node;
  }
  return node;
}
static Node *conditional_expression(void) {
  Node *cond = logical_or_expression();
  if (consume('?')) {
    Node *then_expr = expression();
    expect(':');
    Node *else_expr = conditional_expression();
    return new_node_cond(cond, then_expr, else_expr);
  }
  return cond;
}

static Node *assignment_expression(void) {
  Node *lhs = conditional_expression();
  if (consume('='))
    return new_node('=', lhs, assignment_expression());
  if (consume(TK_MUL_ASSIGN))
    return new_node(ND_MUL_ASSIGN, lhs, assignment_expression());
  if (consume(TK_DIV_ASSIGN))
    return new_node(ND_DIV_ASSIGN, lhs, assignment_expression());
  if (consume(TK_MOD_ASSIGN))
    return new_node(ND_MOD_ASSIGN, lhs, assignment_expression());
  if (consume(TK_ADD_ASSIGN))
    return new_node(ND_ADD_ASSIGN, lhs, assignment_expression());
  if (consume(TK_SUB_ASSIGN))
    return new_node(ND_SUB_ASSIGN, lhs, assignment_expression());
  if (consume(TK_LSHIFT_ASSIGN))
    return new_node(ND_LSHIFT_ASSIGN, lhs, assignment_expression());
  if (consume(TK_RSHIFT_ASSIGN))
    return new_node(ND_RSHIFT_ASSIGN, lhs, assignment_expression());
  if (consume(TK_AND_ASSIGN))
    return new_node(ND_AND_ASSIGN, lhs, assignment_expression());
  if (consume(TK_OR_ASSIGN))
    return new_node(ND_OR_ASSIGN, lhs, assignment_expression());
  if (consume(TK_XOR_ASSIGN))
    return new_node(ND_XOR_ASSIGN, lhs, assignment_expression());
  return lhs;
}

static Node *expression(void) {
  Node *node = assignment_expression();
  while (true) {
    if (consume(','))
      node = new_node(',', node, assignment_expression());
    else
      return node;
  }
  return node;
}

static Node *statement(void) {
  switch (get_token(pos)->ty) {
  case TK_IF: {
    pos++;
    expect('(');
    Node *cond = expression();
    expect(')');
    Node *then_stmt = statement();
    Node *else_stmt = &null_stmt;
    if (consume(TK_ELSE))
      else_stmt = statement();
    return new_node_if(cond, then_stmt, else_stmt);
  }
  case TK_WHILE: {
    pos++;
    expect('(');
    Node *cond = expression();
    expect(')');
    Node *body = statement();
    return new_node_while(cond, body);
  }
  case TK_DO: {
    pos++;
    Node *body = statement();
    expect(TK_WHILE);
    expect('(');
    Node *cond = expression();
    expect(')');
    expect(';');
    return new_node_do_while(cond, body);
  }
  case TK_FOR: {
    pos++;
    Node *init = NULL;
    Node *cond = NULL;
    Node *inc = NULL;
    expect('(');
    if (get_token(pos)->ty != ';')
      init = expression();
    expect(';');
    if (get_token(pos)->ty != ';')
      cond = expression();
    expect(';');
    if (get_token(pos)->ty != ')')
      inc = expression();
    expect(')');
    Node *body = statement();
    return new_node_for(init, cond, inc, body);
  }
  case TK_BREAK: {
    pos++;
    expect(';');
    return new_node(ND_BREAK, NULL, NULL);
  }
  case TK_CONTINUE: {
    pos++;
    expect(';');
    return new_node(ND_CONTINUE, NULL, NULL);
  }
  case '{': {
    return compound_statement();
  }
  case ';': {
    pos++;
    return &null_stmt;
  }
  default: {
    Node *expr = expression();
    expect(';');
    return new_node_expr(expr);
  }
  }
}

static Node *compound_statement(void) {
  expect('{');

  Node *node = new_node(ND_COMPOUND, NULL, NULL);
  node->stmts = new_vector();
  while (!consume('}')) {
    vec_push(node->stmts, statement());
  }
  return node;
}
