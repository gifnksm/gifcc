#include "gifcc.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

static Node *expression(void);
static Node *assignment_expression(void);
static Node *equality_expression(void);
static Node *addtive_expression(void);
static Node *multiplicative_expression(void);
static Node *postfix_expression(void);
static Vector *argument_expression_list(void);
static Node *primary_expression(void);

static int pos = 0;
static Vector *code;
static int stack_size = 0;
static Map *stack_map;

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

static bool consume(int ty) {
  if (get_token(pos)->ty != ty)
    return false;
  pos++;
  return true;
}

Node *get_node(int pos) { return code->data[pos]; }
int get_stack_size(void) { return stack_size; }
int get_stack_offset(char *name) { return *(int *)map_get(stack_map, name); }

void program(void) {
  code = new_vector();
  stack_map = new_map();

  while (get_token(pos)->ty != TK_EOF) {
    vec_push(code, expression());
    while (consume(';'))
      ;
  }
  vec_push(code, NULL);
}

static Node *expression(void) { return assignment_expression(); }

static Node *assignment_expression(void) {
  Node *lhs = equality_expression();
  if (consume('='))
    return new_node('=', lhs, assignment_expression());
  return lhs;
}

static Node *equality_expression(void) {
  Node *node = addtive_expression();
  while (true) {
    if (consume(TK_EQEQ))
      node = new_node(ND_EQEQ, node, addtive_expression());
    else if (consume(TK_NOTEQ))
      node = new_node(ND_NOTEQ, node, addtive_expression());
    else
      return node;
  }
}

static Node *addtive_expression(void) {
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

static Node *multiplicative_expression(void) {
  Node *node = postfix_expression();
  while (true) {
    if (consume('*'))
      node = new_node('*', node, postfix_expression());
    else if (consume('/'))
      node = new_node('/', node, postfix_expression());
    else
      return node;
  }
}

static Node *postfix_expression(void) {
  Node *node = primary_expression();
  while (true) {
    if (consume('(')) {
      Vector *argument = NULL;
      if (get_token(pos)->ty != ')') {
        argument = argument_expression_list();
      }
      if (!consume(')'))
        error("開きカッコに対応する閇じカッコがありません: %s",
              get_token(pos)->input);
      node = new_node_call(node, argument);
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

static Node *primary_expression(void) {
  if (get_token(pos)->ty == TK_NUM)
    return new_node_num(get_token(pos++)->val);
  if (get_token(pos)->ty == TK_IDENT)
    return new_node_ident(get_token(pos++)->name);
  if (consume('(')) {
    Node *node = expression();
    if (!consume(')'))
      error("開きカッコに対応する閉じカッコがありません: %s",
            get_token(pos)->input);
    return node;
  }
  error("数値でも開きカッコでもないトークンです: %s", get_token(pos)->input);
}
