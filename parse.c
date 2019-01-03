#include "9cc.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

static Node *assign(void);
static Node *equal(void);
static Node *add(void);
static Node *mul(void);
static Node *term(void);

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
    vec_push(code, assign());
    while (consume(';'))
      ;
  }
  vec_push(code, NULL);
}

static Node *assign(void) {
  Node *lhs = equal();
  if (consume('='))
    return new_node('=', lhs, assign());
  return lhs;
}

static Node *equal(void) {
  Node *node = add();
  while (true) {
    if (consume(TK_EQEQ))
      node = new_node(ND_EQEQ, node, add());
    else if (consume(TK_NOTEQ))
      node = new_node(ND_NOTEQ, node, add());
    else
      return node;
  }
}

static Node *add(void) {
  Node *node = mul();
  while (true) {
    if (consume('+'))
      node = new_node('+', node, mul());
    else if (consume('-'))
      node = new_node('-', node, mul());
    else
      return node;
  }
}

static Node *mul(void) {
  Node *node = term();
  while (true) {
    if (consume('*'))
      node = new_node('*', node, term());
    else if (consume('/'))
      node = new_node('/', node, term());
    else
      return node;
  }
}

static Node *term(void) {
  if (get_token(pos)->ty == TK_NUM)
    return new_node_num(get_token(pos++)->val);
  if (get_token(pos)->ty == TK_IDENT)
    return new_node_ident(get_token(pos++)->name);
  if (consume('(')) {
    Node *node = assign();
    if (!consume(')'))
      error("開きカッコに対応する閉じカッコがありません: %s",
            get_token(pos)->input);
    return node;
  }
  error("数値でも開きカッコでもないトークンです: %s", get_token(pos)->input);
}
