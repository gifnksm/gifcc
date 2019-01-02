#include "9cc.h"
#include <stdlib.h>

static Node *assign(void);
static Node *equal(void);
static Node *expr(void);
static Node *mul(void);
static Node *term(void);

static int pos = 0;
static Vector *code;

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

static Node *new_node_ident(char name) {
  Node *node = malloc(sizeof(Node));
  node->ty = ND_IDENT;
  node->name = name;
  return node;
}

Node *get_node(int pos) { return code->data[pos]; }

void program(void) {
  code = new_vector();
  while (get_token(pos)->ty != TK_EOF) {
    vec_push(code, assign());
    if (get_token(pos)->ty == ';')
      pos++;
  }
  vec_push(code, NULL);
}

static Node *assign(void) {
  Node *lhs = equal();
  if (get_token(pos)->ty == '=') {
    pos++;
    return new_node('=', lhs, assign());
  }
  return lhs;
}

static Node *equal(void) {
  Node *lhs = expr();
  if (get_token(pos)->ty == TK_EQEQ) {
    pos++;
    return new_node(ND_EQEQ, lhs, equal());
  }
  if (get_token(pos)->ty == TK_NOTEQ) {
    pos++;
    return new_node(ND_NOTEQ, lhs, equal());
  }
  return lhs;
}

static Node *expr(void) {
  Node *lhs = mul();
  if (get_token(pos)->ty == '+') {
    pos++;
    return new_node('+', lhs, expr());
  }
  if (get_token(pos)->ty == '-') {
    pos++;
    return new_node('-', lhs, expr());
  }
  return lhs;
}

static Node *mul(void) {
  Node *lhs = term();
  if (get_token(pos)->ty == '*') {
    pos++;
    return new_node('*', lhs, mul());
  }
  if (get_token(pos)->ty == '/') {
    pos++;
    return new_node('/', lhs, mul());
  }
  return lhs;
}

static Node *term(void) {
  if (get_token(pos)->ty == TK_NUM)
    return new_node_num(get_token(pos++)->val);
  if (get_token(pos)->ty == TK_IDENT)
    return new_node_ident(get_token(pos++)->name);
  if (get_token(pos)->ty == '(') {
    pos++;
    Node *node = assign();
    if (get_token(pos)->ty != ')')
      error("開きカッコに対応する閉じカッコがありません: %s",
            get_token(pos)->input);

    pos++;
    return node;
  }
  error("数値でも開きカッコでもないトークンです: %s", get_token(pos)->input);
}
