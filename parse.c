#include <stdlib.h>

#include "9cc.h"

static Node *assign(void);
static Node *equal(void);
static Node *expr(void);
static Node *mul(void);
static Node *term(void);

static int pos = 0;

Node *code[100];

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

void program(void) {
  int i = 0;
  while (tokens[pos].ty != TK_EOF) {
    code[i++] = assign();
    if (tokens[pos].ty == ';')
      pos++;
  }
  code[i] = NULL;
}

static Node *assign(void) {
  Node *lhs = equal();
  if (tokens[pos].ty == '=') {
    pos++;
    return new_node('=', lhs, assign());
  }
  return lhs;
}

static Node *equal(void) {
  Node *lhs = expr();
  if (tokens[pos].ty == TK_EQEQ) {
    pos++;
    return new_node(ND_EQEQ, lhs, equal());
  }
  if (tokens[pos].ty == TK_NOTEQ) {
    pos++;
    return new_node(ND_NOTEQ, lhs, equal());
  }
  return lhs;
}

static Node *expr(void) {
  Node *lhs = mul();
  if (tokens[pos].ty == '+') {
    pos++;
    return new_node('+', lhs, expr());
  }
  if (tokens[pos].ty == '-') {
    pos++;
    return new_node('-', lhs, expr());
  }
  return lhs;
}

static Node *mul(void) {
  Node *lhs = term();
  if (tokens[pos].ty == '*') {
    pos++;
    return new_node('*', lhs, mul());
  }
  if (tokens[pos].ty == '/') {
    pos++;
    return new_node('/', lhs, mul());
  }
  return lhs;
}

static Node *term(void) {
  if (tokens[pos].ty == TK_NUM)
    return new_node_num(tokens[pos++].val);
  if (tokens[pos].ty == TK_IDENT)
    return new_node_ident(tokens[pos++].name);
  if (tokens[pos].ty == '(') {
    pos++;
    Node *node = assign();
    if (tokens[pos].ty != ')')
      error("開きカッコに対応する閉じカッコがありません: %s",
            tokens[pos].input);

    pos++;
    return node;
  }
  error("数値でも開きカッコでもないトークンです: %s", tokens[pos].input);
}
