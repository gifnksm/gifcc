#pragma once

typedef struct {
  void **data;
  int capacity;
  int len;
} Vector;

typedef struct {
  Vector *keys;
  Vector *vals;
} Map;

// トークンの型を表す値
enum {
  TK_NUM = 256, // 整数トークン
  TK_IDENT,     // 識別子
  TK_EQEQ,      // `==`
  TK_NOTEQ,     // `!=`
  TK_EOF,       // 入力の終わりを表すトークン
};

// トークンの型
typedef struct {
  int ty;      // トークンの型
  int val;     // tyがTK_NUMの場合、その数値
  char name;   // tyがTK_IDENTの場合、その名前
  char *input; // トークン文字列 (エラーメッセージ用)
} Token;

enum {
  ND_NUM = 256, // 整数のノードの型
  ND_IDENT,
  ND_EQEQ,
  ND_NOTEQ,
};

typedef struct Node {
  int ty;           // ノードの型
  struct Node *lhs; // 左辺
  struct Node *rhs; // 右辺
  int val;          // tyがND_NUMの場合のみ使う
  char name;        // tyがND_IDENTの場合のみ使う
} Node;

extern Token tokens[100];
extern Node *code[100];

__attribute__((noreturn, format(printf, 1, 2))) void error(char *fmt, ...);
Vector *new_vector(void);
void vec_push(Vector *vec, void *elem);
Map *new_map(void);
void map_put(Map *map, char *key, void *val);
void *map_get(Map *map, char *key);
void tokenize(char *p);
void program(void);
void gen(Node *node);
