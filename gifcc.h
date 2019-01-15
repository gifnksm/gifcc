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
  TK_NUM = 256,     // 整数トークン
  TK_IDENT,         // 識別子
  TK_EQEQ,          // `==`
  TK_NOTEQ,         // `!=`
  TK_LTEQ,          // `<=`
  TK_GTEQ,          // `>=`
  TK_LSHIFT,        // `<<`
  TK_RSHIFT,        // `>>`
  TK_LOGAND,        // `&&`
  TK_LOGOR,         // `||`
  TK_INC,           // `++`
  TK_DEC,           // `--`
  TK_MUL_ASSIGN,    // `*=`
  TK_DIV_ASSIGN,    // `/=`
  TK_MOD_ASSIGN,    // `%=`
  TK_ADD_ASSIGN,    // `+=`
  TK_SUB_ASSIGN,    // `-=`
  TK_LSHIFT_ASSIGN, // `<<=`
  TK_RSHIFT_ASSIGN, // `>>=`
  TK_AND_ASSIGN,    // `&=`
  TK_OR_ASSIGN,     // `|=`
  TK_XOR_ASSIGN,    // `^=`
  TK_IF,            // `if`
  TK_ELSE,          // `else`
  TK_SWITCH,        // `switch`
  TK_CASE,          // `case`
  TK_DEFAULT,       // `default`
  TK_WHILE,         // `while`
  TK_DO,            // `do`
  TK_FOR,           // `for`
  TK_GOTO,          // `goto`
  TK_BREAK,         // `break`
  TK_CONTINUE,      // `continue`
  TK_EOF,           // 入力の終わりを表すトークン
};

// トークンの型
typedef struct {
  int ty;      // トークンの型
  int val;     // tyがTK_NUMの場合、その数値
  char *name;  // tyがTK_IDENTの場合、その名前
  char *input; // トークン文字列 (エラーメッセージ用)
} Token;

enum {
  ND_NUM = 256, // 整数のノードの型
  ND_IDENT,
  ND_EQEQ,
  ND_NOTEQ,
  ND_LTEQ,
  ND_GTEQ,
  ND_LSHIFT,
  ND_RSHIFT,
  ND_LOGAND,
  ND_LOGOR,
  ND_COND,
  ND_INC,
  ND_DEC,
  ND_MUL_ASSIGN,
  ND_DIV_ASSIGN,
  ND_MOD_ASSIGN,
  ND_ADD_ASSIGN,
  ND_SUB_ASSIGN,
  ND_LSHIFT_ASSIGN,
  ND_RSHIFT_ASSIGN,
  ND_AND_ASSIGN,
  ND_OR_ASSIGN,
  ND_XOR_ASSIGN,
  ND_CALL,
  // statement
  ND_EXPR,
  ND_COMPOUND,
  ND_IF,
  ND_SWITCH,
  ND_CASE,
  ND_DEFAULT,
  ND_LABEL,
  ND_WHILE,
  ND_DO_WHILE,
  ND_FOR,
  ND_GOTO,
  ND_BREAK,
  ND_CONTINUE,
  ND_NULL,
};

typedef struct Node {
  int ty;           // ノードの型
  struct Node *lhs; // 左辺
  struct Node *rhs; // 右辺

  int val;    // tyがND_NUMの場合のみ使う
  char *name; // tyがND_IDENT, ND_LABEL, ND_GOTOの場合のみ使う

  // ND_CALL: <callee>(<argument>...)
  struct Node *callee;
  Vector *argument;

  // ND_COND:     <cond> ? <then_node> : <else_node>
  // ND_IF:       if (<cond>) <then_node> else <else_node>
  // ND_SWITCH:   switch (<cond>) <body>
  // ND_WHILE:    while (<cond>) <body>
  // ND_DO_WHILE: do <body> while(<cond>);
  // ND_FOR:      for (<init>; <cond>; <inc>) <body>
  struct Node *init;
  struct Node *cond;
  struct Node *inc;
  struct Node *then_node;
  struct Node *else_node;
  struct Node *body;

  // ND_SWITCH
  Vector *cases;
  struct Node *default_case;

  // ND_CASE, ND_DEFAULT, ND_LABEL
  char *label;

  struct Node *expr; // tyがND_EXPR, ND_CASEの場合のみ使う

  Vector *stmts; // tyがND_COMPOUNDの場合のみ使う
} Node;

#define error(fmt, ...) error_raw(__FILE__, __LINE__, fmt, ##__VA_ARGS__)

__attribute__((noreturn, format(printf, 3, 4))) void
error_raw(const char *file, int line, char *fmt, ...);

static inline int align(int n, int s) { return ((n + (s - 1)) / s) * s; }

Vector *new_vector(void);
void vec_push(Vector *vec, void *elem);
void *vec_pop(Vector *vec);
Map *new_map(void);
void map_put(Map *map, char *key, void *val);
void *map_get(Map *map, char *key);
void runtest(void);

Token *get_token(int pos);
void tokenize(char *p);

Node *get_node(int pos);
void program(void);
int get_stack_size(void);
int get_stack_offset(char *name);
char *get_label(char *name);

char *make_label(void);
void gen(Node *node);
