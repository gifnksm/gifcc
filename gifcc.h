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
  TK_VOID,          // `void`
  TK_INT,           // `int`
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
  TK_RETURN,        // `return`
  TK_EOF,           // 入力の終わりを表すトークン
};

// トークンの型
typedef struct {
  int ty;            // トークンの型
  int val;           // tyがTK_NUMの場合、その数値
  char *name;        // tyがTK_IDENTの場合、その名前
  const char *input; // トークン文字列 (エラーメッセージ用)
} Token;

enum {
  EX_NUM = 256, // 整数のノードの型
  EX_IDENT,
  EX_EQEQ,
  EX_NOTEQ,
  EX_LTEQ,
  EX_GTEQ,
  EX_LSHIFT,
  EX_RSHIFT,
  EX_LOGAND,
  EX_LOGOR,
  EX_COND,
  EX_INC,
  EX_DEC,
  EX_MUL_ASSIGN,
  EX_DIV_ASSIGN,
  EX_MOD_ASSIGN,
  EX_ADD_ASSIGN,
  EX_SUB_ASSIGN,
  EX_LSHIFT_ASSIGN,
  EX_RSHIFT_ASSIGN,
  EX_AND_ASSIGN,
  EX_OR_ASSIGN,
  EX_XOR_ASSIGN,
  EX_CALL,
  EX_CAST,
};

enum {
  ST_EXPR,
  ST_COMPOUND,
  ST_IF,
  ST_SWITCH,
  ST_CASE,
  ST_DEFAULT,
  ST_LABEL,
  ST_WHILE,
  ST_DO_WHILE,
  ST_FOR,
  ST_GOTO,
  ST_BREAK,
  ST_CONTINUE,
  ST_RETURN,
  ST_NULL,
};

typedef struct Type {
  enum { TY_INT, TY_PTR } ty;
  struct Type *ptrof;
} Type;

typedef struct StackVar {
  int offset;
  Type *type;
} StackVar;

typedef struct Expr {
  int ty;         // ノードの型
  Type *val_type; // 値の型

  struct Expr *lhs;  // 左辺
  struct Expr *rhs;  // 右辺
  struct Expr *cond; // 条件式 (tyがEX_CONDの場合のみ使う)

  int val;    // tyがEX_NUMの場合のみ使う
  char *name; // tyがEX_IDENTの場合のみ使う

  // EX_CALL: <callee>(<argument>...)
  struct Expr *callee;
  Vector *argument;

  // EX_CAST: (<val_type>)<expr>
  struct Expr *expr;
} Expr;

typedef struct Stmt {
  int ty;

  // ST_LABEL, ST_GOTO
  char *name;

  // ST_IF:       if (<cond>) <then_stmt> else <else_stmt>
  // ST_SWITCH:   switch (<cond>) <body>
  // ST_WHILE:    while (<cond>) <body>
  // ST_DO_WHILE: do <body> while(<cond>);
  // ST_FOR:      for (<init>; <cond>; <inc>) <body>
  struct Expr *init;
  struct Expr *cond;
  struct Expr *inc;
  struct Stmt *then_stmt;
  struct Stmt *else_stmt;
  struct Stmt *body;

  // ST_SWITCH
  Vector *cases;
  struct Stmt *default_case;

  // ST_CASE, ST_DEFAULT, ST_LABEL
  char *label;

  // ST_EXPR:   <expr>;
  // ST_CASE:   case <expr>:
  // ST_RETURN: return <expr>:
  struct Expr *expr;

  Vector *stmts; // tyがST_COMPOUNDの場合のみ使う
} Stmt;

typedef struct Function {
  char *name;
  int stack_size;
  Map *stack_map;
  Map *label_map;
  Vector *params;
  Stmt *body;
} Function;

typedef struct Tokenizer Tokenizer;

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

Tokenizer *new_tokenizer(const char *input);
void token_succ(Tokenizer *tokenizer);
Token *token_peek(Tokenizer *tokenizer);
Token *token_peek_ahead(Tokenizer *tokenizer, int n);
Token *token_pop(Tokenizer *tokenizer);
Token *token_consume(Tokenizer *tokenizer, int ty);
Token *token_expect(Tokenizer *tokenizer, int ty);

Vector *parse(const char *input);

char *make_label(void);
void gen(Function *func);

static inline StackVar *get_stack_variable(Function *func, char *name) {
  return map_get(func->stack_map, name);
}

static inline char *get_label(Function *func, char *name) {
  return map_get(func->label_map, name);
}
