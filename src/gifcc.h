#pragma once

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdnoreturn.h>

typedef struct {
  void **data;
  int capacity;
  int len;
} Vector;

typedef struct {
  Vector *keys;
  Vector *vals;
} Map;

typedef struct {
  char *data;
  int capacity;
  int len;
} String;

typedef struct {
  int *data;
  int capacity;
  int len;
} IntVector;

// トークンの型を表す値
enum {
  TK_NUM = 256,     // 整数トークン
  TK_IDENT,         // 識別子
  TK_STR,           // 文字列リテラル
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
  TK_CHAR,          // `char`
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

typedef struct {
  int start;
  int len;
} Range;

// トークンの型
typedef struct {
  int ty;     // トークンの型
  int val;    // tyがTK_NUMの場合、その数値
  char *name; // tyがTK_IDENTの場合、その名前
  char *str;  // tyがTK_STRの場合、その値
  Range range;
} Token;

enum {
  EX_NUM = 256, // 整数のノードの型
  EX_STACK_VAR,
  EX_GLOBAL_VAR,
  EX_STR,
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
  EX_CALL,
  EX_CAST,
};

typedef enum {
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
} stmt_t;

typedef enum {
  TY_VOID,
  TY_INT,
  TY_CHAR,
  TY_PTR,
  TY_ARRAY,
  TY_FUNC,
} type_t;

typedef struct Type {
  type_t ty;
  struct Type *ptrof;
  int array_len;
  struct Type *func_ret;
  Vector *func_param;
} Type;

typedef struct StackVar {
  int offset;
  Type *type;
} StackVar;

typedef struct GlobalVar {
  char *name;
  Type *type;
} GlobalVar;

typedef struct Param {
  char *name;
  Type *type;
  StackVar *stack_var;
} Param;

typedef struct StringLiteral {
  char *name;
  char *val;
} StringLiteral;

typedef struct Expr {
  int ty;         // ノードの型
  Type *val_type; // 値の型
  Range range;    // ソースコード中の位置

  struct Expr *lhs;  // 左辺
  struct Expr *rhs;  // 右辺
  struct Expr *cond; // 条件式 (tyがEX_CONDの場合のみ使う)

  int val;    // tyがEX_NUMの場合のみ使う
  char *name; // tyがEX_IDENT, EX_STRの場合のみ使う

  // EX_CALL: <callee>(<argument>...)
  struct Expr *callee;
  Vector *argument;

  StackVar *stack_var;
  GlobalVar *global_var;

  // EX_CAST: (<val_type>)<expr>
  struct Expr *expr;
} Expr;

typedef struct Stmt {
  stmt_t ty;

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
  Type *type;
  int stack_size;
  Map *label_map;
  Stmt *body;
} Function;

typedef struct TranslationUnit {
  Vector *func_list;
  Vector *gvar_list;
  Vector *str_list;
} TranslationUnit;

typedef struct Reader Reader;
typedef struct Tokenizer Tokenizer;

#define error(fmt, ...) error_raw(__FILE__, __LINE__, fmt, ##__VA_ARGS__)
noreturn __attribute__((format(printf, 3, 4))) void
error_raw(const char *dbg_file, int dbg_line, char *fmt, ...);

static inline int align(int n, int s) { return ((n + (s - 1)) / s) * s; }

Vector *new_vector(void);
void vec_push(Vector *vec, void *elem);
void *vec_pop(Vector *vec);
Map *new_map(void);
void map_put(Map *map, char *key, void *val);
void *map_get(Map *map, char *key);
String *new_string(void);
void str_push(String *str, char elem);
void print_string_literal(char *str);
IntVector *new_int_vector(void);
void int_vec_push(IntVector *vec, int elem);
void runtest(void);

Reader *new_reader(FILE *file, const char *filename);
char reader_peek(const Reader *reader);
char reader_peek_ahead(const Reader *reader, int n);
void reader_succ(Reader *reader);
void reader_succ_n(Reader *reader, int n);
char reader_pop(Reader *Reader);
bool reader_consume(Reader *reader, char ch);
bool reader_consume_str(Reader *reader, const char *str);
void reader_expect(Reader *reader, char ch);
int reader_get_offset(const Reader *reader);
const char *reader_get_filename(const Reader *reader);
void reader_get_position(const Reader *reader, int offset, int *line,
                         int *column);
const char *reader_get_source(const Reader *reader, Range range);
#define reader_error(reader, fmt, ...)                                         \
  reader_error_with_raw(reader, reader_get_offset(reader), __FILE__, __LINE__, \
                        fmt, ##__VA_ARGS__)
#define reader_error_with(reader, offset, fmt, ...)                            \
  reader_error_with_raw(reader, offset, __FILE__, __LINE__, fmt, ##__VA_ARGS__)
noreturn __attribute__((format(printf, 5, 6))) void
reader_error_with_raw(const Reader *reader, int offset, const char *dbg_file,
                      int dbg_line, char *fmt, ...);

Tokenizer *new_tokenizer(Reader *reader);
void token_succ(Tokenizer *tokenizer);
Token *token_peek(Tokenizer *tokenizer);
Token *token_peek_ahead(Tokenizer *tokenizer, int n);
Token *token_pop(Tokenizer *tokenizer);
Token *token_consume(Tokenizer *tokenizer, int ty);
bool token_consume2(Tokenizer *tokenizer, int ty1, int ty2);
Token *token_expect(Tokenizer *tokenizer, int ty);
const char *token_kind_to_str(int kind);
#define token_error(tokenizer, fmt, ...)                                       \
  token_error_with_raw(tokenizer, token_peek(tokenizer), __FILE__, __LINE__,   \
                       fmt, ##__VA_ARGS__)
#define token_error_with(tokenizer, token, fmt, ...)                           \
  token_error_with_raw(tokenizer, token, __FILE__, __LINE__, fmt, ##__VA_ARGS__)
noreturn __attribute__((format(printf, 5, 6))) void
token_error_with_raw(const Tokenizer *tokenizer, Token *token,
                     const char *dbg_file, int dbg_line, char *fmt, ...);

int get_val_size(Type *ty);
int get_val_align(Type *ty);
TranslationUnit *parse(Reader *reader);

char *make_label(void);
void gen(TranslationUnit *tunit);

static inline Range range_join(Range a, Range b) {
  assert(a.start > 0);
  assert(b.start >= 0);
  int aend = a.start + a.len;
  int bend = b.start + b.len;
  int start = a.start < b.start ? a.start : b.start;
  int end = aend > bend ? aend : bend;
  return (Range){.start = start, .len = (end - start)};
}

static inline char *get_label(Function *func, char *name) {
  return map_get(func->label_map, name);
}
