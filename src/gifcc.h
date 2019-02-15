#pragma once

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdnoreturn.h>

#define NEW(type) ((type *)calloc(1, sizeof(type)))

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
  TK_ARROW,         // `->`
  TK_ELIPSIS,       // `...`
  TK_VOID,          // `void`
  TK_INT,           // `int`
  TK_SHORT,         // `short`
  TK_LONG,          // `long`
  TK_CHAR,          // `char`
  TK_SIGNED,        // `signed`
  TK_UNSIGNED,      // `unsigned`
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
  TK_STRUCT,        // `struct`
  TK_UNION,         // `union`
  TK_ENUM,          // `enum`
  TK_SIZEOF,        // `sizeof`
  TK_TYPEDEF,       // `typedef`
  TK_EXTERN,        // `extern`
  TK_STATIC,        // `static`
  TK_CONST,         // `const`
  TK_EOF,           // 入力の終わりを表すトークン
};

typedef struct Reader Reader;
typedef struct Tokenizer Tokenizer;

typedef struct {
  const Reader *reader;
  int start;
  int len;
} Range;

#define SET_NUMBER_VAL(dest, num)                                              \
  do {                                                                         \
    switch ((num)->type) {                                                     \
    case TY_CHAR:                                                              \
      (dest) = (num)->char_val;                                                \
      break;                                                                   \
    case TY_S_CHAR:                                                            \
      (dest) = (num)->s_char_val;                                              \
      break;                                                                   \
    case TY_S_SHORT:                                                           \
      (dest) = (num)->s_short_val;                                             \
      break;                                                                   \
    case TY_S_INT:                                                             \
      (dest) = (num)->s_int_val;                                               \
      break;                                                                   \
    case TY_S_LONG:                                                            \
      (dest) = (num)->s_long_val;                                              \
      break;                                                                   \
    case TY_S_LLONG:                                                           \
      (dest) = (num)->s_llong_val;                                             \
      break;                                                                   \
    case TY_U_CHAR:                                                            \
      (dest) = (num)->u_char_val;                                              \
      break;                                                                   \
    case TY_U_SHORT:                                                           \
      (dest) = (num)->u_short_val;                                             \
      break;                                                                   \
    case TY_U_INT:                                                             \
      (dest) = (num)->u_int_val;                                               \
      break;                                                                   \
    case TY_U_LONG:                                                            \
      (dest) = (num)->u_long_val;                                              \
      break;                                                                   \
    case TY_U_LLONG:                                                           \
      (dest) = (num)->u_llong_val;                                             \
      break;                                                                   \
    case TY_PTR:                                                               \
      (dest) = (num)->ptr_val;                                                 \
      break;                                                                   \
    case TY_ENUM:                                                              \
      (dest) = (num)->enum_val;                                                \
      break;                                                                   \
    case TY_VOID:                                                              \
    case TY_ARRAY:                                                             \
    case TY_FUNC:                                                              \
    case TY_STRUCT:                                                            \
    case TY_UNION:                                                             \
      abort();                                                                 \
    }                                                                          \
  } while (false);

typedef enum {
  TY_VOID,
  TY_CHAR,
  TY_S_CHAR,
  TY_S_SHORT,
  TY_S_INT,
  TY_S_LONG,
  TY_S_LLONG,
  TY_U_CHAR,
  TY_U_SHORT,
  TY_U_INT,
  TY_U_LONG,
  TY_U_LLONG,
  TY_PTR,
  TY_ARRAY,
  TY_FUNC,
  TY_STRUCT,
  TY_UNION,
  TY_ENUM,
} type_t;

typedef struct Number {
  type_t type;
  union {
    char char_val;
    signed char s_char_val;
    signed short s_short_val;
    signed int s_int_val;
    signed long s_long_val;
    signed long long s_llong_val;
    unsigned char u_char_val;
    unsigned short u_short_val;
    unsigned int u_int_val;
    unsigned long u_long_val;
    unsigned long long u_llong_val;
    intptr_t ptr_val;
    int enum_val;
  };
} Number;

// トークンの型
typedef struct {
  int ty;         // トークンの型
  Number num_val; // tyがTK_NUMの場合、その数値
  char *name;     // tyがTK_IDENTの場合、その名前
  char *str;      // tyがTK_STRの場合、その値
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

typedef struct Type {
  type_t ty;
  bool is_const;
  struct Type *ptrof;
  int array_len;
  struct Type *func_ret;
  Vector *func_param;
  bool func_has_varargs;
  char *tag;
  Map *member_name_map;
  Vector *member_list;
  int member_size;
  int member_align;
} Type;

typedef struct Member Member;
typedef struct Expr Expr;
typedef struct Initializer {
  Type *type;
  Map *members;
  Vector *elements;
  Expr *expr;
} Initializer;

typedef struct StackVar {
  int offset;
  Type *type;
  Range range;
} StackVar;

typedef struct GlobalVar {
  char *name;
  Type *type;
  Range range;
  bool is_static;
  Initializer *init;
} GlobalVar;

typedef struct Param {
  Token *name;
  Type *type;
  Range range;
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

  Number num_val;  // tyがEX_NUMの場合のみ使う
  char *name;      // tyがEX_IDENT, EX_STR, `.` の場合のみ使う
  int incdec_size; // tyがEX_INC, EX_DECの場合のみ使う

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
  Range range;

  // ST_LABEL, ST_GOTO
  char *name;

  // ST_IF:       if (<cond>) <then_stmt> else <else_stmt>
  // ST_SWITCH:   switch (<cond>) <body>
  // ST_WHILE:    while (<cond>) <body>
  // ST_DO_WHILE: do <body> while(<cond>);
  // ST_FOR:      for (<init>; <cond>; <inc>) <body>
  // ST_CASE:     case: <body>
  // ST_DEFAULT:  default: <body>
  // ST_LABEL:    <label>: <body>
  char *label;
  struct Expr *init;
  struct Expr *cond;
  struct Expr *inc;
  struct Stmt *then_stmt;
  struct Stmt *else_stmt;
  struct Stmt *body;

  // ST_SWITCH
  Vector *cases;
  struct Stmt *default_case;

  // ST_EXPR:   <expr>;
  // ST_CASE:   case <expr>:
  // ST_RETURN: return <expr>:
  struct Expr *expr;

  Vector *stmts; // tyがST_COMPOUNDの場合のみ使う
} Stmt;

typedef struct Member {
  char *name;
  Type *type;
  int offset;
} Member;

typedef struct Function {
  char *name;
  Type *type;
  Range range;
  bool is_static;
  int stack_size;
  Map *label_map;
  Stmt *body;
} Function;

typedef struct TranslationUnit {
  Vector *func_list;
  Vector *gvar_list;
  Vector *str_list;
} TranslationUnit;

#define error(fmt, ...) error_raw(__FILE__, __LINE__, (fmt), ##__VA_ARGS__)
noreturn __attribute__((format(printf, 3, 4))) void
error_raw(const char *dbg_file, int dbg_line, const char *fmt, ...);
noreturn void error_raw_v(const char *dbg_file, int dbg_line, const char *fmt,
                          va_list ap);

static inline int align(int n, int s) {
  return (s != 0) ? ((n + (s - 1)) / s) * s : 0;
}

Vector *new_vector(void);
void vec_push(Vector *vec, void *elem);
void *vec_pop(Vector *vec);
void *vec_peek(Vector *vec);
void vec_extend(Vector *vec, int len);
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
void reader_get_position(const Reader *reader, int offset,
                         const char **filename, int *line, int *column);
#define reader_error_here(reader, fmt, ...)                                    \
  reader_error_offset_raw(reader, reader_get_offset(reader), __FILE__,         \
                          __LINE__, (fmt), ##__VA_ARGS__)
#define reader_error_offset(reader, offset, fmt, ...)                          \
  reader_error_offset_raw((reader), (offset), __FILE__, __LINE__, (fmt),       \
                          ##__VA_ARGS__)
noreturn __attribute__((format(printf, 5, 6))) void
reader_error_offset_raw(const Reader *reader, int offset, const char *dbg_file,
                        int dbg_line, const char *fmt, ...);

#define range_error(range, fmt, ...)                                           \
  range_error_raw((range), __FILE__, __LINE__, (fmt), ##__VA_ARGS__)
noreturn __attribute__((format(printf, 4, 5))) void
range_error_raw(Range range, const char *dbg_file, int dbg_line,
                const char *fmt, ...);
noreturn void range_error_raw_v(Range range, const char *dbg_file, int dbg_line,
                                const char *fmt, va_list ap);

#define range_warn(range, fmt, ...)                                            \
  range_warn_raw((range), __FILE__, __LINE__, (fmt), ##__VA_ARGS__)
__attribute__((format(printf, 4, 5))) void range_warn_raw(Range range,
                                                          const char *dbg_file,
                                                          int dbg_line,
                                                          const char *fmt, ...);
void range_warn_raw_v(Range range, const char *dbg_file, int dbg_line,
                      const char *fmt, va_list ap);

Tokenizer *new_tokenizer(Reader *reader);
void token_succ(Tokenizer *tokenizer);
Token *token_peek(Tokenizer *tokenizer);
Token *token_peek_ahead(Tokenizer *tokenizer, int n);
Token *token_pop(Tokenizer *tokenizer);
Token *token_consume(Tokenizer *tokenizer, int ty);
Token *token_consume2(Tokenizer *tokenizer, int ty1, int ty2);
Token *token_expect(Tokenizer *tokenizer, int ty);
const char *token_kind_to_str(int kind);
const Reader *token_get_reader(const Tokenizer *tokenizer);

bool is_signed_int_type(Type *ty, Range range);
int get_val_size(Type *ty, Range range);
int get_val_align(Type *ty, Range range);
TranslationUnit *parse(Reader *reader);

char *make_label(const char *s);
void gen(TranslationUnit *tunit);

static inline Range range_join(Range a, Range b) {
  assert(a.start >= 0);
  assert(b.start >= 0);
  assert(a.reader == b.reader);
  int aend = a.start + a.len;
  int bend = b.start + b.len;
  int start = a.start < b.start ? a.start : b.start;
  int end = aend > bend ? aend : bend;
  return (Range){.reader = a.reader, .start = start, .len = (end - start)};
}

static inline char *get_label(Function *func, char *name) {
  return map_get(func->label_map, name);
}
