#ifndef GIFCC_H
#define GIFCC_H

#include "include_path.h"

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>

#define NEW(type) ((type *)calloc(1, sizeof(type)))

typedef struct Vector Vector;
typedef struct Set Set;
typedef struct Map Map;
typedef struct String String;
typedef struct IntVector IntVector;

// トークンの型を表す値
enum {
  TK_NUM = 256,     // 整数トークン
  TK_CHARCONST,     // 文字定数
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
  TK_XOR_ASSIGN,    // `^=`
  TK_OR_ASSIGN,     // `|=`
  TK_ARROW,         // `->`
  TK_ELIPSIS,       // `...`
  TK_HASH_HASH,     // `##`
  TK_VOID,          // `void`
  TK_INT,           // `int`
  TK_SHORT,         // `short`
  TK_LONG,          // `long`
  TK_CHAR,          // `char`
  TK_SIGNED,        // `signed`
  TK_UNSIGNED,      // `unsigned`
  TK_BOOL,          // `_Bool`
  TK_FLOAT,         // `float`
  TK_DOUBLE,        // `double`
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
  TK_ALIGNOF,       // `_Alignof`
  TK_TYPEDEF,       // `typedef`
  TK_EXTERN,        // `extern`
  TK_STATIC,        // `static`
  TK_CONST,         // `const`
  TK_RESTRICT,      // `restrict`
  TK_VOLATILE,      // `volatile`
  TK_INLINE,        // `inline`
  TK_NORETURN,      // `_Noreturn`
  TK_GENERIC,       // `_Generic`
  TK_EOF,           // 入力の終わりを表すトークン
};

typedef struct Range Range;

#define SET_NUMBER_VAL(dest, num)                                              \
  do {                                                                         \
    switch ((num)->type) {                                                     \
    case TY_BOOL:                                                              \
      (dest) = (num)->bool_val;                                                \
      break;                                                                   \
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
    case TY_FLOAT:                                                             \
      (dest) = (num)->float_val;                                               \
      break;                                                                   \
    case TY_DOUBLE:                                                            \
      (dest) = (num)->double_val;                                              \
      break;                                                                   \
    case TY_LDOUBLE:                                                           \
      (dest) = (num)->ldouble_val;                                             \
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
    case TY_BUILTIN:                                                           \
      abort();                                                                 \
    }                                                                          \
  } while (false);

typedef enum {
  TY_VOID,
  TY_BOOL,
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
  TY_FLOAT,
  TY_DOUBLE,
  TY_LDOUBLE,
  TY_PTR,
  TY_ARRAY,
  TY_FUNC,
  TY_STRUCT,
  TY_UNION,
  TY_ENUM,
  TY_BUILTIN,
} type_t;

#define TY_SIZE_T TY_U_LONG
#define TY_PTRDIFF_T TY_S_LONG
#define TY_WCHAR_T TY_S_INT

typedef struct Number {
  type_t type;
  union {
    bool bool_val;
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
    float float_val;
    double double_val;
    long double ldouble_val;
    intptr_t ptr_val;
    int enum_val;
    uint64_t bytes[2];
  };
} Number;

// トークンの型
typedef struct {
  int ty;
  const Range *range;
  Set *pp_hideset;
  const char *num;
  char *ident;
  const char *str;
  Number char_val;
} Token;

typedef enum {
  // primary expression
  EX_NUM, // 整数のノードの型
  EX_STACK_VAR,
  EX_GLOBAL_VAR,
  EX_STR,
  EX_COMPOUND,
  EX_STMT,

  // prefix unary operator
  EX_PRE_INC,
  EX_PRE_DEC,
  EX_ADDRESS,
  EX_INDIRECT,
  EX_PLUS,
  EX_MINUS,
  EX_NOT,
  EX_LOG_NOT,
  EX_CAST,

  // postfix unary operator
  EX_CALL,
  EX_POST_INC,
  EX_POST_DEC,
  EX_DOT,

  // binary operator
  EX_ADD,
  EX_SUB,
  EX_MUL,
  EX_DIV,
  EX_MOD,
  EX_EQEQ,
  EX_NOTEQ,
  EX_LT,
  EX_GT,
  EX_LTEQ,
  EX_GTEQ,
  EX_LSHIFT,
  EX_RSHIFT,
  EX_AND,
  EX_XOR,
  EX_OR,
  EX_LOG_AND,
  EX_LOG_OR,
  EX_ASSIGN,
  EX_MUL_ASSIGN,
  EX_DIV_ASSIGN,
  EX_MOD_ASSIGN,
  EX_ADD_ASSIGN,
  EX_SUB_ASSIGN,
  EX_LSHIFT_ASSIGN,
  EX_RSHIFT_ASSIGN,
  EX_AND_ASSIGN,
  EX_XOR_ASSIGN,
  EX_OR_ASSIGN,
  EX_COMMA,

  // ternary operator
  EX_COND,

  // compiler builtins
  EX_BUILTIN_FUNC,
  EX_BUILTIN_VA_START,
  EX_BUILTIN_VA_ARG,
  EX_BUILTIN_VA_END,
  EX_BUILTIN_VA_COPY,
} expr_t;

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

typedef struct TypeQualifier {
  bool is_const;
  bool is_restrict;
  bool is_volatile;
} TypeQualifier;

typedef struct StorageClassSpecifier {
  bool is_typedef;
  bool is_extern;
  bool is_static;
} StorageClassSpecifier;

typedef struct FunctionSpecifier {
  bool is_inline;
  bool is_noreturn;
} FunctionSpecifier;

typedef struct StructBody {
  int struct_id;
  Map *member_name_map;
  Vector *member_list;
  int member_size;
  int member_align;
  bool has_flex_array;
} StructBody;

typedef struct Type Type;
typedef struct Type {
  type_t ty;
  TypeQualifier qualifier;
  const char *tag;
  union {
    // TY_PTR
    Type *ptr;

    // TY_ARRAY
    struct {
      int len;
      Type *elem;
    } array;

    // TY_FUNC
    struct {
      Type *ret;
      Vector *param;
      bool has_varargs;
    } func;

    // TY_STRUCT/TY_UNION
    StructBody *struct_body;

    // TY_ENUM
    struct {
      int enum_id;
    } enum_body;
  };
} Type;

typedef struct Expr Expr;
typedef struct Initializer {
  Type *type;
  Vector *members;
  Vector *elements;
  Expr *expr;
} Initializer;

typedef struct Member Member;
typedef struct MemberInitializer {
  const Member *member;
  Initializer *init;
} MemberInitializer;

typedef struct StackVar {
  const char *name;
  int offset;
  Type *type;
  const Range *range;
} StackVar;

typedef struct GlobalVar {
  const char *name;
  Type *type;
  const Range *range;
  StorageClassSpecifier storage_class;
  Initializer *init;
} GlobalVar;

typedef struct Param {
  Token *name;
  Type *type;
  const Range *range;
  StackVar *stack_var;
} Param;

typedef struct StringLiteral {
  const char *name;
  const char *val;
} StringLiteral;

typedef struct Scope Scope;
typedef Expr *builtin_func_handler_t(Scope *scope, Expr *callee,
                                     Vector *argument, const Range *range);

typedef struct Stmt Stmt;
typedef struct Expr {
  expr_t ty;
  Type *val_type; // 値の型
  const Range *range;

  union {
    // EX_NUM
    Number num;

    // EX_STACK_VAR
    StackVar *stack_var;

    // EX_GLOBAL_VAR
    struct {
      const char *name;
      GlobalVar *def;
    } global_var;

    // EX_STR
    const StringLiteral *str;

    // EX_COMPOUND
    Initializer *compound;

    // EX_CALL
    struct {
      Expr *callee;
      Vector *argument;
    } call;

    // EX_DOT
    struct {
      Expr *operand;
      Member *member;
    } dot;

    // EX_COND
    struct {
      Expr *cond;
      Expr *then_expr;
      Expr *else_expr;
    } cond;

    // EX_COMMA
    struct {
      Vector *exprs;
    } comma;

    // EX_STMT
    Stmt *stmt;

    // EX_BUILTIN_FUNC
    struct {
      const char *name;
      builtin_func_handler_t *handler;
    } builtin_func;

    // EX_BUILTIN_VA_START
    struct {
      Expr *ap;
      Expr *last;
    } builtin_va_start;

    // EX_BUILTIN_VA_ARG
    struct {
      Expr *ap;
    } builtin_va_arg;

    // EX_BUILTIN_VA_END
    struct {
      Expr *ap;
    } builtin_va_end;

    // EX_BUILTIN_VA_COPY
    struct {
      Expr *dest;
      Expr *src;
    } builtin_va_copy;

    // other unary operator
    struct {
      Expr *operand;
    } unop;

    // other binary operator
    struct {
      Expr *lhs;
      Expr *rhs;
    } binop;
  };
} Expr;

typedef struct Stmt {
  stmt_t ty;
  Type *val_type;
  const Range *range;

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
  int index;
} Member;

typedef struct Function {
  char *name;
  Type *type;
  const Range *range;
  StorageClassSpecifier storage_class;
  FunctionSpecifier func;
  Vector *var_list;
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
typedef void tokenizer_listener_fun_t(void *, const Token *);
typedef struct Scope Scope;

#define error(fmt, ...) error_raw(__FILE__, __LINE__, (fmt), ##__VA_ARGS__)
noreturn __attribute__((format(printf, 3, 4))) void
error_raw(const char *dbg_file, int dbg_line, const char *fmt, ...);
noreturn void error_raw_v(const char *dbg_file, int dbg_line, const char *fmt,
                          va_list ap);

static inline int align(int n, int s) {
  return (s != 0) ? ((n + (s - 1)) / s) * s : n;
}

// vector.c
Vector *new_vector(void);
Vector *vec_clone(const Vector *vec);
int vec_len(const Vector *vec);
void *vec_first(const Vector *vec);
void *vec_last(const Vector *vec);
void *vec_get(const Vector *vec, int n);
void vec_set(Vector *vec, int n, void *val);
void *vec_rget(const Vector *vec, int n);
void vec_push(Vector *vec, void *elem);
void *vec_pop(Vector *vec);
void vec_insert(Vector *vec, int n, void *elem);
void *vec_remove(Vector *vec, int n);
void vec_append(Vector *dst, Vector *src);
void vec_extend(Vector *vec, int len);
void vec_reserve(Vector *vec, int len);

// int_vector.c
IntVector *new_int_vector(void);
int int_vec_len(const IntVector *vec);
int int_vec_get(const IntVector *vec, int n);
void int_vec_push(IntVector *vec, int elem);

// string.c
String *new_string(void);
void str_push(String *str, char elem);
void str_append(String *str, const char *elems);
char *str_get_raw(String *str);

// set.c
Set *new_set(void);
Set *set_clone(const Set *set);
int set_size(const Set *set);
const char *set_get_by_index(Set *set, int n);
void set_insert(Set *set, const char *key);
bool set_contains(const Set *set, const char *key);
Set *set_intersection(const Set *a, const Set *b);

// map.c
Map *new_map(void);
int map_size(const Map *map);
void *map_get_by_index(Map *map, int n, const char **key);
void map_put(Map *map, const char *key, void *val);
void *map_get(Map *map, const char *key);
bool map_remove(Map *map, const char *key);

// file.c
char *replace_suffix(const char *filename, const char *from_suffix,
                     const char *to_suffix);
FILE *open_output_file(const char *filename);
void complete_output_file(void);

// util.c
char *format_string_literal(const char *str);
char *__attribute__((format(printf, 1, 2))) format(const char *fmt, ...);

// util_test.c
void runtest(void);

// reader.c
const Range *range_from_reader(const Reader *reader, int start, int end);
const Range *range_builtin(const Reader *reader);
const Range *range_add_expanded_from(const Range *range,
                                     const Range *expanded_from);
const Range *range_join(const Range *a, const Range *b);
void range_get_start(const Range *range, const char **filename, int *line,
                     int *column);
void range_get_end(const Range *range, const char **filename, int *line,
                   int *column);
Reader *new_reader(void);
void reader_add_file(Reader *reader, FILE *fp, const char *filename);
void reader_set_position(Reader *reader, const int *line, const char *filename);
char reader_peek(const Reader *reader);
void reader_succ(Reader *reader);
void reader_succ_n(Reader *reader, int n);
char reader_pop(Reader *Reader);
bool reader_consume(Reader *reader, char ch);
bool reader_consume_str(Reader *reader, const char *str);
void reader_expect(Reader *reader, char ch);
bool reader_is_sol(const Reader *reader);
int reader_get_offset(const Reader *reader);
void reader_get_position(const Reader *reader, int offset,
                         const char **filename, int *line, int *column);
char *reader_get_source(const Range *range);
#define reader_error_here(reader, fmt, ...)                                    \
  reader_error_offset_raw(reader, reader_get_offset(reader), __FILE__,         \
                          __LINE__, (fmt), ##__VA_ARGS__)
#define reader_error_offset(reader, offset, fmt, ...)                          \
  reader_error_offset_raw((reader), (offset), __FILE__, __LINE__, (fmt),       \
                          ##__VA_ARGS__)
noreturn __attribute__((format(printf, 5, 6))) void
reader_error_offset_raw(const Reader *reader, int offset, const char *dbg_file,
                        int dbg_line, const char *fmt, ...);
#define reader_warn_here(reader, fmt, ...)                                     \
  reader_warn_offset_raw(reader, reader_get_offset(reader), __FILE__,          \
                         __LINE__, (fmt), ##__VA_ARGS__)
#define reader_warn_offset(reader, offset, fmt, ...)                           \
  reader_warn_offset_raw((reader), (offset), __FILE__, __LINE__, (fmt),        \
                         ##__VA_ARGS__)
__attribute__((format(printf, 5, 6))) void
reader_warn_offset_raw(const Reader *reader, int offset, const char *dbg_file,
                       int dbg_line, const char *fmt, ...);

#define range_error(range, fmt, ...)                                           \
  range_error_raw((range), __FILE__, __LINE__, (fmt), ##__VA_ARGS__)
noreturn __attribute__((format(printf, 4, 5))) void
range_error_raw(const Range *range, const char *dbg_file, int dbg_line,
                const char *fmt, ...);
noreturn void range_error_raw_v(const Range *range, const char *dbg_file,
                                int dbg_line, const char *fmt, va_list ap);

#define range_warn(range, fmt, ...)                                            \
  range_warn_raw((range), __FILE__, __LINE__, (fmt), ##__VA_ARGS__)
__attribute__((format(printf, 4, 5))) void range_warn_raw(const Range *range,
                                                          const char *dbg_file,
                                                          int dbg_line,
                                                          const char *fmt, ...);
void range_warn_raw_v(const Range *range, const char *dbg_file, int dbg_line,
                      const char *fmt, va_list ap);

#define range_internal_error(range, fmt, ...)                                  \
  range_error(range, "ICE " fmt, ##__VA_ARGS__)

#define range_assert(range, cond, fmt, ...)                                    \
  do {                                                                         \
    if (!(cond)) {                                                             \
      range_internal_error((range), "`%s` " fmt, #cond, ##__VA_ARGS__);        \
    }                                                                          \
  } while (0)

// number.c
Number new_number(type_t ty, unsigned long long val);
Number new_number_float(type_t ty, long double val);
Number new_number_int(int val);
Number new_number_size_t(size_t val);
Number new_number_ptrdiff_t(ptrdiff_t val);
Number new_number_wchar_t(wchar_t val);
char *format_number(Number num);

// tokenize.c
Tokenizer *new_tokenizer(Reader *reader);
void consume_all_tokens(Tokenizer *tokenizer);
void token_add_listener(Tokenizer *tokenizer, tokenizer_listener_fun_t *fun,
                        void *arg);
void token_succ(Tokenizer *tokenizer);
Token *token_peek(Tokenizer *tokenizer);
Token *token_peek_ahead(Tokenizer *tokenizer, int n);
Token *token_pop(Tokenizer *tokenizer);
Token *token_consume(Tokenizer *tokenizer, int ty);
Token *token_consume2(Tokenizer *tokenizer, int ty1, int ty2);
Token *token_expect(Tokenizer *tokenizer, int ty);
const char *token_kind_to_str(int kind);
const Reader *token_get_reader(const Tokenizer *tokenizer);

// type.c
extern const TypeQualifier EMPTY_TYPE_QUALIFIER;
extern const TypeQualifier CONST_TYPE_QUALIFIER;
Type *new_type(int ty, TypeQualifier tq);
Type *clone_type(Type *type);
Type *to_unqualified(Type *type);
Type *new_type_ptr(Type *base_type, TypeQualifier tq);
Type *new_type_array(Type *base_type, Number len, TypeQualifier tq);
Type *new_type_unsized_array(Type *base_type, TypeQualifier tq);
Type *new_type_func(Type *ret_type, Vector *func_param, bool has_varargs,
                    TypeQualifier tq);
Type *new_type_struct(type_t ty, const char *tag, TypeQualifier tq);
Type *new_type_opaque_struct(type_t ty, const char *tag, TypeQualifier tq);
Type *new_type_enum(const char *tag, TypeQualifier tq);
Type *new_type_builtin_va_list(const Range *range);
void init_struct_body(StructBody *body);
void register_struct_member(Type *type, char *member_name, Type *member_type,
                            const Range *range);
const Member *lookup_struct_member(Type *type, const char *name);
bool is_unqualified_type(const Type *type);
bool is_same_type_qualifier(const TypeQualifier *tq1, const TypeQualifier *tq2);
bool is_sametype(Type *ty1, Type *ty2);
bool is_integer_type(Type *ty);
bool is_float_type(Type *ty);
int get_int_type_rank(Type *ty, const Range *range);
bool is_signed_int_type(Type *ty, const Range *range);
bool is_arith_type(Type *ty);
bool is_ptr_type(Type *ty);
bool is_array_type(Type *ty);
bool is_func_type(Type *ty);
char *format_type(const Type *type, bool detail);

// parse.c
Scope *new_pp_scope(const Tokenizer *tokenizer);
int get_val_size(const Type *ty, const Range *range);
int get_val_align(const Type *ty, const Range *range);
Expr *constant_expression(Tokenizer *tokenizer, Scope *scope);
TranslationUnit *parse(Tokenizer *tokenizer);

// sema.c
void sema_expr(Expr *expr);
void sema(TranslationUnit *tunit);

// codegen.c
char *make_label(const char *s);
void gen(FILE *fp, TranslationUnit *tunit);

// inline functions
static inline char *get_label(Function *func, char *name) {
  return map_get(func->label_map, name);
}

#endif // GIFCC_H
