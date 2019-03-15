#include "gifcc.h"
#include <assert.h>
#include <limits.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

typedef struct GlobalCtxt {
  Vector *func_list;
  Vector *gvar_list;
  Vector *str_list;
} GlobalCtxt;

typedef struct FuncCtxt {
  char *name;
  Type *type;
  Vector *var_list;
  Vector *switches;
  Map *label_map;
} FuncCtxt;

typedef enum {
  DECL_STACK_VAR,
  DECL_GLOBAL_VAR,
  DECL_NUMBER,
  DECL_BUILTIN_FUNC,
} decl_t;

typedef struct Decl {
  decl_t kind;
  Type *type;
  StackVar *stack_var;
  GlobalVar *global_var;
  Number *num_val;
  builtin_func_handler_t *builtin_func;
} Decl;

typedef struct Scope {
  Map *decl_map;
  Map *typedef_map;
  Map *tag_map;

  GlobalCtxt *global_ctxt;
  FuncCtxt *func_ctxt;
  struct Scope *outer;
} Scope;

typedef enum { DEF_GLOBAL_VAR, DEF_STACK_VAR, DEF_FUNC } def_type_t;

typedef struct VarDef {
  def_type_t type;
  Token *name;
  Initializer *init;
  StackVar *stack_var;
  GlobalVar *global_var;
  Function *func;
} VarDef;

typedef struct TypeSpecifier {
  const Range *range;
  enum {
    BASE_TYPE_UNSPECIFIED,
    BASE_TYPE_CHAR,
    BASE_TYPE_INT,
    BASE_TYPE_DOUBLE,
  } base_type;
  enum { SIGN_UNSPECIFIED, SIGN_SIGNED, SIGN_UNSIGNED } signedness;
  enum { SIZE_UNSPECIFIED, SIZE_SHORT, SIZE_LONG, SIZE_LLONG } size;
  Type *concrete_type;
} TypeSpecifier;

typedef enum {
  CONCRETE_DECLARATOR,
  ABSTRACT_DECLARATOR,
  ANY_DECLARATOR,
} declarator_type_t;

static const StorageClassSpecifier EMPTY_STORAGE_CLASS_SPECIFIER = {};
static const TypeSpecifier EMPTY_TYPE_SPECIFIER = {};
static const FunctionSpecifier EMPTY_FUNCTION_SPECIFIER = {};

static Initializer *new_initializer(Type *type);
static GlobalCtxt *new_global_ctxt(void);
static FuncCtxt *new_func_ctxt(char *name, Type *type);
static Scope *new_scope(GlobalCtxt *gcx, FuncCtxt *fcx, Scope *outer);
static Scope *new_global_scope(GlobalCtxt *gcx, const Tokenizer *tokenizer);
static Scope *new_func_scope(Scope *global, FuncCtxt *fcx);
static Scope *new_inner_scope(Scope *outer);
static VarDef *register_var(Scope *scope, Token *name, Type *type,
                            const Range *range, StorageClassSpecifier scs);
static VarDef *register_func(Scope *scope, Token *name, Type *type);
static void register_extern(Scope *scope, Token *name, Type *type);
static StackVar *register_stack_var(Scope *scope, Token *name, Type *type,
                                    const Range *range);
static GlobalVar *register_global_var(Scope *scope, Token *name, Type *type,
                                      const Range *range,
                                      StorageClassSpecifier scs);
static void register_number(Scope *scope, Token *name, Type *type,
                            Number *number);
static void register_builtin_func(Scope *scope, const char *name,
                                  builtin_func_handler_t *handler);
static bool register_decl(Scope *scope, decl_t kind, const char *name,
                          Type *type, StackVar *svar, GlobalVar *gvar,
                          Number *num_val, builtin_func_handler_t *handler);
static Decl *get_decl(Scope *scope, const char *name);
static bool register_tag(Scope *scope, char *tag, Type *type);
static Type *get_tag(Scope *scope, char *tag);
static bool register_typedef(Scope *scope, char *name, Type *type);
static Type *get_typedef(Scope *scope, char *name);
static Type *integer_promoted(Scope *scope, Expr **e);
static Type *arith_converted(Scope *scope, Expr **e1, Expr **e2);
static bool token_is_storage_class_specifier(Token *token);
static bool consume_storage_class_specifier(Tokenizer *tokenizer,
                                            StorageClassSpecifier *scs);
static bool token_is_type_name(Scope *scope, Token *token);
static bool token_is_declaration_specifiers(Scope *scope, Token *token);
static bool token_is_type_specifier(Scope *scope, Token *token);
static Type *construct_type_specifier(TypeSpecifier ts, TypeQualifier tq);
static bool token_is_type_qualifier(Token *token);
static bool consume_type_qualifier(Tokenizer *tokenizer, TypeQualifier *tq);
static bool token_is_function_specifier(Token *token);
static bool consume_function_specifier(Tokenizer *tokenizer,
                                       FunctionSpecifier *fs);
static noreturn void binop_type_error_raw(int ty, Expr *lhs, Expr *rhs,
                                          const char *dbg_file, int dbg_line);
static Expr *coerce_array2ptr(Scope *scope, Expr *expr);
static Expr *coerce_func2ptr(Scope *scope, Expr *expr);
static bool is_null_ptr_const(Expr *expr);
static Expr *new_expr(int ty, Type *val_type, const Range *range);
static Expr *new_expr_num(Number val, const Range *range);
static Expr *new_expr_ident(Scope *scope, Token *ident);
static Expr *new_expr_str(Scope *scope, const char *val, const Range *range);
static Expr *new_expr_call(Scope *scope, Expr *callee, Vector *argument,
                           const Range *range);
static Expr *new_expr_builtin_va_start(Scope *scope, Expr *callee,
                                       Vector *argument, const Range *range);
static Expr *new_expr_builtin_va_arg(Scope *scope, Expr *callee,
                                     Vector *argument, const Range *range);
static Expr *new_expr_builtin_va_end(Scope *scope, Expr *callee,
                                     Vector *argument, const Range *range);
static Expr *new_expr_builtin_va_copy(Scope *scope, Expr *callee,
                                      Vector *argument, const Range *range);
static Expr *new_expr_postfix(Scope *scope, int ty, Expr *operand,
                              const Range *range);
static Expr *new_expr_cast(Scope *scope, Type *val_type, Expr *operand,
                           const Range *range);
static Expr *new_expr_compound(Scope *scope, Type *val_type, Initializer *init,
                               const Range *range);
static Expr *new_expr_unary(Scope *scope, int op, Expr *operand,
                            const Range *range);
static Expr *new_expr_binop(Scope *scope, int op, Expr *lhs, Expr *rhs,
                            const Range *range);
static Expr *new_expr_cond(Scope *scope, Expr *cond, Expr *then_expr,
                           Expr *else_expr, const Range *range);
static Expr *new_expr_index(Scope *scope, Expr *array, Expr *index,
                            const Range *range);
static Expr *new_expr_dot(Expr *operand, const char *name, const Range *range);
static Expr *new_expr_arrow(Scope *scope, Expr *operand, const char *name,
                            const Range *range);
static Stmt *new_stmt(int ty, const Range *range);
static Stmt *new_stmt_expr(Expr *expr, const Range *range);
static Stmt *new_stmt_if(Expr *cond, Stmt *then_stmt, Stmt *else_stmt,
                         const Range *range);
static Stmt *new_stmt_switch(Expr *cond, Stmt *body, const Range *range);
static Stmt *new_stmt_case(Expr *expr, Stmt *body, const Range *range);
static Stmt *new_stmt_default(Stmt *body, const Range *range);
static Stmt *new_stmt_label(FuncCtxt *fcx, char *name, Stmt *body,
                            const Range *range);
static Stmt *new_stmt_while(Expr *cond, Stmt *body, const Range *range);
static Stmt *new_stmt_do_while(Expr *cond, Stmt *body, const Range *range);
static Stmt *new_stmt_for(Expr *init, Expr *cond, Expr *inc, Stmt *body,
                          const Range *range);
static Stmt *new_stmt_goto(char *name, const Range *range);
static Stmt *new_stmt_return(Scope *scope, Expr *expr, const Range *range);
static Stmt *new_stmt_compound(Vector *stmts, const Range *range);

static Expr *builtin_va_start_handler(Scope *scope, Expr *callee,
                                      Vector *argument, const Range *range);
static Expr *builtin_va_arg_handler(Scope *scope, Expr *callee,
                                    Vector *argument, const Range *range);
static Expr *builtin_va_end_handler(Scope *scope, Expr *callee,
                                    Vector *argument, const Range *range);
static Expr *builtin_va_copy_handler(Scope *scope, Expr *callee,
                                     Vector *argument, const Range *range);

// expression
static Number read_integer(Token *token);
static Number read_float(Token *token);
static Number read_number(Token *token);
static Expr *primary_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *postfix_expression(Tokenizer *tokenizer, Scope *scope);
static Vector *argument_expression_list(Tokenizer *tokenizer, Scope *scope);
static Expr *unary_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *cast_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *binary_expression(Tokenizer *tokenizer, Scope *scope,
                               const int *tks, const int *exs,
                               Expr *(*op_parser)(Tokenizer *, Scope *));
static Expr *multiplicative_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *additive_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *shift_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *relational_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *equality_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *and_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *exclusive_or_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *inclusive_or_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *logical_and_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *logical_or_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *conditional_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *assignment_expression(Tokenizer *tokenizer, Scope *scope);
static Expr *expression(Tokenizer *tokenizer, Scope *scope);

// declaration
static Vector *declaration(Tokenizer *tokenizer, Scope *scope);
static void declaration_specifiers(Tokenizer *tokenizer, Scope *scope,
                                   Type **type, StorageClassSpecifier *scs,
                                   FunctionSpecifier *fs);
static Type *struct_or_union_specifier(Scope *scope, Tokenizer *tokenizer,
                                       Token *token);
static void struct_declaration(Scope *scope, Tokenizer *tokenizer, Type *type);
static Type *specifier_qualifier_list(Scope *scope, Tokenizer *tokenizer);
static Type *enum_specifier(Scope *scope, Tokenizer *tokenizer, Token *token);
static void enumerator(Scope *scope, Tokenizer *tokenizer, Type *type,
                       int *val);
static void declarator_common(Scope *scope, Tokenizer *tokenizer,
                              declarator_type_t dec_type, Type *base_type,
                              Token **name, Type **type, const Range **range);
static void direct_declarator_common(Scope *scope, Tokenizer *tokenizer,
                                     declarator_type_t dec_type,
                                     Type *base_type, Token **name, Type **type,
                                     const Range **range);
static void declarator(Scope *scope, Tokenizer *tokenizer, Type *base_type,
                       Token **name, Type **type, const Range **range);
static Param *parameter_declaration(Scope *scope, Tokenizer *tokenizer);
static Param *parameter_declarator(Scope *scope, Tokenizer *tokenizer,
                                   Type *base_type);
static Type *type_name(Scope *scope, Tokenizer *tokenizer);
static void abstract_declarator(Scope *scope, Tokenizer *tokenizer,
                                Type *base_type, Type **type,
                                const Range **range);
static void struct_initializer(Tokenizer *tokenizer, Scope *scope, Type *type,
                               bool brace_root, Initializer **init);
static void union_initializer(Tokenizer *tokenizer, Scope *scope, Type *type,
                              bool brace_root, Initializer **init);
static void array_initializer(Tokenizer *tokenizer, Scope *scope, Type *type,
                              bool brace_root, Initializer **init);
static void initializer_inner(Tokenizer *tokenizer, Scope *scope, Type *type,
                              Initializer **init, const Range **range);
static void initializer(Tokenizer *tokenizer, Scope *scope, Type *type,
                        Initializer **init, const Range **range);

// statement
static Stmt *statement(Tokenizer *tokenizer, Scope *scope);
static void gen_init(Scope *scope, Expr **expr, Initializer *init, Expr *dest,
                     Type *type);
static Stmt *compound_statement(Tokenizer *tokenizer, Scope *scope);

// top-level
static Function *function_definition(Tokenizer *tokenizer, Scope *global_scope,
                                     Type *type, char *name,
                                     StorageClassSpecifier scs,
                                     FunctionSpecifier fs, const Range *start);
static GlobalVar *new_global_variable(Type *type, const char *name,
                                      const Range *range,
                                      StorageClassSpecifier scs);
static TranslationUnit *translation_unit(Tokenizer *tokenizer);

TranslationUnit *parse(Reader *reader) {
  Tokenizer *tokenizer = new_tokenizer(reader);
  return translation_unit(tokenizer);
}

static Initializer *new_initializer(Type *type) {
  Initializer *init = NEW(Initializer);
  init->type = type;
  init->members = NULL;
  init->elements = NULL;
  switch (type->ty) {
  case TY_STRUCT: {
    init->members = new_map();
    StructBody *body = type->struct_body;
    for (int i = 0; i < vec_len(body->member_list); i++) {
      Member *member = vec_get(body->member_list, i);
      map_put(init->members, member->name, NULL);
    }
    break;
  }
  case TY_UNION: {
    init->members = new_map();
    StructBody *body = type->struct_body;
    if (vec_len(body->member_list) > 0) {
      map_put(init->members, NULL, NULL);
    }
    break;
  }
  case TY_ARRAY:
    init->elements = new_vector();
    vec_extend(init->elements, type->array_len);
    break;
  default:
    break;
  }
  return init;
}

static GlobalCtxt *new_global_ctxt(void) {
  GlobalCtxt *gcx = NEW(GlobalCtxt);
  gcx->func_list = new_vector();
  gcx->gvar_list = new_vector();
  gcx->str_list = new_vector();

  return gcx;
}

static FuncCtxt *new_func_ctxt(char *name, Type *type) {
  FuncCtxt *fcx = NEW(FuncCtxt);

  fcx->name = name;
  fcx->type = type;
  fcx->var_list = new_vector();
  fcx->switches = new_vector();
  fcx->label_map = new_map();

  return fcx;
}

static Scope *new_scope(GlobalCtxt *gcx, FuncCtxt *fcx, Scope *outer) {
  Scope *scope = NEW(Scope);
  scope->decl_map = new_map();
  scope->typedef_map = new_map();
  scope->tag_map = new_map();

  scope->global_ctxt = gcx;
  scope->func_ctxt = fcx;
  scope->outer = outer;
  return scope;
}

static Scope *new_global_scope(GlobalCtxt *gcx, const Tokenizer *tokenizer) {
  Scope *scope = new_scope(gcx, NULL, NULL);

  const Reader *reader = token_get_reader(tokenizer);
  const Range *range = range_builtin(reader);

  register_typedef(scope, "__builtin_va_list", new_type_builtin_va_list(range));

  register_builtin_func(scope, "__builtin_va_start", builtin_va_start_handler);
  register_builtin_func(scope, "__builtin_va_arg", builtin_va_arg_handler);
  register_builtin_func(scope, "__builtin_va_end", builtin_va_end_handler);
  register_builtin_func(scope, "__builtin_va_copy", builtin_va_copy_handler);

  return scope;
}

static Scope *new_func_scope(Scope *global, FuncCtxt *fcx) {
  return new_scope(global->global_ctxt, fcx, global);
}

static Scope *new_inner_scope(Scope *outer) {
  return new_scope(outer->global_ctxt, outer->func_ctxt, outer);
}

Scope *new_pp_scope(const Tokenizer *tokenizer) {
  GlobalCtxt *gcx = new_global_ctxt();
  return new_global_scope(gcx, tokenizer);
}

static VarDef *register_var(Scope *scope, Token *name, Type *type,
                            const Range *range, StorageClassSpecifier scs) {
  def_type_t ty;
  StackVar *svar = NULL;
  GlobalVar *gvar = NULL;
  if (scope->func_ctxt != NULL) {
    if (!scs.is_static) {
      ty = DEF_STACK_VAR;
      svar = register_stack_var(scope, name, type, range);
    } else {
      char buf[256];
      sprintf(buf, "%s.%s", scope->func_ctxt->name, name->ident);
      ty = DEF_GLOBAL_VAR;
      gvar = register_global_var(scope, name, type, range, scs);
      gvar->name = make_label(buf);
    }
  } else {
    ty = DEF_GLOBAL_VAR;
    gvar = register_global_var(scope, name, type, range, scs);
  }

  VarDef *def = NEW(VarDef);
  def->type = ty;
  def->name = name;
  def->init = NULL;
  def->stack_var = svar;
  def->global_var = gvar;
  def->func = NULL;
  return def;
}

static VarDef *register_func(Scope *scope, Token *name, Type *type) {
  (void)register_decl(scope, DECL_GLOBAL_VAR, name->ident, type, NULL, NULL,
                      NULL, NULL);

  VarDef *def = NEW(VarDef);
  def->type = DEF_FUNC;
  def->name = name;
  def->init = NULL;
  def->stack_var = NULL;
  def->global_var = NULL;
  def->func = NULL;

  return def;
}

static void register_extern(Scope *scope, Token *name, Type *type) {
  (void)register_decl(scope, DECL_GLOBAL_VAR, name->ident, type, NULL, NULL,
                      NULL, NULL);
}

static StackVar *register_stack_var(Scope *scope, Token *name, Type *type,
                                    const Range *range) {
  if (type->ty == TY_VOID) {
    range_error(range, "void型の変数は定義できません: %s", name->ident);
  }

  FuncCtxt *fcx = scope->func_ctxt;

  StackVar *var = NEW(StackVar);
  var->name = name->ident;
  var->offset = INT_MIN; // Initialize with invalid value
  var->type = type;
  var->range = range;
  vec_push(fcx->var_list, var);

  if (!register_decl(scope, DECL_STACK_VAR, name->ident, type, var, NULL, NULL,
                     NULL)) {
    range_error(range, "同じ名前のローカル変数が複数あります: %s", name->ident);
  }

  return var;
}

static GlobalVar *register_global_var(Scope *scope, Token *name, Type *type,
                                      const Range *range,
                                      StorageClassSpecifier scs) {
  GlobalVar *gvar = new_global_variable(type, name->ident, range, scs);
  if (!register_decl(scope, DECL_GLOBAL_VAR, name->ident, type, NULL, gvar,
                     NULL, NULL)) {
    Decl *decl = get_decl(scope, name->ident);
    if (decl->global_var == NULL) {
      decl->global_var = gvar;
    } else {
      gvar = decl->global_var;
    }
  }
  return gvar;
}

static void register_number(Scope *scope, Token *name, Type *type,
                            Number *number) {
  if (!register_decl(scope, DECL_NUMBER, name->ident, type, NULL, NULL, number,
                     NULL)) {
    range_error(name->range, "定義済みの識別子です: %s", name->ident);
  }
}

static void register_builtin_func(Scope *scope, const char *name,
                                  builtin_func_handler_t *handler) {
  Type *type = new_type(TY_BUILTIN, EMPTY_TYPE_QUALIFIER);
  (void)register_decl(scope, DECL_BUILTIN_FUNC, name, type, NULL, NULL, NULL,
                      handler);
}

static bool register_decl(Scope *scope, decl_t kind, const char *name,
                          Type *type, StackVar *svar, GlobalVar *gvar,
                          Number *num_val, builtin_func_handler_t *handler) {
  if (map_get(scope->decl_map, name)) {
    return false;
  }
  Decl *decl = NEW(Decl);
  decl->kind = kind;
  decl->type = type;
  decl->stack_var = svar;
  decl->global_var = gvar;
  decl->num_val = num_val;
  decl->builtin_func = handler;
  map_put(scope->decl_map, name, decl);
  return true;
}

static Decl *get_decl(Scope *scope, const char *name) {
  while (scope != NULL) {
    Decl *decl = map_get(scope->decl_map, name);
    if (decl) {
      return decl;
    }
    scope = scope->outer;
  }
  return NULL;
}

static bool register_tag(Scope *scope, char *tag, Type *type) {
  if (map_get(scope->tag_map, tag)) {
    return false;
  }
  map_put(scope->tag_map, tag, type);
  return true;
}

static Type *get_tag(Scope *scope, char *tag) {
  while (scope != NULL) {
    Type *type = map_get(scope->tag_map, tag);
    if (type) {
      return type;
    }
    scope = scope->outer;
  }
  return NULL;
}

static bool register_typedef(Scope *scope, char *name, Type *type) {
  if (map_get(scope->typedef_map, name)) {
    return false;
  }
  map_put(scope->typedef_map, name, type);
  return true;
}

static Type *get_typedef(Scope *scope, char *name) {
  while (scope != NULL) {
    Type *type = map_get(scope->typedef_map, name);
    if (type != NULL) {
      return type;
    }
    scope = scope->outer;
  }
  return NULL;
}

static Type *integer_promoted(Scope *scope, Expr **e) {
  if (!is_integer_type((*e)->val_type)) {
    return NULL;
  }
  // BOOL, S/UCHAR, (S_,U_)SHORT, ENUM は S_INT へ昇格する
  switch ((*e)->val_type->ty) {
  case TY_BOOL:
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_U_CHAR:
  case TY_S_SHORT:
  case TY_U_SHORT:
  case TY_ENUM:
    *e = new_expr_cast(scope, new_type(TY_S_INT, EMPTY_TYPE_QUALIFIER), *e,
                       (*e)->range);
    break;
  case TY_S_INT:
  case TY_U_INT:
  case TY_S_LONG:
  case TY_U_LONG:
  case TY_S_LLONG:
  case TY_U_LLONG:
  case TY_FLOAT:
  case TY_DOUBLE:
  case TY_LDOUBLE:
  case TY_VOID:
  case TY_PTR:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
  case TY_BUILTIN:
    break;
  }
  return (*e)->val_type;
}

static Type *arith_converted(Scope *scope, Expr **e1, Expr **e2) {
  Type *ty1 = (*e1)->val_type;
  Type *ty2 = (*e2)->val_type;
  const Range *r1 = (*e1)->range;
  const Range *r2 = (*e2)->range;

  if (!is_arith_type(ty1) || !is_arith_type(ty2)) {
    return NULL;
  }

  if (is_float_type(ty1) || is_float_type(ty2)) {
    Type *type;
    if (ty1->ty == TY_LDOUBLE || ty2->ty == TY_LDOUBLE) {
      type = new_type(TY_LDOUBLE, EMPTY_TYPE_QUALIFIER);
    } else if (ty1->ty == TY_DOUBLE || ty2->ty == TY_DOUBLE) {
      type = new_type(TY_DOUBLE, EMPTY_TYPE_QUALIFIER);
    } else {
      assert(ty1->ty == TY_FLOAT || ty2->ty == TY_FLOAT);
      type = new_type(TY_FLOAT, EMPTY_TYPE_QUALIFIER);
    }
    *e1 = new_expr_cast(scope, type, *e1, r1);
    *e2 = new_expr_cast(scope, type, *e2, r2);
    return type;
  }

  ty1 = integer_promoted(scope, e1);
  ty2 = integer_promoted(scope, e2);

  bool is_signed1 = is_signed_int_type(ty1, r1);
  bool is_signed2 = is_signed_int_type(ty2, r2);
  int rank1 = get_int_type_rank(ty1, r1);
  int rank2 = get_int_type_rank(ty2, r2);

  if (rank1 < rank2) {
    // revert argument order
    return arith_converted(scope, e2, e1);
  }

  if ((is_signed1 ^ is_signed2) == 0) {
    // both operands have signed type or both have unsigned type
    if (rank1 > rank2) {
      *e2 = new_expr_cast(scope, ty1, *e2, r2);
      return ty1;
    }
    assert(is_sametype(ty1, ty2));
    return ty1;
  }

  if (!is_signed1) {
    // unsigned operand has rank greater or equal to another
    *e2 = new_expr_cast(scope, ty1, *e2, r2);
    return ty1;
  }

  assert(is_signed1 && !is_signed2);
  if (get_val_size(ty1, r1) > get_val_size(ty2, r2)) {
    // signed type can represent all of the value of unsigned type
    *e2 = new_expr_cast(scope, ty1, *e2, r2);
    return ty1;
  }

  Type *ty;
  switch (ty1->ty) {
  case TY_S_INT:
    ty = new_type(TY_U_INT, EMPTY_TYPE_QUALIFIER);
    break;
  case TY_S_LONG:
    ty = new_type(TY_U_LONG, EMPTY_TYPE_QUALIFIER);
    break;
  case TY_S_LLONG:
    ty = new_type(TY_U_LLONG, EMPTY_TYPE_QUALIFIER);
    break;
  default:
    assert(false);
  }

  *e1 = new_expr_cast(scope, ty, *e1, r1);
  *e2 = new_expr_cast(scope, ty, *e2, r2);
  return ty;
}

static bool token_is_storage_class_specifier(Token *token) {
  switch (token->ty) {
  case TK_TYPEDEF:
  case TK_EXTERN:
  case TK_STATIC:
    return true;
  default:
    return false;
  }
}

static bool consume_storage_class_specifier(Tokenizer *tokenizer,
                                            StorageClassSpecifier *scs) {
  bool consumed = false;
  while (true) {
    Token *token;
    if ((token = token_consume(tokenizer, TK_TYPEDEF)) != NULL) {
      scs->is_typedef = true;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_EXTERN)) != NULL) {
      scs->is_extern = true;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_STATIC)) != NULL) {
      scs->is_static = true;
      consumed = true;
      continue;
    }

    assert(!token_is_storage_class_specifier(token_peek(tokenizer)));
    break;
  }

  return consumed;
}

static bool token_is_type_name(Scope *scope, Token *token) {
  return token_is_type_specifier(scope, token) ||
         token_is_type_qualifier(token);
}

static bool token_is_declaration_specifiers(Scope *scope, Token *token) {
  return token_is_storage_class_specifier(token) ||
         token_is_type_specifier(scope, token) ||
         token_is_function_specifier(token) || token_is_type_qualifier(token);
}

static bool token_is_type_specifier(Scope *scope, Token *token) {
  switch (token->ty) {
  case TK_VOID:
  case TK_BOOL:
  case TK_INT:
  case TK_SHORT:
  case TK_LONG:
  case TK_CHAR:
  case TK_SIGNED:
  case TK_UNSIGNED:
  case TK_FLOAT:
  case TK_DOUBLE:
  case TK_STRUCT:
  case TK_UNION:
  case TK_ENUM:
    return true;
  case TK_IDENT:
    return get_typedef(scope, token->ident) != NULL;
  default:
    return false;
  }
}

static bool consume_type_specifier(Scope *scope, Tokenizer *tokenizer,
                                   TypeSpecifier *ts) {
  bool consumed = false;
  while (true) {
    bool base_type_specified = ts->base_type != BASE_TYPE_UNSPECIFIED;
    bool signedness_specified = ts->signedness != SIGN_UNSPECIFIED;
    bool size_specified = ts->size != SIZE_UNSPECIFIED;
    bool concrete_type_specified = ts->concrete_type != NULL;
    bool can_be_concrete_type = !base_type_specified && !signedness_specified &&
                                !size_specified && !concrete_type_specified;

    Token *token;
    if ((token = token_consume(tokenizer, TK_VOID)) != NULL) {
      if (concrete_type_specified) {
        range_error(token->range, "無効な型です");
      }
      ts->concrete_type = new_type(TY_VOID, EMPTY_TYPE_QUALIFIER);
      ts->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_BOOL)) != NULL) {
      if (concrete_type_specified) {
        range_error(token->range, "無効な型です");
      }
      ts->concrete_type = new_type(TY_BOOL, EMPTY_TYPE_QUALIFIER);
      ts->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_FLOAT)) != NULL) {
      if (concrete_type_specified) {
        range_error(token->range, "無効な型です");
      }
      ts->concrete_type = new_type(TY_FLOAT, EMPTY_TYPE_QUALIFIER);
      ts->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_STRUCT)) != NULL ||
        (token = token_consume(tokenizer, TK_UNION)) != NULL) {
      if (concrete_type_specified) {
        range_error(token->range, "無効な型です");
      }
      ts->concrete_type = struct_or_union_specifier(scope, tokenizer, token);
      ts->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_ENUM)) != NULL) {
      if (concrete_type_specified) {
        range_error(token->range, "無効な型です");
      }
      ts->concrete_type = enum_specifier(scope, tokenizer, token);
      ts->range = token->range;
      consumed = true;
      continue;
    }

    if ((token = token_consume(tokenizer, TK_INT)) != NULL) {
      if (base_type_specified) {
        range_error(token->range, "無効な型です");
      }
      ts->base_type = BASE_TYPE_INT;
      ts->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_CHAR)) != NULL) {
      if (base_type_specified) {
        range_error(token->range, "無効な型です");
      }
      ts->base_type = BASE_TYPE_CHAR;
      ts->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_DOUBLE)) != NULL) {
      if (base_type_specified) {
        range_error(token->range, "無効な型です");
      }
      ts->base_type = BASE_TYPE_DOUBLE;
      ts->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_SHORT)) != NULL) {
      if (size_specified) {
        range_error(token->range, "無効な型です");
      }
      ts->size = SIZE_SHORT;
      ts->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_LONG)) != NULL) {
      if (ts->size == SIZE_SHORT || ts->size == SIZE_LLONG) {
        range_error(token->range, "無効な型です");
      }
      if (ts->size == SIZE_UNSPECIFIED) {
        ts->size = SIZE_LONG;
      } else {
        assert(ts->size == SIZE_LONG);
        ts->size = SIZE_LLONG;
      }
      ts->range = token->range;
      consumed = true;
      continue;
    }

    if ((token = token_consume(tokenizer, TK_SIGNED)) != NULL) {
      if (signedness_specified) {
        range_error(token->range, "無効な型です");
      }
      ts->signedness = SIGN_SIGNED;
      ts->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_UNSIGNED)) != NULL) {
      if (signedness_specified) {
        range_error(token->range, "無効な型です");
      }
      ts->signedness = SIGN_UNSIGNED;
      ts->range = token->range;
      consumed = true;
      continue;
    }

    token = token_peek(tokenizer);
    if (token->ty == TK_IDENT && can_be_concrete_type) {
      Type *type = get_typedef(scope, token->ident);
      if (type != NULL) {
        token_succ(tokenizer);
        ts->concrete_type = type;
        ts->range = token->range;
        consumed = true;
        continue;
      }
    }

    assert(!can_be_concrete_type ||
           !token_is_type_specifier(scope, token_peek(tokenizer)));
    break;
  }

  return consumed;
}

static Type *construct_type_specifier(TypeSpecifier ts, TypeQualifier tq) {
  if (ts.concrete_type != NULL) {
    if (ts.base_type != BASE_TYPE_UNSPECIFIED ||
        ts.signedness != SIGN_UNSPECIFIED || ts.size != SIZE_UNSPECIFIED) {
      range_error(ts.range, "無効な型です");
    }
    Type *type = clone_type(ts.concrete_type);
    type->qualifier = tq;
    return type;
  }
  if (ts.base_type == BASE_TYPE_UNSPECIFIED &&
      ts.signedness == SIGN_UNSPECIFIED && ts.size == SIZE_UNSPECIFIED) {
    return NULL;
  }

  switch (ts.base_type) {
  case BASE_TYPE_UNSPECIFIED:
  case BASE_TYPE_INT:
    switch (ts.signedness) {
    case SIGN_UNSPECIFIED:
    case SIGN_SIGNED:
      switch (ts.size) {
      case SIZE_UNSPECIFIED:
        return new_type(TY_S_INT, tq);
      case SIZE_SHORT:
        return new_type(TY_S_SHORT, tq);
      case SIZE_LONG:
        return new_type(TY_S_LONG, tq);
      case SIZE_LLONG:
        return new_type(TY_S_LLONG, tq);
      }
      break;
    case SIGN_UNSIGNED:
      switch (ts.size) {
      case SIZE_UNSPECIFIED:
        return new_type(TY_U_INT, tq);
      case SIZE_SHORT:
        return new_type(TY_U_SHORT, tq);
      case SIZE_LONG:
        return new_type(TY_U_LONG, tq);
      case SIZE_LLONG:
        return new_type(TY_U_LLONG, tq);
      }
      break;
    }

    break;
  case BASE_TYPE_CHAR:
    if (ts.size != SIZE_UNSPECIFIED) {
      range_error(ts.range, "無効な型です");
    }
    switch (ts.signedness) {
    case SIGN_UNSPECIFIED:
      return new_type(TY_CHAR, tq);
    case SIGN_SIGNED:
      return new_type(TY_S_CHAR, tq);
    case SIGN_UNSIGNED:
      return new_type(TY_U_CHAR, tq);
    }
    break;
  case BASE_TYPE_DOUBLE:
    if (ts.signedness != SIGN_UNSPECIFIED) {
      range_error(ts.range, "無効な型です");
    }
    switch (ts.size) {
    case SIZE_UNSPECIFIED:
      return new_type(TY_DOUBLE, tq);
    case SIZE_LONG:
      return new_type(TY_LDOUBLE, tq);
    case SIZE_SHORT:
    case SIZE_LLONG:
      range_error(ts.range, "無効な型です");
    }
  }
  assert(false);
}

static bool token_is_type_qualifier(Token *token) {
  switch (token->ty) {
  case TK_CONST:
  case TK_RESTRICT:
  case TK_VOLATILE:
    return true;
  default:
    return false;
  }
}
static bool consume_type_qualifier(Tokenizer *tokenizer, TypeQualifier *tq) {
  bool consumed = false;
  while (true) {
    Token *token;
    if ((token = token_consume(tokenizer, TK_CONST)) != NULL) {
      if (tq->is_const) {
        range_warn(token->range, "`const` が重複しています");
      }
      tq->is_const = true;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_RESTRICT)) != NULL) {
      if (tq->is_restrict) {
        range_warn(token->range, "`restrict` が重複しています");
      }
      tq->is_restrict = true;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_VOLATILE)) != NULL) {
      if (tq->is_volatile) {
        range_warn(token->range, "`volatile` が重複しています");
      }
      tq->is_volatile = true;
      consumed = true;
      continue;
    }

    assert(!token_is_type_qualifier(token_peek(tokenizer)));
    break;
  }

  return consumed;
}

static bool token_is_function_specifier(Token *token) {
  switch (token->ty) {
  case TK_INLINE:
  case TK_NORETURN:
    return true;
  default:
    return false;
  }
}

static bool consume_function_specifier(Tokenizer *tokenizer,
                                       FunctionSpecifier *fs) {
  bool consumed = false;
  while (true) {
    Token *token;
    if ((token = token_consume(tokenizer, TK_INLINE)) != NULL) {
      fs->is_inline = true;
      consumed = true;
      continue;
    }
    if ((token = token_consume(tokenizer, TK_NORETURN)) != NULL) {
      fs->is_noreturn = true;
      consumed = true;
      continue;
    }

    assert(!token_is_function_specifier(token_peek(tokenizer)));
    break;
  }

  return consumed;
}

int get_val_size(const Type *ty, const Range *range) {
  switch (ty->ty) {
  case TY_VOID:
    return sizeof(void);
  case TY_BOOL:
    return sizeof(bool);
  case TY_CHAR:
    return sizeof(char);
  case TY_S_CHAR:
    return sizeof(signed char);
  case TY_S_INT:
    return sizeof(signed int);
  case TY_S_SHORT:
    return sizeof(signed short);
  case TY_S_LONG:
    return sizeof(signed long);
  case TY_S_LLONG:
    return sizeof(signed long long);
  case TY_U_CHAR:
    return sizeof(unsigned char);
  case TY_U_INT:
    return sizeof(unsigned int);
  case TY_U_SHORT:
    return sizeof(unsigned short);
  case TY_U_LONG:
    return sizeof(unsigned long);
  case TY_U_LLONG:
    return sizeof(unsigned long long);
  case TY_FLOAT:
    return sizeof(float);
  case TY_DOUBLE:
    return sizeof(double);
  case TY_LDOUBLE:
    return sizeof(long double);
  case TY_PTR:
    return sizeof(void *);
  case TY_ARRAY:
    if (ty->array_len < 0) {
      range_error(range, "不完全な配列型のサイズを取得しようとしました: %s",
                  format_type(ty, false));
    }
    return get_val_size(ty->ptrof, range) * ty->array_len;
  case TY_FUNC:
    range_error(range, "関数型の値サイズを取得しようとしました: %s",
                format_type(ty, false));
  case TY_STRUCT:
  case TY_UNION:
    if (ty->struct_body->member_list == NULL) {
      range_error(range, "不完全型の値のサイズを取得しようとしました: %s",
                  format_type(ty, false));
    }
    return align(ty->struct_body->member_size, ty->struct_body->member_align);
  case TY_ENUM:
    return sizeof(int);
  case TY_BUILTIN:
    range_error(range, "ビルトイン型の値サイズを取得しようとしました: %s",
                format_type(ty, false));
  }
  range_error(range, "不明な型のサイズを取得しようとしました: %s",
              format_type(ty, false));
}

int get_val_align(const Type *ty, const Range *range) {
  switch (ty->ty) {
  case TY_VOID:
    return alignof(void);
  case TY_BOOL:
    return alignof(bool);
  case TY_CHAR:
    return alignof(char);
  case TY_S_CHAR:
    return alignof(signed char);
  case TY_S_SHORT:
    return alignof(signed short);
  case TY_S_INT:
    return alignof(signed int);
  case TY_S_LONG:
    return alignof(signed long);
  case TY_S_LLONG:
    return alignof(signed long long);
  case TY_U_CHAR:
    return alignof(unsigned char);
  case TY_U_SHORT:
    return alignof(unsigned short);
  case TY_U_INT:
    return alignof(unsigned int);
  case TY_U_LONG:
    return alignof(unsigned long);
  case TY_U_LLONG:
    return alignof(unsigned long long);
  case TY_FLOAT:
    return alignof(float);
  case TY_DOUBLE:
    return alignof(double);
  case TY_LDOUBLE:
    return alignof(long double);
  case TY_PTR:
    return alignof(void *);
  case TY_ARRAY:
    return get_val_align(ty->ptrof, range);
  case TY_FUNC:
    range_error(range, "関数型の値アラインメントを取得しようとしました: %s",
                format_type(ty, false));
  case TY_STRUCT:
  case TY_UNION:
    if (ty->struct_body->member_list == NULL) {
      range_error(range,
                  "不完全型の値のアラインメントを取得しようとしました: %s",
                  format_type(ty, false));
    }
    return ty->struct_body->member_align;
  case TY_ENUM:
    return alignof(int);
  case TY_BUILTIN:
    range_error(range,
                "ビルトイン型の値のアラインメントを取得しようとしました: %s",
                format_type(ty, false));
  }
  range_error(range, "不明な型の値アラインメントを取得しようとしました: %s",
              format_type(ty, false));
}

#define binop_type_error(ty, lhs, rhs)                                         \
  binop_type_error_raw((ty), (lhs), (rhs), __FILE__, __LINE__)
static noreturn void binop_type_error_raw(int ty, Expr *lhs, Expr *rhs,
                                          const char *dbg_file, int dbg_line) {
  range_error_raw(range_join(lhs->range, rhs->range), dbg_file, dbg_line,
                  "不正な型の値に対する演算です: 演算=%d(%c), 左辺=%s, 右辺=%s",
                  ty, ty, format_type(lhs->val_type, false),
                  format_type(rhs->val_type, false));
}

static Expr *coerce_array2ptr(Scope *scope, Expr *expr) {
  if (is_array_type(expr->val_type)) {
    return new_expr_unary(scope, EX_ADDRESS, expr, expr->range);
  }
  return expr;
}
static Expr *coerce_func2ptr(Scope *scope, Expr *expr) {
  if (is_func_type(expr->val_type)) {
    return new_expr_unary(scope, EX_ADDRESS, expr, expr->range);
  }
  return expr;
}

static bool is_null_ptr_const(Expr *expr) {
  if (!is_integer_type(expr->val_type)) {
    return false;
  }

  // evaluate constant expression
  sema_expr(expr);

  if (expr->ty != EX_NUM) {
    return false;
  }
  int n;
  SET_NUMBER_VAL(n, &expr->num);
  return n == 0;
}

static Expr *new_expr(int ty, Type *val_type, const Range *range) {
  Expr *expr = NEW(Expr);
  expr->ty = ty;
  expr->val_type = val_type;
  expr->range = range;
  return expr;
}

static Expr *new_expr_num(Number val, const Range *range) {
  Expr *expr =
      new_expr(EX_NUM, new_type(val.type, EMPTY_TYPE_QUALIFIER), range);
  expr->num = val;
  return expr;
}

static Expr *new_expr_ident(Scope *scope, Token *ident) {
  assert(ident->ty == TK_IDENT);
  const char *name = ident->ident;
  const Range *range = ident->range;

  int ty;
  Type *type;
  if (strcmp(name, "__func__") == 0) {
    if (scope->func_ctxt == NULL) {
      range_error(range, "関数外で__func__が使用されました");
    }
    return new_expr_str(scope, scope->func_ctxt->name, range);
  }
  Decl *decl = get_decl(scope, name);
  StackVar *svar = NULL;
  GlobalVar *gvar = NULL;
  Number *num_val = NULL;
  builtin_func_handler_t *builtin_func = NULL;
  if (decl != NULL) {
    switch (decl->kind) {
    case DECL_STACK_VAR:
      ty = EX_STACK_VAR;
      type = decl->type;
      svar = decl->stack_var;
      break;
    case DECL_GLOBAL_VAR:
      ty = EX_GLOBAL_VAR;
      type = decl->type;
      gvar = decl->global_var;
      break;
    case DECL_NUMBER:
      ty = EX_NUM;
      type = decl->type;
      num_val = decl->num_val;
      break;
    case DECL_BUILTIN_FUNC:
      ty = EX_BUILTIN_FUNC;
      type = new_type(TY_BUILTIN, EMPTY_TYPE_QUALIFIER);
      builtin_func = decl->builtin_func;
      break;
    }
  } else {
    range_warn(range, "未定義の識別子です: %s", name);
    ty = EX_GLOBAL_VAR;
    type = new_type(TY_S_INT, EMPTY_TYPE_QUALIFIER);
    gvar = register_global_var(scope, ident, type, ident->range,
                               EMPTY_STORAGE_CLASS_SPECIFIER);
  }

  Expr *expr = new_expr(ty, type, range);
  switch (ty) {
  case EX_STACK_VAR:
    expr->stack_var = svar;
    break;
  case EX_GLOBAL_VAR:
    expr->global_var.name = gvar != NULL ? gvar->name : name;
    expr->global_var.def = gvar;
    break;
  case EX_NUM:
    expr->num = *num_val;
    break;
  case EX_BUILTIN_FUNC:
    expr->builtin_func.name = name;
    expr->builtin_func.handler = builtin_func;
    break;
  default:
    assert(false);
  }
  return expr;
}

static Expr *new_expr_str(Scope *scope, const char *val, const Range *range) {
  Type *type = new_type_ptr(new_type(TY_CHAR, CONST_TYPE_QUALIFIER),
                            EMPTY_TYPE_QUALIFIER);
  Expr *expr = new_expr(EX_STR, type, range);

  expr->str = make_label("str");
  StringLiteral *str = NEW(StringLiteral);
  str->name = expr->str;
  str->val = val;
  vec_push(scope->global_ctxt->str_list, str);
  return expr;
}

static Expr *new_expr_call(Scope *scope, Expr *callee, Vector *argument,
                           const Range *range) {
  if (callee->ty == EX_BUILTIN_FUNC) {
    return callee->builtin_func.handler(scope, callee, argument, range);
  }

  callee = coerce_array2ptr(scope, callee);

  Type *func_type;
  Type *ret_type;
  if (is_func_type(callee->val_type)) {
    func_type = callee->val_type;
  } else if (is_ptr_type(callee->val_type) &&
             is_func_type(callee->val_type->ptrof)) {
    func_type = callee->val_type->ptrof;
  } else {
    range_warn(range, "未知の関数です");
    func_type = new_type_func(new_type(TY_S_INT, EMPTY_TYPE_QUALIFIER), NULL,
                              false, EMPTY_TYPE_QUALIFIER);
  }

  ret_type = func_type->func_ret;

  int narg = 0;
  if (argument != NULL) {
    for (int i = 0; i < vec_len(argument); i++) {
      vec_set(argument, i, coerce_array2ptr(scope, vec_get(argument, i)));
      vec_set(argument, i, coerce_func2ptr(scope, vec_get(argument, i)));
    }
    narg = vec_len(argument);
  }

  Vector *params = func_type->func_param;
  int nparam = params != NULL ? vec_len(params) : 0;
  if (params != NULL) {
    if ((narg < nparam) || (narg > nparam && !func_type->func_has_varargs)) {
      range_error(range,
                  "関数の引数の個数が一致しません: argument=%d, parameter=%d",
                  narg, nparam);
    }
  }

  for (int i = 0; i < nparam; i++) {
    Param *param = vec_get(params, i);
    Expr *arg = vec_get(argument, i);
    vec_set(argument, i, new_expr_cast(scope, param->type, arg, arg->range));
  }
  // default argument promotion
  for (int i = nparam; i < narg; i++) {
    Expr *arg = vec_get(argument, i);
    if (is_integer_type(arg->val_type)) {
      integer_promoted(scope, &arg);
    } else if (arg->val_type->ty == TY_FLOAT) {
      arg = new_expr_cast(scope, new_type(TY_DOUBLE, EMPTY_TYPE_QUALIFIER), arg,
                          arg->range);
    } else {
      // do nothing
    }
    vec_set(argument, i, arg);
  }

  Expr *expr = new_expr(EX_CALL, ret_type, range);
  expr->call.callee = callee;
  expr->call.argument = argument;
  return expr;
}

static Expr *new_expr_builtin_va_start(Scope *scope,
                                       Expr *callee __attribute__((unused)),
                                       Vector *argument, const Range *range) {
  int narg = argument != NULL ? vec_len(argument) : 0;

  if (argument == NULL || vec_len(argument) != 2) {
    range_error(range,
                "関数の引数の個数が一致しません: argument=%d, parameter=%d",
                narg, 2);
  }

  Expr *ap = vec_get(argument, 0);
  Expr *last = vec_get(argument, 1);

  ap = coerce_array2ptr(scope, ap);
  ap = coerce_func2ptr(scope, ap);

  Type *type = new_type(TY_VOID, EMPTY_TYPE_QUALIFIER);
  Expr *expr = new_expr(EX_BUILTIN_VA_START, type, range);
  expr->builtin_va_start.ap = ap;
  expr->builtin_va_start.last = last;
  return expr;
}

static Expr *new_expr_builtin_va_arg(Scope *scope __attribute__((unused)),
                                     Expr *callee __attribute__((unused)),
                                     Vector *argument __attribute__((unused)),
                                     const Range *range) {
  int narg = argument != NULL ? vec_len(argument) : 0;

  if (argument == NULL || vec_len(argument) != 2) {
    range_error(range,
                "関数の引数の個数が一致しません: argument=%d, parameter=%d",
                narg, 2);
  }

  Expr *ap = vec_get(argument, 0);
  Expr *type_expr = vec_get(argument, 1);

  ap = coerce_array2ptr(scope, ap);
  ap = coerce_func2ptr(scope, ap);

  if (!is_ptr_type(type_expr->val_type)) {
    range_error(range, "ポインタ型ではありません: %s",
                format_type(type_expr->val_type, false));
  }

  Type *type = type_expr->val_type->ptrof;
  Expr *expr = new_expr(EX_BUILTIN_VA_ARG, type, range);
  expr->builtin_va_arg.ap = ap;
  expr->builtin_va_arg.type = type;
  return expr;
}

static Expr *new_expr_builtin_va_end(Scope *scope __attribute__((unused)),
                                     Expr *callee __attribute__((unused)),
                                     Vector *argument __attribute__((unused)),
                                     const Range *range) {
  int narg = argument != NULL ? vec_len(argument) : 0;

  if (argument == NULL || vec_len(argument) != 1) {
    range_error(range,
                "関数の引数の個数が一致しません: argument=%d, parameter=%d",
                narg, 2);
  }

  Expr *ap = vec_get(argument, 0);

  ap = coerce_array2ptr(scope, ap);
  ap = coerce_func2ptr(scope, ap);

  Type *type = new_type(TY_VOID, EMPTY_TYPE_QUALIFIER);
  Expr *expr = new_expr(EX_BUILTIN_VA_END, type, range);
  expr->builtin_va_end.ap = ap;
  return expr;
}

static Expr *new_expr_builtin_va_copy(Scope *scope __attribute__((unused)),
                                      Expr *callee __attribute__((unused)),
                                      Vector *argument __attribute__((unused)),
                                      const Range *range) {
  int narg = argument != NULL ? vec_len(argument) : 0;

  if (argument == NULL || vec_len(argument) != 2) {
    range_error(range,
                "関数の引数の個数が一致しません: argument=%d, parameter=%d",
                narg, 2);
  }

  Expr *dest = vec_get(argument, 0);
  Expr *src = vec_get(argument, 1);

  dest = coerce_array2ptr(scope, dest);
  dest = coerce_func2ptr(scope, dest);

  src = coerce_array2ptr(scope, src);
  src = coerce_func2ptr(scope, src);

  Type *type = new_type(TY_VOID, EMPTY_TYPE_QUALIFIER);
  Expr *expr = new_expr(EX_BUILTIN_VA_COPY, type, range);
  Expr *idx = new_expr_num(new_number_int(0), expr->range);
  expr->builtin_va_copy.dest = new_expr_index(scope, dest, idx, dest->range);
  expr->builtin_va_copy.src = new_expr_index(scope, src, idx, src->range);
  return expr;
}

static Expr *new_expr_postfix(Scope *scope, int ty, Expr *operand,
                              const Range *range) {
  operand = coerce_array2ptr(scope, operand);
  operand = coerce_func2ptr(scope, operand);

  Expr *expr = new_expr(ty, operand->val_type, range);
  expr->unop.operand = operand;
  return expr;
}

static Expr *new_expr_cast(Scope *scope, Type *val_type, Expr *operand,
                           const Range *range) {
  operand = coerce_array2ptr(scope, operand);
  operand = coerce_func2ptr(scope, operand);

  if (is_sametype(operand->val_type, val_type)) {
    return operand;
  }

  Expr *expr = new_expr(EX_CAST, val_type, range);
  if (val_type->ty == TY_BOOL) {
    expr->unop.operand =
        new_expr_binop(scope, EX_NOTEQ, operand,
                       new_expr_num(new_number_int(0), range), range);
  } else {
    expr->unop.operand = operand;
  }
  return expr;
}

static Expr *new_expr_compound(Scope *scope, Type *val_type, Initializer *init,
                               const Range *range) {
  if (scope->func_ctxt != NULL) {
    FuncCtxt *fcx = scope->func_ctxt;
    StackVar *var = NEW(StackVar);
    var->name = ".compound";
    var->offset = INT_MIN;
    var->type = val_type;
    var->range = range;
    vec_push(fcx->var_list, var);

    Expr *expr = NULL;
    Expr *dest = new_expr(EX_STACK_VAR, val_type, range);
    dest->stack_var = var;
    gen_init(scope, &expr, init, dest, val_type);
    if (expr != NULL) {
      expr = new_expr_binop(scope, EX_COMMA, expr, dest, range);
    } else {
      expr = dest;
    }
    return expr;
  }

  Expr *expr = new_expr(EX_COMPOUND, val_type, range);
  expr->compound = init;
  return expr;
}

static Expr *new_expr_unary(Scope *scope, int op, Expr *operand,
                            const Range *range) {
  if (op != EX_ADDRESS) {
    // & 以外は array, func は ptr とみなす
    operand = coerce_array2ptr(scope, operand);
    operand = coerce_func2ptr(scope, operand);
  }

  Type *val_type;
  switch (op) {
  case EX_PRE_INC:
  case EX_PRE_DEC: {
    val_type = operand->val_type;
    break;
  case EX_ADDRESS: {
    if (is_array_type(operand->val_type)) {
      val_type = new_type_ptr(operand->val_type->ptrof, EMPTY_TYPE_QUALIFIER);
    } else {
      val_type = new_type_ptr(operand->val_type, EMPTY_TYPE_QUALIFIER);
    }
    break;
  }
  case EX_INDIRECT: {
    if (operand->val_type->ty != TY_PTR) {
      range_error(range, "ポインタ型でない値に対するデリファレンスです: %s",
                  format_type(operand->val_type, false));
    }
    if (operand->val_type->ptrof->ty == TY_FUNC) {
      return operand;
    }
    val_type = operand->val_type->ptrof;
    break;
  }
  case EX_PLUS:
  case EX_MINUS:
    if (!is_arith_type(operand->val_type)) {
      range_error(range,
                  "不正な型の値に対する演算です: 算術型ではありません: %s",
                  format_type(operand->val_type, false));
    }
    if (is_integer_type(operand->val_type)) {
      val_type = integer_promoted(scope, &operand);
    } else {
      val_type = operand->val_type;
    }
    break;
  case EX_LOG_NOT: {
    return new_expr_binop(
        scope, EX_EQEQ, operand,
        new_expr_cast(scope, operand->val_type,
                      new_expr_num(new_number_int(0), operand->range),
                      operand->range),
        operand->range);
  }
  case EX_NOT:
    if (!is_integer_type(operand->val_type)) {
      range_error(range,
                  "不正な型の値に対する演算です: 整数型ではありません: %s",
                  format_type(operand->val_type, false));
    }
    val_type = integer_promoted(scope, &operand);
    break;
  default:
    assert(false);
  }
  }

  Expr *expr = new_expr(op, val_type, range);
  expr->unop.operand = operand;
  return expr;
}

static Expr *new_expr_binop(Scope *scope, int op, Expr *lhs, Expr *rhs,
                            const Range *range) {
  lhs = coerce_array2ptr(scope, lhs);
  lhs = coerce_func2ptr(scope, lhs);
  rhs = coerce_array2ptr(scope, rhs);
  rhs = coerce_func2ptr(scope, rhs);

  Type *val_type;
  switch (op) {
  // multiplicative
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
    val_type = arith_converted(scope, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(op, lhs, rhs);
    }
    break;
  // additive
  case EX_ADD:
    if (is_ptr_type(lhs->val_type)) {
      if (is_ptr_type(rhs->val_type) || !is_integer_type(rhs->val_type)) {
        binop_type_error(op, lhs, rhs);
      }

      // ptr + int => ptr + (size * int)
      Expr *size = new_expr_num(
          new_number_size_t(get_val_size(lhs->val_type->ptrof, lhs->range)),
          range);
      rhs = new_expr_binop(scope, EX_MUL, rhs, size, range);
      val_type = lhs->val_type;
      break;
    }

    if (is_ptr_type(rhs->val_type)) {
      if (!is_integer_type(lhs->val_type)) {
        binop_type_error(op, lhs, rhs);
      }

      // int + ptr => (size * int) + ptr
      Expr *size = new_expr_num(
          new_number_size_t(get_val_size(rhs->val_type->ptrof, rhs->range)),
          range);
      lhs = new_expr_binop(scope, EX_MUL, lhs, size, range);
      val_type = rhs->val_type;
      break;
    }

    // int + int
    val_type = arith_converted(scope, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(op, lhs, rhs);
    }
    break;
  case EX_SUB:
    if (is_ptr_type(lhs->val_type)) {
      if (is_ptr_type(rhs->val_type)) {
        if (!is_sametype(lhs->val_type, rhs->val_type)) {
          binop_type_error(op, lhs, rhs);
        }

        // ptr - ptr
        Expr *sub =
            new_expr(EX_SUB, new_type(TY_S_LONG, EMPTY_TYPE_QUALIFIER), range);
        sub->binop.lhs = lhs;
        sub->binop.rhs = rhs;
        Expr *size = new_expr_num(new_number_ptrdiff_t(get_val_size(
                                      lhs->val_type->ptrof, lhs->range)),
                                  range);
        return new_expr_binop(scope, EX_DIV, sub, size, range);
      }

      if (is_integer_type(rhs->val_type)) {
        // int - int
        Expr *size = new_expr_num(
            new_number_size_t(get_val_size(lhs->val_type->ptrof, lhs->range)),
            range);
        rhs = new_expr_binop(scope, EX_MUL, rhs, size, range);
        val_type = lhs->val_type;
        break;
      }
      binop_type_error(op, lhs, rhs);
    }

    if (is_ptr_type(rhs->val_type)) {
      binop_type_error(op, lhs, rhs);
    }

    val_type = arith_converted(scope, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(op, lhs, rhs);
    }
    break;
  // shift
  case EX_LSHIFT:
  case EX_RSHIFT:
    if (!is_integer_type(lhs->val_type) || !is_integer_type(rhs->val_type)) {
      binop_type_error(op, lhs, rhs);
    }
    val_type = integer_promoted(scope, &lhs);
    if (val_type == NULL) {
      binop_type_error(op, lhs, rhs);
    }
    break;

  case EX_LT:
  case EX_GT:
  case EX_LTEQ:
  case EX_GTEQ:
  case EX_EQEQ:
  case EX_NOTEQ:
    arith_converted(scope, &lhs, &rhs);
    val_type = new_type(TY_S_INT, EMPTY_TYPE_QUALIFIER);
    break;
  // and
  case EX_AND:
  case EX_XOR:
  case EX_OR:
    if (!is_integer_type(lhs->val_type) || !is_integer_type(rhs->val_type)) {
      binop_type_error(op, lhs, rhs);
    }
    val_type = arith_converted(scope, &lhs, &rhs);
    if (val_type == NULL) {
      binop_type_error(op, lhs, rhs);
    }
    break;
  case EX_LOG_AND: {
    val_type = new_type(TY_S_INT, EMPTY_TYPE_QUALIFIER);
    break;
  }
  case EX_LOG_OR:
    val_type = new_type(TY_S_INT, EMPTY_TYPE_QUALIFIER);
    break;
  case EX_ASSIGN:
    rhs = new_expr_cast(scope, lhs->val_type, rhs, rhs->range);
    val_type = lhs->val_type;
    break;
  case EX_MUL_ASSIGN:
    return new_expr_binop(scope, EX_ASSIGN, lhs,
                          new_expr_binop(scope, EX_MUL, lhs, rhs, range),
                          range);
  case EX_DIV_ASSIGN:
    return new_expr_binop(scope, EX_ASSIGN, lhs,
                          new_expr_binop(scope, EX_DIV, lhs, rhs, range),
                          range);
  case EX_MOD_ASSIGN:
    return new_expr_binop(scope, EX_ASSIGN, lhs,
                          new_expr_binop(scope, EX_MOD, lhs, rhs, range),
                          range);
  case EX_ADD_ASSIGN:
    return new_expr_binop(scope, EX_ASSIGN, lhs,
                          new_expr_binop(scope, EX_ADD, lhs, rhs, range),
                          range);
  case EX_SUB_ASSIGN:
    return new_expr_binop(scope, EX_ASSIGN, lhs,
                          new_expr_binop(scope, EX_SUB, lhs, rhs, range),
                          range);
  case EX_LSHIFT_ASSIGN:
    return new_expr_binop(scope, EX_ASSIGN, lhs,
                          new_expr_binop(scope, EX_LSHIFT, lhs, rhs, range),
                          range);
  case EX_RSHIFT_ASSIGN:
    return new_expr_binop(scope, EX_ASSIGN, lhs,
                          new_expr_binop(scope, EX_RSHIFT, lhs, rhs, range),
                          range);
  case EX_AND_ASSIGN:
    return new_expr_binop(scope, EX_ASSIGN, lhs,
                          new_expr_binop(scope, EX_AND, lhs, rhs, range),
                          range);
  case EX_XOR_ASSIGN:
    return new_expr_binop(scope, EX_ASSIGN, lhs,
                          new_expr_binop(scope, EX_XOR, lhs, rhs, range),
                          range);
  case EX_OR_ASSIGN:
    return new_expr_binop(scope, EX_ASSIGN, lhs,
                          new_expr_binop(scope, EX_OR, lhs, rhs, range), range);
  case EX_COMMA: {
    if (lhs->ty == EX_COMMA && rhs->ty == EX_COMMA) {
      vec_append(lhs->comma.exprs, rhs->comma.exprs);
      lhs->range = range;
      lhs->val_type = rhs->val_type;
      return lhs;
    }
    if (lhs->ty == EX_COMMA) {
      vec_push(lhs->comma.exprs, rhs);
      lhs->range = range;
      lhs->val_type = rhs->val_type;
      return lhs;
    }
    if (rhs->ty == EX_COMMA) {
      vec_insert(rhs->comma.exprs, 0, lhs);
      rhs->range = range;
      return rhs;
    }
    Expr *expr = new_expr(op, rhs->val_type, range);
    expr->comma.exprs = new_vector();
    vec_push(expr->comma.exprs, lhs);
    vec_push(expr->comma.exprs, rhs);
    return expr;
  }
  default:
    assert(false);
  }

  Expr *expr = new_expr(op, val_type, range);
  expr->binop.lhs = lhs;
  expr->binop.rhs = rhs;
  return expr;
}

static Expr *new_expr_cond(Scope *scope, Expr *cond, Expr *then_expr,
                           Expr *else_expr, const Range *range) {
  cond = coerce_array2ptr(scope, cond);
  cond = coerce_func2ptr(scope, cond);
  then_expr = coerce_array2ptr(scope, then_expr);
  then_expr = coerce_func2ptr(scope, then_expr);
  else_expr = coerce_array2ptr(scope, else_expr);
  else_expr = coerce_func2ptr(scope, else_expr);

  Type *val_type;
  if (is_arith_type(then_expr->val_type) &&
      is_arith_type(else_expr->val_type)) {
    val_type = arith_converted(scope, &then_expr, &else_expr);
  } else if (is_ptr_type(then_expr->val_type) &&
             is_ptr_type(else_expr->val_type)) {
    if (then_expr->val_type->ptrof->ty == TY_VOID) {
      val_type = else_expr->val_type;
    } else if (else_expr->val_type->ptrof->ty == TY_VOID) {
      val_type = then_expr->val_type;
    } else {
      if (!is_sametype(then_expr->val_type, else_expr->val_type)) {
        range_error(range, "条件演算子の両辺の型が異なります: %s, %s",
                    format_type(then_expr->val_type, false),
                    format_type(else_expr->val_type, false));
      }
      val_type = then_expr->val_type;
    }
  } else if (is_ptr_type(then_expr->val_type) && is_null_ptr_const(else_expr)) {
    val_type = then_expr->val_type;
  } else if (is_ptr_type(else_expr->val_type) && is_null_ptr_const(then_expr)) {
    val_type = else_expr->val_type;
  } else {
    if (!is_sametype(then_expr->val_type, else_expr->val_type)) {
      range_error(range, "条件演算子の両辺の型が異なります: %s, %s",
                  format_type(then_expr->val_type, false),
                  format_type(else_expr->val_type, false));
    }
    val_type = then_expr->val_type;
  }

  Expr *expr = new_expr(EX_COND, val_type, range);
  expr->cond.cond = cond;
  expr->cond.then_expr = then_expr;
  expr->cond.else_expr = else_expr;
  return expr;
}

static Expr *new_expr_index(Scope *scope, Expr *array, Expr *index,
                            const Range *range) {
  array = coerce_array2ptr(scope, array);
  array = coerce_func2ptr(scope, array);
  index = coerce_array2ptr(scope, index);
  index = coerce_func2ptr(scope, index);

  return new_expr_unary(scope, EX_INDIRECT,
                        new_expr_binop(scope, EX_ADD, array, index, range),
                        range);
}

static Expr *new_expr_dot(Expr *operand, const char *name, const Range *range) {
  if (operand->val_type->ty != TY_STRUCT && operand->val_type->ty != TY_UNION) {
    range_error(range, "構造体または共用体以外のメンバへのアクセスです");
  }

  StructBody *body = operand->val_type->struct_body;
  Member *member =
      body->member_name_map ? map_get(body->member_name_map, name) : NULL;

  Expr *expr = new_expr(EX_DOT, member->type, range);
  expr->dot.operand = operand;
  expr->dot.member = member;
  return expr;
}

static Expr *new_expr_arrow(Scope *scope, Expr *operand, const char *name,
                            const Range *range) {
  operand = coerce_array2ptr(scope, operand);
  operand = coerce_func2ptr(scope, operand);
  if (operand->val_type->ty != TY_PTR ||
      (operand->val_type->ptrof->ty != TY_STRUCT &&
       operand->val_type->ptrof->ty != TY_UNION)) {
    range_error(range, "構造体または共用体以外のメンバへのアクセスです: %s",
                format_type(operand->val_type, false));
  }

  StructBody *body = operand->val_type->ptrof->struct_body;
  Member *member =
      body->member_name_map ? map_get(body->member_name_map, name) : NULL;
  if (member == NULL) {
    range_error(range, "存在しないメンバへのアクセスです: %s %s",
                format_type(operand->val_type, false), name);
  }
  Expr *expr =
      new_expr(EX_ADD, new_type_ptr(member->type, EMPTY_TYPE_QUALIFIER), range);
  expr->binop.lhs = operand;
  expr->binop.rhs = new_expr_num(new_number_size_t(member->offset), range);
  return new_expr_unary(scope, EX_INDIRECT, expr, range);
}

static Stmt *new_stmt(int ty, const Range *range) {
  Stmt *stmt = NEW(Stmt);
  stmt->ty = ty;
  stmt->range = range;
  return stmt;
}

static Stmt *new_stmt_expr(Expr *expr, const Range *range) {
  Stmt *stmt = new_stmt(ST_EXPR, range);
  stmt->expr = expr;
  return stmt;
}

static Stmt *new_stmt_if(Expr *cond, Stmt *then_stmt, Stmt *else_stmt,
                         const Range *range) {
  Stmt *stmt = new_stmt(ST_IF, range);
  stmt->cond = cond;
  stmt->then_stmt = then_stmt;
  stmt->else_stmt = else_stmt;
  return stmt;
}

static Stmt *new_stmt_switch(Expr *cond, Stmt *body, const Range *range) {
  Stmt *stmt = new_stmt(ST_SWITCH, range);
  stmt->cond = cond;
  stmt->body = body;
  stmt->cases = new_vector();
  stmt->default_case = NULL;
  return stmt;
}

static Stmt *new_stmt_case(Expr *expr, Stmt *body, const Range *range) {
  Stmt *stmt = new_stmt(ST_CASE, range);
  stmt->expr = expr;
  stmt->label = make_label("case");
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_default(Stmt *body, const Range *range) {
  Stmt *stmt = new_stmt(ST_DEFAULT, range);
  stmt->label = make_label("default");
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_label(FuncCtxt *fcx, char *name, Stmt *body,
                            const Range *range) {
  Stmt *stmt = new_stmt(ST_LABEL, range);
  stmt->name = name;
  stmt->label = make_label(name);
  stmt->body = body;
  map_put(fcx->label_map, stmt->name, stmt->label);
  return stmt;
}

static Stmt *new_stmt_while(Expr *cond, Stmt *body, const Range *range) {
  Stmt *stmt = new_stmt(ST_WHILE, range);
  stmt->cond = cond;
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_do_while(Expr *cond, Stmt *body, const Range *range) {
  Stmt *stmt = new_stmt(ST_DO_WHILE, range);
  stmt->cond = cond;
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_for(Expr *init, Expr *cond, Expr *inc, Stmt *body,
                          const Range *range) {
  Stmt *stmt = new_stmt(ST_FOR, range);
  stmt->init = init;
  stmt->cond = cond;
  stmt->inc = inc;
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_goto(char *name, const Range *range) {
  Stmt *stmt = new_stmt(ST_GOTO, range);
  stmt->name = name;
  return stmt;
}

static Stmt *new_stmt_return(Scope *scope, Expr *expr, const Range *range) {
  Stmt *stmt = new_stmt(ST_RETURN, range);
  if (expr != NULL) {
    stmt->expr = new_expr_cast(scope, scope->func_ctxt->type->func_ret, expr,
                               expr->range);
  } else {
    stmt->expr = NULL;
  }
  return stmt;
}

static Stmt *new_stmt_compound(Vector *stmts, const Range *range) {
  Stmt *stmt = new_stmt(ST_COMPOUND, range);
  stmt->stmts = stmts;
  return stmt;
}

static Expr *builtin_va_start_handler(Scope *scope, Expr *callee,
                                      Vector *argument, const Range *range) {
  return new_expr_builtin_va_start(scope, callee, argument, range);
}

static Expr *builtin_va_arg_handler(Scope *scope, Expr *callee,
                                    Vector *argument, const Range *range) {
  return new_expr_builtin_va_arg(scope, callee, argument, range);
}
static Expr *builtin_va_end_handler(Scope *scope, Expr *callee,
                                    Vector *argument, const Range *range) {
  return new_expr_builtin_va_end(scope, callee, argument, range);
}
static Expr *builtin_va_copy_handler(Scope *scope, Expr *callee,
                                     Vector *argument, const Range *range) {
  return new_expr_builtin_va_copy(scope, callee, argument, range);
}

static Number read_float(Token *token) {
  assert(token->ty == TK_NUM);
  char *suffix = NULL;
  long double val = strtold(token->num, &suffix);
  type_t type = TY_VOID;
  if (strcmp(suffix, "") == 0) {
    type = TY_DOUBLE;
  } else if (strcasecmp(suffix, "l") == 0) {
    type = TY_LDOUBLE;
  } else if (strcasecmp(suffix, "f") == 0) {
    type = TY_FLOAT;
  } else {
    range_error(token->range, "不正なサフィックスです");
  }
  return new_number_float(type, val);
}

static Number read_integer(Token *token) {
  typedef struct Suffix {
    const char *suffix;
    type_t type;
  } Suffix;
  Suffix SUFFIX[] = {
      // unsigned long long int
      {"ull", TY_U_LLONG},
      {"llu", TY_U_LLONG},
      {"uLL", TY_U_LLONG},
      {"LLu", TY_U_LLONG},
      {"Ull", TY_U_LLONG},
      {"llU", TY_U_LLONG},
      {"ULL", TY_U_LLONG},
      {"LLU", TY_U_LLONG},
      // unsigned long int
      {"ul", TY_U_LONG},
      {"lu", TY_U_LONG},
      {"uL", TY_U_LONG},
      {"Lu", TY_U_LONG},
      {"Ul", TY_U_LONG},
      {"lU", TY_U_LONG},
      {"UL", TY_U_LONG},
      {"LU", TY_U_LONG},
      // unsigned int
      {"u", TY_U_INT},
      {"U", TY_U_INT},
      // signed long long int
      {"ll", TY_S_LLONG},
      {"LL", TY_S_LLONG},
      // signed long int
      {"l", TY_S_LONG},
      {"L", TY_S_LONG},
      // stub
      {NULL, TY_VOID},
  };

  assert(token->ty == TK_NUM);
  char *suffix = NULL;
  unsigned long long val;
  if (strncasecmp("0b", token->num, 2) == 0) {
    // NonStandard/GNU: binary prefix integer literal
    val = strtoull(&token->num[2], &suffix, 2);
  } else {
    val = strtoull(token->num, &suffix, 0);
  }

  bool isbase10 = token->num[0] != '0';

  type_t ty = TY_VOID;
  if (strcmp(suffix, "") == 0) {
    // no suffix
    if (val <= INT_MAX) {
      ty = TY_S_INT;
    } else if (!isbase10 && val <= UINT_MAX) {
      ty = TY_U_INT;
    } else if (val <= LONG_MAX) {
      ty = TY_S_LONG;
    } else if (!isbase10 && val <= ULONG_MAX) {
      ty = TY_U_LONG;
    } else if (val <= LLONG_MAX) {
      ty = TY_S_LLONG;
    } else {
      assert(val <= ULLONG_MAX);
      ty = TY_U_LLONG;
    }
  } else {
    for (int i = 0; SUFFIX[i].suffix != NULL; i++) {
      if (strcmp(SUFFIX[i].suffix, suffix) == 0) {
        ty = SUFFIX[i].type;
        break;
      }
    }
    if (ty == TY_VOID) {
      range_error(token->range, "不正な整数のサフィックスです");
    }
  }

  return new_number(ty, val);
}

static Number read_number(Token *token) {
  assert(token->ty == TK_NUM);

  bool is_float =
      strpbrk(token->num, ".pP") ||
      (strncasecmp(token->num, "0x", 2) && strpbrk(token->num, "eE"));
  if (is_float) {
    return read_float(token);
  }
  return read_integer(token);
}

static Expr *primary_expression(Tokenizer *tokenizer, Scope *scope) {
  Token *token = NULL;
  if ((token = token_consume(tokenizer, TK_NUM)) != NULL) {
    return new_expr_num(read_number(token), token->range);
  }

  if ((token = token_consume(tokenizer, TK_CHARCONST)) != NULL) {
    return new_expr_num(token->char_val, token->range);
  }

  if ((token = token_consume(tokenizer, TK_IDENT)) != NULL) {
    return new_expr_ident(scope, token);
  }
  if ((token = token_consume(tokenizer, TK_STR)) != NULL) {
    return new_expr_str(scope, token->str, token->range);
  }

  if (token_consume(tokenizer, '(')) {
    Expr *expr = expression(tokenizer, scope);
    token_expect(tokenizer, ')');
    return expr;
  }

  range_error(token_peek(tokenizer)->range,
              "数値でも開きカッコでもないトークンです");
}

static Expr *postfix_expression(Tokenizer *tokenizer, Scope *scope) {
  Token *token;
  Expr *expr = primary_expression(tokenizer, scope);
  while (true) {
    if (token_consume(tokenizer, '[')) {
      Expr *operand = expression(tokenizer, scope);
      Token *end = token_expect(tokenizer, ']');
      expr = new_expr_index(scope, expr, operand,
                            range_join(expr->range, end->range));
      continue;
    }

    if (token_consume(tokenizer, '(')) {
      Vector *argument = NULL;
      if (token_peek(tokenizer)->ty != ')') {
        argument = argument_expression_list(tokenizer, scope);
      }
      Token *end = token_expect(tokenizer, ')');
      expr = new_expr_call(scope, expr, argument,
                           range_join(expr->range, end->range));
      continue;
    }

    if (token_consume(tokenizer, '.')) {
      Token *member = token_expect(tokenizer, TK_IDENT);
      expr = new_expr_dot(expr, member->ident,
                          range_join(expr->range, member->range));
      continue;
    }

    if (token_consume(tokenizer, TK_ARROW)) {
      Token *member = token_expect(tokenizer, TK_IDENT);
      expr = new_expr_arrow(scope, expr, member->ident,
                            range_join(expr->range, member->range));
      continue;
    }

    if ((token = token_consume(tokenizer, TK_INC)) != NULL) {
      expr = new_expr_postfix(scope, EX_POST_INC, expr,
                              range_join(expr->range, token->range));
      continue;
    }

    if ((token = token_consume(tokenizer, TK_DEC)) != NULL) {
      expr = new_expr_postfix(scope, EX_POST_DEC, expr,
                              range_join(expr->range, token->range));
      continue;
    }

    return expr;
  }
}

static Vector *argument_expression_list(Tokenizer *tokenizer, Scope *scope) {
  Vector *argument = new_vector();
  while (true) {
    vec_push(argument, assignment_expression(tokenizer, scope));
    if (!token_consume(tokenizer, ',')) {
      break;
    }
  }
  return argument;
}

static Expr *unary_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {'&', '*', '+', '-', '~', '!', TK_INC, TK_DEC, '\0'};
  const int EXS[] = {EX_ADDRESS, EX_INDIRECT, EX_PLUS,    EX_MINUS, EX_NOT,
                     EX_LOG_NOT, EX_PRE_INC,  EX_PRE_DEC, '\0'};
  for (int i = 0; TKS[i] != '\0'; i++) {
    int tk = TKS[i];
    int ex = EXS[i];
    Token *token;
    if ((token = token_consume(tokenizer, tk)) != NULL) {
      Expr *operand = cast_expression(tokenizer, scope);
      return new_expr_unary(scope, ex, operand,
                            range_join(token->range, operand->range));
    }
  }

  Token *token;
  if ((token = token_consume(tokenizer, TK_SIZEOF)) != NULL) {
    if (token_peek(tokenizer)->ty != '(' ||
        !token_is_type_name(scope, token_peek_ahead(tokenizer, 1))) {
      Expr *expr = unary_expression(tokenizer, scope);
      return new_expr_num(
          new_number_size_t(get_val_size(expr->val_type, expr->range)),
          range_join(token->range, expr->range));
    }
    token_expect(tokenizer, '(');
    Type *type = type_name(scope, tokenizer);
    Token *end = token_expect(tokenizer, ')');
    const Range *range = range_join(token->range, end->range);
    return new_expr_num(new_number_size_t(get_val_size(type, range)), range);
  }
  if ((token = token_consume(tokenizer, TK_ALIGNOF)) != NULL) {
    token_expect(tokenizer, '(');
    Type *type = type_name(scope, tokenizer);
    Token *end = token_expect(tokenizer, ')');
    const Range *range = range_join(token->range, end->range);
    return new_expr_num(new_number_size_t(get_val_align(type, range)), range);
  }
  return postfix_expression(tokenizer, scope);
}

static Expr *cast_expression(Tokenizer *tokenizer, Scope *scope) {
  Token *token = token_peek(tokenizer);
  if (token->ty == '(' &&
      token_is_type_name(scope, token_peek_ahead(tokenizer, 1))) {
    const Range *start = token->range;
    token_succ(tokenizer);
    Type *val_type = type_name(scope, tokenizer);
    token_expect(tokenizer, ')');
    if (token_peek(tokenizer)->ty == '{') {
      Initializer *init = NULL;
      const Range *range = NULL;
      initializer(tokenizer, scope, val_type, &init, &range);
      return new_expr_compound(scope, val_type, init, range);
    }
    Expr *operand = cast_expression(tokenizer, scope);
    return new_expr_cast(scope, val_type, operand,
                         range_join(start, operand->range));
  }
  return unary_expression(tokenizer, scope);
}

static Expr *binary_expression(Tokenizer *tokenizer, Scope *scope,
                               const int *tks, const int *exs,
                               Expr *(*op_parser)(Tokenizer *, Scope *)) {
  Expr *expr = op_parser(tokenizer, scope);
  bool found;
  do {
    found = false;
    for (int i = 0; tks[i] != '\0'; i++) {
      int tk = tks[i];
      int ex = exs[i];
      Token *token;
      if ((token = token_consume(tokenizer, tk)) != NULL) {
        Expr *operand = op_parser(tokenizer, scope);
        expr = new_expr_binop(scope, ex, expr, operand,
                              range_join(expr->range, operand->range));
        found = true;
        break;
      }
    }
  } while (found);
  return expr;
}

static Expr *multiplicative_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {'*', '/', '%', '\0'};
  const int EXS[] = {EX_MUL, EX_DIV, EX_MOD, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, cast_expression);
}
static Expr *additive_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {'+', '-', '\0'};
  const int EXS[] = {EX_ADD, EX_SUB, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS,
                           multiplicative_expression);
}
static Expr *shift_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {TK_LSHIFT, TK_RSHIFT, '\0'};
  const int EXS[] = {EX_LSHIFT, EX_RSHIFT, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, additive_expression);
}
static Expr *relational_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {'<', '>', TK_LTEQ, TK_GTEQ, '\0'};
  const int EXS[] = {EX_LT, EX_GT, EX_LTEQ, EX_GTEQ, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, shift_expression);
}
static Expr *equality_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {TK_EQEQ, TK_NOTEQ, '\0'};
  const int EXS[] = {EX_EQEQ, EX_NOTEQ, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, relational_expression);
}
static Expr *and_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {'&', '\0'};
  const int EXS[] = {EX_AND, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, equality_expression);
}
static Expr *exclusive_or_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {'^', '\0'};
  const int EXS[] = {EX_XOR, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, and_expression);
}
static Expr *inclusive_or_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {'|', '\0'};
  const int EXS[] = {EX_OR, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, exclusive_or_expression);
}
static Expr *logical_and_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {TK_LOGAND, '\0'};
  const int EXS[] = {EX_LOG_AND, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, inclusive_or_expression);
}
static Expr *logical_or_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {TK_LOGOR, '\0'};
  const int EXS[] = {EX_LOG_OR, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, logical_and_expression);
}
static Expr *conditional_expression(Tokenizer *tokenizer, Scope *scope) {
  Expr *cond = logical_or_expression(tokenizer, scope);
  if (token_consume(tokenizer, '?')) {
    Expr *then_expr = expression(tokenizer, scope);
    token_expect(tokenizer, ':');
    Expr *else_expr = conditional_expression(tokenizer, scope);
    return new_expr_cond(scope, cond, then_expr, else_expr,
                         range_join(cond->range, else_expr->range));
  }
  return cond;
}

static Expr *assignment_expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {
      '=',           TK_MUL_ASSIGN, TK_DIV_ASSIGN,    TK_MOD_ASSIGN,
      TK_ADD_ASSIGN, TK_SUB_ASSIGN, TK_LSHIFT_ASSIGN, TK_RSHIFT_ASSIGN,
      TK_AND_ASSIGN, TK_XOR_ASSIGN, TK_OR_ASSIGN,     '\0',
  };
  const int EXS[] = {
      EX_ASSIGN,     EX_MUL_ASSIGN, EX_DIV_ASSIGN,    EX_MOD_ASSIGN,
      EX_ADD_ASSIGN, EX_SUB_ASSIGN, EX_LSHIFT_ASSIGN, EX_RSHIFT_ASSIGN,
      EX_AND_ASSIGN, EX_XOR_ASSIGN, EX_OR_ASSIGN,     '\0',
  };

  Expr *lhs = conditional_expression(tokenizer, scope);
  for (int i = 0; TKS[i] != '\0'; i++) {
    int tk = TKS[i];
    int ex = EXS[i];
    if (token_consume(tokenizer, tk)) {
      Expr *rhs = assignment_expression(tokenizer, scope);
      const Range *range = range_join(lhs->range, rhs->range);
      return new_expr_binop(scope, ex, lhs, rhs, range);
    }
  }

  return lhs;
}

static Expr *expression(Tokenizer *tokenizer, Scope *scope) {
  const int TKS[] = {',', '\0'};
  const int EXS[] = {EX_COMMA, '\0'};
  return binary_expression(tokenizer, scope, TKS, EXS, assignment_expression);
}

Expr *constant_expression(Tokenizer *tokenizer, Scope *scope) {
  Expr *expr = conditional_expression(tokenizer, scope);
  sema_expr(expr);
  if (expr->ty != EX_NUM) {
    range_error(expr->range, "定数式ではありません");
  }
  return expr;
}

static Vector *declaration(Tokenizer *tokenizer, Scope *scope) {
  Vector *def_list = new_vector();

  StorageClassSpecifier scs = EMPTY_STORAGE_CLASS_SPECIFIER;
  Type *base_type = NULL;
  FunctionSpecifier fs = EMPTY_FUNCTION_SPECIFIER;

  declaration_specifiers(tokenizer, scope, &base_type, &scs, &fs);

  if (token_consume(tokenizer, ';')) {
    return def_list;
  }

  Token *name;
  Type *type;
  const Range *range;
  declarator(scope, tokenizer, base_type, &name, &type, &range);

  if (scs.is_typedef) {
    register_typedef(scope, name->ident, type);
  } else if (scs.is_extern) {
    register_extern(scope, name, type);
  } else {
    if (is_func_type(type)) {
      VarDef *def = register_func(scope, name, type);
      if (token_peek(tokenizer)->ty == '{') {
        def->func = function_definition(tokenizer, scope, type, name->ident,
                                        scs, fs, range);
        vec_push(def_list, def);
        return def_list;
      }
    } else {
      VarDef *def = register_var(scope, name, type, range, scs);
      if (token_consume(tokenizer, '=')) {
        initializer(tokenizer, scope, type, &def->init, NULL);
      }
      vec_push(def_list, def);
    }
  }

  while (token_consume(tokenizer, ',')) {
    declarator(scope, tokenizer, base_type, &name, &type, &range);
    if (scs.is_typedef) {
      register_typedef(scope, name->ident, type);
    } else if (scs.is_extern) {
      register_extern(scope, name, type);
    } else {
      if (is_func_type(type)) {
        (void)register_func(scope, name, type);
      } else {
        VarDef *def = register_var(scope, name, type, range, scs);
        if (token_consume(tokenizer, '=')) {
          initializer(tokenizer, scope, type, &def->init, NULL);
        }
        vec_push(def_list, def);
      }
    }
  }
  token_expect(tokenizer, ';');

  return def_list;
}

static void declaration_specifiers(Tokenizer *tokenizer, Scope *scope,
                                   Type **type, StorageClassSpecifier *scs,
                                   FunctionSpecifier *fs) {
  TypeSpecifier ts = EMPTY_TYPE_SPECIFIER;
  TypeQualifier tq = EMPTY_TYPE_QUALIFIER;
  while (true) {
    if (consume_storage_class_specifier(tokenizer, scs)) {
      continue;
    }
    if (consume_type_specifier(scope, tokenizer, &ts)) {
      continue;
    }
    if (consume_type_qualifier(tokenizer, &tq)) {
      continue;
    }
    if (consume_function_specifier(tokenizer, fs)) {
      continue;
    }
    break;
  }
  *type = construct_type_specifier(ts, tq);
  if (*type == NULL) {
    range_error(token_peek(tokenizer)->range, "型名がありません");
  }
}

static Type *struct_or_union_specifier(Scope *scope, Tokenizer *tokenizer,
                                       Token *token) {
  assert(token->ty == TK_STRUCT || token->ty == TK_UNION);
  Token *tag = token_consume(tokenizer, TK_IDENT);

  type_t ty = token->ty == TK_STRUCT ? TY_STRUCT : TY_UNION;

  if (token_consume(tokenizer, '{')) {
    Type *type;
    if (tag != NULL) {
      type = new_type_struct(ty, tag ? tag->ident : NULL, EMPTY_TYPE_QUALIFIER);
      if (!register_tag(scope, tag->ident, type)) {
        Type *predef_type = get_tag(scope, tag->ident);
        assert(predef_type != NULL);
        if (predef_type->ty != ty) {
          range_error(tag->range, "タグの種別が違います: 前回の定義: %d",
                      predef_type->ty);
        }
        StructBody *body = predef_type->struct_body;
        if (body->member_list != NULL) {
          range_error(tag->range, "構造体の多重定義です");
        }
        init_struct_body(body);

        type = predef_type;
      }
    } else {
      type = new_type_struct(ty, tag ? tag->ident : NULL, EMPTY_TYPE_QUALIFIER);
    }
    while (token_peek(tokenizer)->ty != '}') {
      struct_declaration(scope, tokenizer, type);
    }
    token_expect(tokenizer, '}');
    return type;
  }

  if (tag == NULL) {
    range_error(token->range,
                "構造体または共用体のタグまたは `{` がありません");
  }

  Type *type = get_tag(scope, tag->ident);
  if (type != NULL) {
    return type;
  }
  type = new_type_opaque_struct(ty, tag->ident, EMPTY_TYPE_QUALIFIER);
  register_tag(scope, tag->ident, type);
  return type;
}

static void struct_declaration(Scope *scope, Tokenizer *tokenizer, Type *type) {
  assert(type->ty == TY_STRUCT || type->ty == TY_UNION);

  Type *base_type = specifier_qualifier_list(scope, tokenizer);

  Token *member_name = NULL;
  Type *member_type = base_type;
  const Range *range = token_peek(tokenizer)->range;
  if (token_peek(tokenizer)->ty != ';') {
    declarator(scope, tokenizer, base_type, &member_name, &member_type, &range);
  }
  register_struct_member(type, member_name != NULL ? member_name->ident : NULL,
                         member_type, range);

  while (token_consume(tokenizer, ',')) {
    declarator(scope, tokenizer, base_type, &member_name, &member_type, &range);
    register_struct_member(type,
                           member_name != NULL ? member_name->ident : NULL,
                           member_type, range);
  }
  token_expect(tokenizer, ';');
}

static Type *specifier_qualifier_list(Scope *scope, Tokenizer *tokenizer) {
  TypeSpecifier ts = EMPTY_TYPE_SPECIFIER;
  TypeQualifier tq = EMPTY_TYPE_QUALIFIER;
  while (true) {
    if (consume_type_specifier(scope, tokenizer, &ts)) {
      continue;
    }
    if (consume_type_qualifier(tokenizer, &tq)) {
      continue;
    }
    break;
  }
  Type *type = construct_type_specifier(ts, tq);
  if (type == NULL) {
    range_error(token_peek(tokenizer)->range, "型名がありません");
  }
  return type;
}

static Type *enum_specifier(Scope *scope, Tokenizer *tokenizer, Token *token) {
  Token *tag_ident = token_consume(tokenizer, TK_IDENT);
  if (tag_ident == NULL && token_peek(tokenizer)->ty != '{') {
    range_error(token->range, "列挙型のタグまたは `{` がありません");
  }

  char *tag = tag_ident != NULL ? tag_ident->ident : NULL;

  if (token_consume(tokenizer, '{')) {
    Type *type = new_type_enum(tag != NULL ? tag : NULL, EMPTY_TYPE_QUALIFIER);
    if (tag != NULL) {
      if (!register_tag(scope, tag, type)) {
        range_error(tag_ident->range, "同じタグ名の列挙型の多重定義です: %s",
                    tag);
      }
    }
    int val = 0;
    while (token_peek(tokenizer)->ty != '}') {
      enumerator(scope, tokenizer, type, &val);
    }
    token_expect(tokenizer, '}');
    return type;
  }
  if (tag != NULL) {
    Type *type = get_tag(scope, tag);
    if (type != NULL) {
      return type;
    }
  }
  return new_type_enum(tag, EMPTY_TYPE_QUALIFIER);
}

static void enumerator(Scope *scope, Tokenizer *tokenizer, Type *type,
                       int *val) {
  Token *ident = token_expect(tokenizer, TK_IDENT);
  if (token_consume(tokenizer, '=')) {
    Expr *expr = constant_expression(tokenizer, scope);
    expr = new_expr_cast(scope, type, expr, expr->range);
    sema_expr(expr);
    if (expr->ty != EX_NUM) {
      range_error(expr->range, "列挙型の値が定数式の数値ではありません");
    }
    SET_NUMBER_VAL(*val, &expr->num);
  }
  token_consume(tokenizer, ',');

  Number *number = NEW(Number);
  number->type = TY_ENUM;
  number->enum_val = *val;
  register_number(scope, ident, type, number);

  (*val)++;
}

static void declarator_common(Scope *scope, Tokenizer *tokenizer,
                              declarator_type_t dec_type, Type *base_type,
                              Token **name, Type **type, const Range **range) {
  const Range *start = token_peek(tokenizer)->range;
  while (token_consume(tokenizer, '*')) {
    TypeQualifier tq = EMPTY_TYPE_QUALIFIER;
    consume_type_qualifier(tokenizer, &tq);
    base_type = new_type_ptr(base_type, tq);
  }
  const Range *end;
  direct_declarator_common(scope, tokenizer, dec_type, base_type, name, type,
                           &end);
  *range = range_join(start, end);
}

static void direct_declarator_common(Scope *scope, Tokenizer *tokenizer,
                                     declarator_type_t dec_type,
                                     Type *base_type, Token **name, Type **type,
                                     const Range **range) {
  Type *placeholder = NEW(Type);
  *range = token_peek(tokenizer)->range;

  while (true) {
    Token *token;
    if (dec_type != ABSTRACT_DECLARATOR &&
        (token = token_consume(tokenizer, TK_IDENT)) != NULL) {
      *name = token;
      *type = placeholder;
      *range = range_join(*range, token->range);
      break;
    }

    if (token_peek(tokenizer)->ty == '(') {
      Token *next = token_peek_ahead(tokenizer, 1);
      bool is_declarator = next->ty == '*';
      if (dec_type != CONCRETE_DECLARATOR && next->ty == '[') {
        is_declarator = true;
      }
      if (dec_type != ABSTRACT_DECLARATOR && !token_is_type_name(scope, next)) {
        is_declarator = true;
      }
      if (is_declarator) {
        token_succ(tokenizer);
        const Range *mid;
        declarator_common(scope, tokenizer, dec_type, placeholder, name, type,
                          &mid);
        Token *token = token_expect(tokenizer, ')');
        *range = range_join(*range, token->range);
        break;
      }
    }

    if (dec_type == CONCRETE_DECLARATOR) {
      range_error(token_peek(tokenizer)->range, "識別子がありません");
    }
    *name = NULL;
    *type = base_type;
    *range = *range;
    break;
  }

  while (true) {
    if (token_consume(tokenizer, '[')) {
      Type *inner = NEW(Type);
      TypeQualifier tq = EMPTY_TYPE_QUALIFIER;
      while (true) {
        if (consume_type_qualifier(tokenizer, &tq)) {
          continue;
        }
        if (token_consume(tokenizer, TK_STATIC)) {
          continue;
        }
        break;
      }
      if (token_consume(tokenizer, '*')) {
        *placeholder = *new_type_unsized_array(inner, tq);
      } else if (token_peek(tokenizer)->ty == ']') {
        *placeholder = *new_type_unsized_array(inner, tq);
      } else {
        Expr *len = constant_expression(tokenizer, scope);
        assert(len->ty == EX_NUM);
        *placeholder = *new_type_array(inner, len->num, tq);
      }
      placeholder = inner;
      Token *end = token_expect(tokenizer, ']');
      *range = range_join(*range, end->range);
      continue;
    }

    if (token_consume(tokenizer, '(')) {
      Vector *params;
      bool has_varargs = false;
      Token *end;
      if ((end = token_consume(tokenizer, ')'))) {
        params = NULL;
      } else if ((end = token_consume2(tokenizer, TK_VOID, ')'))) {
        params = new_vector();
      } else {
        params = new_vector();
        while (true) {
          Param *param = parameter_declaration(scope, tokenizer);
          if (is_array_type(param->type)) {
            // array型の引数はポインタ型とみなす
            Type *type = NEW(Type);
            *type = *param->type;
            type->ty = TY_PTR;
            param->type = type;
          }
          vec_push(params, param);
          if (token_peek(tokenizer)->ty == ')') {
            break;
          }
          token_expect(tokenizer, ',');
          if (token_consume(tokenizer, TK_ELIPSIS)) {
            has_varargs = true;
            break;
          }
        }
        end = token_expect(tokenizer, ')');
      }

      *range = range_join(*range, end->range);

      Type *inner = NEW(Type);
      *placeholder =
          *new_type_func(inner, params, has_varargs, EMPTY_TYPE_QUALIFIER);
      placeholder = inner;
      continue;
    }

    break;
  }

  *placeholder = *base_type;
}

static void declarator(Scope *scope, Tokenizer *tokenizer, Type *base_type,
                       Token **name, Type **type, const Range **range) {
  declarator_common(scope, tokenizer, CONCRETE_DECLARATOR, base_type, name,
                    type, range);
}

static Param *parameter_declaration(Scope *scope, Tokenizer *tokenizer) {
  StorageClassSpecifier scs = EMPTY_STORAGE_CLASS_SPECIFIER;
  Type *base_type = NULL;
  FunctionSpecifier fs = EMPTY_FUNCTION_SPECIFIER;

  declaration_specifiers(tokenizer, scope, &base_type, &scs, &fs);

  return parameter_declarator(scope, tokenizer, base_type);
}

static Param *parameter_declarator(Scope *scope, Tokenizer *tokenizer,
                                   Type *base_type) {
  Param *param = NEW(Param);
  declarator_common(scope, tokenizer, ANY_DECLARATOR, base_type, &param->name,
                    &param->type, &param->range);
  return param;
}

static Type *type_name(Scope *scope, Tokenizer *tokenizer) {
  Type *base_type = specifier_qualifier_list(scope, tokenizer);

  Type *type;
  const Range *range;
  abstract_declarator(scope, tokenizer, base_type, &type, &range);
  return type;
}

static void abstract_declarator(Scope *scope, Tokenizer *tokenizer,
                                Type *base_type, Type **type,
                                const Range **range) {
  Token *name;
  declarator_common(scope, tokenizer, ABSTRACT_DECLARATOR, base_type, &name,
                    type, range);
  assert(name == NULL);
}

static void struct_initializer(Tokenizer *tokenizer, Scope *scope, Type *type,
                               bool brace_root, Initializer **init) {
  assert(type->ty == TY_STRUCT);
  StructBody *body = type->struct_body;

  if (*init == NULL) {
    *init = new_initializer(type);
  }

  while (true) {
    int idx = 0;

    // initializer with designator
    if (token_peek(tokenizer)->ty == '.' &&
        token_peek_ahead(tokenizer, 1)->ty == TK_IDENT) {
      Token *ident = token_peek_ahead(tokenizer, 1);
      for (int i = 0; i < vec_len(body->member_list); i++) {
        Member *member = vec_get(body->member_list, i);
        if (member->name == NULL || strcmp(member->name, ident->ident) == 0) {
          if (member->name != NULL) {
            token_expect(tokenizer, '.');
            token_expect(tokenizer, TK_IDENT);
            token_consume(tokenizer, '=');
          }
          Token *current = token_peek(tokenizer);
          Initializer *meminit = map_get_by_index((*init)->members, i, NULL);
          // if name is null, try parsing designator as inner struct/union's
          // designator
          initializer_inner(tokenizer, scope, member->type, &meminit, NULL);
          if (token_peek(tokenizer) == current) {
            // if the designator is not found in inner struct/union, continue to
            // next member
            continue;
          }
          idx = i + 1;
          map_set_by_index((*init)->members, i, member->name, meminit);
          token_consume(tokenizer, ',');
          break;
        }
      }
    }

    // initializers without designator.
    if (token_peek(tokenizer)->ty != '.' && token_peek(tokenizer)->ty != '}') {
      for (int i = idx; i < vec_len(body->member_list); i++) {
        Initializer *meminit = map_get_by_index((*init)->members, i, NULL);
        Member *member = vec_get(body->member_list, i);
        initializer_inner(tokenizer, scope, member->type, &meminit, NULL);
        map_set_by_index((*init)->members, i, member->name, meminit);
        if ((i < vec_len(body->member_list) - 1 &&
             token_consume(tokenizer, ',') == NULL) ||
            (token_peek(tokenizer)->ty == '.')) {
          break;
        }
      }
    }

    if (!brace_root || token_peek(tokenizer)->ty != '.') {
      break;
    }
  }
}

static void union_initializer(Tokenizer *tokenizer, Scope *scope, Type *type,
                              bool brace_root, Initializer **init) {
  assert(type->ty == TY_UNION);
  StructBody *body = type->struct_body;

  if (*init == NULL) {
    *init = new_initializer(type);
  }

  while (true) {
    if (token_peek(tokenizer)->ty == '.' &&
        token_peek_ahead(tokenizer, 1)->ty == TK_IDENT) {
      Token *ident = token_peek_ahead(tokenizer, 1);
      for (int i = 0; i < vec_len(body->member_list); i++) {
        Member *member = vec_get(body->member_list, i);
        if (member->name == NULL || strcmp(member->name, ident->ident) == 0) {
          if (member->name != NULL) {
            token_expect(tokenizer, '.');
            token_expect(tokenizer, TK_IDENT);
            token_consume(tokenizer, '=');
          }
          Token *current = token_peek(tokenizer);
          Initializer *meminit = map_get_by_index((*init)->members, 0, NULL);
          // if name is null, try parsing designator as inner struct/union's
          // designator
          initializer_inner(tokenizer, scope, member->type, &meminit, NULL);
          if (token_peek(tokenizer) == current) {
            // if the designator is not found in inner struct/union, continue to
            // next member
            continue;
          }
          map_set_by_index((*init)->members, 0, member->name, meminit);
          token_consume(tokenizer, ',');
          break;
        }
      }
    } else {
      Initializer *meminit = map_get_by_index((*init)->members, 0, NULL);
      Member *member = vec_get(body->member_list, 0);
      initializer_inner(tokenizer, scope, member->type, &meminit, NULL);
      map_set_by_index((*init)->members, 0, member->name, meminit);
      break;
    }

    if (!brace_root || token_peek(tokenizer)->ty != '.') {
      break;
    }
  }
}

static void array_initializer(Tokenizer *tokenizer, Scope *scope, Type *type,
                              bool brace_root, Initializer **init) {
  assert(type->ty == TY_ARRAY);
  if (*init == NULL) {
    *init = new_initializer(type);
  }

  Token *str;
  if ((str = token_consume(tokenizer, TK_STR)) != NULL) {
    if (type->ptrof->ty != TY_CHAR && type->ptrof->ty != TY_S_CHAR &&
        type->ptrof->ty != TY_U_CHAR) {
      range_error(str->range,
                  "文字型以外の配列を文字列リテラルで初期化できません");
    }
    if (type->array_len < 0) {
      type->array_len = strlen(str->str) + 1;
      vec_extend((*init)->elements, type->array_len);
    }
    for (int i = 0; i < type->array_len; i++) {
      Initializer *eleminit = new_initializer(type->ptrof);
      eleminit->expr =
          new_expr_num(new_number(type->ptrof->ty, str->str[i]), str->range);
      vec_set((*init)->elements, i, eleminit);
      if (str->str[i] == '\0') {
        break;
      }
    }
    return;
  }

  if (token_peek(tokenizer)->ty == '}') {
    if (type->array_len < 0) {
      type->array_len = vec_len((*init)->elements);
    }
    return;
  }

  while (true) {
    int idx = 0;

    if (token_consume(tokenizer, '[')) {
      Expr *idx_expr = constant_expression(tokenizer, scope);
      assert(idx_expr->ty == EX_NUM);
      token_expect(tokenizer, ']');
      token_consume(tokenizer, '=');
      int i;
      SET_NUMBER_VAL(i, &idx_expr->num);
      if (type->array_len < 0) {
        vec_extend((*init)->elements, i + 1);
      } else {
        if (i >= type->array_len) {
          range_error(idx_expr->range,
                      "配列サイズを超過するインデックスです: %d", i);
        }
      }
      Initializer *eleminit = vec_get((*init)->elements, i);
      initializer_inner(tokenizer, scope, type->ptrof, &eleminit, NULL);
      vec_set((*init)->elements, i, eleminit);
      token_consume(tokenizer, ',');
      idx = i + 1;
    }

    if (token_peek(tokenizer)->ty != '[' && token_peek(tokenizer)->ty != '}') {
      int max_len = type->array_len < 0 ? INT_MAX : type->array_len;
      for (int i = idx; i < max_len; i++) {
        if (token_peek(tokenizer)->ty == '}') {
          break;
        }
        vec_extend((*init)->elements, i + 1);
        Initializer *eleminit = vec_get((*init)->elements, i);
        initializer_inner(tokenizer, scope, type->ptrof, &eleminit, NULL);
        vec_set((*init)->elements, i, eleminit);
        if ((i < max_len - 1 && token_consume(tokenizer, ',') == NULL)) {
          break;
        }
        if (token_peek(tokenizer)->ty == '[' ||
            token_peek(tokenizer)->ty == '.') {
          break;
        }
      }
      if (token_peek(tokenizer)->ty != '[') {
        break;
      }
    }

    if (!brace_root || token_peek(tokenizer)->ty != '[') {
      break;
    }
  }

  if (type->array_len < 0) {
    type->array_len = vec_len((*init)->elements);
  }
}

static void initializer_inner(Tokenizer *tokenizer, Scope *scope, Type *type,
                              Initializer **init, const Range **range) {
  Token *token;
  if ((token = token_consume(tokenizer, '{')) != NULL) {
    const Range *start = token->range;
    switch (type->ty) {
    case TY_BOOL:
    case TY_CHAR:
    case TY_S_CHAR:
    case TY_S_INT:
    case TY_S_SHORT:
    case TY_S_LONG:
    case TY_S_LLONG:
    case TY_U_CHAR:
    case TY_U_INT:
    case TY_U_SHORT:
    case TY_U_LONG:
    case TY_U_LLONG:
    case TY_FLOAT:
    case TY_DOUBLE:
    case TY_LDOUBLE:
    case TY_PTR:
    case TY_ENUM:
      initializer(tokenizer, scope, type, init, NULL);
      break;
    case TY_STRUCT:
      struct_initializer(tokenizer, scope, type, true, init);
      break;
    case TY_UNION:
      union_initializer(tokenizer, scope, type, true, init);
      break;
    case TY_ARRAY:
      array_initializer(tokenizer, scope, type, true, init);
      break;
    case TY_VOID:
    case TY_FUNC:
    case TY_BUILTIN:
      range_error(token->range, "初期化できない型です: %s",
                  format_type(type, false));
    }
    (void)token_consume(tokenizer, ',');
    const Range *end = token_expect(tokenizer, '}')->range;
    if (range != NULL) {
      *range = range_join(start, end);
    }
    return;
  };

  if (type->ty == TY_STRUCT) {
    struct_initializer(tokenizer, scope, type, false, init);
    return;
  }
  if (type->ty == TY_UNION) {
    union_initializer(tokenizer, scope, type, false, init);
    return;
  }
  if (type->ty == TY_ARRAY) {
    array_initializer(tokenizer, scope, type, false, init);
    return;
  }

  {
    *init = new_initializer(type);
    Expr *expr = assignment_expression(tokenizer, scope);
    (*init)->expr = new_expr_cast(scope, type, expr, expr->range);
    if (range != NULL) {
      *range = expr->range;
    }
  }
}

static void initializer(Tokenizer *tokenizer, Scope *scope, Type *type,
                        Initializer **init, const Range **range) {
  if (token_peek(tokenizer)->ty == '{') {
    initializer_inner(tokenizer, scope, type, init, range);
    return;
  };

  if (type->ty == TY_ARRAY && token_peek(tokenizer)->ty == TK_STR) {
    initializer_inner(tokenizer, scope, type, init, range);
    return;
  }

  {
    *init = new_initializer(type);
    Expr *expr = assignment_expression(tokenizer, scope);
    (*init)->expr = new_expr_cast(scope, type, expr, expr->range);
    if (range != NULL) {
      *range = expr->range;
    }
  }
}

static Stmt *statement(Tokenizer *tokenizer, Scope *scope) {
  Token *start = token_peek(tokenizer);
  switch (start->ty) {
  case TK_IF: {
    token_succ(tokenizer);
    token_expect(tokenizer, '(');
    Expr *cond = expression(tokenizer, scope);
    token_expect(tokenizer, ')');
    Stmt *then_stmt = statement(tokenizer, scope);
    Stmt *else_stmt = new_stmt(ST_NULL, then_stmt->range);
    const Range *range;
    if (token_consume(tokenizer, TK_ELSE)) {
      else_stmt = statement(tokenizer, scope);
      range = range_join(start->range, else_stmt->range);
    } else {
      range = range_join(start->range, then_stmt->range);
    }
    return new_stmt_if(cond, then_stmt, else_stmt, range);
  }
  case TK_SWITCH: {
    token_succ(tokenizer);
    token_expect(tokenizer, '(');
    Expr *cond = expression(tokenizer, scope);
    token_expect(tokenizer, ')');
    Stmt *stmt = new_stmt_switch(cond, NULL, start->range);
    vec_push(scope->func_ctxt->switches, stmt);
    stmt->body = statement(tokenizer, scope);
    stmt->range = range_join(stmt->range, stmt->body->range);
    vec_pop(scope->func_ctxt->switches);
    return stmt;
  }
  case TK_CASE: {
    token_succ(tokenizer);
    Expr *expr = constant_expression(tokenizer, scope);
    token_expect(tokenizer, ':');
    Stmt *body = statement(tokenizer, scope);
    Stmt *stmt =
        new_stmt_case(expr, body, range_join(start->range, body->range));
    if (vec_len(scope->func_ctxt->switches) <= 0) {
      range_error(stmt->range, "switch文中でない箇所にcase文があります");
    }
    Stmt *switch_stmt = vec_last(scope->func_ctxt->switches);
    vec_push(switch_stmt->cases, stmt);
    return stmt;
  }
  case TK_DEFAULT: {
    token_succ(tokenizer);
    token_expect(tokenizer, ':');
    Stmt *body = statement(tokenizer, scope);
    Stmt *stmt = new_stmt_default(body, range_join(start->range, body->range));
    if (vec_len(scope->func_ctxt->switches) <= 0) {
      range_error(stmt->range, "switch文中でない箇所にcase文があります");
    }
    Stmt *switch_expr = vec_last(scope->func_ctxt->switches);
    switch_expr->default_case = stmt;
    return stmt;
  }
  case TK_WHILE: {
    token_succ(tokenizer);
    token_expect(tokenizer, '(');
    Expr *cond = expression(tokenizer, scope);
    token_expect(tokenizer, ')');
    Stmt *body = statement(tokenizer, scope);
    return new_stmt_while(cond, body, range_join(start->range, body->range));
  }
  case TK_DO: {
    token_succ(tokenizer);
    Stmt *body = statement(tokenizer, scope);
    token_expect(tokenizer, TK_WHILE);
    token_expect(tokenizer, '(');
    Expr *cond = expression(tokenizer, scope);
    token_expect(tokenizer, ')');
    Token *end = token_expect(tokenizer, ';');
    return new_stmt_do_while(cond, body, range_join(start->range, end->range));
  }
  case TK_FOR: {
    Scope *inner = new_inner_scope(scope);
    token_succ(tokenizer);
    Expr *init = NULL;
    Expr *cond = NULL;
    Expr *inc = NULL;
    token_expect(tokenizer, '(');
    if (!token_consume(tokenizer, ';')) {
      if (token_is_declaration_specifiers(inner, token_peek(tokenizer))) {
        Vector *def_list = declaration(tokenizer, inner);
        for (int i = 0; i < vec_len(def_list); i++) {
          VarDef *def = vec_get(def_list, i);
          switch (def->type) {
          case DEF_FUNC:
            range_error(def->name->range, "関数内で関数は定義できません");
            break;
          case DEF_GLOBAL_VAR:
            range_error(def->name->range, "ローカル変数ではありません");
          case DEF_STACK_VAR: {
            if (def->init == NULL) {
              continue;
            }
            Expr *dest = new_expr_ident(inner, def->name);
            gen_init(inner, &init, def->init, dest, dest->val_type);
          }
          }
        }
      } else {
        init = expression(tokenizer, inner);
        token_expect(tokenizer, ';');
      }
    }
    if (!token_consume(tokenizer, ';')) {
      cond = expression(tokenizer, inner);
      token_expect(tokenizer, ';');
    }
    if (!token_consume(tokenizer, ')')) {
      inc = expression(tokenizer, inner);
      token_expect(tokenizer, ')');
    }
    Stmt *body = statement(tokenizer, inner);
    return new_stmt_for(init, cond, inc, body,
                        range_join(start->range, body->range));
  }
  case TK_GOTO: {
    token_succ(tokenizer);
    char *name = token_expect(tokenizer, TK_IDENT)->ident;
    Token *end = token_expect(tokenizer, ';');
    return new_stmt_goto(name, range_join(start->range, end->range));
  }
  case TK_BREAK: {
    token_succ(tokenizer);
    Token *end = token_expect(tokenizer, ';');
    return new_stmt(ST_BREAK, range_join(start->range, end->range));
  }
  case TK_CONTINUE: {
    token_succ(tokenizer);
    Token *end = token_expect(tokenizer, ';');
    return new_stmt(ST_CONTINUE, range_join(start->range, end->range));
  }
  case TK_RETURN: {
    token_succ(tokenizer);
    Expr *expr = NULL;
    if (token_peek(tokenizer)->ty != ';') {
      expr = expression(tokenizer, scope);
    }
    Token *end = token_expect(tokenizer, ';');
    return new_stmt_return(scope, expr, range_join(start->range, end->range));
  }
  case '{': {
    Scope *inner = new_inner_scope(scope);
    return compound_statement(tokenizer, inner);
  }
  case ';': {
    Token *end = token_pop(tokenizer);
    return new_stmt(ST_NULL, end->range);
  }
  case TK_IDENT: {
    if (token_peek_ahead(tokenizer, 1)->ty == ':') {
      Token *ident = token_pop(tokenizer);
      token_expect(tokenizer, ':');
      Stmt *body = statement(tokenizer, scope);
      Stmt *stmt = new_stmt_label(scope->func_ctxt, ident->ident, body,
                                  range_join(start->range, body->range));
      return stmt;
    }
  }
  // fall through
  default: {
    Expr *expr = expression(tokenizer, scope);
    Token *end = token_expect(tokenizer, ';');
    return new_stmt_expr(expr, range_join(expr->range, end->range));
  }
  }
}

static void gen_init(Scope *scope, Expr **expr, Initializer *init, Expr *dest,
                     Type *type) {
  switch (type->ty) {
  case TY_BOOL:
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_S_SHORT:
  case TY_S_INT:
  case TY_S_LONG:
  case TY_S_LLONG:
  case TY_U_CHAR:
  case TY_U_SHORT:
  case TY_U_INT:
  case TY_U_LONG:
  case TY_U_LLONG:
  case TY_FLOAT:
  case TY_DOUBLE:
  case TY_LDOUBLE:
  case TY_PTR:
  case TY_ENUM: {
    Expr *assign = new_expr_binop(
        scope, EX_ASSIGN, dest,
        init != NULL ? init->expr
                     : new_expr_num(new_number_int(0), dest->range),
        init != NULL ? range_join(init->expr->range, dest->range)
                     : dest->range);
    if (*expr != NULL) {
      *expr = new_expr_binop(scope, EX_COMMA, *expr, assign,
                             range_join((*expr)->range, assign->range));
    } else {
      *expr = assign;
    }
    return;
  }
  case TY_ARRAY: {
    for (int i = 0; i < type->array_len; i++) {
      Expr *index = new_expr_num(new_number_int(i), dest->range);
      Initializer *eleminit = init != NULL ? vec_get(init->elements, i) : NULL;
      Expr *elem = new_expr_index(scope, dest, index, dest->range);
      gen_init(scope, expr, eleminit, elem, type->ptrof);
    }
    return;
  }
  case TY_STRUCT: {
    StructBody *body = type->struct_body;
    for (int i = 0; i < vec_len(body->member_list); i++) {
      Member *member = vec_get(body->member_list, i);
      const char *name = member->name;
      Initializer *meminit =
          init != NULL ? map_get_by_index(init->members, i, &name) : NULL;
      Expr *mem = name != NULL ? new_expr_dot(dest, name, dest->range) : dest;
      gen_init(scope, expr, meminit, mem, member->type);
    }
    return;
  }
  case TY_UNION: {
    StructBody *body = type->struct_body;
    if (init != NULL) {
      for (int i = 0; i < map_size(init->members); i++) {
        const char *name;
        Initializer *meminit = map_get_by_index(init->members, i, &name);
        Expr *mem = name != NULL ? new_expr_dot(dest, name, dest->range) : dest;
        Type *type = meminit->type;
        gen_init(scope, expr, meminit, mem, type);
      }
    } else {
      if (vec_len(body->member_list) > 0) {
        Member *member = vec_get(body->member_list, 0);
        Expr *mem = member->name != NULL
                        ? new_expr_dot(dest, member->name, dest->range)
                        : dest;
        Type *type = member->type;
        gen_init(scope, expr, NULL, mem, type);
      }
    }
    return;
  }
  case TY_VOID:
  case TY_FUNC:
  case TY_BUILTIN:
    break;
  }

  range_error(dest->range, "不正な型の初貴化です: %s",
              format_type(type, false));
}

static Stmt *compound_statement(Tokenizer *tokenizer, Scope *scope) {
  Token *start = token_expect(tokenizer, '{');

  const Range *range = start->range;
  Vector *stmts = new_vector();
  while (!token_consume(tokenizer, '}')) {
    Token *token = token_peek(tokenizer);
    if ((token->ty != TK_IDENT || token_peek_ahead(tokenizer, 1)->ty != ':') &&
        (token_is_declaration_specifiers(scope, token))) {

      Vector *def_list = declaration(tokenizer, scope);
      for (int i = 0; i < vec_len(def_list); i++) {
        VarDef *def = vec_get(def_list, i);
        switch (def->type) {
        case DEF_FUNC:
          range_error(def->name->range, "関数内で関数は定義できません");
        case DEF_GLOBAL_VAR:
          def->global_var->init = def->init;
          vec_push(scope->global_ctxt->gvar_list, def->global_var);
          break;
        case DEF_STACK_VAR: {
          if (def->init == NULL) {
            continue;
          }

          Expr *dest = new_expr_ident(scope, def->name);
          Expr *expr = NULL;
          gen_init(scope, &expr, def->init, dest, dest->val_type);
          if (expr != NULL) {
            vec_push(stmts, new_stmt_expr(expr, expr->range));
          }
          break;
        }
        }
      }

      continue;
    }

    Stmt *s = statement(tokenizer, scope);
    range = range_join(range, s->range);
    vec_push(stmts, s);
  }
  return new_stmt_compound(stmts, range);
}

static Function *function_definition(Tokenizer *tokenizer, Scope *global_scope,
                                     Type *type, char *name,
                                     StorageClassSpecifier scs,
                                     FunctionSpecifier fs, const Range *start) {
  FuncCtxt *fcx = new_func_ctxt(name, type);
  Scope *scope = new_func_scope(global_scope, fcx);
  if (type->func_param != NULL) {
    for (int i = 0; i < vec_len(type->func_param); i++) {
      Param *param = vec_get(type->func_param, i);
      param->stack_var =
          register_stack_var(scope, param->name, param->type, param->range);
    }
  }

  Stmt *body = compound_statement(tokenizer, scope);

  Function *func = NEW(Function);
  func->name = name;
  func->type = type;
  func->storage_class = scs;
  func->func = fs;
  func->range = range_join(start, body->range);
  func->var_list = fcx->var_list;
  func->label_map = fcx->label_map;
  func->body = body;

  return func;
}

static GlobalVar *new_global_variable(Type *type, const char *name,
                                      const Range *range,
                                      StorageClassSpecifier scs) {
  GlobalVar *gvar = NEW(GlobalVar);
  gvar->type = type;
  gvar->name = name;
  gvar->range = range;
  gvar->storage_class = scs;
  gvar->init = NULL;
  return gvar;
}

static TranslationUnit *translation_unit(Tokenizer *tokenizer) {
  GlobalCtxt *gcx = new_global_ctxt();
  Scope *scope = new_global_scope(gcx, tokenizer);

  Map *gvar_map = new_map();

  while (token_peek(tokenizer)->ty != TK_EOF) {
    Vector *def_list = declaration(tokenizer, scope);
    for (int i = 0; i < vec_len(def_list); i++) {
      VarDef *def = vec_get(def_list, i);
      switch (def->type) {
      case DEF_FUNC:
        vec_push(gcx->func_list, def->func);
        break;
      case DEF_GLOBAL_VAR: {
        GlobalVar *prev_def = map_get(gvar_map, def->name->ident);
        if (prev_def == NULL) {
          def->global_var->init = def->init;
          vec_push(gcx->gvar_list, def->global_var);
          map_put(gvar_map, def->name->ident, def->global_var);
        } else {
          if (def->init != NULL) {
            if (prev_def->init != NULL) {
              range_error(def->global_var->range,
                          "グローバル変数が複数回定義されました: %s",
                          def->name->ident);
            }
            prev_def->init = def->init;
          }
        }
        break;
      }
      case DEF_STACK_VAR:
        abort();
      }
      if (def->init == NULL) {
        continue;
      }
    }
  }

  TranslationUnit *tunit = NEW(TranslationUnit);
  tunit->func_list = gcx->func_list;
  tunit->gvar_list = gcx->gvar_list;
  tunit->str_list = gcx->str_list;
  return tunit;
}
