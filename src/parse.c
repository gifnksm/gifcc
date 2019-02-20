#include "gifcc.h"
#include <assert.h>
#include <limits.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

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

typedef struct Decl {
  Type *type;
  StackVar *stack_var;
  GlobalVar *global_var;
  Number *num_val;
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

static Initializer *new_initializer(Type *type);
static GlobalCtxt *new_global_ctxt(void);
static FuncCtxt *new_func_ctxt(char *name, Type *type);
static Scope *new_scope(GlobalCtxt *gcx, FuncCtxt *fcx, Scope *outer);
static Scope *new_global_scope(GlobalCtxt *gcx);
static Scope *new_func_scope(Scope *global, FuncCtxt *fcx);
static Scope *new_inner_scope(Scope *outer);
static Member *new_member(char *name, Type *type, int offset, Range range);
static VarDef *register_var(Scope *scope, Token *name, Type *type, Range range,
                            bool is_static);
static VarDef *register_func(Scope *scope, Token *name, Type *type);
static StackVar *register_stack_var(Scope *scope, Token *name, Type *type,
                                    Range range);
static GlobalVar *register_global_var(Scope *scope, Token *name, Type *type,
                                      Range range, bool is_static);
static void register_member(Type *type, char *member_name, Type *member_type,
                            Range range);
static bool register_decl(Scope *scope, char *name, Type *type, StackVar *svar,
                          GlobalVar *gvar, Number *num_val);
static Decl *get_decl(Scope *scope, char *name);
static bool register_tag(Scope *scope, char *tag, Type *type);
static Type *get_tag(Scope *scope, char *tag);
static bool register_typedef(Scope *scope, char *name, Type *type);
static Type *get_typedef(Scope *scope, char *name);
static bool is_sametype(Type *ty1, Type *ty2);
static bool is_integer_type(Type *ty);
static int get_int_type_rank(Type *ty, Range range);

static bool is_arith_type(Type *ty);
static bool is_ptr_type(Type *ty);
static bool is_array_type(Type *ty);
static Type *integer_promoted(Scope *scope, Expr **e);
static Type *arith_converted(Scope *scope, Expr **e1, Expr **e2);
static bool token_is_typename(Scope *scope, Token *token);
static bool token_is_storage_class_specifier(Token *token);
static noreturn void binop_type_error_raw(int ty, Expr *lhs, Expr *rhs,
                                          const char *dbg_file, int dbg_line);
static Type *new_type(int ty, bool is_const);
static Type *new_type_ptr(Type *base_type, bool is_const);
static Type *new_type_array(Type *base_type, Number len, bool is_const);
static Type *new_type_unsized_array(Type *base_type, bool is_const);
static Type *new_type_func(Type *ret_type, Vector *func_param, bool has_varargs,
                           bool is_const);
static Type *new_type_struct(int tk, char *tag, bool is_const);
static Type *new_type_anon_struct(int tk, bool is_const);
static Type *new_type_enum(char *tag, bool is_const);
static Expr *coerce_array2ptr(Scope *scope, Expr *expr);
static Expr *coerce_func2ptr(Scope *scope, Expr *expr);
static Expr *new_expr(int ty, Type *val_type, Range range);
static Expr *new_expr_num(Number val, Range range);
static Expr *new_expr_ident(Scope *scope, char *name, Range range);
static Expr *new_expr_str(Scope *scope, char *val, Range range);
static Expr *new_expr_call(Scope *scope, Expr *callee, Vector *argument,
                           Range range);
static Expr *new_expr_postfix(Scope *scope, int ty, Expr *operand, Range range);
static Expr *new_expr_cast(Scope *scope, Type *val_type, Expr *operand,
                           Range range);
static Number eval_unaryop(int op, type_t type, Number na, Range range);
static Expr *new_expr_unary(Scope *scope, int op, Expr *operand, Range range);
static Number eval_binop(int op, type_t type, Number na, Number nb,
                         Range range);
static Number eval_binop_shift(int op, type_t type, Number na, Number nb,
                               Range range);
static Expr *new_expr_binop(Scope *scope, int op, Expr *lhs, Expr *rhs,
                            Range range);
static Expr *new_expr_cond(Scope *scope, Expr *cond, Expr *then_expr,
                           Expr *else_expr, Range range);
static Expr *new_expr_index(Scope *scope, Expr *array, Expr *index,
                            Range range);
static Expr *new_expr_dot(Scope *scope, Expr *operand, char *name, Range range);
static Expr *new_expr_arrow(Scope *scope, Expr *operand, char *name,
                            Range range);
static Stmt *new_stmt(int ty, Range range);
static Stmt *new_stmt_expr(Expr *expr, Range range);
static Stmt *new_stmt_if(Expr *cond, Stmt *then_stmt, Stmt *else_stmt,
                         Range range);
static Stmt *new_stmt_switch(Expr *cond, Stmt *body, Range range);
static Stmt *new_stmt_case(Expr *expr, Stmt *body, Range range);
static Stmt *new_stmt_default(Stmt *body, Range range);
static Stmt *new_stmt_label(FuncCtxt *fcx, char *name, Stmt *body, Range range);
static Stmt *new_stmt_while(Expr *cond, Stmt *body, Range range);
static Stmt *new_stmt_do_while(Expr *cond, Stmt *body, Range range);
static Stmt *new_stmt_for(Expr *init, Expr *cond, Expr *inc, Stmt *body,
                          Range range);
static Stmt *new_stmt_goto(char *name, Range range);
static Stmt *new_stmt_return(Scope *scope, Expr *expr, Range range);
static Stmt *new_stmt_compound(Vector *stmts, Range range);

// expression
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
static Type *type_specifier(Scope *scope, Tokenizer *tokenizer);
static void struct_declaration(Scope *scope, Tokenizer *tokenizer, Type *type);
static Type *struct_or_union_specifier(Scope *scope, Tokenizer *tokenizer,
                                       Token *token, bool is_const);
static void enumerator(Scope *scope, Tokenizer *tokenizer, Type *type,
                       int *val);
static Type *enum_specifier(Scope *scope, Tokenizer *tokenizer, Token *token,
                            bool is_const);
static Type *type_name(Scope *scope, Tokenizer *tokenizer);
static void declarator(Scope *scope, Tokenizer *tokenizer, Type *base_type,
                       Token **name, Type **type, Range *range);
static void direct_declarator(Scope *scope, Tokenizer *tokenizer,
                              Type *base_type, Token **name, Type **type,
                              Range *range);
static void initializer(Tokenizer *tokenizer, Scope *scope, Type *type,
                        Initializer **init);

// statement
static Stmt *statement(Tokenizer *tokenizer, Scope *scope);
static void gen_init(Scope *scope, Vector *stmts, Initializer *init,
                     Expr *dest);
static Stmt *compound_statement(Tokenizer *tokenizer, Scope *scope);

// top-level
static Function *function_definition(Tokenizer *tokenizer, Scope *global_scope,
                                     Type *type, char *name, bool is_static,
                                     Range start);
static GlobalVar *new_global_variable(Type *type, char *name, Range range,
                                      bool is_static);
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
  case TY_STRUCT:
    init->members = new_map();
    for (int i = 0; i < vec_len(type->member_list); i++) {
      Member *member = vec_get(type->member_list, i);
      map_put(init->members, member->name, NULL);
    }
    break;
  case TY_UNION:
    init->members = new_map();
    if (vec_len(type->member_list) > 0) {
      map_put(init->members, NULL, NULL);
    }
    break;
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

static Scope *new_global_scope(GlobalCtxt *gcx) {
  return new_scope(gcx, NULL, NULL);
}

static Scope *new_func_scope(Scope *global, FuncCtxt *fcx) {
  return new_scope(global->global_ctxt, fcx, global);
}

static Scope *new_inner_scope(Scope *outer) {
  return new_scope(outer->global_ctxt, outer->func_ctxt, outer);
}

Scope *new_pp_scope(void) {
  GlobalCtxt *gcx = new_global_ctxt();
  return new_global_scope(gcx);
}

static Member *new_member(char *name, Type *type, int offset, Range range) {
  if (type->ty == TY_VOID) {
    range_error(range, "void型のメンバーです: %s", name);
  }
  Member *member = NEW(Member);
  member->name = name;
  member->type = type;
  member->offset = offset;
  return member;
}

static VarDef *register_var(Scope *scope, Token *name, Type *type, Range range,
                            bool is_static) {
  def_type_t ty;
  StackVar *svar = NULL;
  GlobalVar *gvar = NULL;
  if (scope->func_ctxt != NULL) {
    if (!is_static) {
      ty = DEF_STACK_VAR;
      svar = register_stack_var(scope, name, type, range);
    } else {
      char buf[256];
      sprintf(buf, "%s.%s", scope->func_ctxt->name, name->name);
      ty = DEF_GLOBAL_VAR;
      gvar = register_global_var(scope, name, type, range, is_static);
      gvar->name = make_label(buf);
    }
  } else {
    ty = DEF_GLOBAL_VAR;
    gvar = register_global_var(scope, name, type, range, is_static);
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
  (void)register_decl(scope, name->name, type, NULL, NULL, NULL);

  VarDef *def = NEW(VarDef);
  def->type = DEF_FUNC;
  def->name = name;
  def->init = NULL;
  def->stack_var = NULL;
  def->global_var = NULL;
  def->func = NULL;

  return def;
}

static StackVar *register_stack_var(Scope *scope, Token *name, Type *type,
                                    Range range) {
  if (type->ty == TY_VOID) {
    range_error(range, "void型の変数は定義できません: %s", name->name);
  }

  FuncCtxt *fcx = scope->func_ctxt;

  StackVar *var = NEW(StackVar);
  var->name = name->name;
  var->offset = INT_MIN; // Initialize with invalid value
  var->type = type;
  var->range = range;
  vec_push(fcx->var_list, var);

  if (!register_decl(scope, name->name, type, var, NULL, NULL)) {
    range_error(range, "同じ名前のローカル変数が複数あります: %s", name->name);
  }

  return var;
}

static GlobalVar *register_global_var(Scope *scope, Token *name, Type *type,
                                      Range range, bool is_static) {
  GlobalVar *gvar = new_global_variable(type, name->name, range, is_static);
  if (!register_decl(scope, name->name, type, NULL, gvar, NULL)) {
    Decl *decl = get_decl(scope, name->name);
    if (decl->global_var == NULL) {
      decl->global_var = gvar;
    } else {
      gvar = decl->global_var;
    }
  }
  return gvar;
}

static void register_member(Type *type, char *member_name, Type *member_type,
                            Range range) {
  assert(type->ty == TY_STRUCT || type->ty == TY_UNION);

  int offset;
  if (type->ty == TY_STRUCT) {
    type->member_size =
        align(type->member_size, get_val_align(member_type, range));
    offset = type->member_size;
    type->member_size += get_val_size(member_type, range);
  } else {
    if (get_val_size(member_type, range) > type->member_size) {
      type->member_size = get_val_size(member_type, range);
    }
    offset = 0;
  }

  if (type->member_align < get_val_align(member_type, range)) {
    type->member_align = get_val_align(member_type, range);
  }

  Member *member = new_member(member_name, member_type, offset, range);
  vec_push(type->member_list, member);
  if (member_name == NULL) {
    assert(member_type->ty == TY_STRUCT || member_type->ty == TY_UNION);
    Map *inner_members = member_type->member_name_map;
    for (int i = 0; i < map_size(inner_members); i++) {
      Member *inner = map_get_by_index(inner_members, i, NULL);
      Member *inner_member =
          new_member(inner->name, inner->type, offset + inner->offset, range);
      if (map_get(type->member_name_map, inner_member->name)) {
        range_error(range, "同じ名前のメンバ変数が複数あります: %s",
                    inner_member->name);
      }
      map_put(type->member_name_map, inner_member->name, inner_member);
    }
  } else {
    if (map_get(type->member_name_map, member->name)) {
      range_error(range, "同じ名前のメンバ変数が複数あります: %s",
                  member->name);
    }
    map_put(type->member_name_map, member->name, member);
  }
}

static bool register_decl(Scope *scope, char *name, Type *type, StackVar *svar,
                          GlobalVar *gvar, Number *num_val) {
  if (map_get(scope->decl_map, name)) {
    return false;
  }
  Decl *decl = NEW(Decl);
  decl->type = type;
  decl->stack_var = svar;
  decl->global_var = gvar;
  decl->num_val = num_val;
  map_put(scope->decl_map, name, decl);
  return true;
}

static Decl *get_decl(Scope *scope, char *name) {
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
    if (type) {
      return type;
    }
    scope = scope->outer;
  }
  return NULL;
}

static bool is_sametype(Type *ty1, Type *ty2) {
  if (ty1->ty != ty2->ty) {
    return false;
  }
  if (ty1->ty == TY_PTR) {
    return is_sametype(ty1->ptrof, ty2->ptrof);
  }
  return true;
}

static bool is_integer_type(Type *ty) {
  switch (ty->ty) {
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
  case TY_ENUM:
    return true;
  case TY_VOID:
  case TY_PTR:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    return false;
  }
  assert(false);
  return false;
}
static int get_int_type_rank(Type *ty, Range range) {
  switch (ty->ty) {
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_U_CHAR:
    return 1;
  case TY_S_SHORT:
  case TY_U_SHORT:
    return 2;
  case TY_S_INT:
  case TY_U_INT:
  case TY_ENUM:
    return 3;
  case TY_S_LONG:
  case TY_U_LONG:
    return 4;
  case TY_S_LLONG:
  case TY_U_LLONG:
    return 5;
  case TY_VOID:
  case TY_PTR:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    break;
  }
  range_error(range, "整数型ではない型です: %d", ty->ty);
}
bool is_signed_int_type(Type *ty, Range range) {
  switch (ty->ty) {
  case TY_CHAR:
    return true;
  case TY_S_CHAR:
  case TY_S_SHORT:
  case TY_S_INT:
  case TY_S_LONG:
  case TY_S_LLONG:
  case TY_ENUM:
    return true;
  case TY_U_CHAR:
  case TY_U_SHORT:
  case TY_U_INT:
  case TY_U_LONG:
  case TY_U_LLONG:
    return false;
  case TY_PTR:
  case TY_VOID:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    break;
  }
  range_error(range, "整数型ではない型です: %d", ty->ty);
}
static bool is_arith_type(Type *ty) { return is_integer_type(ty); }
static bool is_ptr_type(Type *ty) { return ty->ty == TY_PTR; }
static bool is_array_type(Type *ty) { return ty->ty == TY_ARRAY; }
static bool is_func_type(Type *ty) { return ty->ty == TY_FUNC; }
static Type *integer_promoted(Scope *scope, Expr **e) {
  if (!is_integer_type((*e)->val_type)) {
    return NULL;
  }
  // S/UCHAR, (S_,U_)SHORT, ENUM は S_INT へ昇格する
  switch ((*e)->val_type->ty) {
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_U_CHAR:
  case TY_S_SHORT:
  case TY_U_SHORT:
  case TY_ENUM:
    *e = new_expr_cast(scope, new_type(TY_S_INT, false), *e, (*e)->range);
    break;
  case TY_S_INT:
  case TY_U_INT:
  case TY_S_LONG:
  case TY_U_LONG:
  case TY_S_LLONG:
  case TY_U_LLONG:
  case TY_VOID:
  case TY_PTR:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    break;
  }
  return (*e)->val_type;
}

static Type *arith_converted(Scope *scope, Expr **e1, Expr **e2) {
  if (!is_arith_type((*e1)->val_type) || !is_arith_type((*e2)->val_type)) {
    return NULL;
  }

  Type *ty1 = integer_promoted(scope, e1);
  Type *ty2 = integer_promoted(scope, e2);

  Range r1 = (*e1)->range;
  Range r2 = (*e2)->range;
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
    ty = new_type(TY_U_INT, false);
    break;
  case TY_S_LONG:
    ty = new_type(TY_U_LONG, false);
    break;
  case TY_S_LLONG:
    ty = new_type(TY_U_LLONG, false);
    break;
  default:
    assert(false);
  }

  *e1 = new_expr_cast(scope, ty, *e1, r1);
  *e2 = new_expr_cast(scope, ty, *e2, r2);
  return ty;
}

static bool token_is_typename(Scope *scope, Token *token) {
  switch (token->ty) {
  case TK_VOID:
  case TK_INT:
  case TK_SHORT:
  case TK_LONG:
  case TK_CHAR:
  case TK_SIGNED:
  case TK_UNSIGNED:
  case TK_STRUCT:
  case TK_UNION:
  case TK_ENUM:
    return true;
  case TK_IDENT:
    return get_typedef(scope, token->name);
  default:
    return false;
  }
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

static bool token_is_type_qualifier(Token *token) {
  switch (token->ty) {
  case TK_CONST:
    return true;
  default:
    return false;
  }
}

int get_val_size(Type *ty, Range range) {
  switch (ty->ty) {
  case TY_VOID:
    return sizeof(void);
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
  case TY_PTR:
    return sizeof(void *);
  case TY_ARRAY:
    if (ty->array_len < 0) {
      range_error(range, "不完全な配列型のサイズを取得しようとしました");
    }
    return get_val_size(ty->ptrof, range) * ty->array_len;
  case TY_FUNC:
    range_error(range, "関数型の値サイズを取得しようとしました");
  case TY_STRUCT:
  case TY_UNION:
    if (ty->member_list == NULL) {
      range_error(range, "不完全型の値のサイズを取得しようとしました");
    }
    return align(ty->member_size, ty->member_align);
  case TY_ENUM:
    return sizeof(int);
  }
  range_error(range, "不明な型のサイズを取得しようとしました");
}

int get_val_align(Type *ty, Range range) {
  switch (ty->ty) {
  case TY_VOID:
    return alignof(void);
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
  case TY_PTR:
    return alignof(void *);
  case TY_ARRAY:
    return get_val_align(ty->ptrof, range);
  case TY_FUNC:
    range_error(range, "関数型の値アラインメントを取得しようとしました");
  case TY_STRUCT:
  case TY_UNION:
    return ty->member_align;
  case TY_ENUM:
    return alignof(int);
  }
  range_error(range, "不明な型の値アラインメントを取得しようとしました");
}

#define binop_type_error(ty, lhs, rhs)                                         \
  binop_type_error_raw((ty), (lhs), (rhs), __FILE__, __LINE__)
static noreturn void binop_type_error_raw(int ty, Expr *lhs, Expr *rhs,
                                          const char *dbg_file, int dbg_line) {
  range_error_raw(range_join(lhs->range, rhs->range), dbg_file, dbg_line,
                  "不正な型の値に対する演算です: 演算=%d(%c), 左辺=%d, 右辺=%d",
                  ty, ty, lhs->val_type->ty, rhs->val_type->ty);
}

static Type *new_type(int ty, bool is_const) {
  Type *type = NEW(Type);
  type->ty = ty;
  type->is_const = is_const;
  return type;
}

static Type *new_type_ptr(Type *base_type, bool is_const) {
  Type *ptrtype = new_type(TY_PTR, is_const);
  ptrtype->ptrof = base_type;
  return ptrtype;
}

static Type *new_type_array(Type *base_type, Number len, bool is_const) {
  int l;
  SET_NUMBER_VAL(l, &len);
  Type *ptrtype = new_type(TY_ARRAY, is_const);
  ptrtype->ty = TY_ARRAY;
  ptrtype->ptrof = base_type;
  ptrtype->array_len = l;
  return ptrtype;
}

static Type *new_type_unsized_array(Type *base_type, bool is_const) {
  Type *ptrtype = new_type(TY_ARRAY, is_const);
  ptrtype->ptrof = base_type;
  ptrtype->array_len = -1;
  return ptrtype;
}

static Type *new_type_func(Type *ret_type, Vector *func_param, bool has_varargs,
                           bool is_const) {
  Type *funtype = new_type(TY_FUNC, is_const);
  funtype->func_ret = ret_type;
  funtype->func_param = func_param;
  funtype->func_has_varargs = has_varargs;
  return funtype;
}

static Type *new_type_struct(int tk, char *tag, bool is_const) {
  assert(tk == TK_STRUCT || tk == TK_UNION);
  Type *type = new_type(tk == TK_STRUCT ? TY_STRUCT : TY_UNION, is_const);
  type->tag = tag;
  type->member_name_map = new_map();
  type->member_list = new_vector();
  type->member_size = 0;
  type->member_align = 0;
  return type;
}

static Type *new_type_anon_struct(int tk, bool is_const) {
  assert(tk == TK_STRUCT || tk == TK_UNION);
  Type *type = new_type(tk == TK_STRUCT ? TY_STRUCT : TY_UNION, is_const);
  type->member_name_map = NULL;
  type->member_list = NULL;
  return type;
}

static Type *new_type_enum(char *tag, bool is_const) {
  Type *type = new_type(TY_ENUM, is_const);
  type->tag = tag;
  return type;
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

static Expr *new_expr(int ty, Type *val_type, Range range) {
  Expr *expr = NEW(Expr);
  expr->ty = ty;
  expr->val_type = val_type;
  expr->range = range;
  return expr;
}

static Expr *new_expr_num(Number val, Range range) {
  Expr *expr = new_expr(EX_NUM, new_type(val.type, false), range);
  expr->num = val;
  return expr;
}

static Expr *new_expr_ident(Scope *scope, char *name, Range range) {
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
  if (decl != NULL) {
    if (decl->stack_var != NULL) {
      ty = EX_STACK_VAR;
      type = decl->type;
      svar = decl->stack_var;
    } else if (decl->num_val != NULL) {
      ty = EX_NUM;
      type = decl->type;
      num_val = decl->num_val;
    } else {
      ty = EX_GLOBAL_VAR;
      type = decl->type;
      gvar = decl->global_var;
    }
  } else {
    range_warn(range, "未定義の識別子です: %s", name);
    ty = EX_GLOBAL_VAR;
    type = new_type(TY_S_INT, false);
    gvar = new_global_variable(type, name, range, false);
  }

  Expr *expr = new_expr(ty, type, range);
  if (ty == EX_STACK_VAR) {
    expr->stack_var = svar;
  } else if (ty == EX_GLOBAL_VAR) {
    expr->global_var.name = gvar != NULL ? gvar->name : name;
    expr->global_var.def = gvar;
  } else if (ty == EX_NUM) {
    expr->num = *num_val;
  } else {
    assert(false);
  }
  return expr;
}

static Expr *new_expr_str(Scope *scope, char *val, Range range) {
  Type *type = new_type_ptr(new_type(TY_CHAR, true), false);
  Expr *expr = new_expr(EX_STR, type, range);

  expr->str = make_label("str");
  StringLiteral *str = NEW(StringLiteral);
  str->name = expr->str;
  str->val = val;
  vec_push(scope->global_ctxt->str_list, str);
  return expr;
}

static Expr *new_expr_call(Scope *scope, Expr *callee, Vector *argument,
                           Range range) {
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
    func_type = new_type_func(new_type(TY_S_INT, false), NULL, false, false);
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

  Expr *expr = new_expr(EX_CALL, ret_type, range);
  expr->call.callee = callee;
  expr->call.argument = argument;
  return expr;
}

static Expr *new_expr_postfix(Scope *scope, int ty, Expr *operand,
                              Range range) {
  operand = coerce_array2ptr(scope, operand);
  operand = coerce_func2ptr(scope, operand);

  Expr *expr = new_expr(ty, operand->val_type, range);
  expr->unop.operand = operand;
  return expr;
}

static Expr *new_expr_cast(Scope *scope, Type *val_type, Expr *operand,
                           Range range) {
  operand = coerce_array2ptr(scope, operand);
  operand = coerce_func2ptr(scope, operand);

  if (is_sametype(operand->val_type, val_type)) {
    return operand;
  }

  if (operand->ty == EX_NUM) {
    Number *opnum = &operand->num;
    switch (val_type->ty) {
    case TY_VOID:
      operand->val_type = val_type;
      return operand;
    case TY_CHAR:
      SET_NUMBER_VAL(opnum->char_val, opnum);
      operand->val_type = val_type;
      opnum->type = val_type->ty;
      return operand;
    case TY_S_CHAR:
      SET_NUMBER_VAL(opnum->s_char_val, opnum);
      operand->val_type = val_type;
      opnum->type = val_type->ty;
      return operand;
    case TY_S_SHORT:
      SET_NUMBER_VAL(opnum->s_short_val, opnum);
      operand->val_type = val_type;
      opnum->type = val_type->ty;
      return operand;
    case TY_S_INT:
      SET_NUMBER_VAL(opnum->s_int_val, opnum);
      operand->val_type = val_type;
      opnum->type = val_type->ty;
      return operand;
    case TY_S_LONG:
      SET_NUMBER_VAL(opnum->s_long_val, opnum);
      operand->val_type = val_type;
      opnum->type = val_type->ty;
      return operand;
    case TY_S_LLONG:
      SET_NUMBER_VAL(opnum->s_llong_val, opnum);
      operand->val_type = val_type;
      opnum->type = val_type->ty;
      return operand;
    case TY_U_CHAR:
      SET_NUMBER_VAL(opnum->u_char_val, opnum);
      operand->val_type = val_type;
      opnum->type = val_type->ty;
      return operand;
    case TY_U_SHORT:
      SET_NUMBER_VAL(opnum->u_short_val, opnum);
      operand->val_type = val_type;
      opnum->type = val_type->ty;
      return operand;
    case TY_U_INT:
      SET_NUMBER_VAL(opnum->u_int_val, opnum);
      operand->val_type = val_type;
      opnum->type = val_type->ty;
      return operand;
    case TY_U_LONG:
      SET_NUMBER_VAL(opnum->u_long_val, opnum);
      operand->val_type = val_type;
      opnum->type = val_type->ty;
      return operand;
    case TY_U_LLONG:
      SET_NUMBER_VAL(opnum->u_llong_val, opnum);
      operand->val_type = val_type;
      opnum->type = val_type->ty;
      return operand;
    case TY_PTR:
      SET_NUMBER_VAL(opnum->ptr_val, opnum);
      operand->val_type = val_type;
      opnum->type = val_type->ty;
      return operand;
    case TY_ENUM:
      SET_NUMBER_VAL(opnum->enum_val, opnum);
      operand->val_type = val_type;
      opnum->type = val_type->ty;
      return operand;
    case TY_ARRAY:
    case TY_FUNC:
    case TY_STRUCT:
    case TY_UNION:
      break;
    }
  }

  Expr *expr = new_expr(EX_CAST, val_type, range);
  expr->unop.operand = operand;
  return expr;
}

#define UNARYOP(op, num, r, a)                                                 \
  switch ((op)) {                                                              \
  case EX_MINUS:                                                               \
    *(r) = (-(a));                                                             \
    return (num);                                                              \
  case EX_NOT:                                                                 \
    *(r) = (~(a));                                                             \
    return (num);                                                              \
  }

static Number eval_unaryop(int op, type_t type, Number na, Range range) {
  Number num = {.type = type};
  switch (type) {
  case TY_S_INT: {
    signed int a, *r = &num.s_int_val;
    SET_NUMBER_VAL(a, &na);
    UNARYOP(op, num, r, a);
    break;
  }

  case TY_S_LONG: {
    signed long a, *r = &num.s_long_val;
    SET_NUMBER_VAL(a, &na);
    UNARYOP(op, num, r, a);
    break;
  }

  case TY_S_LLONG: {
    signed long long a, *r = &num.s_llong_val;
    SET_NUMBER_VAL(a, &na);
    UNARYOP(op, num, r, a);
    break;
  }

  case TY_U_INT: {
    unsigned int a, *r = &num.u_int_val;
    SET_NUMBER_VAL(a, &na);
    UNARYOP(op, num, r, a);
    break;
  }

  case TY_U_LONG: {
    unsigned long a, *r = &num.u_long_val;
    SET_NUMBER_VAL(a, &na);
    UNARYOP(op, num, r, a);
    break;
  }

  case TY_U_LLONG: {
    unsigned long long a, *r = &num.u_llong_val;
    SET_NUMBER_VAL(a, &na);
    UNARYOP(op, num, r, a);
    break;
  }

  case TY_VOID:
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_S_SHORT:
  case TY_U_CHAR:
  case TY_U_SHORT:
  case TY_PTR:
  case TY_ENUM:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    break;
  }
  range_error(range, "不正な型の演算です: %d(%c), %d, %d", op, op, type,
              na.type);
}

static Expr *new_expr_unary(Scope *scope, int op, Expr *operand, Range range) {
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
      val_type = new_type_ptr(operand->val_type->ptrof, false);
    } else {
      val_type = new_type_ptr(operand->val_type, false);
    }
    break;
  }
  case EX_INDIRECT: {
    if (operand->val_type->ty != TY_PTR) {
      range_error(range, "ポインタ型でない値に対するデリファレンスです");
    }
    if (operand->val_type->ptrof->ty == TY_FUNC) {
      return operand;
    }
    val_type = operand->val_type->ptrof;
    break;
  }
  case EX_PLUS:
    if (!is_arith_type(operand->val_type)) {
      range_error(range, "不正な型の値に対する演算です: 算術型ではありません");
    }
    if (is_integer_type(operand->val_type)) {
      val_type = integer_promoted(scope, &operand);
    }
    return operand;
  case EX_MINUS:
    if (!is_arith_type(operand->val_type)) {
      range_error(range, "不正な型の値に対する演算です: 算術型ではありません");
    }
    if (is_integer_type(operand->val_type)) {
      val_type = integer_promoted(scope, &operand);
    } else {
      val_type = operand->val_type;
    }
    if (operand->ty == EX_NUM) {
      operand->num = eval_unaryop(op, val_type->ty, operand->num, range);
      operand->range = range;
      return operand;
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
      range_error(range, "不正な型の値に対する演算です: 整数型ではありません");
    }
    val_type = integer_promoted(scope, &operand);
    if (operand->ty == EX_NUM) {
      operand->num = eval_unaryop(op, val_type->ty, operand->num, range);
      operand->range = range;
      return operand;
    }
    break;
  default:
    assert(false);
  }
  }

  Expr *expr = new_expr(op, val_type, range);
  expr->unop.operand = operand;
  return expr;
}

#define BINOP(op, num, r, a, b)                                                \
  switch ((op)) {                                                              \
  case EX_MUL:                                                                 \
    *(r) = ((a) * (b));                                                        \
    return (num);                                                              \
  case EX_DIV:                                                                 \
    *(r) = ((a) / (b));                                                        \
    return (num);                                                              \
  case EX_MOD:                                                                 \
    *(r) = ((a) % (b));                                                        \
    return (num);                                                              \
  case EX_ADD:                                                                 \
    *(r) = ((a) + (b));                                                        \
    return (num);                                                              \
  case EX_SUB:                                                                 \
    *(r) = ((a) - (b));                                                        \
    return (num);                                                              \
  case EX_AND:                                                                 \
    *(r) = ((a) & (b));                                                        \
    return (num);                                                              \
  case EX_XOR:                                                                 \
    *(r) = ((a) ^ (b));                                                        \
    return (num);                                                              \
  case EX_OR:                                                                  \
    *(r) = ((a) | (b));                                                        \
    return (num);                                                              \
  case EX_LT:                                                                  \
    *(r) = ((a) < (b));                                                        \
    return (num);                                                              \
  case EX_GT:                                                                  \
    *(r) = ((a) > (b));                                                        \
    return (num);                                                              \
  case EX_LTEQ:                                                                \
    *(r) = ((a) <= (b));                                                       \
    return (num);                                                              \
  case EX_GTEQ:                                                                \
    *(r) = ((a) >= (b));                                                       \
    return (num);                                                              \
  case EX_EQEQ:                                                                \
    *(r) = ((a) == (b));                                                       \
    return (num);                                                              \
  case EX_NOTEQ:                                                               \
    *(r) = ((a) != (b));                                                       \
    return (num);                                                              \
  }

static Number eval_binop(int op, type_t type, Number na, Number nb,
                         Range range) {
  assert(na.type == nb.type);
  Number num = {.type = type};
  switch (type) {
  case TY_S_INT: {
    signed int a, b, *r = &num.s_int_val;
    SET_NUMBER_VAL(a, &na);
    SET_NUMBER_VAL(b, &nb);
    BINOP(op, num, r, a, b);
    break;
  }

  case TY_S_LONG: {
    signed long a, b, *r = &num.s_long_val;
    SET_NUMBER_VAL(a, &na);
    SET_NUMBER_VAL(b, &nb);
    BINOP(op, num, r, a, b);
    break;
  }

  case TY_S_LLONG: {
    signed long long a, b, *r = &num.s_llong_val;
    SET_NUMBER_VAL(a, &na);
    SET_NUMBER_VAL(b, &nb);
    BINOP(op, num, r, a, b);
    break;
  }

  case TY_U_INT: {
    unsigned int a, b, *r = &num.u_int_val;
    SET_NUMBER_VAL(a, &na);
    SET_NUMBER_VAL(b, &nb);
    BINOP(op, num, r, a, b);
    break;
  }

  case TY_U_LONG: {
    unsigned long a, b, *r = &num.u_long_val;
    SET_NUMBER_VAL(a, &na);
    SET_NUMBER_VAL(b, &nb);
    BINOP(op, num, r, a, b);
    break;
  }

  case TY_U_LLONG: {
    unsigned long long a, b, *r = &num.u_llong_val;
    SET_NUMBER_VAL(a, &na);
    SET_NUMBER_VAL(b, &nb);
    BINOP(op, num, r, a, b);
    break;
  }

  case TY_VOID:
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_S_SHORT:
  case TY_U_CHAR:
  case TY_U_SHORT:
  case TY_PTR:
  case TY_ENUM:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    break;
  }
  range_error(range, "不正な型の演算です: %d(%c), %d, %d, %d", op, op, type,
              na.type, nb.type);
}

#define BINOP_SHIFT(op, num, r, a, b)                                          \
  switch ((op)) {                                                              \
  case EX_LSHIFT:                                                              \
    *(r) = ((a) << (b));                                                       \
    return (num);                                                              \
  case EX_RSHIFT:                                                              \
    *(r) = ((a) >> (b));                                                       \
    return (num);                                                              \
  }

static Number eval_binop_shift(int op, type_t type, Number na, Number nb,
                               Range range) {
  unsigned char width;
  SET_NUMBER_VAL(width, &nb);

  Number num = {.type = type};
  switch (type) {
  case TY_S_INT: {
    signed int a, *r = &num.s_int_val;
    SET_NUMBER_VAL(a, &na);
    BINOP_SHIFT(op, num, r, a, width);
    break;
  }

  case TY_S_LONG: {
    signed long a, *r = &num.s_long_val;
    SET_NUMBER_VAL(a, &na);
    BINOP_SHIFT(op, num, r, a, width);
    break;
  }

  case TY_S_LLONG: {
    signed long long a, *r = &num.s_llong_val;
    SET_NUMBER_VAL(a, &na);
    BINOP_SHIFT(op, num, r, a, width);
    break;
  }

  case TY_U_INT: {
    unsigned int a, *r = &num.u_int_val;
    SET_NUMBER_VAL(a, &na);
    BINOP_SHIFT(op, num, r, a, width);
    break;
  }

  case TY_U_LONG: {
    unsigned long a, *r = &num.u_long_val;
    SET_NUMBER_VAL(a, &na);
    BINOP_SHIFT(op, num, r, a, width);
    break;
  }

  case TY_U_LLONG: {
    unsigned long long a, *r = &num.u_llong_val;
    SET_NUMBER_VAL(a, &na);
    BINOP_SHIFT(op, num, r, a, width);
    break;
  }

  case TY_VOID:
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_S_SHORT:
  case TY_U_CHAR:
  case TY_U_SHORT:
  case TY_PTR:
  case TY_ENUM:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
    break;
  }
  range_error(range, "不正な型の演算です: %d(%c), %d, %d, %d", op, op, type,
              na.type, nb.type);
}

static Expr *new_expr_binop(Scope *scope, int op, Expr *lhs, Expr *rhs,
                            Range range) {
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
    if (lhs->ty == EX_NUM && rhs->ty == EX_NUM) {
      lhs->num = eval_binop(op, val_type->ty, lhs->num, rhs->num, range);
      lhs->range = range;
      return lhs;
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
    if (lhs->ty == EX_NUM && rhs->ty == EX_NUM) {
      lhs->num = eval_binop(op, val_type->ty, lhs->num, rhs->num, range);
      lhs->range = range;
      return lhs;
    }
    break;
  case EX_SUB:
    if (is_ptr_type(lhs->val_type)) {
      if (is_ptr_type(rhs->val_type)) {
        if (!is_sametype(lhs->val_type, rhs->val_type)) {
          binop_type_error(op, lhs, rhs);
        }

        // ptr - ptr
        Expr *sub = new_expr(EX_SUB, new_type(TY_S_LONG, false), range);
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
    if (lhs->ty == EX_NUM && rhs->ty == EX_NUM) {
      lhs->num = eval_binop(op, val_type->ty, lhs->num, rhs->num, range);
      lhs->range = range;
      return lhs;
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
    if (lhs->ty == EX_NUM && rhs->ty == EX_NUM) {
      lhs->num = eval_binop_shift(op, val_type->ty, lhs->num, rhs->num, range);
      lhs->range = range;
      return lhs;
    }
    break;

  case EX_LT:
  case EX_GT:
  case EX_LTEQ:
  case EX_GTEQ:
  case EX_EQEQ:
  case EX_NOTEQ:
    val_type = arith_converted(scope, &lhs, &rhs);
    if (lhs->ty == EX_NUM && rhs->ty == EX_NUM) {
      lhs->num = eval_binop(op, val_type->ty, lhs->num, rhs->num, range);
      lhs->range = range;
      return new_expr_cast(scope, new_type(TY_S_INT, false), lhs, range);
    }
    val_type = new_type(TY_S_INT, false);
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
    if (lhs->ty == EX_NUM && rhs->ty == EX_NUM) {
      lhs->num = eval_binop(op, val_type->ty, lhs->num, rhs->num, range);
      lhs->range = range;
      return lhs;
    }
    break;
  case EX_LOG_AND: {
    bool lhs_is_true = false;
    if (lhs->ty == EX_NUM) {
      int lval;
      SET_NUMBER_VAL(lval, &lhs->num);
      if (lval == 0) {
        return new_expr_num(new_number_int(0),
                            range_join(lhs->range, rhs->range));
      }
      lhs_is_true = true;
    }
    if (rhs->ty == EX_NUM) {
      int rval;
      SET_NUMBER_VAL(rval, &rhs->num);
      if (rval == 0) {
        return new_expr_num(new_number_int(0),
                            range_join(lhs->range, rhs->range));
      }
      if (lhs_is_true) {
        return new_expr_num(new_number_int(1),
                            range_join(lhs->range, rhs->range));
      }
    }
    val_type = new_type(TY_S_INT, false);
    break;
  }
  case EX_LOG_OR: {
    bool lhs_is_false = false;
    if (lhs->ty == EX_NUM) {
      int lval;
      SET_NUMBER_VAL(lval, &lhs->num);
      if (lval != 0) {
        return new_expr_num(new_number_int(1),
                            range_join(lhs->range, rhs->range));
      }
      lhs_is_false = true;
    }
    if (rhs->ty == EX_NUM) {
      int rval;
      SET_NUMBER_VAL(rval, &rhs->num);
      if (rval != 0) {
        return new_expr_num(new_number_int(1),
                            range_join(lhs->range, rhs->range));
      }
      if (lhs_is_false) {
        return new_expr_num(new_number_int(0),
                            range_join(lhs->range, rhs->range));
      }
    }
    val_type = new_type(TY_S_INT, false);
    break;
  }
  case EX_ASSIGN:
    rhs = new_expr_cast(scope, lhs->val_type, rhs, rhs->range);
    val_type = lhs->val_type;
    break;
  case EX_COMMA:
    val_type = lhs->val_type;
    break;
  default:
    assert(false);
  }

  Expr *expr = new_expr(op, val_type, range);
  expr->binop.lhs = lhs;
  expr->binop.rhs = rhs;
  return expr;
}

static Expr *new_expr_cond(Scope *scope, Expr *cond, Expr *then_expr,
                           Expr *else_expr, Range range) {
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
  } else {
    if (!is_sametype(then_expr->val_type, else_expr->val_type)) {
      range_error(range, "条件演算子の両辺の型が異なります: %d, %d",
                  then_expr->val_type->ty, else_expr->val_type->ty);
    }
    val_type = then_expr->val_type;
  }

  if (cond->ty == EX_NUM) {
    int i = 0;
    SET_NUMBER_VAL(i, &cond->num);
    if (i != 0) {
      return then_expr;
    }
    return else_expr;
  }
  Expr *expr = new_expr(EX_COND, val_type, range);
  expr->cond.cond = cond;
  expr->cond.then_expr = then_expr;
  expr->cond.else_expr = else_expr;
  return expr;
}

static Expr *new_expr_index(Scope *scope, Expr *array, Expr *index,
                            Range range) {
  array = coerce_array2ptr(scope, array);
  array = coerce_func2ptr(scope, array);
  index = coerce_array2ptr(scope, index);
  index = coerce_func2ptr(scope, index);

  return new_expr_unary(scope, EX_INDIRECT,
                        new_expr_binop(scope, EX_ADD, array, index, range),
                        range);
}

static Expr *new_expr_dot(Scope *scope, Expr *operand, char *name,
                          Range range) {
  if (operand->val_type->ty != TY_STRUCT && operand->val_type->ty != TY_UNION) {
    range_error(range, "構造体または共用体以外のメンバへのアクセスです");
  }
  return new_expr_arrow(
      scope, new_expr_unary(scope, EX_ADDRESS, operand, range), name, range);
}

static Expr *new_expr_arrow(Scope *scope, Expr *operand, char *name,
                            Range range) {
  if (operand->val_type->ty != TY_PTR ||
      (operand->val_type->ptrof->ty != TY_STRUCT &&
       operand->val_type->ptrof->ty != TY_UNION)) {
    range_error(range, "構造体または共用体以外のメンバへのアクセスです");
  }
  Member *member =
      operand->val_type->ptrof->member_name_map
          ? map_get(operand->val_type->ptrof->member_name_map, name)
          : NULL;
  if (member == NULL) {
    range_error(range, "存在しないメンバへのアクセスです: %s", name);
  }
  Expr *expr = new_expr(EX_ADD, new_type_ptr(member->type, false), range);
  expr->binop.lhs = operand;
  expr->binop.rhs = new_expr_num(new_number_size_t(member->offset), range);
  return new_expr_unary(scope, EX_INDIRECT, expr, range);
}

static Stmt *new_stmt(int ty, Range range) {
  Stmt *stmt = NEW(Stmt);
  stmt->ty = ty;
  stmt->range = range;
  return stmt;
}

static Stmt *new_stmt_expr(Expr *expr, Range range) {
  Stmt *stmt = new_stmt(ST_EXPR, range);
  stmt->expr = expr;
  return stmt;
}

static Stmt *new_stmt_if(Expr *cond, Stmt *then_stmt, Stmt *else_stmt,
                         Range range) {
  Stmt *stmt = new_stmt(ST_IF, range);
  stmt->cond = cond;
  stmt->then_stmt = then_stmt;
  stmt->else_stmt = else_stmt;
  return stmt;
}

static Stmt *new_stmt_switch(Expr *cond, Stmt *body, Range range) {
  Stmt *stmt = new_stmt(ST_SWITCH, range);
  stmt->cond = cond;
  stmt->body = body;
  stmt->cases = new_vector();
  stmt->default_case = NULL;
  return stmt;
}

static Stmt *new_stmt_case(Expr *expr, Stmt *body, Range range) {
  Stmt *stmt = new_stmt(ST_CASE, range);
  stmt->expr = expr;
  stmt->label = make_label("case");
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_default(Stmt *body, Range range) {
  Stmt *stmt = new_stmt(ST_DEFAULT, range);
  stmt->label = make_label("default");
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_label(FuncCtxt *fcx, char *name, Stmt *body,
                            Range range) {
  Stmt *stmt = new_stmt(ST_LABEL, range);
  stmt->name = name;
  stmt->label = make_label(name);
  stmt->body = body;
  map_put(fcx->label_map, stmt->name, stmt->label);
  return stmt;
}

static Stmt *new_stmt_while(Expr *cond, Stmt *body, Range range) {
  Stmt *stmt = new_stmt(ST_WHILE, range);
  stmt->cond = cond;
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_do_while(Expr *cond, Stmt *body, Range range) {
  Stmt *stmt = new_stmt(ST_DO_WHILE, range);
  stmt->cond = cond;
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_for(Expr *init, Expr *cond, Expr *inc, Stmt *body,
                          Range range) {
  Stmt *stmt = new_stmt(ST_FOR, range);
  stmt->init = init;
  stmt->cond = cond;
  stmt->inc = inc;
  stmt->body = body;
  return stmt;
}

static Stmt *new_stmt_goto(char *name, Range range) {
  Stmt *stmt = new_stmt(ST_GOTO, range);
  stmt->name = name;
  return stmt;
}

static Stmt *new_stmt_return(Scope *scope, Expr *expr, Range range) {
  Stmt *stmt = new_stmt(ST_RETURN, range);
  if (expr != NULL) {
    stmt->expr = new_expr_cast(scope, scope->func_ctxt->type->func_ret, expr,
                               expr->range);
  } else {
    stmt->expr = NULL;
  }
  return stmt;
}

static Stmt *new_stmt_compound(Vector *stmts, Range range) {
  Stmt *stmt = new_stmt(ST_COMPOUND, range);
  stmt->stmts = stmts;
  return stmt;
}

static Expr *primary_expression(Tokenizer *tokenizer, Scope *scope) {
  Token *token = NULL;
  if ((token = token_consume(tokenizer, TK_NUM)) != NULL) {
    return new_expr_num(token->num_val, token->range);
  }
  if ((token = token_consume(tokenizer, TK_IDENT)) != NULL) {
    return new_expr_ident(scope, token->name, token->range);
  }
  if ((token = token_consume(tokenizer, TK_STR)) != NULL) {
    char *val = strdup(token->str);
    size_t len = strlen(val);
    Range range = token->range;
    Token *t;
    while ((t = token_consume(tokenizer, TK_STR)) != NULL) {
      size_t extra_len = strlen(t->str);
      val = realloc(val, len + extra_len + 1);
      memcpy(&val[len], t->str, extra_len + 1);
      len += extra_len;
      range = range_join(range, t->range);
    }
    return new_expr_str(scope, val, range);
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
    } else if (token_consume(tokenizer, '.')) {
      Token *member = token_expect(tokenizer, TK_IDENT);
      expr = new_expr_dot(scope, expr, member->name,
                          range_join(expr->range, member->range));
    } else if (token_consume(tokenizer, TK_ARROW)) {
      Token *member = token_expect(tokenizer, TK_IDENT);
      expr = new_expr_arrow(scope, expr, member->name,
                            range_join(expr->range, member->range));
    } else if (token_consume(tokenizer, '(')) {
      Vector *argument = NULL;
      if (token_peek(tokenizer)->ty != ')') {
        argument = argument_expression_list(tokenizer, scope);
      }
      Token *end = token_expect(tokenizer, ')');
      expr = new_expr_call(scope, expr, argument,
                           range_join(expr->range, end->range));
    } else if ((token = token_consume(tokenizer, TK_INC)) != NULL) {
      expr = new_expr_postfix(scope, EX_POST_INC, expr,
                              range_join(expr->range, token->range));
    } else if ((token = token_consume(tokenizer, TK_DEC)) != NULL) {
      expr = new_expr_postfix(scope, EX_POST_DEC, expr, token->range);
    } else {
      return expr;
    }
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
        !token_is_typename(scope, token_peek_ahead(tokenizer, 1))) {
      Expr *expr = unary_expression(tokenizer, scope);
      return new_expr_num(
          new_number_size_t(get_val_size(expr->val_type, expr->range)),
          range_join(token->range, expr->range));
    }
    token_expect(tokenizer, '(');
    Type *type = type_name(scope, tokenizer);
    Token *end = token_expect(tokenizer, ')');
    return new_expr_num(new_number_size_t(get_val_size(type, end->range)),
                        range_join(token->range, end->range));
  }
  return postfix_expression(tokenizer, scope);
}

static Expr *cast_expression(Tokenizer *tokenizer, Scope *scope) {
  Token *token = token_peek(tokenizer);
  if (token->ty == '(' &&
      token_is_typename(scope, token_peek_ahead(tokenizer, 1))) {
    Range start = token->range;
    token_succ(tokenizer);
    Type *val_type = type_name(scope, tokenizer);
    token_expect(tokenizer, ')');
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
  Expr *lhs = conditional_expression(tokenizer, scope);
  if (token_consume(tokenizer, '=')) {
    Expr *rhs = assignment_expression(tokenizer, scope);
    return new_expr_binop(scope, EX_ASSIGN, lhs, rhs,
                          range_join(lhs->range, rhs->range));
  }

  typedef struct {
    int token_ty;
    int expr_ty;
  } Pair;

  const Pair PAIRS[] = {
      {TK_MUL_ASSIGN, EX_MUL},
      {TK_DIV_ASSIGN, EX_DIV},
      {TK_MOD_ASSIGN, EX_MOD},
      {TK_ADD_ASSIGN, EX_ADD},
      {TK_SUB_ASSIGN, EX_SUB},
      {TK_LSHIFT_ASSIGN, EX_LSHIFT},
      {TK_RSHIFT_ASSIGN, EX_RSHIFT},
      {TK_AND_ASSIGN, EX_AND},
      {TK_XOR_ASSIGN, EX_XOR},
      {TK_OR_ASSIGN, EX_OR},
      {TK_EOF, '\0'},
  };

  for (int i = 0; PAIRS[i].token_ty != TK_EOF; i++) {
    const Pair *p = &PAIRS[i];
    if (token_consume(tokenizer, p->token_ty)) {
      Expr *rhs = assignment_expression(tokenizer, scope);
      Range range = range_join(lhs->range, rhs->range);
      return new_expr_binop(scope, EX_ASSIGN, lhs,
                            new_expr_binop(scope, p->expr_ty, lhs, rhs, range),
                            range);
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
  return conditional_expression(tokenizer, scope);
}

static Vector *declaration(Tokenizer *tokenizer, Scope *scope) {
  Vector *def_list = new_vector();
  bool is_typedef = token_consume(tokenizer, TK_TYPEDEF);
  bool is_extern = token_consume(tokenizer, TK_EXTERN);
  bool is_static = token_consume(tokenizer, TK_STATIC);
  Type *base_type = type_specifier(scope, tokenizer);
  if (token_consume(tokenizer, ';')) {
    return def_list;
  }

  Token *name;
  Type *type;
  Range range;
  declarator(scope, tokenizer, base_type, &name, &type, &range);

  if (is_typedef) {
    register_typedef(scope, name->name, type);
  } else if (is_extern) {
    register_decl(scope, name->name, type, NULL, NULL, NULL);
  } else {
    if (is_func_type(type)) {
      VarDef *def = register_func(scope, name, type);
      if (token_peek(tokenizer)->ty == '{') {
        def->func = function_definition(tokenizer, scope, type, name->name,
                                        is_static, range);
        vec_push(def_list, def);
        return def_list;
      }
    } else {
      VarDef *def = register_var(scope, name, type, range, is_static);
      if (token_consume(tokenizer, '=')) {
        initializer(tokenizer, scope, type, &def->init);
      }
      vec_push(def_list, def);
    }
  }

  while (token_consume(tokenizer, ',')) {
    declarator(scope, tokenizer, base_type, &name, &type, &range);
    if (is_typedef) {
      register_typedef(scope, name->name, type);
    } else if (is_extern) {
      register_decl(scope, name->name, type, NULL, NULL, NULL);
    } else {
      if (is_func_type(type)) {
        (void)register_func(scope, name, type);
      } else {
        VarDef *def = register_var(scope, name, type, range, is_static);
        if (token_consume(tokenizer, '=')) {
          initializer(tokenizer, scope, type, &def->init);
        }
        vec_push(def_list, def);
      }
    }
  }
  token_expect(tokenizer, ';');

  return def_list;
}

static Type *type_specifier(Scope *scope, Tokenizer *tokenizer) {
  bool is_const = token_consume(tokenizer, TK_CONST);
  Token *token = token_pop(tokenizer);
  switch (token->ty) {
  case TK_VOID:
    return new_type(TY_VOID, is_const);

  case TK_CHAR:
    return new_type(TY_CHAR, is_const);
  case TK_INT:
    return new_type(TY_S_INT, is_const);
  case TK_SHORT:
    (void)token_consume(tokenizer, TK_INT);
    return new_type(TY_S_SHORT, is_const);
  case TK_LONG:
    if (token_consume(tokenizer, TK_LONG)) {
      (void)token_consume(tokenizer, TK_INT);
      return new_type(TY_S_LLONG, is_const);
    }
    (void)token_consume(tokenizer, TK_INT);
    return new_type(TY_S_LONG, is_const);

  case TK_SIGNED:
    if (token_consume(tokenizer, TK_CHAR)) {
      return new_type(TY_S_CHAR, is_const);
    }
    if (token_consume(tokenizer, TK_INT)) {
      return new_type(TY_S_INT, is_const);
    }
    if (token_consume(tokenizer, TK_SHORT)) {
      (void)token_consume(tokenizer, TK_INT);
      return new_type(TY_S_SHORT, is_const);
    }
    if (token_consume(tokenizer, TK_LONG)) {
      if (token_consume(tokenizer, TK_LONG)) {
        (void)token_consume(tokenizer, TK_INT);
        return new_type(TY_S_LLONG, is_const);
      }
      (void)token_consume(tokenizer, TK_INT);
      return new_type(TY_S_LONG, is_const);
    }
    return new_type(TY_S_INT, is_const);

  case TK_UNSIGNED:
    if (token_consume(tokenizer, TK_CHAR)) {
      return new_type(TY_U_CHAR, is_const);
    }
    if (token_consume(tokenizer, TK_INT)) {
      return new_type(TY_U_INT, is_const);
    }
    if (token_consume(tokenizer, TK_SHORT)) {
      (void)token_consume(tokenizer, TK_INT);
      return new_type(TY_U_SHORT, is_const);
    }
    if (token_consume(tokenizer, TK_LONG)) {
      if (token_consume(tokenizer, TK_LONG)) {
        (void)token_consume(tokenizer, TK_INT);
        return new_type(TY_U_LLONG, is_const);
      }
      (void)token_consume(tokenizer, TK_INT);
      return new_type(TY_U_LONG, is_const);
    }
    return new_type(TY_U_INT, is_const);

  case TK_STRUCT:
  case TK_UNION:
    return struct_or_union_specifier(scope, tokenizer, token, is_const);
  case TK_ENUM:
    return enum_specifier(scope, tokenizer, token, is_const);
  case TK_IDENT: {
    Type *type = get_typedef(scope, token->name);
    if (type != NULL) {
      if (is_const) {
        Type *copy_type = NEW(Type);
        *copy_type = *type;
        type = copy_type;
        type->is_const = true;
      }
      return type;
    }
  }
    // fallthrough
  default:
    range_error(token->range, "型名がありません");
  }
}

static void struct_declaration(Scope *scope, Tokenizer *tokenizer, Type *type) {
  assert(type->ty == TY_STRUCT || type->ty == TY_UNION);
  Type *base_type = type_specifier(scope, tokenizer);
  Token *member_name;
  Type *member_type;
  Range range;
  declarator(scope, tokenizer, base_type, &member_name, &member_type, &range);
  register_member(type, member_name != NULL ? member_name->name : NULL,
                  member_type, range);

  while (token_consume(tokenizer, ',')) {
    declarator(scope, tokenizer, base_type, &member_name, &member_type, &range);
    register_member(type, member_name != NULL ? member_name->name : NULL,
                    member_type, range);
  }
  token_expect(tokenizer, ';');
}

static Type *struct_or_union_specifier(Scope *scope, Tokenizer *tokenizer,
                                       Token *token, bool is_const) {
  assert(token->ty == TK_STRUCT || token->ty == TK_UNION);
  Token *tag = token_consume(tokenizer, TK_IDENT);
  if (tag == NULL && token_peek(tokenizer)->ty != '{') {
    range_error(token->range,
                "構造体または共用体のタグまたは `{` がありません");
  }

  if (token_consume(tokenizer, '{')) {
    Type *type = new_type_struct(token->ty, tag ? tag->name : NULL, is_const);
    if (tag != NULL) {
      if (!register_tag(scope, tag->name, type)) {
        range_error(tag->range,
                    "同じタグ名の構造体または共用体の多重定義です: %s",
                    tag->name);
      }
    }
    while (token_peek(tokenizer)->ty != '}') {
      struct_declaration(scope, tokenizer, type);
    }
    token_expect(tokenizer, '}');
    return type;
  }

  Type *type = get_tag(scope, tag->name);
  if (type != NULL) {
    return type;
  }

  return new_type_anon_struct(token->ty, is_const);
}

static void enumerator(Scope *scope, Tokenizer *tokenizer, Type *type,
                       int *val) {
  Token *ident = token_expect(tokenizer, TK_IDENT);
  if (token_consume(tokenizer, '=')) {
    Expr *expr = constant_expression(tokenizer, scope);
    expr = new_expr_cast(scope, type, expr, expr->range);
    if (expr->ty != EX_NUM) {
      range_error(expr->range, "列挙型の値が定数式の数値ではありません");
    }
    SET_NUMBER_VAL(*val, &expr->num);
  }
  token_consume(tokenizer, ',');

  Number *number = NEW(Number);
  number->type = TY_ENUM;
  number->enum_val = *val;
  if (!register_decl(scope, ident->name, type, NULL, NULL, number)) {
    range_error(ident->range, "定義済みの識別子です: %s", ident->name);
  }
  (*val)++;
}

static Type *enum_specifier(Scope *scope, Tokenizer *tokenizer, Token *token,
                            bool is_const) {
  Token *tag_ident = token_consume(tokenizer, TK_IDENT);
  if (tag_ident == NULL && token_peek(tokenizer)->ty != '{') {
    range_error(token->range, "列挙型のタグまたは `{` がありません");
  }

  char *tag = tag_ident != NULL ? tag_ident->name : NULL;

  if (token_consume(tokenizer, '{')) {
    Type *type = new_type_enum(tag != NULL ? tag : NULL, is_const);
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
  return new_type_enum(tag, is_const);
}

static Type *type_name(Scope *scope, Tokenizer *tokenizer) {
  Type *type = type_specifier(scope, tokenizer);
  while (token_consume(tokenizer, '*')) {
    type = new_type_ptr(type, false);
  }
  while (token_consume(tokenizer, '[')) {
    if (token_peek(tokenizer)->ty == ']') {
      type = new_type_unsized_array(type, false);
    } else {
      type =
          new_type_array(type, token_expect(tokenizer, TK_NUM)->num_val, false);
    }
    token_expect(tokenizer, ']');
  }
  return type;
}

static void declarator(Scope *scope, Tokenizer *tokenizer, Type *base_type,
                       Token **name, Type **type, Range *range) {
  Range start = token_peek(tokenizer)->range;
  while (token_consume(tokenizer, '*')) {
    bool is_const = token_consume(tokenizer, TK_CONST);
    base_type = new_type_ptr(base_type, is_const);
  }
  Range end;
  direct_declarator(scope, tokenizer, base_type, name, type, &end);
  *range = range_join(start, end);
}

static void direct_declarator(Scope *scope, Tokenizer *tokenizer,
                              Type *base_type, Token **name, Type **type,
                              Range *range) {
  Type *placeholder = NEW(Type);
  *range = token_peek(tokenizer)->range;

  {
    Token *token;
    if ((token = token_consume(tokenizer, TK_IDENT)) != NULL) {
      *name = token;
      *type = placeholder;
      *range = range_join(*range, token->range);
    } else if (token_consume(tokenizer, '(')) {
      Range mid;
      declarator(scope, tokenizer, placeholder, name, type, &mid);
      Token *token = token_expect(tokenizer, ')');
      *range = range_join(*range, token->range);
    } else {
      *name = NULL;
      *type = base_type;
      *range = *range;
    }
  }

  while (true) {
    if (token_consume(tokenizer, '[')) {
      Type *inner = NEW(Type);
      if (token_peek(tokenizer)->ty == ']') {
        *placeholder = *new_type_unsized_array(inner, false);
      } else {
        *placeholder = *new_type_array(
            inner, token_expect(tokenizer, TK_NUM)->num_val, false);
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
          Type *base_type = type_specifier(scope, tokenizer);
          Param *param = NEW(Param);
          declarator(scope, tokenizer, base_type, &param->name, &param->type,
                     &param->range);
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
      *placeholder = *new_type_func(inner, params, has_varargs, false);
      placeholder = inner;
      continue;
    }

    break;
  }

  *placeholder = *base_type;
}

static void struct_initializer(Tokenizer *tokenizer, Scope *scope, Type *type,
                               bool brace_root, Initializer **init) {
  assert(type->ty == TY_STRUCT);
  if (*init == NULL) {
    *init = new_initializer(type);
  }

  while (true) {
    int idx = 0;

    // initializer with designator
    if (token_peek(tokenizer)->ty == '.' &&
        token_peek_ahead(tokenizer, 1)->ty == TK_IDENT) {
      Token *ident = token_peek_ahead(tokenizer, 1);
      for (int i = 0; i < vec_len(type->member_list); i++) {
        Member *member = vec_get(type->member_list, i);
        if (member->name == NULL || strcmp(member->name, ident->name) == 0) {
          if (member->name != NULL) {
            token_expect(tokenizer, '.');
            token_expect(tokenizer, TK_IDENT);
            token_consume(tokenizer, '=');
          }
          Token *current = token_peek(tokenizer);
          Initializer *meminit = map_get_by_index((*init)->members, i, NULL);
          // if name is null, try parsing designator as inner struct/union's
          // designator
          initializer(tokenizer, scope, member->type, &meminit);
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
      for (int i = idx; i < vec_len(type->member_list); i++) {
        Initializer *meminit = map_get_by_index((*init)->members, i, NULL);
        Member *member = vec_get(type->member_list, i);
        initializer(tokenizer, scope, member->type, &meminit);
        map_set_by_index((*init)->members, i, member->name, meminit);
        if ((i < vec_len(type->member_list) - 1 &&
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
  if (*init == NULL) {
    *init = new_initializer(type);
  }

  while (true) {
    if (token_peek(tokenizer)->ty == '.' &&
        token_peek_ahead(tokenizer, 1)->ty == TK_IDENT) {
      Token *ident = token_peek_ahead(tokenizer, 1);
      for (int i = 0; i < vec_len(type->member_list); i++) {
        Member *member = vec_get(type->member_list, i);
        if (member->name == NULL || strcmp(member->name, ident->name) == 0) {
          if (member->name != NULL) {
            token_expect(tokenizer, '.');
            token_expect(tokenizer, TK_IDENT);
            token_consume(tokenizer, '=');
          }
          Token *current = token_peek(tokenizer);
          Initializer *meminit = map_get_by_index((*init)->members, 0, NULL);
          // if name is null, try parsing designator as inner struct/union's
          // designator
          initializer(tokenizer, scope, member->type, &meminit);
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
      Member *member = vec_get(type->member_list, 0);
      initializer(tokenizer, scope, member->type, &meminit);
      map_set_by_index((*init)->members, 0, member->name, meminit);
      break;
    }

    if (!brace_root || token_peek(tokenizer)->ty != '.') {
      break;
    }
  }
}

static void array_initializer(Tokenizer *tokenizer, Scope *scope, Type *type,
                              Initializer **init) {
  assert(type->ty == TY_ARRAY);
  if (*init == NULL) {
    *init = new_initializer(type);
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
      Token *num = token_expect(tokenizer, TK_NUM);
      token_expect(tokenizer, ']');
      token_consume(tokenizer, '=');
      int i;
      SET_NUMBER_VAL(i, &num->num_val);
      if (type->array_len < 0) {
        vec_extend((*init)->elements, i + 1);
      } else {
        if (i >= type->array_len) {
          range_error(num->range, "配列サイズを超過するインデックスです: %d",
                      i);
        }
      }
      Initializer *eleminit = vec_get((*init)->elements, i);
      initializer(tokenizer, scope, type->ptrof, &eleminit);
      vec_set((*init)->elements, i, eleminit);
      token_consume(tokenizer, ',');
      idx = i + 1;
    }

    if (token_peek(tokenizer)->ty != '[') {
      int max_len = type->array_len < 0 ? INT_MAX : type->array_len;
      for (int i = idx; i < max_len; i++) {
        vec_extend((*init)->elements, i + 1);
        Initializer *eleminit = vec_get((*init)->elements, i);
        initializer(tokenizer, scope, type->ptrof, &eleminit);
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
  }

  if (type->array_len < 0) {
    type->array_len = vec_len((*init)->elements);
  }
}

static void initializer(Tokenizer *tokenizer, Scope *scope, Type *type,
                        Initializer **init) {
  Token *token;
  if ((token = token_consume(tokenizer, '{')) != NULL) {
    switch (type->ty) {
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
    case TY_PTR:
    case TY_ENUM:
      initializer(tokenizer, scope, type, init);
      break;
    case TY_STRUCT:
      struct_initializer(tokenizer, scope, type, true, init);
      break;
    case TY_UNION:
      union_initializer(tokenizer, scope, type, true, init);
      break;
    case TY_ARRAY:
      array_initializer(tokenizer, scope, type, init);
      break;
    case TY_VOID:
    case TY_FUNC:
      range_error(token->range, "初期化できない型です: %d", type->ty);
    }
    (void)token_consume(tokenizer, ',');
    token_expect(tokenizer, '}');
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
    array_initializer(tokenizer, scope, type, init);
    return;
  }

  {
    *init = new_initializer(type);
    Expr *expr = assignment_expression(tokenizer, scope);
    (*init)->expr = new_expr_cast(scope, type, expr, expr->range);
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
    Range range;
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
    token_succ(tokenizer);
    Expr *init = NULL;
    Expr *cond = NULL;
    Expr *inc = NULL;
    token_expect(tokenizer, '(');
    if (token_peek(tokenizer)->ty != ';') {
      init = expression(tokenizer, scope);
    }
    token_expect(tokenizer, ';');
    if (token_peek(tokenizer)->ty != ';') {
      cond = expression(tokenizer, scope);
    }
    token_expect(tokenizer, ';');
    if (token_peek(tokenizer)->ty != ')') {
      inc = expression(tokenizer, scope);
    }
    token_expect(tokenizer, ')');
    Stmt *body = statement(tokenizer, scope);
    return new_stmt_for(init, cond, inc, body,
                        range_join(start->range, body->range));
  }
  case TK_GOTO: {
    token_succ(tokenizer);
    char *name = token_expect(tokenizer, TK_IDENT)->name;
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
      Stmt *stmt = new_stmt_label(scope->func_ctxt, ident->name, body,
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

static void gen_init(Scope *scope, Vector *stmts, Initializer *init,
                     Expr *dest) {
  if (init == NULL) {
    Expr *expr = new_expr_binop(scope, EX_ASSIGN, dest,
                                new_expr_num(new_number_int(0), dest->range),
                                dest->range);
    Stmt *s = new_stmt_expr(expr, expr->range);
    vec_push(stmts, s);
    return;
  }

  if (init->members != NULL) {
    for (int i = 0; i < map_size(init->members); i++) {
      char *name;
      Initializer *meminit = map_get_by_index(init->members, i, &name);
      Expr *mem =
          name != NULL ? new_expr_dot(scope, dest, name, dest->range) : dest;
      gen_init(scope, stmts, meminit, mem);
    }
    return;
  }

  if (init->elements != NULL) {
    for (int i = 0; i < vec_len(init->elements); i++) {
      Expr *index = new_expr_num(new_number_int(i), dest->range);
      Initializer *meminit = vec_get(init->elements, i);
      Expr *mem = new_expr_index(scope, dest, index, dest->range);
      gen_init(scope, stmts, meminit, mem);
    }
    return;
  }

  if (init->expr != NULL) {
    Expr *expr = new_expr_binop(scope, EX_ASSIGN, dest, init->expr,
                                range_join(dest->range, init->expr->range));
    Stmt *s = new_stmt_expr(expr, expr->range);
    vec_push(stmts, s);
    return;
  }

  assert(false);
}

static Stmt *compound_statement(Tokenizer *tokenizer, Scope *scope) {
  Token *start = token_expect(tokenizer, '{');

  Range range = start->range;
  Vector *stmts = new_vector();
  while (!token_consume(tokenizer, '}')) {
    Token *token = token_peek(tokenizer);
    if (token_is_typename(scope, token) ||
        token_is_storage_class_specifier(token) ||
        token_is_type_qualifier(token)) {

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
          Initializer *init = def->init;
          if (init == NULL) {
            continue;
          }

          Expr *dest = new_expr_ident(scope, def->name->name, def->name->range);
          gen_init(scope, stmts, init, dest);
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
                                     Type *type, char *name, bool is_static,
                                     Range start) {
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

  int stack_size = 0;
  for (int i = 0; i < vec_len(fcx->var_list); i++) {
    StackVar *svar = vec_get(fcx->var_list, i);
    stack_size = align(stack_size, get_val_align(svar->type, svar->range));
    svar->offset = stack_size;
    stack_size += get_val_size(svar->type, svar->range);
  }

  Function *func = NEW(Function);
  func->name = name;
  func->type = type;
  func->is_static = is_static;
  func->range = range_join(start, body->range);
  func->stack_size = stack_size;
  func->label_map = fcx->label_map;
  func->body = body;

  return func;
}

static GlobalVar *new_global_variable(Type *type, char *name, Range range,
                                      bool is_static) {
  GlobalVar *gvar = NEW(GlobalVar);
  gvar->type = type;
  gvar->name = name;
  gvar->range = range;
  gvar->is_static = is_static;
  gvar->init = NULL;
  return gvar;
}

static TranslationUnit *translation_unit(Tokenizer *tokenizer) {
  GlobalCtxt *gcx = new_global_ctxt();
  Scope *scope = new_global_scope(gcx);

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
        GlobalVar *prev_def = map_get(gvar_map, def->name->name);
        if (prev_def == NULL) {
          def->global_var->init = def->init;
          vec_push(gcx->gvar_list, def->global_var);
          map_put(gvar_map, def->name->name, def->global_var);
        } else {
          if (def->init != NULL) {
            if (prev_def->init != NULL) {
              range_error(def->global_var->range,
                          "グローバル変数が複数回定義されました: %s",
                          def->name->name);
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
