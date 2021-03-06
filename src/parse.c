#include "gifcc.h"
#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

typedef struct GlobalCtxt {
  FunctionVector *func_list;
  GlobalVarVector *gvar_list;
  StringLiteralVector *str_list;
} GlobalCtxt;

typedef struct FuncCtxt {
  const char *name;
  Type *type;
  StackVarVector *var_list;
  StmtVector *switches;
  Map *label_map;
} FuncCtxt;

typedef enum {
  DECL_STACK_VAR,
  DECL_GLOBAL_VAR,
  DECL_NUMBER,
  DECL_STRING,
  DECL_BUILTIN_FUNC,
} decl_t;

typedef struct Decl {
  decl_t kind;
  Type *type;
  StackVar *stack_var;
  GlobalVar *global_var;
  Number *number;
  const char *string;
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
typedef DEFINE_VECTOR(VarDefVector, VarDef *) VarDefVector;

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

typedef enum {
  DESIG_INDEX,
  DESIG_MEMBER,
} designator_t;

typedef struct Designator {
  designator_t type;
  const Range *range;
  union {
    struct {
      Number first;
      Number last;
    } index;
    const char *member;
  };
} Designator;
typedef DEFINE_VECTOR(DesignatorVector, Designator *) DesignatorVector;

typedef struct ParseInit ParseInit;
typedef struct InitElem {
  const Range *range;
  DesignatorVector *designation;
  ParseInit *pinit;
} InitElem;
typedef DEFINE_VECTOR(InitElemVector, InitElem *) InitElemVector;

typedef struct ParseInit {
  const Range *range;
  Expr *expr;
  InitElemVector *list;
} ParseInit;

static const StorageClassSpecifier EMPTY_STORAGE_CLASS_SPECIFIER = {};
static const TypeSpecifier EMPTY_TYPE_SPECIFIER = {};
static const FunctionSpecifier EMPTY_FUNCTION_SPECIFIER = {};

static Initializer *new_initializer(Type *type);
static MemberInitializer *new_member_initializer(Member *member);
static ParseInit *new_parse_init_list(const Range *range, InitElemVector *list);
static ParseInit *new_parse_init_expr(Expr *expr);
static GlobalCtxt *new_global_ctxt(void);
static FuncCtxt *new_func_ctxt(const char *name, Type *type);
static Scope *new_scope(GlobalCtxt *gcx, FuncCtxt *fcx, Scope *outer);
static Scope *new_global_scope(GlobalCtxt *gcx, const Reader *reader);
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
static void register_string(Scope *scope, const char *name, const char *val);
static void register_builtin_func(Scope *scope, const char *name,
                                  builtin_func_handler_t *handler);
static bool register_decl(Scope *scope, decl_t kind, const char *name,
                          Type *type, StackVar *svar, GlobalVar *gvar,
                          Number *number, const char *string,
                          builtin_func_handler_t *handler);
static Decl *get_decl(Scope *scope, const char *name);
static bool register_tag(Scope *scope, const char *tag, Type *type);
static Type *get_tag(Scope *scope, const char *tag);
static bool register_typedef(Scope *scope, const char *name, Type *type);
static Type *get_typedef(Scope *scope, const char *name);
static StringLiteral *get_string_literal(Scope *scope, const char *val);
static void register_label(FuncCtxt *fcx, const char *c_label,
                           const char *asm_label);
static bool token_is_storage_class_specifier(Token *token);
static bool consume_storage_class_specifier(TokenIterator *ts,
                                            StorageClassSpecifier *scs);
static bool token_is_type_name(Scope *scope, Token *token);
static bool token_is_declaration_specifiers(Scope *scope, Token *token);
static bool token_is_type_specifier(Scope *scope, Token *token);
static Type *construct_type_specifier(TypeSpecifier ts, TypeQualifier tq);
static bool token_is_type_qualifier(Token *token);
static bool consume_type_qualifier(TokenIterator *ts, TypeQualifier *tq);
static bool token_is_function_specifier(Token *token);
static bool consume_function_specifier(TokenIterator *ts,
                                       FunctionSpecifier *fs);

static Expr *builtin_va_start_handler(Expr *callee, ExprVector *arguments,
                                      const Range *range);
static Expr *builtin_va_arg_handler(Expr *callee, ExprVector *arguments,
                                    const Range *range);
static Expr *builtin_va_end_handler(Expr *callee, ExprVector *arguments,
                                    const Range *range);
static Expr *builtin_va_copy_handler(Expr *callee, ExprVector *arguments,
                                     const Range *range);

// expression
static Expr *primary_expression(TokenIterator *ts, Scope *scope);
static Expr *postfix_expression(TokenIterator *ts, Scope *scope);
static ExprVector *argument_expression_list(TokenIterator *ts, Scope *scope);
static Expr *unary_expression(TokenIterator *ts, Scope *scope);
static Expr *cast_expression(TokenIterator *ts, Scope *scope);
static Expr *binary_expression(TokenIterator *ts, Scope *scope, const int *tks,
                               const int *exs,
                               Expr *(*op_parser)(TokenIterator *, Scope *));
static Expr *multiplicative_expression(TokenIterator *ts, Scope *scope);
static Expr *additive_expression(TokenIterator *ts, Scope *scope);
static Expr *shift_expression(TokenIterator *ts, Scope *scope);
static Expr *relational_expression(TokenIterator *ts, Scope *scope);
static Expr *equality_expression(TokenIterator *ts, Scope *scope);
static Expr *and_expression(TokenIterator *ts, Scope *scope);
static Expr *exclusive_or_expression(TokenIterator *ts, Scope *scope);
static Expr *inclusive_or_expression(TokenIterator *ts, Scope *scope);
static Expr *logical_and_expression(TokenIterator *ts, Scope *scope);
static Expr *logical_or_expression(TokenIterator *ts, Scope *scope);
static Expr *conditional_expression(TokenIterator *ts, Scope *scope);
static Expr *assignment_expression(TokenIterator *ts, Scope *scope);
static Expr *expression(TokenIterator *ts, Scope *scope);

// declaration
static VarDefVector *declaration(TokenIterator *ts, Scope *scope);
static void declaration_specifiers(TokenIterator *ts, Scope *scope, Type **type,
                                   StorageClassSpecifier *scs,
                                   FunctionSpecifier *fs);
static Type *struct_or_union_specifier(Scope *scope, TokenIterator *ts,
                                       Token *token);
static void struct_declaration(Scope *scope, TokenIterator *ts, Type *type);
static void struct_declarator(Scope *scope, TokenIterator *ts, Type *base_type,
                              Token **name, Type **type, const Range **range);
static Type *specifier_qualifier_list(Scope *scope, TokenIterator *ts);
static Type *enum_specifier(Scope *scope, TokenIterator *ts, Token *token);
static void enumerator(Scope *scope, TokenIterator *ts, Type *type, int *val);
static void declarator_common(Scope *scope, TokenIterator *ts,
                              declarator_type_t dec_type, Type *base_type,
                              Token **name, Type **type, const Range **range);
static void direct_declarator_common(Scope *scope, TokenIterator *ts,
                                     declarator_type_t dec_type,
                                     Type *base_type, Token **name, Type **type,
                                     const Range **range);
static void declarator(Scope *scope, TokenIterator *ts, Type *base_type,
                       Token **name, Type **type, const Range **range);
static Param *parameter_declaration(Scope *scope, TokenIterator *ts);
static Param *parameter_declarator(Scope *scope, TokenIterator *ts,
                                   Type *base_type);
static Type *type_name(Scope *scope, TokenIterator *ts);
static void abstract_declarator(Scope *scope, TokenIterator *ts,
                                Type *base_type, Type **type,
                                const Range **range);
static ParseInit *parse_initializer(TokenIterator *ts, Scope *scope);
static const Member *consume_member_designator(Type *type, InitElem *elem);
static void assign_struct_initializer(Scope *scope, InitElemVector *list,
                                      bool is_root, Type *type,
                                      const Range *range, Initializer **init);
static void assign_union_initializer(Scope *scope, InitElemVector *list,
                                     bool is_root, Type *type,
                                     const Range *range, Initializer **init);
static void assign_array_initializer(Scope *scope, InitElemVector *list,
                                     bool is_root, Type *type,
                                     const Range *range, Initializer **init);
static void assign_initializer(Scope *scope, ParseInit *pinit, Type *type,
                               Initializer **init);
static void assign_initializer_list(Scope *scope, InitElemVector *list,
                                    bool is_root, Type *type,
                                    const Range *range, Initializer **init);
static void initializer(TokenIterator *ts, Scope *scope, Type *type,
                        Initializer **init, const Range **range);

// statement
static Stmt *statement(TokenIterator *ts, Scope *scope);
static Stmt *compound_statement(TokenIterator *ts, Scope *scope);

// top-level
static Function *function_definition(TokenIterator *ts, Scope *global_scope,
                                     Type *type, const char *name,
                                     StorageClassSpecifier scs,
                                     FunctionSpecifier fs, const Range *start);
static GlobalVar *new_global_variable(Type *type, const char *name,
                                      const Range *range,
                                      StorageClassSpecifier scs);
static TranslationUnit *translation_unit(const Reader *reader,
                                         TokenIterator *ts);

TranslationUnit *parse(const Reader *reader, TokenIterator *ts) {
  return translation_unit(reader, ts);
}

static Initializer *new_initializer(Type *type) {
  Initializer *init = NEW(Initializer);
  init->type = type;
  init->members = NULL;
  init->elements = NULL;
  switch (type->ty) {
  case TY_STRUCT: {
    init->members = NEW_VECTOR(MemberInitializerVector);
    StructBody *body = type->struct_body;
    VEC_FOREACH (Member *member, body->members) {
      MemberInitializer *meminit = new_member_initializer(member);
      VEC_PUSH(init->members, meminit);
    }
    break;
  }
  case TY_UNION: {
    init->members = NEW_VECTOR(MemberInitializerVector);
    StructBody *body = type->struct_body;
    if (VEC_LEN(body->members) > 0) {
      MemberInitializer *meminit = new_member_initializer(NULL);
      VEC_PUSH(init->members, meminit);
    }
    break;
  }
  case TY_ARRAY:
    init->elements = NEW_VECTOR(InitializerVector);
    VEC_EXTEND(init->elements, type->array.len, NULL);
    break;
  default:
    break;
  }
  return init;
}

static MemberInitializer *new_member_initializer(Member *member) {
  MemberInitializer *meminit = NEW(MemberInitializer);
  meminit->member = member;
  return meminit;
}

static ParseInit *new_parse_init_list(const Range *range,
                                      InitElemVector *list) {
  ParseInit *pinit = NEW(ParseInit);
  pinit->range = range;
  pinit->list = list;
  return pinit;
}

static ParseInit *new_parse_init_expr(Expr *expr) {
  ParseInit *pinit = NEW(ParseInit);
  pinit->range = expr->range;
  pinit->expr = expr;
  return pinit;
}

static GlobalCtxt *new_global_ctxt(void) {
  GlobalCtxt *gcx = NEW(GlobalCtxt);
  gcx->func_list = NEW_VECTOR(FunctionVector);
  gcx->gvar_list = NEW_VECTOR(GlobalVarVector);
  gcx->str_list = NEW_VECTOR(StringLiteralVector);

  return gcx;
}

static FuncCtxt *new_func_ctxt(const char *name, Type *type) {
  FuncCtxt *fcx = NEW(FuncCtxt);

  fcx->name = name;
  fcx->type = type;
  fcx->var_list = NEW_VECTOR(StackVarVector);
  fcx->switches = NEW_VECTOR(StmtVector);
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

static Scope *new_global_scope(GlobalCtxt *gcx, const Reader *reader) {
  Scope *scope = new_scope(gcx, NULL, NULL);

  const Range *range = range_builtin(reader);

  register_typedef(scope, "__builtin_va_list", new_type_builtin_va_list(range));

  register_builtin_func(scope, "__builtin_va_start", builtin_va_start_handler);
  register_builtin_func(scope, "__builtin_va_arg", builtin_va_arg_handler);
  register_builtin_func(scope, "__builtin_va_end", builtin_va_end_handler);
  register_builtin_func(scope, "__builtin_va_copy", builtin_va_copy_handler);

  return scope;
}

static Scope *new_func_scope(Scope *global, FuncCtxt *fcx) {
  Scope *scope = new_scope(global->global_ctxt, fcx, global);

  register_string(scope, "__func__", fcx->name);

  return scope;
}

static Scope *new_inner_scope(Scope *outer) {
  return new_scope(outer->global_ctxt, outer->func_ctxt, outer);
}

Scope *new_pp_scope(const Reader *reader) {
  GlobalCtxt *gcx = new_global_ctxt();
  return new_global_scope(gcx, reader);
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
                      NULL, NULL, NULL);

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
                      NULL, NULL, NULL);
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
  VEC_PUSH(fcx->var_list, var);

  if (!register_decl(scope, DECL_STACK_VAR, name->ident, type, var, NULL, NULL,
                     NULL, NULL)) {
    range_error(range, "同じ名前のローカル変数が複数あります: %s", name->ident);
  }

  return var;
}

static GlobalVar *register_global_var(Scope *scope, Token *name, Type *type,
                                      const Range *range,
                                      StorageClassSpecifier scs) {
  GlobalVar *gvar = new_global_variable(type, name->ident, range, scs);
  if (!register_decl(scope, DECL_GLOBAL_VAR, name->ident, type, NULL, gvar,
                     NULL, NULL, NULL)) {
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
                     NULL, NULL)) {
    range_error(name->range, "定義済みの識別子です: %s", name->ident);
  }
}

static void register_string(Scope *scope, const char *name, const char *val) {
  Type *type =
      new_type_array(new_type(TY_CHAR, EMPTY_TYPE_QUALIFIER),
                     new_number_int(strlen(val)), EMPTY_TYPE_QUALIFIER);
  (void)register_decl(scope, DECL_STRING, name, type, NULL, NULL, NULL, val,
                      NULL);
}

static void register_builtin_func(Scope *scope, const char *name,
                                  builtin_func_handler_t *handler) {
  Type *type = new_type(TY_BUILTIN, EMPTY_TYPE_QUALIFIER);
  (void)register_decl(scope, DECL_BUILTIN_FUNC, name, type, NULL, NULL, NULL,
                      NULL, handler);
}

static bool register_decl(Scope *scope, decl_t kind, const char *name,
                          Type *type, StackVar *svar, GlobalVar *gvar,
                          Number *number, const char *string,
                          builtin_func_handler_t *handler) {
  if (map_get(scope->decl_map, name)) {
    return false;
  }
  Decl *decl = NEW(Decl);
  decl->kind = kind;
  decl->type = type;
  decl->stack_var = svar;
  decl->global_var = gvar;
  decl->number = number;
  decl->string = string;
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

static bool register_tag(Scope *scope, const char *tag, Type *type) {
  if (map_get(scope->tag_map, tag)) {
    return false;
  }
  map_put(scope->tag_map, tag, type);
  return true;
}

static Type *get_tag(Scope *scope, const char *tag) {
  while (scope != NULL) {
    Type *type = map_get(scope->tag_map, tag);
    if (type) {
      return type;
    }
    scope = scope->outer;
  }
  return NULL;
}

static bool register_typedef(Scope *scope, const char *name, Type *type) {
  if (map_get(scope->typedef_map, name)) {
    return false;
  }
  map_put(scope->typedef_map, name, type);
  return true;
}

static Type *get_typedef(Scope *scope, const char *name) {
  while (scope != NULL) {
    Type *type = map_get(scope->typedef_map, name);
    if (type != NULL) {
      return type;
    }
    scope = scope->outer;
  }
  return NULL;
}

static StringLiteral *get_string_literal(Scope *scope, const char *val) {
  VEC_FOREACH (StringLiteral *lit, scope->global_ctxt->str_list) {
    if (strcmp(lit->val, val) == 0) {
      return lit;
    }
  }

  StringLiteral *lit = NEW(StringLiteral);
  lit->name = make_label("str");
  lit->val = val;
  VEC_PUSH(scope->global_ctxt->str_list, lit);
  return lit;
}

static void register_label(FuncCtxt *fcx, const char *c_label,
                           const char *asm_label) {
  map_put(fcx->label_map, c_label, (void *)asm_label);
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

static bool consume_storage_class_specifier(TokenIterator *ts,
                                            StorageClassSpecifier *scs) {
  bool consumed = false;
  while (true) {
    Token *token;
    if ((token = ts_consume(ts, TK_TYPEDEF)) != NULL) {
      scs->is_typedef = true;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_EXTERN)) != NULL) {
      scs->is_extern = true;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_STATIC)) != NULL) {
      scs->is_static = true;
      consumed = true;
      continue;
    }

    assert(!token_is_storage_class_specifier(ts_peek(ts)));
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
  case TK_TYPEOF:
    return true;
  case TK_IDENT:
    return get_typedef(scope, token->ident) != NULL;
  default:
    return false;
  }
}

static bool consume_type_specifier(Scope *scope, TokenIterator *ts,
                                   TypeSpecifier *tspec) {
  bool consumed = false;
  while (true) {
    bool base_type_specified = tspec->base_type != BASE_TYPE_UNSPECIFIED;
    bool signedness_specified = tspec->signedness != SIGN_UNSPECIFIED;
    bool size_specified = tspec->size != SIZE_UNSPECIFIED;
    bool concrete_type_specified = tspec->concrete_type != NULL;
    bool can_be_concrete_type = !base_type_specified && !signedness_specified &&
                                !size_specified && !concrete_type_specified;

    Token *token;
    if ((token = ts_consume(ts, TK_VOID)) != NULL) {
      if (concrete_type_specified) {
        range_error(token->range, "無効な型です");
      }
      tspec->concrete_type = new_type(TY_VOID, EMPTY_TYPE_QUALIFIER);
      tspec->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_BOOL)) != NULL) {
      if (concrete_type_specified) {
        range_error(token->range, "無効な型です");
      }
      tspec->concrete_type = new_type(TY_BOOL, EMPTY_TYPE_QUALIFIER);
      tspec->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_FLOAT)) != NULL) {
      if (concrete_type_specified) {
        range_error(token->range, "無効な型です");
      }
      tspec->concrete_type = new_type(TY_FLOAT, EMPTY_TYPE_QUALIFIER);
      tspec->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_STRUCT)) != NULL ||
        (token = ts_consume(ts, TK_UNION)) != NULL) {
      if (concrete_type_specified) {
        range_error(token->range, "無効な型です");
      }
      tspec->concrete_type = struct_or_union_specifier(scope, ts, token);
      tspec->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_ENUM)) != NULL) {
      if (concrete_type_specified) {
        range_error(token->range, "無効な型です");
      }
      tspec->concrete_type = enum_specifier(scope, ts, token);
      tspec->range = token->range;
      consumed = true;
      continue;
    }

    if ((token = ts_consume(ts, TK_INT)) != NULL) {
      if (base_type_specified) {
        range_error(token->range, "無効な型です");
      }
      tspec->base_type = BASE_TYPE_INT;
      tspec->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_CHAR)) != NULL) {
      if (base_type_specified) {
        range_error(token->range, "無効な型です");
      }
      tspec->base_type = BASE_TYPE_CHAR;
      tspec->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_DOUBLE)) != NULL) {
      if (base_type_specified) {
        range_error(token->range, "無効な型です");
      }
      tspec->base_type = BASE_TYPE_DOUBLE;
      tspec->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_SHORT)) != NULL) {
      if (size_specified) {
        range_error(token->range, "無効な型です");
      }
      tspec->size = SIZE_SHORT;
      tspec->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_LONG)) != NULL) {
      if (tspec->size == SIZE_SHORT || tspec->size == SIZE_LLONG) {
        range_error(token->range, "無効な型です");
      }
      if (tspec->size == SIZE_UNSPECIFIED) {
        tspec->size = SIZE_LONG;
      } else {
        assert(tspec->size == SIZE_LONG);
        tspec->size = SIZE_LLONG;
      }
      tspec->range = token->range;
      consumed = true;
      continue;
    }

    if ((token = ts_consume(ts, TK_SIGNED)) != NULL) {
      if (signedness_specified) {
        range_error(token->range, "無効な型です");
      }
      tspec->signedness = SIGN_SIGNED;
      tspec->range = token->range;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_UNSIGNED)) != NULL) {
      if (signedness_specified) {
        range_error(token->range, "無効な型です");
      }
      tspec->signedness = SIGN_UNSIGNED;
      tspec->range = token->range;
      consumed = true;
      continue;
    }

    // NonStandard/GNU: referring to a type with typeof
    if ((token = ts_consume(ts, TK_TYPEOF)) != NULL) {
      ts_expect(ts, '(');
      Type *type;
      if (token_is_type_name(scope, ts_peek_ahead(ts, 0))) {
        type = type_name(scope, ts);
      } else {
        Expr *expr = unary_expression(ts, scope);
        type = expr->val_type;
      }
      Token *end = ts_expect(ts, ')');
      tspec->concrete_type = type;
      tspec->range = range_join(token->range, end->range);
      consumed = true;
      continue;
    }

    token = ts_peek(ts);
    if (token->ty == TK_IDENT && can_be_concrete_type) {
      Type *type = get_typedef(scope, token->ident);
      if (type != NULL) {
        ts_succ(ts);
        tspec->concrete_type = type;
        tspec->range = token->range;
        consumed = true;
        continue;
      }
    }

    assert(!can_be_concrete_type ||
           !token_is_type_specifier(scope, ts_peek(ts)));
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
static bool consume_type_qualifier(TokenIterator *ts, TypeQualifier *tq) {
  bool consumed = false;
  while (true) {
    Token *token;
    if ((token = ts_consume(ts, TK_CONST)) != NULL) {
      if (tq->is_const) {
        range_warn(token->range, "`const` が重複しています");
      }
      tq->is_const = true;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_RESTRICT)) != NULL) {
      if (tq->is_restrict) {
        range_warn(token->range, "`restrict` が重複しています");
      }
      tq->is_restrict = true;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_VOLATILE)) != NULL) {
      if (tq->is_volatile) {
        range_warn(token->range, "`volatile` が重複しています");
      }
      tq->is_volatile = true;
      consumed = true;
      continue;
    }

    assert(!token_is_type_qualifier(ts_peek(ts)));
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

static bool consume_function_specifier(TokenIterator *ts,
                                       FunctionSpecifier *fs) {
  bool consumed = false;
  while (true) {
    Token *token;
    if ((token = ts_consume(ts, TK_INLINE)) != NULL) {
      fs->is_inline = true;
      consumed = true;
      continue;
    }
    if ((token = ts_consume(ts, TK_NORETURN)) != NULL) {
      fs->is_noreturn = true;
      consumed = true;
      continue;
    }

    assert(!token_is_function_specifier(ts_peek(ts)));
    break;
  }

  return consumed;
}

static Expr *builtin_va_start_handler(Expr *callee, ExprVector *arguments,
                                      const Range *range) {
  return new_expr_builtin_va_start(callee, arguments, range);
}

static Expr *builtin_va_arg_handler(Expr *callee, ExprVector *arguments,
                                    const Range *range) {
  return new_expr_builtin_va_arg(callee, arguments, range);
}
static Expr *builtin_va_end_handler(Expr *callee, ExprVector *arguments,
                                    const Range *range) {
  return new_expr_builtin_va_end(callee, arguments, range);
}
static Expr *builtin_va_copy_handler(Expr *callee, ExprVector *arguments,
                                     const Range *range) {
  return new_expr_builtin_va_copy(callee, arguments, range);
}

static Expr *primary_expression(TokenIterator *ts, Scope *scope) {
  Token *token = NULL;
  if ((token = ts_consume(ts, TK_NUM)) != NULL) {
    return new_expr_num(token->num, token->range);
  }
  if ((token = ts_consume(ts, TK_CHARCONST)) != NULL) {
    return new_expr_num(token->char_val, token->range);
  }
  if ((token = ts_consume(ts, TK_STR)) != NULL) {
    StringLiteral *lit = get_string_literal(scope, token->str);
    return new_expr_str(lit, token->range);
  }

  if ((token = ts_consume(ts, TK_IDENT)) != NULL) {
    const char *name = token->ident;
    const Range *range = token->range;
    Decl *decl = get_decl(scope, name);
    if (decl == NULL) {
      range_error(range, "use of undeclared identifier: %s", name);
    }
    switch (decl->kind) {
    case DECL_STACK_VAR: {
      return new_expr_stack_var(decl->stack_var, range);
    }
    case DECL_GLOBAL_VAR: {
      GlobalVar *gvar = decl->global_var;
      return new_expr_global_var(decl->type, gvar != NULL ? gvar->name : name,
                                 gvar, range);
    }
    case DECL_NUMBER: {
      return new_expr_num(*decl->number, range);
    }
    case DECL_STRING: {
      StringLiteral *lit = get_string_literal(scope, decl->string);
      return new_expr_str(lit, range);
    }
    case DECL_BUILTIN_FUNC: {
      return new_expr_builtin_func(name, decl->builtin_func, range);
    }
    }
    range_internal_error(range, "invalid decl kind: %d", decl->kind);
  }

  if ((token = ts_consume(ts, '(')) != NULL) {
    // NonStandard/GNU: Statement Exprs
    if (ts_peek(ts)->ty == '{') {
      Scope *inner = new_inner_scope(scope);
      Stmt *stmt = compound_statement(ts, inner);
      Token *end = ts_expect(ts, ')');
      return new_expr_stmt(stmt, range_join(token->range, end->range));
    }

    Expr *expr = expression(ts, scope);
    Token *end = ts_expect(ts, ')');
    expr->range = range_join(token->range, end->range);
    return expr;
  }

  if ((token = ts_consume(ts, TK_GENERIC)) != NULL) {
    ts_expect(ts, '(');
    Expr *control = assignment_expression(ts, scope);
    ts_expect(ts, ',');
    GenericAssociationVector *assoc_list = NEW_VECTOR(GenericAssociationVector);
    while (ts_peek(ts)->ty != ')') {
      GenericAssociation *assoc = NEW(GenericAssociation);
      if ((token = ts_consume(ts, TK_DEFAULT)) != NULL) {
        assoc->type = NULL;
        assoc->range = token->range;
      } else {
        assoc->range = ts_peek(ts)->range;
        assoc->type = type_name(scope, ts);
      }
      Token *colon = ts_expect(ts, ':');
      assoc->range = range_join(assoc->range, colon->range);
      assoc->expr = assignment_expression(ts, scope);
      VEC_PUSH(assoc_list, assoc);
      if (!ts_consume(ts, ',')) {
        break;
      }
    }
    ts_expect(ts, ')');
    return new_expr_generic(control, assoc_list);
  }

  range_error(ts_peek(ts)->range, "数値でも開きカッコでもないトークンです");
}

static Expr *postfix_expression(TokenIterator *ts, Scope *scope) {
  Token *token;
  Expr *expr = primary_expression(ts, scope);
  while (true) {
    if (ts_consume(ts, '[')) {
      Expr *operand = expression(ts, scope);
      Token *end = ts_expect(ts, ']');
      expr = new_expr_index(expr, operand, range_join(expr->range, end->range));
      continue;
    }

    if (ts_consume(ts, '(')) {
      ExprVector *arguments = NULL;
      if (ts_peek(ts)->ty != ')') {
        arguments = argument_expression_list(ts, scope);
      }
      Token *end = ts_expect(ts, ')');
      expr =
          new_expr_call(expr, arguments, range_join(expr->range, end->range));
      continue;
    }

    if (ts_consume(ts, '.')) {
      Token *member = ts_expect(ts, TK_IDENT);
      expr = new_expr_dot(expr, member->ident,
                          range_join(expr->range, member->range));
      continue;
    }

    if (ts_consume(ts, TK_ARROW)) {
      Token *member = ts_expect(ts, TK_IDENT);
      expr = new_expr_arrow(expr, member->ident,
                            range_join(expr->range, member->range));
      continue;
    }

    if ((token = ts_consume(ts, TK_INC)) != NULL) {
      expr = new_expr_postfix(EX_POST_INC, expr,
                              range_join(expr->range, token->range));
      continue;
    }

    if ((token = ts_consume(ts, TK_DEC)) != NULL) {
      expr = new_expr_postfix(EX_POST_DEC, expr,
                              range_join(expr->range, token->range));
      continue;
    }

    return expr;
  }
}

static ExprVector *argument_expression_list(TokenIterator *ts, Scope *scope) {
  ExprVector *arguments = NEW_VECTOR(ExprVector);
  while (true) {
    VEC_PUSH(arguments, assignment_expression(ts, scope));
    if (!ts_consume(ts, ',')) {
      break;
    }
  }
  return arguments;
}

static Expr *unary_expression(TokenIterator *ts, Scope *scope) {
  const int TKS[] = {'&', '*', '+', '-', '~', '!', TK_INC, TK_DEC, '\0'};
  const int EXS[] = {EX_ADDRESS, EX_INDIRECT, EX_PLUS,    EX_MINUS, EX_NOT,
                     EX_LOG_NOT, EX_PRE_INC,  EX_PRE_DEC, '\0'};
  for (int i = 0; TKS[i] != '\0'; i++) {
    int tk = TKS[i];
    int ex = EXS[i];
    Token *token;
    if ((token = ts_consume(ts, tk)) != NULL) {
      Expr *operand = cast_expression(ts, scope);
      return new_expr_unary(ex, operand,
                            range_join(token->range, operand->range));
    }
  }

  Token *token;
  if ((token = ts_consume(ts, TK_SIZEOF)) != NULL) {
    if (ts_peek(ts)->ty != '(' ||
        !token_is_type_name(scope, ts_peek_ahead(ts, 1))) {
      Expr *expr = unary_expression(ts, scope);
      return new_expr_num(
          new_number_size_t(get_val_size(expr->val_type, expr->range)),
          range_join(token->range, expr->range));
    }
    ts_expect(ts, '(');
    Type *type = type_name(scope, ts);
    Token *end = ts_expect(ts, ')');
    const Range *range = range_join(token->range, end->range);
    return new_expr_num(new_number_size_t(get_val_size(type, range)), range);
  }
  if ((token = ts_consume(ts, TK_ALIGNOF)) != NULL) {
    ts_expect(ts, '(');
    Type *type = type_name(scope, ts);
    Token *end = ts_expect(ts, ')');
    const Range *range = range_join(token->range, end->range);
    return new_expr_num(new_number_size_t(get_val_align(type, range)), range);
  }
  return postfix_expression(ts, scope);
}

static Expr *cast_expression(TokenIterator *ts, Scope *scope) {
  Token *token = ts_peek(ts);
  if (token->ty == '(' && token_is_type_name(scope, ts_peek_ahead(ts, 1))) {
    const Range *start = token->range;
    ts_succ(ts);
    Type *val_type = type_name(scope, ts);
    ts_expect(ts, ')');
    if (ts_peek(ts)->ty == '{') {
      Initializer *init = NULL;
      const Range *range = NULL;
      initializer(ts, scope, val_type, &init, &range);

      StackVar *svar = NULL;
      FuncCtxt *fcx = scope->func_ctxt;
      if (fcx != NULL) {
        svar = NEW(StackVar);
        svar->name = ".compound";
        svar->offset = INT_MIN;
        svar->type = val_type;
        svar->range = range;
        VEC_PUSH(fcx->var_list, svar);
      }

      return new_expr_compound(val_type, svar, init, range);
    }
    Expr *operand = cast_expression(ts, scope);
    return new_expr_cast(val_type, operand, range_join(start, operand->range));
  }
  return unary_expression(ts, scope);
}

static Expr *binary_expression(TokenIterator *ts, Scope *scope, const int *tks,
                               const int *exs,
                               Expr *(*op_parser)(TokenIterator *, Scope *)) {
  Expr *expr = op_parser(ts, scope);
  bool found;
  do {
    found = false;
    for (int i = 0; tks[i] != '\0'; i++) {
      int tk = tks[i];
      int ex = exs[i];
      Token *token;
      if ((token = ts_consume(ts, tk)) != NULL) {
        Expr *operand = op_parser(ts, scope);
        expr = new_expr_binop(ex, expr, operand,
                              range_join(expr->range, operand->range));
        found = true;
        break;
      }
    }
  } while (found);
  return expr;
}

static Expr *multiplicative_expression(TokenIterator *ts, Scope *scope) {
  const int TKS[] = {'*', '/', '%', '\0'};
  const int EXS[] = {EX_MUL, EX_DIV, EX_MOD, '\0'};
  return binary_expression(ts, scope, TKS, EXS, cast_expression);
}
static Expr *additive_expression(TokenIterator *ts, Scope *scope) {
  const int TKS[] = {'+', '-', '\0'};
  const int EXS[] = {EX_ADD, EX_SUB, '\0'};
  return binary_expression(ts, scope, TKS, EXS, multiplicative_expression);
}
static Expr *shift_expression(TokenIterator *ts, Scope *scope) {
  const int TKS[] = {TK_LSHIFT, TK_RSHIFT, '\0'};
  const int EXS[] = {EX_LSHIFT, EX_RSHIFT, '\0'};
  return binary_expression(ts, scope, TKS, EXS, additive_expression);
}
static Expr *relational_expression(TokenIterator *ts, Scope *scope) {
  const int TKS[] = {'<', '>', TK_LTEQ, TK_GTEQ, '\0'};
  const int EXS[] = {EX_LT, EX_GT, EX_LTEQ, EX_GTEQ, '\0'};
  return binary_expression(ts, scope, TKS, EXS, shift_expression);
}
static Expr *equality_expression(TokenIterator *ts, Scope *scope) {
  const int TKS[] = {TK_EQEQ, TK_NOTEQ, '\0'};
  const int EXS[] = {EX_EQEQ, EX_NOTEQ, '\0'};
  return binary_expression(ts, scope, TKS, EXS, relational_expression);
}
static Expr *and_expression(TokenIterator *ts, Scope *scope) {
  const int TKS[] = {'&', '\0'};
  const int EXS[] = {EX_AND, '\0'};
  return binary_expression(ts, scope, TKS, EXS, equality_expression);
}
static Expr *exclusive_or_expression(TokenIterator *ts, Scope *scope) {
  const int TKS[] = {'^', '\0'};
  const int EXS[] = {EX_XOR, '\0'};
  return binary_expression(ts, scope, TKS, EXS, and_expression);
}
static Expr *inclusive_or_expression(TokenIterator *ts, Scope *scope) {
  const int TKS[] = {'|', '\0'};
  const int EXS[] = {EX_OR, '\0'};
  return binary_expression(ts, scope, TKS, EXS, exclusive_or_expression);
}
static Expr *logical_and_expression(TokenIterator *ts, Scope *scope) {
  const int TKS[] = {TK_LOGAND, '\0'};
  const int EXS[] = {EX_LOG_AND, '\0'};
  return binary_expression(ts, scope, TKS, EXS, inclusive_or_expression);
}
static Expr *logical_or_expression(TokenIterator *ts, Scope *scope) {
  const int TKS[] = {TK_LOGOR, '\0'};
  const int EXS[] = {EX_LOG_OR, '\0'};
  return binary_expression(ts, scope, TKS, EXS, logical_and_expression);
}
static Expr *conditional_expression(TokenIterator *ts, Scope *scope) {
  Expr *cond = logical_or_expression(ts, scope);
  if (ts_consume(ts, '?')) {
    Expr *then_expr = expression(ts, scope);
    ts_expect(ts, ':');
    Expr *else_expr = conditional_expression(ts, scope);
    return new_expr_cond(cond, then_expr, else_expr,
                         range_join(cond->range, else_expr->range));
  }
  return cond;
}

static Expr *assignment_expression(TokenIterator *ts, Scope *scope) {
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

  Expr *lhs = conditional_expression(ts, scope);
  for (int i = 0; TKS[i] != '\0'; i++) {
    int tk = TKS[i];
    int ex = EXS[i];
    if (ts_consume(ts, tk)) {
      Expr *rhs = assignment_expression(ts, scope);
      const Range *range = range_join(lhs->range, rhs->range);
      return new_expr_binop(ex, lhs, rhs, range);
    }
  }

  return lhs;
}

static Expr *expression(TokenIterator *ts, Scope *scope) {
  const int TKS[] = {',', '\0'};
  const int EXS[] = {EX_COMMA, '\0'};
  return binary_expression(ts, scope, TKS, EXS, assignment_expression);
}

Expr *constant_expression(TokenIterator *ts, Scope *scope) {
  Expr *expr = conditional_expression(ts, scope);
  sema_expr(expr);
  if (expr->ty != EX_NUM) {
    range_error(expr->range, "not a constant expression");
  }
  return expr;
}

Number integer_constant_expression(TokenIterator *ts, Scope *scope) {
  Expr *expr = constant_expression(ts, scope);
  assert(expr->ty == EX_NUM);
  if (!is_integer_type(expr->val_type)) {
    range_error(expr->range,
                "expression is not an integer constant expression");
  }
  return expr->num;
}

static VarDefVector *declaration(TokenIterator *ts, Scope *scope) {
  VarDefVector *def_list = NEW_VECTOR(VarDefVector);

  StorageClassSpecifier scs = EMPTY_STORAGE_CLASS_SPECIFIER;
  Type *base_type = NULL;
  FunctionSpecifier fs = EMPTY_FUNCTION_SPECIFIER;

  declaration_specifiers(ts, scope, &base_type, &scs, &fs);

  if (ts_consume(ts, ';')) {
    return def_list;
  }

  Token *name;
  Type *type;
  const Range *range;
  declarator(scope, ts, base_type, &name, &type, &range);

  if (scs.is_typedef) {
    register_typedef(scope, name->ident, type);
  } else if (scs.is_extern) {
    register_extern(scope, name, type);
  } else {
    if (is_func_type(type)) {
      VarDef *def = register_func(scope, name, type);
      if (ts_peek(ts)->ty == '{') {
        def->func =
            function_definition(ts, scope, type, name->ident, scs, fs, range);
        VEC_PUSH(def_list, def);
        return def_list;
      }
    } else {
      VarDef *def = register_var(scope, name, type, range, scs);
      if (ts_consume(ts, '=')) {
        initializer(ts, scope, type, &def->init, NULL);
      }
      VEC_PUSH(def_list, def);
    }
  }

  while (ts_consume(ts, ',')) {
    declarator(scope, ts, base_type, &name, &type, &range);
    if (scs.is_typedef) {
      register_typedef(scope, name->ident, type);
    } else if (scs.is_extern) {
      register_extern(scope, name, type);
    } else {
      if (is_func_type(type)) {
        (void)register_func(scope, name, type);
      } else {
        VarDef *def = register_var(scope, name, type, range, scs);
        if (ts_consume(ts, '=')) {
          initializer(ts, scope, type, &def->init, NULL);
        }
        VEC_PUSH(def_list, def);
      }
    }
  }
  ts_expect(ts, ';');

  return def_list;
}

static void declaration_specifiers(TokenIterator *ts, Scope *scope, Type **type,
                                   StorageClassSpecifier *scs,
                                   FunctionSpecifier *fs) {
  TypeSpecifier tspec = EMPTY_TYPE_SPECIFIER;
  TypeQualifier tqual = EMPTY_TYPE_QUALIFIER;
  while (true) {
    if (consume_storage_class_specifier(ts, scs)) {
      continue;
    }
    if (consume_type_specifier(scope, ts, &tspec)) {
      continue;
    }
    if (consume_type_qualifier(ts, &tqual)) {
      continue;
    }
    if (consume_function_specifier(ts, fs)) {
      continue;
    }
    break;
  }
  *type = construct_type_specifier(tspec, tqual);
  if (*type == NULL) {
    range_error(ts_peek(ts)->range, "型名がありません");
  }
}

static Type *struct_or_union_specifier(Scope *scope, TokenIterator *ts,
                                       Token *token) {
  assert(token->ty == TK_STRUCT || token->ty == TK_UNION);
  Token *tag = ts_consume(ts, TK_IDENT);

  type_t ty = token->ty == TK_STRUCT ? TY_STRUCT : TY_UNION;

  if (ts_consume(ts, '{')) {
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
        if (body->members != NULL) {
          range_error(tag->range, "構造体の多重定義です");
        }
        init_struct_body(body);

        type = predef_type;
      }
    } else {
      type = new_type_struct(ty, tag ? tag->ident : NULL, EMPTY_TYPE_QUALIFIER);
    }
    while (ts_peek(ts)->ty != '}') {
      struct_declaration(scope, ts, type);
    }
    ts_expect(ts, '}');
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

static void struct_declaration(Scope *scope, TokenIterator *ts, Type *type) {
  assert(type->ty == TY_STRUCT || type->ty == TY_UNION);

  Type *base_type = specifier_qualifier_list(scope, ts);

  Token *member_name = NULL;
  Type *member_type = base_type;
  const Range *range = ts_peek(ts)->range;
  if (ts_peek(ts)->ty != ';') {
    struct_declarator(scope, ts, base_type, &member_name, &member_type, &range);
  }
  register_struct_member(type, member_name != NULL ? member_name->ident : NULL,
                         member_type, range);

  while (ts_consume(ts, ',')) {
    struct_declarator(scope, ts, base_type, &member_name, &member_type, &range);
    register_struct_member(type,
                           member_name != NULL ? member_name->ident : NULL,
                           member_type, range);
  }
  ts_expect(ts, ';');
}

static void struct_declarator(Scope *scope, TokenIterator *ts, Type *base_type,
                              Token **name, Type **type, const Range **range) {
  if (ts_peek(ts)->ty != ':') {
    declarator(scope, ts, base_type, name, type, range);
  }

  if (ts_consume(ts, ':')) {
    if (!is_integer_type(*type)) {
      range_error(*range, "bit-field has non-integral type '%s'",
                  format_type(*type, false));
    }

    Number num = integer_constant_expression(ts, scope);
    int bit_width;
    int size = get_val_size(*type, *range);
    SET_NUMBER_VAL(bit_width, &num);

    if (bit_width < 0) {
      range_error(*range, "bit-field has negative width (%d)", bit_width);
    }
    if (bit_width > size * CHAR_BIT) {
      range_error(
          *range,
          "width of bit-field (%d bits) exceeds witdh of its type (%d bits)",
          bit_width, size * CHAR_BIT);
    }
    if (name != NULL && bit_width == 0) {
      range_error(*range, "named bit-field '%s' has zero width",
                  (*name)->ident);
    }
    // TODO: implement bit-field
  }
}

static Type *specifier_qualifier_list(Scope *scope, TokenIterator *ts) {
  TypeSpecifier tspec = EMPTY_TYPE_SPECIFIER;
  TypeQualifier tqual = EMPTY_TYPE_QUALIFIER;
  while (true) {
    if (consume_type_specifier(scope, ts, &tspec)) {
      continue;
    }
    if (consume_type_qualifier(ts, &tqual)) {
      continue;
    }
    break;
  }
  Type *type = construct_type_specifier(tspec, tqual);
  if (type == NULL) {
    range_error(ts_peek(ts)->range, "型名がありません");
  }
  return type;
}

static Type *enum_specifier(Scope *scope, TokenIterator *ts, Token *token) {
  Token *tag_ident = ts_consume(ts, TK_IDENT);
  if (tag_ident == NULL && ts_peek(ts)->ty != '{') {
    range_error(token->range, "列挙型のタグまたは `{` がありません");
  }

  const char *tag = tag_ident != NULL ? tag_ident->ident : NULL;

  if (ts_consume(ts, '{')) {
    Type *type = new_type_enum(tag != NULL ? tag : NULL, EMPTY_TYPE_QUALIFIER);
    if (tag != NULL) {
      if (!register_tag(scope, tag, type)) {
        range_error(tag_ident->range, "同じタグ名の列挙型の多重定義です: %s",
                    tag);
      }
    }
    int val = 0;
    while (ts_peek(ts)->ty != '}') {
      enumerator(scope, ts, type, &val);
    }
    ts_expect(ts, '}');
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

static void enumerator(Scope *scope, TokenIterator *ts, Type *type, int *val) {
  Token *ident = ts_expect(ts, TK_IDENT);
  if (ts_consume(ts, '=')) {
    Number num = integer_constant_expression(ts, scope);
    SET_NUMBER_VAL(*val, &num);
  }
  ts_consume(ts, ',');

  Number *number = NEW(Number);
  number->type = TY_ENUM;
  number->enum_val = *val;
  register_number(scope, ident, type, number);

  (*val)++;
}

static void declarator_common(Scope *scope, TokenIterator *ts,
                              declarator_type_t dec_type, Type *base_type,
                              Token **name, Type **type, const Range **range) {
  const Range *start = ts_peek(ts)->range;
  while (ts_consume(ts, '*')) {
    TypeQualifier tq = EMPTY_TYPE_QUALIFIER;
    consume_type_qualifier(ts, &tq);
    base_type = new_type_ptr(base_type, tq);
  }
  const Range *end;
  direct_declarator_common(scope, ts, dec_type, base_type, name, type, &end);
  *range = range_join(start, end);
}

static void direct_declarator_common(Scope *scope, TokenIterator *ts,
                                     declarator_type_t dec_type,
                                     Type *base_type, Token **name, Type **type,
                                     const Range **range) {
  Type *placeholder = NEW(Type);
  *range = ts_peek(ts)->range;

  while (true) {
    Token *token;
    if (dec_type != ABSTRACT_DECLARATOR &&
        (token = ts_consume(ts, TK_IDENT)) != NULL) {
      *name = token;
      *type = placeholder;
      *range = range_join(*range, token->range);
      break;
    }

    if (ts_peek(ts)->ty == '(') {
      Token *next = ts_peek_ahead(ts, 1);
      bool is_declarator = next->ty == '*';
      if (dec_type != CONCRETE_DECLARATOR && next->ty == '[') {
        is_declarator = true;
      }
      if (dec_type != ABSTRACT_DECLARATOR && !token_is_type_name(scope, next)) {
        is_declarator = true;
      }
      if (is_declarator) {
        ts_succ(ts);
        const Range *mid;
        declarator_common(scope, ts, dec_type, placeholder, name, type, &mid);
        Token *token = ts_expect(ts, ')');
        *range = range_join(*range, token->range);
        break;
      }
    }

    if (dec_type == CONCRETE_DECLARATOR) {
      range_error(ts_peek(ts)->range, "識別子がありません");
    }
    *name = NULL;
    *type = placeholder;
    *range = *range;
    break;
  }

  while (true) {
    if (ts_consume(ts, '[')) {
      Type *inner = NEW(Type);
      TypeQualifier tq = EMPTY_TYPE_QUALIFIER;
      while (true) {
        if (consume_type_qualifier(ts, &tq)) {
          continue;
        }
        if (ts_consume(ts, TK_STATIC)) {
          continue;
        }
        break;
      }
      if (ts_consume(ts, '*')) {
        *placeholder = *new_type_unsized_array(inner, tq);
      } else if (ts_peek(ts)->ty == ']') {
        *placeholder = *new_type_unsized_array(inner, tq);
      } else {
        Number num = integer_constant_expression(ts, scope);
        *placeholder = *new_type_array(inner, num, tq);
      }
      placeholder = inner;
      Token *end = ts_expect(ts, ']');
      *range = range_join(*range, end->range);
      continue;
    }

    if (ts_consume(ts, '(')) {
      ParamVector *params;
      bool has_varargs = false;
      Token *end;
      if ((end = ts_consume(ts, ')'))) {
        params = NULL;
      } else if ((end = ts_consume2(ts, TK_VOID, ')'))) {
        params = NEW_VECTOR(ParamVector);
      } else {
        params = NEW_VECTOR(ParamVector);
        while (true) {
          Param *param = parameter_declaration(scope, ts);
          if (is_array_type(param->type)) {
            // array型の引数はポインタ型とみなす
            Type *type =
                new_type_ptr(param->type->array.elem, param->type->qualifier);
            param->type = type;
          }
          VEC_PUSH(params, param);
          if (ts_peek(ts)->ty == ')') {
            break;
          }
          ts_expect(ts, ',');
          if (ts_consume(ts, TK_ELIPSIS)) {
            has_varargs = true;
            break;
          }
        }
        end = ts_expect(ts, ')');
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

static void declarator(Scope *scope, TokenIterator *ts, Type *base_type,
                       Token **name, Type **type, const Range **range) {
  declarator_common(scope, ts, CONCRETE_DECLARATOR, base_type, name, type,
                    range);
}

static Param *parameter_declaration(Scope *scope, TokenIterator *ts) {
  StorageClassSpecifier scs = EMPTY_STORAGE_CLASS_SPECIFIER;
  Type *base_type = NULL;
  FunctionSpecifier fs = EMPTY_FUNCTION_SPECIFIER;

  declaration_specifiers(ts, scope, &base_type, &scs, &fs);

  return parameter_declarator(scope, ts, base_type);
}

static Param *parameter_declarator(Scope *scope, TokenIterator *ts,
                                   Type *base_type) {
  Param *param = NEW(Param);
  declarator_common(scope, ts, ANY_DECLARATOR, base_type, &param->name,
                    &param->type, &param->range);
  return param;
}

static Type *type_name(Scope *scope, TokenIterator *ts) {
  Type *base_type = specifier_qualifier_list(scope, ts);

  Type *type;
  const Range *range;
  abstract_declarator(scope, ts, base_type, &type, &range);
  return type;
}

static void abstract_declarator(Scope *scope, TokenIterator *ts,
                                Type *base_type, Type **type,
                                const Range **range) {
  Token *name;
  declarator_common(scope, ts, ABSTRACT_DECLARATOR, base_type, &name, type,
                    range);
  assert(name == NULL);
}

static Designator *designator(TokenIterator *ts, Scope *scope) {
  Token *token;
  if ((token = ts_consume(ts, '[')) != NULL) {
    Number first = integer_constant_expression(ts, scope);
    Number last = first;
    if (ts_consume(ts, TK_ELIPSIS)) {
      // NonStandard/GNU: designated initializers for initializing a range of
      // elements
      last = integer_constant_expression(ts, scope);
    }
    Token *end = ts_expect(ts, ']');
    const Range *range = range_join(token->range, end->range);

    Designator *desig = NEW(Designator);
    desig->type = DESIG_INDEX;
    desig->index.first = first;
    desig->index.last = last;
    desig->range = range;
    return desig;
  }

  if ((token = ts_consume(ts, '.')) != NULL) {
    Token *ident = ts_expect(ts, TK_IDENT);
    Designator *desig = NEW(Designator);
    desig->type = DESIG_MEMBER;
    desig->member = ident->ident;
    desig->range = range_join(token->range, ident->range);
    return desig;
  }

  unreachable();
}

static DesignatorVector *designation(TokenIterator *ts, Scope *scope) {
  DesignatorVector *list = NEW_VECTOR(DesignatorVector);
  while (ts_peek(ts)->ty == '[' || ts_peek(ts)->ty == '.') {
    VEC_PUSH(list, designator(ts, scope));
  }
  ts_expect(ts, '=');
  return list;
}

static InitElemVector *initializer_list(TokenIterator *ts, Scope *scope) {
  InitElemVector *list = NEW_VECTOR(InitElemVector);
  while (ts_peek(ts)->ty != '}') {
    InitElem *elem = NEW(InitElem);
    Token *start = ts_peek(ts);
    if (ts_peek(ts)->ty == '[' || ts_peek(ts)->ty == '.') {
      elem->designation = designation(ts, scope);
    }
    elem->pinit = parse_initializer(ts, scope);
    elem->range = range_join(start->range, elem->pinit->range);
    VEC_PUSH(list, elem);
    if (ts_consume(ts, ',') == NULL) {
      break;
    }
  }
  return list;
}

static ParseInit *parse_initializer(TokenIterator *ts, Scope *scope) {
  Token *token;
  if ((token = ts_consume(ts, '{')) != NULL) {
    InitElemVector *list = initializer_list(ts, scope);
    Token *end = ts_expect(ts, '}');
    const Range *range = range_join(token->range, end->range);
    return new_parse_init_list(range, list);
  }

  Expr *expr = assignment_expression(ts, scope);
  return new_parse_init_expr(expr);
}

static const Member *consume_member_designator(Type *type, InitElem *elem) {
  assert(type->ty == TY_STRUCT || type->ty == TY_UNION);
  if (elem->designation == NULL) {
    return NULL;
  }

  Designator *desig = VEC_GET(elem->designation, 0);
  if (desig->type != DESIG_MEMBER) {
    assert(desig->type == DESIG_INDEX);
    range_error(desig->range,
                "array designator cannot initialize non-array type: %s",
                format_type(type, false));
  }

  const Member *member = lookup_struct_member(type, desig->member);
  if (member == NULL) {
    range_error(desig->range,
                "field designator '%s' does not refer to any field in type: %s",
                desig->member, format_type(type, false));
  }
  if (member->name != NULL) {
    VEC_REMOVE(elem->designation, 0);
    if (VEC_LEN(elem->designation) == 0) {
      elem->designation = NULL;
    }
  }
  return member;
}

static void assign_struct_initializer(Scope *scope, InitElemVector *list,
                                      bool is_root, Type *type,
                                      const Range *range, Initializer **init) {
  assert(type->ty == TY_STRUCT);
  StructBody *body = type->struct_body;

  if (*init == NULL) {
    *init = new_initializer(type);
  }

  bool is_first = true;

  int memidx = 0;
  while (VEC_LEN(list) > 0) {
    InitElem *elem = VEC_GET(list, 0);
    if (!is_first && !is_root && elem->designation != NULL) {
      break;
    }
    if (!is_root && is_first) {
      if (elem->pinit->expr != NULL &&
          is_same_type(elem->pinit->expr->val_type, type)) {
        (*init)->expr = elem->pinit->expr;
        (*init)->members = NULL;
        VEC_REMOVE(list, 0);
        return;
      }
    }
    is_first = false;

    const Member *member = consume_member_designator(type, elem);
    if (member == NULL) {
      if (memidx >= VEC_LEN(body->members)) {
        break;
      }
      member = VEC_GET(body->members, memidx);
      assert(member->index == memidx);
    }

    MemberInitializer *meminit = VEC_GET((*init)->members, member->index);
    memidx = member->index + 1;
    assign_initializer_list(scope, list, false, member->type, range,
                            &meminit->init);
  }
}

static void assign_union_initializer(Scope *scope, InitElemVector *list,
                                     bool is_root, Type *type,
                                     const Range *range, Initializer **init) {
  assert(type->ty == TY_UNION);
  StructBody *body = type->struct_body;

  if (*init == NULL) {
    *init = new_initializer(type);
  }

  bool is_first = true;

  int memidx = 0;
  while (VEC_LEN(list) > 0) {
    InitElem *elem = VEC_GET(list, 0);
    if (!is_first && !is_root && elem->designation != NULL) {
      break;
    }
    if (!is_root && is_first) {
      if (elem->pinit->expr != NULL &&
          is_same_type(elem->pinit->expr->val_type, type)) {
        (*init)->expr = elem->pinit->expr;
        (*init)->members = NULL;
        VEC_REMOVE(list, 0);
        return;
      }
    }
    is_first = false;

    const Member *member = consume_member_designator(type, elem);
    if (member == NULL) {
      if (memidx >= VEC_LEN(body->members)) {
        break;
      }
      member = VEC_GET(body->members, memidx);
    }

    MemberInitializer *meminit = VEC_GET((*init)->members, 0);
    memidx = member->index + 1;
    meminit->member = member;
    assign_initializer_list(scope, list, false, member->type, range,
                            &meminit->init);
    if (!is_root) {
      break;
    }
  }
}

static bool consume_array_designator(Type *type, InitElem *elem, int *first,
                                     int *last) {
  assert(type->ty == TY_ARRAY);

  if (elem->designation == NULL) {
    return false;
  }

  Designator *desig = VEC_REMOVE(elem->designation, 0);
  if (VEC_LEN(elem->designation) == 0) {
    elem->designation = NULL;
  }

  if (desig->type != DESIG_INDEX) {
    assert(desig->type == DESIG_MEMBER);
    range_error(
        desig->range,
        "field designator cannot initialize non-struct, non-union type: %s",
        format_type(type, false));
  }
  SET_NUMBER_VAL(*first, &desig->index.first);
  SET_NUMBER_VAL(*last, &desig->index.last);
  if (type->array.len >= 0 && *last >= type->array.len) {
    range_error(desig->range,
                "array designator index (%d) exceeds array bounds (%d)", *last,
                type->array.len);
  }
  return true;
}

static bool consume_str_for_array(ParseInit *pinit, Type *type,
                                  Initializer **init) {
  assert(is_array_type(type));
  if (pinit->expr == NULL || pinit->expr->ty != EX_STR) {
    return false;
  }

  Type *elem_type = type->array.elem;

  Expr *str = pinit->expr;
  if (elem_type->ty != TY_CHAR && elem_type->ty != TY_S_CHAR &&
      elem_type->ty != TY_U_CHAR) {
    range_error(str->range,
                "cannot initialize non-char array with string literal");
  }

  if (type->array.len < 0) {
    type->array.len = strlen(str->str->val) + 1;
    VEC_EXTEND((*init)->elements, type->array.len, NULL);
  }

  for (int i = 0; i < type->array.len; i++) {
    Initializer *eleminit = new_initializer(elem_type);
    eleminit->expr =
        new_expr_num(new_number(elem_type->ty, str->str->val[i]), str->range);
    VEC_SET((*init)->elements, i, eleminit);
    if (str->str->val[i] == '\0') {
      break;
    }
  }

  return true;
}

static void assign_array_initializer(Scope *scope, InitElemVector *list,
                                     bool is_root, Type *type,
                                     const Range *range, Initializer **init) {
  assert(type->ty == TY_ARRAY);

  if (*init == NULL) {
    *init = new_initializer(type);
  }

  bool is_first = true;

  int elemidx = 0;
  while (VEC_LEN(list) > 0) {
    InitElem *elem = VEC_GET(list, 0);
    if (!is_first && !is_root && elem->designation != NULL) {
      break;
    }
    if (!is_root && is_first) {
      if (consume_str_for_array(elem->pinit, type, init)) {
        VEC_REMOVE(list, 0);
        return;
      }
    }
    is_first = false;

    int first = elemidx, last = elemidx;
    if (!consume_array_designator(type, elem, &first, &last)) {
      if (type->array.len >= 0 && elemidx >= type->array.len) {
        break;
      }
    }

    if (type->array.len < 0) {
      VEC_EXTEND((*init)->elements, last + 1, NULL);
    }

    Initializer *eleminit = NULL;
    assign_initializer_list(scope, list, false, type->array.elem, range,
                            &eleminit);
    for (int i = first; i <= last; i++) {
      VEC_SET((*init)->elements, i, eleminit);
    }
    elemidx = last + 1;
  }

  if (type->array.len < 0) {
    type->array.len = VEC_LEN((*init)->elements);
  }
}

static void assign_initializer(Scope *scope, ParseInit *pinit, Type *type,
                               Initializer **init) {
  if (pinit->expr != NULL) {
    *init = new_initializer(type);

    if (is_array_type(type) && consume_str_for_array(pinit, type, init)) {
      return;
    }

    (*init)->expr = new_expr_cast(type, pinit->expr, pinit->range);
    return;
  }

  assert(pinit->list != NULL);
  assign_initializer_list(scope, pinit->list, true, type, pinit->range, init);
}

static void assign_initializer_list(Scope *scope, InitElemVector *list,
                                    bool is_root, Type *type,
                                    const Range *range, Initializer **init) {
  if (init == NULL) {
    *init = new_initializer(type);
  }

  if (!is_root && VEC_LEN(list) > 0) {
    InitElem *elem = VEC_GET(list, 0);
    if (elem->designation == NULL && elem->pinit->list != NULL) {
      VEC_REMOVE(list, 0);
      assign_initializer(scope, elem->pinit, type, init);
      return;
    }
  }

  switch (type->ty) {
  case TY_STRUCT:
    assign_struct_initializer(scope, list, is_root, type, range, init);
    return;
  case TY_UNION:
    assign_union_initializer(scope, list, is_root, type, range, init);
    return;
  case TY_ARRAY:
    assign_array_initializer(scope, list, is_root, type, range, init);
    return;
  default:
    break;
  }

  if (is_root) {
    if (VEC_LEN(list) == 0) {
      range_error(range, "scalar initializer cannot be empty: %s",
                  format_type(type, false));
    }
    if (VEC_LEN(list) > 1) {
      range_warn(range, "excess elements in scalar initializer: %s",
                 format_type(type, false));
    }
  } else {
    if (VEC_LEN(list) == 0) {
      return;
    }
  }
  InitElem *elem = VEC_REMOVE(list, 0);
  if (elem->designation != NULL) {
    Designator *desig = VEC_GET(elem->designation, 0);
    range_error(desig->range, "designator in initilizer for scalar type: %s",
                format_type(type, false));
  }
  assign_initializer(scope, elem->pinit, type, init);
}

static void initializer(TokenIterator *ts, Scope *scope, Type *type,
                        Initializer **init, const Range **range) {
  ParseInit *pinit = parse_initializer(ts, scope);
  if (range != NULL) {
    *range = pinit->range;
  }
  assign_initializer(scope, pinit, type, init);
}

static Stmt *statement(TokenIterator *ts, Scope *scope) {
  Token *start = ts_peek(ts);
  switch (start->ty) {
  case TK_IF: {
    ts_succ(ts);
    ts_expect(ts, '(');
    Expr *cond = expression(ts, scope);
    ts_expect(ts, ')');
    Stmt *then_stmt = statement(ts, scope);
    Stmt *else_stmt = new_stmt_null(then_stmt->range);
    const Range *range;
    if (ts_consume(ts, TK_ELSE)) {
      else_stmt = statement(ts, scope);
      range = range_join(start->range, else_stmt->range);
    } else {
      range = range_join(start->range, then_stmt->range);
    }
    return new_stmt_if(cond, then_stmt, else_stmt, range);
  }
  case TK_SWITCH: {
    ts_succ(ts);
    ts_expect(ts, '(');
    Expr *cond = expression(ts, scope);
    ts_expect(ts, ')');
    Stmt *stmt = new_stmt_switch(cond, NULL, start->range);
    VEC_PUSH(scope->func_ctxt->switches, stmt);
    stmt->body = statement(ts, scope);
    stmt->range = range_join(stmt->range, stmt->body->range);
    VEC_POP(scope->func_ctxt->switches);
    return stmt;
  }
  case TK_CASE: {
    ts_succ(ts);
    Number num = integer_constant_expression(ts, scope);
    ts_expect(ts, ':');
    Stmt *body = statement(ts, scope);
    Stmt *stmt =
        new_stmt_case(num, body, range_join(start->range, body->range));
    if (VEC_LEN(scope->func_ctxt->switches) <= 0) {
      range_error(stmt->range, "switch文中でない箇所にcase文があります");
    }
    Stmt *switch_stmt = VEC_LAST(scope->func_ctxt->switches);
    VEC_PUSH(switch_stmt->cases, stmt);
    return stmt;
  }
  case TK_DEFAULT: {
    ts_succ(ts);
    ts_expect(ts, ':');
    Stmt *body = statement(ts, scope);
    Stmt *stmt = new_stmt_default(body, range_join(start->range, body->range));
    if (VEC_LEN(scope->func_ctxt->switches) <= 0) {
      range_error(stmt->range, "switch文中でない箇所にcase文があります");
    }
    Stmt *switch_expr = VEC_LAST(scope->func_ctxt->switches);
    switch_expr->default_case = stmt;
    return stmt;
  }
  case TK_WHILE: {
    ts_succ(ts);
    ts_expect(ts, '(');
    Expr *cond = expression(ts, scope);
    ts_expect(ts, ')');
    Stmt *body = statement(ts, scope);
    return new_stmt_while(cond, body, range_join(start->range, body->range));
  }
  case TK_DO: {
    ts_succ(ts);
    Stmt *body = statement(ts, scope);
    ts_expect(ts, TK_WHILE);
    ts_expect(ts, '(');
    Expr *cond = expression(ts, scope);
    ts_expect(ts, ')');
    Token *end = ts_expect(ts, ';');
    return new_stmt_do_while(cond, body, range_join(start->range, end->range));
  }
  case TK_FOR: {
    Scope *inner = new_inner_scope(scope);
    ts_succ(ts);
    Stmt *init = NULL;
    Expr *cond = NULL;
    Expr *inc = NULL;
    ts_expect(ts, '(');
    if (!ts_consume(ts, ';')) {
      if (token_is_declaration_specifiers(inner, ts_peek(ts))) {
        StackVarDeclVector *svar_decl = NULL;
        const Range *svar_range = NULL;
        VarDefVector *def_list = declaration(ts, inner);
        for (int i = 0; i < VEC_LEN(def_list); i++) {
          VarDef *def = VEC_GET(def_list, i);
          switch (def->type) {
          case DEF_FUNC:
            range_error(def->name->range, "関数内で関数は定義できません");
            break;
          case DEF_GLOBAL_VAR:
            range_error(def->name->range, "ローカル変数ではありません");
          case DEF_STACK_VAR: {
            if (svar_decl == NULL) {
              svar_decl = NEW_VECTOR(StackVarDeclVector);
              svar_range = def->stack_var->range;
            } else {
              svar_range = range_join(svar_range, def->stack_var->range);
            }
            StackVarDecl *decl = NEW(StackVarDecl);
            decl->stack_var = def->stack_var;
            decl->init = def->init;
            VEC_PUSH(svar_decl, decl);
            break;
          }
          }
        }
        init = new_stmt_decl(svar_decl, svar_range);
      } else {
        Expr *expr = expression(ts, inner);
        init = new_stmt_expr(expr, expr->range);
        ts_expect(ts, ';');
      }
    }
    if (!ts_consume(ts, ';')) {
      cond = expression(ts, inner);
      ts_expect(ts, ';');
    }
    if (!ts_consume(ts, ')')) {
      inc = expression(ts, inner);
      ts_expect(ts, ')');
    }
    Stmt *body = statement(ts, inner);
    return new_stmt_for(init, cond, inc, body,
                        range_join(start->range, body->range));
  }
  case TK_GOTO: {
    ts_succ(ts);
    const char *c_label = ts_expect(ts, TK_IDENT)->ident;
    Token *end = ts_expect(ts, ';');
    return new_stmt_goto(c_label, range_join(start->range, end->range));
  }
  case TK_BREAK: {
    ts_succ(ts);
    Token *end = ts_expect(ts, ';');
    return new_stmt_break(range_join(start->range, end->range));
  }
  case TK_CONTINUE: {
    ts_succ(ts);
    Token *end = ts_expect(ts, ';');
    return new_stmt_continue(range_join(start->range, end->range));
  }
  case TK_RETURN: {
    ts_succ(ts);
    Expr *expr = NULL;
    if (ts_peek(ts)->ty != ';') {
      expr = expression(ts, scope);
    }
    Token *end = ts_expect(ts, ';');
    return new_stmt_return(scope->func_ctxt->type->func.ret, expr,
                           range_join(start->range, end->range));
  }
  case '{': {
    Scope *inner = new_inner_scope(scope);
    return compound_statement(ts, inner);
  }
  case ';': {
    Token *end = ts_pop(ts);
    return new_stmt_null(end->range);
  }
  case TK_IDENT: {
    if (ts_peek_ahead(ts, 1)->ty == ':') {
      Token *ident = ts_pop(ts);
      ts_expect(ts, ':');
      Stmt *body = statement(ts, scope);

      const char *c_label = ident->ident;
      const char *asm_label = make_label(c_label);
      register_label(scope->func_ctxt, c_label, asm_label);
      Stmt *stmt = new_stmt_label(c_label, asm_label, body,
                                  range_join(start->range, body->range));
      return stmt;
    }
  }
  // fall through
  default: {
    Expr *expr = expression(ts, scope);
    Token *end = ts_expect(ts, ';');
    return new_stmt_expr(expr, range_join(expr->range, end->range));
  }
  }
}

static Stmt *compound_statement(TokenIterator *ts, Scope *scope) {
  Token *start = ts_expect(ts, '{');

  const Range *range = start->range;
  StmtVector *stmts = NEW_VECTOR(StmtVector);
  while (!ts_consume(ts, '}')) {
    Token *token = ts_peek(ts);
    if ((token->ty != TK_IDENT || ts_peek_ahead(ts, 1)->ty != ':') &&
        (token_is_declaration_specifiers(scope, token))) {

      StackVarDeclVector *svar_decl = NULL;
      const Range *svar_range = NULL;
      VarDefVector *def_list = declaration(ts, scope);
      for (int i = 0; i < VEC_LEN(def_list); i++) {
        VarDef *def = VEC_GET(def_list, i);
        switch (def->type) {
        case DEF_FUNC:
          range_error(def->name->range, "関数内で関数は定義できません");
        case DEF_GLOBAL_VAR:
          def->global_var->init = def->init;
          VEC_PUSH(scope->global_ctxt->gvar_list, def->global_var);
          break;
        case DEF_STACK_VAR: {
          if (svar_decl == NULL) {
            svar_decl = NEW_VECTOR(StackVarDeclVector);
            svar_range = def->stack_var->range;
          } else {
            svar_range = range_join(svar_range, def->stack_var->range);
          }
          StackVarDecl *decl = NEW(StackVarDecl);
          decl->stack_var = def->stack_var;
          decl->init = def->init;
          VEC_PUSH(svar_decl, decl);
          break;
        }
        }
      }

      if (svar_decl != NULL) {
        Stmt *s = new_stmt_decl(svar_decl, svar_range);
        VEC_PUSH(stmts, s);
      }
      continue;
    }

    Stmt *s = statement(ts, scope);
    range = range_join(range, s->range);
    VEC_PUSH(stmts, s);
  }
  return new_stmt_compound(stmts, range);
}

static Function *function_definition(TokenIterator *ts, Scope *global_scope,
                                     Type *type, const char *name,
                                     StorageClassSpecifier scs,
                                     FunctionSpecifier fs, const Range *start) {
  FuncCtxt *fcx = new_func_ctxt(name, type);
  Scope *scope = new_func_scope(global_scope, fcx);
  if (type->func.params != NULL) {
    for (int i = 0; i < VEC_LEN(type->func.params); i++) {
      Param *param = VEC_GET(type->func.params, i);
      param->stack_var =
          register_stack_var(scope, param->name, param->type, param->range);
    }
  }

  Stmt *body = compound_statement(ts, scope);

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

static TranslationUnit *translation_unit(const Reader *reader,
                                         TokenIterator *ts) {
  GlobalCtxt *gcx = new_global_ctxt();
  Scope *scope = new_global_scope(gcx, reader);

  Map *gvar_map = new_map();

  while (ts_peek(ts)->ty != TK_EOF) {
    VarDefVector *def_list = declaration(ts, scope);
    for (int i = 0; i < VEC_LEN(def_list); i++) {
      VarDef *def = VEC_GET(def_list, i);
      switch (def->type) {
      case DEF_FUNC:
        VEC_PUSH(gcx->func_list, def->func);
        break;
      case DEF_GLOBAL_VAR: {
        GlobalVar *prev_def = map_get(gvar_map, def->name->ident);
        if (prev_def == NULL) {
          def->global_var->init = def->init;
          VEC_PUSH(gcx->gvar_list, def->global_var);
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
