#include "gifcc.h"

const TypeQualifier EMPTY_TYPE_QUALIFIER = {};
const TypeQualifier CONST_TYPE_QUALIFIER = {.is_const = true};

static StructBody *new_struct_body(void);

Type *new_type(int ty, TypeQualifier tq) {
  Type *type = NEW(Type);
  type->ty = ty;
  type->qualifier = tq;
  return type;
}

Type *clone_type(Type *type) {
  Type *cloned = NEW(Type);
  *cloned = *type;
  return cloned;
}

Type *to_unqualified(Type *type) {
  if (is_unqualified_type(type)) {
    return type;
  }
  Type *cloned = NEW(Type);
  *cloned = *type;
  cloned->qualifier = EMPTY_TYPE_QUALIFIER;
  return cloned;
}

Type *new_type_ptr(Type *base_type, TypeQualifier tq) {
  Type *ptrtype = new_type(TY_PTR, tq);
  ptrtype->ptr = base_type;
  return ptrtype;
}

Type *new_type_array(Type *base_type, Number len, TypeQualifier tq) {
  int l;
  SET_NUMBER_VAL(l, &len);
  Type *ptrtype = new_type(TY_ARRAY, tq);
  ptrtype->ty = TY_ARRAY;
  ptrtype->array.len = l;
  ptrtype->array.elem = base_type;
  return ptrtype;
}

Type *new_type_unsized_array(Type *base_type, TypeQualifier tq) {
  Type *ptrtype = new_type(TY_ARRAY, tq);
  ptrtype->array.len = -1;
  ptrtype->array.elem = base_type;
  return ptrtype;
}

Type *new_type_func(Type *ret_type, Vector *func_param, bool has_varargs,
                    TypeQualifier tq) {
  Type *funtype = new_type(TY_FUNC, tq);
  funtype->func.ret = ret_type;
  funtype->func.param = func_param;
  funtype->func.has_varargs = has_varargs;
  return funtype;
}

Type *new_type_struct(type_t ty, const char *tag, TypeQualifier tq) {
  assert(ty == TY_STRUCT || ty == TY_UNION);

  Type *type = new_type(ty, tq);
  type->tag = tag;
  type->struct_body = new_struct_body();
  init_struct_body(type->struct_body);

  return type;
}

Type *new_type_opaque_struct(type_t ty, const char *tag, TypeQualifier tq) {
  assert(ty == TY_STRUCT || ty == TY_UNION);
  Type *type = new_type(ty, tq);
  type->tag = tag;
  type->struct_body = new_struct_body();
  return type;
}

static StructBody *new_struct_body(void) {
  static int struct_id = 0;
  StructBody *body = NEW(StructBody);
  body->struct_id = struct_id++;
  return body;
}

Type *new_type_enum(const char *tag, TypeQualifier tq) {
  static int enum_id = 0;
  Type *type = new_type(TY_ENUM, tq);
  type->tag = tag;
  type->enum_body.enum_id = enum_id++;
  return type;
}

Type *new_type_builtin_va_list(const Range *range) {
  Type *elem = new_type(TY_STRUCT, EMPTY_TYPE_QUALIFIER);
  elem->tag = "__builtin_va_elem";
  elem->struct_body = new_struct_body();
  init_struct_body(elem->struct_body);

  Type *ty_offset = new_type(TY_U_INT, EMPTY_TYPE_QUALIFIER);
  Type *ty_area = new_type_ptr(new_type(TY_VOID, EMPTY_TYPE_QUALIFIER),
                               EMPTY_TYPE_QUALIFIER);

  register_struct_member(elem, "gp_offset", ty_offset, range);
  register_struct_member(elem, "fp_offset", ty_offset, range);
  register_struct_member(elem, "overflow_arg_area", ty_area, range);
  register_struct_member(elem, "reg_save_area", ty_area, range);

  return new_type_array(elem, new_number_int(1), EMPTY_TYPE_QUALIFIER);
}

void init_struct_body(StructBody *body) {
  body->member_name_map = new_map();
  body->member_list = new_vector();
  body->member_size = 0;
  body->member_align = 0;
}

static Member *new_member(char *name, Type *type, int offset, int index,
                          const Range *range) {
  if (type->ty == TY_VOID) {
    range_error(range, "void型のメンバーです: %s", name);
  }
  Member *member = NEW(Member);
  member->name = name;
  member->type = type;
  member->offset = offset;
  member->index = index;
  return member;
}

void register_struct_member(Type *type, char *member_name, Type *member_type,
                            const Range *range) {
  assert(type->ty == TY_STRUCT || type->ty == TY_UNION);

  StructBody *body = type->struct_body;

  if (body->has_flex_array) {
    range_error(range, "flexible array member is not at the end of struct");
  }

  int mem_size, mem_align;
  if (member_type->ty == TY_ARRAY && member_type->array.len < 0) {
    body->has_flex_array = true;
    mem_size = 0;
    mem_align = get_val_align(member_type->array.elem, range);
  } else if ((member_type->ty == TY_STRUCT || member_type->ty == TY_UNION) &&
             member_type->struct_body->has_flex_array) {
    StructBody *mem_body = member_type->struct_body;
    body->has_flex_array = true;
    mem_size = mem_body->member_size;
    mem_align = mem_body->member_align;
  } else {
    mem_size = get_val_size(member_type, range);
    mem_align = get_val_align(member_type, range);
  }

  int offset;
  if (type->ty == TY_STRUCT) {
    body->member_size = align(body->member_size, mem_align);
    offset = body->member_size;
    body->member_size += mem_size;
  } else {
    if (mem_size > body->member_size) {
      body->member_size = mem_size;
    }
    offset = 0;
  }

  if (body->member_align < mem_align) {
    body->member_align = mem_align;
  }

  int index = vec_len(body->member_list);
  Member *member = new_member(member_name, member_type, offset, index, range);
  vec_push(body->member_list, member);

  if (member_name == NULL) {
    if (member_type->ty == TY_STRUCT || member_type->ty == TY_UNION) {
      StructBody *member_body = member_type->struct_body;
      Map *inner_members = member_body->member_name_map;
      for (int i = 0; i < map_size(inner_members); i++) {
        Member *inner = map_get_by_index(inner_members, i, NULL);
        Member *inner_member = new_member(inner->name, inner->type,
                                          offset + inner->offset, index, range);
        if (map_get(body->member_name_map, inner_member->name)) {
          range_error(range, "同じ名前のメンバ変数が複数あります: %s",
                      inner_member->name);
        }
        map_put(body->member_name_map, inner_member->name, inner_member);
      }
    }
  } else {
    if (map_get(body->member_name_map, member->name)) {
      range_error(range, "同じ名前のメンバ変数が複数あります: %s",
                  member->name);
    }
    map_put(body->member_name_map, member->name, member);
  }
}

const Member *lookup_struct_member(Type *type, const char *name) {
  assert(type->ty == TY_STRUCT || type->ty == TY_UNION);
  StructBody *body = type->struct_body;
  Member *member = map_get(body->member_name_map, name);
  if (member == NULL) {
    return NULL;
  }
  return vec_get(body->member_list, member->index);
}

bool is_unqualified_type(const Type *type) {
  return is_same_type_qualifier(&type->qualifier, &EMPTY_TYPE_QUALIFIER);
}

bool is_same_type_qualifier(const TypeQualifier *tq1,
                            const TypeQualifier *tq2) {
  if (tq1->is_const != tq2->is_const) {
    return false;
  }
  if (tq1->is_restrict != tq2->is_restrict) {
    return false;
  }
  if (tq1->is_volatile != tq2->is_volatile) {
    return false;
  }
  return true;
}

bool is_sametype(Type *ty1, Type *ty2) {
  if (ty1->ty != ty2->ty) {
    return false;
  }

  if (!is_same_type_qualifier(&ty1->qualifier, &ty2->qualifier)) {
    return false;
  }

  switch (ty1->ty) {
  case TY_VOID:
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
    return true;
  case TY_PTR:
    return is_sametype(ty1->ptr, ty2->ptr);
  case TY_ARRAY:
    return (ty1->array.len == ty2->array.len) &&
           is_sametype(ty1->array.elem, ty2->array.elem);
  case TY_FUNC: {
    if (!is_sametype(ty1->func.ret, ty2->func.ret)) {
      return false;
    }
    if ((ty1->func.has_varargs ^ ty2->func.has_varargs) != 0) {
      return false;
    }
    if (((ty1->func.param == NULL) ^ (ty2->func.param == NULL)) != 0) {
      return false;
    }
    if (ty1->func.param != NULL) {
      if (vec_len(ty1->func.param) != vec_len(ty2->func.param)) {
        return false;
      }
      for (int i = 0; i < vec_len(ty1->func.param); i++) {
        Param *p1 = vec_get(ty1->func.param, i);
        Param *p2 = vec_get(ty2->func.param, i);
        if (!is_sametype(p1->type, p2->type)) {
          return false;
        }
      }
    }
    return true;
  }
  case TY_STRUCT:
  case TY_UNION:
    return ty1->struct_body->struct_id == ty2->struct_body->struct_id;
  case TY_ENUM:
    return ty1->enum_body.enum_id == ty2->enum_body.enum_id;
  case TY_BUILTIN:
    return true;
  }
  assert(false);
}

bool is_integer_type(Type *ty) {
  switch (ty->ty) {
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
  case TY_ENUM:
    return true;
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
    return false;
  }
  assert(false);
  return false;
}
bool is_float_type(Type *ty) {
  switch (ty->ty) {
  case TY_FLOAT:
  case TY_DOUBLE:
  case TY_LDOUBLE:
    return true;
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
  case TY_ENUM:
  case TY_VOID:
  case TY_PTR:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
  case TY_BUILTIN:
    return false;
  }
  assert(false);
  return false;
}
int get_int_type_rank(Type *ty, const Range *range) {
  switch (ty->ty) {
  case TY_BOOL:
    return 1;
  case TY_CHAR:
  case TY_S_CHAR:
  case TY_U_CHAR:
    return 2;
  case TY_S_SHORT:
  case TY_U_SHORT:
    return 3;
  case TY_S_INT:
  case TY_U_INT:
  case TY_ENUM:
    return 4;
  case TY_S_LONG:
  case TY_U_LONG:
    return 5;
  case TY_S_LLONG:
  case TY_U_LLONG:
    return 6;
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
  range_error(range, "整数型ではない型です: %s", format_type(ty, false));
}
bool is_signed_int_type(Type *ty, const Range *range) {
  switch (ty->ty) {
  case TY_BOOL:
    return false;
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
  case TY_FLOAT:
  case TY_DOUBLE:
  case TY_LDOUBLE:
  case TY_PTR:
  case TY_VOID:
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
  case TY_BUILTIN:
    break;
  }
  range_error(range, "整数型ではない型です: %s", format_type(ty, false));
}
bool is_arith_type(Type *ty) {
  return is_integer_type(ty) || is_float_type(ty);
}
bool is_ptr_type(Type *ty) { return ty->ty == TY_PTR; }
bool is_array_type(Type *ty) { return ty->ty == TY_ARRAY; }
bool is_func_type(Type *ty) { return ty->ty == TY_FUNC; }

char *format_type(const Type *type, bool detail) {
  const char *const_qual = "";
  const char *restrict_qual = "";
  if (type->qualifier.is_const) {
    const_qual = "const ";
  }
  if (type->qualifier.is_restrict) {
    restrict_qual = "restrict ";
  }

  const char *type_str = NULL;
  switch (type->ty) {
  case TY_VOID:
    type_str = "void";
    break;
  case TY_BOOL:
    type_str = "_Bool";
    break;
  case TY_CHAR:
    type_str = "char";
    break;
  case TY_S_CHAR:
    type_str = "signed char";
    break;
  case TY_S_SHORT:
    type_str = "signed short";
    break;
  case TY_S_INT:
    type_str = "signed int";
    break;
  case TY_S_LONG:
    type_str = "signed long";
    break;
  case TY_S_LLONG:
    type_str = "signed long long";
    break;
  case TY_U_CHAR:
    type_str = "unsigned char";
    break;
  case TY_U_SHORT:
    type_str = "unsigned short";
    break;
  case TY_U_INT:
    type_str = "unsigned int";
    break;
  case TY_U_LONG:
    type_str = "unsigned long";
    break;
  case TY_U_LLONG:
    type_str = "unsigned long long";
    break;
  case TY_FLOAT:
    type_str = "float";
    break;
  case TY_DOUBLE:
    type_str = "double";
    break;
  case TY_LDOUBLE:
    type_str = "long double";
    break;
  case TY_PTR:
    type_str = format("PTR(%s)", format_type(type->ptr, false));
    break;
  case TY_ARRAY:
    type_str = format("ARRAY[%d](%s)", type->array.len,
                      format_type(type->array.elem, false));
    break;
  case TY_FUNC:
    type_str = format("FUNC(");
    if (type->func.param != NULL) {
      for (int i = 0; i < vec_len(type->func.param); i++) {
        if (i > 0) {
          type_str = format("%s, ", type_str);
        }
        Param *param = vec_get(type->func.param, i);
        type_str = format("%s%s", type_str, format_type(param->type, false));
        if (param->name != NULL) {
          type_str = format("%s %s", type_str, param->name->ident);
        }
      }
      if (type->func.has_varargs) {
        if (vec_len(type->func.param) > 0) {
          type_str = format("%s, ", type_str);
        }
        type_str = format("%s...", type_str);
      } else {
        if (vec_len(type->func.param) == 0) {
          type_str = format("%svoid", type_str);
        }
      }
    }
    type_str =
        format("%s) -> %s", type_str, format_type(type->func.ret, false));
    break;
  case TY_STRUCT:
  case TY_UNION: {
    const char *kind = type->ty == TY_STRUCT ? "struct" : "union";
    const StructBody *body = type->struct_body;
    if (type->tag == NULL) {
      type_str = format("%s <anonymous>", kind);
    } else {
      type_str = format("%s %s", kind, type->tag);
    }
    if (detail) {
      if (body->member_list != NULL) {
        type_str = format("%s {", type_str);
        for (int i = 0; i < vec_len(body->member_list); i++) {
          Member *member = vec_get(body->member_list, i);
          type_str = format("%s %s %s;", type_str,
                            format_type(member->type, true), member->name);
        }
        type_str = format("%s }", type_str);
      } else {
        type_str = format("%s {<opaque>}", type_str);
      }
    }
    break;
  }
  case TY_ENUM:
    if (type->tag == NULL) {
      type_str = "enum <anonymous>";
    } else {
      type_str = format("enum %s", type->tag);
    }
    break;
  case TY_BUILTIN:
    type_str = "<builitin>";
    break;
  }

  if (type_str == NULL) {
    error("未知の型です: %d\n", type->ty);
  }

  return format("%s%s%s", const_qual, restrict_qual, type_str);
}
