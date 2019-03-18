#include "gifcc.h"

const TypeQualifier EMPTY_TYPE_QUALIFIER = {};
const TypeQualifier CONST_TYPE_QUALIFIER = {.is_const = true};

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

Type *new_type_ptr(Type *base_type, TypeQualifier tq) {
  Type *ptrtype = new_type(TY_PTR, tq);
  ptrtype->ptrof = base_type;
  return ptrtype;
}

Type *new_type_array(Type *base_type, Number len, TypeQualifier tq) {
  int l;
  SET_NUMBER_VAL(l, &len);
  Type *ptrtype = new_type(TY_ARRAY, tq);
  ptrtype->ty = TY_ARRAY;
  ptrtype->ptrof = base_type;
  ptrtype->array_len = l;
  return ptrtype;
}

Type *new_type_unsized_array(Type *base_type, TypeQualifier tq) {
  Type *ptrtype = new_type(TY_ARRAY, tq);
  ptrtype->ptrof = base_type;
  ptrtype->array_len = -1;
  return ptrtype;
}

Type *new_type_func(Type *ret_type, Vector *func_param, bool has_varargs,
                    TypeQualifier tq) {
  Type *funtype = new_type(TY_FUNC, tq);
  funtype->func_ret = ret_type;
  funtype->func_param = func_param;
  funtype->func_has_varargs = has_varargs;
  return funtype;
}

Type *new_type_struct(type_t ty, const char *tag, TypeQualifier tq) {
  assert(ty == TY_STRUCT || ty == TY_UNION);

  Type *type = new_type(ty, tq);
  type->tag = tag;
  type->struct_body = NEW(StructBody);
  init_struct_body(type->struct_body);

  return type;
}

Type *new_type_opaque_struct(type_t ty, const char *tag, TypeQualifier tq) {
  assert(ty == TY_STRUCT || ty == TY_UNION);
  Type *type = new_type(ty, tq);
  type->tag = tag;
  type->struct_body = NEW(StructBody);
  return type;
}

Type *new_type_enum(const char *tag, TypeQualifier tq) {
  Type *type = new_type(TY_ENUM, tq);
  type->tag = tag;
  return type;
}

Type *new_type_builtin_va_list(const Range *range) {
  Type *elem = new_type(TY_STRUCT, EMPTY_TYPE_QUALIFIER);
  elem->tag = "__builtin_va_elem";
  elem->struct_body = NEW(StructBody);
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

  int offset;
  if (type->ty == TY_STRUCT) {
    body->member_size =
        align(body->member_size, get_val_align(member_type, range));
    offset = body->member_size;
    body->member_size += get_val_size(member_type, range);
  } else {
    if (get_val_size(member_type, range) > body->member_size) {
      body->member_size = get_val_size(member_type, range);
    }
    offset = 0;
  }

  if (body->member_align < get_val_align(member_type, range)) {
    body->member_align = get_val_align(member_type, range);
  }

  int index = vec_len(body->member_list);
  Member *member = new_member(member_name, member_type, offset, index, range);
  vec_push(body->member_list, member);
  if (member_name == NULL) {
    assert(member_type->ty == TY_STRUCT || member_type->ty == TY_UNION);
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

bool is_sametype(Type *ty1, Type *ty2) {
  if (ty1->ty != ty2->ty) {
    return false;
  }
  if (ty1->ty == TY_PTR) {
    return is_sametype(ty1->ptrof, ty2->ptrof);
  }
  return true;
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
    type_str = format("PTR(%s)", format_type(type->ptrof, false));
    break;
  case TY_ARRAY:
    type_str = format("ARRAY[%d](%s)", type->array_len,
                      format_type(type->ptrof, false));
    break;
  case TY_FUNC:
    type_str = format("FUNC(");
    if (type->func_param != NULL) {
      for (int i = 0; i < vec_len(type->func_param); i++) {
        if (i > 0) {
          type_str = format("%s, ", type_str);
        }
        Param *param = vec_get(type->func_param, i);
        type_str = format("%s%s", type_str, format_type(param->type, false));
        if (param->name != NULL) {
          type_str = format("%s %s", type_str, param->name->ident);
        }
      }
      if (type->func_has_varargs) {
        if (vec_len(type->func_param) > 0) {
          type_str = format("%s, ", type_str);
        }
        type_str = format("%s...", type_str);
      } else {
        if (vec_len(type->func_param) == 0) {
          type_str = format("%svoid", type_str);
        }
      }
    }
    type_str =
        format("%s) -> %s", type_str, format_type(type->func_ret, false));
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
