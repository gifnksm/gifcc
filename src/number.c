#include "gifcc.h"
#include <inttypes.h>

Number new_number(type_t ty, unsigned long long val) {
  Number num = {.type = ty};
  switch (ty) {
  case TY_BOOL:
    num.bool_val = val;
    return num;
  case TY_CHAR:
    num.char_val = val;
    return num;
  case TY_S_CHAR:
    num.s_char_val = val;
    return num;
  case TY_S_INT:
    num.s_int_val = val;
    return num;
  case TY_S_SHORT:
    num.s_short_val = val;
    return num;
  case TY_S_LONG:
    num.s_long_val = val;
    return num;
  case TY_S_LLONG:
    num.s_llong_val = val;
    return num;
  case TY_U_CHAR:
    num.s_char_val = val;
    return num;
  case TY_U_INT:
    num.s_int_val = val;
    return num;
  case TY_U_SHORT:
    num.s_short_val = val;
    return num;
  case TY_U_LONG:
    num.s_long_val = val;
    return num;
  case TY_U_LLONG:
    num.s_llong_val = val;
    return num;
  case TY_PTR:
    num.ptr_val = val;
    return num;
  case TY_ENUM:
    num.enum_val = val;
    return num;
  case TY_FLOAT:
    num.float_val = val;
    return num;
  case TY_DOUBLE:
    num.double_val = val;
    return num;
  case TY_LDOUBLE:
    num.ldouble_val = val;
    return num;
  case TY_STRUCT:
  case TY_UNION:
  case TY_ARRAY:
  case TY_VOID:
  case TY_FUNC:
  case TY_BUILTIN:
    break;
  }
  assert(false);
}

Number new_number_float(type_t ty, long double val) {
  Number num = {.type = ty};
  switch (ty) {
  case TY_BOOL:
    num.bool_val = val;
    return num;
  case TY_CHAR:
    num.char_val = val;
    return num;
  case TY_S_CHAR:
    num.s_char_val = val;
    return num;
  case TY_S_INT:
    num.s_int_val = val;
    return num;
  case TY_S_SHORT:
    num.s_short_val = val;
    return num;
  case TY_S_LONG:
    num.s_long_val = val;
    return num;
  case TY_S_LLONG:
    num.s_llong_val = val;
    return num;
  case TY_U_CHAR:
    num.s_char_val = val;
    return num;
  case TY_U_INT:
    num.s_int_val = val;
    return num;
  case TY_U_SHORT:
    num.s_short_val = val;
    return num;
  case TY_U_LONG:
    num.s_long_val = val;
    return num;
  case TY_U_LLONG:
    num.s_llong_val = val;
    return num;
  case TY_PTR:
    num.ptr_val = val;
    return num;
  case TY_ENUM:
    num.enum_val = val;
    return num;
  case TY_FLOAT:
    num.float_val = val;
    return num;
  case TY_DOUBLE:
    num.double_val = val;
    return num;
  case TY_LDOUBLE:
    num.ldouble_val = val;
    return num;
  case TY_STRUCT:
  case TY_UNION:
  case TY_ARRAY:
  case TY_VOID:
  case TY_FUNC:
  case TY_BUILTIN:
    break;
  }
  assert(false);
}

Number new_number_int(int val) { return new_number(TY_S_INT, val); }
Number new_number_size_t(size_t val) { return new_number(TY_SIZE_T, val); }
Number new_number_ptrdiff_t(ptrdiff_t val) {
  return new_number(TY_PTRDIFF_T, val);
}
Number new_number_wchar_t(wchar_t val) { return new_number(TY_WCHAR_T, val); }

char *format_number(Number num) {
  switch (num.type) {
  case TY_BOOL:
    return format("%d", num.bool_val);
  case TY_CHAR:
    return format("%hhd", num.char_val);
  case TY_S_CHAR:
    return format("%hhd", num.s_char_val);
  case TY_S_SHORT:
    return format("%hd", num.s_short_val);
  case TY_S_INT:
    return format("%d", num.s_int_val);
  case TY_S_LONG:
    return format("%ld", num.s_long_val);
  case TY_S_LLONG:
    return format("%lld", num.s_llong_val);
  case TY_U_CHAR:
    return format("%hhu", num.u_char_val);
  case TY_U_SHORT:
    return format("%hu", num.u_short_val);
  case TY_U_INT:
    return format("%u", num.u_int_val);
  case TY_U_LONG:
    return format("%lu", num.u_long_val);
  case TY_U_LLONG:
    return format("%llu", num.u_llong_val);
  case TY_PTR:
    return format("%" PRIdPTR, num.ptr_val);
  case TY_ENUM:
    return format("%d", num.enum_val);
  case TY_FLOAT:
    return format("%f", num.float_val);
  case TY_DOUBLE:
    return format("%f", num.double_val);
  case TY_LDOUBLE:
    return format("%Lf", num.ldouble_val);
  case TY_VOID:
    return format("void");
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
  case TY_BUILTIN:
    break;
  }
  assert(false);
}
