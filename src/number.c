#include "gifcc.h"

Number new_number(type_t ty, unsigned long long val) {
  Number num = {.type = ty};
  switch (ty) {
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
  case TY_STRUCT:
  case TY_UNION:
  case TY_ARRAY:
  case TY_VOID:
  case TY_FUNC:
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
