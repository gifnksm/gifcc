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
    num.u_char_val = val;
    return num;
  case TY_U_INT:
    num.u_int_val = val;
    return num;
  case TY_U_SHORT:
    num.u_short_val = val;
    return num;
  case TY_U_LONG:
    num.u_long_val = val;
    return num;
  case TY_U_LLONG:
    num.u_llong_val = val;
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
Number new_number_uint64(uint64_t val) { return new_number(TY_U_LLONG, val); }
Number new_number_size_t(size_t val) { return new_number(TY_SIZE_T, val); }
Number new_number_ptrdiff_t(ptrdiff_t val) {
  return new_number(TY_PTRDIFF_T, val);
}
Number new_number_wchar_t(wchar_t val) { return new_number(TY_WCHAR_T, val); }

static bool is_number_has_int_value(Number num, unsigned long long expected) {
  switch (num.type) {
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
  case TY_PTR:
  case TY_ENUM: {
    unsigned long long val;
    SET_NUMBER_VAL(val, &num);
    return val == expected;
  }

  case TY_FLOAT:
  case TY_DOUBLE:
  case TY_LDOUBLE:
  case TY_STRUCT:
  case TY_UNION:
  case TY_ARRAY:
  case TY_VOID:
  case TY_FUNC:
  case TY_BUILTIN:
    break;
  }
  return false;
}

bool is_number_zero(Number num) { return is_number_has_int_value(num, 0); }

const char *format_number(Number num) {
  switch (num.type) {
  case TY_BOOL:
    return format("%d {0x%016lx, 0x%016lx}", num.bool_val, num.bytes[0],
                  num.bytes[1]);
  case TY_CHAR:
    return format("%hhd {0x%016lx, 0x%016lx}", num.char_val, num.bytes[0],
                  num.bytes[1]);
  case TY_S_CHAR:
    return format("%hhd {0x%016lx, 0x%016lx}", num.s_char_val, num.bytes[0],
                  num.bytes[1]);
  case TY_S_SHORT:
    return format("%hd {0x%016lx, 0x%016lx}", num.s_short_val, num.bytes[0],
                  num.bytes[1]);
  case TY_S_INT:
    return format("%d {0x%016lx, 0x%016lx}", num.s_int_val, num.bytes[0],
                  num.bytes[1]);
  case TY_S_LONG:
    return format("%ld {0x%016lx, 0x%016lx}", num.s_long_val, num.bytes[0],
                  num.bytes[1]);
  case TY_S_LLONG:
    return format("%lld {0x%016lx, 0x%016lx}", num.s_llong_val, num.bytes[0],
                  num.bytes[1]);
  case TY_U_CHAR:
    return format("%hhu {0x%016lx, 0x%016lx}", num.u_char_val, num.bytes[0],
                  num.bytes[1]);
  case TY_U_SHORT:
    return format("%hu {0x%016lx, 0x%016lx}", num.u_short_val, num.bytes[0],
                  num.bytes[1]);
  case TY_U_INT:
    return format("%u {0x%016lx, 0x%016lx}", num.u_int_val, num.bytes[0],
                  num.bytes[1]);
  case TY_U_LONG:
    return format("%lu {0x%016lx, 0x%016lx}", num.u_long_val, num.bytes[0],
                  num.bytes[1]);
  case TY_U_LLONG:
    return format("%llu {0x%016lx, 0x%016lx}", num.u_llong_val, num.bytes[0],
                  num.bytes[1]);
  case TY_PTR:
    return format("%" PRIdPTR "{0x%016lx, 0x%016lx}", num.ptr_val, num.bytes[0],
                  num.bytes[1]);
  case TY_ENUM:
    return format("%d {0x%016lx, 0x%016lx}", num.enum_val, num.bytes[0],
                  num.bytes[1]);
  case TY_FLOAT:
    return format("%f {0x%016lx, 0x%016lx}", num.float_val, num.bytes[0],
                  num.bytes[1]);
  case TY_DOUBLE:
    return format("%f {0x%016lx, 0x%016lx}", num.double_val, num.bytes[0],
                  num.bytes[1]);
  case TY_LDOUBLE:
    return format("%Lf {0x%016lx, 0x%016lx}", num.ldouble_val, num.bytes[0],
                  num.bytes[1]);
  case TY_VOID:
    return format("void {0x%016lx, 0x%016lx}", num.bytes[0], num.bytes[1]);
  case TY_ARRAY:
  case TY_FUNC:
  case TY_STRUCT:
  case TY_UNION:
  case TY_BUILTIN:
    break;
  }
  assert(false);
}
