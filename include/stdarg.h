#ifndef _STDARG_H
#define _STDARG_H

typedef struct {
  unsigned int gp_offset;
  unsigned int fp_offset;
  void *overflow_arg_area;
  void *reg_save_area;
} va_list[1];

typedef va_list __gnuc_va_list;

#endif // _STDARG_H
