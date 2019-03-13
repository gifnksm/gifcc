#ifndef _STDARG_H
#define _STDARG_H

typedef __builtin_va_list va_list;
typedef va_list __gnuc_va_list;

#define va_start(ap, last) __builtin_va_start(ap, last)
#define va_arg(ap, type) __builtin_va_arg(ap, (type *)0)
#define va_end(ap) __builtin_va_end(ap)
#define va_copy(dest, src) __builtin_va_copy(dest, src)

#endif // _STDARG_H
