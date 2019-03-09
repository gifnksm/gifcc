#include "gifcc.h"
#include <ctype.h>
#include <stdarg.h>

void print_string_literal(const char *str) {
  printf("\"");
  for (int i = 0; str[i] != '\0'; i++) {
    switch (str[i]) {
    case '\'':
    case '\"':
    case '\\':
      printf("\\%c", str[i]);
      break;
    case '\a':
      printf("\\a");
      break;
    case '\b':
      printf("\\b");
      break;
    case '\f':
      printf("\\f");
      break;
    case '\n':
      printf("\\n");
      break;
    case '\r':
      printf("\\r");
      break;
    case '\t':
      printf("\\t");
      break;
    case '\v':
      printf("\\v");
      break;
    default:
      if (isgraph(str[i]) || isspace(str[i])) {
        printf("%c", str[i]);
      } else {
        printf("\\%hho", str[i]);
      }
    }
  }
  printf("\"");
}

static int alloc_printf_v(char **strp, const char *fmt, va_list ap) {
  va_list aq;
  va_copy(aq, ap);

  char *buf = NULL;
  int len = vsnprintf(buf, 0, fmt, ap);
  if (len < 0) {
    return len;
  }

  buf = malloc(len + 1);
  int len2 = vsnprintf(buf, len + 1, fmt, aq);
  if (len2 < 0) {
    return len2;
  }
  assert(len == len2);
  *strp = buf;

  va_end(aq);

  return len;
}

char *__attribute__((format(printf, 1, 2))) format(const char *fmt, ...) {
  char *buf = NULL;
  va_list ap;

  va_start(ap, fmt);
  alloc_printf_v(&buf, fmt, ap);
  va_end(ap);

  return buf;
}
