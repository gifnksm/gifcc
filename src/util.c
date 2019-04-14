#include "gifcc.h"
#include <ctype.h>
#include <stdarg.h>

// djb2 hash algorithm
uint64_t str2hash(const char *str) {
  uint64_t hash = 5381;
  for (int i = 0; str[i] != '\0'; i++) {
    hash = ((hash << 5) + hash) + str[i];
  }
  return hash;
}

bool is_hex_digit(int c) { return isxdigit(c) != 0; }
int hex2num(int c) {
  assert(is_hex_digit(c));
  if ('0' <= c && c <= '9') {
    return c - '0';
  }
  if ('a' <= c && c <= 'f') {
    return (c - 'a') + 0xa;
  }
  assert('A' <= c && c <= 'F');
  return (c - 'A') + 0xa;
}
bool is_oct_digit(int c) { return '0' <= c && c <= '7'; }
int oct2num(int c) {
  assert(is_oct_digit(c));
  return c - '0';
}

bool is_ident_head(int c) {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}
bool is_ident_tail(int c) { return is_ident_head(c) || ('0' <= c && c <= '9'); }

char *format_string_literal(const char *str) {
  String *buf = new_string();
  str_push(buf, '"');
  for (int i = 0; str[i] != '\0'; i++) {
    switch (str[i]) {
    case '\'':
    case '\"':
    case '\\':
      str_append(buf, format("\\%c", str[i]));
      break;
    case '\a':
      str_append(buf, format("\\a"));
      break;
    case '\b':
      str_append(buf, format("\\b"));
      break;
    case '\f':
      str_append(buf, format("\\f"));
      break;
    case '\n':
      str_append(buf, format("\\n"));
      break;
    case '\r':
      str_append(buf, format("\\r"));
      break;
    case '\t':
      str_append(buf, format("\\t"));
      break;
    case '\v':
      str_append(buf, format("\\v"));
      break;
    default:
      if (isgraph(str[i]) || isspace(str[i])) {
        str_append(buf, format("%c", str[i]));
      } else {
        str_append(buf, format("\\%hho", str[i]));
      }
    }
  }
  str_push(buf, '"');
  str_push(buf, '\0');
  return str_get_raw(buf);
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
