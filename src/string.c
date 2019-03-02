#include "gifcc.h"

typedef struct String {
  char *data;
  int capacity;
  int len;
} String;

String *new_string(void) {
  String *str = NEW(String);
  str->data = NULL;
  str->capacity = 0;
  str->len = 0;
  return str;
}
void str_push(String *str, char elem) {
  if (str->capacity == str->len) {
    str->capacity = (str->capacity == 0) ? 16 : str->capacity * 2;
    str->data = realloc(str->data, sizeof(char) * str->capacity);
  }
  str->data[str->len++] = elem;
}
void str_append(String *str, const char *elems) {
  for (int i = 0; elems[i] != '\0'; i++) {
    str_push(str, elems[i]);
  }
}
char *str_get_raw(String *str) { return str->data; }
