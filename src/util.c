#include "gifcc.h"
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Vector {
  void **data;
  int capacity;
  int len;
} Vector;

typedef struct Map {
  Vector *keys;
  Vector *vals;
} Map;

typedef struct IntVector {
  int *data;
  int capacity;
  int len;
} IntVector;

typedef struct String {
  char *data;
  int capacity;
  int len;
} String;

Vector *new_vector(void) {
  Vector *vec = NEW(Vector);
  vec->data = NULL;
  vec->capacity = 0;
  vec->len = 0;
  return vec;
}

Vector *vec_clone(Vector *vec) {
  Vector *cloned = NEW(Vector);
  vec_reserve(cloned, vec->capacity);
  for (int i = 0; i < vec_len(vec); i++) {
    vec_push(cloned, vec_get(vec, i));
  }
  return cloned;
}

int vec_len(const Vector *vec) { return vec->len; }

void *vec_first(Vector *vec) {
  assert(vec->len > 0);
  return vec_get(vec, 0);
}

void *vec_last(Vector *vec) { return vec_rget(vec, 0); }

void *vec_get(Vector *vec, int n) {
  assert(vec->len > n);
  return vec->data[n];
}

void vec_set(Vector *vec, int n, void *val) {
  assert(vec->len > n);
  vec->data[n] = val;
}

void *vec_rget(Vector *vec, int n) {
  assert(vec->len > n);
  return vec->data[vec->len - 1 - n];
}

void vec_push(Vector *vec, void *elem) {
  vec_reserve(vec, vec->len + 1);
  vec->data[vec->len++] = elem;
}

void *vec_pop(Vector *vec) {
  assert(vec->len > 0);
  return vec->data[--vec->len];
}
void vec_insert(Vector *vec, int n, void *elem) {
  assert(n <= vec->len);
  vec_reserve(vec, vec->len + 1);
  memmove(&vec->data[n + 1], &vec->data[n], (vec->len - n) * sizeof(void *));
  vec->data[n] = elem;
  vec->len++;
}
void *vec_remove(Vector *vec, int n) {
  assert(n < vec->len);
  void *ret = vec->data[n];
  memmove(&vec->data[n], &vec->data[n + 1],
          (vec->len - n - 1) * sizeof(void *));
  vec->len--;
  return ret;
}
void vec_append(Vector *dst, Vector *src) {
  for (int i = 0; i < vec_len(src); i++) {
    vec_push(dst, vec_get(src, i));
  }
}
void vec_reserve(Vector *vec, int len) {
  int cap = vec->capacity;
  if (cap == 0) {
    cap = 16;
  }
  while (cap < len) {
    cap *= 2;
  }
  if (cap > vec->capacity) {
    vec->data = realloc(vec->data, sizeof(void *) * cap);
    vec->capacity = cap;
  }
}
void vec_extend(Vector *vec, int len) {
  while (vec->len < len) {
    vec_push(vec, NULL);
  }
}

Map *new_map(void) {
  Map *map = NEW(Map);
  map->keys = new_vector();
  map->vals = new_vector();
  return map;
}

int map_size(const Map *map) { return map->keys->len; }

void *map_get_by_index(Map *map, int n, char **key) {
  assert(n < map_size(map));
  if (key != NULL) {
    *key = map->keys->data[n];
  }
  return map->vals->data[n];
}
void map_set_by_index(Map *map, int n, char *key, void *val) {
  assert(n < map_size(map));
  map->keys->data[n] = key;
  map->vals->data[n] = val;
}

void map_put(Map *map, char *key, void *val) {
  vec_push(map->keys, key);
  vec_push(map->vals, val);
}

void *map_get(Map *map, char *key) {
  for (int i = map->keys->len - 1; i >= 0; i--) {
    const char *map_key = map->keys->data[i];
    if (map_key == NULL) {
      continue;
    }
    if (strcmp(map_key, key) == 0) {
      return map->vals->data[i];
    }
  }
  return NULL;
}

bool map_remove(Map *map, char *key) {
  bool removed = false;
  for (int i = 0; i < map_size(map); i++) {
    const char *map_key = map->keys->data[i];
    if (map_key == NULL) {
      continue;
    }
    if (strcmp(map_key, key) == 0) {
      map->keys->data[i] = NULL;
      map->vals->data[i] = NULL;
      removed = true;
    }
  }
  return removed;
}

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

IntVector *new_int_vector(void) {
  IntVector *vec = NEW(IntVector);
  vec->data = NULL;
  vec->capacity = 0;
  vec->len = 0;
  return vec;
}
int int_vec_len(const IntVector *vec) { return vec->len; }
int int_vec_get(const IntVector *vec, int n) {
  assert(n < vec->len);
  return vec->data[n];
}
void int_vec_push(IntVector *vec, int elem) {
  if (vec->capacity == vec->len) {
    vec->capacity = (vec->capacity == 0) ? 16 : vec->capacity * 2;
    vec->data = realloc(vec->data, sizeof(int) * vec->capacity);
  }
  vec->data[vec->len++] = elem;
}

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

int __attribute__((format(printf, 2, 3)))
alloc_printf(char **strp, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int ret = alloc_printf_v(strp, fmt, ap);
  va_end(ap);
  return ret;
}

int alloc_printf_v(char **strp, const char *fmt, va_list ap) {
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
