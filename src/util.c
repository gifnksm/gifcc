#include "gifcc.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Vector *new_vector(void) {
  Vector *vec = NEW(Vector);
  vec->data = NULL;
  vec->capacity = 0;
  vec->len = 0;
  return vec;
}

void vec_push(Vector *vec, void *elem) {
  if (vec->capacity == vec->len) {
    vec->capacity = (vec->capacity == 0) ? 16 : vec->capacity * 2;
    vec->data = realloc(vec->data, sizeof(void *) * vec->capacity);
  }
  vec->data[vec->len++] = elem;
}

void *vec_pop(Vector *vec) { return vec->data[vec->len--]; }

void *vec_peek(Vector *vec) { return vec_peek_n(vec, 0); }

void *vec_peek_n(Vector *vec, int n) {
  assert(vec->len > n);
  return vec->data[vec->len - 1 - n];
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

void map_put(Map *map, char *key, void *val) {
  vec_push(map->keys, key);
  vec_push(map->vals, val);
}

void *map_get(Map *map, char *key) {
  for (int i = map->keys->len - 1; i >= 0; i--) {
    if (strcmp(map->keys->data[i], key) == 0) {
      return map->vals->data[i];
    }
  }
  return NULL;
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

IntVector *new_int_vector(void) {
  IntVector *vec = NEW(IntVector);
  vec->data = NULL;
  vec->capacity = 0;
  vec->len = 0;
  return vec;
}
void int_vec_push(IntVector *vec, int elem) {
  if (vec->capacity == vec->len) {
    vec->capacity = (vec->capacity == 0) ? 16 : vec->capacity * 2;
    vec->data = realloc(vec->data, sizeof(int) * vec->capacity);
  }
  vec->data[vec->len++] = elem;
}

void print_string_literal(char *str) {
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
