#include "gifcc.h"
#include <string.h>

typedef struct CharVector {
  Char *data;
  int capacity;
  int len;
} CharVector;

CharVector *new_char_vector(void) {
  CharVector *vec = NEW(CharVector);
  vec->data = NULL;
  vec->capacity = 0;
  vec->len = 0;
  return vec;
}
int char_vec_len(const CharVector *vec) { return vec->len; }
Char char_vec_get(const CharVector *vec, int n) {
  assert(n < vec->len);
  return vec->data[n];
}
void char_vec_push(CharVector *vec, Char elem) {
  if (vec->capacity == vec->len) {
    vec->capacity = (vec->capacity == 0) ? 16 : vec->capacity * 2;
    vec->data = realloc(vec->data, sizeof(Char) * vec->capacity);
  }
  vec->data[vec->len++] = elem;
}
Char char_vec_remove(CharVector *vec, int n) {
  assert(n < vec->len);
  Char ret = vec->data[n];

  if (vec->len == 1) {
    vec->len = 0;
    return ret;
  }

  memmove(&vec->data[n], &vec->data[n + 1], (vec->len - n - 1) * sizeof(Char));
  vec->len--;
  return ret;
}
