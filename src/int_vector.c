#include "gifcc.h"
#include <string.h>

typedef struct IntVector {
  int *data;
  int capacity;
  int len;
} IntVector;

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
int int_vec_remove(IntVector *vec, int n) {
  assert(n < vec->len);
  int ret = vec->data[n];

  if (vec->len == 1) {
    vec->len = 0;
    return ret;
  }

  memmove(&vec->data[n], &vec->data[n + 1], (vec->len - n - 1) * sizeof(int));
  vec->len--;
  return ret;
}
