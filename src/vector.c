#include "gifcc.h"
#include <string.h>

typedef struct Vector {
  void **data;
  int capacity;
  int len;
} Vector;

Vector *new_vector(void) {
  Vector *vec = NEW(Vector);
  vec->data = NULL;
  vec->capacity = 0;
  vec->len = 0;
  return vec;
}

Vector *vec_clone(const Vector *vec) {
  Vector *cloned = NEW(Vector);
  vec_reserve(cloned, vec->capacity);
  for (int i = 0; i < vec_len(vec); i++) {
    vec_push(cloned, vec_get(vec, i));
  }
  return cloned;
}

int vec_len(const Vector *vec) { return vec->len; }

void *vec_first(const Vector *vec) {
  assert(vec->len > 0);
  return vec_get(vec, 0);
}

void *vec_last(const Vector *vec) { return vec_rget(vec, 0); }

void *vec_get(const Vector *vec, int n) {
  assert(vec->len > n);
  return vec->data[n];
}

void vec_set(Vector *vec, int n, void *val) {
  assert(vec->len > n);
  vec->data[n] = val;
}

void *vec_rget(const Vector *vec, int n) {
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
  if (vec->len == 1) {
    vec->len--;
    return ret;
  }

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

void vec_clear(Vector *vec) { vec->len = 0; }

typedef struct RawVector {
  void *vec_data;
  int vec_capacity;
  int vec_len;
} RawVector;

void _vec_reserve(void **data, int *cap, size_t elemsize, int len) {
  int old_cap = *cap;
  int new_cap = old_cap;
  if (new_cap == 0) {
    new_cap = 16;
  }
  while (new_cap < len) {
    new_cap *= 2;
  }
  if (new_cap > old_cap) {
    *data = realloc(*data, elemsize * new_cap);
    *cap = new_cap;
  }
}
