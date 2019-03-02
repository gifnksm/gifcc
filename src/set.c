#include "gifcc.h"
#include <string.h>

typedef struct Set {
  Vector *keys;
} Set;

Set *new_set(void) {
  Set *set = NEW(Set);
  set->keys = new_vector();
  return set;
}

Set *set_clone(const Set *set) {
  Set *cloned = NEW(Set);
  cloned->keys = vec_clone(set->keys);
  return cloned;
}
int set_size(const Set *set) { return vec_len(set->keys); }

const char *set_get_by_index(Set *set, int n) {
  assert(n < set_size(set));
  return vec_get(set->keys, n);
}

void set_insert(Set *set, const char *key) { vec_push(set->keys, (void *)key); }
bool set_contains(const Set *set, const char *key) {
  for (int i = 0; i < vec_len(set->keys); i++) {
    const char *set_key = vec_get(set->keys, i);
    if (set_key == NULL) {
      continue;
    }
    if (strcmp(set_key, key) == 0) {
      return true;
    }
  }
  return false;
}

Set *set_intersection(const Set *a, const Set *b) {
  Set *set = new_set();
  for (int i = 0; i < vec_len(a->keys); i++) {
    const char *key = vec_get(a->keys, i);
    if (set_contains(b, key)) {
      set_insert(set, key);
    }
  }
  return set;
}
