#include "gifcc.h"
#include <string.h>

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
