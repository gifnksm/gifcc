#include "gifcc.h"
#include <string.h>

#define DEFINE_VECTOR(typename, elemtype)                                      \
  struct typename {                                                            \
    elemtype *vec_data;                                                        \
    int vec_capacity;                                                          \
    int vec_len;                                                               \
  }

#define NEW_VECTOR(vectype) (NEW(vectype))
#define VEC_ELEM_SIZE(vec) (sizeof((vec)->vec_data[0]))

void _vec_reserve(void **data, int *cap, size_t elemsize, int len);
#define VEC_RESERVE(vec, len)                                                  \
  _vec_reserve((void **)&(vec)->vec_data, &(vec)->vec_capacity,                \
               VEC_ELEM_SIZE(vec), len)

#define VEC_CLONE(vec)                                                         \
  ({                                                                           \
    typeof(vec) cloned = NEW(typeof(*vec));                                    \
    VEC_RESERVE(cloned, (vec)->vec_capacity);                                  \
    memcpy(cloned->vec_data, (vec)->vec_data,                                  \
           VEC_ELEM_SIZE(vec) * VEC_LEN(vec));                                 \
    (cloned)->vec_len = VEC_LEN(vec);                                          \
    cloned;                                                                    \
  })

#define VEC_LEN(vec) ((vec)->vec_len)
#define VEC_AS_PTR(vec) ((vec)->vec_data)
#define VEC_BSEARCH(vec, key, compar)                                          \
  ((typeof(&(vec)->vec_data[0]))bsearch((key), VEC_AS_PTR(vec), VEC_LEN(vec),  \
                                        VEC_ELEM_SIZE(vec), (compar)))

#define VEC_GET(vec, n)                                                        \
  ({                                                                           \
    assert(VEC_LEN(vec) > (n));                                                \
    (vec)->vec_data[n];                                                        \
  })
#define VEC_SET(vec, n, val)                                                   \
  ({                                                                           \
    assert(VEC_LEN(vec) > (n));                                                \
    (vec)->vec_data[n] = val;                                                  \
  })
#define VEC_GET_REF(vec, n)                                                    \
  ({                                                                           \
    assert(VEC_LEN(vec) > (n));                                                \
    &(vec)->vec_data[n];                                                       \
  })

#define VEC_FIRST(vec) VEC_GET(vec, 0)
#define VEC_LAST(vec) VEC_GET(vec, VEC_LEN(vec) - 1)
#define VEC_RGET(vec, n) VEC_GET(vec, VEC_LEN(vec) - n - 1)

#define VEC_FIRST_REF(vec) VEC_GET_REF(vec, 0)
#define VEC_LAST_REF(vec) VEC_GET_REF(vec, VEC_LEN(vec) - 1)
#define VEC_RGET_REF(vec, n) VEC_GET_REF(vec, VEC_LEN(vec) - n - 1)

#define VEC_PUSH(vec, elem)                                                    \
  {                                                                            \
    VEC_RESERVE((vec), VEC_LEN(vec) + 1);                                      \
    (vec)->vec_data[(vec)->vec_len++] = elem;                                  \
  }
#define VEC_POP(vec)                                                           \
  ({                                                                           \
    assert(VEC_LEN(vec) > 0);                                                  \
    (vec)->vec_data[--(vec)->vec_len];                                         \
  })

#define VEC_INSERT(vec, n, elem)                                               \
  ({                                                                           \
    assert((n) <= VEC_LEN(vec));                                               \
    VEC_RESERVE((vec), VEC_LEN(vec) + 1);                                      \
    if (n < VEC_LEN(vec) - 1) {                                                \
      memmove(&(vec)->vec_data[(n) + 1], &(vec)->vec_data[n],                  \
              (VEC_LEN(vec) - n) * VEC_ELEM_SIZE(vec));                        \
    }                                                                          \
    (vec)->vec_data[n] = (elem);                                               \
    (vec)->vec_len++;                                                          \
  })
#define VEC_REMOVE(vec, n)                                                     \
  ({                                                                           \
    assert((n) < VEC_LEN(vec));                                                \
    typeof((vec)->vec_data[0]) ret = VEC_GET(vec, n);                          \
    if (VEC_LEN(vec) > 1) {                                                    \
      memmove(&(vec)->vec_data[n], &(vec)->vec_data[(n) + 1],                  \
              (VEC_LEN(vec) - (n)-1) * VEC_ELEM_SIZE(vec));                    \
    }                                                                          \
    (vec)->vec_len--;                                                          \
    ret;                                                                       \
  })

#define VEC_APPEND(dst, src)                                                   \
  ({                                                                           \
    VEC_RESERVE((dst), VEC_LEN(dst) + VEC_LEN(src));                           \
    memcpy(&(dst)->vec_data[VEC_LEN(dst)], (src)->vec_data,                    \
           VEC_LEN(src) * VEC_ELEM_SIZE(src));                                 \
    (dst)->vec_len += VEC_LEN(src);                                            \
  })

#define VEC_EXTEND(vec, len, elem)                                             \
  ({                                                                           \
    VEC_RESERVE((vec), len);                                                   \
    while (VEC_LEN(vec) < (len)) {                                             \
      (vec)->vec_data[(vec)->vec_len++] = elem;                                \
    }                                                                          \
  })

#define VEC_CLEAR(vec) ({ (vec)->vec_len = 0; })

#define VEC_FOREACH_IDX(item, idx, vec)                                        \
  for (int _keep = true, idx = 0; _keep && idx < VEC_LEN(vec);                 \
       _keep = !_keep, idx++)                                                  \
    for (item = VEC_GET(vec, idx); _keep; _keep = !_keep)

#define VEC_FOREACH(item, vec) VEC_FOREACH_IDX (item, _idx, vec)

#define VEC_FOREACH_REVERSE_IDX(item, idx, vec)                                \
  for (int _keep = true, idx = VEC_LEN(vec) - 1; _keep && idx >= 0;            \
       _keep = !_keep, idx--)                                                  \
    for (item = VEC_GET(vec, idx); _keep; _keep = !_keep)

#define VEC_FOREACH_REVERSE(item, vec) VEC_FOREACH_REVERSE_IDX (item, _idx, vec)
