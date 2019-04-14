#include "gifcc.h"
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static void expect(int line, intptr_t expected, intptr_t actual) {
  if (expected == actual) {
    return;
  }

  fprintf(stderr, "%d: %" PRIdPTR " expected, but got %" PRIdPTR "\n", line,
          expected, actual);
  exit(1);
}

static void test_vector(void) {
  typedef DEFINE_VECTOR(IntVector, int) IntVector;

  IntVector *vec = NEW_VECTOR(IntVector);
  expect(__LINE__, 0, VEC_LEN(vec));

  for (int i = 0; i < 100; i++) {
    VEC_PUSH(vec, i);
  }

  expect(__LINE__, 100, VEC_LEN(vec));
  expect(__LINE__, 0, VEC_GET(vec, 0));
  expect(__LINE__, 50, VEC_GET(vec, 50));
  expect(__LINE__, 99, VEC_GET(vec, 99));
  expect(__LINE__, 99, VEC_RGET(vec, 0));
  expect(__LINE__, 49, VEC_RGET(vec, 50));
  expect(__LINE__, 0, VEC_RGET(vec, 99));

  IntVector *cloned = VEC_CLONE(vec);

  expect(__LINE__, 8, VEC_REMOVE(vec, 8));
  expect(__LINE__, 99, VEC_LEN(vec));
  for (int i = 0; i < 8; i++) {
    expect(__LINE__, i, VEC_GET(vec, i));
  }
  for (int i = 8; i < 99; i++) {
    expect(__LINE__, i + 1, VEC_GET(vec, i));
  }
  for (int i = 0; i < 100; i++) {
    expect(__LINE__, i, VEC_GET(cloned, i));
  }

  VEC_INSERT(vec, 8, 100);
  VEC_INSERT(vec, 0, 200);
  VEC_INSERT(vec, 101, 300);
  expect(__LINE__, 102, VEC_LEN(vec));
  expect(__LINE__, 200, VEC_GET(vec, 0));
  for (int i = 1; i < 9; i++) {
    expect(__LINE__, i - 1, VEC_GET(vec, i));
  }
  expect(__LINE__, 100, VEC_GET(vec, 9));
  for (int i = 10; i < 101; i++) {
    expect(__LINE__, i - 1, VEC_GET(vec, i));
  }
  expect(__LINE__, 300, VEC_GET(vec, 101));
}

static void test_map(void) {
  Map *map = new_map();
  expect(__LINE__, 0, (intptr_t)map_get(map, "foo"));
  expect(__LINE__, 0, map_size(map));

  map_put(map, "foo", (void *)2);
  expect(__LINE__, 2, (intptr_t)map_get(map, "foo"));
  expect(__LINE__, 1, map_size(map));
  map_put(map, "foo", (void *)2);
  expect(__LINE__, 2, (intptr_t)map_get(map, "foo"));
  expect(__LINE__, 1, map_size(map));

  map_put(map, "bar", (void *)4);
  expect(__LINE__, 4, (intptr_t)map_get(map, "bar"));
  expect(__LINE__, 2, map_size(map));

  map_put(map, "foo", (void *)6);
  expect(__LINE__, 6, (intptr_t)map_get(map, "foo"));
  expect(__LINE__, 2, map_size(map));
}

static void test_set(void) {
  Set *set = new_set();
  expect(__LINE__, 0, set_contains(set, "foo"));
  expect(__LINE__, 0, set_size(set));

  set_insert(set, "foo");
  expect(__LINE__, 1, set_contains(set, "foo"));
  expect(__LINE__, 1, set_size(set));
  set_insert(set, "foo");
  expect(__LINE__, 1, set_contains(set, "foo"));
  expect(__LINE__, 1, set_size(set));

  set_insert(set, "bar");
  expect(__LINE__, 1, set_contains(set, "foo"));
  expect(__LINE__, 1, set_contains(set, "bar"));
  expect(__LINE__, 2, set_size(set));

  Set *set2 = new_set();
  set_insert(set2, "foo");
  set_insert(set2, "baz");

  Set *set3 = set_intersection(set, set2);
  expect(__LINE__, 1, set_contains(set3, "foo"));
  expect(__LINE__, 1, set_size(set3));
}

void runtest(void) {
  test_vector();
  test_map();
  test_set();
  printf("OK\n");
}
