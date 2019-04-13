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
  Vector *vec = new_vector();
  expect(__LINE__, 0, vec_len(vec));

  for (intptr_t i = 0; i < 100; i++) {
    vec_push(vec, (void *)i);
  }

  expect(__LINE__, 100, vec_len(vec));
  expect(__LINE__, 0, (intptr_t)vec_get(vec, 0));
  expect(__LINE__, 50, (intptr_t)vec_get(vec, 50));
  expect(__LINE__, 99, (intptr_t)vec_get(vec, 99));

  expect(__LINE__, 8, (intptr_t)vec_remove(vec, 8));
  expect(__LINE__, 99, vec_len(vec));
  for (intptr_t i = 0; i < 8; i++) {
    expect(__LINE__, i, (intptr_t)vec_get(vec, i));
  }
  for (intptr_t i = 8; i < 99; i++) {
    expect(__LINE__, i + 1, (intptr_t)vec_get(vec, i));
  }

  vec_insert(vec, 8, (void *)100);
  vec_insert(vec, 0, (void *)200);
  vec_insert(vec, 101, (void *)300);
  expect(__LINE__, 102, vec_len(vec));
  expect(__LINE__, 200, (intptr_t)vec_get(vec, 0));
  for (intptr_t i = 1; i < 9; i++) {
    expect(__LINE__, i - 1, (intptr_t)vec_get(vec, i));
  }
  expect(__LINE__, 100, (intptr_t)vec_get(vec, 9));
  for (intptr_t i = 10; i < 101; i++) {
    expect(__LINE__, i - 1, (intptr_t)vec_get(vec, i));
  }
  expect(__LINE__, 300, (intptr_t)vec_get(vec, 101));
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
