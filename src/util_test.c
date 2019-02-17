#include "gifcc.h"
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static void expect(int line, intptr_t expected, intptr_t actual) {
  if (expected == actual) {
    return;
  }

  fprintf(stderr, "%d: %" PRIXPTR " expected, but got %" PRIXPTR "\n", line,
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
}

static void test_map(void) {
  Map *map = new_map();
  expect(__LINE__, 0, (intptr_t)map_get(map, "foo"));

  map_put(map, "foo", (void *)2);
  expect(__LINE__, 2, (intptr_t)map_get(map, "foo"));

  map_put(map, "bar", (void *)4);
  expect(__LINE__, 4, (intptr_t)map_get(map, "bar"));

  map_put(map, "foo", (void *)6);
  expect(__LINE__, 6, (intptr_t)map_get(map, "foo"));
}

void runtest(void) {
  test_vector();
  test_map();
  printf("OK\n");
}
