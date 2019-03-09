#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>

#define TEST(name)                                                             \
  { #name, name }
#define CHECK_INT(a, b) check_int(__FILE__, __LINE__, (a), (b))
#define RUN_TEST(test_list) run_test(__FILE__, (test_list))

typedef struct Test {
  const char *name;
  void (*func)(void);
} Test;

static int num_check = 0;
static const char *suite_name = NULL;
static const char *test_name = NULL;
static inline void check_int(const char *file, int line, int a, int b) {
  if (a != b) {
    printf("  %s::%s FAILED %d != %d (%s:%d)\n", suite_name, test_name, a, b,
           file, line);
    abort();
  }
  num_check++;
}

static void run_test(const char *suite_name, const Test *test_list) {
  int num_test = 0;
  suite_name = suite_name;
  for (int i = 0; test_list[i].name != NULL; i++) {
    const Test *test = &test_list[i];
    test_name = test->name;
    num_check = 0;
    test->func();
    printf("  %s::%s OK (%d assertions)\n", suite_name, test->name, num_check);
    num_test++;
  }
  printf("%s OK (%d test)\n", suite_name, num_test);
}

#endif
