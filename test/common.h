#ifndef COMMON_H
#define COMMON_H

#include <assert.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>

#define TEST(name)                                                             \
  { #name, name }
#define CHECK_INT(a, b) check_int(__FILE__, __LINE__, (a), (b))
#define RUN_TEST(argc, argv, test_list)                                        \
  run_test((argc), (argv), __FILE__, (test_list))
#define TEST_PRINTF(fmt, ...)                                                  \
  printf("    %s::%s " fmt " (%s:%d)\n", (g_suite_name), (g_test_name),        \
         ##__VA_ARGS__, __FILE__, __LINE__)

typedef struct Test {
  const char *name;
  void (*func)(void);
} Test;

static inline __attribute__((format(printf, 1, 2))) char *
format(const char *fmt, ...) {
  char *buf = NULL;
  va_list ap, aq;

  va_start(ap, fmt);
  va_copy(aq, ap);

  int len = vsnprintf(buf, 0, fmt, ap);
  if (len < 0) {
    return NULL;
  }
  buf = malloc(len + 1);
  int len2 = vsnprintf(buf, len + 1, fmt, aq);
  if (len2 < 0) {
    return NULL;
  }

  va_end(ap);

  return buf;
}

static int g_num_check = 0;
static const char *g_fail_message = NULL;
static const char *g_suite_name = NULL;
static const char *g_test_name = NULL;
static jmp_buf g_jmp_buf;
static inline void check_int(const char *file, int line, int a, int b) {
  g_num_check++;
  if (a != b) {
    const char *msg = format("%s::%s FAILED %d != %d (%s:%d)", g_suite_name,
                             g_test_name, a, b, file, line);
    printf("    %s\n", msg);

    if (getenv("ABORT_ON_ERROR") != NULL) {
      fprintf(stderr, "ABORT: %s", msg);
      abort();
    }

    g_fail_message = msg;
    longjmp(g_jmp_buf, 1);
  }
}

static inline noreturn void run_test(int argc, char *argv[],
                                     const char *suite_name,
                                     const Test *test_list) {
  int num_test = 0;
  bool success = true;

  if (argc != 2) {
    fprintf(stderr, "Usage: %s <tap-name>\n", argv[0]);
    exit(1);
  }

  FILE *tap_fp = fopen(argv[1], "w");
  if (tap_fp == NULL) {
    perror("failed to open tap file");
    exit(1);
  }

  g_suite_name = suite_name;
  fprintf(tap_fp, "TAP version 13\n");
  for (int i = 0; test_list[i].name != NULL; i++) {
    num_test++;
  }
  fprintf(tap_fp, "1..%d\n", num_test);
  for (int i = 0; test_list[i].name != NULL; i++) {
    const Test *test = &test_list[i];
    g_test_name = test->name;
    g_num_check = 0;
    g_fail_message = NULL;
    printf("  %s::%s start\n", suite_name, test->name);
    if (setjmp(g_jmp_buf) == 0) {
      test->func();
      printf("  %s::%s OK (%d assertions)\n", suite_name, test->name,
             g_num_check);
      fprintf(tap_fp, "ok %s::%s # %d assersions\n", suite_name, test->name,
              g_num_check);
    } else {
      printf("  %s::%s NOT OK (%d assertions)\n", suite_name, test->name,
             g_num_check);
      fprintf(tap_fp, "not ok %s::%s # %s\n", suite_name, test->name,
              g_fail_message);
      success = false;
    }
  }
  if (!success) {
    exit(EXIT_FAILURE);
  }
  exit(EXIT_SUCCESS);
}

#endif
