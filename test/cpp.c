#include "cpp.h"

#ifdef FOO
#error "FOO must not be defined"
#endif

#ifndef FOO
#else
#error "FOO must not be defined"
#endif

#define BAR
#ifdef BAR
static int n0 = 100;
#else
#error "BAR must be defined"
#endif

#ifdef FOO
#ifdef BAR
#error "FOO + BAR"
#else
#error "FOO + not BAR"
#endif
#else
#ifdef BAR
static int n1 = 200;
#else
#error "not FOO + not BAR"
#endif
#endif

#if 0 && 1
#error "0 && 1 must not be true"
#elif 0 || 0
#error "0 || 0 must not be true"
#elif 1
static int n2 = 300;
#else
#error "never come"
#endif

#define FOO
#if defined(FOO)
static int n3 = 400;
#else
#error "FOO must be defined"
#endif

#undef FOO
#if !defined(FOO)
static int n4 = 500;
#else
#error "FOO must not be defined"
#endif

// comment                                      \
this is also commen
// clang-format off
static int n5 = 6\
00;
// clang-format on

static void check_int(int a, int b) {
  if (a != b) {
    printf("FAILED %d != %d\n", a, b);
    abort();
  }
}

int main(void) {
  check_int(100, n0);
  check_int(200, n1);
  check_int(300, n2);
  check_int(400, n3);
  check_int(500, n4);
  check_int(600, n5);

  puts("OK");
  return 0;
}
