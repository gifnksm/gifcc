#include "cpp.h"

#define CHECK_INT(a, b) check_int(__FILE__, __LINE__, (a), (b))
static void check_int(const char *file, int line, int a, int b);

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

#define DEFVAR(var, val, type) type var = val
DEFVAR(n6, 700, int);

#define CAT2(a, b) a##b
#define CAT(a, b) CAT2(a, b)
#define AB(x) CAT(x, y)
static void run01(void) {
  static int xy = 800;
  CHECK_INT(800, CAT(A, B)(x));
}

static void run02(void) {
  printf("__DATE__ = %s\n", __DATE__);
  printf("__TIME__ = %s\n", __TIME__);
  printf("__FILE__ = %s\n", __FILE__);
  printf("__LINE__ = %d\n", __LINE__);
}

static void check_int(const char *file, int line, int a, int b) {
  if (a != b) {
    printf("%s:%d:FAILED %d != %d\n", file, line, a, b);
    abort();
  }
}

int main(void) {
  CHECK_INT(100, n0);
  CHECK_INT(200, n1);
  CHECK_INT(300, n2);
  CHECK_INT(400, n3);
  CHECK_INT(500, n4);
  CHECK_INT(600, n5);
  CHECK_INT(700, n6);
  run01();
  run02();

  puts("OK");
  return 0;
}
