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

#define DEFVAR(var, val, type) type var = val
DEFVAR(n6, 700, int);

#line 100
#if __LINE__ != 100
#error "ERROR"
#endif

static void test01(void) {
  CHECK_INT(100, n0);
  CHECK_INT(200, n1);
  CHECK_INT(300, n2);
  CHECK_INT(400, n3);
  CHECK_INT(500, n4);
  CHECK_INT(600, n5);
  CHECK_INT(700, n6);
}

#define CAT2(a, b) a##b
#define CAT(a, b) CAT2(a, b)
#define AB(x) CAT(x, y)
static void test02(void) {
  static int xy = 800;
  CHECK_INT(800, CAT(A, B)(x));
}

static void test03(void) {
  TEST_PRINTF("__DATE__ = %s", __DATE__);
  TEST_PRINTF("__TIME__ = %s", __TIME__);
  TEST_PRINTF("__FILE__ = %s", __FILE__);
  TEST_PRINTF("__LINE__ = %d", __LINE__);
}

int main(int argc, char *argv[]) {
  Test test_list[] = {TEST(test01), TEST(test02), TEST(test03), {NULL, NULL}};
  RUN_TEST(argc, argv, test_list);
}
