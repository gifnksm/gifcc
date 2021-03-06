#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

static void test01(void) {
  CHECK_INT(0, 0);
  CHECK_INT(42, 42);
  CHECK_INT(21, 5 + 20 - 4);
  CHECK_INT(41, 12 + 34 - 5);
  CHECK_INT(47, 5 + 6 * 7);
  CHECK_INT(15, 5 * (9 - 6));
  CHECK_INT(4, (3 + 5) / 2);
}

static void test02_01(void) {
  int a = 2, b = 2;
  CHECK_INT(4, a + b);
  return;
}
static void test02_02(void) {
  int a = 3;
  int b = 5 * 6 - 8;
  CHECK_INT(14, a + b / 2);
  return;
}
static void test02_03(void) {
  int a, b;
  b = (a = 5);
  CHECK_INT(10, a + b);
  return;
}
static void test02(void) {
  test02_01();
  test02_02();
  test02_03();
  CHECK_INT(1, 10 + 2 == 3 * 4);
  CHECK_INT(10, 10 + (2 != 1 * 2));
  CHECK_INT(12, 10 + ((2 != 1) * 2));
}

static void test03_01(void) {
  int c;
  int a = 3;
  int b = 5;
  CHECK_INT(0, c = a == b);
}
static void test03_02(void) {
  int c;
  int a = 3;
  int b = 5;
  CHECK_INT(0, c = a == b);
}
static void test03_03(void) {
  int a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y,
      z;
  a = b = c = d = e = f = g = h = i = j = k = l = m = n = o = p = q = r = s =
      t = u = v = w = x = y = z = 1;
  CHECK_INT(a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q +
                r + s + t + u + v + w + x + y + z,
            26);
}
static void test03_04(void) {
  (void)1;
  (void)2;
  (void)3;
  (void)4;
  (void)5;
  (void)6;
  (void)7;
  (void)8;
  (void)9;
  (void)10;
  (void)11;
  (void)12;
  (void)13;
  (void)14;
  (void)15;
  (void)16;
  (void)17;
  (void)18;
  (void)19;
  (void)20;
  (void)21;
  (void)22;
  (void)23;
  (void)24;
  (void)25;
  (void)26;
  (void)27;
  (void)28;
  (void)29;
  (void)30;
  (void)31;
  (void)32;
  (void)33;
  (void)34;
  (void)35;
  (void)36;
  (void)37;
  (void)38;
  (void)39;
  (void)40;
  (void)41;
  (void)42;
  (void)43;
  (void)44;
  (void)45;
  (void)46;
  (void)47;
  (void)48;
  (void)49;
  (void)50;
  (void)51;
  (void)52;
  (void)53;
  (void)54;
  (void)55;
  (void)56;
  (void)57;
  (void)58;
  (void)59;
  (void)60;
  (void)61;
  (void)62;
  (void)63;
  (void)64;
  (void)65;
  (void)66;
  (void)67;
  (void)68;
  (void)69;
  (void)70;
  (void)71;
  (void)72;
  (void)73;
  (void)74;
  (void)75;
  (void)76;
  (void)77;
  (void)78;
  (void)79;
  (void)80;
  (void)81;
  (void)82;
  (void)83;
  (void)84;
  (void)85;
  (void)86;
  (void)87;
  (void)88;
  (void)89;
  (void)90;
  (void)91;
  (void)92;
  (void)93;
  (void)94;
  (void)95;
  (void)96;
  (void)97;
  (void)98;
  (void)99;
  (void)100;
  (void)101;
  (void)102;
  (void)103;
  (void)104;
  (void)105;
  (void)106;
  (void)107;
  (void)108;
  (void)109;
  (void)110;
  (void)111;
  (void)112;
  (void)113;
  (void)114;
  (void)115;
  (void)116;
  (void)117;
  (void)118;
  (void)119;
  CHECK_INT(120, 120);
}
static void test03_05(void) {
  int a = 1, c = 2, d = 3;
  CHECK_INT(6, a + c + d);
}
static void test03_06(void) {
  int a = 1, ab = 2, abc = 3;
  CHECK_INT(6, a + ab + abc);
}
static void test03_07(void) {
  int _ = 123, a_ = 1;
  CHECK_INT(124, _ + a_);
}
static void test03(void) {
  test03_01();
  test03_02();
  test03_03();
  test03_04();
  test03_05();
  test03_06();
  test03_07();
}

static void test04(void) {
  int a = 0;
  CHECK_INT(8, 1 << 2 << 1);
  CHECK_INT(2, 1 << 2 >> 1);
  CHECK_INT(0, 1 >> 3);
  CHECK_INT(-1, 0 - 1 >> 3);
  CHECK_INT(1, 3 < 5);
  CHECK_INT(0, 3 > 5);
  CHECK_INT(1, 3 <= 5);
  CHECK_INT(0, 3 >= 5);
  CHECK_INT(0, 3 < 3);
  CHECK_INT(0, 3 > 3);
  CHECK_INT(1, 3 <= 3);
  CHECK_INT(1, 3 >= 3);
  CHECK_INT(1, 5 & 3);
  CHECK_INT(6, 5 ^ 3);
  CHECK_INT(7, 5 | 3);
  CHECK_INT(1, 3 && 5);
  CHECK_INT(0, 3 && 0);
  CHECK_INT(0, 0 && (a = 1));
  CHECK_INT(0, a);
  CHECK_INT(1, 3 || 5);
  CHECK_INT(1, 3 || (a = 1));
  CHECK_INT(0, a);
  CHECK_INT(1, 0 || 1);
  CHECK_INT(0, 0 || 0);
  CHECK_INT(12, 1 ? 12 : 34);
  CHECK_INT(34, 0 ? 12 : 34);
  CHECK_INT(3, 8 % 5);
  CHECK_INT(-4, ~3);
  CHECK_INT(0, !1);
  CHECK_INT(0, !3);
  CHECK_INT(1, !0);
}

static void test05(void) {
  int a, b;

  a = +3;
  CHECK_INT(103, 100 + a);
  CHECK_INT(3, +a);

  a = -3;
  CHECK_INT(97, 100 + a);
  CHECK_INT(3, -a);

  a = 2;
  CHECK_INT(3, ++a);

  a = 2;
  b = ++a;
  CHECK_INT(33, 10 * a + b);

  a = 2;
  CHECK_INT(1, --a);

  a = 2;
  b = --a;
  CHECK_INT(11, 10 * a + b);

  a = 2;
  CHECK_INT(2, a++);
  CHECK_INT(3, a);

  a = 2;
  b = a++;
  CHECK_INT(32, 10 * a + b);

  a = 2;
  CHECK_INT(2, a--);
  CHECK_INT(1, a);

  a = 2;
  b = a--;
  CHECK_INT(12, 10 * a + b);

  a = 0xff0;
  CHECK_INT(0x00f, 0xfff & (~a));
}

static void test06(void) {
  (void)1;
  ;
  (void)2;
  ;
  ;
  ;
  CHECK_INT(3, 3);
}

static void test07(void) {
  int a, b, c, s;

  a = 1;
  b = 1;
  CHECK_INT(8, a + b + 6);

  a = b = 1;
  if (1) {
    a = 5;
    b = 3;
  }
  CHECK_INT(8, a + b);

  a = b = 1;
  if (0) {
    a = 5;
    b = 3;
  }
  CHECK_INT(2, a + b);

  a = b = 1;
  if (1) {
    a = 5;
    b = 3;
  } else {
    a = 3;
    b = 2;
  }
  CHECK_INT(8, a + b);

  a = b = 1;
  if (0) {
    a = 5;
    b = 3;
  } else {
    a = 3;
    b = 2;
  }
  CHECK_INT(5, a + b);

  a = b = 1;
  if (1)
    a = 5;
  b = 3;
  CHECK_INT(8, a + b);

  a = b = 1;
  if (0)
    a = 5;
  b = 3;
  CHECK_INT(4, a + b);

  a = b = 1;
  if (1)
    a = 5;
  else
    a = 3;
  b = 2;
  CHECK_INT(7, a + b);

  a = b = 1;
  if (0)
    a = 5;
  else
    a = 3;
  b = 2;
  CHECK_INT(5, a + b);

  a = 0;
  while (a < 5) {
    a++;
  }
  CHECK_INT(5, a);

  a = b = 0;
  while (a < 5) {
    b = b + 2;
    a++;
  }
  CHECK_INT(10, b);

  do
    a = 10;
  while (0);
  CHECK_INT(10, a);

  a = 0;
  do
    a++;
  while (a < 10);
  CHECK_INT(10, a);

  s = 0;
  for (a = 1; a <= 10; a++)
    s = s + a;
  CHECK_INT(55, s);

  a = 1;
  s = 0;
  for (; a <= 10; a++)
    s = s + a;
  CHECK_INT(55, s);

  a = 1;
  s = 0;
  for (; a <= 10;) {
    s = s + a;
    a++;
  }
  CHECK_INT(55, s);

  a = 0;
  s = 0;
  while (1) {
    a++;
    if (a >= 10)
      break;
    s = s + a;
  }
  CHECK_INT(45, s);

  a = 0;
  s = 0;
  do {
    a++;
    if (a >= 10)
      break;
    s = s + a;
  } while (1);
  CHECK_INT(45, s);

  a = 0;
  s = 0;
  for (;;) {
    a++;
    if (a >= 10)
      break;
    s = s + a;
  }
  CHECK_INT(45, s);

  a = 0;
  s = 0;
  while (a < 10) {
    a++;
    if (a % 2)
      continue;
    s = s + a;
  }
  CHECK_INT(30, s);

  a = 0;
  s = 0;
  do {
    a++;
    if (a % 2)
      continue;
    s = s + a;
  } while (a < 10);
  CHECK_INT(30, s);

  a = 0;
  s = 0;
  for (a = 1; a <= 10; a++) {
    if (a % 2)
      continue;
    s = s + a;
  }
  CHECK_INT(30, s);
}

static void test08(void) {
  int a, *p;

  a = 4;
  a *= 3;
  CHECK_INT(12, a);

  a = 5;
  a /= 3;
  CHECK_INT(1, a);

  a = 5;
  a %= 3;
  CHECK_INT(2, a);

  a = 4;
  a += 3;
  CHECK_INT(7, a);

  a = 4;
  a -= 3;
  CHECK_INT(1, a);

  a = 4;
  a <<= 3;
  CHECK_INT(32, a);

  a = 32;
  a >>= 3;
  CHECK_INT(4, a);

  a = 5;
  a &= 3;
  CHECK_INT(1, a);

  a = 5;
  a |= 3;
  CHECK_INT(7, a);

  a = 5;
  a ^= 3;
  CHECK_INT(6, a);

  a = 5;
  p = &a;
  *p += 3;
  CHECK_INT(8, a);
}

static int test09_01(int n) {
  switch (n) {
  case 0:
    return 0;
  case 3:
    return 3;
  default:
    return 255;
  }
  abort();
}
static void test09_02(void) {
  int c = 0;
  for (int i = 0; i < 5; i++) {
    switch (i) {
    case 3:
      c += 1;
      if (i != 3)
        abort();
      break;
    case 0:
    case 1:
      c += 1;
      if (i != 0 && i != 1)
        abort();
      break;
    case 5:
      abort();
    default:
      c += 1;
      if (i != 2 && i != 4)
        abort();
      break;
    }
  }
  CHECK_INT(5, c);
}
static void test09_03(void) {
  int i = 3, s = 0;
  goto INIT;
  for (i = 0; i < 5; i++) {
  INIT:
    s += i;
  }
  CHECK_INT(7, s);
}
static void test09_04(void) {
  int x = 5;
  switch (11)
  case 0:
    abort();
  CHECK_INT(5, x);
}
static void test09(void) {
  CHECK_INT(3, test09_01(3));
  CHECK_INT(0, test09_01(0));
  CHECK_INT(255, test09_01(8));
  test09_02();
  test09_03();
  test09_04();
}

static void test10(void) {
  CHECK_INT(255, 0xff);
  CHECK_INT(16, 0x10);
  CHECK_INT(255, 0377);
  CHECK_INT(8, 010);
  CHECK_INT(255, 255);
  CHECK_INT(10, 10);
  CHECK_INT(97, 'a');
  CHECK_INT(10, '\n');
  CHECK_INT(10, '\xa');
  CHECK_INT(10, '\x0a');
  CHECK_INT(10, '\x00a');
  CHECK_INT(1, '\1');
  CHECK_INT(8, '\10');
  CHECK_INT(8, '\010');
}

static int test11_01(void) {
  if (1)
    return 3;
  abort();
}
static void test11(void) { CHECK_INT(3, test11_01()); }

static int ret3() { return 3; }
static int ret8() { return 8; }
static int add_38(void) { return ret3() + ret8(); }
static int to_double(int n) { return 2 * n; }
static int add(int a, int b) { return a + b; }
static int sub(int a, int b) { return a - b; }
static int fib(int n) {
  return (n <= 0) ? 0 : (n == 1) ? 1 : fib(n - 2) + fib(n - 1);
}
static void test12(void) {
  int a, b;
  a = add_38();
  b = ret3();

  CHECK_INT(113, a * 10 + b);
  CHECK_INT(20, to_double(3) + to_double(7));
  CHECK_INT(10, add(3, 5) + sub(5, 3));
  CHECK_INT(0, fib(0));
  CHECK_INT(1, fib(1));
  CHECK_INT(1, fib(2));
  CHECK_INT(2, fib(3));
  CHECK_INT(3, fib(4));
  CHECK_INT(5, fib(5));
  CHECK_INT(8, fib(6));
  CHECK_INT(13, fib(7));
  CHECK_INT(21, fib(8));
  CHECK_INT(34, fib(9));
  CHECK_INT(55, fib(10));
  CHECK_INT(89, fib(11));
  CHECK_INT(144, fib(12));
}

static int add6(int a, int b, int c, int d, int e, int f) {
  return a + b + c + d + e + f;
}
static int digits6(int a, int b, int c, int d, int e, int f) {
  return ((((((a * 10) + b) * 10 + c) * 10 + d) * 10 + e) * 10 + f);
}
static int add10(int a, int b, int c, int d, int e, int f, int g, int h, int i,
                 int j) {
  return a + b + c + d + e + f + g + h + i + j;
}
static int digits10(int a, int b, int c, int d, int e, int f, int g, int h,
                    int i, int j) {
  return (
      (((((((((a * 10) + b) * 10 + c) * 10 + d) * 10 + e) * 10 + f) * 10 + g) *
            10 +
        h) *
           10 +
       i) *
          10 +
      j);
}

static void test13(void) {
  CHECK_INT(123456, digits6(1, 2, 3, 4, 5, 6));
  CHECK_INT(21, add6(1, 2, 3, 4, 5, 6));
  CHECK_INT(1234567890, digits10(1, 2, 3, 4, 5, 6, 7, 8, 9, 0));
  CHECK_INT(45, add10(1, 2, 3, 4, 5, 6, 7, 8, 9, 0));
}

static void test14(void) {
  int a, *b, **c, ***d;
  (void)a;
  (void)b;
  (void)c;
  (void)d;
}

static void test15(void) {
  int *p, *q, i;

  p = (int *)30;
  q = (int *)10;

  i = (int)(p + 3);
  CHECK_INT(42, i);

  i = (int)(3 + p);
  CHECK_INT(42, i);

  p += 3;
  CHECK_INT(42, (int)p);

  i = (int)(p - 3);
  CHECK_INT(30, i);

  i = p - q;
  CHECK_INT(8, i);

  p = (int *)8;
  p++;
  CHECK_INT(12, (int)p);

  ++p;
  CHECK_INT(16, (int)p);

  p--;
  CHECK_INT(12, (int)p);

  --p;
  CHECK_INT(8, (int)p);

  p = (int *)10;
  i = 5;
  i += (int)p;
  CHECK_INT(15, i);
}

static void test16(void) {
  int a[3];

  *a = 1;
  *(a + 1) = 2;
  int *p;
  p = a;
  CHECK_INT(3, *p + *(p + 1));

  a[0] = 5;
  a[1] = 4;
  a[2] = 3;
  CHECK_INT(12, a[0] + a[1] + a[2]);
  CHECK_INT(12, 0 [a] + 1 [a] + 2 [a]);

  CHECK_INT(2, &a[2] - &a[0]);
}

static int *test17_01(int *x) { return x + 1; }
static void test17_02(void) {}
static void *test17_03(void *p) { return p; }
static void test17(void) {
  int a[4];

  a[0] = 1;
  a[1] = 2;
  a[2] = 4;
  a[3] = 8;
  CHECK_INT(8, test17_01(a)[2]);

  test17_02();

  void *p, *q;
  p = (int *)3;
  q = test17_03(p);
  CHECK_INT(1, p == q);
}

static int test18_g;
static int test18_h[3];
static void test18_init(void) {
  test18_g = 3;
  test18_h[0] = 1;
  test18_h[1] = 2;
  test18_h[2] = 3;
}

static int test18_add(int n) {
  test18_g += n;
  test18_h[0] *= test18_h[0];
  test18_h[1] *= test18_h[1];
  test18_h[2] *= test18_h[2];

  return test18_g;
}
static void test18(void) {
  test18_g = 100;
  CHECK_INT(test18_g, 100);

  test18_init();
  (void)test18_add(7);
  CHECK_INT(10, test18_g);
  CHECK_INT(1, test18_h[0]);
  CHECK_INT(4, test18_h[1]);
  CHECK_INT(9, test18_h[2]);
}

static void test19(void) {
  char x[4];
  x[0] = 'a';
  x[1] = 'b';
  x[2] = 'c';
  x[3] = '\0';
  TEST_PRINTF("%s", x);

  x[0] = -1;
  x[1] = 2;
  int y;
  y = 4;
  CHECK_INT(3, x[0] + y);

  CHECK_INT(1, &x[1] - &x[0]);
}

static void test20(void) {
  TEST_PRINTF("    hogehoge");
  TEST_PRINTF("    \n\n\r\r");
  TEST_PRINTF("    ほげほげ");

  TEST_PRINTF("%s %d", "abc", 456);
}

static void test21(void) {
  int a[3][5];

  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 5; j++) {
      a[i][j] = i * j;
    }
  }

  for (int i = 0; i < 3; i++) {
    char buf[1024];
    int idx = 0;
    for (int j = 0; j < 5; j++) {
      idx += sprintf(&buf[idx], " %2d %p (%2zu)", a[i][j], &a[i][j],
                     &a[i][j] - &a[0][0]);
    }
    TEST_PRINTF("%2d: %s", i, buf);
  }

  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 5; j++) {
      CHECK_INT(10000 * i + 1000 * j + i * j, 10000 * i + 1000 * j + a[i][j]);
    }
  }
}

static int test22_g;
static void test22_func(int n) { test22_g = n; }
static void test22(void) {
  int(*a)[3], b[3], (**c)[3];

  c = &a;
  a = &b;
  (*a)[0] = 3;
  (**c)[1] = 4;
  (*a)[2] = 5;

  CHECK_INT(3, b[0]);
  CHECK_INT(4, b[1]);
  CHECK_INT(5, b[2]);

  test22_g = 0;
  void (*f)(int n);
  f = &test22_func;
  f(3);
  CHECK_INT(3, test22_g);
}

static void test23_fun(int a, int b) {
  int c, d, e;
  c = 3;
  d = 4;
  e = 5;
  CHECK_INT(1, a);
  CHECK_INT(2, b);
  CHECK_INT(3, c);
  CHECK_INT(4, d);
  CHECK_INT(5, e);

  {
    int c, f;
    c = 6;
    e = 7;
    f = 8;
    CHECK_INT(1, a);
    CHECK_INT(2, b);
    CHECK_INT(6, c);
    CHECK_INT(4, d);
    CHECK_INT(7, e);
    CHECK_INT(8, f);
  }

  CHECK_INT(1, a);
  CHECK_INT(2, b);
  CHECK_INT(3, c);
  CHECK_INT(4, d);
  CHECK_INT(7, e);
}
static void test23(void) { test23_fun(1, 2); }

static void test24(void) {
  char a = 64, b = a, c = a, d = a;
  CHECK_INT(256, a + b + c + d);

  CHECK_INT(256, (char)64 + (char)64 + (char)64 + (char)64);
  CHECK_INT(64, (char)320);
  CHECK_INT(256, (char)320 + (char)320 + (char)320 + (char)320);
}

static void test25(void) {
  // comment
  /* comment */
}

struct Test26 {
  int z;
};
static struct Test26 test26_x;
static struct { int y; } test26_y, test26_z;
static void test26(void) {
  struct Foo {
    int x, y;
  } s;

  s.x = 3;
  s.y = 5;

  CHECK_INT(3, s.x);
  CHECK_INT(5, s.y);
  CHECK_INT(8, s.x + s.y);

  struct {
    int z;
    int w;
  } t;
  t.z = t.w = 8;
  CHECK_INT(64, t.z * t.w);

  struct Foo s2;
  s2.x = 8;
  s2.y = 1;
  CHECK_INT(9, s2.x + s2.y);

  struct Bar {
    int x;
  };
  struct Bar u;
  u.x = 3;
  CHECK_INT(3, u.x);

  struct Foo *q;
  q = &s;
  CHECK_INT(8, q->x + q->y);
  CHECK_INT(1, &q->x == &s.x);

  struct P {
    struct P *x;
    int v;
  } p;
  p.x = &p;
  p.v = 123;
  CHECK_INT(123, p.x->x->x->x->v);

  struct Test26 k;
  k.z = 3;
  CHECK_INT(3, k.z);

  test26_x.z = 5;
  test26_y.y = 100;
  test26_z.y = 200;
  CHECK_INT(305, test26_x.z + test26_y.y + test26_z.y);
}

typedef int test27_type;
static void test27(void) {
  typedef int x;
  typedef int y(int x);
  typedef int z[3];

  x a;
  y *b;
  z c[5];

  a = 3;
  b = 0;
  c[0][0] = 1;
}

static int test28_foo(int, int, int);
static int test28_foo(int a, int b, int c) { return a + b + c; }
static void test28(void) { CHECK_INT(12, test28_foo(3, 4, 5)); }

static void test29(void) {
  int a[7];
  struct Foo {
    int a[9];
  };

  CHECK_INT(4, sizeof(int));
  CHECK_INT(4, sizeof 3);
  CHECK_INT(4, sizeof(3 + 5));
  CHECK_INT(28, sizeof a);
  CHECK_INT(36, sizeof(struct Foo));
}

static void test30(void) {
  union {
    int a;
    int b;
  } u;

  u.a = 1;
  CHECK_INT(1, u.a);

  u.b = 2;

  CHECK_INT(2, u.a);
  CHECK_INT(2, u.b);
}

static int test31_x = 3;
static void test31(void) { CHECK_INT(3, test31_x); }

static long test32_x = 8;
static long *test32_p = &test32_x;
static char test32_fun_char(void) { return 0; }
static short test32_fun_short(void) { return 0; }
static int test32_fun_int(void) { return 0; }
static long test32_fun_long(void) { return 0; }
static int *test32_fun_ptr(void) { return 0; }
static void test32(void) {
  long x = 3;
  int y = 3;

  CHECK_INT(8, test32_x);
  *test32_p = 1;
  CHECK_INT(1, test32_x);

  CHECK_INT(8, sizeof(x));
  CHECK_INT(8, sizeof(x + y));
  CHECK_INT(8, sizeof(y + x));
  CHECK_INT(4, sizeof(y));

  CHECK_INT(8, sizeof &test32_fun_char);
  CHECK_INT(1, sizeof test32_fun_char());
  CHECK_INT(0, test32_fun_char());
  CHECK_INT(8, sizeof &test32_fun_short);
  CHECK_INT(2, sizeof test32_fun_short());
  CHECK_INT(0, test32_fun_short());
  CHECK_INT(8, sizeof &test32_fun_int);
  CHECK_INT(4, sizeof test32_fun_int());
  CHECK_INT(0, test32_fun_int());
  CHECK_INT(8, sizeof &test32_fun_long);
  CHECK_INT(8, sizeof test32_fun_long());
  CHECK_INT(0, test32_fun_long());
  CHECK_INT(8, sizeof &test32_fun_ptr);
  CHECK_INT(8, sizeof test32_fun_ptr());
  CHECK_INT(0, (int)test32_fun_ptr());
}

static void test33(void) {
  struct {
    struct {
      int a;
      int b;
    };
  } s;
  s.a = 4;
  s.b = 8;
  CHECK_INT(12, s.a + s.b);

  struct {
    union {
      struct {
        int x;
      };
    };
  } p;

  p.x = 3;
  CHECK_INT(3, p.x);
}

static void test34(void) {
  char ca = 3, cb = 5;
  CHECK_INT(8, ca + cb);
  CHECK_INT(-2, ca - cb);
  CHECK_INT(15, ca * cb);
  CHECK_INT(0, ca / cb);
  CHECK_INT(3, ca % cb);
  CHECK_INT(0, ca == cb);
  CHECK_INT(1, ca != cb);
  CHECK_INT(1, ca < cb);
  CHECK_INT(0, ca > cb);
  CHECK_INT(1, ca <= cb);
  CHECK_INT(0, ca >= cb);
  CHECK_INT(96, ca << cb);
  CHECK_INT(0, ca >> cb);
  CHECK_INT(1, ca & cb);
  CHECK_INT(7, ca | cb);
  CHECK_INT(5, (ca, cb));

  short sa = 3, sb = 5;
  CHECK_INT(8, sa + sb);
  CHECK_INT(-2, sa - sb);
  CHECK_INT(15, sa * sb);
  CHECK_INT(0, sa / sb);
  CHECK_INT(3, sa % sb);
  CHECK_INT(0, sa == sb);
  CHECK_INT(1, sa != sb);
  CHECK_INT(1, sa < sb);
  CHECK_INT(0, sa > sb);
  CHECK_INT(1, sa <= sb);
  CHECK_INT(0, sa >= sb);
  CHECK_INT(96, sa << sb);
  CHECK_INT(0, sa >> sb);
  CHECK_INT(1, sa & sb);
  CHECK_INT(7, sa | sb);
  CHECK_INT(5, (sa, sb));

  int ia = 3, ib = 5;
  CHECK_INT(8, ia + ib);
  CHECK_INT(-2, ia - ib);
  CHECK_INT(15, ia * ib);
  CHECK_INT(0, ia / ib);
  CHECK_INT(3, ia % ib);
  CHECK_INT(0, ia == ib);
  CHECK_INT(1, ia != ib);
  CHECK_INT(1, ia < ib);
  CHECK_INT(0, ia > ib);
  CHECK_INT(1, ia <= ib);
  CHECK_INT(0, ia >= ib);
  CHECK_INT(96, ia << ib);
  CHECK_INT(0, ia >> ib);
  CHECK_INT(1, ia & ib);
  CHECK_INT(7, ia | ib);
  CHECK_INT(5, (ia, ib));

  long la = 3, lb = 5;
  CHECK_INT(8, la + lb);
  CHECK_INT(-2, la - lb);
  CHECK_INT(15, la * lb);
  CHECK_INT(0, la / lb);
  CHECK_INT(3, la % lb);
  CHECK_INT(0, la == lb);
  CHECK_INT(1, la != lb);
  CHECK_INT(1, la < lb);
  CHECK_INT(0, la > lb);
  CHECK_INT(1, la <= lb);
  CHECK_INT(0, la >= lb);
  CHECK_INT(96, la << lb);
  CHECK_INT(0, la >> lb);
  CHECK_INT(1, la & lb);
  CHECK_INT(7, la | lb);
  CHECK_INT(5, (la, lb));
}

static void test35(void) {
  char cx = 0;
  cx = cx + 1;
  CHECK_INT(1, cx);

  short sx = 0;
  sx = sx + 1;
  CHECK_INT(1, sx);
}

static int test36_f(int test36_a), test36_g(int test36_a), test36_a = 100;
static void test36(void) {
  CHECK_INT(0, test36_f(1) - test36_g(1));
  CHECK_INT(100, test36_a);
}
static int test36_f(int test36_a) { return test36_a; }
static int test36_g(int test36_a) { return test36_a; }

static struct {
  int a;
  int b;
  char c;
  int d;
  short e;
  void *f;
} test37_s = {1, 2, 3, 4, 5, (void *)6};
static struct {
  int a;
  int b;
  char c;
  int d;
  short e;
  void *f;
} test37_t = {{{1}}, {2}, {3}, {{{4}}}, {5}, (void *)6};
static void test37(void) {
  CHECK_INT(1, test37_s.a);
  CHECK_INT(2, test37_s.b);
  CHECK_INT(3, test37_s.c);
  CHECK_INT(4, test37_s.d);
  CHECK_INT(5, test37_s.e);
  CHECK_INT(6, (int)test37_s.f);

  CHECK_INT(1, test37_t.a);
  CHECK_INT(2, test37_t.b);
  CHECK_INT(3, test37_t.c);
  CHECK_INT(4, test37_t.d);
  CHECK_INT(5, test37_t.e);
  CHECK_INT(6, (int)test37_t.f);

  struct S {
    int a;
    int b;
    char c;
    int d;
    short e;
    void *f;
  } s = {11, 12, 13, 14, 15, (void *)16};
  CHECK_INT(11, s.a);
  CHECK_INT(12, s.b);
  CHECK_INT(13, s.c);
  CHECK_INT(14, s.d);
  CHECK_INT(15, s.e);
  CHECK_INT(16, (int)s.f);

  struct T {
    int a;
    int b;
    char c;
    int d;
    short e;
    void *f;
  } t = {{{11}}, {12}, {13}, {{{14}}}, {15}, (void *)16};
  CHECK_INT(11, t.a);
  CHECK_INT(12, t.b);
  CHECK_INT(13, t.c);
  CHECK_INT(14, t.d);
  CHECK_INT(15, t.e);
  CHECK_INT(16, (int)t.f);
}

static union {
  int a;
  int b;
  char c;
  int d;
  short e;
} test38_u = {1};
static void test38(void) {
  CHECK_INT(1, test38_u.a);
  CHECK_INT(1, test38_u.b);
  CHECK_INT(1, test38_u.c);
  CHECK_INT(1, test38_u.d);
  CHECK_INT(1, test38_u.e);

  union U {
    int a;
    int b;
    char c;
    int d;
    short e;
  } u = {11};
  CHECK_INT(11, u.a);
  CHECK_INT(11, u.b);
  CHECK_INT(11, u.c);
  CHECK_INT(11, u.d);
  CHECK_INT(11, u.e);
}

static struct {
  int a;
  int b;
  union {
    int c;
    int d;
  };
  struct {
    int x;
    int y;
  } e;
} test39_s = {1, 2, {3}, {4, 5}};
static struct {
  int a;
  int b;
  union {
    int c;
    int d;
  };
  struct {
    int x;
    int y;
  } e;
} test39_t = {21, 22, 23, 24, 25};

static void test39(void) {
  CHECK_INT(1, test39_s.a);
  CHECK_INT(2, test39_s.b);
  CHECK_INT(3, test39_s.c);
  CHECK_INT(3, test39_s.d);
  CHECK_INT(4, test39_s.e.x);
  CHECK_INT(5, test39_s.e.y);

  CHECK_INT(21, test39_t.a);
  CHECK_INT(22, test39_t.b);
  CHECK_INT(23, test39_t.c);
  CHECK_INT(23, test39_t.d);
  CHECK_INT(24, test39_t.e.x);
  CHECK_INT(25, test39_t.e.y);

  struct {
    int a;
    int b;
    union {
      int c;
      int d;
    };
    struct {
      int x;
      int y;
    } e;
  } s = {11, 12, {13}, {14, 15}};
  CHECK_INT(11, s.a);
  CHECK_INT(12, s.b);
  CHECK_INT(13, s.c);
  CHECK_INT(13, s.d);
  CHECK_INT(14, s.e.x);
  CHECK_INT(15, s.e.y);

  struct {
    int a;
    int b;
    union {
      int c;
      int d;
    };
    struct {
      int x;
      int y;
    } e;
  } t = {31, 32, 33, 34, 35};
  CHECK_INT(31, t.a);
  CHECK_INT(32, t.b);
  CHECK_INT(33, t.c);
  CHECK_INT(33, t.d);
  CHECK_INT(34, t.e.x);
  CHECK_INT(35, t.e.y);
}

static void test40(void) {
  union {
    struct {
      int a;
      int b;
    };
    int c;
    int d;
  } s = {{1, 2}};
  CHECK_INT(1, s.a);
  CHECK_INT(2, s.b);
  CHECK_INT(1, s.c);
  CHECK_INT(1, s.d);
}

static long test41_f(void) { return 111; }
static void test41(void) {
  CHECK_INT(111, test41_f());
  CHECK_INT(8, sizeof(test41_f()));

  long (*ptr)(void) = test41_f;
  CHECK_INT(111, ptr());
  CHECK_INT(8, sizeof(ptr()));
}

extern int test42_n;
static int test42_l;
extern int test42_f(void);
static int test42_h(void);
static void test42(void) {
  extern int test42_m;
  extern int test42_g(void);

  CHECK_INT(8, test42_n);
  CHECK_INT(32, test42_f());
  CHECK_INT(10, test42_m);
  CHECK_INT(64, test42_g());
  CHECK_INT(12, test42_l);
  CHECK_INT(128, test42_h());
}
int test42_n;
int test42_n = 8;
int test42_n;
int test42_m;
int test42_m = 10;
int test42_m;
static int test42_l;
static int test42_l = 12;
static int test42_l;
int test42_f(void) { return 32; }
int test42_g(void) { return 64; }
static int test42_h(void) { return 128; }

static void test43(void) {
  char c1 = 0;
  short int s1 = 1;
  short s2 = 2;
  int i1 = 3;
  long int l1 = 4;
  long l2 = 5;
  long long int ll1 = 6;
  long long ll2 = 7;

  CHECK_INT(0, c1);
  CHECK_INT(1, s1);
  CHECK_INT(2, s2);
  CHECK_INT(3, i1);
  CHECK_INT(4, l1);
  CHECK_INT(5, l2);
  CHECK_INT(6, ll1);
  CHECK_INT(7, ll2);
}

static void test44(void) {
  signed char sc1 = 0;
  signed short int ss1 = 1;
  signed short ss2 = 2;
  signed int si1 = 3;
  signed long int sl1 = 4;
  signed long sl2 = 5;
  signed long long int sll1 = 6;
  signed long long sll2 = 7;
  signed s1 = 8;

  CHECK_INT(0, sc1);
  CHECK_INT(1, ss1);
  CHECK_INT(2, ss2);
  CHECK_INT(3, si1);
  CHECK_INT(4, sl1);
  CHECK_INT(5, sl2);
  CHECK_INT(6, sll1);
  CHECK_INT(7, sll2);
  CHECK_INT(8, s1);
}

static int test45_n = 8;
static int test45_f(void) {
  static int x = 0;
  return x++;
}
static int test45_g(void) {
  int s = 0;
  static int x = 0;
  {
    static int x = 10;
    s += x;
    x += 10;
  }
  s += x;
  x++;
  return s;
}
static void test45(void) {
  static int x = 0;
  CHECK_INT(8, test45_n);
  CHECK_INT(0, test45_f());
  CHECK_INT(1, test45_f());
  CHECK_INT(2, test45_f());
  CHECK_INT(0, x);
  x = 100;
  CHECK_INT(3, test45_f());

  CHECK_INT(10, test45_g());
  CHECK_INT(21, test45_g());
  CHECK_INT(32, test45_g());
}

static struct {
  int a;
  int b;
} test46_s = {.b = 123, .a = 456};
static struct {
  int a;
  int b;
  struct {
    int a;
    int b;
  } c;
  int d;
  int e;
} test46_t = {1, .c.a = 3, 4, 5};
static struct {
  int a;
  int b;
  struct {
    int c;
    int d;
  };
  struct {
    int e;
    int f;
  };
  int g;
  int h;
} test46_u = {1, .c = 3, .f = 5};
static void test46(void) {
  CHECK_INT(456, test46_s.a);
  CHECK_INT(123, test46_s.b);

  CHECK_INT(1, test46_t.a);
  CHECK_INT(0, test46_t.b);
  CHECK_INT(3, test46_t.c.a);
  CHECK_INT(4, test46_t.c.b);
  CHECK_INT(5, test46_t.d);
  CHECK_INT(0, test46_t.e);

  CHECK_INT(1, test46_u.a);
  CHECK_INT(0, test46_u.b);
  CHECK_INT(3, test46_u.c);
  CHECK_INT(0, test46_u.d);
  CHECK_INT(0, test46_u.e);
  CHECK_INT(5, test46_u.f);
  CHECK_INT(0, test46_u.g);
  CHECK_INT(0, test46_u.h);

  struct {
    int a;
    int b;
  } s = {.b = 123, .a = 456};
  struct {
    int a;
    int b;
    struct {
      int a;
      int b;
    } c;
    int d;
    int e;
  } t = {1, .c.a = 3, 4, 5};
  static struct {
    int a;
    int b;
    struct {
      int c;
      int d;
    };
    struct {
      int e;
      int f;
    };
    int g;
    int h;
  } u = {1, .c = 3, .f = 5};
  CHECK_INT(456, s.a);
  CHECK_INT(123, s.b);

  CHECK_INT(1, t.a);
  CHECK_INT(0, t.b);
  CHECK_INT(3, t.c.a);
  CHECK_INT(4, t.c.b);
  CHECK_INT(5, t.d);
  CHECK_INT(0, t.e);

  CHECK_INT(1, u.a);
  CHECK_INT(0, u.b);
  CHECK_INT(3, u.c);
  CHECK_INT(0, u.d);
  CHECK_INT(0, u.e);
  CHECK_INT(5, u.f);
  CHECK_INT(0, u.g);
  CHECK_INT(0, u.h);

  static struct {
    int a;
    int b;
  } s_s = {.b = 123, .a = 456};
  static struct {
    int a;
    int b;
    struct {
      int a;
      int b;
    } c;
    int d;
    int e;
  } s_t = {1, .c.a = 3, 4, 5};

  static struct {
    int a;
    int b;
    struct {
      int c;
      int d;
    };
    struct {
      int e;
      int f;
    };
    int g;
    int h;
  } s_u = {1, .c = 3, .f = 5};

  CHECK_INT(456, s_s.a);
  CHECK_INT(123, s_s.b);

  CHECK_INT(1, s_t.a);
  CHECK_INT(0, s_t.b);
  CHECK_INT(3, s_t.c.a);
  CHECK_INT(4, s_t.c.b);
  CHECK_INT(5, s_t.d);
  CHECK_INT(0, s_t.e);

  CHECK_INT(1, s_u.a);
  CHECK_INT(0, s_u.b);
  CHECK_INT(3, s_u.c);
  CHECK_INT(0, s_u.d);
  CHECK_INT(0, s_u.e);
  CHECK_INT(5, s_u.f);
  CHECK_INT(0, s_u.g);
  CHECK_INT(0, s_u.h);
}

static union {
  struct {
    int a;
    int b;
  } x;
} test47_s = {1, 2};
static union {
  struct {
    int a;
    int b;
  } x;
  struct {
    long a;
    long b;
    long c;
  } y;
} test47_t = {.y.b = 200, .y.a = 100};
static void test47(void) {
  CHECK_INT(1, test47_s.x.a);
  CHECK_INT(2, test47_s.x.b);
  CHECK_INT(100, test47_t.y.a);
  CHECK_INT(200, test47_t.y.b);
  CHECK_INT(0, test47_t.y.c);

  union {
    struct {
      int a;
      int b;
    } x;
  } s = {1, 2};
  union {
    struct {
      int a;
      int b;
    } x;
    struct {
      long a;
      long b;
      long c;
    } y;
  } t = {.y.b = 200, .y.a = 100};

  CHECK_INT(1, s.x.a);
  CHECK_INT(2, s.x.b);
  CHECK_INT(100, t.y.a);
  CHECK_INT(200, t.y.b);
  CHECK_INT(0, t.y.c);

  static union {
    struct {
      int a;
      int b;
    } x;
  } s_s = {1, 2};
  static union {
    struct {
      int a;
      int b;
    } x;
    struct {
      long a;
      long b;
      long c;
    } y;
  } s_t = {.y.b = 200, .y.a = 100};

  CHECK_INT(1, s_s.x.a);
  CHECK_INT(2, s_s.x.b);
  CHECK_INT(100, s_t.y.a);
  CHECK_INT(200, s_t.y.b);
  CHECK_INT(0, s_t.y.c);
}

static int test48_array[4] = {1, 2, 3};
static struct {
  int a;
  int b[4];
  int c;
} test48_s = {1, 2, 3, .c = 4};
static struct {
  int a;
  int b[4];
  int c;
} test48_t = {1, 2, 3, 4, 5, 6};
static struct {
  int a;
  int b[4];
  int c;
} test48_u = {1, {2, 3, 4, 5}, 6};
static void test48(void) {
  CHECK_INT(1, test48_array[0]);
  CHECK_INT(2, test48_array[1]);
  CHECK_INT(3, test48_array[2]);
  CHECK_INT(0, test48_array[3]);

  CHECK_INT(1, test48_s.a);
  CHECK_INT(2, test48_s.b[0]);
  CHECK_INT(3, test48_s.b[1]);
  CHECK_INT(0, test48_s.b[2]);
  CHECK_INT(0, test48_s.b[3]);
  CHECK_INT(4, test48_s.c);

  CHECK_INT(1, test48_t.a);
  CHECK_INT(2, test48_t.b[0]);
  CHECK_INT(3, test48_t.b[1]);
  CHECK_INT(4, test48_t.b[2]);
  CHECK_INT(5, test48_t.b[3]);
  CHECK_INT(6, test48_t.c);

  CHECK_INT(1, test48_u.a);
  CHECK_INT(2, test48_u.b[0]);
  CHECK_INT(3, test48_u.b[1]);
  CHECK_INT(4, test48_u.b[2]);
  CHECK_INT(5, test48_u.b[3]);
  CHECK_INT(6, test48_u.c);

  int array[4] = {1, 2, 3};
  struct {
    int a;
    int b[4];
    int c;
  } s = {1, 2, 3, .c = 4};
  struct {
    int a;
    int b[4];
    int c;
  } t = {1, 2, 3, 4, 5, 6};
  struct {
    int a;
    int b[4];
    int c;
  } u = {1, {2, 3, 4, 5}, 6};
  CHECK_INT(1, array[0]);
  CHECK_INT(2, array[1]);
  CHECK_INT(3, array[2]);
  CHECK_INT(0, array[3]);

  CHECK_INT(1, s.a);
  CHECK_INT(2, s.b[0]);
  CHECK_INT(3, s.b[1]);
  CHECK_INT(0, s.b[2]);
  CHECK_INT(0, s.b[3]);
  CHECK_INT(4, s.c);

  CHECK_INT(1, t.a);
  CHECK_INT(2, t.b[0]);
  CHECK_INT(3, t.b[1]);
  CHECK_INT(4, t.b[2]);
  CHECK_INT(5, t.b[3]);
  CHECK_INT(6, t.c);

  CHECK_INT(1, u.a);
  CHECK_INT(2, u.b[0]);
  CHECK_INT(3, u.b[1]);
  CHECK_INT(4, u.b[2]);
  CHECK_INT(5, u.b[3]);
  CHECK_INT(6, u.c);

  static int s_array[4] = {1, 2, 3};
  static struct {
    int a;
    int b[4];
    int c;
  } s_s = {1, 2, 3, .c = 4};
  static struct {
    int a;
    int b[4];
    int c;
  } s_t = {1, 2, 3, 4, 5, 6};
  static struct {
    int a;
    int b[4];
    int c;
  } s_u = {1, {2, 3, 4, 5}, 6};
  CHECK_INT(1, s_array[0]);
  CHECK_INT(2, s_array[1]);
  CHECK_INT(3, s_array[2]);
  CHECK_INT(0, s_array[3]);

  CHECK_INT(1, s_s.a);
  CHECK_INT(2, s_s.b[0]);
  CHECK_INT(3, s_s.b[1]);
  CHECK_INT(0, s_s.b[2]);
  CHECK_INT(0, s_s.b[3]);
  CHECK_INT(4, s_s.c);

  CHECK_INT(1, s_t.a);
  CHECK_INT(2, s_t.b[0]);
  CHECK_INT(3, s_t.b[1]);
  CHECK_INT(4, s_t.b[2]);
  CHECK_INT(5, s_t.b[3]);
  CHECK_INT(6, s_t.c);

  CHECK_INT(1, s_u.a);
  CHECK_INT(2, s_u.b[0]);
  CHECK_INT(3, s_u.b[1]);
  CHECK_INT(4, s_u.b[2]);
  CHECK_INT(5, s_u.b[3]);
  CHECK_INT(6, s_u.c);
}

static int test49_a0[] = {};
static int test49_a4[] = {1, 2, 3, 4};
static int test49_a6[6] = {1, 2, 3, 4};
static int test49_ax[] = {1, [3] = 8, 9};
static void test49(void) {
  CHECK_INT(0, sizeof(test49_a0));
  CHECK_INT(sizeof(int) * 4, sizeof(test49_a4));
  CHECK_INT(sizeof(int) * 6, sizeof(test49_a6));
  CHECK_INT(sizeof(int) * 5, sizeof(test49_ax));
  CHECK_INT(1, test49_a4[0]);
  CHECK_INT(2, test49_a4[1]);
  CHECK_INT(3, test49_a4[2]);
  CHECK_INT(4, test49_a4[3]);
  CHECK_INT(1, test49_a6[0]);
  CHECK_INT(2, test49_a6[1]);
  CHECK_INT(3, test49_a6[2]);
  CHECK_INT(4, test49_a6[3]);
  CHECK_INT(0, test49_a6[4]);
  CHECK_INT(0, test49_a6[5]);
  CHECK_INT(1, test49_ax[0]);
  CHECK_INT(0, test49_ax[1]);
  CHECK_INT(0, test49_ax[2]);
  CHECK_INT(8, test49_ax[3]);
  CHECK_INT(9, test49_ax[4]);

  int a0[] = {};
  int a4[] = {1, 2, 3, 4};
  int a6[6] = {1, 2, 3, 4};
  int ax[] = {1, [3] = 8, 9};
  CHECK_INT(0, sizeof(a0));
  CHECK_INT(sizeof(int) * 4, sizeof(a4));
  CHECK_INT(sizeof(int) * 6, sizeof(a6));
  CHECK_INT(sizeof(int) * 5, sizeof(ax));
  CHECK_INT(1, a4[0]);
  CHECK_INT(2, a4[1]);
  CHECK_INT(3, a4[2]);
  CHECK_INT(4, a4[3]);
  CHECK_INT(1, a6[0]);
  CHECK_INT(2, a6[1]);
  CHECK_INT(3, a6[2]);
  CHECK_INT(4, a6[3]);
  CHECK_INT(0, a6[4]);
  CHECK_INT(0, a6[5]);
  CHECK_INT(1, ax[0]);
  CHECK_INT(0, ax[1]);
  CHECK_INT(0, ax[2]);
  CHECK_INT(8, ax[3]);
  CHECK_INT(9, ax[4]);

  static int s_a0[] = {};
  static int s_a4[] = {1, 2, 3, 4};
  static int s_a6[6] = {1, 2, 3, 4};
  static int s_ax[] = {1, [3] = 8, 9};
  CHECK_INT(0, sizeof(s_a0));
  CHECK_INT(sizeof(int) * 4, sizeof(s_a4));
  CHECK_INT(1, s_a4[0]);
  CHECK_INT(2, s_a4[1]);
  CHECK_INT(3, s_a4[2]);
  CHECK_INT(4, s_a4[3]);
  CHECK_INT(1, s_a6[0]);
  CHECK_INT(2, s_a6[1]);
  CHECK_INT(3, s_a6[2]);
  CHECK_INT(4, s_a6[3]);
  CHECK_INT(0, s_a6[4]);
  CHECK_INT(0, s_a6[5]);
  CHECK_INT(1, s_ax[0]);
  CHECK_INT(0, s_ax[1]);
  CHECK_INT(0, s_ax[2]);
  CHECK_INT(8, s_ax[3]);
  CHECK_INT(9, s_ax[4]);
}

static void test50(void) {
  const char *s = "abc"
                  "def";

  CHECK_INT('a', s[0]);
  CHECK_INT('b', s[1]);
  CHECK_INT('c', s[2]);
  CHECK_INT('d', s[3]);
  CHECK_INT('e', s[4]);
  CHECK_INT('f', s[5]);
  CHECK_INT(1, s[0] == 'a');
  CHECK_INT(1, s[1] == 'b');
  CHECK_INT(1, s[2] == 'c');
  CHECK_INT(1, s[3] == 'd');
  CHECK_INT(1, s[4] == 'e');
  CHECK_INT(1, s[5] == 'f');
}

enum test51_enum {
  TEST51_X,
  TEST51_Y = 3,
  TEST51_Z,
  TEST51_W = 8,
};
static void test51(void) {
  CHECK_INT(0, TEST51_X);
  CHECK_INT(3, TEST51_Y);
  CHECK_INT(4, TEST51_Z);
  CHECK_INT(8, TEST51_W);

  enum test51_enum x = TEST51_Z;
  CHECK_INT(7, x + 3);

  enum { X1 = 8, Y1, Z1 } z = Y1;
  CHECK_INT(9, z);
}

static void test52(void) {
  unsigned char sc1 = 0;
  unsigned short int ss1 = 1;
  unsigned short ss2 = 2;
  unsigned int si1 = 3;
  unsigned long int sl1 = 4;
  unsigned long sl2 = 5;
  unsigned long long int sll1 = 6;
  unsigned long long sll2 = 7;
  unsigned s1 = 8;

  CHECK_INT(0, sc1);
  CHECK_INT(1, ss1);
  CHECK_INT(2, ss2);
  CHECK_INT(3, si1);
  CHECK_INT(4, sl1);
  CHECK_INT(5, sl2);
  CHECK_INT(6, sll1);
  CHECK_INT(7, sll2);
  CHECK_INT(8, s1);
}

static void test53(void) {
  unsigned x = 5;
  unsigned y = 3;

  CHECK_INT(8, x + y);
  CHECK_INT(2, x - y);
  CHECK_INT(-2, y - x);
  CHECK_INT(15, x * y);
  CHECK_INT(1, x / y);
}

static void test54(void) {
  unsigned char uc = 1;
  unsigned short us = 2;
  unsigned int ui = 3;
  unsigned long ul = 4;
  unsigned long ull = 5;

  signed char ic = 6;
  signed short is = 7;
  signed int ii = 8;
  signed long il = 9;
  signed long ill = 10;

  ull = uc;
  CHECK_INT(1, ull == uc);
  ull = us;
  CHECK_INT(1, ull == us);
  ull = ui;
  CHECK_INT(1, ull == ui);
  ull = ul;
  CHECK_INT(1, ull == ul);
  ull = ull;
  CHECK_INT(1, ull == ull);
  ull = ic;
  CHECK_INT(1, ull == ic);
  ull = is;
  CHECK_INT(1, ull == is);
  ull = ii;
  CHECK_INT(1, ull == ii);
  ull = il;
  CHECK_INT(1, ull == il);
  ull = ill;
  CHECK_INT(1, ull == ill);

  ill = uc;
  CHECK_INT(1, ill == uc);
  ill = us;
  CHECK_INT(1, ill == us);
  ill = ui;
  CHECK_INT(1, ill == ui);
  ill = ul;
  CHECK_INT(1, ill == ul);
  ill = ull;
  CHECK_INT(1, ill == ull);
  ill = ic;
  CHECK_INT(1, ill == ic);
  ill = is;
  CHECK_INT(1, ill == is);
  ill = ii;
  CHECK_INT(1, ill == ii);
  ill = il;
  CHECK_INT(1, ill == il);
  ill = ill;
  CHECK_INT(1, ill == ill);
}

static void test55(void) {
  const char *s = "hogehoge";
  CHECK_INT(1, s != 0);

  const int a[4] = {1, 2, 3, 4};
  const int *const p = a;
  const int *const *const pp = &p;
  CHECK_INT(1, (*pp)[0]);
  CHECK_INT(2, (*pp)[1]);
  CHECK_INT(3, (*pp)[2]);
  CHECK_INT(4, (*pp)[3]);
}

static int test56_varargs(int a, int b, ...) { return a + b; }
static int test56_noargs() { return 8; }
static void test56(void) {
  CHECK_INT(3, test56_varargs(1, 2, 3));
  CHECK_INT(8, test56_noargs());
  CHECK_INT(8, test56_noargs(1));
  CHECK_INT(8, test56_noargs(1, 2));
}

#define TEST57_DEF 123
#define test57_x test57_y
int test57_x = 3;
#undef test57_x
int test57_x = 8;
static void test57(void) {
  CHECK_INT(123, TEST57_DEF);
  CHECK_INT(8, test57_x);
  CHECK_INT(3, test57_y);
}

static void test58(void) {
  CHECK_INT(4, sizeof(123));
  CHECK_INT(8, sizeof(123L));
  CHECK_INT(8, sizeof(123LL));

  CHECK_INT(123, 123L);
  CHECK_INT(123, 123LL);
  CHECK_INT(123, 123l);
  CHECK_INT(123, 123ll);

  CHECK_INT(123, 123U);
  CHECK_INT(123, 123UL);
  CHECK_INT(123, 123ULL);
  CHECK_INT(123, 123Ul);
  CHECK_INT(123, 123Ull);

  CHECK_INT(123, 123LU);
  CHECK_INT(123, 123LLU);
  CHECK_INT(123, 123lU);
  CHECK_INT(123, 123llU);

  CHECK_INT(123, 123u);
  CHECK_INT(123, 123uL);
  CHECK_INT(123, 123uLL);
  CHECK_INT(123, 123ul);
  CHECK_INT(123, 123ull);

  CHECK_INT(123, 123Lu);
  CHECK_INT(123, 123LLu);
  CHECK_INT(123, 123lu);
  CHECK_INT(123, 123llu);

  CHECK_INT(1, (~0ULL) >> ((sizeof(long long) * 8) - 1));
  CHECK_INT(1, (~0U) >> ((sizeof(int) * 8) - 1));

  CHECK_INT(1, (0xffffffffffffffff) >> ((sizeof(long long) * 8) - 1));
  CHECK_INT(1, (0xffffffffffffffff) > 0);
}

static void test59(void) {
  wchar_t wc = L'x';
  CHECK_INT('x', wc);
}

static void test60(void) { CHECK_INT(1, 1 ? 1 : 0l); }
static void test61(void) {
  int a[3 + 5] = {};
  CHECK_INT(8, sizeof(a) / sizeof(a[0]));
  for (int i = 0; i < sizeof(a) / sizeof(a[0]); i++) {
    CHECK_INT(0, a[i]);
  }
}

static void test62(void) {
  CHECK_INT(8, (7, 8));
  int a;
  *(a = 0, &a) = 3;
  CHECK_INT(3, a);
}

static int test63_func(int a[100]) {
  CHECK_INT(8, sizeof(a));
  int s = 0;
  for (int i = 0; i < 100; i++) {
    s += a[i];
  }
  return s;
}

static void test63(void) {
  int a[100];
  for (int i = 0; i < 100; i++) {
    a[i] = i;
  }
  CHECK_INT(4950, test63_func(a));
}

static char test64_static[] = "static str";
static void test64(void) {
  char local[] = "local str";
  char local_static[] = "local static str";

  CHECK_INT(0, strcmp("static str", test64_static));
  CHECK_INT(0, strcmp("local str", local));
  CHECK_INT(0, strcmp("local static str", local_static));

  CHECK_INT(strlen("static str") + 1, sizeof(test64_static));
  CHECK_INT(strlen("local str") + 1, sizeof(local));
  CHECK_INT(strlen("local static str") + 1, sizeof(local_static));
}

const static char *test65_s = "ABC"
                              "DEF";
const static char test65_a[] = "GHI"
                               "JKL";
static void test65(void) {
  const char *s = "ABC"
                  "DEF";
  const char a[] = "GHI"
                   "JKL";
  CHECK_INT(0, strcmp(test65_s, "ABCDEF"));
  CHECK_INT(0, strcmp(test65_a, "GHIJKL"));
  CHECK_INT(7, sizeof(test65_a) / sizeof(test65_a[0]));
  CHECK_INT(0, strcmp(s, "ABCDEF"));
  CHECK_INT(0, strcmp(a, "GHIJKL"));
  CHECK_INT(7, sizeof(a) / sizeof(a[0]));
}

typedef struct test66_s test66_s;
static void test66(void) {
test66_s:
  return;
}

static void test67(void) {
  int x = 123;
  _Bool a = x;
  _Bool b = 123;
  CHECK_INT(1, a);
  CHECK_INT(1, b);
  x = 0;
  a = x;
  b = 0;
  CHECK_INT(0, a);
  CHECK_INT(0, b);
}

static void test68(void) {
  const volatile int *restrict const *const volatile s = NULL;
  (void)s;
}

static void test69(void) {
  typedef struct S1 S1;
  struct S1 {
    S1 *s1;
  } s1 = {NULL};
  typedef struct S2 S2;
  struct S2 {
    S1 s1;
    S2 *s2;
  } s2;

  s2.s1.s1 = NULL;
  s2.s2 = NULL;

  CHECK_INT(8, sizeof(s1));
  CHECK_INT(16, sizeof(s2));
}

typedef struct Test70_S {
  int a[79];
  struct X {
    int x;
    int y;
  } x;

} Test70_S;
static Test70_S test70_s;
static void test70(void) {
  typedef Test70_S S;
  S a = {};
  S b = {};
  int len = sizeof(a.a) / sizeof(a.a[0]);
  for (int i = 0; i < len; i++) {
    CHECK_INT(0, a.a[i]);
    a.a[i] = i;
  }
  for (int i = 0; i < len; i++) {
    CHECK_INT(0, test70_s.a[i]);
    test70_s.a[i] = i * i;
  }
  (a, b);
  b = a;
  for (int i = 0; i < len; i++) {
    CHECK_INT(a.a[i], b.a[i]);
  }
  b = test70_s;
  for (int i = 0; i < len; i++) {
    CHECK_INT(test70_s.a[i], b.a[i]);
  }
  b = (test70_s.a[1] < a.a[1]) ? test70_s : a;
  for (int i = 0; i < len; i++) {
    CHECK_INT(a.a[i], b.a[i]);
  }

  S *x = &b;
  S *y = &test70_s;
  *x = *y;
  for (int i = 0; i < len; i++) {
    CHECK_INT(test70_s.a[i], b.a[i]);
  }
}

struct Test71_S {
  int a;
  int b;
  int c;
};
struct Test71_S test71_s = (struct Test71_S){7, 8, 9};
struct Test71_S *test71_p = &(struct Test71_S){10, 11, 12};
static int test71_i = 0;
static int test71_j = 0;
static int test71_k = 0;
typedef struct Test71_Ref Test71_Ref;
typedef struct Test71_Ref {
  Test71_Ref *next;
  int *ptr;
} Test71_Ref;
static Test71_Ref test71_ref = {
    .ptr = &test71_i,
    .next =
        &(Test71_Ref){.ptr = &test71_j,
                      .next = &(Test71_Ref){.ptr = &test71_k, .next = NULL}}};

static void test71(void) {
  struct S {
    int x;
    int y;
    int z;
  };
  struct S s;
  s = (struct S){3, 4, 5};
  CHECK_INT(3, s.x);
  CHECK_INT(4, s.y);
  CHECK_INT(5, s.z);

  struct S *p;
  p = &(struct S){1, 2, 3};
  CHECK_INT(1, p->x);
  CHECK_INT(2, p->y);
  CHECK_INT(3, p->z);

  CHECK_INT(7, test71_s.a);
  CHECK_INT(8, test71_s.b);
  CHECK_INT(9, test71_s.c);
  CHECK_INT(10, test71_p->a);
  CHECK_INT(11, test71_p->b);
  CHECK_INT(12, test71_p->c);

  Test71_Ref *ref = &test71_ref;
  for (int i = 5; ref != NULL; i++) {
    *ref->ptr = i;
    ref = ref->next;
  }
  CHECK_INT(5, test71_i);
  CHECK_INT(6, test71_j);
  CHECK_INT(7, test71_k);
}

struct Test72_Arg1 {
  int a;
  int b;
  int c;
  int d;
};
struct Test72_Arg2 {
  long x;
  long y;
  long z;
};
static void test72_func(struct Test72_Arg1 a1, struct Test72_Arg2 a2) {
  CHECK_INT(3, a1.a);
  CHECK_INT(4, a1.b);
  CHECK_INT(5, a1.c);
  CHECK_INT(6, a1.d);
  CHECK_INT(10, a2.x);
  CHECK_INT(20, a2.y);
  CHECK_INT(30, a2.z);
}
static void test72(void) {
  struct Test72_Arg1 a1 = {3, 4, 5, 6};
  struct Test72_Arg2 a2 = {10, 20, 30};
  test72_func(a1, a2);
}

struct Test73_Ret1 {
  int a;
  int b;
  int c;
  int d;
};
struct Test73_Ret2 {
  long x;
  long y;
  long z;
};
static struct Test73_Ret1 test73_func1(void) {
  return (struct Test73_Ret1){3, 4, 5, 6};
}
static struct Test73_Ret2 test73_func2(void) {
  return (struct Test73_Ret2){10, 20, 30};
}
static void test73(void) {
  struct Test73_Ret1 r1;
  r1 = test73_func1();
  CHECK_INT(3, r1.a);
  CHECK_INT(4, r1.b);
  CHECK_INT(5, r1.c);
  CHECK_INT(6, r1.d);
  struct Test73_Ret2 r2;
  r2 = test73_func2();
  CHECK_INT(10, r2.x);
  CHECK_INT(20, r2.y);
  CHECK_INT(30, r2.z);
}

static void test74(void) {
  int n = 1;
  int m = 2;

  float fx = n;
  float fy = m;
  float fz = m;
  float fs = -(fx / fy + 3) * fz;
  CHECK_INT(-7, fs);

  CHECK_INT(1, fy > fx);
  CHECK_INT(0, fy < fx);
  CHECK_INT(1, fy >= fx);
  CHECK_INT(0, fy <= fx);
  CHECK_INT(0, fy == fx);
  CHECK_INT(1, fy != fx);

  CHECK_INT(0, fx > fy);
  CHECK_INT(1, fx < fy);
  CHECK_INT(0, fx >= fy);
  CHECK_INT(1, fx <= fy);
  CHECK_INT(0, fx == fy);
  CHECK_INT(1, fx != fy);

  CHECK_INT(0, fy > fz);
  CHECK_INT(0, fy < fz);
  CHECK_INT(1, fy >= fz);
  CHECK_INT(1, fy <= fz);
  CHECK_INT(1, fy == fz);
  CHECK_INT(0, fy != fz);

  double dx = n;
  double dy = m;
  double dz = m;
  double ds = -(dx / dy + 3) * dz;
  CHECK_INT(-7, ds);

  CHECK_INT(1, dy > dx);
  CHECK_INT(0, dy < dx);
  CHECK_INT(1, dy >= dx);
  CHECK_INT(0, dy <= dx);
  CHECK_INT(0, dy == dx);
  CHECK_INT(1, dy != dx);

  CHECK_INT(0, dx > dy);
  CHECK_INT(1, dx < dy);
  CHECK_INT(0, dx >= dy);
  CHECK_INT(1, dx <= dy);
  CHECK_INT(0, dx == dy);
  CHECK_INT(1, dx != dy);

  CHECK_INT(0, dy > dz);
  CHECK_INT(0, dy < dz);
  CHECK_INT(1, dy >= dz);
  CHECK_INT(1, dy <= dz);
  CHECK_INT(1, dy == dz);
  CHECK_INT(0, dy != dz);

  CHECK_INT(1, fx == dx);
  CHECK_INT(1, (float)dx == fx);

  long double ldx = n;
  long double ldy = m;
  long double ldz = m;
  long double lds = -(ldx / ldy + 3) * ldz;
  CHECK_INT(-7, lds);

  CHECK_INT(1, ldy > ldx);
  CHECK_INT(0, ldy < ldx);
  CHECK_INT(1, ldy >= ldx);
  CHECK_INT(0, ldy <= ldx);
  CHECK_INT(0, ldy == ldx);
  CHECK_INT(1, ldy != ldx);

  CHECK_INT(0, ldx > ldy);
  CHECK_INT(1, ldx < ldy);
  CHECK_INT(0, ldx >= ldy);
  CHECK_INT(1, ldx <= ldy);
  CHECK_INT(0, ldx == ldy);
  CHECK_INT(1, ldx != ldy);

  CHECK_INT(0, ldy > ldz);
  CHECK_INT(0, ldy < ldz);
  CHECK_INT(1, ldy >= ldz);
  CHECK_INT(1, ldy <= ldz);
  CHECK_INT(1, ldy == ldz);
  CHECK_INT(0, ldy != ldz);

  CHECK_INT(1, fx == ldx);
  CHECK_INT(1, dx == ldx);
  CHECK_INT(1, (float)ldx == fx);
  CHECK_INT(1, (double)ldx == dx);
}

static float test75_f(float f) { return 2 * f; }
static double test75_d(double d) { return 2 * d; }
static long double test75_ld(long double ld) { return 2 * ld; }
static void test75(void) {
  float f = 1;
  CHECK_INT(1, test75_f(f) == 2 * f);
  TEST_PRINTF("f = %f", f);
  double d = 1;
  CHECK_INT(1, test75_d(d) == 2 * d);
  TEST_PRINTF("d = %f", f);
  long double ld = 1;
  CHECK_INT(1, test75_ld(ld) == 2 * ld);
  TEST_PRINTF("ld = %Lf", ld);
}

typedef struct Test76_S {
  char n[3];
} Test76_S;
static int test76_f(Test76_S s) { return s.n[0] + s.n[1] + s.n[2]; }
static void test76(void) {
  Test76_S s = {{1, 2, 3}};
  CHECK_INT(6, test76_f(s));
}

static int test77_sum(int n, ...) {
  int sum = 0;
  va_list ap;
  va_start(ap, n);

  TEST_PRINTF("overflow_arg_area: %p", ((int **)ap)[1]);
  TEST_PRINTF("reg_save_area: %p", ((int **)ap)[2]);

  for (int i = 0; i < n; i++) {
    int x = va_arg(ap, int);
    TEST_PRINTF("%d: %d", i, x);
    TEST_PRINTF("gp_offset: %d, fp_offset: %d", ((int *)ap)[0], ((int *)ap)[1]);
    sum += x;
  }

  va_end(ap);
  return sum;
}

static int test77_sum_va(int n, va_list ap) {
  int sum = 0;
  for (int i = 0; i < n; i++) {
    sum += va_arg(ap, int);
  }
  return sum;
}

static int test77_sum2(int n, ...) {
  int sum = 0;
  va_list ap, aq;

  va_start(ap, n);
  va_copy(aq, ap);

  for (int i = 0; i < n; i++) {
    sum += va_arg(ap, int);
  }

  va_end(ap);
  sum += test77_sum_va(n, aq);
  va_end(aq);
  return sum;
}

static double test77_sum_double(int n, ...) {
  va_list ap;
  va_start(ap, n);
  double sum = 0;
  for (int i = 0; i < n; i++) {
    sum += va_arg(ap, double);
  }
  return sum;
}

static void test77_vprintf(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vprintf(fmt, ap);
  va_end(ap);
}

static void test77(void) {
  CHECK_INT(15, test77_sum(5, 1, 2, 3, 4, 5));
  CHECK_INT(0, test77_sum(0));
  CHECK_INT(55, test77_sum(10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  CHECK_INT(110, test77_sum2(10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  CHECK_INT(110, test77_sum2(10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  CHECK_INT(7, test77_sum_double(5, 0.5, 1.0, 1.5, 2.0, 2.5));
  CHECK_INT(27, test77_sum_double(10, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0,
                                  4.5, 5.0));
  test77_vprintf("    %d %d %s %s %d %d %s %f %f %f\n", 1, 3, "foo", "bar", 8,
                 10, "baz", 1.8, 2.0, 3.0);
}

typedef struct Test78_S {
  struct {
    int a;
    int b;
    int c;
  } x;
} Test78_S;

static Test78_S test78_f(void) { return (Test78_S){1, 2, 3}; }

static void test78(void) {
  CHECK_INT(6, test78_f().x.a + test78_f().x.b + test78_f().x.c);
  Test78_S s = test78_f();
  CHECK_INT(6, s.x.a + s.x.b + s.x.c);
}

static void test79(void) {
  char *suffix = NULL;
  long double val = strtold("1.234", &suffix);
  TEST_PRINTF("%Lf", val);
}

typedef struct Test80_S {
  int a;
  int b;
  int c[];
} Test80_S;
Test80_S test80_s = {1, 2, {3, 4, 5}};
static void test80(void) {
  CHECK_INT(sizeof(int) * 2, sizeof(Test80_S));
  CHECK_INT(sizeof(int) * 2, sizeof(test80_s));
}

static void test81(void) {
  int a = 10;
  long b = 20;
  typedef struct X {
  } X;
  typedef struct Y {
  } Y;
  X x;
  Y y;

  CHECK_INT(2, _Generic(a, X : 0, Y : 1, default : 2));
  CHECK_INT(2, _Generic(b, int : 0, default : 1, long : 2));
  CHECK_INT(1, _Generic(x, int : 0, X : 1, Y : 2, default : 3));
  CHECK_INT(2, _Generic(y, int : 0, X : 1, Y : 2, default : 3));
  // clang-format off
  CHECK_INT(10, _Generic(a,
                         short: a * 0,
                         int: a * 1,
                         long: a * 2,
                         default : a * 3));
  // clang-format on
}

typedef struct Test82_P {
  int x;
  int y;
} Test82_P;
typedef union Test82_Q {
  int x;
  int y;
} Test82_Q;
struct Test82_S {
  Test82_P a;
  struct {
    int x;
    int y;
  } b;
  Test82_Q c;
} test82_s = {(Test82_P){3, 4}, {5, 6}, (Test82_Q){7}};
static void test82(void) {
  CHECK_INT(3, test82_s.a.x);
  CHECK_INT(4, test82_s.a.y);
  CHECK_INT(5, test82_s.b.x);
  CHECK_INT(6, test82_s.b.y);
  CHECK_INT(7, test82_s.c.x);
}

static void test83(void) {
  CHECK_INT(3, ({ 3; }));
  CHECK_INT(12, ({
              int x = 4;
              int y = 3;
              (x * y);
            }));
}

static void test84(void) {
  bool cond = true;
  3 ? TEST_PRINTF("OK") : abort();
  0 ? abort() : TEST_PRINTF("OK");
  cond ? TEST_PRINTF("OK") : abort();
  !cond ? abort() : TEST_PRINTF("OK");
}

static void test85(void) {
  typedef union {
    char x;
    int a[16];
  } U;

  U u = {0};
  for (int i = 0; i < 16; i++) {
    CHECK_INT(0, u.a[i]);
  }
}

static void test86(void) {
  int a[] = {
      [0 ... 4] = 10,
      [5 ... 9] = 20,
      [15 ... 19] = 30,
  };
  CHECK_INT(20 * sizeof(int), sizeof(a));
  for (int i = 0; i < 5; i++) {
    CHECK_INT(10, a[i]);
  }
  for (int i = 5; i < 10; i++) {
    CHECK_INT(20, a[i]);
  }
  for (int i = 10; i < 15; i++) {
    CHECK_INT(0, a[i]);
  }
  for (int i = 15; i < 20; i++) {
    CHECK_INT(30, a[i]);
  }
}

static void test87(void) {
  struct {
    char s[6];
    int n;
  } s = {"hello", 8};
  CHECK_INT(0, strcmp(s.s, "hello"));
  CHECK_INT(8, s.n);
}

static bool test88_f(void) { return NULL; }
static void test88(void) {
  CHECK_INT(0, test88_f());
  CHECK_INT(0, (bool)NULL);
}

#define TEST89_PTR(T) typeof(T *)
#define TEST89_ARRAY(T, N) typeof(T[N])

static void test89(void) {
  bool x = 3;
  typeof(x) y = 8;
  CHECK_INT(1, y);

  CHECK_INT(1, (typeof(x))8);

  typeof(typeof(char *)[4]) s1 = {"abc", "def", "ghi", "jkl"};
  CHECK_INT(0, strcmp(s1[0], "abc"));
  CHECK_INT(0, strcmp(s1[1], "def"));
  CHECK_INT(0, strcmp(s1[2], "ghi"));
  CHECK_INT(0, strcmp(s1[3], "jkl"));

  TEST89_ARRAY(TEST89_PTR(char), 4) s2 = {"abc", "def", "ghi", "jkl"};
  CHECK_INT(0, strcmp(s2[0], "abc"));
  CHECK_INT(0, strcmp(s2[1], "def"));
  CHECK_INT(0, strcmp(s2[2], "ghi"));
  CHECK_INT(0, strcmp(s2[3], "jkl"));
}

static void test90(void) {
  CHECK_INT(10, L'\n');
  CHECK_INT(0x3042, L'\x3042');
  CHECK_INT(12354, L'あ');
}

int main(int argc, char *argv[]) {
  Test test_list[] = {
      TEST(test01), TEST(test02), TEST(test03), TEST(test04), TEST(test05),
      TEST(test06), TEST(test07), TEST(test08), TEST(test09), TEST(test10),
      TEST(test11), TEST(test12), TEST(test13), TEST(test14), TEST(test15),
      TEST(test16), TEST(test17), TEST(test18), TEST(test19), TEST(test20),
      TEST(test21), TEST(test22), TEST(test23), TEST(test24), TEST(test25),
      TEST(test26), TEST(test27), TEST(test28), TEST(test29), TEST(test30),
      TEST(test31), TEST(test32), TEST(test33), TEST(test34), TEST(test35),
      TEST(test36), TEST(test37), TEST(test38), TEST(test39), TEST(test40),
      TEST(test41), TEST(test42), TEST(test43), TEST(test44), TEST(test45),
      TEST(test46), TEST(test47), TEST(test48), TEST(test49), TEST(test50),
      TEST(test51), TEST(test52), TEST(test53), TEST(test54), TEST(test55),
      TEST(test56), TEST(test57), TEST(test58), TEST(test59), TEST(test60),
      TEST(test61), TEST(test62), TEST(test63), TEST(test64), TEST(test65),
      TEST(test66), TEST(test67), TEST(test68), TEST(test69), TEST(test70),
      TEST(test71), TEST(test72), TEST(test73), TEST(test74), TEST(test75),
      TEST(test76), TEST(test77), TEST(test78), TEST(test79), TEST(test80),
      TEST(test81), TEST(test82), TEST(test83), TEST(test84), TEST(test85),
      TEST(test86), TEST(test87), TEST(test88), TEST(test89), TEST(test90),
      {NULL, NULL},
  };
  RUN_TEST(argc, argv, test_list);
}
