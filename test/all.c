#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int num_check;
static void check_int(int a, int b);

static void test01(void) {
  check_int(0, 0);
  check_int(42, 42);
  check_int(21, 5 + 20 - 4);
  check_int(41, 12 + 34 - 5);
  check_int(47, 5 + 6 * 7);
  check_int(15, 5 * (9 - 6));
  check_int(4, (3 + 5) / 2);
}

static void test02_01(void) {
  int a = 2, b = 2;
  check_int(4, a + b);
  return;
}
static void test02_02(void) {
  int a = 3;
  int b = 5 * 6 - 8;
  check_int(14, a + b / 2);
  return;
}
static void test02_03(void) {
  int a, b;
  b = (a = 5);
  check_int(10, a + b);
  return;
}
static void test02(void) {
  test02_01();
  test02_02();
  test02_03();
  check_int(1, 10 + 2 == 3 * 4);
  check_int(10, 10 + (2 != 1 * 2));
  check_int(12, 10 + ((2 != 1) * 2));
}

static void test03_01(void) {
  int c;
  int a = 3;
  int b = 5;
  check_int(0, c = a == b);
}
static void test03_02(void) {
  int c;
  int a = 3;
  int b = 5;
  check_int(0, c = a == b);
}
static void test03_03(void) {
  int a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y,
      z;
  a = b = c = d = e = f = g = h = i = j = k = l = m = n = o = p = q = r = s =
      t = u = v = w = x = y = z = 1;
  check_int(a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q +
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
  check_int(120, 120);
}
static void test03_05(void) {
  int a = 1, c = 2, d = 3;
  check_int(6, a + c + d);
}
static void test03_06(void) {
  int a = 1, ab = 2, abc = 3;
  check_int(6, a + ab + abc);
}
static void test03_07(void) {
  int _ = 123, a_ = 1;
  check_int(124, _ + a_);
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
  check_int(8, 1 << 2 << 1);
  check_int(2, 1 << 2 >> 1);
  check_int(0, 1 >> 3);
  check_int(-1, 0 - 1 >> 3);
  check_int(1, 3 < 5);
  check_int(0, 3 > 5);
  check_int(1, 3 <= 5);
  check_int(0, 3 >= 5);
  check_int(0, 3 < 3);
  check_int(0, 3 > 3);
  check_int(1, 3 <= 3);
  check_int(1, 3 >= 3);
  check_int(1, 5 & 3);
  check_int(6, 5 ^ 3);
  check_int(7, 5 | 3);
  check_int(1, 3 && 5);
  check_int(0, 3 && 0);
  check_int(0, 0 && (a = 1));
  check_int(0, a);
  check_int(1, 3 || 5);
  check_int(1, 3 || (a = 1));
  check_int(0, a);
  check_int(1, 0 || 1);
  check_int(0, 0 || 0);
  check_int(12, 1 ? 12 : 34);
  check_int(34, 0 ? 12 : 34);
  check_int(3, 8 % 5);
  check_int(-4, ~3);
  check_int(0, !1);
  check_int(0, !3);
  check_int(1, !0);
}

static void test05(void) {
  int a, b;

  a = +3;
  check_int(103, 100 + a);

  a = -3;
  check_int(97, 100 + a);

  a = 2;
  check_int(3, ++a);

  a = 2;
  b = ++a;
  check_int(33, 10 * a + b);

  a = 2;
  check_int(1, --a);

  a = 2;
  b = --a;
  check_int(11, 10 * a + b);

  a = 2;
  check_int(2, a++);
  check_int(3, a);

  a = 2;
  b = a++;
  check_int(32, 10 * a + b);

  a = 2;
  check_int(2, a--);
  check_int(1, a);

  a = 2;
  b = a--;
  check_int(12, 10 * a + b);
}

static void test06(void) {
  (void)1;
  ;
  (void)2;
  ;
  ;
  ;
  check_int(3, 3);
}

static void test07(void) {
  int a, b, c, s;

  a = 1;
  b = 1;
  check_int(8, a + b + 6);

  a = b = 1;
  if (1) {
    a = 5;
    b = 3;
  }
  check_int(8, a + b);

  a = b = 1;
  if (0) {
    a = 5;
    b = 3;
  }
  check_int(2, a + b);

  a = b = 1;
  if (1) {
    a = 5;
    b = 3;
  } else {
    a = 3;
    b = 2;
  }
  check_int(8, a + b);

  a = b = 1;
  if (0) {
    a = 5;
    b = 3;
  } else {
    a = 3;
    b = 2;
  }
  check_int(5, a + b);

  a = b = 1;
  if (1)
    a = 5;
  b = 3;
  check_int(8, a + b);

  a = b = 1;
  if (0)
    a = 5;
  b = 3;
  check_int(4, a + b);

  a = b = 1;
  if (1)
    a = 5;
  else
    a = 3;
  b = 2;
  check_int(7, a + b);

  a = b = 1;
  if (0)
    a = 5;
  else
    a = 3;
  b = 2;
  check_int(5, a + b);

  a = 0;
  while (a < 5) {
    a++;
  }
  check_int(5, a);

  a = b = 0;
  while (a < 5) {
    b = b + 2;
    a++;
  }
  check_int(10, b);

  c = 97;
  while (c <= 122) {
    putchar(c);
    c++;
  };
  check_int(10, putchar(10));

  do
    a = 10;
  while (0);
  check_int(10, a);

  a = 0;
  do
    a++;
  while (a < 10);
  check_int(10, a);

  s = 0;
  for (a = 1; a <= 10; a++)
    s = s + a;
  check_int(55, s);

  a = 1;
  s = 0;
  for (; a <= 10; a++)
    s = s + a;
  check_int(55, s);

  a = 1;
  s = 0;
  for (; a <= 10;) {
    s = s + a;
    a++;
  }
  check_int(55, s);

  a = 0;
  s = 0;
  while (1) {
    a++;
    if (a >= 10)
      break;
    s = s + a;
  }
  check_int(45, s);

  a = 0;
  s = 0;
  do {
    a++;
    if (a >= 10)
      break;
    s = s + a;
  } while (1);
  check_int(45, s);

  a = 0;
  s = 0;
  for (;;) {
    a++;
    if (a >= 10)
      break;
    s = s + a;
  }
  check_int(45, s);

  a = 0;
  s = 0;
  while (a < 10) {
    a++;
    if (a % 2)
      continue;
    s = s + a;
  }
  check_int(30, s);

  a = 0;
  s = 0;
  do {
    a++;
    if (a % 2)
      continue;
    s = s + a;
  } while (a < 10);
  check_int(30, s);

  a = 0;
  s = 0;
  for (a = 1; a <= 10; a++) {
    if (a % 2)
      continue;
    s = s + a;
  }
  check_int(30, s);
}

static void test08(void) {
  int a, *p;

  a = 4;
  a *= 3;
  check_int(12, a);

  a = 5;
  a /= 3;
  check_int(1, a);

  a = 5;
  a %= 3;
  check_int(2, a);

  a = 4;
  a += 3;
  check_int(7, a);

  a = 4;
  a -= 3;
  check_int(1, a);

  a = 4;
  a <<= 3;
  check_int(32, a);

  a = 32;
  a >>= 3;
  check_int(4, a);

  a = 5;
  a &= 3;
  check_int(1, a);

  a = 5;
  a |= 3;
  check_int(7, a);

  a = 5;
  a ^= 3;
  check_int(6, a);

  a = 5;
  p = &a;
  *p += 3;
  check_int(8, a);
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
  int c = 0, i;
  for (i = 0; i < 5; i++) {
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
  check_int(5, c);
}
static void test09_03(void) {
  int i = 3, s = 0;
  goto INIT;
  for (i = 0; i < 5; i++) {
  INIT:
    s += i;
  }
  check_int(7, s);
}
static void test09_04(void) {
  int x = 5;
  switch (11)
  case 0:
    abort();
  check_int(5, x);
}
static void test09(void) {
  check_int(3, test09_01(3));
  check_int(0, test09_01(0));
  check_int(255, test09_01(8));
  test09_02();
  test09_03();
  test09_04();
}

static void test10(void) {
  check_int(255, 0xff);
  check_int(16, 0x10);
  check_int(255, 0377);
  check_int(8, 010);
  check_int(255, 255);
  check_int(10, 10);
  check_int(97, 'a');
  check_int(10, '\n');
  check_int(10, '\xa');
  check_int(10, '\x0a');
  check_int(10, '\x00a');
  check_int(1, '\1');
  check_int(8, '\10');
  check_int(8, '\010');
}

static int test11_01(void) {
  if (1)
    return 3;
  abort();
}
static void test11(void) { check_int(3, test11_01()); }

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

  check_int(113, a * 10 + b);
  check_int(20, to_double(3) + to_double(7));
  check_int(10, add(3, 5) + sub(5, 3));
  check_int(0, fib(0));
  check_int(1, fib(1));
  check_int(1, fib(2));
  check_int(2, fib(3));
  check_int(3, fib(4));
  check_int(5, fib(5));
  check_int(8, fib(6));
  check_int(13, fib(7));
  check_int(21, fib(8));
  check_int(34, fib(9));
  check_int(55, fib(10));
  check_int(89, fib(11));
  check_int(144, fib(12));
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
  check_int(123456, digits6(1, 2, 3, 4, 5, 6));
  check_int(21, add6(1, 2, 3, 4, 5, 6));
  check_int(1234567890, digits10(1, 2, 3, 4, 5, 6, 7, 8, 9, 0));
  check_int(45, add10(1, 2, 3, 4, 5, 6, 7, 8, 9, 0));
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

  p = 30;
  q = 10;

  i = p + 3;
  check_int(42, i);

  i = 3 + p;
  check_int(42, i);

  p += 3;
  check_int(42, p);

  i = 3;
  i += p;
  check_int(54, i);

  i = p - 3;
  check_int(30, i);

  i = p - q;
  check_int(8, i);

  p -= q;
  check_int(8, p);

  p = 8;
  p++;
  check_int(12, p);

  ++p;
  check_int(16, p);

  p--;
  check_int(12, p);

  --p;
  check_int(8, p);

  p = 10;
  i = 5;
  i += (int)p;
  check_int(15, i);

  p = 10;
  i = 5;
  i += (int *)p;
  check_int(30, i);

  p = 10;
  i = 5;
  i += (int **)p;
  check_int(50, i);
}

static void test16(void) {
  int a[3];

  *a = 1;
  *(a + 1) = 2;
  int *p;
  p = a;
  check_int(3, *p + *(p + 1));

  a[0] = 5;
  a[1] = 4;
  a[2] = 3;
  check_int(12, a[0] + a[1] + a[2]);
  check_int(12, 0 [a] + 1 [a] + 2 [a]);

  check_int(2, &a[2] - &a[0]);
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
  check_int(8, test17_01(a)[2]);

  test17_02();

  void *p, *q;
  p = 3;
  q = test17_03(p);
  check_int(1, p == q);
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
  check_int(test18_g, 100);

  test18_init();
  (void)test18_add(7);
  check_int(10, test18_g);
  check_int(1, test18_h[0]);
  check_int(4, test18_h[1]);
  check_int(9, test18_h[2]);
}

static void test19(void) {
  char x[4];
  x[0] = 'a';
  x[1] = 'b';
  x[2] = 'c';
  x[3] = '\0';
  puts(x);

  x[0] = -1;
  x[1] = 2;
  int y;
  y = 4;
  check_int(3, x[0] + y);

  check_int(1, &x[1] - &x[0]);
}

static void test20(void) {
  puts("hogehoge");
  puts("\n\n\r\r");
  puts("ほげほげ");

  printf("%s %d\n", "abc", 456);
}

static void test21(void) {
  int a[3][5], i, j;

  for (i = 0; i < 3; i++) {
    for (j = 0; j < 5; j++) {
      a[i][j] = i * j;
    }
  }

  for (i = 0; i < 3; i++) {
    printf("%d:", i);
    for (j = 0; j < 5; j++) {
      printf(" %d", a[i][j]);
      printf(" %p", &a[i][j]);
      printf(" (%zu)", &a[i][j] - &a[0][0]);
    }
    printf("\n");
  }

  for (i = 0; i < 3; i++) {
    for (j = 0; j < 5; j++) {
      check_int(10000 * i + 1000 * j + i * j, 10000 * i + 1000 * j + a[i][j]);
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

  check_int(3, b[0]);
  check_int(4, b[1]);
  check_int(5, b[2]);

  test22_g = 0;
  void (*f)(int n);
  f = &test22_func;
  f(3);
  check_int(3, test22_g);
}

static void test23_fun(int a, int b) {
  int c, d, e;
  c = 3;
  d = 4;
  e = 5;
  check_int(1, a);
  check_int(2, b);
  check_int(3, c);
  check_int(4, d);
  check_int(5, e);

  {
    int c, f;
    c = 6;
    e = 7;
    f = 8;
    check_int(1, a);
    check_int(2, b);
    check_int(6, c);
    check_int(4, d);
    check_int(7, e);
    check_int(8, f);
  }

  check_int(1, a);
  check_int(2, b);
  check_int(3, c);
  check_int(4, d);
  check_int(7, e);
}
static void test23(void) { test23_fun(1, 2); }

static void test24(void) {
  char a = 64, b = a, c = a, d = a;
  check_int(256, a + b + c + d);

  check_int(256, (char)64 + (char)64 + (char)64 + (char)64);
  check_int(64, (char)320);
  check_int(256, (char)320 + (char)320 + (char)320 + (char)320);
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

  check_int(8, s.x + s.y);

  struct {
    int z;
    int w;
  } t;
  t.z = t.w = 8;
  check_int(64, t.z * t.w);

  struct Foo s2;
  s2.x = 8;
  s2.y = 1;
  check_int(9, s2.x + s2.y);

  struct Bar {
    int x;
  };
  struct Bar u;
  u.x = 3;
  check_int(3, u.x);

  struct Foo *q;
  q = &s;
  check_int(8, q->x + q->y);
  check_int(1, &q->x == &s.x);

  struct P {
    struct P *x;
    int v;
  } p;
  p.x = &p;
  p.v = 123;
  check_int(123, p.x->x->x->x->v);

  struct Test26 k;
  k.z = 3;
  check_int(3, k.z);

  test26_x.z = 5;
  test26_y.y = 100;
  test26_z.y = 200;
  check_int(305, test26_x.z + test26_y.y + test26_z.y);
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
static void test28(void) { check_int(12, test28_foo(3, 4, 5)); }

static void test29(void) {
  int a[7];
  struct Foo {
    int a[9];
  };

  check_int(4, sizeof(int));
  check_int(4, sizeof 3);
  check_int(4, sizeof(3 + 5));
  check_int(28, sizeof a);
  check_int(36, sizeof(struct Foo));
}

static void test30(void) {
  union {
    int a;
    int b;
  } u;

  u.a = 1;
  check_int(1, u.a);

  u.b = 2;

  check_int(2, u.a);
  check_int(2, u.b);
}

static int test31_x = 3;
static void test31(void) { check_int(3, test31_x); }

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

  check_int(8, test32_x);
  *test32_p = 1;
  check_int(1, test32_x);

  check_int(8, sizeof(x));
  check_int(8, sizeof(x + y));
  check_int(8, sizeof(y + x));
  check_int(4, sizeof(y));

  check_int(8, sizeof &test32_fun_char);
  check_int(1, sizeof test32_fun_char());
  check_int(0, test32_fun_char());
  check_int(8, sizeof &test32_fun_short);
  check_int(2, sizeof test32_fun_short());
  check_int(0, test32_fun_short());
  check_int(8, sizeof &test32_fun_int);
  check_int(4, sizeof test32_fun_int());
  check_int(0, test32_fun_int());
  check_int(8, sizeof &test32_fun_long);
  check_int(8, sizeof test32_fun_long());
  check_int(0, test32_fun_long());
  check_int(8, sizeof &test32_fun_ptr);
  check_int(8, sizeof test32_fun_ptr());
  check_int(0, (int)test32_fun_ptr());
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
  check_int(12, s.a + s.b);

  struct {
    union {
      struct {
        int x;
      };
    };
  } p;

  p.x = 3;
  check_int(3, p.x);
}

static void test34(void) {
  char ca = 3, cb = 5;
  check_int(8, ca + cb);
  check_int(-2, ca - cb);
  check_int(15, ca * cb);
  check_int(0, ca / cb);
  check_int(3, ca % cb);
  check_int(0, ca == cb);
  check_int(1, ca != cb);
  check_int(1, ca < cb);
  check_int(0, ca > cb);
  check_int(1, ca <= cb);
  check_int(0, ca >= cb);
  check_int(96, ca << cb);
  check_int(0, ca >> cb);
  check_int(1, ca & cb);
  check_int(7, ca | cb);
  check_int(5, (ca, cb));

  short sa = 3, sb = 5;
  check_int(8, sa + sb);
  check_int(-2, sa - sb);
  check_int(15, sa * sb);
  check_int(0, sa / sb);
  check_int(3, sa % sb);
  check_int(0, sa == sb);
  check_int(1, sa != sb);
  check_int(1, sa < sb);
  check_int(0, sa > sb);
  check_int(1, sa <= sb);
  check_int(0, sa >= sb);
  check_int(96, sa << sb);
  check_int(0, sa >> sb);
  check_int(1, sa & sb);
  check_int(7, sa | sb);
  check_int(5, (sa, sb));

  int ia = 3, ib = 5;
  check_int(8, ia + ib);
  check_int(-2, ia - ib);
  check_int(15, ia * ib);
  check_int(0, ia / ib);
  check_int(3, ia % ib);
  check_int(0, ia == ib);
  check_int(1, ia != ib);
  check_int(1, ia < ib);
  check_int(0, ia > ib);
  check_int(1, ia <= ib);
  check_int(0, ia >= ib);
  check_int(96, ia << ib);
  check_int(0, ia >> ib);
  check_int(1, ia & ib);
  check_int(7, ia | ib);
  check_int(5, (ia, ib));

  long la = 3, lb = 5;
  check_int(8, la + lb);
  check_int(-2, la - lb);
  check_int(15, la * lb);
  check_int(0, la / lb);
  check_int(3, la % lb);
  check_int(0, la == lb);
  check_int(1, la != lb);
  check_int(1, la < lb);
  check_int(0, la > lb);
  check_int(1, la <= lb);
  check_int(0, la >= lb);
  check_int(96, la << lb);
  check_int(0, la >> lb);
  check_int(1, la & lb);
  check_int(7, la | lb);
  check_int(5, (la, lb));
}

static void test35(void) {
  char cx = 0;
  cx = cx + 1;
  check_int(1, cx);

  short sx = 0;
  sx = sx + 1;
  check_int(1, sx);
}

static int test36_f(int test36_a), test36_g(int test36_a), test36_a = 100;
static void test36(void) {
  check_int(0, test36_f(1) - test36_g(1));
  check_int(100, test36_a);
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
} test37_t = {{{1}}, {2}, {3}, {{{4}}}, {5}, 6};
static void test37(void) {
  check_int(1, test37_s.a);
  check_int(2, test37_s.b);
  check_int(3, test37_s.c);
  check_int(4, test37_s.d);
  check_int(5, test37_s.e);
  check_int(6, test37_s.f);

  check_int(1, test37_t.a);
  check_int(2, test37_t.b);
  check_int(3, test37_t.c);
  check_int(4, test37_t.d);
  check_int(5, test37_t.e);
  check_int(6, test37_t.f);

  struct S {
    int a;
    int b;
    char c;
    int d;
    short e;
    void *f;
  } s = {11, 12, 13, 14, 15, 16};
  check_int(11, s.a);
  check_int(12, s.b);
  check_int(13, s.c);
  check_int(14, s.d);
  check_int(15, s.e);
  check_int(16, s.f);

  struct T {
    int a;
    int b;
    char c;
    int d;
    short e;
    void *f;
  } t = {{{11}}, {12}, {13}, {{{14}}}, {15}, 16};
  check_int(11, t.a);
  check_int(12, t.b);
  check_int(13, t.c);
  check_int(14, t.d);
  check_int(15, t.e);
  check_int(16, t.f);
}

static union {
  int a;
  int b;
  char c;
  int d;
  short e;
} test38_u = {1};
static void test38(void) {
  check_int(1, test38_u.a);
  check_int(1, test38_u.b);
  check_int(1, test38_u.c);
  check_int(1, test38_u.d);
  check_int(1, test38_u.e);

  union U {
    int a;
    int b;
    char c;
    int d;
    short e;
  } u = {11};
  check_int(11, u.a);
  check_int(11, u.b);
  check_int(11, u.c);
  check_int(11, u.d);
  check_int(11, u.e);
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
  check_int(1, test39_s.a);
  check_int(2, test39_s.b);
  check_int(3, test39_s.c);
  check_int(3, test39_s.d);
  check_int(4, test39_s.e.x);
  check_int(5, test39_s.e.y);

  check_int(21, test39_t.a);
  check_int(22, test39_t.b);
  check_int(23, test39_t.c);
  check_int(23, test39_t.d);
  check_int(24, test39_t.e.x);
  check_int(25, test39_t.e.y);

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
  check_int(11, s.a);
  check_int(12, s.b);
  check_int(13, s.c);
  check_int(13, s.d);
  check_int(14, s.e.x);
  check_int(15, s.e.y);

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
  check_int(31, t.a);
  check_int(32, t.b);
  check_int(33, t.c);
  check_int(33, t.d);
  check_int(34, t.e.x);
  check_int(35, t.e.y);
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
  check_int(1, s.a);
  check_int(2, s.b);
  check_int(1, s.c);
  check_int(1, s.d);
}

static long test41_f(void) { return 111; }
static void test41(void) {
  check_int(111, test41_f());
  check_int(8, sizeof(test41_f()));

  long (*ptr)(void) = test41_f;
  check_int(111, ptr());
  check_int(8, sizeof(ptr()));
}

extern int test42_n;
static int test42_l;
extern int test42_f(void);
static int test42_h(void);
static void test42(void) {
  extern int test42_m;
  extern int test42_g(void);

  check_int(8, test42_n);
  check_int(32, test42_f());
  check_int(10, test42_m);
  check_int(64, test42_g());
  check_int(12, test42_l);
  check_int(128, test42_h());
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

  check_int(0, c1);
  check_int(1, s1);
  check_int(2, s2);
  check_int(3, i1);
  check_int(4, l1);
  check_int(5, l2);
  check_int(6, ll1);
  check_int(7, ll2);
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

  check_int(0, sc1);
  check_int(1, ss1);
  check_int(2, ss2);
  check_int(3, si1);
  check_int(4, sl1);
  check_int(5, sl2);
  check_int(6, sll1);
  check_int(7, sll2);
  check_int(8, s1);
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
  check_int(8, test45_n);
  check_int(0, test45_f());
  check_int(1, test45_f());
  check_int(2, test45_f());
  check_int(0, x);
  x = 100;
  check_int(3, test45_f());

  check_int(10, test45_g());
  check_int(21, test45_g());
  check_int(32, test45_g());
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
  check_int(456, test46_s.a);
  check_int(123, test46_s.b);

  check_int(1, test46_t.a);
  check_int(0, test46_t.b);
  check_int(3, test46_t.c.a);
  check_int(4, test46_t.c.b);
  check_int(5, test46_t.d);
  check_int(0, test46_t.e);

  check_int(1, test46_u.a);
  check_int(0, test46_u.b);
  check_int(3, test46_u.c);
  check_int(0, test46_u.d);
  check_int(0, test46_u.e);
  check_int(5, test46_u.f);
  check_int(0, test46_u.g);
  check_int(0, test46_u.h);

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
  check_int(456, s.a);
  check_int(123, s.b);

  check_int(1, t.a);
  check_int(0, t.b);
  check_int(3, t.c.a);
  check_int(4, t.c.b);
  check_int(5, t.d);
  check_int(0, t.e);

  check_int(1, u.a);
  check_int(0, u.b);
  check_int(3, u.c);
  check_int(0, u.d);
  check_int(0, u.e);
  check_int(5, u.f);
  check_int(0, u.g);
  check_int(0, u.h);

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

  check_int(456, s_s.a);
  check_int(123, s_s.b);

  check_int(1, s_t.a);
  check_int(0, s_t.b);
  check_int(3, s_t.c.a);
  check_int(4, s_t.c.b);
  check_int(5, s_t.d);
  check_int(0, s_t.e);

  check_int(1, s_u.a);
  check_int(0, s_u.b);
  check_int(3, s_u.c);
  check_int(0, s_u.d);
  check_int(0, s_u.e);
  check_int(5, s_u.f);
  check_int(0, s_u.g);
  check_int(0, s_u.h);
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
  check_int(1, test47_s.x.a);
  check_int(2, test47_s.x.b);
  check_int(100, test47_t.y.a);
  check_int(200, test47_t.y.b);
  check_int(0, test47_t.y.c);

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

  check_int(1, s.x.a);
  check_int(2, s.x.b);
  check_int(100, t.y.a);
  check_int(200, t.y.b);
  check_int(0, t.y.c);

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

  check_int(1, s_s.x.a);
  check_int(2, s_s.x.b);
  check_int(100, s_t.y.a);
  check_int(200, s_t.y.b);
  check_int(0, s_t.y.c);
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
  check_int(1, test48_array[0]);
  check_int(2, test48_array[1]);
  check_int(3, test48_array[2]);
  check_int(0, test48_array[3]);

  check_int(1, test48_s.a);
  check_int(2, test48_s.b[0]);
  check_int(3, test48_s.b[1]);
  check_int(0, test48_s.b[2]);
  check_int(0, test48_s.b[3]);
  check_int(4, test48_s.c);

  check_int(1, test48_t.a);
  check_int(2, test48_t.b[0]);
  check_int(3, test48_t.b[1]);
  check_int(4, test48_t.b[2]);
  check_int(5, test48_t.b[3]);
  check_int(6, test48_t.c);

  check_int(1, test48_u.a);
  check_int(2, test48_u.b[0]);
  check_int(3, test48_u.b[1]);
  check_int(4, test48_u.b[2]);
  check_int(5, test48_u.b[3]);
  check_int(6, test48_u.c);

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
  check_int(1, array[0]);
  check_int(2, array[1]);
  check_int(3, array[2]);
  check_int(0, array[3]);

  check_int(1, s.a);
  check_int(2, s.b[0]);
  check_int(3, s.b[1]);
  check_int(0, s.b[2]);
  check_int(0, s.b[3]);
  check_int(4, s.c);

  check_int(1, t.a);
  check_int(2, t.b[0]);
  check_int(3, t.b[1]);
  check_int(4, t.b[2]);
  check_int(5, t.b[3]);
  check_int(6, t.c);

  check_int(1, u.a);
  check_int(2, u.b[0]);
  check_int(3, u.b[1]);
  check_int(4, u.b[2]);
  check_int(5, u.b[3]);
  check_int(6, u.c);

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
  check_int(1, s_array[0]);
  check_int(2, s_array[1]);
  check_int(3, s_array[2]);
  check_int(0, s_array[3]);

  check_int(1, s_s.a);
  check_int(2, s_s.b[0]);
  check_int(3, s_s.b[1]);
  check_int(0, s_s.b[2]);
  check_int(0, s_s.b[3]);
  check_int(4, s_s.c);

  check_int(1, s_t.a);
  check_int(2, s_t.b[0]);
  check_int(3, s_t.b[1]);
  check_int(4, s_t.b[2]);
  check_int(5, s_t.b[3]);
  check_int(6, s_t.c);

  check_int(1, s_u.a);
  check_int(2, s_u.b[0]);
  check_int(3, s_u.b[1]);
  check_int(4, s_u.b[2]);
  check_int(5, s_u.b[3]);
  check_int(6, s_u.c);
}

static int test49_a0[] = {};
static int test49_a4[] = {1, 2, 3, 4};
static int test49_a6[6] = {1, 2, 3, 4};
static int test49_ax[] = {1, [3] = 8, 9};
static void test49(void) {
  check_int(0, sizeof(test49_a0));
  check_int(sizeof(int) * 4, sizeof(test49_a4));
  check_int(sizeof(int) * 6, sizeof(test49_a6));
  check_int(sizeof(int) * 5, sizeof(test49_ax));
  check_int(1, test49_a4[0]);
  check_int(2, test49_a4[1]);
  check_int(3, test49_a4[2]);
  check_int(4, test49_a4[3]);
  check_int(1, test49_a6[0]);
  check_int(2, test49_a6[1]);
  check_int(3, test49_a6[2]);
  check_int(4, test49_a6[3]);
  check_int(0, test49_a6[4]);
  check_int(0, test49_a6[5]);
  check_int(1, test49_ax[0]);
  check_int(0, test49_ax[1]);
  check_int(0, test49_ax[2]);
  check_int(8, test49_ax[3]);
  check_int(9, test49_ax[4]);

  int a0[] = {};
  int a4[] = {1, 2, 3, 4};
  int a6[6] = {1, 2, 3, 4};
  int ax[] = {1, [3] = 8, 9};
  check_int(0, sizeof(a0));
  check_int(sizeof(int) * 4, sizeof(a4));
  check_int(sizeof(int) * 6, sizeof(a6));
  check_int(sizeof(int) * 5, sizeof(ax));
  check_int(1, a4[0]);
  check_int(2, a4[1]);
  check_int(3, a4[2]);
  check_int(4, a4[3]);
  check_int(1, a6[0]);
  check_int(2, a6[1]);
  check_int(3, a6[2]);
  check_int(4, a6[3]);
  check_int(0, a6[4]);
  check_int(0, a6[5]);
  check_int(1, ax[0]);
  check_int(0, ax[1]);
  check_int(0, ax[2]);
  check_int(8, ax[3]);
  check_int(9, ax[4]);

  static int s_a0[] = {};
  static int s_a4[] = {1, 2, 3, 4};
  static int s_a6[6] = {1, 2, 3, 4};
  static int s_ax[] = {1, [3] = 8, 9};
  check_int(0, sizeof(s_a0));
  check_int(sizeof(int) * 4, sizeof(s_a4));
  check_int(1, s_a4[0]);
  check_int(2, s_a4[1]);
  check_int(3, s_a4[2]);
  check_int(4, s_a4[3]);
  check_int(1, s_a6[0]);
  check_int(2, s_a6[1]);
  check_int(3, s_a6[2]);
  check_int(4, s_a6[3]);
  check_int(0, s_a6[4]);
  check_int(0, s_a6[5]);
  check_int(1, s_ax[0]);
  check_int(0, s_ax[1]);
  check_int(0, s_ax[2]);
  check_int(8, s_ax[3]);
  check_int(9, s_ax[4]);
}

static void test50(void) {
  char *s = "abc"
            "def";

  check_int('a', s[0]);
  check_int('b', s[1]);
  check_int('c', s[2]);
  check_int('d', s[3]);
  check_int('e', s[4]);
  check_int('f', s[5]);
  check_int(1, s[0] == 'a');
  check_int(1, s[1] == 'b');
  check_int(1, s[2] == 'c');
  check_int(1, s[3] == 'd');
  check_int(1, s[4] == 'e');
  check_int(1, s[5] == 'f');
}

enum test51_enum {
  TEST51_X,
  TEST51_Y = 3,
  TEST51_Z,
  TEST51_W = 8,
};
static void test51(void) {
  check_int(0, TEST51_X);
  check_int(3, TEST51_Y);
  check_int(4, TEST51_Z);
  check_int(8, TEST51_W);

  enum test51_enum x = TEST51_Z;
  check_int(7, x + 3);

  enum { X1 = 8, Y1, Z1 } z = Y1;
  check_int(9, z);
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

  check_int(0, sc1);
  check_int(1, ss1);
  check_int(2, ss2);
  check_int(3, si1);
  check_int(4, sl1);
  check_int(5, sl2);
  check_int(6, sll1);
  check_int(7, sll2);
  check_int(8, s1);
}

static void test53(void) {
  unsigned x = 5;
  unsigned y = 3;

  check_int(8, x + y);
  check_int(2, x - y);
  check_int(-2, y - x);
  check_int(15, x * y);
  check_int(1, x / y);
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
  check_int(1, ull == uc);
  ull = us;
  check_int(1, ull == us);
  ull = ui;
  check_int(1, ull == ui);
  ull = ul;
  check_int(1, ull == ul);
  ull = ull;
  check_int(1, ull == ull);
  ull = ic;
  check_int(1, ull == ic);
  ull = is;
  check_int(1, ull == is);
  ull = ii;
  check_int(1, ull == ii);
  ull = il;
  check_int(1, ull == il);
  ull = ill;
  check_int(1, ull == ill);

  ill = uc;
  check_int(1, ill == uc);
  ill = us;
  check_int(1, ill == us);
  ill = ui;
  check_int(1, ill == ui);
  ill = ul;
  check_int(1, ill == ul);
  ill = ull;
  check_int(1, ill == ull);
  ill = ic;
  check_int(1, ill == ic);
  ill = is;
  check_int(1, ill == is);
  ill = ii;
  check_int(1, ill == ii);
  ill = il;
  check_int(1, ill == il);
  ill = ill;
  check_int(1, ill == ill);
}

static void test55(void) {
  const char *s = "hogehoge";
  check_int(1, s != 0);

  const int a[4] = {1, 2, 3, 4};
  const int *const p = a;
  const int *const *const pp = &p;
  check_int(1, (*pp)[0]);
  check_int(2, (*pp)[1]);
  check_int(3, (*pp)[2]);
  check_int(4, (*pp)[3]);
}

static int test56_varargs(int a, int b, ...) { return a + b; }
static int test56_noargs() { return 8; }
static void test56(void) {
  check_int(3, test56_varargs(1, 2, 3));
  check_int(8, test56_noargs());
  check_int(8, test56_noargs(1));
  check_int(8, test56_noargs(1, 2));
}

#define TEST57_DEF 123
#define test57_x test57_y
int test57_x = 3;
#undef test57_x
int test57_x = 8;
static void test57(void) {
  check_int(123, TEST57_DEF);
  check_int(8, test57_x);
  check_int(3, test57_y);
}

static void test58(void) {
  check_int(4, sizeof(123));
  check_int(8, sizeof(123L));
  check_int(8, sizeof(123LL));

  check_int(123, 123L);
  check_int(123, 123LL);
  check_int(123, 123l);
  check_int(123, 123ll);

  check_int(123, 123U);
  check_int(123, 123UL);
  check_int(123, 123ULL);
  check_int(123, 123Ul);
  check_int(123, 123Ull);

  check_int(123, 123LU);
  check_int(123, 123LLU);
  check_int(123, 123lU);
  check_int(123, 123llU);

  check_int(123, 123u);
  check_int(123, 123uL);
  check_int(123, 123uLL);
  check_int(123, 123ul);
  check_int(123, 123ull);

  check_int(123, 123Lu);
  check_int(123, 123LLu);
  check_int(123, 123lu);
  check_int(123, 123llu);

  check_int(1, (~0ULL) >> ((sizeof(long long) * 8) - 1));
  check_int(1, (~0U) >> ((sizeof(int) * 8) - 1));

  check_int(1, (0xffffffffffffffff) >> ((sizeof(long long) * 8) - 1));
  check_int(1, (0xffffffffffffffff) > 0);
}

static void test59(void) {
  wchar_t wc = L'x';
  check_int('x', wc);
}

static void test60(void) { check_int(1, 1 ? 1 : 0l); }
static void test61(void) {
  int a[3 + 5] = {};
  check_int(8, sizeof(a) / sizeof(a[0]));
  int i;
  for (i = 0; i < sizeof(a) / sizeof(a[0]); i++) {
    check_int(0, a[i]);
  }
}

static void test62(void) {
  check_int(8, (7, 8));
  int a;
  *(a = 0, &a) = 3;
  check_int(3, a);
}

static int test63_func(int a[100]) {
  check_int(8, sizeof(a));
  int s = 0;
  int i;
  for (i = 0; i < 100; i++) {
    s += a[i];
  }
  return s;
}

static void test63(void) {
  int a[100];
  int i;
  for (i = 0; i < 100; i++) {
    a[i] = i;
  }
  check_int(4950, test63_func(a));
}

static char test64_static[] = "static str";
static void test64(void) {
  char local[] = "local str";
  char local_static[] = "local static str";

  check_int(0, strcmp("static str", test64_static));
  check_int(0, strcmp("local str", local));
  check_int(0, strcmp("local static str", local_static));

  check_int(strlen("static str") + 1, sizeof(test64_static));
  check_int(strlen("local str") + 1, sizeof(local));
  check_int(strlen("local static str") + 1, sizeof(local_static));
}

static void test65(void) {
  const char *s = "ABC"
                  "DEF";
  const char a[] = "GHI"
                   "JKL";
  check_int(0, strcmp(s, "ABCDEF"));
  check_int(0, strcmp(a, "GHIJKL"));
  check_int(7, sizeof(a) / sizeof(a[0]));
}

typedef struct test66_s test66_s;
static void test66(void) {
test66_s:
  return;
}

static int num_check = 0;
static void check_int(int a, int b) {
  if (a != b) {
    printf("FAILED %d != %d\n", a, b);
    abort();
  }
  num_check++;
}

#define TEST(name)                                                             \
  { #name, name }

int main(void) {
  typedef struct Test {
    const char *name;
    void (*func)(void);
  } Test;
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
      TEST(test66), {NULL, NULL},
  };
  int i = 0;
  for (i = 0; test_list[i].name != NULL; i++) {
    Test *test = &test_list[i];
    test->func();
    printf("%s OK (%d assertions)\n", test->name, num_check);
    num_check = 0;
  }

  return 0;
}
