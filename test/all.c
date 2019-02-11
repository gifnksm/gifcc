void abort(void);

int num_check = 0;
void check_int(int a, int b) {
  if (a != b) {
    printf("FAILED %d != %d\n", a, b);
    abort();
  }
  num_check++;
}

void test_ok(char *name) {
  printf("%s OK (%d assertions)\n", name, num_check);
  num_check = 0;
}

void test01(void) {
  check_int(0, 0);
  check_int(42, 42);
  check_int(21, 5 + 20 - 4);
  check_int(41, 12 + 34 - 5);
  check_int(47, 5 + 6 * 7);
  check_int(15, 5 * (9 - 6));
  check_int(4, (3 + 5) / 2);

  test_ok(__func__);
}

void test02_01(void) {
  int a = 2, b = 2;
  check_int(4, a + b);
  return;
}
void test02_02(void) {
  int a = 3;
  int b = 5 * 6 - 8;
  check_int(14, a + b / 2);
  return;
}
void test02_03(void) {
  int a, b;
  b = (a = 5);
  check_int(10, a + b);
  return;
}
void test02(void) {
  test02_01();
  test02_02();
  test02_03();
  check_int(1, 10 + 2 == 3 * 4);
  check_int(10, 10 + (2 != 1 * 2));
  check_int(12, 10 + ((2 != 1) * 2));

  test_ok(__func__);
}

void test03_01(void) {
  int c;
  int a = 3;
  int b = 5;
  check_int(0, c = a == b);
}
void test03_02(void) {
  int c;
  int a = 3;
  int b = 5;
  check_int(0, c = a == b);
}
void test03_03(void) {
  int a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y,
      z;
  a = b = c = d = e = f = g = h = i = j = k = l = m = n = o = p = q = r = s =
      t = u = v = w = x = y = z = 1;
  check_int(a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q +
                r + s + t + u + v + w + x + y + z,
            26);
}
void test03_04(void) {
  1;
  2;
  3;
  4;
  5;
  6;
  7;
  8;
  9;
  10;
  11;
  12;
  13;
  14;
  15;
  16;
  17;
  18;
  19;
  20;
  21;
  22;
  23;
  24;
  25;
  26;
  27;
  28;
  29;
  30;
  31;
  32;
  33;
  34;
  35;
  36;
  37;
  38;
  39;
  40;
  41;
  42;
  43;
  44;
  45;
  46;
  47;
  48;
  49;
  50;
  51;
  52;
  53;
  54;
  55;
  56;
  57;
  58;
  59;
  60;
  61;
  62;
  63;
  64;
  65;
  66;
  67;
  68;
  69;
  70;
  71;
  72;
  73;
  74;
  75;
  76;
  77;
  78;
  79;
  80;
  81;
  82;
  83;
  84;
  85;
  86;
  87;
  88;
  89;
  90;
  91;
  92;
  93;
  94;
  95;
  96;
  97;
  98;
  99;
  100;
  101;
  102;
  103;
  104;
  105;
  106;
  107;
  108;
  109;
  110;
  111;
  112;
  113;
  114;
  115;
  116;
  117;
  118;
  119;
  check_int(120, 120);
}
void test03_05(void) {
  int a = 1, c = 2, d = 3;
  check_int(6, a + c + d);
}
void test03_06(void) {
  int a = 1, ab = 2, abc = 3;
  check_int(6, a + ab + abc);
}
void test03_07(void) {
  int _ = 123, a_ = 1;
  check_int(124, _ + a_);
}
void test03(void) {
  test03_01();
  test03_02();
  test03_03();
  test03_04();
  test03_05();
  test03_06();
  test03_07();

  test_ok(__func__);
}

void test04(void) {
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

  test_ok(__func__);
}

void test05(void) {
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

  test_ok(__func__);
}

void test06(void) {
  1;
  ;
  2;
  ;
  ;
  ;
  check_int(3, 3);

  test_ok(__func__);
}

void test07(void) {
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

  test_ok(__func__);
}

void test08(void) {
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

  test_ok(__func__);
}

int test09_01(int n) {
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
void test09_02(void) {
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
void test09_03(void) {
  int i = 3, s = 0;
  goto INIT;
  for (i = 0; i < 5; i++) {
  INIT:
    s += i;
  }
  check_int(7, s);
}
void test09_04(void) {
  int x = 5;
  switch (1)
  case 0:
    abort();
  check_int(5, x);
}
void test09(void) {
  check_int(3, test09_01(3));
  check_int(0, test09_01(0));
  check_int(255, test09_01(8));
  test09_02();
  test09_03();
  test09_04();

  test_ok(__func__);
}

void test10(void) {
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

  test_ok(__func__);
}

int test11_01(void) {
  if (1)
    return 3;
  abort();
}
void test11(void) {
  check_int(3, test11_01());
  test_ok(__func__);
}

int ret3() { return 3; }
int ret8() { return 8; }
int add_38(void) { return ret3() + ret8(); }
int to_double(int n) { return 2 * n; }
int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }
int fib(int n) { return (n <= 0) ? 0 : (n == 1) ? 1 : fib(n - 2) + fib(n - 1); }
void test12(void) {
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

  test_ok(__func__);
}

int add6(int a, int b, int c, int d, int e, int f) {
  return a + b + c + d + e + f;
}
int digits6(int a, int b, int c, int d, int e, int f) {
  return ((((((a * 10) + b) * 10 + c) * 10 + d) * 10 + e) * 10 + f);
}
int add10(int a, int b, int c, int d, int e, int f, int g, int h, int i,
          int j) {
  return a + b + c + d + e + f + g + h + i + j;
}
int digits10(int a, int b, int c, int d, int e, int f, int g, int h, int i,
             int j) {
  return (
      (((((((((a * 10) + b) * 10 + c) * 10 + d) * 10 + e) * 10 + f) * 10 + g) *
            10 +
        h) *
           10 +
       i) *
          10 +
      j);
}

void test13(void) {
  check_int(123456, digits6(1, 2, 3, 4, 5, 6));
  check_int(21, add6(1, 2, 3, 4, 5, 6));
  check_int(1234567890, digits10(1, 2, 3, 4, 5, 6, 7, 8, 9, 0));
  check_int(45, add10(1, 2, 3, 4, 5, 6, 7, 8, 9, 0));

  test_ok(__func__);
}

void test14(void) {
  int a, *b, **c, ***d;

  test_ok(__func__);
}

void test15(void) {
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

  test_ok(__func__);
}

void test16(void) {
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

  test_ok(__func__);
}

int *test17_01(int *x) { return x + 1; }
void test17_02(void) {}
void *test17_03(void *p) { return p; }
void test17(void) {
  int a[4];

  a[0] = 1;
  a[1] = 2;
  a[2] = 4;
  a[3] = 8;
  check_int(8, test17_01(a)[2]);

  void *p, *q;
  p = 3;
  q = test17_03(p);
  check_int(1, p == q);

  test_ok(__func__);
}

int test18_g;
int test18_h[3];
void test18_init(void) {
  test18_g = 3;
  test18_h[0] = 1;
  test18_h[1] = 2;
  test18_h[2] = 3;
}

int test18_add(int n) {
  test18_g += n;
  test18_h[0] *= test18_h[0];
  test18_h[1] *= test18_h[1];
  test18_h[2] *= test18_h[2];
}
void test18(void) {
  test18_g = 100;
  check_int(test18_g, 100);

  test18_init();
  test18_add(7);
  check_int(10, test18_g);
  check_int(1, test18_h[0]);
  check_int(4, test18_h[1]);
  check_int(9, test18_h[2]);

  test_ok(__func__);
}

void test19(void) {
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

  test_ok(__func__);
}

void test20(void) {
  puts("hogehoge");
  puts("\n\n\r\r");
  puts("ほげほげ");

  printf("%s %d\n", "abc", 456);

  test_ok(__func__);
}

void test21(void) {
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

  test_ok(__func__);
}

int test22_g;
void test22_func(int n) { test22_g = n; }
void test22(void) {
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

  test_ok(__func__);
}

void test23_fun(int a, int b) {
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
void test23(void) {
  test23_fun(1, 2);

  test_ok(__func__);
}

void test24(void) {
  char a = 64, b = a, c = a, d = a;
  check_int(256, a + b + c + d);

  check_int(256, (char)64 + (char)64 + (char)64 + (char)64);
  check_int(64, (char)320);
  check_int(256, (char)320 + (char)320 + (char)320 + (char)320);

  test_ok(__func__);
}

void test25(void) {
  // comment
  /* comment */
  test_ok(__func__);
}

struct Test26 {
  int z;
};
struct Test26 test26_x;
struct {
  int y;
} test26_y, test26_z;
void test26(void) {
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
  test_ok(__func__);
}

typedef int test27_type;
void test27(void) {
  typedef int x;
  typedef int y(int x);
  typedef int z[3];

  x a;
  y *b;
  z c[5];

  a = 3;
  b = 0;
  c[0][0] = 1;

  test_ok(__func__);
}

int test28_foo(int, int, int);
int test28_foo(int a, int b, int c) { return a + b + c; }
void test28(void) {
  check_int(12, test28_foo(3, 4, 5));

  test_ok(__func__);
}

void test29(void) {
  int a[7];
  struct Foo {
    int a[9];
  };

  check_int(4, sizeof(int));
  check_int(4, sizeof 3);
  check_int(4, sizeof(3 + 5));
  check_int(28, sizeof a);
  check_int(36, sizeof(struct Foo));

  test_ok(__func__);
}

void test30(void) {
  union {
    int a;
    int b;
  } u;

  u.a = 1;
  check_int(1, u.a);

  u.b = 2;

  check_int(2, u.a);
  check_int(2, u.b);

  test_ok(__func__);
}

int test31_x = 3;
void test31(void) {
  check_int(3, test31_x);
  test_ok(__func__);
}

long test32_x = 8;
long *test32_p = &test32_x;
char test32_fun_char(void) { return 0; }
short test32_fun_short(void) { return 0; }
int test32_fun_int(void) { return 0; }
long test32_fun_long(void) { return 0; }
int *test32_fun_ptr(void) { return 0; }
void test32(void) {
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
  check_int(8, sizeof &test32_fun_char);
  check_int(2, sizeof test32_fun_short());
  check_int(8, sizeof &test32_fun_int);
  check_int(4, sizeof test32_fun_int());
  check_int(8, sizeof &test32_fun_long);
  check_int(8, sizeof test32_fun_long());
  check_int(8, sizeof &test32_fun_ptr);
  check_int(8, sizeof test32_fun_ptr());

  test_ok(__func__);
}

void test33(void) {
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

  test_ok(__func__);
}

void test34(void) {
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

  test_ok(__func__);
}

void test35(void) {
  char cx = 0;
  cx = cx + 1;
  check_int(1, cx);

  short sx = 0;
  sx = sx + 1;
  check_int(1, sx);

  test_ok(__func__);
}

int test36_f(int a), test36_g(int a), a;
void test36(void) {
  check_int(0, test36_f(1) - test36_g(1));
  test_ok(__func__);
}
int test36_f(int a) { return a; }
int test36_g(int a) { return a; }

struct {
  int a;
  int b;
  char c;
  int d;
  short e;
  void *f;
} test37_s = {1, 2, 3, 4, 5, 6};
struct {
  int a;
  int b;
  char c;
  int d;
  short e;
  void *f;
} test37_t = {{{1}}, {2}, {3}, {{{4}}}, {5}, 6};
void test37(void) {
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

  test_ok(__func__);
}

union {
  int a;
  int b;
  char c;
  int d;
  short e;
} test38_u = {1};
void test38(void) {
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

  test_ok(__func__);
}

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
} test39_s = {1, 2, {3}, {4, 5}};
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
} test39_t = {21, 22, 23, 24, 25};

void test39(void) {
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

  test_ok(__func__);
}

void test40(void) {
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

  test_ok(__func__);
}

long test41_f(void) { return 111; }
void test41(void) {
  check_int(111, test41_f());
  check_int(8, sizeof(test41_f()));

  long (*ptr)(void) = test41_f;
  check_int(111, ptr());
  check_int(8, sizeof(ptr()));

  test_ok(__func__);
}

extern int test42_n;
extern int test42_f(void);
void test42(void) {
  extern int test42_m;
  extern int test42_g(void);
  check_int(8, test42_n);
  check_int(32, test42_f());
  check_int(10, test42_m);
  check_int(64, test42_g());

  test_ok(__func__);
}
int test42_n;
int test42_n = 8;
int test42_n;
int test42_m;
int test42_m = 10;
int test42_m;
int test42_f(void) { return 32; }
int test42_g(void) { return 64; }

void test43(void) {
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

  test_ok(__func__);
}

void test44(void) {
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

  test_ok(__func__);
}

int main(void) {
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
  test09();
  test10();
  test11();
  test13();
  test14();
  test15();
  test16();
  test17();
  test18();
  test19();
  test20();
  test21();
  test22();
  test23();
  test24();
  test25();
  test26();
  test27();
  test28();
  test29();
  test30();
  test31();
  test32();
  test33();
  test34();
  test35();
  test36();
  test37();
  test38();
  test39();
  test40();
  test41();
  test42();
  test43();
  test44();

  return 0;
}
