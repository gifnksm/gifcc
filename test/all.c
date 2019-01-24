int num_check;
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

  test_ok("test01");
}

void test02_01(void) {
  int a;
  int b;
  a = b = 2;
  check_int(4, a + b);
}
void test02_02(void) {
  int a;
  int b;
  a = 3;
  b = 5 * 6 - 8;
  check_int(14, a + b / 2);
}
void test02_03(void) {
  int a;
  int b;
  b = (a = 5);
  check_int(10, a + b);
}
void test02(void) {
  test02_01();
  test02_02();
  test02_03();
  check_int(1, 10 + 2 == 3 * 4);
  check_int(10, 10 + (2 != 1 * 2));
  check_int(12, 10 + ((2 != 1) * 2));

  test_ok("test02");
}

void test03_01(void) {
  int a;
  int b;
  int c;
  a = 3;
  b = 5;
  check_int(0, c = a == b);
}
void test03_02(void) {
  int a;
  int b;
  int c;
  a = 3;
  b = 5;
  check_int(0, c = a == b);
}
void test03_03(void) {
  int a;
  int b;
  int c;
  int d;
  int e;
  int f;
  int g;
  int h;
  int i;
  int j;
  int k;
  int l;
  int m;
  int n;
  int o;
  int p;
  int q;
  int r;
  int s;
  int t;
  int u;
  int v;
  int w;
  int x;
  int y;
  int z;
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
  int a;
  int c;
  int d;
  a = 1;
  c = 2;
  d = 3;
  check_int(6, a + c + d);
}
void test03_06(void) {
  int a;
  int ab;
  int abc;
  a = 1;
  ab = 2;
  abc = 3;
  check_int(6, a + ab + abc);
}
void test03_07(void) {
  int _;
  int a_;
  _ = 123;
  a_ = 1;
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

  test_ok("test03");
}

void test04(void) {
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
  check_int(0, 0 && abort());
  check_int(1, 3 || 5);
  check_int(1, 3 || abort());
  check_int(1, 0 || 1);
  check_int(0, 0 || 0);
  check_int(12, 1 ? 12 : 34);
  check_int(34, 0 ? 12 : 34);
  check_int(3, 8 % 5);
  check_int(-4, ~3);
  check_int(0, !1);
  check_int(0, !3);
  check_int(1, !0);

  test_ok("test04");
}

void test05(void) {
  int a;
  int b;

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

  test_ok("test05");
}

void test06(void) {
  1;
  ;
  2;
  ;
  ;
  ;
  check_int(3, 3);

  test_ok("test06");
}

void test07(void) {
  int a;
  int b;
  int c;
  int s;

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

  test_ok("test07");
}

void test08(void) {
  int a;
  int *p;

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

  test_ok("test08");
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
  int c;
  int i;
  c = 0;
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
  int i;
  int s;
  i = 3;
  s = 0;
  goto INIT;
  for (i = 0; i < 5; i++) {
  INIT:
    s += i;
  }
  check_int(7, s);
}
void test09(void) {
  check_int(3, test09_01(3));
  check_int(0, test09_01(0));
  check_int(255, test09_01(8));
  test09_02();
  test09_03();

  test_ok("test09");
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

  test_ok("test10");
}

int test11_01(void) {
  if (1)
    return 3;
  abort();
}
void test11(void) {
  check_int(3, test11_01());
  test_ok("test11");
}

int ret3() { return 3; }
int ret8() { return 8; }
int add_38(void) { return ret3() + ret8(); }
int to_double(int n) { return 2 * n; }
int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }
int fib(int n) { return (n <= 0) ? 0 : (n == 1) ? 1 : fib(n - 2) + fib(n - 1); }
void test12(void) {
  int a;
  int b;
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

  test_ok("test12");
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

  test_ok("test13");
}

void test14(void) {
  int a;
  int *b;
  int **c;
  int ***d;

  test_ok("test14");
}

void test15(void) {
  int *p;
  int *q;
  int i;

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

  test_ok("test15");
}

int main(void) {
  num_check = 0;
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

  return 0;
}
