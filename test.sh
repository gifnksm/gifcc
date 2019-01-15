#!/bin/bash

index=0

try() {
  expected="$1"
  input="$2"

  echo "test #${index}:"
  echo "${input}" | sed 's/^/  /'

  mkdir -p target/test
  target/gifcc --output token "${input}" > target/test/${index}.token
  if [ "$?" -ne 0 ]; then
    echo "test #${index}: gifcc(token) failed"
    exit 1
  fi
  target/gifcc --output ast "${input}" > target/test/${index}.ast
  if [ "$?" -ne 0 ]; then
    echo "test #${index}: gifcc(ast) failed"
    exit 1
  fi
  target/gifcc "${input}" > target/test/${index}.s
  if [ "$?" -ne 0 ]; then
    echo "test #${index}: gifcc(asm) failed"
    exit 1
  fi
  gcc -o target/test/${index} target/test/${index}.s
  if [ "$?" -ne 0 ]; then
    echo "test #${index}: gcc failed"
    exit 1
  fi
  echo -n "$INPUT" | target/test/${index}
  actual="$?"

  if [ "${actual}" = "${expected}" ]; then
    echo "  => ${actual}"
  else
    echo "test #${index}: ${expected} expected, but got ${actual}"
    exit 1
  fi

  (( index += 1 ))
}

try 0 '{return 0;}'
try 42 '{return 42;}'
try 21 '{return 5+20-4;}'
try 41 '{ return 12 + 34 - 5 ;}'
try 47 '{return 5 + 6 * 7;}'
try 15 '{return 5 * (9 - 6);}'
try 4 '{return (3 + 5) / 2;}'
try 4 '{a = b = 2; return a + b;}'
try 14 '
{
  a = 3;
  b = 5 * 6 - 8;
  return a + b / 2;
}
'
try 10 '{b = (a = 5); return a + b;}'
try 1 '{return 10 + 2 == 3 * 4;}'
try 10 '{return 10 + (2 != 1 * 2);}'
try 12 '{return 10 + ((2 != 1) * 2);}'
try 0 '
{
  a = 3;
  b = 5;
  return c = a == b;
}
'
try 0 '
{
  a = 3;
  b = 5;
  return c = a == b;
}
'
try 26 '
{
  a = b = c = d = e = f = g = h = i = j = k = l = m = n = o = p = q = r = s = t = u = v = w = x = y = z = 1;
  return a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s + t + u + v + w + x + y + z;
}
'
try 120 '
{
  1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40;
  41;42;43;44;45;46;47;48;49;50;51;52;53;54;55;56;57;58;59;60;61;62;63;64;65;66;67;68;69;70;71;72;73;74;75;76;77;78;79;80;
  81;82;83;84;85;86;87;88;89;90;91;92;93;94;95;96;97;98;99;100;101;102;103;104;105;106;107;108;109;110;111;112;113;114;115;116;117;118;119; return 120;
}
'
try 6 '
{
  a=1;c=2;d=3;
  return a+c+d;
}
'
try 6 '
{
  a = 1;
  ab = 2;
  abc = 3;
  return a + ab + abc;
}
'
try 124 '
{
  _ = 123;
  a_ = 1;
  return _ + a_;
}
'
INPUT=a try 97 '
{
  return getchar();
}
'
INPUT='aBc
' try 4 '
{
  return (getchar() == 97)
    + (getchar() == 66)
    + (getchar() == 99)
    + (getchar() == 10);
}
'
try 10 '
{
  putchar(97);
  putchar(66);
  putchar(99);
  return putchar(10);
}
'
INPUT='xy' try 10 '
{
  a = getchar();
  putchar(a);
  putchar(getchar());
  c = a + 2;
  putchar(c);
  return putchar(10);
}
'
try 8 '{return 1 << 2 << 1;}'
try 2 '{return 1 << 2 >> 1;}'
try 0 '{return 1 >> 3;}'
try 255 '{return 0 - 1 >> 3;}'
try 1 '{return 3 < 5;}'
try 0 '{return 3 > 5;}'
try 1 '{return 3 <= 5;}'
try 0 '{return 3 >= 5;}'
try 0 '{return 3 < 3;}'
try 0 '{return 3 > 3;}'
try 1 '{return 3 <= 3;}'
try 1 '{return 3 >= 3;}'
try 1 '{return 5 & 3;}'
try 6 '{return 5 ^ 3;}'
try 7 '{return 5 | 3;}'
try 1 '{return 3 && 5;}'
try 0 '{return 3 && 0;}'
try 0 '{return 0 && abort();}'
try 1 '{return 3 || 5;}'
try 1 '{return 3 || abort();}'
try 1 '{return 0 || 1;}'
try 0 '{return 0 || 0;}'
try 12 '{return 1 ? 12 : 34;}'
try 34 '{return 0 ? 12 : 34;}'
try 3 '{return 8 % 5;}'
try 103 '{a = +3; return 100 + a;}'
try 97 '{a = -3; return 100 + a;}'
try 252 '{return ~3;}'
try 3 '{a = 2; return ++a;}'
try 33 '{a = 2; b = ++a; return 10 * a + b;}'
try 1 '{a = 2; return --a;}'
try 11 '{a = 2; b = --a; return 10 * a + b;}'
try 2 '{a = 2; return a++;}'
try 32 '{a = 2; b = a++; return 10 * a + b;}'
try 2 '{a = 2; return a--;}'
try 12 '{a = 2; b = a--; return 10 * a + b;}'
try 3 '{1;;2;;;; return 3;}'
try 8 '{{ a = 1; b = 1; return a + b + 6; }}'
try 8 '{a = b = 1; if (1) { a = 5; b = 3; } return a + b;}'
try 2 '{a = b = 1; if (0) { a = 5; b = 3; } return a + b;}'
try 8 '{a = b = 1; if (1) { a = 5; b = 3; } else { a = 3; b = 2; } return  a + b;}'
try 5 '{a = b = 1; if (0) { a = 5; b = 3; } else { a = 3; b = 2; } return a + b;}'
try 8 '{a = b = 1; if (1) a = 5; b = 3; return a + b;}'
try 4 '{a = b = 1; if (0) a = 5; b = 3; return a + b;}'
try 7 '{a = b = 1; if (1) a = 5; else a = 3; b = 2; return a + b;}'
try 5 '{a = b = 1; if (0) a = 5; else a = 3; b = 2; return a + b;}'
try 5 '{a = 0; while (a < 5) { a++; } return a;}'
try 10 '{a = b = 0; while (a < 5) { b = b + 2; a++; } return b;}'
try 10 '{c = 97; while (c <= 122) { putchar(c); c++; }; return putchar(10); }'
try 10 '{a = 0; do a = 10; while (0); return a;}'
try 10 '{a = 0; do a++; while (a < 10); return a;}'
try 55 '{s = 0; for (a = 1; a <= 10; a++) s = s + a; return s;}'
try 55 '{a = 1; s = 0; for (; a <= 10; a++) s = s + a; return s;}'
try 55 '{a = 1; s = 0; for (; a <= 10;) { s = s + a; a++; } return s;}'
try 45 '{a = 0; s = 0; while (1) { a++; if (a >= 10) break; s = s + a; } return s;}'
try 45 '{a = 0; s = 0; do { a++; if (a >= 10) break; s = s + a; } while (1); return s;}'
try 45 '{a = 0; s = 0; for (;;) { a++; if (a >= 10) break; s = s + a; } return s;}'
try 30 '{a = 0; s = 0; while (a < 10) { a++; if (a % 2) continue; s = s + a; } return s;}'
try 30 '{a = 0; s = 0; do { a++; if (a % 2) continue; s = s + a; } while (a < 10); return s;}'
try 30 '{a = 0; s = 0; for (a = 1; a <= 10; a++) { if (a % 2) continue; s = s + a; } return s;}'
try 0 '{return !1;}'
try 0 '{return !3;}'
try 1 '{return !0;}'
try 12 '{a = 4; a *= 3; return a;}'
try 1 '{a = 5; a /= 3; return a;}'
try 2 '{a = 5; a %= 3; return a;}'
try 7 '{a = 4; a += 3; return a;}'
try 1 '{a = 4; a -= 3; return a;}'
try 32 '{a = 4; a <<= 3; return a;}'
try 4 '{a = 32; a >>= 3; return a;}'
try 1 '{a = 5; a &= 3; return a;}'
try 7 '{a = 5; a |= 3; return a;}'
try 6 '{a = 5; a ^= 3; return a;}'
try 3 '{return 5, 4, 3;}'
try 1 '{return &a == &a;}'
try 1 '{return &a != 0;}'
try 8 '{a = 5; b = &a; *b += 3; return a;}'
try 3 '
{
  switch (3) {
  case 0: exit(0);
  case 3: exit(3);
  default: exit(255);
  }
  abort();
}
'
try 0 '
{
  switch (0) {
  case 0: exit(0);
  case 3: exit(3);
  default: exit(255);
  }
  abort();
}
'
try 255 '
{
  switch (8) {
  case 0: exit(0);
  case 3: exit(3);
  default: exit(255);
  }
  abort();
}
'
try 5 '
{
  c = 0;
  for (i = 0; i < 5; i++) {
    switch (i) {
      case 3:
        c += 1;
        if (i != 3) abort();
        break;
      case 0:
      case 1:
        c += 1;
        if (i != 0 && i != 1) abort();
        break;
      case 5:
        abort();
      default:
        c += 1;
        if (i != 2 && i != 4) abort();
        break;
    }
  }
  exit(c);
  abort();
}
'
try 7 '
{
  i = 3;
  s = 0;
  goto INIT;
  for (i = 0; i < 5; i++) {
    INIT:
      s += i;
  }
  exit(s);
  abort();
}
'
try 255 '{return 0xff;}'
try 16 '{return 0x10;}'
try 255 '{return 0377;}'
try 8 '{return 010;}'
try 255 '{return 255;}'
try 10 '{return 10;}'
try 97 "{return 'a';}"
try 10 "{return '\n';}"
try 10 "{return '\xa';}"
try 10 "{return '\x0a';}"
try 10 "{return '\x00a';}"
try 1 "{return '\1';}"
try 8 "{return '\10';}"
try 8 "{return '\010';}"
try 3 '{ if (1) return 3; abort(); }'

echo OK
