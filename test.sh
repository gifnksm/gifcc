#!/bin/bash

index=0

compiletest() {
    filename="${1}"
    name=$(basename "${filename%%.c}")

    for kind in token ast; do
        target/gifcc --output "${kind}" "${filename}" > target/test/${name}."${kind}"
        if [ "$?" -ne 0 ]; then
            echo "test [${name}]: gifcc(${kind}) failed"
            exit 1
        fi
    done

    target/gifcc "${filename}" > target/test/${name}.s
    if [ "$?" -ne 0 ]; then
        echo "test [${name}]: gifcc(asm) failed"
        exit 1
    fi

    gcc -o target/test/${name} target/test/${name}.s
    if [ "$?" -ne 0 ]; then
        echo "test [${name}]: gcc failed"
        exit 1
    fi
    target/test/${name}
    if [ "$?" -ne 0 ]; then
        echo "test [${name}]: exec failed"
        exit 1
    fi

    echo "test [${name}]: OK"
}

mkdir -p target/test
for src in test/*.c; do
    compiletest "${src}"
done

try() {
  expected="$1"
  input="$2"

  echo "test #${index}:"
  echo "${input}" | sed 's/^/  /'

  mkdir -p target/test
  echo "${input}" | target/gifcc --output token > target/test/${index}.token
  if [ "$?" -ne 0 ]; then
    echo "test #${index}: gifcc(token) failed"
    exit 1
  fi
  echo "${input}" | target/gifcc --output ast > target/test/${index}.ast
  if [ "$?" -ne 0 ]; then
    echo "test #${index}: gifcc(ast) failed"
    exit 1
  fi
  echo "${input}" | target/gifcc > target/test/${index}.s
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

INPUT=a try 97 '
int main(void) {
  return getchar();
}
'
INPUT='aBc
' try 4 '
int main(void) {
  return (getchar() == 97)
    + (getchar() == 66)
    + (getchar() == 99)
    + (getchar() == 10);
}
'
try 10 '
int main(void) {
  putchar(97);
  putchar(66);
  putchar(99);
  return putchar(10);
}
'
INPUT='xy' try 10 '
int main(void) {
  int a;
  int c;
  a = getchar();
  putchar(a);
  putchar(getchar());
  c = a + 2;
  putchar(c);
  return putchar(10);
}
'

try 3 '
int main(void) {
  int a[2];
  *a = 1;
  *(a + 1) = 2;
  int *p;
  p = a;
  return *p + *(p + 1);
}
'
try 12 '
int main(void) {
  int a[3];
  a[0] = 5;
  a[1] = 4;
  a[2] = 3;
  return a[0] + a[1] + a[2];
}
'
try 2 '
int main(void) {
  int a[3];
  return &a[2] - &a[0];
}
'
try 12 '
int main(void) {
  int a[3];
  0[a] = 5;
  1[a] = 4;
  2[a] = 3;
  return 0[a] + 1[a] + 2[a];
}
'
try 8 '
int *func(int *x) {
  return x + 1;
}
int main(void) {
  int a[4];
  a[0] = 1;
  a[1] = 2;
  a[2] = 4;
  a[3] = 8;
  return func(a)[2];
}
'
try 1 '
void foo(void) {}
void *bar(void *p) { return p; }
int main(void) {
  void *p;
  void *q;
  p = 3;
  q = bar(p);
  return p == q;
}
'
try 100 '
  int g;
  int main(void) {
    g = 100;
    return g;
  }
'
try 0 '
int g;
int h[3];
int init(void) {
  g = 3;
  h[0] = 1;
  h[1] = 2;
  h[2] = 3;
}
int add(int n) {
  g += n;
  h[0] *= h[0];
  h[1] *= h[1];
  h[2] *= h[2];
}
int main(void) {
  init();
  add(7);
  if (g != 10) {
    abort();
  }
  if (h[0] != 1) {
    abort();
  }
  if (h[1] != 4) {
    abort();
  }
  if (h[2] != 9) {
    abort();
  }
  return 0;
}
'
try 4 "
int main(void) {
  char x[4];
  x[0] = 'a';
  x[1] = 'b';
  x[2] = 'c';
  x[3] = '\0';
  return puts(x);
}
"
try 3 '
int main(void) {
  char x[3];
  x[0] = -1;
  x[1] = 2;
  int y;
  y = 4;
  return x[0] + y;
}
'
try 3 '
int main(void) {
  puts("hogehoge");
  puts("\n\n\r\r");
  puts("ほげほげ");
  return 3;
}
'
try 0 '
int main(void) {
  printf("%s %d\n", "123", 456);
  return 0;
}
'

echo OK
