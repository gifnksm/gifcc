#!/bin/bash

try() {
  expected="$1"
  input="$2"

  ./9cc "${input}" > tmp.s
  if [ "$?" -ne 0 ]; then
    echo "9cc failed"
    exit 1
  fi
  gcc -o tmp tmp.s
  if [ "$?" -ne 0 ]; then
    echo "gcc failed"
    exit 1
  fi
  ./tmp
  actual="$?"

  if [ "${actual}" = "${expected}" ]; then
    echo "${input} => ${actual}"
  else
    echo "${expected} expected, but got ${actual}"
    exit 1
  fi
}

try 0 0
try 42 42
try 21 '5+20-4'
try 41 ' 12 + 34 - 5 '
try 47 '5 + 6 * 7'
try 15 '5 * (9 - 6)'
try 4 '(3 + 5) / 2'
try 4 'a = b = 2; a + b'
try 14 '
  a = 3;
  b = 5 * 6 - 8;
  a + b / 2;
'
try 10 'b = (a = 5); a + b;'
try 1 '10 + 2 == 3 * 4'
try 10 '10 + (2 != 1 * 2)'
try 12 '10 + ((2 != 1) * 2)'
try 0 '
  a = 3;
  b = 5;
  c = a == b;
'
try 0 '
  a = 3;
  b = 5;
  c = a == b;
'
try 26 '
  a = b = c = d = e = f = g = h = i = j = k = l = m = n = o = p = q = r = s = t = u = v = w = x = y = z = 1;
  a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s + t + u + v + w + x + y + z;
'
echo OK
