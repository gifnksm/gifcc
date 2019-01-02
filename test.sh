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
try 120 '
  1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40;
  41;42;43;44;45;46;47;48;49;50;51;52;53;54;55;56;57;58;59;60;61;62;63;64;65;66;67;68;69;70;71;72;73;74;75;76;77;78;79;80;
  81;82;83;84;85;86;87;88;89;90;91;92;93;94;95;96;97;98;99;100;101;102;103;104;105;106;107;108;109;110;111;112;113;114;115;116;117;118;119;120;
'
echo OK
