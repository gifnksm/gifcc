#include <ctype.h>
#include <stdlib.h>

#include "9cc.h"

// トークナイズした結果のトークン列はこの配列に保存する
// 100個以上のトークンは来ないものとする
Token tokens[100];

// pが指している文字列をトークンに分割してtokensに保存する
void tokenize(char *p) {
  int i = 0;
  while (*p) {
    // 空白文字をスキップ
    if (isspace(*p)) {
      p++;
      continue;
    }

    if (*p == '=' && *(p + 1) == '=') {
      tokens[i].ty = TK_EQEQ;
      tokens[i].input = p;
      i++;
      p += 2;
      continue;
    }

    if (*p == '!' && *(p + 1) == '=') {
      tokens[i].ty = TK_NOTEQ;
      tokens[i].input = p;
      i++;
      p += 2;
      continue;
    }

    if (*p == '+' || *p == '-' || *p == '*' || *p == '/' || *p == '(' ||
        *p == ')' || *p == '=' || *p == ';') {
      tokens[i].ty = *p;
      tokens[i].input = p;
      i++;
      p++;
      continue;
    }

    if ('a' <= *p && *p <= 'z') {
      tokens[i].ty = TK_IDENT;
      tokens[i].input = p;
      tokens[i].name = *p;
      i++;
      p++;
      continue;
    }

    if (isdigit(*p)) {
      tokens[i].ty = TK_NUM;
      tokens[i].input = p;
      tokens[i].val = strtol(p, &p, 10);
      i++;
      continue;
    }

    error("トークナイズできません: %s\n", p);
  }

  tokens[i].ty = TK_EOF;
  tokens[i].input = p;
}
