#include "9cc.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// エラーを報告するための関数
void error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  va_end(ap);
  exit(1);
}

int main(int argc, char **argv) {
  if (argc != 2) {
    error("引数の個数が正しくありません");
  }

  if (strcmp(argv[1], "-test") == 0) {
    runtest();
    exit(0);
  }

  // トークナイズしてパースする
  // 結果はcodeに保存される
  tokenize(argv[1]);
  program();

  // アセンブリの前半部分を出力
  printf(".intel_syntax noprefix\n");
  printf(".global main\n");
  printf("main:\n");

  // プロローグ
  // スタックサイズ分の領域を確保する
  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");
  printf("  sub rsp, %d\n", get_stack_size());

  // 先頭の式から順にコード生成
  for (int i = 0; get_node(i); i++) {
    gen(get_node(i));

    // 式の評価結果としてスタックに一つの値が残っている
    // はずなので、スタックが溢れないようにポップしておく
    printf("  pop rax\n");
  }

  // エピローグ
  // 最後の式の結果がRAXに残っているのでそれが返り値になる
  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
  return 0;
}
