#include "cpp.h"

#ifdef FOO
#error "FOO must not be defined"
#endif

#ifndef FOO
#else
#error "FOO must not be defined"
#endif

int main(void) {
  puts("OK");
  return 0;
}
