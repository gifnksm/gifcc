void check_int(int a, int b) {
  if (a != b) {
    printf("%d != %d\n", a, b);
    abort();
  }
}

void test_ok(char *name) { printf("%s OK\n", name); }

void test01(void) {
  check_int(0, 0);
  test_ok("test01");
}

int main(void) {
  test01();
  return 0;
}
