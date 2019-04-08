# gifcc [![CircleCI](https://circleci.com/gh/gifnksm/gifcc.svg?style=svg)](https://circleci.com/gh/gifnksm/gifcc) [![codecov](https://codecov.io/gh/gifnksm/gifcc/branch/master/graph/badge.svg)](https://codecov.io/gh/gifnksm/gifcc)

"gifcc" is a self-hosted C compiler written by gifnksm.

## Features

* self-hosted
* Many C11 features
* Some GNU syntax extension

## Compiler stages

gifcc build process has 3 stages.

* stage1: gifcc binary built with system's C compiler (gcc)
* stage2: gifcc binary built with stage1 gifcc
* stage3: gifcc binary built with stage2 gifcc

If gifcc compiles C source code correctly, stage2 binary and stage3 binary will be identical.

## Requirements

* C development tools (gcc, GNU Make, ...)
* python3
* [TMSU](https://github.com/oniony/TMSU) (for running c-testsuite)

## Operations

* Build stage1, 2, 3 compiler

   ```console
   $ make stage1
   ...
   $ make stage2
   ...
   $ make stage3
   ...
   ```

* Run basic tests for each stages

    ```console
    $ make stage1-test
    ...
    $ make stage2-test
    ...
    $ make stage3-test
    ...
    ```

* Run all tests for each stages

    ```console
    $ make stage1-test-full
    ...
    $ make stage2-test-full
    ...
    $ make stage3-test-full
    ...
    ```

## Special Thanks

* [低レイヤを知りたい人のためのCコンパイラ作成入門](https://www.sigbus.info/compilerbook)
* [できる！　コンパイラ作成の資料まとめ](https://anqou.net/poc/2019/01/03/post-2650/)
* [Compiler Explorer](https://godbolt.org/)
* [Wandbox](https://wandbox.org/)
* [Dave Prosser's C Preprocessing Algorithm](https://www.spinellis.gr/blog/20060626/)
* [c-testsuite](https://github.com/c-testsuite/c-testsuite)

## References

* [Programming languages — C](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1548.pdf)
* [x86 and amd64 instruction reference](https://www.felixcloutier.com/x86/index.html)
* [System V Application Binary Interface](https://www.uclibc.org/docs/psABI-x86_64.pdf)
* [GNU as](https://sourceware.org/binutils/docs/as/)
