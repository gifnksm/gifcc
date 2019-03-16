OUTDIR=./target

GEN_HDRS=src/include_path.h
SRCS=$(wildcard src/*.c)

STAGE1_OBJS=$(patsubst src/%.c,$(OUTDIR)/stage1/%.o,$(SRCS))
STAGE1_DEPS=$(patsubst src/%.c,$(OUTDIR)/stage1/%.d,$(SRCS))

STAGE2_TOKEN=$(patsubst src/%.c,$(OUTDIR)/stage2/%.token,$(SRCS))
STAGE2_AST=$(patsubst src/%.c,$(OUTDIR)/stage2/%.ast,$(SRCS))
STAGE2_SEMA=$(patsubst src/%.c,$(OUTDIR)/stage2/%.sema,$(SRCS))
STAGE2_ASMS=$(patsubst src/%.c,$(OUTDIR)/stage2/%.s,$(SRCS))
STAGE2_OBJS=$(patsubst src/%.c,$(OUTDIR)/stage2/%.o,$(SRCS))

STAGE1_CC=$(CC)
STAGE1_CFLAGS=-Wall -Wextra -std=c11 -g3 -D_POSIX_C_SOURCE=201809L -MMD -fdiagnostics-color
STAGE1_GIFCC=target/stage1/gifcc

STAGE2_CC=$(STAGE1_GIFCC)
STAGE2_CFLAGS=
STAGE2_GIFCC=target/stage2/gifcc

LDFLAGS=

ifdef ASAN
  STAGE1_CFLAGS  += -fsanitize=address
  LDFLAGS += -fsanitize=address
  export ASAN_OPTIONS=detect_leaks=0
endif

ifdef COVERAGE
  STAGE1_CFLAGS  += --coverage
  LDFLAGS += --coverage
endif

all: stage1
.PHONY: all

test:
.PHONY: test
test-full:
.PHONY: test-full

stage1: $(STAGE1_GIFCC)
.PHONY: stage1

stage2: $(STAGE2_GIFCC) $(STAGE2_TOKEN) $(STAGE2_AST) $(STAGE2_SEMA)
.PHONY: stage2

stage1-test-gifcc: $(STAGE1_GIFCC)
	$< --test
.PHONY: stage1-test-gifcc
test: stage1-test-gifcc

stage1-test-compile: $(STAGE1_GIFCC)
	$(MAKE) -C test STAGE=stage1 gcc-run
	$(MAKE) -C test STAGE=stage1 run
.PHONY: stage1-test-compile
test: stage1-test-compile

stage1-test-c-testsuite: $(STAGE1_GIFCC)
	./scripts/run_c-testsuite stage1
.PHONY: stage1-test-c-testsuite
test-full: stage1-test-c-testsuite

clean:
	$(RM) -r target $(GEN_HDRS)
.PHONY: clean

format:
	clang-format -i $(SRCS) $(HDRS)
.PHONY: format

clang-tidy:
	clang-tidy -fix -fix-errors $(SRCS) -- $(CFLAGS)
.PHONY: clang-tidy

lcov:
	lcov --capture --directory . --output-file target/coverage.info
	genhtml target/coverage.info --output-directory target/html

$(STAGE1_GIFCC): $(STAGE1_OBJS)
	$(CC) $(LDFLAGS) -o $@ $^
$(OUTDIR)/stage1/%.o: src/%.c $(GEN_HDRS) Makefile | $(OUTDIR)/stage1/
	$(STAGE1_CC) $(STAGE1_CFLAGS) -c -o $@ $<

$(STAGE2_GIFCC): $(STAGE2_OBJS)
	$(CC) $(LDFLAGS) -o $@ $^
$(OUTDIR)/stage2/%.token: src/%.c $(STAGE2_CC) | $(OUTDIR)/stage2/
	$(STAGE2_CC) $(STAGE2_CFLAGS) $< --output token > $@.tmp
	mv $@.tmp $@
$(OUTDIR)/stage2/%.ast: src/%.c $(STAGE2_CC) | $(OUTDIR)/stage2/
	$(STAGE2_CC) $(STAGE2_CFLAGS) $< --output ast > $@.tmp
	mv $@.tmp $@
$(OUTDIR)/stage2/%.sema: src/%.c $(STAGE2_CC) | $(OUTDIR)/stage2/
	$(STAGE2_CC) $(STAGE2_CFLAGS) $< --output sema > $@.tmp
	mv $@.tmp $@
$(OUTDIR)/stage2/%.s: src/%.c $(STAGE2_CC) | $(OUTDIR)/stage2/
	$(STAGE2_CC) $(STAGE2_CFLAGS) $< > $@.tmp
	mv $@.tmp $@
$(OUTDIR)/stage2/%.o: $(OUTDIR)/stage2/%.s
	$(CC) -c -o $@ $<

src/include_path.h: FORCE
	scripts/gen_include_path $@

%/:
	mkdir -p $@

FORCE:
.PHONY: FORCE

-include $(STAGE1_DEPS)
