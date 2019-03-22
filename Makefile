OUTDIR=./target

GEN_HDRS=src/include_path.h
SRCS=$(wildcard src/*.c)

STAGE1_OBJS=$(patsubst src/%.c,$(OUTDIR)/stage1/%.o,$(SRCS))
STAGE1_DEPS=$(patsubst src/%.c,$(OUTDIR)/stage1/%.d,$(SRCS))

STAGE2_TOKENS=$(patsubst src/%.c,$(OUTDIR)/stage2/%.token,$(SRCS))
STAGE2_ASTS=$(patsubst src/%.c,$(OUTDIR)/stage2/%.ast,$(SRCS))
STAGE2_SEMAS=$(patsubst src/%.c,$(OUTDIR)/stage2/%.sema,$(SRCS))
STAGE2_ASMS=$(patsubst src/%.c,$(OUTDIR)/stage2/%.s,$(SRCS))
STAGE2_OBJS=$(patsubst src/%.c,$(OUTDIR)/stage2/%.o,$(SRCS))

STAGE3_TOKENS=$(patsubst src/%.c,$(OUTDIR)/stage3/%.token,$(SRCS))
STAGE3_ASTS=$(patsubst src/%.c,$(OUTDIR)/stage3/%.ast,$(SRCS))
STAGE3_SEMAS=$(patsubst src/%.c,$(OUTDIR)/stage3/%.sema,$(SRCS))
STAGE3_ASMS=$(patsubst src/%.c,$(OUTDIR)/stage3/%.s,$(SRCS))
STAGE3_OBJS=$(patsubst src/%.c,$(OUTDIR)/stage3/%.o,$(SRCS))

STAGE1_CC=$(CC)
STAGE1_CFLAGS=-Wall -Wextra -std=c11 -g3 -D_POSIX_C_SOURCE=201809L -MMD -fdiagnostics-color
STAGE1_GIFCC=target/stage1/gifcc

STAGE2_CC=$(STAGE1_GIFCC)
STAGE2_CFLAGS=
STAGE2_GIFCC=target/stage2/gifcc

STAGE3_CC=$(STAGE2_GIFCC)
STAGE3_CFLAGS=
STAGE3_GIFCC=target/stage3/gifcc

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
test: gcc-test stage1-test
.PHONY: test
test-full: stage1-test-full
.PHONY: test-full

stage1: $(STAGE1_GIFCC) target/stage1/
.PHONY: stage1
stage2: $(STAGE2_GIFCC) $(STAGE2_TOKENS) $(STAGE2_ASTS) $(STAGE2_SEMAS) $(STAGE2_ASMS) target/stage2/
.PHONY: stage2
stage3: $(STAGE3_GIFCC) $(STAGE3_TOKENS) $(STAGE3_ASTS) $(STAGE3_SEMAS) $(STAGE3_ASMS) target/stage3/
.PHONY: stage3

gcc-test:
.PHONY: gcc-test
gcc-test-compile:
	$(MAKE) -C test gcc-run STAGE=gcc
.PHONY: gcc-test-compile
gcc-test: gcc-test-compile

define stage-test
stage$(1)-test:
.PHONY: stage$(1)-test
stage$(1)-test-full: stage$(1)-test
.PHONY: stage$(1)-test-full

stage$(1)-test-gifcc: $(STAGE$(1)_GIFCC)
	$$< --test
.PHONY: stage$(1)-test-gifcc
stage$(1)-test: stage$(1)-test-gifcc
stage$(1)-test-compile: $(STAGE$(1)_GIFCC)
	$(MAKE) -C test STAGE=stage$(1) run
.PHONY: stage$(1)-test-compile
stage$(1)-test: stage$(1)-test-compile
stage$(1)-test-c-testsuite: $(STAGE$(1)_GIFCC)
	./scripts/run_c-testsuite stage$(1)
.PHONY: stage$(1)-test-c-testsuite
stage$(1)-test-full: stage$(1)-test-c-testsuite
endef

$(eval $(call stage-test,1))
$(eval $(call stage-test,2))
$(eval $(call stage-test,3))

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

define gifcc-build
$(STAGE$(1)_GIFCC): $(STAGE$(1)_OBJS)
	$(CC) $(LDFLAGS) -o $$@ $$^
$(OUTDIR)/stage$(1)/%.token: src/%.c $(STAGE$(1)_CC) | $(OUTDIR)/stage$(1)/
	$(STAGE$(1)_CC) $(STAGE$(1)_CFLAGS) $$< --emit token > $$@.tmp
	mv $$@.tmp $$@
$(OUTDIR)/stage$(1)/%.ast: src/%.c $(STAGE$(1)_CC) | $(OUTDIR)/stage$(1)/
	$(STAGE$(1)_CC) $(STAGE$(1)_CFLAGS) $$< --emit ast > $$@.tmp
	mv $$@.tmp $$@
$(OUTDIR)/stage$(1)/%.sema: src/%.c $(STAGE$(1)_CC) | $(OUTDIR)/stage$(1)/
	$(STAGE$(1)_CC) $(STAGE$(1)_CFLAGS) $$< --emit sema > $$@.tmp
	mv $$@.tmp $$@
$(OUTDIR)/stage$(1)/%.s: src/%.c $(STAGE$(1)_CC) | $(OUTDIR)/stage$(1)/
	$(STAGE$(1)_CC) $(STAGE$(1)_CFLAGS) $$< > $$@.tmp
	mv $$@.tmp $$@
$(OUTDIR)/stage$(1)/%.o: $(OUTDIR)/stage$(1)/%.s
	$(CC) -c -o $$@ $$<
endef

$(eval $(call gifcc-build,2))
$(eval $(call gifcc-build,3))

src/include_path.h: FORCE
	scripts/gen_include_path $@

%/:
	mkdir -p $@

FORCE:
.PHONY: FORCE

-include $(STAGE1_DEPS)
