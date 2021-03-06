OUTDIR=./target

GEN_HDRS=src/include_path.h
HDRS=$(filter-out $(GEN_HDRS),$(wildcard src/*.h)) $(GEN_HDRS)
SRCS=$(wildcard src/*.c)

STAGE1_OBJS=$(patsubst src/%.c,$(OUTDIR)/stage1/%.o,$(SRCS))
STAGE1_DEPS=$(patsubst src/%.c,$(OUTDIR)/stage1/%.d,$(SRCS))

STAGE2_ASMS=$(patsubst src/%.c,$(OUTDIR)/stage2/%.s,$(SRCS))
STAGE2_OBJS=$(patsubst src/%.c,$(OUTDIR)/stage2/%.o,$(SRCS))

STAGE3_ASMS=$(patsubst src/%.c,$(OUTDIR)/stage3/%.s,$(SRCS))
STAGE3_OBJS=$(patsubst src/%.c,$(OUTDIR)/stage3/%.o,$(SRCS))

STAGE1_CC=$(CC)
STAGE1_CFLAGS=-Wall -Wextra -g3 -D_POSIX_C_SOURCE=201809L -MMD -fdiagnostics-color
STAGE1_GIFCC=target/stage1/gifcc

STAGE2_CC=$(STAGE1_GIFCC)
STAGE2_CFLAGS=--emit all
STAGE2_GIFCC=target/stage2/gifcc

STAGE3_CC=$(STAGE2_GIFCC)
STAGE3_CFLAGS=--emit all
STAGE3_GIFCC=target/stage3/gifcc

LCOV_OPT=\
  --rc lcov_branch_coverage=1 \
  --rc lcov_excl_br_line='\Wassert\(' \
  --rc lcov_excl_line='\Wassert\((false|0)\);'
GENHTML_OPT=\
  --rc lcov_branch_coverage=1

LDFLAGS=-no-pie

ifdef ASAN
  STAGE1_CFLAGS += -fsanitize=address
  LDFLAGS += -fsanitize=address
  export ASAN_OPTIONS=detect_leaks=0
endif

ifdef COVERAGE
  STAGE1_CFLAGS += --coverage
  LDFLAGS += --coverage
endif

ifdef RELEASE
  STAGE1_CFLAGS += -O2 -DNDEBUG
endif

all: stage1
.PHONY: all
test: gcc-test stage1-test
.PHONY: test
test-full: stage1-test-full
.PHONY: test-full

stage1: $(STAGE1_GIFCC) target/stage1/
.PHONY: stage1
stage2: $(STAGE2_GIFCC) $(STAGE2_ASMS) target/stage2/
.PHONY: stage2
stage3: $(STAGE3_GIFCC) $(STAGE3_ASMS) target/stage3/
.PHONY: stage3

gcc-test:
.PHONY: gcc-test
gcc-test-full: gcc-test
.PHONY: gcc-test-full
gcc-test-compile:
	$(MAKE) -C test run STAGE=gcc
.PHONY: gcc-test-compile
gcc-test: gcc-test-compile
gcc-test-c-testsuite:
	./scripts/run_c-testsuite gcc
.PHONY: gcc-c-testsuite
gcc-test-full: gcc-test-c-testsuite

define stage-test
stage$(1)-test:
.PHONY: stage$(1)-test
stage$(1)-test-full: stage$(1)-test
.PHONY: stage$(1)-test-full

stage$(1)-test-gifcc: $$(STAGE$(1)_GIFCC)
	$$< --test
.PHONY: stage$(1)-test-gifcc
stage$(1)-test: stage$(1)-test-gifcc
stage$(1)-test-compile: $$(STAGE$(1)_GIFCC)
	$$(MAKE) -C test STAGE=stage$(1) run
.PHONY: stage$(1)-test-compile
stage$(1)-test: stage$(1)-test-compile
stage$(1)-test-c-testsuite: $$(STAGE$(1)_GIFCC)
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
format: $(GEN_HDRS)
	clang-format -i $(SRCS) $(HDRS)
.PHONY: format
clang-tidy: $(GEN_HDRS)
	clang-tidy -fix -fix-errors $(SRCS) -- $(STAGE1_CFLAGS)
.PHONY: clang-tidy
lcov:
	lcov --capture --directory . --output-file target/coverage.info $(LCOV_OPT)
	genhtml target/coverage.info --output-directory target/html $(GENHTML_OPT)

$(STAGE1_GIFCC): $(STAGE1_OBJS)
	$(CC) $(LDFLAGS) -o $@ $^
$(OUTDIR)/stage1/%.o: src/%.c $(GEN_HDRS) Makefile | $(OUTDIR)/stage1/
	$(STAGE1_CC) $(STAGE1_CFLAGS) -c -o $@ $<

define gifcc-build
$$(STAGE$(1)_GIFCC): $$(STAGE$(1)_OBJS)
	$$(CC) $$(LDFLAGS) -o $$@ $$^
$$(OUTDIR)/stage$(1)/%.s: src/%.c $$(STAGE$(1)_CC) | $$(OUTDIR)/stage$(1)/
	$$(STAGE$(1)_CC) $$(STAGE$(1)_CFLAGS) $$< -o $$@
$$(OUTDIR)/stage$(1)/%.o: $$(OUTDIR)/stage$(1)/%.s
	$$(CC) -c -o $$@ $$<
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
