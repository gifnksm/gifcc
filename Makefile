OUTDIR=./target

CFLAGS=-Wall -Wextra -std=c11 -g3 -D_POSIX_C_SOURCE=201809L -MMD -fdiagnostics-color -DGIFCC_INCLUDE=\"$(abspath ./include)\"

SRCS=$(wildcard src/*.c)
OBJS=$(patsubst src/%.c,$(OUTDIR)/%.o,$(SRCS))
DEPS=$(patsubst src/%.c,$(OUTDIR)/%.d,$(SRCS))

ifdef ASAN
CFLAGS  += -fsanitize=address
LDFLAGS += -fsanitize=address
export ASAN_OPTIONS=detect_leaks=0
endif

all: $(OUTDIR)/gifcc
.PHONY: all

$(OUTDIR)/gifcc: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^

test:
.PHONY: test

test-full: test
.PHONY: test-full

test-gifcc: $(OUTDIR)/gifcc
	$(OUTDIR)/gifcc --test
.PHONY: test-gifcc
test:  test-gifcc

test-compile: $(OUTDIR)/gifcc
	$(MAKE) -C test gcc-run
	$(MAKE) -C test run
.PHONY: test-compile
test: test-compile

test-c-testsuite: $(OUTDIR)/gifcc
	./scripts/run_c-testsuite
.PHONY: test-c-testsuite
test-full: test-c-testsuite

clean:
	$(RM) -r target
.PHONY: clean

format:
	clang-format -i $(SRCS) $(HDRS)
.PHONY: format

clang-tidy:
	clang-tidy -fix -fix-errors $(SRCS) -- $(CFLAGS)

$(OUTDIR)/%.o: src/%.c | $(OUTDIR)/
	$(CC) $(CFLAGS) -c -o $@ $<

%/:
	mkdir -p $@

-include $(DEPS)
