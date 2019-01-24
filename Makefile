CFLAGS=-Wall -Wextra -std=c11 -g3 -D_POSIX_C_SOURCE=201809L -MMD -fdiagnostics-color
SRCS=$(wildcard src/*.c)
OBJS=$(patsubst src/%.c,target/%.o,$(SRCS))
DEPS=$(patsubst src/%.c,target/%.d,$(SRCS))

ifdef ASAN
CFLAGS  += -fsanitize=address
LDFLAGS += -fsanitize=address
export ASAN_OPTIONS=detect_leaks=0
endif

target/gifcc: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^

test: target/gifcc
	./target/gifcc --test
	./test.sh
.PHONY: test

clean:
	$(RM) -r target
.PHONY: clean

format:
	clang-format -i $(SRCS) $(HDRS)
.PHONY: format

clang-tidy:
	clang-tidy -fix -fix-errors $(SRCS) -- $(CFLAGS)

target/%.o: src/%.c | target/
	$(CC) $(CFLAGS) -c -o $@ $<

%/:
	mkdir -p $@

-include $(DEPS)
