CFLAGS=-Wall -Wextra -std=c11 -g3 -D_POSIX_C_SOURCE=201809L -MMD -fdiagnostics-color
SRCS=$(wildcard *.c)
HDRS=$(wildcard *.h)
OBJS=$(patsubst %.c,target/%.o,$(SRCS))
DEPS=$(patsubst %.c,target/%.d,$(SRCS))

target/gifcc: $(OBJS)
	$(CC) -o $@ $^

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

target/%.o: %.c | target/
	$(CC) $(CFLAGS) -c -o $@ $<

%/:
	mkdir -p $@

-include $(DEPS)
