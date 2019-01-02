CFLAGS=-Wall -std=c11 -MMD
SRCS=$(wildcard *.c)
HDRS=$(wildcard *.h)
OBJS=$(patsubst %.c,target/%.o,$(SRCS))
DEPS=$(patsubst %.c,target/%.d,$(SRCS))

target/9cc: $(OBJS)
	$(CC) -o $@ $^

test: target/9cc
	./target/9cc -test
	./test.sh
.PHONY: test

clean:
	$(RM) -r target
.PHONY: clean

format:
	clang-format -i $(SRCS) $(HDRS)
.PHONY: format

target/%.o: %.c | target/
	$(CC) $(CFLAGS) -c -o $@ $<

%/:
	mkdir -p $@

-include $(DEPS)
