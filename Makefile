CFLAGS=-Wall -std=c11 -MMD
SRCS=$(wildcard *.c)
HDRS=$(wildcard *.h)
OBJS=$(SRCS:.c=.o)
DEPS=$(SRCS:.c=.d)

9cc: $(OBJS)
	$(CC) $^ -o $@

test: 9cc
	./test.sh
.PHONY: test

clean:
	rm -f 9cc *.o *.d *~ tmp*
.PHONY: clean

format:
	clang-format -i $(SRCS) $(HDRS)
.PHONY: format

-include $(DEPS)
