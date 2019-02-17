#include <stddef.h>

typedef void FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

int puts(const char *s);
int putchar(int c);
int printf(const char *fmt, ...);
int fprintf(FILE *stream, const char *fmt, ...);
