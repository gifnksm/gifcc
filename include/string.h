#include <stddef.h>

// Copying functions
void *memcpy(void *s1, const void *s2, size_t n);
void *memmove(void *s1, const void *s2, size_t n);
char *strcpy(char *s1, const char *s2);
char *strncpy(char *s1, const char *s2, size_t n);

// Concatenation functions
char *strcat(char *s1, const char *s2);
char *strncat(char *s1, const char *s2, size_t n);

// Comparison functions
int memcmp(const void *s1, const void *s2, size_t n);
int strcmp(const char *s1, const char *s2);
int strcoll(const char *s1, const char *s2);
int strncmp(const char *s1, const char *s2, size_t n);
size_t strxfrm(char *s1, const char *s2, size_t n);

// Search functions
void *memchr(const void *s1, int c, size_t n);
char *strchr(const char *s, int c);
size_t strcspan(const char *s1, const char *s2);
char *strpbrk(const char *s1, const char *s2);
char *strrchr(const char *s, int c);
size_t strspn(const char *s1, const char *s2);
char *strstr(const char *s1, const char *s2);
char *strtok(char *s1, const char *s2);

// Miscellaneous functions
void *memset(void *s, int c, size_t n);
char *strerror(int errnum);
size_t strlen(const char *s);
