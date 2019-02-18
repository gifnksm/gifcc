#include "gifcc.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct File {
  const char *source;
  const char *name;
  int size;
  int offset;
  IntVector *line_offset;
} File;

typedef struct FileOffset {
  int index;
  int global_offset;
  int file_offset;
  File *file;
} FileOffset;

struct Reader {
  int offset;
  Vector *file_stack;
  Vector *file_offset;
};

typedef enum {
  MESSAGE_ERROR,
  MESSAGE_WARN,
  MESSAGE_NOTE,
} message_t;

static void switch_file(Reader *reader, File *file);
static char reader_peek_ahead(const Reader *reader, int n);
static File *peek_file(const Reader *reader);
static File *peek_file_n(const Reader *reader, int n);
static FileOffset *get_file_offset(const Reader *reader, int offset);
static __attribute__((format(printf, 5, 6))) void
print_message(message_t msg, Range range, const char *dbg_file, int dbg_line,
              const char *fmt, ...);
static void print_message_raw(message_t msg, Range range, const char *dbg_file,
                              int dbg_line, const char *fmt, va_list ap);
static void print_source(Range range);
static void print_expanded_message(Range range, const char *dbg_file,
                                   int dbg_line);
static File *new_file(FILE *fp, const char *filename);
static char *read_whole_file(File *file, FILE *fp);

Reader *new_reader(void) {
  Reader *reader = NEW(Reader);
  reader->file_stack = new_vector();
  reader->file_offset = new_vector();
  return reader;
}

void reader_add_file(Reader *reader, FILE *fp, const char *filename) {
  File *file = new_file(fp, filename);
  vec_push(reader->file_stack, file);
  switch_file(reader, file);
}

static void switch_file(Reader *reader, File *file) {
  FileOffset *fo = NEW(FileOffset);
  fo->index = vec_len(reader->file_offset);
  fo->global_offset = reader->offset;
  fo->file_offset = file != NULL ? file->offset : 0;
  fo->file = file;
  vec_push(reader->file_offset, fo);
}

static File *peek_file(const Reader *reader) { return peek_file_n(reader, 0); }

static File *peek_file_n(const Reader *reader, int n) {
  if (n >= vec_len(reader->file_stack)) {
    return NULL;
  }
  return vec_rget(reader->file_stack, n);
}

char reader_peek(const Reader *reader) {
  File *file = peek_file(reader);
  if (file == NULL) {
    return '\0';
  }
  assert(file->offset < file->size);
  return file->source[file->offset];
}

static char reader_peek_ahead(const Reader *reader, int n) {
  int file_idx = 0;
  int count = n;
  while (true) {
    File *file = peek_file_n(reader, file_idx);
    if (file == NULL) {
      return '\0';
    }
    if (count >= file->size - file->offset) {
      count -= (file->size - file->offset);
      file_idx++;
      continue;
    }
    return file->source[file->offset + count];
  }
}

void reader_succ(Reader *reader) {
  File *file = peek_file(reader);
  assert(file != NULL);
  reader->offset++;
  file->offset++;
  if (file->offset >= file->size) {
    vec_pop(reader->file_stack);
    switch_file(reader, peek_file(reader));
  }
}

void reader_succ_n(Reader *reader, int n) {
  for (int i = 0; i < n; i++) {
    reader_succ(reader);
  }
}

char reader_pop(Reader *reader) {
  char ch = reader_peek(reader);
  reader_succ(reader);
  return ch;
}

bool reader_consume(Reader *reader, char ch) {
  if (reader_peek(reader) == ch) {
    reader_succ(reader);
    return true;
  }
  return false;
}

bool reader_consume_str(Reader *reader, const char *str) {
  int len = strlen(str);
  for (int i = 0; i < len; i++) {
    if (reader_peek_ahead(reader, i) != str[i]) {
      return false;
    }
  }
  reader_succ_n(reader, len);
  return true;
}

void reader_expect(Reader *reader, char ch) {
  if (!reader_consume(reader, ch)) {
    reader_error_here(reader, "'%c' がありません", ch);
  }
}

int reader_get_offset(const Reader *reader) { return reader->offset; }

static FileOffset *get_file_offset(const Reader *reader, int offset) {
  assert(vec_len(reader->file_offset) > 0);
  for (int i = vec_len(reader->file_offset) - 1; i >= 0; i--) {
    FileOffset *fo = vec_get(reader->file_offset, i);
    if (fo->global_offset > offset) {
      continue;
    }
    if (fo->file == NULL) {
      continue;
    }
    return fo;
  }
  assert(false);
}

void reader_get_position(const Reader *reader, int offset,
                         const char **filename, int *line, int *column) {
  FileOffset *fo = get_file_offset(reader, offset);
  for (int j = int_vec_len(fo->file->line_offset) - 1; j >= 0; j--) {
    int line_start = int_vec_get(fo->file->line_offset, j);
    if (line_start > offset - fo->global_offset + fo->file_offset) {
      continue;
    }

    if (filename != NULL) {
      *filename = fo->file->name;
    }
    if (line != NULL) {
      *line = j + 1;
    }
    if (column != NULL) {
      *column = offset - fo->global_offset + fo->file_offset - line_start + 1;
    }
    return;
  }
  assert(false);
}

noreturn __attribute__((format(printf, 5, 6))) void
reader_error_offset_raw(const Reader *reader, int offset, const char *dbg_file,
                        int dbg_line, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  range_error_raw_v((Range){.reader = reader, .start = offset, .len = 1},
                    dbg_file, dbg_line, fmt, ap);
}

__attribute__((format(printf, 5, 6))) void
reader_warn_offset_raw(const Reader *reader, int offset, const char *dbg_file,
                       int dbg_line, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  range_warn_raw_v((Range){.reader = reader, .start = offset, .len = 1},
                   dbg_file, dbg_line, fmt, ap);
}

noreturn __attribute__((format(printf, 4, 5))) void
range_error_raw(Range range, const char *dbg_file, int dbg_line,
                const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  range_error_raw_v(range, dbg_file, dbg_line, fmt, ap);
}

noreturn void range_error_raw_v(Range range, const char *dbg_file, int dbg_line,
                                const char *fmt, va_list ap) {
  print_message_raw(MESSAGE_ERROR, range, dbg_file, dbg_line, fmt, ap);
  print_source(range);
  print_expanded_message(range, dbg_file, dbg_line);

  exit(1);
}

__attribute__((format(printf, 4, 5))) void
range_warn_raw(Range range, const char *dbg_file, int dbg_line, const char *fmt,
               ...) {
  va_list ap;
  va_start(ap, fmt);
  range_warn_raw_v(range, dbg_file, dbg_line, fmt, ap);
}
void range_warn_raw_v(Range range, const char *dbg_file, int dbg_line,
                      const char *fmt, va_list ap) {
  print_message_raw(MESSAGE_WARN, range, dbg_file, dbg_line, fmt, ap);
  print_source(range);
  print_expanded_message(range, dbg_file, dbg_line);
}

static __attribute__((format(printf, 5, 6))) void
print_message(message_t msg, Range range, const char *dbg_file, int dbg_line,
              const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  print_message_raw(msg, range, dbg_file, dbg_line, fmt, ap);
  va_end(ap);
}

static void print_message_raw(message_t msg, Range range, const char *dbg_file,
                              int dbg_line, const char *fmt, va_list ap) {
  const char *filename;
  int line, column;
  reader_get_position(range.reader, range.start, &filename, &line, &column);

  fprintf(stderr, "%s:%d:%d: ", filename, line, column);

  switch (msg) {
  case MESSAGE_ERROR:
    fprintf(stderr, "warning: ");
    break;
  case MESSAGE_WARN:
    fprintf(stderr, "error: ");
    break;
  case MESSAGE_NOTE:
    fprintf(stderr, "note: ");
    break;
  }

  vfprintf(stderr, fmt, ap);

  fprintf(stderr, " (DEBUG:%s:%d)\n", dbg_file, dbg_line);
}

static void print_source(Range range) {
  const Reader *reader = range.reader;
  int start = range.start;
  int len = range.len;
  while (len > 0) {
    FileOffset *fo = get_file_offset(reader, range.start);
    FileOffset *next_fo = fo->index < vec_len(reader->file_offset) - 1
                              ? vec_get(reader->file_offset, fo->index + 1)
                              : NULL;
    int file_end = next_fo != NULL
                       ? next_fo->global_offset
                       : fo->global_offset + (fo->file->size - fo->file_offset);
    int sf = start - fo->global_offset;
    int ef = sf + len > file_end ? file_end : sf + len;
    int file_len = ef - sf;

    const char *start_filename;
    int start_line, start_column;
    reader_get_position(reader, start, &start_filename, &start_line,
                        &start_column);
    const char *end_filename;
    int end_line, end_column;
    reader_get_position(reader, start + len, &end_filename, &end_line,
                        &end_column);

    for (int line = start_line; line <= end_line; line++) {
      int sl = int_vec_get(fo->file->line_offset, line - 1);
      int el = int_vec_get(fo->file->line_offset, line);
      const char *line_str = &fo->file->source[sl];
      int line_len = el - sl;
      int sc = (line == start_line) ? start_column - 1 : 0;
      int ec = (line == end_line) ? end_column - 1 : line_len;

      for (int i = 0; i < line_len; i++) {
        if (line_str[i] == '\t') {
          fputc(' ', stderr);
        } else {
          fputc(line_str[i], stderr);
        }
      }

      for (int i = 0; i < sc; i++) {
        fprintf(stderr, " ");
      }
      if (line == start_line) {
        fprintf(stderr, "^");
      } else {
        fprintf(stderr, "~");
      }
      for (int i = sc + 1; i < ec; i++) {
        fprintf(stderr, "~");
      }
      fprintf(stderr, "\n");
    }

    start += file_len;
    len -= file_len;
  }
}

static void print_expanded_message(Range range, const char *dbg_file,
                                   int dbg_line) {
  while (range.expanded_from != NULL) {
    range = *range.expanded_from;
    print_message(MESSAGE_NOTE, range, dbg_file, dbg_line, "マクロ展開元");
    print_source(range);
  }
}

static File *new_file(FILE *fp, const char *filename) {
  File *file = NEW(File);

  file->source = NULL;
  file->name = filename;
  file->size = 0;
  file->offset = 0;
  file->line_offset = new_int_vector();
  int_vec_push(file->line_offset, file->offset);
  read_whole_file(file, fp);
  fclose(fp);

  return file;
}

static char *read_whole_file(File *file, FILE *fp) {
  const size_t BUF_SIZE = 10240;
  size_t size = 0;
  char *source = NULL;

  while (true) {
    source = realloc(source, size + BUF_SIZE + 1);
    size_t nread = fread(&source[size], 1, BUF_SIZE, fp);
    size += nread;
    if (nread < BUF_SIZE) {
      if (!feof(fp)) {
        error("ファイルの読み込みに失敗しました: %s", file->name);
      }
      break;
    }
  }

  file->source = source;
  file->size = size;

  for (size_t i = 0; i < size; i++) {
    if (source[i] == '\n') {
      int_vec_push(file->line_offset, i + 1);
    }
  }

  return source;
}
