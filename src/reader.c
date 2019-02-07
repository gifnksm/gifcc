#include "gifcc.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Reader {
  const char *source;
  const char *filename;
  int size;
  int offset;
  int column;
  IntVector *line_offset;
};

static char *read_whole_file(Reader *reader, FILE *file);

Reader *new_reader(FILE *file, const char *filename) {
  Reader *reader = malloc(sizeof(Reader));

  reader->source = NULL;
  reader->filename = filename;
  reader->size = 0;
  reader->offset = 0;
  reader->line_offset = new_int_vector();
  int_vec_push(reader->line_offset, reader->offset);

  read_whole_file(reader, file);

  return reader;
}

char reader_peek(const Reader *reader) {
  return reader->source[reader->offset];
}
char reader_peek_ahead(const Reader *reader, int n) {
  assert(n >= 0);
  assert(reader->offset + n < reader->size);

  return reader->source[reader->offset + n];
}

void reader_succ(Reader *reader) {
  assert(reader->offset < reader->size - 1);
  reader->offset++;
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
const char *reader_get_filename(const Reader *reader) {
  return reader->filename;
}
void reader_get_position(const Reader *reader, int offset, int *line,
                         int *column) {
  assert(reader->line_offset->len > 0);
  for (int i = reader->line_offset->len - 1; i >= 0; i--) {
    int line_start = reader->line_offset->data[i];
    if (line_start <= offset) {
      *line = i + 1;
      *column = offset - line_start + 1;
      return;
    }
  }
  assert(false);
}

char *reader_get_source(const Reader *reader, Range range) {
  assert(range.start < reader->size);
  assert(range.start + range.len <= reader->size);
  return strndup(&reader->source[range.start], range.len);
}

char *reader_get_line(const Reader *reader, int line) {
  assert(1 <= line && line < reader->line_offset->len);
  int start = reader->line_offset->data[line - 1];
  int end = reader->line_offset->data[line];
  return reader_get_source(reader, (Range){.start = start, .len = end - start});
}

noreturn __attribute__((format(printf, 5, 6))) void
reader_error_offset_raw(const Reader *reader, int offset, const char *dbg_file,
                        int dbg_line, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  reader_error_range_raw_v(reader, (Range){.start = offset, .len = 1}, dbg_file,
                           dbg_line, fmt, ap);
}

noreturn __attribute__((format(printf, 5, 6))) void
reader_error_range_raw(const Reader *reader, Range range, const char *dbg_file,
                       int dbg_line, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  reader_error_range_raw_v(reader, range, dbg_file, dbg_line, fmt, ap);
}

noreturn void reader_error_range_raw_v(const Reader *reader, Range range,
                                       const char *dbg_file, int dbg_line,
                                       const char *fmt, va_list ap) {
  int start_line, start_column;
  reader_get_position(reader, range.start, &start_line, &start_column);
  int end_line, end_column;
  reader_get_position(reader, range.start + range.len - 1, &end_line,
                      &end_column);
  fprintf(stderr, "%s:%d:%d: error: ", reader_get_filename(reader), start_line,
          start_column);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, " (DEBUG:%s:%d)\n", dbg_file, dbg_line);

  for (int line = start_line; line <= end_line; line++) {
    char *line_str = reader_get_line(reader, line);
    int line_len = strlen(line_str);
    int sc = (line == start_line) ? start_column - 1 : 0;
    int ec = (line == end_line) ? end_column - 1 : line_len;

    for (int i = 0; i < line_len; i++) {
      if (line_str[i] == '\t') {
        line_str[i] = ' ';
      }
    }

    fprintf(stderr, "%s", line_str);
    for (int i = 0; i < sc; i++) {
      fprintf(stderr, " ");
    }
    fprintf(stderr, "^");
    for (int i = sc + 1; i <= ec; i++) {
      fprintf(stderr, "~");
    }
    fprintf(stderr, "\n");
  }

  exit(1);
}

static char *read_whole_file(Reader *reader, FILE *file) {
  const size_t BUF_SIZE = 10240;
  size_t size = 0;
  char *source = NULL;

  while (true) {
    source = realloc(source, size + BUF_SIZE + 1);
    size_t nread = fread(&source[size], 1, BUF_SIZE, file);
    size += nread;
    if (nread < BUF_SIZE) {
      if (!feof(file)) {
        error("ファイルの読み込みに失敗しました: %s", reader->filename);
      }
      assert(nread <= BUF_SIZE);
      source[size] = '\0';
      size++;
      break;
    }
  }

  reader->source = source;
  reader->size = size;

  for (size_t i = 0; i < size; i++) {
    if (source[i] == '\n') {
      int_vec_push(reader->line_offset, i + 1);
    }
  }
  int_vec_push(reader->line_offset, size - 1);

  return source;
}
