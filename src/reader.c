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

static void print_source(Range range);
static char *read_whole_file(Reader *reader, FILE *file);

Reader *new_reader(FILE *file, const char *filename) {
  Reader *reader = NEW(Reader);

  reader->source = NULL;
  reader->filename = filename;
  reader->size = 0;
  reader->offset = 0;
  reader->line_offset = new_int_vector();
  int_vec_push(reader->line_offset, reader->offset);

  read_whole_file(reader, file);
  fclose(file);

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

void reader_get_position(const Reader *reader, int offset,
                         const char **filename, int *line, int *column) {
  assert(reader->line_offset->len > 0);
  *filename = reader->filename;
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

noreturn __attribute__((format(printf, 5, 6))) void
reader_error_offset_raw(const Reader *reader, int offset, const char *dbg_file,
                        int dbg_line, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  range_error_raw_v((Range){.reader = reader, .start = offset, .len = 1},
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
  const char *start_filename;
  int start_line, start_column;
  reader_get_position(range.reader, range.start, &start_filename, &start_line,
                      &start_column);
  const char *end_filename;
  int end_line, end_column;
  reader_get_position(range.reader, range.start + range.len - 1, &end_filename,
                      &end_line, &end_column);
  fprintf(stderr, "%s:%d:%d: error: ", start_filename, start_line,
          start_column);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, " (DEBUG:%s:%d)\n", dbg_file, dbg_line);

  print_source(range);

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
  const char *start_filename;
  int start_line, start_column;
  reader_get_position(range.reader, range.start, &start_filename, &start_line,
                      &start_column);
  const char *end_filename;
  int end_line, end_column;
  reader_get_position(range.reader, range.start + range.len - 1, &end_filename,
                      &end_line, &end_column);
  fprintf(stderr, "%s:%d:%d: warning: ", start_filename, start_line,
          start_column);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, " (DEBUG:%s:%d)\n", dbg_file, dbg_line);

  print_source(range);
}

static void print_source(Range range) {
  const char *start_filename;
  int start_line, start_column;
  reader_get_position(range.reader, range.start, &start_filename, &start_line,
                      &start_column);
  const char *end_filename;
  int end_line, end_column;
  reader_get_position(range.reader, range.start + range.len - 1, &end_filename,
                      &end_line, &end_column);

  for (int line = start_line; line <= end_line; line++) {
    int sl = range.reader->line_offset->data[line - 1];
    int el = range.reader->line_offset->data[line];
    const char *line_str = &range.reader->source[sl];
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
    fprintf(stderr, "^");
    for (int i = sc + 1; i <= ec; i++) {
      fprintf(stderr, "~");
    }
    fprintf(stderr, "\n");
  }
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
