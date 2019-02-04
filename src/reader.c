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

  char cur = reader_peek(reader);
  reader->offset++;
  if (cur == '\n') {
    int_vec_push(reader->line_offset, reader->offset);
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
    error("'%c' がありません: %s", ch, reader_rest(reader));
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

const char *reader_get_source(const Reader *reader, Range range) {
  assert(range.start < reader->size);
  assert(range.start + range.len <= reader->size);
  return strndup(&reader->source[range.start], range.len);
}

const char *reader_rest(Reader *reader) {
  return &reader->source[reader->offset];
}

noreturn __attribute__((format(printf, 5, 6))) void
reader_error_with_raw(const Reader *reader, int offset, const char *dbg_file,
                      int dbg_line, char *fmt, ...) {
  int line, column;
  reader_get_position(reader, offset, &line, &column);
  fprintf(stderr, "%s:%d:%d: error: ", reader_get_filename(reader), line,
          column);

  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);

  fprintf(stderr, " (DEBUG:%s:%d)\n", dbg_file, dbg_line);
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

  return source;
}
