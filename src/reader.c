#include "gifcc.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Reader {
  const char *source;
  const char *filename;
  int size;
  int offset;
  int line;
  int column;
};

static char *read_whole_file(Reader *reader, FILE *file);

Reader *new_reader(FILE *file, const char *filename) {
  Reader *reader = malloc(sizeof(Reader));

  reader->source = NULL;
  reader->filename = filename;
  reader->size = 0;
  reader->offset = 0;
  reader->line = 1;
  reader->column = 1;

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
  if (cur == '\n') {
    reader->line++;
    reader->column = 1;
  } else {
    reader->column++;
  }
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
    error("'%c' がありません: %s", ch, reader_rest(reader));
  }
}

const char *reader_rest(Reader *reader) {
  return &reader->source[reader->offset];
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
