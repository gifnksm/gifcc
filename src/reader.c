#include "gifcc.h"
#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct File {
  const char *source;
  const char *name;
  int stack_idx;
  int size;
  int offset;
  IntVector *line_offset;
} File;

typedef struct FileOffset {
  int index;
  int global_offset;
  int file_offset;
  int line_bias;
  const char *alt_filename;
  File *file;
} FileOffset;

typedef struct Reader {
  int offset;
  bool is_sol;
  Vector *file_stack;
  Vector *file_offset;
} Reader;

typedef struct Range {
  const Reader *reader;
  int start;
  int len;
  const Range *expanded_from;
} Range;

typedef enum {
  MESSAGE_ERROR,
  MESSAGE_WARN,
  MESSAGE_NOTE,
} message_t;

static char reader_pop(Reader *reader);
static FileOffset *switch_file(Reader *reader, File *file);
static char reader_peek_ahead(Reader *reader, int n);
static File *peek_file(const Reader *reader);
static File *peek_file_n(const Reader *reader, int n);
static FileOffset *get_file_offset(const Reader *reader, int offset);
static __attribute__((format(printf, 5, 6))) void
print_message(message_t msg, const Range *range, const char *dbg_file,
              int dbg_line, const char *fmt, ...);
static void print_message_raw(message_t msg, const Range *range,
                              const char *dbg_file, int dbg_line,
                              const char *fmt, va_list ap);
static void print_source(const Range *range);
static void print_expanded_message(const Range *range, const char *dbg_file,
                                   int dbg_line);
static File *new_builtin_file(void);
static File *new_file(FILE *fp, const char *filename);
static char *read_whole_file(File *file, FILE *fp);

const Range *range_from_reader(const Reader *reader, int start, int end) {
  assert(start <= end);
  Range *range = NEW(Range);
  *range = (Range){
      .reader = reader,
      .start = start,
      .len = (end - start),
  };
  return range;
}

const Range *range_builtin(const Reader *reader) {
  return range_from_reader(reader, 0, 0);
}

const Range *range_add_expanded_from(const Range *range,
                                     const Range *expanded_from) {
  if (range->expanded_from == NULL) {
    Range *cloned = NEW(Range);
    *cloned = *range;
    cloned->expanded_from = expanded_from;
    return cloned;
  }

  const Range *exp =
      range_add_expanded_from(range->expanded_from, expanded_from);
  if (exp == range->expanded_from) {
    return range;
  }

  Range *cloned = NEW(Range);
  *cloned = *range;
  cloned->expanded_from = exp;
  return cloned;
}

const Range *range_join(const Range *a, const Range *b) {
  assert(a->start >= 0);
  assert(b->start >= 0);
  assert(a->reader == b->reader);

  const Reader *ar = a->reader;
  const Reader *br = b->reader;

  while (true) {
    File *af = get_file_offset(ar, a->start)->file;
    File *bf = get_file_offset(br, b->start)->file;

    if (af->stack_idx < bf->stack_idx) {
      if (b->expanded_from == NULL) {
        break;
      }
      b = b->expanded_from;
      continue;
    }
    if (af->stack_idx > bf->stack_idx) {
      if (a->expanded_from == NULL) {
        break;
      }
      a = a->expanded_from;
    }
    if (af == bf || a->expanded_from == NULL || b->expanded_from == NULL) {
      break;
    }
    a = a->expanded_from;
    b = b->expanded_from;
  }

  int aend = a->start + a->len;
  int bend = b->start + b->len;
  int start = a->start < b->start ? a->start : b->start;
  int end = aend > bend ? aend : bend;

  Range *range = NEW(Range);
  *range = (Range){
      .reader = a->reader,
      .expanded_from = a->expanded_from,
      .start = start,
      .len = end - start,
  };
  return range;
}

void range_get_start(const Range *range, const char **filename, int *line,
                     int *column) {
  reader_get_position(range->reader, range->start, filename, line, column);
}

void range_get_end(const Range *range, const char **filename, int *line,
                   int *column) {
  reader_get_position(range->reader, range->start + range->len, filename, line,
                      column);
}

static const char *trim_filename(const char *filename) {
  if (strncmp(filename, GIFCC_INCLUDE, sizeof(GIFCC_INCLUDE) - 1) == 0) {
    return format("<gifcc>%s", &filename[sizeof(GIFCC_INCLUDE) - 1]);
  }
  return filename;
}

char *format_range_start(const Range *range) {
  const char *filename;
  int line, column;
  range_get_start(range, &filename, &line, &column);
  return format("%32s:%4d:%3d", trim_filename(filename), line, column);
}

char *format_range_end(const Range *range) {
  const char *filename;
  int line, column;
  range_get_end(range, &filename, &line, &column);
  return format("%32s:%4d:%3d", trim_filename(filename), line, column);
}

Reader *new_reader(void) {
  Reader *reader = NEW(Reader);
  *reader = (Reader){
      .offset = 0,
      .is_sol = true,
      .file_stack = new_vector(),
      .file_offset = new_vector(),
  };

  File *file = new_builtin_file();
  file->stack_idx = INT_MAX;
  (void)switch_file(reader, file);

  reader->offset = 1;

  return reader;
}

static bool next(void *arg, Char *output) {
  Reader *reader = arg;
  int offset = reader->offset;
  char ch = reader_pop(reader);

  *output = (Char){
      .val = ch,
      .start = offset,
      .end = offset + 1,
      .reader = reader,
  };
  return true;
}

CharIterator *char_iterator_from_reader(Reader *reader) {
  return new_char_iterator(next, reader);
}

void reader_add_file(Reader *reader, FILE *fp, const char *filename) {
  File *file = new_file(fp, filename);
  file->stack_idx = vec_len(reader->file_stack);
  vec_push(reader->file_stack, file);
  (void)switch_file(reader, file);
}

int reader_get_offset(const Reader *reader) { return reader->offset; }

void reader_set_position(Reader *reader, const int *line,
                         const char *filename) {
  int current_line;
  reader_get_position(reader, reader->offset, NULL, &current_line, NULL);

  File *file = peek_file(reader);
  FileOffset *fo = switch_file(reader, file);
  if (line != NULL) {
    fo->line_bias = *line - current_line;
  }
  fo->alt_filename = filename;
}

static FileOffset *switch_file(Reader *reader, File *file) {
  FileOffset *fo = NEW(FileOffset);
  fo->index = vec_len(reader->file_offset);
  fo->global_offset = reader->offset;
  fo->file_offset = file != NULL ? file->offset : 0;
  fo->file = file;
  vec_push(reader->file_offset, fo);
  reader->is_sol = true;
  return fo;
}

static File *peek_file(const Reader *reader) { return peek_file_n(reader, 0); }
static File *peek_file_n(const Reader *reader, int n) {
  if (n >= vec_len(reader->file_stack)) {
    return NULL;
  }
  return vec_rget(reader->file_stack, n);
}

char reader_peek(Reader *reader) { return reader_peek_ahead(reader, 0); }

static char reader_peek_ahead(Reader *reader, int n) {
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

static void reader_succ(Reader *reader) {
  File *file = peek_file(reader);
  assert(file != NULL);

  reader->is_sol = file->source[file->offset] == '\n';
  reader->offset++;
  file->offset++;
  if (file->offset >= file->size) {
    vec_pop(reader->file_stack);
    (void)switch_file(reader, peek_file(reader));
  }
}

static char reader_pop(Reader *reader) {
  char ch = reader_peek(reader);
  if (ch != '\0') {
    reader_succ(reader);
  }
  return ch;
}

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

static void reader_get_position_inner(const Reader *reader, int offset,
                                      bool get_real, const char **filename,
                                      int *line, int *column) {
  FileOffset *fo = get_file_offset(reader, offset);
  for (int j = int_vec_len(fo->file->line_offset) - 1; j >= 0; j--) {
    int line_start = int_vec_get(fo->file->line_offset, j);
    if (line_start > offset - fo->global_offset + fo->file_offset) {
      continue;
    }

    if (filename != NULL) {
      if (get_real || fo->alt_filename == NULL) {
        *filename = fo->file->name;
      } else {
        *filename = fo->alt_filename;
      }
    }
    if (line != NULL) {
      *line = j + 1;
      if (!get_real) {
        *line += fo->line_bias;
      }
    }
    if (column != NULL) {
      *column = offset - fo->global_offset + fo->file_offset - line_start + 1;
    }
    return;
  }
  assert(false);
}

void reader_get_position(const Reader *reader, int offset,
                         const char **filename, int *line, int *column) {
  reader_get_position_inner(reader, offset, false, filename, line, column);
}
void reader_get_real_position(const Reader *reader, int offset,
                              const char **filename, int *line, int *column) {
  reader_get_position_inner(reader, offset, true, filename, line, column);
}
char *reader_get_source(const Range *range) {
  const Reader *reader = range->reader;
  int offset = range->start;
  FileOffset *fo = get_file_offset(reader, offset);
  const char *source =
      &fo->file->source[offset - fo->global_offset + fo->file_offset];
  return strndup(source, range->len);
}

noreturn __attribute__((format(printf, 5, 6))) void
reader_error_offset_raw(const Reader *reader, int offset, const char *dbg_file,
                        int dbg_line, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  range_error_raw_v(&(Range){.reader = reader, .start = offset, .len = 1},
                    dbg_file, dbg_line, fmt, ap);
}

__attribute__((format(printf, 5, 6))) void
reader_warn_offset_raw(const Reader *reader, int offset, const char *dbg_file,
                       int dbg_line, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  range_warn_raw_v(&(Range){.reader = reader, .start = offset, .len = 1},
                   dbg_file, dbg_line, fmt, ap);
}

noreturn __attribute__((format(printf, 4, 5))) void
range_error_raw(const Range *range, const char *dbg_file, int dbg_line,
                const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  range_error_raw_v(range, dbg_file, dbg_line, fmt, ap);
}

noreturn void range_error_raw_v(const Range *range, const char *dbg_file,
                                int dbg_line, const char *fmt, va_list ap) {
  print_message_raw(MESSAGE_ERROR, range, dbg_file, dbg_line, fmt, ap);
  print_source(range);
  print_expanded_message(range, dbg_file, dbg_line);

  exit(1);
}

__attribute__((format(printf, 4, 5))) void
range_warn_raw(const Range *range, const char *dbg_file, int dbg_line,
               const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  range_warn_raw_v(range, dbg_file, dbg_line, fmt, ap);
}
void range_warn_raw_v(const Range *range, const char *dbg_file, int dbg_line,
                      const char *fmt, va_list ap) {
  print_message_raw(MESSAGE_WARN, range, dbg_file, dbg_line, fmt, ap);
  print_source(range);
  print_expanded_message(range, dbg_file, dbg_line);
}

static __attribute__((format(printf, 5, 6))) void
print_message(message_t msg, const Range *range, const char *dbg_file,
              int dbg_line, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  print_message_raw(msg, range, dbg_file, dbg_line, fmt, ap);
  va_end(ap);
}

static void print_message_raw(message_t msg, const Range *range,
                              const char *dbg_file, int dbg_line,
                              const char *fmt, va_list ap) {
  const char *filename;
  int line, column;
  reader_get_position(range->reader, range->start, &filename, &line, &column);

  fprintf(stderr, "%s:%d:%d: ", filename, line, column);

  switch (msg) {
  case MESSAGE_ERROR:
    fprintf(stderr, "error: ");
    break;
  case MESSAGE_WARN:
    fprintf(stderr, "warning: ");
    break;
  case MESSAGE_NOTE:
    fprintf(stderr, "note: ");
    break;
  }

  vfprintf(stderr, fmt, ap);

  fprintf(stderr, " (DEBUG:%s:%d)\n", dbg_file, dbg_line);
}

static void print_source(const Range *range) {
  const Reader *reader = range->reader;
  int start = range->start;
  int len = range->len;
  while (len > 0) {
    FileOffset *fo = get_file_offset(reader, start);
    FileOffset *next_fo = fo->index < vec_len(reader->file_offset) - 1
                              ? vec_get(reader->file_offset, fo->index + 1)
                              : NULL;
    int file_end = next_fo != NULL
                       ? next_fo->global_offset
                       : fo->global_offset + (fo->file->size - fo->file_offset);
    int sf = start - fo->global_offset;
    int ef = file_end - fo->global_offset;
    int file_len = ef - sf;
    if (file_len > len) {
      file_len = len;
    }

    const char *start_filename;
    int start_line, start_column;
    reader_get_real_position(reader, start, &start_filename, &start_line,
                             &start_column);
    const char *end_filename;
    int end_line, end_column;
    reader_get_real_position(reader, start + file_len - 1, &end_filename,
                             &end_line, &end_column);
    assert(strcmp(start_filename, end_filename) == 0);

    for (int line = start_line; line <= end_line; line++) {
      int sl = int_vec_get(fo->file->line_offset, line - 1);
      int el = int_vec_get(fo->file->line_offset, line);
      const char *line_str = &fo->file->source[sl];
      int line_len = el - sl;
      int sc = (line == start_line) ? start_column : 0;
      int ec = (line == end_line) ? end_column : line_len;

      for (int i = 0; i < line_len; i++) {
        if (line_str[i] == '\t') {
          fputc(' ', stderr);
        } else {
          fputc(line_str[i], stderr);
        }
      }

      for (int i = 1; i <= sc - 1; i++) {
        fprintf(stderr, " ");
      }
      if (line == start_line) {
        fprintf(stderr, "^");
      } else {
        fprintf(stderr, "~");
      }
      for (int i = sc + 1; i <= ec; i++) {
        fprintf(stderr, "~");
      }
      fprintf(stderr, "\n");
    }

    start += file_len;
    len -= file_len;
  }
}

static void print_expanded_message(const Range *range, const char *dbg_file,
                                   int dbg_line) {
  while (range->expanded_from != NULL) {
    range = range->expanded_from;
    print_message(MESSAGE_NOTE, range, dbg_file, dbg_line, "マクロ展開元");
    print_source(range);
  }
}

static File *new_builtin_file(void) {
  File *file = NEW(File);
  file->source = "";
  file->name = "<builtin>";
  file->offset = 0;
  file->line_offset = new_int_vector();
  int_vec_push(file->line_offset, file->offset);
  return file;
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
