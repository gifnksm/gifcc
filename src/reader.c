#include "gifcc.h"
#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DISPATCH_BASE(reader, func, ...)                                       \
  ((reader)->type == READER_BASE)                                              \
      ? func##_base(&(reader)->base, ##__VA_ARGS__)                            \
      : func((reader)->adapter.inner, ##__VA_ARGS__)

#define DISPATCH(reader, func, ...)                                            \
  ((reader)->type == READER_BASE)                                              \
      ? func##_base(&(reader)->base, ##__VA_ARGS__)                            \
      : func##_adapter(&(reader)->adapter, ##__VA_ARGS__)

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

typedef struct Base {
  int offset;
  bool is_sol;
  Vector *file_stack;
  Vector *file_offset;
} Base;

typedef struct Adapter {
  int offset;
  bool is_sol;
  Reader *inner;
  reader_pop_fun_t *pop;
  void *arg;
  IntVector *chars;
  IntVector *offsets;
} Adapter;

struct Reader {
  enum { READER_BASE, READER_ADAPTER } type;
  union {
    Base base;
    Adapter adapter;
  };
};

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

static const Base *reader_from_range(const Range *range);
static FileOffset *switch_file(Base *reader, File *file);
static char reader_peek_ahead_adapter(Adapter *reader, int n);
static char reader_peek_ahead_base(Base *reader, int n);
static char reader_peek_ahead(Reader *reader, int n);
static void reader_succ_base(Base *reader);
static void reader_succ_adapter(Adapter *reader);
static File *peek_file(const Base *reader);
static File *peek_file_n(const Base *reader, int n);
static FileOffset *get_file_offset(const Base *reader, int offset);
static void reader_get_position_base(const Base *reader, int offset,
                                     const char **filename, int *line,
                                     int *column);
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

static const Base *reader_from_range(const Range *range) {
  const Reader *reader = range->reader;
  while (reader->type == READER_ADAPTER) {
    reader = reader->adapter.inner;
  }
  assert(reader->type == READER_BASE);
  return &reader->base;
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

  const Base *ar = reader_from_range(a);
  const Base *br = reader_from_range(b);

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

Reader *new_reader(void) {
  Reader *reader = NEW(Reader);
  reader->type = READER_BASE;
  reader->base = (Base){
      .offset = 0,
      .is_sol = true,
      .file_stack = new_vector(),
      .file_offset = new_vector(),
  };

  File *file = new_builtin_file();
  file->stack_idx = INT_MAX;
  (void)switch_file(&reader->base, file);

  reader->base.offset = 1;

  return reader;
}

Reader *new_filtered_reader(Reader *base, reader_pop_fun_t *pop, void *arg) {
  Reader *reader = NEW(Reader);
  reader->type = READER_ADAPTER;
  reader->adapter = (Adapter){
      .offset = reader_get_offset(base),
      .is_sol = true,
      .inner = base,
      .pop = pop,
      .arg = arg,
      .chars = new_int_vector(),
      .offsets = new_int_vector(),
  };
  return reader;
}

static void reader_add_file_base(Base *reader, FILE *fp, const char *filename) {
  File *file = new_file(fp, filename);
  file->stack_idx = vec_len(reader->file_stack);
  vec_push(reader->file_stack, file);
  (void)switch_file(reader, file);
}
void reader_add_file(Reader *reader, FILE *fp, const char *filename) {
  DISPATCH_BASE(reader, reader_add_file, fp, filename);
}

static void reader_set_position_base(Base *reader, const int *line,
                                     const char *filename) {
  int current_line;
  reader_get_position_base(reader, reader->offset, NULL, &current_line, NULL);

  File *file = peek_file(reader);
  FileOffset *fo = switch_file(reader, file);
  if (line != NULL) {
    fo->line_bias = *line - current_line;
  }
  fo->alt_filename = filename;
}
void reader_set_position(Reader *reader, const int *line,
                         const char *filename) {
  DISPATCH_BASE(reader, reader_set_position, line, filename);
}

static FileOffset *switch_file(Base *reader, File *file) {
  FileOffset *fo = NEW(FileOffset);
  fo->index = vec_len(reader->file_offset);
  fo->global_offset = reader->offset;
  fo->file_offset = file != NULL ? file->offset : 0;
  fo->file = file;
  vec_push(reader->file_offset, fo);
  reader->is_sol = true;
  return fo;
}

static File *peek_file(const Base *reader) { return peek_file_n(reader, 0); }
static File *peek_file_n(const Base *reader, int n) {
  if (n >= vec_len(reader->file_stack)) {
    return NULL;
  }
  return vec_rget(reader->file_stack, n);
}

char reader_peek(Reader *reader) { return reader_peek_ahead(reader, 0); }

static char reader_peek_ahead_base(Base *reader, int n) {
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
static char reader_peek_ahead_adapter(Adapter *reader, int n) {
  while (int_vec_len(reader->chars) <= n) {
    char c = reader->pop(reader->arg, reader->inner);
    int_vec_push(reader->chars, c);
    int_vec_push(reader->offsets, reader_get_offset(reader->inner));
    if (c == '\0') {
      return c;
    }
  }
  return int_vec_get(reader->chars, n);
}
static char reader_peek_ahead(Reader *reader, int n) {
  return DISPATCH(reader, reader_peek_ahead, n);
}

static void reader_succ_base(Base *reader) {
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
static void reader_succ_adapter(Adapter *reader) {
  int last, offset;
  if (int_vec_len(reader->chars) > 0) {
    last = int_vec_remove(reader->chars, 0);
    offset = int_vec_remove(reader->offsets, 0);
  } else {
    last = reader->pop(reader->arg, reader->inner);
    offset = reader_get_offset(reader->inner);
  }
  reader->is_sol = (last == '\n');
  reader->offset = offset;
}
void reader_succ(Reader *reader) { DISPATCH(reader, reader_succ); }

char reader_pop(Reader *reader) {
  char ch = reader_peek(reader);
  if (ch != '\0') {
    reader_succ(reader);
  }
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
  for (int i = 0; i < len; i++) {
    reader_succ(reader);
  }
  return true;
}
void reader_expect(Reader *reader, char ch) {
  if (!reader_consume(reader, ch)) {
    reader_error_here(reader, "'%c' がありません", ch);
  }
}

static bool reader_is_sol_base(const Base *reader) { return reader->is_sol; }
static bool reader_is_sol_adapter(const Adapter *reader) {
  return reader->is_sol;
}
bool reader_is_sol(const Reader *reader) {
  return DISPATCH(reader, reader_is_sol);
}
static int reader_get_offset_base(const Base *reader) { return reader->offset; }
static int reader_get_offset_adapter(const Adapter *reader) {
  return reader->offset;
}
int reader_get_offset(const Reader *reader) {
  return DISPATCH(reader, reader_get_offset);
}

static FileOffset *get_file_offset(const Base *reader, int offset) {
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

static void reader_get_position_inner(const Base *reader, int offset,
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

static void reader_get_position_base(const Base *reader, int offset,
                                     const char **filename, int *line,
                                     int *column) {
  reader_get_position_inner(reader, offset, false, filename, line, column);
}
void reader_get_position(const Reader *reader, int offset,
                         const char **filename, int *line, int *column) {
  DISPATCH_BASE(reader, reader_get_position, offset, filename, line, column);
}
static void reader_get_real_position_base(const Base *reader, int offset,
                                          const char **filename, int *line,
                                          int *column) {
  reader_get_position_inner(reader, offset, true, filename, line, column);
}
void reader_get_real_position(const Reader *reader, int offset,
                              const char **filename, int *line, int *column) {
  DISPATCH_BASE(reader, reader_get_real_position, offset, filename, line,
                column);
}
char *reader_get_source(const Range *range) {
  const Base *reader = reader_from_range(range);
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
  const Base *reader = reader_from_range(range);
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
    reader_get_real_position_base(reader, start, &start_filename, &start_line,
                                  &start_column);
    const char *end_filename;
    int end_line, end_column;
    reader_get_real_position_base(reader, start + file_len - 1, &end_filename,
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
