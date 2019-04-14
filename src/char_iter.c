#include "gifcc.h"
#include <string.h>

typedef struct CharIterator {
  cs_next_line_fn_t *next_line;
  void *arg;
  CharVector *chars;
  int index;
} CharIterator;

static const Char CHAR_INVALID = {.val = '\0'};
static bool load_next_line_if_insufficient(CharIterator *cs, int n);
static bool load_next_line_if_empty(CharIterator *cs);

CharIterator *new_char_iterator(cs_next_line_fn_t *next_line, void *arg) {
  CharIterator *cs = NEW(CharIterator);
  *cs = (CharIterator){
      .next_line = next_line,
      .arg = arg,
      .chars = NEW_VECTOR(CharVector),
      .index = 0,
  };
  return cs;
}

bool cs_pop(CharIterator *cs, Char *output) {
  if (!load_next_line_if_empty(cs)) {
    return false;
  }

  Char ch = VEC_GET(cs->chars, cs->index);
  cs->index++;
  if (output != NULL) {
    *output = ch;
  }
  return true;
}

bool cs_pop_line(CharIterator *cs, CharVector *output) {
  if (!load_next_line_if_empty(cs)) {
    return false;
  }

  if (output != NULL) {
    VEC_APPEND(output, cs->chars);
  }
  VEC_CLEAR(cs->chars);
  return true;
}

void cs_succ(CharIterator *cs) { cs_pop(cs, NULL); }
void cs_succ_to_eol(CharIterator *cs) {
  if (!load_next_line_if_empty(cs)) {
    return;
  }
  cs->index = VEC_LEN(cs->chars) - 1;
}

Char cs_peek(CharIterator *cs) {
  if (!load_next_line_if_empty(cs)) {
    return CHAR_INVALID;
  }
  return VEC_GET(cs->chars, cs->index);
}

Char cs_peek_ahead(CharIterator *cs, int n) {
  if (!load_next_line_if_insufficient(cs, n)) {
    return CHAR_INVALID;
  }
  return VEC_GET(cs->chars, n + cs->index);
}

bool cs_consume(CharIterator *cs, char ch, const Reader **reader, int *start,
                int *end) {
  Char c = cs_peek(cs);
  if (c.val != ch) {
    return false;
  }
  cs_succ(cs);

  if (reader != NULL) {
    *reader = c.reader;
  }
  if (start != NULL) {
    *start = c.start;
  }
  if (end != NULL) {
    *end = c.end;
  }
  return true;
}

bool cs_consume_str(CharIterator *cs, const char *str, const Reader **reader,
                    int *start, int *end) {
  int len = strlen(str);
  Char ch_start = cs_peek(cs);
  Char ch_end = ch_start;
  for (int i = 0; i < len; i++) {
    Char ch = cs_peek_ahead(cs, i);
    if (ch.val != str[i]) {
      return false;
    }
    ch_end = ch;
  }

  if (reader != NULL) {
    *reader = ch_start.reader;
  }
  if (start != NULL) {
    *start = ch_start.start;
  }
  if (end != NULL) {
    *end = ch_end.end;
  }

  for (int i = 0; i < len; i++) {
    cs_succ(cs);
  }
  return true;
}

Char cs_expect(CharIterator *cs, char ch) {
  const Reader *reader;
  int start, end;
  if (!cs_consume(cs, ch, &reader, &start, &end)) {
    Char chere = cs_peek(cs);
    const Range *range =
        range_from_reader(chere.reader, chere.start, chere.end);
    range_error(range, "'%c' expected", ch);
  }

  return (Char){
      .val = ch,
      .start = start,
      .end = end,
      .reader = reader,
  };
}

static bool load_next_line_if_insufficient(CharIterator *cs, int n) {
  if (VEC_LEN(cs->chars) == cs->index) {
    VEC_CLEAR(cs->chars);
    cs->index = 0;
  }

  if (VEC_LEN(cs->chars) <= cs->index + n) {
    if (!cs->next_line(cs->arg, cs->chars)) {
      return false;
    }
  }

  assert(VEC_LEN(cs->chars) > cs->index + n);
  assert(VEC_LAST(cs->chars).val == '\n' || VEC_LAST(cs->chars).val == '\0');

  return true;
}
static bool load_next_line_if_empty(CharIterator *cs) {
  if (VEC_LEN(cs->chars) == cs->index) {
    VEC_CLEAR(cs->chars);
    cs->index = 0;

    if (!cs->next_line(cs->arg, cs->chars)) {
      return false;
    }
  }

  assert(VEC_LAST(cs->chars).val == '\n' || VEC_LAST(cs->chars).val == '\0');

  return true;
}
