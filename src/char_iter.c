#include "gifcc.h"
#include <string.h>

typedef struct CharIterator {
  cs_next_fn_t *next;
  void *arg;
  CharVector *chars;
  Char last;
} CharIterator;

static const Char CHAR_INVALID = {.val = '\0'};

CharIterator *new_char_iterator(cs_next_fn_t *next, void *arg) {
  CharIterator *cs = NEW(CharIterator);
  *cs = (CharIterator){
      .next = next,
      .arg = arg,
      .chars = NEW_VECTOR(CharVector),
      .last = {.val = '\n'},
  };
  return cs;
}

bool cs_pop(CharIterator *cs, Char *output) {
  Char ch;
  if (VEC_LEN(cs->chars) == 0) {
    if (!cs->next(cs->arg, &ch)) {
      return false;
    }
  } else {
    ch = VEC_REMOVE(cs->chars, 0);
  }
  cs->last = ch;
  if (output != NULL) {
    *output = ch;
  }
  return true;
}

void cs_succ(CharIterator *cs) { cs_pop(cs, NULL); }

Char cs_peek(CharIterator *cs) {
  if (VEC_LEN(cs->chars) == 0) {
    Char ch;
    if (!cs->next(cs->arg, &ch)) {
      return CHAR_INVALID;
    }
    VEC_PUSH(cs->chars, ch);
  }
  return VEC_FIRST(cs->chars);
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
    Char ch;
    if (VEC_LEN(cs->chars) <= i) {
      if (!cs->next(cs->arg, &ch)) {
        return false;
      }
      VEC_PUSH(cs->chars, ch);
    } else {
      ch = VEC_GET(cs->chars, i);
    }
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

bool cs_is_sol(CharIterator *cs) { return cs->last.val == '\n'; }
