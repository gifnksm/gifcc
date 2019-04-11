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
      .chars = new_char_vector(),
      .last = {.val = '\n'},
  };
  return cs;
}

bool cs_pop(CharIterator *cs, Char *output) {
  Char ch;
  if (char_vec_len(cs->chars) == 0) {
    if (!cs->next(cs->arg, &ch)) {
      return false;
    }
  } else {
    ch = char_vec_remove(cs->chars, 0);
  }
  cs->last = ch;
  if (output != NULL) {
    *output = ch;
  }
  return true;
}

void cs_succ(CharIterator *cs) { cs_pop(cs, NULL); }

Char cs_peek(CharIterator *cs) { return cs_peek_ahead(cs, 0); }
Char cs_peek_ahead(CharIterator *cs, int n) {
  while (char_vec_len(cs->chars) <= n) {
    Char ch;
    if (!cs->next(cs->arg, &ch)) {
      return CHAR_INVALID;
    }
    char_vec_push(cs->chars, ch);
  }
  return char_vec_get(cs->chars, n);
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
  for (int i = 0; i < len; i++) {
    Char c = cs_peek_ahead(cs, i);
    if (c.val != str[i]) {
      return false;
    }
  }

  if (reader != NULL) {
    *reader = cs_peek(cs).reader;
  }
  if (start != NULL) {
    *start = cs_peek(cs).start;
  }
  if (end != NULL) {
    *end = cs_peek_ahead(cs, len - 1).end;
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
