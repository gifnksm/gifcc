#include "gifcc.h"
#include <ctype.h>

static bool read_token(CharIterator *cs, TokenVector *output);
static bool skip_space(CharIterator *cs);
static bool skip_comment(CharIterator *cs);
static bool skip_space_or_comment(CharIterator *cs);
static Token *pp_directive(CharIterator *cs);
static const char *macro_name(CharIterator *cs, const Range **range);
static void macro_func_arg(CharIterator *cs, StrVector **params,
                           bool *has_varargs, const Range **range);
static TokenVector *normal_tokens(CharIterator *cs, const Range **range);
static void read_normal_tokens_to_eol(CharIterator *cs, TokenVector *output,
                                      const Range **range);
static const char *read_chars_to_eol(CharIterator *cs, const Range **range);
static Token *normal_token(CharIterator *cs);
static Token *punctuator(CharIterator *cs);
static Token *identifier(CharIterator *cs);
static Token *constant(CharIterator *cs);
static Token *number_constant(CharIterator *cs);
static Token *character_constant(CharIterator *cs);
static Token *string_literal(CharIterator *cs);
static String *read_identifier(CharIterator *cs, const Reader **reader,
                               int *start, int *end);

static bool next(void *arg, TokenVector *output) {
  CharIterator *cs = arg;
  return read_token(cs, output);
}

TokenIterator *new_pp_tokenizer(CharIterator *cs) {
  return new_token_iterator(next, cs);
}

static bool read_token(CharIterator *cs, TokenVector *output) {
  Char ch;
  while (true) {
    ch = cs_peek(cs);
    if (skip_space_or_comment(cs)) {
      continue;
    }
    if (ch.val == '\0') {
      break;
    }
    if (ch.val == '\n') {
      // do nothing
    } else if (ch.val == '#') {
      VEC_PUSH(output, pp_directive(cs));
      ch = cs_peek(cs);
    } else {
      read_normal_tokens_to_eol(cs, output, NULL);
      ch = cs_peek(cs);
    }

    if (ch.val == '\0') {
      break;
    }
    if (ch.val == '\n') {
      cs_succ(cs);
      return true;
    }
    range_error(range_from_reader(ch.reader, ch.start, ch.end),
                "cannot tokenizer: '%c'", ch.val);
  }

  if (ch.val == '\0') {
    VEC_PUSH(output,
             new_token(TK_EOF, range_from_reader(ch.reader, ch.start, ch.end)));
  }
  return true;
}

static bool skip_space(CharIterator *cs) {
  bool skipped = false;
  while (true) {
    Char ch = cs_peek(cs);
    if (!isspace(ch.val) || ch.val == '\n') {
      return skipped;
    }
    skipped = true;
    cs_succ(cs);
  }
}

static bool skip_comment(CharIterator *cs) {
  bool skipped = false;

  while (true) {
    if (cs_consume_str(cs, "//", NULL, NULL, NULL)) {
      while (true) {
        Char ch = cs_peek(cs);
        if (ch.val == '\n' || ch.val == '\0') {
          break;
        }
        cs_succ(cs);
      }
      skipped = true;
      continue;
    }
    if (cs_consume_str(cs, "/*", NULL, NULL, NULL)) {
      while (true) {
        if (cs_consume_str(cs, "*/", NULL, NULL, NULL)) {
          break;
        }
        if (cs_peek(cs).val == '\0') {
          reader_error_here(cs_peek(cs).reader,
                            "コメントの終端文字列 `*/` がありません");
        }
        cs_succ(cs);
      }
      skipped = true;
      continue;
    }
    break;
  }

  return skipped;
}

static bool skip_space_or_comment(CharIterator *cs) {
  bool skipped = false;
  while (true) {
    if (skip_space(cs)) {
      skipped = true;
      continue;
    }
    if (skip_comment(cs)) {
      skipped = true;
      continue;
    }
    break;
  }
  return skipped;
}

static Token *pp_directive(CharIterator *cs) {
  Char start = cs_expect(cs, '#');
  const Range *range = range_from_reader(start.reader, start.start, start.end);
  skip_space_or_comment(cs);

  Char ch = cs_peek(cs);
  if (ch.val == '\n' || ch.val == '\0') {
    return new_token_pp_null(range);
  }

  ch = cs_peek(cs);
  Token *token = normal_token(cs);
  if (token->ty != TK_PP_IDENT) {
    range_error(token->range, "invalid preprocessor directive");
  }

  range = range_join(range, token->range);

  skip_space_or_comment(cs);

  if (strcmp(token->pp_ident, "if") == 0) {
    TokenVector *tokens = normal_tokens(cs, &range);
    return new_token_pp_if(tokens, range);
  }
  if (strcmp(token->pp_ident, "elif") == 0) {
    TokenVector *tokens = normal_tokens(cs, &range);
    return new_token_pp_elif(tokens, range);
  }
  if (strcmp(token->pp_ident, "ifdef") == 0) {
    const char *ident = macro_name(cs, &range);
    skip_space_or_comment(cs);
    return new_token_pp_ifdef(ident, range);
  }
  if (strcmp(token->pp_ident, "ifndef") == 0) {
    const char *ident = macro_name(cs, &range);
    skip_space_or_comment(cs);
    return new_token_pp_ifndef(ident, range);
  }
  if (strcmp(token->pp_ident, "else") == 0) {
    skip_space_or_comment(cs);
    return new_token_pp_else(range);
  }
  if (strcmp(token->pp_ident, "endif") == 0) {
    skip_space_or_comment(cs);
    return new_token_pp_endif(range);
  }
  if (strcmp(token->pp_ident, "include") == 0) {
    TokenVector *tokens = normal_tokens(cs, &range);
    return new_token_pp_include(tokens, range);
  }
  if (strcmp(token->pp_ident, "define") == 0) {
    const char *ident = macro_name(cs, &range);
    StrVector *params = NULL;
    bool has_varargs = false;
    macro_func_arg(cs, &params, &has_varargs, &range);
    skip_space_or_comment(cs);
    TokenVector *replacements = normal_tokens(cs, &range);
    return new_token_pp_define(ident, params, has_varargs, replacements, range);
  }
  if (strcmp(token->pp_ident, "undef") == 0) {
    const char *ident = macro_name(cs, &range);
    skip_space_or_comment(cs);
    return new_token_pp_undef(ident, range);
  }
  if (strcmp(token->pp_ident, "error") == 0) {
    const char *message = read_chars_to_eol(cs, &range);
    return new_token_pp_error(message, range);
  }
  if (strcmp(token->pp_ident, "line") == 0) {
    TokenVector *tokens = normal_tokens(cs, &range);
    return new_token_pp_line(tokens, range);
  }

  const char *rest = read_chars_to_eol(cs, &range);
  return new_token_pp_unknown(token->pp_ident, rest, range);
}

static const char *macro_name(CharIterator *cs, const Range **range) {
  Token *token = normal_token(cs);
  if (token == NULL) {
    Char ch = cs_peek(cs);
    range_error(range_from_reader(ch.reader, ch.start, ch.end),
                "macro name missing");
  }
  if (token->ty != TK_PP_IDENT) {
    range_error(token->range, "macro name must be an identifier");
  }
  if (range != NULL) {
    *range = range_join(*range, token->range);
  }
  return token->pp_ident;
}

static void macro_func_arg(CharIterator *cs, StrVector **params,
                           bool *has_varargs, const Range **range) {
  *has_varargs = false;
  *params = NULL;
  const Reader *reader;
  int start, end;
  if (!cs_consume(cs, '(', &reader, &start, &end)) {
    return;
  }
  const Range *param_range = range_from_reader(reader, start, end);

  *params = NEW_VECTOR(StrVector);

  while (true) {
    skip_space_or_comment(cs);
    if (cs_peek(cs).val == ')') {
      break;
    }

    Token *token = normal_token(cs);
    if (token == NULL) {
      break;
    }
    if (token->ty == TK_ELIPSIS) {
      *has_varargs = true;
      skip_space_or_comment(cs);
      break;
    }
    if (token->ty != TK_PP_IDENT) {
      range_error(token->range, "invalid token in macro parameter list");
    }
    VEC_PUSH(*params, token->pp_ident);
    skip_space_or_comment(cs);
    if (cs_peek(cs).val == ')') {
      break;
    }
    cs_expect(cs, ',');
  }

  Token *rparen = normal_token(cs);
  if (rparen == NULL) {
    Char ch = cs_peek(cs);
    range_error(range_from_reader(ch.reader, ch.start, ch.end),
                "missing ')' in macro parameter list");
  }
  if (rparen->ty != ')') {
    range_error(rparen->range, "missing ')' in macro parameter list");
  }
  param_range = range_join(param_range, rparen->range);

  if (range != NULL) {
    *range = range_join(*range, param_range);
  }
}

static TokenVector *normal_tokens(CharIterator *cs, const Range **range) {
  TokenVector *tokens = NEW_VECTOR(TokenVector);
  read_normal_tokens_to_eol(cs, tokens, range);
  return tokens;
}

static void read_normal_tokens_to_eol(CharIterator *cs, TokenVector *output,
                                      const Range **range) {
  int old_len = VEC_LEN(output);
  while (true) {
    skip_space_or_comment(cs);
    Char ch = cs_peek(cs);
    if (ch.val == '\n' || ch.val == '\0') {
      break;
    }
    Token *token = normal_token(cs);
    if (token == NULL) {
      break;
    }
    VEC_PUSH(output, token);
  }

  if (range != NULL && VEC_LEN(output) > old_len) {
    Token *first = VEC_GET(output, old_len);
    Token *last = VEC_LAST(output);
    *range = range_join(*range, range_join(first->range, last->range));
  }
}

static const char *read_chars_to_eol(CharIterator *cs, const Range **range) {
  String *str = new_string();
  Char start = cs_peek(cs);
  int end = start.end;
  while (true) {
    Char ch = cs_peek(cs);
    if (ch.val == '\n' || ch.val == '\0') {
      break;
    }
    str_push(str, ch.val);
    cs_succ(cs);
    end = ch.end;
  }
  str_push(str, '\0');
  if (range != NULL) {
    *range =
        range_join(*range, range_from_reader(start.reader, start.start, end));
  }
  return str_get_raw(str);
}

static Token *normal_token(CharIterator *cs) {
  Token *token = NULL;
  if ((token = punctuator(cs)) != NULL) {
    return token;
  }
  if ((token = constant(cs)) != NULL) {
    return token;
  }
  if ((token = string_literal(cs)) != NULL) {
    return token;
  }
  if ((token = identifier(cs)) != NULL) {
    return token;
  }
  return NULL;
}

static Token *punctuator(CharIterator *cs) {
  int kind, start, end;
  const Reader *reader;

  for (int i = 0; LONG_PUNCT_TOKENS[i].str != NULL; i++) {
    const LongToken *tk = &LONG_PUNCT_TOKENS[i];
    if (cs_consume_str(cs, tk->str, &reader, &start, &end)) {
      kind = tk->kind;
      goto Hit;
    }
  }

  for (int i = 0; SHORT_PUNCT_TOKENS[i] != '\0'; i++) {
    char tk = SHORT_PUNCT_TOKENS[i];
    if (cs_consume(cs, tk, &reader, &start, &end)) {
      kind = tk;
      goto Hit;
    }
  }

  return NULL;

Hit:;
  return new_token(kind, range_from_reader(reader, start, end));
}

static Token *identifier(CharIterator *cs) {
  int start, end;
  const Reader *reader;

  String *str = read_identifier(cs, &reader, &start, &end);
  if (str == NULL) {
    return NULL;
  }
  const Range *range = range_from_reader(reader, start, end);
  return new_token_pp_ident(str_get_raw(str), range);
}

static Token *constant(CharIterator *cs) {
  Token *token;
  if ((token = number_constant(cs)) != NULL ||
      (token = character_constant(cs)) != NULL) {
    return token;
  }
  return NULL;
}

static Token *number_constant(CharIterator *cs) {
  if (!isdigit(cs_peek(cs).val)) {
    return NULL;
  }

  String *str = new_string();
  Char ch;
  cs_pop(cs, &ch);
  str_push(str, ch.val);
  int start = ch.start;

  Char last = ch;
  while (true) {
    ch = cs_peek(cs);
    bool is_float = strchr("eEpP", last.val) && strchr("+-", ch.val);
    if (!isdigit(ch.val) && !is_ident_head(ch.val) && ch.val != '.' &&
        !is_float) {
      break;
    }
    str_push(str, ch.val);
    cs_succ(cs);
    last = ch;
  }

  int end = last.end;

  str_push(str, '\0');
  return new_token_pp_num(str_get_raw(str),
                          range_from_reader(ch.reader, start, end));
}

static Token *character_constant(CharIterator *cs) {
  String *s = new_string();

  int start;
  if (cs_consume_str(cs, "L\'", NULL, &start, NULL)) {
    str_append(s, "L\'");
  } else if (cs_consume(cs, '\'', NULL, &start, NULL)) {
    str_push(s, '\'');
  } else {
    return NULL;
  }

  int end;
  Char ch;
  while (true) {
    if (cs_consume(cs, '\\', NULL, NULL, NULL)) {
      str_push(s, '\\');
      cs_pop(cs, &ch);
      str_push(s, ch.val);
      continue;
    }

    cs_pop(cs, &ch);
    if (ch.val == '\0' || ch.val == '\n') {
      reader_error_offset(ch.reader, ch.start,
                          "missing terminating ' character");
    }
    str_push(s, ch.val);
    if (ch.val == '\'') {
      end = ch.end;
      break;
    }
  }
  str_push(s, '\0');
  return new_token_pp_char(str_get_raw(s),
                           range_from_reader(ch.reader, start, end));
}

static Token *string_literal(CharIterator *cs) {
  String *s = new_string();

  int start;
  if (!cs_consume(cs, '"', NULL, &start, NULL)) {
    return NULL;
  }
  str_push(s, '"');

  int end;
  Char ch;
  while (true) {
    if (cs_consume(cs, '\\', NULL, NULL, NULL)) {
      str_push(s, '\\');
      cs_pop(cs, &ch);
      str_push(s, ch.val);
      continue;
    }

    cs_pop(cs, &ch);
    if (ch.val == '\0' || ch.val == '\n') {
      reader_error_offset(ch.reader, ch.start,
                          "missing terminating '\"' character");
    }
    str_push(s, ch.val);
    if (ch.val == '\"') {
      end = ch.end;
      break;
    }
  }
  str_push(s, '\0');

  return new_token_pp_str(str_get_raw(s),
                          range_from_reader(ch.reader, start, end));
}

static String *read_identifier(CharIterator *cs, const Reader **reader,
                               int *start, int *end) {
  Char ch = cs_peek(cs);
  Char cstart = ch;
  if (!is_ident_head(ch.val)) {
    return NULL;
  }

  String *str = new_string();
  str_push(str, ch.val);
  cs_succ(cs);

  while (true) {
    ch = cs_peek(cs);
    if (!is_ident_tail(ch.val)) {
      break;
    }
    str_push(str, ch.val);
    cs_succ(cs);
  }
  str_push(str, '\0');

  if (reader != NULL) {
    *reader = cstart.reader;
  }
  if (start != NULL) {
    *start = cstart.start;
  }
  if (end != NULL) {
    *end = ch.start;
  }

  return str;
}
