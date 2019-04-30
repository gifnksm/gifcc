#include "gifcc.h"
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <strings.h>
#include <wchar.h>

static void convert_char(Token *token);
static void convert_str(Token *token);
static void convert_number(Token *token);
static void convert_ident(Token *token);
static Number read_float(Token *token);
static Number read_integer(Token *token);
static int read_char(const char **input, const Range *range);
static wchar_t read_wchar(const char **input, const Range *range);

static bool phase2_next_line(void *arg, CharVector *output) {
  CharIterator *cs = arg;

  // Translation phase #2
  // * backslash character (\) immediately followed by a new-line character is
  //   deleted
  while (true) {
    int old_len = VEC_LEN(output);
    if (!cs_pop_line(cs, output)) {
      return false;
    }

    if (VEC_LEN(output) > old_len + 1) {
      if (VEC_RGET(output, 1).val == '\\' && VEC_LAST(output).val == '\n') {
        VEC_POP(output);
        VEC_POP(output);
        continue;
      }
    }
    return true;
  }
}

CharIterator *phase2_filter(CharIterator *cs) {
  return new_char_iterator(phase2_next_line, cs);
}

TokenIterator *phase4_filter(TokenIterator *ts, Reader *reader) {
  // Translation phase #4
  // * preprocessing directives are executed, macro invocations are expanded.
  return new_preprocessor(ts, reader);
}

static bool phase5_next(void *arg, TokenVector *output) {
  TokenIterator *ts = arg;

  // Translation phase #5
  // * converts escape sequence in character constants and string literals
  //   to character set
  Token *token = ts_pop(ts);
  if (token == NULL) {
    return false;
  }
  if (token->ty == TK_PP_CHAR) {
    convert_char(token);
  }
  if (token->ty == TK_PP_STR) {
    convert_str(token);
  }
  VEC_PUSH(output, token);
  return true;
}

TokenIterator *phase5_filter(TokenIterator *ts) {
  return new_token_iterator(phase5_next, ts);
}

static bool phase6_next(void *arg, TokenVector *output) {
  TokenIterator *ts = arg;

  // Translation phase #6:
  // * concatenate adjacent string literal tokens
  Token *token = ts_pop(ts);
  if (token == NULL) {
    return false;
  }

  if (token->ty == TK_STR) {
    Token *next;
    while ((next = token_consume_skip_space(ts, TK_STR)) != NULL) {
      token->str = format("%s%s", token->str, next->str);
      token->range = range_join(token->range, next->range);
    }
  }

  VEC_PUSH(output, token);
  return true;
}

TokenIterator *phase6_filter(TokenIterator *ts) {
  return new_token_iterator(phase6_next, ts);
}

static bool phase7_next(void *arg, TokenVector *output) {
  TokenIterator *ts = arg;

  // Translation phase #7:
  // * convert pp tokens into tokens
  // * white-space characters separating tokens are no longer significant
  Token *token = ts_pop(ts);
  if (token == NULL) {
    return false;
  }
  if (token->ty == TK_PP_SPACE) {
    // drop token
    return true;
  }
  if (token->ty == TK_PP_NUM) {
    convert_number(token);
  }
  if (token->ty == TK_PP_IDENT) {
    convert_ident(token);
  }

  VEC_PUSH(output, token);
  return true;
}

TokenIterator *phase7_filter(TokenIterator *ts) {
  return new_token_iterator(phase7_next, ts);
}

static void convert_char(Token *token) {
  assert(token->ty == TK_PP_CHAR);

  char *input = strdup(token->pp_char);

  char *sep = strchr(input, '\'');
  range_assert(token->range, sep != NULL, "invalid character constant");
  *sep = '\0';
  const char *prefix = input;
  input = sep + 1;

  int len = strlen(input) - 1;
  range_assert(token->range, input[len] == '\'', "invalid character constant");
  input[len] = '\0';

  token->ty = TK_CHARCONST;
  token->pp_char = NULL;
  if (strcmp(prefix, "L") == 0) {
    const char *i = input;
    wchar_t c = read_wchar(&i, token->range);
    token->char_val = new_number_wchar_t(c);
  } else if (strcmp(prefix, "") == 0) {
    const char *i = input;
    int c = read_char(&i, token->range);
    if (*i != '\0') {
      range_warn(token->range, "multi-character character constatn: %s", i);
    }
    token->char_val = new_number_int(c);
  } else {
    range_internal_error(token->range, "invalid character constant prefix");
  }
}

static void convert_str(Token *token) {
  assert(token->ty == TK_PP_STR);

  char *input = strdup(token->pp_str);
  range_assert(token->range, input[0] == '"', "invalid string literal");
  input++;
  int len = strlen(input) - 1;
  range_assert(token->range, input[len] == '"', "invalid string literal");
  input[len] = '\0';

  String *str = new_string();
  const char *i = input;
  while (*i != '\0') {
    int c = read_char(&i, token->range);
    str_push(str, c);
  }
  str_push(str, '\0');

  token->ty = TK_STR;
  token->pp_str = NULL;
  token->str = str_get_raw(str);
}

static void convert_number(Token *token) {
  assert(token->ty == TK_PP_NUM);
  bool is_float =
      strpbrk(token->pp_num, ".pP") ||
      (strncasecmp(token->pp_num, "0x", 2) && strpbrk(token->pp_num, "eE"));

  Number num;
  if (is_float) {
    num = read_float(token);
  } else {
    num = read_integer(token);
  }

  token->ty = TK_NUM;
  token->num = num;
}

static void convert_ident(Token *token) {
  assert(token->ty == TK_PP_IDENT);
  const char *ident = token->pp_ident;
  token->pp_ident = NULL;

  for (int i = 0; LONG_IDENT_TOKENS[i].str != NULL; i++) {
    const LongToken *tk = &LONG_IDENT_TOKENS[i];
    if (strcmp(ident, tk->str) == 0) {
      token->ty = tk->kind;
      return;
    }
  }

  token->ty = TK_IDENT;
  token->ident = ident;
}

static Number read_float(Token *token) {
  assert(token->ty == TK_PP_NUM);
  char *suffix = NULL;
  long double val = strtold(token->pp_num, &suffix);
  type_t type = TY_VOID;
  if (strcmp(suffix, "") == 0) {
    type = TY_DOUBLE;
  } else if (strcasecmp(suffix, "l") == 0) {
    type = TY_LDOUBLE;
  } else if (strcasecmp(suffix, "f") == 0) {
    type = TY_FLOAT;
  } else {
    range_error(token->range, "不正なサフィックスです");
  }
  return new_number_float(type, val);
}

static Number read_integer(Token *token) {
  typedef struct Suffix {
    const char *suffix;
    type_t type;
  } Suffix;
  Suffix SUFFIX[] = {
      // unsigned long long int
      {"ull", TY_U_LLONG},
      {"llu", TY_U_LLONG},
      {"uLL", TY_U_LLONG},
      {"LLu", TY_U_LLONG},
      {"Ull", TY_U_LLONG},
      {"llU", TY_U_LLONG},
      {"ULL", TY_U_LLONG},
      {"LLU", TY_U_LLONG},
      // unsigned long int
      {"ul", TY_U_LONG},
      {"lu", TY_U_LONG},
      {"uL", TY_U_LONG},
      {"Lu", TY_U_LONG},
      {"Ul", TY_U_LONG},
      {"lU", TY_U_LONG},
      {"UL", TY_U_LONG},
      {"LU", TY_U_LONG},
      // unsigned int
      {"u", TY_U_INT},
      {"U", TY_U_INT},
      // signed long long int
      {"ll", TY_S_LLONG},
      {"LL", TY_S_LLONG},
      // signed long int
      {"l", TY_S_LONG},
      {"L", TY_S_LONG},
      // stub
      {NULL, TY_VOID},
  };

  assert(token->ty == TK_PP_NUM);
  char *suffix = NULL;
  unsigned long long val;
  if (strncasecmp("0b", token->pp_num, 2) == 0) {
    // NonStandard/GNU: binary prefix integer literal
    val = strtoull(&token->pp_num[2], &suffix, 2);
  } else {
    val = strtoull(token->pp_num, &suffix, 0);
  }

  bool isbase10 = token->pp_num[0] != '0';

  type_t ty = TY_VOID;
  if (strcmp(suffix, "") == 0) {
    // no suffix
    if (val <= INT_MAX) {
      ty = TY_S_INT;
    } else if (!isbase10 && val <= UINT_MAX) {
      ty = TY_U_INT;
    } else if (val <= LONG_MAX) {
      ty = TY_S_LONG;
    } else if (!isbase10 && val <= ULONG_MAX) {
      ty = TY_U_LONG;
    } else if (val <= LLONG_MAX) {
      ty = TY_S_LLONG;
    } else {
      assert(val <= ULLONG_MAX);
      ty = TY_U_LLONG;
    }
  } else {
    for (int i = 0; SUFFIX[i].suffix != NULL; i++) {
      if (strcmp(SUFFIX[i].suffix, suffix) == 0) {
        ty = SUFFIX[i].type;
        break;
      }
    }
    if (ty == TY_VOID) {
      range_error(token->range, "不正な整数のサフィックスです");
    }
  }

  return new_number(ty, val);
}

static int read_char(const char **input, const Range *range) {
  const char *cs = *input;

  if (*cs != '\\') {
    *input = cs + 1;
    return *cs;
  }
  cs++;

  struct {
    char raw;
    char meta;
  } ESCAPE_CHARS[] = {
      {'\'', '\''}, {'"', '"'},  {'?', '?'},  {'\\', '\\'},
      {'a', '\a'},  {'b', '\b'}, {'f', '\f'}, {'n', '\n'},
      {'r', '\r'},  {'t', '\t'}, {'v', '\v'}, {'\0', '\0'},
  };

  for (int i = 0; ESCAPE_CHARS[i].raw != '\0'; i++) {
    if (*cs == ESCAPE_CHARS[i].raw) {
      *input = cs + 1;
      return ESCAPE_CHARS[i].meta;
    }
  }

  if (*cs == 'x') {
    cs++;
    char *endptr = NULL;
    long val = strtol(cs, &endptr, 16);
    if (cs == endptr) {
      range_error(range, "\\x used with no following hex digits");
    }
    *input = endptr;
    return val;
  }

  if (is_oct_digit(*cs)) {
    char *endptr = NULL;
    long val = strtol(cs, &endptr, 8);
    assert(cs != endptr);
    *input = endptr;
    return val;
  }

  range_error(range, "unknown escape sequence '\\%c'", *cs);
}

static wchar_t read_wchar(const char **input, const Range *range) {
  const char *cs = *input;

  if (*cs != '\\') {
    wchar_t val;
    mbstate_t state;
    memset(&state, 0, sizeof(state));
    size_t len = mbrtowc(&val, cs, strlen(cs), &state);
    if ((ssize_t)len < 0) {
      range_error(range, "%s", strerror(errno));
    }
    *input = cs + len;
    return val;
  }
  cs++;

  struct {
    char raw;
    wchar_t meta;
  } ESCAPE_CHARS[] = {
      {'\'', L'\''}, {'"', L'"'},  {'?', L'?'},  {'\\', L'\\'},
      {'a', L'\a'},  {'b', L'\b'}, {'f', L'\f'}, {'n', L'\n'},
      {'r', L'\r'},  {'t', L'\t'}, {'v', L'\v'}, {'\0', L'\0'},
  };

  for (int i = 0; ESCAPE_CHARS[i].raw != '\0'; i++) {
    if (*cs == ESCAPE_CHARS[i].raw) {
      *input = cs + 1;
      return ESCAPE_CHARS[i].meta;
    }
  }

  if (*cs == 'x') {
    cs++;
    char *endptr = NULL;
    long val = strtol(cs, &endptr, 16);
    if (cs == endptr) {
      range_error(range, "\\x used with no following hex digits");
    }
    *input = endptr;
    return val;
  }

  if (is_oct_digit(*cs)) {
    char *endptr = NULL;
    long val = strtol(cs, &endptr, 8);
    assert(cs != endptr);
    *input = endptr;
    return val;
  }

  range_error(range, "unknown escape sequence '\\%c'", *cs);
}
