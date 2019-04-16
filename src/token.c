#include "gifcc.h"
#include <string.h>

const LongToken LONG_IDENT_TOKENS[] = {
    {"void", TK_VOID},
    {"int", TK_INT},
    {"short", TK_SHORT},
    {"long", TK_LONG},
    {"char", TK_CHAR},
    {"signed", TK_SIGNED},
    {"unsigned", TK_UNSIGNED},
    {"_Bool", TK_BOOL},
    {"float", TK_FLOAT},
    {"double", TK_DOUBLE},
    {"if", TK_IF},
    {"else", TK_ELSE},
    {"switch", TK_SWITCH},
    {"case", TK_CASE},
    {"default", TK_DEFAULT},
    {"while", TK_WHILE},
    {"do", TK_DO},
    {"for", TK_FOR},
    {"goto", TK_GOTO},
    {"break", TK_BREAK},
    {"continue", TK_CONTINUE},
    {"return", TK_RETURN},
    {"struct", TK_STRUCT},
    {"union", TK_UNION},
    {"enum", TK_ENUM},
    {"sizeof", TK_SIZEOF},
    {"_Alignof", TK_ALIGNOF},
    {"typedef", TK_TYPEDEF},
    {"extern", TK_EXTERN},
    {"static", TK_STATIC},
    {"const", TK_CONST},
    {"restrict", TK_RESTRICT},
    {"volatile", TK_VOLATILE},
    {"inline", TK_INLINE},
    {"_Noreturn", TK_NORETURN},
    {"_Generic", TK_GENERIC},
    {"typeof", TK_TYPEOF},
    {NULL, '\0'},
};
const LongToken LONG_PUNCT_TOKENS[] = {
    {"==", TK_EQEQ},       {"!=", TK_NOTEQ},      {"<<=", TK_LSHIFT_ASSIGN},
    {"<<", TK_LSHIFT},     {"<=", TK_LTEQ},       {">>=", TK_RSHIFT_ASSIGN},
    {">>", TK_RSHIFT},     {">=", TK_GTEQ},       {"&&", TK_LOGAND},
    {"&=", TK_AND_ASSIGN}, {"||", TK_LOGOR},      {"|=", TK_OR_ASSIGN},
    {"^=", TK_XOR_ASSIGN}, {"++", TK_INC},        {"+=", TK_ADD_ASSIGN},
    {"--", TK_DEC},        {"-=", TK_SUB_ASSIGN}, {"*=", TK_MUL_ASSIGN},
    {"/=", TK_DIV_ASSIGN}, {"%=", TK_MOD_ASSIGN}, {"->", TK_ARROW},
    {"...", TK_ELIPSIS},   {"##", TK_HASH_HASH},  {NULL, '\0'},
};
const char SHORT_PUNCT_TOKENS[] = "=!<>&|^+-*/%();?:~{}[],.#\\";

Token *new_token(int ty, const Range *range) {
  Token *token = NEW(Token);
  token->ty = ty;
  token->range = range;
  return token;
}

Token *token_clone(Token *token, const Range *expanded_from) {
  Token *cloned = NEW(Token);
  *cloned = *token;
  if (token->pp_hideset != NULL) {
    cloned->pp_hideset = set_clone(token->pp_hideset);
  }
  cloned->range = range_add_expanded_from(token->range, expanded_from);

  return cloned;
}

Token *new_token_pp_num(const char *num, const Range *range) {
  Token *token = new_token(TK_PP_NUM, range);
  token->pp_num = num;
  return token;
}

Token *new_token_pp_ident(const char *ident, const Range *range) {
  Token *token = new_token(TK_PP_IDENT, range);
  token->pp_ident = ident;
  return token;
}

Token *new_token_pp_char(const char *ch, const Range *range) {
  Token *token = new_token(TK_PP_CHAR, range);
  token->pp_char = ch;
  return token;
}

Token *new_token_pp_str(const char *str, const Range *range) {
  Token *token = new_token(TK_PP_STR, range);
  token->pp_str = str;
  return token;
}
Token *new_token_pp_space(const Range *range) {
  return new_token(TK_PP_SPACE, range);
}

Token *new_token_pp_null(const Range *range) {
  return new_token(TK_PP_NULL, range);
}
Token *new_token_pp_if(TokenVector *tokens, const Range *range) {
  Token *token = new_token(TK_PP_IF, range);
  token->pp_if.tokens = tokens;
  return token;
}
Token *new_token_pp_elif(TokenVector *tokens, const Range *range) {
  Token *token = new_token(TK_PP_ELIF, range);
  token->pp_elif.tokens = tokens;
  return token;
}
Token *new_token_pp_ifdef(const char *ident, const Range *range) {
  Token *token = new_token(TK_PP_IFDEF, range);
  token->pp_ifdef.ident = ident;
  return token;
}
Token *new_token_pp_ifndef(const char *ident, const Range *range) {
  Token *token = new_token(TK_PP_IFNDEF, range);
  token->pp_ifndef.ident = ident;
  return token;
}
Token *new_token_pp_else(const Range *range) {
  return new_token(TK_PP_ELSE, range);
}
Token *new_token_pp_endif(const Range *range) {
  return new_token(TK_PP_ENDIF, range);
}

Token *new_token_pp_include(TokenVector *tokens, const Range *range) {
  Token *token = new_token(TK_PP_INCLUDE, range);
  token->pp_include.tokens = tokens;
  return token;
}
Token *new_token_pp_define(const char *ident, StrVector *params,
                           bool has_varargs, TokenVector *replacements,
                           const Range *range) {
  Token *token = new_token(TK_PP_DEFINE, range);
  token->pp_define.ident = ident;
  token->pp_define.params = params;
  token->pp_define.has_varargs = has_varargs;
  token->pp_define.replacements = replacements;
  return token;
}
Token *new_token_pp_undef(const char *ident, const Range *range) {
  Token *token = new_token(TK_PP_UNDEF, range);
  token->pp_undef.ident = ident;
  return token;
}
Token *new_token_pp_error(const char *message, const Range *range) {
  Token *token = new_token(TK_PP_ERROR, range);
  token->pp_error.message = message;
  return token;
}
Token *new_token_pp_line(TokenVector *tokens, const Range *range) {
  Token *token = new_token(TK_PP_LINE, range);
  token->pp_line.tokens = tokens;
  return token;
}
Token *new_token_pp_unknown(const char *ident, const char *rest,
                            const Range *range) {
  Token *token = new_token(TK_PP_UNKNOWN, range);
  token->pp_unknown.ident = ident;
  token->pp_unknown.rest = rest;
  return token;
}
Token *new_token_eof(const Range *range) { return new_token(TK_EOF, range); }

const char *token_kind_to_str(int kind) {
  if (kind <= 255) {
    return format("%c", kind);
  }

  switch (kind) {
  case TK_PP_IDENT:
    return "PP_IDENT";
  case TK_PP_NUM:
    return "PP_NUM";
  case TK_PP_CHAR:
    return "PP_CHAR";
  case TK_PP_STR:
    return "PP_STR";
  case TK_PP_SPACE:
    return "PP_SPACE";
  case TK_PP_NULL:
    return "PP_NULL";
  case TK_PP_IF:
    return "PP_IF";
  case TK_PP_ELIF:
    return "PP_ELIF";
  case TK_PP_IFDEF:
    return "PP_IFDEF";
  case TK_PP_IFNDEF:
    return "PP_IFNDEF";
  case TK_PP_ELSE:
    return "PP_ELSE";
  case TK_PP_ENDIF:
    return "PP_ENDIF";
  case TK_PP_INCLUDE:
    return "PP_INCLUDE";
  case TK_PP_DEFINE:
    return "PP_DEFINE";
  case TK_PP_UNDEF:
    return "PP_UNDEF";
  case TK_PP_ERROR:
    return "PP_ERROR";
  case TK_PP_LINE:
    return "PP_LINE";
  case TK_PP_UNKNOWN:
    return "PP_UNKNOWN";
  case TK_NUM:
    return "NUM";
  case TK_IDENT:
    return "IDENT";
  case TK_CHARCONST:
    return "CHARCONST";
  case TK_STR:
    return "STR";
  case TK_EOF:
    return "EOF";
  default: {
    const LongToken *ltss[] = {LONG_IDENT_TOKENS, LONG_PUNCT_TOKENS, NULL};
    for (int j = 0; ltss[j] != NULL; j++) {
      const LongToken *lts = ltss[j];
      for (int i = 0; lts[i].str != NULL; i++) {
        const LongToken *tk = &lts[i];
        if (tk->kind == kind) {
          return tk->str;
        }
      }
    }
    abort();
  }
  }
}

static void dump_tokens(FILE *fp, const TokenVector *tokens) {
  for (int i = 0; i < VEC_LEN(tokens); i++) {
    Token *tk = VEC_GET(tokens, i);
    switch (tk->ty) {
    case TK_PP_NUM:
      fprintf(fp, "%s", tk->pp_num);
      break;
    case TK_PP_IDENT:
      fprintf(fp, "%s", tk->pp_ident);
      break;
    case TK_PP_CHAR:
      fprintf(fp, "%s", tk->pp_char);
      break;
    case TK_PP_STR:
      fprintf(fp, "%s", tk->pp_str);
      break;
    case TK_PP_SPACE:
      fprintf(fp, " ");
      break;
    default:
      fprintf(fp, "%s", token_kind_to_str(tk->ty));
      break;
    }
  }
}

static void dump_macro_params(FILE *fp, const char *ident, StrVector *params,
                              bool has_varargs) {
  fprintf(fp, "%s", ident);
  if (params == NULL) {
    return;
  }

  fprintf(fp, "(");
  for (int i = 0; i < VEC_LEN(params); i++) {
    fprintf(fp, "%s%s", (i > 0) ? ", " : "", VEC_GET(params, i));
  }
  if (has_varargs) {
    if (VEC_LEN(params) > 0) {
      fprintf(fp, ", ...");
    } else {
      fprintf(fp, "...");
    }
  }
  fprintf(fp, ")");
}

static void dump_token(FILE *fp, const Token *token) {
  fprintf(fp, "%s: %-16s ", format_range_start(token->range),
          token_kind_to_str(token->ty));
  switch (token->ty) {
  case TK_PP_NUM:
    fprintf(fp, "%s", token->pp_num);
    break;
  case TK_PP_IDENT:
    fprintf(fp, "%s", token->pp_ident);
    break;
  case TK_PP_CHAR:
    fprintf(fp, "%s", token->pp_char);
    break;
  case TK_PP_STR:
    fprintf(fp, "%s", token->pp_str);
    break;
  case TK_PP_IF:
    dump_tokens(fp, token->pp_if.tokens);
    break;
  case TK_PP_ELIF:
    dump_tokens(fp, token->pp_elif.tokens);
    break;
  case TK_PP_IFDEF:
    fprintf(fp, "%s", token->pp_ifdef.ident);
    break;
  case TK_PP_IFNDEF:
    fprintf(fp, "%s", token->pp_ifndef.ident);
    break;
  case TK_PP_INCLUDE:
    dump_tokens(fp, token->pp_include.tokens);
    break;
  case TK_PP_DEFINE:
    dump_macro_params(fp, token->pp_define.ident, token->pp_define.params,
                      token->pp_define.has_varargs);
    fprintf(fp, " ");
    dump_tokens(fp, token->pp_define.replacements);
    break;
  case TK_PP_UNDEF:
    fprintf(fp, "%s", token->pp_undef.ident);
    break;
  case TK_PP_ERROR:
    fprintf(fp, "%s", token->pp_error.message);
    break;
  case TK_PP_LINE:
    dump_tokens(fp, token->pp_line.tokens);
    break;
  case TK_PP_UNKNOWN:
    fprintf(fp, "%s %s", token->pp_unknown.ident, token->pp_unknown.rest);
    break;
  case TK_NUM:
    fprintf(fp, "%s", format_number(token->num));
    break;
  case TK_IDENT:
    fprintf(fp, "%s", token->ident);
    break;
  case TK_CHARCONST:
    fprintf(fp, "%s", format_number(token->char_val));
    break;
  case TK_STR:
    fprintf(fp, "%s", format_string_literal(token->str));
    break;
  default:
    break;
  }
  fprintf(fp, "\n");
}

typedef struct {
  FILE *fp;
  TokenIterator *ts;
} TokenFilterArg;

static bool token_filter(void *arg, TokenVector *output) {
  TokenFilterArg *tfa = arg;
  Token *token = ts_pop(tfa->ts);
  if (token == NULL) {
    return false;
  }
  dump_token(tfa->fp, token);
  VEC_PUSH(output, token);
  return true;
}

TokenIterator *new_token_dumper(TokenIterator *ts, FILE *fp) {
  TokenFilterArg *arg = NEW(TokenFilterArg);
  *arg = (TokenFilterArg){.fp = fp, .ts = ts};
  return new_token_iterator(token_filter, arg);
}

void trim_spaces(TokenVector *tokens) {
  while (VEC_LEN(tokens) > 0 && VEC_FIRST(tokens)->ty == TK_PP_SPACE) {
    VEC_REMOVE(tokens, 0);
  }
  while (VEC_LEN(tokens) > 0 && VEC_LAST(tokens)->ty == TK_PP_SPACE) {
    VEC_POP(tokens);
  }
}

void trim_surrounding_spaces(TokenVector *tokens, int ty) {
  int i = 0;
  while (i < VEC_LEN(tokens)) {
    if (VEC_GET(tokens, i)->ty == ty) {
      while (i + 1 < VEC_LEN(tokens) &&
             VEC_GET(tokens, i + 1)->ty == TK_PP_SPACE) {
        VEC_REMOVE(tokens, i + 1);
      }

      while (i > 0 && VEC_GET(tokens, i - 1)->ty == TK_PP_SPACE) {
        VEC_REMOVE(tokens, i - 1);
        i--;
      }

      assert(VEC_GET(tokens, i)->ty == ty);
    }
    i++;
  }
}

Token *token_consume_skip_space(TokenIterator *ts, int ty) {
  int i = 0;
  while (true) {
    Token *next = ts_peek_ahead(ts, i);
    if (next->ty == TK_PP_SPACE) {
      i++;
      continue;
    }

    if (next->ty != ty) {
      return NULL;
    }

    for (int j = 0; j < i; j++) {
      ts_pop(ts);
    }
    return ts_pop(ts);
  }
}
