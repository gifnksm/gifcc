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

Token *new_token_char(Number val, const Range *range) {
  Token *token = new_token(TK_CHARCONST, range);
  token->char_val = val;
  return token;
}

Token *new_token_str(const char *str, const Range *range) {
  Token *token = new_token(TK_STR, range);
  token->str = str;
  return token;
}

static const char *quote(const char *s) { return format("`%s`", s); }

const char *token_kind_to_str(int kind) {
  if (kind <= 255) {
    char str[] = " ";
    str[0] = kind;
    return quote(str);
  }

  switch (kind) {
  case TK_PP_IDENT:
    return "PP_IDENT";
  case TK_PP_NUM:
    return "PP_NUM";
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
          return quote(tk->str);
        }
      }
    }
    abort();
  }
  }
}

const char *token_to_str(const Token *token) {
  if (token->ty <= 255) {
    return format("%c", token->ty);
  }
  if (token->ty == TK_PP_IDENT) {
    return token->pp_ident;
  }
  if (token->ty == TK_PP_NUM) {
    return token->pp_num;
  }
  if (token->ty == TK_NUM) {
    return format_number(token->num);
  }
  for (int i = 0; LONG_PUNCT_TOKENS[i].str != NULL; i++) {
    if (LONG_PUNCT_TOKENS[i].kind == token->ty) {
      return LONG_PUNCT_TOKENS[i].str;
    }
  }
  range_error(token->range, "結合できないトークンです");
}

static void dump_token(FILE *fp, const Token *token) {
  fprintf(fp, "%s: %-8s ", format_range_start(token->range),
          token_kind_to_str(token->ty));
  switch (token->ty) {
  case TK_PP_NUM:
    fprintf(fp, "%s", token->pp_num);
    break;
  case TK_PP_IDENT:
    fprintf(fp, "%s", token->pp_ident);
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

static bool token_filter(void *arg, Vector *output) {
  TokenFilterArg *tfa = arg;
  Token *token = ts_pop(tfa->ts);
  if (token == NULL) {
    return false;
  }
  dump_token(tfa->fp, token);
  vec_push(output, token);
  return true;
}

TokenIterator *new_token_dumper(TokenIterator *ts, FILE *fp) {
  TokenFilterArg *arg = NEW(TokenFilterArg);
  *arg = (TokenFilterArg){.fp = fp, .ts = ts};
  return new_token_iterator(token_filter, arg);
}
