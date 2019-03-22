#include "gifcc.h"
#include <errno.h>
#include <string.h>
#include <unistd.h>

typedef struct {
  FILE *fp;
  const char *tmpname;
  const char *outname;
} OutputFile;

static Vector *file_list = NULL;
static void cleanup(void);

char *replace_suffix(const char *filename, const char *from_suffix,
                     const char *to_suffix) {
  char *buf = strdup(filename);
  size_t buf_len = strlen(buf);
  size_t suf_len = strlen(from_suffix);
  if (buf_len >= suf_len) {
    bool found = true;
    for (size_t i = 0; i < suf_len; i++) {
      if (filename[buf_len - i - 1] != from_suffix[suf_len - i - 1]) {
        found = false;
        break;
      }
    }
    if (found) {
      buf[buf_len - suf_len] = '\0';
    }
  }
  return format("%s%s", buf, to_suffix);
}

FILE *open_output_file(const char *filename) {
  if (file_list == NULL) {
    file_list = new_vector();
    atexit(cleanup);
  }

  char *tmpname = format("%s.tmp", filename);
  FILE *fp = fopen(tmpname, "w");
  if (fp == NULL) {
    error("failed to open file: %s", strerror(errno));
  }

  OutputFile *file = NEW(OutputFile);
  file->fp = fp;
  file->tmpname = tmpname;
  file->outname = filename;
  vec_push(file_list, file);

  return fp;
}

void complete_output_file(void) {
  if (file_list == NULL) {
    return;
  }

  while (vec_len(file_list) > 0) {
    OutputFile *file = vec_pop(file_list);
    fclose(file->fp);
    if (rename(file->tmpname, file->outname) < 0) {
      error("failed to rename file: %s. %s => %s", strerror(errno),
            file->tmpname, file->outname);
    }
  }
}

static void cleanup(void) {
  while (vec_len(file_list) > 0) {
    OutputFile *file = vec_pop(file_list);
    (void)unlink(file->tmpname);
    (void)fclose(file->fp);
  }
}
