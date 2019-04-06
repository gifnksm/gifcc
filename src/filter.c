#include "gifcc.h"

static char phase2_pop(void *arg UNUSED, Reader *reader) {
  // Translation phase #2
  // * backslash character (\) immediately followed by a new-line character is
  //   deleted
  reader_consume_str(reader, "\\\n");
  return reader_pop(reader);
}

Reader *phase2_filter(Reader *reader) {
  return new_filtered_reader(reader, phase2_pop, NULL);
}
