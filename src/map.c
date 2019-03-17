#include "gifcc.h"
#include <string.h>

typedef struct Map {
  Vector *keys;
  Vector *vals;
} Map;

Map *new_map(void) {
  Map *map = NEW(Map);
  map->keys = new_vector();
  map->vals = new_vector();
  return map;
}

int map_size(const Map *map) { return vec_len(map->keys); }

void *map_get_by_index(Map *map, int n, const char **key) {
  assert(n < map_size(map));
  if (key != NULL) {
    *key = vec_get(map->keys, n);
  }
  return vec_get(map->vals, n);
}

void map_put(Map *map, const char *key, void *val) {
  vec_push(map->keys, (void *)key);
  vec_push(map->vals, val);
}

void *map_get(Map *map, const char *key) {
  for (int i = vec_len(map->keys) - 1; i >= 0; i--) {
    const char *map_key = vec_get(map->keys, i);
    if (map_key == NULL) {
      continue;
    }
    if (strcmp(map_key, key) == 0) {
      return vec_get(map->vals, i);
    }
  }
  return NULL;
}

bool map_remove(Map *map, const char *key) {
  bool removed = false;
  for (int i = 0; i < map_size(map); i++) {
    const char *map_key = vec_get(map->keys, i);
    if (map_key == NULL) {
      continue;
    }
    if (strcmp(map_key, key) == 0) {
      vec_set(map->keys, i, NULL);
      vec_set(map->vals, i, NULL);
      removed = true;
    }
  }
  return removed;
}
