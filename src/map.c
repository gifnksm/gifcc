#include "gifcc.h"
#include <string.h>

#define TABLE_SIZE 64

typedef struct Entry {
  const char *key;
  void *val;
} Entry;

typedef struct EntryArray {
  int size;
  int capacity;
  Entry *entries;
} EntryArray;

typedef struct Map {
  int size;
  EntryArray array[TABLE_SIZE];
} Map;

Map *new_map(void) { return NEW(Map); }

static EntryArray *get_array(Map *map, const char *key);
static Entry *lookup_entry(EntryArray *arr, const char *key);

int map_size(const Map *map) { return map->size; }

void *map_get_by_index(Map *map, int n, const char **key) {
  assert(n < map_size(map));
  int sum = 0;
  for (int i = 0; i < TABLE_SIZE; i++) {
    EntryArray *arr = &map->array[i];
    if (n < sum || sum + arr->size <= n) {
      sum += arr->size;
      continue;
    }

    for (int j = 0; j < arr->capacity; j++) {
      if (arr->entries[j].key == NULL) {
        continue;
      }

      if (sum == n) {
        Entry *found = &arr->entries[j];
        if (key != NULL) {
          *key = found->key;
        }
        return found->val;
      }
      sum++;
    }
  }

  assert(false);
  return NULL;
}

void map_put(Map *map, const char *key, void *val) {
  EntryArray *arr = get_array(map, key);

  Entry *dest = NULL;
  for (int i = 0; i < arr->capacity; i++) {
    Entry *entry = &arr->entries[i];
    if (entry->key == NULL) {
      if (dest == NULL) {
        dest = entry;
      }
    } else {
      if (strcmp(entry->key, key) == 0) {
        dest = entry;
        break;
      }
    }
  }

  if (dest == NULL) {
    int old_cap = arr->capacity;
    int new_cap = old_cap;
    if (new_cap == 0) {
      new_cap = 16;
    } else {
      new_cap *= 2;
    }
    arr->capacity = new_cap;
    arr->entries = realloc(arr->entries, new_cap * sizeof(Entry));
    memset(&arr->entries[old_cap], 0, sizeof(Entry) * (new_cap - old_cap));
    dest = &arr->entries[old_cap];
  }

  if (dest->key == NULL) {
    dest->key = key;
    arr->size++;
    map->size++;
  }

  dest->val = val;
}

void *map_get(Map *map, const char *key) {
  EntryArray *arr = get_array(map, key);
  Entry *entry = lookup_entry(arr, key);
  if (entry == NULL) {
    return NULL;
  }
  return entry->val;
}

bool map_remove(Map *map, const char *key) {
  EntryArray *arr = get_array(map, key);
  Entry *entry = lookup_entry(arr, key);
  if (entry == NULL) {
    return false;
  }
  entry->key = NULL;
  entry->val = NULL;
  arr->size--;
  map->size--;
  return true;
}

static EntryArray *get_array(Map *map, const char *key) {
  uint64_t hash = str2hash(key);
  return &map->array[hash % TABLE_SIZE];
}

static Entry *lookup_entry(EntryArray *arr, const char *key) {
  for (int i = 0; i < arr->capacity; i++) {
    Entry *entry = &arr->entries[i];
    if (entry->key == NULL) {
      continue;
    }
    if (strcmp(entry->key, key) == 0) {
      return entry;
    }
  }
  return NULL;
}
