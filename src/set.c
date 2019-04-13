#include "gifcc.h"
#include <string.h>

#define TABLE_SIZE 64

typedef struct Entry {
  const char *key;
} Entry;

typedef struct EntryArray {
  int size;
  int capacity;
  Entry *entries;
} EntryArray;

typedef struct Set {
  int size;
  EntryArray array[TABLE_SIZE];
} Set;

static EntryArray *get_array(Set *set, const char *key);
static Entry *lookup_entry(EntryArray *arr, const char *key);

Set *new_set(void) { return NEW(Set); }

Set *set_clone(const Set *set) {
  Set *cloned = NEW(Set);
  for (int i = 0; i < TABLE_SIZE; i++) {
    const EntryArray *src = &set->array[i];
    EntryArray *dest = &cloned->array[i];
    if (src->size == 0) {
      continue;
    }

    size_t size = sizeof(src->entries[0]) * src->capacity;
    dest->entries = malloc(size);
    memcpy(dest->entries, src->entries, size);
    dest->capacity = src->capacity;
    dest->size = src->size;
  }
  return cloned;
}
int set_size(const Set *set) { return set->size; }

const char *set_get_by_index(Set *set, int n) {
  assert(n < set_size(set));
  int sum = 0;
  for (int i = 0; i < TABLE_SIZE; i++) {
    EntryArray *arr = &set->array[i];
    if (n < sum || sum + arr->size <= n) {
      sum += arr->size;
      continue;
    }

    for (int j = 0; j < arr->capacity; j++) {
      if (arr->entries[j].key == NULL) {
        continue;
      }

      if (sum == n) {
        return arr->entries[j].key;
      }
    }
    sum++;
  }

  assert(false);
  return NULL;
}

void set_insert(Set *set, const char *key) {
  EntryArray *arr = get_array(set, key);
  Entry *dest = NULL;
  for (int i = 0; i < arr->capacity; i++) {
    Entry *entry = &arr->entries[i];
    if (entry->key == NULL) {
      if (dest == NULL) {
        dest = entry;
      }
    } else {
      if (strcmp(entry->key, key) == 0) {
        return;
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

  dest->key = key;
  arr->size++;
  set->size++;
}
bool set_contains(const Set *set, const char *key) {
  EntryArray *arr = get_array((Set *)set, key);
  return lookup_entry(arr, key) != NULL;
}

Set *set_intersection(const Set *a, const Set *b) {
  Set *set = new_set();
  for (int i = 0; i < TABLE_SIZE; i++) {
    EntryArray *dest = &set->array[i];
    const EntryArray *ea = &a->array[i];
    const EntryArray *eb = &b->array[i];

    int cap = ea->size < eb->size ? ea->size : eb->size;
    dest->capacity = cap;
    dest->entries = calloc(cap, sizeof(Entry));
    for (int j = 0; j < ea->capacity; j++) {
      const char *key = ea->entries[j].key;
      if (key == NULL) {
        continue;
      }
      if (lookup_entry((EntryArray *)eb, key) != NULL) {
        dest->entries[dest->size].key = key;
        dest->size++;
      }
    }

    set->size += dest->size;
  }
  return set;
}

static EntryArray *get_array(Set *set, const char *key) {
  uint64_t hash = str2hash(key);
  return &set->array[hash % TABLE_SIZE];
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
