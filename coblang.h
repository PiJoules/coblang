#ifndef COBLANG_H_
#define COBLANG_H_

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define COBL_STRING_SIZE \
  (sizeof(void*) + \
   sizeof(unsigned long) * 2)

// An opaque struct used for interacting with cobl-string.
struct CoblString {
  uint8_t buff[COBL_STRING_SIZE];
} __attribute__((aligned(alignof(void*))));

#define COBL_VECTOR_SIZE \
  (sizeof(void*) + \
   sizeof(unsigned long) * 4)

// An opaque struct used for interacting with cobl-vector.
struct CoblVector {
  uint8_t buff[COBL_VECTOR_SIZE];
} __attribute__((aligned(alignof(void*))));

#define COBL_TREE_MAP_SIZE (sizeof(void*) * 7)

// An opaque struct used for interacting with cobl-tree-map.
struct CoblTreeMap {
  uint8_t buff[COBL_TREE_MAP_SIZE];
} __attribute__((aligned(alignof(void*))));

typedef long TreeMapCmp(const void *, const void *);
typedef const void *KeyCtor(const void *);
typedef void KeyDtor(const void *);

#ifdef __cplusplus
}  // extern "C"
#endif

#endif  // COBLANG_H_
