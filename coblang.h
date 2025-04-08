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

// An opaque struct used for interacting with cobl-string.
struct CoblVector {
  uint8_t buff[COBL_VECTOR_SIZE];
} __attribute__((aligned(alignof(void*))));

#ifdef __cplusplus
}  // extern "C"
#endif

#endif  // COBLANG_H_
