#ifndef RTYPES_H
#define RTYPES_H

#include <stdint.h>
#include <stdlib.h>

typedef int8_t   i8;
typedef int16_t  i16;
typedef int32_t  i32;
typedef int64_t  i64;
typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef float    f32;
typedef double   f64;

#define new(T)        (T*)malloc(   sizeof(T));
#define new_arr(T, N) (T*)calloc(N, sizeof(T));

#endif
