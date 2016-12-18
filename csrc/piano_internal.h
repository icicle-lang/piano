#ifndef __PIANO_INTERNAL_H
#define __PIANO_INTERNAL_H

#include "piano.h"

typedef struct piano_section32 {
    int32_t offset;
    int32_t length;
} piano_section32_t;

typedef struct piano {
    int64_t min_time;
    int64_t max_time;
    int32_t max_count;

    piano_section32_t *buckets;

    int32_t count;
    uint32_t *hashes;

    piano_section32_t *id_sections;
    uint8_t *id_data;

    piano_section32_t *time_sections;
    int64_t *time_data;
} piano_t;

error_t piano_lookup_binary (
    piano_t *piano
  , const uint8_t *entity_id
  , size_t entity_id_size
  , int64_t *out_count
  , const int64_t **out_times
  );

#endif//__PIANO_INTERNAL_H
