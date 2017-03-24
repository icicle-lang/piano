#ifndef __PIANO_INTERNAL_H
#define __PIANO_INTERNAL_H

#include "piano.h"

typedef struct piano_section32 {
    int32_t offset;
    int32_t length;
} piano_section32_t;

struct piano {
    int64_t min_time;
    int64_t max_time;
    int32_t max_count;

    piano_section32_t *buckets;

    int32_t count;
    uint32_t *hashes;

    piano_section32_t *id_sections;
    uint8_t *id_data;

    piano_section32_t *label_sections;
    int64_t *label_time_data;
    int64_t *label_name_offsets;
    int64_t *label_name_lengths;
    uint8_t *label_name_data;
};

error_t piano_lookup_binary (
    piano_t *piano
  , const uint8_t *entity_id
  , size_t entity_id_size
  , int64_t *out_count
  , const int64_t **out_label_times
  , const int64_t **out_label_name_offsets
  , const int64_t **out_label_name_lengths
  , const uint8_t **out_label_name_data
  );

#endif//__PIANO_INTERNAL_H
