#ifndef __PIANO_INTERNAL_H
#define __PIANO_INTERNAL_H

#include "piano.h"

error_t piano_lookup_binary (
    piano_t *piano
  , const uint8_t *entity_id
  , size_t entity_id_size
  , int64_t *out_count
  , const int64_t **out_times
  );

#endif//__PIANO_INTERNAL_H
