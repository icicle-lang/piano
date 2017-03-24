#ifndef __PIANO_H
#define __PIANO_H

#if CABAL
#include "anemone_base.h"
#else
#include "../lib/anemone/csrc/anemone_base.h"
#endif

//
// Abstract piano_t data structure.
//
typedef struct piano piano_t;

//
// Returns the earliest chord time across all of the entities.
//
int64_t piano_min_time (
    piano_t *piano
  );

//
// Returns the latest chord time across all of the entities.
//
int64_t piano_max_time (
    piano_t *piano
  );

//
// Returns the maximum number of chord times associated with any given entity.
//
// This can be useful if you're processing queries on a per entity basis and
// would like to pre-allocate some data structures which you will reuse for
// every entity.
//
int64_t piano_max_count (
    piano_t *piano
  );

//
// Lookup an entity in the chord descriptor.
//
// If the entity is found, its array of labeled chord times are returned,
// sorted oldest to newest. The times returned are an exclusive bound on the
// scope of the chord query for the entity requested.
//
// Times are in seconds since 1600-03-01 (ivory epoch).
//
// If the entity is not found, the |out_count| will be set to 0.
//
error_t piano_lookup (
    piano_t *piano
  , const uint8_t *needle_id
  , size_t needle_id_size
  , int64_t *out_count
  , const int64_t **out_label_times
  , const int64_t **out_label_name_offsets
  , const int64_t **out_label_name_lengths
  , const uint8_t **out_label_name_data
  );

#endif//__PIANO_H
