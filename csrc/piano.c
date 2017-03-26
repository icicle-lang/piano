#if CABAL
#include "anemone_hash.h"
#include "anemone_memcmp.h"
#else
#include "../lib/anemone/csrc/anemone_hash.h"
#include "../lib/anemone/csrc/anemone_memcmp.h"
#endif

#include "piano.h"
#include "piano_internal.h"

#include <stdio.h>

// NOTE: This is duplicated in Piano.Data, be sure to update
// NOTE: it there too if you change this.
static const uint64_t piano_seed = 0xd97ab4d1cade4055;

ANEMONE_STATIC
ANEMONE_INLINE
uint32_t piano_hash (const uint8_t *id, size_t id_size)
{
    return anemone_fasthash32 (piano_seed, id, id_size);
}

ANEMONE_STATIC
ANEMONE_INLINE
int64_t piano_memcmp (const uint8_t* buf0, size_t len0, const uint8_t* buf1, size_t len1)
{
    size_t len = len0 < len1 ? len0 : len1;
    int64_t cmp = anemone_memcmp (buf0, buf1, len);

    if (cmp == 0) {
        /* if buf0 is shorter, it is smaller */
        return (int64_t)len0 - (int64_t)len1;
    } else {
        return cmp;
    }
}

ANEMONE_STATIC
ANEMONE_INLINE
int64_t piano_compare (
    uint32_t hash0
  , const uint8_t *id0
  , size_t id_len0
  , uint32_t hash1
  , const uint8_t *id1
  , size_t id_len1
  )
{
    int64_t hash_cmp = (int64_t)hash0 - (int64_t)hash1;

    if (hash_cmp != 0) {
        return hash_cmp;
    }

    return piano_memcmp (id0, id_len0, id1, id_len1);
}

ANEMONE_STATIC
ANEMONE_INLINE
uint8_t * piano_section32_uint8_start (piano_section32_t section, uint8_t *data)
{
    return data + section.offset;
}

ANEMONE_STATIC
ANEMONE_INLINE
int64_t * piano_section32_int64_start (piano_section32_t section, int64_t *data)
{
    return data + section.offset;
}

int64_t piano_min_time (piano_t *piano)
{
    return piano->min_time;
}

int64_t piano_max_time (piano_t *piano)
{
    return piano->max_time;
}

int64_t piano_max_count (piano_t *piano)
{
    return piano->max_count;
}

error_t piano_lookup_binary (
    piano_t *piano
  , const uint8_t *needle_id
  , size_t needle_id_size
  , int64_t *out_count
  , const int64_t **out_label_times
  , const int64_t **out_label_name_offsets
  , const int64_t **out_label_name_lengths
  , const uint8_t **out_label_name_data
  )
{
    uint32_t needle_hash = piano_hash (needle_id, needle_id_size);

    int32_t count = piano->count;
    uint32_t *hashes = piano->hashes;

    piano_section32_t *id_sections = piano->id_sections;
    uint8_t *id_data = piano->id_data;

    piano_section32_t *label_sections = piano->label_sections;
    int64_t *label_time_data = piano->label_time_data;
    int64_t *label_name_offsets = piano->label_name_offsets;
    int64_t *label_name_lengths = piano->label_name_lengths;
    uint8_t *label_name_data = piano->label_name_data;

    int32_t lo = 0;
    int32_t hi = count - 1;

    for (;;) {
        int32_t m = (hi + lo) / 2;

        uint32_t hash = hashes[m];
        piano_section32_t id_section = id_sections[m];
        piano_section32_t label_section = label_sections[m];

        int64_t cmp = piano_compare (
            needle_hash
          , needle_id
          , needle_id_size
          , hash
          , piano_section32_uint8_start (id_section, id_data)
          , id_section.length
          );

        if (cmp == 0) {
            *out_count = label_section.length;
            *out_label_times = piano_section32_int64_start (label_section, label_time_data);
            *out_label_name_offsets = piano_section32_int64_start (label_section, label_name_offsets);
            *out_label_name_lengths = piano_section32_int64_start (label_section, label_name_lengths);
            *out_label_name_data = label_name_data;
            return 0;
        }

        if (cmp > 0) {
            lo = m + 1;
        } else {
            hi = m - 1;
        }

        if (lo > hi) {
            *out_count = 0;
            *out_label_times = NULL;
            *out_label_name_offsets = NULL;
            *out_label_name_lengths = NULL;
            *out_label_name_data = NULL;
            return 0;
        }
    }
}

error_t piano_lookup (
    piano_t *piano
  , const uint8_t *needle_id
  , size_t needle_id_size
  , int64_t *out_count
  , const int64_t **out_label_times
  , const int64_t **out_label_name_offsets
  , const int64_t **out_label_name_lengths
  , const uint8_t **out_label_name_data
  )
{
    uint32_t needle_hash = piano_hash (needle_id, needle_id_size);

    piano_section32_t *buckets = piano->buckets;
    piano_section32_t bucket = buckets[needle_hash >> 16];

    int32_t offset = bucket.offset;
    int32_t count = bucket.length;

    uint32_t *hashes = piano->hashes + offset;

    piano_section32_t *id_sections = piano->id_sections + offset;
    uint8_t *id_data = piano->id_data;

    piano_section32_t *label_sections = piano->label_sections + offset;
    int64_t *label_time_data = piano->label_time_data;
    int64_t *label_name_offsets = piano->label_name_offsets;
    int64_t *label_name_lengths = piano->label_name_lengths;
    uint8_t *label_name_data = piano->label_name_data;

    int32_t lo = 0;
    int32_t hi = count - 1;

    for (;;) {
        int32_t m = (hi + lo) / 2;

        uint32_t hash = hashes[m];
        piano_section32_t id_section = id_sections[m];

        int64_t cmp = piano_compare (
            needle_hash
          , needle_id
          , needle_id_size
          , hash
          , piano_section32_uint8_start (id_section, id_data)
          , id_section.length
          );

        if (cmp == 0) {
            piano_section32_t label_section = label_sections[m];

            *out_count = label_section.length;
            *out_label_times = piano_section32_int64_start (label_section, label_time_data);
            *out_label_name_offsets = piano_section32_int64_start (label_section, label_name_offsets);
            *out_label_name_lengths = piano_section32_int64_start (label_section, label_name_lengths);
            *out_label_name_data = label_name_data;

            return 0;
        }

        if (cmp > 0) {
            lo = m + 1;
        } else {
            hi = m - 1;
        }

        if (lo > hi) {
            *out_count = 0;
            *out_label_times = NULL;
            *out_label_name_offsets = NULL;
            *out_label_name_lengths = NULL;
            *out_label_name_data = NULL;
            return 0;
        }
    }
}
