#if CABAL
#include "anemone_hash.h"
#include "anemone_memcmp.h"
#else
#include "../lib/anemone/csrc/anemone_hash.h"
#include "../lib/anemone/csrc/anemone_memcmp.h"
#endif

#include "piano.h"

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

error_t piano_lookup_linear (
    piano_t *piano
  , const uint8_t *needle_id
  , size_t needle_id_size
  , int64_t *out_count
  , const int64_t **out_times
  )
{
    uint32_t needle_hash = piano_hash (needle_id, needle_id_size);

    int32_t count = piano->count;
    uint32_t *hashes = piano->hashes;

    piano_section32_t *id_sections = piano->id_sections;
    uint8_t *id_data = piano->id_data;

    piano_section32_t *time_sections = piano->time_sections;
    int64_t *time_data = piano->time_data;

    for (int32_t i = 0; i < count; i++) {
        uint32_t hash = hashes[i];
        piano_section32_t id_section = id_sections[i];
        piano_section32_t time_section = time_sections[i];

        int64_t cmp = piano_compare (
            needle_hash
          , needle_id
          , needle_id_size
          , hash
          , piano_section32_uint8_start (id_section, id_data)
          , id_section.length
          );

        if (cmp != 0) {
            continue;
        }

        *out_count = time_section.length;
        *out_times = piano_section32_int64_start (time_section, time_data);

        return 0;
    }

    *out_count = 0;
    *out_times = NULL;

    return 0;
}

error_t piano_lookup_binary (
    piano_t *piano
  , const uint8_t *needle_id
  , size_t needle_id_size
  , int64_t *out_count
  , const int64_t **out_times
  )
{
    uint32_t needle_hash = piano_hash (needle_id, needle_id_size);

    int32_t count = piano->count;
    uint32_t *hashes = piano->hashes;

    piano_section32_t *id_sections = piano->id_sections;
    uint8_t *id_data = piano->id_data;

    piano_section32_t *time_sections = piano->time_sections;
    int64_t *time_data = piano->time_data;

    int32_t lo = 0;
    int32_t hi = count - 1;

    for (;;) {
        int32_t m = (hi + lo) / 2;

        uint32_t hash = hashes[m];
        piano_section32_t id_section = id_sections[m];
        piano_section32_t time_section = time_sections[m];

        int64_t cmp = piano_compare (
            needle_hash
          , needle_id
          , needle_id_size
          , hash
          , piano_section32_uint8_start (id_section, id_data)
          , id_section.length
          );

        if (cmp == 0) {
            *out_count = time_section.length;
            *out_times = piano_section32_int64_start (time_section, time_data);
            return 0;
        }

        if (cmp > 0) {
            lo = m + 1;
        } else {
            hi = m - 1;
        }

        if (lo > hi) {
            *out_count = 0;
            *out_times = NULL;
            return 0;
        }
    }
}
