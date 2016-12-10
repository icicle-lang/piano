#if CABAL
#include "anemone_base.h"
#else
#include "../lib/anemone/csrc/anemone_base.h"
#endif

typedef struct piano_section32 {
    int32_t offset;
    int32_t length;
} piano_section32_t;

typedef struct piano {
    int32_t count;
    uint32_t *hashes;

    piano_section32_t *id_sections;
    uint8_t *id_data;

    piano_section32_t *time_sections;
    int64_t *time_data;
} piano_t;

error_t piano_lookup_linear (
    piano_t *piano
  , const uint8_t *entity_id
  , size_t entity_id_size
  , int64_t *out_count
  , const int64_t **out_times
  );
