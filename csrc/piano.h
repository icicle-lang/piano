#ifndef __PIANO_H
#define __PIANO_H

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

#endif//__PIANO_H
