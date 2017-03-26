{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Piano.Foreign.Bindings where

import Anemone.Foreign.Data (CError(..))

--
-- This module contains 1:1 bindings for all the zebra header files, in the
-- style of bindings-DSL, for "nice" wrappers, see the Piano.Foreign module.
--

#include <bindings.dsl.h>
#include "piano_bindings.h"
#include "piano.h"
#include "piano_internal.h"

#strict_import

#starttype struct piano_section32
#field offset , Int32
#field length , Int32
#stoptype

#starttype struct piano
#field min_time , Int64
#field max_time , Int64
#field max_count , Int32
#field buckets , Ptr <piano_section32>
#field count , Int32
#field hashes , Ptr Word32
#field id_sections , Ptr <piano_section32>
#field id_data , Ptr Word8
#field label_sections, Ptr <piano_section32>
#field label_time_data , Ptr Int64
#field label_name_offsets , Ptr Int64
#field label_name_lengths , Ptr Int64
#field label_name_data , Ptr Word8
#stoptype

#ccall_unsafe piano_min_time , Ptr <piano> -> IO Int64
#ccall_unsafe piano_max_time , Ptr <piano> -> IO Int64
#ccall_unsafe piano_max_count , Ptr <piano> -> IO Int64
#ccall_unsafe piano_lookup , Ptr <piano> -> Ptr Word8 -> CSize -> Ptr Int64 -> Ptr (Ptr Int64) -> Ptr (Ptr Int64) -> Ptr (Ptr Int64) -> Ptr (Ptr Word8) -> IO CError
#ccall_unsafe piano_lookup_binary , Ptr <piano> -> Ptr Word8 -> CSize -> Ptr Int64 -> Ptr (Ptr Int64) -> Ptr (Ptr Int64) -> Ptr (Ptr Int64) -> Ptr (Ptr Word8) -> IO CError
