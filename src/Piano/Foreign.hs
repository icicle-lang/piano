{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Piano.Foreign (
    Piano(..)
  , newPiano
  , lookupLinear
  , lookupBinary
  ) where

import           Anemone.Foreign.Data (CError(..), CSize(..))

import qualified Data.ByteString as B
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Internal as B
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Thyme (Day(..))
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Unboxed
import           Data.Word (Word8)

import           Foreign.Concurrent (newForeignPtr)
import           Foreign.ForeignPtr (ForeignPtr, newForeignPtr_, withForeignPtr)
import           Foreign.Marshal (alloca, malloc, mallocBytes, newArray, free)
import           Foreign.Ptr (Ptr, plusPtr)
import           Foreign.Storable (peek, poke)

import           P

import           Piano.Data
import           Piano.Foreign.Bindings

import           System.IO (IO)


newtype Piano =
  Piano {
      unPiano :: ForeignPtr C'piano
    }

instance NFData Piano where
  rnf !_ =
    ()

allocIdSections :: [ByteString] -> IO (Ptr C'piano_section32)
allocIdSections bss =
  let
    lengths =
      fmap (fromIntegral . B.length) bss

    offsets =
      List.scanl' (+) 0 lengths
  in
    newArray $ List.zipWith C'piano_section32 offsets lengths

allocIdData :: [ByteString] -> IO (Ptr Word8)
allocIdData bss = do
  let
    PS fp off len =
      B.concat bss

  dst <- mallocBytes len

  withForeignPtr fp $ \src ->
    B.memcpy (dst `plusPtr` off) src len

  pure dst

allocTimeSections :: [Set a] -> IO (Ptr C'piano_section32)
allocTimeSections tss =
  let
    lengths =
      fmap (fromIntegral . Set.size) tss

    offsets =
      List.scanl' (+) 0 lengths
  in
    newArray $ List.zipWith C'piano_section32 offsets lengths

allocTimeData :: [Set Int64] -> IO (Ptr Int64)
allocTimeData =
  newArray . concatMap Set.toList

allocPiano :: Map Entity (Set Day) -> IO (Ptr C'piano)
allocPiano entities = do
  pPiano <- malloc
  pHashes <- newArray . fmap entityHash $ Map.keys entities
  pIdSections <- allocIdSections . fmap entityId $ Map.keys entities
  pIdData <- allocIdData . fmap entityId $ Map.keys entities
  pTimeSections <- allocTimeSections $ Map.elems entities
  pTimeData <- allocTimeData . fmap (Set.mapMonotonic toIvorySeconds) $ Map.elems entities

  poke pPiano C'piano {
      c'piano'count = fromIntegral (Map.size entities)
    , c'piano'hashes = pHashes
    , c'piano'id_sections = pIdSections
    , c'piano'id_data = pIdData
    , c'piano'time_sections = pTimeSections
    , c'piano'time_data = pTimeData
    }

  pure pPiano

freePiano :: Ptr C'piano -> IO ()
freePiano pPiano = do
  piano <- peek pPiano
  free pPiano

  free $ c'piano'hashes piano
  free $ c'piano'id_sections piano
  free $ c'piano'id_data piano
  free $ c'piano'time_sections piano
  free $ c'piano'time_data piano

newPiano :: Map Entity (Set Day) -> IO Piano
newPiano keys = do
  ptr <- allocPiano keys
  Piano <$> newForeignPtr ptr (freePiano ptr)

type ForeignLookup =
  Ptr C'piano -> Ptr Word8 -> CSize -> Ptr Int64 -> Ptr (Ptr Int64) -> IO CError

lookupLinear :: Piano -> ByteString -> IO (Maybe (Unboxed.Vector Day))
lookupLinear =
  lookupWith c'piano_lookup_linear

lookupBinary :: Piano -> ByteString -> IO (Maybe (Unboxed.Vector Day))
lookupBinary =
  lookupWith c'piano_lookup_binary

lookupWith :: ForeignLookup -> Piano -> ByteString -> IO (Maybe (Unboxed.Vector Day))
lookupWith c_lookup (Piano pfp) (PS nfp noff nlen) =
  withForeignPtr pfp $ \pptr ->
  withForeignPtr nfp $ \nptr ->
  alloca $ \pcount ->
  alloca $ \pptimes -> do
    err <- c_lookup pptr (nptr `plusPtr` noff) (fromIntegral nlen) pcount pptimes

    time_count <- fromIntegral <$> peek pcount

    if err == 0 && time_count /= 0 then do
      ptimes <- peek pptimes
      fptimes <- newForeignPtr_ ptimes

      let
        -- force the evaluation as 'ptimes' won't exist when we return
        !times =
          Unboxed.map fromIvorySeconds .
          Unboxed.convert $
          Storable.unsafeFromForeignPtr0 fptimes time_count

      pure $ Just times
    else
      pure Nothing
{-# INLINE lookupWith #-}
