{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module Piano.Foreign (
    ForeignPiano(..)
  , newForeignPiano

  , CPiano(..)
  , withCPiano

  , lookup
  , lookupBinary

  , getMinTime
  , getMaxTime
  , getMaxCount
  ) where

import           Anemone.Foreign.Data (CError(..), CSize(..))

import           Data.Bits (shiftR)
import qualified Data.ByteString as ByteString
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Internal as ByteString
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable
import           Data.Word (Word8, Word32)

import           Foreign.Concurrent (newForeignPtr)
import           Foreign.ForeignPtr (ForeignPtr, newForeignPtr_, withForeignPtr)
import           Foreign.Marshal (alloca, malloc, mallocBytes, newArray, free)
import           Foreign.Ptr (Ptr, plusPtr)
import           Foreign.Storable (peek, poke)

import           P

import           Piano.Data
import           Piano.Foreign.Bindings

import           System.IO (IO)


newtype ForeignPiano =
  ForeignPiano {
      unPiano :: ForeignPtr C'piano
    }

newtype CPiano =
  CPiano {
      unCPiano :: Ptr C'piano
    }

instance NFData ForeignPiano where
  rnf !_ =
    ()

allocBuckets :: [Word32] -> IO (Ptr C'piano_section32)
allocBuckets hs =
  let
    bucketShift :: Int
    bucketShift =
      16

    maxBucket :: Word32
    maxBucket =
      maxBound `shiftR` bucketShift

    bucket :: Word32 -> Word32
    bucket h =
      fromIntegral (h `shiftR` bucketShift) :: Word32

    fromHash :: Word32 -> (Word32, Int32)
    fromHash h =
      (bucket h, 1)

    buckets0 :: Map Word32 Int32
    buckets0 =
      Map.fromAscList $
      fmap (, 0) [0..maxBucket]

    bucketsN :: Map Word32 Int32
    bucketsN =
      Map.fromAscListWith (+) $
      fmap fromHash hs

    buckets :: Map Word32 Int32
    buckets =
      Map.union bucketsN buckets0

    lengths :: [Int32]
    lengths =
      Map.elems buckets

    offsets :: [Int32]
    offsets =
      List.scanl' (+) 0 lengths
  in
    newArray $ List.zipWith C'piano_section32 offsets lengths

allocStringSections :: [ByteString] -> IO (Ptr C'piano_section32)
allocStringSections bss =
  let
    lengths =
      fmap (fromIntegral . ByteString.length) bss

    offsets =
      List.scanl' (+) 0 lengths
  in
    newArray $ List.zipWith C'piano_section32 offsets lengths

allocStringData :: [ByteString] -> IO (Ptr Word8)
allocStringData bss = do
  let
    PS fp off len =
      ByteString.concat bss

  dst <- mallocBytes len

  withForeignPtr fp $ \src ->
    ByteString.memcpy (dst `plusPtr` off) src len

  pure dst

allocaLabelSections :: [Set a] -> IO (Ptr C'piano_section32)
allocaLabelSections tss =
  let
    lengths =
      fmap (fromIntegral . Set.size) tss

    offsets =
      List.scanl' (+) 0 lengths
  in
    newArray $ List.zipWith C'piano_section32 offsets lengths

allocLabelTimeData :: [Set Label] -> IO (Ptr Int64)
allocLabelTimeData =
  newArray . fmap (unEndTime . labelTime) . concatMap Set.toList

allocLabelNameOffsets :: [Set Label] -> IO (Ptr Int64)
allocLabelNameOffsets =
  newArray . List.scanl' (+) 0 . fmap (fromIntegral . ByteString.length . labelName) . concatMap Set.toList

allocLabelNameLengths :: [Set Label] -> IO (Ptr Int64)
allocLabelNameLengths =
  newArray . fmap (fromIntegral . ByteString.length . labelName) . concatMap Set.toList

allocLabelNameData :: [Set Label] -> IO (Ptr Word8)
allocLabelNameData =
  allocStringData . fmap labelName . concatMap Set.toList

allocPiano :: Piano -> IO (Ptr C'piano)
allocPiano (Piano minTime maxTime maxCount entities) = do
  pPiano <- malloc
  pBuckets <- allocBuckets . fmap entityHash $ Map.keys entities
  pHashes <- newArray . fmap entityHash $ Map.keys entities
  pIdSections <- allocStringSections . fmap entityId $ Map.keys entities
  pIdData <- allocStringData . fmap entityId $ Map.keys entities
  pLabelSections <- allocaLabelSections $ Map.elems entities
  pLabelTimeData <- allocLabelTimeData $ Map.elems entities
  pLabelNameOffsets <- allocLabelNameOffsets $ Map.elems entities
  pLabelNameLengths <- allocLabelNameLengths $ Map.elems entities
  pLabelNameData <- allocLabelNameData $ Map.elems entities

  poke pPiano C'piano {
      c'piano'min_time = unEndTime minTime
    , c'piano'max_time = unEndTime maxTime
    , c'piano'max_count = fromIntegral maxCount
    , c'piano'buckets = pBuckets
    , c'piano'count = fromIntegral (Map.size entities)
    , c'piano'hashes = pHashes
    , c'piano'id_sections = pIdSections
    , c'piano'id_data = pIdData
    , c'piano'label_sections = pLabelSections
    , c'piano'label_time_data = pLabelTimeData
    , c'piano'label_name_offsets = pLabelNameOffsets
    , c'piano'label_name_lengths = pLabelNameLengths
    , c'piano'label_name_data = pLabelNameData
    }

  pure pPiano

freePiano :: Ptr C'piano -> IO ()
freePiano pPiano = do
  piano <- peek pPiano
  free pPiano

  free $ c'piano'buckets piano
  free $ c'piano'hashes piano
  free $ c'piano'id_sections piano
  free $ c'piano'id_data piano
  free $ c'piano'label_sections piano
  free $ c'piano'label_time_data piano
  free $ c'piano'label_name_offsets piano
  free $ c'piano'label_name_lengths piano
  free $ c'piano'label_name_data piano

newForeignPiano :: Piano -> IO ForeignPiano
newForeignPiano piano = do
  ptr <- allocPiano piano
  ForeignPiano <$> newForeignPtr ptr (freePiano ptr)

withCPiano :: ForeignPiano -> (CPiano -> IO a) -> IO a
withCPiano (ForeignPiano fp) io =
  withForeignPtr fp $ \ptr ->
    io (CPiano ptr)

type ForeignLookup =
  Ptr C'piano -> Ptr Word8 -> CSize -> Ptr Int64 -> Ptr (Ptr Int64) -> Ptr (Ptr Int64) -> Ptr (Ptr Int64) -> Ptr (Ptr Word8) -> IO CError

lookup :: ForeignPiano -> ByteString -> IO (Maybe (Boxed.Vector Label))
lookup =
  lookupWith unsafe'c'piano_lookup

lookupBinary :: ForeignPiano -> ByteString -> IO (Maybe (Boxed.Vector Label))
lookupBinary =
  lookupWith unsafe'c'piano_lookup_binary

lookupWith :: ForeignLookup -> ForeignPiano -> ByteString -> IO (Maybe (Boxed.Vector Label))
lookupWith c_lookup (ForeignPiano pfp) (PS nfp noff nlen) =
  withForeignPtr pfp $ \pptr ->
  withForeignPtr nfp $ \nptr ->
  alloca $ \pcount ->
  alloca $ \pptimes ->
  alloca $ \ppoffsets ->
  alloca $ \pplengths ->
  alloca $ \ppnames -> do
    err <- c_lookup pptr (nptr `plusPtr` noff) (fromIntegral nlen) pcount pptimes ppoffsets pplengths ppnames

    label_count <- fromIntegral <$> peek pcount

    if err == 0 && label_count /= 0 then do
      ptimes <- peek pptimes
      fptimes <- newForeignPtr_ ptimes

      poffsets <- peek ppoffsets
      fpoffsets <- newForeignPtr_ poffsets

      plengths <- peek pplengths
      fplengths <- newForeignPtr_ plengths

      pnames <- peek ppnames
      fpnames <- newForeignPtr_ pnames

      let
        times =
          Boxed.map EndTime .  Boxed.convert $ Storable.unsafeFromForeignPtr0 fptimes label_count

        offsets =
          Boxed.convert $ Storable.unsafeFromForeignPtr0 fpoffsets label_count

        lengths =
          Boxed.convert $ Storable.unsafeFromForeignPtr0 fplengths label_count

        names =
          Boxed.map ByteString.copy $
          Boxed.zipWith (\off len -> PS fpnames (fromIntegral off) (fromIntegral len)) offsets lengths

        -- force the evaluation as the pointers won't exist when we return
        !labels =
          Boxed.zipWith Label times names

      pure $ Just labels
    else
      pure Nothing
{-# INLINE lookupWith #-}

getMinTime :: ForeignPiano -> IO EndTime
getMinTime fp =
  fmap EndTime . withCPiano fp $
    unsafe'c'piano_min_time . unCPiano

getMaxTime :: ForeignPiano -> IO EndTime
getMaxTime fp =
  fmap EndTime . withCPiano fp $
    unsafe'c'piano_max_time . unCPiano

getMaxCount :: ForeignPiano -> IO Int
getMaxCount fp =
  fmap fromIntegral . withCPiano fp $
    unsafe'c'piano_max_count . unCPiano
