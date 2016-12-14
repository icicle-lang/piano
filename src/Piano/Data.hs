{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Piano.Data (
    Piano(..)

  , EndTime(..)
  , fromInclusive
  , fromExclusive
  , ivoryEpoch

  , Entity
  , mkEntity
  , unsafeMkEntity
  , entityHash
  , entityId

  , Key(..)
  , sortKeys
  , sortUnboxedKeys
  ) where

import           Anemone.Foreign.Hash (fasthash32')

import qualified Data.ByteString as B
import           Data.ByteString.Internal (ByteString(..))
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Thyme (Day(..))
import           Data.Thyme.Time (addDays, diffDays)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Algorithms.AmericanFlag as American
import qualified Data.Vector.Unboxed as Unboxed
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Data.Word (Word8, Word32)

import           GHC.Generics (Generic)

import           Foreign.ForeignPtr (ForeignPtr)

import           P

import qualified Prelude as Savage

import           System.IO.Unsafe (unsafePerformIO)

import           X.Text.Show (gshowsPrec)


data Piano =
  Piano {
      pianoMinTime :: !EndTime
    , pianoMaxTime :: !EndTime
    , pianoEntities :: !(Map Entity (Set EndTime))
    } deriving (Eq, Ord, Show, Generic)

-- | A chord time range is a point in time which exclusively bounds the scope
--   of facts we are interested in for a given entity.
--
--   Chord files are pairs of <entity>|<date> where the <date> is an inclusive
--   bound on the scope of facts. So if we parse the date 2016-01-01, then the
--   'EndTime' will be 2016-01-02 00:00:00.
--
newtype EndTime =
  EndTime {
      unEndTime :: Int64
    } deriving (Eq, Ord, Generic)

data Entity =
  Entity {
      entityHash :: !Word32
    , entityId :: !ByteString
    } deriving (Eq, Ord, Generic)

data Key =
  Key {
      keyEntity :: !Entity
    , keyTime :: !EndTime
    } deriving (Eq, Ord, Generic)

instance American.Lexicographic Key where
  terminate (Key (Entity _ bs) _) n =
    -- Unforunately we can't write this in terms of the other instances, so we
    -- have to cheat and inline their implementations:
    --
    --   n >= sizeof(entity hash) + sizeof(entity id) + sizeof(time)
    --
    n >= 4 + B.length bs + 8
  {-# INLINE terminate #-}

  size _ =
    American.size (Savage.undefined :: Word32) `max`
    American.size (Savage.undefined :: ByteString) `max`
    American.size (Savage.undefined :: Int64)
  {-# INLINE size #-}

  index i (Key (Entity h e) (EndTime t)) =
    if i < 4 then
      American.index i h
    else if i < 4 + B.length e then
      American.index (i - 4) e
    else
      American.index (i - 4 - B.length e) t
  {-# INLINE index #-}

instance Show EndTime where
  showsPrec =
    gshowsPrec

instance Show Entity where
  showsPrec =
    gshowsPrec

instance Show Key where
  showsPrec =
    gshowsPrec

instance NFData Piano

instance NFData EndTime

instance NFData Entity

instance NFData Key

derivingUnbox "EndTime"
  [t| EndTime -> Int64 |]
  [| unEndTime |]
  [| EndTime |]

sortKeys :: Boxed.Vector Key -> Boxed.Vector Key
sortKeys keys =
  unsafePerformIO $ do
    mkeys <- Boxed.thaw keys
    American.sort mkeys
    Boxed.unsafeFreeze mkeys

sortUnboxedKeys ::
  ForeignPtr Word8 ->
  Unboxed.Vector (Word32, Int, Int, EndTime) ->
  Unboxed.Vector (Word32, Int, Int, EndTime)
sortUnboxedKeys fp keys =
  unsafePerformIO $ do
    mkeys <- Unboxed.thaw keys

    let
      cmp (hash0, off0, len0, time0) (hash1, off1, len1, time1) =
        compare
          (Key (Entity hash0 (PS fp off0 len0)) time0)
          (Key (Entity hash1 (PS fp off1 len1)) time1)

      terminate (hash, off, len, time) n =
        American.terminate (Key (Entity hash (PS fp off len)) time) n

      size =
        American.size (Key (Entity 0 B.empty) (EndTime 0))

      index i (hash, off, len, time) =
        American.index i (Key (Entity hash (PS fp off len)) time)

    American.sortBy cmp terminate size index mkeys

    Unboxed.unsafeFreeze mkeys

mkEntity :: ByteString -> Entity
mkEntity bs =
  Entity (hashEntity bs) bs
{-# INLINE mkEntity #-}

unsafeMkEntity :: Word32 -> ByteString -> Entity
unsafeMkEntity =
  Entity
{-# INLINE unsafeMkEntity #-}

hashEntity :: ByteString -> Word32
hashEntity =
  -- NOTE: This is duplicated in piano.c, be sure to update
  -- NOTE: it there too if you change this.
  fasthash32' 0xd97ab4d1cade4055
{-# INLINE hashEntity #-}

fromInclusive :: Day -> EndTime
fromInclusive day =
  let
    !endDay =
      addDays 1 day
  in
    EndTime . fromIntegral $
      (diffDays endDay ivoryEpoch) * 86400
{-# INLINE fromInclusive #-}

fromExclusive :: EndTime -> Day
fromExclusive (EndTime time) =
  let
    !endDays =
      fromIntegral time `div` 86400

    !days =
      endDays - 1
  in
    addDays days ivoryEpoch
{-# INLINE fromExclusive #-}

ivoryEpoch :: Day
ivoryEpoch =
  ModifiedJulianDay (-94493) -- 1600-03-01
{-# INLINE ivoryEpoch #-}
