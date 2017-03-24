{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Piano.Data (
    Piano(..)
  , fromKeys

  , EndTime(..)
  , fromInclusive
  , fromExclusive
  , ivoryEpoch

  , Label(..)

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
import qualified Data.Foldable as Foldable
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
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
    -- | The earliest chord time across all of the entities.
      pianoMinTime :: !EndTime

    -- | The latest chord time across all of the entities.
    , pianoMaxTime :: !EndTime

    -- | The maximum number of chord times associated with any given entity.
    , pianoMaxCount :: !Int

    -- | The chord times we need to query for each entity.
    , pianoEntities :: !(Map Entity (Set Label))
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

data Label =
  Label {
      labelTime :: !EndTime
    , labelName :: !ByteString
    } deriving (Eq, Ord, Generic)

data Entity =
  Entity {
      entityHash :: !Word32
    , entityId :: !ByteString
    } deriving (Eq, Ord, Generic)

data Key =
  Key {
      keyEntity :: !Entity
    , keyLabel :: !Label
    } deriving (Eq, Ord, Generic)

instance American.Lexicographic Key where
  terminate (Key (Entity _ eid) (Label _ lb)) n =
    -- Unforunately we can't write this in terms of the other instances, so we
    -- have to cheat and inline their implementations:
    --
    --   n >= sizeof(entity hash) + sizeof(entity id) + sizeof(label time) + sizeof(label name)
    --
    n >= 4 + B.length eid + 8 + B.length lb
  {-# INLINE terminate #-}

  size _ =
    American.size (Savage.undefined :: Word32) `max`
    American.size (Savage.undefined :: ByteString) `max`
    American.size (Savage.undefined :: Int64)
  {-# INLINE size #-}

  index i (Key (Entity h e) (Label (EndTime t) l)) =
    if i < 4 then
      American.index i h
    else if i < 4 + B.length e then
      American.index (i - 4) e
    else if i < 4 + B.length e + 8 then
      American.index (i - 4 - B.length e) t
    else
      American.index (i - 4 - B.length e - 8) l
  {-# INLINE index #-}

instance Show EndTime where
  showsPrec =
    gshowsPrec

instance Show Label where
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

instance NFData Label

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
  Unboxed.Vector (Word32, Int, Int, EndTime, Int, Int) ->
  Unboxed.Vector (Word32, Int, Int, EndTime, Int, Int)
sortUnboxedKeys fp keys =
  unsafePerformIO $ do
    mkeys <- Unboxed.thaw keys

    let
      cmp (hash0, eoff0, elen0, time0, loff0, llen0) (hash1, eoff1, elen1, time1, loff1, llen1) =
        compare
          (Key (Entity hash0 (PS fp eoff0 elen0)) (Label time0 (PS fp loff0 llen0)))
          (Key (Entity hash1 (PS fp eoff1 elen1)) (Label time1 (PS fp loff1 llen1)))

      terminate (hash, eoff, elen, time, loff, llen) n =
        American.terminate (Key (Entity hash (PS fp eoff elen)) (Label time (PS fp loff llen))) n

      size =
        American.size (Key (Entity 0 B.empty) (Label (EndTime 0) B.empty))

      index i (hash, eoff, elen, time, loff, llen) =
        American.index i (Key (Entity hash (PS fp eoff elen)) (Label time (PS fp loff llen)))

    American.sortBy cmp terminate size index mkeys

    Unboxed.unsafeFreeze mkeys

fromKey :: Key -> (Entity, Set Label)
fromKey (Key e t) =
  (e, Set.singleton t)

fromKeys :: NonEmpty Key -> Piano
fromKeys ks =
  let
    minTime =
      Foldable.minimum $ fmap (labelTime . keyLabel) ks

    maxTime =
      Foldable.maximum $ fmap (labelTime . keyLabel) ks

    maxCount =
      Foldable.maximum . fmap length $ Map.elems entities

    entities =
      Map.fromListWith Set.union . toList $ fmap fromKey ks
  in
    Piano minTime maxTime maxCount entities

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
