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
  ) where

import           Anemone.Foreign.Hash (fasthash32')

import           Control.Monad.ST (runST)

import qualified Data.ByteString as B
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.Foldable as Foldable
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Thyme (Day(..))
import           Data.Thyme.Time (addDays, diffDays)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Algorithms.AmericanFlag as American
import           Data.Word (Word32)

import           GHC.Generics (Generic)

import           P

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
  extent (Key (Entity _ eid) (Label _ lb)) =
    -- Unforunately we can't write this in terms of the other instances, so we
    -- have to cheat and inline their implementations:
    --
    --   n >= sizeof(entity hash) + sizeof(entity id) + sizeof(label time) + sizeof(label name)
    --
    4 + B.length eid + 8 + B.length lb
  {-# INLINE extent #-}

  size _ =
    American.size (Proxy :: Proxy Word32) `max`
    American.size (Proxy :: Proxy ByteString) `max`
    American.size (Proxy :: Proxy Int64)
  {-# INLINE size #-}

  index i (Key (Entity h e) (Label (EndTime t) l))
    | i < 4 = American.index i h
    | i < 4 + B.length e = American.index (i - 4) e
    | i < 4 + B.length e + 8 = American.index (i - 4 - B.length e) t
    | otherwise = American.index (i - 4 - B.length e - 8) l
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

sortKeys :: Boxed.Vector Key -> Boxed.Vector Key
sortKeys keys =
  runST $ do
    mkeys <- Boxed.thaw keys
    American.sort mkeys
    Boxed.unsafeFreeze mkeys

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
