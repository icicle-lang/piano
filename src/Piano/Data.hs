{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Piano.Data (
    Entity
  , mkEntity
  , unsafeMkEntity
  , entityHash
  , entityId

  , Key(..)
  , sortKeys
  , sortUnboxedKeys

  , toIvorySeconds
  , fromIvorySeconds
  , ivoryEpoch
  ) where

import           Anemone.Foreign.Hash (fasthash32')

import qualified Data.ByteString as B
import           Data.ByteString.Internal (ByteString(..))
import           Data.Thyme (Day(..))
import           Data.Thyme.Time (addDays, diffDays)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Algorithms.AmericanFlag as American
import qualified Data.Vector.Unboxed as Unboxed
import           Data.Word (Word8, Word32)

import           GHC.Generics (Generic)

import           Foreign.ForeignPtr (ForeignPtr)

import           P

import qualified Prelude as Savage

import           System.IO.Unsafe (unsafePerformIO)

import           X.Text.Show (gshowsPrec)

data Entity =
  Entity {
      entityHash :: !Word32
    , entityId :: !ByteString
    } deriving (Eq, Ord, Generic)

data Key =
  Key {
      keyEntity :: !Entity
    , keyTime :: !Day
    } deriving (Eq, Ord, Generic)

instance American.Lexicographic Key where
  terminate (Key (Entity _ bs) _) n =
    -- Unforunately we can't write this in terms of the other instances, so we
    -- have to cheat and inline their implementations:
    --
    --   n >= sizeof(entity hash) + sizeof(entity id) + sizeof(day)
    --
    n >= 4 + B.length bs + 8
  {-# INLINE terminate #-}

  size _ =
    American.size (Savage.undefined :: Word32) `max`
    American.size (Savage.undefined :: ByteString) `max`
    American.size (Savage.undefined :: Int)
  {-# INLINE size #-}

  index i (Key (Entity h e) t) =
    if i < 4 then
      American.index i h
    else if i < 4 + B.length e then
      American.index (i - 4) e
    else
      American.index (i - 4 - B.length e) (toModifiedJulianDay t)
  {-# INLINE index #-}

instance Show Entity where
  showsPrec =
    gshowsPrec

instance Show Key where
  showsPrec =
    gshowsPrec

instance NFData Entity

instance NFData Key

sortKeys :: Boxed.Vector Key -> Boxed.Vector Key
sortKeys keys =
  unsafePerformIO $ do
    mkeys <- Boxed.thaw keys
    American.sort mkeys
    Boxed.unsafeFreeze mkeys

sortUnboxedKeys ::
  ForeignPtr Word8 ->
  Unboxed.Vector (Word32, Int, Int, Day) ->
  Unboxed.Vector (Word32, Int, Int, Day)
sortUnboxedKeys fp keys =
  unsafePerformIO $ do
    mkeys <- Unboxed.thaw keys

    let
      cmp (hash0, off0, len0, day0) (hash1, off1, len1, day1) =
        compare
          (Key (Entity hash0 (PS fp off0 len0)) day0)
          (Key (Entity hash1 (PS fp off1 len1)) day1)

      terminate (hash, off, len, day) n =
        American.terminate (Key (Entity hash (PS fp off len)) day) n

      size =
        American.size (Key (Entity 0 B.empty) ivoryEpoch)

      index i (hash, off, len, day) =
        American.index i (Key (Entity hash (PS fp off len)) day)

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

toIvorySeconds :: Day -> Int64
toIvorySeconds day =
  fromIntegral $ (diffDays day ivoryEpoch) * 86400
{-# INLINE toIvorySeconds #-}

fromIvorySeconds :: Int64 -> Day
fromIvorySeconds time =
  addDays (fromIntegral time `div` 86400) ivoryEpoch
{-# INLINE fromIvorySeconds #-}

ivoryEpoch :: Day
ivoryEpoch =
  ModifiedJulianDay (-94493) -- 1600-03-01
{-# INLINE ivoryEpoch #-}
