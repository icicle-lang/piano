{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Data where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Jack

import qualified Data.ByteString as B
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.List as List
import           Data.Thyme (Day)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Unboxed
import           Data.Word (Word8, Word32)

import           Foreign.ForeignPtr (ForeignPtr)

import           P

import           Piano.Data

import           System.IO (IO)

import           Test.Piano.Jack


fromUnboxedKeys :: ForeignPtr Word8 -> Unboxed.Vector (Word32, Int, Int, Day) -> [Key]
fromUnboxedKeys fp keys =
  let
    fromUnboxed (hash, off, len, time) =
      Key (unsafeMkEntity hash $ PS fp off len) time
  in
    fmap fromUnboxed $
    Unboxed.toList keys

toUnboxedKeys :: [Key] -> (ForeignPtr Word8, Unboxed.Vector (Word32, Int, Int, Day))
toUnboxedKeys keys =
  let
    PS fp off _ =
      B.concat $
      fmap (entityId . keyEntity) keys

    lengths =
      fmap (fromIntegral . B.length) $
      fmap (entityId . keyEntity) keys

    offsets =
      List.scanl' (+) off lengths

    hashes =
      fmap (entityHash . keyEntity) keys

    times =
      fmap keyTime keys
  in
    (fp, Unboxed.fromList $ List.zip4 hashes offsets lengths times)

prop_sort_keys :: Property
prop_sort_keys =
  gamble (listOf jKey) $ \keys ->
    List.sort keys
    ===
    Boxed.toList (sortKeys (Boxed.fromList keys))

prop_sort_unboxed_keys :: Property
prop_sort_unboxed_keys =
  gamble (listOf jKey) $ \keys ->
    List.sort keys
    ===
    let
      (fp, ukeys) =
        toUnboxedKeys keys
    in
      fromUnboxedKeys fp (sortUnboxedKeys fp ukeys)

return []
tests :: IO Bool
tests =
  $disorderCheckEnvAll TestRunMore
