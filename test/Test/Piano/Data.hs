{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Data where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Jack

import qualified Data.ByteString as ByteString
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.List as List
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Unboxed
import           Data.Word (Word8, Word32)

import           Foreign.ForeignPtr (ForeignPtr)

import           P

import           Piano.Data

import           System.IO (IO)

import           Test.Piano.Jack


fromUnboxedKeys :: ForeignPtr Word8 -> Unboxed.Vector (Word32, Int, Int, EndTime, Int, Int) -> [Key]
fromUnboxedKeys fp keys =
  let
    fromUnboxed (hash, eoff, elen, time, loff, llen) =
      Key (unsafeMkEntity hash $ PS fp eoff elen) (Label time $ PS fp loff llen)
  in
    fmap fromUnboxed $
    Unboxed.toList keys

toUnboxedKeys :: [Key] -> (ForeignPtr Word8, Unboxed.Vector (Word32, Int, Int, EndTime, Int, Int))
toUnboxedKeys keys =
  let
    entities =
      ByteString.concat $ fmap (entityId . keyEntity) keys

    labels =
      ByteString.concat $ fmap (labelName . keyLabel) keys

    PS fp off _ =
      entities <> labels

    elengths =
      fmap (fromIntegral . ByteString.length) $
      fmap (entityId . keyEntity) keys

    eoffsets =
      List.scanl' (+) off elengths

    hashes =
      fmap (entityHash . keyEntity) keys

    times =
      fmap (labelTime . keyLabel) keys

    llengths =
      fmap (fromIntegral . ByteString.length) $
      fmap (labelName . keyLabel) keys

    loffsets =
      List.scanl' (+) (off + ByteString.length entities) llengths
  in
    (fp, Unboxed.fromList $ List.zip6 hashes eoffsets elengths times loffsets llengths)

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
