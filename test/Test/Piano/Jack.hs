{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Jack where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Thyme.Time (fromGregorian, toGregorian)
import           Data.Word (Word8)

import           Hedgehog
import           Hedgehog.Corpus
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Gen.QuickCheck
import qualified Hedgehog.Range as Range

import           P

import           Piano.Data

import           Text.Printf (printf)


jKey :: Gen Key
jKey =
  Key
    <$> jEntity
    <*> jLabel

jDateKey :: Gen Key
jDateKey =
  Key
    <$> jEntity
    <*> jDateLabel

jEntity :: Gen Entity
jEntity =
  fmap mkEntity $
  Gen.choice [
      Gen.element muppets
    , ByteString.pack <$> Gen.list (Range.linear 1 30) jNameChar
    ]

jNameChar :: Gen Word8
jNameChar =
  Gen.filter (\b -> b /= pipe && b /= feed) arbitrary

jPiano :: Gen Piano
jPiano =
  fromKeys <$> Gen.nonEmpty (Range.linear 1 1000) jDateKey

feed :: Word8
feed =
  0x0A -- '\n'

pipe :: Word8
pipe =
  0x7C -- '|'

jLabel :: Gen Label
jLabel =
  Label
    <$> jEndTime
    <*> Gen.choice [Gen.element boats, ByteString.pack <$> Gen.list (Range.linear 1 30) jNameChar]

jDateLabel :: Gen Label
jDateLabel = do
  time <- jEndTime
  case toGregorian $ fromExclusive time of
    (y, m, d) ->
      pure $ Label time (Char8.pack $ printf "%04d-%02d-%02d" y m d)

jEndTime :: Gen EndTime
jEndTime =
  fmap fromInclusive $
  fromGregorian
    <$> Gen.int (Range.linear 1600 3000)
    <*> Gen.int (Range.linear 1 12)
    <*> Gen.int (Range.linear 1 31)

toKeys :: Piano -> [Key]
toKeys =
  concatMap (\(e, ts) -> fmap (Key e) $ Set.toList ts) .
  Map.toList .
  pianoEntities
