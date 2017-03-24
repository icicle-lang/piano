{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Jack where

import           Disorder.Corpus (muppets, boats)
import           Disorder.Jack

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Thyme.Time (fromGregorian, toGregorian)
import           Data.Word (Word8)

import           P

import           Piano.Data

import           Text.Printf (printf)


jKey :: Jack Key
jKey =
  Key
    <$> jEntity
    <*> jLabel

jDateKey :: Jack Key
jDateKey =
  Key
    <$> jEntity
    <*> jDateLabel

jEntity :: Jack Entity
jEntity =
  fmap mkEntity $
  oneOf [
      elements muppets
    , ByteString.pack <$> listOf jNameChar
    ]

jNameChar :: Jack Word8
jNameChar =
  arbitrary `suchThat` \b ->
    b /= pipe &&
    b /= feed

jPiano :: Jack Piano
jPiano =
  fromKeys <$> listOf1 jDateKey

feed :: Word8
feed =
  0x0A -- '\n'

pipe :: Word8
pipe =
  0x7C -- '|'

jLabel :: Jack Label
jLabel =
  Label
    <$> jEndTime
    <*> oneOf [elements boats, ByteString.pack <$> listOf jNameChar]

jDateLabel :: Jack Label
jDateLabel = do
  time <- jEndTime
  case toGregorian $ fromExclusive time of
    (y, m, d) ->
      pure $ Label time (Char8.pack $ printf "%04d-%02d-%02d" y m d)

jEndTime :: Jack EndTime
jEndTime =
  fmap fromInclusive $
  fromGregorian
    <$> choose (1600, 3000)
    <*> choose (1, 12)
    <*> choose (1, 31)

toKeys :: Piano -> [Key]
toKeys =
  concatMap (\(e, ts) -> fmap (Key e) $ Set.toList ts) .
  Map.toList .
  pianoEntities
