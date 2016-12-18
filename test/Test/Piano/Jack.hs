{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Jack where

import           Disorder.Corpus (muppets)
import           Disorder.Jack

import qualified Data.ByteString as B
import           Data.Foldable (minimum, maximum)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Thyme.Time (fromGregorian)
import           Data.Word (Word8)

import           P

import           Piano.Data


jKey :: Jack Key
jKey =
  Key
    <$> jEntity
    <*> jEndTime

jEntity :: Jack Entity
jEntity =
  fmap mkEntity $
  oneOf [
      elements muppets
    , B.pack <$> listOf jEntityChar
    ]

jEntityChar :: Jack Word8
jEntityChar =
  arbitrary `suchThat` \b ->
    b /= pipe &&
    b /= feed

jPiano :: Jack Piano
jPiano =
  fromKeys <$> listOf1 jKey

feed :: Word8
feed =
  0x0A -- '\n'

pipe :: Word8
pipe =
  0x7C -- '|'

jEndTime :: Jack EndTime
jEndTime =
  fmap fromInclusive $
  fromGregorian
    <$> choose (1600, 3000)
    <*> choose (1, 12)
    <*> choose (1, 31)

fromKey :: Key -> (Entity, Set EndTime)
fromKey (Key e t) =
  (e, Set.singleton t)

fromKeys :: NonEmpty Key -> Piano
fromKeys ks =
  let
    minTime =
      minimum $ fmap keyTime ks

    maxTime =
      maximum $ fmap keyTime ks

    maxCount =
      maximum . fmap length $ Map.elems entities

    entities =
      Map.fromListWith Set.union . toList $ fmap fromKey ks
  in
    Piano minTime maxTime maxCount entities

toKeys :: Piano -> [Key]
toKeys =
  concatMap (\(e, ts) -> fmap (Key e) $ Set.toList ts) .
  Map.toList .
  pianoEntities
