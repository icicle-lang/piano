{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Jack where

import           Disorder.Corpus (muppets)
import           Disorder.Jack

import qualified Data.ByteString as B
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Thyme (Day)
import           Data.Thyme.Time (fromGregorian)
import           Data.Word (Word8)

import           P

import           Piano.Data


jKey :: Jack Key
jKey =
  Key
    <$> jEntity
    <*> jTime

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

feed :: Word8
feed =
  0x0A -- '\n'

pipe :: Word8
pipe =
  0x7C -- '|'

jTime :: Jack Day
jTime =
  fromGregorian
    <$> choose (1600, 3000)
    <*> choose (1, 12)
    <*> choose (1, 31)

fromKey :: Key -> (Entity, Set Day)
fromKey (Key e t) =
  (e, Set.singleton t)

fromKeys :: [Key] -> Map Entity (Set Day)
fromKeys =
  Map.fromListWith Set.union . fmap fromKey 

toKeys :: Map Entity (Set Day) -> [Key]
toKeys =
  concatMap (\(e, ts) -> fmap (Key e) $ Set.toList ts) .
  Map.toList
