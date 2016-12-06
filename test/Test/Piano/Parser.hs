{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Parser where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Corpus (muppets)
import           Disorder.Jack

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Thyme (Day)
import           Data.Thyme.Time (fromGregorian)
import qualified Data.Vector as Boxed
import           Data.Word (Word8)

import           P

import           Piano.Data
import           Piano.Parser

import           System.IO (IO)


jKey :: Jack Key
jKey =
  Key
    <$> jEntity
    <*> jTime

jEntity :: Jack ByteString
jEntity =
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

prop_tripping_keys :: Property
prop_tripping_keys =
  gamble (Boxed.fromList <$> listOf jKey) $
    tripping renderKeys parseKeys

prop_tripping_key :: Property
prop_tripping_key =
  gamble jKey $
    tripping renderKey parseKey

prop_tripping_time :: Property
prop_tripping_time =
  gamble jTime $
    tripping renderTime parseTime

return []
tests :: IO Bool
tests =
  $disorderCheckEnvAll TestRunMore
