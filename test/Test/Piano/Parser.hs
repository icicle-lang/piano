{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Parser where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Either (testEither)
import           Disorder.Jack

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           P

import           Piano.Data
import           Piano.Parser

import           System.IO (IO)

import           Test.Piano.Jack


prop_parse_keys_valid_map :: Property
prop_parse_keys_valid_map =
  gamble jDateKey $ \k0@(Key e t) ->
  gamble (listOf jDateKey) $ \ks0 ->
    testEither renderParserError $ do
      let
        keys0 =
          k0 :| ks0

        piano0 =
          fromKeys keys0

      Piano minTime maxTime maxCount ks <-
        parsePiano . renderInclusiveKeys $ toList keys0

      pure $ conjoin [
          counterexample "Minimum time was incorrect" $
            pianoMinTime piano0 === minTime

        , counterexample "Maximum time was incorrect" $
            pianoMaxTime piano0 === maxTime

        , counterexample "Maximum count was incorrect" $
            pianoMaxCount piano0 === maxCount

        , case Map.lookup e ks of
            Nothing ->
              counterexample ("Entity not found: " <> show e) $
              False
            Just ts ->
              counterexample ("Found entity: " <> show (e, ts)) $
              counterexample ("Time was missing: " <> show t) $
              Set.member t ts
        ]

prop_tripping_keys :: Property
prop_tripping_keys =
  gamble (fromKeys <$> listOf1 jDateKey) $
    tripping (renderInclusiveKeys . toKeys) parsePiano

prop_tripping_key :: Property
prop_tripping_key =
  gamble jDateKey $
    tripping renderInclusiveKey parseKey

prop_tripping_date :: Property
prop_tripping_date =
  gamble (fromExclusive <$> jEndTime) $
    tripping renderDate parseDate

return []
tests :: IO Bool
tests =
  $disorderCheckEnvAll TestRunMore
