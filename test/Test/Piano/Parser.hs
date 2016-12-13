{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Parser where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Either (testEither)
import           Disorder.Jack

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           P

import           Piano.Data
import           Piano.Parser

import           System.IO (IO)

import           Test.Piano.Jack


prop_parse_keys_valid_map :: Property
prop_parse_keys_valid_map =
  gamble jKey $ \k0@(Key e t) ->
  gamble (listOf jKey) $ \ks0 ->
    testEither renderParserError $ do
      let
        keys0 =
          k0 : ks0

        minTime0 =
          List.minimum $ fmap keyTime keys0

        maxTime0 =
          List.maximum $ fmap keyTime keys0

      Piano minTime maxTime ks <- parsePiano $ renderKeys keys0

      pure $ conjoin [
          counterexample "Minimum time was incorrect" $
            minTime0 === minTime

        , counterexample "Maximum time was incorrect" $
            maxTime0 === maxTime

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
  gamble (fromKeys <$> listOf1 jKey) $
    tripping (renderKeys . toKeys) parsePiano

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
