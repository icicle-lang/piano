{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Parser where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Either (testEither)
import           Disorder.Jack

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
      ks <- parseKeys $ renderKeys (k0 : ks0)
      case Map.lookup e ks of
        Nothing ->
          pure .
            counterexample ("Entity not found: " <> show e) $
            False
        Just ts ->
          pure .
            counterexample ("Found entity: " <> show (e, ts)) $
            counterexample ("Time was missing: " <> show t) $
            Set.member t ts

prop_tripping_keys :: Property
prop_tripping_keys =
  gamble (fromKeys <$> listOf jKey) $
    tripping (renderKeys . toKeys) parseKeys

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
