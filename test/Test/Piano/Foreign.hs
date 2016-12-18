{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Foreign where

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Jack

import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector.Unboxed as Unboxed

import           P

import           Piano.Data
import           Piano.Foreign

import           System.IO (IO)

import           Test.Piano.Jack


law_lookup :: (ForeignPiano -> ByteString -> IO (Maybe (Unboxed.Vector EndTime))) -> Property
law_lookup lookupFn =
  gamble jKey $ \k0@(Key e t) ->
  gamble (listOf jKey) $ \ks0 ->
  testIO $ do
    let
      keys =
        fromKeys (k0 :| ks0)

    piano <- newForeignPiano keys
    mts <- lookupFn piano $ entityId e

    case mts of
      Nothing ->
        pure .
          counterexample ("Entity not found: " <> show e) $
          False
      Just ts ->
        pure .
          counterexample ("Found entity: " <> show (e, ts)) $
          counterexample ("Time was missing: " <> show t) $
          Unboxed.elem t ts

prop_lookup :: Property
prop_lookup =
  law_lookup lookup

prop_lookup_binary :: Property
prop_lookup_binary =
  law_lookup lookupBinary

prop_get_min_time :: Property
prop_get_min_time =
  gamble jPiano $ \piano ->
  testIO $ do
    foreign <- newForeignPiano piano
    foreignMinTime <- getMinTime foreign
    pure $
      pianoMinTime piano
      ===
      foreignMinTime

prop_get_max_time :: Property
prop_get_max_time =
  gamble jPiano $ \piano ->
  testIO $ do
    foreign <- newForeignPiano piano
    foreignMaxTime <- getMaxTime foreign
    pure $
      pianoMaxTime piano
      ===
      foreignMaxTime

prop_get_max_count :: Property
prop_get_max_count =
  gamble jPiano $ \piano ->
  testIO $ do
    foreign <- newForeignPiano piano
    foreignMaxCount <- getMaxCount foreign
    pure $
      pianoMaxCount piano
      ===
      foreignMaxCount

return []
tests :: IO Bool
tests =
  $disorderCheckEnvAll TestRunMore
