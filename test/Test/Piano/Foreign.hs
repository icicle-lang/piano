{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Foreign where

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Jack

import qualified Data.Vector.Unboxed as Unboxed

import           P

import           Piano.Data
import           Piano.Foreign

import           System.IO (IO)

import           Test.Piano.Jack


prop_lookup_linear :: Property
prop_lookup_linear =
  gamble jKey $ \k0@(Key e t) ->
  gamble (listOf jKey) $ \ks0 ->
  testIO $ do
    let
      keys =
        fromKeys (k0 : ks0)

    piano <- newPiano keys
    mts <- lookupLinear piano $ entityId e

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

return []
tests :: IO Bool
tests =
  $disorderCheckEnvAll TestRunMore
