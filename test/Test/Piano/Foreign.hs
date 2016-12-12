{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Foreign where

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Jack

import           Data.ByteString (ByteString)
import           Data.Thyme (Day)
import qualified Data.Vector.Unboxed as Unboxed

import           P

import           Piano.Data
import           Piano.Foreign

import           System.IO (IO)

import           Test.Piano.Jack


law_lookup :: (Piano -> ByteString -> IO (Maybe (Unboxed.Vector Day))) -> Property
law_lookup lookup =
  gamble jKey $ \k0@(Key e t) ->
  gamble (listOf jKey) $ \ks0 ->
  testIO $ do
    let
      keys =
        fromKeys (k0 : ks0)

    piano <- newPiano keys
    mts <- lookup piano $ entityId e

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

prop_lookup_linear :: Property
prop_lookup_linear =
  law_lookup lookupLinear

prop_lookup_binary :: Property
prop_lookup_binary =
  law_lookup lookupBinary

return []
tests :: IO Bool
tests =
  $disorderCheckEnvAll TestRunMore
