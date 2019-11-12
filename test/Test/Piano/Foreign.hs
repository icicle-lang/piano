{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Foreign where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Control.Monad.IO.Class (liftIO)

import           Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Boxed

import           P

import           Piano.Data
import           Piano.Foreign

import           System.IO (IO)

import           Test.Piano.Jack


law_lookup :: (ForeignPiano -> ByteString -> IO (Maybe (Boxed.Vector Label))) -> Property
law_lookup lookupFn =
  property $ do
    keys' <- forAll $ Gen.nonEmpty (Range.linear 1 100) jKey
    test $ do
      let
        Key e l =
          NonEmpty.head keys'

        keys =
          fromKeys keys'

      piano <- liftIO $ newForeignPiano keys
      mts   <- liftIO $ lookupFn piano $ entityId e

      case mts of
        Nothing -> do
          annotate ("Entity not found: " <> show e)
          failure
        Just ts -> do
          annotate ("Found entity: " <> show (e, ts))
          annotate ("Label was missing: " <> show l)
          assert $
            Boxed.elem l ts

prop_lookup :: Property
prop_lookup =
  law_lookup lookup

prop_lookup_binary :: Property
prop_lookup_binary =
  law_lookup lookupBinary

prop_get_min_time :: Property
prop_get_min_time =
  property $
    forAll jPiano >>= \piano ->
    test $ do
      foreign        <- liftIO $ newForeignPiano piano
      foreignMinTime <- liftIO $ getMinTime foreign
      pianoMinTime piano === foreignMinTime

prop_get_max_time :: Property
prop_get_max_time =
  property $
    forAll jPiano >>= \piano ->
    test $ do
      foreign        <- liftIO $ newForeignPiano piano
      foreignMaxTime <- liftIO $ getMaxTime foreign

      pianoMaxTime piano === foreignMaxTime

prop_get_max_count :: Property
prop_get_max_count =
  property $
    forAll jPiano >>= \piano ->
      test $ do
        foreign         <- liftIO $ newForeignPiano piano
        foreignMaxCount <- liftIO $ getMaxCount foreign
        pianoMaxCount piano === foreignMaxCount

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
