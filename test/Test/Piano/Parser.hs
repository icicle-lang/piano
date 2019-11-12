{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Parser where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Control.Monad.Trans.Either

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           P

import           Piano.Data
import           Piano.Parser

import           System.IO (IO)

import           Test.Piano.Jack


prop_parse_keys_valid_map :: Property
prop_parse_keys_valid_map = property $ do
  k0@(Key e t) <- forAll jDateKey
  ks0          <- forAll (Gen.list (Range.linear 1 100) jDateKey)

  test . evalExceptT $ do
    let
      keys0 =
        k0 :| ks0

      piano0 =
        fromKeys keys0

    Piano minTime maxTime maxCount ks <-
      hoistEither $
        parsePiano . renderInclusiveKeys $ toList keys0

    annotate "Minimum time was incorrect"
    pianoMinTime piano0 === minTime

    annotate "Maximum time was incorrect"
    pianoMaxTime piano0 === maxTime

    annotate "Maximum count was incorrect"
    pianoMaxCount piano0 === maxCount

    case Map.lookup e ks of
      Nothing -> do
        annotate $ "Entity not found: " <> show e
        failure

      Just ts -> do
        annotate ("Found entity: " <> show (e, ts))
        annotate ("Time was missing: " <> show t)
        assert $ Set.member t ts


prop_tripping_keys :: Property
prop_tripping_keys = property $
  forAll (fromKeys <$> Gen.nonEmpty (Range.linear 1 100) jDateKey) >>=
    tripping `flip` (renderInclusiveKeys . toKeys) `flip` parsePiano

prop_tripping_key :: Property
prop_tripping_key = property $
  forAll jDateKey >>=
    tripping `flip` renderInclusiveKey `flip` parseKey

prop_tripping_date :: Property
prop_tripping_date = property $
  forAll (fromExclusive <$> jEndTime) >>=
    tripping `flip` renderDate `flip` parseDate

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
