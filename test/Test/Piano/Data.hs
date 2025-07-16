{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Piano.Data where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


import qualified Data.List as List
import qualified Data.Vector as Boxed

import           P

import           Piano.Data

import           System.IO (IO)

import           Test.Piano.Jack

prop_sort_keys :: Property
prop_sort_keys =
  property $
    forAll (Gen.list (Range.linear 1 1000) jKey) >>= \keys ->
      List.sort keys === Boxed.toList (sortKeys (Boxed.fromList keys))

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
