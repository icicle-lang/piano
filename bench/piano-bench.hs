{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Criterion.Main (Benchmark, env, bgroup, bench, nf, nfIO)
import           Criterion.Main (defaultMainWith, defaultConfig)
import           Criterion.Types (Config(..))

import           GHC.Generics (Generic)

import           P

import           Piano.Data
import           Piano.Foreign

import           System.IO (IO)

import           Text.Printf (printf)


main :: IO ()
main =
  defaultMainWith config [
      benchmark
    ]

config :: Config
config =
  defaultConfig {
      reportFile =
        Just "dist/build/piano-bench.html"
    , csvFile =
        Just "dist/build/piano-bench.csv"
    }

data Env =
  Env {
      envPiano :: !Piano
    , envForeign :: !ForeignPiano
    } deriving (Generic)

instance NFData Env

mkEnv :: IO Env
mkEnv = do
  let
    time =
      EndTime 0

    mkKey n =
      (entity n, Set.singleton time)

    piano =
      Piano time time 1 .
      Map.fromAscList $
      fmap mkKey [0..entityCount - 1]

  Env piano <$> newForeignPiano piano

entityCount :: Int
entityCount =
  1000000

entity :: Int -> Entity
entity =
  mkEntity . Char8.pack . printf "SAVAGE+%040d"

benchmark :: Benchmark
benchmark =
  env mkEnv $ \ ~x ->
  let
    !needle =
      entity $ entityCount - 1
  in
    bgroup "lookup" [
        bench "Data.Map" $
          nf (Map.lookup needle . pianoEntities . envPiano) x
      , bench "binary-search" $
          nfIO (lookupBinary (envForeign x) (entityId needle))
      , bench "hashy-search" $
          nfIO (lookup (envForeign x) (entityId needle))
      ]
