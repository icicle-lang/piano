{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Piano.Parser (
    ParserError(..)
  , renderParserError

  , parsePiano
  , parseKey
  , parseDate

  , renderInclusiveKeys
  , renderInclusiveKey
  , renderDate
  ) where

import           Anemone.Parser (TimeError, renderTimeError, parseDay)

import           Control.Monad.ST (runST)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Unsafe as B
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import           Data.Thyme (Day)
import           Data.Thyme.Time (toGregorian)
import qualified Data.Vector as Boxed
import           Data.Word (Word8, Word32)

import           P

import           Piano.Data

import           Text.Printf (printf)

import qualified X.Data.Vector.Grow as Grow


data ParserError =
    ParserTimeError !TimeError
  | ParserTimeMissing !ByteString
  | ParserUnsupportedTimeFormat !ByteString
  | ParserNoData
    deriving (Eq, Ord, Show)

renderParserError :: ParserError -> Text
renderParserError = \case
  ParserTimeError err ->
    "Failed parsing row: " <> renderTimeError err
  ParserTimeMissing bs ->
    "Failed parsing row, time field missing: " <> T.decodeUtf8 bs
  ParserUnsupportedTimeFormat bs ->
    "Failed parsing row, unsupported time format: " <> T.decodeUtf8 bs
  ParserNoData ->
    "Failed parsing chord descriptor, the file was empty."

parsePiano :: ByteString -> Either ParserError Piano
parsePiano bss0 =
  runST $ do
    u <- Grow.new 1024

    let
      loop bss1 =
        if B.null bss1 then
          fromUnboxedKeys <$> Grow.unsafeFreeze u
        else
          let
            (bs, bss2) =
              splitOn feed bss1
          in
            case parseKey bs of
              Left err ->
                pure $ Left err
              Right (Key entity (Label time label)) -> do
                let
                  hash =
                    entityHash entity

                Grow.add u (hash, entityId entity, time, label)
                loop bss2

    loop bss0
{-# INLINE parsePiano #-}

fromUnboxedKeys :: Boxed.Vector (Word32, ByteString, EndTime, ByteString) -> Either ParserError Piano
fromUnboxedKeys xs =
  let
    minTime =
      Boxed.minimum $
      Boxed.map (\(_, _, t, _) -> t) xs

    maxTime =
      Boxed.maximum $
      Boxed.map (\(_, _, t, _) -> t) xs

    maxCount =
      List.maximum .
      fmap List.length $
      Map.elems entities

    entities =
      Map.fromAscListWith Set.union .
      fmap fromUnboxedKey .
      Boxed.toList $
      sortUnboxedKeys xs
  in
    if Boxed.null xs then
      Left ParserNoData
    else
      Right $ Piano minTime maxTime maxCount entities
{-# INLINE fromUnboxedKeys #-}

fromUnboxedKey :: (Word32, ByteString, EndTime, ByteString) -> (Entity, Set Label)
fromUnboxedKey (hash, ent, time, lab) =
  (unsafeMkEntity hash $ ent, Set.singleton . Label time $ lab)
{-# INLINE fromUnboxedKey #-}

parseKey :: ByteString -> Either ParserError Key
parseKey bs =
  let
    (entity, time0) =
      splitOn pipe bs
  in
    if B.null time0 then
      Left $ ParserTimeMissing bs
    else do
      !time <- fromInclusive <$> parseDate time0
      Right $ Key (mkEntity entity) (Label time time0)
{-# INLINE parseKey #-}

parseDate :: ByteString -> Either ParserError Day
parseDate bs = do
  case parseDay bs of
    Left err ->
      Left $ ParserTimeError err
    Right (date, remains) ->
      if B.null remains then
        Right date
      else
        Left $ ParserUnsupportedTimeFormat bs
{-# INLINE parseDate #-}

renderInclusiveKeys :: [Key] -> ByteString
renderInclusiveKeys =
  Char8.unlines . fmap renderInclusiveKey

renderInclusiveKey :: Key -> ByteString
renderInclusiveKey (Key e (Label t _)) =
  entityId e <> "|" <> renderDate (fromExclusive t)

renderDate :: Day -> ByteString
renderDate date =
  let
    (y, m, d) =
      toGregorian date
  in
    Char8.pack $ printf "%04d-%02d-%02d" y m d

pipe :: Word8
pipe =
  0x7C -- '|'
{-# INLINE pipe #-}

feed :: Word8
feed =
  0x0A -- '\n'
{-# INLINE feed #-}

splitOn :: Word8 -> ByteString -> (ByteString, ByteString)
splitOn w bs =
  case B.elemIndex w bs of
    Nothing ->
      (bs, B.empty)
    Just ix ->
      (B.unsafeTake ix bs, B.unsafeDrop (ix + 1) bs)
{-# INLINE splitOn #-}
