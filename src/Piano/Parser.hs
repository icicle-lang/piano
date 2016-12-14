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
  , parseEndTime

  , renderKeys
  , renderKey
  , renderEndTime
  ) where

import           Anemone.Parser (TimeError, renderTimeError, parseDay)

import           Control.Monad.ST (runST)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Unsafe as B
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import           Data.Thyme.Time (toGregorian)
import qualified Data.Vector.Unboxed as Unboxed
import           Data.Word (Word8, Word32)

import           Foreign.ForeignPtr (ForeignPtr)

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
parsePiano bss0@(PS fp _ _) =
  runST $ do
    u <- Grow.new 1024

    let
      loop bss1 =
        if B.null bss1 then
          fromUnboxedKeys fp <$> Grow.unsafeFreeze u
        else
          let
            (bs, bss2) =
              splitOn feed bss1
          in
            case parseKey bs of
              Left err ->
                pure $ Left err
              Right (Key entity time) -> do
                let
                  hash =
                    entityHash entity

                  PS _ off len =
                    entityId entity

                Grow.add u (hash, off, len, time)
                loop bss2

    loop bss0
{-# INLINE parsePiano #-}

fromUnboxedKeys :: ForeignPtr Word8 -> Unboxed.Vector (Word32, Int, Int, EndTime) -> Either ParserError Piano
fromUnboxedKeys fp xs =
  let
    minTime =
      Unboxed.minimum $
      Unboxed.map (\(_, _, _, t) -> t) xs

    maxTime =
      Unboxed.maximum $
      Unboxed.map (\(_, _, _, t) -> t) xs

    entities =
      Map.fromAscListWith Set.union .
      fmap (fromUnboxedKey fp) .
      Unboxed.toList $
      sortUnboxedKeys fp xs
  in
    if Unboxed.null xs then
      Left ParserNoData
    else
      Right $ Piano minTime maxTime entities
{-# INLINE fromUnboxedKeys #-}

fromUnboxedKey :: ForeignPtr Word8 -> (Word32, Int, Int, EndTime) -> (Entity, Set EndTime)
fromUnboxedKey fp (hash, off, len, time) =
  (unsafeMkEntity hash $ PS fp off len, Set.singleton time)
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
      !time <- parseEndTime time0
      Right $ Key (mkEntity entity) time
{-# INLINE parseKey #-}

parseEndTime :: ByteString -> Either ParserError EndTime
parseEndTime bs = do
  case parseDay bs of
    Left err ->
      Left $ ParserTimeError err
    Right (time, remains) ->
      if B.null remains then
        Right $ fromInclusive time
      else
        Left $ ParserUnsupportedTimeFormat bs
{-# INLINE parseEndTime #-}

renderKeys :: [Key] -> ByteString
renderKeys =
  Char8.unlines . fmap renderKey

renderKey :: Key -> ByteString
renderKey (Key e t) =
  entityId e <> "|" <> renderEndTime t

renderEndTime :: EndTime -> ByteString
renderEndTime time =
  let
    (y, m, d) =
      toGregorian $
      fromExclusive time
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
