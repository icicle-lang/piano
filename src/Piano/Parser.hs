{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Piano.Parser (
    ParserError(..)
  , renderParserError

  , parseKeys
  , parseKey
  , parseTime

  , renderKeys
  , renderKey
  , renderTime
  ) where

import           Anemone.Parser (TimeError, renderTimeError, parseDay)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text.Encoding as T
import           Data.Thyme (Day)
import           Data.Thyme.Time (toGregorian)
import qualified Data.Vector as Boxed
import           Data.Word (Word8)

import           P

import           Piano.Data

import           Text.Printf (printf)


data ParserError =
    ParserTimeError !TimeError
  | ParserTimeMissing !ByteString
  | ParserUnsupportedTimeFormat !ByteString
    deriving (Eq, Ord, Show)

renderParserError :: ParserError -> Text
renderParserError = \case
  ParserTimeError err ->
    "Failed parsing row: " <> renderTimeError err
  ParserTimeMissing bs ->
    "Failed parsing row, time field missing: " <> T.decodeUtf8 bs
  ParserUnsupportedTimeFormat bs ->
    "Failed parsing row, unsupported time format: " <> T.decodeUtf8 bs

pipe :: Word8
pipe =
  0x7C -- '|'
{-# INLINE pipe #-}

parseKeys :: ByteString -> Either ParserError (Boxed.Vector Key)
parseKeys =
  Boxed.mapM parseKey . Boxed.fromList . Char8.lines
{-# INLINABLE parseKeys #-}

parseKey :: ByteString -> Either ParserError Key
parseKey bs =
  case B.elemIndex pipe bs of
    Nothing ->
      Left $ ParserTimeMissing bs

    Just ix -> do
      let
        !entity =
          B.unsafeTake ix bs

        !time0 =
          B.unsafeDrop (ix + 1) bs

      !time <- parseTime time0

      pure $ Key entity time
{-# INLINABLE parseKey #-}

parseTime :: ByteString -> Either ParserError Day
parseTime bs = do
  (!time, !remains) <- first ParserTimeError $ parseDay bs
  if B.null remains then
    pure time
  else
    Left $ ParserUnsupportedTimeFormat bs
{-# INLINABLE parseTime #-}

renderKeys :: Boxed.Vector Key -> ByteString
renderKeys =
  Char8.unlines . Boxed.toList . fmap renderKey

renderKey :: Key -> ByteString
renderKey (Key e t) =
  e <> "|" <> renderTime t

renderTime :: Day -> ByteString
renderTime day =
  let
    (y, m, d) =
      toGregorian day
  in
    Char8.pack $ printf "%04d-%02d-%02d" y m d
