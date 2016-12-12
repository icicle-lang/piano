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

import           Control.Monad.ST (runST)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text.Encoding as T
import           Data.Thyme (Day)
import           Data.Thyme.Time (toGregorian)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Unboxed
import           Data.Word (Word8)

import           P

import           Piano.Data

import           Text.Printf (printf)

import qualified X.Data.Vector.Grow as Grow


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

parseKeys :: ByteString -> Either ParserError (Boxed.Vector Key)
parseKeys bss0@(PS fp _ _) =
  runST $ do
    u <- Grow.new 1024

    let
      boxKeys :: Unboxed.Vector (Int, Int, Day) -> Boxed.Vector Key
      boxKeys =
        fmap boxKey . Boxed.convert

      boxKey :: (Int, Int, Day) -> Key
      boxKey (off, len, time) =
        Key (PS fp off len) time

      loop bss1 =
        if B.null bss1 then
          Right . boxKeys <$> Grow.unsafeFreeze u
        else
          let
            (bs, bss2) =
              splitOn feed bss1
          in
            case parseKey bs of
              Left err ->
                pure $ Left err
              Right (Key (PS _ off len) time) -> do
                Grow.add u (off, len, time)
                loop bss2

    loop bss0
{-# INLINE parseKeys #-}

parseKey :: ByteString -> Either ParserError Key
parseKey bs =
  let
    (entity, time0) =
      splitOn pipe bs
  in
    if B.null time0 then
      Left $ ParserTimeMissing bs
    else do
      !time <- parseTime time0
      Right $ Key entity time
{-# INLINE parseKey #-}

parseTime :: ByteString -> Either ParserError Day
parseTime bs = do
  case parseDay bs of
    Left err ->
      Left $ ParserTimeError err
    Right (time, remains) ->
      if B.null remains then
        Right time
      else
        Left $ ParserUnsupportedTimeFormat bs
{-# INLINE parseTime #-}

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
