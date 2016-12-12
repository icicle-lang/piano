{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_piano
import           DependencyInfo_ambiata_piano

import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as B

import           P

import           Piano.Parser

import           System.IO (IO, FilePath, BufferMode(..))
import           System.IO (hSetBuffering, stdout, stderr, putStrLn, print)
import           System.Exit (exitSuccess)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)
import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Options.Applicative (Parser, Mod, CommandFields)
import           X.Options.Applicative (SafeCommand(..), RunType(..))
import           X.Options.Applicative (dispatch, safeCommand, command')
import           X.Options.Applicative (help, metavar, argument, str, subparser)


data PianoCommand =
  PianoCheck !FilePath
  deriving (Eq, Show)

data PianoError =
  PianoParserError !ParserError
  deriving (Eq, Ord, Show)

renderPianoError :: PianoError -> Text
renderPianoError = \case
  PianoParserError err ->
    renderParserError err

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch parser >>= \sc ->
    case sc of
      VersionCommand ->
        putStrLn buildInfoVersion >> exitSuccess
      DependencyCommand ->
        mapM_ putStrLn dependencyInfo
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        orDie renderPianoError $ run c

parser :: Parser (SafeCommand PianoCommand)
parser =
  safeCommand . subparser . mconcat $ commands

commands :: [Mod CommandFields PianoCommand]
commands = [
    command' "check" "Validate that a chord descriptor can be loaded." pPianoCheck
  ]

pPianoCheck :: Parser PianoCommand
pPianoCheck =
  PianoCheck
    <$> pDescriptor

pDescriptor :: Parser FilePath
pDescriptor =
  argument str $
    metavar "PATH" <>
    help "The path to a chord descriptor"

run :: PianoCommand -> EitherT PianoError IO ()
run = \case
  PianoCheck path -> do
    bs <- liftIO $ B.readFile path
    _ <- firstT PianoParserError . hoistEither $ parseKeys bs
    pure ()
