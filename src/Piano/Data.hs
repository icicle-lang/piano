{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Piano.Data (
    Key(..)
  ) where

import           Data.ByteString (ByteString)
import           Data.Thyme (Day)

import           GHC.Generics (Generic)

import           P

import           X.Text.Show (gshowsPrec)


data Key =
  Key {
      keyEntity :: !ByteString
    , keyTime :: !Day
    } deriving (Eq, Ord, Generic)

instance Show Key where
  showsPrec =
    gshowsPrec
