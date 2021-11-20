{-# LANGUAGE DeriveGeneric #-}
module Hydra.Errors
  ( Qualified(..)
  , _Qualified
  , _Qualified_value
  , _Qualified_warnings
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set

data Qualified a
  = Qualified
    {-| @type optional:
                variable: a -}
    { qualifiedValue :: Maybe a
    -- | @type list: string
    , qualifiedWarnings :: [String] } deriving (Eq, Generic, Ord, Read, Show)

_Qualified = "hydra/errors.Qualified" :: String
_Qualified_value = "value" :: String
_Qualified_warnings = "warnings" :: String
