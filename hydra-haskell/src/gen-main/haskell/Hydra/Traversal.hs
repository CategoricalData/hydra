{-# LANGUAGE DeriveGeneric #-}
module Hydra.Traversal
  ( TermStep(..)
  , _TermStep
  , _TermStep_applicationArgument
  , _TermStep_applicationFunction
  , _TermStep_case
  , _TermStep_compareTo
  , _TermStep_lambdaBody
  , _TermStep_list
  , _TermStep_mapKey
  , _TermStep_mapValue
  , _TermStep_record
  , _TermStep_set
  , _TermStep_union
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set
import Hydra.Core

data TermStep
  = TermStepApplicationFunction
  | TermStepApplicationArgument
  -- | @type hydra/core.FieldName
  | TermStepCase FieldName
  | TermStepCompareTo
  | TermStepLambdaBody
  -- | @type integer
  | TermStepList Int
  -- | @type integer
  | TermStepMapKey Int
  -- | @type integer
  | TermStepMapValue Int
  -- | @type hydra/core.FieldName
  | TermStepRecord FieldName
  -- | @type integer
  | TermStepSet Int
  | TermStepUnion deriving (Eq, Generic, Ord, Read, Show)

_TermStep = "hydra/traversal.TermStep" :: String
_TermStep_applicationArgument = "applicationArgument" :: String
_TermStep_applicationFunction = "applicationFunction" :: String
_TermStep_case = "case" :: String
_TermStep_compareTo = "compareTo" :: String
_TermStep_lambdaBody = "lambdaBody" :: String
_TermStep_list = "list" :: String
_TermStep_mapKey = "mapKey" :: String
_TermStep_mapValue = "mapValue" :: String
_TermStep_record = "record" :: String
_TermStep_set = "set" :: String
_TermStep_union = "union" :: String
