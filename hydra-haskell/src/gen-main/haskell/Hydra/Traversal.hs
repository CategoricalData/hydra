{-# LANGUAGE DeriveGeneric #-}
module Hydra.Traversal
  ( Result(..)
  , Step(..)
  , StepDirection(..)
  , TermStep(..)
  , _Result
  , _Result_failure
  , _Result_success
  , _Step
  , _StepDirection
  , _StepDirection_in
  , _StepDirection_out
  , _Step_in
  , _Step_out
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

data Result a
  -- | @type variable: a
  = ResultSuccess a
  -- | @type string
  | ResultFailure String

data Step a b
  = Step
    {-| @type function:
                from:
                - variable: a
                to:
                  parameterized:
                    genericType: hydra/traversal.Result
                    parameters:
                    - type:
                        variable: b
                      variable: a -}
    { stepOut :: a -> (Result b)
    {-| @type function:
                from:
                - variable: b
                to:
                  parameterized:
                    genericType: hydra/traversal.Result
                    parameters:
                    - type:
                        variable: a
                      variable: a -}
    , stepIn :: b -> (Result a) }

data StepDirection
  = StepDirectionOut
  | StepDirectionIn deriving (Eq, Generic, Ord, Read, Show)

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

_Result = "hydra/traversal.Result" :: String
_Result_failure = "failure" :: String
_Result_success = "success" :: String
_Step = "hydra/traversal.Step" :: String
_StepDirection = "hydra/traversal.StepDirection" :: String
_StepDirection_in = "in" :: String
_StepDirection_out = "out" :: String
_Step_in = "in" :: String
_Step_out = "out" :: String
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
