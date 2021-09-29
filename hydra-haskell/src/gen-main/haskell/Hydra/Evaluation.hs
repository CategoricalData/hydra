{-# LANGUAGE DeriveGeneric #-}
module Hydra.Evaluation
  ( Context(..)
  , EvaluationStrategy(..)
  , PrimitiveFunction(..)
  , TermStep(..)
  , _Context
  , _Context_elements
  , _Context_functions
  , _Context_strategy
  , _EvaluationStrategy
  , _EvaluationStrategy_opaqueTermVariants
  , _PrimitiveFunction
  , _PrimitiveFunction_implementation
  , _PrimitiveFunction_type
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
import Hydra.Graph

data Context
  = Context
    {-| @type map:
                keys: hydra/core.Name
                values: hydra/graph.Element -}
    { contextElements :: (Map Name Element)
    {-| @type map:
                keys: hydra/core.Name
                values: hydra/evaluation.PrimitiveFunction -}
    , contextFunctions :: (Map Name PrimitiveFunction)
    -- | @type hydra/evaluation.EvaluationStrategy
    , contextStrategy :: EvaluationStrategy }

data EvaluationStrategy
  = EvaluationStrategy
    {-| Whether a term of a given variant is considered to be fully reduced,
        without further inspection
        
        @type set: hydra/core.TermVariant -}
    { evaluationStrategyOpaqueTermVariants :: (Set TermVariant) } deriving (Eq, Generic, Ord, Read, Show)

data PrimitiveFunction
  = PrimitiveFunction
    {-| @type function:
                from:
                - list: hydra/core.Term
                to: hydra/core.Term -}
    { primitiveFunctionImplementation :: [Term] -> Term
    -- | @type hydra/core.FunctionType
    , primitiveFunctionType :: FunctionType }

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

_Context = "hydra/evaluation.Context" :: String
_Context_elements = "elements" :: String
_Context_functions = "functions" :: String
_Context_strategy = "strategy" :: String
_EvaluationStrategy = "hydra/evaluation.EvaluationStrategy" :: String
_EvaluationStrategy_opaqueTermVariants = "opaqueTermVariants" :: String
_PrimitiveFunction = "hydra/evaluation.PrimitiveFunction" :: String
_PrimitiveFunction_implementation = "implementation" :: String
_PrimitiveFunction_type = "type" :: String
_TermStep = "hydra/evaluation.TermStep" :: String
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
