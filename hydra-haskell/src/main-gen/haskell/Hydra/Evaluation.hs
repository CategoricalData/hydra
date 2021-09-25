{-# LANGUAGE DeriveGeneric #-}
module Hydra.Evaluation
  ( Context(..)
  , EvaluationStrategy(..)
  , PrimitiveFunction(..)
  , _Context
  , _Context_elements
  , _Context_functions
  , _Context_reductionStyle
  , _EvaluationStrategy
  , _EvaluationStrategy_opaqueTermVariants
  , _PrimitiveFunction
  , _PrimitiveFunction_implementation
  , _PrimitiveFunction_type
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
    , contextReductionStyle :: EvaluationStrategy }

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

_Context = "hydra/evaluation.Context" :: String
_Context_elements = "elements" :: String
_Context_functions = "functions" :: String
_Context_reductionStyle = "reductionStyle" :: String
_EvaluationStrategy = "hydra/evaluation.EvaluationStrategy" :: String
_EvaluationStrategy_opaqueTermVariants = "opaqueTermVariants" :: String
_PrimitiveFunction = "hydra/evaluation.PrimitiveFunction" :: String
_PrimitiveFunction_implementation = "implementation" :: String
_PrimitiveFunction_type = "type" :: String
