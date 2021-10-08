{-# LANGUAGE DeriveGeneric #-}
module Hydra.Evaluation
  ( Context(..)
  , EvaluationStrategy(..)
  , PrimitiveFunction(..)
  , Result(..)
  , Step(..)
  , StepDirection(..)
  , _Context
  , _Context_elements
  , _Context_functions
  , _Context_graphs
  , _Context_strategy
  , _EvaluationStrategy
  , _EvaluationStrategy_opaqueTermVariants
  , _PrimitiveFunction
  , _PrimitiveFunction_implementation
  , _PrimitiveFunction_name
  , _PrimitiveFunction_type
  , _Result
  , _Result_failure
  , _Result_success
  , _Step
  , _StepDirection
  , _StepDirection_in
  , _StepDirection_out
  , _Step_in
  , _Step_out
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set
import Hydra.Core
import Hydra.Graph

data Context
  = Context
    -- | @type hydra/graph.GraphSet
    { contextGraphs :: GraphSet
    {-| @type map:
                keys: hydra/core.Name
                values: hydra/graph.Element -}
    , contextElements :: (Map Name Element)
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
    -- | @type hydra/core.Name
    { primitiveFunctionName :: Name
    -- | @type hydra/core.FunctionType
    , primitiveFunctionType :: FunctionType
    {-| @type function:
                from:
                - list: hydra/core.Term
                to: hydra/core.Term -}
    , primitiveFunctionImplementation :: [Term] -> Term }

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
                    genericType: hydra/evaluation.Result
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
                    genericType: hydra/evaluation.Result
                    parameters:
                    - type:
                        variable: a
                      variable: a -}
    , stepIn :: b -> (Result a) }

data StepDirection
  = StepDirectionOut
  | StepDirectionIn deriving (Eq, Generic, Ord, Read, Show)

_Context = "hydra/evaluation.Context" :: String
_Context_elements = "elements" :: String
_Context_functions = "functions" :: String
_Context_graphs = "graphs" :: String
_Context_strategy = "strategy" :: String
_EvaluationStrategy = "hydra/evaluation.EvaluationStrategy" :: String
_EvaluationStrategy_opaqueTermVariants = "opaqueTermVariants" :: String
_PrimitiveFunction = "hydra/evaluation.PrimitiveFunction" :: String
_PrimitiveFunction_implementation = "implementation" :: String
_PrimitiveFunction_name = "name" :: String
_PrimitiveFunction_type = "type" :: String
_Result = "hydra/evaluation.Result" :: String
_Result_failure = "failure" :: String
_Result_success = "success" :: String
_Step = "hydra/evaluation.Step" :: String
_StepDirection = "hydra/evaluation.StepDirection" :: String
_StepDirection_in = "in" :: String
_StepDirection_out = "out" :: String
_Step_in = "in" :: String
_Step_out = "out" :: String
