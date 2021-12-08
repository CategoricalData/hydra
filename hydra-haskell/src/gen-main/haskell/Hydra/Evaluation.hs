{-# LANGUAGE DeriveGeneric #-}
module Hydra.Evaluation
  ( Context(..)
  , EvaluationStrategy(..)
  , InputSpec(..)
  , OutputSpec(..)
  , PrimitiveFunction(..)
  , Result(..)
  , Step(..)
  , StepDirection(..)
  , _Context
  , _Context_descriptionOf
  , _Context_elements
  , _Context_functions
  , _Context_graphs
  , _Context_setTypeOf
  , _Context_strategy
  , _Context_typeOf
  , _EvaluationStrategy
  , _EvaluationStrategy_opaqueTermVariants
  , _InputSpec
  , _InputSpec_type
  , _InputSpec_unmarshal
  , _OutputSpec
  , _OutputSpec_marshal
  , _OutputSpec_type
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

data Context a
  = Context
    {-| @type parameterized:
                genericType: hydra/graph.GraphSet
                parameters:
                - type:
                    variable: a
                  variable: a -}
    { contextGraphs :: GraphSet a
    {-| @type map:
                keys: hydra/core.Name
                values:
                  parameterized:
                    genericType: hydra/graph.Element
                    parameters:
                    - type:
                        variable: a
                      variable: a -}
    , contextElements :: (Map (Name) (Element a))
    {-| @type map:
                keys: hydra/core.Name
                values:
                  parameterized:
                    genericType: hydra/evaluation.PrimitiveFunction
                    parameters:
                    - type:
                        variable: a
                      variable: a -}
    , contextFunctions :: (Map (Name) (PrimitiveFunction a))
    -- | @type hydra/evaluation.EvaluationStrategy
    , contextStrategy :: EvaluationStrategy
    {-| @type function:
                from:
                - variable: a
                to:
                  optional: string -}
    , contextDescriptionOf :: a -> (Maybe String)
    {-| @type function:
                from:
                - variable: a
                to:
                  optional: hydra/core.Type -}
    , contextTypeOf :: a -> (Maybe Type)
    {-| @type function:
                from:
                - optional: hydra/core.Type
                - variable: a
                to:
                  variable: a -}
    , contextSetTypeOf :: (Maybe Type) -> (a -> a) }

data EvaluationStrategy
  = EvaluationStrategy
    {-| Whether a term of a given variant is considered to be fully reduced,
        without further inspection
        
        @type set: hydra/core.TermVariant -}
    { evaluationStrategyOpaqueTermVariants :: (Set (TermVariant)) } deriving (Eq, Generic, Ord, Read, Show)

{-| A helper object for specifying and unmarshalling an argument to a primitive
    function -}
data InputSpec a m
  = InputSpec
    -- | @type hydra/core.Type
    { inputSpecType :: Type
    {-| @type function:
                from:
                - parameterized:
                    genericType: hydra/core.Term
                    parameters:
                    - type:
                        variable: m
                      variable: a
                to:
                  parameterized:
                    genericType: hydra/evaluation.Result
                    parameters:
                    - type:
                        variable: a
                      variable: a -}
    , inputSpecUnmarshal :: (Term m) -> (Result a) }

{-| A helper object for specifying and marshalling the output of a primitive
    function -}
data OutputSpec a m
  = OutputSpec
    -- | @type hydra/core.Type
    { outputSpecType :: Type
    {-| @type function:
                from:
                - variable: a
                to:
                  parameterized:
                    genericType: hydra/core.Term
                    parameters:
                    - type:
                        variable: m
                      variable: a -}
    , outputSpecMarshal :: a -> (Term m) }

data PrimitiveFunction a
  = PrimitiveFunction
    -- | @type hydra/core.Name
    { primitiveFunctionName :: Name
    -- | @type hydra/core.FunctionType
    , primitiveFunctionType :: FunctionType
    {-| @type function:
                from:
                - list:
                    parameterized:
                      genericType: hydra/core.Term
                      parameters:
                      - type:
                          variable: a
                        variable: a
                to:
                  parameterized:
                    genericType: hydra/evaluation.Result
                    parameters:
                    - type:
                        parameterized:
                          genericType: hydra/core.Term
                          parameters:
                          - type:
                              variable: a
                            variable: a
                      variable: a -}
    , primitiveFunctionImplementation :: [Term a] -> (Result (Term a)) }

data Result a
  -- | @type variable: a
  = ResultSuccess a
  -- | @type string
  | ResultFailure String deriving (Eq, Generic, Ord, Read, Show)

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
_Context_descriptionOf = "descriptionOf" :: String
_Context_elements = "elements" :: String
_Context_functions = "functions" :: String
_Context_graphs = "graphs" :: String
_Context_setTypeOf = "setTypeOf" :: String
_Context_strategy = "strategy" :: String
_Context_typeOf = "typeOf" :: String
_EvaluationStrategy = "hydra/evaluation.EvaluationStrategy" :: String
_EvaluationStrategy_opaqueTermVariants = "opaqueTermVariants" :: String
_InputSpec = "hydra/evaluation.InputSpec" :: String
_InputSpec_type = "type" :: String
_InputSpec_unmarshal = "unmarshal" :: String
_OutputSpec = "hydra/evaluation.OutputSpec" :: String
_OutputSpec_marshal = "marshal" :: String
_OutputSpec_type = "type" :: String
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
