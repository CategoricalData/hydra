{-# LANGUAGE DeriveGeneric #-}
module Hydra.Evaluation
  ( Context(..)
  , PrimitiveFunction(..)
  , ReductionStyle(..)
  , _Context
  , _Context_elements
  , _Context_functions
  , _Context_reductionStyle
  , _PrimitiveFunction
  , _PrimitiveFunction_implementation
  , _PrimitiveFunction_type
  , _ReductionStyle
  , _ReductionStyle_caseStatements
  , _ReductionStyle_elements
  , _ReductionStyle_lambdas
  , _ReductionStyle_records
  , _ReductionStyle_unions
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
    -- | @type hydra/evaluation.ReductionStyle
    , contextReductionStyle :: ReductionStyle }

data PrimitiveFunction
  = PrimitiveFunction
    {-| @type function:
                from:
                - list: hydra/core.Term
                to: hydra/core.Term -}
    { primitiveFunctionImplementation :: [Term] -> Term
    -- | @type hydra/core.FunctionType
    , primitiveFunctionType :: FunctionType }

data ReductionStyle
  = ReductionStyle
    {-| Whether all lambda terms are considered to be fully reduced
        
        @type boolean -}
    { reductionStyleLambdas :: Bool
    {-| Whether all record terms are considered to be fully reduced
        
        @type boolean -}
    , reductionStyleRecords :: Bool
    {-| Whether all union terms are considered to be fully reduced
        
        @type boolean -}
    , reductionStyleUnions :: Bool
    {-| Whether all element reference terms are considered to be fully reduced
        
        @type boolean -}
    , reductionStyleElements :: Bool
    {-| Whether all case statement terms are considered to be fully reduced
        
        @type boolean -}
    , reductionStyleCaseStatements :: Bool } deriving (Eq, Generic, Ord, Read, Show)

_Context = "hydra/evaluation.Context" :: String
_Context_elements = "elements" :: String
_Context_functions = "functions" :: String
_Context_reductionStyle = "reductionStyle" :: String
_PrimitiveFunction = "hydra/evaluation.PrimitiveFunction" :: String
_PrimitiveFunction_implementation = "implementation" :: String
_PrimitiveFunction_type = "type" :: String
_ReductionStyle = "hydra/evaluation.ReductionStyle" :: String
_ReductionStyle_caseStatements = "caseStatements" :: String
_ReductionStyle_elements = "elements" :: String
_ReductionStyle_lambdas = "lambdas" :: String
_ReductionStyle_records = "records" :: String
_ReductionStyle_unions = "unions" :: String
