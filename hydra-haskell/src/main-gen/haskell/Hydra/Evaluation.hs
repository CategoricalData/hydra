{-# LANGUAGE DeriveGeneric #-}
module Hydra.Evaluation
  ( Context(..)
  , PrimitiveFunction(..)
  , ReductionStyle(..)
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
