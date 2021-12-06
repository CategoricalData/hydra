{-# LANGUAGE DeriveGeneric #-}
module Hydra.Adapter
  ( Adapter(..)
  , AdapterContext(..)
  , Language_Constraints(..)
  , Language_Name
  , Language(..)
  , _Adapter
  , _AdapterContext
  , _AdapterContext_evaluation
  , _AdapterContext_source
  , _AdapterContext_target
  , _Adapter_isLossy
  , _Adapter_source
  , _Adapter_step
  , _Adapter_target
  , _Language
  , _Language_Constraints
  , _Language_Constraints_floatTypes
  , _Language_Constraints_functionVariants
  , _Language_Constraints_integerTypes
  , _Language_Constraints_literalVariants
  , _Language_Constraints_termVariants
  , _Language_Constraints_typeVariants
  , _Language_Constraints_types
  , _Language_Name
  , _Language_constraints
  , _Language_name
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set
import Hydra.Core
import Hydra.Evaluation

data Adapter t v
  = Adapter
    -- | @type boolean
    { adapterIsLossy :: Bool
    -- | @type variable: t
    , adapterSource :: t
    -- | @type variable: t
    , adapterTarget :: t
    {-| @type parameterized:
                genericType: hydra/evaluation.Step
                parameters:
                - type:
                    variable: v
                  variable: a
                - type:
                    variable: v
                  variable: b -}
    , adapterStep :: Step v v }

data AdapterContext a
  = AdapterContext
    {-| @type parameterized:
                genericType: hydra/evaluation.Context
                parameters:
                - type:
                    variable: a
                  variable: a -}
    { adapterContextEvaluation :: Context a
    -- | @type hydra/adapter.Language
    , adapterContextSource :: Language
    -- | @type hydra/adapter.Language
    , adapterContextTarget :: Language }

data Language_Constraints
  = Language_Constraints
    -- | @type set: hydra/core.LiteralVariant
    { languageConstraintsLiteralVariants :: (Set (LiteralVariant))
    -- | @type set: hydra/core.FloatType
    , languageConstraintsFloatTypes :: (Set (FloatType))
    -- | @type set: hydra/core.FunctionVariant
    , languageConstraintsFunctionVariants :: (Set (FunctionVariant))
    -- | @type set: hydra/core.IntegerType
    , languageConstraintsIntegerTypes :: (Set (IntegerType))
    -- | @type set: hydra/core.TermVariant
    , languageConstraintsTermVariants :: (Set (TermVariant))
    -- | @type set: hydra/core.TypeVariant
    , languageConstraintsTypeVariants :: (Set (TypeVariant))
    {-| @type function:
                from:
                - hydra/core.Type
                to: boolean -}
    , languageConstraintsTypes :: Type -> Bool }

-- | @type string
type Language_Name = String

data Language
  = Language
    -- | @type hydra/adapter.Language.Name
    { languageName :: Language_Name
    -- | @type hydra/adapter.Language.Constraints
    , languageConstraints :: Language_Constraints }

_Adapter = "hydra/adapter.Adapter" :: String
_AdapterContext = "hydra/adapter.AdapterContext" :: String
_AdapterContext_evaluation = "evaluation" :: String
_AdapterContext_source = "source" :: String
_AdapterContext_target = "target" :: String
_Adapter_isLossy = "isLossy" :: String
_Adapter_source = "source" :: String
_Adapter_step = "step" :: String
_Adapter_target = "target" :: String
_Language = "hydra/adapter.Language" :: String
_Language_Constraints = "hydra/adapter.Language_Constraints" :: String
_Language_Constraints_floatTypes = "floatTypes" :: String
_Language_Constraints_functionVariants = "functionVariants" :: String
_Language_Constraints_integerTypes = "integerTypes" :: String
_Language_Constraints_literalVariants = "literalVariants" :: String
_Language_Constraints_termVariants = "termVariants" :: String
_Language_Constraints_typeVariants = "typeVariants" :: String
_Language_Constraints_types = "types" :: String
_Language_Name = "hydra/adapter.Language_Name" :: String
_Language_constraints = "constraints" :: String
_Language_name = "name" :: String
