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
  , _Adapter_mapping
  , _Adapter_source
  , _Adapter_target
  , _Language
  , _Language_Constraints
  , _Language_Constraints_atomicVariants
  , _Language_Constraints_floatVariants
  , _Language_Constraints_integerVariants
  , _Language_Constraints_termVariants
  , _Language_Constraints_typeVariants
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
    , adapterMapping :: Step v v }

data AdapterContext
  = AdapterContext
    -- | @type hydra/evaluation.Context
    { adapterContextEvaluation :: Context
    -- | @type hydra/adapter.Language
    , adapterContextSource :: Language
    -- | @type hydra/adapter.Language
    , adapterContextTarget :: Language }

data Language_Constraints
  = Language_Constraints
    -- | @type set: hydra/core.AtomicVariant
    { languageConstraintsAtomicVariants :: (Set AtomicVariant)
    -- | @type set: hydra/core.FloatVariant
    , languageConstraintsFloatVariants :: (Set FloatVariant)
    -- | @type set: hydra/core.IntegerVariant
    , languageConstraintsIntegerVariants :: (Set IntegerVariant)
    -- | @type set: hydra/core.TermVariant
    , languageConstraintsTermVariants :: (Set TermVariant)
    -- | @type set: hydra/core.TypeVariant
    , languageConstraintsTypeVariants :: (Set TypeVariant) } deriving (Eq, Generic, Ord, Read, Show)

-- | @type string
type Language_Name = String

data Language
  = Language
    -- | @type hydra/adapter.Language.Name
    { languageName :: Language_Name
    -- | @type hydra/adapter.Language.Constraints
    , languageConstraints :: Language_Constraints } deriving (Eq, Generic, Ord, Read, Show)

_Adapter = "hydra/adapter.Adapter" :: String
_AdapterContext = "hydra/adapter.AdapterContext" :: String
_AdapterContext_evaluation = "evaluation" :: String
_AdapterContext_source = "source" :: String
_AdapterContext_target = "target" :: String
_Adapter_isLossy = "isLossy" :: String
_Adapter_mapping = "mapping" :: String
_Adapter_source = "source" :: String
_Adapter_target = "target" :: String
_Language = "hydra/adapter.Language" :: String
_Language_Constraints = "hydra/adapter.Language_Constraints" :: String
_Language_Constraints_atomicVariants = "atomicVariants" :: String
_Language_Constraints_floatVariants = "floatVariants" :: String
_Language_Constraints_integerVariants = "integerVariants" :: String
_Language_Constraints_termVariants = "termVariants" :: String
_Language_Constraints_typeVariants = "typeVariants" :: String
_Language_Name = "hydra/adapter.Language_Name" :: String
_Language_constraints = "constraints" :: String
_Language_name = "name" :: String
