{-# LANGUAGE DeriveGeneric #-}
module Hydra.Translation
  ( AdapterContext(..)
  , Language_Constraints(..)
  , Language_Name
  , Language(..)
  , _AdapterContext
  , _AdapterContext_evaluation
  , _AdapterContext_source
  , _AdapterContext_target
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

data AdapterContext
  = AdapterContext
    -- | @type hydra/evaluation.Context
    { adapterContextEvaluation :: Context
    -- | @type hydra/translation.Language
    , adapterContextSource :: Language
    -- | @type hydra/translation.Language
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
    -- | @type hydra/translation.Language.Name
    { languageName :: Language_Name
    -- | @type hydra/translation.Language.Constraints
    , languageConstraints :: Language_Constraints } deriving (Eq, Generic, Ord, Read, Show)

_AdapterContext = "hydra/translation.AdapterContext" :: String
_AdapterContext_evaluation = "evaluation" :: String
_AdapterContext_source = "source" :: String
_AdapterContext_target = "target" :: String
_Language = "hydra/translation.Language" :: String
_Language_Constraints = "hydra/translation.Language_Constraints" :: String
_Language_Constraints_atomicVariants = "atomicVariants" :: String
_Language_Constraints_floatVariants = "floatVariants" :: String
_Language_Constraints_integerVariants = "integerVariants" :: String
_Language_Constraints_termVariants = "termVariants" :: String
_Language_Constraints_typeVariants = "typeVariants" :: String
_Language_Name = "hydra/translation.Language_Name" :: String
_Language_constraints = "constraints" :: String
_Language_name = "name" :: String
