{-# LANGUAGE DeriveGeneric #-}
module Hydra.Translation
  ( Language_Constraints(..)
  , Language_Name
  , Language(..)
  , TranslationContext(..)
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
  , _TranslationContext
  , _TranslationContext_evaluation
  , _TranslationContext_source
  , _TranslationContext_target
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set
import Hydra.Core
import Hydra.Evaluation

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

data TranslationContext
  = TranslationContext
    -- | @type hydra/evaluation.Context
    { translationContextEvaluation :: Context
    -- | @type hydra/translation.Language
    , translationContextSource :: Language
    -- | @type hydra/translation.Language
    , translationContextTarget :: Language }

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
_TranslationContext = "hydra/translation.TranslationContext" :: String
_TranslationContext_evaluation = "evaluation" :: String
_TranslationContext_source = "source" :: String
_TranslationContext_target = "target" :: String
