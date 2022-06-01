module Hydra.Adapter where

import qualified Hydra.Core as Core
import qualified Hydra.Evaluation as Evaluation
import Data.Map
import Data.Set

data Adapter t v 
  = Adapter {
    adapterIsLossy :: Bool,
    adapterSource :: t,
    adapterTarget :: t,
    adapterStep :: (Evaluation.Step v v)}

_Adapter = (Core.Name "hydra/adapter.Adapter")

_Adapter_isLossy = (Core.FieldName "isLossy")

_Adapter_source = (Core.FieldName "source")

_Adapter_target = (Core.FieldName "target")

_Adapter_step = (Core.FieldName "step")

data AdapterContext a 
  = AdapterContext {
    adapterContextEvaluation :: (Evaluation.Context a),
    adapterContextSource :: (Language a),
    adapterContextTarget :: (Language a)}

_AdapterContext = (Core.Name "hydra/adapter.AdapterContext")

_AdapterContext_evaluation = (Core.FieldName "evaluation")

_AdapterContext_source = (Core.FieldName "source")

_AdapterContext_target = (Core.FieldName "target")

data LanguageConstraints m 
  = LanguageConstraints {
    languageConstraintsLiteralVariants :: (Set Core.LiteralVariant),
    languageConstraintsFloatTypes :: (Set Core.FloatType),
    languageConstraintsFunctionVariants :: (Set Core.FunctionVariant),
    languageConstraintsIntegerTypes :: (Set Core.IntegerType),
    languageConstraintsDataVariants :: (Set Core.DataVariant),
    languageConstraintsTypeVariants :: (Set Core.TypeVariant),
    languageConstraintsTypes :: (Core.Type m -> Bool)}

_LanguageConstraints = (Core.Name "hydra/adapter.LanguageConstraints")

_LanguageConstraints_literalVariants = (Core.FieldName "literalVariants")

_LanguageConstraints_floatTypes = (Core.FieldName "floatTypes")

_LanguageConstraints_functionVariants = (Core.FieldName "functionVariants")

_LanguageConstraints_integerTypes = (Core.FieldName "integerTypes")

_LanguageConstraints_dataVariants = (Core.FieldName "dataVariants")

_LanguageConstraints_typeVariants = (Core.FieldName "typeVariants")

_LanguageConstraints_types = (Core.FieldName "types")

newtype LanguageName 
  = LanguageName {
    unLanguageName :: String}
  deriving (Eq, Ord, Read, Show)

_LanguageName = (Core.Name "hydra/adapter.LanguageName")

data Language m 
  = Language {
    languageName :: LanguageName,
    languageConstraints :: (LanguageConstraints m)}

_Language = (Core.Name "hydra/adapter.Language")

_Language_name = (Core.FieldName "name")

_Language_constraints = (Core.FieldName "constraints")