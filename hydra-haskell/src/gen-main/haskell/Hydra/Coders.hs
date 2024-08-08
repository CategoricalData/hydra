-- | Abstractions for paired transformations between languages

module Hydra.Coders where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Mantle as Mantle
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | An evaluation context together with a source language and a target language
data AdapterContext = 
  AdapterContext {
    adapterContextGraph :: Graph.Graph,
    adapterContextLanguage :: Language,
    adapterContextAdapters :: (Map Core.Name (Compute.Adapter AdapterContext AdapterContext Core.Type Core.Type Core.Term Core.Term))}

_AdapterContext = (Core.Name "hydra/coders.AdapterContext")

_AdapterContext_graph = (Core.Name "graph")

_AdapterContext_language = (Core.Name "language")

_AdapterContext_adapters = (Core.Name "adapters")

_AdapterContext_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/coders.AdapterContext"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "graph"),
      Core.fieldTypeType = Graph._Graph_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "language"),
      Core.fieldTypeType = _Language_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "adapters"),
      Core.fieldTypeType = (Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = Core._Name_type_,
        Core.mapTypeValues = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
            Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = Compute._Adapter_type_,
                    Core.applicationTypeArgument = _AdapterContext_type_})),
                  Core.applicationTypeArgument = _AdapterContext_type_})),
                Core.applicationTypeArgument = Core._Type_type_})),
              Core.applicationTypeArgument = Core._Type_type_})),
            Core.applicationTypeArgument = Core._Term_type_})),
          Core.applicationTypeArgument = Core._Term_type_}))}))}]}))

-- | Indicates either the 'out' or the 'in' direction of a coder
data CoderDirection = 
  CoderDirectionEncode  |
  CoderDirectionDecode 
  deriving (Eq, Ord, Read, Show)

_CoderDirection = (Core.Name "hydra/coders.CoderDirection")

_CoderDirection_encode = (Core.Name "encode")

_CoderDirection_decode = (Core.Name "decode")

_CoderDirection_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/coders.CoderDirection"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "encode"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "decode"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

-- | A named language together with language-specific constraints
data Language = 
  Language {
    languageName :: LanguageName,
    languageConstraints :: LanguageConstraints}

_Language = (Core.Name "hydra/coders.Language")

_Language_name = (Core.Name "name")

_Language_constraints = (Core.Name "constraints")

_Language_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/coders.Language"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _LanguageName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constraints"),
      Core.fieldTypeType = _LanguageConstraints_type_}]}))

-- | A set of constraints on valid type and term expressions, characterizing a language
data LanguageConstraints = 
  LanguageConstraints {
    -- | All supported elimination variants
    languageConstraintsEliminationVariants :: (Set Mantle.EliminationVariant),
    -- | All supported literal variants
    languageConstraintsLiteralVariants :: (Set Mantle.LiteralVariant),
    -- | All supported float types
    languageConstraintsFloatTypes :: (Set Core.FloatType),
    -- | All supported function variants
    languageConstraintsFunctionVariants :: (Set Mantle.FunctionVariant),
    -- | All supported integer types
    languageConstraintsIntegerTypes :: (Set Core.IntegerType),
    -- | All supported term variants
    languageConstraintsTermVariants :: (Set Mantle.TermVariant),
    -- | All supported type variants
    languageConstraintsTypeVariants :: (Set Mantle.TypeVariant),
    -- | A logical set of types, as a predicate which tests a type for inclusion
    languageConstraintsTypes :: (Core.Type -> Bool)}

_LanguageConstraints = (Core.Name "hydra/coders.LanguageConstraints")

_LanguageConstraints_eliminationVariants = (Core.Name "eliminationVariants")

_LanguageConstraints_literalVariants = (Core.Name "literalVariants")

_LanguageConstraints_floatTypes = (Core.Name "floatTypes")

_LanguageConstraints_functionVariants = (Core.Name "functionVariants")

_LanguageConstraints_integerTypes = (Core.Name "integerTypes")

_LanguageConstraints_termVariants = (Core.Name "termVariants")

_LanguageConstraints_typeVariants = (Core.Name "typeVariants")

_LanguageConstraints_types = (Core.Name "types")

_LanguageConstraints_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/coders.LanguageConstraints"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "eliminationVariants"),
      Core.fieldTypeType = (Core.TypeSet Mantle._EliminationVariant_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literalVariants"),
      Core.fieldTypeType = (Core.TypeSet Mantle._LiteralVariant_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "floatTypes"),
      Core.fieldTypeType = (Core.TypeSet Core._FloatType_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "functionVariants"),
      Core.fieldTypeType = (Core.TypeSet Mantle._FunctionVariant_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "integerTypes"),
      Core.fieldTypeType = (Core.TypeSet Core._IntegerType_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "termVariants"),
      Core.fieldTypeType = (Core.TypeSet Mantle._TermVariant_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeVariants"),
      Core.fieldTypeType = (Core.TypeSet Mantle._TypeVariant_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "types"),
      Core.fieldTypeType = (Core.TypeFunction (Core.FunctionType {
        Core.functionTypeDomain = Core._Type_type_,
        Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))}]}))

-- | The unique name of a language
newtype LanguageName = 
  LanguageName {
    unLanguageName :: String}
  deriving (Eq, Ord, Read, Show)

_LanguageName = (Core.Name "hydra/coders.LanguageName")

_LanguageName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | Specifies either a pre-order or post-order traversal
data TraversalOrder = 
  -- | Pre-order traversal
  TraversalOrderPre  |
  -- | Post-order traversal
  TraversalOrderPost 
  deriving (Eq, Ord, Read, Show)

_TraversalOrder = (Core.Name "hydra/coders.TraversalOrder")

_TraversalOrder_pre = (Core.Name "pre")

_TraversalOrder_post = (Core.Name "post")

_TraversalOrder_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/coders.TraversalOrder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pre"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "post"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))