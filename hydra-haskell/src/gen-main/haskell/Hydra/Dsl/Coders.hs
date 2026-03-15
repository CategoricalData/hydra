-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.coders

module Hydra.Dsl.Coders where

import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

adapterContext :: (Graph.Graph -> Coders.Language -> M.Map Core.Name (Compute.Adapter Core.Type Core.Type Core.Term Core.Term) -> Coders.AdapterContext)
adapterContext graph language adapters = Coders.AdapterContext {
  Coders.adapterContextGraph = graph,
  Coders.adapterContextLanguage = language,
  Coders.adapterContextAdapters = adapters}

adapterContextGraph :: (Coders.AdapterContext -> Graph.Graph)
adapterContextGraph = Coders.adapterContextGraph

adapterContextLanguage :: (Coders.AdapterContext -> Coders.Language)
adapterContextLanguage = Coders.adapterContextLanguage

adapterContextAdapters :: (Coders.AdapterContext -> M.Map Core.Name (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
adapterContextAdapters = Coders.adapterContextAdapters

adapterContextWithGraph :: (Coders.AdapterContext -> Graph.Graph -> Coders.AdapterContext)
adapterContextWithGraph original newVal = Coders.AdapterContext {
  Coders.adapterContextGraph = newVal,
  Coders.adapterContextLanguage = (Coders.adapterContextLanguage original),
  Coders.adapterContextAdapters = (Coders.adapterContextAdapters original)}

adapterContextWithLanguage :: (Coders.AdapterContext -> Coders.Language -> Coders.AdapterContext)
adapterContextWithLanguage original newVal = Coders.AdapterContext {
  Coders.adapterContextGraph = (Coders.adapterContextGraph original),
  Coders.adapterContextLanguage = newVal,
  Coders.adapterContextAdapters = (Coders.adapterContextAdapters original)}

adapterContextWithAdapters :: (Coders.AdapterContext -> M.Map Core.Name (Compute.Adapter Core.Type Core.Type Core.Term Core.Term) -> Coders.AdapterContext)
adapterContextWithAdapters original newVal = Coders.AdapterContext {
  Coders.adapterContextGraph = (Coders.adapterContextGraph original),
  Coders.adapterContextLanguage = (Coders.adapterContextLanguage original),
  Coders.adapterContextAdapters = newVal}

coderDirectionEncode :: Coders.CoderDirection
coderDirectionEncode = Coders.CoderDirectionEncode

coderDirectionDecode :: Coders.CoderDirection
coderDirectionDecode = Coders.CoderDirectionDecode

language :: (Coders.LanguageName -> Coders.LanguageConstraints -> Coders.Language)
language name constraints = Coders.Language {
  Coders.languageName = name,
  Coders.languageConstraints = constraints}

languageName :: (Coders.Language -> Coders.LanguageName)
languageName = Coders.languageName

languageConstraints :: (Coders.Language -> Coders.LanguageConstraints)
languageConstraints = Coders.languageConstraints

languageWithName :: (Coders.Language -> Coders.LanguageName -> Coders.Language)
languageWithName original newVal = Coders.Language {
  Coders.languageName = newVal,
  Coders.languageConstraints = (Coders.languageConstraints original)}

languageWithConstraints :: (Coders.Language -> Coders.LanguageConstraints -> Coders.Language)
languageWithConstraints original newVal = Coders.Language {
  Coders.languageName = (Coders.languageName original),
  Coders.languageConstraints = newVal}

languageConstraints_ :: (S.Set Variants.EliminationVariant -> S.Set Variants.LiteralVariant -> S.Set Core.FloatType -> S.Set Variants.FunctionVariant -> S.Set Core.IntegerType -> S.Set Variants.TermVariant -> S.Set Variants.TypeVariant -> (Core.Type -> Bool) -> Coders.LanguageConstraints)
languageConstraints_ eliminationVariants literalVariants floatTypes functionVariants integerTypes termVariants typeVariants types = Coders.LanguageConstraints {
  Coders.languageConstraintsEliminationVariants = eliminationVariants,
  Coders.languageConstraintsLiteralVariants = literalVariants,
  Coders.languageConstraintsFloatTypes = floatTypes,
  Coders.languageConstraintsFunctionVariants = functionVariants,
  Coders.languageConstraintsIntegerTypes = integerTypes,
  Coders.languageConstraintsTermVariants = termVariants,
  Coders.languageConstraintsTypeVariants = typeVariants,
  Coders.languageConstraintsTypes = types}

languageConstraintsEliminationVariants :: (Coders.LanguageConstraints -> S.Set Variants.EliminationVariant)
languageConstraintsEliminationVariants = Coders.languageConstraintsEliminationVariants

languageConstraintsLiteralVariants :: (Coders.LanguageConstraints -> S.Set Variants.LiteralVariant)
languageConstraintsLiteralVariants = Coders.languageConstraintsLiteralVariants

languageConstraintsFloatTypes :: (Coders.LanguageConstraints -> S.Set Core.FloatType)
languageConstraintsFloatTypes = Coders.languageConstraintsFloatTypes

languageConstraintsFunctionVariants :: (Coders.LanguageConstraints -> S.Set Variants.FunctionVariant)
languageConstraintsFunctionVariants = Coders.languageConstraintsFunctionVariants

languageConstraintsIntegerTypes :: (Coders.LanguageConstraints -> S.Set Core.IntegerType)
languageConstraintsIntegerTypes = Coders.languageConstraintsIntegerTypes

languageConstraintsTermVariants :: (Coders.LanguageConstraints -> S.Set Variants.TermVariant)
languageConstraintsTermVariants = Coders.languageConstraintsTermVariants

languageConstraintsTypeVariants :: (Coders.LanguageConstraints -> S.Set Variants.TypeVariant)
languageConstraintsTypeVariants = Coders.languageConstraintsTypeVariants

languageConstraintsTypes :: (Coders.LanguageConstraints -> Core.Type -> Bool)
languageConstraintsTypes = Coders.languageConstraintsTypes

languageConstraintsWithEliminationVariants :: (Coders.LanguageConstraints -> S.Set Variants.EliminationVariant -> Coders.LanguageConstraints)
languageConstraintsWithEliminationVariants original newVal = Coders.LanguageConstraints {
  Coders.languageConstraintsEliminationVariants = newVal,
  Coders.languageConstraintsLiteralVariants = (Coders.languageConstraintsLiteralVariants original),
  Coders.languageConstraintsFloatTypes = (Coders.languageConstraintsFloatTypes original),
  Coders.languageConstraintsFunctionVariants = (Coders.languageConstraintsFunctionVariants original),
  Coders.languageConstraintsIntegerTypes = (Coders.languageConstraintsIntegerTypes original),
  Coders.languageConstraintsTermVariants = (Coders.languageConstraintsTermVariants original),
  Coders.languageConstraintsTypeVariants = (Coders.languageConstraintsTypeVariants original),
  Coders.languageConstraintsTypes = (Coders.languageConstraintsTypes original)}

languageConstraintsWithLiteralVariants :: (Coders.LanguageConstraints -> S.Set Variants.LiteralVariant -> Coders.LanguageConstraints)
languageConstraintsWithLiteralVariants original newVal = Coders.LanguageConstraints {
  Coders.languageConstraintsEliminationVariants = (Coders.languageConstraintsEliminationVariants original),
  Coders.languageConstraintsLiteralVariants = newVal,
  Coders.languageConstraintsFloatTypes = (Coders.languageConstraintsFloatTypes original),
  Coders.languageConstraintsFunctionVariants = (Coders.languageConstraintsFunctionVariants original),
  Coders.languageConstraintsIntegerTypes = (Coders.languageConstraintsIntegerTypes original),
  Coders.languageConstraintsTermVariants = (Coders.languageConstraintsTermVariants original),
  Coders.languageConstraintsTypeVariants = (Coders.languageConstraintsTypeVariants original),
  Coders.languageConstraintsTypes = (Coders.languageConstraintsTypes original)}

languageConstraintsWithFloatTypes :: (Coders.LanguageConstraints -> S.Set Core.FloatType -> Coders.LanguageConstraints)
languageConstraintsWithFloatTypes original newVal = Coders.LanguageConstraints {
  Coders.languageConstraintsEliminationVariants = (Coders.languageConstraintsEliminationVariants original),
  Coders.languageConstraintsLiteralVariants = (Coders.languageConstraintsLiteralVariants original),
  Coders.languageConstraintsFloatTypes = newVal,
  Coders.languageConstraintsFunctionVariants = (Coders.languageConstraintsFunctionVariants original),
  Coders.languageConstraintsIntegerTypes = (Coders.languageConstraintsIntegerTypes original),
  Coders.languageConstraintsTermVariants = (Coders.languageConstraintsTermVariants original),
  Coders.languageConstraintsTypeVariants = (Coders.languageConstraintsTypeVariants original),
  Coders.languageConstraintsTypes = (Coders.languageConstraintsTypes original)}

languageConstraintsWithFunctionVariants :: (Coders.LanguageConstraints -> S.Set Variants.FunctionVariant -> Coders.LanguageConstraints)
languageConstraintsWithFunctionVariants original newVal = Coders.LanguageConstraints {
  Coders.languageConstraintsEliminationVariants = (Coders.languageConstraintsEliminationVariants original),
  Coders.languageConstraintsLiteralVariants = (Coders.languageConstraintsLiteralVariants original),
  Coders.languageConstraintsFloatTypes = (Coders.languageConstraintsFloatTypes original),
  Coders.languageConstraintsFunctionVariants = newVal,
  Coders.languageConstraintsIntegerTypes = (Coders.languageConstraintsIntegerTypes original),
  Coders.languageConstraintsTermVariants = (Coders.languageConstraintsTermVariants original),
  Coders.languageConstraintsTypeVariants = (Coders.languageConstraintsTypeVariants original),
  Coders.languageConstraintsTypes = (Coders.languageConstraintsTypes original)}

languageConstraintsWithIntegerTypes :: (Coders.LanguageConstraints -> S.Set Core.IntegerType -> Coders.LanguageConstraints)
languageConstraintsWithIntegerTypes original newVal = Coders.LanguageConstraints {
  Coders.languageConstraintsEliminationVariants = (Coders.languageConstraintsEliminationVariants original),
  Coders.languageConstraintsLiteralVariants = (Coders.languageConstraintsLiteralVariants original),
  Coders.languageConstraintsFloatTypes = (Coders.languageConstraintsFloatTypes original),
  Coders.languageConstraintsFunctionVariants = (Coders.languageConstraintsFunctionVariants original),
  Coders.languageConstraintsIntegerTypes = newVal,
  Coders.languageConstraintsTermVariants = (Coders.languageConstraintsTermVariants original),
  Coders.languageConstraintsTypeVariants = (Coders.languageConstraintsTypeVariants original),
  Coders.languageConstraintsTypes = (Coders.languageConstraintsTypes original)}

languageConstraintsWithTermVariants :: (Coders.LanguageConstraints -> S.Set Variants.TermVariant -> Coders.LanguageConstraints)
languageConstraintsWithTermVariants original newVal = Coders.LanguageConstraints {
  Coders.languageConstraintsEliminationVariants = (Coders.languageConstraintsEliminationVariants original),
  Coders.languageConstraintsLiteralVariants = (Coders.languageConstraintsLiteralVariants original),
  Coders.languageConstraintsFloatTypes = (Coders.languageConstraintsFloatTypes original),
  Coders.languageConstraintsFunctionVariants = (Coders.languageConstraintsFunctionVariants original),
  Coders.languageConstraintsIntegerTypes = (Coders.languageConstraintsIntegerTypes original),
  Coders.languageConstraintsTermVariants = newVal,
  Coders.languageConstraintsTypeVariants = (Coders.languageConstraintsTypeVariants original),
  Coders.languageConstraintsTypes = (Coders.languageConstraintsTypes original)}

languageConstraintsWithTypeVariants :: (Coders.LanguageConstraints -> S.Set Variants.TypeVariant -> Coders.LanguageConstraints)
languageConstraintsWithTypeVariants original newVal = Coders.LanguageConstraints {
  Coders.languageConstraintsEliminationVariants = (Coders.languageConstraintsEliminationVariants original),
  Coders.languageConstraintsLiteralVariants = (Coders.languageConstraintsLiteralVariants original),
  Coders.languageConstraintsFloatTypes = (Coders.languageConstraintsFloatTypes original),
  Coders.languageConstraintsFunctionVariants = (Coders.languageConstraintsFunctionVariants original),
  Coders.languageConstraintsIntegerTypes = (Coders.languageConstraintsIntegerTypes original),
  Coders.languageConstraintsTermVariants = (Coders.languageConstraintsTermVariants original),
  Coders.languageConstraintsTypeVariants = newVal,
  Coders.languageConstraintsTypes = (Coders.languageConstraintsTypes original)}

languageConstraintsWithTypes :: (Coders.LanguageConstraints -> (Core.Type -> Bool) -> Coders.LanguageConstraints)
languageConstraintsWithTypes original newVal = Coders.LanguageConstraints {
  Coders.languageConstraintsEliminationVariants = (Coders.languageConstraintsEliminationVariants original),
  Coders.languageConstraintsLiteralVariants = (Coders.languageConstraintsLiteralVariants original),
  Coders.languageConstraintsFloatTypes = (Coders.languageConstraintsFloatTypes original),
  Coders.languageConstraintsFunctionVariants = (Coders.languageConstraintsFunctionVariants original),
  Coders.languageConstraintsIntegerTypes = (Coders.languageConstraintsIntegerTypes original),
  Coders.languageConstraintsTermVariants = (Coders.languageConstraintsTermVariants original),
  Coders.languageConstraintsTypeVariants = (Coders.languageConstraintsTypeVariants original),
  Coders.languageConstraintsTypes = newVal}

languageName_ :: (String -> Coders.LanguageName)
languageName_ x = (Coders.LanguageName x)

unLanguageName :: (Coders.LanguageName -> String)
unLanguageName = Coders.unLanguageName

traversalOrderPre :: Coders.TraversalOrder
traversalOrderPre = Coders.TraversalOrderPre

traversalOrderPost :: Coders.TraversalOrder
traversalOrderPost = Coders.TraversalOrderPost
