module Hydra.Dsl.Coders where

import Hydra.Kernel
import Hydra.Dsl.Phantoms as Phantoms
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.Lib.Sets as Sets
import Hydra.Dsl.Mantle as Mantle
import qualified Hydra.Dsl.TTypes as T

import qualified Data.Map as M
import qualified Data.Set as S


adapterContext :: TTerm Graph -> TTerm Language -> TTerm (M.Map Name (Adapter AdapterContext AdapterContext Type Type Term Term)) -> TTerm AdapterContext
adapterContext graph language adapters = Phantoms.record _AdapterContext [
  _AdapterContext_graph>>: graph,
  _AdapterContext_language>>: language,
  _AdapterContext_adapters>>: adapters]

coderDirectionEncode :: TTerm CoderDirection
coderDirectionEncode = unitVariant _CoderDirection _CoderDirection_encode

coderDirectionDecode :: TTerm CoderDirection
coderDirectionDecode = unitVariant _CoderDirection _CoderDirection_decode

language
  :: String
  -> [EliminationVariant]
  -> [LiteralVariant]
  -> [FloatType]
  -> [FunctionVariant]
  -> [IntegerType]
  -> [TermVariant]
  -> [TypeVariant]
  -> TTerm (Type -> Bool)
  -> TTerm Language
language name eliminationVariants literalVariants floatTypes functionVariants integerTypes termVariants typeVariants typePredicate = Phantoms.record _Language [
  _Language_name>>: wrap _LanguageName $ string name,
  _Language_constraints>>: Phantoms.record _LanguageConstraints [
    _LanguageConstraints_eliminationVariants>>: Sets.fromList $ list (Mantle.eliminationVariant <$> eliminationVariants),
    _LanguageConstraints_literalVariants>>: Sets.fromList $ list (Mantle.literalVariant <$> literalVariants),
    _LanguageConstraints_floatTypes>>: Sets.fromList $ list (T.float <$> floatTypes),
    _LanguageConstraints_functionVariants>>: Sets.fromList $ list (Mantle.functionVariant <$> functionVariants),
    _LanguageConstraints_integerTypes>>: Sets.fromList $ list (T.integer <$> integerTypes),
    _LanguageConstraints_termVariants>>: Sets.fromList $ list (Mantle.termVariant <$> termVariants),
    _LanguageConstraints_typeVariants>>: Sets.fromList $ list (Mantle.typeVariant <$> typeVariants),
    _LanguageConstraints_types>>: typePredicate]]

languageConstraintsEliminationVariants :: TTerm LanguageConstraints -> TTerm (S.Set EliminationVariant)
languageConstraintsEliminationVariants c = project _LanguageConstraints _LanguageConstraints_eliminationVariants @@ c

languageConstraintsLiteralVariants :: TTerm LanguageConstraints -> TTerm (S.Set LiteralVariant)
languageConstraintsLiteralVariants c = project _LanguageConstraints _LanguageConstraints_literalVariants @@ c

languageConstraintsFloatTypes :: TTerm LanguageConstraints -> TTerm (S.Set FloatType)
languageConstraintsFloatTypes c = project _LanguageConstraints _LanguageConstraints_floatTypes @@ c

languageConstraintsFunctionVariants :: TTerm LanguageConstraints -> TTerm (S.Set FunctionVariant)
languageConstraintsFunctionVariants c = project _LanguageConstraints _LanguageConstraints_functionVariants @@ c

languageConstraintsIntegerTypes :: TTerm LanguageConstraints -> TTerm (S.Set IntegerType)
languageConstraintsIntegerTypes c = project _LanguageConstraints _LanguageConstraints_integerTypes @@ c

languageConstraintsTermVariants :: TTerm LanguageConstraints -> TTerm (S.Set TermVariant)
languageConstraintsTermVariants c = project _LanguageConstraints _LanguageConstraints_termVariants @@ c

languageConstraintsTypeVariants :: TTerm LanguageConstraints -> TTerm (S.Set TypeVariant)
languageConstraintsTypeVariants c = project _LanguageConstraints _LanguageConstraints_typeVariants @@ c

languageConstraintsTypes :: TTerm LanguageConstraints -> TTerm (Type -> Bool)
languageConstraintsTypes c = project _LanguageConstraints _LanguageConstraints_types @@ c

traversalOrderPre = unitVariant _TraversalOrder _TraversalOrder_pre
traversalOrderPost = unitVariant _TraversalOrder _TraversalOrder_post
