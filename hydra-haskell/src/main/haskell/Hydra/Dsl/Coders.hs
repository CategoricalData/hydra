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

adapterContextGraph :: TTerm AdapterContext -> TTerm Graph
adapterContextGraph c = project _AdapterContext _AdapterContext_graph @@ c

adapterContextLanguage :: TTerm AdapterContext -> TTerm Language
adapterContextLanguage c = project _AdapterContext _AdapterContext_language @@ c

adapterContextAdapters :: TTerm AdapterContext -> TTerm (M.Map Name (Adapter AdapterContext AdapterContext Type Type Term Term))
adapterContextAdapters c = project _AdapterContext _AdapterContext_adapters @@ c

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

languageNew :: TTerm LanguageName -> TTerm LanguageConstraints -> TTerm Language
languageNew name constraints = record _Language [
    _Language_name>>: name,
    _Language_constraints>>: constraints]

languageNameNew :: TTerm String -> TTerm LanguageName
languageNameNew = wrap _LanguageName

unLanguageName :: TTerm LanguageName -> TTerm String
unLanguageName n = unwrap _LanguageName @@ n

languageName :: TTerm Language -> TTerm LanguageName
languageName c = project _Language _Language_name @@ c

languageConstraints :: TTerm Language -> TTerm LanguageConstraints
languageConstraints c = project _Language _Language_constraints @@ c

languageConstraintsNew :: TTerm (S.Set EliminationVariant)
                    -> TTerm (S.Set LiteralVariant)
                    -> TTerm (S.Set FloatType)
                    -> TTerm (S.Set FunctionVariant)
                    -> TTerm (S.Set IntegerType)
                    -> TTerm (S.Set TermVariant)
                    -> TTerm (S.Set TypeVariant)
                    -> TTerm (Type -> Bool)
                    -> TTerm LanguageConstraints
languageConstraintsNew eliminationVariants
                    literalVariants
                    floatTypes
                    functionVariants
                    integerTypes
                    termVariants
                    typeVariants
                    types = record _LanguageConstraints [
    _LanguageConstraints_eliminationVariants>>: eliminationVariants,
    _LanguageConstraints_literalVariants>>: literalVariants,
    _LanguageConstraints_floatTypes>>: floatTypes,
    _LanguageConstraints_functionVariants>>: functionVariants,
    _LanguageConstraints_integerTypes>>: integerTypes,
    _LanguageConstraints_termVariants>>: termVariants,
    _LanguageConstraints_typeVariants>>: typeVariants,
    _LanguageConstraints_types>>: types]

languageConstraintsEliminationVariants :: TTerm LanguageConstraints -> TTerm (S.Set EliminationVariant)
languageConstraintsEliminationVariants lc = project _LanguageConstraints _LanguageConstraints_eliminationVariants @@ lc

languageConstraintsLiteralVariants :: TTerm LanguageConstraints -> TTerm (S.Set LiteralVariant)
languageConstraintsLiteralVariants lc = project _LanguageConstraints _LanguageConstraints_literalVariants @@ lc

languageConstraintsFloatTypes :: TTerm LanguageConstraints -> TTerm (S.Set FloatType)
languageConstraintsFloatTypes lc = project _LanguageConstraints _LanguageConstraints_floatTypes @@ lc

languageConstraintsFunctionVariants :: TTerm LanguageConstraints -> TTerm (S.Set FunctionVariant)
languageConstraintsFunctionVariants lc = project _LanguageConstraints _LanguageConstraints_functionVariants @@ lc

languageConstraintsIntegerTypes :: TTerm LanguageConstraints -> TTerm (S.Set IntegerType)
languageConstraintsIntegerTypes lc = project _LanguageConstraints _LanguageConstraints_integerTypes @@ lc

languageConstraintsTermVariants :: TTerm LanguageConstraints -> TTerm (S.Set TermVariant)
languageConstraintsTermVariants lc = project _LanguageConstraints _LanguageConstraints_termVariants @@ lc

languageConstraintsTypeVariants :: TTerm LanguageConstraints -> TTerm (S.Set TypeVariant)
languageConstraintsTypeVariants lc = project _LanguageConstraints _LanguageConstraints_typeVariants @@ lc

languageConstraintsTypes :: TTerm LanguageConstraints -> TTerm (Type -> Bool)
languageConstraintsTypes lc = project _LanguageConstraints _LanguageConstraints_types @@ lc

traversalOrderPre = unitVariant _TraversalOrder _TraversalOrder_pre
traversalOrderPost = unitVariant _TraversalOrder _TraversalOrder_post
