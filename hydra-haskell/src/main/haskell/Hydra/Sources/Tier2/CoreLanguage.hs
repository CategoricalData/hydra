{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.CoreLanguage where

import Hydra.Kernel
import Hydra.Dsl.Base
import Hydra.Sources.Tier2.Basics
import qualified Hydra.Dsl.Lib.Sets as Sets


hydraCoreLanguageModule :: Module Kv
hydraCoreLanguageModule = Module ns elements [hydraBasicsModule] Nothing
  where
    ns = Namespace "hydra/coreLanguage"
    elements = [el hydraCoreLanguageDef]

hydraCoreLanguageDef :: Definition (Language a)
hydraCoreLanguageDef = definitionInModule hydraCoreLanguageModule "hydraCoreLanguage" $
  doc "Language constraints for Java" $
  typed languageA $
  record _Language [
    _Language_name>>: wrap _LanguageName "hydra/core",
    _Language_constraints>>: record _LanguageConstraints [
    _LanguageConstraints_eliminationVariants>>: Sets.fromList @@ ref eliminationVariantsDef,
    _LanguageConstraints_literalVariants>>: Sets.fromList @@ ref literalVariantsDef,
    _LanguageConstraints_floatTypes>>: Sets.fromList @@ ref floatTypesDef,
    _LanguageConstraints_functionVariants>>: Sets.fromList @@ ref functionVariantsDef,
    _LanguageConstraints_integerTypes>>: Sets.fromList @@ ref integerTypesDef,
    _LanguageConstraints_termVariants>>: Sets.fromList @@ ref termVariantsDef,
    _LanguageConstraints_typeVariants>>: Sets.fromList @@ ref typeVariantsDef,
    _LanguageConstraints_types>>: constant true]]
