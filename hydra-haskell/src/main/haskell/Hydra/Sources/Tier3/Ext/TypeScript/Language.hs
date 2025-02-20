{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier3.Ext.TypeScript.Language (typeScriptLanguageModule) where

import Hydra.Sources.Tier2.All
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Coders as Coders
import Hydra.Dsl.Lib.Equality as Equality
import Hydra.Dsl.Lib.Flows as Flows
import Hydra.Dsl.Lib.Lists as Lists
import Hydra.Dsl.Lib.Logic as Logic
import Hydra.Dsl.Lib.Maps as Maps
import Hydra.Dsl.Lib.Sets as Sets
import Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Dsl.Core as Core
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.ShorthandTypes

import qualified Data.Set as S


typeScriptLanguageDefinition :: String -> TTerm a -> TElement a
typeScriptLanguageDefinition = definitionInModule typeScriptLanguageModule

typeScriptLanguageModule :: Module
typeScriptLanguageModule = Module ns elements
    [hydraCodersModule, hydraLexicalModule] [hydraCoreModule, hydraGraphModule, hydraCodersModule] $
    Just "Language constraints for TypeScript"
  where
    ns = Namespace "hydra.ext.typeScript.language"
    elements = [
      el typeScriptLanguageDef,
      el typeScriptReservedWordsDef]

typeScriptLanguageDef :: TElement Language
typeScriptLanguageDef = typeScriptLanguageDefinition "typeScriptLanguage" $
  doc "Language constraints for TypeScript" $
  typed languageT $
  record _Language [
    _Language_name>>: wrap _LanguageName "hydra.langs.typeScript",
    _Language_constraints>>: record _LanguageConstraints [
      _LanguageConstraints_eliminationVariants>>: Sets.empty,
      _LanguageConstraints_literalVariants>>: Sets.fromList $ list (unitVariant _LiteralVariant <$> [
        _LiteralVariant_boolean,
        _LiteralVariant_float,
        _LiteralVariant_integer,
        _LiteralVariant_string]),
      _LanguageConstraints_floatTypes>>: Sets.fromList $ list (unitVariant _FloatType <$> [
        _FloatType_float64]),
      _LanguageConstraints_functionVariants>>: Sets.empty,
      _LanguageConstraints_integerTypes>>: Sets.fromList $ list (unitVariant _IntegerType <$> [
        _IntegerType_bigint]),
      _LanguageConstraints_termVariants>>: Sets.fromList $ list (unitVariant _TermVariant <$> [
        _TermVariant_list,
        _TermVariant_literal,
        _TermVariant_map,
        _TermVariant_optional,
        _TermVariant_record,
        _TermVariant_union]),
      _LanguageConstraints_typeVariants>>: Sets.fromList $ list (unitVariant _TypeVariant <$> [
        _TypeVariant_annotated,
        _TypeVariant_list,
        _TypeVariant_literal,
        _TypeVariant_map,
        _TypeVariant_optional,
        _TypeVariant_record,
        _TypeVariant_union,
        _TypeVariant_variable]),
      _LanguageConstraints_types>>: match _Type (Just true) [
        _Type_map>>: lambda "mt" (match _Type (Just true) [
          _Type_optional>>: constant false] @@ (ref stripTypeDef @@ (Core.mapTypeValues @@ var "mt")))]]]

typeScriptReservedWordsDef :: TElement (S.Set String)
typeScriptReservedWordsDef = typeScriptLanguageDefinition "typeScriptReservedWords" $
    doc "A set of reserved words in TypeScript. Taken directly from https://github.com/microsoft/TypeScript/issues/2536" $
    typed (tSet tString) $
    (Sets.fromList (Lists.concat $
      list [var "reservedWords", var "strictModeReservedWords", var "contextuallKeywords"]))
  `with` [
    "reservedWords">: list [
      "delete", "do", "else", "enum", "export", "extends", "false", "finally", "for", "function", "if", "import",
      "in", "instanceof", "new", "null", "return", "super", "switch", "this", "throw", "true", "try", "typeof", "var",
      "void", "while", "with"],
    "strictModeReservedWords">: list [
      "as", "implements", "interface", "let", "package", "private", "protected", "public", "static", "yield"],
    "contextuallKeywords">: list [
      "any", "boolean", "constructor", "declare", "from", "get", "module", "number", "of", "require", "set", "string",
      "symbol", "type"]]
