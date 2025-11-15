-- | Language constraints for TypeScript

module Hydra.Ext.TypeScript.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Meta as Meta
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Language constraints for TypeScript
typeScriptLanguage :: Coders.Language
typeScriptLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.ext.typeScript"),
  Coders.languageConstraints = Coders.LanguageConstraints {
    Coders.languageConstraintsEliminationVariants = eliminationVariants,
    Coders.languageConstraintsLiteralVariants = literalVariants,
    Coders.languageConstraintsFloatTypes = floatTypes,
    Coders.languageConstraintsFunctionVariants = functionVariants,
    Coders.languageConstraintsIntegerTypes = integerTypes,
    Coders.languageConstraintsTermVariants = termVariants,
    Coders.languageConstraintsTypeVariants = typeVariants,
    Coders.languageConstraintsTypes = types}} 
  where 
    eliminationVariants = Sets.empty
    literalVariants = (Sets.fromList [
      Meta.LiteralVariantBoolean,
      Meta.LiteralVariantFloat,
      Meta.LiteralVariantInteger,
      Meta.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeFloat64])
    functionVariants = Sets.empty
    integerTypes = (Sets.fromList [
      Core.IntegerTypeBigint])
    termVariants = (Sets.fromList [
      Meta.TermVariantList,
      Meta.TermVariantLiteral,
      Meta.TermVariantMap,
      Meta.TermVariantMaybe,
      Meta.TermVariantRecord,
      Meta.TermVariantUnion])
    typeVariants = (Sets.fromList [
      Meta.TypeVariantList,
      Meta.TypeVariantLiteral,
      Meta.TypeVariantMap,
      Meta.TypeVariantMaybe,
      Meta.TypeVariantRecord,
      Meta.TypeVariantUnion])
    types = (\x -> case x of
      Core.TypeMap v1 -> ((\x -> case x of
        Core.TypeMaybe _ -> False
        _ -> True) (Rewriting.deannotateType (Core.mapTypeValues v1)))
      _ -> True)

-- | A set of reserved words in TypeScript. Taken directly from https://github.com/microsoft/TypeScript/issues/2536
typeScriptReservedWords :: (S.Set String)
typeScriptReservedWords = (Sets.fromList (Lists.concat [
  reservedWords,
  strictModeReservedWords,
  contextuallKeywords])) 
  where 
    reservedWords = [
      "delete",
      "do",
      "else",
      "enum",
      "export",
      "extends",
      "false",
      "finally",
      "for",
      "function",
      "if",
      "import",
      "in",
      "instanceof",
      "new",
      "null",
      "return",
      "super",
      "switch",
      "this",
      "throw",
      "true",
      "try",
      "typeof",
      "var",
      "void",
      "while",
      "with"]
    strictModeReservedWords = [
      "as",
      "implements",
      "interface",
      "let",
      "package",
      "private",
      "protected",
      "public",
      "static",
      "yield"]
    contextuallKeywords = [
      "any",
      "boolean",
      "constructor",
      "declare",
      "from",
      "get",
      "module",
      "number",
      "of",
      "require",
      "set",
      "string",
      "symbol",
      "type"]
