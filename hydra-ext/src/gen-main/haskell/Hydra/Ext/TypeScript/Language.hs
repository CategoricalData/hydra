-- | Language constraints for TypeScript

module Hydra.Ext.TypeScript.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Language constraints for TypeScript
typeScriptLanguage :: Coders.Language
typeScriptLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.langs.typeScript"),
  Coders.languageConstraints = Coders.LanguageConstraints {
    Coders.languageConstraintsEliminationVariants = Sets.empty,
    Coders.languageConstraintsLiteralVariants = (Sets.fromList [
      Mantle.LiteralVariantBoolean,
      Mantle.LiteralVariantFloat,
      Mantle.LiteralVariantInteger,
      Mantle.LiteralVariantString]),
    Coders.languageConstraintsFloatTypes = (Sets.fromList [
      Core.FloatTypeFloat64]),
    Coders.languageConstraintsFunctionVariants = Sets.empty,
    Coders.languageConstraintsIntegerTypes = (Sets.fromList [
      Core.IntegerTypeBigint]),
    Coders.languageConstraintsTermVariants = (Sets.fromList [
      Mantle.TermVariantList,
      Mantle.TermVariantLiteral,
      Mantle.TermVariantMap,
      Mantle.TermVariantOptional,
      Mantle.TermVariantRecord,
      Mantle.TermVariantUnion]),
    Coders.languageConstraintsTypeVariants = (Sets.fromList [
      Mantle.TypeVariantAnnotated,
      Mantle.TypeVariantList,
      Mantle.TypeVariantLiteral,
      Mantle.TypeVariantMap,
      Mantle.TypeVariantOptional,
      Mantle.TypeVariantRecord,
      Mantle.TypeVariantUnion,
      Mantle.TypeVariantVariable]),
    Coders.languageConstraintsTypes = (\x -> case x of
      Core.TypeMap v1 -> ((\x -> case x of
        Core.TypeOptional _ -> False
        _ -> True) (Rewriting.deannotateType (Core.mapTypeValues v1)))
      _ -> True)}}

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
