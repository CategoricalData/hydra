{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier3.Ext.Python.Language where

import Hydra.Kernel
import qualified Hydra.Sources.Tier1.All as Tier1
import qualified Hydra.Sources.Tier2.All as Tier2
import Hydra.Dsl.Phantoms
import Hydra.Dsl.Coders as Coders
import Hydra.Dsl.Lib.Equality as Equality
import Hydra.Dsl.Lib.Flows as Flows
import Hydra.Dsl.Lib.Lists as Lists
import Hydra.Dsl.Lib.Logic as Logic
import Hydra.Dsl.Lib.Maps as Maps
import Hydra.Dsl.Lib.Sets as Sets
import Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Data.Set as S


pythonLanguageDefinition :: String -> TTerm a -> TElement a
pythonLanguageDefinition = definitionInModule pythonLanguageModule

pythonLanguageModule :: Module
pythonLanguageModule = Module ns elements
    [Tier1.hydraCodersModule, Tier2.hydraLexicalModule] [Tier1.hydraCoreModule, Tier1.hydraGraphModule, Tier1.hydraCodersModule] $
    Just "Language constraints and reserved words for Python 3"
  where
    ns = Namespace "hydra.ext.python.language"
    elements = [
      el pythonLanguageDef,
      el pythonReservedWordsDef]

pythonLanguageDef :: TElement Language
pythonLanguageDef = pythonLanguageDefinition "pythonLanguage" $
    doc "Language constraints for Python 3" $
    Coders.language "hydra.ext.python"
      eliminationVariants
      literalVariants
      floatTypes
      functionVariants
      integerTypes
      termVariants
      typeVariants
      typePredicate
  where
      eliminationVariants = [ -- TODO: verify whether all are supported
        EliminationVariantProduct,
        EliminationVariantRecord,
        EliminationVariantUnion,
        EliminationVariantWrap]
      literalVariants = [
        LiteralVariantBinary, -- bytes
        LiteralVariantBoolean, -- bool
        LiteralVariantFloat, -- (see float types)
        LiteralVariantInteger, -- (see integer types)
        LiteralVariantString] -- str
      floatTypes = [FloatTypeFloat64] -- Python has only one floating-point type
      functionVariants = [
        FunctionVariantElimination,
        FunctionVariantLambda,
        FunctionVariantPrimitive]
      integerTypes = [IntegerTypeBigint] -- Python has only one integer type
      termVariants = [ -- TODO: verify whether all are supported
        TermVariantApplication,
        TermVariantFunction,
        TermVariantLet,
        TermVariantList,
        TermVariantLiteral,
        TermVariantMap,
        TermVariantOptional,
        TermVariantProduct,
        TermVariantRecord,
        TermVariantSet,
        TermVariantUnion,
        -- TODO: TermVariantUnit, mapping to Python's None
        TermVariantVariable,
        TermVariantWrap]
      typeVariants = [ -- TODO: verify whether all are supported
        TypeVariantAnnotated,
        TypeVariantApplication,
        TypeVariantFunction,
        TypeVariantForall,
        TypeVariantList,
        TypeVariantLiteral,
        TypeVariantMap,
        TypeVariantOptional,
        TypeVariantProduct,
        TypeVariantRecord,
        TypeVariantSet,
        TypeVariantUnion,
        -- TODO: TypeVariantUnit, mapping to Python's NoneType
        TypeVariantVariable,
        TypeVariantWrap]
      typePredicate = constant true -- TODO: verify whether all are supported

pythonReservedWordsDef :: TElement (S.Set String)
pythonReservedWordsDef = pythonLanguageDefinition "pythonReservedWords" $
  doc "A set of reserved words in Python" $
  lets [
    "pythonKeywords">:
      doc "Python keywords, as enumerated at https://docs.python.org/3.13/reference/lexical_analysis.html#keywords" $
      list [
        "False", "None", "True", "and", "as", "assert", "async", "await", "break", "class", "continue", "def", "del",
        "elif", "else", "except", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", "nonlocal",
        "not", "or", "pass", "raise", "return", "try", "while", "with", "yield"],
    "hydraPythonKeywords">:
      doc "Reserved words which are specific to Hydra-Python" $
      list ["Node", "FrozenDict"]]
    $ Sets.fromList $ Lists.concat2 (var "pythonKeywords") (var "hydraPythonKeywords")
