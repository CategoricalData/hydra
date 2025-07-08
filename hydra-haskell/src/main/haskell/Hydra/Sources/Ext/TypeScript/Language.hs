{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Ext.TypeScript.Language (typeScriptLanguageModule) where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors                        as Accessors
import qualified Hydra.Dsl.Annotations                      as Anns
import qualified Hydra.Dsl.Ast                              as Ast
import qualified Hydra.Dsl.Coders                           as Coders
import qualified Hydra.Dsl.Compute                          as Compute
import qualified Hydra.Dsl.Core                             as Core
import qualified Hydra.Dsl.Graph                            as Graph
import qualified Hydra.Dsl.Grammar                          as Grammar
import qualified Hydra.Dsl.Lib.Chars                        as Chars
import qualified Hydra.Dsl.Lib.Equality                     as Equality
import qualified Hydra.Dsl.Lib.Flows                        as Flows
import qualified Hydra.Dsl.Lib.Lists                        as Lists
import qualified Hydra.Dsl.Lib.Literals                     as Literals
import qualified Hydra.Dsl.Lib.Logic                        as Logic
import qualified Hydra.Dsl.Lib.Maps                         as Maps
import qualified Hydra.Dsl.Lib.Math                         as Math
import qualified Hydra.Dsl.Lib.Optionals                    as Optionals
import qualified Hydra.Dsl.Lib.Sets                         as Sets
import           Hydra.Dsl.Lib.Strings                      as Strings
import qualified Hydra.Dsl.Mantle                           as Mantle
import qualified Hydra.Dsl.Module                           as Module
import           Hydra.Dsl.Phantoms                         as Phantoms
import qualified Hydra.Dsl.TTerms                           as TTerms
import qualified Hydra.Dsl.TTypes                           as TTypes
import qualified Hydra.Dsl.Tabular                          as Tabular
import qualified Hydra.Dsl.Terms                            as Terms
import qualified Hydra.Dsl.Topology                         as Topology
import qualified Hydra.Dsl.Types                            as Types
import qualified Hydra.Dsl.Typing                           as Typing
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Decode.Core     as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Decoding        as Decoding
import qualified Hydra.Sources.Kernel.Terms.Describe.Core   as DescribeCore
import qualified Hydra.Sources.Kernel.Terms.Describe.Mantle as DescribeMantle
import qualified Hydra.Sources.Kernel.Terms.Encode.Core     as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Mantle  as ExtractMantle
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors  as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Mantle     as ShowMantle
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
import qualified Hydra.Sources.Kernel.Terms.Variants        as Variants
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y


typeScriptLanguageDefinition :: String -> TTerm a -> TElement a
typeScriptLanguageDefinition = definitionInModule typeScriptLanguageModule

typeScriptLanguageModule :: Module
typeScriptLanguageModule = Module ns elements
    [Rewriting.hydraRewritingModule]
    [KernelTypes.hydraCoreModule, KernelTypes.hydraGraphModule, KernelTypes.hydraCodersModule] $
    Just "Language constraints for TypeScript"
  where
    ns = Namespace "hydra.ext.typeScript.language"
    elements = [
      el typeScriptLanguageDef,
      el typeScriptReservedWordsDef]

typeScriptLanguageDef :: TElement Language
typeScriptLanguageDef = typeScriptLanguageDefinition "typeScriptLanguage" $
  doc "Language constraints for TypeScript" $
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
          _Type_optional>>: constant false] @@ (ref Rewriting.deannotateTypeDef @@ (Core.mapTypeValues $ var "mt")))]]]

typeScriptReservedWordsDef :: TElement (S.Set String)
typeScriptReservedWordsDef = typeScriptLanguageDefinition "typeScriptReservedWords" $
    doc "A set of reserved words in TypeScript. Taken directly from https://github.com/microsoft/TypeScript/issues/2536" $
    lets [
      "reservedWords">: list [
        "delete", "do", "else", "enum", "export", "extends", "false", "finally", "for", "function", "if", "import",
        "in", "instanceof", "new", "null", "return", "super", "switch", "this", "throw", "true", "try", "typeof", "var",
        "void", "while", "with"],
      "strictModeReservedWords">: list [
        "as", "implements", "interface", "let", "package", "private", "protected", "public", "static", "yield"],
      "contextuallKeywords">: list [
        "any", "boolean", "constructor", "declare", "from", "get", "module", "number", "of", "require", "set", "string",
        "symbol", "type"]]
      $ Sets.fromList $ Lists.concat $
        list [var "reservedWords", var "strictModeReservedWords", var "contextuallKeywords"]
