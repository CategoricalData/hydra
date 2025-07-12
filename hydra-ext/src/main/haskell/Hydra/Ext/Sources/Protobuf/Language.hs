{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Sources.Protobuf.Language (protobufLanguageModule) where

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


protobufLanguageDefinition :: String -> TTerm a -> TElement a
protobufLanguageDefinition = definitionInModule protobufLanguageModule

protobufLanguageModule :: Module
protobufLanguageModule = Module ns elements
    [Lexical.module_, Rewriting.module_]
    KernelTypes.kernelTypesModules $
    Just "Language constraints for Protobuf v3"
  where
    ns = Namespace "hydra.ext.protobuf.language"
    elements = [
      el protobufLanguageDef,
      el protobufReservedWordsDef]

protobufLanguageDef :: TElement Language
protobufLanguageDef = protobufLanguageDefinition "protobufLanguage" $
  doc "Language constraints for Protocol Buffers v3" $ lets [
  "eliminationVariants">: Sets.fromList $ list [],
  "literalVariants">: Sets.fromList $ list [
    Mantle.literalVariantBinary,
    Mantle.literalVariantBoolean,
    Mantle.literalVariantFloat,
    Mantle.literalVariantInteger,
    Mantle.literalVariantString],
  "floatTypes">: Sets.fromList $ list [
    Core.floatTypeFloat32,
    Core.floatTypeFloat64],
  "functionVariants">: Sets.fromList $ list [],
  "integerTypes">: Sets.fromList $ list [
    Core.integerTypeInt32,
    Core.integerTypeInt64,
    Core.integerTypeUint32,
    Core.integerTypeUint64],
  "termVariants">: Sets.fromList $ list [
    Mantle.termVariantList,
    Mantle.termVariantLiteral,
    Mantle.termVariantMap,
    Mantle.termVariantOptional,
    Mantle.termVariantRecord,
    Mantle.termVariantUnion,
    Mantle.termVariantUnit],
  "typeVariants">: Sets.fromList $ list [
    Mantle.typeVariantAnnotated,
    Mantle.typeVariantList,
    Mantle.typeVariantLiteral,
    Mantle.typeVariantMap,
    Mantle.typeVariantOptional,
    Mantle.typeVariantRecord,
    Mantle.typeVariantUnion,
    Mantle.typeVariantUnit,
    Mantle.typeVariantVariable],
  "typePredicate">: lambda "typ" $ cases _Type (var "typ")
    (Just true) [
    _Type_map>>: lambda "mt" $ lets [
      "valuesType">: Core.mapTypeValues $ var "mt",
      "stripped">: ref Rewriting.deannotateTypeDef @@ var "valuesType"] $
      cases _Type (var "stripped")
        (Just true) [
        _Type_optional>>: constant false]]] $
  Coders.language
    (Coders.languageName $ string "hydra.ext.protobuf")
    (Coders.languageConstraints
      (var "eliminationVariants")
      (var "literalVariants")
      (var "floatTypes")
      (var "functionVariants")
      (var "integerTypes")
      (var "termVariants")
      (var "typeVariants")
      (var "typePredicate"))

protobufReservedWordsDef :: TElement (S.Set String)
protobufReservedWordsDef = protobufLanguageDefinition "protobufReservedWords" $
  doc "A set of reserved words in Protobuf" $ lets [
  "fieldNames">:
    doc "See: http://google.github.io/proto-lens/reserved-names.html" $
    list $ string <$> [
      "case", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import", "in", "infix", "infixl",
      "infixr", "instance", "let", "mdo", "module", "newtype", "of", "pattern", "proc", "rec", "then", "type", "where"]] $
  Sets.fromList $ Lists.concat $ list [var "fieldNames"]
