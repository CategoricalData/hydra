module Hydra.Ext.Sources.Graphql.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Meta.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Json                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Meta.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple   as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Ast
import qualified Hydra.Ext.Org.Graphql.Syntax as G
import Hydra.Ext.Sources.Graphql.Syntax (graphqlSyntaxModule)


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.graphql.serde"

module_ :: Module
module_ = Module ns elements
    [Serialization.ns]
    (moduleNamespace graphqlSyntaxModule:KernelTypes.kernelTypesNamespaces) $
    Just "Serialization functions for converting GraphQL AST to abstract expressions"
  where
    elements = [
      toBinding exprDefinition,
      toBinding exprDescription,
      toBinding exprDocument,
      toBinding exprEnumTypeDefinition,
      toBinding exprEnumValue,
      toBinding exprEnumValueDefinition,
      toBinding exprFieldDefinition,
      toBinding exprListType,
      toBinding exprName,
      toBinding exprNamedType,
      toBinding exprNonNullType,
      toBinding exprObjectTypeDefinition,
      toBinding exprType,
      toBinding exprTypeDefinition,
      toBinding exprTypeSystemDefinition,
      toBinding exprTypeSystemDefinitionOrExtension,
      toBinding withDescription]


exprDefinition :: TBinding (G.Definition -> Expr)
exprDefinition = define "exprDefinition" $
  doc "Convert a GraphQL definition to an expression" $
  lambda "def" $
    cases G._Definition (var "def") Nothing [
      G._Definition_executable>>: constant $
        Serialization.cst @@ string "Unsupported: executable definition",
      G._Definition_typeSystem>>: lambda "de" $
        exprTypeSystemDefinitionOrExtension @@ var "de"]

exprDescription :: TBinding (G.Description -> Expr)
exprDescription = define "exprDescription" $
  doc "Convert a GraphQL description to a triple-quoted comment" $
  lambda "desc" $ lets [
    "delim">: Serialization.cst @@ string "\"\"\"",
    "text">: unwrap G._StringValue @@ (unwrap G._Description @@ var "desc")] $
    Serialization.newlineSep @@ list [var "delim", Serialization.cst @@ var "text", var "delim"]

exprDocument :: TBinding (G.Document -> Expr)
exprDocument = define "exprDocument" $
  doc "Convert a GraphQL document to an expression" $
  lambda "d" $ Serialization.doubleNewlineSep @@
    (Lists.map exprDefinition (unwrap G._Document @@ var "d"))

exprEnumTypeDefinition :: TBinding (G.EnumTypeDefinition -> Expr)
exprEnumTypeDefinition = define "exprEnumTypeDefinition" $
  doc "Convert a GraphQL enum type definition to an expression" $
  lambda "def" $ lets [
    "desc">: project G._EnumTypeDefinition G._EnumTypeDefinition_Description @@ var "def",
    "name">: project G._EnumTypeDefinition G._EnumTypeDefinition_Name @@ var "def",
    "values">: project G._EnumTypeDefinition G._EnumTypeDefinition_EnumValuesDefinition @@ var "def",
    "valuesExpr">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "vs" $ Lists.map exprEnumValueDefinition (unwrap G._EnumValuesDefinition @@ var "vs"))
      (var "values")] $
    withDescription @@ var "desc" @@
      (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "enum",
        exprName @@ var "name",
        Serialization.curlyBracesList @@ nothing @@ Serialization.fullBlockStyle @@ var "valuesExpr"])

exprEnumValue :: TBinding (G.EnumValue -> Expr)
exprEnumValue = define "exprEnumValue" $
  doc "Convert a GraphQL enum value to an expression" $
  lambda "ev" $ exprName @@ (unwrap G._EnumValue @@ var "ev")

exprEnumValueDefinition :: TBinding (G.EnumValueDefinition -> Expr)
exprEnumValueDefinition = define "exprEnumValueDefinition" $
  doc "Convert a GraphQL enum value definition to an expression" $
  lambda "def" $ lets [
    "desc">: project G._EnumValueDefinition G._EnumValueDefinition_Description @@ var "def",
    "ev">: project G._EnumValueDefinition G._EnumValueDefinition_EnumValue @@ var "def"] $
    withDescription @@ var "desc" @@ (exprEnumValue @@ var "ev")

exprFieldDefinition :: TBinding (G.FieldDefinition -> Expr)
exprFieldDefinition = define "exprFieldDefinition" $
  doc "Convert a GraphQL field definition to an expression" $
  lambda "def" $ lets [
    "desc">: project G._FieldDefinition G._FieldDefinition_Description @@ var "def",
    "name">: project G._FieldDefinition G._FieldDefinition_Name @@ var "def",
    "typ">: project G._FieldDefinition G._FieldDefinition_Type @@ var "def",
    "namePart">: Serialization.noSep @@ list [exprName @@ var "name", Serialization.cst @@ string ":"],
    "typePart">: exprType @@ var "typ"] $
    withDescription @@ var "desc" @@
      (Serialization.spaceSep @@ list [var "namePart", var "typePart"])

exprListType :: TBinding (G.ListType -> Expr)
exprListType = define "exprListType" $
  doc "Convert a GraphQL list type to an expression" $
  lambda "lt" $ Serialization.noSep @@ list [
    Serialization.cst @@ string "[",
    exprType @@ (unwrap G._ListType @@ var "lt"),
    Serialization.cst @@ string "]"]

exprName :: TBinding (G.Name -> Expr)
exprName = define "exprName" $
  doc "Convert a GraphQL name to an expression" $
  lambda "n" $ Serialization.cst @@ (unwrap G._Name @@ var "n")

exprNamedType :: TBinding (G.NamedType -> Expr)
exprNamedType = define "exprNamedType" $
  doc "Convert a GraphQL named type to an expression" $
  lambda "nt" $ exprName @@ (unwrap G._NamedType @@ var "nt")

exprNonNullType :: TBinding (G.NonNullType -> Expr)
exprNonNullType = define "exprNonNullType" $
  doc "Convert a GraphQL non-null type to an expression" $
  lambda "nnt" $ lets [
    "typeExpr">: cases G._NonNullType (var "nnt") Nothing [
      G._NonNullType_named>>: lambda "nt" $ exprNamedType @@ var "nt",
      G._NonNullType_list>>: lambda "lt" $ exprListType @@ var "lt"]] $
    Serialization.noSep @@ list [var "typeExpr", Serialization.cst @@ string "!"]

exprObjectTypeDefinition :: TBinding (G.ObjectTypeDefinition -> Expr)
exprObjectTypeDefinition = define "exprObjectTypeDefinition" $
  doc "Convert a GraphQL object type definition to an expression" $
  lambda "def" $ lets [
    "desc">: project G._ObjectTypeDefinition G._ObjectTypeDefinition_Description @@ var "def",
    "name">: project G._ObjectTypeDefinition G._ObjectTypeDefinition_Name @@ var "def",
    "fields">: project G._ObjectTypeDefinition G._ObjectTypeDefinition_FieldsDefinition @@ var "def",
    "fieldsExpr">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "fs" $ Lists.map exprFieldDefinition (unwrap G._FieldsDefinition @@ var "fs"))
      (var "fields")] $
    withDescription @@ var "desc" @@
      (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "type",
        exprName @@ var "name",
        Serialization.curlyBracesList @@ nothing @@ Serialization.fullBlockStyle @@ var "fieldsExpr"])

exprType :: TBinding (G.Type -> Expr)
exprType = define "exprType" $
  doc "Convert a GraphQL type to an expression" $
  lambda "typ" $
    cases G._Type (var "typ") Nothing [
      G._Type_named>>: lambda "nt" $ exprNamedType @@ var "nt",
      G._Type_list>>: lambda "lt" $ exprListType @@ var "lt",
      G._Type_nonNull>>: lambda "nnt" $ exprNonNullType @@ var "nnt"]

exprTypeDefinition :: TBinding (G.TypeDefinition -> Expr)
exprTypeDefinition = define "exprTypeDefinition" $
  doc "Convert a GraphQL type definition to an expression" $
  lambda "def" $
    cases G._TypeDefinition (var "def") Nothing [
      G._TypeDefinition_scalar>>: constant $
        Serialization.cst @@ string "Unsupported: scalar type definition",
      G._TypeDefinition_object>>: lambda "od" $ exprObjectTypeDefinition @@ var "od",
      G._TypeDefinition_interface>>: constant $
        Serialization.cst @@ string "Unsupported: interface type definition",
      G._TypeDefinition_union>>: constant $
        Serialization.cst @@ string "Unsupported: union type definition",
      G._TypeDefinition_enum>>: lambda "ed" $ exprEnumTypeDefinition @@ var "ed",
      G._TypeDefinition_inputObject>>: constant $
        Serialization.cst @@ string "Unsupported: input object type definition"]

exprTypeSystemDefinition :: TBinding (G.TypeSystemDefinition -> Expr)
exprTypeSystemDefinition = define "exprTypeSystemDefinition" $
  doc "Convert a GraphQL type system definition to an expression" $
  lambda "def" $
    cases G._TypeSystemDefinition (var "def") Nothing [
      G._TypeSystemDefinition_schema>>: constant $
        Serialization.cst @@ string "Unsupported: schema definition",
      G._TypeSystemDefinition_type>>: lambda "dt" $ exprTypeDefinition @@ var "dt",
      G._TypeSystemDefinition_directive>>: constant $
        Serialization.cst @@ string "Unsupported: directive definition"]

exprTypeSystemDefinitionOrExtension :: TBinding (G.TypeSystemDefinitionOrExtension -> Expr)
exprTypeSystemDefinitionOrExtension = define "exprTypeSystemDefinitionOrExtension" $
  doc "Convert a GraphQL type system definition or extension to an expression" $
  lambda "de" $
    cases G._TypeSystemDefinitionOrExtension (var "de") Nothing [
      G._TypeSystemDefinitionOrExtension_definition>>: lambda "d" $
        exprTypeSystemDefinition @@ var "d",
      G._TypeSystemDefinitionOrExtension_extension>>: constant $
        Serialization.cst @@ string "Unsupported: type system extension"]

withDescription :: TBinding (Maybe G.Description -> Expr -> Expr)
withDescription = define "withDescription" $
  doc "Prepend an optional description to an expression" $
  lambda "mdesc" $ lambda "expr" $
    Serialization.newlineSep @@ (Maybes.cat $ list [
      Maybes.map exprDescription (var "mdesc"),
      Maybes.pure (var "expr")])
