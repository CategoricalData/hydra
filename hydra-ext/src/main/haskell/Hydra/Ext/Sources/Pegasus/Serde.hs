module Hydra.Ext.Sources.Pegasus.Serde where

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
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
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
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
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
import qualified Hydra.Ext.Pegasus.Pdl as PDL
import qualified Hydra.Ext.Sources.Pegasus.Pdl as PdlSyntax


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.pegasus.serde"

module_ :: Module
module_ = Module ns elements
    [Formatting.ns, Serialization.ns]
    (PdlSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Serialization functions for converting Pegasus PDL AST to abstract expressions"
  where
    elements = [
      toBinding exprAnnotations,
      toBinding exprEnumField,
      toBinding exprImport,
      toBinding exprNamedSchema,
      toBinding exprPrimitiveType,
      toBinding exprQualifiedName,
      toBinding exprRecordField,
      toBinding exprSchema,
      toBinding exprSchemaFile,
      toBinding exprUnionMember,
      toBinding withAnnotations]


exprAnnotations :: TBinding (PDL.Annotations -> Maybe Expr)
exprAnnotations = define "exprAnnotations" $
  doc "Convert PDL annotations to an optional expression (doc comment)" $
  lambda "anns" $ lets [
    "d">: project PDL._Annotations PDL._Annotations_doc @@ var "anns"] $
    Maybes.map (lambda "s" $ Serialization.cst @@ (Formatting.javaStyleComment @@ var "s")) (var "d")

exprEnumField :: TBinding (PDL.EnumField -> Expr)
exprEnumField = define "exprEnumField" $
  doc "Convert a PDL enum field to an expression" $
  lambda "ef" $ lets [
    "name">: unwrap PDL._EnumFieldName @@ (project PDL._EnumField PDL._EnumField_name @@ var "ef"),
    "anns">: project PDL._EnumField PDL._EnumField_annotations @@ var "ef"] $
    withAnnotations @@ var "anns" @@ (Serialization.cst @@ var "name")

exprImport :: TBinding (PDL.QualifiedName -> Expr)
exprImport = define "exprImport" $
  doc "Convert a qualified name to an import expression" $
  lambda "qn" $ Serialization.spaceSep @@ list [
    Serialization.cst @@ string "import",
    exprQualifiedName @@ var "qn"]

exprNamedSchema :: TBinding (PDL.NamedSchema -> Expr)
exprNamedSchema = define "exprNamedSchema" $
  doc "Convert a named schema to an expression" $
  lambda "ns" $ lets [
    "qn">: project PDL._NamedSchema PDL._NamedSchema_qualifiedName @@ var "ns",
    "t">: project PDL._NamedSchema PDL._NamedSchema_type @@ var "ns",
    "anns">: project PDL._NamedSchema PDL._NamedSchema_annotations @@ var "ns"] $
    withAnnotations @@ var "anns" @@
      (cases PDL._NamedSchemaType (var "t") Nothing [
        PDL._NamedSchemaType_record>>: lambda "rs" $ lets [
          "fields">: project PDL._RecordSchema PDL._RecordSchema_fields @@ var "rs"] $
          Serialization.spaceSep @@ list [
            Serialization.cst @@ string "record",
            exprQualifiedName @@ var "qn",
            Serialization.curlyBracesList @@ nothing @@ Serialization.fullBlockStyle @@
              (Lists.map exprRecordField (var "fields"))],
        PDL._NamedSchemaType_enum>>: lambda "es" $ lets [
          "fields">: project PDL._EnumSchema PDL._EnumSchema_fields @@ var "es"] $
          Serialization.spaceSep @@ list [
            Serialization.cst @@ string "enum",
            exprQualifiedName @@ var "qn",
            Serialization.curlyBracesList @@ nothing @@ Serialization.fullBlockStyle @@
              (Lists.map exprEnumField (var "fields"))],
        PDL._NamedSchemaType_typeref>>: lambda "schema" $
          Serialization.spaceSep @@ list [
            Serialization.cst @@ string "typeref",
            exprQualifiedName @@ var "qn",
            Serialization.cst @@ string "=",
            exprSchema @@ var "schema"]])

exprPrimitiveType :: TBinding (PDL.PrimitiveType -> Expr)
exprPrimitiveType = define "exprPrimitiveType" $
  doc "Convert a primitive type to an expression" $
  lambda "pt" $ Serialization.cst @@
    (cases PDL._PrimitiveType (var "pt") Nothing [
      PDL._PrimitiveType_boolean>>: constant $ string "boolean",
      PDL._PrimitiveType_bytes>>: constant $ string "bytes",
      PDL._PrimitiveType_double>>: constant $ string "double",
      PDL._PrimitiveType_float>>: constant $ string "float",
      PDL._PrimitiveType_int>>: constant $ string "int",
      PDL._PrimitiveType_long>>: constant $ string "long",
      PDL._PrimitiveType_string>>: constant $ string "string"])

exprQualifiedName :: TBinding (PDL.QualifiedName -> Expr)
exprQualifiedName = define "exprQualifiedName" $
  doc "Convert a qualified name to an expression" $
  lambda "qn" $ lets [
    "name">: unwrap PDL._Name @@ (project PDL._QualifiedName PDL._QualifiedName_name @@ var "qn"),
    "ns">: project PDL._QualifiedName PDL._QualifiedName_namespace @@ var "qn",
    "parts">: Maybes.cat $ list [
      Maybes.map (lambda "n" $ unwrap PDL._Namespace @@ var "n") (var "ns"),
      Maybes.pure (var "name")]] $
    Serialization.cst @@ (Strings.intercalate (string ".") (var "parts"))

exprRecordField :: TBinding (PDL.RecordField -> Expr)
exprRecordField = define "exprRecordField" $
  doc "Convert a record field to an expression" $
  lambda "rf" $ lets [
    "name">: unwrap PDL._FieldName @@ (project PDL._RecordField PDL._RecordField_name @@ var "rf"),
    "schema">: project PDL._RecordField PDL._RecordField_value @@ var "rf",
    "optional">: project PDL._RecordField PDL._RecordField_optional @@ var "rf",
    "anns">: project PDL._RecordField PDL._RecordField_annotations @@ var "rf"] $
    withAnnotations @@ var "anns" @@
      (Serialization.spaceSep @@ (Maybes.cat $ list [
        Maybes.pure (Serialization.cst @@ (Strings.cat2 (var "name") (string ":"))),
        Logic.ifElse (var "optional")
          (Maybes.pure (Serialization.cst @@ string "optional"))
          nothing,
        Maybes.pure (exprSchema @@ var "schema")]))

exprSchema :: TBinding (PDL.Schema -> Expr)
exprSchema = define "exprSchema" $
  doc "Convert a schema to an expression" $
  lambda "schema" $
    cases PDL._Schema (var "schema") Nothing [
      PDL._Schema_array>>: lambda "s" $ Serialization.noSep @@ list [
        Serialization.cst @@ string "array",
        Serialization.bracketList @@ Serialization.inlineStyle @@ list [exprSchema @@ var "s"]],
      PDL._Schema_map>>: lambda "s" $ Serialization.noSep @@ list [
        Serialization.cst @@ string "map",
        Serialization.bracketList @@ Serialization.inlineStyle @@ list [
          Serialization.cst @@ string "string",
          exprSchema @@ var "s"]],
      PDL._Schema_named>>: lambda "qn" $ exprQualifiedName @@ var "qn",
      PDL._Schema_null>>: constant $ Serialization.cst @@ string "null",
      PDL._Schema_primitive>>: lambda "pt" $ exprPrimitiveType @@ var "pt",
      PDL._Schema_union>>: lambda "us" $
        Serialization.noSep @@ list [
          Serialization.cst @@ string "union",
          Serialization.bracketList @@ Serialization.fullBlockStyle @@
            (Lists.map exprUnionMember (unwrap PDL._UnionSchema @@ var "us"))]]

exprSchemaFile :: TBinding (PDL.SchemaFile -> Expr)
exprSchemaFile = define "exprSchemaFile" $
  doc "Convert a schema file to an expression" $
  lambda "sf" $ lets [
    "ns">: unwrap PDL._Namespace @@ (project PDL._SchemaFile PDL._SchemaFile_namespace @@ var "sf"),
    "pkg">: project PDL._SchemaFile PDL._SchemaFile_package @@ var "sf",
    "imports">: project PDL._SchemaFile PDL._SchemaFile_imports @@ var "sf",
    "schemas">: project PDL._SchemaFile PDL._SchemaFile_schemas @@ var "sf",
    "namespaceSec">: Maybes.pure (Serialization.spaceSep @@ list [
      Serialization.cst @@ string "namespace",
      Serialization.cst @@ var "ns"]),
    "packageSec">: Maybes.map
      (lambda "p" $ Serialization.spaceSep @@ list [
        Serialization.cst @@ string "package",
        Serialization.cst @@ (unwrap PDL._Package @@ var "p")])
      (var "pkg"),
    "importsSec">: Logic.ifElse (Lists.null (var "imports"))
      nothing
      (Maybes.pure (Serialization.newlineSep @@ (Lists.map exprImport (var "imports")))),
    "schemaSecs">: Lists.map (lambda "s" $ Maybes.pure (exprNamedSchema @@ var "s")) (var "schemas")] $
    Serialization.doubleNewlineSep @@ (Maybes.cat $
      Lists.concat $ list [
        list [var "namespaceSec", var "packageSec", var "importsSec"],
        var "schemaSecs"])

exprUnionMember :: TBinding (PDL.UnionMember -> Expr)
exprUnionMember = define "exprUnionMember" $
  doc "Convert a union member to an expression" $
  lambda "um" $ lets [
    "alias">: project PDL._UnionMember PDL._UnionMember_alias @@ var "um",
    "schema">: project PDL._UnionMember PDL._UnionMember_value @@ var "um",
    "anns">: project PDL._UnionMember PDL._UnionMember_annotations @@ var "um"] $
    withAnnotations @@ var "anns" @@
      (Serialization.spaceSep @@ (Maybes.cat $ list [
        Maybes.map (lambda "fn" $
          Serialization.cst @@ (Strings.cat2 (unwrap PDL._FieldName @@ var "fn") (string ":")))
          (var "alias"),
        Maybes.pure (exprSchema @@ var "schema")]))

withAnnotations :: TBinding (PDL.Annotations -> Expr -> Expr)
withAnnotations = define "withAnnotations" $
  doc "Prepend annotations (doc comment) to an expression" $
  lambda "anns" $ lambda "expr" $
    Serialization.newlineSep @@ (Maybes.cat $ list [
      exprAnnotations @@ var "anns",
      Maybes.pure (var "expr")])
