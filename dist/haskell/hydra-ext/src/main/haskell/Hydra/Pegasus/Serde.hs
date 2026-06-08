-- Note: this is an automatically generated file. Do not edit.
-- | Serialization functions for converting Pegasus PDL AST to abstract expressions

module Hydra.Pegasus.Serde where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Pegasus.Pdl as Pdl
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Convert PDL annotations to an optional expression (doc comment)
annotationsToExpr :: Pdl.Annotations -> Maybe Ast.Expr
annotationsToExpr anns =

      let d = Pdl.annotationsDoc anns
      in (Optionals.map (\s -> Serialization.cst (Formatting.javaStyleComment s)) d)
-- | Convert a PDL enum field to an expression
enumFieldToExpr :: Pdl.EnumField -> Ast.Expr
enumFieldToExpr ef =

      let name = Pdl.unEnumFieldName (Pdl.enumFieldName ef)
          anns = Pdl.enumFieldAnnotations ef
      in (withAnnotations anns (Serialization.cst name))
-- | Convert a qualified name to an import expression
importToExpr :: Pdl.QualifiedName -> Ast.Expr
importToExpr qn =
    Serialization.spaceSep [
      Serialization.cst "import",
      (qualifiedNameToExpr qn)]
-- | Convert a named schema to an expression
namedSchemaToExpr :: Pdl.NamedSchema -> Ast.Expr
namedSchemaToExpr ns =

      let qn = Pdl.namedSchemaQualifiedName ns
          t = Pdl.namedSchemaType ns
          anns = Pdl.namedSchemaAnnotations ns
      in (withAnnotations anns (case t of
        Pdl.NamedSchemaTypeRecord v0 ->
          let fields = Pdl.recordSchemaFields v0
          in (Serialization.spaceSep [
            Serialization.cst "record",
            (qualifiedNameToExpr qn),
            (Serialization.curlyBracesList Nothing Serialization.fullBlockStyle (Lists.map recordFieldToExpr fields))])
        Pdl.NamedSchemaTypeEnum v0 ->
          let fields = Pdl.enumSchemaFields v0
          in (Serialization.spaceSep [
            Serialization.cst "enum",
            (qualifiedNameToExpr qn),
            (Serialization.curlyBracesList Nothing Serialization.fullBlockStyle (Lists.map enumFieldToExpr fields))])
        Pdl.NamedSchemaTypeTyperef v0 -> Serialization.spaceSep [
          Serialization.cst "typeref",
          (qualifiedNameToExpr qn),
          (Serialization.cst "="),
          (schemaToExpr v0)]))
-- | Convert a primitive type to an expression
primitiveTypeToExpr :: Pdl.PrimitiveType -> Ast.Expr
primitiveTypeToExpr pt =
    Serialization.cst (case pt of
      Pdl.PrimitiveTypeBoolean -> "boolean"
      Pdl.PrimitiveTypeBytes -> "bytes"
      Pdl.PrimitiveTypeDouble -> "double"
      Pdl.PrimitiveTypeFloat -> "float"
      Pdl.PrimitiveTypeInt -> "int"
      Pdl.PrimitiveTypeLong -> "long"
      Pdl.PrimitiveTypeString -> "string")
-- | Convert a qualified name to an expression
qualifiedNameToExpr :: Pdl.QualifiedName -> Ast.Expr
qualifiedNameToExpr qn =

      let name = Pdl.unName (Pdl.qualifiedNameName qn)
          ns = Pdl.qualifiedNameNamespace qn
          parts =
                  Optionals.cat [
                    Optionals.map (\n -> Pdl.unNamespace n) ns,
                    (Optionals.pure name)]
      in (Serialization.cst (Strings.intercalate "." parts))
-- | Convert a record field to an expression
recordFieldToExpr :: Pdl.RecordField -> Ast.Expr
recordFieldToExpr rf =

      let name = Pdl.unFieldName (Pdl.recordFieldName rf)
          schema = Pdl.recordFieldValue rf
          optional = Pdl.recordFieldOptional rf
          anns = Pdl.recordFieldAnnotations rf
      in (withAnnotations anns (Serialization.spaceSep (Optionals.cat [
        Optionals.pure (Serialization.cst (Strings.cat2 name ":")),
        (Logic.ifElse optional (Optionals.pure (Serialization.cst "optional")) Nothing),
        (Optionals.pure (schemaToExpr schema))])))
-- | Convert a schema file to an expression
schemaFileToExpr :: Pdl.SchemaFile -> Ast.Expr
schemaFileToExpr sf =

      let ns = Pdl.unNamespace (Pdl.schemaFileNamespace sf)
          pkg = Pdl.schemaFilePackage sf
          imports = Pdl.schemaFileImports sf
          schemas = Pdl.schemaFileSchemas sf
          namespaceSec =
                  Optionals.pure (Serialization.spaceSep [
                    Serialization.cst "namespace",
                    (Serialization.cst ns)])
          packageSec =
                  Optionals.map (\p -> Serialization.spaceSep [
                    Serialization.cst "package",
                    (Serialization.cst (Pdl.unPackage p))]) pkg
          importsSec =
                  Logic.ifElse (Lists.null imports) Nothing (Optionals.pure (Serialization.newlineSep (Lists.map importToExpr imports)))
          schemaSecs = Lists.map (\s -> Optionals.pure (namedSchemaToExpr s)) schemas
      in (Serialization.doubleNewlineSep (Optionals.cat (Lists.concat [
        [
          namespaceSec,
          packageSec,
          importsSec],
        schemaSecs])))
-- | Convert a schema to an expression
schemaToExpr :: Pdl.Schema -> Ast.Expr
schemaToExpr schema =
    case schema of
      Pdl.SchemaArray v0 -> Serialization.noSep [
        Serialization.cst "array",
        (Serialization.bracketList Serialization.inlineStyle [
          schemaToExpr v0])]
      Pdl.SchemaMap v0 -> Serialization.noSep [
        Serialization.cst "map",
        (Serialization.bracketList Serialization.inlineStyle [
          Serialization.cst "string",
          (schemaToExpr v0)])]
      Pdl.SchemaNamed v0 -> qualifiedNameToExpr v0
      Pdl.SchemaNull -> Serialization.cst "null"
      Pdl.SchemaPrimitive v0 -> primitiveTypeToExpr v0
      Pdl.SchemaUnion v0 -> Serialization.noSep [
        Serialization.cst "union",
        (Serialization.bracketList Serialization.fullBlockStyle (Lists.map unionMemberToExpr (Pdl.unUnionSchema v0)))]
-- | Convert a union member to an expression
unionMemberToExpr :: Pdl.UnionMember -> Ast.Expr
unionMemberToExpr um =

      let alias = Pdl.unionMemberAlias um
          schema = Pdl.unionMemberValue um
          anns = Pdl.unionMemberAnnotations um
      in (withAnnotations anns (Serialization.spaceSep (Optionals.cat [
        Optionals.map (\fn -> Serialization.cst (Strings.cat2 (Pdl.unFieldName fn) ":")) alias,
        (Optionals.pure (schemaToExpr schema))])))
-- | Prepend annotations (doc comment) to an expression
withAnnotations :: Pdl.Annotations -> Ast.Expr -> Ast.Expr
withAnnotations anns expr =
    Serialization.newlineSep (Optionals.cat [
      annotationsToExpr anns,
      (Optionals.pure expr)])
