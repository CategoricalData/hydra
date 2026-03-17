-- Note: this is an automatically generated file. Do not edit.

-- | Serialization functions for converting Pegasus PDL AST to abstract expressions

module Hydra.Ext.Pegasus.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Ext.Pegasus.Pdl as Pdl
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert PDL annotations to an optional expression (doc comment)
exprAnnotations :: Pdl.Annotations -> Maybe Ast.Expr
exprAnnotations anns =

      let d = Pdl.annotationsDoc anns
      in (Maybes.map (\s -> Serialization.cst (Formatting.javaStyleComment s)) d)

-- | Convert a PDL enum field to an expression
exprEnumField :: Pdl.EnumField -> Ast.Expr
exprEnumField ef =

      let name = Pdl.unEnumFieldName (Pdl.enumFieldName ef)
          anns = Pdl.enumFieldAnnotations ef
      in (withAnnotations anns (Serialization.cst name))

-- | Convert a qualified name to an import expression
exprImport :: Pdl.QualifiedName -> Ast.Expr
exprImport qn =
    Serialization.spaceSep [
      Serialization.cst "import",
      (exprQualifiedName qn)]

-- | Convert a named schema to an expression
exprNamedSchema :: Pdl.NamedSchema -> Ast.Expr
exprNamedSchema ns =

      let qn = Pdl.namedSchemaQualifiedName ns
          t = Pdl.namedSchemaType ns
          anns = Pdl.namedSchemaAnnotations ns
      in (withAnnotations anns (case t of
        Pdl.NamedSchemaTypeRecord v0 ->
          let fields = Pdl.recordSchemaFields v0
          in (Serialization.spaceSep [
            Serialization.cst "record",
            (exprQualifiedName qn),
            (Serialization.curlyBracesList Nothing Serialization.fullBlockStyle (Lists.map exprRecordField fields))])
        Pdl.NamedSchemaTypeEnum v0 ->
          let fields = Pdl.enumSchemaFields v0
          in (Serialization.spaceSep [
            Serialization.cst "enum",
            (exprQualifiedName qn),
            (Serialization.curlyBracesList Nothing Serialization.fullBlockStyle (Lists.map exprEnumField fields))])
        Pdl.NamedSchemaTypeTyperef v0 -> Serialization.spaceSep [
          Serialization.cst "typeref",
          (exprQualifiedName qn),
          (Serialization.cst "="),
          (exprSchema v0)]))

-- | Convert a primitive type to an expression
exprPrimitiveType :: Pdl.PrimitiveType -> Ast.Expr
exprPrimitiveType pt =
    Serialization.cst (case pt of
      Pdl.PrimitiveTypeBoolean -> "boolean"
      Pdl.PrimitiveTypeBytes -> "bytes"
      Pdl.PrimitiveTypeDouble -> "double"
      Pdl.PrimitiveTypeFloat -> "float"
      Pdl.PrimitiveTypeInt -> "int"
      Pdl.PrimitiveTypeLong -> "long"
      Pdl.PrimitiveTypeString -> "string")

-- | Convert a qualified name to an expression
exprQualifiedName :: Pdl.QualifiedName -> Ast.Expr
exprQualifiedName qn =

      let name = Pdl.unName (Pdl.qualifiedNameName qn)
          ns = Pdl.qualifiedNameNamespace qn
          parts =
                  Maybes.cat [
                    Maybes.map (\n -> Pdl.unNamespace n) ns,
                    (Maybes.pure name)]
      in (Serialization.cst (Strings.intercalate "." parts))

-- | Convert a record field to an expression
exprRecordField :: Pdl.RecordField -> Ast.Expr
exprRecordField rf =

      let name = Pdl.unFieldName (Pdl.recordFieldName rf)
          schema = Pdl.recordFieldValue rf
          optional = Pdl.recordFieldOptional rf
          anns = Pdl.recordFieldAnnotations rf
      in (withAnnotations anns (Serialization.spaceSep (Maybes.cat [
        Maybes.pure (Serialization.cst (Strings.cat2 name ":")),
        (Logic.ifElse optional (Maybes.pure (Serialization.cst "optional")) Nothing),
        (Maybes.pure (exprSchema schema))])))

-- | Convert a schema to an expression
exprSchema :: Pdl.Schema -> Ast.Expr
exprSchema schema =
    case schema of
      Pdl.SchemaArray v0 -> Serialization.noSep [
        Serialization.cst "array",
        (Serialization.bracketList Serialization.inlineStyle [
          exprSchema v0])]
      Pdl.SchemaMap v0 -> Serialization.noSep [
        Serialization.cst "map",
        (Serialization.bracketList Serialization.inlineStyle [
          Serialization.cst "string",
          (exprSchema v0)])]
      Pdl.SchemaNamed v0 -> exprQualifiedName v0
      Pdl.SchemaNull -> Serialization.cst "null"
      Pdl.SchemaPrimitive v0 -> exprPrimitiveType v0
      Pdl.SchemaUnion v0 -> Serialization.noSep [
        Serialization.cst "union",
        (Serialization.bracketList Serialization.fullBlockStyle (Lists.map exprUnionMember (Pdl.unUnionSchema v0)))]

-- | Convert a schema file to an expression
exprSchemaFile :: Pdl.SchemaFile -> Ast.Expr
exprSchemaFile sf =

      let ns = Pdl.unNamespace (Pdl.schemaFileNamespace sf)
          pkg = Pdl.schemaFilePackage sf
          imports = Pdl.schemaFileImports sf
          schemas = Pdl.schemaFileSchemas sf
          namespaceSec =
                  Maybes.pure (Serialization.spaceSep [
                    Serialization.cst "namespace",
                    (Serialization.cst ns)])
          packageSec =
                  Maybes.map (\p -> Serialization.spaceSep [
                    Serialization.cst "package",
                    (Serialization.cst (Pdl.unPackage p))]) pkg
          importsSec = Logic.ifElse (Lists.null imports) Nothing (Maybes.pure (Serialization.newlineSep (Lists.map exprImport imports)))
          schemaSecs = Lists.map (\s -> Maybes.pure (exprNamedSchema s)) schemas
      in (Serialization.doubleNewlineSep (Maybes.cat (Lists.concat [
        [
          namespaceSec,
          packageSec,
          importsSec],
        schemaSecs])))

-- | Convert a union member to an expression
exprUnionMember :: Pdl.UnionMember -> Ast.Expr
exprUnionMember um =

      let alias = Pdl.unionMemberAlias um
          schema = Pdl.unionMemberValue um
          anns = Pdl.unionMemberAnnotations um
      in (withAnnotations anns (Serialization.spaceSep (Maybes.cat [
        Maybes.map (\fn -> Serialization.cst (Strings.cat2 (Pdl.unFieldName fn) ":")) alias,
        (Maybes.pure (exprSchema schema))])))

-- | Prepend annotations (doc comment) to an expression
withAnnotations :: Pdl.Annotations -> Ast.Expr -> Ast.Expr
withAnnotations anns expr =
    Serialization.newlineSep (Maybes.cat [
      exprAnnotations anns,
      (Maybes.pure expr)])
