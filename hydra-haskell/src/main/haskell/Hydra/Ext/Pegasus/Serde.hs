module Hydra.Ext.Pegasus.Serde where

import Hydra.Staging.Serialization
import Hydra.Staging.Formatting
import qualified Hydra.Ast as CT
import qualified Hydra.Ext.Pegasus.Pdl as PDL

import qualified Data.List as L
import qualified Data.Maybe as Y


exprAnnotations :: PDL.Annotations -> Y.Maybe CT.Expr
exprAnnotations (PDL.Annotations doc _) = cst . javaStyleComment <$> doc

exprEnumField :: PDL.EnumField -> CT.Expr
exprEnumField (PDL.EnumField (PDL.EnumFieldName name) anns) = withAnnotations anns $ cst name

exprImport :: PDL.QualifiedName -> CT.Expr
exprImport qn = spaceSep [cst "import", exprQualifiedName qn]

exprNamedSchema :: PDL.NamedSchema -> CT.Expr
exprNamedSchema (PDL.NamedSchema qn t anns) = withAnnotations anns $
  case t of
    PDL.NamedSchemaTypeRecord (PDL.RecordSchema fields _) -> spaceSep [cst "record", exprQualifiedName qn,
      curlyBracesList Nothing fullBlockStyle (exprRecordField <$> fields)]
    PDL.NamedSchemaTypeEnum (PDL.EnumSchema fields) -> spaceSep [cst "enum", exprQualifiedName qn,
      curlyBracesList Nothing fullBlockStyle (exprEnumField <$> fields)]
    PDL.NamedSchemaTypeTyperef schema -> spaceSep [cst "typeref", exprQualifiedName qn, cst "=", exprSchema schema]

exprPrimitiveType :: PDL.PrimitiveType -> CT.Expr
exprPrimitiveType pt = cst $ case pt of
  PDL.PrimitiveTypeBoolean -> "boolean"
  PDL.PrimitiveTypeBytes -> "bytes"
  PDL.PrimitiveTypeDouble -> "double"
  PDL.PrimitiveTypeFloat -> "float"
  PDL.PrimitiveTypeInt -> "int"
  PDL.PrimitiveTypeLong -> "long"
  PDL.PrimitiveTypeString -> "string"

exprQualifiedName :: PDL.QualifiedName -> CT.Expr
exprQualifiedName (PDL.QualifiedName (PDL.Name name) ns) = cst $ L.intercalate "." $ Y.catMaybes [h <$> ns, Just name]
  where
    h (PDL.Namespace ns) = ns

exprRecordField :: PDL.RecordField -> CT.Expr
exprRecordField (PDL.RecordField (PDL.FieldName name) schema optional def anns) = withAnnotations anns $
  spaceSep $ Y.catMaybes [ -- TODO: default
    Just $ cst $ name ++ ":",
    if optional then Just (cst "optional") else Nothing,
    Just $ exprSchema schema]

exprSchema :: PDL.Schema -> CT.Expr
exprSchema schema = case schema of
  PDL.SchemaArray s -> noSep [cst "array", bracketList inlineStyle [exprSchema s]]
--  PDL.SchemaFixed i ->
--  PDL.SchemaInline ns ->
  PDL.SchemaMap s -> noSep [cst "map", bracketList inlineStyle [cst "string", exprSchema s]]
  PDL.SchemaNamed qn -> exprQualifiedName qn
  PDL.SchemaNull -> cst "null"
  PDL.SchemaPrimitive pt -> exprPrimitiveType pt
  PDL.SchemaUnion (PDL.UnionSchema us) -> noSep [cst "union", bracketList fullBlockStyle (exprUnionMember <$> us)]

exprSchemaFile :: PDL.SchemaFile -> CT.Expr
exprSchemaFile (PDL.SchemaFile (PDL.Namespace ns) pkg imports schemas) = doubleNewlineSep $ Y.catMaybes
    [namespaceSec, packageSec, importsSec] ++ schemaSecs
  where
    namespaceSec = Just $ spaceSep [cst "namespace", cst ns]
    packageSec = fmap (\(PDL.Package p) -> spaceSep [cst "package", cst p]) pkg
    importsSec = if L.null imports
      then Nothing
      else Just $ newlineSep (exprImport <$> imports)
    schemaSecs = exprNamedSchema <$> schemas

exprUnionMember :: PDL.UnionMember -> CT.Expr
exprUnionMember (PDL.UnionMember alias schema anns) = withAnnotations anns $
  spaceSep $ Y.catMaybes [
    fmap (\(PDL.FieldName n) -> cst $ n ++ ":") alias,
    Just $ exprSchema schema]

withAnnotations :: PDL.Annotations -> CT.Expr -> CT.Expr
withAnnotations anns expr = newlineSep $ Y.catMaybes [exprAnnotations anns, Y.Just expr]
