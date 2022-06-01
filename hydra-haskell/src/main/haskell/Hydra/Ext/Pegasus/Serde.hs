module Hydra.Ext.Pegasus.Serde (
  moduleToPdlString,
) where

import Hydra.Errors
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Ext.Pegasus.Coder
import Hydra.Util.Codetree.Script
import Hydra.Util.Formatting
import qualified Hydra.Util.Codetree.Ast as CT
import qualified Hydra.Ext.Pegasus.Pdl as PDL
import Hydra.Impl.Haskell.Extras

import qualified Data.List as L
import qualified Data.Maybe as Y


moduleToPdlString :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified String
moduleToPdlString cx g = do
  sf <- moduleToPegasusSchema cx g
  return $ printExpr $ parenthesize $ exprSchemaFile sf

exprAnnotations :: PDL.Annotations -> Y.Maybe CT.Expr
exprAnnotations (PDL.Annotations doc _) = cst . javaStyleComment <$> doc

exprEnumField :: PDL.EnumField -> CT.Expr
exprEnumField (PDL.EnumField (PDL.EnumFieldName name) anns) = withAnnotations anns $ cst name

exprImport :: PDL.QualifiedName -> CT.Expr
exprImport qn = spaceSep [cst "import", exprQualifiedName qn]

exprNamedSchema :: PDL.NamedSchema -> CT.Expr
exprNamedSchema (PDL.NamedSchema qn t anns) = withAnnotations anns $
  case t of
    PDL.NamedSchema_TypeRecord (PDL.RecordSchema fields _) -> spaceSep [cst "record", exprQualifiedName qn,
      curlyBracesList fullBlockStyle (exprRecordField <$> fields)]
    PDL.NamedSchema_TypeEnum (PDL.EnumSchema fields) -> spaceSep [cst "enum", exprQualifiedName qn,
      curlyBracesList fullBlockStyle (exprEnumField <$> fields)]
    PDL.NamedSchema_TypeTyperef schema -> spaceSep [cst "typeref", exprQualifiedName qn, cst "=", exprSchema schema]

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
    if optional then Just (cst "optional") else Nothing,
    Just $ cst $ name ++ ":",
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
