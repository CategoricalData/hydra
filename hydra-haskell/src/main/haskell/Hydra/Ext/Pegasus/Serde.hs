module Hydra.Ext.Pegasus.Serde (
  dataGraphToPdlString,
) where

import Hydra.Errors
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Ext.Pegasus.Coder
import Hydra.Util.Codetree.Print
import Hydra.Util.Codetree.Script
import qualified Hydra.Util.Codetree.Ast as CT
import qualified Hydra.Ext.Pegasus.Pdl as PDL
import Hydra.Impl.Haskell.Extras

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as Y


dataGraphToPdlString :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified String
dataGraphToPdlString cx g = do
  sf <- dataGraphToPegasusSchema cx g
  return $ printExpr $ parenthesize $ exprSchemaFile sf

exprEnumField :: PDL.EnumField -> CT.Expr
exprEnumField (PDL.EnumField name anns) = cst name -- TODO: annotations

exprImport :: PDL.QualifiedName -> CT.Expr
exprImport qn = spaceSep [cst "import", exprQualifiedName qn]

exprNamedSchema :: PDL.NamedSchema -> CT.Expr
exprNamedSchema (PDL.NamedSchema qn t anns) = case t of -- TODO: annotations
  PDL.NamedSchema_TypeRecord (PDL.RecordSchema fields _) -> spaceSep [cst "record", exprQualifiedName qn,
    curlyBracesList True (exprRecordField <$> fields)]
  PDL.NamedSchema_TypeEnum (PDL.EnumSchema fields) -> spaceSep [cst "enum", exprQualifiedName qn,
    curlyBracesList True (exprEnumField <$> fields)]
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
exprQualifiedName (PDL.QualifiedName name ns) = cst $ L.intercalate "." $ Y.catMaybes [ns, Just name]

exprRecordField :: PDL.RecordField -> CT.Expr
exprRecordField (PDL.RecordField name schema optional def anns) = spaceSep $ Y.catMaybes [ -- TODO: annotations, default
  if optional then Just (cst "optional") else Nothing,
  Just $ cst $ name ++ ":",
  Just $ exprSchema schema]

exprSchema :: PDL.Schema -> CT.Expr
exprSchema schema = case schema of
  PDL.SchemaArray s -> noSep [cst "array", bracketList False [exprSchema s]]
--  PDL.SchemaFixed i ->
--  PDL.SchemaInline ns ->
  PDL.SchemaMap s -> noSep [cst "map", bracketList False [cst "string", exprSchema s]]
  PDL.SchemaNamed qn -> exprQualifiedName qn
  PDL.SchemaNull -> cst "null"
  PDL.SchemaPrimitive pt -> exprPrimitiveType pt
  PDL.SchemaUnion us -> noSep [cst "union", bracketList True (exprUnionMember <$> us)]

exprSchemaFile :: PDL.SchemaFile -> CT.Expr
exprSchemaFile (PDL.SchemaFile ns pkg imports schemas) = doubleNewlineSep $ (Y.catMaybes
    [namespaceSec, packageSec, importsSec]) ++ schemaSecs
  where
    namespaceSec = Just $ spaceSep [cst "namespace", cst ns]
    packageSec = fmap (\p -> spaceSep [cst "package", cst p]) pkg
    importsSec = if L.null imports
      then Nothing
      else Just $ newlineSep (exprImport <$> imports)
    schemaSecs = exprNamedSchema <$> schemas

exprUnionMember :: PDL.UnionMember -> CT.Expr
exprUnionMember (PDL.UnionMember alias schema anns) = spaceSep $ Y.catMaybes [ -- TODO: annotations
  fmap (\n -> cst $ n ++ ":") alias,
  Just $ exprSchema schema]
