module Hydra.Langs.Protobuf.Serde (
  descriptionOptionName,
  writeProtoFile) where

import Hydra.Tools.Serialization
import Hydra.Tools.Formatting
import qualified Hydra.Ast as CT
import qualified Hydra.Langs.Protobuf.Proto3 as P3

import qualified Data.List as L
import qualified Data.Maybe as Y


-- A special Protobuf option for descriptions (documentation)
descriptionOptionName = "_description"

protoBlock :: [CT.Expr] -> CT.Expr
protoBlock = brackets curlyBraces fullBlockStyle . doubleNewlineSep

semi :: CT.Expr -> CT.Expr
semi e = noSep [e, cst ";"]

optDesc :: Bool -> [P3.Option] -> CT.Expr -> CT.Expr
optDesc doubleNewline opts expr = if L.null descs
    then expr
    else sep [cst $ asComment (unValue $ P3.optionValue $ L.head descs), expr]
  where
    sep = if doubleNewline then doubleNewlineSep else newlineSep
    descs = L.filter (\(P3.Option name value) -> name == descriptionOptionName) opts
    asComment = L.intercalate "\n" . fmap (\s -> "// " ++ s) . lines
    unValue v = case v of
      P3.ValueBoolean b -> if b then "true" else "false"
      P3.ValueString s -> s

writeDefinition :: P3.Definition -> CT.Expr
writeDefinition def = case def of
  P3.DefinitionEnum enum -> writeEnumDefinition enum
  P3.DefinitionMessage msg -> writeMessageDefinition msg

writeEnumDefinition :: P3.EnumDefinition -> CT.Expr
writeEnumDefinition (P3.EnumDefinition name values options) = optDesc False options $ spaceSep [
  cst "enum",
  cst $ P3.unTypeName name,
  protoBlock (writeEnumValue <$> values)]

writeEnumValue :: P3.EnumValue -> CT.Expr
writeEnumValue (P3.EnumValue name number options) = optDesc False options $ semi $ spaceSep [
    cst $ P3.unEnumValueName name,
    cst "=",
    cst $ show number]

writeField :: P3.Field -> CT.Expr
writeField (P3.Field name jsonName typ num options) = optDesc False options $ case typ of
  P3.FieldTypeOneof fields -> spaceSep [
    cst "oneof",
    cst $ P3.unFieldName name,
    protoBlock (writeField <$> fields)]
  _ -> semi $ spaceSep [ -- TODO: jsonName
    writeFieldType typ,
    cst $ P3.unFieldName name,
    cst "=",
    cst $ show num]

writeFieldType :: P3.FieldType -> CT.Expr
writeFieldType ftyp = case ftyp of
  P3.FieldTypeMap st -> noSep [cst "map", angleBracesList inlineStyle [cst "string", writeSimpleType st]]
  P3.FieldTypeRepeated st -> spaceSep [cst "repeated", writeSimpleType st]
  P3.FieldTypeSimple st -> writeSimpleType st

writeImport :: P3.FileReference -> CT.Expr
writeImport (P3.FileReference path) = semi $ spaceSep [cst "import", cst $ show path]

writeMessageDefinition :: P3.MessageDefinition -> CT.Expr
writeMessageDefinition (P3.MessageDefinition name fields options) = optDesc False options $ spaceSep [
  cst "message",
  cst $ P3.unTypeName name,
  protoBlock (writeField <$> fields)]

writeOption :: P3.Option -> CT.Expr
writeOption (P3.Option name value) = semi $ spaceSep [cst "option", cst name, cst "=", writeValue value]

writeProtoFile :: P3.ProtoFile -> CT.Expr
writeProtoFile (P3.ProtoFile pkg imports defs options) = optDesc True options $ doubleNewlineSep
    $ Y.catMaybes [headerSec, importsSec, optionsSec, defsSec]
  where
    headerSec = Just $ newlineSep [
      semi $ cst "syntax = \"proto3\"",
      semi $ spaceSep [cst "package", cst (P3.unPackageName pkg)]]
    importsSec = if L.null imports
      then Nothing
      else Just $ newlineSep $ writeImport <$> imports
    optionsSec = if L.null options1
      then Nothing
      else Just $ newlineSep $ writeOption <$> options1
    defsSec = if L.null defs
      then Nothing
      else Just $ doubleNewlineSep $ writeDefinition <$> defs
    options1 = L.filter (\(P3.Option name value) -> name /= descriptionOptionName) options

writeScalarType :: P3.ScalarType -> CT.Expr
writeScalarType sct = cst $ case sct of
  P3.ScalarTypeBool -> "bool"
  P3.ScalarTypeBytes -> "bytes"
  P3.ScalarTypeDouble -> "double"
  P3.ScalarTypeFixed32 -> "fixed32"
  P3.ScalarTypeFixed64 -> "fixed64"
  P3.ScalarTypeFloat -> "float"
  P3.ScalarTypeInt32 -> "int32"
  P3.ScalarTypeInt64 -> "int64"
  P3.ScalarTypeSfixed32 -> "sfixed32"
  P3.ScalarTypeSfixed64 -> "sfixed64"
  P3.ScalarTypeSint32 -> "sint32"
  P3.ScalarTypeSint64 -> "sint64"
  P3.ScalarTypeString -> "string"
  P3.ScalarTypeUint32 -> "uint32"
  P3.ScalarTypeUint64 -> "uint64"

writeSimpleType :: P3.SimpleType -> CT.Expr
writeSimpleType st = case st of
  P3.SimpleTypeReference name -> cst $ P3.unTypeName name
  P3.SimpleTypeScalar sct -> writeScalarType sct

writeValue :: P3.Value -> CT.Expr
writeValue v = cst $ case v of
  P3.ValueBoolean b -> if b then "true" else "false"
  P3.ValueString s -> show s