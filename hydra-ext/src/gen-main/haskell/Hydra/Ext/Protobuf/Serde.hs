-- Note: this is an automatically generated file. Do not edit.

-- | Serialization functions for converting Protocol Buffers v3 AST to abstract expressions

module Hydra.Ext.Protobuf.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Ext.Protobuf.Proto3 as Proto3
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | The name of the deprecated option
deprecatedOptionName :: String
deprecatedOptionName = "deprecated"

-- | A special Protobuf option name for descriptions (documentation)
descriptionOptionName :: String
descriptionOptionName = "_description"

-- | Filter out internal options (those whose names start with underscore)
excludeInternalOptions :: [Proto3.Option] -> [Proto3.Option]
excludeInternalOptions opts =
    Lists.filter (\opt -> Logic.not (Equality.equal (Strings.charAt 0 (Proto3.optionName opt)) 95)) opts

-- | Prepend an optional description comment to an expression
optDesc :: Bool -> [Proto3.Option] -> Ast.Expr -> Ast.Expr
optDesc doubleNewline opts expr =

      let descs = Lists.filter (\opt -> Equality.equal (Proto3.optionName opt) "_description") opts
      in (Logic.ifElse (Lists.null descs) expr (
        let descValue = Proto3.optionValue (Lists.head descs)
            descStr =
                    case descValue of
                      Proto3.ValueBoolean v0 -> Logic.ifElse v0 "true" "false"
                      Proto3.ValueString v0 -> v0
            commentLines = Lists.map (\line -> Strings.cat2 "// " line) (Strings.lines descStr)
            comment = Serialization.cst (Strings.intercalate "\n" commentLines)
            sep =
                    Logic.ifElse doubleNewline (Serialization.doubleNewlineSep [
                      comment,
                      expr]) (Serialization.newlineSep [
                      comment,
                      expr])
        in sep))

-- | Wrap expressions in a curly-braced block with double-newline separation
protoBlock :: [Ast.Expr] -> Ast.Expr
protoBlock exprs =
    Serialization.brackets Serialization.curlyBraces Serialization.fullBlockStyle (Serialization.doubleNewlineSep exprs)

-- | Append a semicolon to an expression
semi :: Ast.Expr -> Ast.Expr
semi e =
    Serialization.noSep [
      e,
      (Serialization.cst ";")]

-- | Convert a definition to an expression
writeDefinition :: Proto3.Definition -> Ast.Expr
writeDefinition def =
    case def of
      Proto3.DefinitionEnum v0 -> writeEnumDefinition v0
      Proto3.DefinitionMessage v0 -> writeMessageDefinition v0

-- | Convert an enum definition to an expression
writeEnumDefinition :: Proto3.EnumDefinition -> Ast.Expr
writeEnumDefinition ed =

      let name = Proto3.enumDefinitionName ed
          values = Proto3.enumDefinitionValues ed
          options = Proto3.enumDefinitionOptions ed
      in (optDesc False options (Serialization.spaceSep [
        Serialization.cst "enum",
        (Serialization.cst (Proto3.unTypeName name)),
        (protoBlock (Lists.map writeEnumValue values))]))

-- | Convert an enum value to an expression
writeEnumValue :: Proto3.EnumValue -> Ast.Expr
writeEnumValue ev =

      let name = Proto3.enumValueName ev
          number = Proto3.enumValueNumber ev
          options = Proto3.enumValueOptions ev
      in (optDesc False options (semi (Serialization.spaceSep [
        Serialization.cst (Proto3.unEnumValueName name),
        (Serialization.cst "="),
        (Serialization.cst (Literals.showInt32 number))])))

-- | Convert a field to an expression
writeField :: Proto3.Field -> Ast.Expr
writeField f =

      let name = Proto3.fieldName f
          typ = Proto3.fieldType f
          num = Proto3.fieldNumber f
          options = Proto3.fieldOptions f
      in (optDesc False options (case typ of
        Proto3.FieldTypeOneof v0 -> Serialization.spaceSep [
          Serialization.cst "oneof",
          (Serialization.cst (Proto3.unFieldName name)),
          (protoBlock (Lists.map writeField v0))]
        Proto3.FieldTypeMap v0 ->
          let kt = Proto3.mapTypeKeys v0
              vt = Proto3.mapTypeValues v0
          in (semi (Serialization.spaceSep (Maybes.cat [
            Maybes.pure (writeFieldType typ),
            (Maybes.pure (Serialization.cst (Proto3.unFieldName name))),
            (Maybes.pure (Serialization.cst "=")),
            (Maybes.pure (Serialization.cst (Literals.showInt32 num))),
            (writeFieldOptions options)])))
        Proto3.FieldTypeRepeated _ -> semi (Serialization.spaceSep (Maybes.cat [
          Maybes.pure (writeFieldType typ),
          (Maybes.pure (Serialization.cst (Proto3.unFieldName name))),
          (Maybes.pure (Serialization.cst "=")),
          (Maybes.pure (Serialization.cst (Literals.showInt32 num))),
          (writeFieldOptions options)]))
        Proto3.FieldTypeSimple _ -> semi (Serialization.spaceSep (Maybes.cat [
          Maybes.pure (writeFieldType typ),
          (Maybes.pure (Serialization.cst (Proto3.unFieldName name))),
          (Maybes.pure (Serialization.cst "=")),
          (Maybes.pure (Serialization.cst (Literals.showInt32 num))),
          (writeFieldOptions options)]))))

-- | Convert a field option to an expression
writeFieldOption :: Proto3.Option -> Ast.Expr
writeFieldOption opt =

      let name = Proto3.optionName opt
          value = Proto3.optionValue opt
      in (Serialization.spaceSep [
        Serialization.cst name,
        (Serialization.cst "="),
        (writeValue value)])

-- | Convert field options to an optional bracket-enclosed expression
writeFieldOptions :: [Proto3.Option] -> Maybe Ast.Expr
writeFieldOptions opts0 =

      let opts = excludeInternalOptions opts0
      in (Logic.ifElse (Lists.null opts) Nothing (Maybes.pure (Serialization.bracketList Serialization.inlineStyle (Lists.map writeFieldOption opts))))

-- | Convert a field type to an expression
writeFieldType :: Proto3.FieldType -> Ast.Expr
writeFieldType ftyp =
    case ftyp of
      Proto3.FieldTypeMap v0 ->
        let kt = Proto3.mapTypeKeys v0
            vt = Proto3.mapTypeValues v0
        in (Serialization.noSep [
          Serialization.cst "map",
          (Serialization.angleBracesList Serialization.inlineStyle [
            writeSimpleType kt,
            (writeSimpleType vt)])])
      Proto3.FieldTypeRepeated v0 -> Serialization.spaceSep [
        Serialization.cst "repeated",
        (writeSimpleType v0)]
      Proto3.FieldTypeSimple v0 -> writeSimpleType v0
      Proto3.FieldTypeOneof _ -> Serialization.cst "oneof"

-- | Convert a file-level option to an expression
writeFileOption :: Proto3.Option -> Ast.Expr
writeFileOption opt =

      let name = Proto3.optionName opt
          value = Proto3.optionValue opt
      in (semi (Serialization.spaceSep [
        Serialization.cst "option",
        (Serialization.cst name),
        (Serialization.cst "="),
        (writeValue value)]))

-- | Convert file-level options to an optional newline-separated expression
writeFileOptions :: [Proto3.Option] -> Maybe Ast.Expr
writeFileOptions opts0 =

      let opts = excludeInternalOptions opts0
      in (Logic.ifElse (Lists.null opts) Nothing (Maybes.pure (Serialization.newlineSep (Lists.map writeFileOption opts))))

-- | Convert a file reference to an import expression
writeImport :: Proto3.FileReference -> Ast.Expr
writeImport ref =
    semi (Serialization.spaceSep [
      Serialization.cst "import",
      (Serialization.cst (Literals.showString (Proto3.unFileReference ref)))])

-- | Convert a message definition to an expression
writeMessageDefinition :: Proto3.MessageDefinition -> Ast.Expr
writeMessageDefinition md =

      let name = Proto3.messageDefinitionName md
          fields = Proto3.messageDefinitionFields md
          options = Proto3.messageDefinitionOptions md
      in (optDesc False options (Serialization.spaceSep [
        Serialization.cst "message",
        (Serialization.cst (Proto3.unTypeName name)),
        (protoBlock (Lists.map writeField fields))]))

-- | Convert a proto file to an expression
writeProtoFile :: Proto3.ProtoFile -> Ast.Expr
writeProtoFile pf =

      let pkg = Proto3.protoFilePackage pf
          imports = Proto3.protoFileImports pf
          defs = Proto3.protoFileTypes pf
          options = Proto3.protoFileOptions pf
          headerSec =
                  Maybes.pure (Serialization.newlineSep [
                    semi (Serialization.cst "syntax = \"proto3\""),
                    (semi (Serialization.spaceSep [
                      Serialization.cst "package",
                      (Serialization.cst (Proto3.unPackageName pkg))]))])
          importsSec = Logic.ifElse (Lists.null imports) Nothing (Maybes.pure (Serialization.newlineSep (Lists.map writeImport imports)))
          options1 = Lists.filter (\opt -> Logic.not (Equality.equal (Proto3.optionName opt) "_description")) options
          optionsSec = writeFileOptions options1
          defsSec = Logic.ifElse (Lists.null defs) Nothing (Maybes.pure (Serialization.doubleNewlineSep (Lists.map writeDefinition defs)))
      in (optDesc True options (Serialization.doubleNewlineSep (Maybes.cat [
        headerSec,
        importsSec,
        optionsSec,
        defsSec])))

-- | Convert a scalar type to an expression
writeScalarType :: Proto3.ScalarType -> Ast.Expr
writeScalarType sct =
    Serialization.cst (case sct of
      Proto3.ScalarTypeBool -> "bool"
      Proto3.ScalarTypeBytes -> "bytes"
      Proto3.ScalarTypeDouble -> "double"
      Proto3.ScalarTypeFixed32 -> "fixed32"
      Proto3.ScalarTypeFixed64 -> "fixed64"
      Proto3.ScalarTypeFloat -> "float"
      Proto3.ScalarTypeInt32 -> "int32"
      Proto3.ScalarTypeInt64 -> "int64"
      Proto3.ScalarTypeSfixed32 -> "sfixed32"
      Proto3.ScalarTypeSfixed64 -> "sfixed64"
      Proto3.ScalarTypeSint32 -> "sint32"
      Proto3.ScalarTypeSint64 -> "sint64"
      Proto3.ScalarTypeString -> "string"
      Proto3.ScalarTypeUint32 -> "uint32"
      Proto3.ScalarTypeUint64 -> "uint64")

-- | Convert a simple type to an expression
writeSimpleType :: Proto3.SimpleType -> Ast.Expr
writeSimpleType st =
    case st of
      Proto3.SimpleTypeReference v0 -> Serialization.cst (Proto3.unTypeName v0)
      Proto3.SimpleTypeScalar v0 -> writeScalarType v0

-- | Convert a value to an expression
writeValue :: Proto3.Value -> Ast.Expr
writeValue v =
    Serialization.cst (case v of
      Proto3.ValueBoolean v0 -> Logic.ifElse v0 "true" "false"
      Proto3.ValueString v0 -> Literals.showString v0)
