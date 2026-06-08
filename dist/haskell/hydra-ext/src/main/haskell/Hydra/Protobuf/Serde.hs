-- Note: this is an automatically generated file. Do not edit.
-- | Serialization functions for converting Protocol Buffers v3 AST to abstract expressions

module Hydra.Protobuf.Serde where
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
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Protobuf.Proto3 as Proto3
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
-- | Convert a definition to an expression
definitionToExpr :: Proto3.Definition -> Ast.Expr
definitionToExpr def =
    case def of
      Proto3.DefinitionEnum v0 -> enumDefinitionToExpr v0
      Proto3.DefinitionMessage v0 -> messageDefinitionToExpr v0
-- | The name of the deprecated option
deprecatedOptionName :: String
deprecatedOptionName = "deprecated"
-- | A special Protobuf option name for descriptions (documentation)
descriptionOptionName :: String
descriptionOptionName = "_description"
-- | Convert an enum definition to an expression
enumDefinitionToExpr :: Proto3.EnumDefinition -> Ast.Expr
enumDefinitionToExpr ed =

      let name = Proto3.enumDefinitionName ed
          values = Proto3.enumDefinitionValues ed
          options = Proto3.enumDefinitionOptions ed
      in (optDesc False options (Serialization.spaceSep [
        Serialization.cst "enum",
        (Serialization.cst (Proto3.unTypeName name)),
        (protoBlock (Lists.map enumValueToExpr values))]))
-- | Convert an enum value to an expression
enumValueToExpr :: Proto3.EnumValue -> Ast.Expr
enumValueToExpr ev =

      let name = Proto3.enumValueName ev
          number = Proto3.enumValueNumber ev
          options = Proto3.enumValueOptions ev
      in (optDesc False options (semi (Serialization.spaceSep [
        Serialization.cst (Proto3.unEnumValueName name),
        (Serialization.cst "="),
        (Serialization.cst (Literals.showInt32 number))])))
-- | Filter out internal options (those whose names start with underscore)
excludeInternalOptions :: [Proto3.Option] -> [Proto3.Option]
excludeInternalOptions opts =
    Lists.filter (\opt -> Logic.not (Equality.equal (Optionals.fromOptional 0 (Strings.maybeCharAt 0 (Proto3.optionName opt))) 95)) opts
-- | Convert a field option to an expression
fieldOptionToExpr :: Proto3.Option -> Ast.Expr
fieldOptionToExpr opt =

      let name = Proto3.optionName opt
          value = Proto3.optionValue opt
      in (Serialization.spaceSep [
        Serialization.cst name,
        (Serialization.cst "="),
        (valueToExpr value)])
-- | Convert field options to an optional bracket-enclosed expression
fieldOptionsToExpr :: [Proto3.Option] -> Maybe Ast.Expr
fieldOptionsToExpr opts0 =

      let opts = excludeInternalOptions opts0
      in (Logic.ifElse (Lists.null opts) Nothing (Optionals.pure (Serialization.bracketList Serialization.inlineStyle (Lists.map fieldOptionToExpr opts))))
-- | Convert a field to an expression
fieldToExpr :: Proto3.Field -> Ast.Expr
fieldToExpr f =

      let name = Proto3.fieldName f
          typ = Proto3.fieldType f
          num = Proto3.fieldNumber f
          options = Proto3.fieldOptions f
      in (optDesc False options (case typ of
        Proto3.FieldTypeOneof v0 -> Serialization.spaceSep [
          Serialization.cst "oneof",
          (Serialization.cst (Proto3.unFieldName name)),
          (protoBlock (Lists.map fieldToExpr v0))]
        Proto3.FieldTypeMap v0 ->
          let kt = Proto3.mapTypeKeys v0
              vt = Proto3.mapTypeValues v0
          in (semi (Serialization.spaceSep (Optionals.cat [
            Optionals.pure (fieldTypeToExpr typ),
            (Optionals.pure (Serialization.cst (Proto3.unFieldName name))),
            (Optionals.pure (Serialization.cst "=")),
            (Optionals.pure (Serialization.cst (Literals.showInt32 num))),
            (fieldOptionsToExpr options)])))
        Proto3.FieldTypeRepeated _ -> semi (Serialization.spaceSep (Optionals.cat [
          Optionals.pure (fieldTypeToExpr typ),
          (Optionals.pure (Serialization.cst (Proto3.unFieldName name))),
          (Optionals.pure (Serialization.cst "=")),
          (Optionals.pure (Serialization.cst (Literals.showInt32 num))),
          (fieldOptionsToExpr options)]))
        Proto3.FieldTypeSimple _ -> semi (Serialization.spaceSep (Optionals.cat [
          Optionals.pure (fieldTypeToExpr typ),
          (Optionals.pure (Serialization.cst (Proto3.unFieldName name))),
          (Optionals.pure (Serialization.cst "=")),
          (Optionals.pure (Serialization.cst (Literals.showInt32 num))),
          (fieldOptionsToExpr options)]))))
-- | Convert a field type to an expression
fieldTypeToExpr :: Proto3.FieldType -> Ast.Expr
fieldTypeToExpr ftyp =
    case ftyp of
      Proto3.FieldTypeMap v0 ->
        let kt = Proto3.mapTypeKeys v0
            vt = Proto3.mapTypeValues v0
        in (Serialization.noSep [
          Serialization.cst "map",
          (Serialization.angleBracesList Serialization.inlineStyle [
            simpleTypeToExpr kt,
            (simpleTypeToExpr vt)])])
      Proto3.FieldTypeRepeated v0 -> Serialization.spaceSep [
        Serialization.cst "repeated",
        (simpleTypeToExpr v0)]
      Proto3.FieldTypeSimple v0 -> simpleTypeToExpr v0
      Proto3.FieldTypeOneof _ -> Serialization.cst "oneof"
-- | Convert a file-level option to an expression
fileOptionToExpr :: Proto3.Option -> Ast.Expr
fileOptionToExpr opt =

      let name = Proto3.optionName opt
          value = Proto3.optionValue opt
      in (semi (Serialization.spaceSep [
        Serialization.cst "option",
        (Serialization.cst name),
        (Serialization.cst "="),
        (valueToExpr value)]))
-- | Convert file-level options to an optional newline-separated expression
fileOptionsToExpr :: [Proto3.Option] -> Maybe Ast.Expr
fileOptionsToExpr opts0 =

      let opts = excludeInternalOptions opts0
      in (Logic.ifElse (Lists.null opts) Nothing (Optionals.pure (Serialization.newlineSep (Lists.map fileOptionToExpr opts))))
-- | Convert a file reference to an import expression
importToExpr :: Proto3.FileReference -> Ast.Expr
importToExpr ref =
    semi (Serialization.spaceSep [
      Serialization.cst "import",
      (Serialization.cst (Literals.showString (Proto3.unFileReference ref)))])
-- | Convert a message definition to an expression
messageDefinitionToExpr :: Proto3.MessageDefinition -> Ast.Expr
messageDefinitionToExpr md =

      let name = Proto3.messageDefinitionName md
          fields = Proto3.messageDefinitionFields md
          options = Proto3.messageDefinitionOptions md
      in (optDesc False options (Serialization.spaceSep [
        Serialization.cst "message",
        (Serialization.cst (Proto3.unTypeName name)),
        (protoBlock (Lists.map fieldToExpr fields))]))
-- | Prepend an optional description comment to an expression
optDesc :: Bool -> [Proto3.Option] -> Ast.Expr -> Ast.Expr
optDesc doubleNewline opts expr =

      let descs = Lists.filter (\opt -> Equality.equal (Proto3.optionName opt) "_description") opts
      in (Optionals.cases (Lists.maybeHead descs) expr (\firstDesc ->
        let descValue = Proto3.optionValue firstDesc
            descStr =
                    case descValue of
                      Proto3.ValueBoolean v0 -> Logic.ifElse v0 "true" "false"
                      Proto3.ValueString v0 -> v0
            commentLines =
                    Lists.map (\line -> Logic.ifElse (Equality.equal line "") "//" (Strings.cat2 "// " line)) (Strings.lines descStr)
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
-- | Convert a proto file to an expression
protoFileToExpr :: Proto3.ProtoFile -> Ast.Expr
protoFileToExpr pf =

      let pkg = Proto3.protoFilePackage pf
          imports = Proto3.protoFileImports pf
          defs = Proto3.protoFileTypes pf
          options = Proto3.protoFileOptions pf
          headerSec =
                  Optionals.pure (Serialization.newlineSep [
                    semi (Serialization.cst "syntax = \"proto3\""),
                    (semi (Serialization.spaceSep [
                      Serialization.cst "package",
                      (Serialization.cst (Proto3.unPackageName pkg))]))])
          importsSec =
                  Logic.ifElse (Lists.null imports) Nothing (Optionals.pure (Serialization.newlineSep (Lists.map importToExpr imports)))
          options1 = Lists.filter (\opt -> Logic.not (Equality.equal (Proto3.optionName opt) "_description")) options
          optionsSec = fileOptionsToExpr options1
          defsSec =
                  Logic.ifElse (Lists.null defs) Nothing (Optionals.pure (Serialization.doubleNewlineSep (Lists.map definitionToExpr defs)))
      in (optDesc True options (Serialization.doubleNewlineSep (Optionals.cat [
        headerSec,
        importsSec,
        optionsSec,
        defsSec])))
-- | Convert a scalar type to an expression
scalarTypeToExpr :: Proto3.ScalarType -> Ast.Expr
scalarTypeToExpr sct =
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
-- | Append a semicolon to an expression
semi :: Ast.Expr -> Ast.Expr
semi e =
    Serialization.noSep [
      e,
      (Serialization.cst ";")]
-- | Convert a simple type to an expression
simpleTypeToExpr :: Proto3.SimpleType -> Ast.Expr
simpleTypeToExpr st =
    case st of
      Proto3.SimpleTypeReference v0 -> Serialization.cst (Proto3.unTypeName v0)
      Proto3.SimpleTypeScalar v0 -> scalarTypeToExpr v0
-- | Convert a value to an expression
valueToExpr :: Proto3.Value -> Ast.Expr
valueToExpr v =
    Serialization.cst (case v of
      Proto3.ValueBoolean v0 -> Logic.ifElse v0 "true" "false"
      Proto3.ValueString v0 -> Literals.showString v0)
