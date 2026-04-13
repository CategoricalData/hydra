module Hydra.Sources.Protobuf.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
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
import qualified Hydra.Dsl.Packaging                     as Packaging
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
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
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
import qualified Hydra.Protobuf.Proto3 as P3
import qualified Hydra.Sources.Protobuf.Proto3 as Proto3Syntax


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.protobuf.serde"

module_ :: Module
module_ = Module ns definitions
    [Formatting.ns, Serialization.ns]
    (Proto3Syntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Serialization functions for converting Protocol Buffers v3 AST to abstract expressions"
  where
    definitions = [
      toDefinition deprecatedOptionName,
      toDefinition descriptionOptionName,
      toDefinition excludeInternalOptions,
      toDefinition optDesc,
      toDefinition protoBlock,
      toDefinition semi,
      toDefinition writeDefinition,
      toDefinition writeEnumDefinition,
      toDefinition writeEnumValue,
      toDefinition writeField,
      toDefinition writeFieldOption,
      toDefinition writeFieldOptions,
      toDefinition writeFieldType,
      toDefinition writeFileOption,
      toDefinition writeFileOptions,
      toDefinition writeImport,
      toDefinition writeMessageDefinition,
      toDefinition writeProtoFile,
      toDefinition writeScalarType,
      toDefinition writeSimpleType,
      toDefinition writeValue]


deprecatedOptionName :: TTermDefinition String
deprecatedOptionName = define "deprecatedOptionName" $
  doc "The name of the deprecated option" $
  string "deprecated"

descriptionOptionName :: TTermDefinition String
descriptionOptionName = define "descriptionOptionName" $
  doc "A special Protobuf option name for descriptions (documentation)" $
  string "_description"

excludeInternalOptions :: TTermDefinition ([P3.Option] -> [P3.Option])
excludeInternalOptions = define "excludeInternalOptions" $
  doc "Filter out internal options (those whose names start with underscore)" $
  lambda "opts" $
    Lists.filter
      (lambda "opt" $ Logic.not $
        Equality.equal
          (Strings.charAt (int32 0) (project P3._Option P3._Option_name @@ var "opt"))
          (int32 95))  -- 95 = '_'
      (var "opts")

optDesc :: TTermDefinition (Bool -> [P3.Option] -> Expr -> Expr)
optDesc = define "optDesc" $
  doc "Prepend an optional description comment to an expression" $
  lambda "doubleNewline" $ lambda "opts" $ lambda "expr" $ lets [
    "descs">: Lists.filter
      (lambda "opt" $ Equality.equal (project P3._Option P3._Option_name @@ var "opt") (string "_description"))
      (var "opts")] $
    Logic.ifElse (Lists.null (var "descs"))
      (var "expr")
      (lets [
        "descValue">: project P3._Option P3._Option_value @@ (Lists.head (var "descs")),
        "descStr">: cases P3._Value (var "descValue") Nothing [
          P3._Value_boolean>>: lambda "b" $ Logic.ifElse (var "b") (string "true") (string "false"),
          P3._Value_string>>: lambda "s" $ var "s"],
        "commentLines">: Lists.map
          (lambda "line" $ Strings.cat2 (string "// ") (var "line"))
          (Strings.lines (var "descStr")),
        "comment">: Serialization.cst @@ (Strings.intercalate (string "\n") (var "commentLines")),
        "sep">: Logic.ifElse (var "doubleNewline")
          (Serialization.doubleNewlineSep @@ list [var "comment", var "expr"])
          (Serialization.newlineSep @@ list [var "comment", var "expr"])] $
        var "sep")

protoBlock :: TTermDefinition ([Expr] -> Expr)
protoBlock = define "protoBlock" $
  doc "Wrap expressions in a curly-braced block with double-newline separation" $
  lambda "exprs" $
    Serialization.brackets @@ Serialization.curlyBraces @@ Serialization.fullBlockStyle @@
      (Serialization.doubleNewlineSep @@ var "exprs")

semi :: TTermDefinition (Expr -> Expr)
semi = define "semi" $
  doc "Append a semicolon to an expression" $
  lambda "e" $ Serialization.noSep @@ list [var "e", Serialization.cst @@ string ";"]

writeDefinition :: TTermDefinition (P3.Definition -> Expr)
writeDefinition = define "writeDefinition" $
  doc "Convert a definition to an expression" $
  lambda "def" $
    cases P3._Definition (var "def") Nothing [
      P3._Definition_enum>>: lambda "e" $ writeEnumDefinition @@ var "e",
      P3._Definition_message>>: lambda "m" $ writeMessageDefinition @@ var "m"]

writeEnumDefinition :: TTermDefinition (P3.EnumDefinition -> Expr)
writeEnumDefinition = define "writeEnumDefinition" $
  doc "Convert an enum definition to an expression" $
  lambda "ed" $ lets [
    "name">: project P3._EnumDefinition P3._EnumDefinition_name @@ var "ed",
    "values">: project P3._EnumDefinition P3._EnumDefinition_values @@ var "ed",
    "options">: project P3._EnumDefinition P3._EnumDefinition_options @@ var "ed"] $
    optDesc @@ false @@ var "options" @@
      (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "enum",
        Serialization.cst @@ (unwrap P3._TypeName @@ var "name"),
        protoBlock @@ (Lists.map writeEnumValue (var "values"))])

writeEnumValue :: TTermDefinition (P3.EnumValue -> Expr)
writeEnumValue = define "writeEnumValue" $
  doc "Convert an enum value to an expression" $
  lambda "ev" $ lets [
    "name">: project P3._EnumValue P3._EnumValue_name @@ var "ev",
    "number">: project P3._EnumValue P3._EnumValue_number @@ var "ev",
    "options">: project P3._EnumValue P3._EnumValue_options @@ var "ev"] $
    optDesc @@ false @@ var "options" @@
      (semi @@ (Serialization.spaceSep @@ list [
        Serialization.cst @@ (unwrap P3._EnumValueName @@ var "name"),
        Serialization.cst @@ string "=",
        Serialization.cst @@ (Literals.showInt32 (var "number"))]))

writeField :: TTermDefinition (P3.Field -> Expr)
writeField = define "writeField" $
  doc "Convert a field to an expression" $
  lambda "f" $ lets [
    "name">: project P3._Field P3._Field_name @@ var "f",
    "typ">: project P3._Field P3._Field_type @@ var "f",
    "num">: project P3._Field P3._Field_number @@ var "f",
    "options">: project P3._Field P3._Field_options @@ var "f"] $
    optDesc @@ false @@ var "options" @@
      (cases P3._FieldType (var "typ") Nothing [
        P3._FieldType_oneof>>: lambda "fields" $
          Serialization.spaceSep @@ list [
            Serialization.cst @@ string "oneof",
            Serialization.cst @@ (unwrap P3._FieldName @@ var "name"),
            protoBlock @@ (Lists.map writeField (var "fields"))],
        P3._FieldType_map>>: lambda "mt" $ lets [
          "kt">: project P3._MapType P3._MapType_keys @@ var "mt",
          "vt">: project P3._MapType P3._MapType_values @@ var "mt"] $
          semi @@ (Serialization.spaceSep @@ (Maybes.cat $ list [
            Maybes.pure (writeFieldType @@ var "typ"),
            Maybes.pure (Serialization.cst @@ (unwrap P3._FieldName @@ var "name")),
            Maybes.pure (Serialization.cst @@ string "="),
            Maybes.pure (Serialization.cst @@ (Literals.showInt32 (var "num"))),
            writeFieldOptions @@ var "options"])),
        P3._FieldType_repeated>>: lambda "st" $
          semi @@ (Serialization.spaceSep @@ (Maybes.cat $ list [
            Maybes.pure (writeFieldType @@ var "typ"),
            Maybes.pure (Serialization.cst @@ (unwrap P3._FieldName @@ var "name")),
            Maybes.pure (Serialization.cst @@ string "="),
            Maybes.pure (Serialization.cst @@ (Literals.showInt32 (var "num"))),
            writeFieldOptions @@ var "options"])),
        P3._FieldType_simple>>: lambda "st" $
          semi @@ (Serialization.spaceSep @@ (Maybes.cat $ list [
            Maybes.pure (writeFieldType @@ var "typ"),
            Maybes.pure (Serialization.cst @@ (unwrap P3._FieldName @@ var "name")),
            Maybes.pure (Serialization.cst @@ string "="),
            Maybes.pure (Serialization.cst @@ (Literals.showInt32 (var "num"))),
            writeFieldOptions @@ var "options"]))])

writeFieldOption :: TTermDefinition (P3.Option -> Expr)
writeFieldOption = define "writeFieldOption" $
  doc "Convert a field option to an expression" $
  lambda "opt" $ lets [
    "name">: project P3._Option P3._Option_name @@ var "opt",
    "value">: project P3._Option P3._Option_value @@ var "opt"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ var "name",
      Serialization.cst @@ string "=",
      writeValue @@ var "value"]

writeFieldOptions :: TTermDefinition ([P3.Option] -> Maybe Expr)
writeFieldOptions = define "writeFieldOptions" $
  doc "Convert field options to an optional bracket-enclosed expression" $
  lambda "opts0" $ lets [
    "opts">: excludeInternalOptions @@ var "opts0"] $
    Logic.ifElse (Lists.null (var "opts"))
      nothing
      (Maybes.pure (Serialization.bracketList @@ Serialization.inlineStyle @@
        (Lists.map writeFieldOption (var "opts"))))

writeFieldType :: TTermDefinition (P3.FieldType -> Expr)
writeFieldType = define "writeFieldType" $
  doc "Convert a field type to an expression" $
  lambda "ftyp" $
    cases P3._FieldType (var "ftyp") Nothing [
      P3._FieldType_map>>: lambda "mt" $ lets [
        "kt">: project P3._MapType P3._MapType_keys @@ var "mt",
        "vt">: project P3._MapType P3._MapType_values @@ var "mt"] $
        Serialization.noSep @@ list [
          Serialization.cst @@ string "map",
          Serialization.angleBracesList @@ Serialization.inlineStyle @@ list [
            writeSimpleType @@ var "kt",
            writeSimpleType @@ var "vt"]],
      P3._FieldType_repeated>>: lambda "st" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "repeated",
          writeSimpleType @@ var "st"],
      P3._FieldType_simple>>: lambda "st" $ writeSimpleType @@ var "st",
      P3._FieldType_oneof>>: lambda "fields" $
        Serialization.cst @@ string "oneof"]

writeFileOption :: TTermDefinition (P3.Option -> Expr)
writeFileOption = define "writeFileOption" $
  doc "Convert a file-level option to an expression" $
  lambda "opt" $ lets [
    "name">: project P3._Option P3._Option_name @@ var "opt",
    "value">: project P3._Option P3._Option_value @@ var "opt"] $
    semi @@ (Serialization.spaceSep @@ list [
      Serialization.cst @@ string "option",
      Serialization.cst @@ var "name",
      Serialization.cst @@ string "=",
      writeValue @@ var "value"])

writeFileOptions :: TTermDefinition ([P3.Option] -> Maybe Expr)
writeFileOptions = define "writeFileOptions" $
  doc "Convert file-level options to an optional newline-separated expression" $
  lambda "opts0" $ lets [
    "opts">: excludeInternalOptions @@ var "opts0"] $
    Logic.ifElse (Lists.null (var "opts"))
      nothing
      (Maybes.pure (Serialization.newlineSep @@ (Lists.map writeFileOption (var "opts"))))

writeImport :: TTermDefinition (P3.FileReference -> Expr)
writeImport = define "writeImport" $
  doc "Convert a file reference to an import expression" $
  lambda "ref" $
    semi @@ (Serialization.spaceSep @@ list [
      Serialization.cst @@ string "import",
      Serialization.cst @@ (Literals.showString (unwrap P3._FileReference @@ var "ref"))])

writeMessageDefinition :: TTermDefinition (P3.MessageDefinition -> Expr)
writeMessageDefinition = define "writeMessageDefinition" $
  doc "Convert a message definition to an expression" $
  lambda "md" $ lets [
    "name">: project P3._MessageDefinition P3._MessageDefinition_name @@ var "md",
    "fields">: project P3._MessageDefinition P3._MessageDefinition_fields @@ var "md",
    "options">: project P3._MessageDefinition P3._MessageDefinition_options @@ var "md"] $
    optDesc @@ false @@ var "options" @@
      (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "message",
        Serialization.cst @@ (unwrap P3._TypeName @@ var "name"),
        protoBlock @@ (Lists.map writeField (var "fields"))])

writeProtoFile :: TTermDefinition (P3.ProtoFile -> Expr)
writeProtoFile = define "writeProtoFile" $
  doc "Convert a proto file to an expression" $
  lambda "pf" $ lets [
    "pkg">: project P3._ProtoFile P3._ProtoFile_package @@ var "pf",
    "imports">: project P3._ProtoFile P3._ProtoFile_imports @@ var "pf",
    "defs">: project P3._ProtoFile P3._ProtoFile_types @@ var "pf",
    "options">: project P3._ProtoFile P3._ProtoFile_options @@ var "pf",
    "headerSec">: Maybes.pure (Serialization.newlineSep @@ list [
      semi @@ (Serialization.cst @@ string "syntax = \"proto3\""),
      semi @@ (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "package",
        Serialization.cst @@ (unwrap P3._PackageName @@ var "pkg")])]),
    "importsSec">: Logic.ifElse (Lists.null (var "imports"))
      nothing
      (Maybes.pure (Serialization.newlineSep @@ (Lists.map writeImport (var "imports")))),
    "options1">: Lists.filter
      (lambda "opt" $ Logic.not $ Equality.equal (project P3._Option P3._Option_name @@ var "opt") (string "_description"))
      (var "options"),
    "optionsSec">: writeFileOptions @@ var "options1",
    "defsSec">: Logic.ifElse (Lists.null (var "defs"))
      nothing
      (Maybes.pure (Serialization.doubleNewlineSep @@ (Lists.map writeDefinition (var "defs"))))] $
    optDesc @@ true @@ var "options" @@
      (Serialization.doubleNewlineSep @@ (Maybes.cat $ list [
        var "headerSec", var "importsSec", var "optionsSec", var "defsSec"]))

writeScalarType :: TTermDefinition (P3.ScalarType -> Expr)
writeScalarType = define "writeScalarType" $
  doc "Convert a scalar type to an expression" $
  lambda "sct" $ Serialization.cst @@
    (cases P3._ScalarType (var "sct") Nothing [
      P3._ScalarType_bool>>: constant $ string "bool",
      P3._ScalarType_bytes>>: constant $ string "bytes",
      P3._ScalarType_double>>: constant $ string "double",
      P3._ScalarType_fixed32>>: constant $ string "fixed32",
      P3._ScalarType_fixed64>>: constant $ string "fixed64",
      P3._ScalarType_float>>: constant $ string "float",
      P3._ScalarType_int32>>: constant $ string "int32",
      P3._ScalarType_int64>>: constant $ string "int64",
      P3._ScalarType_sfixed32>>: constant $ string "sfixed32",
      P3._ScalarType_sfixed64>>: constant $ string "sfixed64",
      P3._ScalarType_sint32>>: constant $ string "sint32",
      P3._ScalarType_sint64>>: constant $ string "sint64",
      P3._ScalarType_string>>: constant $ string "string",
      P3._ScalarType_uint32>>: constant $ string "uint32",
      P3._ScalarType_uint64>>: constant $ string "uint64"])

writeSimpleType :: TTermDefinition (P3.SimpleType -> Expr)
writeSimpleType = define "writeSimpleType" $
  doc "Convert a simple type to an expression" $
  lambda "st" $
    cases P3._SimpleType (var "st") Nothing [
      P3._SimpleType_reference>>: lambda "name" $
        Serialization.cst @@ (unwrap P3._TypeName @@ var "name"),
      P3._SimpleType_scalar>>: lambda "sct" $ writeScalarType @@ var "sct"]

writeValue :: TTermDefinition (P3.Value -> Expr)
writeValue = define "writeValue" $
  doc "Convert a value to an expression" $
  lambda "v" $ Serialization.cst @@
    (cases P3._Value (var "v") Nothing [
      P3._Value_boolean>>: lambda "b" $ Logic.ifElse (var "b") (string "true") (string "false"),
      P3._Value_string>>: lambda "s" $ Literals.showString (var "s")])
