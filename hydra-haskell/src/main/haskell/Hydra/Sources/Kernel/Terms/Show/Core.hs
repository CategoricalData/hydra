
module Hydra.Sources.Kernel.Terms.Show.Core where

-- Standard imports for kernel terms modules (slightly modified for conflict avoidance)
import Hydra.Kernel hiding (literalType)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors    as Accessors
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Meta.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Meta.Coders       as Coders
import qualified Hydra.Dsl.Meta.Compute      as Compute
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Grammar      as Grammar
import qualified Hydra.Dsl.Grammars          as Grammars
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Meta.Json         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows    as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Meta.Module       as Module
import qualified Hydra.Dsl.Meta.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms hiding (
  binding, elimination, field, fields, fieldType, floatType, floatValue, function, injection, integerType,
  integerValue, lambda, literal, literalType, term, type_, typeScheme)
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Meta.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Meta.Typing       as Typing
import qualified Hydra.Dsl.Meta.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: Namespace
ns = Namespace "hydra.show.core"

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces $
    Just "String representations of hydra.core types"
  where
   elements = [
     toBinding readTerm, -- TODO: move this to hydra.read.core
     toBinding binding,
     toBinding elimination,
     toBinding field,
     toBinding fieldType,
     toBinding fields,
     toBinding floatValue,
     toBinding floatType,
     toBinding function,
     toBinding injection,
     toBinding integerValue,
     toBinding integerType,
     toBinding lambda,
     toBinding let_,
     toBinding list_,
     toBinding literal,
     toBinding literalType,
     toBinding term,
     toBinding type_,
     toBinding typeScheme]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

readTerm :: TBinding (String -> Maybe Term)
readTerm = define "readTerm" $
  doc "A placeholder for reading terms from their serialized form. Not implemented." $
  "s" ~> just $ Core.termLiteral $ Core.literalString $ var "s"

binding :: TBinding (Binding -> String)
binding = define "binding" $
  doc "Show a binding as a string" $
  "el" ~>
  "name" <~ unwrap _Name @@ (Core.bindingName $ var "el") $
  "t" <~ Core.bindingTerm (var "el") $
  "typeStr" <~ Maybes.maybe
    (string "")
    ("ts" ~> Strings.concat [string ":(", typeScheme @@ var "ts", string ")"])
    (Core.bindingType $ var "el") $
  Strings.cat $ list [
    var "name",
    var "typeStr",
    string " = ",
    term @@ var "t"]
      
elimination :: TBinding (Elimination -> String)
elimination = define "elimination" $
  doc "Show an elimination as a string" $
  "elm" ~>
  cases _Elimination (var "elm") Nothing [
    _Elimination_record>>: "proj" ~>
      "tname" <~ unwrap _Name @@ (Core.projectionTypeName $ var "proj") $
      "fname" <~ unwrap _Name @@ (Core.projectionField $ var "proj") $
      Strings.cat $ list [
        string "project(",
        var "tname",
        string "){",
        var "fname",
        string "}"],
    _Elimination_union>>: "cs" ~>
      "tname" <~ unwrap _Name @@ (Core.caseStatementTypeName $ var "cs") $
      "mdef" <~ Core.caseStatementDefault (var "cs") $
      "cases" <~ Core.caseStatementCases (var "cs") $
      "defaultField" <~ Maybes.maybe
        (list ([] :: [TTerm Field]))
        ("d" ~> list [Core.field (Core.name $ string "[default]") (var "d")])
        (var "mdef") $
      "allFields" <~ Lists.concat (list [var "cases", var "defaultField"]) $
      Strings.cat $ list [
        string "case(",
        var "tname",
        string ")",
        fields @@ var "allFields"],
    _Elimination_wrap>>: "tname" ~> Strings.cat $ list [
      string "unwrap(",
      unwrap _Name @@ var "tname",
      string ")"]]

field :: TBinding (Field -> String)
field = define "field" $
  "field" ~>
  "fname" <~ unwrap _Name @@ (Core.fieldName $ var "field") $
  "fterm" <~ Core.fieldTerm (var "field") $
  Strings.cat $ list [var "fname", string "=", term @@ var "fterm"]

fieldType :: TBinding (FieldType -> String)
fieldType = define "fieldType" $
  "ft" ~>
  "fname" <~ unwrap _Name @@ (Core.fieldTypeName $ var "ft") $
  "ftyp" <~ Core.fieldTypeType (var "ft") $
  Strings.cat $ list [
    var "fname",
    string ":",
    type_ @@ var "ftyp"]

fields :: TBinding ([Field] -> String)
fields = define "fields" $
  doc "Show a list of fields as a string" $
  "flds" ~>
  "fieldStrs" <~ Lists.map field (var "flds") $
  Strings.cat $ list [
    string "{",
    Strings.intercalate (string ", ") (var "fieldStrs"),
    string "}"]

floatValue :: TBinding (FloatValue -> String)
floatValue = define "float" $
  doc "Show a float value as a string" $
  "fv" ~> cases _FloatValue (var "fv") Nothing [
    _FloatValue_bigfloat>>: "v" ~> Literals.showBigfloat (var "v") ++ (string ":bigfloat"),
    _FloatValue_float32>>: "v" ~> Literals.showFloat32 (var "v") ++ (string ":float32"),
    _FloatValue_float64>>: "v" ~> Literals.showFloat64 (var "v") ++ (string ":float64")]

floatType :: TBinding (FloatType -> String)
floatType = define "floatType" $
  doc "Show a float type as a string" $
  "ft" ~> cases _FloatType (var "ft") Nothing [
    _FloatType_bigfloat>>: constant $ string "bigfloat",
    _FloatType_float32>>: constant $ string "float32",
    _FloatType_float64>>: constant $ string "float64"]

function :: TBinding (Function -> String)
function = define "function" $
  doc "Show a function as a string" $
  "f" ~> cases _Function (var "f") Nothing [
    _Function_elimination>>: elimination,
    _Function_lambda>>: lambda,
    _Function_primitive>>: "name" ~> Strings.cat2 (unwrap _Name @@ var "name") (string "!")]

injection :: TBinding (Injection -> String)
injection = define "injection" $
  doc "Show an injection as a string" $
  "inj" ~>
  "tname" <~ Core.injectionTypeName (var "inj") $
  "f" <~ Core.injectionField (var "inj") $
  Strings.cat $ list [
    string "inject(",
    unwrap _Name @@ var "tname",
    string ")",
    fields @@ (list [var "f"])]

integerValue :: TBinding (IntegerValue -> String)
integerValue = define "integer" $
  doc "Show an integer value as a string" $
  "iv" ~> cases _IntegerValue (var "iv") Nothing [
    _IntegerValue_bigint>>: "v" ~> Literals.showBigint (var "v") ++ (string ":bigint"),
    _IntegerValue_int8>>: "v" ~> Literals.showInt8 (var "v") ++ (string ":int8"),
    _IntegerValue_int16>>: "v" ~> Literals.showInt16 (var "v") ++ (string ":int16"),
    _IntegerValue_int32>>: "v" ~> Literals.showInt32 (var "v") ++ (string ":int32"),
    _IntegerValue_int64>>: "v" ~> Literals.showInt64 (var "v") ++ (string ":int64"),
    _IntegerValue_uint8>>: "v" ~> Literals.showUint8 (var "v") ++ (string ":uint8"),
    _IntegerValue_uint16>>: "v" ~> Literals.showUint16 (var "v") ++ (string ":uint16"),
    _IntegerValue_uint32>>: "v" ~> Literals.showUint32 (var "v") ++ (string ":uint32"),
    _IntegerValue_uint64>>: "v" ~> Literals.showUint64 (var "v") ++ (string ":uint64")]

integerType :: TBinding (IntegerType -> String)
integerType = define "integerType" $
  doc "Show an integer type as a string" $
  "it" ~> cases _IntegerType (var "it") Nothing [
    _IntegerType_bigint>>: constant $ string "bigint",
    _IntegerType_int8>>: constant $ string "int8",
    _IntegerType_int16>>: constant $ string "int16",
    _IntegerType_int32>>: constant $ string "int32",
    _IntegerType_int64>>: constant $ string "int64",
    _IntegerType_uint8>>: constant $ string "uint8",
    _IntegerType_uint16>>: constant $ string "uint16",
    _IntegerType_uint32>>: constant $ string "uint32",
    _IntegerType_uint64>>: constant $ string "uint64"]

lambda :: TBinding (Lambda -> String)
lambda = define "lambda" $
  doc "Show a lambda as a string" $
  "l" ~>
  "v" <~ unwrap _Name @@ (Core.lambdaParameter $ var "l") $
  "mt" <~ Core.lambdaDomain (var "l") $
  "body" <~ Core.lambdaBody (var "l") $
  "typeStr" <~ Maybes.maybe
    (string "")
    ("t" ~> Strings.cat2 (string ":") (type_ @@ var "t"))
    (var "mt") $
  Strings.cat $ list [
    string "λ",
    var "v",
    var "typeStr",
    string ".",
    term @@ var "body"]

let_ :: TBinding (Let -> String)
let_ = define "let" $
  doc "Show a let expression as a string" $
  "l" ~>
  "bindings" <~ Core.letBindings (var "l") $
  "env" <~ Core.letBody (var "l") $
  "bindingStrs" <~ Lists.map binding (var "bindings") $
  Strings.cat $ list [
    string "let ",
    Strings.intercalate (string ", ") (var "bindingStrs"),
    string " in ",
    term @@ var "env"]

list_ :: TBinding ((a -> String) -> [a] -> String)
list_ = define "list" $
  doc "Show a list using a given function to show each element" $
  "f" ~> "xs" ~>
  "elementStrs" <~ Lists.map (var "f") (var "xs") $
  Strings.cat $ list [
    string "[",
    Strings.intercalate (string ", ") (var "elementStrs"),
    string "]"]

literal :: TBinding (Literal -> String)
literal = define "literal" $
  doc "Show a literal as a string" $
  "l" ~> cases _Literal (var "l") Nothing [
    _Literal_binary>>: constant $ string "[binary]",
    _Literal_boolean>>: "b" ~> Logic.ifElse (var "b") (string "true") (string "false"),
    _Literal_float>>: "fv" ~> floatValue @@ var "fv",
    _Literal_integer>>: "iv" ~> integerValue @@ var "iv",
    _Literal_string>>: "s" ~> Literals.showString $ var "s"]

literalType :: TBinding (LiteralType -> String)
literalType = define "literalType" $
  doc "Show a literal type as a string" $
  "lt" ~> cases _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant $ string "binary",
    _LiteralType_boolean>>: constant $ string "boolean",
    _LiteralType_float>>: "ft" ~> floatType @@ var "ft",
    _LiteralType_integer>>: "it" ~> integerType @@ var "it",
    _LiteralType_string>>: constant $ string "string"]

term :: TBinding (Term -> String)
term = define "term" $
  doc "Show a term as a string" $
  "t" ~>
  "gatherTerms" <~ ("prev" ~> "app" ~>
    "lhs" <~ Core.applicationFunction (var "app") $
    "rhs" <~ Core.applicationArgument (var "app") $
    cases _Term (var "lhs")
      (Just $ Lists.cons (var "lhs") (Lists.cons (var "rhs") (var "prev"))) [
      _Term_application>>: "app2" ~> var "gatherTerms" @@ (Lists.cons (var "rhs") (var "prev")) @@ var "app2"]) $
  cases _Term (var "t") Nothing [
    _Term_annotated>>: "at" ~> term @@ (Core.annotatedTermBody $ var "at"),
    _Term_application>>: "app" ~>
      "terms" <~ var "gatherTerms" @@ (list ([] :: [TTerm Term])) @@ var "app" $
      "termStrs" <~ Lists.map term (var "terms") $
      Strings.cat $ list [
        string "(",
        Strings.intercalate (string " @ ") (var "termStrs"),
        string ")"],
    _Term_either>>: "e" ~> Eithers.either_
      ("l" ~> Strings.cat $ list [
        string "left(",
        term @@ var "l",
        string ")"])
      ("r" ~> Strings.cat $ list [
        string "right(",
        term @@ var "r",
        string ")"])
      (var "e"),
    _Term_function>>: function,
    _Term_let>>: "l" ~> let_ @@ var "l",
    _Term_list>>: "els" ~>
      "termStrs" <~ Lists.map term (var "els") $
      Strings.cat $ list [
        string "[",
        Strings.intercalate (string ", ") (var "termStrs"),
        string "]"],
    _Term_literal>>: "lit" ~> literal @@ var "lit",
    _Term_map>>: "m" ~>
      "entry" <~ ("p" ~> Strings.cat $ list [
        term @@ (Pairs.first $ var "p"),
        string "=",
        term @@ (Pairs.second $ var "p")]) $
      Strings.cat $ list [
        string "{",
        Strings.intercalate (string ", ") $ Lists.map (var "entry") $ Maps.toList $ var "m",
        string "}"],
    _Term_maybe>>: "mt" ~> Maybes.maybe
      (string "nothing")
      ("t" ~> Strings.cat $ list [
        string "just(",
        term @@ var "t",
        string ")"])
      (var "mt"),
    _Term_pair>>: "p" ~> Strings.cat $ list [
      string "(",
      term @@ (Pairs.first $ var "p"),
      string ", ",
      term @@ (Pairs.second $ var "p"),
      string ")"],
    _Term_record>>: "rec" ~>
      "tname" <~ unwrap _Name @@ (Core.recordTypeName $ var "rec") $
      "flds" <~ Core.recordFields (var "rec") $
      Strings.cat $ list [
        string "record(",
        var "tname",
        string ")",
        fields @@ var "flds"],
    _Term_set>>: "s" ~>
      Strings.cat $ list [
        string "{",
        Strings.intercalate (string ", ") (Lists.map term $ Sets.toList $ var "s"),
        string "}"],
    _Term_typeLambda>>: "ta" ~>
      "param" <~ unwrap _Name @@ (Core.typeLambdaParameter $ var "ta") $
      "body" <~ Core.typeLambdaBody (var "ta") $
      Strings.cat $ list [
        string "Λ",
        var "param",
        string ".",
        term @@ var "body"],
    _Term_typeApplication>>: "tt" ~>
      "t2" <~ Core.typeApplicationTermBody (var "tt") $
      "typ" <~ Core.typeApplicationTermType (var "tt") $
      Strings.cat $ list [
        term @@ var "t2",
        string "⟨",
        type_ @@ var "typ",
        string "⟩"],
    _Term_union>>: injection,
    _Term_unit>>: constant $ string "unit",
    _Term_variable>>: "name" ~> unwrap _Name @@ var "name",
    _Term_wrap>>: "wt" ~>
      "tname" <~ unwrap _Name @@ (Core.wrappedTermTypeName $ var "wt") $
      "term1" <~ Core.wrappedTermBody (var "wt") $
      Strings.cat $ list [
        string "wrap(",
        var "tname",
        string "){",
        term @@ var "term1",
        string "}"]]

type_ :: TBinding (Type -> String)
type_ = define "type" $
  doc "Show a type as a string" $
  "typ" ~>
  "showRowType" <~ ("rt" ~>
    "flds" <~ Core.rowTypeFields (var "rt") $
    "fieldStrs" <~ Lists.map fieldType (var "flds") $
    Strings.cat $ list [
      string "{",
      Strings.intercalate (string ", ") (var "fieldStrs"),
      string "}"]) $
  "gatherTypes" <~ ("prev" ~> "app" ~>
    "lhs" <~ Core.applicationTypeFunction (var "app") $
    "rhs" <~ Core.applicationTypeArgument (var "app") $
    cases _Type (var "lhs")
      (Just $ Lists.cons (var "lhs") (Lists.cons (var "rhs") (var "prev"))) [
      _Type_application>>: "app2" ~> var "gatherTypes" @@ (Lists.cons (var "rhs") (var "prev")) @@ var "app2"]) $
  "gatherFunctionTypes" <~ ("prev" ~> "t" ~>
    cases _Type (var "t")
      (Just $ Lists.reverse $ Lists.cons (var "t") (var "prev")) [
        _Type_function>>: "ft" ~>
          "dom" <~ Core.functionTypeDomain (var "ft") $
          "cod" <~ Core.functionTypeCodomain (var "ft") $
          var "gatherFunctionTypes" @@ (Lists.cons (var "dom") (var "prev")) @@ var "cod"]) $
  cases _Type (var "typ") Nothing [
    _Type_annotated>>: "at" ~> type_ @@ (Core.annotatedTypeBody $ var "at"),
    _Type_application>>: "app" ~>
      "types" <~ var "gatherTypes" @@ (list ([] :: [TTerm Type])) @@ var "app" $
      "typeStrs" <~ Lists.map type_ (var "types") $
      Strings.cat $ list [
        string "(",
        Strings.intercalate (string " @ ") (var "typeStrs"),
        string ")"],
    _Type_either>>: "et" ~>
      "leftTyp" <~ Core.eitherTypeLeft (var "et") $
      "rightTyp" <~ Core.eitherTypeRight (var "et") $
      Strings.cat $ list [
        string "either<",
        type_ @@ var "leftTyp",
        string ", ",
        type_ @@ var "rightTyp",
        string ">"],
    _Type_forall>>: "ft" ~>
      "var" <~ unwrap _Name @@ (Core.forallTypeParameter $ var "ft") $
      "body" <~ Core.forallTypeBody (var "ft") $
      Strings.cat $ list [
        string "(∀",
        var "var",
        string ".",
        type_ @@ var "body",
        string ")"],
    _Type_function>>: "ft" ~>
      "types" <~ var "gatherFunctionTypes" @@ (list ([] :: [TTerm Type])) @@ var "typ" $
      "typeStrs" <~ Lists.map type_ (var "types") $
      Strings.cat $ list [
        string "(",
        Strings.intercalate (string " → ") (var "typeStrs"),
        string ")"],
    _Type_list>>: "etyp" ~> Strings.cat $ list [
      string "list<",
      type_ @@ var "etyp",
      string ">"],
    _Type_literal>>: "lt" ~> literalType @@ var "lt",
    _Type_map>>: "mt" ~>
      "keyTyp" <~ Core.mapTypeKeys (var "mt") $
      "valTyp" <~ Core.mapTypeValues (var "mt") $
      Strings.cat $ list [
        string "map<",
        type_ @@ var "keyTyp",
        string ", ",
        type_ @@ var "valTyp",
        string ">"],
    _Type_maybe>>: "etyp" ~> Strings.cat $ list [
      string "maybe<",
      type_ @@ var "etyp",
      string ">"],
    _Type_pair>>: "pt" ~>
      "firstTyp" <~ Core.pairTypeFirst (var "pt") $
      "secondTyp" <~ Core.pairTypeSecond (var "pt") $
      Strings.cat $ list [
        string "(",
        type_ @@ var "firstTyp",
        string ", ",
        type_ @@ var "secondTyp",
        string ")"],
    _Type_record>>: "rt" ~> Strings.cat2 (string "record") (var "showRowType" @@ var "rt"),
    _Type_set>>: "etyp" ~> Strings.cat $ list [
      string "set<",
      type_ @@ var "etyp",
      string ">"],
    _Type_union>>: "rt" ~> Strings.cat2 (string "union") (var "showRowType" @@ var "rt"),
    _Type_unit>>: constant $ string "unit",
    _Type_variable>>: "name" ~> unwrap _Name @@ var "name",
    _Type_wrap>>: "wt" ~>
      "tname" <~ unwrap _Name @@ (Core.wrappedTypeTypeName $ var "wt") $
      "typ1" <~ Core.wrappedTypeBody (var "wt") $
      Strings.cat $ list [string "wrap[", var "tname", string "](", type_ @@ var "typ1", string ")"]]

typeScheme :: TBinding (TypeScheme -> String)
typeScheme = define "typeScheme" $
  doc "Show a type scheme as a string" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "body" <~ Core.typeSchemeType (var "ts") $
  "varNames" <~ Lists.map (unwrap _Name) (var "vars") $
  "fa" <~ Logic.ifElse (Lists.null $ var "vars")
    (string "")
    (Strings.cat $ list [
      string "∀[",
      Strings.intercalate (string ",") (var "varNames"),
      string "]."]) $
  Strings.cat $ list [
    string "(",
    var "fa",
    type_ @@ var "body",
    string ")"]
