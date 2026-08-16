{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Terms.Print.Core where

-- Standard imports for kernel terms modules (slightly modified for conflict avoidance)
import Hydra.Kernel hiding (literalType)
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms hiding (
  binding, field, fields, fieldType, floatType, floatValue, injection, integerType,
  integerValue, lambda, literal, literalType, project, term, type_, typeScheme)
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: ModuleName
ns = ModuleName "hydra.print.core"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> (kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "String representations of hydra.core types")}
  where
   definitions = [
     toDefinition binding,
     toDefinition caseStatement,
     toDefinition either_,
     toDefinition field,
     toDefinition fieldType,
     toDefinition fields,
     toDefinition floatValue,
     toDefinition floatType,
     toDefinition injection,
     toDefinition integerValue,
     toDefinition integerType,
     toDefinition lambda,
     toDefinition let_,
     toDefinition list_,
     toDefinition literal,
     toDefinition literalType,
     toDefinition (map_ :: TypedTermDefinition ((Int -> String) -> (v -> String) -> M.Map Int v -> String)),
     toDefinition optional_,
     toDefinition pair_,
     toDefinition projection,
     toDefinition readTerm,
     toDefinition (set_ :: TypedTermDefinition ((Int -> String) -> S.Set Int -> String)),
     toDefinition term,
     toDefinition type_,
     toDefinition typeScheme]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

binding :: TypedTermDefinition (Binding -> String)
binding = define "binding" $
  doc "Show a binding as a string" $
  "el" ~>
  "name" <~ unwrap _Name @@ (Core.bindingName $ var "el") $
  "t" <~ Core.bindingTerm (var "el") $
  "typeStr" <~ Optionals.cases (Core.bindingTypeScheme $ var "el") (string "") ("ts" ~> Strings.concat (list [string ":(", typeScheme @@ var "ts", string ")"])) $
  Strings.concat $ list [
    var "name",
    var "typeStr",
    string " = ",
    term @@ var "t"]

caseStatement :: TypedTermDefinition (CaseStatement -> String)
caseStatement = define "caseStatement" $
  doc "Show a case statement as a string" $
  "cs" ~>
  "tname" <~ unwrap _Name @@ (Core.caseStatementTypeName $ var "cs") $
  "mdef" <~ Core.caseStatementDefault (var "cs") $
  "csCases" <~ Core.caseStatementCases (var "cs") $
  "caseFields" <~ Lists.map
    ("alt" ~> Core.field (Core.caseAlternativeName $ var "alt") (Core.caseAlternativeHandler $ var "alt"))
    (var "csCases") $
  "defaultField" <~ Optionals.cases (var "mdef") (list ([] :: [TypedTerm Field])) ("d" ~> list [Core.field (Core.name $ string "[default]") (var "d")]) $
  "allFields" <~ Lists.concat (list [var "caseFields", var "defaultField"]) $
  Strings.concat $ list [
    string "case(",
    var "tname",
    string ")",
    fields @@ var "allFields"]
      
either_ :: TypedTermDefinition ((a -> String) -> (b -> String) -> Prelude.Either a b -> String)
either_ = define "either" $
  doc "Show an Either value using given functions for left and right" $
  "showA" ~> "showB" ~> "e" ~>
  Eithers.either
    ("a" ~> Strings.concat2 (string "left(") (Strings.concat2 (var "showA" @@ var "a") (string ")")))
    ("b" ~> Strings.concat2 (string "right(") (Strings.concat2 (var "showB" @@ var "b") (string ")")))
    (var "e")

field :: TypedTermDefinition (Field -> String)
field = define "field" $
  doc "Show a field as a string" $
  "field" ~>
  "fname" <~ unwrap _Name @@ (Core.fieldName $ var "field") $
  "fterm" <~ Core.fieldTerm (var "field") $
  Strings.concat $ list [var "fname", string "=", term @@ var "fterm"]

fieldType :: TypedTermDefinition (FieldType -> String)
fieldType = define "fieldType" $
  doc "Show a field type as a string" $
  "ft" ~>
  "fname" <~ unwrap _Name @@ (Core.fieldTypeName $ var "ft") $
  "ftyp" <~ Core.fieldTypeType (var "ft") $
  Strings.concat $ list [
    var "fname",
    string ":",
    type_ @@ var "ftyp"]

fields :: TypedTermDefinition ([Field] -> String)
fields = define "fields" $
  doc "Show a list of fields as a string" $
  "flds" ~>
  "fieldStrs" <~ Lists.map (asTerm field) (var "flds") $
  Strings.concat $ list [
    string "{",
    Strings.join (string ", ") (var "fieldStrs"),
    string "}"]

floatValue :: TypedTermDefinition (FloatValue -> String)
floatValue = define "float" $
  doc "Show a float value as a string" $
  "fv" ~> cases _FloatValue (var "fv") Nothing [
    _FloatValue_float32>>: "v" ~> Literals.showFloat32 (var "v") ++ (string ":float32"),
    _FloatValue_float64>>: "v" ~> Literals.showFloat64 (var "v") ++ (string ":float64")]

floatType :: TypedTermDefinition (FloatType -> String)
floatType = define "floatType" $
  doc "Show a float type as a string" $
  "ft" ~> cases _FloatType (var "ft") Nothing [
    _FloatType_float32>>: constant $ string "float32",
    _FloatType_float64>>: constant $ string "float64"]

injection :: TypedTermDefinition (Injection -> String)
injection = define "injection" $
  doc "Show an injection as a string" $
  "inj" ~>
  "tname" <~ Core.injectionTypeName (var "inj") $
  "f" <~ Core.injectionField (var "inj") $
  Strings.concat $ list [
    string "inject(",
    unwrap _Name @@ var "tname",
    string ")",
    fields @@ (list [var "f"])]

integerValue :: TypedTermDefinition (IntegerValue -> String)
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

integerType :: TypedTermDefinition (IntegerType -> String)
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

lambda :: TypedTermDefinition (Lambda -> String)
lambda = define "lambda" $
  doc "Show a lambda as a string" $
  "l" ~>
  "v" <~ unwrap _Name @@ (Core.lambdaParameter $ var "l") $
  "mt" <~ Core.lambdaDomain (var "l") $
  "body" <~ Core.lambdaBody (var "l") $
  "typeStr" <~ Optionals.cases (var "mt") (string "") ("t" ~> Strings.concat2 (string ":") (type_ @@ var "t")) $
  Strings.concat $ list [
    string "λ",
    var "v",
    var "typeStr",
    string ".",
    term @@ var "body"]

let_ :: TypedTermDefinition (Let -> String)
let_ = define "let" $
  doc "Show a let expression as a string" $
  "l" ~>
  "bindings" <~ Core.letBindings (var "l") $
  "env" <~ Core.letBody (var "l") $
  "bindingStrs" <~ Lists.map (asTerm binding) (var "bindings") $
  Strings.concat $ list [
    string "let ",
    Strings.join (string ", ") (var "bindingStrs"),
    string " in ",
    term @@ var "env"]

list_ :: TypedTermDefinition ((a -> String) -> [a] -> String)
list_ = define "list" $
  doc "Show a list using a given function to show each element" $
  "f" ~> "xs" ~>
  "elementStrs" <~ Lists.map (var "f") (var "xs") $
  Strings.concat $ list [
    string "[",
    Strings.join (string ", ") (var "elementStrs"),
    string "]"]

literal :: TypedTermDefinition (Literal -> String)
literal = define "literal" $
  doc "Show a literal as a string" $
  "l" ~> cases _Literal (var "l") Nothing [
    _Literal_binary>>: constant $ string "[binary]",
    _Literal_boolean>>: "b" ~> Logic.ifElse (var "b") (string "true") (string "false"),
    _Literal_decimal>>: "d" ~> Literals.showDecimal $ var "d",
    _Literal_float>>: "fv" ~> floatValue @@ var "fv",
    _Literal_integer>>: "iv" ~> integerValue @@ var "iv",
    _Literal_string>>: "s" ~> Literals.printString $ var "s"]

literalType :: TypedTermDefinition (LiteralType -> String)
literalType = define "literalType" $
  doc "Show a literal type as a string" $
  "lt" ~> cases _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant $ string "binary",
    _LiteralType_boolean>>: constant $ string "boolean",
    _LiteralType_decimal>>: constant $ string "decimal",
    _LiteralType_float>>: "ft" ~> floatType @@ var "ft",
    _LiteralType_integer>>: "it" ~> integerType @@ var "it",
    _LiteralType_string>>: constant $ string "string"]

-- map_/set_ carry an `Ord` constraint + `forall` because the generated `Hydra.Dsl.Lib.{Maps,Sets}`
-- expose the primitive's `Ord` key/element constraint (the old hand-written `Meta.Lib.*` did not),
-- which also forces a placeholder concrete type at registration in `definitions`. See #467.
map_ :: forall k v. Ord k => TypedTermDefinition ((k -> String) -> (v -> String) -> M.Map k v -> String)
map_ = define "map" $
  doc "Show a map using given functions to show keys and values" $
  "showK" ~> "showV" ~> "m" ~>
  "pairStrs" <~ Lists.map ("p" ~> Strings.concat $ list [
    var "showK" @@ (Pairs.first $ var "p"),
    string ": ",
    var "showV" @@ (Pairs.second $ var "p")]) (Maps.toList (var "m" :: TypedTerm (M.Map k v))) $
  Strings.concat $ list [
    string "{",
    Strings.join (string ", ") (var "pairStrs"),
    string "}"]

optional_ :: TypedTermDefinition ((a -> String) -> Maybe a -> String)
optional_ = define "optional" $
  doc "Show an optional value using a given function to show the element" $
  "f" ~> "mx" ~>
  Optionals.cases (var "mx") (string "none") ("x" ~> Strings.concat2 (string "given(") (Strings.concat2 (var "f" @@ var "x") (string ")")))

pair_ :: TypedTermDefinition ((a -> String) -> (b -> String) -> (a, b) -> String)
pair_ = define "pair" $
  doc "Show a pair using given functions to show each element" $
  "showA" ~> "showB" ~> "p" ~>
  Strings.concat $ list [
    string "(",
    var "showA" @@ (Pairs.first $ var "p"),
    string ", ",
    var "showB" @@ (Pairs.second $ var "p"),
    string ")"]

projection :: TypedTermDefinition (Projection -> String)
projection = define "projection" $
  doc "Show a projection as a string" $
  "proj" ~>
  "tname" <~ unwrap _Name @@ (Core.projectionTypeName $ var "proj") $
  "fname" <~ unwrap _Name @@ (Core.projectionFieldName $ var "proj") $
  Strings.concat $ list [
    string "project(",
    var "tname",
    string "){",
    var "fname",
    string "}"]

readTerm :: TypedTermDefinition (String -> Maybe Term)
readTerm = define "readTerm" $
  doc "A placeholder for reading terms from their serialized form. Not implemented." $
  "s" ~> just $ Core.termLiteral $ Core.literalString $ var "s"

set_ :: forall a. Ord a => TypedTermDefinition ((a -> String) -> S.Set a -> String)
set_ = define "set" $
  doc "Show a set using a given function to show each element" $
  "f" ~> "xs" ~>
  "elementStrs" <~ Lists.map (var "f") (Sets.toList (var "xs" :: TypedTerm (S.Set a))) $
  Strings.concat $ list [
    string "{",
    Strings.join (string ", ") (var "elementStrs"),
    string "}"]

term :: TypedTermDefinition (Term -> String)
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
      "terms" <~ var "gatherTerms" @@ (list ([] :: [TypedTerm Term])) @@ var "app" $
      "termStrs" <~ Lists.map (asTerm term) (var "terms") $
      Strings.concat $ list [
        string "(",
        Strings.join (string " @ ") (var "termStrs"),
        string ")"],
    _Term_cases>>: caseStatement,
    _Term_either>>: "e" ~> Eithers.either
      ("l" ~> Strings.concat $ list [
        string "left(",
        term @@ var "l",
        string ")"])
      ("r" ~> Strings.concat $ list [
        string "right(",
        term @@ var "r",
        string ")"])
      (var "e"),
    _Term_lambda>>: lambda,
    _Term_let>>: "l" ~> let_ @@ var "l",
    _Term_list>>: "els" ~>
      "termStrs" <~ Lists.map (asTerm term) (var "els") $
      Strings.concat $ list [
        string "[",
        Strings.join (string ", ") (var "termStrs"),
        string "]"],
    _Term_literal>>: "lit" ~> literal @@ var "lit",
    _Term_map>>: "m" ~>
      "entry" <~ ("p" ~> Strings.concat $ list [
        term @@ (Pairs.first $ var "p"),
        string "=",
        term @@ (Pairs.second $ var "p")]) $
      Strings.concat $ list [
        string "{",
        Strings.join (string ", ") $ Lists.map (var "entry") $ Maps.toList (var "m" :: TypedTerm (M.Map Term Term)),
        string "}"],
    _Term_optional>>: "mt" ~> Optionals.cases (var "mt") (string "none") ("t" ~> Strings.concat $ list [
        string "given(",
        term @@ var "t",
        string ")"]),
    _Term_pair>>: "p" ~> Strings.concat $ list [
      string "(",
      term @@ (Pairs.first $ var "p"),
      string ", ",
      term @@ (Pairs.second $ var "p"),
      string ")"],
    _Term_project>>: projection,
    _Term_record>>: "rec" ~>
      "tname" <~ unwrap _Name @@ (Core.recordTypeName $ var "rec") $
      "flds" <~ Core.recordFields (var "rec") $
      Strings.concat $ list [
        string "record(",
        var "tname",
        string ")",
        fields @@ var "flds"],
    _Term_set>>: "s" ~>
      Strings.concat $ list [
        string "{",
        Strings.join (string ", ") (Lists.map (asTerm term) $ Sets.toList $ var "s"),
        string "}"],
    _Term_typeLambda>>: "ta" ~>
      "param" <~ unwrap _Name @@ (Core.typeLambdaParameter $ var "ta") $
      "body" <~ Core.typeLambdaBody (var "ta") $
      Strings.concat $ list [
        string "Λ",
        var "param",
        string ".",
        term @@ var "body"],
    _Term_typeApplication>>: "tt" ~>
      "t2" <~ Core.typeApplicationTermBody (var "tt") $
      "typ" <~ Core.typeApplicationTermType (var "tt") $
      Strings.concat $ list [
        term @@ var "t2",
        string "⟨",
        type_ @@ var "typ",
        string "⟩"],
    _Term_inject>>: injection,
    _Term_unit>>: constant $ string "unit",
    _Term_unwrap>>: "tname" ~> Strings.concat $ list [
      string "unwrap(",
      unwrap _Name @@ var "tname",
      string ")"],
    _Term_variable>>: "name" ~> unwrap _Name @@ var "name",
    _Term_wrap>>: "wt" ~>
      "tname" <~ unwrap _Name @@ (Core.wrappedTermTypeName $ var "wt") $
      "term1" <~ Core.wrappedTermBody (var "wt") $
      Strings.concat $ list [
        string "wrap(",
        var "tname",
        string "){",
        term @@ var "term1",
        string "}"]]

typeScheme :: TypedTermDefinition (TypeScheme -> String)
typeScheme = define "typeScheme" $
  doc "Show a type scheme as a string" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "body" <~ Core.typeSchemeBody (var "ts") $
  "varNames" <~ Lists.map (unwrap _Name) (var "vars") $
  "fa" <~ Logic.ifElse (Lists.null $ var "vars")
    (string "")
    (Strings.concat $ list [
      string "forall ",
      Strings.join (string ",") (var "varNames"),
      string ". "]) $
  "toConstraintPair" <~ ("v" ~> "c" ~> Strings.concat $ list [
    match _TypeClassConstraint Nothing [
      _TypeClassConstraint_simple>>: "n" ~> Core.unName (var "n")] @@ (var "c"),
    string " ",
    Core.unName (var "v")]) $
  "toConstraintPairs" <~ ("p" ~> Lists.map
    (var "toConstraintPair" @@ (Pairs.first $ var "p")) $
    Core.typeVariableConstraintsClasses $ Pairs.second $ var "p") $
  "tc" <~ optCases (Core.typeSchemeConstraints (var "ts"))
    (list ([] :: [TypedTerm String]))
    ("m" ~> Lists.concat $ Lists.map (var "toConstraintPairs") $ Maps.toList (var "m" :: TypedTerm (M.Map Name TypeVariableConstraints))) $
  Strings.concat $ list [
    string "(",
    var "fa",
    Logic.ifElse (Lists.null $ var "tc")
      (string "")
      (Strings.concat $ list [
        string "(",
        Strings.join (string ", ") (var "tc"),
        string ") => "]),
    type_ @@ var "body",
    string ")"]

type_ :: TypedTermDefinition (Type -> String)
type_ = define "type" $
  doc "Show a type as a string" $
  "typ" ~>
  "showRowType" <~ ("flds" ~>
    "fieldStrs" <~ Lists.map (asTerm fieldType) (var "flds") $
    Strings.concat $ list [
      string "{",
      Strings.join (string ", ") (var "fieldStrs"),
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
      "types" <~ var "gatherTypes" @@ (list ([] :: [TypedTerm Type])) @@ var "app" $
      "typeStrs" <~ Lists.map (asTerm type_) (var "types") $
      Strings.concat $ list [
        string "(",
        Strings.join (string " @ ") (var "typeStrs"),
        string ")"],
    _Type_effect>>: "etyp" ~> Strings.concat $ list [
      string "effect<",
      type_ @@ var "etyp",
      string ">"],
    _Type_either>>: "et" ~>
      "leftTyp" <~ Core.eitherTypeLeft (var "et") $
      "rightTyp" <~ Core.eitherTypeRight (var "et") $
      Strings.concat $ list [
        string "either<",
        type_ @@ var "leftTyp",
        string ", ",
        type_ @@ var "rightTyp",
        string ">"],
    _Type_forall>>: "ft" ~>
      "var" <~ unwrap _Name @@ (Core.forallTypeParameter $ var "ft") $
      "body" <~ Core.forallTypeBody (var "ft") $
      Strings.concat $ list [
        string "(∀",
        var "var",
        string ".",
        type_ @@ var "body",
        string ")"],
    _Type_function>>: "ft" ~>
      "types" <~ var "gatherFunctionTypes" @@ (list ([] :: [TypedTerm Type])) @@ var "typ" $
      "typeStrs" <~ Lists.map (asTerm type_) (var "types") $
      Strings.concat $ list [
        string "(",
        Strings.join (string " → ") (var "typeStrs"),
        string ")"],
    _Type_list>>: "etyp" ~> Strings.concat $ list [
      string "list<",
      type_ @@ var "etyp",
      string ">"],
    _Type_literal>>: "lt" ~> literalType @@ var "lt",
    _Type_map>>: "mt" ~>
      "keyTyp" <~ Core.mapTypeKeys (var "mt") $
      "valTyp" <~ Core.mapTypeValues (var "mt") $
      Strings.concat $ list [
        string "map<",
        type_ @@ var "keyTyp",
        string ", ",
        type_ @@ var "valTyp",
        string ">"],
    _Type_optional>>: "etyp" ~> Strings.concat $ list [
      string "optional<",
      type_ @@ var "etyp",
      string ">"],
    _Type_pair>>: "pt" ~>
      "firstTyp" <~ Core.pairTypeFirst (var "pt") $
      "secondTyp" <~ Core.pairTypeSecond (var "pt") $
      Strings.concat $ list [
        string "(",
        type_ @@ var "firstTyp",
        string ", ",
        type_ @@ var "secondTyp",
        string ")"],
    _Type_record>>: "rt" ~> Strings.concat2 (string "record") (var "showRowType" @@ var "rt"),
    _Type_set>>: "etyp" ~> Strings.concat $ list [
      string "set<",
      type_ @@ var "etyp",
      string ">"],
    _Type_union>>: "rt" ~> Strings.concat2 (string "union") (var "showRowType" @@ var "rt"),
    _Type_unit>>: constant $ string "unit",
    _Type_variable>>: "name" ~> unwrap _Name @@ var "name",
    _Type_void>>: constant $ string "void",
    _Type_wrap>>: "wt" ~>
      Strings.concat $ list [string "wrap(", type_ @@ var "wt", string ")"]]
