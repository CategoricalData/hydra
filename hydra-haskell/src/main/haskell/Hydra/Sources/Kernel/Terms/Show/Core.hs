{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Show.Core where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


module_ :: Module
module_ = Module (Namespace "hydra.show.core") elements
    []
    kernelTypesModules $
    Just "String representations of hydra.core types"
  where
   elements = [
     el readTermDef, -- TODO: move this to hydra.read.core
     el bindingDef,
     el eliminationDef,
     el fieldDef,
     el fieldTypeDef,
     el fieldsDef,
     el floatValueDef,
     el floatTypeDef,
     el functionDef,
     el injectionDef,
     el integerValueDef,
     el integerTypeDef,
     el lambdaDef,
     el listDef,
     el literalDef,
     el literalTypeDef,
     el termDef,
     el typeDef,
     el typeSchemeDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

readTermDef :: TBinding (String -> Maybe Term)
readTermDef = define "readTerm" $
  doc "A placeholder for reading terms from their serialized form. Not implemented." $
  "s" ~> just $ Core.termLiteral $ Core.literalString $ var "s"

bindingDef :: TBinding (Binding -> String)
bindingDef = define "binding" $
  doc "Show a binding as a string" $
  "el" ~>
  "name" <~ unwrap _Name @@ (Core.bindingName $ var "el") $
  "t" <~ Core.bindingTerm (var "el") $
  "typeStr" <~ Maybes.maybe
    (string "")
    ("ts" ~> Strings.concat [string ":(", ref typeSchemeDef @@ var "ts", string ")"])
    (Core.bindingType $ var "el") $
  Strings.cat $ list [
    var "name",
    var "typeStr",
    string " = ",
    ref termDef @@ var "t"]
      
eliminationDef :: TBinding (Elimination -> String)
eliminationDef = define "elimination" $
  doc "Show an elimination as a string" $
  "elm" ~>
  cases _Elimination (var "elm") Nothing [
    _Elimination_product>>: "tp" ~>
--      "domain" <~ Core.tupleProjectionDomain (var "tp") $ -- TODO: show domain if present
      Strings.cat $ list [
        string "[",
        Literals.showInt32 (Core.tupleProjectionIndex $ var "tp"),
        string "/",
        Literals.showInt32 (Core.tupleProjectionArity $ var "tp"),
        string "]"],
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
        (list [])
        ("d" ~> list [Core.field (Core.name $ string "[default]") (var "d")])
        (var "mdef") $
      "allFields" <~ Lists.concat (list [var "cases", var "defaultField"]) $
      Strings.cat $ list [
        string "case(",
        var "tname",
        string ")",
        ref fieldsDef @@ var "allFields"],
    _Elimination_wrap>>: "tname" ~> Strings.cat $ list [
      string "unwrap(",
      unwrap _Name @@ var "tname",
      string ")"]]

fieldDef :: TBinding (Field -> String)
fieldDef = define "field" $
  "field" ~>
  "fname" <~ unwrap _Name @@ (Core.fieldName $ var "field") $
  "fterm" <~ Core.fieldTerm (var "field") $
  Strings.cat $ list [var "fname",  "=", ref termDef @@ var "fterm"]

fieldTypeDef :: TBinding (FieldType -> String)
fieldTypeDef = define "fieldType" $
  "ft" ~>
  "fname" <~ unwrap _Name @@ (Core.fieldTypeName $ var "ft") $
  "ftyp" <~ Core.fieldTypeType (var "ft") $
  Strings.cat $ list [
    var "fname",
    string ":",
    ref typeDef @@ var "ftyp"]

fieldsDef :: TBinding ([Field] -> String)
fieldsDef = define "fields" $
  doc "Show a list of fields as a string" $
  "flds" ~>
  "fieldStrs" <~ Lists.map (ref fieldDef) (var "flds") $
  Strings.cat $ list [
    string "{",
    Strings.intercalate (string ", ") (var "fieldStrs"),
    string "}"]

floatValueDef :: TBinding (FloatValue -> String)
floatValueDef = define "float" $
  doc "Show a float value as a string" $
  "fv" ~> cases _FloatValue (var "fv") Nothing [
    _FloatValue_bigfloat>>: "v" ~> Literals.showBigfloat (var "v") ++ ":bigfloat",
    _FloatValue_float32>>: "v" ~> Literals.showFloat32 (var "v") ++ ":float32",
    _FloatValue_float64>>: "v" ~> Literals.showFloat64 (var "v") ++ ":float64"]

floatTypeDef :: TBinding (FloatType -> String)
floatTypeDef = define "floatType" $
  doc "Show a float type as a string" $
  "ft" ~> cases _FloatType (var "ft") Nothing [
    _FloatType_bigfloat>>: constant $ string "bigfloat",
    _FloatType_float32>>: constant $ string "float32",
    _FloatType_float64>>: constant $ string "float64"]

functionDef :: TBinding (Function -> String)
functionDef = define "function" $
  doc "Show a function as a string" $
  "f" ~> cases _Function (var "f") Nothing [
    _Function_elimination>>: ref eliminationDef,
    _Function_lambda>>: ref lambdaDef,
    _Function_primitive>>: "name" ~> Strings.cat2 (unwrap _Name @@ var "name") (string "!")]

injectionDef :: TBinding (Injection -> String)
injectionDef = define "injection" $
  doc "Show an injection as a string" $
  "inj" ~>
  "tname" <~ Core.injectionTypeName (var "inj") $
  "f" <~ Core.injectionField (var "inj") $
  Strings.cat $ list [
    string "inject(",
    unwrap _Name @@ var "tname",
    string ")",
    ref fieldsDef @@ (list [var "f"])]

integerValueDef :: TBinding (IntegerValue -> String)
integerValueDef = define "integer" $
  doc "Show an integer value as a string" $
  "iv" ~> cases _IntegerValue (var "iv") Nothing [
    _IntegerValue_bigint>>: "v" ~> Literals.showBigint (var "v") ++ ":bigint",
    _IntegerValue_int8>>: "v" ~> Literals.showInt8 (var "v") ++ ":int8",
    _IntegerValue_int16>>: "v" ~> Literals.showInt16 (var "v") ++ ":int16",
    _IntegerValue_int32>>: "v" ~> Literals.showInt32 (var "v") ++ ":int32",
    _IntegerValue_int64>>: "v" ~> Literals.showInt64 (var "v") ++ ":int64",
    _IntegerValue_uint8>>: "v" ~> Literals.showUint8 (var "v") ++ ":uint8",
    _IntegerValue_uint16>>: "v" ~> Literals.showUint16 (var "v") ++ ":uint16",
    _IntegerValue_uint32>>: "v" ~> Literals.showUint32 (var "v") ++ ":uint32",
    _IntegerValue_uint64>>: "v" ~> Literals.showUint64 (var "v") ++ ":uint64"]

integerTypeDef :: TBinding (IntegerType -> String)
integerTypeDef = define "integerType" $
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

lambdaDef :: TBinding (Lambda -> String)
lambdaDef = define "lambda" $
  doc "Show a lambda as a string" $
  "l" ~>
  "v" <~ unwrap _Name @@ (Core.lambdaParameter $ var "l") $
  "mt" <~ Core.lambdaDomain (var "l") $
  "body" <~ Core.lambdaBody (var "l") $
  "typeStr" <~ Maybes.maybe
    (string "")
    ("t" ~> Strings.cat2 (string ":") (ref typeDef @@ var "t"))
    (var "mt") $
  Strings.cat $ list [
    string "λ",
    var "v",
    var "typeStr",
    string ".",
    ref termDef @@ var "body"]

listDef :: TBinding ((a -> String) -> [a] -> String)
listDef = define "list" $
  doc "Show a list using a given function to show each element" $
  "f" ~> "xs" ~>
  "elementStrs" <~ Lists.map (var "f") (var "xs") $
  Strings.cat $ list [
    string "[",
    Strings.intercalate (string ", ") (var "elementStrs"),
    string "]"]

literalDef :: TBinding (Literal -> String)
literalDef = define "literal" $
  doc "Show a literal as a string" $
  "l" ~> cases _Literal (var "l") Nothing [
    _Literal_binary>>: constant $ string "[binary]",
    _Literal_boolean>>: "b" ~> Logic.ifElse (var "b") (string "true") (string "false"),
    _Literal_float>>: "fv" ~> ref floatValueDef @@ var "fv",
    _Literal_integer>>: "iv" ~> ref integerValueDef @@ var "iv",
    _Literal_string>>: "s" ~> Literals.showString $ var "s"]

literalTypeDef :: TBinding (LiteralType -> String)
literalTypeDef = define "literalType" $
  doc "Show a literal type as a string" $
  "lt" ~> cases _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant $ string "binary",
    _LiteralType_boolean>>: constant $ string "boolean",
    _LiteralType_float>>: "ft" ~> ref floatTypeDef @@ var "ft",
    _LiteralType_integer>>: "it" ~> ref integerTypeDef @@ var "it",
    _LiteralType_string>>: constant $ string "string"]

termDef :: TBinding (Term -> String)
termDef = define "term" $
  doc "Show a term as a string" $
  "t" ~>
  "gatherTerms" <~ ("prev" ~> "app" ~>
    "lhs" <~ Core.applicationFunction (var "app") $
    "rhs" <~ Core.applicationArgument (var "app") $
    cases _Term (var "lhs")
      (Just $ Lists.cons (var "lhs") (Lists.cons (var "rhs") (var "prev"))) [
      _Term_application>>: "app2" ~> var "gatherTerms" @@ (Lists.cons (var "rhs") (var "prev")) @@ var "app2"]) $
  cases _Term (var "t") Nothing [
    _Term_annotated>>: "at" ~> ref termDef @@ (Core.annotatedTermBody $ var "at"),
    _Term_application>>: "app" ~>
      "terms" <~ var "gatherTerms" @@ (list []) @@ var "app" $
      "termStrs" <~ Lists.map (ref termDef) (var "terms") $
      Strings.cat $ list [
        string "(",
        Strings.intercalate (string " @ ") (var "termStrs"),
        string ")"],
    _Term_either>>: "e" ~> Eithers.either_
      ("l" ~> Strings.cat $ list [
        string "left(",
        ref termDef @@ var "l",
        string ")"])
      ("r" ~> Strings.cat $ list [
        string "right(",
        ref termDef @@ var "r",
        string ")"])
      (var "e"),
    _Term_function>>: ref functionDef,
    _Term_let>>: "l" ~>
      "bindings" <~ Core.letBindings (var "l") $
      "env" <~ Core.letBody (var "l") $
      "bindingStrs" <~ Lists.map (ref bindingDef) (var "bindings") $
      Strings.cat $ list [
        string "let ",
        Strings.intercalate (string ", ") (var "bindingStrs"),
        string " in ",
        ref termDef @@ var "env"],
    _Term_list>>: "els" ~>
      "termStrs" <~ Lists.map (ref termDef) (var "els") $
      Strings.cat $ list [
        string "[",
        Strings.intercalate (string ", ") (var "termStrs"),
        string "]"],
    _Term_literal>>: "lit" ~> ref literalDef @@ var "lit",
    _Term_map>>: "m" ~>
      "entry" <~ ("p" ~> Strings.cat $ list [
        ref termDef @@ (Pairs.first $ var "p"),
        string "=",
        ref termDef @@ (Pairs.second $ var "p")]) $
      Strings.cat $ list [
        string "{",
        Strings.intercalate (string ", ") $ Lists.map (var "entry") $ Maps.toList $ var "m",
        string "}"],
    _Term_maybe>>: "mt" ~> Maybes.maybe
      (string "nothing")
      ("t" ~> Strings.cat $ list [
        string "just(",
        ref termDef @@ var "t",
        string ")"])
      (var "mt"),
    _Term_pair>>: "p" ~> Strings.cat $ list [
      string "(",
      ref termDef @@ (Pairs.first $ var "p"),
      string ", ",
      ref termDef @@ (Pairs.second $ var "p"),
      string ")"],
    _Term_product>>: "els" ~>
      "termStrs" <~ Lists.map (ref termDef) (var "els") $
      Strings.cat $ list [
        string "(",
        Strings.intercalate (string ", ") (var "termStrs"),
        string ")"],
    _Term_record>>: "rec" ~>
      "tname" <~ unwrap _Name @@ (Core.recordTypeName $ var "rec") $
      "flds" <~ Core.recordFields (var "rec") $
      Strings.cat $ list [
        string "record(",
        var "tname",
        string ")",
        ref fieldsDef @@ var "flds"],
    _Term_set>>: "s" ~>
      Strings.cat $ list [
        string "{",
        Strings.intercalate (string ", ") (Lists.map (ref termDef) $ Sets.toList $ var "s"),
        string "}"],
    _Term_typeLambda>>: "ta" ~>
      "param" <~ unwrap _Name @@ (Core.typeLambdaParameter $ var "ta") $
      "body" <~ Core.typeLambdaBody (var "ta") $
      Strings.cat $ list [
        string "Λ",
        var "param",
        string ".",
        ref termDef @@ var "body"],
    _Term_typeApplication>>: "tt" ~>
      "t2" <~ Core.typeApplicationTermBody (var "tt") $
      "typ" <~ Core.typeApplicationTermType (var "tt") $
      Strings.cat $ list [
        ref termDef @@ var "t2",
        string "⟨",
        ref typeDef @@ var "typ",
        string "⟩"],
    _Term_union>>: ref injectionDef,
    _Term_unit>>: constant $ string "unit",
    _Term_variable>>: "name" ~> unwrap _Name @@ var "name",
    _Term_wrap>>: "wt" ~>
      "tname" <~ unwrap _Name @@ (Core.wrappedTermTypeName $ var "wt") $
      "term1" <~ Core.wrappedTermBody (var "wt") $
      Strings.cat $ list [
        string "wrap(",
        var "tname",
        string "){",
        ref termDef @@ var "term1",
        string "}"]]

typeDef :: TBinding (Type -> String)
typeDef = define "type" $
  doc "Show a type as a string" $
  "typ" ~>
  "showRowType" <~ ("rt" ~>
    "flds" <~ Core.rowTypeFields (var "rt") $
    "fieldStrs" <~ Lists.map (ref fieldTypeDef) (var "flds") $
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
    _Type_annotated>>: "at" ~> ref typeDef @@ (Core.annotatedTypeBody $ var "at"),
    _Type_application>>: "app" ~>
      "types" <~ var "gatherTypes" @@ (list []) @@ var "app" $
      "typeStrs" <~ Lists.map (ref typeDef) (var "types") $
      Strings.cat $ list [
        string "(",
        Strings.intercalate (string " @ ") (var "typeStrs"),
        string ")"],
    _Type_either>>: "et" ~>
      "leftTyp" <~ Core.eitherTypeLeft (var "et") $
      "rightTyp" <~ Core.eitherTypeRight (var "et") $
      Strings.cat $ list [
        string "either<",
        ref typeDef @@ var "leftTyp",
        string ", ",
        ref typeDef @@ var "rightTyp",
        string ">"],
    _Type_forall>>: "ft" ~>
      "var" <~ unwrap _Name @@ (Core.forallTypeParameter $ var "ft") $
      "body" <~ Core.forallTypeBody (var "ft") $
      Strings.cat $ list [
        string "(∀",
        var "var",
        string ".",
        ref typeDef @@ var "body",
        string ")"],
    _Type_function>>: "ft" ~>
      "types" <~ var "gatherFunctionTypes" @@ (list []) @@ var "typ" $
      "typeStrs" <~ Lists.map (ref typeDef) (var "types") $
      Strings.cat $ list [
        string "(",
        Strings.intercalate (string " → ") (var "typeStrs"),
        string ")"],
    _Type_list>>: "etyp" ~> Strings.cat $ list [
      string "list<",
      ref typeDef @@ var "etyp",
      string ">"],
    _Type_literal>>: "lt" ~> ref literalTypeDef @@ var "lt",
    _Type_map>>: "mt" ~>
      "keyTyp" <~ Core.mapTypeKeys (var "mt") $
      "valTyp" <~ Core.mapTypeValues (var "mt") $
      Strings.cat $ list [
        string "map<",
        ref typeDef @@ var "keyTyp",
        string ", ",
        ref typeDef @@ var "valTyp",
        string ">"],
    _Type_maybe>>: "etyp" ~> Strings.cat $ list [
      string "maybe<",
      ref typeDef @@ var "etyp",
      string ">"],
    _Type_pair>>: "pt" ~>
      "firstTyp" <~ Core.pairTypeFirst (var "pt") $
      "secondTyp" <~ Core.pairTypeSecond (var "pt") $
      Strings.cat $ list [
        string "(",
        ref typeDef @@ var "firstTyp",
        string ", ",
        ref typeDef @@ var "secondTyp",
        ")"],
    _Type_product>>: "types" ~>
      "typeStrs" <~ Lists.map (ref typeDef) (var "types") $
      Strings.intercalate (string "×") (var "typeStrs"),
    _Type_record>>: "rt" ~> Strings.cat2 (string "record") (var "showRowType" @@ var "rt"),
    _Type_set>>: "etyp" ~> Strings.cat $ list [
      string "set<",
      ref typeDef @@ var "etyp",
      string ">"],
    _Type_union>>: "rt" ~> Strings.cat2 (string "union") (var "showRowType" @@ var "rt"),
    _Type_unit>>: constant $ string "unit",
    _Type_variable>>: "name" ~> unwrap _Name @@ var "name",
    _Type_wrap>>: "wt" ~>
      "tname" <~ unwrap _Name @@ (Core.wrappedTypeTypeName $ var "wt") $
      "typ1" <~ Core.wrappedTypeBody (var "wt") $
      Strings.cat $ list [string "wrap[", var "tname", string "](", ref typeDef @@ var "typ1", string ")"]]

typeSchemeDef :: TBinding (TypeScheme -> String)
typeSchemeDef = define "typeScheme" $
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
    ref typeDef @@ var "body",
    string ")"]
