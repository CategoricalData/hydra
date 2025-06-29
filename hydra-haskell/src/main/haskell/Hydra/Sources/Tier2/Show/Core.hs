{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Show.Core where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Coders                 as Coders
import qualified Hydra.Dsl.Compute                as Compute
import qualified Hydra.Dsl.Core                   as Core
import qualified Hydra.Dsl.Graph                  as Graph
import qualified Hydra.Dsl.Lib.Chars              as Chars
import qualified Hydra.Dsl.Lib.Equality           as Equality
import qualified Hydra.Dsl.Lib.Flows              as Flows
import qualified Hydra.Dsl.Lib.Lists              as Lists
import qualified Hydra.Dsl.Lib.Literals           as Literals
import qualified Hydra.Dsl.Lib.Logic              as Logic
import qualified Hydra.Dsl.Lib.Maps               as Maps
import qualified Hydra.Dsl.Lib.Math               as Math
import qualified Hydra.Dsl.Lib.Optionals          as Optionals
import           Hydra.Dsl.Phantoms               as Phantoms
import qualified Hydra.Dsl.Lib.Sets               as Sets
import           Hydra.Dsl.Lib.Strings            as Strings
import qualified Hydra.Dsl.Mantle                 as Mantle
import qualified Hydra.Dsl.Module                 as Module
import qualified Hydra.Dsl.TTerms                 as TTerms
import qualified Hydra.Dsl.TTypes                 as TTypes
import qualified Hydra.Dsl.Terms                  as Terms
import qualified Hydra.Dsl.Topology               as Topology
import qualified Hydra.Dsl.Types                  as Types
import qualified Hydra.Dsl.Typing                 as Typing
import qualified Hydra.Sources.Tier1.All          as Tier1
import qualified Hydra.Sources.Tier1.Constants    as Constants
import qualified Hydra.Sources.Tier1.Decode       as Decode
import qualified Hydra.Sources.Tier1.Encode.Core  as EncodeCore
import qualified Hydra.Sources.Tier1.Formatting   as Formatting
import qualified Hydra.Sources.Tier1.Functions    as Functions
import qualified Hydra.Sources.Tier1.Literals     as Literals
import qualified Hydra.Sources.Tier1.Messages     as Messages
import qualified Hydra.Sources.Tier1.Strip        as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

--import qualified Hydra.Sources.Tier2.Accessors as Accessors
--import qualified Hydra.Sources.Tier2.Adapters as Adapters
--import qualified Hydra.Sources.Tier2.AdapterUtils as AdapterUtils
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
--import qualified Hydra.Sources.Tier2.CoreLanguage as CoreLanguage
--import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Errors as Errors
--import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
import qualified Hydra.Sources.Tier2.Flows as Flows_
--import qualified Hydra.Sources.Tier2.GrammarToModule as GrammarToModule
--import qualified Hydra.Sources.Tier2.Inference as Inference
--import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.LiteralAdapters as LiteralAdapters
--import qualified Hydra.Sources.Tier2.Qnames as Qnames
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
--import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
--import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
--import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templating as Templating
--import qualified Hydra.Sources.Tier2.TermAdapters as TermAdapters
--import qualified Hydra.Sources.Tier2.Unification as Unification
--import qualified Hydra.Sources.Tier2.Variants as Variants


showCoreModule :: Module
showCoreModule = Module (Namespace "hydra.show.core") elements
    [Flows_.hydraFlowsModule, Tier1.hydraStripModule]
    [Tier1.hydraComputeModule, Tier1.hydraGraphModule, Tier1.hydraMantleModule, Tier1.hydraTypingModule] $
    Just "Haskell implementations of hydra.lib.io primitives"
  where
   elements = [
     el readTermDef, -- TODO: move this to hydra.read.core
     el elementDef, -- TODO: move this to hydra.show.graph
     el floatValueDef,
     el floatTypeDef,
     el graphDef, -- TODO: move this to hydra.show.graph
     el integerValueDef,
     el integerTypeDef,
     el listDef,
     el literalDef,
     el literalTypeDef,
     el termDef,
     el termVariantDef, -- TODO: move this to hydra.show.mantle
     el typeDef,
     el typeConstraintDef, -- TODO: move this to hydra.show.typing
     el typeSchemeDef,
     el typeSubstDef] -- TODO: move this to hydra.show.typing

showCoreDefinition :: String -> TTerm a -> TElement a
showCoreDefinition = definitionInModule showCoreModule

readTermDef :: TElement (String -> Maybe Term)
readTermDef = showCoreDefinition "readTerm" $
  doc "A placeholder for reading terms from their serialized form. Not implemented." $
  constant nothing

elementDef :: TElement (Element -> String)
elementDef = showCoreDefinition "element" $
  doc "Show an element as a string" $
  lambda "el" $ lets [
    "name">: unwrap _Name @@ (Graph.elementName $ var "el"),
    "t">: Graph.elementTerm $ var "el",
    "typeStr">: Optionals.maybe
      (string "")
      (lambda "ts" $ Strings.cat2 (string " : ") (ref typeSchemeDef @@ var "ts"))
      (Graph.elementType $ var "el")] $
    Strings.cat $ list [
      var "name",
      string " = ",
      ref termDef @@ var "t",
      var "typeStr"]

floatValueDef :: TElement (FloatValue -> String)
floatValueDef = showCoreDefinition "float" $
  doc "Show a float value as a string" $
  lambda "fv" $ cases _FloatValue (var "fv") Nothing [
    _FloatValue_bigfloat>>: lambda "v" $ Literals.showBigfloat $ var "v",
    _FloatValue_float32>>: lambda "v" $ Literals.showFloat32 $ var "v",
    _FloatValue_float64>>: lambda "v" $ Literals.showFloat64 $ var "v"]

floatTypeDef :: TElement (FloatType -> String)
floatTypeDef = showCoreDefinition "floatType" $
  doc "Show a float type as a string" $
  lambda "ft" $ cases _FloatType (var "ft") Nothing [
    _FloatType_bigfloat>>: constant $ string "bigfloat",
    _FloatType_float32>>: constant $ string "float32",
    _FloatType_float64>>: constant $ string "float64"]

graphDef :: TElement (Graph -> String)
graphDef = showCoreDefinition "graph" $
  doc "Show a graph as a string" $
  lambda "graph" $ lets [
    "elements">: Maps.elems $ Graph.graphElements $ var "graph",
    "elementStrs">: Lists.map (ref elementDef) (var "elements")] $
    Strings.cat $ list [
      string "{",
      Strings.intercalate (string ", ") (var "elementStrs"),
      string "}"]

integerValueDef :: TElement (IntegerValue -> String)
integerValueDef = showCoreDefinition "integer" $
  doc "Show an integer value as a string" $
  lambda "iv" $ cases _IntegerValue (var "iv") Nothing [
    _IntegerValue_bigint>>: lambda "v" $ Literals.showBigint $ var "v",
    _IntegerValue_int8>>: lambda "v" $ Literals.showInt8 $ var "v",
    _IntegerValue_int16>>: lambda "v" $ Literals.showInt16 $ var "v",
    _IntegerValue_int32>>: lambda "v" $ Literals.showInt32 $ var "v",
    _IntegerValue_int64>>: lambda "v" $ Literals.showInt64 $ var "v",
    _IntegerValue_uint8>>: lambda "v" $ Literals.showUint8 $ var "v",
    _IntegerValue_uint16>>: lambda "v" $ Literals.showUint16 $ var "v",
    _IntegerValue_uint32>>: lambda "v" $ Literals.showUint32 $ var "v",
    _IntegerValue_uint64>>: lambda "v" $ Literals.showUint64 $ var "v"]

integerTypeDef :: TElement (IntegerType -> String)
integerTypeDef = showCoreDefinition "integerType" $
  doc "Show an integer type as a string" $
  lambda "it" $ cases _IntegerType (var "it") Nothing [
    _IntegerType_bigint>>: constant $ string "bigint",
    _IntegerType_int8>>: constant $ string "int8",
    _IntegerType_int16>>: constant $ string "int16",
    _IntegerType_int32>>: constant $ string "int32",
    _IntegerType_int64>>: constant $ string "int64",
    _IntegerType_uint8>>: constant $ string "uint8",
    _IntegerType_uint16>>: constant $ string "uint16",
    _IntegerType_uint32>>: constant $ string "uint32",
    _IntegerType_uint64>>: constant $ string "uint64"]

listDef :: TElement ((a -> String) -> [a] -> String)
listDef = showCoreDefinition "list" $
  doc "Show a list using a function to show each element" $
  lambdas ["f", "xs"] $ lets [
    "elementStrs">: Lists.map (var "f") (var "xs")] $
    Strings.cat $ list [
      string "[",
      Strings.intercalate (string ", ") (var "elementStrs"),
      string "]"]

literalDef :: TElement (Literal -> String)
literalDef = showCoreDefinition "literal" $
  doc "Show a literal as a string" $
  lambda "l" $ cases _Literal (var "l") Nothing [
    _Literal_binary>>: constant $ string "[binary]",
    _Literal_boolean>>: lambda "b" $ Logic.ifElse (var "b") (string "true") (string "false"),
    _Literal_float>>: lambda "fv" $ ref floatValueDef @@ var "fv",
    _Literal_integer>>: lambda "iv" $ ref integerValueDef @@ var "iv",
    _Literal_string>>: lambda "s" $ Literals.showString $ var "s"]

literalTypeDef :: TElement (LiteralType -> String)
literalTypeDef = showCoreDefinition "literalType" $
  doc "Show a literal type as a string" $
  lambda "lt" $ cases _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant $ string "binary",
    _LiteralType_boolean>>: constant $ string "boolean",
    _LiteralType_float>>: lambda "ft" $ ref floatTypeDef @@ var "ft",
    _LiteralType_integer>>: lambda "it" $ ref integerTypeDef @@ var "it",
    _LiteralType_string>>: constant $ string "string"]

termDef :: TElement (Term -> String)
termDef = showCoreDefinition "term" $
  doc "Show a term as a string" $
  lambda "t" $ lets [
    "showField">: lambda "field" $ lets [
      "fname">: unwrap _Name @@ (Core.fieldName $ var "field"),
      "fterm">: Core.fieldTerm $ var "field"] $
      Strings.cat2 (var "fname") $ Strings.cat2 (string "=") (ref termDef @@ var "fterm"),
    "showFields">: lambda "fields" $ lets [
      "fieldStrs">: Lists.map (var "showField") (var "fields")] $
      Strings.cat $ list [
        string "{",
        Strings.intercalate (string ", ") (var "fieldStrs"),
        string "}"],
    "gatherTerms">: lambdas ["prev", "app"] $ lets [
      "lhs">: Core.applicationFunction $ var "app",
      "rhs">: Core.applicationArgument $ var "app",
      "strippedLhs">: ref Strip.stripTermDef @@ var "lhs"] $
      cases _Term (var "strippedLhs")
        (Just $ Lists.cons (var "strippedLhs") (Lists.cons (var "rhs") (var "prev"))) [
        _Term_application>>: lambda "app2" $ var "gatherTerms" @@ (Lists.cons (var "rhs") (var "prev")) @@ var "app2"],
    "showBinding">: lambda "binding" $ lets [
      "v">: unwrap _Name @@ (Core.letBindingName $ var "binding"),
      "bindingTerm">: Core.letBindingTerm $ var "binding",
      "typeStr">: Optionals.maybe
        (string "")
        (lambda "ts" $ Strings.cat2 (string ":") (ref typeSchemeDef @@ var "ts"))
        (Core.letBindingType $ var "binding")] $
      Strings.cat $ list [
        var "v",
        string "=",
        ref termDef @@ var "bindingTerm",
        var "typeStr"]] $
    cases _Term (ref Strip.stripTermDef @@ var "t") Nothing [
      _Term_application>>: lambda "app" $ lets [
        "terms">: var "gatherTerms" @@ (list []) @@ var "app",
        "termStrs">: Lists.map (ref termDef) (var "terms")] $
        Strings.cat $ list [
          string "(",
          Strings.intercalate (string " @ ") (var "termStrs"),
          string ")"],
      _Term_function>>: lambda "f" $ cases _Function (var "f") Nothing [
        _Function_elimination>>: lambda "elm" $ cases _Elimination (var "elm") Nothing [
          _Elimination_product>>: lambda "tp" $ lets [
            "arity">: Core.tupleProjectionArity $ var "tp",
            "index">: Core.tupleProjectionIndex $ var "tp",
            "domain">: Core.tupleProjectionDomain $ var "tp"] $ -- TODO: show domain if present
            Strings.cat $ list [
              string "]",
              Literals.showInt32 $ var "index",
              string "/",
              Literals.showInt32 $ var "arity",
              string "]"],
          _Elimination_record>>: lambda "proj" $ lets [
            "tname">: unwrap _Name @@ (Core.projectionTypeName $ var "proj"),
            "fname">: unwrap _Name @@ (Core.projectionField $ var "proj")] $
            Strings.cat $ list [
              string "project(",
              var "tname",
              string "){",
              var "fname",
              string "}"],
          _Elimination_union>>: lambda "cs" $ lets [
            "tname">: unwrap _Name @@ (Core.caseStatementTypeName $ var "cs"),
            "mdef">: Core.caseStatementDefault $ var "cs",
            "cases">: Core.caseStatementCases $ var "cs",
            "defaultField">: Optionals.maybe
              (list [])
              (lambda "d" $ list [Core.field (Core.name $ string "[default]") (var "d")])
              (var "mdef"),
            "allFields">: Lists.concat $ list [var "cases", var "defaultField"]] $
            Strings.cat $ list [
              string "case(",
              var "tname",
              string ")",
              var "showFields" @@ var "allFields"],
          _Elimination_wrap>>: lambda "tname" $ Strings.cat $ list [
            string "unwrap(",
            unwrap _Name @@ var "tname",
            string ")"]],
        _Function_lambda>>: lambda "l" $ lets [
          "v">: unwrap _Name @@ (Core.lambdaParameter $ var "l"),
          "mt">: Core.lambdaDomain $ var "l",
          "body">: Core.lambdaBody $ var "l",
          "typeStr">: Optionals.maybe
            (string "")
            (lambda "t" $ Strings.cat2 (string ":") (ref typeDef @@ var "t"))
            (var "mt")] $
          Strings.cat $ list [
            string "λ",
            var "v",
            var "typeStr",
            string ".",
            ref termDef @@ var "body"],
        _Function_primitive>>: lambda "name" $ Strings.cat2 (unwrap _Name @@ var "name") (string "!")],
      _Term_let>>: lambda "l" $ lets [
        "bindings">: Core.letBindings $ var "l",
        "env">: Core.letEnvironment $ var "l",
        "bindingStrs">: Lists.map (var "showBinding") (var "bindings")] $
        Strings.cat $ list [
          string "let ",
          Strings.intercalate (string ", ") (var "bindingStrs"),
          string " in ",
          ref termDef @@ var "env"],
      _Term_list>>: lambda "els" $ lets [
        "termStrs">: Lists.map (ref termDef) (var "els")] $
        Strings.cat $ list [
          string "[",
          Strings.intercalate (string ", ") (var "termStrs"),
          string "]"],
      _Term_literal>>: lambda "lit" $ ref literalDef @@ var "lit",
      _Term_map>>: lambda "m" $ lets [
        "entry">: lambda "p" $ Strings.cat $ list [
          ref termDef @@ (first $ var "p"),
          string "=",
          ref termDef @@ (second $ var "p")]] $
        Strings.cat $ list [
          string "{",
          Strings.intercalate (string ", ") $ Lists.map (var "entry") $ Maps.toList $ var "m",
          string "}"],
      _Term_optional>>: lambda "mt" $ Optionals.maybe
        (string "nothing")
        (lambda "t" $ Strings.cat $ list [
          string "just(",
          ref termDef @@ var "t",
          string ")"])
        (var "mt"),
      _Term_product>>: lambda "els" $ lets [
        "termStrs">: Lists.map (ref termDef) (var "els")] $
        Strings.cat $ list [
          string "(",
          Strings.intercalate (string ", ") (var "termStrs"),
          string ")"],
      _Term_record>>: lambda "rec" $ lets [
        "tname">: unwrap _Name @@ (Core.recordTypeName $ var "rec"),
        "fields">: Core.recordFields $ var "rec"] $
        Strings.cat $ list [
          string "record(",
          var "tname",
          string ")",
          var "showFields" @@ var "fields"],
      _Term_set>>: lambda "s" $
        Strings.cat $ list [
          string "{",
          Strings.intercalate (string ", ") (Lists.map (ref termDef) $ Sets.toList $ var "s"),
          string "}"],
      _Term_sum>>: lambda "s" $ lets [
        "index">: Core.sumIndex $ var "s",
        "size">: Core.sumSize $ var "s",
        "t2">: Core.sumTerm $ var "s"] $
        Strings.cat $ list [
          string "(",
          Literals.showInt32 $ var "index",
          string "/",
          Literals.showInt32 $ var "size",
          string "=",
          ref termDef @@ var "t2",
          string ")"],
      _Term_typeAbstraction>>: lambda "ta" $ lets [
        "param">: unwrap _Name @@ (Core.typeAbstractionParameter $ var "ta"),
        "body">: Core.typeAbstractionBody $ var "ta"] $
        Strings.cat $ list [
          string "Λ",
          var "param",
          string ".",
          ref termDef @@ var "body"],
      _Term_typeApplication>>: lambda "tt" $ lets [
        "t2">: Core.typedTermTerm $ var "tt",
        "typ">: Core.typedTermType $ var "tt"] $
        Strings.cat $ list [
          ref termDef @@ var "t2",
          string "⟨",
          ref typeDef @@ var "typ",
          string "⟩"],
      _Term_typed>>: lambda "tt" $ lets [
        "t2">: Core.typedTermTerm $ var "tt",
        "typ">: Core.typedTermType $ var "tt"] $
        Strings.cat $ list [
          string "(",
          ref termDef @@ var "t2",
          string " : ",
          ref typeDef @@ var "typ",
          string ")"],
      _Term_union>>: lambda "inj" $ lets [
        "tname">: unwrap _Name @@ (Core.injectionTypeName $ var "inj"),
        "f">: Core.injectionField $ var "inj"] $
        Strings.cat $ list [
          string "inject(",
          var "tname",
          string ")",
          var "showFields" @@ (list [var "f"])],
      _Term_variable>>: lambda "name" $ unwrap _Name @@ var "name",
      _Term_wrap>>: lambda "wt" $ lets [
        "tname">: unwrap _Name @@ (Core.wrappedTermTypeName $ var "wt"),
        "term1">: Core.wrappedTermObject $ var "wt"] $
        Strings.cat $ list [
          string "wrap(",
          var "tname",
          string "){",
          ref termDef @@ var "term1",
          string "}"]]

termVariantDef :: TElement (TermVariant -> String)
termVariantDef = showCoreDefinition "termVariant" $
  doc "Show a TermVariant as a string" $
  match _TermVariant Nothing [
    _TermVariant_annotated>>: constant $ string "annotated",
    _TermVariant_application>>: constant $ string "application",
    _TermVariant_function>>: constant $ string "function",
    _TermVariant_let>>: constant $ string "let",
    _TermVariant_list>>: constant $ string "list",
    _TermVariant_literal>>: constant $ string "literal",
    _TermVariant_map>>: constant $ string "map",
    _TermVariant_optional>>: constant $ string "optional",
    _TermVariant_product>>: constant $ string "product",
    _TermVariant_record>>: constant $ string "record",
    _TermVariant_set>>: constant $ string "set",
    _TermVariant_sum>>: constant $ string "sum",
    _TermVariant_typeAbstraction>>: constant $ string "typeAbstraction",
    _TermVariant_typeApplication>>: constant $ string "typeApplication",
    _TermVariant_typed>>: constant $ string "typed",
    _TermVariant_union>>: constant $ string "union",
    _TermVariant_variable>>: constant $ string "variable",
    _TermVariant_wrap>>: constant $ string "wrap"]

typeDef :: TElement (Type -> String)
typeDef = showCoreDefinition "type_" $
  doc "Show a type as a string" $
  lambda "typ" $ lets [
    "showFieldType">: lambda "ft" $ lets [
      "fname">: unwrap _Name @@ (Core.fieldTypeName $ var "ft"),
      "ftyp">: Core.fieldTypeType $ var "ft"] $
      Strings.cat $ list [
        var "fname",
        string " = ",
        ref typeDef @@ var "ftyp"],
    "showRowType">: lambda "rt" $ lets [
      "fields">: Core.rowTypeFields $ var "rt",
      "fieldStrs">: Lists.map (var "showFieldType") (var "fields")] $
      Strings.cat $ list [
        string "{",
        Strings.intercalate (string ", ") (var "fieldStrs"),
        string "}"],
    "gatherTypes">: lambdas ["prev", "app"] $ lets [
      "lhs">: Core.applicationTypeFunction $ var "app",
      "rhs">: Core.applicationTypeArgument $ var "app",
      "strippedLhs">: ref Strip.stripTypeDef @@ var "lhs"] $
      cases _Type (var "strippedLhs")
        (Just $ Lists.cons (var "strippedLhs") (Lists.cons (var "rhs") (var "prev"))) [
        _Type_application>>: lambda "app2" $ var "gatherTypes" @@ (Lists.cons (var "rhs") (var "prev")) @@ var "app2"],
    "gatherFunctionTypes">: lambdas ["prev", "t"] $
      cases _Type (ref Strip.stripTypeDef @@ var "t")
        (Just $ Lists.reverse $ Lists.cons (var "t") (var "prev")) [
          _Type_function>>: lambda "ft" $ lets [
            "dom">: Core.functionTypeDomain $ var "ft",
            "cod">: Core.functionTypeCodomain $ var "ft"] $
            var "gatherFunctionTypes" @@ (Lists.cons (var "dom") (var "prev")) @@ var "cod"]] $
    cases _Type (ref Strip.stripTypeDef @@ var "typ") Nothing [
      _Type_application>>: lambda "app" $ lets [
        "types">: var "gatherTypes" @@ (list []) @@ var "app",
        "typeStrs">: Lists.map (ref typeDef) (var "types")] $
        Strings.cat $ list [
          string "(",
          Strings.intercalate (string " @ ") (var "typeStrs"),
          string ")"],
      _Type_forall>>: lambda "ft" $ lets [
        "var">: unwrap _Name @@ (Core.forallTypeParameter $ var "ft"),
        "body">: Core.forallTypeBody $ var "ft"] $
        Strings.cat $ list [
          string "(∀",
          var "var",
          string ".",
          ref typeDef @@ var "body",
          string ")"],
      _Type_function>>: lambda "ft" $ lets [
        "types">: var "gatherFunctionTypes" @@ (list []) @@ var "typ",
        "typeStrs">: Lists.map (ref typeDef) (var "types")] $
        Strings.cat $ list [
          string "(",
          Strings.intercalate (string " → ") (var "typeStrs"),
          string ")"],
      _Type_list>>: lambda "etyp" $ Strings.cat $ list [
        string "list<",
        ref typeDef @@ var "etyp",
        string ">"],
      _Type_literal>>: lambda "lt" $ ref literalTypeDef @@ var "lt",
      _Type_map>>: lambda "mt" $ lets [
        "keyTyp">: Core.mapTypeKeys $ var "mt",
        "valTyp">: Core.mapTypeValues $ var "mt"] $
        Strings.cat $ list [
          string "map<",
          ref typeDef @@ var "keyTyp",
          string ", ",
          ref typeDef @@ var "valTyp",
          string ">"],
      _Type_optional>>: lambda "etyp" $ Strings.cat $ list [
        string "optional<",
        ref typeDef @@ var "etyp",
        string ">"],
      _Type_product>>: lambda "types" $ lets [
        "typeStrs">: Lists.map (ref typeDef) (var "types")] $
        Strings.intercalate (string "×") (var "typeStrs"),
      _Type_record>>: lambda "rt" $ Strings.cat2 (string "record") (var "showRowType" @@ var "rt"),
      _Type_set>>: lambda "etyp" $ Strings.cat $ list [
        string "set<",
        ref typeDef @@ var "etyp",
        string ">"],
      _Type_sum>>: lambda "types" $ lets [
        "typeStrs">: Lists.map (ref typeDef) (var "types")] $
        Strings.intercalate (string "+") (var "typeStrs"),
      _Type_union>>: lambda "rt" $ Strings.cat2 (string "union") (var "showRowType" @@ var "rt"),
      _Type_variable>>: lambda "name" $ unwrap _Name @@ var "name",
      _Type_wrap>>: lambda "wt" $ lets [
        "tname">: unwrap _Name @@ (Core.wrappedTypeTypeName $ var "wt"),
        "typ1">: Core.wrappedTypeObject $ var "wt"] $
        Strings.cat $ list [
          string "wrap[",
          var "tname",
          string "](",
          ref typeDef @@ var "typ1",
          string ")"]]

typeConstraintDef :: TElement (TypeConstraint -> String)
typeConstraintDef = showCoreDefinition "typeConstraint" $
  doc "Show a type constraint as a string" $
  lambda "tc" $ lets [
    "ltyp">: Typing.typeConstraintLeft $ var "tc",
    "rtyp">: Typing.typeConstraintRight $ var "tc"] $
    Strings.cat $ list [
      ref typeDef @@ var "ltyp",
      string "≡",
      ref typeDef @@ var "rtyp"]

typeSchemeDef :: TElement (TypeScheme -> String)
typeSchemeDef = showCoreDefinition "typeScheme" $
  doc "Show a type scheme as a string" $
  lambda "ts" $ lets [
    "vars">: Core.typeSchemeVariables $ var "ts",
    "body">: Core.typeSchemeType $ var "ts",
    "varNames">: Lists.map (unwrap _Name) (var "vars"),
    "fa">: Logic.ifElse (Lists.null $ var "vars")
      (string "")
      (Strings.cat $ list [
        string "∀[",
        Strings.intercalate (string ",") (var "varNames"),
        string "]."])] $
    Strings.cat $ list [
      string "(",
      var "fa",
      ref typeDef @@ var "body",
      string ")"]

typeSubstDef :: TElement (TypeSubst -> String)
typeSubstDef = showCoreDefinition "typeSubst" $
  doc "Show a type substitution as a string" $
  lambda "ts" $ lets [
    "subst">: Typing.unTypeSubst $ var "ts",
    "pairs">: Maps.toList $ var "subst",
    "showPair">: lambda "pair" $ lets [
      "name">: unwrap _Name @@ (first $ var "pair"),
      "typ">: second $ var "pair"] $
      Strings.cat $ list [
        var "name",
        string "↦",
        ref typeDef @@ var "typ"],
    "pairStrs">: Lists.map (var "showPair") (var "pairs")] $
    Strings.cat $ list [
      string "{",
      Strings.intercalate (string ",") (var "pairStrs"),
      string "}"]
