{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Json.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors                        as Accessors
import qualified Hydra.Dsl.Annotations                      as Anns
import qualified Hydra.Dsl.Ast                              as Ast
import qualified Hydra.Dsl.Coders                           as Coders
import qualified Hydra.Dsl.Compute                          as Compute
import qualified Hydra.Dsl.Core                             as Core
import qualified Hydra.Dsl.Grammar                          as Grammar
import qualified Hydra.Dsl.Graph                            as Graph
import qualified Hydra.Dsl.Json                             as Json
import qualified Hydra.Dsl.Lib.Chars                        as Chars
import qualified Hydra.Dsl.Lib.Equality                     as Equality
import qualified Hydra.Dsl.Lib.Flows                        as Flows
import qualified Hydra.Dsl.Lib.Lists                        as Lists
import qualified Hydra.Dsl.Lib.Literals                     as Literals
import qualified Hydra.Dsl.Lib.Logic                        as Logic
import qualified Hydra.Dsl.Lib.Maps                         as Maps
import qualified Hydra.Dsl.Lib.Math                         as Math
import qualified Hydra.Dsl.Lib.Optionals                    as Optionals
import qualified Hydra.Dsl.Lib.Sets                         as Sets
import           Hydra.Dsl.Lib.Strings                      as Strings
import qualified Hydra.Dsl.Mantle                           as Mantle
import qualified Hydra.Dsl.Module                           as Module
import           Hydra.Dsl.Phantoms                         as Phantoms
import qualified Hydra.Dsl.TTerms                           as TTerms
import qualified Hydra.Dsl.TTypes                           as TTypes
import qualified Hydra.Dsl.Tabular                          as Tabular
import qualified Hydra.Dsl.Terms                            as Terms
import qualified Hydra.Dsl.Topology                         as Topology
import qualified Hydra.Dsl.Types                            as Types
import qualified Hydra.Dsl.Typing                           as Typing
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Decode.Core     as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Decoding        as Decoding
import qualified Hydra.Sources.Kernel.Terms.Describe.Core   as DescribeCore
import qualified Hydra.Sources.Kernel.Terms.Describe.Mantle as DescribeMantle
import qualified Hydra.Sources.Kernel.Terms.Encode.Core     as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Mantle  as ExtractMantle
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors  as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Mantle     as ShowMantle
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
import qualified Hydra.Sources.Kernel.Terms.Variants        as Variants
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y

import Hydra.Json
import qualified Hydra.Sources.Json.Language as JsonLanguage
import qualified Hydra.Sources.Kernel.Terms.Literals as HydraLiterals


module_ :: Module
module_ = Module (Namespace "hydra.ext.org.json.coder") elements
    [AdaptModules.module_, AdaptTerms.module_, AdaptUtils.module_, EncodeCore.module_,
     ExtractCore.module_, HydraLiterals.module_, Monads.module_, Rewriting.module_, JsonLanguage.module_,
     Variants.module_]
    KernelTypes.kernelTypesModules $
    Just "JSON encoding and decoding for Hydra terms"
  where
    elements = [
      el jsonCoderDef,
      el literalJsonCoderDef,
      el recordCoderDef,
      el encodeRecordDef,
      el decodeRecordDef,
      el termCoderDef,
      el unitCoderDef,
      el untypedTermToJsonDef,
      el readStringStubDef,
      el showValueDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

jsonCoderDef :: TBinding (Type -> Flow Graph (Coder Graph Graph Term Value))
jsonCoderDef = define "jsonCoder" $
  doc "Create a JSON coder for a given type" $
  lambda "typ" $ binds [
    "adapter">: ref AdaptModules.languageAdapterDef @@ ref JsonLanguage.jsonLanguageDef @@ var "typ",
    "coder">: ref termCoderDef @@ (Compute.adapterTarget $ var "adapter")] $
    produce $ ref AdaptUtils.composeCodersDef @@ (Compute.adapterCoder $ var "adapter") @@ var "coder"

literalJsonCoderDef :: TBinding (LiteralType -> Flow Graph (Coder Graph Graph Literal Value))
literalJsonCoderDef = define "literalJsonCoder" $
  doc "Create a JSON coder for literal types" $
  lambda "lt" $ produce $ cases _LiteralType (var "lt") Nothing [
    _LiteralType_boolean>>: constant $ Compute.coder
      (lambda "lit" $ binds [
        "b">: ref ExtractCore.booleanLiteralDef @@ var "lit"] $
        produce $ Json.valueBoolean $ var "b")
      (lambda "s" $ cases _Value (var "s")
        (Just $ ref Monads.unexpectedDef @@ string "boolean" @@ (ref showValueDef @@ var "s")) [
        _Value_boolean>>: lambda "b" $ produce $ Core.literalBoolean $ var "b"]),
    _LiteralType_float>>: constant $ Compute.coder
      (lambda "lit" $ binds [
        "f">: ref ExtractCore.floatLiteralDef @@ var "lit",
        "bf">: ref ExtractCore.bigfloatValueDef @@ var "f"] $
        produce $ Json.valueNumber $ var "bf")
      (lambda "s" $ cases _Value (var "s")
        (Just $ ref Monads.unexpectedDef @@ string "number" @@ (ref showValueDef @@ var "s")) [
        _Value_number>>: lambda "f" $ produce $ Core.literalFloat $ Core.floatValueBigfloat $ var "f"]),
    _LiteralType_integer>>: constant $ Compute.coder
      (lambda "lit" $ binds [
        "i">: ref ExtractCore.integerLiteralDef @@ var "lit",
        "bi">: ref ExtractCore.bigintValueDef @@ var "i"] $
        produce $ Json.valueNumber $ Literals.bigintToBigfloat $ var "bi")
      (lambda "s" $ cases _Value (var "s")
        (Just $ ref Monads.unexpectedDef @@ string "number" @@ (ref showValueDef @@ var "s")) [
        _Value_number>>: lambda "f" $ lets [
          "bi">: Literals.bigfloatToBigint $ var "f"] $
          produce $ Core.literalInteger $ Core.integerValueBigint $ var "bi"]),
    _LiteralType_string>>: constant $ Compute.coder
      (lambda "lit" $ binds [
        "s">: ref ExtractCore.stringLiteralDef @@ var "lit"] $
        produce $ Json.valueString $ var "s")
      (lambda "s" $ cases _Value (var "s")
        (Just $ ref Monads.unexpectedDef @@ string "string" @@ (ref showValueDef @@ var "s")) [
        _Value_string>>: lambda "s'" $ produce $ Core.literalString $ var "s'"])]

recordCoderDef :: TBinding (RowType -> Flow Graph (Coder Graph Graph Term Value))
recordCoderDef = define "recordCoder" $
  doc "Create a JSON coder for record types" $
  lambda "rt" $ lets [
    "fields">: Core.rowTypeFields $ var "rt",
    "getCoder">: lambda "f" $ binds [
      "coder">: ref termCoderDef @@ (Core.fieldTypeType $ var "f")] $
      produce $ pair (var "f") (var "coder")] $ binds [
    "coders">: Flows.mapList (var "getCoder") (var "fields")] $
    produce $ Compute.coder (ref encodeRecordDef @@ var "coders") (ref decodeRecordDef @@ var "rt" @@ var "coders")

encodeRecordDef :: TBinding ([(FieldType, Coder Graph Graph Term Value)] -> Term -> Flow Graph Value)
encodeRecordDef = define "encodeRecord" $
  doc "Encode a record term to JSON" $
  lambdas ["coders", "term"] $ lets [
    "stripped">: ref Rewriting.deannotateTermDef @@ var "term"] $ binds [
    "record">: ref ExtractCore.termRecordDef @@ var "stripped"] $ lets [
    "fields">: Core.recordFields $ var "record",
    "encodeField">: lambda "coderAndField" $ lets [
      "coder">: first $ var "coderAndField",
      "field">: second $ var "coderAndField",
      "ft">: first $ var "coder",
      "coder'">: second $ var "coder",
      "fname">: Core.fieldName $ var "field",
      "fvalue">: Core.fieldTerm $ var "field"] $
      cases _Type (Core.fieldTypeType $ var "ft")
        (Just $ binds [
          "encoded">: Compute.coderEncode (var "coder'") @@ var "fvalue"] $
          produce $ just $ pair (Core.unName $ var "fname") (var "encoded")) [
        _Type_optional>>: lambda "ot" $ cases _Term (var "fvalue")
          (Just $ binds [
            "encoded">: Compute.coderEncode (var "coder'") @@ var "fvalue"] $
            produce $ just $ pair (Core.unName $ var "fname") (var "encoded")) [
          _Term_optional>>: lambda "opt" $ Optionals.maybe
            (produce nothing)
            (lambda "v" $ binds [
              "encoded">: Compute.coderEncode (var "coder'") @@ var "v"] $
              produce $ just $ pair (Core.unName $ var "fname") (var "encoded"))
            (var "opt")]]] $ binds [
    "maybeFields">: Flows.mapList (var "encodeField") (Lists.zip (var "coders") (var "fields"))] $
    produce $ Json.valueObject $ Maps.fromList $ Optionals.cat $ var "maybeFields"

decodeRecordDef :: TBinding (RowType -> [(FieldType, Coder Graph Graph Term Value)] -> Value -> Flow Graph Term)
decodeRecordDef = define "decodeRecord" $
  doc "Decode a JSON value to a record term" $
  lambdas ["rt", "coders", "n"] $ cases _Value (var "n")
    (Just $ ref Monads.unexpectedDef @@ string "object" @@ (ref showValueDef @@ var "n")) [
    _Value_object>>: lambda "m" $ lets [
      "decodeField">: lambda "coder" $ lets [
        "ft">: first $ var "coder",
        "coder'">: second $ var "coder",
        "fname">: Core.fieldTypeName $ var "ft",
        "defaultValue">: Json.valueNull,
        "jsonValue">: Optionals.fromMaybe (var "defaultValue") $ Maps.lookup (Core.unName $ var "fname") (var "m")] $ binds [
        "v">: Compute.coderDecode (var "coder'") @@ var "jsonValue"] $
        produce $ Core.field (var "fname") (var "v")] $ binds [
      "fields">: Flows.mapList (var "decodeField") (var "coders")] $
      produce $ Core.termRecord $ Core.record (Core.rowTypeTypeName $ var "rt") (var "fields")]

termCoderDef :: TBinding (Type -> Flow Graph (Coder Graph Graph Term Value))
termCoderDef = define "termCoder" $
  doc "Create a JSON coder for term types" $
  lambda "typ" $ lets [
    "stripped">: ref Rewriting.deannotateTypeDef @@ var "typ"] $
    cases _Type (var "stripped")
      (Just $ Flows.fail $ Strings.cat $ list [
        string "unsupported type in JSON: ",
        ref ShowCore.typeDef @@ var "typ"]) [
      _Type_literal>>: lambda "at" $ binds [
        "ac">: ref literalJsonCoderDef @@ var "at"] $
        produce $ Compute.coder
          (lambda "term" $ cases _Term (var "term")
            (Just $ ref Monads.unexpectedDef @@ string "literal term" @@ (ref ShowCore.termDef @@ var "term")) [
            _Term_literal>>: lambda "av" $ Compute.coderEncode (var "ac") @@ var "av"])
          (lambda "n" $ binds [
            "lit">: Compute.coderDecode (var "ac") @@ var "n"] $
            produce $ Core.termLiteral $ var "lit"),
      _Type_list>>: lambda "lt" $ binds [
        "lc">: ref termCoderDef @@ var "lt"] $
        produce $ Compute.coder
          (lambda "term" $ cases _Term (var "term")
            (Just $ ref Monads.unexpectedDef @@ string "list term" @@ (ref ShowCore.termDef @@ var "term")) [
            _Term_list>>: lambda "els" $ binds [
              "encodedEls">: Flows.mapList (Compute.coderEncode $ var "lc") (var "els")] $
              produce $ Json.valueArray $ var "encodedEls"])
          (lambda "n" $ cases _Value (var "n")
            (Just $ ref Monads.unexpectedDef @@ string "sequence" @@ (ref showValueDef @@ var "n")) [
            _Value_array>>: lambda "nodes" $ binds [
              "decodedNodes">: Flows.mapList (Compute.coderDecode $ var "lc") (var "nodes")] $
              produce $ Core.termList $ var "decodedNodes"]),
      _Type_map>>: lambda "mt" $ lets [
        "kt">: Core.mapTypeKeys $ var "mt",
        "vt">: Core.mapTypeValues $ var "mt"] $ binds [
        "kc">: ref termCoderDef @@ var "kt",
        "vc">: ref termCoderDef @@ var "vt",
        "cx">: ref Monads.getStateDef] $ lets [
        "isStringKey">: Equality.equal (ref Rewriting.deannotateTypeDef @@ var "kt") TTypes.string,
        "toString">: lambda "v" $ Logic.ifElse (var "isStringKey")
          (cases _Term (ref Rewriting.deannotateTermDef @@ var "v")
            (Just $ ref ShowCore.termDef @@ var "v") [
            _Term_literal>>: lambda "lit" $ cases _Literal (var "lit")
              (Just $ ref ShowCore.termDef @@ var "v") [
              _Literal_string>>: lambda "s" $ var "s"]])
          (ref ShowCore.termDef @@ var "v"),
        "fromString">: lambda "s" $ Logic.ifElse (var "isStringKey")
          (Core.termLiteral $ Core.literalString $ var "s")
          (ref readStringStubDef @@ var "s"),
        "encodeEntry">: lambda "kv" $ lets [
          "k">: first $ var "kv",
          "v">: second $ var "kv"] $ binds [
          "encodedV">: Compute.coderEncode (var "vc") @@ var "v"] $
          produce $ pair (var "toString" @@ var "k") (var "encodedV"),
        "decodeEntry">: lambda "kv" $ lets [
          "k">: first $ var "kv",
          "v">: second $ var "kv"] $ binds [
          "decodedV">: Compute.coderDecode (var "vc") @@ var "v"] $
          produce $ pair (var "fromString" @@ var "k") (var "decodedV")] $
        produce $ Compute.coder
          (lambda "term" $ cases _Term (var "term")
            (Just $ ref Monads.unexpectedDef @@ string "map term" @@ (ref ShowCore.termDef @@ var "term")) [
            _Term_map>>: lambda "m" $ binds [
              "entries">: Flows.mapList (var "encodeEntry") $ Maps.toList $ var "m"] $
              produce $ Json.valueObject $ Maps.fromList $ var "entries"])
          (lambda "n" $ cases _Value (var "n")
            (Just $ ref Monads.unexpectedDef @@ string "mapping" @@ (ref showValueDef @@ var "n")) [
            _Value_object>>: lambda "m" $ binds [
              "entries">: Flows.mapList (var "decodeEntry") $ Maps.toList $ var "m"] $
              produce $ Core.termMap $ Maps.fromList $ var "entries"]),
      _Type_optional>>: lambda "ot" $ binds [
        "oc">: ref termCoderDef @@ var "ot"] $
        produce $ Compute.coder
          (lambda "t" $ lets [
            "stripped">: ref Rewriting.deannotateTermDef @@ var "t"] $
            cases _Term (var "stripped")
              (Just $ ref Monads.unexpectedDef @@ string "optional term" @@ (ref ShowCore.termDef @@ var "t")) [
              _Term_optional>>: lambda "el" $ Optionals.maybe
                (produce Json.valueNull)
                (Compute.coderEncode $ var "oc")
                (var "el")])
          (lambda "n" $ cases _Value (var "n")
            (Just $ binds [
              "decoded">: Compute.coderDecode (var "oc") @@ var "n"] $
              produce $ Core.termOptional $ just $ var "decoded") [
            _Value_null>>: constant $ produce $ Core.termOptional nothing]),
      _Type_record>>: lambda "rt" $ ref recordCoderDef @@ var "rt",
      _Type_unit>>: constant $ produce $ ref unitCoderDef,
      _Type_variable>>: lambda "name" $ produce $ Compute.coder
        (lambda "term" $ produce $ Json.valueString $ Strings.cat $ list [
          string "variable '",
          Core.unName $ var "name",
          string "' for: ",
          ref ShowCore.termDef @@ var "term"])
        (lambda "term" $ Flows.fail $ Strings.cat $ list [
          string "type variable ",
          Core.unName $ var "name",
          string " does not support decoding"])]

unitCoderDef :: TBinding (Coder Graph Graph Term Value)
unitCoderDef = define "unitCoder" $
  doc "JSON coder for unit values" $
  Compute.coder
    (lambda "term" $ cases _Term (ref Rewriting.deannotateTermDef @@ var "term")
      (Just $ ref Monads.unexpectedDef @@ string "unit" @@ (ref ShowCore.termDef @@ var "term")) [
      _Term_unit>>: constant $ produce Json.valueNull])
    (lambda "n" $ cases _Value (var "n")
      (Just $ ref Monads.unexpectedDef @@ string "null" @@ (ref showValueDef @@ var "n")) [
      _Value_null>>: constant $ produce Core.termUnit])

untypedTermToJsonDef :: TBinding (Term -> Flow s Value)
untypedTermToJsonDef = define "untypedTermToJson" $
  doc "A simplistic, unidirectional encoding for terms as JSON values. Not type-aware; best used for human consumption." $
  lambda "term" $ lets [
    "unexp">: lambda "msg" $ produce $ Json.valueString $ Strings.cat2 (string "FAIL: ") (var "msg"),
    "asRecord">: lambda "fields" $ ref untypedTermToJsonDef @@ (Core.termRecord $ Core.record (Core.name $ string "") (var "fields")),
    "asVariant">: lambdas ["name", "term"] $ ref untypedTermToJsonDef @@
      (Core.termUnion $ Core.injection (Core.name $ string "") $ Core.field (Core.name $ var "name") (var "term")),
    "fieldToKeyval">: lambda "f" $ lets [
      "forTerm">: lambda "t" $ cases _Term (var "t")
        (Just $ Flows.map (unaryFunction just) $ ref untypedTermToJsonDef @@ var "t") [
        _Term_optional>>: lambda "mt" $ Optionals.maybe
          (produce nothing)
          (var "forTerm")
          (var "mt")]] $ binds [
      "mjson">: var "forTerm" @@ (Core.fieldTerm $ var "f")] $
      produce $ Optionals.map
        (lambda "j" $ pair (Core.unName $ Core.fieldName $ var "f") (var "j"))
        (var "mjson")] $
    cases _Term (var "term")
      (Just $ var "unexp" @@ (Strings.cat $ list [
        string "unsupported term variant: ",
        ref ShowCore.termDef @@ var "term"])) [
      _Term_annotated>>: lambda "at" $ lets [
        "term1">: Core.annotatedTermSubject $ var "at",
        "ann">: Core.annotatedTermAnnotation $ var "at",
        "encodePair">: lambda "kv" $ lets [
          "k">: Core.unName $ first $ var "kv",
          "v">: second $ var "kv"] $ binds [
          "json">: ref untypedTermToJsonDef @@ var "v"] $
          produce $ pair (var "k") (var "json")] $ binds [
        "json">: ref untypedTermToJsonDef @@ var "term1",
        "pairs">: Flows.mapList (var "encodePair") $ Maps.toList $ var "ann"] $
        produce $ Json.valueObject $ Maps.fromList $ list [
          pair (string "term") (var "json"),
          pair (string "annotations") (Json.valueObject $ Maps.fromList $ var "pairs")],
      _Term_application>>: lambda "app" $ var "asRecord" @@ list [
        Core.field (Core.name $ string "function") (Core.applicationFunction $ var "app"),
        Core.field (Core.name $ string "argument") (Core.applicationArgument $ var "app")],
      _Term_function>>: lambda "f" $ cases _Function (var "f") Nothing [
        _Function_elimination>>: lambda "elm" $ cases _Elimination (var "elm")
          (Just $ var "unexp" @@ (Strings.cat $ list [
            string "unexpected elimination variant: ",
            ref ShowCore.eliminationDef @@ var "elm"])) [
          _Elimination_record>>: lambda "proj" $ var "asVariant" @@ string "project" @@
            (Core.termVariable $ Core.projectionField $ var "proj")],
        _Function_lambda>>: lambda "l" $ var "asRecord" @@ list [
          Core.field (Core.name $ string "parameter") (Core.termVariable $ Core.lambdaParameter $ var "l"),
          Core.field (Core.name $ string "domain") (Core.termOptional $
            Optionals.map (ref EncodeCore.typeDef) (Core.lambdaDomain $ var "l")),
          Core.field (Core.name $ string "body") (Core.lambdaBody $ var "l")],
        _Function_primitive>>: lambda "name" $ produce $ Json.valueString $ Core.unName $ var "name"],
      _Term_let>>: lambda "lt" $ lets [
        "bindings">: Core.letBindings $ var "lt",
        "env">: Core.letBody $ var "lt",
        "fromBinding">: lambda "b" $ Core.field
          (Core.bindingName $ var "b")
          (Core.bindingTerm $ var "b")] $
        var "asRecord" @@ list [
          Core.field (Core.name $ string "bindings") (Core.termRecord $ Core.record
            (Core.name $ string "")
            (Lists.map (var "fromBinding") (var "bindings"))),
          Core.field (Core.name $ string "environment") (var "env")],
      _Term_list>>: lambda "terms" $ binds [
        "jsonTerms">: Flows.mapList (ref untypedTermToJsonDef) (var "terms")] $
        produce $ Json.valueArray $ var "jsonTerms",
      _Term_literal>>: lambda "lit" $ produce $ cases _Literal (var "lit") Nothing [
        _Literal_binary>>: lambda "b" $ Json.valueString $ Literals.binaryToString $ var "b",
        _Literal_boolean>>: lambda "b" $ Json.valueBoolean $ var "b",
        _Literal_float>>: lambda "f" $ Json.valueNumber $ ref HydraLiterals.floatValueToBigfloatDef @@ var "f",
        _Literal_integer>>: lambda "i" $ lets [
          "bf">: ref HydraLiterals.integerValueToBigintDef @@ var "i",
          "f">: Literals.bigintToBigfloat $ var "bf"] $
          Json.valueNumber $ var "f",
        _Literal_string>>: lambda "s" $ Json.valueString $ var "s"],
      _Term_optional>>: lambda "mt" $ Optionals.maybe
        (produce Json.valueNull)
        (ref untypedTermToJsonDef)
        (var "mt"),
      _Term_product>>: lambda "els" $ ref untypedTermToJsonDef @@ (Core.termList $ var "els"),
      _Term_record>>: lambda "r" $ lets [
        "fields">: Core.recordFields $ var "r"] $ binds [
        "keyvals">: Flows.mapList (var "fieldToKeyval") (var "fields")] $
        produce $ Json.valueObject $ Maps.fromList $ Optionals.cat $ var "keyvals",
      _Term_set>>: lambda "vals" $ ref untypedTermToJsonDef @@ (Core.termList $ Sets.toList $ var "vals"),
      _Term_sum>>: lambda "s" $ var "asRecord" @@ list [
        Core.field (Core.name $ string "index") (Core.termLiteral $ Core.literalInteger $
          Core.integerValueInt32 $ Core.sumIndex $ var "s"),
        Core.field (Core.name $ string "size") (Core.termLiteral $ Core.literalInteger $
          Core.integerValueInt32 $ Core.sumSize $ var "s"),
        Core.field (Core.name $ string "term") (Core.sumTerm $ var "s")],
      _Term_typeLambda>>: lambda "ta" $ var "asRecord" @@ list [
        Core.field (Core.name $ string "parameter") (Core.termVariable $ Core.typeLambdaParameter $ var "ta"),
        Core.field (Core.name $ string "body") (Core.typeLambdaBody $ var "ta")],
      _Term_typeApplication>>: lambda "tt" $ var "asRecord" @@ list [
        Core.field (Core.name $ string "term") (Core.typedTermTerm $ var "tt"),
        Core.field (Core.name $ string "type") (ref EncodeCore.typeDef @@ (Core.typedTermType $ var "tt"))],
      _Term_union>>: lambda "i" $ lets [
        "field">: Core.injectionField $ var "i"] $
        Logic.ifElse (Equality.equal (Core.fieldTerm $ var "field") Core.termUnit)
          (produce $ Json.valueString $ Core.unName $ Core.fieldName $ var "field")
          (binds [
            "mkeyval">: var "fieldToKeyval" @@ var "field"] $
            produce $ Json.valueObject $ Maps.fromList $ Optionals.maybe
              (list [])
              (lambda "keyval" $ list [var "keyval"])
              (var "mkeyval")),
      _Term_variable>>: lambda "v" $ produce $ Json.valueString $ Core.unName $ var "v",
      _Term_wrap>>: lambda "wt" $ ref untypedTermToJsonDef @@ (Core.wrappedTermObject $ var "wt")]

readStringStubDef :: TBinding (String -> Term)
readStringStubDef = define "readStringStub" $
  doc "Placeholder for reading a string into a term (to be implemented)" $
  lambda "s" $ Core.termLiteral $ Core.literalString $ Strings.cat2 (string "TODO: read ") (var "s")

-- TODO: implement this function, and deduplicate with hydra.json.coder.showValue
showValueDef :: TBinding (Value -> String)
showValueDef = define "showValue" $
  doc "Show a JSON value as a string (placeholder implementation)" $
  lambda "value" $ string "TODO: implement showValue"
