module Hydra.Sources.Shacl.Coder where

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
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Errors                      as Error
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
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
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
import qualified Hydra.Rdf.Syntax as Rdf
import qualified Hydra.Shacl.Model as Shacl
import qualified Hydra.Sources.Shacl.Model as ShaclModel
import qualified Hydra.Sources.Rdf.Syntax as RdfSyntax
import qualified Hydra.Sources.Rdf.Utils as RdfUtils


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.shacl.coder"

module_ :: Module
module_ = Module ns definitions
    [Names.ns, Strip.ns, Annotations.ns, moduleNamespace DecodeCore.module_, ExtractCore.ns, Formatting.ns, Lexical.ns, RdfUtils.ns]
    (ShaclModel.ns:RdfSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "SHACL coder: converts Hydra types and terms to SHACL shapes and RDF descriptions"
  where
    definitions = [
      toDefinition err,
      toDefinition unexpectedE,
      toDefinition shaclCoder,
      toDefinition common,
      toDefinition defaultCommonProperties,
      toDefinition elementIri,
      toDefinition encodeField,
      toDefinition encodeFieldType,
      toDefinition encodeLiteralType,
      toDefinition encodeTerm,
      toDefinition encodeList,
      toDefinition foldAccumResult,
      toDefinition encodeType,
      toDefinition node,
      toDefinition property,
      toDefinition withType]


-- | Construct a Left (InContext Error) error
err :: TTermDefinition (Context -> String -> Either Error a)
err = define "err" $
  doc "Construct an error result with a context and message" $
  lambda "cx" $ lambda "msg" $
    left (Error.errorOther $ Error.otherError (var "msg"))

-- | Construct an 'expected X, found Y' error
unexpectedE :: TTermDefinition (Context -> String -> String -> Either Error a)
unexpectedE = define "unexpectedE" $
  doc "Construct an error for unexpected input, given expected and found descriptions" $
  lambda "cx" $ lambda "expected" $ lambda "found" $
    err @@ var "cx" @@ (Strings.cat $ list [
      string "Expected ",
      var "expected",
      string ", found: ",
      var "found"])

-- | Main SHACL coder: encode a module's type elements into a ShapesGraph
shaclCoder :: TTermDefinition (Module -> Context -> Graph -> Either Error (Shacl.ShapesGraph, Context))
shaclCoder = define "shaclCoder" $
  doc "Encode a module's type elements as a SHACL ShapesGraph" $
  lambda "mod" $ lambda "cx" $ lambda "g" $ lets [
    "typeEls">: Maybes.cat (Lists.map
      ("d" ~> cases _Definition (var "d") (Just nothing) [
        _Definition_type>>: "td" ~>
          just (Annotations.typeBinding @@ (Packaging.typeDefinitionName $ var "td") @@ (Core.typeSchemeType $ Packaging.typeDefinitionType $ var "td"))])
      (Packaging.moduleDefinitions (var "mod"))),
    "toShape">: lambda "el" $
      Eithers.bind
        (Eithers.bimap
          ("__de" ~> Error.errorOther (Error.otherError ((unwrap _DecodingError) @@ var "__de")))
          ("__t" ~> var "__t")
          (Phantoms.decoderFor _Type @@ var "g" @@ (Core.bindingTerm (var "el"))))
        ("__typ" ~> Eithers.map
          ("__cp" ~> record Shacl._Definition [
            Shacl._Definition_iri>>: elementIri @@ var "el",
            Shacl._Definition_target>>: inject Shacl._Shape Shacl._Shape_node
              (record Shacl._NodeShape [Shacl._NodeShape_common>>: var "__cp"])])
          (encodeType @@ (Core.bindingName (var "el")) @@ var "__typ" @@ var "cx"))] $
    Eithers.map
      ("__shapes" ~> pair
        (wrap Shacl._ShapesGraph (Sets.fromList (var "__shapes")))
        (var "cx"))
      (Eithers.mapList (var "toShape") (var "typeEls"))

-- | Construct CommonProperties with the given constraints and defaults for everything else
common :: TTermDefinition ([Shacl.CommonConstraint] -> Shacl.CommonProperties)
common = define "common" $
  doc "Construct CommonProperties from a list of constraints, using defaults for other fields" $
  lambda "constraints" $
    record Shacl._CommonProperties [
      Shacl._CommonProperties_constraints>>: Sets.fromList (var "constraints"),
      Shacl._CommonProperties_deactivated>>: nothing,
      Shacl._CommonProperties_message>>: wrap Rdf._LangStrings Maps.empty,
      Shacl._CommonProperties_severity>>: inject Shacl._Severity Shacl._Severity_info unit,
      Shacl._CommonProperties_targetClass>>: Sets.empty,
      Shacl._CommonProperties_targetNode>>: Sets.empty,
      Shacl._CommonProperties_targetObjectsOf>>: Sets.empty,
      Shacl._CommonProperties_targetSubjectsOf>>: Sets.empty]

-- | Default (empty) CommonProperties
defaultCommonProperties :: TTermDefinition Shacl.CommonProperties
defaultCommonProperties = define "defaultCommonProperties" $
  doc "Default CommonProperties with empty constraints and default severity" $
  common @@ (list ([] :: [TTerm Shacl.CommonConstraint]))

-- | Convert a Binding's name to an RDF IRI
elementIri :: TTermDefinition (Binding -> Rdf.Iri)
elementIri = define "elementIri" $
  doc "Convert a binding's name to an RDF IRI" $
  lambda "el" $
    nameToIri @@ (Core.bindingName (var "el"))

-- | Encode a record field as RDF triples
encodeField :: TTermDefinition (Name -> Rdf.Resource -> Field -> Context -> Graph -> Either Error ([Rdf.Triple], Context))
encodeField = define "encodeField" $
  doc "Encode a record field as RDF triples with a given subject" $
  lambda "rname" $ lambda "subject" $ lambda "field" $ lambda "cx" $ lambda "g" $ lets [
    "pair1">: nextBlankNode @@ var "cx",
    "node">: Pairs.first (var "pair1"),
    "cx1">: Pairs.second (var "pair1")] $
    Eithers.bind
      (encodeTerm @@ var "node" @@ (Core.fieldTerm (var "field")) @@ var "cx1" @@ var "g")
      ("__r1" ~> lets [
        "descs">: Pairs.first (var "__r1"),
        "cx2">: Pairs.second (var "__r1")] $
        right (pair
          (Lists.concat2
            (triplesOf @@ var "descs")
            (forObjects @@ var "subject"
              @@ (propertyIri @@ var "rname" @@ (Core.fieldName (var "field")))
              @@ (subjectsOf @@ var "descs")))
          (var "cx2")))

-- | Encode a FieldType as a SHACL property shape definition
encodeFieldType :: TTermDefinition (Name -> Maybe Integer -> FieldType -> Context -> Either Error (Shacl.Definition Shacl.PropertyShape))
encodeFieldType = define "encodeFieldType" $
  doc "Encode a FieldType as a SHACL property shape Definition" $
  lambda "rname" $ lambda "order" $ lambda "ft" $ lambda "cx" $ lets [
    "fname">: Core.fieldTypeName (var "ft"),
    "ftype">: Core.fieldTypeType (var "ft"),
    "iri">: propertyIri @@ var "rname" @@ var "fname",
    "forType">: lambda "mn" $ lambda "mx" $ lambda "t" $
      cases _Type (Strip.deannotateType @@ var "t") (Just (var "forTypeDefault" @@ var "mn" @@ var "mx" @@ var "t")) [
        _Type_maybe>>: lambda "ot" $ var "forType" @@ (just (bigint 0)) @@ var "mx" @@ var "ot",
        _Type_set>>: lambda "st" $ var "forType" @@ var "mn" @@ nothing @@ var "st"],
    -- Default case: build property shape
    "forTypeDefault">: lambda "mn" $ lambda "mx" $ lambda "t" $
      Eithers.map
        ("__cp" ~> lets [
          "baseProp">: property @@ var "iri",
          "minC">: Maybes.map
            ("__n" ~> inject Shacl._PropertyShapeConstraint Shacl._PropertyShapeConstraint_minCount (var "__n"))
            (var "mn"),
          "maxC">: Maybes.map
            ("__n" ~> inject Shacl._PropertyShapeConstraint Shacl._PropertyShapeConstraint_maxCount (var "__n"))
            (var "mx")] $
          record Shacl._Definition [
            Shacl._Definition_iri>>: var "iri",
            Shacl._Definition_target>>:
              record Shacl._PropertyShape [
                Shacl._PropertyShape_common>>: var "__cp",
                Shacl._PropertyShape_constraints>>: Sets.fromList (Maybes.cat $ list [var "minC", var "maxC"]),
                Shacl._PropertyShape_defaultValue>>: nothing,
                Shacl._PropertyShape_description>>: wrap Rdf._LangStrings Maps.empty,
                Shacl._PropertyShape_name>>: wrap Rdf._LangStrings Maps.empty,
                Shacl._PropertyShape_order>>: var "order",
                Shacl._PropertyShape_path>>: var "iri"]])
        (encodeType @@ var "rname" @@ var "t" @@ var "cx")] $
    -- Dispatch on the type: peel optional/set wrappers, then build shape
    var "forType" @@ (just (bigint 1)) @@ (just (bigint 1)) @@ var "ftype"

-- | Encode a Hydra LiteralType as SHACL CommonProperties with a datatype constraint
encodeLiteralType :: TTermDefinition (LiteralType -> Shacl.CommonProperties)
encodeLiteralType = define "encodeLiteralType" $
  doc "Encode a LiteralType as SHACL CommonProperties with an XSD datatype constraint" $
  lambda "lt" $ lets [
    "xsd">: lambda "local" $ common @@ list [
      inject Shacl._CommonConstraint Shacl._CommonConstraint_datatype
        (xmlSchemaDatatypeIri @@ var "local")]] $
    cases _LiteralType (var "lt") Nothing [
      _LiteralType_binary>>: constant $ var "xsd" @@ string "base64Binary",
      _LiteralType_boolean>>: constant $ var "xsd" @@ string "boolean",
      _LiteralType_float>>: lambda "ft" $
        cases _FloatType (var "ft") Nothing [
          _FloatType_bigfloat>>: constant $ var "xsd" @@ string "decimal",
          _FloatType_float32>>: constant $ var "xsd" @@ string "float",
          _FloatType_float64>>: constant $ var "xsd" @@ string "double"],
      _LiteralType_integer>>: lambda "it" $
        cases _IntegerType (var "it") Nothing [
          _IntegerType_bigint>>: constant $ var "xsd" @@ string "integer",
          _IntegerType_int8>>: constant $ var "xsd" @@ string "byte",
          _IntegerType_int16>>: constant $ var "xsd" @@ string "short",
          _IntegerType_int32>>: constant $ var "xsd" @@ string "int",
          _IntegerType_int64>>: constant $ var "xsd" @@ string "long",
          _IntegerType_uint8>>: constant $ var "xsd" @@ string "unsignedByte",
          _IntegerType_uint16>>: constant $ var "xsd" @@ string "unsignedShort",
          _IntegerType_uint32>>: constant $ var "xsd" @@ string "unsignedInt",
          _IntegerType_uint64>>: constant $ var "xsd" @@ string "unsignedLong"],
      _LiteralType_string>>: constant $ var "xsd" @@ string "string"]

-- | Encode a Hydra Term as a list of RDF Descriptions
encodeTerm :: TTermDefinition (Rdf.Resource -> Term -> Context -> Graph -> Either Error ([Rdf.Description], Context))
encodeTerm = define "encodeTerm" $
  doc "Encode a Hydra term as a list of RDF Descriptions" $
  lambda "subject" $ lambda "term" $ lambda "cx" $ lambda "g" $
    cases _Term (var "term") (Just (unexpectedE @@ var "cx" @@ string "RDF-compatible term" @@ string "unsupported term variant")) [
      _Term_annotated>>: lambda "at" $
        encodeTerm @@ var "subject" @@ (Core.annotatedTermBody (var "at")) @@ var "cx" @@ var "g",
      _Term_list>>: lambda "terms" $
        encodeList @@ var "subject" @@ var "terms" @@ var "cx" @@ var "g",
      _Term_literal>>: lambda "lit" $ right $ pair
        (list [record Rdf._Description [
          Rdf._Description_subject>>: inject Rdf._Node Rdf._Node_literal (encodeLiteral @@ var "lit"),
          Rdf._Description_graph>>: wrap Rdf._Graph Sets.empty]])
        (var "cx"),
      _Term_map>>: lambda "m" $
        Eithers.map
          ("__r" ~> pair
            (list [record Rdf._Description [
              Rdf._Description_subject>>: resourceToNode @@ var "subject",
              Rdf._Description_graph>>: wrap Rdf._Graph (Sets.fromList (Lists.concat (Pairs.first (var "__r"))))]])
            (Pairs.second (var "__r")))
          (foldAccumResult
            @@ ("__cx0" ~> lambda "kv" $
              Eithers.bind
                (ExtractCore.string @@ var "g" @@ (Strip.deannotateTerm @@ (Pairs.first (var "kv"))))
                ("__ks" ~> lets [
                  "pair2">: nextBlankNode @@ var "__cx0",
                  "node2">: Pairs.first (var "pair2"),
                  "cx2">: Pairs.second (var "pair2")] $
                  Eithers.map
                    ("__dr" ~> pair
                      (Lists.concat2
                        (forObjects @@ var "subject" @@ (keyIri @@ var "__ks") @@ (subjectsOf @@ Pairs.first (var "__dr")))
                        (triplesOf @@ Pairs.first (var "__dr")))
                      (Pairs.second (var "__dr")))
                    (encodeTerm @@ var "node2" @@ (Pairs.second (var "kv")) @@ var "cx2" @@ var "g")))
            @@ var "cx"
            @@ (Maps.toList (var "m"))),
      _Term_wrap>>: lambda "wt" $
        Eithers.map
          ("__dr" ~> lets [
            "descs">: Pairs.first (var "__dr"),
            "cx1">: Pairs.second (var "__dr")] $
            pair
              (Lists.cons
                (withType @@ (Core.wrappedTermTypeName (var "wt")) @@ (Lists.head (var "descs")))
                (Lists.tail (var "descs")))
              (var "cx1"))
          (encodeTerm @@ var "subject" @@ (Core.wrappedTermBody (var "wt")) @@ var "cx" @@ var "g"),
      _Term_maybe>>: lambda "mterm" $
        Maybes.maybe
          (right (pair (list ([] :: [TTerm Rdf.Description])) (var "cx")))
          ("__inner" ~> encodeTerm @@ var "subject" @@ var "__inner" @@ var "cx" @@ var "g")
          (var "mterm"),
      _Term_record>>: lambda "rec" $ lets [
        "rname">: Core.recordTypeName (var "rec"),
        "fields">: Core.recordFields (var "rec")] $
        Eithers.map
          ("__r" ~> pair
            (list [withType @@ var "rname" @@ record Rdf._Description [
              Rdf._Description_subject>>: resourceToNode @@ var "subject",
              Rdf._Description_graph>>: wrap Rdf._Graph (Sets.fromList (Lists.concat (Pairs.first (var "__r"))))]])
            (Pairs.second (var "__r")))
          (foldAccumResult
            @@ ("__cx0" ~> lambda "field" $
              encodeField @@ var "rname" @@ var "subject" @@ var "field" @@ var "__cx0" @@ var "g")
            @@ var "cx"
            @@ var "fields"),
      _Term_set>>: lambda "terms" $
        Eithers.map
          ("__r" ~> pair
            (Lists.concat (Pairs.first (var "__r")))
            (Pairs.second (var "__r")))
          (foldAccumResult
            @@ ("__cx0" ~> lambda "t" $ lets [
              "pair3">: nextBlankNode @@ var "__cx0",
              "node3">: Pairs.first (var "pair3"),
              "cx3">: Pairs.second (var "pair3")] $
              encodeTerm @@ var "node3" @@ var "t" @@ var "cx3" @@ var "g")
            @@ var "cx"
            @@ (Sets.toList (var "terms"))),
      _Term_union>>: lambda "inj" $ lets [
        "rname">: Core.injectionTypeName (var "inj"),
        "field">: Core.injectionField (var "inj")] $
        Eithers.map
          ("__r" ~> pair
            (list [withType @@ var "rname" @@ record Rdf._Description [
              Rdf._Description_subject>>: resourceToNode @@ var "subject",
              Rdf._Description_graph>>: wrap Rdf._Graph (Sets.fromList (Pairs.first (var "__r")))]])
            (Pairs.second (var "__r")))
          (encodeField @@ var "rname" @@ var "subject" @@ var "field" @@ var "cx" @@ var "g")]

-- | Helper for encoding lists as RDF (recursive)
encodeList :: TTermDefinition (Rdf.Resource -> [Term] -> Context -> Graph -> Either Error ([Rdf.Description], Context))
encodeList = define "encodeList" $
  doc "Encode a list of terms as RDF list structure" $
  lambda "subj" $ lambda "terms" $ lambda "cx0" $ lambda "g" $
    Logic.ifElse (Lists.null (var "terms"))
      (right $ pair
        (list [record Rdf._Description [
          Rdf._Description_subject>>: inject Rdf._Node Rdf._Node_iri (wrap Rdf._Iri (string "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")),
          Rdf._Description_graph>>: wrap Rdf._Graph Sets.empty]])
        (var "cx0"))
      (lets [
        "pair1">: nextBlankNode @@ var "cx0",
        "node1">: Pairs.first (var "pair1"),
        "cx1">: Pairs.second (var "pair1")] $
        Eithers.bind
          (encodeTerm @@ var "node1" @@ (Lists.head (var "terms")) @@ var "cx1" @@ var "g")
          ("__r1" ~> lets [
            "fdescs">: Pairs.first (var "__r1"),
            "cx2">: Pairs.second (var "__r1"),
            "firstTriples">: Lists.concat2
              (triplesOf @@ var "fdescs")
              (forObjects @@ var "subj" @@ (rdfIri @@ string "first") @@ (subjectsOf @@ var "fdescs")),
            "pair2">: nextBlankNode @@ var "cx2",
            "next">: Pairs.first (var "pair2"),
            "cx3">: Pairs.second (var "pair2")] $
            Eithers.map
              ("__r2" ~> lets [
                "rdescs">: Pairs.first (var "__r2"),
                "cx4">: Pairs.second (var "__r2"),
                "restTriples">: Lists.concat2
                  (triplesOf @@ var "rdescs")
                  (forObjects @@ var "subj" @@ (rdfIri @@ string "rest") @@ (subjectsOf @@ var "rdescs"))] $
                pair
                  (list [record Rdf._Description [
                    Rdf._Description_subject>>: resourceToNode @@ var "subj",
                    Rdf._Description_graph>>: wrap Rdf._Graph (Sets.fromList (Lists.concat2 (var "firstTriples") (var "restTriples")))]])
                  (var "cx4"))
              (encodeList @@ var "next" @@ (Lists.tail (var "terms")) @@ var "cx3" @@ var "g")))

-- | Fold over a list, accumulating results and threading context
foldAccumResult :: TTermDefinition ((Context -> a -> Either Error (b, Context)) -> Context -> [a] -> Either Error ([b], Context))
foldAccumResult = define "foldAccumResult" $
  doc "Fold over a list, accumulating results and threading context through each step" $
  lambda "f" $ lambda "cx" $ lambda "xs" $
    Logic.ifElse (Lists.null (var "xs"))
      (right (pair (list ([] :: [TTerm b])) (var "cx")))
      (Eithers.bind
        (var "f" @@ var "cx" @@ (Lists.head (var "xs")))
        ("__r" ~> Eithers.map
          ("__rest" ~> pair
            (Lists.cons (Pairs.first (var "__r")) (Pairs.first (var "__rest")))
            (Pairs.second (var "__rest")))
          (foldAccumResult @@ var "f" @@ (Pairs.second (var "__r")) @@ (Lists.tail (var "xs")))))

-- | Encode a Hydra Type as SHACL CommonProperties
encodeType :: TTermDefinition (Name -> Type -> Context -> Either Error Shacl.CommonProperties)
encodeType = define "encodeType" $
  doc "Encode a Hydra type as SHACL CommonProperties" $
  lambda "tname" $ lambda "typ" $ lambda "cx" $ lets [
    "any">: right (common @@ (list ([] :: [TTerm Shacl.CommonConstraint])))] $
    cases _Type (Strip.deannotateType @@ var "typ") (Just (unexpectedE @@ var "cx" @@ string "type" @@ string "unsupported type variant")) [
      _Type_either>>: lambda "_" $ var "any",
      _Type_list>>: lambda "_" $ var "any",
      _Type_literal>>: lambda "lt" $ right (encodeLiteralType @@ var "lt"),
      _Type_map>>: lambda "_" $ var "any",
      _Type_pair>>: lambda "_" $ var "any",
      _Type_wrap>>: lambda "_" $ var "any",
      _Type_record>>: "fts" ~>
        Eithers.map
          ("__props" ~> common @@ list [
            inject Shacl._CommonConstraint Shacl._CommonConstraint_property
              (Sets.fromList (Lists.map
                ("__p" ~> inject Shacl._Reference Shacl._Reference_definition (var "__p"))
                (var "__props")))])
          (Eithers.mapList
            ("__pair" ~> encodeFieldType @@ var "tname" @@ (just (Pairs.first (var "__pair"))) @@ (Pairs.second (var "__pair")) @@ var "cx")
            (Lists.zip (Lists.map ("__i" ~> Literals.int32ToBigint (var "__i")) (Math.range (int32 0) (Lists.length (var "fts")))) (var "fts"))),
      _Type_set>>: lambda "_" $ var "any",
      _Type_union>>: "fts" ~>
        Eithers.map
          ("__props" ~> common @@ list [
            inject Shacl._CommonConstraint Shacl._CommonConstraint_xone
              (Sets.fromList (Lists.map
                ("__p" ~> inject Shacl._Reference Shacl._Reference_anonymous (node @@ list [
                  inject Shacl._CommonConstraint Shacl._CommonConstraint_property
                    (Sets.fromList (list [inject Shacl._Reference Shacl._Reference_definition (var "__p")]))]))
                (var "__props")))])
          (Eithers.mapList
            ("__ft" ~> encodeFieldType @@ var "tname" @@ nothing @@ var "__ft" @@ var "cx")
            (var "fts")),
      _Type_unit>>: constant $ var "any",
      _Type_variable>>: lambda "vname" $
        right (common @@ list [
          inject Shacl._CommonConstraint Shacl._CommonConstraint_node
            (Sets.fromList (list [
              inject Shacl._Reference Shacl._Reference_named (nameToIri @@ var "vname")]))])]

-- | Construct a SHACL node shape from a list of common constraints
node :: TTermDefinition ([Shacl.CommonConstraint] -> Shacl.Shape)
node = define "node" $
  doc "Construct a SHACL node shape from a list of common constraints" $
  lambda "constraints" $
    inject Shacl._Shape Shacl._Shape_node
      (record Shacl._NodeShape [Shacl._NodeShape_common>>: common @@ var "constraints"])

-- | Construct a default SHACL property shape with a given IRI path
property :: TTermDefinition (Rdf.Iri -> Shacl.PropertyShape)
property = define "property" $
  doc "Construct a default property shape with the given IRI as its path" $
  lambda "iri" $
    record Shacl._PropertyShape [
      Shacl._PropertyShape_common>>: defaultCommonProperties,
      Shacl._PropertyShape_constraints>>: Sets.empty,
      Shacl._PropertyShape_defaultValue>>: nothing,
      Shacl._PropertyShape_description>>: wrap Rdf._LangStrings Maps.empty,
      Shacl._PropertyShape_name>>: wrap Rdf._LangStrings Maps.empty,
      Shacl._PropertyShape_order>>: nothing,
      Shacl._PropertyShape_path>>: var "iri"]

-- | Add an rdf:type triple to an RDF Description
withType :: TTermDefinition (Name -> Rdf.Description -> Rdf.Description)
withType = define "withType" $
  doc "Add an rdf:type triple to an RDF Description" $
  lambda "name" $ lambda "desc" $ lets [
    "subj">: project Rdf._Description Rdf._Description_subject @@ var "desc",
    "triples">: unwrap Rdf._Graph @@ (project Rdf._Description Rdf._Description_graph @@ var "desc"),
    "subjRes">: cases Rdf._Node (var "subj") Nothing [
      Rdf._Node_iri>>: lambda "iri" $ inject Rdf._Resource Rdf._Resource_iri (var "iri"),
      Rdf._Node_bnode>>: lambda "bnode" $ inject Rdf._Resource Rdf._Resource_bnode (var "bnode")],
    "triple">: record Rdf._Triple [
      Rdf._Triple_subject>>: var "subjRes",
      Rdf._Triple_predicate>>: rdfIri @@ string "type",
      Rdf._Triple_object>>: inject Rdf._Node Rdf._Node_iri (nameToIri @@ var "name")]] $
    record Rdf._Description [
      Rdf._Description_subject>>: var "subj",
      Rdf._Description_graph>>: wrap Rdf._Graph (Sets.insert (var "triple") (var "triples"))]


-- Utility functions referenced by the coder but defined in Rdf.Utils.
-- These are provided as DSL term references to the staging implementations.

-- | Convert a Name to an RDF IRI
nameToIri :: TTerm (Name -> Rdf.Iri)
nameToIri = TTerm $ TermVariable $ Name "hydra.rdf.utils.nameToIri"

-- | Get the next blank node, updating the context
nextBlankNode :: TTerm (Context -> (Rdf.Resource, Context))
nextBlankNode = TTerm $ TermVariable $ Name "hydra.rdf.utils.nextBlankNode"

-- | Construct triples from a subject, predicate IRI, and list of object nodes
forObjects :: TTerm (Rdf.Resource -> Rdf.Iri -> [Rdf.Node] -> [Rdf.Triple])
forObjects = TTerm $ TermVariable $ Name "hydra.rdf.utils.forObjects"

-- | Construct an IRI for a record field property
propertyIri :: TTerm (Name -> Name -> Rdf.Iri)
propertyIri = TTerm $ TermVariable $ Name "hydra.rdf.utils.propertyIri"

-- | Construct an RDF namespace IRI
rdfIri :: TTerm (String -> Rdf.Iri)
rdfIri = TTerm $ TermVariable $ Name "hydra.rdf.utils.rdfIri"

-- | Convert an RDF Resource to a Node
resourceToNode :: TTerm (Rdf.Resource -> Rdf.Node)
resourceToNode = TTerm $ TermVariable $ Name "hydra.rdf.utils.resourceToNode"

-- | Extract subject nodes from a list of Descriptions
subjectsOf :: TTerm ([Rdf.Description] -> [Rdf.Node])
subjectsOf = TTerm $ TermVariable $ Name "hydra.rdf.utils.subjectsOf"

-- | Extract triples from a list of Descriptions
triplesOf :: TTerm ([Rdf.Description] -> [Rdf.Triple])
triplesOf = TTerm $ TermVariable $ Name "hydra.rdf.utils.triplesOf"

-- | Construct an XSD datatype IRI from a local name
xmlSchemaDatatypeIri :: TTerm (String -> Rdf.Iri)
xmlSchemaDatatypeIri = TTerm $ TermVariable $ Name "hydra.rdf.utils.xmlSchemaDatatypeIri"

-- | Construct a key IRI from a string
keyIri :: TTerm (String -> Rdf.Iri)
keyIri = TTerm $ TermVariable $ Name "hydra.rdf.utils.keyIri"

-- | Encode a Hydra Literal as an RDF Literal
encodeLiteral :: TTerm (Literal -> Rdf.Literal)
encodeLiteral = TTerm $ TermVariable $ Name "hydra.rdf.utils.encodeLiteral"
