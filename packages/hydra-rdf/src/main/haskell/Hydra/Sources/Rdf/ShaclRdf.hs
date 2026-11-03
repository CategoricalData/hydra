{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Rdf.ShaclRdf where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import qualified Hydra.Core.Dsl.Lib.Strings                as Strings
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms                   as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Core.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Core.Dsl.Paths                      as Paths
import qualified Hydra.Core.Dsl.Ast                        as Ast
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base                       as MetaBase
import qualified Hydra.Core.Dsl.Coders                     as Coders
import qualified Hydra.Core.Dsl.Util                    as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core                       as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Core.Dsl.Json.Model                       as Json
import qualified Hydra.Core.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality               as Equality
import qualified Hydra.Core.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Core.Dsl.Lib.Literals               as Literals
import qualified Hydra.Core.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Core.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Core.Dsl.Lib.Math                   as Math
import qualified Hydra.Core.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Core.Dsl.Lib.Ordering               as Ordering
import qualified Hydra.Core.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Core.Dsl.Packaging                     as Packaging
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Core.Dsl.Topology                   as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Core.Dsl.Typing                     as Typing
import qualified Hydra.Core.Dsl.Util                       as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types                           as Types
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
import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Print.Core      as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Graph     as PrintGraph
import qualified Hydra.Sources.Kernel.Terms.Print.Variants  as PrintVariants
import qualified Hydra.Sources.Kernel.Terms.Print.Typing    as PrintTyping
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
import qualified Hydra.Rdf.Shacl.Model as Shacl
import qualified Hydra.Sources.Rdf.Serde as RdfSerde
import qualified Hydra.Sources.Rdf.Syntax as RdfSyntax
import qualified Hydra.Sources.Rdf.Utils as RdfUtils
import qualified Hydra.Sources.Shacl.Model as ShaclModel


define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

ns :: ModuleName
ns = ModuleName "hydra.rdf.shaclRdf"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$>
              ([RdfSerde.ns, RdfUtils.ns] L.++ (ShaclModel.ns:RdfSyntax.ns:KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = Bootstrap.descriptionMetadata (Just
              "Serialize a SHACL ShapesGraph to RDF triples using the SHACL vocabulary")}
  where
    definitions = [
      toDefinition anonymousBlankNode,
      toDefinition booleanNode,
      toDefinition commonConstraintToTriples,
      toDefinition commonPropertiesToTriples,
      toDefinition definitionToTriples,
      toDefinition escapeBlankNodeLabelPart,
      toDefinition inlineNodeShapeToTriples,
      toDefinition inlineShapeToTriples,
      toDefinition integerNode,
      toDefinition iriNode,
      toDefinition iriOrLiteralToNode,
      toDefinition iriResource,
      toDefinition nodeKindToIri,
      toDefinition nodeShapeToTriples,
      toDefinition propertyRefToTriples,
      toDefinition propertyShapeConstraintToTriples,
      toDefinition propertyShapeToTriples,
      toDefinition rdf,
      toDefinition rdfListToTriples,
      toDefinition referenceListToTriples,
      toDefinition referenceToObject,
      toDefinition referenceToTriples,
      toDefinition sh,
      toDefinition shapesGraphToNtriples,
      toDefinition shapesGraphToTriples,
      toDefinition stringNode,
      toDefinition tripleIII,
      toDefinition tripleIIN,
      toDefinition tripleRII,
      toDefinition tripleRIN,
      toDefinition tripleToDescription,
      toDefinition xsd]


-- | Allocate a deterministic blank node for an anonymous object of a SHACL property
anonymousBlankNode :: TypedTermDefinition (Rdf.Resource -> String -> Int -> Rdf.Resource)
anonymousBlankNode = define "anonymousBlankNode" $
  doc ("Allocate a deterministic blank node for the k-th anonymous object of a SHACL property (given by its"
    <> " local name) on a subject. The label is derived from the subject, the property, and the ordinal, so"
    <> " serialization stays pure and stable. An IRI subject is escaped to [A-Za-z0-9_] characters; a blank"
    <> " node subject (itself allocated here) is used as-is. Because every allocated label contains '-' and"
    <> " an escaped IRI never does, the labels are injective.") $
  lambda "subj" $ lambda "rel" $ lambda "k" $ lets [
    "parentKey">: match Rdf._Resource (var "subj") Nothing [
      Rdf._Resource_iri>>: lambda "i" $ escapeBlankNodeLabelPart @@ (unwrap Rdf._Iri @@ var "i"),
      Rdf._Resource_bnode>>: lambda "b" $ unwrap Rdf._BlankNode @@ var "b"]] $
    inject Rdf._Resource Rdf._Resource_bnode $ wrap Rdf._BlankNode $ Strings.concat (list [
      var "parentKey", string "-", var "rel", string "-", Literals.printInt32 (var "k")])

-- | Create a boolean literal node
booleanNode :: TypedTermDefinition (Bool -> Rdf.Node)
booleanNode = define "booleanNode" $
  doc "Create a boolean literal node" $
  lambda "b" $
    inject Rdf._Node Rdf._Node_literal $
      record Rdf._Literal [
        Rdf._Literal_lexicalForm>>: Logic.ifElse (var "b") (string "true") (string "false"),
        Rdf._Literal_datatypeIri>>: xsd @@ string "boolean",
        Rdf._Literal_languageTag>>: nothing]

-- | Serialize a CommonConstraint to triples
commonConstraintToTriples :: TypedTermDefinition (Rdf.Resource -> Shacl.CommonConstraint -> [Rdf.Triple])
commonConstraintToTriples = define "commonConstraintToTriples" $
  doc "Serialize a CommonConstraint to triples" $
  lambda "subj" $ lambda "cc" $
    match Shacl._CommonConstraint (var "cc") Nothing [
      Shacl._CommonConstraint_datatype>>: lambda "i" $
        list [tripleRII @@ var "subj" @@ (sh @@ string "datatype") @@ var "i"],
      Shacl._CommonConstraint_class>>: constant $
        -- RdfsClass is a stand-in type (wraps unit), not usable as an IRI
        list ([] :: [TypedTerm Rdf.Triple]),
      Shacl._CommonConstraint_nodeKind>>: lambda "nk" $
        list [tripleRII @@ var "subj" @@ (sh @@ string "nodeKind") @@ (nodeKindToIri @@ var "nk")],
      Shacl._CommonConstraint_property>>: lambda "refs" $
        Lists.concat (indexedMap (propertyRefToTriples @@ var "subj") (Sets.toList (var "refs"))),
      Shacl._CommonConstraint_maxInclusive>>: lambda "lit" $
        list [tripleRIN @@ var "subj" @@ (sh @@ string "maxInclusive") @@ (inject Rdf._Node Rdf._Node_literal (var "lit"))],
      Shacl._CommonConstraint_minInclusive>>: lambda "lit" $
        list [tripleRIN @@ var "subj" @@ (sh @@ string "minInclusive") @@ (inject Rdf._Node Rdf._Node_literal (var "lit"))],
      Shacl._CommonConstraint_maxExclusive>>: lambda "lit" $
        list [tripleRIN @@ var "subj" @@ (sh @@ string "maxExclusive") @@ (inject Rdf._Node Rdf._Node_literal (var "lit"))],
      Shacl._CommonConstraint_minExclusive>>: lambda "lit" $
        list [tripleRIN @@ var "subj" @@ (sh @@ string "minExclusive") @@ (inject Rdf._Node Rdf._Node_literal (var "lit"))],
      Shacl._CommonConstraint_maxLength>>: lambda "n" $
        list [tripleRIN @@ var "subj" @@ (sh @@ string "maxLength") @@ (integerNode @@ var "n")],
      Shacl._CommonConstraint_minLength>>: lambda "n" $
        list [tripleRIN @@ var "subj" @@ (sh @@ string "minLength") @@ (integerNode @@ var "n")],
      Shacl._CommonConstraint_pattern>>: lambda "pat" $ lets [
        "regex_">: project Shacl._Pattern Shacl._Pattern_regex @@ var "pat",
        "flags_">: project Shacl._Pattern Shacl._Pattern_flags @@ var "pat"] $
        Lists.concat (list [
          list [tripleRIN @@ var "subj" @@ (sh @@ string "pattern") @@ (stringNode @@ var "regex_")],
          Optionals.match (var "flags_") (list ([] :: [TypedTerm Rdf.Triple]))
            (lambda "f" $ list [tripleRIN @@ var "subj" @@ (sh @@ string "flags") @@ (stringNode @@ var "f")])]),
      Shacl._CommonConstraint_closed>>: lambda "cl" $
        list [tripleRIN @@ var "subj" @@ (sh @@ string "closed")
          @@ (booleanNode @@ (project Shacl._Closed Shacl._Closed_isClosed @@ var "cl"))],
      Shacl._CommonConstraint_hasValue>>: lambda "nodes" $
        Lists.map (lambda "n" $ tripleRIN @@ var "subj" @@ (sh @@ string "hasValue") @@ var "n")
          (Sets.toList (var "nodes" :: TypedTerm (S.Set Rdf.Node))),
      Shacl._CommonConstraint_in>>: lambda "nodes" $
        rdfListToTriples @@ var "subj" @@ string "in" @@ var "nodes",
      Shacl._CommonConstraint_node>>: lambda "refs" $
        Lists.concat (indexedMap (referenceToTriples @@ var "subj" @@ string "node" @@ inlineNodeShapeToTriples)
          (Sets.toList (var "refs" :: TypedTerm (S.Set (Shacl.Reference Shacl.NodeShape))))),
      Shacl._CommonConstraint_not>>: lambda "refs" $
        Lists.concat (indexedMap (referenceToTriples @@ var "subj" @@ string "not" @@ inlineShapeToTriples)
          (Sets.toList (var "refs" :: TypedTerm (S.Set (Shacl.Reference Shacl.Shape))))),
      Shacl._CommonConstraint_and>>: lambda "refs" $
        -- sh:and takes a single SHACL list of shapes
        referenceListToTriples @@ var "subj" @@ string "and" @@ inlineShapeToTriples
          @@ (Sets.toList (var "refs" :: TypedTerm (S.Set (Shacl.Reference Shacl.Shape)))),
      Shacl._CommonConstraint_or>>: lambda "refs" $
        -- sh:or takes a single SHACL list of shapes
        referenceListToTriples @@ var "subj" @@ string "or" @@ inlineShapeToTriples
          @@ (Sets.toList (var "refs" :: TypedTerm (S.Set (Shacl.Reference Shacl.Shape)))),
      Shacl._CommonConstraint_xone>>: lambda "refs" $
        -- sh:xone takes a single SHACL list of shapes
        referenceListToTriples @@ var "subj" @@ string "xone" @@ inlineShapeToTriples
          @@ (Sets.toList (var "refs" :: TypedTerm (S.Set (Shacl.Reference Shacl.Shape)))),
      Shacl._CommonConstraint_equals>>: constant $
        -- Property is a stand-in type, not usable as an IRI
        list ([] :: [TypedTerm Rdf.Triple]),
      Shacl._CommonConstraint_disjoint>>: constant $
        -- Property is a stand-in type, not usable as an IRI
        list ([] :: [TypedTerm Rdf.Triple]),
      Shacl._CommonConstraint_languageIn>>: lambda "tags" $
        rdfListToTriples @@ var "subj" @@ string "languageIn" @@ Lists.map
          (lambda "t" $ stringNode @@ (unwrap Rdf._LanguageTag @@ var "t"))
          (Sets.toList (var "tags" :: TypedTerm (S.Set Rdf.LanguageTag)))]

-- | Serialize CommonProperties to triples
commonPropertiesToTriples :: TypedTermDefinition (Rdf.Resource -> Shacl.CommonProperties -> [Rdf.Triple])
commonPropertiesToTriples = define "commonPropertiesToTriples" $
  doc "Serialize CommonProperties to triples" $
  lambda "subj" $ lambda "cp" $
    Lists.concat (list [
      Lists.concat (Lists.map
        (commonConstraintToTriples @@ var "subj")
        (Sets.toList (project Shacl._CommonProperties Shacl._CommonProperties_constraints @@ var "cp"))),
      Optionals.match
        (project Shacl._CommonProperties Shacl._CommonProperties_deactivated @@ var "cp")
        (list ([] :: [TypedTerm Rdf.Triple]))
        (lambda "b" $ list [tripleRIN @@ var "subj" @@ (sh @@ string "deactivated") @@ (booleanNode @@ var "b")]),
      -- Note: targetClass, targetNode, targetObjectsOf, targetSubjectsOf are typically empty
      -- for shapes generated by the Hydra SHACL coder. The RDF model's RdfsClass and Property
      -- types are stand-ins (not IRIs), so we skip them if non-empty.
      Lists.concat (Lists.map
        (lambda "n" $ list [tripleRIN @@ var "subj" @@ (sh @@ string "targetNode") @@ (iriOrLiteralToNode @@ var "n")])
        (Sets.toList ((project Shacl._CommonProperties Shacl._CommonProperties_targetNode @@ var "cp")
          :: TypedTerm (S.Set Rdf.IriOrLiteral))))])

-- | Convert a Definition<Shape> to triples
definitionToTriples :: TypedTermDefinition (Shacl.Definition Shacl.Shape -> [Rdf.Triple])
definitionToTriples = define "definitionToTriples" $
  doc "Serialize a Definition<Shape> to triples" $
  lambda "d" $ lets [
    "iri_">: project Shacl._Definition Shacl._Definition_iri @@ var "d",
    "shape">: project Shacl._Definition Shacl._Definition_target @@ var "d"] $
    match Shacl._Shape (var "shape") Nothing [
      Shacl._Shape_node>>: lambda "ns_" $ nodeShapeToTriples @@ var "iri_" @@ var "ns_",
      Shacl._Shape_property>>: lambda "ps" $ propertyShapeToTriples @@ (iriResource @@ var "iri_") @@ var "ps"]

-- | Escape a string to a blank node label fragment
escapeBlankNodeLabelPart :: TypedTermDefinition (String -> String)
escapeBlankNodeLabelPart = define "escapeBlankNodeLabelPart" $
  doc ("Escape a string for use in a blank node label: ASCII letters and digits are kept, and every other"
    <> " character becomes '_' followed by its decimal code point and '_'. The result is valid in N-Triples"
    <> " blank node labels and never contains '-'.") $
  lambda "s" $
    Strings.concat (Lists.map
      (lambda "c" $ Logic.ifElse
        (Logic.or (Logic.and (Ordering.gte (var "c") (int32 48)) (Ordering.lte (var "c") (int32 57)))
          (Logic.or (Logic.and (Ordering.gte (var "c") (int32 65)) (Ordering.lte (var "c") (int32 90)))
            (Logic.and (Ordering.gte (var "c") (int32 97)) (Ordering.lte (var "c") (int32 122)))))
        (Strings.fromList (list [var "c"]))
        (Strings.concat (list [string "_", Literals.printInt32 (var "c"), string "_"])))
      (Strings.toList (var "s")))

-- | Serialize an inline (possibly anonymous) NodeShape to triples, given its subject resource
inlineNodeShapeToTriples :: TypedTermDefinition (Rdf.Resource -> Shacl.NodeShape -> [Rdf.Triple])
inlineNodeShapeToTriples = define "inlineNodeShapeToTriples" $
  doc "Serialize a NodeShape to triples, given a subject resource (may be blank node), without any target" $
  lambda "subj" $ lambda "ns_" $
    Lists.cons
      (tripleRII @@ var "subj" @@ (rdf @@ string "type") @@ (sh @@ string "NodeShape"))
      (commonPropertiesToTriples @@ var "subj"
        @@ (project Shacl._NodeShape Shacl._NodeShape_common @@ var "ns_"))

-- | Serialize an inline (possibly anonymous) Shape to triples, given its subject resource
inlineShapeToTriples :: TypedTermDefinition (Rdf.Resource -> Shacl.Shape -> [Rdf.Triple])
inlineShapeToTriples = define "inlineShapeToTriples" $
  doc "Serialize a Shape to triples, given a subject resource (may be blank node), without any target" $
  lambda "subj" $ lambda "shape" $
    match Shacl._Shape (var "shape") Nothing [
      Shacl._Shape_node>>: lambda "ns_" $ inlineNodeShapeToTriples @@ var "subj" @@ var "ns_",
      Shacl._Shape_property>>: lambda "ps" $ propertyShapeToTriples @@ var "subj" @@ var "ps"]

-- | Create an integer literal node
integerNode :: TypedTermDefinition (Integer -> Rdf.Node)
integerNode = define "integerNode" $
  doc "Create an integer literal node" $
  lambda "n" $
    inject Rdf._Node Rdf._Node_literal $
      record Rdf._Literal [
        Rdf._Literal_lexicalForm>>: Literals.printBigint (var "n"),
        Rdf._Literal_datatypeIri>>: xsd @@ string "integer",
        Rdf._Literal_languageTag>>: nothing]

-- | Create an IRI node
iriNode :: TypedTermDefinition (Rdf.Iri -> Rdf.Node)
iriNode = define "iriNode" $
  doc "Create an IRI node" $
  lambda "i" $
    inject Rdf._Node Rdf._Node_iri (var "i")

-- | Convert an IriOrLiteral to an RDF Node
iriOrLiteralToNode :: TypedTermDefinition (Rdf.IriOrLiteral -> Rdf.Node)
iriOrLiteralToNode = define "iriOrLiteralToNode" $
  doc "Convert an IriOrLiteral to an RDF Node" $
  lambda "il" $
    match Rdf._IriOrLiteral (var "il") Nothing [
      Rdf._IriOrLiteral_iri>>: lambda "i" $ inject Rdf._Node Rdf._Node_iri (var "i"),
      Rdf._IriOrLiteral_literal>>: lambda "lit" $ inject Rdf._Node Rdf._Node_literal (var "lit")]

-- | Create an IRI resource
iriResource :: TypedTermDefinition (Rdf.Iri -> Rdf.Resource)
iriResource = define "iriResource" $
  doc "Create an IRI resource" $
  lambda "i" $
    inject Rdf._Resource Rdf._Resource_iri (var "i")

-- | Convert a NodeKind to its SHACL IRI
nodeKindToIri :: TypedTermDefinition (Shacl.NodeKind -> Rdf.Iri)
nodeKindToIri = define "nodeKindToIri" $
  doc "Convert a NodeKind to its SHACL IRI" $
  lambda "nk" $
    match Shacl._NodeKind (var "nk") Nothing [
      Shacl._NodeKind_blankNode>>: constant $ sh @@ string "BlankNode",
      Shacl._NodeKind_iri>>: constant $ sh @@ string "IRI",
      Shacl._NodeKind_literal>>: constant $ sh @@ string "Literal",
      Shacl._NodeKind_blankNodeOrIri>>: constant $ sh @@ string "BlankNodeOrIRI",
      Shacl._NodeKind_blankNodeOrLiteral>>: constant $ sh @@ string "BlankNodeOrLiteral",
      Shacl._NodeKind_iriOrLiteral>>: constant $ sh @@ string "IRIOrLiteral"]

-- | Convert a NodeShape to triples, given the shape's IRI
nodeShapeToTriples :: TypedTermDefinition (Rdf.Iri -> Shacl.NodeShape -> [Rdf.Triple])
nodeShapeToTriples = define "nodeShapeToTriples" $
  doc "Serialize a NodeShape to triples" $
  lambda "iri_" $ lambda "ns_" $
    Lists.cons
      (tripleIII @@ var "iri_" @@ (sh @@ string "targetClass") @@ var "iri_")
      (inlineNodeShapeToTriples @@ (iriResource @@ var "iri_") @@ var "ns_")

-- | Serialize a property shape reference (may be inline or named)
propertyRefToTriples :: TypedTermDefinition (Rdf.Resource -> Int -> Shacl.Reference Shacl.PropertyShape -> [Rdf.Triple])
propertyRefToTriples = define "propertyRefToTriples" $
  doc ("Serialize the k-th property shape reference of a subject (may be named, anonymous, or an inline"
    <> " definition)") $
  lambda "subj" $ lambda "k" $ lambda "ref" $
    match Shacl._Reference (var "ref")
      (Just $ referenceToTriples @@ var "subj" @@ string "property" @@ propertyShapeToTriples @@ var "k" @@ var "ref") [
      Shacl._Reference_definition>>: lambda "def" $ lets [
        "iri_">: project Shacl._Definition Shacl._Definition_iri @@ var "def",
        "ps">: project Shacl._Definition Shacl._Definition_target @@ var "def"] $
        Lists.concat (list [
          list [tripleRII @@ var "subj" @@ (sh @@ string "property") @@ var "iri_"],
          propertyShapeToTriples @@ (iriResource @@ var "iri_") @@ var "ps"])]

-- | Serialize a PropertyShapeConstraint to triples
propertyShapeConstraintToTriples :: TypedTermDefinition (Rdf.Resource -> Shacl.PropertyShapeConstraint -> [Rdf.Triple])
propertyShapeConstraintToTriples = define "propertyShapeConstraintToTriples" $
  doc "Serialize a PropertyShapeConstraint to triples" $
  lambda "subj" $ lambda "psc" $
    match Shacl._PropertyShapeConstraint (var "psc") Nothing [
      Shacl._PropertyShapeConstraint_minCount>>: lambda "n" $
        list [tripleRIN @@ var "subj" @@ (sh @@ string "minCount") @@ (integerNode @@ var "n")],
      Shacl._PropertyShapeConstraint_maxCount>>: lambda "n" $
        list [tripleRIN @@ var "subj" @@ (sh @@ string "maxCount") @@ (integerNode @@ var "n")],
      Shacl._PropertyShapeConstraint_lessThan>>: constant $
        -- Property is a stand-in type, not usable as an IRI
        list ([] :: [TypedTerm Rdf.Triple]),
      Shacl._PropertyShapeConstraint_lessThanOrEquals>>: constant $
        -- Property is a stand-in type, not usable as an IRI
        list ([] :: [TypedTerm Rdf.Triple]),
      Shacl._PropertyShapeConstraint_uniqueLang>>: lambda "b" $
        list [tripleRIN @@ var "subj" @@ (sh @@ string "uniqueLang") @@ (booleanNode @@ var "b")],
      Shacl._PropertyShapeConstraint_qualifiedValueShape>>: constant $
        -- Complex; omit for now
        list ([] :: [TypedTerm Rdf.Triple])]

-- | Convert a PropertyShape to triples, given a subject resource (may be blank node)
propertyShapeToTriples :: TypedTermDefinition (Rdf.Resource -> Shacl.PropertyShape -> [Rdf.Triple])
propertyShapeToTriples = define "propertyShapeToTriples" $
  doc "Serialize a PropertyShape to triples, given a subject resource (may be blank node)" $
  lambda "subj" $ lambda "ps" $
    Lists.concat (list [
      list [
        tripleRII @@ var "subj" @@ (rdf @@ string "type") @@ (sh @@ string "PropertyShape"),
        tripleRII @@ var "subj" @@ (sh @@ string "path") @@ (project Shacl._PropertyShape Shacl._PropertyShape_path @@ var "ps")],
      commonPropertiesToTriples @@ var "subj" @@ (project Shacl._PropertyShape Shacl._PropertyShape_common @@ var "ps"),
      Lists.concat (Lists.map
        (propertyShapeConstraintToTriples @@ var "subj")
        (Sets.toList (project Shacl._PropertyShape Shacl._PropertyShape_constraints @@ var "ps"))),
      Optionals.match
        (project Shacl._PropertyShape Shacl._PropertyShape_order @@ var "ps")
        (list ([] :: [TypedTerm Rdf.Triple]))
        (lambda "n" $ list [tripleRIN @@ var "subj" @@ (sh @@ string "order") @@ (integerNode @@ var "n")])])

-- | RDF namespace
rdf :: TypedTermDefinition (String -> Rdf.Iri)
rdf = define "rdf" $
  doc "Construct an IRI in the RDF namespace" $
  lambda "local" $
    RdfUtils.rdfIri @@ var "local"

-- | Serialize a list-valued SHACL property as an RDF collection
rdfListToTriples :: TypedTermDefinition (Rdf.Resource -> String -> [Rdf.Node] -> [Rdf.Triple])
rdfListToTriples = define "rdfListToTriples" $
  doc ("Serialize a list-valued SHACL property (given by its local name, e.g. sh:in) as a single triple whose"
    <> " object is an RDF collection: a chain of rdf:first/rdf:rest cells on deterministic blank nodes,"
    <> " terminated by rdf:nil. An empty list is rdf:nil itself. Cell labels use the property name suffixed"
    <> " with 'List', so they never collide with blank nodes allocated for the list's own members.") $
  lambda "subj" $ lambda "rel" $ lambda "nodes" $ lets [
    "n">: Lists.length (var "nodes"),
    "cell">: lambda "i" $ anonymousBlankNode @@ var "subj" @@ Strings.concat2 (var "rel") (string "List") @@ var "i",
    "cellNode">: lambda "i" $ Logic.ifElse (Ordering.lt (var "i") (var "n"))
      (RdfUtils.resourceToNode @@ (var "cell" @@ var "i"))
      (iriNode @@ (rdf @@ string "nil"))] $
    Lists.cons
      (tripleRIN @@ var "subj" @@ (sh @@ var "rel") @@ (var "cellNode" @@ int32 0))
      (Lists.concat (Lists.map
        (lambda "p" $ lets [
          "i">: Pairs.first (var "p"),
          "c">: var "cell" @@ var "i"] $ list [
          tripleRIN @@ var "c" @@ (rdf @@ string "first") @@ Pairs.second (var "p"),
          tripleRIN @@ var "c" @@ (rdf @@ string "rest") @@ (var "cellNode" @@ Math.add (var "i") (int32 1))])
        (Lists.zip (Math.range (int32 0) (var "n")) (var "nodes"))))

-- | Serialize a reference-valued SHACL property whose value is a single list of shapes
referenceListToTriples :: TypedTermDefinition (Rdf.Resource -> String -> (Rdf.Resource -> a -> [Rdf.Triple]) -> [Shacl.Reference a] -> [Rdf.Triple])
referenceListToTriples = define "referenceListToTriples" $
  doc ("Serialize a reference-valued SHACL property whose value is a single SHACL list of shapes (sh:and,"
    <> " sh:or, sh:xone): one triple whose object is the list of referenced shapes, plus the triples of any"
    <> " anonymous shapes.") $
  lambda "subj" $ lambda "rel" $ lambda "toTriples" $ lambda "refs" $ lets [
    "objs">: Optionals.givens (indexedMap (referenceToObject @@ var "subj" @@ var "rel" @@ var "toTriples") (var "refs"))] $
    Lists.concat2
      (rdfListToTriples @@ var "subj" @@ var "rel" @@ Lists.map (lambda "o" $ Pairs.first (var "o")) (var "objs"))
      (Lists.concat (Lists.map (lambda "o" $ Pairs.second (var "o")) (var "objs")))

-- | Resolve the k-th reference-valued object of a SHACL property to an RDF node plus supporting triples
referenceToObject :: TypedTermDefinition (Rdf.Resource -> String -> (Rdf.Resource -> a -> [Rdf.Triple]) -> Int -> Shacl.Reference a -> Maybe (Rdf.Node, [Rdf.Triple]))
referenceToObject = define "referenceToObject" $
  doc ("Resolve the k-th reference-valued object of a SHACL property (given by its local name) on a subject"
    <> " to an RDF node, together with any triples describing it. A named reference is its IRI; an anonymous"
    <> " reference is a fresh deterministic blank node, onto which the inline shape is serialized using the"
    <> " given function. Inline definitions are not handled here (see propertyRefToTriples), and yield nothing.") $
  lambda "subj" $ lambda "rel" $ lambda "toTriples" $ lambda "k" $ lambda "ref" $
    match Shacl._Reference (var "ref") (Just nothing) [
      Shacl._Reference_named>>: lambda "i" $
        just (pair (iriNode @@ var "i") (list ([] :: [TypedTerm Rdf.Triple]))),
      Shacl._Reference_anonymous>>: lambda "x" $ lets [
        "b">: anonymousBlankNode @@ var "subj" @@ var "rel" @@ var "k"] $
        just (pair (RdfUtils.resourceToNode @@ var "b") (var "toTriples" @@ var "b" @@ var "x"))]

-- | Serialize the k-th reference-valued object of a SHACL property on a subject
referenceToTriples :: TypedTermDefinition (Rdf.Resource -> String -> (Rdf.Resource -> a -> [Rdf.Triple]) -> Int -> Shacl.Reference a -> [Rdf.Triple])
referenceToTriples = define "referenceToTriples" $
  doc ("Serialize the k-th reference-valued object of a SHACL property (given by its local name) on a"
    <> " subject, as a triple linking the subject to the referenced shape plus any triples describing it.") $
  lambda "subj" $ lambda "rel" $ lambda "toTriples" $ lambda "k" $ lambda "ref" $
    Optionals.match (referenceToObject @@ var "subj" @@ var "rel" @@ var "toTriples" @@ var "k" @@ var "ref")
      (list ([] :: [TypedTerm Rdf.Triple]))
      (lambda "o" $ Lists.cons
        (tripleRIN @@ var "subj" @@ (sh @@ var "rel") @@ Pairs.first (var "o"))
        (Pairs.second (var "o")))

-- | SHACL namespace
sh :: TypedTermDefinition (String -> Rdf.Iri)
sh = define "sh" $
  doc "Construct an IRI in the SHACL namespace" $
  lambda "local" $
    RdfUtils.iri @@ string "http://www.w3.org/ns/shacl#" @@ var "local"

-- | Convert a ShapesGraph to N-Triples string
shapesGraphToNtriples :: TypedTermDefinition (Shacl.ShapesGraph -> String)
shapesGraphToNtriples = define "shapesGraphToNtriples" $
  doc "Convert a ShapesGraph to N-Triples string" $
  lambda "sg" $ lets [
    "triples">: shapesGraphToTriples @@ var "sg",
    "descs">: Lists.map ("t" ~> tripleToDescription @@ var "t") (var "triples")] $
    RdfSerde.rdfGraphToNtriples @@ (RdfUtils.descriptionsToGraph @@ var "descs")

-- | Convert a ShapesGraph to RDF triples
shapesGraphToTriples :: TypedTermDefinition (Shacl.ShapesGraph -> [Rdf.Triple])
shapesGraphToTriples = define "shapesGraphToTriples" $
  doc "Convert a ShapesGraph to RDF triples" $
  lambda "sg" $
    Lists.concat (Lists.map ("d" ~> definitionToTriples @@ var "d")
      (Sets.toList ((unwrap Shacl._ShapesGraph @@ var "sg") :: TypedTerm (S.Set (Shacl.Definition Shacl.Shape)))))

-- | Create a string literal node
stringNode :: TypedTermDefinition (String -> Rdf.Node)
stringNode = define "stringNode" $
  doc "Create a string literal node" $
  lambda "s" $
    inject Rdf._Node Rdf._Node_literal $
      record Rdf._Literal [
        Rdf._Literal_lexicalForm>>: var "s",
        Rdf._Literal_datatypeIri>>: xsd @@ string "string",
        Rdf._Literal_languageTag>>: nothing]

-- | Create a triple from IRI subject, IRI predicate, and IRI object
tripleIII :: TypedTermDefinition (Rdf.Iri -> Rdf.Iri -> Rdf.Iri -> Rdf.Triple)
tripleIII = define "tripleIII" $
  doc "Create a triple from IRI subject, IRI predicate, and IRI object" $
  lambda "s" $ lambda "p" $ lambda "o" $
    record Rdf._Triple [
      Rdf._Triple_subject>>: iriResource @@ var "s",
      Rdf._Triple_predicate>>: var "p",
      Rdf._Triple_object>>: iriNode @@ var "o"]

-- | Create a triple from IRI subject, IRI predicate, and node object
tripleIIN :: TypedTermDefinition (Rdf.Iri -> Rdf.Iri -> Rdf.Node -> Rdf.Triple)
tripleIIN = define "tripleIIN" $
  doc "Create a triple from IRI subject, IRI predicate, and node object" $
  lambda "s" $ lambda "p" $ lambda "o" $
    record Rdf._Triple [
      Rdf._Triple_subject>>: iriResource @@ var "s",
      Rdf._Triple_predicate>>: var "p",
      Rdf._Triple_object>>: var "o"]

-- | Create a triple from resource subject, IRI predicate, and IRI object
tripleRII :: TypedTermDefinition (Rdf.Resource -> Rdf.Iri -> Rdf.Iri -> Rdf.Triple)
tripleRII = define "tripleRII" $
  doc "Create a triple from resource subject, IRI predicate, and IRI object" $
  lambda "s" $ lambda "p" $ lambda "o" $
    record Rdf._Triple [
      Rdf._Triple_subject>>: var "s",
      Rdf._Triple_predicate>>: var "p",
      Rdf._Triple_object>>: iriNode @@ var "o"]

-- | Create a triple from resource subject, IRI predicate, and node object
tripleRIN :: TypedTermDefinition (Rdf.Resource -> Rdf.Iri -> Rdf.Node -> Rdf.Triple)
tripleRIN = define "tripleRIN" $
  doc "Create a triple from resource subject, IRI predicate, and node object" $
  lambda "s" $ lambda "p" $ lambda "o" $
    record Rdf._Triple [
      Rdf._Triple_subject>>: var "s",
      Rdf._Triple_predicate>>: var "p",
      Rdf._Triple_object>>: var "o"]

-- | Convert a single triple to a description (one triple per description)
tripleToDescription :: TypedTermDefinition (Rdf.Triple -> Rdf.Description)
tripleToDescription = define "tripleToDescription" $
  doc "Convert a single triple to a description (one triple per description)" $
  lambda "t" $ lets [
    "subj">: project Rdf._Triple Rdf._Triple_subject @@ var "t"] $
    record Rdf._Description [
      Rdf._Description_subject>>: RdfUtils.resourceToNode @@ var "subj",
      Rdf._Description_graph>>: wrap Rdf._Graph (Sets.singleton (var "t") :: TypedTerm (S.Set Rdf.Triple))]

-- | XSD namespace
xsd :: TypedTermDefinition (String -> Rdf.Iri)
xsd = define "xsd" $
  doc "Construct an IRI in the XML Schema namespace" $
  lambda "local" $
    RdfUtils.xmlSchemaDatatypeIri @@ var "local"

-- | Apply a binary function to each element of a list along with its 0-based ordinal. The ordinal is what
-- anonymousBlankNode uses to keep sibling blank nodes distinct; Sets.toList is ascending on every host, so
-- ordinals over a set's elements (and hence blank node labels) are deterministic.
indexedMap :: TypedTerm (Int -> x -> y) -> TypedTerm [x] -> TypedTerm [y]
indexedMap f xs = Lists.map
  (lambda "p" $ f @@ Pairs.first (var "p") @@ Pairs.second (var "p"))
  (Lists.zip (Math.range (int32 0) (Lists.length xs)) xs)
