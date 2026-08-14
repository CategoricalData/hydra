{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Rdf.ShaclRdf where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import qualified Hydra.Dsl.Lib.Strings                as Strings
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                   as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core                       as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Literals               as Literals
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Math                   as Math
import qualified Hydra.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms                      as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants                   as Variants
import qualified Hydra.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular                         as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Overlay.Haskell.Dsl.Types                           as Types
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
import qualified Hydra.Shacl.Model as Shacl
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
      toDefinition booleanNode,
      toDefinition commonConstraintToTriples,
      toDefinition commonPropertiesToTriples,
      toDefinition definitionToTriples,
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
    cases Shacl._CommonConstraint (var "cc") Nothing [
      Shacl._CommonConstraint_datatype>>: lambda "i" $
        list [tripleRII @@ var "subj" @@ (sh @@ string "datatype") @@ var "i"],
      Shacl._CommonConstraint_class>>: constant $
        -- RdfsClass is a stand-in type (wraps unit), not usable as an IRI
        list ([] :: [TypedTerm Rdf.Triple]),
      Shacl._CommonConstraint_nodeKind>>: lambda "nk" $
        list [tripleRII @@ var "subj" @@ (sh @@ string "nodeKind") @@ (nodeKindToIri @@ var "nk")],
      Shacl._CommonConstraint_property>>: lambda "refs" $
        Lists.concat (Lists.map (propertyRefToTriples @@ var "subj") (Sets.toList (var "refs"))),
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
          Optionals.cases (var "flags_") (list ([] :: [TypedTerm Rdf.Triple]))
            (lambda "f" $ list [tripleRIN @@ var "subj" @@ (sh @@ string "flags") @@ (stringNode @@ var "f")])]),
      Shacl._CommonConstraint_closed>>: lambda "cl" $
        list [tripleRIN @@ var "subj" @@ (sh @@ string "closed")
          @@ (booleanNode @@ (project Shacl._Closed Shacl._Closed_isClosed @@ var "cl"))],
      Shacl._CommonConstraint_hasValue>>: lambda "nodes" $
        Lists.map (lambda "n" $ tripleRIN @@ var "subj" @@ (sh @@ string "hasValue") @@ var "n")
          (Sets.toList (var "nodes" :: TypedTerm (S.Set Rdf.Node))),
      Shacl._CommonConstraint_in>>: lambda "nodes" $
        -- sh:in is an RDF list; simplified here as individual triples
        Lists.map (lambda "n" $ tripleRIN @@ var "subj" @@ (sh @@ string "in") @@ var "n") (var "nodes"),
      Shacl._CommonConstraint_node>>: lambda "refs" $
        Lists.concat (Lists.map
          (lambda "ref" $ cases Shacl._Reference (var "ref") (Just (list ([] :: [TypedTerm Rdf.Triple]))) [
            Shacl._Reference_named>>: lambda "i" $ list [tripleRII @@ var "subj" @@ (sh @@ string "node") @@ var "i"]])
          (Sets.toList (var "refs" :: TypedTerm (S.Set (Shacl.Reference Shacl.NodeShape))))),
      Shacl._CommonConstraint_not>>: lambda "refs" $
        Lists.concat (Lists.map
          (lambda "ref" $ cases Shacl._Reference (var "ref") (Just (list ([] :: [TypedTerm Rdf.Triple]))) [
            Shacl._Reference_named>>: lambda "i" $ list [tripleRII @@ var "subj" @@ (sh @@ string "not") @@ var "i"]])
          (Sets.toList (var "refs" :: TypedTerm (S.Set (Shacl.Reference Shacl.Shape))))),
      Shacl._CommonConstraint_and>>: lambda "refs" $
        Lists.concat (Lists.map
          (lambda "ref" $ cases Shacl._Reference (var "ref") (Just (list ([] :: [TypedTerm Rdf.Triple]))) [
            Shacl._Reference_named>>: lambda "i" $ list [tripleRII @@ var "subj" @@ (sh @@ string "and") @@ var "i"]])
          (Sets.toList (var "refs" :: TypedTerm (S.Set (Shacl.Reference Shacl.Shape))))),
      Shacl._CommonConstraint_or>>: lambda "refs" $
        Lists.concat (Lists.map
          (lambda "ref" $ cases Shacl._Reference (var "ref") (Just (list ([] :: [TypedTerm Rdf.Triple]))) [
            Shacl._Reference_named>>: lambda "i" $ list [tripleRII @@ var "subj" @@ (sh @@ string "or") @@ var "i"]])
          (Sets.toList (var "refs" :: TypedTerm (S.Set (Shacl.Reference Shacl.Shape))))),
      Shacl._CommonConstraint_xone>>: lambda "refs" $
        Lists.concat (Lists.map
          (lambda "ref" $ cases Shacl._Reference (var "ref") (Just (list ([] :: [TypedTerm Rdf.Triple]))) [
            Shacl._Reference_named>>: lambda "i" $ list [tripleRII @@ var "subj" @@ (sh @@ string "xone") @@ var "i"]])
          (Sets.toList (var "refs" :: TypedTerm (S.Set (Shacl.Reference Shacl.Shape))))),
      Shacl._CommonConstraint_equals>>: constant $
        -- Property is a stand-in type, not usable as an IRI
        list ([] :: [TypedTerm Rdf.Triple]),
      Shacl._CommonConstraint_disjoint>>: constant $
        -- Property is a stand-in type, not usable as an IRI
        list ([] :: [TypedTerm Rdf.Triple]),
      Shacl._CommonConstraint_languageIn>>: constant $
        -- Language-in requires RDF list encoding; omit for now
        list ([] :: [TypedTerm Rdf.Triple])]

-- | Serialize CommonProperties to triples
commonPropertiesToTriples :: TypedTermDefinition (Rdf.Resource -> Shacl.CommonProperties -> [Rdf.Triple])
commonPropertiesToTriples = define "commonPropertiesToTriples" $
  doc "Serialize CommonProperties to triples" $
  lambda "subj" $ lambda "cp" $
    Lists.concat (list [
      Lists.concat (Lists.map
        (commonConstraintToTriples @@ var "subj")
        (Sets.toList (project Shacl._CommonProperties Shacl._CommonProperties_constraints @@ var "cp"))),
      Optionals.cases
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
    cases Shacl._Shape (var "shape") Nothing [
      Shacl._Shape_node>>: lambda "ns_" $ nodeShapeToTriples @@ var "iri_" @@ var "ns_",
      Shacl._Shape_property>>: lambda "ps" $ propertyShapeToTriples @@ (iriResource @@ var "iri_") @@ var "ps"]

-- | Create an integer literal node
integerNode :: TypedTermDefinition (Integer -> Rdf.Node)
integerNode = define "integerNode" $
  doc "Create an integer literal node" $
  lambda "n" $
    inject Rdf._Node Rdf._Node_literal $
      record Rdf._Literal [
        Rdf._Literal_lexicalForm>>: Literals.showBigint (var "n"),
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
    cases Rdf._IriOrLiteral (var "il") Nothing [
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
    cases Shacl._NodeKind (var "nk") Nothing [
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
    Lists.concat (list [
      list [
        tripleIII @@ var "iri_" @@ (rdf @@ string "type") @@ (sh @@ string "NodeShape"),
        tripleIII @@ var "iri_" @@ (sh @@ string "targetClass") @@ var "iri_"],
      commonPropertiesToTriples @@ (iriResource @@ var "iri_")
        @@ (project Shacl._NodeShape Shacl._NodeShape_common @@ var "ns_")])

-- | Serialize a property shape reference (may be inline or named)
propertyRefToTriples :: TypedTermDefinition (Rdf.Resource -> Shacl.Reference Shacl.PropertyShape -> [Rdf.Triple])
propertyRefToTriples = define "propertyRefToTriples" $
  doc "Serialize a property shape reference (may be inline or named)" $
  lambda "subj" $ lambda "ref" $
    cases Shacl._Reference (var "ref") Nothing [
      Shacl._Reference_named>>: lambda "i" $
        list [tripleRII @@ var "subj" @@ (sh @@ string "property") @@ var "i"],
      Shacl._Reference_definition>>: lambda "def" $ lets [
        "iri_">: project Shacl._Definition Shacl._Definition_iri @@ var "def",
        "ps">: project Shacl._Definition Shacl._Definition_target @@ var "def"] $
        Lists.concat (list [
          list [tripleRII @@ var "subj" @@ (sh @@ string "property") @@ var "iri_"],
          propertyShapeToTriples @@ (iriResource @@ var "iri_") @@ var "ps"]),
      Shacl._Reference_anonymous>>: constant $
        -- Anonymous property shapes would need blank node allocation; skip for now
        list ([] :: [TypedTerm Rdf.Triple])]

-- | Serialize a PropertyShapeConstraint to triples
propertyShapeConstraintToTriples :: TypedTermDefinition (Rdf.Resource -> Shacl.PropertyShapeConstraint -> [Rdf.Triple])
propertyShapeConstraintToTriples = define "propertyShapeConstraintToTriples" $
  doc "Serialize a PropertyShapeConstraint to triples" $
  lambda "subj" $ lambda "psc" $
    cases Shacl._PropertyShapeConstraint (var "psc") Nothing [
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
      Optionals.cases
        (project Shacl._PropertyShape Shacl._PropertyShape_order @@ var "ps")
        (list ([] :: [TypedTerm Rdf.Triple]))
        (lambda "n" $ list [tripleRIN @@ var "subj" @@ (sh @@ string "order") @@ (integerNode @@ var "n")])])

-- | RDF namespace
rdf :: TypedTermDefinition (String -> Rdf.Iri)
rdf = define "rdf" $
  doc "Construct an IRI in the RDF namespace" $
  lambda "local" $
    RdfUtils.rdfIri @@ var "local"

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
