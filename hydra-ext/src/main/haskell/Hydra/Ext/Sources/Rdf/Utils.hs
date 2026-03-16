module Hydra.Ext.Sources.Rdf.Utils where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Grammar                    as Grammar
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
import qualified Hydra.Dsl.Module                     as Module
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
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
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
import qualified Hydra.Ext.Org.W3.Rdf.Syntax as Rdf
import qualified Hydra.Ext.Sources.Rdf.Syntax as RdfSyntax


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.rdf.utils"

module_ :: Module
module_ = Module ns elements
    [Formatting.ns, Names.ns, Annotations.ns]
    (RdfSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Utility functions for working with RDF graphs and descriptions"
  where
    elements = [
      toBinding key_rdfBlankNodeCounter,
      toBinding descriptionsToGraph,
      toBinding emptyDescription,
      toBinding emptyRdfGraph,
      toBinding emptyLangStrings,
      toBinding encodeLiteral,
      toBinding forObjects,
      toBinding iri,
      toBinding keyIri,
      toBinding mergeGraphs,
      toBinding nameToIri,
      toBinding nextBlankNode,
      toBinding propertyIri,
      toBinding rdfIri,
      toBinding resourceToNode,
      toBinding subjectsOf,
      toBinding triplesOf,
      toBinding xmlSchemaDatatypeIri]


-- | The key used for tracking blank node counters
key_rdfBlankNodeCounter :: TBinding Name
key_rdfBlankNodeCounter = define "key_rdfBlankNodeCounter" $
  doc "The key used for tracking blank node counters" $
  wrap _Name $ string "rdfBlankNodeCounter"

-- | Convert a list of descriptions to an RDF graph
descriptionsToGraph :: TBinding ([Rdf.Description] -> Rdf.Graph)
descriptionsToGraph = define "descriptionsToGraph" $
  doc "Convert a list of descriptions to an RDF graph" $
  lambda "ds" $
    wrap Rdf._Graph (Sets.fromList (triplesOf @@ var "ds"))

-- | Create an empty description with the given node
emptyDescription :: TBinding (Rdf.Node -> Rdf.Description)
emptyDescription = define "emptyDescription" $
  doc "Create an empty description with a given node" $
  lambda "node" $
    record Rdf._Description [
      Rdf._Description_subject>>: var "node",
      Rdf._Description_graph>>: emptyRdfGraph]

-- | An empty RDF graph
emptyRdfGraph :: TBinding Rdf.Graph
emptyRdfGraph = define "emptyRdfGraph" $
  doc "An empty RDF graph" $
  wrap Rdf._Graph Sets.empty

-- | An empty LangStrings value
emptyLangStrings :: TBinding Rdf.LangStrings
emptyLangStrings = define "emptyLangStrings" $
  doc "An empty LangStrings value" $
  wrap Rdf._LangStrings Maps.empty

-- | Encode a Hydra literal as an RDF literal
encodeLiteral :: TBinding (Literal -> Rdf.Literal)
encodeLiteral = define "encodeLiteral" $
  doc "Encode a Hydra literal as an RDF literal" $
  lambda "lit" $
    cases _Literal (var "lit") Nothing [
      _Literal_binary>>: lambda "s" $
        record Rdf._Literal [
          Rdf._Literal_lexicalForm>>: Literals.binaryToString (var "s"),
          Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "base64Binary",
          Rdf._Literal_languageTag>>: nothing],
      _Literal_boolean>>: lambda "b" $
        record Rdf._Literal [
          Rdf._Literal_lexicalForm>>: Logic.ifElse (var "b") (string "true") (string "false"),
          Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "boolean",
          Rdf._Literal_languageTag>>: nothing],
      _Literal_float>>: lambda "f" $
        cases _FloatValue (var "f") Nothing [
          _FloatValue_bigfloat>>: lambda "v" $
            record Rdf._Literal [
              Rdf._Literal_lexicalForm>>: Literals.showBigfloat (var "v"),
              Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "decimal",
              Rdf._Literal_languageTag>>: nothing],
          _FloatValue_float32>>: lambda "v" $
            record Rdf._Literal [
              Rdf._Literal_lexicalForm>>: Literals.showFloat32 (var "v"),
              Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "float",
              Rdf._Literal_languageTag>>: nothing],
          _FloatValue_float64>>: lambda "v" $
            record Rdf._Literal [
              Rdf._Literal_lexicalForm>>: Literals.showFloat64 (var "v"),
              Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "double",
              Rdf._Literal_languageTag>>: nothing]],
      _Literal_integer>>: lambda "i" $
        cases _IntegerValue (var "i") Nothing [
          _IntegerValue_bigint>>: lambda "v" $
            record Rdf._Literal [
              Rdf._Literal_lexicalForm>>: Literals.showBigint (var "v"),
              Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "integer",
              Rdf._Literal_languageTag>>: nothing],
          _IntegerValue_int8>>: lambda "v" $
            record Rdf._Literal [
              Rdf._Literal_lexicalForm>>: Literals.showInt8 (var "v"),
              Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "byte",
              Rdf._Literal_languageTag>>: nothing],
          _IntegerValue_int16>>: lambda "v" $
            record Rdf._Literal [
              Rdf._Literal_lexicalForm>>: Literals.showInt16 (var "v"),
              Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "short",
              Rdf._Literal_languageTag>>: nothing],
          _IntegerValue_int32>>: lambda "v" $
            record Rdf._Literal [
              Rdf._Literal_lexicalForm>>: Literals.showInt32 (var "v"),
              Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "int",
              Rdf._Literal_languageTag>>: nothing],
          _IntegerValue_int64>>: lambda "v" $
            record Rdf._Literal [
              Rdf._Literal_lexicalForm>>: Literals.showInt64 (var "v"),
              Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "long",
              Rdf._Literal_languageTag>>: nothing],
          _IntegerValue_uint8>>: lambda "v" $
            record Rdf._Literal [
              Rdf._Literal_lexicalForm>>: Literals.showUint8 (var "v"),
              Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "unsignedByte",
              Rdf._Literal_languageTag>>: nothing],
          _IntegerValue_uint16>>: lambda "v" $
            record Rdf._Literal [
              Rdf._Literal_lexicalForm>>: Literals.showUint16 (var "v"),
              Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "unsignedShort",
              Rdf._Literal_languageTag>>: nothing],
          _IntegerValue_uint32>>: lambda "v" $
            record Rdf._Literal [
              Rdf._Literal_lexicalForm>>: Literals.showUint32 (var "v"),
              Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "unsignedInt",
              Rdf._Literal_languageTag>>: nothing],
          _IntegerValue_uint64>>: lambda "v" $
            record Rdf._Literal [
              Rdf._Literal_lexicalForm>>: Literals.showUint64 (var "v"),
              Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "unsignedLong",
              Rdf._Literal_languageTag>>: nothing]],
      _Literal_string>>: lambda "s" $
        record Rdf._Literal [
          Rdf._Literal_lexicalForm>>: var "s",
          Rdf._Literal_datatypeIri>>: xmlSchemaDatatypeIri @@ string "string",
          Rdf._Literal_languageTag>>: nothing]]

-- | Create triples from a subject, predicate, and list of object nodes
forObjects :: TBinding (Rdf.Resource -> Rdf.Iri -> [Rdf.Node] -> [Rdf.Triple])
forObjects = define "forObjects" $
  doc "Create triples from a subject, predicate, and list of object nodes" $
  lambda "subj" $ lambda "pred" $ lambda "objs" $
    Lists.map
      (lambda "obj" $ record Rdf._Triple [
        Rdf._Triple_subject>>: var "subj",
        Rdf._Triple_predicate>>: var "pred",
        Rdf._Triple_object>>: var "obj"])
      (var "objs")

-- | Construct an IRI from a namespace and local name
iri :: TBinding (String -> String -> Rdf.Iri)
iri = define "iri" $
  doc "Construct an IRI from a namespace and local name" $
  lambda "ns" $ lambda "local" $
    wrap Rdf._Iri (Strings.cat2 (var "ns") (var "local"))

-- | Construct a key IRI from a local name
keyIri :: TBinding (String -> Rdf.Iri)
keyIri = define "keyIri" $
  doc "Construct a key IRI from a local name" $
  lambda "local" $
    iri @@ string "urn:key:" @@ var "local"

-- | Merge a list of RDF graphs into a single graph
mergeGraphs :: TBinding ([Rdf.Graph] -> Rdf.Graph)
mergeGraphs = define "mergeGraphs" $
  doc "Merge a list of RDF graphs into a single graph" $
  lambda "graphs" $
    wrap Rdf._Graph
      (Sets.unions (Lists.map (unwrap Rdf._Graph) (var "graphs")))

-- | Convert a Hydra name to an RDF IRI
nameToIri :: TBinding (Name -> Rdf.Iri)
nameToIri = define "nameToIri" $
  doc "Convert a Hydra name to an RDF IRI" $
  lambda "name" $
    wrap Rdf._Iri (Strings.cat2 (string "urn:") (Core.unName $ var "name"))

-- | Generate the next blank node and an updated context
nextBlankNode :: TBinding (Context -> (Rdf.Resource, Context))
nextBlankNode = define "nextBlankNode" $
  doc "Generate the next blank node and an updated context" $
  lambda "cx" $ lets [
    "result">: Annotations.nextCount @@ key_rdfBlankNodeCounter @@ var "cx",
    "count">: Pairs.first (var "result"),
    "cx'">: Pairs.second (var "result")] $
    pair
      (inject Rdf._Resource Rdf._Resource_bnode
        (wrap Rdf._BlankNode (Strings.cat2 (string "b") (Literals.showInt32 (var "count")))))
      (var "cx'")

-- | Construct a property IRI from a record name and field name
propertyIri :: TBinding (Name -> Name -> Rdf.Iri)
propertyIri = define "propertyIri" $
  doc "Construct a property IRI from a record name and field name" $
  lambda "rname" $ lambda "fname" $ lets [
    "qualName">: Names.qualifyName @@ var "rname",
    "gname">: Module.qualifiedNameNamespace (var "qualName"),
    "local_">: Module.qualifiedNameLocal (var "qualName")] $
    wrap Rdf._Iri
      (Strings.cat $ list [
        string "urn:",
        Maybes.maybe (string "") (unwrap _Namespace) (var "gname"),
        string "#",
        Formatting.decapitalize @@ var "local_",
        Formatting.capitalize @@ (Core.unName $ var "fname")])

-- | Construct an RDF namespace IRI
rdfIri :: TBinding (String -> Rdf.Iri)
rdfIri = define "rdfIri" $
  doc "Construct an RDF namespace IRI" $
  lambda "local" $
    iri @@ string "http://www.w3.org/1999/02/22-rdf-syntax-ns#" @@ var "local"

-- | Convert a resource to a node
resourceToNode :: TBinding (Rdf.Resource -> Rdf.Node)
resourceToNode = define "resourceToNode" $
  doc "Convert a resource to a node" $
  lambda "r" $
    cases Rdf._Resource (var "r") Nothing [
      Rdf._Resource_iri>>: lambda "i" $ inject Rdf._Node Rdf._Node_iri (var "i"),
      Rdf._Resource_bnode>>: lambda "b" $ inject Rdf._Node Rdf._Node_bnode (var "b")]

-- | Extract subjects from a list of descriptions
subjectsOf :: TBinding ([Rdf.Description] -> [Rdf.Node])
subjectsOf = define "subjectsOf" $
  doc "Extract subjects from a list of descriptions" $
  lambda "descs" $
    Lists.map (project Rdf._Description Rdf._Description_subject) (var "descs")

-- | Extract all triples from a list of descriptions
triplesOf :: TBinding ([Rdf.Description] -> [Rdf.Triple])
triplesOf = define "triplesOf" $
  doc "Extract all triples from a list of descriptions" $
  lambda "descs" $
    Lists.concat (Lists.map
      (lambda "d" $ Sets.toList (unwrap Rdf._Graph @@ (project Rdf._Description Rdf._Description_graph @@ var "d")))
      (var "descs"))

-- | Construct an XML Schema datatype IRI
xmlSchemaDatatypeIri :: TBinding (String -> Rdf.Iri)
xmlSchemaDatatypeIri = define "xmlSchemaDatatypeIri" $
  doc "Construct an XML Schema datatype IRI" $
  lambda "local" $
    iri @@ string "http://www.w3.org/2001/XMLSchema#" @@ var "local"
