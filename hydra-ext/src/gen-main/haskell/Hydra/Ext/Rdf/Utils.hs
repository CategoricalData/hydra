-- Note: this is an automatically generated file. Do not edit.

-- | Utility functions for working with RDF graphs and descriptions

module Hydra.Ext.Rdf.Utils where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.W3.Rdf.Syntax as Syntax
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | The key used for tracking blank node counters
key_rdfBlankNodeCounter :: Core.Name
key_rdfBlankNodeCounter = Core.Name "rdfBlankNodeCounter"

-- | Convert a list of descriptions to an RDF graph
descriptionsToGraph :: [Syntax.Description] -> Syntax.Graph
descriptionsToGraph ds = Syntax.Graph (Sets.fromList (triplesOf ds))

-- | Create an empty description with a given node
emptyDescription :: Syntax.Node -> Syntax.Description
emptyDescription node =
    Syntax.Description {
      Syntax.descriptionSubject = node,
      Syntax.descriptionGraph = emptyRdfGraph}

-- | An empty RDF graph
emptyRdfGraph :: Syntax.Graph
emptyRdfGraph = Syntax.Graph Sets.empty

-- | An empty LangStrings value
emptyLangStrings :: Syntax.LangStrings
emptyLangStrings = Syntax.LangStrings Maps.empty

-- | Encode a Hydra literal as an RDF literal
encodeLiteral :: Core.Literal -> Syntax.Literal
encodeLiteral lit =
    case lit of
      Core.LiteralBinary v0 -> Syntax.Literal {
        Syntax.literalLexicalForm = (Literals.binaryToString v0),
        Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "base64Binary"),
        Syntax.literalLanguageTag = Nothing}
      Core.LiteralBoolean v0 -> Syntax.Literal {
        Syntax.literalLexicalForm = (Logic.ifElse v0 "true" "false"),
        Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "boolean"),
        Syntax.literalLanguageTag = Nothing}
      Core.LiteralFloat v0 -> case v0 of
        Core.FloatValueBigfloat v1 -> Syntax.Literal {
          Syntax.literalLexicalForm = (Literals.showBigfloat v1),
          Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "decimal"),
          Syntax.literalLanguageTag = Nothing}
        Core.FloatValueFloat32 v1 -> Syntax.Literal {
          Syntax.literalLexicalForm = (Literals.showFloat32 v1),
          Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "float"),
          Syntax.literalLanguageTag = Nothing}
        Core.FloatValueFloat64 v1 -> Syntax.Literal {
          Syntax.literalLexicalForm = (Literals.showFloat64 v1),
          Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "double"),
          Syntax.literalLanguageTag = Nothing}
      Core.LiteralInteger v0 -> case v0 of
        Core.IntegerValueBigint v1 -> Syntax.Literal {
          Syntax.literalLexicalForm = (Literals.showBigint v1),
          Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "integer"),
          Syntax.literalLanguageTag = Nothing}
        Core.IntegerValueInt8 v1 -> Syntax.Literal {
          Syntax.literalLexicalForm = (Literals.showInt8 v1),
          Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "byte"),
          Syntax.literalLanguageTag = Nothing}
        Core.IntegerValueInt16 v1 -> Syntax.Literal {
          Syntax.literalLexicalForm = (Literals.showInt16 v1),
          Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "short"),
          Syntax.literalLanguageTag = Nothing}
        Core.IntegerValueInt32 v1 -> Syntax.Literal {
          Syntax.literalLexicalForm = (Literals.showInt32 v1),
          Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "int"),
          Syntax.literalLanguageTag = Nothing}
        Core.IntegerValueInt64 v1 -> Syntax.Literal {
          Syntax.literalLexicalForm = (Literals.showInt64 v1),
          Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "long"),
          Syntax.literalLanguageTag = Nothing}
        Core.IntegerValueUint8 v1 -> Syntax.Literal {
          Syntax.literalLexicalForm = (Literals.showUint8 v1),
          Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "unsignedByte"),
          Syntax.literalLanguageTag = Nothing}
        Core.IntegerValueUint16 v1 -> Syntax.Literal {
          Syntax.literalLexicalForm = (Literals.showUint16 v1),
          Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "unsignedShort"),
          Syntax.literalLanguageTag = Nothing}
        Core.IntegerValueUint32 v1 -> Syntax.Literal {
          Syntax.literalLexicalForm = (Literals.showUint32 v1),
          Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "unsignedInt"),
          Syntax.literalLanguageTag = Nothing}
        Core.IntegerValueUint64 v1 -> Syntax.Literal {
          Syntax.literalLexicalForm = (Literals.showUint64 v1),
          Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "unsignedLong"),
          Syntax.literalLanguageTag = Nothing}
      Core.LiteralString v0 -> Syntax.Literal {
        Syntax.literalLexicalForm = v0,
        Syntax.literalDatatypeIri = (xmlSchemaDatatypeIri "string"),
        Syntax.literalLanguageTag = Nothing}

-- | Create triples from a subject, predicate, and list of object nodes
forObjects :: Syntax.Resource -> Syntax.Iri -> [Syntax.Node] -> [Syntax.Triple]
forObjects subj pred objs =
    Lists.map (\obj -> Syntax.Triple {
      Syntax.tripleSubject = subj,
      Syntax.triplePredicate = pred,
      Syntax.tripleObject = obj}) objs

-- | Construct an IRI from a namespace and local name
iri :: String -> String -> Syntax.Iri
iri ns local = Syntax.Iri (Strings.cat2 ns local)

-- | Construct a key IRI from a local name
keyIri :: String -> Syntax.Iri
keyIri local = iri "urn:key:" local

-- | Merge a list of RDF graphs into a single graph
mergeGraphs :: [Syntax.Graph] -> Syntax.Graph
mergeGraphs graphs = Syntax.Graph (Sets.unions (Lists.map Syntax.unGraph graphs))

-- | Convert a Hydra name to an RDF IRI
nameToIri :: Core.Name -> Syntax.Iri
nameToIri name = Syntax.Iri (Strings.cat2 "urn:" (Core.unName name))

-- | Generate the next blank node and an updated context
nextBlankNode :: Context.Context -> (Syntax.Resource, Context.Context)
nextBlankNode cx =
     
      let result = Annotations.nextCount key_rdfBlankNodeCounter cx 
          count = Pairs.first result
          cx_ = Pairs.second result
      in (Syntax.ResourceBnode (Syntax.BlankNode (Strings.cat2 "b" (Literals.showInt32 count))), cx_)

-- | Construct a property IRI from a record name and field name
propertyIri :: Core.Name -> Core.Name -> Syntax.Iri
propertyIri rname fname =
     
      let qualName = Names.qualifyName rname 
          gname = Module.qualifiedNameNamespace qualName
          local_ = Module.qualifiedNameLocal qualName
      in (Syntax.Iri (Strings.cat [
        "urn:",
        (Maybes.maybe "" Module.unNamespace gname),
        "#",
        (Formatting.decapitalize local_),
        (Formatting.capitalize (Core.unName fname))]))

-- | Construct an RDF namespace IRI
rdfIri :: String -> Syntax.Iri
rdfIri local = iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#" local

-- | Convert a resource to a node
resourceToNode :: Syntax.Resource -> Syntax.Node
resourceToNode r =
    case r of
      Syntax.ResourceIri v0 -> Syntax.NodeIri v0
      Syntax.ResourceBnode v0 -> Syntax.NodeBnode v0

-- | Extract subjects from a list of descriptions
subjectsOf :: [Syntax.Description] -> [Syntax.Node]
subjectsOf descs = Lists.map Syntax.descriptionSubject descs

-- | Extract all triples from a list of descriptions
triplesOf :: [Syntax.Description] -> [Syntax.Triple]
triplesOf descs = Lists.concat (Lists.map (\d -> Sets.toList (Syntax.unGraph (Syntax.descriptionGraph d))) descs)

-- | Construct an XML Schema datatype IRI
xmlSchemaDatatypeIri :: String -> Syntax.Iri
xmlSchemaDatatypeIri local = iri "http://www.w3.org/2001/XMLSchema#" local
