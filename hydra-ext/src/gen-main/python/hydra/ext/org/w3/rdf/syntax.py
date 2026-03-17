# Note: this is an automatically generated file. Do not edit.

r"""An RDF 1.1 syntax model."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Maybe, Node
from typing import Annotated, TypeAlias, cast
import hydra.core

class BlankNode(Node[str]):
    ...

BlankNode.TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.BlankNode")

class RdfsClass(Node[None]):
    r"""Stand-in for rdfs:Class."""

RdfsClass.TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.RdfsClass")

class Dataset(Node["frozenset[Quad]"]):
    ...

Dataset.TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Dataset")

@dataclass(frozen=True)
class Description:
    r"""A graph of RDF statements together with a distinguished subject and/or object node."""
    
    subject: Node_
    graph: Graph
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Description")
    SUBJECT = hydra.core.Name("subject")
    GRAPH = hydra.core.Name("graph")

class Graph(Node["frozenset[Triple]"]):
    ...

Graph.TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Graph")

class Iri(Node[str]):
    r"""An Internationalized Resource Identifier."""

Iri.TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Iri")

class IriOrLiteralIri(Node["Iri"]):
    ...

class IriOrLiteralLiteral(Node["Literal"]):
    ...

class _IriOrLiteralMeta(type):
    def __getitem__(cls, item):
        return object

# An IRI or a literal; this type is a convenience for downstream models like SHACL which may exclude blank nodes.
class IriOrLiteral(metaclass=_IriOrLiteralMeta):
    r"""IriOrLiteralIri | IriOrLiteralLiteral"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.IriOrLiteral")
    IRI = hydra.core.Name("iri")
    LITERAL = hydra.core.Name("literal")

class LangStrings(Node["FrozenDict[Maybe[LanguageTag], str]"]):
    r"""A convenience type which provides at most one string value per language, and optionally a value without a language."""

LangStrings.TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.LangStrings")

class LanguageTag(Node[str]):
    r"""A BCP47 language tag."""

LanguageTag.TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.LanguageTag")

@dataclass(frozen=True)
class Literal:
    r"""A value such as a string, number, or date."""
    
    lexical_form: Annotated[str, "a Unicode string, which should be in Normal Form C"]
    datatype_iri: Annotated[Iri, "an IRI identifying a datatype that determines how the lexical form maps to a literal value"]
    language_tag: Annotated[Maybe[LanguageTag], "An optional language tag, present if and only if the datatype IRI is http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Literal")
    LEXICAL_FORM = hydra.core.Name("lexicalForm")
    DATATYPE_IRI = hydra.core.Name("datatypeIri")
    LANGUAGE_TAG = hydra.core.Name("languageTag")

class NodeIri(Node["Iri"]):
    ...

class NodeBnode(Node["BlankNode"]):
    ...

class NodeLiteral(Node["Literal"]):
    ...

class _Node_Meta(type):
    def __getitem__(cls, item):
        return object

class Node_(metaclass=_Node_Meta):
    r"""NodeIri | NodeBnode | NodeLiteral"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Node")
    IRI = hydra.core.Name("iri")
    BNODE = hydra.core.Name("bnode")
    LITERAL = hydra.core.Name("literal")

@dataclass(frozen=True)
class Property:
    r"""A type representing an RDF property, and encapsulating its domain, range, and subclass relationships."""
    
    domain: Annotated[frozenset[RdfsClass], "State that any resource that has a given property is an instance of one or more classes"]
    range_: Annotated[frozenset[RdfsClass], "States that the values of a property are instances of one or more classes"]
    sub_property_of: frozenset[Property]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Property")
    DOMAIN = hydra.core.Name("domain")
    RANGE = hydra.core.Name("range")
    SUB_PROPERTY_OF = hydra.core.Name("subPropertyOf")

@dataclass(frozen=True)
class Quad:
    r"""An RDF triple with an optional named graph component."""
    
    subject: Resource
    predicate: Iri
    object: Node_
    graph: Maybe[Iri]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Quad")
    SUBJECT = hydra.core.Name("subject")
    PREDICATE = hydra.core.Name("predicate")
    OBJECT = hydra.core.Name("object")
    GRAPH = hydra.core.Name("graph")

class ResourceIri(Node["Iri"]):
    ...

class ResourceBnode(Node["BlankNode"]):
    ...

class _ResourceMeta(type):
    def __getitem__(cls, item):
        return object

class Resource(metaclass=_ResourceMeta):
    r"""ResourceIri | ResourceBnode"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Resource")
    IRI = hydra.core.Name("iri")
    BNODE = hydra.core.Name("bnode")

@dataclass(frozen=True)
class Triple:
    r"""An RDF triple defined by a subject, predicate, and object."""
    
    subject: Resource
    predicate: Iri
    object: Node_
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Triple")
    SUBJECT = hydra.core.Name("subject")
    PREDICATE = hydra.core.Name("predicate")
    OBJECT = hydra.core.Name("object")
