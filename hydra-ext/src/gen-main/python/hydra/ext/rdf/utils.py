# Note: this is an automatically generated file. Do not edit.

r"""Utility functions for working with RDF graphs and descriptions."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Nothing, frozenlist
from typing import cast
import hydra.annotations
import hydra.core
import hydra.ext.org.w3.rdf.syntax
import hydra.formatting
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.names

def triples_of(descs: frozenlist[hydra.ext.org.w3.rdf.syntax.Description]) -> frozenlist[hydra.ext.org.w3.rdf.syntax.Triple]:
    r"""Extract all triples from a list of descriptions."""
    
    return hydra.lib.lists.concat(hydra.lib.lists.map((lambda d: hydra.lib.sets.to_list(d.graph.value)), descs))

def descriptions_to_graph(ds: frozenlist[hydra.ext.org.w3.rdf.syntax.Description]) -> hydra.ext.org.w3.rdf.syntax.Graph:
    r"""Convert a list of descriptions to an RDF graph."""
    
    return hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.from_list(triples_of(ds)))

@lru_cache(1)
def empty_rdf_graph() -> hydra.ext.org.w3.rdf.syntax.Graph:
    r"""An empty RDF graph."""
    
    return hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.empty())

def empty_description(node: hydra.ext.org.w3.rdf.syntax.Node_) -> hydra.ext.org.w3.rdf.syntax.Description:
    r"""Create an empty description with a given node."""
    
    return hydra.ext.org.w3.rdf.syntax.Description(node, empty_rdf_graph())

@lru_cache(1)
def empty_lang_strings() -> hydra.ext.org.w3.rdf.syntax.LangStrings:
    r"""An empty LangStrings value."""
    
    return hydra.ext.org.w3.rdf.syntax.LangStrings(hydra.lib.maps.empty())

def iri(ns: str, local: str) -> hydra.ext.org.w3.rdf.syntax.Iri:
    r"""Construct an IRI from a namespace and local name."""
    
    return hydra.ext.org.w3.rdf.syntax.Iri(hydra.lib.strings.cat2(ns, local))

def xml_schema_datatype_iri(local: str) -> hydra.ext.org.w3.rdf.syntax.Iri:
    r"""Construct an XML Schema datatype IRI."""
    
    return iri("http://www.w3.org/2001/XMLSchema#", local)

def encode_literal(lit: hydra.core.Literal):
    def _hoist_hydra_ext_rdf_utils_encode_literal_1(v1):
        match v1:
            case hydra.core.FloatValueBigfloat(value=v):
                return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.show_bigfloat(v), xml_schema_datatype_iri("decimal"), Nothing())
            
            case hydra.core.FloatValueFloat32(value=v):
                return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.show_float32(v), xml_schema_datatype_iri("float"), Nothing())
            
            case hydra.core.FloatValueFloat64(value=v):
                return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.show_float64(v), xml_schema_datatype_iri("double"), Nothing())
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_ext_rdf_utils_encode_literal_2(v1):
        match v1:
            case hydra.core.IntegerValueBigint(value=v):
                return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.show_bigint(v), xml_schema_datatype_iri("integer"), Nothing())
            
            case hydra.core.IntegerValueInt8(value=v):
                return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.show_int8(v), xml_schema_datatype_iri("byte"), Nothing())
            
            case hydra.core.IntegerValueInt16(value=v):
                return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.show_int16(v), xml_schema_datatype_iri("short"), Nothing())
            
            case hydra.core.IntegerValueInt32(value=v):
                return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.show_int32(v), xml_schema_datatype_iri("int"), Nothing())
            
            case hydra.core.IntegerValueInt64(value=v):
                return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.show_int64(v), xml_schema_datatype_iri("long"), Nothing())
            
            case hydra.core.IntegerValueUint8(value=v):
                return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.show_uint8(v), xml_schema_datatype_iri("unsignedByte"), Nothing())
            
            case hydra.core.IntegerValueUint16(value=v):
                return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.show_uint16(v), xml_schema_datatype_iri("unsignedShort"), Nothing())
            
            case hydra.core.IntegerValueUint32(value=v):
                return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.show_uint32(v), xml_schema_datatype_iri("unsignedInt"), Nothing())
            
            case hydra.core.IntegerValueUint64(value=v):
                return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.show_uint64(v), xml_schema_datatype_iri("unsignedLong"), Nothing())
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    match lit:
        case hydra.core.LiteralBinary(value=s):
            return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.binary_to_string(s), xml_schema_datatype_iri("base64Binary"), Nothing())
        
        case hydra.core.LiteralBoolean(value=b):
            return hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.logic.if_else(b, (lambda : "true"), (lambda : "false")), xml_schema_datatype_iri("boolean"), Nothing())
        
        case hydra.core.LiteralFloat(value=f):
            return _hoist_hydra_ext_rdf_utils_encode_literal_1(f)
        
        case hydra.core.LiteralInteger(value=i):
            return _hoist_hydra_ext_rdf_utils_encode_literal_2(i)
        
        case hydra.core.LiteralString(value=s2):
            return hydra.ext.org.w3.rdf.syntax.Literal(s2, xml_schema_datatype_iri("string"), Nothing())
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def for_objects(subj: hydra.ext.org.w3.rdf.syntax.Resource, pred: hydra.ext.org.w3.rdf.syntax.Iri, objs: frozenlist[hydra.ext.org.w3.rdf.syntax.Node_]) -> frozenlist[hydra.ext.org.w3.rdf.syntax.Triple]:
    r"""Create triples from a subject, predicate, and list of object nodes."""
    
    return hydra.lib.lists.map((lambda obj: hydra.ext.org.w3.rdf.syntax.Triple(subj, pred, obj)), objs)

def key_iri(local: str) -> hydra.ext.org.w3.rdf.syntax.Iri:
    r"""Construct a key IRI from a local name."""
    
    return iri("urn:key:", local)

# The key used for tracking blank node counters.
key_rdf_blank_node_counter = hydra.core.Name("rdfBlankNodeCounter")

def merge_graphs(graphs: frozenlist[hydra.ext.org.w3.rdf.syntax.Graph]) -> hydra.ext.org.w3.rdf.syntax.Graph:
    r"""Merge a list of RDF graphs into a single graph."""
    
    return hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.unions(hydra.lib.lists.map((lambda v1: v1.value), graphs)))

def name_to_iri(name: hydra.core.Name) -> hydra.ext.org.w3.rdf.syntax.Iri:
    r"""Convert a Hydra name to an RDF IRI."""
    
    return hydra.ext.org.w3.rdf.syntax.Iri(hydra.lib.strings.cat2("urn:", name.value))

def next_blank_node(cx: hydra.context.Context) -> tuple[hydra.ext.org.w3.rdf.syntax.Resource, hydra.context.Context]:
    r"""Generate the next blank node and an updated context."""
    
    @lru_cache(1)
    def result() -> tuple[int, hydra.context.Context]:
        return hydra.annotations.next_count(key_rdf_blank_node_counter, cx)
    @lru_cache(1)
    def count() -> int:
        return hydra.lib.pairs.first(result())
    @lru_cache(1)
    def cx_() -> hydra.context.Context:
        return hydra.lib.pairs.second(result())
    return (cast(hydra.ext.org.w3.rdf.syntax.Resource, hydra.ext.org.w3.rdf.syntax.ResourceBnode(hydra.ext.org.w3.rdf.syntax.BlankNode(hydra.lib.strings.cat2("b", hydra.lib.literals.show_int32(count()))))), cx_())

def property_iri(rname: hydra.core.Name, fname: hydra.core.Name) -> hydra.ext.org.w3.rdf.syntax.Iri:
    r"""Construct a property IRI from a record name and field name."""
    
    @lru_cache(1)
    def qual_name() -> hydra.module.QualifiedName:
        return hydra.names.qualify_name(rname)
    gname = qual_name().namespace
    local_ = qual_name().local
    return hydra.ext.org.w3.rdf.syntax.Iri(hydra.lib.strings.cat(("urn:", hydra.lib.maybes.maybe((lambda : ""), (lambda v1: v1.value), gname), "#", hydra.formatting.decapitalize(local_), hydra.formatting.capitalize(fname.value))))

def rdf_iri(local: str) -> hydra.ext.org.w3.rdf.syntax.Iri:
    r"""Construct an RDF namespace IRI."""
    
    return iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#", local)

def resource_to_node(r: hydra.ext.org.w3.rdf.syntax.Resource) -> hydra.ext.org.w3.rdf.syntax.Node_:
    r"""Convert a resource to a node."""
    
    match r:
        case hydra.ext.org.w3.rdf.syntax.ResourceIri(value=i):
            return cast(hydra.ext.org.w3.rdf.syntax.Node_, hydra.ext.org.w3.rdf.syntax.NodeIri(i))
        
        case hydra.ext.org.w3.rdf.syntax.ResourceBnode(value=b):
            return cast(hydra.ext.org.w3.rdf.syntax.Node_, hydra.ext.org.w3.rdf.syntax.NodeBnode(b))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def subjects_of(descs: frozenlist[hydra.ext.org.w3.rdf.syntax.Description]) -> frozenlist[hydra.ext.org.w3.rdf.syntax.Node_]:
    r"""Extract subjects from a list of descriptions."""
    
    return hydra.lib.lists.map((lambda v1: v1.subject), descs)
