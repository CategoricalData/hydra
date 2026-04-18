# Note: this is an automatically generated file. Do not edit.

r"""Serialization functions for converting RDF graphs to N-Triples format expressions."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.sets
import hydra.lib.strings
import hydra.rdf.syntax
import hydra.serialization

def escape_iri_char(c: int) -> str:
    r"""Escape a single IRI character code to a string."""

    return hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.equality.gte(c, 128), hydra.lib.logic.or_(hydra.lib.equality.lte(c, 32), hydra.lib.logic.or_(hydra.lib.equality.equal(c, 60), hydra.lib.logic.or_(hydra.lib.equality.equal(c, 62), hydra.lib.logic.or_(hydra.lib.equality.equal(c, 34), hydra.lib.logic.or_(hydra.lib.equality.equal(c, 123), hydra.lib.logic.or_(hydra.lib.equality.equal(c, 125), hydra.lib.logic.or_(hydra.lib.equality.equal(c, 124), hydra.lib.logic.or_(hydra.lib.equality.equal(c, 94), hydra.lib.logic.or_(hydra.lib.equality.equal(c, 96), hydra.lib.equality.equal(c, 92))))))))))), (lambda : "?"), (lambda : hydra.lib.strings.from_list((c,))))

def escape_iri_str(s: str) -> str:
    r"""Escape a string for use in an IRI. Non-printable and special characters are replaced with ?."""

    return hydra.lib.strings.cat(hydra.lib.lists.map(escape_iri_char, hydra.lib.strings.to_list(s)))

def escape_literal_char(c: int) -> str:
    r"""Escape a single literal character code to a string."""

    return hydra.lib.logic.if_else(hydra.lib.equality.gte(c, 128), (lambda : "?"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 34), (lambda : "\\\""), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 92), (lambda : "\\\\"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 10), (lambda : "\\n"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 13), (lambda : "\\r"), (lambda : hydra.lib.strings.from_list((c,))))))))))))

def escape_literal_string(s: str) -> str:
    r"""Escape a string for use in an N-Triples literal."""

    return hydra.lib.strings.cat(hydra.lib.lists.map(escape_literal_char, hydra.lib.strings.to_list(s)))

def write_iri(iri: hydra.rdf.syntax.Iri) -> hydra.ast.Expr:
    r"""Convert an IRI to an expression."""

    return hydra.serialization.no_sep((hydra.serialization.cst("<"), hydra.serialization.cst(escape_iri_str(iri.value)), hydra.serialization.cst(">")))

def write_blank_node(bnode: hydra.rdf.syntax.BlankNode) -> hydra.ast.Expr:
    r"""Convert a blank node to an expression."""

    return hydra.serialization.no_sep((hydra.serialization.cst("_:"), hydra.serialization.cst(bnode.value)))

def write_language_tag(lang: hydra.rdf.syntax.LanguageTag) -> hydra.ast.Expr:
    r"""Convert a language tag to an expression."""

    return hydra.serialization.no_sep((hydra.serialization.cst("@"), hydra.serialization.cst(lang.value)))

def write_literal(lit: hydra.rdf.syntax.Literal) -> hydra.ast.Expr:
    r"""Convert a literal to an expression."""

    lex = lit.lexical_form
    dt = lit.datatype_iri
    lang = lit.language_tag
    @lru_cache(1)
    def lex_expr() -> hydra.ast.Expr:
        return hydra.serialization.cst(hydra.lib.strings.cat(("\"", escape_literal_string(lex), "\"")))
    @lru_cache(1)
    def suffix() -> hydra.ast.Expr:
        return hydra.lib.maybes.maybe((lambda : hydra.serialization.no_sep((hydra.serialization.cst("^^"), write_iri(dt)))), (lambda x1: write_language_tag(x1)), lang)
    return hydra.serialization.no_sep((lex_expr(), suffix()))

def write_node(n: hydra.rdf.syntax.Node_) -> hydra.ast.Expr:
    r"""Convert a node to an expression."""

    match n:
        case hydra.rdf.syntax.NodeIri(value=iri):
            return write_iri(iri)

        case hydra.rdf.syntax.NodeBnode(value=bnode):
            return write_blank_node(bnode)

        case hydra.rdf.syntax.NodeLiteral(value=lit):
            return write_literal(lit)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_resource(r: hydra.rdf.syntax.Resource) -> hydra.ast.Expr:
    r"""Convert a resource to an expression."""

    match r:
        case hydra.rdf.syntax.ResourceIri(value=iri):
            return write_iri(iri)

        case hydra.rdf.syntax.ResourceBnode(value=bnode):
            return write_blank_node(bnode)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_triple(t: hydra.rdf.syntax.Triple) -> hydra.ast.Expr:
    r"""Convert a triple to an expression."""

    subj = t.subject
    pred = t.predicate
    obj = t.object
    return hydra.serialization.space_sep((write_resource(subj), write_iri(pred), write_node(obj), hydra.serialization.cst(".")))

def write_graph(g: hydra.rdf.syntax.Graph) -> hydra.ast.Expr:
    r"""Convert an RDF graph to an expression."""

    return hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_triple(x1)), hydra.lib.sets.to_list(g.value)))

def rdf_graph_to_ntriples(g: hydra.rdf.syntax.Graph) -> str:
    r"""Convert an RDF graph to an N-Triples string."""

    return hydra.serialization.print_expr(write_graph(g))
