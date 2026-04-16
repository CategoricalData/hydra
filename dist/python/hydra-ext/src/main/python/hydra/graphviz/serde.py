# Note: this is an automatically generated file. Do not edit.

r"""Serialization functions for converting Graphviz DOT AST to abstract expressions."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.graphviz.dot
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.serialization

def write_id(i: hydra.graphviz.dot.Id) -> hydra.ast.Expr:
    r"""Convert an identifier to an expression."""

    return hydra.serialization.cst(hydra.lib.strings.cat(("\"", i.value, "\"")))

def write_equality_pair(eq: hydra.graphviz.dot.EqualityPair) -> hydra.ast.Expr:
    r"""Convert an equality pair to an expression."""

    l = eq.left
    r = eq.right
    return hydra.serialization.space_sep((write_id(l), hydra.serialization.cst("="), write_id(r)))

def write_attr_list(al: hydra.graphviz.dot.AttrList) -> hydra.ast.Expr:
    r"""Convert an attribute list to an expression."""

    return hydra.serialization.space_sep(hydra.lib.lists.map((lambda alist: hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_equality_pair(x1)), alist)))), al.value))

def write_attr_type(t: hydra.graphviz.dot.AttrType) -> hydra.ast.Expr:
    r"""Convert an attribute type to an expression."""

    match t:
        case hydra.graphviz.dot.AttrType.GRAPH:
            return hydra.serialization.cst("graph")

        case hydra.graphviz.dot.AttrType.NODE:
            return hydra.serialization.cst("node")

        case hydra.graphviz.dot.AttrType.EDGE:
            return hydra.serialization.cst("edge")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_attr_stmt(as_: hydra.graphviz.dot.AttrStmt) -> hydra.ast.Expr:
    r"""Convert an attribute statement to an expression."""

    t = as_.type
    attr = as_.attributes
    return hydra.serialization.space_sep((write_attr_type(t), write_attr_list(attr)))

def write_compass_pt(p: hydra.graphviz.dot.CompassPt) -> hydra.ast.Expr:
    r"""Convert a compass point to an expression."""

    match p:
        case hydra.graphviz.dot.CompassPt.N:
            return hydra.serialization.cst("n")

        case hydra.graphviz.dot.CompassPt.NE:
            return hydra.serialization.cst("ne")

        case hydra.graphviz.dot.CompassPt.E:
            return hydra.serialization.cst("e")

        case hydra.graphviz.dot.CompassPt.SE:
            return hydra.serialization.cst("se")

        case hydra.graphviz.dot.CompassPt.S:
            return hydra.serialization.cst("s")

        case hydra.graphviz.dot.CompassPt.SW:
            return hydra.serialization.cst("sw")

        case hydra.graphviz.dot.CompassPt.W:
            return hydra.serialization.cst("w")

        case hydra.graphviz.dot.CompassPt.NW:
            return hydra.serialization.cst("nw")

        case hydra.graphviz.dot.CompassPt.C:
            return hydra.serialization.cst("c")

        case hydra.graphviz.dot.CompassPt.NONE:
            return hydra.serialization.cst("none")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_port(p: hydra.graphviz.dot.Port) -> hydra.ast.Expr:
    r"""Convert a port to an expression."""

    mi = p.id
    mp = p.position
    @lru_cache(1)
    def pre() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda i: (hydra.serialization.cst(":"), write_id(i))), mi)
    @lru_cache(1)
    def suf() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda cp: (hydra.serialization.cst(":"), write_compass_pt(cp))), mp)
    return hydra.serialization.no_sep(hydra.lib.lists.concat((pre(), suf())))

def write_node_id(nid: hydra.graphviz.dot.NodeId) -> hydra.ast.Expr:
    r"""Convert a node identifier to an expression."""

    i = nid.id
    mp = nid.port
    return hydra.serialization.no_sep(hydra.lib.maybes.cat((hydra.lib.maybes.pure(write_id(i)), hydra.lib.maybes.map((lambda x1: write_port(x1)), mp))))

def write_node_stmt(ns: hydra.graphviz.dot.NodeStmt) -> hydra.ast.Expr:
    r"""Convert a node statement to an expression."""

    i = ns.id
    attr = ns.attributes
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.maybes.pure(write_node_id(i)), hydra.lib.maybes.map((lambda x1: write_attr_list(x1)), attr))))

def write_subgraph_id(sid: hydra.graphviz.dot.SubgraphId) -> hydra.ast.Expr:
    r"""Convert a subgraph identifier to an expression."""

    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.maybes.pure(hydra.serialization.cst("subgraph")), hydra.lib.maybes.map((lambda x1: write_id(x1)), sid.value))))

def write_edge_stmt(directed: bool, es: hydra.graphviz.dot.EdgeStmt) -> hydra.ast.Expr:
    r"""Convert an edge statement to an expression."""

    l = es.left
    r = es.right
    attr = es.attributes
    @lru_cache(1)
    def arrow() -> str:
        return hydra.lib.logic.if_else(directed, (lambda : "->"), (lambda : "--"))
    @lru_cache(1)
    def rhs_parts() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda n: (hydra.serialization.cst(arrow()), write_node_or_subgraph(directed, n))), r))
    @lru_cache(1)
    def attr_parts() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda a: (write_attr_list(a),)), attr)
    return hydra.serialization.space_sep(hydra.lib.lists.concat(((write_node_or_subgraph(directed, l),), rhs_parts(), attr_parts())))

def write_node_or_subgraph(directed: bool, ns: hydra.graphviz.dot.NodeOrSubgraph) -> hydra.ast.Expr:
    r"""Convert a node or subgraph to an expression."""

    match ns:
        case hydra.graphviz.dot.NodeOrSubgraphNode(value=n):
            return write_node_id(n)

        case hydra.graphviz.dot.NodeOrSubgraphSubgraph(value=sg):
            return write_subgraph(directed, sg)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_stmt(directed: bool, s: hydra.graphviz.dot.Stmt) -> hydra.ast.Expr:
    r"""Convert a statement to an expression."""

    match s:
        case hydra.graphviz.dot.StmtNode(value=n):
            return write_node_stmt(n)

        case hydra.graphviz.dot.StmtEdge(value=e):
            return write_edge_stmt(directed, e)

        case hydra.graphviz.dot.StmtAttr(value=a):
            return write_attr_stmt(a)

        case hydra.graphviz.dot.StmtEquals(value=eq):
            return write_equality_pair(eq)

        case hydra.graphviz.dot.StmtSubgraph(value=sg):
            return write_subgraph(directed, sg)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_subgraph(directed: bool, sg: hydra.graphviz.dot.Subgraph) -> hydra.ast.Expr:
    r"""Convert a subgraph to an expression."""

    mid = sg.subgraph_id
    stmts = sg.statements
    @lru_cache(1)
    def body() -> hydra.ast.Expr:
        return hydra.serialization.brackets(hydra.serialization.curly_braces, hydra.serialization.inline_style, hydra.serialization.space_sep(hydra.lib.lists.map((lambda v1: write_stmt(directed, v1)), stmts)))
    return hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda x1: write_subgraph_id(x1)), mid), hydra.lib.maybes.pure(body()))))

def write_graph(g: hydra.graphviz.dot.Graph) -> hydra.ast.Expr:
    r"""Convert a graph to an expression."""

    strict = g.strict
    directed = g.directed
    stmts = g.statements
    @lru_cache(1)
    def graph_keyword() -> str:
        return hydra.lib.logic.if_else(directed, (lambda : "digraph"), (lambda : "graph"))
    @lru_cache(1)
    def graph_expr() -> hydra.ast.Expr:
        return hydra.lib.logic.if_else(strict, (lambda : hydra.serialization.space_sep((hydra.serialization.cst("strict"), hydra.serialization.cst(graph_keyword())))), (lambda : hydra.serialization.cst(graph_keyword())))
    @lru_cache(1)
    def body() -> hydra.ast.Expr:
        return hydra.serialization.brackets(hydra.serialization.curly_braces, hydra.serialization.full_block_style, hydra.serialization.symbol_sep(";", hydra.serialization.full_block_style, hydra.lib.lists.map((lambda v1: write_stmt(directed, v1)), stmts)))
    return hydra.serialization.space_sep((graph_expr(), body()))
