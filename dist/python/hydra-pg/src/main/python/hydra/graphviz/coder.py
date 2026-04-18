# Note: this is an automatically generated file. Do not edit.

r"""Functions for converting Hydra terms to Graphviz DOT graphs."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.graphviz.dot
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.paths
import hydra.rewriting
import hydra.show.paths

T0 = TypeVar("T0")

def label_attr(lab: str) -> hydra.graphviz.dot.EqualityPair:
    r"""Create a DOT label attribute."""

    return hydra.graphviz.dot.EqualityPair(hydra.graphviz.dot.Id("label"), hydra.graphviz.dot.Id(lab))

# The 'element' node style.
node_style_element = "element"

# The 'simple' node style.
node_style_simple = "simple"

# The 'variable' node style.
node_style_variable = "variable"

def label_attrs(style: str, lab: str) -> hydra.graphviz.dot.AttrList:
    r"""Create DOT label attributes with a node style."""

    @lru_cache(1)
    def style_attrs() -> frozenlist[hydra.graphviz.dot.EqualityPair]:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(style, node_style_simple), (lambda : ()), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(style, node_style_element), (lambda : (hydra.graphviz.dot.EqualityPair(hydra.graphviz.dot.Id("style"), hydra.graphviz.dot.Id("filled")), hydra.graphviz.dot.EqualityPair(hydra.graphviz.dot.Id("fillcolor"), hydra.graphviz.dot.Id("lightyellow")))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(style, node_style_variable), (lambda : (hydra.graphviz.dot.EqualityPair(hydra.graphviz.dot.Id("style"), hydra.graphviz.dot.Id("filled")), hydra.graphviz.dot.EqualityPair(hydra.graphviz.dot.Id("fillcolor"), hydra.graphviz.dot.Id("lightcyan")))), (lambda : (hydra.graphviz.dot.EqualityPair(hydra.graphviz.dot.Id("style"), hydra.graphviz.dot.Id("filled")), hydra.graphviz.dot.EqualityPair(hydra.graphviz.dot.Id("fillcolor"), hydra.graphviz.dot.Id("linen")))))))))
    return hydra.graphviz.dot.AttrList((hydra.lib.lists.concat2((label_attr(lab),), style_attrs()),))

# The 'primitive' node style.
node_style_primitive = "primitive"

# Construct a map from namespace to prefix for all standard libraries.
standard_namespaces = FrozenDict({
  hydra.packaging.Namespace("hydra.lib.chars"): "chars",
  hydra.packaging.Namespace("hydra.lib.eithers"): "eithers",
  hydra.packaging.Namespace("hydra.lib.equality"): "equality",
  hydra.packaging.Namespace("hydra.lib.lists"): "lists",
  hydra.packaging.Namespace("hydra.lib.literals"): "literals",
  hydra.packaging.Namespace("hydra.lib.logic"): "logic",
  hydra.packaging.Namespace("hydra.lib.maps"): "maps",
  hydra.packaging.Namespace("hydra.lib.math"): "math",
  hydra.packaging.Namespace("hydra.lib.maybes"): "maybes",
  hydra.packaging.Namespace("hydra.lib.pairs"): "pairs",
  hydra.packaging.Namespace("hydra.lib.regex"): "regex",
  hydra.packaging.Namespace("hydra.lib.sets"): "sets",
  hydra.packaging.Namespace("hydra.lib.strings"): "strings"})

def term_label(compact: bool, namespaces: FrozenDict[hydra.packaging.Namespace, str], term: hydra.core.Term):
    r"""Compute a label and node style for a term."""

    def simple_label(lab: T0) -> tuple[T0, str]:
        return (lab, node_style_simple)
    def _hoist_simple_label_body_1(v1):
        match v1:
            case hydra.core.IntegerValueBigint(value=v):
                return hydra.lib.literals.show_bigint(v)

            case hydra.core.IntegerValueInt8(value=v):
                return hydra.lib.literals.show_int8(v)

            case hydra.core.IntegerValueInt16(value=v):
                return hydra.lib.literals.show_int16(v)

            case hydra.core.IntegerValueInt32(value=v):
                return hydra.lib.literals.show_int32(v)

            case hydra.core.IntegerValueInt64(value=v):
                return hydra.lib.literals.show_int64(v)

            case hydra.core.IntegerValueUint8(value=v):
                return hydra.lib.literals.show_uint8(v)

            case hydra.core.IntegerValueUint16(value=v):
                return hydra.lib.literals.show_uint16(v)

            case hydra.core.IntegerValueUint32(value=v):
                return hydra.lib.literals.show_uint32(v)

            case hydra.core.IntegerValueUint64(value=v):
                return hydra.lib.literals.show_uint64(v)

            case _:
                return "?"
    def _hoist_simple_label_body_2(v1):
        match v1:
            case hydra.core.FloatValueBigfloat(value=v):
                return hydra.lib.literals.show_bigfloat(v)

            case hydra.core.FloatValueFloat32(value=v):
                return hydra.lib.literals.show_float32(v)

            case hydra.core.FloatValueFloat64(value=v):
                return hydra.lib.literals.show_float64(v)

            case _:
                return "?"
    def _hoist_simple_label_body_3(v1):
        match v1:
            case hydra.core.LiteralBinary(value=s):
                return hydra.lib.literals.binary_to_string(s)

            case hydra.core.LiteralBoolean(value=b):
                return hydra.lib.literals.show_boolean(b)

            case hydra.core.LiteralInteger(value=i):
                return _hoist_simple_label_body_1(i)

            case hydra.core.LiteralFloat(value=f):
                return _hoist_simple_label_body_2(f)

            case hydra.core.LiteralString(value=s):
                return s

            case _:
                return "?"
    match term:
        case hydra.core.TermAnnotated():
            return simple_label("@{}")

        case hydra.core.TermApplication():
            return simple_label(hydra.lib.logic.if_else(compact, (lambda : "$"), (lambda : "apply")))

        case hydra.core.TermLambda():
            return simple_label(hydra.lib.logic.if_else(compact, (lambda : "λ"), (lambda : "lambda")))

        case hydra.core.TermProject(value=proj):
            return simple_label(hydra.lib.strings.cat(("{", hydra.names.compact_name(namespaces, proj.type_name), "}.", proj.field.value)))

        case hydra.core.TermCases(value=cs):
            return simple_label(hydra.lib.strings.cat(("cases_{", hydra.names.compact_name(namespaces, cs.type_name), "}")))

        case hydra.core.TermUnwrap(value=name):
            return simple_label(hydra.lib.strings.cat(("unwrap_{", hydra.names.compact_name(namespaces, name), "}")))

        case hydra.core.TermLet():
            return simple_label("let")

        case hydra.core.TermList():
            return simple_label(hydra.lib.logic.if_else(compact, (lambda : "[]"), (lambda : "list")))

        case hydra.core.TermLiteral(value=l):
            return simple_label(_hoist_simple_label_body_3(l))

        case hydra.core.TermMap():
            return simple_label(hydra.lib.logic.if_else(compact, (lambda : "<,>"), (lambda : "map")))

        case hydra.core.TermMaybe():
            return simple_label(hydra.lib.logic.if_else(compact, (lambda : "opt"), (lambda : "optional")))

        case hydra.core.TermRecord(value=rec):
            return simple_label(hydra.lib.strings.cat2("∧", hydra.names.compact_name(namespaces, rec.type_name)))

        case hydra.core.TermTypeLambda():
            return simple_label("tyabs")

        case hydra.core.TermTypeApplication():
            return simple_label("tyapp")

        case hydra.core.TermInject(value=inj):
            return simple_label(hydra.lib.strings.cat2("⊻", hydra.names.compact_name(namespaces, inj.type_name)))

        case hydra.core.TermVariable(value=name2):
            return simple_label(hydra.names.compact_name(namespaces, name2))

        case hydra.core.TermWrap(value=wt):
            return simple_label(hydra.lib.strings.cat(("(", hydra.names.compact_name(namespaces, wt.type_name), ")")))

        case _:
            return simple_label("?")

def to_node_id(i: hydra.graphviz.dot.Id) -> hydra.graphviz.dot.NodeId:
    r"""Create a DOT NodeId from an Id."""

    return hydra.graphviz.dot.NodeId(i, Nothing())

def to_node_or_subgraph(i: hydra.graphviz.dot.Id) -> hydra.graphviz.dot.NodeOrSubgraph:
    r"""Create a DOT NodeOrSubgraph from an Id."""

    return cast(hydra.graphviz.dot.NodeOrSubgraph, hydra.graphviz.dot.NodeOrSubgraphNode(to_node_id(i)))

def to_edge_stmt(i1: hydra.graphviz.dot.Id, i2: hydra.graphviz.dot.Id, attrs: Maybe[hydra.graphviz.dot.AttrList]) -> hydra.graphviz.dot.Stmt:
    r"""Create a DOT edge statement."""

    return cast(hydra.graphviz.dot.Stmt, hydra.graphviz.dot.StmtEdge(hydra.graphviz.dot.EdgeStmt(to_node_or_subgraph(i1), (to_node_or_subgraph(i2),), attrs)))

def term_to_dot_stmts(namespaces: FrozenDict[hydra.packaging.Namespace, str], term: hydra.core.Term) -> frozenlist[hydra.graphviz.dot.Stmt]:
    r"""Convert a term to full DOT statements showing term structure."""

    def encode(mlabstyle: Maybe[tuple[str, str]], is_element: bool, ids: FrozenDict[hydra.core.Name, hydra.graphviz.dot.Id], mparent: Maybe[hydra.graphviz.dot.Id], stmts_visited: tuple[frozenlist[hydra.graphviz.dot.Stmt], frozenset[str]], accessor_term: tuple[hydra.paths.SubtermStep, hydra.core.Term]) -> tuple[frozenlist[hydra.graphviz.dot.Stmt], frozenset[str]]:
        @lru_cache(1)
        def accessor() -> hydra.paths.SubtermStep:
            return hydra.lib.pairs.first(accessor_term)
        @lru_cache(1)
        def current_term() -> hydra.core.Term:
            return hydra.lib.pairs.second(accessor_term)
        @lru_cache(1)
        def stmts() -> frozenlist[hydra.graphviz.dot.Stmt]:
            return hydra.lib.pairs.first(stmts_visited)
        @lru_cache(1)
        def visited() -> frozenset[str]:
            return hydra.lib.pairs.second(stmts_visited)
        @lru_cache(1)
        def term_l_s() -> tuple[str, str]:
            return term_label(True, namespaces, current_term())
        @lru_cache(1)
        def raw_label() -> str:
            return hydra.lib.pairs.first(term_l_s())
        @lru_cache(1)
        def term_node_style() -> str:
            return hydra.lib.pairs.second(term_l_s())
        def label_of(vis: frozenset[str], t: hydra.core.Term) -> tuple[str, str]:
            @lru_cache(1)
            def tls() -> tuple[str, str]:
                return term_label(True, namespaces, t)
            @lru_cache(1)
            def l() -> str:
                return hydra.lib.pairs.first(tls())
            @lru_cache(1)
            def s() -> str:
                return hydra.lib.pairs.second(tls())
            return (hydra.names.unique_label(vis, l()), s())
        @lru_cache(1)
        def labstyle() -> tuple[str, str]:
            return hydra.lib.maybes.maybe((lambda : label_of(visited(), current_term())), (lambda ls: ls), mlabstyle)
        @lru_cache(1)
        def label() -> str:
            return hydra.lib.pairs.first(labstyle())
        @lru_cache(1)
        def style() -> str:
            return hydra.lib.pairs.second(labstyle())
        @lru_cache(1)
        def node_style() -> str:
            return hydra.lib.logic.if_else(is_element, (lambda : node_style_element), (lambda : term_node_style()))
        self_id = hydra.graphviz.dot.Id(label())
        @lru_cache(1)
        def self_visited() -> frozenset[str]:
            return hydra.lib.sets.insert(label(), visited())
        @lru_cache(1)
        def node_stmt() -> hydra.graphviz.dot.Stmt:
            return cast(hydra.graphviz.dot.Stmt, hydra.graphviz.dot.StmtNode(hydra.graphviz.dot.NodeStmt(to_node_id(self_id), Just(label_attrs(node_style(), raw_label())))))
        def to_accessor_edge_stmt(acc: hydra.paths.SubtermStep, sty: str, i1: hydra.graphviz.dot.Id, i2: hydra.graphviz.dot.Id) -> hydra.graphviz.dot.Stmt:
            return to_edge_stmt(i1, i2, hydra.lib.maybes.map((lambda s: label_attrs(sty, s)), hydra.show.paths.subterm_step(acc)))
        def edge_attrs(lab: str) -> hydra.graphviz.dot.AttrList:
            return hydra.graphviz.dot.AttrList(((hydra.graphviz.dot.EqualityPair(hydra.graphviz.dot.Id("label"), hydra.graphviz.dot.Id(lab)),),))
        @lru_cache(1)
        def parent_stmt() -> frozenlist[hydra.graphviz.dot.Stmt]:
            return hydra.lib.maybes.maybe((lambda : ()), (lambda parent: (to_accessor_edge_stmt(accessor(), style(), parent, self_id),)), mparent)
        @lru_cache(1)
        def self_stmts() -> frozenlist[hydra.graphviz.dot.Stmt]:
            return hydra.lib.lists.concat((stmts(), (node_stmt(),), parent_stmt()))
        @lru_cache(1)
        def dflt() -> tuple[frozenlist[hydra.graphviz.dot.Stmt], frozenset[str]]:
            return hydra.lib.lists.foldl((lambda v1, v2: encode(Nothing(), False, ids, Just(self_id), v1, v2)), (self_stmts(), self_visited()), hydra.rewriting.subterms_with_steps(current_term()))
        match current_term():
            case hydra.core.TermLambda(value=lam):
                v = lam.parameter
                body = lam.body
                vstr = v.value
                @lru_cache(1)
                def var_label() -> str:
                    return hydra.names.unique_label(self_visited(), vstr)
                var_id = hydra.graphviz.dot.Id(var_label())
                @lru_cache(1)
                def visited1() -> frozenset[str]:
                    return hydra.lib.sets.insert(var_label(), self_visited())
                @lru_cache(1)
                def ids1() -> FrozenDict[hydra.core.Name, hydra.graphviz.dot.Id]:
                    return hydra.lib.maps.insert(v, var_id, ids)
                @lru_cache(1)
                def var_node_stmt() -> hydra.graphviz.dot.Stmt:
                    return cast(hydra.graphviz.dot.Stmt, hydra.graphviz.dot.StmtNode(hydra.graphviz.dot.NodeStmt(to_node_id(var_id), Just(label_attrs(node_style_variable, vstr)))))
                @lru_cache(1)
                def var_edge_stmt() -> hydra.graphviz.dot.Stmt:
                    return cast(hydra.graphviz.dot.Stmt, hydra.graphviz.dot.StmtEdge(hydra.graphviz.dot.EdgeStmt(to_node_or_subgraph(self_id), (to_node_or_subgraph(var_id),), Just(edge_attrs("var")))))
                return encode(Nothing(), False, ids1(), Just(self_id), (hydra.lib.lists.concat((self_stmts(), (var_node_stmt(), var_edge_stmt()))), visited1()), (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepLambdaBody()), body))

            case hydra.core.TermLet(value=let_expr):
                bindings = let_expr.bindings
                env = let_expr.body
                def add_binding_ids(ids_vis: tuple[FrozenDict[hydra.core.Name, hydra.graphviz.dot.Id], frozenset[str]], binding: hydra.core.Binding) -> tuple[FrozenDict[hydra.core.Name, hydra.graphviz.dot.Id], frozenset[str]]:
                    @lru_cache(1)
                    def cur_ids() -> FrozenDict[hydra.core.Name, hydra.graphviz.dot.Id]:
                        return hydra.lib.pairs.first(ids_vis)
                    @lru_cache(1)
                    def cur_vis() -> frozenset[str]:
                        return hydra.lib.pairs.second(ids_vis)
                    bname = binding.name
                    bterm = binding.term
                    @lru_cache(1)
                    def bls() -> tuple[str, str]:
                        return label_of(cur_vis(), bterm)
                    @lru_cache(1)
                    def blab() -> str:
                        return hydra.lib.pairs.first(bls())
                    return (hydra.lib.maps.insert(bname, hydra.graphviz.dot.Id(blab()), cur_ids()), hydra.lib.sets.insert(blab(), cur_vis()))
                @lru_cache(1)
                def ids_vis1() -> tuple[FrozenDict[hydra.core.Name, hydra.graphviz.dot.Id], frozenset[str]]:
                    return hydra.lib.lists.foldl((lambda x1, x2: add_binding_ids(x1, x2)), (ids, visited()), bindings)
                @lru_cache(1)
                def ids1() -> FrozenDict[hydra.core.Name, hydra.graphviz.dot.Id]:
                    return hydra.lib.pairs.first(ids_vis1())
                def add_binding_term(st_vis: tuple[frozenlist[hydra.graphviz.dot.Stmt], frozenset[str]], binding: hydra.core.Binding) -> tuple[frozenlist[hydra.graphviz.dot.Stmt], frozenset[str]]:
                    bname = binding.name
                    bterm = binding.term
                    @lru_cache(1)
                    def blab() -> str:
                        return hydra.lib.maybes.from_maybe((lambda : hydra.graphviz.dot.Id("?")), hydra.lib.maps.lookup(bname, ids1())).value
                    return encode(Just((blab(), node_style_element)), True, ids1(), Just(self_id), st_vis, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepLetBinding(bname)), bterm))
                @lru_cache(1)
                def stmts1() -> tuple[frozenlist[hydra.graphviz.dot.Stmt], frozenset[str]]:
                    return hydra.lib.lists.foldl((lambda x1, x2: add_binding_term(x1, x2)), (self_stmts(), self_visited()), bindings)
                return encode(Nothing(), False, ids1(), Just(self_id), stmts1(), (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepLetBody()), env))

            case hydra.core.TermVariable(value=name):
                return hydra.lib.maybes.maybe((lambda : dflt()), (lambda i: (hydra.lib.lists.concat2(stmts(), (to_accessor_edge_stmt(accessor(), style(), hydra.lib.maybes.from_maybe((lambda : self_id), mparent), i),)), visited())), hydra.lib.maps.lookup(name, ids))

            case _:
                return dflt()
    return hydra.lib.pairs.first(encode(Nothing(), False, hydra.lib.maps.empty(), Nothing(), ((), hydra.lib.sets.empty()), (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepAnnotatedBody()), term)))

def term_to_dot_graph(term: hydra.core.Term) -> hydra.graphviz.dot.Graph:
    r"""Convert a term to a full DOT graph."""

    return hydra.graphviz.dot.Graph(False, True, Nothing(), term_to_dot_stmts(standard_namespaces, term))

def term_to_subterm_dot_stmts(namespaces: FrozenDict[hydra.packaging.Namespace, str], term: hydra.core.Term) -> frozenlist[hydra.graphviz.dot.Stmt]:
    r"""Convert a term to subterm-style DOT statements."""

    @lru_cache(1)
    def accessor_graph() -> hydra.paths.SubtermGraph:
        return hydra.show.paths.term_to_subterm_graph(namespaces, term)
    nodes = accessor_graph().nodes
    edges = accessor_graph().edges
    def node_stmt(node: hydra.paths.SubtermNode) -> hydra.graphviz.dot.Stmt:
        return cast(hydra.graphviz.dot.Stmt, hydra.graphviz.dot.StmtNode(hydra.graphviz.dot.NodeStmt(to_node_id(hydra.graphviz.dot.Id(node.id)), Just(hydra.graphviz.dot.AttrList(((label_attr(node.label),),))))))
    def edge_stmt(edge: hydra.paths.SubtermEdge) -> hydra.graphviz.dot.Stmt:
        lab1 = edge.source.id
        lab2 = edge.target.id
        path_accessors = edge.path.value
        @lru_cache(1)
        def show_path() -> str:
            return hydra.lib.strings.intercalate("/", hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.show.paths.subterm_step(x1)), path_accessors)))
        return to_edge_stmt(hydra.graphviz.dot.Id(lab1), hydra.graphviz.dot.Id(lab2), Just(hydra.graphviz.dot.AttrList(((label_attr(show_path()),),))))
    return hydra.lib.lists.concat2(hydra.lib.lists.map((lambda x1: node_stmt(x1)), nodes), hydra.lib.lists.map((lambda x1: edge_stmt(x1)), edges))

def term_to_subterm_dot_graph(term: hydra.core.Term) -> hydra.graphviz.dot.Graph:
    r"""Convert a term to an subterm-style DOT graph."""

    return hydra.graphviz.dot.Graph(False, True, Nothing(), term_to_subterm_dot_stmts(standard_namespaces, term))
