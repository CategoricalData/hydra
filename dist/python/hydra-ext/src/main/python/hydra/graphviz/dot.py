# Note: this is an automatically generated file. Do not edit.

r"""A model from the Graphviz DOT graph description language. Based on the grammar at https://graphviz.org/doc/info/lang.html."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

class Id(Node[str]):
    ...

Id.TYPE_ = hydra.core.Name("hydra.graphviz.dot.Id")

@dataclass(frozen=True)
class Graph:
    strict: bool
    directed: bool
    id: Maybe[Id]
    statements: frozenlist[Stmt]

    TYPE_ = hydra.core.Name("hydra.graphviz.dot.Graph")
    STRICT = hydra.core.Name("strict")
    DIRECTED = hydra.core.Name("directed")
    ID = hydra.core.Name("id")
    STATEMENTS = hydra.core.Name("statements")

class StmtNode(Node["NodeStmt"]):
    ...

class StmtEdge(Node["EdgeStmt"]):
    ...

class StmtAttr(Node["AttrStmt"]):
    ...

class StmtEquals(Node["EqualityPair"]):
    ...

class StmtSubgraph(Node["Subgraph"]):
    ...

class _StmtMeta(type):
    def __getitem__(cls, item):
        return object

class Stmt(metaclass=_StmtMeta):
    r"""StmtNode | StmtEdge | StmtAttr | StmtEquals | StmtSubgraph"""

    TYPE_ = hydra.core.Name("hydra.graphviz.dot.Stmt")
    NODE = hydra.core.Name("node")
    EDGE = hydra.core.Name("edge")
    ATTR = hydra.core.Name("attr")
    EQUALS = hydra.core.Name("equals")
    SUBGRAPH = hydra.core.Name("subgraph")

@dataclass(frozen=True)
class EqualityPair:
    left: Id
    right: Id

    TYPE_ = hydra.core.Name("hydra.graphviz.dot.EqualityPair")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class AttrStmt:
    type: AttrType
    attributes: AttrList

    TYPE_ = hydra.core.Name("hydra.graphviz.dot.AttrStmt")
    TYPE = hydra.core.Name("type")
    ATTRIBUTES = hydra.core.Name("attributes")

class AttrType(Enum):
    GRAPH = hydra.core.Name("graph")

    NODE = hydra.core.Name("node")

    EDGE = hydra.core.Name("edge")

AttrType.TYPE_ = hydra.core.Name("hydra.graphviz.dot.AttrType")

class AttrList(Node["frozenlist[frozenlist[EqualityPair]]"]):
    ...

AttrList.TYPE_ = hydra.core.Name("hydra.graphviz.dot.AttrList")

@dataclass(frozen=True)
class EdgeStmt:
    left: NodeOrSubgraph
    right: frozenlist[NodeOrSubgraph]
    attributes: Maybe[AttrList]

    TYPE_ = hydra.core.Name("hydra.graphviz.dot.EdgeStmt")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")
    ATTRIBUTES = hydra.core.Name("attributes")

class NodeOrSubgraphNode(Node["NodeId"]):
    ...

class NodeOrSubgraphSubgraph(Node["Subgraph"]):
    ...

class _NodeOrSubgraphMeta(type):
    def __getitem__(cls, item):
        return object

class NodeOrSubgraph(metaclass=_NodeOrSubgraphMeta):
    r"""NodeOrSubgraphNode | NodeOrSubgraphSubgraph"""

    TYPE_ = hydra.core.Name("hydra.graphviz.dot.NodeOrSubgraph")
    NODE = hydra.core.Name("node")
    SUBGRAPH = hydra.core.Name("subgraph")

@dataclass(frozen=True)
class NodeStmt:
    id: NodeId
    attributes: Maybe[AttrList]

    TYPE_ = hydra.core.Name("hydra.graphviz.dot.NodeStmt")
    ID = hydra.core.Name("id")
    ATTRIBUTES = hydra.core.Name("attributes")

@dataclass(frozen=True)
class NodeId:
    id: Id
    port: Maybe[Port]

    TYPE_ = hydra.core.Name("hydra.graphviz.dot.NodeId")
    ID = hydra.core.Name("id")
    PORT = hydra.core.Name("port")

@dataclass(frozen=True)
class Port:
    id: Maybe[Id]
    position: Maybe[CompassPt]

    TYPE_ = hydra.core.Name("hydra.graphviz.dot.Port")
    ID = hydra.core.Name("id")
    POSITION = hydra.core.Name("position")

@dataclass(frozen=True)
class Subgraph:
    subgraph_id: Maybe[SubgraphId]
    statements: frozenlist[Stmt]

    TYPE_ = hydra.core.Name("hydra.graphviz.dot.Subgraph")
    SUBGRAPH_ID = hydra.core.Name("subgraphId")
    STATEMENTS = hydra.core.Name("statements")

class SubgraphId(Node["Maybe[Id]"]):
    ...

SubgraphId.TYPE_ = hydra.core.Name("hydra.graphviz.dot.SubgraphId")

class CompassPt(Enum):
    N = hydra.core.Name("n")

    NE = hydra.core.Name("ne")

    E = hydra.core.Name("e")

    SE = hydra.core.Name("se")

    S = hydra.core.Name("s")

    SW = hydra.core.Name("sw")

    W = hydra.core.Name("w")

    NW = hydra.core.Name("nw")

    C = hydra.core.Name("c")

    NONE = hydra.core.Name("none")

CompassPt.TYPE_ = hydra.core.Name("hydra.graphviz.dot.CompassPt")
