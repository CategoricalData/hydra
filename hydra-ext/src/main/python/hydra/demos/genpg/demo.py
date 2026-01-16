#!/usr/bin/env python3
"""
GenPG Demo - Property Graph Generation from CSV Tables

This is the Python equivalent of Hydra.Ext.Demos.GenPG.Demo in Haskell.
It demonstrates end-to-end transformation of relational CSV data into
a property graph in GraphSON format.

Usage:
    cd hydra-ext
    python3 src/main/python/hydra/demos/genpg/demo.py
"""

import os
import sys

# hydra-ext root is six levels up from this file (src/main/python/hydra/demos/genpg/demo.py)
_hydra_ext_root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))))))
# hydra-python root is sibling to hydra-ext
_hydra_python_root = os.path.join(os.path.dirname(_hydra_ext_root), "hydra-python")

# Add paths for modules (order matters - earlier paths take precedence):
# 1. hydra-ext hand-written modules (this demo)
sys.path.insert(0, os.path.join(_hydra_ext_root, "src/main/python"))
# 2. hydra-ext generated modules (hydra.pg.*, hydra.demos.genpg.transform, etc.)
sys.path.insert(0, os.path.join(_hydra_ext_root, "src/gen-main/python"))
# 3. hydra-python generated modules (hydra.core, hydra.compute, etc.)
sys.path.insert(0, os.path.join(_hydra_python_root, "src/gen-main/python"))
# 4. hydra-python hand-written modules (hydra.dsl.python, etc.)
sys.path.insert(0, os.path.join(_hydra_python_root, "src/main/python"))

from hydra.dsl.python import Just, Nothing, Left, Right, FrozenDict, frozenlist
from hydra.pg.model import LazyGraph, Vertex, Edge, Element, ElementVertex, ElementEdge
from hydra.tabular import TableType, Table
from hydra.relational import RelationName
from hydra.pg.graphson.utils import pg_elements_to_graphson, encode_term_value
from hydra.compute import Flow, FlowState, Trace
from hydra.graph import Graph as HydraGraph
import hydra.core
import hydra.json.writer as json_writer
import hydra.sources.libraries
import hydra.demos.genpg.transform as Transform
from hydra.demos.genpg.example import (
    example_database_schema,
    example_graph_schema,
    example_mapping,
)


def run_flow(flow: Flow, state, trace: Trace = None) -> FlowState:
    """Run a flow with the given state and optional trace."""
    if trace is None:
        trace = Trace(stack=(), messages=(), other=FrozenDict({}))
    return flow.value(state, trace)


def lazy_graph_to_elements(graph: LazyGraph) -> list[Element]:
    """Convert a lazy graph to a list of elements."""
    elements: list[Element] = []
    for v in graph.vertices:
        elements.append(ElementVertex(v))
    for e in graph.edges:
        elements.append(ElementEdge(e))
    return elements


def decode_table_io(table_type: TableType, path: str) -> Table:
    """Read and decode a CSV file into a Table of Terms."""
    with open(path, 'r') as f:
        raw_lines = tuple(f.read().splitlines())

    # Parse CSV lines
    parse_result = Transform.parse_table_lines(True, raw_lines)
    match parse_result:
        case Left(value=err):
            raise ValueError(f"CSV read error in {path}: {err}")
        case Right(value=table):
            pass

    # Decode table values
    decode_result = Transform.decode_table(table_type, table)
    match decode_result:
        case Left(value=err):
            raise ValueError(f"Decode error: {err}")
        case Right(value=decoded):
            return decoded


def transform_table(
    table_type: TableType,
    path: str,
    vspecs: frozenlist[Vertex],
    especs: frozenlist[Edge],
    cx: HydraGraph
) -> tuple[frozenlist[Vertex], frozenlist[Edge]]:
    """Transform a table by reading from a file and applying vertex/edge specifications."""
    table = decode_table_io(table_type, path)
    flow = Transform.transform_table_rows(vspecs, especs, table_type, table.data)
    flow_state = run_flow(flow, cx)
    match flow_state.value:
        case Nothing():
            msgs = "; ".join(flow_state.trace.messages)
            raise ValueError(f"Transform error: {msgs}")
        case Just(value=pair):
            return pair


def transform_tables(
    file_root: str,
    table_types: list[TableType],
    spec: LazyGraph,
    cx: HydraGraph
) -> LazyGraph:
    """Transform multiple tables according to a graph mapping specification."""
    # Group specs by table
    specs_result = Transform.element_specs_by_table(spec)
    match specs_result:
        case Left(value=err):
            raise ValueError(f"Error in mapping specification: {err}")
        case Right(value=by_table):
            pass

    tbl_types_by_name = Transform.table_types_by_name(tuple(table_types))

    all_vertices: list = []
    all_edges: list = []

    for tname, (vspecs, especs) in by_table.items():
        rel_name = RelationName(tname)
        table_type = tbl_types_by_name.get(rel_name)
        if table_type is None:
            raise ValueError(f"Table specified in mapping does not exist: {tname}")

        path = os.path.join(file_root, tname)
        vertices, edges = transform_table(table_type, path, vspecs, especs, cx)
        all_vertices.extend(vertices)
        all_edges.extend(edges)

    return Transform.make_lazy_graph(tuple(all_vertices), tuple(all_edges))


def generate_graphson(
    source_root: str,
    table_schemas: list[TableType],
    graph_mapping: LazyGraph,
    output_path: str,
    cx: HydraGraph
) -> None:
    """Generate GraphSON output from CSV sources."""
    print(f"Reading CSV files from {source_root}/")
    table_names = [t.name.value for t in table_schemas]
    print(f"  Tables: {', '.join(table_names)}")

    g = transform_tables(source_root, table_schemas, graph_mapping, cx)
    els = lazy_graph_to_elements(g)

    vertices = [e for e in els if Transform.element_is_vertex(e)]
    edges = [e for e in els if Transform.element_is_edge(e)]

    print("Transforming to property graph...")
    print(f"  Vertices: {len(vertices)}")
    print(f"  Edges: {len(edges)}")

    print(f"Writing GraphSON to {output_path}")
    flow = pg_elements_to_graphson(encode_term_value, tuple(els))
    flow_state = run_flow(flow, cx)
    match flow_state.value:
        case Nothing():
            msgs = "; ".join(flow_state.trace.messages)
            raise ValueError(f"GraphSON encoding error: {msgs}")
        case Just(value=json_values):
            pass

    json_strings = [json_writer.print_json(jv) for jv in json_values]
    os.makedirs(os.path.dirname(output_path) or ".", exist_ok=True)
    with open(output_path, 'w') as f:
        f.write("\n".join(json_strings))
        f.write("\n")

    print(f"Done. Output written to {output_path}")


def _make_graph_context() -> HydraGraph:
    """Create a Hydra graph context for evaluation with primitives."""
    primitives = hydra.sources.libraries.standard_library()
    return HydraGraph(
        elements=FrozenDict({}),
        environment=FrozenDict({}),
        types=FrozenDict({}),
        body=hydra.core.TermUnit(),
        primitives=FrozenDict(primitives),
        schema=Nothing()
    )


def generate_sales_graphson() -> None:
    """Generate GraphSON for the sales dataset."""
    cx = _make_graph_context()

    # Load schemas and mapping from generated Python module
    sales_table_schemas = example_database_schema
    sales_graph = example_mapping()

    source_root = os.path.join(_hydra_ext_root, "demos/genpg/data/sources/sales")
    output_path = os.path.join(_hydra_ext_root, "demos/genpg/output/sales.jsonl")

    generate_graphson(
        source_root=source_root,
        table_schemas=list(sales_table_schemas),
        graph_mapping=sales_graph,
        output_path=output_path,
        cx=cx
    )


def generate_health_graphson() -> None:
    """Generate GraphSON for the health dataset (or your custom dataset).

    Note: The Health dataset schemas are defined in Haskell at
    src/main/haskell/Hydra/Ext/Demos/GenPG/Examples/Health/.
    To enable Python support for Health, you would need to generate
    a corresponding Python module similar to hydra.demos.genpg.example.
    """
    raise NotImplementedError(
        "Health dataset is not yet available in Python mode. "
        "Use Haskell mode (generateHealthGraphSON) or see demos/genpg/README.md."
    )


if __name__ == "__main__":
    generate_sales_graphson()
