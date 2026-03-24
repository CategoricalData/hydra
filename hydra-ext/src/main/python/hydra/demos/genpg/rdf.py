#!/usr/bin/env python3
"""
GenPG RDF Demo - Property Graph to RDF/SHACL conversion

This is the Python equivalent of Hydra.Ext.Demos.GenPG.Rdf in Haskell.
It demonstrates conversion of a property graph (from CSV) into:
  1. A SHACL shapes graph (from the graph schema)
  2. RDF instance data (from the graph data)
  3. Intentionally invalid RDF data (for negative validation)

Usage:
    cd hydra-ext
    python3 src/main/python/hydra/demos/genpg/rdf.py sales
    python3 src/main/python/hydra/demos/genpg/rdf.py health
"""

import os
import sys
import time as _time

# Path setup (same as demo.py)
_hydra_ext_root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))))))
_hydra_python_root = os.path.join(os.path.dirname(_hydra_ext_root), "hydra-python")

sys.path.insert(0, os.path.join(_hydra_ext_root, "src/main/python"))
sys.path.insert(0, os.path.join(_hydra_ext_root, "src/gen-main/python"))
sys.path.insert(0, os.path.join(_hydra_python_root, "src/gen-main/python"))
sys.path.insert(0, os.path.join(_hydra_python_root, "src/main/python"))

from hydra.dsl.python import FrozenDict
import hydra.core
import hydra.ext.org.w3.rdf.syntax as rdf
import hydra.ext.org.w3.shacl.model as shacl
import hydra.ext.rdf.serde as rdf_serde
import hydra.ext.rdf.utils as rdf_utils
import hydra.pg.model as pg
import hydra.pg.rdf.environment as pg_rdf_env
import hydra.pg.rdf.mappings as pg_rdf_mappings
from hydra.demos.genpg.sales import sales_database_schema, sales_graph_schema, sales_mapping
from hydra.demos.genpg.health import health_database_schema, health_graph_schema, health_mapping
from hydra.graph import Graph as HydraGraph
from hydra.context import Context
from hydra.generation import bootstrap_graph
from hydra.tabular import TableType, Table
from hydra.relational import RelationName
import hydra.demos.genpg.transform as Transform

DEMO_NS = "urn:hydra:genpg:"


# ---------------------------------------------------------------------------
# CSV-to-PG pipeline (same as demo.py)

def decode_table_io(table_type, path):
    from hydra.dsl.python import Left, Right
    with open(path, 'r') as f:
        raw_lines = tuple(f.read().splitlines())
    parse_result = Transform.parse_table_lines(True, raw_lines)
    match parse_result:
        case Left(value=err):
            raise ValueError(f"CSV read error in {path}: {err}")
        case Right(value=table):
            pass
    decode_result = Transform.decode_table(table_type, table)
    match decode_result:
        case Left(value=err):
            raise ValueError(f"Decode error: {err}")
        case Right(value=decoded):
            return decoded


def transform_table(table_type, path, vspecs, especs, cx, g):
    from hydra.dsl.python import Left, Right
    table = decode_table_io(table_type, path)
    result = Transform.transform_table_rows(cx, g, vspecs, especs, table_type, table.data)
    match result:
        case Left(value=err):
            raise ValueError(f"Transform error: {err}")
        case Right(value=pair):
            return pair


def transform_tables(file_root, table_types, spec, cx, g):
    from hydra.dsl.python import Left, Right
    specs_result = Transform.element_specs_by_table(spec)
    match specs_result:
        case Left(value=err):
            raise ValueError(f"Error in mapping specification: {err}")
        case Right(value=by_table):
            pass
    tbl_types_by_name = Transform.table_types_by_name(tuple(table_types))
    all_vertices, all_edges = [], []
    for tname, (vspecs, especs) in by_table.items():
        table_type = tbl_types_by_name.get(RelationName(tname))
        if table_type is None:
            raise ValueError(f"Table specified in mapping does not exist: {tname}")
        vertices, edges = transform_table(table_type, os.path.join(file_root, tname), vspecs, especs, cx, g)
        all_vertices.extend(vertices)
        all_edges.extend(edges)
    return Transform.make_lazy_graph(tuple(all_vertices), tuple(all_edges))


# ---------------------------------------------------------------------------
# Instance-level: PG data → RDF (using generated mappings)

def make_default_env(term_to_iri, term_to_literal):
    """Create a PgRdfEnvironment for Term-valued property graphs."""
    return pg_rdf_env.PgRdfEnvironment(
        encode_vertex_id=lambda v: term_to_iri("vertex:", v),
        encode_vertex_label=lambda vl: rdf.Iri(DEMO_NS + vl.value),
        encode_edge_id=lambda v: term_to_iri("edge:", v),
        encode_edge_label=lambda el: rdf.Iri(DEMO_NS + el.value),
        encode_property_key=lambda pk: rdf.Iri(DEMO_NS + pk.value),
        encode_property_value=term_to_literal,
    )


def term_to_iri(prefix, term):
    """Convert a Hydra Term to an IRI."""
    match term:
        case hydra.core.TermLiteral(value=hydra.core.LiteralString(value=s)):
            return rdf.Iri(DEMO_NS + prefix + s)
        case hydra.core.TermLiteral(value=hydra.core.LiteralInteger(
                value=hydra.core.IntegerValueInt32(value=i))):
            return rdf.Iri(DEMO_NS + prefix + str(i))
        case _:
            raise ValueError(f"Unsupported term type for IRI encoding: {term}")


def term_to_literal(term):
    """Convert a Hydra Term to an RDF Literal."""
    match term:
        case hydra.core.TermLiteral(value=lit):
            return rdf_utils.encode_literal(lit)
        case _:
            raise ValueError(f"Expected a literal term: {term}")


def encode_lazy_graph(env, lg):
    """Encode a LazyGraph as an RDF Graph using the generated mappings."""
    vertex_descs = [pg_rdf_mappings.encode_vertex(env, v) for v in lg.vertices]
    edge_descs = [pg_rdf_mappings.encode_edge(env, e) for e in lg.edges]
    return rdf_utils.descriptions_to_graph(tuple(vertex_descs + edge_descs))


# ---------------------------------------------------------------------------
# Schema-level: GraphSchema → SHACL shapes (hand-written, mirrors Haskell)

def graph_schema_to_shapes_graph(schema):
    """Convert a GraphSchema[Type] to a SHACL ShapesGraph."""
    all_edge_types = list(schema.edges.values())
    defs = set()
    for vt in schema.vertices.values():
        defs.add(_vertex_type_to_node_shape(vt, all_edge_types))
    return shacl.ShapesGraph(frozenset(defs))


def _vertex_type_to_node_shape(vt, all_edge_types):
    vlabel = vt.label
    prop_constraints = set()
    for pt in vt.properties:
        ps = _property_type_to_shape(pt)
        prop_constraints.add(shacl.CommonConstraintProperty(
            frozenset([shacl.ReferenceDefinition(
                shacl.Definition(rdf.Iri(DEMO_NS + vlabel.value + "#" + pt.key.value), ps))])))

    for et in all_edge_types:
        if et.out == vlabel:
            ps = shacl.PropertyShape(
                common=_empty_common(constraints=frozenset([
                    shacl.CommonConstraintClass(frozenset([rdf.RdfsClass(None)])),
                    shacl.CommonConstraintNode(
                        frozenset([shacl.ReferenceNamed(rdf.Iri(DEMO_NS + et.in_.value))]))])),
                constraints=frozenset(),
                default_value=None,
                description=rdf.LangStrings(FrozenDict({})),
                name=rdf.LangStrings(FrozenDict({})),
                order=None,
                path=rdf.Iri(DEMO_NS + et.label.value))
            prop_constraints.add(shacl.CommonConstraintProperty(
                frozenset([shacl.ReferenceDefinition(
                    shacl.Definition(
                        rdf.Iri(DEMO_NS + vlabel.value + "#" + et.label.value), ps))])))

    common = _empty_common(
        constraints=frozenset(prop_constraints),
        target_class=frozenset([rdf.RdfsClass(None)]))
    return shacl.Definition(
        rdf.Iri(DEMO_NS + vlabel.value),
        shacl.ShapeNode(shacl.NodeShape(common)))


def _property_type_to_shape(pt):
    dt_iri = _type_to_xsd_iri(pt.value)
    constraints = frozenset([shacl.CommonConstraintDatatype(dt_iri)])
    prop_constraints = (frozenset([shacl.PropertyShapeConstraintMinCount(1)])
                        if pt.required else frozenset())
    return shacl.PropertyShape(
        common=_empty_common(constraints=constraints),
        constraints=prop_constraints,
        default_value=None,
        description=rdf.LangStrings(FrozenDict({})),
        name=rdf.LangStrings(FrozenDict({})),
        order=None,
        path=rdf.Iri(DEMO_NS + pt.key.value))


def _empty_common(constraints=frozenset(), target_class=frozenset()):
    return shacl.CommonProperties(
        constraints=constraints,
        deactivated=None,
        message=rdf.LangStrings(FrozenDict({})),
        severity=shacl.Severity.VIOLATION,
        target_class=target_class,
        target_node=frozenset(),
        target_objects_of=frozenset(),
        target_subjects_of=frozenset())


def _type_to_xsd_iri(typ):
    xsd = "http://www.w3.org/2001/XMLSchema#"
    match typ:
        case hydra.core.TypeLiteral(value=lt):
            return _literal_type_to_xsd(lt)
        case _:
            return rdf.Iri(xsd + "string")


def _literal_type_to_xsd(lt):
    xsd = "http://www.w3.org/2001/XMLSchema#"
    match lt:
        case hydra.core.LiteralTypeBoolean():
            return rdf.Iri(xsd + "boolean")
        case hydra.core.LiteralTypeString():
            return rdf.Iri(xsd + "string")
        case hydra.core.LiteralTypeFloat(value=ft):
            m = {
                hydra.core.FloatType.FLOAT32: "float",
                hydra.core.FloatType.FLOAT64: "double",
                hydra.core.FloatType.BIGFLOAT: "decimal",
            }
            return rdf.Iri(xsd + m.get(ft, "decimal"))
        case hydra.core.LiteralTypeInteger(value=it):
            m = {
                hydra.core.IntegerType.INT32: "int",
                hydra.core.IntegerType.INT64: "long",
                hydra.core.IntegerType.INT8: "byte",
                hydra.core.IntegerType.INT16: "short",
                hydra.core.IntegerType.UINT8: "unsignedByte",
                hydra.core.IntegerType.UINT16: "unsignedShort",
                hydra.core.IntegerType.UINT32: "unsignedInt",
                hydra.core.IntegerType.UINT64: "unsignedLong",
                hydra.core.IntegerType.BIGINT: "integer",
            }
            return rdf.Iri(xsd + m.get(it, "integer"))
        case _:
            return rdf.Iri(xsd + "string")


# ---------------------------------------------------------------------------
# SHACL → N-Triples serialization (hand-written, mirrors Haskell ShaclRdf)

def triples_to_ntriples(triples):
    """Serialize a list of triples to N-Triples string."""
    lines = sorted(set(_triple_to_nt(t) for t in triples))
    return "\n".join(lines) + ("\n" if lines else "")


def graph_to_ntriples(g):
    """Serialize an RDF Graph to N-Triples string."""
    return triples_to_ntriples(list(g.value))


def _triple_to_nt(t):
    return f"{_resource_to_nt(t.subject)} {_iri_to_nt(t.predicate)} {_node_to_nt(t.object)} ."


def _resource_to_nt(r):
    match r:
        case rdf.ResourceIri(value=iri):
            return _iri_to_nt(iri)
        case rdf.ResourceBnode(value=bn):
            return f"_:{bn.value}"


def _iri_to_nt(iri):
    return f"<{iri.value}>"


def _node_to_nt(n):
    match n:
        case rdf.NodeIri(value=iri):
            return _iri_to_nt(iri)
        case rdf.NodeBnode(value=bn):
            return f"_:{bn.value}"
        case rdf.NodeLiteral(value=lit):
            escaped = lit.lexical_form.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")
            return f'"{escaped}"^^{_iri_to_nt(lit.datatype_iri)}'


def shapes_graph_to_ntriples(sg):
    """Convert a ShapesGraph to an N-Triples string."""
    triples = []
    for defn in sg.value:
        triples.extend(_definition_to_triples(defn))
    return triples_to_ntriples(triples)


def _definition_to_triples(defn):
    match defn.target:
        case shacl.ShapeNode(value=ns):
            return _node_shape_to_triples(defn.iri, ns)
        case shacl.ShapeProperty(value=ps):
            return _property_shape_to_triples(rdf.ResourceIri(defn.iri), ps)
        case _:
            return []


def _node_shape_to_triples(iri, ns):
    subj = rdf.ResourceIri(iri)
    triples = [
        rdf.Triple(subj, rdf.Iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                   rdf.NodeIri(rdf.Iri("http://www.w3.org/ns/shacl#NodeShape"))),
        rdf.Triple(subj, rdf.Iri("http://www.w3.org/ns/shacl#targetClass"),
                   rdf.NodeIri(iri)),
    ]
    triples.extend(_common_to_triples(subj, ns.common))
    return triples


def _property_shape_to_triples(subj, ps):
    triples = [
        rdf.Triple(subj, rdf.Iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                   rdf.NodeIri(rdf.Iri("http://www.w3.org/ns/shacl#PropertyShape"))),
        rdf.Triple(subj, rdf.Iri("http://www.w3.org/ns/shacl#path"),
                   rdf.NodeIri(ps.path)),
    ]
    triples.extend(_common_to_triples(subj, ps.common))
    for psc in ps.constraints:
        triples.extend(_property_shape_constraint_to_triples(subj, psc))
    return triples


def _common_to_triples(subj, cp):
    triples = []
    for cc in cp.constraints:
        triples.extend(_common_constraint_to_triples(subj, cc))
    return triples


def _common_constraint_to_triples(subj, cc):
    sh = "http://www.w3.org/ns/shacl#"
    match cc:
        case shacl.CommonConstraintDatatype(value=iri):
            return [rdf.Triple(subj, rdf.Iri(sh + "datatype"), rdf.NodeIri(iri))]
        case shacl.CommonConstraintNode(value=refs):
            triples = []
            for ref in refs:
                match ref:
                    case shacl.ReferenceNamed(value=iri):
                        triples.append(rdf.Triple(subj, rdf.Iri(sh + "node"), rdf.NodeIri(iri)))
            return triples
        case shacl.CommonConstraintProperty(value=refs):
            triples = []
            for ref in refs:
                match ref:
                    case shacl.ReferenceDefinition(value=defn):
                        triples.append(rdf.Triple(subj, rdf.Iri(sh + "property"),
                                                  rdf.NodeIri(defn.iri)))
                        triples.extend(_property_shape_to_triples(rdf.ResourceIri(defn.iri), defn.target))
                    case shacl.ReferenceNamed(value=iri):
                        triples.append(rdf.Triple(subj, rdf.Iri(sh + "property"), rdf.NodeIri(iri)))
            return triples
        case shacl.CommonConstraintClass():
            return []  # RdfsClass is a stand-in type
        case _:
            return []


def _property_shape_constraint_to_triples(subj, psc):
    sh = "http://www.w3.org/ns/shacl#"
    xsd = "http://www.w3.org/2001/XMLSchema#"
    match psc:
        case shacl.PropertyShapeConstraintMinCount(value=n):
            return [rdf.Triple(subj, rdf.Iri(sh + "minCount"),
                               rdf.NodeLiteral(rdf.Literal(str(n), rdf.Iri(xsd + "integer"), None)))]
        case shacl.PropertyShapeConstraintMaxCount(value=n):
            return [rdf.Triple(subj, rdf.Iri(sh + "maxCount"),
                               rdf.NodeLiteral(rdf.Literal(str(n), rdf.Iri(xsd + "integer"), None)))]
        case _:
            return []


# ---------------------------------------------------------------------------
# Invalid data generation

def generate_invalid_data():
    """Generate intentionally non-conforming RDF data."""
    xsd = "http://www.w3.org/2001/XMLSchema#"
    rdf_type = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    triples = [
        # Employee missing required firstName and lastName
        rdf.Triple(rdf.ResourceIri(rdf.Iri("urn:hydra:genpg:vertex:invalid_emp")),
                   rdf.Iri(rdf_type),
                   rdf.NodeIri(rdf.Iri("urn:hydra:genpg:Employee"))),
        rdf.Triple(rdf.ResourceIri(rdf.Iri("urn:hydra:genpg:vertex:invalid_emp")),
                   rdf.Iri("urn:hydra:genpg:email"),
                   rdf.NodeLiteral(rdf.Literal("bad@example.com", rdf.Iri(xsd + "string"), None))),
        # Product with integer name instead of string
        rdf.Triple(rdf.ResourceIri(rdf.Iri("urn:hydra:genpg:vertex:invalid_prod")),
                   rdf.Iri(rdf_type),
                   rdf.NodeIri(rdf.Iri("urn:hydra:genpg:Product"))),
        rdf.Triple(rdf.ResourceIri(rdf.Iri("urn:hydra:genpg:vertex:invalid_prod")),
                   rdf.Iri("urn:hydra:genpg:name"),
                   rdf.NodeLiteral(rdf.Literal("42", rdf.Iri(xsd + "integer"), None))),
    ]
    return triples_to_ntriples(triples)


# ---------------------------------------------------------------------------
# Demo entry points

def generate_rdf(source_root, table_schemas, graph_mapping, graph_schema, output_dir):
    """Generate SHACL shapes + RDF data from CSV sources."""
    g = bootstrap_graph()
    cx = Context(trace=(), messages=(), other=FrozenDict({}))

    print(f"Reading CSV files from {source_root}/")
    start_ns = _time.perf_counter_ns()

    lg = transform_tables(source_root, list(table_schemas), graph_mapping, cx, g)

    # SHACL shapes
    print("Generating SHACL shapes...")
    shapes = graph_schema_to_shapes_graph(graph_schema)
    shapes_nt = shapes_graph_to_ntriples(shapes)
    shapes_file = output_dir + "-shapes.nt"
    with open(shapes_file, 'w') as f:
        f.write(shapes_nt)
    print(f"  Wrote shapes to {shapes_file}")

    # RDF data
    print("Encoding property graph as RDF...")
    env = make_default_env(term_to_iri, term_to_literal)
    data_graph = encode_lazy_graph(env, lg)
    data_nt = graph_to_ntriples(data_graph)
    data_file = output_dir + "-data.nt"
    with open(data_file, 'w') as f:
        f.write(data_nt)
    print(f"  Wrote {len(lg.vertices)} vertex and {len(lg.edges)} edge descriptions to {data_file}")

    # Invalid data
    print("Generating non-conforming RDF data...")
    invalid_nt = generate_invalid_data()
    invalid_file = output_dir + "-invalid.nt"
    with open(invalid_file, 'w') as f:
        f.write(invalid_nt)
    print(f"  Wrote to {invalid_file}")

    elapsed_ns = _time.perf_counter_ns() - start_ns
    print("Done.")
    print(f"HYDRA_TIME_MS={elapsed_ns / 1_000_000.0}", file=sys.stderr)


def generate_sales_rdf():
    source_root = os.path.join(_hydra_ext_root, "demos/genpg/data/sources/sales")
    output_dir = os.path.join(_hydra_ext_root, "demos/genpg/output/sales")
    generate_rdf(source_root, sales_database_schema, sales_mapping(),
                 sales_graph_schema(), output_dir)


def generate_health_rdf():
    source_root = os.path.join(_hydra_ext_root, "demos/genpg/data/sources/health")
    output_dir = os.path.join(_hydra_ext_root, "demos/genpg/output/health")
    generate_rdf(source_root, health_database_schema, health_mapping(),
                 health_graph_schema(), output_dir)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="GenPG RDF Demo")
    parser.add_argument("dataset", nargs="?", choices=["sales", "health"], default="sales")
    parser.add_argument("output_prefix", nargs="?", default=None,
                        help="Output file prefix (e.g., /tmp/run/sales). "
                             "Produces <prefix>-shapes.nt, <prefix>-data.nt, <prefix>-invalid.nt")
    args = parser.parse_args()
    if args.dataset == "sales":
        if args.output_prefix:
            source_root = os.path.join(_hydra_ext_root, "demos/genpg/data/sources/sales")
            generate_rdf(source_root, sales_database_schema, sales_mapping(),
                         sales_graph_schema(), args.output_prefix)
        else:
            generate_sales_rdf()
    else:
        if args.output_prefix:
            source_root = os.path.join(_hydra_ext_root, "demos/genpg/data/sources/health")
            generate_rdf(source_root, health_database_schema, health_mapping(),
                         health_graph_schema(), args.output_prefix)
        else:
            generate_health_rdf()
