"""Python driver for the Neo4j validation demo.

Connects to a running Neo4j over Bolt using the official Neo4j Python driver
(the ``neo4j`` package), reads all nodes and relationships, maps the driver's
types onto Hydra's ``hydra.neo4j.model`` types, and runs
``hydra.validate.neo4j.validate_graph`` against a graph type defined here. The
same validation runs identically in the Java counterpart.

The client-interfacing logic (the driver calls and the mapping from the
driver's types to Hydra's) lives in this demo for now; it will move to a
reusable ``overlay/python/hydra-neo4j`` once #511 lands.

Usage: python3 demo.py [uri] [user] [password]
(defaults: bolt://localhost:7687, neo4j, neo4j). If no Neo4j is reachable, or
the ``neo4j`` package is not installed, the demo prints a notice and exits 0
so it does not break offline runs.
"""

import sys

import hydra.neo4j.model as model
import hydra.validate.neo4j as validate
import hydra.validation as validation


def map_value(o):
    """Map a driver property value to a hydra.neo4j.model.Value."""
    if isinstance(o, bool):
        return model.ValueBoolean(o)
    if isinstance(o, int):
        return model.ValueInteger(o)
    if isinstance(o, float):
        return model.ValueFloat(o)
    if isinstance(o, str):
        return model.ValueString(o)
    if isinstance(o, list):
        return model.ValueList([map_value(x) for x in o])
    # Temporal/spatial/bytes are out of scope for this demo's fixture.
    return model.ValueString(str(o))


def map_properties(driver_props):
    return {model.Key(k): map_value(v) for k, v in driver_props.items()}


def map_node(dn):
    labels = frozenset(model.NodeLabel(l) for l in dn.labels)
    return model.Node_(
        id=model.ElementId(dn.element_id),
        labels=labels,
        properties=map_properties(dict(dn)))


def map_relationship(dr):
    return model.Relationship(
        id=model.ElementId(dr.element_id),
        properties=map_properties(dict(dr)),
        type=model.RelationshipType(dr.type),
        start=model.ElementId(dr.start_node.element_id),
        end=model.ElementId(dr.end_node.element_id))


def movie_graph_type():
    """The graph type (schema) to validate against."""
    def existence(key):
        return model.ConstraintDefinition(
            name=None,
            body=model.ConstraintPropertyExistence(
                model.PropertyExistenceConstraint(model.Key(key))))

    def property_type(key, vtype):
        return model.ConstraintDefinition(
            name=None,
            body=model.ConstraintPropertyType(
                model.PropertyTypeConstraint(model.Key(key), vtype)))

    person = model.NodeElementType(
        identifying_label=model.NodeLabel("Person"),
        implied_labels=frozenset(),
        constraints=[
            existence("name"),
            property_type("name", model.ValueTypeString()),
            property_type("born", model.ValueTypeInteger())])

    movie = model.NodeElementType(
        identifying_label=model.NodeLabel("Movie"),
        implied_labels=frozenset(),
        constraints=[
            existence("title"),
            property_type("title", model.ValueTypeString()),
            existence("released"),
            property_type("released", model.ValueTypeInteger())])

    acted_in = model.RelationshipElementType(
        type=model.RelationshipType("ACTED_IN"),
        start_label=model.NodeLabel("Person"),
        end_label=model.NodeLabel("Movie"),
        constraints=[])

    # Overloaded LIKES: Person->Movie and Person->Person.
    likes_movie = model.RelationshipElementType(
        type=model.RelationshipType("LIKES"),
        start_label=model.NodeLabel("Person"),
        end_label=model.NodeLabel("Movie"),
        constraints=[])
    likes_person = model.RelationshipElementType(
        type=model.RelationshipType("LIKES"),
        start_label=model.NodeLabel("Person"),
        end_label=model.NodeLabel("Person"),
        constraints=[])

    return model.GraphType(
        nodes=[person, movie],
        relationships=[acted_in, likes_movie, likes_person])


def report_all_profile():
    """An open-world profile that reports all violations (high bounds)."""
    base = validate.default_neo4j_profile()
    return validation.ValidationProfile(
        error_rules=base.error_rules,
        warning_rules=base.warning_rules,
        max_errors=1000,
        max_warnings=1000)


def main():
    uri = sys.argv[1] if len(sys.argv) > 1 else "bolt://localhost:7687"
    user = sys.argv[2] if len(sys.argv) > 2 else "neo4j"
    password = sys.argv[3] if len(sys.argv) > 3 else "neo4j"

    try:
        from neo4j import GraphDatabase
    except ImportError:
        print("The 'neo4j' package is not installed (pip install neo4j).")
        print("Skipping the live-validation demo.")
        return

    try:
        driver = GraphDatabase.driver(uri, auth=(user, password))
        driver.verify_connectivity()
    except Exception as e:  # noqa: BLE001 - demo: any connection failure -> skip
        print(f"No Neo4j reachable at {uri} ({e}).")
        print("Skipping the live-validation demo. Start Neo4j and seed it with "
              "demos/neo4j-validation/fixture.cypher to run it.")
        return

    with driver:
        with driver.session() as session:
            nodes = [map_node(r["n"]) for r in session.run("MATCH (n) RETURN n")]
            rels = [map_relationship(r["rel"]) for r in session.run("MATCH ()-[rel]->() RETURN rel")]

    print(f"Read {len(nodes)} nodes and {len(rels)} relationships from Neo4j.")

    graph_type = movie_graph_type()
    profile = report_all_profile()
    result = validate.validate_graph(profile, graph_type, nodes, rels)

    if not result.errors:
        print("The graph conforms to the graph type. No violations.")
    else:
        print(f"{len(result.errors)} violation(s):")
        for err in result.errors:
            print(f"  - {err}")


if __name__ == "__main__":
    main()
