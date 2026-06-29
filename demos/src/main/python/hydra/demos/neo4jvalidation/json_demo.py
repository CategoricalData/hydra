"""Python driver for the Neo4j validation translingual demo (JSON-artifact variant).

Reads the shared ``schema.json`` and each graph JSON file (produced by the Java
``GenerateData`` from DSL definitions), decodes them back into
``hydra.neo4j.model`` values, and validates each graph against the schema with
``hydra.validate.neo4j.validate_graph``. The same validator -- generated from one
Hydra source -- runs identically in the Java and Haskell counterparts; because
every host reads the *same* JSON files, the data and the logic are identical
across languages by construction.

Usage: python3 json_demo.py <data-directory>
"""

import sys
from pathlib import Path

import hydra.error.neo4j as err
import hydra.json.model as jmodel
import hydra.json.parser as jparser
import hydra.neo4j.model as model
import hydra.parsing as parsing
import hydra.validate.neo4j as validate
import hydra.validation as validation


# ---------------------------------------------------------------------------
# JSON access helpers (over hydra.json.model.Value)
# ---------------------------------------------------------------------------

def as_object(v):
    if isinstance(v, jmodel.ValueObject):
        return {k: val for (k, val) in v.value}
    raise ValueError(f"Expected JSON object, got {type(v).__name__}")


def as_array(v):
    if isinstance(v, jmodel.ValueArray):
        return list(v.value)
    raise ValueError(f"Expected JSON array, got {type(v).__name__}")


def as_string(v):
    if isinstance(v, jmodel.ValueString):
        return v.value
    raise ValueError(f"Expected JSON string, got {type(v).__name__}")


def as_bool(v):
    if isinstance(v, jmodel.ValueBoolean):
        return v.value
    raise ValueError(f"Expected JSON boolean, got {type(v).__name__}")


def as_long(v):
    # 64-bit integers are encoded as JSON strings; accept either.
    if isinstance(v, jmodel.ValueString):
        return int(v.value)
    if isinstance(v, jmodel.ValueNumber):
        return int(v.value)
    raise ValueError(f"Expected JSON integer, got {type(v).__name__}")


def field(obj, name):
    if name not in obj:
        raise ValueError(f"Missing required field: {name}")
    return obj[name]


# ---------------------------------------------------------------------------
# Decoders: JSON -> hydra.neo4j.model
# ---------------------------------------------------------------------------

def decode_graph_type(v):
    obj = as_object(v)
    nodes = [decode_node_element_type(x) for x in as_array(field(obj, "nodes"))]
    rels = [decode_rel_element_type(x) for x in as_array(field(obj, "relationships"))]
    return model.GraphType(nodes=nodes, relationships=rels)


def decode_node_element_type(v):
    obj = as_object(v)
    label = model.NodeLabel(as_string(field(obj, "identifyingLabel")))
    implied = frozenset(model.NodeLabel(as_string(x)) for x in as_array(field(obj, "impliedLabels")))
    constraints = [decode_constraint_def(x) for x in as_array(field(obj, "constraints"))]
    return model.NodeElementType(
        identifying_label=label, implied_labels=implied, constraints=constraints)


def decode_rel_element_type(v):
    obj = as_object(v)
    return model.RelationshipElementType(
        type=model.RelationshipType(as_string(field(obj, "type"))),
        start_label=model.NodeLabel(as_string(field(obj, "startLabel"))),
        end_label=model.NodeLabel(as_string(field(obj, "endLabel"))),
        constraints=[decode_constraint_def(x) for x in as_array(field(obj, "constraints"))])


def decode_constraint_def(v):
    obj = as_object(v)
    # The fixtures leave constraint names unset (encoded as null); model them as None.
    return model.ConstraintDefinition(name=None, body=decode_constraint(field(obj, "body")))


def decode_constraint(v):
    obj = as_object(v)
    if "propertyExistence" in obj:
        c = as_object(obj["propertyExistence"])
        return model.ConstraintPropertyExistence(
            model.PropertyExistenceConstraint(model.Key(as_string(field(c, "property")))))
    if "propertyType" in obj:
        c = as_object(obj["propertyType"])
        return model.ConstraintPropertyType(model.PropertyTypeConstraint(
            model.Key(as_string(field(c, "property"))),
            decode_value_type(field(c, "type"))))
    raise ValueError(f"Unsupported constraint in demo fixtures: {list(obj)}")


def decode_value_type(v):
    obj = as_object(v)
    if "boolean" in obj:
        return model.ValueTypeBoolean()
    if "string" in obj:
        return model.ValueTypeString()
    if "integer" in obj:
        return model.ValueTypeInteger()
    if "float" in obj:
        return model.ValueTypeFloat()
    raise ValueError(f"Unsupported value type in demo fixtures: {list(obj)}")


def decode_nodes(v):
    obj = as_object(v)
    return [decode_node(x) for x in as_array(field(obj, "nodes"))]


def decode_relationships(v):
    obj = as_object(v)
    return [decode_relationship(x) for x in as_array(field(obj, "relationships"))]


def decode_node(v):
    obj = as_object(v)
    labels = frozenset(model.NodeLabel(as_string(x)) for x in as_array(field(obj, "labels")))
    return model.Node_(
        id=model.ElementId(as_string(field(obj, "id"))),
        labels=labels,
        properties=decode_properties(field(obj, "properties")))


def decode_relationship(v):
    obj = as_object(v)
    return model.Relationship(
        id=model.ElementId(as_string(field(obj, "id"))),
        properties=decode_properties(field(obj, "properties")),
        type=model.RelationshipType(as_string(field(obj, "type"))),
        start=model.ElementId(as_string(field(obj, "start"))),
        end=model.ElementId(as_string(field(obj, "end"))))


def decode_properties(v):
    result = {}
    for entry in as_array(v):
        e = as_object(entry)
        result[model.Key(as_string(field(e, "key")))] = decode_value(field(e, "value"))
    return result


def decode_value(v):
    obj = as_object(v)
    if "boolean" in obj:
        return model.ValueBoolean(as_bool(obj["boolean"]))
    if "string" in obj:
        return model.ValueString(as_string(obj["string"]))
    if "integer" in obj:
        return model.ValueInteger(as_long(obj["integer"]))
    if "float" in obj:
        return model.ValueFloat(float(obj["float"].value))
    if "list" in obj:
        return model.ValueList([decode_value(x) for x in as_array(obj["list"])])
    raise ValueError(f"Unsupported value in demo fixtures: {list(obj)}")


# ---------------------------------------------------------------------------
# Human-readable rendering of a structured graph-validation error
# ---------------------------------------------------------------------------

def value_type_name(vt):
    return type(vt).__name__.replace("ValueType", "").upper()


def describe_node_error(e):
    if isinstance(e, err.InvalidNodeErrorMissingProperty):
        return f"missing required property '{e.value.key.value}'"
    if isinstance(e, err.InvalidNodeErrorWrongPropertyType):
        return f"property '{e.value.key.value}' has the wrong type (expected {value_type_name(e.value.expected_type)})"
    if isinstance(e, err.InvalidNodeErrorMissingImpliedLabel):
        return f"missing implied label '{e.value.label.value}'"
    if isinstance(e, err.InvalidNodeErrorNoSuchLabel):
        return "no node element type matches the node's labels"
    return type(e).__name__


def describe_rel_error(e):
    if isinstance(e, err.InvalidRelationshipErrorNoMatchingPattern):
        return "endpoints match no declared pattern for this relationship type"
    if isinstance(e, err.InvalidRelationshipErrorMissingProperty):
        return f"missing required property '{e.value.key.value}'"
    if isinstance(e, err.InvalidRelationshipErrorWrongPropertyType):
        return f"property '{e.value.key.value}' has the wrong type (expected {value_type_name(e.value.expected_type)})"
    if isinstance(e, err.InvalidRelationshipErrorNoSuchType):
        return "no relationship element type has this type"
    return type(e).__name__


def describe(err_value):
    if isinstance(err_value, err.InvalidGraphErrorNode):
        return f"node {err_value.value.id.value}: {describe_node_error(err_value.value.error)}"
    if isinstance(err_value, err.InvalidGraphErrorRelationship):
        return f"relationship {err_value.value.id.value}: {describe_rel_error(err_value.value.error)}"
    return str(err_value)


# ---------------------------------------------------------------------------

GRAPH_FILES = [
    "valid",
    "missing_required_property",
    "wrong_property_type",
    "missing_required_released",
    "endpoint_mismatch",
    "valid_richer",
    "valid_likes_person",
    "multiple_violations",
    "optional_wrong_type",
    "acted_in_wrong_direction",
    "undeclared_label_open_world",
]


def parse_json_file(path):
    text = Path(path).read_text(encoding="utf-8")
    result = jparser.parse_json(text)
    if isinstance(result, parsing.ParseResultSuccess):
        return result.value.value
    raise ValueError(f"Failed to parse {path}: {result.value}")


def report_all_profile():
    base = validate.default_neo4j_profile()
    return validation.ValidationProfile(
        error_rules=base.error_rules,
        warning_rules=base.warning_rules,
        max_errors=1000,
        max_warnings=1000)


def graph_names(data_dir):
    # The canonical fixture list when present, otherwise every *.json except schema.json
    # (so a Cypher-ingested graph dropped in is picked up automatically).
    canonical = [n for n in GRAPH_FILES if (data_dir / f"{n}.json").exists()]
    if canonical:
        return canonical
    return sorted(
        p.stem for p in data_dir.glob("*.json") if p.name != "schema.json")


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 json_demo.py <data-directory>", file=sys.stderr)
        sys.exit(1)

    data_dir = Path(sys.argv[1])
    schema = decode_graph_type(parse_json_file(data_dir / "schema.json"))
    profile = report_all_profile()

    for name in graph_names(data_dir):
        graph_path = data_dir / f"{name}.json"
        if not graph_path.exists():
            continue
        graph_json = parse_json_file(graph_path)
        nodes = decode_nodes(graph_json)
        rels = decode_relationships(graph_json)
        result = validate.validate_graph(profile, schema, nodes, rels)
        if not result.errors:
            print(f'Graph "{name}": VALID')
        else:
            print(f'Graph "{name}": INVALID ({len(result.errors)} violation(s))')
            for e in result.errors:
                print(f"  - {describe(e)}")


if __name__ == "__main__":
    main()
