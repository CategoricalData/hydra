// TypeScript driver for the Neo4j validation translingual demo (JSON-artifact variant).
//
// Reads the shared schema.json and each graph JSON file (produced by the Java
// GenerateData from DSL definitions), decodes them into hydra.neo4j.model values,
// and validates each graph against the schema with hydra.validate.neo4j.validateGraph.
// The same validator -- generated from one Hydra source -- runs identically in the
// Java, Python, and Haskell counterparts; because every host reads the same JSON
// files, the data and the logic are identical across languages by construction.
//
// Usage: tsx jsonDemo.ts <data-directory>
//
// Import paths are resolved against the generated hydra-pg TypeScript dist tree
// (dist/typescript/hydra-pg/src/main/typescript) via the tsconfig the runner writes.

import * as fs from "node:fs";
import * as path from "node:path";

import * as model from "hydra/neo4j/model.js";
import * as validate from "hydra/validate/neo4j.js";
import * as Maps from "hydra/overlay/typescript/lib/maps.js";
import * as Sets from "hydra/overlay/typescript/lib/sets.js";
import { Given, None } from "hydra/runtime.js";

// ---------------------------------------------------------------------------
// Decoders: plain JSON (JSON.parse output) -> hydra.neo4j.model
// ---------------------------------------------------------------------------

type Json = any;

function decodeGraphType(j: Json): model.GraphType {
  return {
    nodes: (j.nodes as Json[]).map(decodeNodeElementType),
    relationships: (j.relationships as Json[]).map(decodeRelElementType),
  };
}

function decodeNodeElementType(j: Json): model.NodeElementType {
  return {
    identifyingLabel: { value: j.identifyingLabel },
    impliedLabels: Sets.fromList((j.impliedLabels as string[]).map((s) => ({ value: s }))),
    constraints: (j.constraints as Json[]).map(decodeConstraintDef),
  };
}

function decodeRelElementType(j: Json): model.RelationshipElementType {
  return {
    type_: { value: j.type },
    startLabel: { value: j.startLabel },
    endLabel: { value: j.endLabel },
    constraints: (j.constraints as Json[]).map(decodeConstraintDef),
  };
}

function decodeConstraintDef(j: Json): model.ConstraintDefinition {
  // The fixtures leave constraint names unset (encoded as null); model as none.
  return { name: None, body: decodeConstraint(j.body) };
}

function decodeConstraint(j: Json): model.Constraint {
  if ("propertyExistence" in j) {
    return { tag: "propertyExistence", value: { property: { value: j.propertyExistence.property } } };
  }
  if ("propertyType" in j) {
    return {
      tag: "propertyType",
      value: { property: { value: j.propertyType.property }, type_: decodeValueType(j.propertyType.type) },
    };
  }
  throw new Error("Unsupported constraint in demo fixtures: " + JSON.stringify(Object.keys(j)));
}

function decodeValueType(j: Json): model.ValueType {
  if ("boolean" in j) return { tag: "boolean" };
  if ("string" in j) return { tag: "string" };
  if ("integer" in j) return { tag: "integer" };
  if ("float" in j) return { tag: "float" };
  throw new Error("Unsupported value type in demo fixtures: " + JSON.stringify(Object.keys(j)));
}

function decodeNodes(j: Json): model.Node[] {
  return (j.nodes as Json[]).map(decodeNode);
}

function decodeRelationships(j: Json): model.Relationship[] {
  return (j.relationships as Json[]).map(decodeRelationship);
}

function decodeNode(j: Json): model.Node {
  return {
    id: { value: j.id },
    labels: Sets.fromList((j.labels as string[]).map((s) => ({ value: s }))),
    properties: decodeProperties(j.properties),
  };
}

function decodeRelationship(j: Json): model.Relationship {
  return {
    id: { value: j.id },
    properties: decodeProperties(j.properties),
    type_: { value: j.type },
    start: { value: j.start },
    end: { value: j.end },
  };
}

function decodeProperties(arr: Json[]): ReadonlyMap<model.Key, model.Value> {
  return Maps.fromList(arr.map((e) => [{ value: e.key }, decodeValue(e.value)] as const));
}

function decodeValue(j: Json): model.Value {
  if ("boolean" in j) return { tag: "boolean", value: j.boolean };
  if ("string" in j) return { tag: "string", value: j.string };
  if ("integer" in j) return { tag: "integer", value: BigInt(j.integer) };
  if ("float" in j) return { tag: "float", value: Number(j.float) };
  if ("list" in j) return { tag: "list", value: (j.list as Json[]).map(decodeValue) };
  throw new Error("Unsupported value in demo fixtures: " + JSON.stringify(Object.keys(j)));
}

// ---------------------------------------------------------------------------
// Human-readable rendering of a structured graph-validation error
// ---------------------------------------------------------------------------

function valueTypeName(vt: model.ValueType): string {
  return (vt.tag as string).toUpperCase();
}

function describeNode(e: any): string {
  switch (e.tag) {
    case "missingProperty":
      return `missing required property '${e.value.key.value}'`;
    case "wrongPropertyType":
      return `property '${e.value.key.value}' has the wrong type (expected ${valueTypeName(e.value.expectedType)})`;
    case "missingImpliedLabel":
      return `missing implied label '${e.value.label.value}'`;
    case "noSuchLabel":
      return "no node element type matches the node's labels";
    default:
      return e.tag;
  }
}

function describeRel(e: any): string {
  switch (e.tag) {
    case "noMatchingPattern":
      return "endpoints match no declared pattern for this relationship type";
    case "missingProperty":
      return `missing required property '${e.value.key.value}'`;
    case "wrongPropertyType":
      return `property '${e.value.key.value}' has the wrong type (expected ${valueTypeName(e.value.expectedType)})`;
    case "noSuchType":
      return "no relationship element type has this type";
    default:
      return e.tag;
  }
}

function describe(err: any): string {
  if (err.tag === "node") {
    return `node ${err.value.id.value}: ${describeNode(err.value.error)}`;
  }
  if (err.tag === "relationship") {
    return `relationship ${err.value.id.value}: ${describeRel(err.value.error)}`;
  }
  return JSON.stringify(err);
}

// ---------------------------------------------------------------------------

const GRAPH_FILES = [
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
];

function parseJsonFile(p: string): Json {
  return JSON.parse(fs.readFileSync(p, "utf-8"));
}

function reportAllProfile(): any {
  const base = validate.defaultNeo4jProfile;
  // defaultNeo4jProfile may be a value or a thunk depending on emission; handle both.
  const profile = typeof base === "function" ? base() : base;
  return { ...profile, maxErrors: 1000, maxWarnings: 1000 };
}

// The canonical fixture list when present, otherwise every *.json except schema.json
// (so a Cypher-ingested graph dropped in is picked up automatically).
function graphNames(dataDir: string): string[] {
  const canonical = GRAPH_FILES.filter((n) => fs.existsSync(path.join(dataDir, `${n}.json`)));
  if (canonical.length > 0) return canonical;
  return fs.readdirSync(dataDir)
    .filter((n) => n.endsWith(".json") && n !== "schema.json")
    .map((n) => n.slice(0, -".json".length))
    .sort();
}

function main(): void {
  const dataDir = process.argv[2];
  if (!dataDir) {
    console.error("Usage: tsx jsonDemo.ts <data-directory>");
    process.exit(1);
  }

  const schema = decodeGraphType(parseJsonFile(path.join(dataDir, "schema.json")));
  const profile = reportAllProfile();

  for (const name of graphNames(dataDir)) {
    const graphPath = path.join(dataDir, `${name}.json`);
    if (!fs.existsSync(graphPath)) continue;
    const j = parseJsonFile(graphPath);
    const nodes = decodeNodes(j);
    const rels = decodeRelationships(j);
    const result = validate.validateGraph(profile, schema, nodes, rels);
    if (result.errors.length === 0) {
      console.log(`Graph "${name}": VALID`);
    } else {
      console.log(`Graph "${name}": INVALID (${result.errors.length} violation(s))`);
      for (const e of result.errors) {
        console.log(`  - ${describe(e)}`);
      }
    }
  }
}

main();
