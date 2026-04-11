#!/usr/bin/env python3
"""
GraphQL JSON Demo: Query Hydra kernel modules using GraphQL.

This script loads the generated GraphQL schema and JSON module data,
then runs example queries against them.

Prerequisites:
  pip install graphql-core

Usage:
  python3 query.py
"""

import json
import os
import sys

try:
    from graphql import build_schema, graphql_sync
except ImportError:
    print("Error: graphql-core is required. Install it with:")
    print("  pip install graphql-core")
    sys.exit(1)

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
OUTPUT_DIR = os.path.join(SCRIPT_DIR, "output")
QUERIES_DIR = os.path.join(SCRIPT_DIR, "queries")
JSON_DIR = os.path.join(SCRIPT_DIR, "..", "..", "..", "hydra-haskell", "src", "gen-main", "json", "hydra")


def load_query(name):
    """Load a GraphQL query from the queries/ directory."""
    with open(os.path.join(QUERIES_DIR, name)) as f:
        return f.read()


def load_modules():
    """Load the kernel module data from the existing JSON kernel files (recursive)."""
    modules = []
    for root, dirs, files in os.walk(JSON_DIR):
        for filename in sorted(files):
            if filename.endswith(".json"):
                filepath = os.path.join(root, filename)
                with open(filepath) as f:
                    try:
                        data = json.load(f)
                        if isinstance(data, dict) and "namespace" in data:
                            modules.append(data)
                    except json.JSONDecodeError:
                        pass
    return modules


def unwrap_maybe(val):
    """Unwrap a Hydra Maybe value: null -> None, [x] -> x."""
    if val is None:
        return None
    if isinstance(val, list) and len(val) == 1:
        return val[0]
    return val


def build_graphql_schema():
    """Build a GraphQL schema with resolvers for querying modules."""
    # Load all generated .graphql files
    schema_dir = os.path.join(OUTPUT_DIR, "hydra")
    type_defs = ""
    for filename in sorted(os.listdir(schema_dir)):
        if filename.endswith(".graphql"):
            with open(os.path.join(schema_dir, filename)) as f:
                type_defs += f.read() + "\n"

    # Add Query root type and referenced types from hydra.core
    schema_str = type_defs + """

# Stub scalars for unresolved type variables
scalar a
scalar b
scalar n
scalar t1
scalar t2
scalar v1
scalar v2

# Referenced types from hydra.core (simplified for querying)
type Binding {
  name: Name!
  term: String
  type: String
}

type Name {
  value: String!
}

type Term {
  value: String
}

type Type {
  value: String
}

type TypeScheme {
  variables: [Name!]!
  type: String
}

type Primitive {
  name: Name!
}

# Root query type
type Query {
  \"\"\"All modules in the kernel\"\"\"
  modules: [Module!]!

  \"\"\"Find a module by namespace\"\"\"
  module(namespace: String!): Module

  \"\"\"Find all modules that directly depend on a given namespace (term dependencies)\"\"\"
  dependentsOf(namespace: String!): [Module!]!

  \"\"\"Find all modules whose description contains a search string\"\"\"
  search(query: String!): [Module!]!
}
"""
    return build_schema(schema_str)


def make_resolvers(modules):
    """Create resolver functions for the GraphQL schema."""

    def resolve_namespace(obj, info):
        return obj.get("namespace", "")

    def resolve_description(obj, info):
        return unwrap_maybe(obj.get("description"))

    def resolve_elements(obj, info):
        raw = obj.get("elements", [])
        result = []
        for el in raw:
            name = el.get("name", "")
            result.append({
                "name": {"value": name},
                "term": str(el.get("term", ""))[:100] if el.get("term") else None,
                "type": str(el.get("type")) if el.get("type") else None,
            })
        return result

    def resolve_term_deps(obj, info):
        return obj.get("termDependencies", [])

    def resolve_type_deps(obj, info):
        return obj.get("typeDependencies", [])

    return {
        "Query": {
            "modules": lambda obj, info: modules,
            "module": lambda obj, info, namespace: next(
                (m for m in modules if m.get("namespace") == namespace), None
            ),
            "dependentsOf": lambda obj, info, namespace: [
                m for m in modules if namespace in m.get("termDependencies", [])
            ],
            "search": lambda obj, info, query: [
                m for m in modules
                if query.lower() in str(unwrap_maybe(m.get("description", "")) or "").lower()
            ],
        },
        "Module": {
            "namespace": resolve_namespace,
            "description": resolve_description,
            "elements": resolve_elements,
            "termDependencies": resolve_term_deps,
            "typeDependencies": resolve_type_deps,
        },
        "Namespace": {
            "value": lambda obj, info: obj if isinstance(obj, str) else obj.get("value", ""),
        },
        "Name": {
            "value": lambda obj, info: obj.get("value", "") if isinstance(obj, dict) else str(obj),
        },
    }


def run_query(schema, modules, query_str, variables=None):
    """Execute a GraphQL query and print results."""
    resolvers = make_resolvers(modules)

    # Patch resolvers into schema
    for type_name, field_resolvers in resolvers.items():
        schema_type = schema.type_map.get(type_name)
        if schema_type and hasattr(schema_type, "fields"):
            for field_name, resolver in field_resolvers.items():
                if field_name in schema_type.fields:
                    schema_type.fields[field_name].resolve = resolver

    result = graphql_sync(schema, query_str, variable_values=variables)
    if result.errors:
        for err in result.errors:
            print(f"  Error: {err}")
    return result.data


def main():
    print("=== GraphQL JSON Demo: Querying the Hydra Kernel ===\n")

    if not os.path.exists(JSON_DIR):
        print(f"Error: {JSON_DIR} not found.")
        print("The kernel JSON files should already exist in hydra-kernel/src/gen-main/json/hydra/")
        sys.exit(1)

    modules = load_modules()
    schema = build_graphql_schema()

    # Query 1: List all module namespaces
    print("--- Query 1: List all module namespaces ---")
    result = run_query(schema, modules, load_query("list-modules.graphql"))
    if result:
        namespaces = [m["namespace"]["value"] for m in result["modules"]]
        print(f"  Found {len(namespaces)} modules")
        for ns in sorted(namespaces)[:10]:
            print(f"    {ns}")
        print(f"    ... and {len(namespaces) - 10} more")
    print()

    # Query 2: Find all modules that depend on hydra.rewriting
    print("--- Query 2: Modules depending on hydra.rewriting ---")
    result = run_query(schema, modules,
        load_query("dependents-of.graphql"), {"ns": "hydra.rewriting"})
    if result:
        deps = result["dependentsOf"]
        print(f"  Found {len(deps)} modules depending on hydra.rewriting:")
        for m in sorted(deps, key=lambda x: x["namespace"]["value"]):
            desc = m.get("description") or "(no description)"
            print(f"    {m['namespace']['value']}: {desc}")
    print()

    # Query 3: Get details of a specific module
    print("--- Query 3: Details of hydra.module ---")
    result = run_query(schema, modules,
        load_query("module-details.graphql"), {"ns": "hydra.module"})
    if result and result.get("module"):
        m = result["module"]
        print(f"  Namespace: {m['namespace']['value']}")
        print(f"  Description: {m.get('description')}")
        print(f"  Term dependencies: {[d['value'] for d in m['termDependencies']]}")
        print(f"  Type dependencies: {[d['value'] for d in m['typeDependencies']]}")
    print()

    # Query 4: Search for modules by description
    print("--- Query 4: Search for modules about 'inference' ---")
    result = run_query(schema, modules,
        load_query("search.graphql"), {"q": "inference"})
    if result:
        matches = result["search"]
        print(f"  Found {len(matches)} modules:")
        for m in matches:
            print(f"    {m['namespace']['value']}: {m.get('description')}")
    print()

    # Query 5: Module with element count
    print("--- Query 5: Modules with their element counts ---")
    result = run_query(schema, modules, load_query("element-counts.graphql"))
    if result:
        mods = sorted(result["modules"], key=lambda x: len(x["elements"]), reverse=True)
        print("  Top 10 modules by element count:")
        for m in mods[:10]:
            print(f"    {m['namespace']['value']}: {len(m['elements'])} elements")


if __name__ == "__main__":
    main()
