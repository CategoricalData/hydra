# Extracted test graph builder for Python.
# Separated from test_suite_runner to break circular import:
# test_suite_runner -> test_suite -> test_env -> test_suite_runner

from __future__ import annotations

import hydra.core.model
import hydra.core.graph
import hydra.core.test.test_graph as test_graph
from hydra.core.overlay.python.dsl.python import FrozenDict, None_


def _load_kernel_term_bindings() -> dict[hydra.core.model.Name, hydra.core.model.Binding]:
    import sys
    from hydra.generation import load_modules_from_json, strip_all_term_types

    old_limit = sys.getrecursionlimit()
    sys.setrecursionlimit(10000)

    # Locate dist/json/hydra-kernel: either via HYDRA_JSON_DIR env var or by
    # searching upward from CWD for a dist/json directory.
    import os
    json_dir = os.environ.get("HYDRA_JSON_DIR")
    if not json_dir:
        search = os.path.abspath(os.getcwd())
        while search != "/":
            candidate = os.path.join(search, "dist", "json", "hydra-kernel", "src", "main", "json")
            if os.path.isdir(candidate):
                json_dir = candidate
                break
            search = os.path.dirname(search)
    if not json_dir:
        json_dir = "../../dist/json/hydra-kernel/src/main/json"  # fallback

    evaluator_term_namespaces = [
        hydra.core.model.Name("hydra.core.annotations"),
        hydra.core.model.Name("hydra.core.constants"),
        hydra.core.model.Name("hydra.core.decode.model"),
        hydra.core.model.Name("hydra.core.dependencies"),
        hydra.core.model.Name("hydra.core.encode.model"),
        hydra.core.model.Name("hydra.core.extract.model"),
        hydra.core.model.Name("hydra.core.lexical"),
        hydra.core.model.Name("hydra.core.rewriting"),
        hydra.core.model.Name("hydra.core.scoping"),
        hydra.core.model.Name("hydra.core.print.model"),
        hydra.core.model.Name("hydra.core.strip"),
        hydra.core.model.Name("hydra.core.variables"),
    ]

    term_mods = load_modules_from_json(json_dir, evaluator_term_namespaces)
    term_mods = strip_all_term_types(term_mods)

    sys.setrecursionlimit(old_limit)

    from hydra.core.packaging import DefinitionTerm
    from hydra.core.model import Binding
    bindings = {}
    for mod in term_mods:
        for d in mod.definitions:
            if isinstance(d, DefinitionTerm):
                td = d.value
                bindings[td.name] = Binding(td.name, td.body, td.signature)

    return bindings


def _load_bootstrap_type_schemes() -> FrozenDict:
    from hydra.core.json.bootstrap import types_by_name
    from hydra.core.scoping import f_type_to_type_scheme

    result = {}
    for name, typ in types_by_name.items():
        result[name] = f_type_to_type_scheme(typ)
    return FrozenDict(result)


def build_test_graph() -> hydra.core.graph.Graph:
    import hydra.core.lexical
    from hydra.generation import bootstrap_graph

    bs_graph = bootstrap_graph()

    bootstrap_types = _load_bootstrap_type_schemes()

    from hydra.core.scoping import f_type_to_type_scheme
    test_types_dict = test_graph.test_types()

    all_schema_types = dict(bootstrap_types)
    for name, typ in test_types_dict.items():
        all_schema_types[name] = f_type_to_type_scheme(typ)
    schema_types = FrozenDict(all_schema_types)

    kernel_terms = _load_kernel_term_bindings()
    kernel_term_bindings = list(kernel_terms.values())

    test_terms_dict = test_graph.test_terms()
    data_bindings = [hydra.core.model.Binding(name=name, term=term, type_scheme=None_())
                     for name, term in test_terms_dict.items()]

    return hydra.core.lexical.elements_to_graph(
        bs_graph, schema_types, tuple(kernel_term_bindings + data_bindings))
