# Extracted test graph builder for Python.
# Separated from test_suite_runner to break circular import:
# test_suite_runner -> test_suite -> test_env -> test_suite_runner

from __future__ import annotations

import hydra.core
import hydra.graph
import hydra.test.test_graph as test_graph
from hydra.dsl.python import FrozenDict, Nothing


def _load_kernel_term_bindings() -> dict[hydra.core.Name, hydra.core.Binding]:
    import sys
    from hydra.generation import load_modules_from_json, strip_all_term_types

    old_limit = sys.getrecursionlimit()
    sys.setrecursionlimit(10000)

    json_dir = "../hydra-haskell/src/gen-main/json"

    evaluator_term_namespaces = [
        hydra.core.Name("hydra.annotations"),
        hydra.core.Name("hydra.constants"),
        hydra.core.Name("hydra.decode.core"),
        hydra.core.Name("hydra.dependencies"),
        hydra.core.Name("hydra.encode.core"),
        hydra.core.Name("hydra.extract.core"),
        hydra.core.Name("hydra.lexical"),
        hydra.core.Name("hydra.rewriting"),
        hydra.core.Name("hydra.scoping"),
        hydra.core.Name("hydra.show.core"),
        hydra.core.Name("hydra.strip"),
        hydra.core.Name("hydra.variables"),
    ]

    term_mods = load_modules_from_json(json_dir, evaluator_term_namespaces)
    term_mods = strip_all_term_types(term_mods)

    sys.setrecursionlimit(old_limit)

    from hydra.packaging import DefinitionTerm
    from hydra.core import Binding
    bindings = {}
    for mod in term_mods:
        for d in mod.definitions:
            if isinstance(d, DefinitionTerm):
                td = d.value
                bindings[td.name] = Binding(td.name, td.term, td.type)

    return bindings


def _load_bootstrap_type_schemes() -> FrozenDict:
    from hydra.json.bootstrap import types_by_name
    from hydra.scoping import f_type_to_type_scheme

    result = {}
    for name, typ in types_by_name.items():
        result[name] = f_type_to_type_scheme(typ)
    return FrozenDict(result)


def build_test_graph() -> hydra.graph.Graph:
    import hydra.lexical
    from hydra.generation import bootstrap_graph

    bs_graph = bootstrap_graph()

    bootstrap_types = _load_bootstrap_type_schemes()

    from hydra.scoping import f_type_to_type_scheme
    test_types_dict = test_graph.test_types()

    all_schema_types = dict(bootstrap_types)
    for name, typ in test_types_dict.items():
        all_schema_types[name] = f_type_to_type_scheme(typ)
    schema_types = FrozenDict(all_schema_types)

    kernel_terms = _load_kernel_term_bindings()
    kernel_term_bindings = list(kernel_terms.values())

    test_terms_dict = test_graph.test_terms()
    data_bindings = [hydra.core.Binding(name=name, term=term, type=Nothing())
                     for name, term in test_terms_dict.items()]

    return hydra.lexical.elements_to_graph(
        bs_graph, schema_types, tuple(kernel_term_bindings + data_bindings))
