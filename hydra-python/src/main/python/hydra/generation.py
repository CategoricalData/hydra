"""I/O wrapper for Hydra code generation in Python.

Provides file I/O around the pure Either-based functions in hydra.code_generation.
This is the Python equivalent of Haskell's Hydra.Generation module.
"""

import json
import os
import sys
from functools import lru_cache

# The generated JSON decoder uses recursive variant matching which can
# exceed Python's default recursion limit for deeply nested types.
sys.setrecursionlimit(10000)

from hydra.annotations import is_native_type
from hydra.code_generation import (
    generate_source_files,
    namespace_to_path,
    strip_module_type_schemes,
)
from hydra.context import Context, InContext
from hydra.core import Binding
from hydra.dsl.python import FrozenDict, Just, Left, Nothing, Right
from hydra.graph import Graph
from hydra.json import model as JsonModel
from hydra.module import Module, Namespace
from hydra.rewriting import deannotate_type_recursive, f_type_to_type_scheme, remove_types_from_term
from hydra.sources.libraries import standard_library


@lru_cache(1)
def bootstrap_schema_map():
    """Build a schema map from the bootstrap type map.

    This mirrors Java's Generation.bootstrapSchemaMap(): reads the hard-coded
    type map from hydra.json.bootstrap.types_by_name (generated from the Haskell
    DSL), strips forall/annotation wrappers, and returns a Map[Name, Type]
    suitable for the JSON decoder.

    The bootstrap type map contains types from the kernel modules needed to
    decode Module from JSON: hydra.context, hydra.core,
    hydra.error, hydra.graph, hydra.module, and hydra.util.
    """
    from hydra.json.bootstrap import types_by_name

    result = {}
    for name, typ in types_by_name().items():
        ts = f_type_to_type_scheme(typ)
        result[name] = deannotate_type_recursive(ts.type)
    return FrozenDict(result)


def bootstrap_graph():
    """Create an empty graph with standard primitives (the bootstrap graph)."""
    primitives = standard_library()
    return Graph(
        bound_terms=FrozenDict({}),
        bound_types=FrozenDict({}),
        class_constraints=FrozenDict({}),
        lambda_variables=frozenset(),
        metadata=FrozenDict({}),
        primitives=FrozenDict(primitives),
        schema_types=FrozenDict({}),
        type_variables=frozenset(),
    )


def empty_context():
    """Create an empty Context."""
    return Context(trace=(), messages=(), other=FrozenDict({}))


def unwrap_either(result):
    """Unwrap an Either value, raising on Left."""
    match result:
        case Left(value=err):
            # err may be an InContext wrapping an Error
            if hasattr(err, 'object'):
                try:
                    from hydra.show.error import error
                    raise RuntimeError(f"Error: {error(err.object)}")
                except ImportError:
                    raise RuntimeError(f"Error: {err.object}")
            raise RuntimeError(f"Error: {err}")
        case Right(value=v):
            return v
        case _:
            raise RuntimeError(f"Unexpected result type: {type(result)}")


def _python_to_hydra_json(obj):
    """Convert a Python JSON object (from json.loads) to a Hydra JSON value."""
    if obj is None:
        return JsonModel.ValueNull()
    elif isinstance(obj, bool):
        return JsonModel.ValueBoolean(obj)
    elif isinstance(obj, (int, float)):
        return JsonModel.ValueNumber(float(obj))
    elif isinstance(obj, str):
        return JsonModel.ValueString(obj)
    elif isinstance(obj, list):
        return JsonModel.ValueArray(tuple(_python_to_hydra_json(item) for item in obj))
    elif isinstance(obj, dict):
        return JsonModel.ValueObject(FrozenDict(
            {k: _python_to_hydra_json(v) for k, v in obj.items()}))
    else:
        raise ValueError(f"Unexpected JSON type: {type(obj)}")


def parse_json_file(path):
    """Read a JSON file, parse to hydra.json.model.Value.

    Uses Python's built-in json module for performance and to avoid
    recursion depth issues with the generated parser on large files.
    """
    with open(path, "r", encoding="utf-8") as f:
        obj = json.load(f)
    return _python_to_hydra_json(obj)


def decode_module(bs_graph, schema_map, do_strip_type_schemes, json_val):
    """Decode a single module from a JSON value.

    Uses a pre-built schema map (from bootstrap_schema_map()) to decode the
    JSON into a Term, then decodes the Term into a Module.
    """
    import hydra.json.decode as json_decode
    import hydra.decode.module as decode_mod
    from hydra.core import Name, Type, TypeVariable

    mod_type = TypeVariable(Name("hydra.module.Module"))

    # Step 1: Decode JSON to a Term using the schema map
    json_result = json_decode.from_json(schema_map, Name("hydra.module.Module"), mod_type, json_val)
    match json_result:
        case Left(value=err):
            raise RuntimeError(f"Module JSON decode error: {err}")
        case Right(value=term):
            pass
        case _:
            raise RuntimeError("Unexpected JSON decode result type")

    # Step 2: Decode the Term to a Module
    mod_result = decode_mod.module(bs_graph, term)
    match mod_result:
        case Left(value=dec_err):
            raise RuntimeError(f"Module decode error: {dec_err.value}")
        case Right(value=mod):
            if do_strip_type_schemes:
                return strip_module_type_schemes(mod)
            return mod
        case _:
            raise RuntimeError("Unexpected module decode result type")


def load_modules_from_json(strip_type_schemes, base_path, namespaces):
    """Load modules from JSON files using the bootstrap schema map.

    Uses bootstrap_schema_map() (from hydra.json.bootstrap.types_by_name)
    to decode modules, matching Java's Generation.loadModulesFromJson().
    """
    bs_graph = bootstrap_graph()
    schema_map = bootstrap_schema_map()
    modules = []
    for ns in namespaces:
        file_path = os.path.join(base_path, namespace_to_path(ns) + ".json")
        json_val = parse_json_file(file_path)
        mod = decode_module(bs_graph, schema_map, strip_type_schemes, json_val)
        print(f"  Loaded: {ns.value}")
        modules.append(mod)
    return modules


def read_manifest_field(base_path, field_name):
    """Read a field from manifest.json as a list of Namespaces."""
    manifest_path = os.path.join(base_path, "manifest.json")
    with open(manifest_path, "r", encoding="utf-8") as f:
        manifest = json.load(f)
    return [Namespace(ns) for ns in manifest[field_name]]


def generate_sources(coder, language, do_infer, do_expand, do_hoist_case, do_hoist_poly,
                     base_path, universe, modules_to_generate):
    """Generate source files and write them to disk."""
    import time as _time
    bs_graph = bootstrap_graph()
    cx = empty_context()
    _t0 = _time.time()
    result = generate_source_files(
        coder, language,
        do_infer, do_expand, do_hoist_case, do_hoist_poly,
        bs_graph, tuple(universe), tuple(modules_to_generate), cx)
    files = unwrap_either(result)
    _t1 = _time.time()
    print(f"  Code generation took {_t1-_t0:.1f}s for {len(files)} files", flush=True)
    for path, content in files:
        file_path = os.path.join(base_path, path)
        if not content.endswith("\n"):
            content = content + "\n"
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, "w") as f:
            f.write(content)


def strip_term_types(m):
    """Strip System F type annotations from all term bodies in a module.

    Uses remove_types_from_term which strips TypeApplication, TypeLambda,
    lambda domain types, and let-binding TypeSchemes from terms.
    Module-level TypeSchemes are stripped from term bindings (to avoid
    bigfloat/float64 conflicts) but preserved on type-defining bindings
    (needed by is_native_type for schema graph construction).
    """
    stripped = []
    for b in m.elements:
        new_term = remove_types_from_term(b.term)
        new_type = b.type if is_native_type(b) else Nothing()
        stripped.append(Binding(b.name, new_term, new_type))
    return Module(m.namespace, tuple(stripped), m.type_dependencies, m.term_dependencies, m.description)


def strip_all_term_types(modules):
    """Strip System F type annotations from all modules."""
    return [strip_term_types(m) for m in modules]


def filter_kernel_modules(modules):
    """Filter modules to only kernel modules (exclude hydra.ext.* namespaces)."""
    return [m for m in modules if not m.namespace.value.startswith("hydra.ext.") and not m.namespace.value.startswith("hydra.json.yaml.")]


def filter_type_modules(modules):
    """Filter modules to only those containing type-defining bindings."""
    return [m for m in modules if any(is_native_type(b) for b in m.elements)]


def write_java(base_path, universe, mods):
    """Generate Java source files from modules."""
    from hydra.ext.java.coder import module_to_java
    from hydra.ext.java.language import java_language
    generate_sources(
        module_to_java, java_language(),
        False, True, False, True,
        base_path, universe, mods)


def write_python(base_path, universe, mods):
    """Generate Python source files from modules."""
    from hydra.ext.python.coder import module_to_python
    from hydra.ext.python.language import python_language
    generate_sources(
        module_to_python, python_language(),
        False, True, True, False,
        base_path, universe, mods)


def write_haskell(base_path, universe, mods):
    """Generate Haskell source files from modules."""
    from hydra.ext.haskell.coder import module_to_haskell
    from hydra.ext.haskell.language import haskell_language
    generate_sources(
        module_to_haskell, haskell_language(),
        False, False, False, False,
        base_path, universe, mods)
