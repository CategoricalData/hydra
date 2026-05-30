"""I/O wrapper for Hydra code generation in Python.

Provides file I/O around the pure Either-based functions in hydra.codegen.
This is the Python equivalent of Haskell's Hydra.Generation module.
"""

import json
import os
import sys
from decimal import Decimal
from functools import lru_cache

# The generated JSON decoder uses recursive variant matching which can
# exceed Python's default recursion limit for deeply nested types.
sys.setrecursionlimit(10000)

from hydra.annotations import is_native_type
from hydra.codegen import (
    generate_source_files,
    module_name_to_path,
)
from hydra.typing import InferenceContext
from hydra.core import Binding
from hydra.dsl.python import FrozenDict, Just, Left, Nothing, Right
from hydra.graph import Graph
from hydra.json import model as JsonModel
from hydra.packaging import Module, ModuleName
from hydra.strip import deannotate_type_recursive, remove_types_from_term
from hydra.scoping import f_type_to_type_scheme
from hydra.sources.libraries import standard_library


@lru_cache(1)
def bootstrap_schema_map():
    """Build a schema map from the bootstrap type map.

    This mirrors Java's Generation.bootstrapSchemaMap(): reads the hard-coded
    type map from hydra.json.bootstrap.types_by_name (generated from the Haskell
    DSL), strips forall/annotation wrappers, and returns a Map[Name, Type]
    suitable for the JSON decoder.

    The bootstrap type map contains types from the kernel modules needed to
    decode Module from JSON: hydra.core, hydra.error,
    hydra.graph, hydra.module, hydra.typing, and hydra.util.
    """
    from hydra.json.bootstrap import types_by_name

    result = {}
    for name, typ in types_by_name.items():
        ts = f_type_to_type_scheme(typ)
        result[name] = deannotate_type_recursive(ts.body)
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
    """Create an empty InferenceContext."""
    return InferenceContext(fresh_type_variable_count=0, trace=())


def unwrap_either(result):
    """Unwrap an Either value, raising on Left."""
    match result:
        case Left(value=err):
            try:
                from hydra.show.error import error
                raise RuntimeError(f"Error: {error(err)}")
            except ImportError:
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
        return JsonModel.ValueNumber(Decimal(str(obj)))
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


def decode_module(bs_graph, schema_map, json_val):
    """Decode a single module from a JSON value.

    Uses a pre-built schema map (from bootstrap_schema_map()) to decode the
    JSON into a Term, then decodes the Term into a Module.
    """
    import hydra.json.decode as json_decode
    import hydra.decode.packaging as decode_pkg
    from hydra.core import Name, Type, TypeVariable

    mod_type = TypeVariable(Name("hydra.packaging.Module"))

    # Step 1: Decode JSON to a Term using the schema map
    json_result = json_decode.from_json(schema_map, Name("hydra.packaging.Module"), mod_type, json_val)
    match json_result:
        case Left(value=err):
            raise RuntimeError(f"Module JSON decode error: {err}")
        case Right(value=term):
            pass
        case _:
            raise RuntimeError("Unexpected JSON decode result type")

    # Step 2: Decode the Term to a Module
    mod_result = decode_pkg.module(bs_graph, term)
    match mod_result:
        case Left(value=dec_err):
            raise RuntimeError(f"Module decode error: {dec_err.value}")
        case Right(value=mod):
            return mod
        case _:
            raise RuntimeError("Unexpected module decode result type")


def load_modules_from_json(base_path, namespaces):
    """Load modules from JSON files using the bootstrap schema map.

    Uses bootstrap_schema_map() (from hydra.json.bootstrap.types_by_name)
    to decode modules, matching Java's Generation.loadModulesFromJson().
    """
    bs_graph = bootstrap_graph()
    schema_map = bootstrap_schema_map()
    modules = []
    for ns in namespaces:
        file_path = os.path.join(base_path, module_name_to_path(ns) + ".json")
        json_val = parse_json_file(file_path)
        mod = decode_module(bs_graph, schema_map, json_val)
        print(f"  Loaded: {ns.value}")
        modules.append(mod)
    return modules


def read_manifest_field(base_path, field_name):
    """Read a field from manifest.json as a list of module names."""
    manifest_path = os.path.join(base_path, "manifest.json")
    with open(manifest_path, "r", encoding="utf-8") as f:
        manifest = json.load(f)
    return [ModuleName(ns) for ns in manifest[field_name]]


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
    type conflicts) but preserved on type-defining bindings
    (needed by is_native_type for schema graph construction).
    """
    from hydra.packaging import TermDefinition, DefinitionTerm, DefinitionType
    stripped = []
    for d in m.definitions:
        if isinstance(d, DefinitionTerm):
            td = d.value
            new_term = remove_types_from_term(td.term)
            stripped.append(DefinitionTerm(TermDefinition(td.name, new_term, Nothing())))
        else:
            stripped.append(d)
    return Module(m.description, m.name, m.dependencies, tuple(stripped))


def strip_all_term_types(modules):
    """Strip System F type annotations from all modules."""
    return [strip_term_types(m) for m in modules]


def filter_kernel_modules(modules):
    """Filter modules to only kernel modules (exclude hydra.* namespaces)."""
    return [m for m in modules if not m.name.value.startswith("hydra.") and not m.name.value.startswith("hydra.json.yaml.")]


def filter_type_modules(modules):
    """Filter modules to only those containing type-defining bindings."""
    from hydra.packaging import DefinitionType
    return [m for m in modules if any(
        isinstance(d, DefinitionType) for d in m.definitions)]


def write_java(base_path, universe, mods):
    """Generate Java source files from modules."""
    from hydra.java.coder import module_to_java
    from hydra.java.language import java_language
    generate_sources(
        module_to_java, java_language(),
        False, True, False, True,
        base_path, universe, mods)


def write_python(base_path, universe, mods):
    """Generate Python source files from modules."""
    from hydra.python.coder import module_to_python
    from hydra.python.language import python_language
    generate_sources(
        module_to_python, python_language(),
        False, True, True, False,
        base_path, universe, mods)


def write_haskell(base_path, universe, mods):
    """Generate Haskell source files from modules."""
    from hydra.haskell.coder import module_to_haskell
    from hydra.haskell.language import haskell_language
    generate_sources(
        module_to_haskell, haskell_language(),
        False, False, False, False,
        base_path, universe, mods)


def write_scala(base_path, universe, mods):
    """Generate Scala source files from modules."""
    from hydra.scala.coder import module_to_scala
    from hydra.scala.language import scala_language
    generate_sources(
        module_to_scala, scala_language(),
        False, True, False, False,
        base_path, universe, mods)


def write_typescript(base_path, universe, mods):
    """Generate TypeScript source files from modules.

    do_hoist_case_statements=True mirrors the per-target dispatch in
    heads/typescript/.../bootstrap.ts. Hoisting pulls cases out of inline
    IIFEs into top-level helpers, saving stack frames when the TS runtime
    walks deeply-nested terms (e.g. when TS hosts the Java coder).
    """
    from hydra.type_script.coder import module_to_type_script
    from hydra.type_script.language import type_script_language
    generate_sources(
        module_to_type_script, type_script_language(),
        False, True, True, False,
        base_path, universe, mods)


def write_lisp_dialect(base_path, dialect_name, ext, universe, mods):
    """Generate source files for a Lisp dialect (Clojure, Scheme, Common Lisp, or Emacs Lisp)."""
    from hydra.lisp.coder import module_to_lisp
    from hydra.lisp.language import lisp_language
    from hydra.lisp.serde import program_to_expr
    from hydra.lisp.syntax import Dialect
    from hydra.serialization import print_expr, parenthesize
    from hydra.names import module_name_to_file_path
    from hydra.packaging import FileExtension, ModuleName
    from hydra.util import CaseConvention

    dialect_map = {
        "clojure": Dialect.CLOJURE,
        "scheme": Dialect.SCHEME,
        "common_lisp": Dialect.COMMON_LISP,
        "emacs_lisp": Dialect.EMACS_LISP,
    }
    dialect = dialect_map[dialect_name]

    case_conv = CaseConvention.CAMEL if dialect_name == "clojure" else CaseConvention.LOWER_SNAKE

    def lisp_coder(mod, defs, cx, g):
        result = module_to_lisp(dialect, mod, defs, cx, g)
        match result:
            case Left():
                return result
            case Right(value=program):
                code = print_expr(parenthesize(program_to_expr(program)))
                file_path = module_name_to_file_path(case_conv, FileExtension(ext), mod.name)
                return Right(FrozenDict({file_path: code}))

    generate_sources(
        lisp_coder, lisp_language(),
        False, False, False, False,
        base_path, universe, mods)


# Prefix-to-package table. Order matters: more-specific prefixes first.
# Any namespace not matching any prefix falls through to "hydra-kernel".
# Mirrors Hydra.PackageRouting.packagePrefixes on the Haskell side.
_PACKAGE_PREFIXES = [
    # Coder packages (main runtime modules)
    ("hydra.haskell.",              "hydra-haskell"),
    ("hydra.java.",                 "hydra-java"),
    ("hydra.python.",               "hydra-python"),
    ("hydra.scala.",                "hydra-scala"),
    ("hydra.lisp.",                 "hydra-lisp"),
    ("hydra.coq.",                  "hydra-coq"),
    ("hydra.typeScript.",           "hydra-typescript"),
    ("hydra.go.",                   "hydra-go"),
    # DSL wrapper modules for coder packages
    ("hydra.dsl.haskell.",          "hydra-haskell"),
    ("hydra.dsl.java.",             "hydra-java"),
    ("hydra.dsl.python.",           "hydra-python"),
    ("hydra.dsl.scala.",            "hydra-scala"),
    ("hydra.dsl.lisp.",             "hydra-lisp"),
    ("hydra.dsl.coq.",              "hydra-coq"),
    ("hydra.dsl.typeScript.",       "hydra-typescript"),
    ("hydra.dsl.go.",               "hydra-go"),
    # Synthesized decoder source modules for coder packages
    ("hydra.sources.decode.haskell.",    "hydra-haskell"),
    ("hydra.sources.decode.java.",       "hydra-java"),
    ("hydra.sources.decode.python.",     "hydra-python"),
    ("hydra.sources.decode.scala.",      "hydra-scala"),
    ("hydra.sources.decode.lisp.",       "hydra-lisp"),
    ("hydra.sources.decode.coq.",        "hydra-coq"),
    ("hydra.sources.decode.typeScript.", "hydra-typescript"),
    # Synthesized encoder source modules for coder packages
    ("hydra.sources.encode.haskell.",    "hydra-haskell"),
    ("hydra.sources.encode.java.",       "hydra-java"),
    ("hydra.sources.encode.python.",     "hydra-python"),
    ("hydra.sources.encode.scala.",      "hydra-scala"),
    ("hydra.sources.encode.lisp.",       "hydra-lisp"),
    ("hydra.sources.encode.coq.",        "hydra-coq"),
    ("hydra.sources.encode.typeScript.", "hydra-typescript"),
    # Property graph package
    ("hydra.pg.",                   "hydra-pg"),
    ("hydra.cypher.",               "hydra-pg"),
    ("hydra.graphviz.",             "hydra-pg"),
    ("hydra.tinkerpop.",            "hydra-pg"),
    ("hydra.error.pg",              "hydra-pg"),
    ("hydra.show.error.pg",         "hydra-pg"),
    ("hydra.validate.pg",           "hydra-pg"),
    ("hydra.decode.pg.",            "hydra-pg"),
    ("hydra.encode.pg.",            "hydra-pg"),
    ("hydra.sources.decode.pg.",    "hydra-pg"),
    ("hydra.sources.encode.pg.",    "hydra-pg"),
    ("hydra.demos.genpg.",          "hydra-pg"),
    ("openGql.grammar",             "hydra-pg"),
    ("com.gdblab.pathAlgebra.",     "hydra-pg"),
    ("hydra.dsl.pg.",               "hydra-pg"),
    ("hydra.dsl.cypher.",           "hydra-pg"),
    ("hydra.dsl.graphviz.",         "hydra-pg"),
    ("hydra.dsl.tinkerpop.",        "hydra-pg"),
    ("hydra.dsl.error.pg",          "hydra-pg"),
    ("hydra.dsl.openGql.",          "hydra-pg"),
    ("hydra.dsl.com.gdblab.pathAlgebra.", "hydra-pg"),
    # RDF / OWL / SHACL / ShEx / XML schema package
    ("hydra.rdf.",                  "hydra-rdf"),
    ("hydra.owl.",                  "hydra-rdf"),
    ("hydra.shacl.",                "hydra-rdf"),
    ("hydra.shex.",                 "hydra-rdf"),
    ("hydra.xml.schema",            "hydra-rdf"),
    ("hydra.dsl.rdf.",              "hydra-rdf"),
    ("hydra.dsl.owl.",              "hydra-rdf"),
    ("hydra.dsl.shacl.",            "hydra-rdf"),
    ("hydra.dsl.shex.",             "hydra-rdf"),
    ("hydra.dsl.xml.schema",        "hydra-rdf"),
    # WebAssembly package
    ("hydra.wasm.",                 "hydra-wasm"),
    ("hydra.dsl.wasm.",             "hydra-wasm"),
    # Benchmark package
    ("hydra.bench.",                "hydra-bench"),
    # Extension package (truly-ext coders: Avro, Protobuf, GraphQL, etc.)
    ("hydra.atlas",                 "hydra-ext"),
    ("hydra.avro.",                 "hydra-ext"),
    ("hydra.azure.",                "hydra-ext"),
    ("hydra.cpp.",                  "hydra-ext"),
    ("hydra.csharp.",               "hydra-ext"),
    ("hydra.datalog.",              "hydra-ext"),
    ("hydra.delta.",                "hydra-ext"),
    ("hydra.geojson.",              "hydra-ext"),
    ("hydra.graphql.",              "hydra-ext"),
    ("hydra.iana.",                 "hydra-ext"),
    ("hydra.json.schema",           "hydra-ext"),
    ("hydra.kusto.",                "hydra-ext"),
    ("hydra.osv.",                  "hydra-ext"),
    ("hydra.parquet.",              "hydra-ext"),
    ("hydra.pegasus.",              "hydra-ext"),
    ("hydra.protobuf.",             "hydra-ext"),
    ("hydra.rust.",                 "hydra-ext"),
    ("hydra.sql.",                  "hydra-ext"),
    ("hydra.stac.",                 "hydra-ext"),
    ("hydra.typeScript.",           "hydra-ext"),
    ("hydra.workflow",              "hydra-ext"),
    ("hydra.dsl.atlas",             "hydra-ext"),
    ("hydra.dsl.avro.",             "hydra-ext"),
    ("hydra.dsl.azure.",            "hydra-ext"),
    ("hydra.dsl.cpp.",              "hydra-ext"),
    ("hydra.dsl.csharp.",           "hydra-ext"),
    ("hydra.dsl.datalog.",          "hydra-ext"),
    ("hydra.dsl.delta.",            "hydra-ext"),
    ("hydra.dsl.geojson.",          "hydra-ext"),
    ("hydra.dsl.graphql.",          "hydra-ext"),
    ("hydra.dsl.iana.",             "hydra-ext"),
    ("hydra.dsl.json.schema",       "hydra-ext"),
    ("hydra.dsl.kusto.",            "hydra-ext"),
    ("hydra.dsl.osv.",              "hydra-ext"),
    ("hydra.dsl.parquet.",          "hydra-ext"),
    ("hydra.dsl.pegasus.",          "hydra-ext"),
    ("hydra.dsl.protobuf.",         "hydra-ext"),
    ("hydra.dsl.rust.",             "hydra-ext"),
    ("hydra.dsl.sql.",              "hydra-ext"),
    ("hydra.dsl.stac.",             "hydra-ext"),
    ("hydra.dsl.typeScript.",       "hydra-ext"),
    ("hydra.dsl.workflow",          "hydra-ext"),
    # hydra.yaml.model lives in hydra-kernel; route ext-owned yaml modules explicitly.
    ("hydra.yaml.coder",            "hydra-ext"),
    ("hydra.yaml.language",         "hydra-ext"),
    ("hydra.yaml.serde",            "hydra-ext"),
]


def namespace_to_package(module_name):
    """Map a ModuleName to its owning package name.

    Mirrors Hydra.PackageRouting.namespaceToPackage. Falls back to
    "hydra-kernel" if no prefix matches.
    """
    ns = module_name.value if hasattr(module_name, "value") else module_name
    for prefix, pkg in _PACKAGE_PREFIXES:
        if ns.startswith(prefix):
            return pkg
    return "hydra-kernel"


def group_by_package(mods):
    """Partition a list of modules by owning package.

    Returns a list of (package_name, modules) pairs sorted by package name
    for deterministic output. Mirrors Hydra.PackageRouting.groupByPackage.
    """
    groups = {}
    for m in mods:
        pkg = namespace_to_package(m.name)
        groups.setdefault(pkg, []).append(m)
    return sorted(groups.items(), key=lambda kv: kv[0])


def load_package_deps(hydra_root, pkg):
    """Read a package's declared dependencies from packages/<pkg>/package.json.

    Returns the list of "dependencies" values, or [] if the field is absent
    or the file can't be read. Mirrors Hydra.Generation.loadPackageDeps.
    """
    path = os.path.join(hydra_root, "packages", pkg, "package.json")
    if not os.path.isfile(path):
        return []
    try:
        with open(path, "r", encoding="utf-8") as f:
            obj = json.load(f)
    except (OSError, ValueError):
        return []
    deps = obj.get("dependencies")
    if not isinstance(deps, list):
        return []
    return [d for d in deps if isinstance(d, str)]


def infer_and_write_by_package(
        hydra_root, dist_json_root, universe_mods, mods, seed_acc=()):
    """Per-package iterative inference + JSON write driver.

    Mirrors Hydra.Generation.inferAndWriteByPackage. Processes packages in
    dependency order (topo sort over each package.json's "dependencies"
    field) and runs codegen.infer_modules_given once per package, threading
    the typed-so-far output of upstream packages through as the universe.

    Each iteration writes its package's JSON to disk immediately. That side
    effect forces the inferred modules through serialization, which on the
    Haskell side defeats lazy-thunk retention; on Python it's a no-op for
    laziness but preserves the same control-flow shape so the two drivers
    can evolve together.

    Parameters:
      hydra_root      Worktree root; used to locate packages/<pkg>/package.json.
      dist_json_root  Output JSON root, e.g. <root>/dist/json. Per-package
                      outputs are written to <root>/dist/json/<pkg>/src/main/json/.
      universe_mods   All modules participating in type resolution
                      (kernel + sources). Grouped + iterated.
      mods            Subset that should actually be re-inferred + written.
                      Other packages in universe_mods seed the typed
                      accumulator without producing output.
      seed_acc        Pre-typed modules to seed the accumulator with (e.g.
                      kernel modules whose schemes are already in place from
                      JSON load). Excluded from grouping/iteration.

    Returns the full set of inferred target modules concatenated across
    packages, in topo order.
    """
    from hydra import codegen
    from hydra import sorting as Sorting
    from hydra.dsl.python import FrozenDict, Left, Right

    seed_ns = {m.name.value for m in seed_acc}
    grouping_universe = [m for m in universe_mods if m.name.value not in seed_ns]
    grouping_targets  = [m for m in mods          if m.name.value not in seed_ns]

    target_groups   = group_by_package(grouping_targets)
    universe_groups = group_by_package(grouping_universe)
    pkg_to_mods     = dict(target_groups)
    pkg_to_universe = dict(universe_groups)

    seen = set()
    pkgs_in_scope = []
    for pkg, _ in universe_groups:
        if pkg not in seen:
            seen.add(pkg); pkgs_in_scope.append(pkg)
    for pkg, _ in target_groups:
        if pkg not in seen:
            seen.add(pkg); pkgs_in_scope.append(pkg)

    pkg_deps = []
    for p in pkgs_in_scope:
        deps = load_package_deps(hydra_root, p)
        in_scope = [d for d in deps if d in pkgs_in_scope]
        pkg_deps.append((p, tuple(in_scope)))

    topo_result = Sorting.topological_sort(tuple(pkg_deps))
    match topo_result:
        case Right(value=ordered):
            ordered = list(ordered)
        case Left(value=cycles):
            raise RuntimeError(
                f"infer_and_write_by_package: package dep graph has cycles: {list(cycles)}")
        case _:
            raise RuntimeError(
                f"infer_and_write_by_package: unexpected topo_result {topo_result!r}")

    print(f"  Per-package inference: {len(ordered)} packages in dep order: "
          f"{' -> '.join(ordered)}", flush=True)

    ctx = InferenceContext(fresh_type_variable_count=0, trace=())
    bs_graph = bootstrap_graph()
    acc = list(seed_acc)
    inferred_all = []
    for pkg in ordered:
        pkg_targets  = pkg_to_mods.get(pkg, [])
        pkg_universe = pkg_to_universe.get(pkg, [])
        target_ns    = {m.name.value for m in pkg_targets}
        infer_targets = pkg_targets if pkg_targets else pkg_universe
        typed_universe = acc + pkg_universe
        print(f"  [{pkg}] {len(pkg_targets)} write / "
              f"{len(infer_targets)} infer / {len(acc)} typed-so-far",
              flush=True)
        if not infer_targets:
            continue
        result = codegen.infer_modules_given(
            ctx, bs_graph, tuple(typed_universe), tuple(infer_targets))
        match result:
            case Right(value=inferred):
                inferred = list(inferred)
            case Left(value=err):
                raise RuntimeError(f"infer_and_write_by_package: inference failed for {pkg}: {err}")
            case _:
                raise RuntimeError(
                    f"infer_and_write_by_package: unexpected inference result {result!r}")
        to_write = [m for m in inferred if m.name.value in target_ns]
        if to_write:
            _write_package_split_json(dist_json_root, typed_universe, inferred, to_write)
        acc = acc + inferred
        inferred_all.extend(inferred)
    return inferred_all


def _write_package_split_json(dist_json_root, universe_mods, universe_for_schema, to_write):
    """Encode a set of inferred modules to JSON and write them under
    dist_json_root/<pkg>/src/main/json/. Mirrors
    Hydra.Generation.writePackageSplitJson.
    """
    from hydra import codegen
    graph = codegen.modules_to_graph(
        bootstrap_graph(),
        tuple(universe_mods) + tuple(universe_for_schema),
        tuple(universe_mods))
    schema_map = codegen.build_schema_map(graph)
    for pkg, pkg_mods in group_by_package(to_write):
        pkg_dir = os.path.join(dist_json_root, pkg, "src", "main", "json")
        print(f"  {pkg}: {len(pkg_mods)} modules -> {pkg_dir}", flush=True)
        for m in pkg_mods:
            result = codegen.module_to_json(schema_map, m)
            match result:
                case Right(value=json_str):
                    file_path = os.path.join(pkg_dir, module_name_to_path(m.name) + ".json")
                    os.makedirs(os.path.dirname(file_path), exist_ok=True)
                    new_content = json_str + "\n"
                    if os.path.exists(file_path):
                        with open(file_path, "r", encoding="utf-8") as f:
                            old = f.read()
                        if old == new_content:
                            continue
                    with open(file_path, "w", encoding="utf-8") as f:
                        f.write(new_content)
                case Left(value=err):
                    raise RuntimeError(
                        f"_write_package_split_json: encode failed for "
                        f"{m.name.value}: {err}")
                case _:
                    raise RuntimeError(
                        f"_write_package_split_json: unexpected encode result {result!r}")
