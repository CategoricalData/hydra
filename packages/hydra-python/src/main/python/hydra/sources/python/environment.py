"""Environment types for Python code generation.

Host-native DSL source (authoritative; the former Haskell copy was removed in #346).
"""

import sys

from hydra.core import Type
from hydra.overlay.python.dsl.python import Given, None_
from hydra.overlay.python.dsl.meta.defs import check_complete
from hydra.packaging import (EntityMetadata,
    Module,
    ModuleName,
)

from hydra.sources.python._source_dsl import make_type_def, type_ref, unqualified_dep
import hydra.overlay.python.dsl.annotations as Annotations
import hydra.overlay.python.dsl.types as T


NS = ModuleName("hydra.python.environment")

DEPENDENCIES = [
    unqualified_dep(ModuleName("hydra.python.syntax")),
    unqualified_dep(ModuleName("hydra.util")),
    unqualified_dep(ModuleName("hydra.core")),
    unqualified_dep(ModuleName("hydra.graph")),
    unqualified_dep(ModuleName("hydra.packaging")),
    unqualified_dep(ModuleName("hydra.typing")),
]

_def = make_type_def(NS)


def _env(local: str) -> Type:
    return type_ref(NS, local)


def _syntax(local: str) -> Type:
    return type_ref(ModuleName("hydra.python.syntax"), local)


def _core(local: str) -> Type:
    return type_ref(ModuleName("hydra.core"), local)


def _graph(local: str) -> Type:
    return type_ref(ModuleName("hydra.graph"), local)


def _util(local: str) -> Type:
    return type_ref(ModuleName("hydra.util"), local)


# ----------------------------------------------------------------------
# Type definitions (alphabetical)
# ----------------------------------------------------------------------

def _py_graph():
    return _def(
        "PyGraph",
        Annotations.doc(
            "Combined graph and metadata state for Python code generation",
            T.record([
                T.field("graph", Annotations.doc("The Hydra graph being processed", _graph("Graph"))),
                T.field("metadata", Annotations.doc("Accumulated module metadata", _env("PythonModuleMetadata"))),
            ]),
        ),
    )


def _python_environment():
    return _def(
        "PythonEnvironment",
        Annotations.doc(
            "Environment for Python code generation",
            T.record([
                T.field(
                    "namespaces",
                    Annotations.doc(
                        "ModuleName mapping for imports",
                        T.apply(_util("ModuleNames"), _syntax("DottedName")),
                    ),
                ),
                T.field(
                    "boundTypeVariables",
                    Annotations.doc(
                        "Type variables in scope, with their Python names",
                        T.pair(T.list_(_core("Name")), T.map_(_core("Name"), _syntax("Name"))),
                    ),
                ),
                T.field("graph", Annotations.doc("Graph context for type inference", _graph("Graph"))),
                T.field(
                    "nullaryBindings",
                    Annotations.doc("Set of nullary bindings (need call syntax)", T.set_(_core("Name"))),
                ),
                T.field("version", Annotations.doc("Target Python version", _env("PythonVersion"))),
                T.field(
                    "skipCasts",
                    Annotations.doc(
                        "When True, skip generating cast() calls for reduced memory usage",
                        T.boolean(),
                    ),
                ),
                T.field(
                    "inlineVariables",
                    Annotations.doc(
                        "Variables that are inline let bindings (walrus operators)",
                        T.set_(_core("Name")),
                    ),
                ),
            ]),
        ),
    )


def _python_module_metadata():
    return _def(
        "PythonModuleMetadata",
        Annotations.doc(
            "Temporary metadata used to create the header section of a Python file",
            T.record([
                T.field(
                    "namespaces",
                    Annotations.doc(
                        "ModuleName mapping for imports",
                        T.apply(_util("ModuleNames"), _syntax("DottedName")),
                    ),
                ),
                T.field(
                    "typeVariables",
                    Annotations.doc("Type variables used in the module", T.set_(_core("Name"))),
                ),
                T.field("usesAnnotated", T.boolean()),
                T.field("usesCallable", T.boolean()),
                T.field("usesCast", T.boolean()),
                T.field("usesLruCache", T.boolean()),
                T.field("usesTypeAlias", T.boolean()),
                T.field("usesDataclass", T.boolean()),
                T.field("usesDecimal", T.boolean()),
                T.field("usesEither", T.boolean()),
                T.field("usesEnum", T.boolean()),
                T.field("usesFrozenDict", T.boolean()),
                T.field("usesFrozenList", T.boolean()),
                T.field("usesFrozenSet", T.boolean()),
                T.field("usesGeneric", T.boolean()),
                T.field("usesJust", T.boolean()),
                T.field("usesLeft", T.boolean()),
                T.field("usesMaybe", T.boolean()),
                T.field("usesName", T.boolean()),
                T.field("usesNode", T.boolean()),
                T.field("usesNothing", T.boolean()),
                T.field("usesRight", T.boolean()),
                T.field("usesTypeVar", T.boolean()),
            ]),
        ),
    )


def _python_version():
    return _def(
        "PythonVersion",
        Annotations.doc(
            "Target Python version for code generation",
            T.enum(["python310", "python312"]),
        ),
    )


# ----------------------------------------------------------------------
# Module assembly
# ----------------------------------------------------------------------

# Order in source: pythonVersion, pythonEnvironment, pythonModuleMetadata, pyGraph
# (explicit non-alphabetical order: dependencies precede dependents)

def _build_module() -> Module:
    return Module(
        NS,
        Given(EntityMetadata(
            Given("Environment types for Python code generation"),
            (),
            (),
            None_())),
        DEPENDENCIES,
        (
            _python_version(),
            _python_environment(),
            _python_module_metadata(),
            _py_graph(),
        ),
    )


module_ = _build_module()
check_complete(sys.modules[__name__], module_.definitions)
