"""Environment types for Python code generation.

Mirror of packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Environment.hs.
"""

from hydra.core import Name, Type, TypeScheme
from hydra.dsl.python import Just, Nothing
from hydra.packaging import (
    DefinitionType,
    Module,
    ModuleName,
    TypeDefinition,
)

from hydra.sources.python._source_dsl import unqualified_dep
import hydra.dsl.annotations as Annotations
import hydra.dsl.types as T


NS = ModuleName("hydra.python.environment")

DEPENDENCIES = [
    unqualified_dep(ModuleName("hydra.python.syntax")),
    unqualified_dep(ModuleName("hydra.util")),
    unqualified_dep(ModuleName("hydra.core")),
    unqualified_dep(ModuleName("hydra.graph")),
    unqualified_dep(ModuleName("hydra.packaging")),
    unqualified_dep(ModuleName("hydra.typing")),
]


def _typeref(ns: ModuleName, local: str) -> Type:
    """Construct a TypeVariable reference: <ns>.<local>."""
    return T.variable(f"{ns.value}.{local}")


def _env(local: str) -> Type:
    return _typeref(NS, local)


def _syntax(local: str) -> Type:
    return _typeref(ModuleName("hydra.python.syntax"), local)


def _core(local: str) -> Type:
    return _typeref(ModuleName("hydra.core"), local)


def _graph(local: str) -> Type:
    return _typeref(ModuleName("hydra.graph"), local)


def _util(local: str) -> Type:
    return _typeref(ModuleName("hydra.util"), local)


def _def(local_name: str, typ: Type) -> DefinitionType:
    """Build a DefinitionType for a named type definition."""
    name = Name(f"{NS.value}.{local_name}")
    ts = TypeScheme((), typ, Nothing())
    return DefinitionType(TypeDefinition(name, ts))


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
# (matches Haskell)

def _build_module() -> Module:
    return Module(
        NS,
        Just("Environment types for Python code generation"),
        DEPENDENCIES,
        (
            _python_version(),
            _python_environment(),
            _python_module_metadata(),
            _py_graph(),
        ),
    )


module_ = _build_module()
