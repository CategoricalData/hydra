"""Python naming utilities: encoding Hydra names as Python names.

Mirror of packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Names.hs.
"""

from hydra.core import Name
from hydra.dsl.python import Just
from hydra.packaging import Module, Namespace

import hydra.dsl.meta.lib.equality as Equality
import hydra.dsl.meta.lib.lists as Lists
import hydra.dsl.meta.lib.logic as Logic
import hydra.dsl.meta.lib.maps as Maps
import hydra.dsl.meta.lib.maybes as Maybes
import hydra.dsl.meta.lib.pairs as Pairs
import hydra.dsl.meta.lib.strings as Strings
import hydra.dsl.meta.phantoms as Phantoms
import hydra.dsl.core as Core

from hydra.sources.python import _python_helpers as PyDsl
from hydra.sources.python._kernel_refs import (
    formatting_capitalize,
    formatting_convert_case,
    formatting_sanitize_with_underscores,
    names_namespace_of,
    names_qualify_name,
    packaging_namespaces_focus,
    packaging_qualified_name_local,
    packaging_qualified_name_namespace,
    packaging_un_namespace,
    util_case_convention_camel,
    util_case_convention_lower_snake,
    util_case_convention_pascal,
    util_case_convention_upper_snake,
)


# ----------------------------------------------------------------------
# Namespaces and dependencies (mirroring Haskell)
# ----------------------------------------------------------------------

NS = Namespace("hydra.python.names")

# moduleDependencies = [Names.ns, Formatting.ns, PySerde.ns, pyLanguageNs] L.++
#                       (PyEnvironmentSource.ns:PySyntax.ns:KernelTypes.kernelTypesNamespaces)
KERNEL_TYPES_NAMESPACES = [
    Namespace(n) for n in [
        "hydra.paths", "hydra.ast", "hydra.classes", "hydra.coders",
        "hydra.context", "hydra.core", "hydra.error.checking", "hydra.error.core",
        "hydra.error.packaging", "hydra.errors", "hydra.graph", "hydra.json.model",
        "hydra.packaging", "hydra.parsing", "hydra.phantoms", "hydra.query",
        "hydra.relational", "hydra.tabular", "hydra.testing", "hydra.topology",
        "hydra.typing", "hydra.util", "hydra.variants",
    ]
]
DEPENDENCIES = [
    Namespace("hydra.names"),
    Namespace("hydra.formatting"),
    Namespace("hydra.python.serde"),
    Namespace("hydra.python.language"),
    Namespace("hydra.python.environment"),
    Namespace("hydra.python.syntax"),
] + KERNEL_TYPES_NAMESPACES


# ----------------------------------------------------------------------
# Module skeleton + define helper
# ----------------------------------------------------------------------

_PLACEHOLDER = Module(
    Just("Python naming utilities: encoding Hydra names as Python names"),
    NS,
    DEPENDENCIES,
    (),
)


def _def(local_name, term):
    return Phantoms.definition_in_module(_PLACEHOLDER, local_name, term)


# Frequently used names — define once for reuse.
_PY_NAME = Name("hydra.python.syntax.Name")
_PY_DOTTED_NAME = Name("hydra.python.syntax.DottedName")
_PY_ENV = Name("hydra.python.environment.PythonEnvironment")


def _local(local_name: str):
    """Reference a definition in this module: var('hydra.python.names.<local_name>')."""
    return Phantoms.var(f"hydra.python.names.{local_name}")


def _ref_python_reserved_words():
    return Phantoms.var("hydra.python.language.pythonReservedWords")


def _ap(fun, *args):
    """Left-associated application: _ap(f, a, b, c) == apply(apply(apply(f, a), b), c)."""
    out = fun
    for a in args:
        out = Phantoms.apply(out, a)
    return out


# ----------------------------------------------------------------------
# Definitions (alphabetical)
# ----------------------------------------------------------------------

def _encode_enum_value():
    """encodeName @@ false @@ caseConventionUpperSnake"""
    return _def(
        "encodeEnumValue",
        Phantoms.doc(
            "Encode a name as a Python enum value (UPPER_SNAKE case)",
            _ap(_local("encodeName"), Phantoms.false(), util_case_convention_upper_snake),
        ),
    )


def _encode_field_name():
    """lambdas [env, fname] $ encodeName @@ false @@ lowerSnake @@ env @@ fname"""
    return _def(
        "encodeFieldName",
        Phantoms.doc(
            "Encode a name as a Python field name (lower_snake case)",
            Phantoms.lambdas(
                ["env", "fname"],
                _ap(
                    _local("encodeName"),
                    Phantoms.false(),
                    util_case_convention_lower_snake,
                    Phantoms.var("env"),
                    Phantoms.var("fname"),
                ),
            ),
        ),
    )


def _encode_name():
    """Encode a Hydra name as a Python name with the given case convention."""
    project_namespaces = Phantoms.apply(
        Phantoms.project(_PY_ENV, Name("namespaces")),
        Phantoms.var("env"),
    )
    project_bound_vars = Pairs.second(
        Phantoms.apply(
            Phantoms.project(_PY_ENV, Name("boundTypeVariables")),
            Phantoms.var("env"),
        )
    )
    body = Phantoms.lets(
        [
            Phantoms.field(Name("namespaces"), project_namespaces),
            Phantoms.field(Name("focusPair"), packaging_namespaces_focus(Phantoms.var("namespaces"))),
            Phantoms.field(Name("focusNs"), Pairs.first(Phantoms.var("focusPair"))),
            Phantoms.field(Name("boundVars"), project_bound_vars),
            Phantoms.field(Name("qualName"), _ap(names_qualify_name, Phantoms.var("name"))),
            Phantoms.field(Name("mns"), packaging_qualified_name_namespace(Phantoms.var("qualName"))),
            Phantoms.field(Name("local"), packaging_qualified_name_local(Phantoms.var("qualName"))),
            Phantoms.field(
                Name("pyLocal"),
                _ap(
                    _local("sanitizePythonName"),
                    _ap(
                        formatting_convert_case,
                        util_case_convention_camel,
                        Phantoms.var("conv"),
                        Phantoms.var("local"),
                    ),
                ),
            ),
            Phantoms.field(
                Name("pyNs"),
                Phantoms.lam(
                    "nsVal",
                    Strings.intercalate(
                        Phantoms.string("."),
                        Lists.map(
                            _ap(
                                formatting_convert_case,
                                util_case_convention_camel,
                                util_case_convention_lower_snake,
                            ),
                            Strings.split_on(
                                Phantoms.string("."),
                                packaging_un_namespace(Phantoms.var("nsVal")),
                            ),
                        ),
                    ),
                ),
            ),
        ],
        Logic.if_else(
            Phantoms.var("isQualified"),
            Maybes.maybe(
                # Not a bound type variable
                Logic.if_else(
                    Equality.equal(
                        Phantoms.var("mns"),
                        Phantoms.just(Phantoms.var("focusNs")),
                    ),
                    # Same namespace
                    Phantoms.wrap(
                        _PY_NAME,
                        Logic.if_else(
                            _local("useFutureAnnotations"),
                            Phantoms.var("pyLocal"),
                            _ap(
                                Phantoms.var("hydra.python.serde.escapePythonString"),
                                Phantoms.true(),
                                Phantoms.var("pyLocal"),
                            ),
                        ),
                    ),
                    # Different or no namespace
                    Maybes.maybe(
                        Phantoms.wrap(_PY_NAME, Phantoms.var("pyLocal")),
                        Phantoms.lam(
                            "nsVal",
                            Phantoms.wrap(
                                _PY_NAME,
                                Strings.cat2(
                                    Phantoms.apply(Phantoms.var("pyNs"), Phantoms.var("nsVal")),
                                    Strings.cat2(
                                        Phantoms.string("."),
                                        Phantoms.var("pyLocal"),
                                    ),
                                ),
                            ),
                        ),
                        Phantoms.var("mns"),
                    ),
                ),
                # Bound type variable
                Phantoms.lam("n", Phantoms.var("n")),
                Maps.lookup(Phantoms.var("name"), Phantoms.var("boundVars")),
            ),
            # Not qualified
            Phantoms.wrap(_PY_NAME, Phantoms.var("pyLocal")),
        ),
    )
    return _def(
        "encodeName",
        Phantoms.doc(
            "Encode a Hydra name as a Python name",
            Phantoms.lambdas(["isQualified", "conv", "env", "name"], body),
        ),
    )


def _encode_name_qualified():
    """Encode a name as a fully qualified Python name."""
    project_namespaces = Phantoms.apply(
        Phantoms.project(_PY_ENV, Name("namespaces")),
        Phantoms.var("env"),
    )
    project_bound_vars = Pairs.second(
        Phantoms.apply(
            Phantoms.project(_PY_ENV, Name("boundTypeVariables")),
            Phantoms.var("env"),
        )
    )
    body = Phantoms.lets(
        [
            Phantoms.field(Name("namespaces"), project_namespaces),
            Phantoms.field(Name("focusPair"), packaging_namespaces_focus(Phantoms.var("namespaces"))),
            Phantoms.field(Name("focusNs"), Pairs.first(Phantoms.var("focusPair"))),
            Phantoms.field(Name("boundVars"), project_bound_vars),
            Phantoms.field(
                Name("qualName"),
                _ap(names_qualify_name, Phantoms.var("name")),
            ),
            Phantoms.field(
                Name("mns"),
                packaging_qualified_name_namespace(Phantoms.var("qualName")),
            ),
            Phantoms.field(
                Name("local"),
                packaging_qualified_name_local(Phantoms.var("qualName")),
            ),
        ],
        Maybes.maybe(
            # Not a bound type variable
            Logic.if_else(
                Equality.equal(
                    Phantoms.var("mns"),
                    Phantoms.just(Phantoms.var("focusNs")),
                ),
                # Same namespace
                Phantoms.wrap(
                    _PY_NAME,
                    Logic.if_else(
                        _local("useFutureAnnotations"),
                        Phantoms.var("local"),
                        _ap(
                            Phantoms.var("hydra.python.serde.escapePythonString"),
                            Phantoms.true(),
                            Phantoms.var("local"),
                        ),
                    ),
                ),
                # Different namespace - dotted with sanitization
                Phantoms.wrap(
                    _PY_NAME,
                    Strings.intercalate(
                        Phantoms.string("."),
                        Lists.map(
                            _local("sanitizePythonName"),
                            Strings.split_on(
                                Phantoms.string("."),
                                Core.un_name(Phantoms.var("name")),
                            ),
                        ),
                    ),
                ),
            ),
            # Bound type variable
            Phantoms.lam("n", Phantoms.var("n")),
            Maps.lookup(Phantoms.var("name"), Phantoms.var("boundVars")),
        ),
    )
    return _def(
        "encodeNameQualified",
        Phantoms.doc(
            "Encode a name as a fully qualified Python name",
            Phantoms.lambdas(["env", "name"], body),
        ),
    )


def _encode_namespace():
    """Encode a namespace as a Python dotted name."""
    return _def(
        "encodeNamespace",
        Phantoms.doc(
            "Encode a namespace as a Python dotted name",
            Phantoms.lam(
                "nsVal",
                Phantoms.wrap(
                    _PY_DOTTED_NAME,
                    Lists.map(
                        Phantoms.lam(
                            "part",
                            Phantoms.wrap(
                                _PY_NAME,
                                _ap(
                                    formatting_convert_case,
                                    util_case_convention_camel,
                                    util_case_convention_lower_snake,
                                    Phantoms.var("part"),
                                ),
                            ),
                        ),
                        Strings.split_on(
                            Phantoms.string("."),
                            packaging_un_namespace(Phantoms.var("nsVal")),
                        ),
                    ),
                ),
            ),
        ),
    )


def _encode_type_variable():
    """lambda name $ wrap PyName $ capitalize (unName name)"""
    return _def(
        "encodeTypeVariable",
        Phantoms.doc(
            "Encode a type variable name (capitalized)",
            Phantoms.lam(
                "name",
                Phantoms.wrap(
                    _PY_NAME,
                    _ap(formatting_capitalize, Core.un_name(Phantoms.var("name"))),
                ),
            ),
        ),
    )


def _sanitize_python_name():
    """sanitizeWithUnderscores @@ pythonReservedWords"""
    return _def(
        "sanitizePythonName",
        Phantoms.doc(
            "Sanitize a string to be a valid Python name",
            _ap(formatting_sanitize_with_underscores, _ref_python_reserved_words()),
        ),
    )


def _term_variable_reference():
    """variableReference @@ caseConventionLowerSnake @@ false"""
    return _def(
        "termVariableReference",
        Phantoms.doc(
            "Reference a term variable as a Python expression",
            _ap(_local("variableReference"), util_case_convention_lower_snake, Phantoms.false()),
        ),
    )


def _type_variable_reference():
    """variableReference @@ caseConventionPascal @@ false"""
    return _def(
        "typeVariableReference",
        Phantoms.doc(
            "Reference a type variable as a Python expression",
            _ap(_local("variableReference"), util_case_convention_pascal, Phantoms.false()),
        ),
    )


def _encode_constant_for_field_name():
    """Generate a constant name for a field definition."""
    return _def(
        "encodeConstantForFieldName",
        Phantoms.doc(
            "Generate a constant name for a field definition",
            Phantoms.lambdas(
                ["env", "tname", "fname"],
                Phantoms.wrap(
                    _PY_NAME,
                    _ap(
                        formatting_convert_case,
                        util_case_convention_camel,
                        util_case_convention_upper_snake,
                        Core.un_name(Phantoms.var("fname")),
                    ),
                ),
            ),
        ),
    )


def _encode_constant_for_type_name():
    """Generate a constant name for a type definition (always TYPE_)."""
    return _def(
        "encodeConstantForTypeName",
        Phantoms.doc(
            "Generate a constant name for a type definition",
            Phantoms.lambdas(
                ["env", "tname"],
                Phantoms.wrap(_PY_NAME, Phantoms.string("TYPE_")),
            ),
        ),
    )


def _variable_reference():
    """Reference a variable as a Python expression with optional quoting."""
    project_namespaces = Phantoms.apply(
        Phantoms.project(_PY_ENV, Name("namespaces")),
        Phantoms.var("env"),
    )
    body = Phantoms.lets(
        [
            Phantoms.field(
                Name("pyName"),
                _ap(
                    _local("encodeName"),
                    Phantoms.true(),
                    Phantoms.var("conv"),
                    Phantoms.var("env"),
                    Phantoms.var("name"),
                ),
            ),
            Phantoms.field(
                Name("unquoted"),
                PyDsl.py_name_to_py_expression(Phantoms.var("pyName")),
            ),
            Phantoms.field(Name("namespaces"), project_namespaces),
            Phantoms.field(Name("focusPair"), packaging_namespaces_focus(Phantoms.var("namespaces"))),
            Phantoms.field(Name("focusNs"), Pairs.first(Phantoms.var("focusPair"))),
            Phantoms.field(
                Name("mns"),
                _ap(names_namespace_of, Phantoms.var("name")),
            ),
            Phantoms.field(
                Name("sameNamespace"),
                Maybes.maybe(
                    Phantoms.false(),
                    Phantoms.lam(
                        "ns",
                        Equality.equal(Phantoms.var("ns"), Phantoms.var("focusNs")),
                    ),
                    Phantoms.var("mns"),
                ),
            ),
        ],
        Logic.if_else(
            Logic.and_(Phantoms.var("quoted"), Phantoms.var("sameNamespace")),
            PyDsl.py_string_to_py_expression(
                PyDsl.double_quoted_string(
                    Phantoms.apply(
                        Phantoms.unwrap(_PY_NAME),
                        Phantoms.var("pyName"),
                    ),
                ),
            ),
            Phantoms.var("unquoted"),
        ),
    )
    return _def(
        "variableReference",
        Phantoms.doc(
            "Reference a variable as a Python expression",
            Phantoms.lambdas(["conv", "quoted", "env", "name"], body),
        ),
    )


def _variant_name():
    """Generate a variant name by combining type name and field name."""
    return _def(
        "variantName",
        Phantoms.doc(
            "Generate a variant name from type name and field name",
            Phantoms.lambdas(
                ["isQualified", "env", "tname", "fname"],
                _ap(
                    _local("encodeName"),
                    Phantoms.var("isQualified"),
                    util_case_convention_pascal,
                    Phantoms.var("env"),
                    Phantoms.wrap(
                        Name("hydra.core.Name"),
                        Strings.cat2(
                            Core.un_name(Phantoms.var("tname")),
                            _ap(formatting_capitalize, Core.un_name(Phantoms.var("fname"))),
                        ),
                    ),
                ),
            ),
        ),
    )


def _use_future_annotations():
    """Whether to use __future__ annotations for forward references."""
    return _def(
        "useFutureAnnotations",
        Phantoms.doc(
            "Whether to use __future__ annotations for forward references",
            Phantoms.true(),
        ),
    )


# ----------------------------------------------------------------------
# Module assembly
# ----------------------------------------------------------------------


def _build_module() -> Module:
    defs = (
        Phantoms.to_definition(_encode_constant_for_field_name()),
        Phantoms.to_definition(_encode_constant_for_type_name()),
        Phantoms.to_definition(_encode_enum_value()),
        Phantoms.to_definition(_encode_field_name()),
        Phantoms.to_definition(_encode_name()),
        Phantoms.to_definition(_encode_name_qualified()),
        Phantoms.to_definition(_encode_namespace()),
        Phantoms.to_definition(_encode_type_variable()),
        Phantoms.to_definition(_sanitize_python_name()),
        Phantoms.to_definition(_term_variable_reference()),
        Phantoms.to_definition(_type_variable_reference()),
        Phantoms.to_definition(_use_future_annotations()),
        Phantoms.to_definition(_variable_reference()),
        Phantoms.to_definition(_variant_name()),
    )
    return Module(
        _PLACEHOLDER.description,
        _PLACEHOLDER.namespace,
        _PLACEHOLDER.dependencies,
        defs,
    )


module_ = _build_module()
