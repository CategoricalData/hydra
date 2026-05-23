"""Python naming utilities: encoding Hydra names as Python names.

Mirror of packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Names.hs.
"""

from hydra.core import Name
from hydra.dsl.python import Just
from hydra.packaging import Module, ModuleName

import hydra.dsl.meta.lib.equality as Equality
import hydra.dsl.meta.lib.lists as Lists
import hydra.dsl.meta.lib.logic as Logic
import hydra.dsl.meta.lib.maps as Maps
import hydra.dsl.meta.lib.maybes as Maybes
import hydra.dsl.meta.lib.pairs as Pairs
import hydra.dsl.meta.lib.strings as Strings
from hydra.dsl.meta.phantoms import *  # noqa: F401,F403
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
    packaging_un_module_name,
    util_case_convention_camel,
    util_case_convention_lower_snake,
    util_case_convention_pascal,
    util_case_convention_upper_snake,
)


# ----------------------------------------------------------------------
# Namespaces and dependencies (mirroring Haskell)
# ----------------------------------------------------------------------

NS = ModuleName("hydra.python.names")

# moduleDependencies = [Names.ns, Formatting.ns, PySerde.ns, pyLanguageNs] L.++
#                       (PyEnvironmentSource.ns:PySyntax.ns:KernelTypes.kernelTypesNamespaces)
from hydra.sources.python._source_dsl import (

    KERNEL_TYPES_NAMESPACES,
    make_def,
    make_local,
    unqualified_dep,
)
DEPENDENCIES = [
    unqualified_dep(ModuleName("hydra.names")),
    unqualified_dep(ModuleName("hydra.formatting")),
    unqualified_dep(ModuleName("hydra.python.serde")),
    unqualified_dep(ModuleName("hydra.python.language")),
    unqualified_dep(ModuleName("hydra.python.environment")),
    unqualified_dep(ModuleName("hydra.python.syntax")),
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




_def = make_def(_PLACEHOLDER)
_local = make_local("hydra.python.names")
# Frequently used names — define once for reuse.
_PY_NAME = Name("hydra.python.syntax.Name")
_PY_DOTTED_NAME = Name("hydra.python.syntax.DottedName")
_PY_ENV = Name("hydra.python.environment.PythonEnvironment")


def _ref_python_reserved_words():
    return var("hydra.python.language.pythonReservedWords")


# ----------------------------------------------------------------------
# Definitions (alphabetical)
# ----------------------------------------------------------------------

def _encode_enum_value():
    """encodeName @@ false @@ caseConventionUpperSnake"""
    return _def(
        "encodeEnumValue",
        doc(
            "Encode a name as a Python enum value (UPPER_SNAKE case)",
            _local("encodeName")(false(), util_case_convention_upper_snake),
        ),
    )


def _encode_field_name():
    """lambdas [env, fname] $ encodeName @@ false @@ lowerSnake @@ env @@ fname"""
    return _def(
        "encodeFieldName",
        doc(
            "Encode a name as a Python field name (lower_snake case)",
            lambdas(
                ["env", "fname"],
                _local("encodeName")(false(), util_case_convention_lower_snake, var("env"), var("fname")),
            ),
        ),
    )


def _encode_name():
    """Encode a Hydra name as a Python name with the given case convention."""
    project_namespaces = apply(
        project(_PY_ENV, Name("namespaces")),
        var("env"),
    )
    project_bound_vars = Pairs.second(
        apply(
            project(_PY_ENV, Name("boundTypeVariables")),
            var("env"),
        )
    )
    body = lets(
        [
            field("namespaces", project_namespaces),
            field("focusPair", packaging_namespaces_focus(var("namespaces"))),
            field("focusNs", Pairs.first(var("focusPair"))),
            field("boundVars", project_bound_vars),
            field("qualName", names_qualify_name(var("name"))),
            field("mns", packaging_qualified_name_namespace(var("qualName"))),
            field("local", packaging_qualified_name_local(var("qualName"))),
            field("pyLocal",
                _local("sanitizePythonName")(formatting_convert_case(util_case_convention_camel, var("conv"), var("local"))),
            ),
            field("pyNs",
                lam(
                    "nsVal",
                    Strings.intercalate(
                        string("."),
                        Lists.map(
                            formatting_convert_case(util_case_convention_camel, util_case_convention_lower_snake),
                            Strings.split_on(
                                string("."),
                                packaging_un_module_name(var("nsVal")),
                            ),
                        ),
                    ),
                ),
            ),
        ],
        Logic.if_else(
            var("isQualified"),
            Maybes.maybe(
                # Not a bound type variable
                Logic.if_else(
                    Equality.equal(
                        var("mns"),
                        just(var("focusNs")),
                    ),
                    # Same namespace
                    wrap(
                        _PY_NAME,
                        Logic.if_else(
                            _local("useFutureAnnotations"),
                            var("pyLocal"),
                            var("hydra.python.serde.escapePythonString")(true(), var("pyLocal")),
                        ),
                    ),
                    # Different or no namespace
                    Maybes.maybe(
                        wrap(_PY_NAME, var("pyLocal")),
                        lam(
                            "nsVal",
                            wrap(
                                _PY_NAME,
                                Strings.cat2(
                                    apply(var("pyNs"), var("nsVal")),
                                    Strings.cat2(
                                        string("."),
                                        var("pyLocal"),
                                    ),
                                ),
                            ),
                        ),
                        var("mns"),
                    ),
                ),
                # Bound type variable
                lam("n", var("n")),
                Maps.lookup(var("name"), var("boundVars")),
            ),
            # Not qualified
            wrap(_PY_NAME, var("pyLocal")),
        ),
    )
    return _def(
        "encodeName",
        doc(
            "Encode a Hydra name as a Python name",
            lambdas(["isQualified", "conv", "env", "name"], body),
        ),
    )


def _encode_name_qualified():
    """Encode a name as a fully qualified Python name."""
    project_namespaces = apply(
        project(_PY_ENV, Name("namespaces")),
        var("env"),
    )
    project_bound_vars = Pairs.second(
        apply(
            project(_PY_ENV, Name("boundTypeVariables")),
            var("env"),
        )
    )
    body = lets(
        [
            field("namespaces", project_namespaces),
            field("focusPair", packaging_namespaces_focus(var("namespaces"))),
            field("focusNs", Pairs.first(var("focusPair"))),
            field("boundVars", project_bound_vars),
            field("qualName",
                names_qualify_name(var("name")),
            ),
            field("mns",
                packaging_qualified_name_namespace(var("qualName")),
            ),
            field("local",
                packaging_qualified_name_local(var("qualName")),
            ),
            field("pyNs",
                lam(
                    "nsVal",
                    Strings.intercalate(
                        string("."),
                        Lists.map(
                            formatting_convert_case(util_case_convention_camel, util_case_convention_lower_snake),
                            Strings.split_on(
                                string("."),
                                packaging_un_namespace(var("nsVal")),
                            ),
                        ),
                    ),
                ),
            ),
        ],
        Maybes.maybe(
            # Not a bound type variable
            Logic.if_else(
                Equality.equal(
                    var("mns"),
                    just(var("focusNs")),
                ),
                # Same namespace
                wrap(
                    _PY_NAME,
                    Logic.if_else(
                        _local("useFutureAnnotations"),
                        var("local"),
                        var("hydra.python.serde.escapePythonString")(true(), var("local")),
                    ),
                ),
                # Different namespace - snake-cased namespace + sanitized local
                Maybes.maybe(
                    wrap(_PY_NAME, _local("sanitizePythonName")(var("local"))),
                    lam(
                        "nsVal",
                        wrap(
                            _PY_NAME,
                            Strings.cat2(
                                apply(var("pyNs"), var("nsVal")),
                                Strings.cat2(
                                    string("."),
                                    _local("sanitizePythonName")(var("local")),
                                ),
                            ),
                        ),
                    ),
                    var("mns"),
                ),
            ),
            # Bound type variable
            lam("n", var("n")),
            Maps.lookup(var("name"), var("boundVars")),
        ),
    )
    return _def(
        "encodeNameQualified",
        doc(
            "Encode a name as a fully qualified Python name",
            lambdas(["env", "name"], body),
        ),
    )


def _encode_namespace():
    """Encode a namespace as a Python dotted name."""
    return _def(
        "encodeNamespace",
        doc(
            "Encode a namespace as a Python dotted name",
            lam(
                "nsVal",
                wrap(
                    _PY_DOTTED_NAME,
                    Lists.map(
                        lam(
                            "part",
                            wrap(
                                _PY_NAME,
                                formatting_convert_case(util_case_convention_camel, util_case_convention_lower_snake, var("part")),
                            ),
                        ),
                        Strings.split_on(
                            string("."),
                            packaging_un_module_name(var("nsVal")),
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
        doc(
            "Encode a type variable name (capitalized)",
            lam(
                "name",
                wrap(
                    _PY_NAME,
                    formatting_capitalize(Core.un_name(var("name"))),
                ),
            ),
        ),
    )


def _sanitize_python_name():
    """sanitizeWithUnderscores @@ pythonReservedWords"""
    return _def(
        "sanitizePythonName",
        doc(
            "Sanitize a string to be a valid Python name",
            formatting_sanitize_with_underscores(_ref_python_reserved_words()),
        ),
    )


def _term_variable_reference():
    """variableReference @@ caseConventionLowerSnake @@ false"""
    return _def(
        "termVariableReference",
        doc(
            "Reference a term variable as a Python expression",
            _local("variableReference")(util_case_convention_lower_snake, false()),
        ),
    )


def _type_variable_reference():
    """variableReference @@ caseConventionPascal @@ false"""
    return _def(
        "typeVariableReference",
        doc(
            "Reference a type variable as a Python expression",
            _local("variableReference")(util_case_convention_pascal, false()),
        ),
    )


def _encode_constant_for_field_name():
    """Generate a constant name for a field definition."""
    return _def(
        "encodeConstantForFieldName",
        doc(
            "Generate a constant name for a field definition",
            lambdas(
                ["env", "tname", "fname"],
                wrap(
                    _PY_NAME,
                    formatting_convert_case(util_case_convention_camel, util_case_convention_upper_snake, Core.un_name(var("fname"))),
                ),
            ),
        ),
    )


def _encode_constant_for_type_name():
    """Generate a constant name for a type definition (always TYPE_)."""
    return _def(
        "encodeConstantForTypeName",
        doc(
            "Generate a constant name for a type definition",
            lambdas(
                ["env", "tname"],
                wrap(_PY_NAME, string("TYPE_")),
            ),
        ),
    )


def _variable_reference():
    """Reference a variable as a Python expression with optional quoting."""
    project_namespaces = apply(
        project(_PY_ENV, Name("namespaces")),
        var("env"),
    )
    body = lets(
        [
            field("pyName",
                _local("encodeName")(true(), var("conv"), var("env"), var("name")),
            ),
            field("unquoted",
                PyDsl.py_name_to_py_expression(var("pyName")),
            ),
            field("namespaces", project_namespaces),
            field("focusPair", packaging_namespaces_focus(var("namespaces"))),
            field("focusNs", Pairs.first(var("focusPair"))),
            field("mns",
                names_namespace_of(var("name")),
            ),
            field("sameNamespace",
                Maybes.maybe(
                    false(),
                    lam(
                        "ns",
                        Equality.equal(var("ns"), var("focusNs")),
                    ),
                    var("mns"),
                ),
            ),
        ],
        Logic.if_else(
            Logic.and_(var("quoted"), var("sameNamespace")),
            PyDsl.py_string_to_py_expression(
                PyDsl.double_quoted_string(
                    apply(
                        unwrap(_PY_NAME),
                        var("pyName"),
                    ),
                ),
            ),
            var("unquoted"),
        ),
    )
    return _def(
        "variableReference",
        doc(
            "Reference a variable as a Python expression",
            lambdas(["conv", "quoted", "env", "name"], body),
        ),
    )


def _variant_name():
    """Generate a variant name by combining type name and field name."""
    return _def(
        "variantName",
        doc(
            "Generate a variant name from type name and field name",
            lambdas(
                ["isQualified", "env", "tname", "fname"],
                _local("encodeName")(var("isQualified"), util_case_convention_pascal, var("env"), wrap("hydra.core.Name",
                        Strings.cat2(
                            Core.un_name(var("tname")),
                            formatting_capitalize(Core.un_name(var("fname"))),
                        ),
                    )),
            ),
        ),
    )


def _use_future_annotations():
    """Whether to use __future__ annotations for forward references."""
    return _def(
        "useFutureAnnotations",
        doc(
            "Whether to use __future__ annotations for forward references",
            true(),
        ),
    )


# ----------------------------------------------------------------------
# Module assembly
# ----------------------------------------------------------------------


def _build_module() -> Module:
    defs = (
        to_definition(_encode_constant_for_field_name()),
        to_definition(_encode_constant_for_type_name()),
        to_definition(_encode_enum_value()),
        to_definition(_encode_field_name()),
        to_definition(_encode_name()),
        to_definition(_encode_name_qualified()),
        to_definition(_encode_namespace()),
        to_definition(_encode_type_variable()),
        to_definition(_sanitize_python_name()),
        to_definition(_term_variable_reference()),
        to_definition(_type_variable_reference()),
        to_definition(_use_future_annotations()),
        to_definition(_variable_reference()),
        to_definition(_variant_name()),
    )
    return Module(
        _PLACEHOLDER.description,
        _PLACEHOLDER.name,
        _PLACEHOLDER.dependencies,
        defs,
    )


module_ = _build_module()
