"""Python test code generation codec for pytest-based generation tests.

Mirror of packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Testing.hs.
"""

from hydra.core import Name
from hydra.dsl.python import Just, Nothing
from hydra.packaging import Module, Namespace

import hydra.dsl.meta.lib.chars as Chars
import hydra.dsl.meta.lib.eithers as Eithers
import hydra.dsl.meta.lib.equality as Equality
import hydra.dsl.meta.lib.lists as Lists
import hydra.dsl.meta.lib.logic as Logic
import hydra.dsl.meta.lib.maybes as Maybes
import hydra.dsl.meta.lib.strings as Strings
import hydra.dsl.meta.phantoms as Phantoms
import hydra.dsl.core as Core
import hydra.dsl.packaging as Packaging

from hydra.sources.python._kernel_refs import (
    constants_warning_auto_generated_file,
)


# ----------------------------------------------------------------------
# Namespaces
# ----------------------------------------------------------------------

NS = Namespace("hydra.python.testing")

KERNEL_TYPES_NAMESPACES = [
    Namespace(n) for n in [
        "hydra.paths", "hydra.ast", "hydra.classes", "hydra.coders",
        "hydra.context", "hydra.core", "hydra.error.checking", "hydra.error.core",
        "hydra.error.packaging", "hydra.errors", "hydra.graph", "hydra.json.model",
        "hydra.packaging", "hydra.parsing", "hydra.phantoms", "hydra.query",
        "hydra.relational", "hydra.tabular", "hydra.testing", "hydra.topology",
        "hydra.typing", "hydra.util", "hydra.validation", "hydra.variants",
    ]
]

# Mirror Haskell:
#   [SerializationSource.ns, Formatting.ns, Names.ns, TestUtils.ns, Constants.ns] L.++
#   (PySyntax.ns:KernelTypes.kernelTypesNamespaces)
DEPENDENCIES = [
    Namespace("hydra.serialization"),
    Namespace("hydra.formatting"),
    Namespace("hydra.names"),
    Namespace("hydra.test.utils"),
    Namespace("hydra.constants"),
    Namespace("hydra.python.syntax"),
] + KERNEL_TYPES_NAMESPACES


_PLACEHOLDER = Module(
    Just("Python test code generation codec for pytest-based generation tests"),
    NS,
    DEPENDENCIES,
    (),
)


def _def(local_name, term):
    return Phantoms.definition_in_module(_PLACEHOLDER, local_name, term)


def _local(local_name: str):
    return Phantoms.var(f"hydra.python.testing.{local_name}")


def _ap(fun, *args):
    out = fun
    for a in args:
        out = Phantoms.apply(out, a)
    return out


# Frequently used type names
_TEST_GROUP = Name("hydra.testing.TestGroup")
_TEST_CASE = Name("hydra.testing.TestCase")
_TEST_CASE_WITH_METADATA = Name("hydra.testing.TestCaseWithMetadata")
_UNIVERSAL_TEST_CASE = Name("hydra.testing.UniversalTestCase")
_NAMESPACE = Name("hydra.packaging.Namespace")


# ----------------------------------------------------------------------
# Definitions (alphabetical)
# ----------------------------------------------------------------------

def _build_python_test_module():
    """Build the complete Python test module content."""
    body = Phantoms.lets(
        [
            Phantoms.field(
                Name("groupName_"),
                _ap(Phantoms.project(_TEST_GROUP, Name("name")), Phantoms.var("testGroup")),
            ),
            Phantoms.field(
                Name("header"),
                Strings.cat(Phantoms.list_([
                    Strings.cat2(Phantoms.string("# "), constants_warning_auto_generated_file),
                    Phantoms.string("\n"),
                    Strings.cat2(Phantoms.string("# "), Phantoms.var("groupName_")),
                    Phantoms.string("\n\n"),
                ])),
            ),
        ],
        Strings.cat(Phantoms.list_([
            Phantoms.var("header"),
            Phantoms.var("testBody"),
            Phantoms.string("\n"),
        ])),
    )
    return _def(
        "buildPythonTestModule",
        Phantoms.doc(
            "Build the complete Python test module content",
            Phantoms.lambdas(["testModule", "testGroup", "testBody"], body),
        ),
    )


def _format_python_test_name():
    """Format a test name for Python (snake_case with test_ prefix)."""
    body = Strings.cat2(
        Phantoms.string("test_"),
        Strings.from_list(Lists.map(
            Phantoms.lam(
                "c",
                Logic.if_else(
                    Chars.is_alpha_num(Phantoms.var("c")),
                    Chars.to_lower(Phantoms.var("c")),
                    Phantoms.int32(95),
                ),
            ),
            Strings.to_list(Phantoms.var("name")),
        )),
    )
    return _def(
        "formatPythonTestName",
        Phantoms.doc(
            "Format a test name for Python (snake_case with test_ prefix)",
            Phantoms.lambdas(["name"], body),
        ),
    )


def _generate_python_test_case():
    """Generate a single pytest test case from a test case with metadata."""
    universal_branch_body = Phantoms.lets(
        [
            Phantoms.field(
                Name("actual_"),
                _ap(Phantoms.project(_UNIVERSAL_TEST_CASE, Name("actual")), Phantoms.var("ucase")),
            ),
            Phantoms.field(
                Name("expected_"),
                _ap(Phantoms.project(_UNIVERSAL_TEST_CASE, Name("expected")), Phantoms.var("ucase")),
            ),
            Phantoms.field(
                Name("fullName"),
                Logic.if_else(
                    Lists.null(Phantoms.var("groupPath")),
                    Phantoms.var("name_"),
                    Strings.intercalate(
                        Phantoms.string("__"),
                        Lists.concat2(
                            Phantoms.var("groupPath"),
                            Phantoms.list_([Phantoms.var("name_")]),
                        ),
                    ),
                ),
            ),
            Phantoms.field(
                Name("formattedName"),
                _ap(_local("formatPythonTestName"), Phantoms.var("fullName")),
            ),
        ],
        Phantoms.right(Phantoms.list_([
            Strings.cat(Phantoms.list_([
                Phantoms.string("def "),
                Phantoms.var("formattedName"),
                Phantoms.string("():"),
            ])),
            Strings.cat(Phantoms.list_([
                Phantoms.string("    assert ("),
                Phantoms.var("actual_"),
                Phantoms.string(") == ("),
                Phantoms.var("expected_"),
                Phantoms.string(")"),
            ])),
        ])),
    )
    body = Phantoms.lets(
        [
            Phantoms.field(
                Name("name_"),
                _ap(Phantoms.project(_TEST_CASE_WITH_METADATA, Name("name")), Phantoms.var("tcm")),
            ),
            Phantoms.field(
                Name("tcase"),
                _ap(Phantoms.project(_TEST_CASE_WITH_METADATA, Name("case")), Phantoms.var("tcm")),
            ),
        ],
        Phantoms.cases(
            _TEST_CASE,
            Phantoms.var("tcase"),
            Nothing(),
            [
                Phantoms.field(
                    Name("universal"),
                    Phantoms.lam("ucase", universal_branch_body),
                ),
            ],
        ),
    )
    return _def(
        "generatePythonTestCase",
        Phantoms.doc(
            "Generate a single pytest test case from a test case with metadata",
            Phantoms.lambdas(["groupPath", "tcm"], body),
        ),
    )


def _generate_python_test_file():
    """Generate a Python test file for a test group."""
    return _def(
        "generatePythonTestFile",
        Phantoms.doc(
            "Generate a Python test file for a test group",
            Phantoms.lambdas(
                ["testModule", "testGroup", "_g"],
                _ap(
                    _local("generateTestFileWithPythonCodec"),
                    Phantoms.var("testModule"),
                    Phantoms.var("testGroup"),
                ),
            ),
        ),
    )


def _generate_python_test_group_hierarchy():
    """Generate test hierarchy for Python with nested subgroups."""
    # Inner: the lets after the two <<~ binds.
    inner_body = Phantoms.lets(
        [
            Phantoms.field(
                Name("testCasesStr"),
                Strings.intercalate(
                    Phantoms.string("\n\n"),
                    Lists.concat(Phantoms.var("testCaseLines")),
                ),
            ),
            Phantoms.field(
                Name("subgroupsStr"),
                Strings.intercalate(Phantoms.string("\n\n"), Phantoms.var("subgroupBlocks")),
            ),
        ],
        Phantoms.right(Strings.cat(Phantoms.list_([
            Phantoms.var("testCasesStr"),
            Logic.if_else(
                Logic.or_(
                    Equality.equal(Phantoms.var("testCasesStr"), Phantoms.string("")),
                    Equality.equal(Phantoms.var("subgroupsStr"), Phantoms.string("")),
                ),
                Phantoms.string(""),
                Phantoms.string("\n\n"),
            ),
            Phantoms.var("subgroupsStr"),
        ]))),
    )
    # Subgroups bind body — Haskell: "subgroupBlocks" <<~ Eithers.mapList ... $ inner
    # Translates to: Eithers.bind(<map_list>, lam("subgroupBlocks", inner))
    subgroups_bind_body = Eithers.bind(
        Eithers.map_list(
            Phantoms.lam(
                "subgroup",
                Phantoms.lets(
                    [
                        Phantoms.field(
                            Name("groupName"),
                            _ap(Phantoms.project(_TEST_GROUP, Name("name")), Phantoms.var("subgroup")),
                        ),
                        Phantoms.field(
                            Name("header"),
                            Strings.cat2(Phantoms.string("# "), Phantoms.var("groupName")),
                        ),
                    ],
                    Eithers.map_(
                        Phantoms.lam(
                            "content",
                            Strings.cat(Phantoms.list_([
                                Phantoms.var("header"),
                                Phantoms.string("\n\n"),
                                Phantoms.var("content"),
                            ])),
                        ),
                        _ap(
                            _local("generatePythonTestGroupHierarchy"),
                            Lists.concat2(
                                Phantoms.var("groupPath"),
                                Phantoms.list_([Phantoms.var("groupName")]),
                            ),
                            Phantoms.var("subgroup"),
                        ),
                    ),
                ),
            ),
            Phantoms.var("subgroups"),
        ),
        Phantoms.lam("subgroupBlocks", inner_body),
    )
    # testCaseLines bind body
    test_case_lines_bind_body = Eithers.bind(
        Eithers.map_list(
            Phantoms.lam(
                "tc",
                _ap(
                    _local("generatePythonTestCase"),
                    Phantoms.var("groupPath"),
                    Phantoms.var("tc"),
                ),
            ),
            Phantoms.var("cases_"),
        ),
        Phantoms.lam("testCaseLines", subgroups_bind_body),
    )
    body = Phantoms.lets(
        [
            Phantoms.field(
                Name("cases_"),
                _ap(Phantoms.project(_TEST_GROUP, Name("cases")), Phantoms.var("testGroup")),
            ),
            Phantoms.field(
                Name("subgroups"),
                _ap(Phantoms.project(_TEST_GROUP, Name("subgroups")), Phantoms.var("testGroup")),
            ),
        ],
        test_case_lines_bind_body,
    )
    return _def(
        "generatePythonTestGroupHierarchy",
        Phantoms.doc(
            "Generate test hierarchy for Python with nested subgroups",
            Phantoms.lambdas(["groupPath", "testGroup"], body),
        ),
    )


def _generate_test_file_with_python_codec():
    """Generate a complete test file for Python."""
    test_body_body = Phantoms.lets(
        [
            Phantoms.field(
                Name("testModuleContent"),
                _ap(
                    _local("buildPythonTestModule"),
                    Phantoms.var("testModule"),
                    Phantoms.var("testGroup"),
                    Phantoms.var("testBody"),
                ),
            ),
            Phantoms.field(
                Name("ns_"),
                Packaging.module_namespace(Phantoms.var("testModule")),
            ),
            Phantoms.field(
                Name("parts"),
                Strings.split_on(
                    Phantoms.string("."),
                    _ap(Phantoms.unwrap(_NAMESPACE), Phantoms.var("ns_")),
                ),
            ),
            Phantoms.field(
                Name("dirParts"),
                Maybes.from_maybe(
                    Phantoms.list_([]),
                    Lists.maybe_init(Phantoms.var("parts")),
                ),
            ),
            Phantoms.field(
                Name("fileName"),
                Strings.cat(Phantoms.list_([
                    Phantoms.string("test_"),
                    Maybes.from_maybe(
                        Phantoms.string(""),
                        Lists.maybe_last(Phantoms.var("parts")),
                    ),
                    Phantoms.string(".py"),
                ])),
            ),
            Phantoms.field(
                Name("filePath"),
                Strings.cat(Phantoms.list_([
                    Strings.intercalate(Phantoms.string("/"), Phantoms.var("dirParts")),
                    Phantoms.string("/"),
                    Phantoms.var("fileName"),
                ])),
            ),
        ],
        Phantoms.pair(Phantoms.var("filePath"), Phantoms.var("testModuleContent")),
    )
    body = Eithers.map_(
        Phantoms.lam("testBody", test_body_body),
        _ap(
            _local("generatePythonTestGroupHierarchy"),
            Phantoms.list_([]),
            Phantoms.var("testGroup"),
        ),
    )
    return _def(
        "generateTestFileWithPythonCodec",
        Phantoms.doc(
            "Generate a complete test file for Python",
            Phantoms.lambdas(["testModule", "testGroup"], body),
        ),
    )


# ----------------------------------------------------------------------
# Module assembly (alphabetical, matching Haskell)
# ----------------------------------------------------------------------

def _build_module() -> Module:
    return Module(
        _PLACEHOLDER.description,
        _PLACEHOLDER.namespace,
        _PLACEHOLDER.dependencies,
        (
            Phantoms.to_definition(_build_python_test_module()),
            Phantoms.to_definition(_format_python_test_name()),
            Phantoms.to_definition(_generate_python_test_case()),
            Phantoms.to_definition(_generate_python_test_file()),
            Phantoms.to_definition(_generate_python_test_group_hierarchy()),
            Phantoms.to_definition(_generate_test_file_with_python_codec()),
        ),
    )


module_ = _build_module()
