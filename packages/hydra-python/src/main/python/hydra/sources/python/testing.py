"""Python test code generation codec for pytest-based generation tests.

Mirror of packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Testing.hs.
"""

from hydra.core import Name
from hydra.dsl.python import Just, Nothing
from hydra.packaging import Module, ModuleName

import hydra.dsl.meta.lib.chars as Chars
import hydra.dsl.meta.lib.eithers as Eithers
import hydra.dsl.meta.lib.equality as Equality
import hydra.dsl.meta.lib.lists as Lists
import hydra.dsl.meta.lib.logic as Logic
import hydra.dsl.meta.lib.maybes as Maybes
import hydra.dsl.meta.lib.strings as Strings
from hydra.dsl.meta.phantoms import *  # noqa: F401,F403
import hydra.dsl.core as Core
import hydra.dsl.packaging as Packaging

from hydra.sources.python._kernel_refs import (
    constants_warning_auto_generated_file,
)


# ----------------------------------------------------------------------
# Namespaces
# ----------------------------------------------------------------------

NS = ModuleName("hydra.python.testing")

from hydra.sources.python._source_dsl import (

    KERNEL_TYPES_NAMESPACES,
    make_def,
    make_local,
    unqualified_dep,
)

# Mirror Haskell:
#   [SerializationSource.ns, Formatting.ns, Names.ns, TestUtils.ns, Constants.ns] L.++
#   (PySyntax.ns:KernelTypes.kernelTypesNamespaces)
DEPENDENCIES = [
    unqualified_dep(ModuleName("hydra.serialization")),
    unqualified_dep(ModuleName("hydra.formatting")),
    unqualified_dep(ModuleName("hydra.names")),
    unqualified_dep(ModuleName("hydra.test.utils")),
    unqualified_dep(ModuleName("hydra.constants")),
    unqualified_dep(ModuleName("hydra.python.syntax")),
] + KERNEL_TYPES_NAMESPACES


_PLACEHOLDER = Module(
    Just("Python test code generation codec for pytest-based generation tests"),
    NS,
    DEPENDENCIES,
    (),
)




_def = make_def(_PLACEHOLDER)
_local = make_local("hydra.python.testing")
# Frequently used type names
_TEST_GROUP = Name("hydra.testing.TestGroup")
_TEST_CASE = Name("hydra.testing.TestCase")
_TEST_CASE_WITH_METADATA = Name("hydra.testing.TestCaseWithMetadata")
_UNIVERSAL_TEST_CASE = Name("hydra.testing.UniversalTestCase")
_NAMESPACE = Name("hydra.packaging.ModuleName")


# ----------------------------------------------------------------------
# Definitions (alphabetical)
# ----------------------------------------------------------------------

def _build_python_test_module():
    """Build the complete Python test module content."""
    body = lets(
        [
            field("groupName_",
                project(_TEST_GROUP, Name("name"))(var("testGroup")),
            ),
            field("header",
                Strings.cat(list_([
                    Strings.cat2(string("# "), constants_warning_auto_generated_file),
                    string("\n"),
                    Strings.cat2(string("# "), var("groupName_")),
                    string("\n\n"),
                ])),
            ),
        ],
        Strings.cat(list_([
            var("header"),
            var("testBody"),
            string("\n"),
        ])),
    )
    return _def(
        "buildPythonTestModule",
        doc(
            "Build the complete Python test module content",
            lambdas(["testModule", "testGroup", "testBody"], body),
        ),
    )


def _format_python_test_name():
    """Format a test name for Python (snake_case with test_ prefix)."""
    body = Strings.cat2(
        string("test_"),
        Strings.from_list(Lists.map(
            lam(
                "c",
                Logic.if_else(
                    Chars.is_alpha_num(var("c")),
                    Chars.to_lower(var("c")),
                    int32(95),
                ),
            ),
            Strings.to_list(var("name")),
        )),
    )
    return _def(
        "formatPythonTestName",
        doc(
            "Format a test name for Python (snake_case with test_ prefix)",
            lambdas(["name"], body),
        ),
    )


def _generate_python_test_case():
    """Generate a single pytest test case from a test case with metadata."""
    universal_branch_body = lets(
        [
            field("actual_",
                project(_UNIVERSAL_TEST_CASE, Name("actual"))(var("ucase"))(unit()),
            ),
            field("expected_",
                project(_UNIVERSAL_TEST_CASE, Name("expected"))(var("ucase"))(unit()),
            ),
            field("fullName",
                Logic.if_else(
                    Lists.null(var("groupPath")),
                    var("name_"),
                    Strings.intercalate(
                        string("__"),
                        Lists.concat2(
                            var("groupPath"),
                            list_([var("name_")]),
                        ),
                    ),
                ),
            ),
            field("formattedName",
                _local("formatPythonTestName")(var("fullName")),
            ),
        ],
        right(list_([
            Strings.cat(list_([
                string("def "),
                var("formattedName"),
                string("():"),
            ])),
            Strings.cat(list_([
                string("    assert ("),
                var("actual_"),
                string(") == ("),
                var("expected_"),
                string(")"),
            ])),
        ])),
    )
    body = lets(
        [
            field("name_",
                project(_TEST_CASE_WITH_METADATA, Name("name"))(var("tcm")),
            ),
            field("tcase",
                project(_TEST_CASE_WITH_METADATA, Name("case"))(var("tcm")),
            ),
        ],
        cases(
            _TEST_CASE,
            var("tcase"),
            Nothing(),
            [
                field("universal",
                    lam("ucase", universal_branch_body),
                ),
            ],
        ),
    )
    return _def(
        "generatePythonTestCase",
        doc(
            "Generate a single pytest test case from a test case with metadata",
            lambdas(["groupPath", "tcm"], body),
        ),
    )


def _generate_python_test_file():
    """Generate a Python test file for a test group."""
    return _def(
        "generatePythonTestFile",
        doc(
            "Generate a Python test file for a test group",
            lambdas(
                ["testModule", "testGroup", "_g"],
                _local("generateTestFileWithPythonCodec")(var("testModule"), var("testGroup")),
            ),
        ),
    )


def _generate_python_test_group_hierarchy():
    """Generate test hierarchy for Python with nested subgroups."""
    # Inner: the lets after the two <<~ binds.
    inner_body = lets(
        [
            field("testCasesStr",
                Strings.intercalate(
                    string("\n\n"),
                    Lists.concat(var("testCaseLines")),
                ),
            ),
            field("subgroupsStr",
                Strings.intercalate(string("\n\n"), var("subgroupBlocks")),
            ),
        ],
        right(Strings.cat(list_([
            var("testCasesStr"),
            Logic.if_else(
                Logic.or_(
                    Equality.equal(var("testCasesStr"), string("")),
                    Equality.equal(var("subgroupsStr"), string("")),
                ),
                string(""),
                string("\n\n"),
            ),
            var("subgroupsStr"),
        ]))),
    )
    # Subgroups bind body — Haskell: "subgroupBlocks" <<~ Eithers.mapList ... $ inner
    # Translates to: Eithers.bind(<map_list>, lam("subgroupBlocks", inner))
    subgroups_bind_body = Eithers.bind(
        Eithers.map_list(
            lam(
                "subgroup",
                lets(
                    [
                        field("groupName",
                            project(_TEST_GROUP, Name("name"))(var("subgroup")),
                        ),
                        field("header",
                            Strings.cat2(string("# "), var("groupName")),
                        ),
                    ],
                    Eithers.map_(
                        lam(
                            "content",
                            Strings.cat(list_([
                                var("header"),
                                string("\n\n"),
                                var("content"),
                            ])),
                        ),
                        _local("generatePythonTestGroupHierarchy")(Lists.concat2(
                                var("groupPath"),
                                list_([var("groupName")]),
                            ), var("subgroup")),
                    ),
                ),
            ),
            var("subgroups"),
        ),
        lam("subgroupBlocks", inner_body),
    )
    # testCaseLines bind body
    test_case_lines_bind_body = Eithers.bind(
        Eithers.map_list(
            lam(
                "tc",
                _local("generatePythonTestCase")(var("groupPath"), var("tc")),
            ),
            var("cases_"),
        ),
        lam("testCaseLines", subgroups_bind_body),
    )
    body = lets(
        [
            field("cases_",
                project(_TEST_GROUP, Name("cases"))(var("testGroup")),
            ),
            field("subgroups",
                project(_TEST_GROUP, Name("subgroups"))(var("testGroup")),
            ),
        ],
        test_case_lines_bind_body,
    )
    return _def(
        "generatePythonTestGroupHierarchy",
        doc(
            "Generate test hierarchy for Python with nested subgroups",
            lambdas(["groupPath", "testGroup"], body),
        ),
    )


def _generate_test_file_with_python_codec():
    """Generate a complete test file for Python."""
    test_body_body = lets(
        [
            field("testModuleContent",
                _local("buildPythonTestModule")(var("testModule"), var("testGroup"), var("testBody")),
            ),
            field("ns_",
                Packaging.module_name(var("testModule")),
            ),
            field("parts",
                Strings.split_on(
                    string("."),
                    unwrap(_NAMESPACE)(var("ns_")),
                ),
            ),
            field("dirParts",
                Maybes.from_maybe(
                    list_([]),
                    Lists.maybe_init(var("parts")),
                ),
            ),
            field("fileName",
                Strings.cat(list_([
                    string("test_"),
                    Maybes.from_maybe(
                        string(""),
                        Lists.maybe_last(var("parts")),
                    ),
                    string(".py"),
                ])),
            ),
            field("filePath",
                Strings.cat(list_([
                    Strings.intercalate(string("/"), var("dirParts")),
                    string("/"),
                    var("fileName"),
                ])),
            ),
        ],
        pair(var("filePath"), var("testModuleContent")),
    )
    body = Eithers.map_(
        lam("testBody", test_body_body),
        _local("generatePythonTestGroupHierarchy")(list_([]), var("testGroup")),
    )
    return _def(
        "generateTestFileWithPythonCodec",
        doc(
            "Generate a complete test file for Python",
            lambdas(["testModule", "testGroup"], body),
        ),
    )


# ----------------------------------------------------------------------
# Module assembly (alphabetical, matching Haskell)
# ----------------------------------------------------------------------

def _build_module() -> Module:
    return Module(
        _PLACEHOLDER.description,
        _PLACEHOLDER.name,
        _PLACEHOLDER.dependencies,
        (
            to_definition(_build_python_test_module()),
            to_definition(_format_python_test_name()),
            to_definition(_generate_python_test_case()),
            to_definition(_generate_python_test_file()),
            to_definition(_generate_python_test_group_hierarchy()),
            to_definition(_generate_test_file_with_python_codec()),
        ),
    )


module_ = _build_module()
