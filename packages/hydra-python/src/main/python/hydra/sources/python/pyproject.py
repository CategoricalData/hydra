"""The build configuration for a Python (PEP 621 / pyproject.toml) distribution package.

Host-native DSL source. The Python analog of hydra.gradle.GradleBuildConfiguration
(#511): it captures the binding-specific build configuration of a Python distribution
package beyond its generated sources, so that hand-written bindings folded into
overlay/python/<pkg>/ can declare their third-party PyPI dependencies. The JSON
encoding of a PyProjectBuildConfiguration value is the on-disk
overlay/python/<pkg>/build.json, read by the Python package-build generator.

Unlike hydra.gradle (Gradle spans multiple languages, so it is keyed by build system),
pyproject/PEP 621 is inherently Python-specific, so this lives under hydra.python.*.

The dependency vocabulary (PackageDependency / VersionSpecifier / DependencyScope) is
shared with hydra.gradle via hydra.packaging; only the build-system-specific shell
differs. For now a Python package's only overlay-specific build need is its extra
dependencies (runtime + test); requires-python, the build backend, and namespace-package
configuration are emitted uniformly by the generic per-package generator.
"""

import sys

from hydra.core import Type
from hydra.overlay.python.dsl.python import Given, None_
from hydra.overlay.python.dsl.meta.defs import check_complete
from hydra.packaging import (EntityMetadata,
    DefinitionType,
    Module,
    ModuleName,
)

from hydra.sources.python._source_dsl import make_type_def, type_ref, unqualified_dep
import hydra.overlay.python.dsl.types as T


NS = ModuleName("hydra.python.pyproject")
PACKAGING_NS = ModuleName("hydra.packaging")
DEPENDENCIES = [unqualified_dep(PACKAGING_NS)]
DESCRIPTION = "Build configuration for PEP 621 (pyproject.toml) Python distribution packages."

# The rich field docs live on the Java hydra.gradle analog; this module leaves the
# type structure undecorated (the Python type-codegen path does not tolerate a
# doc-annotated TypeScheme body).
_def = make_type_def(NS)


def _packaging(local: str) -> Type:
    """Reference to a type in hydra.packaging: hydra.packaging.<local>."""
    return type_ref(PACKAGING_NS, local)


def _pyProjectBuildConfiguration() -> DefinitionType:
    # dependencies: third-party (non-project) PyPI deps. Inter-package deps are
    # derived separately. Scope (api/runtime -> [project.dependencies];
    # test -> [project.optional-dependencies]) is honored by the build generator.
    return _def("PyProjectBuildConfiguration",
        T.record([
            T.field("dependencies", T.list_(_packaging("PackageDependency"))),
        ]))


_definitions = [
    _pyProjectBuildConfiguration(),
]


module_ = Module(
    NS,
    Given(EntityMetadata(
        Given(DESCRIPTION),
        (),
        (),
        None_())),
    DEPENDENCIES,
    tuple(_definitions),
)
check_complete(sys.modules[__name__], module_.definitions)
