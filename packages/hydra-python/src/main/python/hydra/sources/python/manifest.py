"""Package manifest for hydra-python.

Mirror of packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Manifest.hs.
Owns the Python coder DSL sources.

This module exports `main_modules` and `test_modules` lists, used by build tooling
to enumerate which Hydra modules belong to the hydra-python package. There is no
per-module JSON output; the manifest contributes to dist/json/hydra-python/manifest.json.
"""

from hydra.packaging import Module

# Imports are deferred to the body of the lists to avoid forcing every module to be built
# when only manifest metadata is needed. Each is the `module_` value from the corresponding
# hydra.sources.python.<X> module.

import hydra.sources.python.coder as _coder
import hydra.sources.python.environment as _environment
import hydra.sources.python.language as _language
import hydra.sources.python.names as _names
import hydra.sources.python.serde as _serde
import hydra.sources.python.syntax as _syntax
import hydra.sources.python.testing as _testing
import hydra.sources.python.utils as _utils


main_modules: list[Module] = [
    _coder.module_,
    _environment.module_,
    _language.module_,
    _names.module_,
    _serde.module_,
    _syntax.module_,
    _testing.module_,
    _utils.module_,
]

test_modules: list[Module] = []
