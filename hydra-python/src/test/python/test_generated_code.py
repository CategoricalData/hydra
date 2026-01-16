"""
hydra-haskell {

writePython "../hydra-python/src/gen-main/python" kernelTypesModules

writePython "../hydra-python/src/gen-test/python" testModules

:set +m

writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Adapt.Literals.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Adapt.Modules.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Adapt.Simple.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Adapt.Terms.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Adapt.Utils.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Decode.Core.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Describe.Core.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Describe.Mantle.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Encode.Core.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Extract.Core.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Extract.Mantle.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Show.Accessors.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Show.Core.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Show.Graph.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Show.Mantle.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Show.Typing.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Annotations.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Arity.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Checking.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Constants.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Decoding.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Formatting.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Grammars.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Inference.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Languages.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Lexical.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Literals.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Monads.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Names.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Reduction.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Rewriting.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Schemas.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Serialization.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Sorting.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Substitution.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Tarjan.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Templates.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Unification.module_]
writePython "../hydra-python/src/gen-main/python" [Hydra.Sources.Kernel.Terms.Variants.module_]

}


Bash {
pytest

# pyright src/main/python/hydra

pyright src/main/python/hydra/dsl/python.py

# All libraries
pyright src/main/python/hydra/lib

# Individual libraries
pyright src/main/python/hydra/lib/chars.py
pyright src/main/python/hydra/lib/equality.py
pyright src/main/python/hydra/lib/flows.py
pyright src/main/python/hydra/lib/lists.py
pyright src/main/python/hydra/lib/literals.py
pyright src/main/python/hydra/lib/logic.py
pyright src/main/python/hydra/lib/maps.py
pyright src/main/python/hydra/lib/math.py
pyright src/main/python/hydra.lib.maybes.py
pyright src/main/python/hydra/lib/sets.py
pyright src/main/python/hydra/lib/strings.py

# Kernel types (generated)
pyright src/gen-main/python/hydra/accessors.py
pyright src/gen-main/python/hydra/ast.py
pyright src/gen-main/python/hydra/coders.py
pyright src/gen-main/python/hydra/compute.py
pyright src/gen-main/python/hydra/constraints.py
pyright src/gen-main/python/hydra/core.py
pyright src/gen-main/python/hydra/grammar.py
pyright src/gen-main/python/hydra/graph.py
pyright src/gen-main/python/hydra/json.py
pyright src/gen-main/python/hydra/mantle.py
pyright src/gen-main/python/hydra/module.py
pyright src/gen-main/python/hydra/phantoms.py
pyright src/gen-main/python/hydra/query.py
pyright src/gen-main/python/hydra/relational.py
pyright src/gen-main/python/hydra/tabular.py
pyright src/gen-main/python/hydra/testing.py
pyright src/gen-main/python/hydra/topology.py
pyright src/gen-main/python/hydra/typing.py
pyright src/gen-main/python/hydra/workflow.py

# Kernel terms (generated)
pyright src/gen-main/python/hydra/adapt/literals.py
#pyright src/gen-main/python/hydra/adapt/modules.py # TODO
pyright src/gen-main/python/hydra/adapt/simple.py
#pyright src/gen-main/python/hydra/adapt/terms.py # TODO
#pyright src/gen-main/python/hydra/adapt/utils.py # TODO
pyright src/gen-main/python/hydra/decode/core.py
pyright src/gen-main/python/hydra/describe/core.py
pyright src/gen-main/python/hydra/describe/mantle.py
pyright src/gen-main/python/hydra/encode/core.py
pyright src/gen-main/python/hydra/extract/core.py
pyright src/gen-main/python/hydra/extract/mantle.py
pyright src/gen-main/python/hydra/show/accessors.py
pyright src/gen-main/python/hydra/show/core.py
pyright src/gen-main/python/hydra/show/graph.py
pyright src/gen-main/python/hydra/show/mantle.py
pyright src/gen-main/python/hydra/show/typing.py
pyright src/gen-main/python/hydra/annotations.py
pyright src/gen-main/python/hydra/arity.py
pyright src/gen-main/python/hydra/checking.py
pyright src/gen-main/python/hydra/constants.py
#pyright src/gen-main/python/hydra/decoding.py # TODO
pyright src/gen-main/python/hydra/formatting.py
pyright src/gen-main/python/hydra/grammars.py
pyright src/gen-main/python/hydra/inference.py
pyright src/gen-main/python/hydra/languages.py
pyright src/gen-main/python/hydra/lexical.py
pyright src/gen-main/python/hydra/literals.py
pyright src/gen-main/python/hydra/monads.py
pyright src/gen-main/python/hydra/names.py
pyright src/gen-main/python/hydra/reduction.py
pyright src/gen-main/python/hydra/rewriting.py
pyright src/gen-main/python/hydra/schemas.py
pyright src/gen-main/python/hydra/serialization.py
pyright src/gen-main/python/hydra/sorting.py
pyright src/gen-main/python/hydra/substitution.py
pyright src/gen-main/python/hydra/tarjan.py
pyright src/gen-main/python/hydra/templates.py
pyright src/gen-main/python/hydra/unification.py
pyright src/gen-main/python/hydra/variants.py
}
"""

# types
from hydra.accessors import *
from hydra.ast import *
from hydra.coders import *
from hydra.compute import *
from hydra.constraints import *
from hydra.core import *
from hydra.grammar import *
from hydra.graph import *
from hydra.json import *
# from hydra.meta import *  # meta module no longer exists after refactoring
from hydra.util import *
from hydra.module import *
from hydra.phantoms import *
from hydra.query import *
from hydra.relational import *
from hydra.tabular import *
from hydra.testing import *
from hydra.topology import *
from hydra.typing import *
from hydra.workflow import *

# terms
# TODO: adapt, decode, describe, encode, extract, show
# from hydra.annotations import *
# from hydra.arity import *
from hydra.constants import *
# from hydra.decoding import *
# from hydra.formatting import *
# from hydra.grammars import *
# from hydra.inference import *
from hydra.languages import *
# from hydra.lexical import *
from hydra.literals import *
# from hydra.monads import *
# from hydra.names import *
# from hydra.reduction import *
# from hydra.rewriting import *
# from hydra.schemas import *
# from hydra.serialization import *
# from hydra.sorting import *
# from hydra.substitution import *
# from hydra.tarjan import *
# from hydra.templates import *
# from hydra.unification import *
from hydra.variants import *

from dataclasses import dataclass
from typing import Generic, TypeVar







""""""
