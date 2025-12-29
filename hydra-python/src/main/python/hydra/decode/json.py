# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.json."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import cast
import hydra.core
import hydra.graph
import hydra.json
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.util

def value(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.json.Value]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.json.Value], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))
