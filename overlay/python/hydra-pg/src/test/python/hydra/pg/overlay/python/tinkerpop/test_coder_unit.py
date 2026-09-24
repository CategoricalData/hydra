"""Unit tests for the Python TinkerPop bridge's pure logic (no Gremlin Server, no gremlinpython).

The graph-touching functions (gremlin_to_hydra / hydra_to_gremlin / validate) require a live
gremlinpython traversal source and are therefore integration tests (see test_coder_integration.py).
These unit tests cover the value-mapping and validation-logic surface, which is server-independent.
"""

import hydra.core.model
from hydra.core.overlay.python.dsl.python import Given, None_

from hydra.pg.overlay.python.tinkerpop import coder


def test_object_to_literal_round_trips():
    # str / int / float survive object -> Literal -> object
    for obj in ["marko", 29, 0.5]:
        lit = coder.object_to_literal(obj)
        assert coder.literal_to_object(lit) == obj


def test_object_to_literal_types():
    assert isinstance(coder.object_to_literal("x"), hydra.core.model.LiteralString)
    assert isinstance(coder.object_to_literal(7), hydra.core.model.LiteralInteger)
    assert isinstance(coder.object_to_literal(1.5), hydra.core.model.LiteralFloat)


def test_show_literal_type():
    assert coder.show_literal_type(hydra.core.model.LiteralTypeString()) == "string"
    assert coder.show_literal_type(
        hydra.core.model.LiteralTypeInteger(hydra.core.model.IntegerType.INT32)) == "integer:int32"
    assert coder.show_literal_type(
        hydra.core.model.LiteralTypeFloat(hydra.core.model.FloatType.FLOAT64)) == "float:float64"


def test_check_literal_accepts_matching_type():
    lt = hydra.core.model.LiteralTypeString()
    lv = coder.object_to_literal("marko")
    result = coder.check_literal(lt, lv)
    assert isinstance(result, None_)


def test_check_literal_rejects_mismatched_type():
    lt = hydra.core.model.LiteralTypeString()
    lv = coder.object_to_literal(29)  # an integer where a string is expected
    result = coder.check_literal(lt, lv)
    assert isinstance(result, Given)


def test_check_literal_int32_matches():
    lt = hydra.core.model.LiteralTypeInteger(hydra.core.model.IntegerType.INT32)
    lv = coder.object_to_literal(29)
    assert isinstance(coder.check_literal(lt, lv), None_)
