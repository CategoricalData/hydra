"""IEEE 754 extended totalOrder conformance for floats, per
docs/specification/ordering-and-equality.md: single NaN, NaN greatest and equal to
itself, -0.0 distinct from and less than +0.0. Covers hydra.lib.equality.equal and
hydra.lib.ordering.compare, both of which route through
hydra.overlay.python.util._compare. See #720.

PersistentMap/PersistentSet keying is exercised only for NaN here: they are backed by
native dict/frozenset, so key identity uses Python's native hash+`==`, not _compare --
this dedups NaN correctly (consistent hash, and here the same NaN object is reused so
identity short-circuits equality) but does NOT distinguish -0.0/+0.0 as keys, since
hash(-0.0) == hash(0.0) and -0.0 == 0.0 natively. That gap is out of scope for #720
(tracked as a follow-up finding in the branch plan)."""

from __future__ import annotations

import math

from hydra.overlay.python.lib.equality import equal
from hydra.overlay.python.lib.ordering import compare as ordering_compare
from hydra.overlay.python.util import PersistentMap, PersistentSet
from hydra.overlay.python.util._compare import compare
from hydra.util import Comparison

NAN = float("nan")
NEG_ZERO = -0.0
POS_ZERO = 0.0


def test_nan_equal_to_itself():
    assert equal(NAN, NAN) is True
    assert compare(NAN, NAN) == 0
    assert ordering_compare(NAN, NAN) == Comparison.EQUAL_TO


def test_nan_greatest():
    for x in (float("-inf"), -1.0, NEG_ZERO, POS_ZERO, 1.0, float("inf")):
        assert compare(x, NAN) == -1
        assert compare(NAN, x) == 1
        assert equal(NAN, x) is False
        assert equal(x, NAN) is False


def test_negative_zero_distinct_from_positive_zero():
    assert equal(NEG_ZERO, POS_ZERO) is False
    assert compare(NEG_ZERO, POS_ZERO) == -1
    assert compare(POS_ZERO, NEG_ZERO) == 1


def test_negative_zero_equal_to_itself():
    assert equal(NEG_ZERO, NEG_ZERO) is True
    assert compare(NEG_ZERO, NEG_ZERO) == 0


def test_finite_values_compare_numerically():
    assert compare(1.0, 2.0) == -1
    assert compare(2.0, 1.0) == 1
    assert compare(1.0, 1.0) == 0
    assert equal(1.0, 1.0) is True
    assert equal(1.0, 2.0) is False


def test_infinities():
    assert compare(float("-inf"), float("inf")) == -1
    assert equal(float("inf"), float("inf")) is True
    assert equal(float("-inf"), float("inf")) is False


def test_extended_total_order_sequence():
    """-inf < negative finite < -0.0 < +0.0 < positive finite < +inf < NaN."""
    ordered = [float("-inf"), -1.0, NEG_ZERO, POS_ZERO, 1.0, float("inf"), NAN]
    for i in range(len(ordered) - 1):
        assert compare(ordered[i], ordered[i + 1]) == -1
        assert compare(ordered[i + 1], ordered[i]) == 1


def test_compare_equal_invariant():
    """compare x y == equalTo iff equal x y, for every pair, including special values."""
    values = [float("-inf"), -1.0, NEG_ZERO, POS_ZERO, 1.0, float("inf"), NAN]
    for x in values:
        for y in values:
            assert (compare(x, y) == 0) == equal(x, y)


def test_map_keying_nan_is_ordinary_key():
    m = PersistentMap.empty().insert(NAN, "nan")
    assert len(m) == 1
    assert m[NAN] == "nan"
    # Re-inserting with an equal (NaN) key replaces the binding, per spec.
    m2 = m.insert(NAN, "nan2")
    assert len(m2) == 1
    assert m2[NAN] == "nan2"


def test_set_dedups_nan():
    s = PersistentSet.empty().insert(NAN).insert(NAN)
    assert len(s) == 1
    elements = list(s)
    assert sum(1 for e in elements if isinstance(e, float) and math.isnan(e)) == 1
