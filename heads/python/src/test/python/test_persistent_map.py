"""Unit tests for hydra.util.PersistentMap."""

from collections.abc import Mapping

import pytest

from hydra.python.util import PersistentMap


def test_empty():
    m = PersistentMap.empty()
    assert m.is_empty()
    assert len(m) == 0


def test_singleton():
    m = PersistentMap.singleton("a", 1)
    assert len(m) == 1
    assert m["a"] == 1


def test_insert_lookup():
    m = PersistentMap.empty().insert("a", 1).insert("b", 2).insert("c", 3)
    assert len(m) == 3
    assert m["a"] == 1
    assert m["b"] == 2
    assert m["c"] == 3


def test_insert_replaces():
    m = PersistentMap.singleton("a", 1).insert("a", 2)
    assert len(m) == 1
    assert m["a"] == 2


def test_insert_returns_same_when_value_unchanged():
    """Inserting an equal value should return the same instance (structure sharing optimization)."""
    m = PersistentMap.singleton("a", 1)
    m2 = m.insert("a", 1)
    # The exact-same root node should be reused.
    assert m2 is m


def test_lookup_absent():
    m = PersistentMap.singleton("a", 1)
    assert m.lookup("missing") is None


def test_getitem_raises_for_missing():
    m = PersistentMap.singleton("a", 1)
    with pytest.raises(KeyError):
        _ = m["missing"]


def test_contains():
    m = PersistentMap.singleton("a", 1)
    assert "a" in m
    assert "b" not in m


def test_immutability():
    """Updates produce new maps; the original is unchanged."""
    m1 = PersistentMap.singleton("a", 1)
    m2 = m1.insert("b", 2)
    assert "b" not in m1
    assert "b" in m2


def test_delete():
    m = PersistentMap.empty().insert("a", 1).insert("b", 2).insert("c", 3)
    m2 = m.delete("b")
    assert "b" in m
    assert "b" not in m2
    assert len(m2) == 2


def test_delete_absent_returns_self():
    m = PersistentMap.singleton("a", 1)
    assert m.delete("missing") is m


def test_iteration_order_is_sorted():
    m = (PersistentMap.empty()
         .insert("c", 3)
         .insert("a", 1)
         .insert("b", 2))
    assert list(m.keys()) == ["a", "b", "c"]


def test_items():
    m = (PersistentMap.empty()
         .insert(2, "b")
         .insert(1, "a")
         .insert(3, "c"))
    assert list(m.items()) == [(1, "a"), (2, "b"), (3, "c")]


def test_values():
    m = (PersistentMap.empty()
         .insert(2, "b")
         .insert(1, "a")
         .insert(3, "c"))
    assert list(m.values()) == ["a", "b", "c"]


def test_union_left_biased():
    a = PersistentMap.from_pairs([("x", 1), ("y", 2)])
    b = PersistentMap.from_pairs([("y", 99), ("z", 3)])
    u = a.union(b)
    assert u["x"] == 1
    assert u["y"] == 2  # a wins
    assert u["z"] == 3
    assert len(u) == 3


def test_alter_insert():
    m = PersistentMap.singleton("a", 1)
    m2 = m.alter(lambda v: 99 if v is None else v + 1, "a")
    assert m2["a"] == 2


def test_alter_delete():
    m = PersistentMap.singleton("a", 1)
    m2 = m.alter(lambda _: None, "a")
    assert "a" not in m2


def test_alter_insert_new():
    m = PersistentMap.singleton("a", 1)
    m2 = m.alter(lambda v: 42 if v is None else v, "b")
    assert m2["b"] == 42


def test_map_values():
    m = PersistentMap.from_pairs([("a", 1), ("b", 2)])
    m2 = m.map_values(lambda v: v * 10)
    assert m2["a"] == 10
    assert m2["b"] == 20


def test_filter():
    m = PersistentMap.from_pairs([("a", 1), ("b", 2), ("c", 3)])
    m2 = m.filter(lambda v: v % 2 == 1)
    assert "a" in m2
    assert "b" not in m2
    assert "c" in m2


def test_from_mapping_roundtrip():
    m = PersistentMap.from_mapping({"a": 1, "b": 2})
    assert m["a"] == 1
    assert m["b"] == 2


def test_is_a_mapping():
    m = PersistentMap.singleton("a", 1)
    assert isinstance(m, Mapping)


def test_eq_with_dict():
    m = PersistentMap.from_pairs([("a", 1), ("b", 2)])
    assert m == {"a": 1, "b": 2}


def test_eq_with_other_persistent_map():
    a = PersistentMap.from_pairs([("a", 1), ("b", 2)])
    b = PersistentMap.from_pairs([("b", 2), ("a", 1)])
    assert a == b


def test_hash():
    a = PersistentMap.from_pairs([("a", 1), ("b", 2)])
    b = PersistentMap.from_pairs([("b", 2), ("a", 1)])
    assert hash(a) == hash(b)


def test_many_inserts_balanced():
    """Insertion of many keys should work (this exercises the RB rebalancing)."""
    m = PersistentMap.empty()
    n = 200
    for i in range(n):
        m = m.insert(i, i * 10)
    assert len(m) == n
    for i in range(n):
        assert m[i] == i * 10
    # Iteration should give them in order.
    assert list(m.keys()) == list(range(n))


def test_many_deletes():
    m = PersistentMap.empty()
    for i in range(100):
        m = m.insert(i, i)
    for i in range(0, 100, 2):
        m = m.delete(i)
    assert len(m) == 50
    assert list(m.keys()) == list(range(1, 100, 2))


def test_complex_keys_via_structural_compare():
    """Keys that aren't naturally <-comparable should still work via the
    structural fallback in _compare."""
    class Wrapped:
        def __init__(self, x: int) -> None:
            self.x = x
        def __eq__(self, other: object) -> bool:
            return isinstance(other, Wrapped) and self.x == other.x
        def __hash__(self) -> int:
            return hash(self.x)
    # Wrapped doesn't define __lt__; structural compare falls back to __dict__.
    a, b, c = Wrapped(1), Wrapped(2), Wrapped(3)
    m = PersistentMap.empty().insert(a, "a").insert(b, "b").insert(c, "c")
    assert m[a] == "a"
    assert m[b] == "b"
    assert m[c] == "c"
    assert len(m) == 3


def test_direct_construction_forbidden():
    with pytest.raises(TypeError):
        PersistentMap()
