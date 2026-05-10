"""Unit tests for hydra.util.PersistentSet."""

from collections.abc import Set as AbstractSet

import pytest

from hydra.python.util import PersistentSet


def test_empty():
    s = PersistentSet.empty()
    assert s.is_empty()
    assert len(s) == 0


def test_singleton():
    s = PersistentSet.singleton("a")
    assert len(s) == 1
    assert "a" in s


def test_of():
    s = PersistentSet.of(1, 2, 3, 2, 1)
    assert len(s) == 3
    assert 1 in s
    assert 2 in s
    assert 3 in s


def test_member():
    s = PersistentSet.of("a", "b", "c")
    assert s.member("a")
    assert not s.member("z")


def test_insert():
    s = PersistentSet.empty().insert("a").insert("b").insert("c")
    assert len(s) == 3
    assert "a" in s and "b" in s and "c" in s


def test_insert_existing_returns_self():
    s = PersistentSet.singleton("a")
    s2 = s.insert("a")
    assert s2 is s


def test_immutability():
    s1 = PersistentSet.singleton("a")
    s2 = s1.insert("b")
    assert "b" not in s1
    assert "b" in s2


def test_delete():
    s = PersistentSet.of(1, 2, 3)
    s2 = s.delete(2)
    assert 2 in s
    assert 2 not in s2
    assert len(s2) == 2


def test_delete_absent_returns_self():
    s = PersistentSet.singleton("a")
    assert s.delete("missing") is s


def test_iteration_sorted():
    s = PersistentSet.of(3, 1, 2)
    assert list(s) == [1, 2, 3]


def test_union():
    a = PersistentSet.of(1, 2, 3)
    b = PersistentSet.of(2, 3, 4)
    u = a.union(b)
    assert list(u) == [1, 2, 3, 4]


def test_union_empty_lhs():
    a = PersistentSet.empty()
    b = PersistentSet.of(1, 2)
    assert a.union(b) is b


def test_intersection():
    a = PersistentSet.of(1, 2, 3, 4)
    b = PersistentSet.of(3, 4, 5, 6)
    assert list(a.intersection(b)) == [3, 4]


def test_intersection_with_empty():
    a = PersistentSet.of(1, 2)
    b = PersistentSet.empty()
    assert a.intersection(b).is_empty()


def test_difference():
    a = PersistentSet.of(1, 2, 3, 4)
    b = PersistentSet.of(3, 4, 5)
    assert list(a.difference(b)) == [1, 2]


def test_unions():
    a = PersistentSet.of(1, 2)
    b = PersistentSet.of(2, 3)
    c = PersistentSet.of(3, 4)
    assert list(PersistentSet.unions([a, b, c])) == [1, 2, 3, 4]


def test_map():
    s = PersistentSet.of(1, 2, 3)
    s2 = s.map(lambda x: x * 10)
    assert list(s2) == [10, 20, 30]


def test_filter():
    s = PersistentSet.of(1, 2, 3, 4)
    s2 = s.filter(lambda x: x % 2 == 0)
    assert list(s2) == [2, 4]


def test_filter_no_change_returns_self():
    s = PersistentSet.of(2, 4, 6)
    s2 = s.filter(lambda x: x % 2 == 0)
    assert s2 is s


def test_to_list():
    s = PersistentSet.of(3, 1, 2)
    assert s.to_list() == [1, 2, 3]


def test_is_a_set():
    s = PersistentSet.singleton("a")
    assert isinstance(s, AbstractSet)


def test_eq_with_python_set():
    s = PersistentSet.of(1, 2, 3)
    assert s == {1, 2, 3}


def test_eq_with_other_persistent_set():
    a = PersistentSet.of(1, 2, 3)
    b = PersistentSet.of(3, 1, 2)
    assert a == b


def test_hash_equal_for_equal_sets():
    a = PersistentSet.of(1, 2, 3)
    b = PersistentSet.of(3, 1, 2)
    assert hash(a) == hash(b)


def test_complex_elements_via_structural_compare():
    """Elements that aren't naturally <-comparable should still work."""
    class Wrapped:
        def __init__(self, x: int) -> None:
            self.x = x
        def __eq__(self, other: object) -> bool:
            return isinstance(other, Wrapped) and self.x == other.x
        def __hash__(self) -> int:
            return hash(self.x)
    a, b, c = Wrapped(1), Wrapped(2), Wrapped(3)
    s = PersistentSet.empty().insert(a).insert(b).insert(c)
    assert a in s
    assert b in s
    assert c in s
    assert len(s) == 3


def test_direct_construction_forbidden():
    with pytest.raises(TypeError):
        PersistentSet()
