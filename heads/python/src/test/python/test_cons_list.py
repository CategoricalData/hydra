"""Unit tests for hydra.util.ConsList.

Mirrors the shape of heads/java/src/test/java/hydra/util/ConsListTest.java.
"""

from collections.abc import Sequence

import pytest

from hydra.python.util import ConsList


def test_empty():
    l = ConsList.empty()
    assert l.is_empty()
    assert len(l) == 0


def test_cons():
    l = ConsList.cons("a", ConsList.empty())
    assert not l.is_empty()
    assert len(l) == 1
    assert l.head == "a"


def test_head_tail():
    l = ConsList.of(1, 2, 3)
    assert l.head == 1
    assert l.tail.head == 2
    assert l.tail.tail.head == 3
    assert l.tail.tail.tail.is_empty()


def test_head_of_empty_raises():
    with pytest.raises(IndexError):
        _ = ConsList.empty().head


def test_tail_of_empty_raises():
    with pytest.raises(IndexError):
        _ = ConsList.empty().tail


def test_size():
    assert len(ConsList.empty()) == 0
    assert len(ConsList.singleton(1)) == 1
    assert len(ConsList.of(1, 2, 3)) == 3


def test_structural_sharing():
    """cons must produce equal tails."""
    tail = ConsList.of(2, 3)
    l1 = ConsList.cons(1, tail)
    l2 = ConsList.cons(0, tail)
    assert l1.tail == tail
    assert l2.tail == tail
    assert l1.tail == l2.tail


def test_get():
    l = ConsList.of("a", "b", "c")
    assert l[0] == "a"
    assert l[1] == "b"
    assert l[2] == "c"


def test_get_negative():
    l = ConsList.of("a", "b", "c")
    assert l[-1] == "c"
    assert l[-3] == "a"


def test_get_out_of_bounds():
    with pytest.raises(IndexError):
        _ = ConsList.of(1)[1]
    with pytest.raises(IndexError):
        _ = ConsList.of(1)[-2]


def test_last():
    assert ConsList.of(1, 2, 3).last() == 3
    assert ConsList.singleton(1).last() == 1
    with pytest.raises(IndexError):
        ConsList.empty().last()


def test_init():
    assert list(ConsList.of(1, 2, 3).init()) == [1, 2]
    assert ConsList.singleton(1).init().is_empty()
    with pytest.raises(IndexError):
        ConsList.empty().init()


def test_take():
    l = ConsList.of(1, 2, 3, 4, 5)
    assert list(l.take(0)) == []
    assert list(l.take(3)) == [1, 2, 3]
    assert list(l.take(10)) == [1, 2, 3, 4, 5]
    assert list(l.take(-1)) == []


def test_drop():
    l = ConsList.of(1, 2, 3, 4, 5)
    assert list(l.drop(0)) == [1, 2, 3, 4, 5]
    assert list(l.drop(3)) == [4, 5]
    assert list(l.drop(10)) == []


def test_drop_shares_structure():
    """drop should produce an equal tail."""
    tail = ConsList.of(4, 5)
    l = ConsList.cons(1, ConsList.cons(2, ConsList.cons(3, tail)))
    assert l.drop(3) == tail


def test_reverse():
    assert list(ConsList.of(1, 2, 3).reverse()) == [3, 2, 1]
    assert ConsList.empty().reverse().is_empty()


def test_map():
    assert list(ConsList.of(1, 2, 3).map(lambda x: x * 2)) == [2, 4, 6]
    assert ConsList.empty().map(lambda x: x).is_empty()


def test_filter():
    assert list(ConsList.of(1, 2, 3, 4).filter(lambda x: x % 2 == 0)) == [2, 4]


def test_foldl():
    assert ConsList.of(1, 2, 3, 4).foldl(lambda a, b: a + b, 0) == 10
    assert ConsList.empty().foldl(lambda a, b: a + b, 42) == 42


def test_foldr():
    # foldr (-) 0 [1,2,3] = 1 - (2 - (3 - 0)) = 2
    assert ConsList.of(1, 2, 3).foldr(lambda a, b: a - b, 0) == 2


def test_concat():
    a = ConsList.of(1, 2, 3)
    b = ConsList.of(4, 5)
    assert list(a.concat(b)) == [1, 2, 3, 4, 5]
    assert ConsList.empty().concat(b) is b
    assert a.concat(ConsList.empty()) is a


def test_iter():
    assert list(ConsList.of(1, 2, 3)) == [1, 2, 3]


def test_contains():
    l = ConsList.of(1, 2, 3)
    assert 2 in l
    assert 99 not in l


def test_eq_with_consist():
    assert ConsList.of(1, 2, 3) == ConsList.of(1, 2, 3)
    assert ConsList.of(1, 2, 3) != ConsList.of(1, 2)


def test_eq_with_other_sequence():
    """A ConsList should compare equal to a list/tuple with the same elements."""
    assert ConsList.of(1, 2, 3) == [1, 2, 3]
    assert ConsList.of(1, 2, 3) == (1, 2, 3)


def test_hash():
    """ConsLists with equal contents hash equal."""
    assert hash(ConsList.of(1, 2, 3)) == hash(ConsList.of(1, 2, 3))


def test_is_a_sequence():
    """ConsList should satisfy collections.abc.Sequence."""
    l = ConsList.of(1, 2, 3)
    assert isinstance(l, Sequence)


def test_slicing_full():
    l = ConsList.of(1, 2, 3, 4, 5)
    assert list(l[1:4]) == [2, 3, 4]


def test_direct_construction_forbidden():
    with pytest.raises(TypeError):
        ConsList()
