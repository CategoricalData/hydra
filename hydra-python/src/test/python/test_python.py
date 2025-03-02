from typing import Any

import pytest
from hydra.dsl.python import FrozenDict, freeze


def test_basic_properties():
    data = {"a": 1, "b": 2}
    fd = FrozenDict(data)

    # Test __getitem__
    assert fd["a"] == 1

    # Test __len__
    assert len(fd) == 2

    # Test __iter__
    keys = list(iter(fd))
    assert set(keys) == {"a", "b"}

    # Test __hash__ (equal FrozenDicts have equal hash)
    fd2 = FrozenDict(data)
    assert hash(fd) == hash(fd2)

    # Test __repr__
    rep = repr(fd)
    assert rep.startswith("FrozenDict(")
    assert "a" in rep and "1" in rep


def test_hashing():
    """Test that FrozenDict can be properly used with hashing operations."""
    fd1 = FrozenDict({"a": 1, "b": 2})
    fd2 = FrozenDict({"a": 1, "b": 2})
    fd3 = FrozenDict({"b": 2, "a": 1})
    fd4 = FrozenDict({"a": 1, "c": 2})

    assert hash(fd1) == hash(fd2)
    assert hash(fd1) == hash(fd3)
    assert hash(fd1) != hash(fd4)

    lookup = {fd1: "value1", fd4: "value2"}
    assert lookup[fd2] == "value1"
    assert lookup[fd3] == "value1"

    # Hash should be consistent after source data changes
    original_data = {"x": 1, "y": 2}
    fd = FrozenDict(original_data)
    original_hash = hash(fd)
    original_data["z"] = 3
    assert hash(fd) == original_hash


def test_copy_method():
    data = {"a": 1}
    fd = FrozenDict(data)
    fd2 = fd.copy({"b": 2})

    assert fd2["a"] == 1
    assert fd2["b"] == 2

    with pytest.raises(KeyError):
        _ = fd["b"]


def test_recursive_freeze_dict():
    data = {"nested": {"x": 10, "y": 20}}
    fd = FrozenDict(data)

    nested = fd.data["nested"]
    assert isinstance(nested, FrozenDict)
    with pytest.raises(TypeError):
        nested["x"] = 100


def test_recursive_freeze_list():
    data = {"list": [1, 2, 3]}
    fd = FrozenDict(data)
    lst = fd.data["list"]
    # Lists should be converted to tuples.
    assert isinstance(lst, tuple)
    assert lst == (1, 2, 3)


def test_recursive_freeze_set():
    data = {"set": {1, 2, 3}}
    fd = FrozenDict(data)
    s = fd.data["set"]
    # Sets should be converted to frozensets.
    assert isinstance(s, frozenset)
    assert s == frozenset({1, 2, 3})


def test_recursive_freeze_tuple():
    data = {"tuple": ({"a": 1}, 2)}
    fd = FrozenDict(data)
    tup = fd.data["tuple"]

    assert isinstance(tup, tuple)

    first = tup[0]
    assert isinstance(first, FrozenDict)

    inside = FrozenDict({"a": 1})
    assert first == inside


def test_getitem_error():
    fd = FrozenDict({"a": 1})
    with pytest.raises(KeyError):
        _ = fd["nonexistent"]


def test_recursive_freeze_function_on_various_types():
    # Test _recursive_freeze directly.
    # For a dict, should return a MappingProxyType.
    frozen_dict = freeze({"key": {"inner": 42}})
    assert isinstance(frozen_dict, FrozenDict)
    # For a list, should return a tuple.
    frozen_list = freeze([{"a": 1}, 2, [3, 4]])
    assert isinstance(frozen_list, tuple)
    assert isinstance(frozen_list[0], FrozenDict)
    # For a set, should return a frozenset.
    frozen_set = freeze({1, 2, 3})
    assert isinstance(frozen_set, frozenset)
    # For a tuple, should return a tuple with recursively frozen items.
    frozen_tuple = freeze(({"b": 2}, 3))
    assert isinstance(frozen_tuple, tuple)
    assert isinstance(frozen_tuple[0], FrozenDict)


def test_equality():
    """Test that FrozenDicts with the same data are equal."""
    fd1 = FrozenDict({"a": 1, "b": 2})
    fd2 = FrozenDict({"a": 1, "b": 2})
    fd3 = FrozenDict({"a": 1, "c": 2})

    # Same content should be equal
    assert fd1 == fd2

    # Different content should not be equal
    assert fd1 != fd3


def test_complex_nesting():
    """Test FrozenDict with deeply nested structures."""
    complex_data = {
        "level1": {"level2": {"level3": [1, 2, {"deep": [3, 4, {"deeper": (5, 6)}]}]}},
        "siblings": [{"name": "sibling1"}, {"name": "sibling2", "data": {1, 2, 3}}],
    }

    fd = FrozenDict[str, Any](complex_data)

    # Navigate to a deeply nested value
    assert fd["level1"]["level2"]["level3"][2]["deep"][2]["deeper"][1] == 6

    # Check that deep structures are properly frozen
    assert isinstance(fd.data["level1"], FrozenDict)
    assert isinstance(fd.data["siblings"], tuple)
    assert isinstance(fd.data["siblings"][0], FrozenDict)
    assert isinstance(fd.data["siblings"][1]["data"], frozenset)


def test_copy_with_none():
    """Test the copy method with None parameter."""
    fd = FrozenDict({"a": 1})
    fd2 = fd.copy(None)

    # Should be equal but not the same object
    assert fd == fd2
    assert fd is not fd2


def test_edge_cases():
    """Test FrozenDict with edge cases."""
    # Empty dict
    empty_fd = FrozenDict[str, Any]({})
    assert len(empty_fd) == 0

    # Dict with None as values
    none_fd = FrozenDict({"null": None})
    assert none_fd["null"] is None

    # Dict with None as keys (if supported)
    none_key_fd = FrozenDict({None: "value"})
    assert none_key_fd[None] == "value"
    print(none_key_fd)

    # Very large dict (just test creation)
    large_dict = {str(i): i for i in range(1000)}
    large_fd = FrozenDict(large_dict)
    assert len(large_fd) == 1000
