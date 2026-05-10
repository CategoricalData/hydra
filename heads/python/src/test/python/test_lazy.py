"""Unit tests for hydra.python.util.Lazy."""

from hydra.python.util import Lazy


def test_call_dispatches_to_get():
    calls = []

    def thunk():
        calls.append(None)
        return 42

    lz = Lazy(thunk)
    assert lz() == 42  # __call__ syntax works
    assert lz() == 42  # cached
    assert lz.get() == 42  # both APIs return same value
    assert calls == [None]  # evaluated only once


def test_evaluates_on_first_get():
    calls = []

    def thunk():
        calls.append(None)
        return 42

    lz = Lazy(thunk)
    assert calls == []
    assert lz.get() == 42
    assert calls == [None]


def test_caches_after_first_get():
    calls = []

    def thunk():
        calls.append(None)
        return "hello"

    lz = Lazy(thunk)
    assert lz.get() == "hello"
    assert lz.get() == "hello"
    assert lz.get() == "hello"
    assert calls == [None]


def test_releases_thunk_after_evaluation():
    captured = ["x" * 1000]  # something the GC could reclaim once unreferenced

    def thunk():
        return captured[0]

    lz = Lazy(thunk)
    lz.get()
    assert lz._fn is None  # implementation detail: thunk dropped


def test_caches_falsy_values():
    calls = []

    def thunk():
        calls.append(None)
        return 0

    lz = Lazy(thunk)
    assert lz.get() == 0
    assert lz.get() == 0
    assert calls == [None]


def test_caches_none():
    calls = []

    def thunk():
        calls.append(None)
        return None

    lz = Lazy(thunk)
    assert lz.get() is None
    assert lz.get() is None
    assert calls == [None]
