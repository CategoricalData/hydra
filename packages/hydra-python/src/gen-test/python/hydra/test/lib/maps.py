# Note: this is an automatically generated file. Do not edit.

r"""Test cases for hydra.lib.maps primitives."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing
from typing import cast
import hydra.core
import hydra.lib.chars
import hydra.lib.equality
import hydra.lib.literals
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.strings
import hydra.show.core
import hydra.testing

@lru_cache(1)
def all_tests() -> hydra.testing.TestGroup:
    r"""Test cases for hydra.lib.maps primitives."""

    return hydra.testing.TestGroup("hydra.lib.maps primitives", Nothing(), (hydra.testing.TestGroup("alter", Nothing(), (), (hydra.testing.TestCaseWithMetadata("insert new key", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.alter((lambda _: Just("new")), 3, FrozenDict({
      1: "a",
      2: "b"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "a",
      2: "b",
      3: "new"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("update existing key", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.alter((lambda _: Just("updated")), 2, FrozenDict({
      1: "a",
      2: "b"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "a",
      2: "updated"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("delete key", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.alter((lambda _: Nothing()), 2, FrozenDict({
      1: "a",
      2: "b"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "a"}))))), Nothing(), ()))), hydra.testing.TestGroup("bimap", Nothing(), (), (hydra.testing.TestCaseWithMetadata("transform both", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.bimap((lambda k: hydra.lib.math.mul(k, 2)), (lambda v: hydra.lib.strings.to_upper(v)), FrozenDict({
      1: "a",
      2: "b"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      2: "A",
      4: "B"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("empty map", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.bimap((lambda k: hydra.lib.math.mul(k, 2)), (lambda v: hydra.lib.strings.to_upper(v)), FrozenDict({}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({}))))), Nothing(), ()))), hydra.testing.TestGroup("elems", Nothing(), (), (hydra.testing.TestCaseWithMetadata("get all elements", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.list((lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.elems(FrozenDict({
      1: "a",
      2: "b"}))), hydra.show.core.list((lambda s: hydra.lib.literals.show_string(s)), ("a", "b"))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("unsorted keys", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.list((lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.elems(FrozenDict({
      1: "a",
      2: "b",
      3: "c"}))), hydra.show.core.list((lambda s: hydra.lib.literals.show_string(s)), ("a", "b", "c"))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("empty map", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.list((lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.elems(FrozenDict({}))), hydra.show.core.list((lambda s: hydra.lib.literals.show_string(s)), ())))), Nothing(), ()))), hydra.testing.TestGroup("empty", Nothing(), (), (hydra.testing.TestCaseWithMetadata("empty map", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.empty()), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({}))))), Nothing(), ()),)), hydra.testing.TestGroup("filter", Nothing(), (), (hydra.testing.TestCaseWithMetadata("filter values starting with a", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.filter((lambda v: hydra.lib.equality.equal(hydra.lib.chars.to_lower(hydra.lib.strings.char_at(0, v)), 97)), FrozenDict({
      1: "a",
      2: "b",
      3: "ab"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "a",
      3: "ab"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("filter all", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.filter((lambda v: hydra.lib.equality.equal(hydra.lib.chars.to_lower(hydra.lib.strings.char_at(0, v)), 97)), FrozenDict({
      1: "b",
      2: "c"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("empty map", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.filter((lambda v: hydra.lib.equality.equal(hydra.lib.chars.to_lower(hydra.lib.strings.char_at(0, v)), 97)), FrozenDict({}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({}))))), Nothing(), ()))), hydra.testing.TestGroup("filterWithKey", Nothing(), (), (hydra.testing.TestCaseWithMetadata("filter by key > 1", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.filter_with_key((lambda k, v: hydra.lib.equality.gt(k, 1)), FrozenDict({
      1: "a",
      2: "b",
      3: "c"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      2: "b",
      3: "c"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("filter all", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.filter_with_key((lambda k, v: hydra.lib.equality.gt(k, 1)), FrozenDict({
      1: "a"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("empty map", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.filter_with_key((lambda k, v: hydra.lib.equality.gt(k, 1)), FrozenDict({}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({}))))), Nothing(), ()))), hydra.testing.TestGroup("findWithDefault", Nothing(), (), (hydra.testing.TestCaseWithMetadata("find existing", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.maps.find_with_default("default", 2, FrozenDict({
      1: "a",
      2: "b"})), "b"))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("use default", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.maps.find_with_default("default", 3, FrozenDict({
      1: "a",
      2: "b"})), "default"))), Nothing(), ()))), hydra.testing.TestGroup("fromList", Nothing(), (), (hydra.testing.TestCaseWithMetadata("create from pairs", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.from_list(((1, "a"), (2, "b")))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "a",
      2: "b"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("duplicate keys", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.from_list(((1, "a"), (1, "b")))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "b"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("empty list", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.from_list(())), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({}))))), Nothing(), ()))), hydra.testing.TestGroup("insert", Nothing(), (), (hydra.testing.TestCaseWithMetadata("insert new key", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.insert(3, "c", FrozenDict({
      1: "a",
      2: "b"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "a",
      2: "b",
      3: "c"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("update existing", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.insert(2, "updated", FrozenDict({
      1: "a",
      2: "b"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "a",
      2: "updated"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("insert into empty", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.insert(1, "x", FrozenDict({}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "x"}))))), Nothing(), ()))), hydra.testing.TestGroup("keys", Nothing(), (), (hydra.testing.TestCaseWithMetadata("get all keys", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.list((lambda n: hydra.lib.literals.show_int32(n)), hydra.lib.maps.keys(FrozenDict({
      1: "a",
      2: "b",
      3: "c"}))), hydra.show.core.list((lambda n: hydra.lib.literals.show_int32(n)), (1, 2, 3))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("unsorted keys", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.list((lambda n: hydra.lib.literals.show_int32(n)), hydra.lib.maps.keys(FrozenDict({
      1: "a",
      2: "b",
      3: "c"}))), hydra.show.core.list((lambda n: hydra.lib.literals.show_int32(n)), (1, 2, 3))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("empty map", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.list((lambda n: hydra.lib.literals.show_int32(n)), hydra.lib.maps.keys(FrozenDict({}))), hydra.show.core.list((lambda n: hydra.lib.literals.show_int32(n)), ())))), Nothing(), ()))), hydra.testing.TestGroup("lookup", Nothing(), (), (hydra.testing.TestCaseWithMetadata("find existing key", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.lookup(2, FrozenDict({
      1: "a",
      2: "b"}))), hydra.show.core.maybe((lambda s: hydra.lib.literals.show_string(s)), Just("b"))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("key not found", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.lookup(3, FrozenDict({
      1: "a",
      2: "b"}))), hydra.show.core.maybe((lambda s: hydra.lib.literals.show_string(s)), Nothing())))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("lookup in empty", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.maybe((lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.lookup(1, FrozenDict({}))), hydra.show.core.maybe((lambda s: hydra.lib.literals.show_string(s)), Nothing())))), Nothing(), ()))), hydra.testing.TestGroup("map", Nothing(), (), (hydra.testing.TestCaseWithMetadata("map over values", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.map((lambda s: hydra.lib.strings.to_upper(s)), FrozenDict({
      1: "a",
      2: "b"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "A",
      2: "B"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("map empty", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.map((lambda s: hydra.lib.strings.to_upper(s)), FrozenDict({}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({}))))), Nothing(), ()))), hydra.testing.TestGroup("mapKeys", Nothing(), (), (hydra.testing.TestCaseWithMetadata("double keys", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.map_keys((lambda k: hydra.lib.math.mul(k, 2)), FrozenDict({
      1: "a",
      2: "b"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      2: "a",
      4: "b"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("empty map", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.map_keys((lambda k: hydra.lib.math.mul(k, 2)), FrozenDict({}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({}))))), Nothing(), ()))), hydra.testing.TestGroup("member", Nothing(), (), (hydra.testing.TestCaseWithMetadata("key exists", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.literals.show_boolean(hydra.lib.maps.member(2, FrozenDict({
      1: "a",
      2: "b"}))), hydra.lib.literals.show_boolean(True)))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("key missing", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.literals.show_boolean(hydra.lib.maps.member(3, FrozenDict({
      1: "a",
      2: "b"}))), hydra.lib.literals.show_boolean(False)))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("empty map", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.literals.show_boolean(hydra.lib.maps.member(1, FrozenDict({}))), hydra.lib.literals.show_boolean(False)))), Nothing(), ()))), hydra.testing.TestGroup("null", Nothing(), (), (hydra.testing.TestCaseWithMetadata("empty map", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.literals.show_boolean(hydra.lib.maps.null(FrozenDict({}))), hydra.lib.literals.show_boolean(True)))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("non-empty map", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.literals.show_boolean(hydra.lib.maps.null(FrozenDict({
      1: "a"}))), hydra.lib.literals.show_boolean(False)))), Nothing(), ()))), hydra.testing.TestGroup("remove", Nothing(), (), (hydra.testing.TestCaseWithMetadata("remove existing", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.delete(2, FrozenDict({
      1: "a",
      2: "b",
      3: "c"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "a",
      3: "c"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("remove non-existing", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.delete(4, FrozenDict({
      1: "a",
      2: "b"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "a",
      2: "b"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("remove from empty", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.delete(1, FrozenDict({}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({}))))), Nothing(), ()))), hydra.testing.TestGroup("singleton", Nothing(), (), (hydra.testing.TestCaseWithMetadata("single entry", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.singleton(42, "hello")), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      42: "hello"}))))), Nothing(), ()),)), hydra.testing.TestGroup("size", Nothing(), (), (hydra.testing.TestCaseWithMetadata("three entries", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.literals.show_int32(hydra.lib.maps.size(FrozenDict({
      1: "a",
      2: "b",
      3: "c"}))), hydra.lib.literals.show_int32(3)))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("single entry", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.literals.show_int32(hydra.lib.maps.size(FrozenDict({
      42: "test"}))), hydra.lib.literals.show_int32(1)))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("empty map", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.lib.literals.show_int32(hydra.lib.maps.size(FrozenDict({}))), hydra.lib.literals.show_int32(0)))), Nothing(), ()))), hydra.testing.TestGroup("toList", Nothing(), (), (hydra.testing.TestCaseWithMetadata("convert to pairs", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.list((lambda p: hydra.show.core.pair((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), p)), hydra.lib.maps.to_list(FrozenDict({
      1: "a",
      2: "b"}))), hydra.show.core.list((lambda p: hydra.show.core.pair((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), p)), ((1, "a"), (2, "b")))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("unsorted keys", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.list((lambda p: hydra.show.core.pair((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), p)), hydra.lib.maps.to_list(FrozenDict({
      1: "a",
      2: "b",
      3: "c"}))), hydra.show.core.list((lambda p: hydra.show.core.pair((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), p)), ((1, "a"), (2, "b"), (3, "c")))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("empty map", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.list((lambda p: hydra.show.core.pair((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), p)), hydra.lib.maps.to_list(FrozenDict({}))), hydra.show.core.list((lambda p: hydra.show.core.pair((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), p)), ())))), Nothing(), ()))), hydra.testing.TestGroup("union", Nothing(), (), (hydra.testing.TestCaseWithMetadata("union two maps", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.union(FrozenDict({
      1: "a",
      2: "b"}), FrozenDict({
      2: "x",
      3: "c"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "a",
      2: "b",
      3: "c"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("union with empty", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.union(FrozenDict({
      1: "a"}), FrozenDict({}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "a"}))))), Nothing(), ()), hydra.testing.TestCaseWithMetadata("empty with map", cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(hydra.testing.UniversalTestCase(hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), hydra.lib.maps.union(FrozenDict({}), FrozenDict({
      1: "a"}))), hydra.show.core.map((lambda n: hydra.lib.literals.show_int32(n)), (lambda s: hydra.lib.literals.show_string(s)), FrozenDict({
      1: "a"}))))), Nothing(), ())))), ())
