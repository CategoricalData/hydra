"""Python implementations of primitive name constants and registration functions."""


from hydra.core import Name
from hydra.graph import Primitive
from hydra.dsl import prims


def qname(namespace: str, local_name: str) -> Name:
    """Qualified name constructor."""
    return Name(f"{namespace}.{local_name}")


def register_chars_primitives() -> dict[Name, Primitive]:
    """Register all chars primitive functions."""
    from hydra.lib import chars

    namespace = "hydra.lib.chars"
    primitives: dict[Name, Primitive] = {}

    primitives[qname(namespace, "isAlphaNum")] = prims.prim1(
        qname(namespace, "isAlphaNum"), chars.is_alpha_num, [], prims.int32(), prims.boolean()
    )
    primitives[qname(namespace, "isLower")] = prims.prim1(
        qname(namespace, "isLower"), chars.is_lower, [], prims.int32(), prims.boolean()
    )
    primitives[qname(namespace, "isSpace")] = prims.prim1(
        qname(namespace, "isSpace"), chars.is_space, [], prims.int32(), prims.boolean()
    )
    primitives[qname(namespace, "isUpper")] = prims.prim1(
        qname(namespace, "isUpper"), chars.is_upper, [], prims.int32(), prims.boolean()
    )
    primitives[qname(namespace, "toLower")] = prims.prim1(
        qname(namespace, "toLower"), chars.to_lower, [], prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "toUpper")] = prims.prim1(
        qname(namespace, "toUpper"), chars.to_upper, [], prims.int32(), prims.int32()
    )

    return primitives


def register_equality_primitives() -> dict[Name, Primitive]:
    """Register all equality primitive functions."""
    from hydra.lib import equality

    namespace = "hydra.lib.equality"
    primitives: dict[Name, Primitive] = {}

    x = prims.variable("x")

    # Note: Equality primitives are polymorphic
    primitives[qname(namespace, "compare")] = prims.prim2(
        qname(namespace, "compare"), equality.compare, ["x"],
        x, x, prims.comparison()
    )
    primitives[qname(namespace, "equal")] = prims.prim2(
        qname(namespace, "equal"), equality.equal, ["x"],
        x, x, prims.boolean()
    )
    primitives[qname(namespace, "identity")] = prims.prim1(
        qname(namespace, "identity"), equality.identity, ["x"],
        x, x
    )
    primitives[qname(namespace, "gt")] = prims.prim2(
        qname(namespace, "gt"), equality.gt, ["x"],
        x, x, prims.boolean()
    )
    primitives[qname(namespace, "gte")] = prims.prim2(
        qname(namespace, "gte"), equality.gte, ["x"],
        x, x, prims.boolean()
    )
    primitives[qname(namespace, "lt")] = prims.prim2(
        qname(namespace, "lt"), equality.lt, ["x"],
        x, x, prims.boolean()
    )
    primitives[qname(namespace, "lte")] = prims.prim2(
        qname(namespace, "lte"), equality.lte, ["x"],
        x, x, prims.boolean()
    )
    primitives[qname(namespace, "max")] = prims.prim2(
        qname(namespace, "max"), equality.max, ["x"],
        x, x, x
    )
    primitives[qname(namespace, "min")] = prims.prim2(
        qname(namespace, "min"), equality.min, ["x"],
        x, x, x
    )

    return primitives


def register_eithers_primitives() -> dict[Name, Primitive]:
    """Register all eithers primitive functions."""
    from hydra.lib import eithers

    namespace = "hydra.lib.eithers"
    primitives: dict[Name, Primitive] = {}

    x = prims.variable("x")
    y = prims.variable("y")

    # Note: 'either' function is special and would need interpreter support
    # For now we register the simpler functions
    primitives[qname(namespace, "fromLeft")] = prims.prim2(
        qname(namespace, "fromLeft"), eithers.from_left, ["x", "y"],
        x, prims.either_(x, y), x
    )
    primitives[qname(namespace, "fromRight")] = prims.prim2(
        qname(namespace, "fromRight"), eithers.from_right, ["x", "y"],
        y, prims.either_(x, y), y
    )
    primitives[qname(namespace, "isLeft")] = prims.prim1(
        qname(namespace, "isLeft"), eithers.is_left, ["x", "y"],
        prims.either_(x, y), prims.boolean()
    )
    primitives[qname(namespace, "isRight")] = prims.prim1(
        qname(namespace, "isRight"), eithers.is_right, ["x", "y"],
        prims.either_(x, y), prims.boolean()
    )
    primitives[qname(namespace, "lefts")] = prims.prim1(
        qname(namespace, "lefts"), eithers.lefts, ["x", "y"],
        prims.list_(prims.either_(x, y)), prims.list_(x)
    )
    primitives[qname(namespace, "partitionEithers")] = prims.prim1(
        qname(namespace, "partitionEithers"), eithers.partition_eithers, ["x", "y"],
        prims.list_(prims.either_(x, y)), prims.pair(prims.list_(x), prims.list_(y))
    )
    primitives[qname(namespace, "rights")] = prims.prim1(
        qname(namespace, "rights"), eithers.rights, ["x", "y"],
        prims.list_(prims.either_(x, y)), prims.list_(y)
    )

    return primitives


def register_flows_primitives() -> dict[Name, Primitive]:
    """Register all flows primitive functions."""
    from hydra.lib import flows

    namespace = "hydra.lib.flows"
    primitives: dict[Name, Primitive] = {}

    s = prims.variable("s")
    x = prims.variable("x")
    y = prims.variable("y")
    k = prims.variable("k")
    v1 = prims.variable("v1")
    v2 = prims.variable("v2")

    primitives[qname(namespace, "apply")] = prims.prim2(
        qname(namespace, "apply"), flows.apply, ["s", "x", "y"],
        prims.flow(s, prims.function(x, y)), prims.flow(s, x), prims.flow(s, y)
    )
    primitives[qname(namespace, "bind")] = prims.prim2(
        qname(namespace, "bind"), flows.bind, ["s", "x", "y"],
        prims.flow(s, x), prims.function(x, prims.flow(s, y)), prims.flow(s, y)
    )
    primitives[qname(namespace, "fail")] = prims.prim1(
        qname(namespace, "fail"), flows.fail, ["s", "x"],
        prims.string(), prims.flow(s, x)
    )
    # Note: foldl needs a curried function, but prim3 expects uncurried.
    # This may need special handling or interpreter support.
    # primitives[qname(namespace, "foldl")] = prims.prim3(
    #     qname(namespace, "foldl"), flows.foldl, ["y", "x", "s"],
    #     prims.function(y, prims.function(x, prims.flow(s, y))), y, prims.list_(x), prims.flow(s, y)
    # )
    primitives[qname(namespace, "map")] = prims.prim2(
        qname(namespace, "map"), flows.map, ["x", "y", "s"],
        prims.function(x, y), prims.flow(s, x), prims.flow(s, y)
    )
    primitives[qname(namespace, "mapElems")] = prims.prim2(
        qname(namespace, "mapElems"), flows.map_elems, ["v1", "s", "v2", "k"],
        prims.function(v1, prims.flow(s, v2)), prims.map_(k, v1), prims.flow(s, prims.map_(k, v2))
    )
    primitives[qname(namespace, "mapKeys")] = prims.prim2(
        qname(namespace, "mapKeys"), flows.map_keys, ["k1", "s", "k2", "v"],
        prims.function(prims.variable("k1"), prims.flow(s, prims.variable("k2"))),
        prims.map_(prims.variable("k1"), prims.variable("v")),
        prims.flow(s, prims.map_(prims.variable("k2"), prims.variable("v")))
    )
    primitives[qname(namespace, "mapList")] = prims.prim2(
        qname(namespace, "mapList"), flows.map_list, ["x", "s", "y"],
        prims.function(x, prims.flow(s, y)), prims.list_(x), prims.flow(s, prims.list_(y))
    )
    primitives[qname(namespace, "mapMaybe")] = prims.prim2(
        qname(namespace, "mapMaybe"), flows.map_maybe, ["x", "s", "y"],
        prims.function(x, prims.flow(s, y)), prims.optional(x), prims.flow(s, prims.optional(y))
    )
    primitives[qname(namespace, "mapSet")] = prims.prim2(
        qname(namespace, "mapSet"), flows.map_set, ["x", "s", "y"],
        prims.function(x, prims.flow(s, y)), prims.set_(x), prims.flow(s, prims.set_(y))
    )
    primitives[qname(namespace, "pure")] = prims.prim1(
        qname(namespace, "pure"), flows.pure, ["x", "s"],
        x, prims.flow(s, x)
    )
    primitives[qname(namespace, "sequence")] = prims.prim1(
        qname(namespace, "sequence"), flows.sequence, ["s", "x"],
        prims.list_(prims.flow(s, x)), prims.flow(s, prims.list_(x))
    )

    return primitives


def register_lists_primitives() -> dict[Name, Primitive]:
    """Register all list primitive functions."""
    from hydra.lib import lists

    namespace = "hydra.lib.lists"
    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")
    b = prims.variable("b")

    primitives[qname(namespace, "concat")] = prims.prim1(
        qname(namespace, "concat"), lists.concat, ["a"],
        prims.list_(prims.list_(a)), prims.list_(a)
    )
    primitives[qname(namespace, "head")] = prims.prim1(
        qname(namespace, "head"), lists.head, ["a"], prims.list_(a), a
    )
    primitives[qname(namespace, "intercalate")] = prims.prim2(
        qname(namespace, "intercalate"), lists.intercalate, ["a"],
        prims.list_(a), prims.list_(prims.list_(a)), prims.list_(a)
    )
    primitives[qname(namespace, "intersperse")] = prims.prim2(
        qname(namespace, "intersperse"), lists.intersperse, ["a"],
        a, prims.list_(a), prims.list_(a)
    )
    primitives[qname(namespace, "last")] = prims.prim1(
        qname(namespace, "last"), lists.last, ["a"], prims.list_(a), a
    )
    primitives[qname(namespace, "length")] = prims.prim1(
        qname(namespace, "length"), lists.length, ["a"], prims.list_(a), prims.int32()
    )
    primitives[qname(namespace, "reverse")] = prims.prim1(
        qname(namespace, "reverse"), lists.reverse, ["a"], prims.list_(a), prims.list_(a)
    )

    return primitives


def register_logic_primitives() -> dict[Name, Primitive]:
    """Register all logic primitive functions."""
    from hydra.lib import logic

    namespace = "hydra.lib.logic"
    primitives: dict[Name, Primitive] = {}

    primitives[qname(namespace, "and")] = prims.prim2(
        qname(namespace, "and"), logic.and_, [], prims.boolean(), prims.boolean(), prims.boolean()
    )
    primitives[qname(namespace, "not")] = prims.prim1(
        qname(namespace, "not"), logic.not_, [], prims.boolean(), prims.boolean()
    )
    primitives[qname(namespace, "or")] = prims.prim2(
        qname(namespace, "or"), logic.or_, [], prims.boolean(), prims.boolean(), prims.boolean()
    )

    return primitives


def register_maps_primitives() -> dict[Name, Primitive]:
    """Register all map primitive functions."""
    from hydra.lib import maps

    namespace = "hydra.lib.maps"
    primitives: dict[Name, Primitive] = {}

    k = prims.variable("k")
    v = prims.variable("v")

    primitives[qname(namespace, "null")] = prims.prim1(
        qname(namespace, "null"), maps.null, ["k", "v"],
        prims.map_(k, v), prims.boolean()
    )
    primitives[qname(namespace, "keys")] = prims.prim1(
        qname(namespace, "keys"), maps.keys, ["k", "v"],
        prims.map_(k, v), prims.list_(k)
    )
    primitives[qname(namespace, "size")] = prims.prim1(
        qname(namespace, "size"), maps.size, ["k", "v"],
        prims.map_(k, v), prims.int32()
    )

    return primitives


def register_math_primitives() -> dict[Name, Primitive]:
    """Register all math primitive functions."""
    from hydra.lib import math

    namespace = "hydra.lib.math"
    primitives: dict[Name, Primitive] = {}

    # Int32 primitives
    primitives[qname(namespace, "abs")] = prims.prim1(
        qname(namespace, "abs"), math.abs_, [], prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "add")] = prims.prim2(
        qname(namespace, "add"), math.add, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "div")] = prims.prim2(
        qname(namespace, "div"), math.div, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "even")] = prims.prim1(
        qname(namespace, "even"), math.even, [], prims.int32(), prims.boolean()
    )
    primitives[qname(namespace, "mod")] = prims.prim2(
        qname(namespace, "mod"), math.mod, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "mul")] = prims.prim2(
        qname(namespace, "mul"), math.mul, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "negate")] = prims.prim1(
        qname(namespace, "negate"), math.negate, [], prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "odd")] = prims.prim1(
        qname(namespace, "odd"), math.odd, [], prims.int32(), prims.boolean()
    )
    primitives[qname(namespace, "pred")] = prims.prim1(
        qname(namespace, "pred"), math.pred, [], prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "range")] = prims.prim2(
        qname(namespace, "range"), math.range_, [], prims.int32(), prims.int32(), prims.list_(prims.int32())
    )
    primitives[qname(namespace, "rem")] = prims.prim2(
        qname(namespace, "rem"), math.rem, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "signum")] = prims.prim1(
        qname(namespace, "signum"), math.signum, [], prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "sub")] = prims.prim2(
        qname(namespace, "sub"), math.sub, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "succ")] = prims.prim1(
        qname(namespace, "succ"), math.succ, [], prims.int32(), prims.int32()
    )

    # Float64 primitives
    primitives[qname(namespace, "acos")] = prims.prim1(
        qname(namespace, "acos"), math.acos, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "acosh")] = prims.prim1(
        qname(namespace, "acosh"), math.acosh, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "asin")] = prims.prim1(
        qname(namespace, "asin"), math.asin, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "asinh")] = prims.prim1(
        qname(namespace, "asinh"), math.asinh, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "atan")] = prims.prim1(
        qname(namespace, "atan"), math.atan, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "atan2")] = prims.prim2(
        qname(namespace, "atan2"), math.atan2, [], prims.float64(), prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "atanh")] = prims.prim1(
        qname(namespace, "atanh"), math.atanh, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "ceiling")] = prims.prim1(
        qname(namespace, "ceiling"), math.ceiling, [], prims.float64(), prims.bigint()
    )
    primitives[qname(namespace, "cos")] = prims.prim1(
        qname(namespace, "cos"), math.cos, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "cosh")] = prims.prim1(
        qname(namespace, "cosh"), math.cosh, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "e")] = prims.prim0(
        qname(namespace, "e"), math.e, [], prims.float64()
    )
    primitives[qname(namespace, "exp")] = prims.prim1(
        qname(namespace, "exp"), math.exp, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "floor")] = prims.prim1(
        qname(namespace, "floor"), math.floor, [], prims.float64(), prims.bigint()
    )
    primitives[qname(namespace, "log")] = prims.prim1(
        qname(namespace, "log"), math.log, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "logBase")] = prims.prim2(
        qname(namespace, "logBase"), math.log_base, [], prims.float64(), prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "pi")] = prims.prim0(
        qname(namespace, "pi"), math.pi, [], prims.float64()
    )
    primitives[qname(namespace, "pow")] = prims.prim2(
        qname(namespace, "pow"), math.pow_, [], prims.float64(), prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "round")] = prims.prim1(
        qname(namespace, "round"), math.round_, [], prims.float64(), prims.bigint()
    )
    primitives[qname(namespace, "sin")] = prims.prim1(
        qname(namespace, "sin"), math.sin, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "sinh")] = prims.prim1(
        qname(namespace, "sinh"), math.sinh, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "sqrt")] = prims.prim1(
        qname(namespace, "sqrt"), math.sqrt, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "tan")] = prims.prim1(
        qname(namespace, "tan"), math.tan, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "tanh")] = prims.prim1(
        qname(namespace, "tanh"), math.tanh, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "truncate")] = prims.prim1(
        qname(namespace, "truncate"), math.truncate, [], prims.float64(), prims.bigint()
    )

    return primitives


def register_maybes_primitives() -> dict[Name, Primitive]:
    """Register all maybe primitive functions."""
    from hydra.lib import maybes

    namespace = "hydra.lib.maybes"
    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")

    primitives[qname(namespace, "isJust")] = prims.prim1(
        qname(namespace, "isJust"), maybes.is_just, ["a"],
        prims.optional(a), prims.boolean()
    )
    primitives[qname(namespace, "isNothing")] = prims.prim1(
        qname(namespace, "isNothing"), maybes.is_nothing, ["a"],
        prims.optional(a), prims.boolean()
    )

    return primitives


def register_sets_primitives() -> dict[Name, Primitive]:
    """Register all set primitive functions."""
    from hydra.lib import sets

    namespace = "hydra.lib.sets"
    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")

    primitives[qname(namespace, "delete")] = prims.prim2(
        qname(namespace, "delete"), sets.delete, ["a"],
        a, prims.set_(a), prims.set_(a)
    )
    primitives[qname(namespace, "difference")] = prims.prim2(
        qname(namespace, "difference"), sets.difference, ["a"],
        prims.set_(a), prims.set_(a), prims.set_(a)
    )
    # Note: empty() is a function that takes no arguments and returns an empty set
    # But prims.prim0 expects a constant value, not a function
    # This needs special handling
    # primitives[qname(namespace, "empty")] = prims.prim0(
    #     qname(namespace, "empty"), sets.empty, ["a"],
    #     prims.set_(a)
    # )
    primitives[qname(namespace, "fromList")] = prims.prim1(
        qname(namespace, "fromList"), sets.from_list, ["a"],
        prims.list_(a), prims.set_(a)
    )
    primitives[qname(namespace, "insert")] = prims.prim2(
        qname(namespace, "insert"), sets.insert, ["a"],
        a, prims.set_(a), prims.set_(a)
    )
    primitives[qname(namespace, "intersection")] = prims.prim2(
        qname(namespace, "intersection"), sets.intersection, ["a"],
        prims.set_(a), prims.set_(a), prims.set_(a)
    )
    primitives[qname(namespace, "map")] = prims.prim2(
        qname(namespace, "map"), sets.map, ["a", "b"],
        prims.function(a, prims.variable("b")), prims.set_(a), prims.set_(prims.variable("b"))
    )
    primitives[qname(namespace, "member")] = prims.prim2(
        qname(namespace, "member"), sets.member, ["a"],
        a, prims.set_(a), prims.boolean()
    )
    primitives[qname(namespace, "null")] = prims.prim1(
        qname(namespace, "null"), sets.null, ["a"],
        prims.set_(a), prims.boolean()
    )
    primitives[qname(namespace, "singleton")] = prims.prim1(
        qname(namespace, "singleton"), sets.singleton, ["a"],
        a, prims.set_(a)
    )
    primitives[qname(namespace, "size")] = prims.prim1(
        qname(namespace, "size"), sets.size, ["a"],
        prims.set_(a), prims.int32()
    )
    primitives[qname(namespace, "toList")] = prims.prim1(
        qname(namespace, "toList"), sets.to_list, ["a"],
        prims.set_(a), prims.list_(a)
    )
    primitives[qname(namespace, "union")] = prims.prim2(
        qname(namespace, "union"), sets.union, ["a"],
        prims.set_(a), prims.set_(a), prims.set_(a)
    )
    primitives[qname(namespace, "unions")] = prims.prim1(
        qname(namespace, "unions"), sets.unions, ["a"],
        prims.list_(prims.set_(a)), prims.set_(a)
    )

    return primitives


def register_strings_primitives() -> dict[Name, Primitive]:
    """Register all string primitive functions."""
    from hydra.lib import strings

    namespace = "hydra.lib.strings"
    primitives: dict[Name, Primitive] = {}

    primitives[qname(namespace, "cat")] = prims.prim1(
        qname(namespace, "cat"), strings.cat, [], prims.list_(prims.string()), prims.string()
    )
    primitives[qname(namespace, "cat2")] = prims.prim2(
        qname(namespace, "cat2"), strings.cat2, [], prims.string(), prims.string(), prims.string()
    )
    primitives[qname(namespace, "charAt")] = prims.prim2(
        qname(namespace, "charAt"), strings.char_at, [], prims.int32(), prims.string(), prims.int32()
    )
    primitives[qname(namespace, "fromList")] = prims.prim1(
        qname(namespace, "fromList"), strings.from_list, [], prims.list_(prims.int32()), prims.string()
    )
    primitives[qname(namespace, "intercalate")] = prims.prim2(
        qname(namespace, "intercalate"), strings.intercalate, [], prims.string(), prims.list_(prims.string()), prims.string()
    )
    primitives[qname(namespace, "length")] = prims.prim1(
        qname(namespace, "length"), strings.length, [], prims.string(), prims.int32()
    )
    primitives[qname(namespace, "lines")] = prims.prim1(
        qname(namespace, "lines"), strings.lines, [], prims.string(), prims.list_(prims.string())
    )
    primitives[qname(namespace, "null")] = prims.prim1(
        qname(namespace, "null"), strings.null, [], prims.string(), prims.boolean()
    )
    primitives[qname(namespace, "splitOn")] = prims.prim2(
        qname(namespace, "splitOn"), strings.split_on, [], prims.string(), prims.string(),
        prims.list_(prims.string())
    )
    primitives[qname(namespace, "toList")] = prims.prim1(
        qname(namespace, "toList"), strings.to_list, [], prims.string(), prims.list_(prims.int32())
    )
    primitives[qname(namespace, "toLower")] = prims.prim1(
        qname(namespace, "toLower"), strings.to_lower, [], prims.string(), prims.string()
    )
    primitives[qname(namespace, "toUpper")] = prims.prim1(
        qname(namespace, "toUpper"), strings.to_upper, [], prims.string(), prims.string()
    )
    primitives[qname(namespace, "unlines")] = prims.prim1(
        qname(namespace, "unlines"), strings.unlines, [], prims.list_(prims.string()), prims.string()
    )

    return primitives


def register_literals_primitives() -> dict[Name, Primitive]:
    """Register all literals primitive functions."""
    from hydra.lib import literals

    namespace = "hydra.lib.literals"
    primitives: dict[Name, Primitive] = {}

    # Conversion primitives
    primitives[qname(namespace, "bigfloatToBigint")] = prims.prim1(
        qname(namespace, "bigfloatToBigint"), literals.bigfloat_to_bigint, [],
        prims.bigfloat(), prims.bigint()
    )
    primitives[qname(namespace, "bigfloatToFloat32")] = prims.prim1(
        qname(namespace, "bigfloatToFloat32"), literals.bigfloat_to_float32, [],
        prims.bigfloat(), prims.float32()
    )
    primitives[qname(namespace, "bigfloatToFloat64")] = prims.prim1(
        qname(namespace, "bigfloatToFloat64"), literals.bigfloat_to_float64, [],
        prims.bigfloat(), prims.float64()
    )
    primitives[qname(namespace, "bigintToBigfloat")] = prims.prim1(
        qname(namespace, "bigintToBigfloat"), literals.bigint_to_bigfloat, [],
        prims.bigint(), prims.bigfloat()
    )
    primitives[qname(namespace, "bigintToInt8")] = prims.prim1(
        qname(namespace, "bigintToInt8"), literals.bigint_to_int8, [],
        prims.bigint(), prims.int8()
    )
    primitives[qname(namespace, "bigintToInt16")] = prims.prim1(
        qname(namespace, "bigintToInt16"), literals.bigint_to_int16, [],
        prims.bigint(), prims.int16()
    )
    primitives[qname(namespace, "bigintToInt32")] = prims.prim1(
        qname(namespace, "bigintToInt32"), literals.bigint_to_int32, [],
        prims.bigint(), prims.int32()
    )
    primitives[qname(namespace, "bigintToInt64")] = prims.prim1(
        qname(namespace, "bigintToInt64"), literals.bigint_to_int64, [],
        prims.bigint(), prims.int64()
    )
    primitives[qname(namespace, "bigintToUint8")] = prims.prim1(
        qname(namespace, "bigintToUint8"), literals.bigint_to_uint8, [],
        prims.bigint(), prims.uint8()
    )
    primitives[qname(namespace, "bigintToUint16")] = prims.prim1(
        qname(namespace, "bigintToUint16"), literals.bigint_to_uint16, [],
        prims.bigint(), prims.uint16()
    )
    primitives[qname(namespace, "bigintToUint32")] = prims.prim1(
        qname(namespace, "bigintToUint32"), literals.bigint_to_uint32, [],
        prims.bigint(), prims.uint32()
    )
    primitives[qname(namespace, "bigintToUint64")] = prims.prim1(
        qname(namespace, "bigintToUint64"), literals.bigint_to_uint64, [],
        prims.bigint(), prims.uint64()
    )
    primitives[qname(namespace, "binaryToString")] = prims.prim1(
        qname(namespace, "binaryToString"), literals.binary_to_string, [],
        prims.binary(), prims.string()
    )
    primitives[qname(namespace, "float32ToBigfloat")] = prims.prim1(
        qname(namespace, "float32ToBigfloat"), literals.float32_to_bigfloat, [],
        prims.float32(), prims.bigfloat()
    )
    primitives[qname(namespace, "float64ToBigfloat")] = prims.prim1(
        qname(namespace, "float64ToBigfloat"), literals.float64_to_bigfloat, [],
        prims.float64(), prims.bigfloat()
    )
    primitives[qname(namespace, "int8ToBigint")] = prims.prim1(
        qname(namespace, "int8ToBigint"), literals.int8_to_bigint, [],
        prims.int8(), prims.bigint()
    )
    primitives[qname(namespace, "int16ToBigint")] = prims.prim1(
        qname(namespace, "int16ToBigint"), literals.int16_to_bigint, [],
        prims.int16(), prims.bigint()
    )
    primitives[qname(namespace, "int32ToBigint")] = prims.prim1(
        qname(namespace, "int32ToBigint"), literals.int32_to_bigint, [],
        prims.int32(), prims.bigint()
    )
    primitives[qname(namespace, "int64ToBigint")] = prims.prim1(
        qname(namespace, "int64ToBigint"), literals.int64_to_bigint, [],
        prims.int64(), prims.bigint()
    )

    # Read primitives
    primitives[qname(namespace, "readBigfloat")] = prims.prim1(
        qname(namespace, "readBigfloat"), literals.read_bigfloat, [],
        prims.string(), prims.optional(prims.bigfloat())
    )
    primitives[qname(namespace, "readBoolean")] = prims.prim1(
        qname(namespace, "readBoolean"), literals.read_boolean, [],
        prims.string(), prims.optional(prims.boolean())
    )
    primitives[qname(namespace, "readFloat32")] = prims.prim1(
        qname(namespace, "readFloat32"), literals.read_float32, [],
        prims.string(), prims.optional(prims.float32())
    )
    primitives[qname(namespace, "readFloat64")] = prims.prim1(
        qname(namespace, "readFloat64"), literals.read_float64, [],
        prims.string(), prims.optional(prims.float64())
    )
    primitives[qname(namespace, "readInt32")] = prims.prim1(
        qname(namespace, "readInt32"), literals.read_int32, [],
        prims.string(), prims.optional(prims.int32())
    )
    primitives[qname(namespace, "readInt64")] = prims.prim1(
        qname(namespace, "readInt64"), literals.read_int64, [],
        prims.string(), prims.optional(prims.int64())
    )
    primitives[qname(namespace, "readString")] = prims.prim1(
        qname(namespace, "readString"), literals.read_string, [],
        prims.string(), prims.optional(prims.string())
    )

    # Show primitives
    primitives[qname(namespace, "showBigfloat")] = prims.prim1(
        qname(namespace, "showBigfloat"), literals.show_bigfloat, [],
        prims.bigfloat(), prims.string()
    )
    primitives[qname(namespace, "showBigint")] = prims.prim1(
        qname(namespace, "showBigint"), literals.show_bigint, [],
        prims.bigint(), prims.string()
    )
    primitives[qname(namespace, "showBoolean")] = prims.prim1(
        qname(namespace, "showBoolean"), literals.show_boolean, [],
        prims.boolean(), prims.string()
    )
    primitives[qname(namespace, "showFloat32")] = prims.prim1(
        qname(namespace, "showFloat32"), literals.show_float32, [],
        prims.float32(), prims.string()
    )
    primitives[qname(namespace, "showFloat64")] = prims.prim1(
        qname(namespace, "showFloat64"), literals.show_float64, [],
        prims.float64(), prims.string()
    )
    primitives[qname(namespace, "showInt8")] = prims.prim1(
        qname(namespace, "showInt8"), literals.show_int8, [],
        prims.int8(), prims.string()
    )
    primitives[qname(namespace, "showInt16")] = prims.prim1(
        qname(namespace, "showInt16"), literals.show_int16, [],
        prims.int16(), prims.string()
    )
    primitives[qname(namespace, "showInt32")] = prims.prim1(
        qname(namespace, "showInt32"), literals.show_int32, [],
        prims.int32(), prims.string()
    )
    primitives[qname(namespace, "showInt64")] = prims.prim1(
        qname(namespace, "showInt64"), literals.show_int64, [],
        prims.int64(), prims.string()
    )
    primitives[qname(namespace, "showUint8")] = prims.prim1(
        qname(namespace, "showUint8"), literals.show_uint8, [],
        prims.uint8(), prims.string()
    )
    primitives[qname(namespace, "showUint16")] = prims.prim1(
        qname(namespace, "showUint16"), literals.show_uint16, [],
        prims.uint16(), prims.string()
    )
    primitives[qname(namespace, "showUint32")] = prims.prim1(
        qname(namespace, "showUint32"), literals.show_uint32, [],
        prims.uint32(), prims.string()
    )
    primitives[qname(namespace, "showUint64")] = prims.prim1(
        qname(namespace, "showUint64"), literals.show_uint64, [],
        prims.uint64(), prims.string()
    )
    primitives[qname(namespace, "showString")] = prims.prim1(
        qname(namespace, "showString"), literals.show_string, [],
        prims.string(), prims.string()
    )
    primitives[qname(namespace, "stringToBinary")] = prims.prim1(
        qname(namespace, "stringToBinary"), literals.string_to_binary, [],
        prims.string(), prims.binary()
    )
    primitives[qname(namespace, "uint8ToBigint")] = prims.prim1(
        qname(namespace, "uint8ToBigint"), literals.uint8_to_bigint, [],
        prims.uint8(), prims.bigint()
    )
    primitives[qname(namespace, "uint16ToBigint")] = prims.prim1(
        qname(namespace, "uint16ToBigint"), literals.uint16_to_bigint, [],
        prims.uint16(), prims.bigint()
    )
    primitives[qname(namespace, "uint32ToBigint")] = prims.prim1(
        qname(namespace, "uint32ToBigint"), literals.uint32_to_bigint, [],
        prims.uint32(), prims.bigint()
    )
    primitives[qname(namespace, "uint64ToBigint")] = prims.prim1(
        qname(namespace, "uint64ToBigint"), literals.uint64_to_bigint, [],
        prims.uint64(), prims.bigint()
    )

    return primitives


def register_tuples_primitives() -> dict[Name, Primitive]:
    """Register all tuples primitive functions."""
    from hydra.lib import tuples

    namespace = "hydra.lib.tuples"
    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")
    b = prims.variable("b")
    c = prims.variable("c")

    primitives[qname(namespace, "curry")] = prims.prim1(
        qname(namespace, "curry"), tuples.curry, ["a", "b", "c"],
        prims.function(prims.pair(a, b), c), prims.function(a, prims.function(b, c))
    )
    primitives[qname(namespace, "fst")] = prims.prim1(
        qname(namespace, "fst"), tuples.fst, ["a", "b"],
        prims.pair(a, b), a
    )
    primitives[qname(namespace, "snd")] = prims.prim1(
        qname(namespace, "snd"), tuples.snd, ["a", "b"],
        prims.pair(a, b), b
    )
    primitives[qname(namespace, "uncurry")] = prims.prim1(
        qname(namespace, "uncurry"), tuples.uncurry, ["a", "b", "c"],
        prims.function(a, prims.function(b, c)), prims.function(prims.pair(a, b), c)
    )

    return primitives


def standard_library() -> dict[Name, Primitive]:
    """Get all standard library primitives."""
    primitives: dict[Name, Primitive] = {}
    primitives.update(register_chars_primitives())
    primitives.update(register_eithers_primitives())
    primitives.update(register_equality_primitives())
    primitives.update(register_flows_primitives())
    primitives.update(register_lists_primitives())
    primitives.update(register_literals_primitives())
    primitives.update(register_logic_primitives())
    primitives.update(register_maps_primitives())
    primitives.update(register_math_primitives())
    primitives.update(register_maybes_primitives())
    primitives.update(register_sets_primitives())
    primitives.update(register_strings_primitives())
    primitives.update(register_tuples_primitives())
    return primitives
