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
    from hydra.eval.lib import eithers as eval_eithers
    from hydra.dsl.python import Just

    namespace = "hydra.lib.eithers"
    primitives: dict[Name, Primitive] = {}

    x = prims.variable("x")
    y = prims.variable("y")
    z = prims.variable("z")

    # bimap :: (x -> y) -> (z -> w) -> Either x z -> Either y w
    w = prims.variable("w")
    primitives[qname(namespace, "bimap")] = prims.prim3_interp(
        qname(namespace, "bimap"), Just(eval_eithers.bimap), ["x", "y", "z", "w"],
        prims.function(x, y), prims.function(z, w), prims.either(x, z), prims.either(y, w)
    )
    # either :: (x -> z) -> (y -> z) -> Either x y -> z
    primitives[qname(namespace, "either")] = prims.prim3_interp(
        qname(namespace, "either"), Just(eval_eithers.either), ["x", "y", "z"],
        prims.function(x, z), prims.function(y, z), prims.either(x, y), z
    )
    primitives[qname(namespace, "fromLeft")] = prims.prim2(
        qname(namespace, "fromLeft"), eithers.from_left, ["x", "y"],
        x, prims.either(x, y), x
    )
    primitives[qname(namespace, "fromRight")] = prims.prim2(
        qname(namespace, "fromRight"), eithers.from_right, ["x", "y"],
        y, prims.either(x, y), y
    )
    primitives[qname(namespace, "isLeft")] = prims.prim1(
        qname(namespace, "isLeft"), eithers.is_left, ["x", "y"],
        prims.either(x, y), prims.boolean()
    )
    primitives[qname(namespace, "isRight")] = prims.prim1(
        qname(namespace, "isRight"), eithers.is_right, ["x", "y"],
        prims.either(x, y), prims.boolean()
    )
    primitives[qname(namespace, "lefts")] = prims.prim1(
        qname(namespace, "lefts"), eithers.lefts, ["x", "y"],
        prims.list_(prims.either(x, y)), prims.list_(x)
    )
    # map :: (x -> y) -> Either z x -> Either z y
    primitives[qname(namespace, "map")] = prims.prim2_interp(
        qname(namespace, "map"), Just(eval_eithers.map), ["x", "y", "z"],
        prims.function(x, y), prims.either(z, x), prims.either(z, y)
    )
    # mapList :: (x -> Either z y) -> [x] -> Either z [y]
    primitives[qname(namespace, "mapList")] = prims.prim2_interp(
        qname(namespace, "mapList"), Just(eval_eithers.map_list), ["x", "y", "z"],
        prims.function(x, prims.either(z, y)), prims.list_(x), prims.either(z, prims.list_(y))
    )
    # mapMaybe :: (x -> Either z y) -> Maybe x -> Either z (Maybe y)
    primitives[qname(namespace, "mapMaybe")] = prims.prim2_interp(
        qname(namespace, "mapMaybe"), Just(eval_eithers.map_maybe), ["x", "y", "z"],
        prims.function(x, prims.either(z, y)), prims.optional(x), prims.either(z, prims.optional(y))
    )
    primitives[qname(namespace, "partitionEithers")] = prims.prim1(
        qname(namespace, "partitionEithers"), eithers.partition_eithers, ["x", "y"],
        prims.list_(prims.either(x, y)), prims.pair(prims.list_(x), prims.list_(y))
    )
    primitives[qname(namespace, "rights")] = prims.prim1(
        qname(namespace, "rights"), eithers.rights, ["x", "y"],
        prims.list_(prims.either(x, y)), prims.list_(y)
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
    from hydra.eval.lib import lists as eval_lists
    from hydra.dsl.python import Nothing, Just

    namespace = "hydra.lib.lists"
    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")
    b = prims.variable("b")
    c = prims.variable("c")

    # prim2: apply :: [a -> b] -> [a] -> [b]
    primitives[qname(namespace, "apply")] = prims.prim2_interp(
        qname(namespace, "apply"), Just(eval_lists.apply), ["a", "b"],
        prims.list_(prims.function(a, b)), prims.list_(a), prims.list_(b)
    )
    # prim2: at :: Int32 -> [a] -> a
    primitives[qname(namespace, "at")] = prims.prim2(
        qname(namespace, "at"), lists.at, ["a"],
        prims.int32(), prims.list_(a), a
    )
    # prim2: bind :: [a] -> (a -> [b]) -> [b]
    primitives[qname(namespace, "bind")] = prims.prim2_interp(
        qname(namespace, "bind"), Just(eval_lists.bind), ["a", "b"],
        prims.list_(a), prims.function(a, prims.list_(b)), prims.list_(b)
    )
    # prim1: concat :: [[a]] -> [a]
    primitives[qname(namespace, "concat")] = prims.prim1(
        qname(namespace, "concat"), lists.concat, ["a"],
        prims.list_(prims.list_(a)), prims.list_(a)
    )
    # prim2: concat2 :: [a] -> [a] -> [a]
    primitives[qname(namespace, "concat2")] = prims.prim2(
        qname(namespace, "concat2"), lists.concat2, ["a"],
        prims.list_(a), prims.list_(a), prims.list_(a)
    )
    # prim2: cons :: a -> [a] -> [a]
    primitives[qname(namespace, "cons")] = prims.prim2(
        qname(namespace, "cons"), lists.cons, ["a"],
        a, prims.list_(a), prims.list_(a)
    )
    # prim2: drop :: Int32 -> [a] -> [a]
    primitives[qname(namespace, "drop")] = prims.prim2(
        qname(namespace, "drop"), lists.drop, ["a"],
        prims.int32(), prims.list_(a), prims.list_(a)
    )
    # prim2: dropWhile :: (a -> Bool) -> [a] -> [a]
    primitives[qname(namespace, "dropWhile")] = prims.prim2_interp(
        qname(namespace, "dropWhile"), Just(eval_lists.drop_while), ["a"],
        prims.function(a, prims.boolean()), prims.list_(a), prims.list_(a)
    )
    # prim2: elem :: a -> [a] -> Bool
    primitives[qname(namespace, "elem")] = prims.prim2(
        qname(namespace, "elem"), lists.elem, ["a"],
        a, prims.list_(a), prims.boolean()
    )
    # prim2: filter :: (a -> Bool) -> [a] -> [a]
    primitives[qname(namespace, "filter")] = prims.prim2_interp(
        qname(namespace, "filter"), Just(eval_lists.filter), ["a"],
        prims.function(a, prims.boolean()), prims.list_(a), prims.list_(a)
    )
    # prim3: foldl :: (b -> a -> b) -> b -> [a] -> b
    primitives[qname(namespace, "foldl")] = prims.prim3_interp(
        qname(namespace, "foldl"), Just(eval_lists.foldl), ["b", "a"],
        prims.function(b, prims.function(a, b)), b, prims.list_(a), b
    )
    # prim1: group :: [a] -> [[a]]
    primitives[qname(namespace, "group")] = prims.prim1(
        qname(namespace, "group"), lists.group, ["a"],
        prims.list_(a), prims.list_(prims.list_(a))
    )
    # prim1: head :: [a] -> a
    primitives[qname(namespace, "head")] = prims.prim1(
        qname(namespace, "head"), lists.head, ["a"], prims.list_(a), a
    )
    # prim1: init :: [a] -> [a]
    primitives[qname(namespace, "init")] = prims.prim1(
        qname(namespace, "init"), lists.init, ["a"], prims.list_(a), prims.list_(a)
    )
    # prim2: intercalate :: [a] -> [[a]] -> [a]
    primitives[qname(namespace, "intercalate")] = prims.prim2(
        qname(namespace, "intercalate"), lists.intercalate, ["a"],
        prims.list_(a), prims.list_(prims.list_(a)), prims.list_(a)
    )
    # prim2: intersperse :: a -> [a] -> [a]
    primitives[qname(namespace, "intersperse")] = prims.prim2(
        qname(namespace, "intersperse"), lists.intersperse, ["a"],
        a, prims.list_(a), prims.list_(a)
    )
    # prim1: last :: [a] -> a
    primitives[qname(namespace, "last")] = prims.prim1(
        qname(namespace, "last"), lists.last, ["a"], prims.list_(a), a
    )
    # prim1: length :: [a] -> Int32
    primitives[qname(namespace, "length")] = prims.prim1(
        qname(namespace, "length"), lists.length, ["a"], prims.list_(a), prims.int32()
    )
    # prim2: map :: (a -> b) -> [a] -> [b]
    primitives[qname(namespace, "map")] = prims.prim2_interp(
        qname(namespace, "map"), Just(eval_lists.map), ["a", "b"],
        prims.function(a, b), prims.list_(a), prims.list_(b)
    )
    # prim1: nub :: [a] -> [a]
    primitives[qname(namespace, "nub")] = prims.prim1(
        qname(namespace, "nub"), lists.nub, ["a"],
        prims.list_(a), prims.list_(a)
    )
    # prim1: null :: [a] -> Bool
    primitives[qname(namespace, "null")] = prims.prim1(
        qname(namespace, "null"), lists.null, ["a"],
        prims.list_(a), prims.boolean()
    )
    # prim1: pure :: a -> [a]
    primitives[qname(namespace, "pure")] = prims.prim1(
        qname(namespace, "pure"), lists.pure, ["a"],
        a, prims.list_(a)
    )
    # prim2: replicate :: Int32 -> a -> [a]
    primitives[qname(namespace, "replicate")] = prims.prim2(
        qname(namespace, "replicate"), lists.replicate, ["a"],
        prims.int32(), a, prims.list_(a)
    )
    # prim1: reverse :: [a] -> [a]
    primitives[qname(namespace, "reverse")] = prims.prim1(
        qname(namespace, "reverse"), lists.reverse, ["a"], prims.list_(a), prims.list_(a)
    )
    # prim1: safeHead :: [a] -> Maybe a
    primitives[qname(namespace, "safeHead")] = prims.prim1(
        qname(namespace, "safeHead"), lists.safe_head, ["a"],
        prims.list_(a), prims.optional(a)
    )
    # prim1: singleton :: a -> [a]
    primitives[qname(namespace, "singleton")] = prims.prim1(
        qname(namespace, "singleton"), lists.singleton, ["a"],
        a, prims.list_(a)
    )
    # prim1: sort :: [a] -> [a]
    primitives[qname(namespace, "sort")] = prims.prim1(
        qname(namespace, "sort"), lists.sort, ["a"],
        prims.list_(a), prims.list_(a)
    )
    # prim2: sortOn :: (a -> b) -> [a] -> [a]
    primitives[qname(namespace, "sortOn")] = prims.prim2_interp(
        qname(namespace, "sortOn"), Just(eval_lists.sort_on), ["a", "b"],
        prims.function(a, b), prims.list_(a), prims.list_(a)
    )
    # prim2: span :: (a -> Bool) -> [a] -> ([a], [a])
    primitives[qname(namespace, "span")] = prims.prim2_interp(
        qname(namespace, "span"), Just(eval_lists.span), ["a"],
        prims.function(a, prims.boolean()), prims.list_(a), prims.pair(prims.list_(a), prims.list_(a))
    )
    # prim1: tail :: [a] -> [a]
    primitives[qname(namespace, "tail")] = prims.prim1(
        qname(namespace, "tail"), lists.tail, ["a"], prims.list_(a), prims.list_(a)
    )
    # prim2: take :: Int32 -> [a] -> [a]
    primitives[qname(namespace, "take")] = prims.prim2(
        qname(namespace, "take"), lists.take, ["a"],
        prims.int32(), prims.list_(a), prims.list_(a)
    )
    # prim1: transpose :: [[a]] -> [[a]]
    primitives[qname(namespace, "transpose")] = prims.prim1(
        qname(namespace, "transpose"), lists.transpose, ["a"],
        prims.list_(prims.list_(a)), prims.list_(prims.list_(a))
    )
    # prim2: zip :: [a] -> [b] -> [(a, b)]
    primitives[qname(namespace, "zip")] = prims.prim2(
        qname(namespace, "zip"), lists.zip, ["a", "b"],
        prims.list_(a), prims.list_(b), prims.list_(prims.pair(a, b))
    )
    # prim3: zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    primitives[qname(namespace, "zipWith")] = prims.prim3_interp(
        qname(namespace, "zipWith"), Just(eval_lists.zip_with), ["a", "b", "c"],
        prims.function(a, prims.function(b, c)), prims.list_(a), prims.list_(b), prims.list_(c)
    )

    return primitives


def register_logic_primitives() -> dict[Name, Primitive]:
    """Register all logic primitive functions."""
    from hydra.lib import logic

    namespace = "hydra.lib.logic"
    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")

    primitives[qname(namespace, "and")] = prims.prim2(
        qname(namespace, "and"), logic.and_, [], prims.boolean(), prims.boolean(), prims.boolean()
    )
    primitives[qname(namespace, "ifElse")] = prims.prim3(
        qname(namespace, "ifElse"), logic.if_else, ["a"],
        prims.boolean(), a, a, a
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
    from hydra.eval.lib import maps as eval_maps
    from hydra.dsl.python import Just

    namespace = "hydra.lib.maps"
    primitives: dict[Name, Primitive] = {}

    k = prims.variable("k")
    k1 = prims.variable("k1")
    k2 = prims.variable("k2")
    v = prims.variable("v")
    v1 = prims.variable("v1")
    v2 = prims.variable("v2")
    map_kv = prims.map_(k, v)

    # prim3: alter :: (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
    primitives[qname(namespace, "alter")] = prims.prim3_interp(
        qname(namespace, "alter"), Just(eval_maps.alter), ["v", "k"],
        prims.function(prims.optional(v), prims.optional(v)), k, map_kv, map_kv
    )
    # prim3: bimap :: (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
    primitives[qname(namespace, "bimap")] = prims.prim3_interp(
        qname(namespace, "bimap"), Just(eval_maps.bimap), ["k1", "k2", "v1", "v2"],
        prims.function(k1, k2), prims.function(v1, v2), prims.map_(k1, v1), prims.map_(k2, v2)
    )
    # prim2: delete :: k -> Map k v -> Map k v
    primitives[qname(namespace, "delete")] = prims.prim2(
        qname(namespace, "delete"), maps.delete, ["k", "v"],
        k, map_kv, map_kv
    )
    # prim1: elems :: Map k v -> [v]
    primitives[qname(namespace, "elems")] = prims.prim1(
        qname(namespace, "elems"), maps.elems, ["k", "v"],
        map_kv, prims.list_(v)
    )
    # prim0: empty :: Map k v
    primitives[qname(namespace, "empty")] = prims.prim0(
        qname(namespace, "empty"), maps.empty, ["k", "v"],
        map_kv
    )
    # prim2: filter :: (v -> Bool) -> Map k v -> Map k v
    primitives[qname(namespace, "filter")] = prims.prim2_interp(
        qname(namespace, "filter"), Just(eval_maps.filter), ["v", "k"],
        prims.function(v, prims.boolean()), map_kv, map_kv
    )
    # prim2: filterWithKey :: (k -> v -> Bool) -> Map k v -> Map k v
    primitives[qname(namespace, "filterWithKey")] = prims.prim2_interp(
        qname(namespace, "filterWithKey"), Just(eval_maps.filter_with_key), ["k", "v"],
        prims.function(k, prims.function(v, prims.boolean())), map_kv, map_kv
    )
    # prim3: findWithDefault :: v -> k -> Map k v -> v
    primitives[qname(namespace, "findWithDefault")] = prims.prim3(
        qname(namespace, "findWithDefault"), maps.find_with_default, ["v", "k"],
        v, k, map_kv, v
    )
    # prim1: fromList :: [(k, v)] -> Map k v
    primitives[qname(namespace, "fromList")] = prims.prim1(
        qname(namespace, "fromList"), maps.from_list, ["k", "v"],
        prims.list_(prims.pair(k, v)), map_kv
    )
    # prim3: insert :: k -> v -> Map k v -> Map k v
    primitives[qname(namespace, "insert")] = prims.prim3(
        qname(namespace, "insert"), maps.insert, ["k", "v"],
        k, v, map_kv, map_kv
    )
    # prim1: keys :: Map k v -> [k]
    primitives[qname(namespace, "keys")] = prims.prim1(
        qname(namespace, "keys"), maps.keys, ["k", "v"],
        map_kv, prims.list_(k)
    )
    # prim2: lookup :: k -> Map k v -> Maybe v
    primitives[qname(namespace, "lookup")] = prims.prim2(
        qname(namespace, "lookup"), maps.lookup, ["k", "v"],
        k, map_kv, prims.optional(v)
    )
    # prim2: map :: (v1 -> v2) -> Map k v1 -> Map k v2
    primitives[qname(namespace, "map")] = prims.prim2_interp(
        qname(namespace, "map"), Just(eval_maps.map), ["v1", "v2", "k"],
        prims.function(v1, v2), prims.map_(k, v1), prims.map_(k, v2)
    )
    # prim2: mapKeys :: (k1 -> k2) -> Map k1 v -> Map k2 v
    primitives[qname(namespace, "mapKeys")] = prims.prim2_interp(
        qname(namespace, "mapKeys"), Just(eval_maps.map_keys), ["k1", "k2", "v"],
        prims.function(k1, k2), prims.map_(k1, v), prims.map_(k2, v)
    )
    # prim2: member :: k -> Map k v -> Bool
    primitives[qname(namespace, "member")] = prims.prim2(
        qname(namespace, "member"), maps.member, ["k", "v"],
        k, map_kv, prims.boolean()
    )
    # prim1: null :: Map k v -> Bool
    primitives[qname(namespace, "null")] = prims.prim1(
        qname(namespace, "null"), maps.null, ["k", "v"],
        map_kv, prims.boolean()
    )
    # prim2: singleton :: k -> v -> Map k v
    primitives[qname(namespace, "singleton")] = prims.prim2(
        qname(namespace, "singleton"), maps.singleton, ["k", "v"],
        k, v, map_kv
    )
    # prim1: size :: Map k v -> Int32
    primitives[qname(namespace, "size")] = prims.prim1(
        qname(namespace, "size"), maps.size, ["k", "v"],
        map_kv, prims.int32()
    )
    # prim1: toList :: Map k v -> [(k, v)]
    primitives[qname(namespace, "toList")] = prims.prim1(
        qname(namespace, "toList"), maps.to_list, ["k", "v"],
        map_kv, prims.list_(prims.pair(k, v))
    )
    # prim2: union :: Map k v -> Map k v -> Map k v
    primitives[qname(namespace, "union")] = prims.prim2(
        qname(namespace, "union"), maps.union, ["k", "v"],
        map_kv, map_kv, map_kv
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
    primitives[qname(namespace, "max")] = prims.prim2(
        qname(namespace, "max"), math.max, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "min")] = prims.prim2(
        qname(namespace, "min"), math.min, [], prims.int32(), prims.int32(), prims.int32()
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
    from hydra.eval.lib import maybes as eval_maybes
    from hydra.dsl.python import Just, Nothing

    namespace = "hydra.lib.maybes"
    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")
    b = prims.variable("b")
    c = prims.variable("c")

    # apply :: Maybe (a -> b) -> Maybe a -> Maybe b
    primitives[qname(namespace, "apply")] = prims.prim2_interp(
        qname(namespace, "apply"), Just(eval_maybes.apply), ["a", "b"],
        prims.optional(prims.function(a, b)), prims.optional(a), prims.optional(b)
    )
    # bind :: Maybe a -> (a -> Maybe b) -> Maybe b
    primitives[qname(namespace, "bind")] = prims.prim2_interp(
        qname(namespace, "bind"), Just(eval_maybes.bind), ["a", "b"],
        prims.optional(a), prims.function(a, prims.optional(b)), prims.optional(b)
    )
    # cases :: Maybe a -> b -> (a -> b) -> b
    primitives[qname(namespace, "cases")] = prims.prim3_interp(
        qname(namespace, "cases"), Just(eval_maybes.cases), ["a", "b"],
        prims.optional(a), b, prims.function(a, b), b
    )
    # cat :: [Maybe a] -> [a]
    primitives[qname(namespace, "cat")] = prims.prim1(
        qname(namespace, "cat"), maybes.cat, ["a"],
        prims.list_(prims.optional(a)), prims.list_(a)
    )
    # compose :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
    primitives[qname(namespace, "compose")] = prims.prim3_interp(
        qname(namespace, "compose"), Just(eval_maybes.compose), ["a", "b", "c"],
        prims.function(a, prims.optional(b)), prims.function(b, prims.optional(c)),
        a, prims.optional(c)
    )
    # fromJust :: Maybe a -> a
    primitives[qname(namespace, "fromJust")] = prims.prim1(
        qname(namespace, "fromJust"), maybes.from_just, ["a"],
        prims.optional(a), a
    )
    primitives[qname(namespace, "fromMaybe")] = prims.prim2(
        qname(namespace, "fromMaybe"), maybes.from_maybe, ["a"],
        a, prims.optional(a), a
    )
    primitives[qname(namespace, "isJust")] = prims.prim1(
        qname(namespace, "isJust"), maybes.is_just, ["a"],
        prims.optional(a), prims.boolean()
    )
    primitives[qname(namespace, "isNothing")] = prims.prim1(
        qname(namespace, "isNothing"), maybes.is_nothing, ["a"],
        prims.optional(a), prims.boolean()
    )
    # map :: (a -> b) -> Maybe a -> Maybe b
    primitives[qname(namespace, "map")] = prims.prim2_interp(
        qname(namespace, "map"), Just(eval_maybes.map), ["a", "b"],
        prims.function(a, b), prims.optional(a), prims.optional(b)
    )
    # mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    primitives[qname(namespace, "mapMaybe")] = prims.prim2_interp(
        qname(namespace, "mapMaybe"), Just(eval_maybes.map_maybe), ["a", "b"],
        prims.function(a, prims.optional(b)), prims.list_(a), prims.list_(b)
    )
    # maybe: b -> (a -> b) -> Maybe a -> b
    # Note: type variables are ordered [b, a] to match Haskell's [_y, _x] order
    primitives[qname(namespace, "maybe")] = prims.prim3_interp(
        qname(namespace, "maybe"), Just(eval_maybes.maybe), ["b", "a"],
        b, prims.function(a, b), prims.optional(a), b
    )
    primitives[qname(namespace, "pure")] = prims.prim1(
        qname(namespace, "pure"), maybes.pure, ["a"],
        a, prims.optional(a)
    )

    return primitives


def register_sets_primitives() -> dict[Name, Primitive]:
    """Register all set primitive functions."""
    from hydra.lib import sets
    from hydra.eval.lib import sets as eval_sets
    from hydra.dsl.python import Just

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
    # prim0: empty :: Set a
    primitives[qname(namespace, "empty")] = prims.prim0(
        qname(namespace, "empty"), sets.empty, ["a"],
        prims.set_(a)
    )
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
    primitives[qname(namespace, "map")] = prims.prim2_interp(
        qname(namespace, "map"), Just(eval_sets.map), ["a", "b"],
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
    primitives[qname(namespace, "readBigint")] = prims.prim1(
        qname(namespace, "readBigint"), literals.read_bigint, [],
        prims.string(), prims.optional(prims.bigint())
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
    primitives[qname(namespace, "readInt8")] = prims.prim1(
        qname(namespace, "readInt8"), literals.read_int8, [],
        prims.string(), prims.optional(prims.int8())
    )
    primitives[qname(namespace, "readInt16")] = prims.prim1(
        qname(namespace, "readInt16"), literals.read_int16, [],
        prims.string(), prims.optional(prims.int16())
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
    primitives[qname(namespace, "readUint8")] = prims.prim1(
        qname(namespace, "readUint8"), literals.read_uint8, [],
        prims.string(), prims.optional(prims.uint8())
    )
    primitives[qname(namespace, "readUint16")] = prims.prim1(
        qname(namespace, "readUint16"), literals.read_uint16, [],
        prims.string(), prims.optional(prims.uint16())
    )
    primitives[qname(namespace, "readUint32")] = prims.prim1(
        qname(namespace, "readUint32"), literals.read_uint32, [],
        prims.string(), prims.optional(prims.uint32())
    )
    primitives[qname(namespace, "readUint64")] = prims.prim1(
        qname(namespace, "readUint64"), literals.read_uint64, [],
        prims.string(), prims.optional(prims.uint64())
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


def register_pairs_primitives() -> dict[Name, Primitive]:
    """Register all pairs primitive functions."""
    from hydra.lib import pairs
    from hydra.eval.lib import pairs as eval_pairs
    from hydra.dsl.python import Just

    namespace = "hydra.lib.pairs"
    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")
    b = prims.variable("b")
    c = prims.variable("c")
    d = prims.variable("d")

    # bimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
    primitives[qname(namespace, "bimap")] = prims.prim3_interp(
        qname(namespace, "bimap"), Just(eval_pairs.bimap), ["a", "b", "c", "d"],
        prims.function(a, c), prims.function(b, d), prims.pair(a, b), prims.pair(c, d)
    )
    primitives[qname(namespace, "first")] = prims.prim1(
        qname(namespace, "first"), pairs.first, ["a", "b"],
        prims.pair(a, b), a
    )
    primitives[qname(namespace, "second")] = prims.prim1(
        qname(namespace, "second"), pairs.second, ["a", "b"],
        prims.pair(a, b), b
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
    primitives.update(register_pairs_primitives())
    primitives.update(register_sets_primitives())
    primitives.update(register_strings_primitives())
    return primitives
