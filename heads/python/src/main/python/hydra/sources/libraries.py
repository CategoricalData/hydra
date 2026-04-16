"""Python implementations of primitive name constants and registration functions."""


from hydra.core import Name
from hydra.graph import Primitive
from hydra.dsl import prims


def qname(namespace: str, local_name: str) -> Name:
    """Qualified name constructor."""
    return Name(f"{namespace}.{local_name}")


def fun(dom, cod):
    """A TermCoder for function types which uses beta reduction to bridge term-level
    functions to native functions. This allows higher-order primitives like map,
    filter, foldl, etc. to use native implementations rather than eval-level ones."""
    import hydra.reduction as reduction
    return prims.function_with_reduce(lambda cx, g, t: reduction.reduce_term(cx, g, True, t), dom, cod)


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
    _xOrd = prims.v_ord("x")
    _xEq = prims.v_eq("x")
    _x = prims.v("x")

    primitives[qname(namespace, "compare")] = prims.prim2(
        qname(namespace, "compare"), equality.compare, [_xOrd],
        x, x, prims.comparison()
    )
    primitives[qname(namespace, "equal")] = prims.prim2(
        qname(namespace, "equal"), equality.equal, [_xEq],
        x, x, prims.boolean()
    )
    primitives[qname(namespace, "identity")] = prims.prim1(
        qname(namespace, "identity"), equality.identity, [_x],
        x, x
    )
    primitives[qname(namespace, "gt")] = prims.prim2(
        qname(namespace, "gt"), equality.gt, [_xOrd],
        x, x, prims.boolean()
    )
    primitives[qname(namespace, "gte")] = prims.prim2(
        qname(namespace, "gte"), equality.gte, [_xOrd],
        x, x, prims.boolean()
    )
    primitives[qname(namespace, "lt")] = prims.prim2(
        qname(namespace, "lt"), equality.lt, [_xOrd],
        x, x, prims.boolean()
    )
    primitives[qname(namespace, "lte")] = prims.prim2(
        qname(namespace, "lte"), equality.lte, [_xOrd],
        x, x, prims.boolean()
    )
    primitives[qname(namespace, "max")] = prims.prim2(
        qname(namespace, "max"), equality.max, [_xOrd],
        x, x, x
    )
    primitives[qname(namespace, "min")] = prims.prim2(
        qname(namespace, "min"), equality.min, [_xOrd],
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
    z = prims.variable("z")
    _x = prims.v("x")
    _y = prims.v("y")
    _z = prims.v("z")

    # bind :: Either x y -> (y -> Either x z) -> Either x z
    primitives[qname(namespace, "bind")] = prims.prim2(
        qname(namespace, "bind"), eithers.bind, [_x, _y, _z],
        prims.either(x, y), fun(y, prims.either(x, z)), prims.either(x, z)
    )
    # bimap :: (x -> z) -> (y -> w) -> Either x y -> Either z w
    w = prims.variable("w")
    _w = prims.v("w")
    primitives[qname(namespace, "bimap")] = prims.prim3(
        qname(namespace, "bimap"), eithers.bimap, [_x, _y, _z, _w],
        fun(x, z), fun(y, w), prims.either(x, y), prims.either(z, w)
    )
    # either :: (x -> z) -> (y -> z) -> Either x y -> z
    primitives[qname(namespace, "either")] = prims.prim3(
        qname(namespace, "either"), eithers.either, [_x, _y, _z],
        fun(x, z), fun(y, z), prims.either(x, y), z
    )
    # foldl :: (x -> y -> Either z x) -> x -> [y] -> Either z x
    primitives[qname(namespace, "foldl")] = prims.prim3(
        qname(namespace, "foldl"), eithers.foldl, [_x, _y, _z],
        fun(x, fun(y, prims.either(z, x))), x, prims.list_(y), prims.either(z, x)
    )
    primitives[qname(namespace, "fromLeft")] = prims.prim2(
        qname(namespace, "fromLeft"), eithers.from_left, [_x, _y],
        x, prims.either(x, y), x
    )
    primitives[qname(namespace, "fromRight")] = prims.prim2(
        qname(namespace, "fromRight"), eithers.from_right, [_x, _y],
        y, prims.either(x, y), y
    )
    primitives[qname(namespace, "isLeft")] = prims.prim1(
        qname(namespace, "isLeft"), eithers.is_left, [_x, _y],
        prims.either(x, y), prims.boolean()
    )
    primitives[qname(namespace, "isRight")] = prims.prim1(
        qname(namespace, "isRight"), eithers.is_right, [_x, _y],
        prims.either(x, y), prims.boolean()
    )
    primitives[qname(namespace, "lefts")] = prims.prim1(
        qname(namespace, "lefts"), eithers.lefts, [_x, _y],
        prims.list_(prims.either(x, y)), prims.list_(x)
    )
    # map :: (x -> y) -> Either z x -> Either z y
    primitives[qname(namespace, "map")] = prims.prim2(
        qname(namespace, "map"), eithers.map, [_x, _y, _z],
        fun(x, y), prims.either(z, x), prims.either(z, y)
    )
    # mapList :: (x -> Either z y) -> [x] -> Either z [y]
    primitives[qname(namespace, "mapList")] = prims.prim2(
        qname(namespace, "mapList"), eithers.map_list, [_x, _y, _z],
        fun(x, prims.either(z, y)), prims.list_(x), prims.either(z, prims.list_(y))
    )
    # mapMaybe :: (x -> Either z y) -> Maybe x -> Either z (Maybe y)
    primitives[qname(namespace, "mapMaybe")] = prims.prim2(
        qname(namespace, "mapMaybe"), eithers.map_maybe, [_x, _y, _z],
        fun(x, prims.either(z, y)), prims.optional(x), prims.either(z, prims.optional(y))
    )
    # mapSet :: (x -> Either z y) -> Set x -> Either z (Set y)
    primitives[qname(namespace, "mapSet")] = prims.prim2(
        qname(namespace, "mapSet"), eithers.map_set, [_x, _y, _z],
        fun(x, prims.either(z, y)), prims.set_(x), prims.either(z, prims.set_(y))
    )
    primitives[qname(namespace, "partitionEithers")] = prims.prim1(
        qname(namespace, "partitionEithers"), eithers.partition_eithers, [_x, _y],
        prims.list_(prims.either(x, y)), prims.pair(prims.list_(x), prims.list_(y))
    )
    primitives[qname(namespace, "rights")] = prims.prim1(
        qname(namespace, "rights"), eithers.rights, [_x, _y],
        prims.list_(prims.either(x, y)), prims.list_(y)
    )

    return primitives


def register_lists_primitives() -> dict[Name, Primitive]:
    """Register all list primitive functions."""
    from hydra.lib import lists

    namespace = "hydra.lib.lists"
    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")
    b = prims.variable("b")
    c = prims.variable("c")
    _a = prims.v("a")
    _b = prims.v("b")
    _c = prims.v("c")
    _aEq = prims.v_eq("a")
    _aOrd = prims.v_ord("a")
    _bOrd = prims.v_ord("b")

    # prim2: apply :: [a -> b] -> [a] -> [b]
    primitives[qname(namespace, "apply")] = prims.prim2(
        qname(namespace, "apply"), lists.apply, [_a, _b],
        prims.list_(fun(a, b)), prims.list_(a), prims.list_(b)
    )
    # prim2: bind :: [a] -> (a -> [b]) -> [b]
    primitives[qname(namespace, "bind")] = prims.prim2(
        qname(namespace, "bind"), lists.bind, [_a, _b],
        prims.list_(a), fun(a, prims.list_(b)), prims.list_(b)
    )
    # prim1: concat :: [[a]] -> [a]
    primitives[qname(namespace, "concat")] = prims.prim1(
        qname(namespace, "concat"), lists.concat, [_a],
        prims.list_(prims.list_(a)), prims.list_(a)
    )
    # prim2: concat2 :: [a] -> [a] -> [a]
    primitives[qname(namespace, "concat2")] = prims.prim2(
        qname(namespace, "concat2"), lists.concat2, [_a],
        prims.list_(a), prims.list_(a), prims.list_(a)
    )
    # prim2: cons :: a -> [a] -> [a]
    primitives[qname(namespace, "cons")] = prims.prim2(
        qname(namespace, "cons"), lists.cons, [_a],
        a, prims.list_(a), prims.list_(a)
    )
    # prim2: drop :: Int32 -> [a] -> [a]
    primitives[qname(namespace, "drop")] = prims.prim2(
        qname(namespace, "drop"), lists.drop, [_a],
        prims.int32(), prims.list_(a), prims.list_(a)
    )
    # prim2: dropWhile :: (a -> Bool) -> [a] -> [a]
    primitives[qname(namespace, "dropWhile")] = prims.prim2(
        qname(namespace, "dropWhile"), lists.drop_while, [_a],
        fun(a, prims.boolean()), prims.list_(a), prims.list_(a)
    )
    # prim2: elem :: Eq a => a -> [a] -> Bool
    primitives[qname(namespace, "elem")] = prims.prim2(
        qname(namespace, "elem"), lists.elem, [_aEq],
        a, prims.list_(a), prims.boolean()
    )
    # prim2: filter :: (a -> Bool) -> [a] -> [a]
    primitives[qname(namespace, "filter")] = prims.prim2(
        qname(namespace, "filter"), lists.filter, [_a],
        fun(a, prims.boolean()), prims.list_(a), prims.list_(a)
    )
    # prim2: find :: (a -> Bool) -> [a] -> Maybe a
    primitives[qname(namespace, "find")] = prims.prim2(
        qname(namespace, "find"), lists.find, [_a],
        fun(a, prims.boolean()), prims.list_(a), prims.optional(a)
    )
    # prim3: foldl :: (b -> a -> b) -> b -> [a] -> b
    primitives[qname(namespace, "foldl")] = prims.prim3(
        qname(namespace, "foldl"), lambda f, init, xs: lists.foldl(lambda acc, el: f(acc)(el), init, xs), [_b, _a],
        fun(b, fun(a, b)), b, prims.list_(a), b
    )
    # prim3: foldr :: (a -> b -> b) -> b -> [a] -> b
    primitives[qname(namespace, "foldr")] = prims.prim3(
        qname(namespace, "foldr"), lambda f, init, xs: lists.foldr(lambda el, acc: f(el)(acc), init, xs), [_a, _b],
        fun(a, fun(b, b)), b, prims.list_(a), b
    )
    # prim1: group :: Eq a => [a] -> [[a]]
    primitives[qname(namespace, "group")] = prims.prim1(
        qname(namespace, "group"), lists.group, [_aEq],
        prims.list_(a), prims.list_(prims.list_(a))
    )
    # prim2: intercalate :: [a] -> [[a]] -> [a]
    primitives[qname(namespace, "intercalate")] = prims.prim2(
        qname(namespace, "intercalate"), lists.intercalate, [_a],
        prims.list_(a), prims.list_(prims.list_(a)), prims.list_(a)
    )
    # prim2: intersperse :: a -> [a] -> [a]
    primitives[qname(namespace, "intersperse")] = prims.prim2(
        qname(namespace, "intersperse"), lists.intersperse, [_a],
        a, prims.list_(a), prims.list_(a)
    )
    # prim1: length :: [a] -> Int32
    primitives[qname(namespace, "length")] = prims.prim1(
        qname(namespace, "length"), lists.length, [_a], prims.list_(a), prims.int32()
    )
    # prim2: map :: (a -> b) -> [a] -> [b]
    primitives[qname(namespace, "map")] = prims.prim2(
        qname(namespace, "map"), lists.map, [_a, _b],
        fun(a, b), prims.list_(a), prims.list_(b)
    )
    # prim2: maybeAt :: Int32 -> [a] -> Maybe a
    primitives[qname(namespace, "maybeAt")] = prims.prim2(
        qname(namespace, "maybeAt"), lists.maybe_at, [_a],
        prims.int32(), prims.list_(a), prims.optional(a)
    )
    # prim1: maybeHead :: [a] -> Maybe a
    primitives[qname(namespace, "maybeHead")] = prims.prim1(
        qname(namespace, "maybeHead"), lists.maybe_head, [_a],
        prims.list_(a), prims.optional(a)
    )
    # prim1: maybeInit :: [a] -> Maybe [a]
    primitives[qname(namespace, "maybeInit")] = prims.prim1(
        qname(namespace, "maybeInit"), lists.maybe_init, [_a],
        prims.list_(a), prims.optional(prims.list_(a))
    )
    # prim1: maybeLast :: [a] -> Maybe a
    primitives[qname(namespace, "maybeLast")] = prims.prim1(
        qname(namespace, "maybeLast"), lists.maybe_last, [_a],
        prims.list_(a), prims.optional(a)
    )
    # prim1: maybeTail :: [a] -> Maybe [a]
    primitives[qname(namespace, "maybeTail")] = prims.prim1(
        qname(namespace, "maybeTail"), lists.maybe_tail, [_a],
        prims.list_(a), prims.optional(prims.list_(a))
    )
    # prim1: nub :: Eq a => [a] -> [a]
    primitives[qname(namespace, "nub")] = prims.prim1(
        qname(namespace, "nub"), lists.nub, [_aEq],
        prims.list_(a), prims.list_(a)
    )
    # prim1: null :: [a] -> Bool
    primitives[qname(namespace, "null")] = prims.prim1(
        qname(namespace, "null"), lists.null, [_a],
        prims.list_(a), prims.boolean()
    )
    # prim2: partition :: (a -> Bool) -> [a] -> ([a], [a])
    primitives[qname(namespace, "partition")] = prims.prim2(
        qname(namespace, "partition"), lists.partition, [_a],
        fun(a, prims.boolean()), prims.list_(a), prims.pair(prims.list_(a), prims.list_(a))
    )
    # prim1: pure :: a -> [a]
    primitives[qname(namespace, "pure")] = prims.prim1(
        qname(namespace, "pure"), lists.pure, [_a],
        a, prims.list_(a)
    )
    # prim2: replicate :: Int32 -> a -> [a]
    primitives[qname(namespace, "replicate")] = prims.prim2(
        qname(namespace, "replicate"), lists.replicate, [_a],
        prims.int32(), a, prims.list_(a)
    )
    # prim1: reverse :: [a] -> [a]
    primitives[qname(namespace, "reverse")] = prims.prim1(
        qname(namespace, "reverse"), lists.reverse, [_a], prims.list_(a), prims.list_(a)
    )
    # prim1: singleton :: a -> [a]
    primitives[qname(namespace, "singleton")] = prims.prim1(
        qname(namespace, "singleton"), lists.singleton, [_a],
        a, prims.list_(a)
    )
    # prim1: sort :: Ord a => [a] -> [a]
    primitives[qname(namespace, "sort")] = prims.prim1(
        qname(namespace, "sort"), lists.sort, [_aOrd],
        prims.list_(a), prims.list_(a)
    )
    # prim2: sortOn :: Ord b => (a -> b) -> [a] -> [a]
    primitives[qname(namespace, "sortOn")] = prims.prim2(
        qname(namespace, "sortOn"), lists.sort_on, [_a, _bOrd],
        fun(a, b), prims.list_(a), prims.list_(a)
    )
    # prim2: span :: (a -> Bool) -> [a] -> ([a], [a])
    primitives[qname(namespace, "span")] = prims.prim2(
        qname(namespace, "span"), lists.span, [_a],
        fun(a, prims.boolean()), prims.list_(a), prims.pair(prims.list_(a), prims.list_(a))
    )
    # prim2: take :: Int32 -> [a] -> [a]
    primitives[qname(namespace, "take")] = prims.prim2(
        qname(namespace, "take"), lists.take, [_a],
        prims.int32(), prims.list_(a), prims.list_(a)
    )
    # prim1: transpose :: [[a]] -> [[a]]
    primitives[qname(namespace, "transpose")] = prims.prim1(
        qname(namespace, "transpose"), lists.transpose, [_a],
        prims.list_(prims.list_(a)), prims.list_(prims.list_(a))
    )
    # prim1: uncons :: [a] -> Maybe (a, [a])
    primitives[qname(namespace, "uncons")] = prims.prim1(
        qname(namespace, "uncons"), lists.uncons, [_a],
        prims.list_(a), prims.optional(prims.pair(a, prims.list_(a)))
    )
    # prim2: zip :: [a] -> [b] -> [(a, b)]
    primitives[qname(namespace, "zip")] = prims.prim2(
        qname(namespace, "zip"), lists.zip, [_a, _b],
        prims.list_(a), prims.list_(b), prims.list_(prims.pair(a, b))
    )
    # prim3: zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    primitives[qname(namespace, "zipWith")] = prims.prim3(
        qname(namespace, "zipWith"), lambda f, xs, ys: lists.zip_with(lambda a, b: f(a)(b), xs, ys), [_a, _b, _c],
        fun(a, fun(b, c)), prims.list_(a), prims.list_(b), prims.list_(c)
    )

    return primitives


def register_logic_primitives() -> dict[Name, Primitive]:
    """Register all logic primitive functions."""
    from hydra.lib import logic

    namespace = "hydra.lib.logic"
    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")
    _a = prims.v("a")

    primitives[qname(namespace, "and")] = prims.prim2(
        qname(namespace, "and"), logic.and_, [], prims.boolean(), prims.boolean(), prims.boolean()
    )
    primitives[qname(namespace, "ifElse")] = prims.prim3(
        qname(namespace, "ifElse"), logic.if_else, [_a],
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

    namespace = "hydra.lib.maps"
    primitives: dict[Name, Primitive] = {}

    k = prims.variable("k")
    k1 = prims.variable("k1")
    k2 = prims.variable("k2")
    v = prims.variable("v")
    v1 = prims.variable("v1")
    v2 = prims.variable("v2")
    map_kv = prims.map_(k, v)
    _v = prims.v("v")
    _v1 = prims.v("v1")
    _v2 = prims.v("v2")
    _kOrd = prims.v_ord("k")
    _k1Ord = prims.v_ord("k1")
    _k2Ord = prims.v_ord("k2")

    # prim3: alter :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
    primitives[qname(namespace, "alter")] = prims.prim3(
        qname(namespace, "alter"), maps.alter, [_v, _kOrd],
        fun(prims.optional(v), prims.optional(v)), k, map_kv, map_kv
    )
    # prim3: bimap :: (Ord k1, Ord k2) => (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
    primitives[qname(namespace, "bimap")] = prims.prim3(
        qname(namespace, "bimap"), maps.bimap, [_k1Ord, _k2Ord, _v1, _v2],
        fun(k1, k2), fun(v1, v2), prims.map_(k1, v1), prims.map_(k2, v2)
    )
    # prim2: delete :: Ord k => k -> Map k v -> Map k v
    primitives[qname(namespace, "delete")] = prims.prim2(
        qname(namespace, "delete"), maps.delete, [_kOrd, _v],
        k, map_kv, map_kv
    )
    # prim1: elems :: Ord k => Map k v -> [v]
    primitives[qname(namespace, "elems")] = prims.prim1(
        qname(namespace, "elems"), maps.elems, [_kOrd, _v],
        map_kv, prims.list_(v)
    )
    # prim0: empty :: Ord k => Map k v
    primitives[qname(namespace, "empty")] = prims.prim0(
        qname(namespace, "empty"), maps.empty, [_kOrd, _v],
        map_kv
    )
    # prim2: filter :: Ord k => (v -> Bool) -> Map k v -> Map k v
    primitives[qname(namespace, "filter")] = prims.prim2(
        qname(namespace, "filter"), maps.filter, [_v, _kOrd],
        fun(v, prims.boolean()), map_kv, map_kv
    )
    # prim2: filterWithKey :: Ord k => (k -> v -> Bool) -> Map k v -> Map k v
    primitives[qname(namespace, "filterWithKey")] = prims.prim2(
        qname(namespace, "filterWithKey"), lambda f, m: maps.filter_with_key(lambda k, v: f(k)(v), m), [_kOrd, _v],
        fun(k, fun(v, prims.boolean())), map_kv, map_kv
    )
    # prim3: findWithDefault :: Ord k => v -> k -> Map k v -> v
    primitives[qname(namespace, "findWithDefault")] = prims.prim3(
        qname(namespace, "findWithDefault"), maps.find_with_default, [_v, _kOrd],
        v, k, map_kv, v
    )
    # prim1: fromList :: Ord k => [(k, v)] -> Map k v
    primitives[qname(namespace, "fromList")] = prims.prim1(
        qname(namespace, "fromList"), maps.from_list, [_kOrd, _v],
        prims.list_(prims.pair(k, v)), map_kv
    )
    # prim3: insert :: Ord k => k -> v -> Map k v -> Map k v
    primitives[qname(namespace, "insert")] = prims.prim3(
        qname(namespace, "insert"), maps.insert, [_kOrd, _v],
        k, v, map_kv, map_kv
    )
    # prim1: keys :: Ord k => Map k v -> [k]
    primitives[qname(namespace, "keys")] = prims.prim1(
        qname(namespace, "keys"), maps.keys, [_kOrd, _v],
        map_kv, prims.list_(k)
    )
    # prim2: lookup :: Ord k => k -> Map k v -> Maybe v
    primitives[qname(namespace, "lookup")] = prims.prim2(
        qname(namespace, "lookup"), maps.lookup, [_kOrd, _v],
        k, map_kv, prims.optional(v)
    )
    # prim2: map :: Ord k => (v1 -> v2) -> Map k v1 -> Map k v2
    primitives[qname(namespace, "map")] = prims.prim2(
        qname(namespace, "map"), maps.map, [_v1, _v2, _kOrd],
        fun(v1, v2), prims.map_(k, v1), prims.map_(k, v2)
    )
    # prim2: mapKeys :: (Ord k1, Ord k2) => (k1 -> k2) -> Map k1 v -> Map k2 v
    primitives[qname(namespace, "mapKeys")] = prims.prim2(
        qname(namespace, "mapKeys"), maps.map_keys, [_k1Ord, _k2Ord, _v],
        fun(k1, k2), prims.map_(k1, v), prims.map_(k2, v)
    )
    # prim2: member :: Ord k => k -> Map k v -> Bool
    primitives[qname(namespace, "member")] = prims.prim2(
        qname(namespace, "member"), maps.member, [_kOrd, _v],
        k, map_kv, prims.boolean()
    )
    # prim1: null :: Ord k => Map k v -> Bool
    primitives[qname(namespace, "null")] = prims.prim1(
        qname(namespace, "null"), maps.null, [_kOrd, _v],
        map_kv, prims.boolean()
    )
    # prim2: singleton :: Ord k => k -> v -> Map k v
    primitives[qname(namespace, "singleton")] = prims.prim2(
        qname(namespace, "singleton"), maps.singleton, [_kOrd, _v],
        k, v, map_kv
    )
    # prim1: size :: Ord k => Map k v -> Int32
    primitives[qname(namespace, "size")] = prims.prim1(
        qname(namespace, "size"), maps.size, [_kOrd, _v],
        map_kv, prims.int32()
    )
    # prim1: toList :: Ord k => Map k v -> [(k, v)]
    primitives[qname(namespace, "toList")] = prims.prim1(
        qname(namespace, "toList"), maps.to_list, [_kOrd, _v],
        map_kv, prims.list_(prims.pair(k, v))
    )
    # prim2: union :: Ord k => Map k v -> Map k v -> Map k v
    primitives[qname(namespace, "union")] = prims.prim2(
        qname(namespace, "union"), maps.union, [_kOrd, _v],
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
    primitives[qname(namespace, "addFloat64")] = prims.prim2(
        qname(namespace, "addFloat64"), math.add_float64, [], prims.float64(), prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "even")] = prims.prim1(
        qname(namespace, "even"), math.even, [], prims.int32(), prims.boolean()
    )
    primitives[qname(namespace, "mul")] = prims.prim2(
        qname(namespace, "mul"), math.mul, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "mulFloat64")] = prims.prim2(
        qname(namespace, "mulFloat64"), math.mul_float64, [], prims.float64(), prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "negate")] = prims.prim1(
        qname(namespace, "negate"), math.negate, [], prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "negateFloat64")] = prims.prim1(
        qname(namespace, "negateFloat64"), math.negate_float64, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "odd")] = prims.prim1(
        qname(namespace, "odd"), math.odd, [], prims.int32(), prims.boolean()
    )
    primitives[qname(namespace, "range")] = prims.prim2(
        qname(namespace, "range"), math.range_, [], prims.int32(), prims.int32(), prims.list_(prims.int32())
    )
    primitives[qname(namespace, "signum")] = prims.prim1(
        qname(namespace, "signum"), math.signum, [], prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "sub")] = prims.prim2(
        qname(namespace, "sub"), math.sub, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "subFloat64")] = prims.prim2(
        qname(namespace, "subFloat64"), math.sub_float64, [], prims.float64(), prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "max")] = prims.prim2(
        qname(namespace, "max"), math.max, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[qname(namespace, "maybeDiv")] = prims.prim2(
        qname(namespace, "maybeDiv"), math.maybe_div, [], prims.int32(), prims.int32(), prims.optional(prims.int32())
    )
    primitives[qname(namespace, "maybeMod")] = prims.prim2(
        qname(namespace, "maybeMod"), math.maybe_mod, [], prims.int32(), prims.int32(), prims.optional(prims.int32())
    )
    primitives[qname(namespace, "maybePred")] = prims.prim1(
        qname(namespace, "maybePred"), math.maybe_pred, [], prims.int32(), prims.optional(prims.int32())
    )
    primitives[qname(namespace, "maybeRem")] = prims.prim2(
        qname(namespace, "maybeRem"), math.maybe_rem, [], prims.int32(), prims.int32(), prims.optional(prims.int32())
    )
    primitives[qname(namespace, "maybeSucc")] = prims.prim1(
        qname(namespace, "maybeSucc"), math.maybe_succ, [], prims.int32(), prims.optional(prims.int32())
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
        qname(namespace, "ceiling"), math.ceiling, [], prims.float64(), prims.float64()
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
        qname(namespace, "floor"), math.floor, [], prims.float64(), prims.float64()
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
        qname(namespace, "round"), math.round_, [], prims.float64(), prims.float64()
    )
    primitives[qname(namespace, "roundBigfloat")] = prims.prim2(
        qname(namespace, "roundBigfloat"), math.round_bigfloat, [], prims.int32(), prims.bigfloat(), prims.bigfloat()
    )
    primitives[qname(namespace, "roundFloat32")] = prims.prim2(
        qname(namespace, "roundFloat32"), math.round_float32, [], prims.int32(), prims.float32(), prims.float32()
    )
    primitives[qname(namespace, "roundFloat64")] = prims.prim2(
        qname(namespace, "roundFloat64"), math.round_float64, [], prims.int32(), prims.float64(), prims.float64()
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
        qname(namespace, "truncate"), math.truncate, [], prims.float64(), prims.float64()
    )

    return primitives


def register_maybes_primitives() -> dict[Name, Primitive]:
    """Register all maybe primitive functions."""
    from hydra.lib import maybes

    namespace = "hydra.lib.maybes"
    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")
    b = prims.variable("b")
    c = prims.variable("c")
    _a = prims.v("a")
    _b = prims.v("b")
    _c = prims.v("c")

    # apply :: Maybe (a -> b) -> Maybe a -> Maybe b
    primitives[qname(namespace, "apply")] = prims.prim2(
        qname(namespace, "apply"), maybes.apply, [_a, _b],
        prims.optional(fun(a, b)), prims.optional(a), prims.optional(b)
    )
    # bind :: Maybe a -> (a -> Maybe b) -> Maybe b
    primitives[qname(namespace, "bind")] = prims.prim2(
        qname(namespace, "bind"), maybes.bind, [_a, _b],
        prims.optional(a), fun(a, prims.optional(b)), prims.optional(b)
    )
    # cases :: Maybe a -> b -> (a -> b) -> b
    primitives[qname(namespace, "cases")] = prims.prim3(
        qname(namespace, "cases"), maybes.cases, [_a, _b],
        prims.optional(a), b, fun(a, b), b
    )
    # cat :: [Maybe a] -> [a]
    primitives[qname(namespace, "cat")] = prims.prim1(
        qname(namespace, "cat"), maybes.cat, [_a],
        prims.list_(prims.optional(a)), prims.list_(a)
    )
    # compose :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
    primitives[qname(namespace, "compose")] = prims.prim3(
        qname(namespace, "compose"), maybes.compose, [_a, _b, _c],
        fun(a, prims.optional(b)), fun(b, prims.optional(c)),
        a, prims.optional(c)
    )
    primitives[qname(namespace, "fromMaybe")] = prims.prim2(
        qname(namespace, "fromMaybe"), maybes.from_maybe, [_a],
        a, prims.optional(a), a
    )
    primitives[qname(namespace, "isJust")] = prims.prim1(
        qname(namespace, "isJust"), maybes.is_just, [_a],
        prims.optional(a), prims.boolean()
    )
    primitives[qname(namespace, "isNothing")] = prims.prim1(
        qname(namespace, "isNothing"), maybes.is_nothing, [_a],
        prims.optional(a), prims.boolean()
    )
    # map :: (a -> b) -> Maybe a -> Maybe b
    primitives[qname(namespace, "map")] = prims.prim2(
        qname(namespace, "map"), maybes.map, [_a, _b],
        fun(a, b), prims.optional(a), prims.optional(b)
    )
    # mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    primitives[qname(namespace, "mapMaybe")] = prims.prim2(
        qname(namespace, "mapMaybe"), maybes.map_maybe, [_a, _b],
        fun(a, prims.optional(b)), prims.list_(a), prims.list_(b)
    )
    # maybe: b -> (a -> b) -> Maybe a -> b
    # Note: type variables are ordered [b, a] to match Haskell's [_y, _x] order
    primitives[qname(namespace, "maybe")] = prims.prim3(
        qname(namespace, "maybe"), maybes.maybe, [_b, _a],
        b, fun(a, b), prims.optional(a), b
    )
    primitives[qname(namespace, "pure")] = prims.prim1(
        qname(namespace, "pure"), maybes.pure, [_a],
        a, prims.optional(a)
    )
    # toList :: Maybe a -> [a]
    primitives[qname(namespace, "toList")] = prims.prim1(
        qname(namespace, "toList"), maybes.to_list, [_a],
        prims.optional(a), prims.list_(a)
    )

    return primitives


def register_sets_primitives() -> dict[Name, Primitive]:
    """Register all set primitive functions."""
    from hydra.lib import sets

    namespace = "hydra.lib.sets"
    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")
    b = prims.variable("b")
    _aOrd = prims.v_ord("a")
    _bOrd = prims.v_ord("b")

    primitives[qname(namespace, "delete")] = prims.prim2(
        qname(namespace, "delete"), sets.delete, [_aOrd],
        a, prims.set_(a), prims.set_(a)
    )
    primitives[qname(namespace, "difference")] = prims.prim2(
        qname(namespace, "difference"), sets.difference, [_aOrd],
        prims.set_(a), prims.set_(a), prims.set_(a)
    )
    # prim0: empty :: Ord a => Set a
    primitives[qname(namespace, "empty")] = prims.prim0(
        qname(namespace, "empty"), sets.empty, [_aOrd],
        prims.set_(a)
    )
    primitives[qname(namespace, "fromList")] = prims.prim1(
        qname(namespace, "fromList"), sets.from_list, [_aOrd],
        prims.list_(a), prims.set_(a)
    )
    primitives[qname(namespace, "insert")] = prims.prim2(
        qname(namespace, "insert"), sets.insert, [_aOrd],
        a, prims.set_(a), prims.set_(a)
    )
    primitives[qname(namespace, "intersection")] = prims.prim2(
        qname(namespace, "intersection"), sets.intersection, [_aOrd],
        prims.set_(a), prims.set_(a), prims.set_(a)
    )
    primitives[qname(namespace, "map")] = prims.prim2(
        qname(namespace, "map"), sets.map, [_aOrd, _bOrd],
        fun(a, b), prims.set_(a), prims.set_(b)
    )
    primitives[qname(namespace, "member")] = prims.prim2(
        qname(namespace, "member"), sets.member, [_aOrd],
        a, prims.set_(a), prims.boolean()
    )
    primitives[qname(namespace, "null")] = prims.prim1(
        qname(namespace, "null"), sets.null, [_aOrd],
        prims.set_(a), prims.boolean()
    )
    primitives[qname(namespace, "singleton")] = prims.prim1(
        qname(namespace, "singleton"), sets.singleton, [_aOrd],
        a, prims.set_(a)
    )
    primitives[qname(namespace, "size")] = prims.prim1(
        qname(namespace, "size"), sets.size, [_aOrd],
        prims.set_(a), prims.int32()
    )
    primitives[qname(namespace, "toList")] = prims.prim1(
        qname(namespace, "toList"), sets.to_list, [_aOrd],
        prims.set_(a), prims.list_(a)
    )
    primitives[qname(namespace, "union")] = prims.prim2(
        qname(namespace, "union"), sets.union, [_aOrd],
        prims.set_(a), prims.set_(a), prims.set_(a)
    )
    primitives[qname(namespace, "unions")] = prims.prim1(
        qname(namespace, "unions"), sets.unions, [_aOrd],
        prims.list_(prims.set_(a)), prims.set_(a)
    )

    return primitives


def register_regex_primitives() -> dict[Name, Primitive]:
    """Register all regex primitive functions."""
    from hydra.lib import regex

    namespace = "hydra.lib.regex"
    primitives: dict[Name, Primitive] = {}

    primitives[qname(namespace, "find")] = prims.prim2(
        qname(namespace, "find"), regex.find, [], prims.string(), prims.string(), prims.optional(prims.string())
    )
    primitives[qname(namespace, "findAll")] = prims.prim2(
        qname(namespace, "findAll"), regex.find_all, [], prims.string(), prims.string(), prims.list_(prims.string())
    )
    primitives[qname(namespace, "matches")] = prims.prim2(
        qname(namespace, "matches"), regex.matches, [], prims.string(), prims.string(), prims.boolean()
    )
    primitives[qname(namespace, "replace")] = prims.prim3(
        qname(namespace, "replace"), regex.replace, [], prims.string(), prims.string(), prims.string(), prims.string()
    )
    primitives[qname(namespace, "replaceAll")] = prims.prim3(
        qname(namespace, "replaceAll"), regex.replace_all, [], prims.string(), prims.string(), prims.string(), prims.string()
    )
    primitives[qname(namespace, "split")] = prims.prim2(
        qname(namespace, "split"), regex.split, [], prims.string(), prims.string(), prims.list_(prims.string())
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
    primitives[qname(namespace, "maybeCharAt")] = prims.prim2(
        qname(namespace, "maybeCharAt"), strings.maybe_char_at, [], prims.int32(), prims.string(), prims.optional(prims.int32())
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
    primitives[qname(namespace, "bigintToDecimal")] = prims.prim1(
        qname(namespace, "bigintToDecimal"), literals.bigint_to_decimal, [],
        prims.bigint(), prims.decimal()
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
    primitives[qname(namespace, "binaryToBytes")] = prims.prim1(
        qname(namespace, "binaryToBytes"), literals.binary_to_bytes, [],
        prims.binary(), prims.list_(prims.int32())
    )
    primitives[qname(namespace, "binaryToString")] = prims.prim1(
        qname(namespace, "binaryToString"), literals.binary_to_string, [],
        prims.binary(), prims.string()
    )
    primitives[qname(namespace, "decimalToBigint")] = prims.prim1(
        qname(namespace, "decimalToBigint"), literals.decimal_to_bigint, [],
        prims.decimal(), prims.bigint()
    )
    primitives[qname(namespace, "decimalToFloat32")] = prims.prim1(
        qname(namespace, "decimalToFloat32"), literals.decimal_to_float32, [],
        prims.decimal(), prims.float32()
    )
    primitives[qname(namespace, "decimalToFloat64")] = prims.prim1(
        qname(namespace, "decimalToFloat64"), literals.decimal_to_float64, [],
        prims.decimal(), prims.float64()
    )
    primitives[qname(namespace, "float32ToBigfloat")] = prims.prim1(
        qname(namespace, "float32ToBigfloat"), literals.float32_to_bigfloat, [],
        prims.float32(), prims.bigfloat()
    )
    primitives[qname(namespace, "float32ToDecimal")] = prims.prim1(
        qname(namespace, "float32ToDecimal"), literals.float32_to_decimal, [],
        prims.float32(), prims.decimal()
    )
    primitives[qname(namespace, "float64ToBigfloat")] = prims.prim1(
        qname(namespace, "float64ToBigfloat"), literals.float64_to_bigfloat, [],
        prims.float64(), prims.bigfloat()
    )
    primitives[qname(namespace, "float64ToDecimal")] = prims.prim1(
        qname(namespace, "float64ToDecimal"), literals.float64_to_decimal, [],
        prims.float64(), prims.decimal()
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
    primitives[qname(namespace, "readDecimal")] = prims.prim1(
        qname(namespace, "readDecimal"), literals.read_decimal, [],
        prims.string(), prims.optional(prims.decimal())
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
    primitives[qname(namespace, "showDecimal")] = prims.prim1(
        qname(namespace, "showDecimal"), literals.show_decimal, [],
        prims.decimal(), prims.string()
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

    namespace = "hydra.lib.pairs"
    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")
    b = prims.variable("b")
    c = prims.variable("c")
    d = prims.variable("d")
    _a = prims.v("a")
    _b = prims.v("b")
    _c = prims.v("c")
    _d = prims.v("d")

    # bimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
    primitives[qname(namespace, "bimap")] = prims.prim3(
        qname(namespace, "bimap"), pairs.bimap, [_a, _b, _c, _d],
        fun(a, c), fun(b, d), prims.pair(a, b), prims.pair(c, d)
    )
    primitives[qname(namespace, "first")] = prims.prim1(
        qname(namespace, "first"), pairs.first, [_a, _b],
        prims.pair(a, b), a
    )
    primitives[qname(namespace, "second")] = prims.prim1(
        qname(namespace, "second"), pairs.second, [_a, _b],
        prims.pair(a, b), b
    )

    return primitives


def standard_library() -> dict[Name, Primitive]:
    """Get all standard library primitives."""
    primitives: dict[Name, Primitive] = {}
    primitives.update(register_chars_primitives())
    primitives.update(register_eithers_primitives())
    primitives.update(register_equality_primitives())
    primitives.update(register_lists_primitives())
    primitives.update(register_literals_primitives())
    primitives.update(register_logic_primitives())
    primitives.update(register_maps_primitives())
    primitives.update(register_math_primitives())
    primitives.update(register_maybes_primitives())
    primitives.update(register_pairs_primitives())
    primitives.update(register_regex_primitives())
    primitives.update(register_sets_primitives())
    primitives.update(register_strings_primitives())
    return primitives
