"""Python primitive registration functions.

Primitive names are derived from the generated hydra.lib.* PrimitiveDefinition def-modules
(via def_<sub>.<fn>.name) — the single source of truth for primitive names (#473) — rather than
hand-written name strings. Signatures remain explicit here because the registry must present each
primitive's type to the interpreter in native type-coder form."""


from hydra.core import Name
from hydra.graph import Primitive
from hydra.overlay.python.dsl import prims


def fun(dom, cod):
    """A TermCoder for function types which uses beta reduction to bridge term-level
    functions to native functions. This allows higher-order primitives like map,
    filter, foldl, etc. to use native implementations rather than eval-level ones."""
    import hydra.reduction as reduction
    return prims.function_with_reduce(lambda cx, g, t: reduction.reduce_term(cx, g, True, t), dom, cod)


def register_chars_primitives() -> dict[Name, Primitive]:
    """Register all chars primitive functions."""
    from hydra.overlay.python.lib import chars
    from hydra.lib import chars as def_chars

    primitives: dict[Name, Primitive] = {}

    primitives[def_chars.is_alpha_num.name] = prims.prim1(
        def_chars.is_alpha_num.name, chars.is_alpha_num, [], prims.int32(), prims.boolean()
    )
    primitives[def_chars.is_lower.name] = prims.prim1(
        def_chars.is_lower.name, chars.is_lower, [], prims.int32(), prims.boolean()
    )
    primitives[def_chars.is_space.name] = prims.prim1(
        def_chars.is_space.name, chars.is_space, [], prims.int32(), prims.boolean()
    )
    primitives[def_chars.is_upper.name] = prims.prim1(
        def_chars.is_upper.name, chars.is_upper, [], prims.int32(), prims.boolean()
    )
    primitives[def_chars.to_lower.name] = prims.prim1(
        def_chars.to_lower.name, chars.to_lower, [], prims.int32(), prims.int32()
    )
    primitives[def_chars.to_upper.name] = prims.prim1(
        def_chars.to_upper.name, chars.to_upper, [], prims.int32(), prims.int32()
    )

    return primitives


def register_equality_primitives() -> dict[Name, Primitive]:
    """Register all equality primitive functions."""
    from hydra.overlay.python.lib import equality
    from hydra.lib import equality as def_equality

    primitives: dict[Name, Primitive] = {}

    x = prims.variable("x")
    _xOrd = prims.v_ord("x")
    _xEq = prims.v_eq("x")
    _x = prims.v("x")

    primitives[def_equality.compare.name] = prims.prim2(
        def_equality.compare.name, equality.compare, [_xOrd],
        x, x, prims.comparison()
    )
    primitives[def_equality.equal.name] = prims.prim2(
        def_equality.equal.name, equality.equal, [_xEq],
        x, x, prims.boolean()
    )
    primitives[def_equality.identity.name] = prims.prim1(
        def_equality.identity.name, equality.identity, [_x],
        x, x
    )
    primitives[def_equality.gt.name] = prims.prim2(
        def_equality.gt.name, equality.gt, [_xOrd],
        x, x, prims.boolean()
    )
    primitives[def_equality.gte.name] = prims.prim2(
        def_equality.gte.name, equality.gte, [_xOrd],
        x, x, prims.boolean()
    )
    primitives[def_equality.lt.name] = prims.prim2(
        def_equality.lt.name, equality.lt, [_xOrd],
        x, x, prims.boolean()
    )
    primitives[def_equality.lte.name] = prims.prim2(
        def_equality.lte.name, equality.lte, [_xOrd],
        x, x, prims.boolean()
    )
    primitives[def_equality.max.name] = prims.prim2(
        def_equality.max.name, equality.max, [_xOrd],
        x, x, x
    )
    primitives[def_equality.min.name] = prims.prim2(
        def_equality.min.name, equality.min, [_xOrd],
        x, x, x
    )

    return primitives


def unsupported_effect_primitive(definition) -> Primitive:
    """Register an effect primitive for name resolution and inference, while keeping Hydra's pure
    reducer from interpreting host effects as ordinary terms. Mirrors the Haskell
    unsupportedEffectPrimitive: the type scheme is taken directly from the def-module's
    PrimitiveDefinition (the single source of truth), and the term-level implementation fails loudly,
    since effect primitives are evaluated through the native (host) path, not the interpreter. For #494."""
    from hydra.errors import ErrorOther, OtherError
    from hydra.overlay.python.dsl.python import Left

    def impl(g, args):
        return Left(ErrorOther(OtherError(
            "effect primitive cannot be reduced by Hydra's pure reducer: "
            + definition.name.value)))

    return Primitive(definition=definition, implementation=impl)


def register_effects_primitives() -> dict[Name, Primitive]:
    """Register all effects primitive functions. In Python the effect type is transparent
    (effect<t> = t), so these have no term-level interpreter implementation; they are registered for
    name resolution and inference only. For #494."""
    from hydra.lib import effects as def_effects

    primitives: dict[Name, Primitive] = {}
    primitives[def_effects.apply.name] = unsupported_effect_primitive(def_effects.apply)
    primitives[def_effects.bind.name] = unsupported_effect_primitive(def_effects.bind)
    primitives[def_effects.compose.name] = unsupported_effect_primitive(def_effects.compose)
    primitives[def_effects.foldl.name] = unsupported_effect_primitive(def_effects.foldl)
    primitives[def_effects.map.name] = unsupported_effect_primitive(def_effects.map)
    primitives[def_effects.map_list.name] = unsupported_effect_primitive(def_effects.map_list)
    primitives[def_effects.map_optional.name] = unsupported_effect_primitive(def_effects.map_optional)
    primitives[def_effects.pure.name] = unsupported_effect_primitive(def_effects.pure)
    return primitives


def register_files_primitives() -> dict[Name, Primitive]:
    """Register all files primitive functions. As effectful primitives, these have no term-level
    interpreter implementation; they are registered for name resolution and inference only, and are
    executed via the native (host) path. For #494."""
    from hydra.lib import files as def_files

    primitives: dict[Name, Primitive] = {}
    primitives[def_files.append_file.name] = unsupported_effect_primitive(def_files.append_file)
    primitives[def_files.copy.name] = unsupported_effect_primitive(def_files.copy)
    primitives[def_files.create_directory.name] = unsupported_effect_primitive(def_files.create_directory)
    primitives[def_files.exists.name] = unsupported_effect_primitive(def_files.exists)
    primitives[def_files.list_directory.name] = unsupported_effect_primitive(def_files.list_directory)
    primitives[def_files.read_file.name] = unsupported_effect_primitive(def_files.read_file)
    primitives[def_files.remove_directory.name] = unsupported_effect_primitive(def_files.remove_directory)
    primitives[def_files.remove_file.name] = unsupported_effect_primitive(def_files.remove_file)
    primitives[def_files.rename.name] = unsupported_effect_primitive(def_files.rename)
    primitives[def_files.status.name] = unsupported_effect_primitive(def_files.status)
    primitives[def_files.write_file.name] = unsupported_effect_primitive(def_files.write_file)
    return primitives


def register_hashing_primitives() -> dict[Name, Primitive]:
    """Register all hashing primitive functions (SHA-256). Pure and total. For #524."""
    from hydra.overlay.python.lib import hashing
    from hydra.lib import hashing as def_hashing

    primitives: dict[Name, Primitive] = {}

    # sha256 :: binary -> binary
    primitives[def_hashing.sha256.name] = prims.prim1(
        def_hashing.sha256.name, hashing.sha256, [],
        prims.binary(), prims.binary()
    )
    # sha256Hex :: binary -> string
    primitives[def_hashing.sha256_hex.name] = prims.prim1(
        def_hashing.sha256_hex.name, hashing.sha256_hex, [],
        prims.binary(), prims.string()
    )

    return primitives


def register_system_primitives() -> dict[Name, Primitive]:
    """Register all system primitive functions. As effectful primitives, these have no term-level
    interpreter implementation; they are registered for name resolution and inference only, and are
    executed via the native (host) path. For #498."""
    from hydra.lib import system as def_system

    primitives: dict[Name, Primitive] = {}
    primitives[def_system.execute.name] = unsupported_effect_primitive(def_system.execute)
    primitives[def_system.exit.name] = unsupported_effect_primitive(def_system.exit)
    primitives[def_system.get_environment.name] = unsupported_effect_primitive(def_system.get_environment)
    primitives[def_system.get_environment_variable.name] = unsupported_effect_primitive(def_system.get_environment_variable)
    primitives[def_system.get_time.name] = unsupported_effect_primitive(def_system.get_time)
    primitives[def_system.get_working_directory.name] = unsupported_effect_primitive(def_system.get_working_directory)
    return primitives


def register_eithers_primitives() -> dict[Name, Primitive]:
    """Register all eithers primitive functions."""
    from hydra.overlay.python.lib import eithers
    from hydra.lib import eithers as def_eithers

    primitives: dict[Name, Primitive] = {}

    x = prims.variable("x")
    y = prims.variable("y")
    z = prims.variable("z")
    _x = prims.v("x")
    _y = prims.v("y")
    _z = prims.v("z")

    # bind :: Either x y -> (y -> Either x z) -> Either x z
    primitives[def_eithers.bind.name] = prims.prim2(
        def_eithers.bind.name, eithers.bind, [_x, _y, _z],
        prims.either(x, y), fun(y, prims.either(x, z)), prims.either(x, z)
    )
    # bimap :: (x -> z) -> (y -> w) -> Either x y -> Either z w
    w = prims.variable("w")
    _w = prims.v("w")
    primitives[def_eithers.bimap.name] = prims.prim3(
        def_eithers.bimap.name, eithers.bimap, [_x, _y, _z, _w],
        fun(x, z), fun(y, w), prims.either(x, y), prims.either(z, w)
    )
    # either :: (x -> z) -> (y -> z) -> Either x y -> z
    primitives[def_eithers.either.name] = prims.prim3(
        def_eithers.either.name, eithers.either, [_x, _y, _z],
        fun(x, z), fun(y, z), prims.either(x, y), z
    )
    # foldl :: (x -> y -> Either z x) -> x -> [y] -> Either z x
    primitives[def_eithers.foldl.name] = prims.prim3(
        def_eithers.foldl.name, eithers.foldl, [_x, _y, _z],
        fun(x, fun(y, prims.either(z, x))), x, prims.list_(y), prims.either(z, x)
    )
    primitives[def_eithers.from_left.name] = prims.prim2(
        def_eithers.from_left.name, eithers.from_left, [_x, _y],
        x, prims.either(x, y), x,
        lazy_args=[0]
    )
    primitives[def_eithers.from_right.name] = prims.prim2(
        def_eithers.from_right.name, eithers.from_right, [_x, _y],
        y, prims.either(x, y), y,
        lazy_args=[0]
    )
    primitives[def_eithers.is_left.name] = prims.prim1(
        def_eithers.is_left.name, eithers.is_left, [_x, _y],
        prims.either(x, y), prims.boolean()
    )
    primitives[def_eithers.is_right.name] = prims.prim1(
        def_eithers.is_right.name, eithers.is_right, [_x, _y],
        prims.either(x, y), prims.boolean()
    )
    primitives[def_eithers.lefts.name] = prims.prim1(
        def_eithers.lefts.name, eithers.lefts, [_x, _y],
        prims.list_(prims.either(x, y)), prims.list_(x)
    )
    # map :: (x -> y) -> Either z x -> Either z y
    primitives[def_eithers.map.name] = prims.prim2(
        def_eithers.map.name, eithers.map, [_x, _y, _z],
        fun(x, y), prims.either(z, x), prims.either(z, y)
    )
    # mapList :: (x -> Either z y) -> [x] -> Either z [y]
    primitives[def_eithers.map_list.name] = prims.prim2(
        def_eithers.map_list.name, eithers.map_list, [_x, _y, _z],
        fun(x, prims.either(z, y)), prims.list_(x), prims.either(z, prims.list_(y))
    )
    # mapOptional :: (x -> Either z y) -> optional x -> Either z (optional y)
    primitives[def_eithers.map_optional.name] = prims.prim2(
        def_eithers.map_optional.name, eithers.map_optional, [_x, _y, _z],
        fun(x, prims.either(z, y)), prims.optional(x), prims.either(z, prims.optional(y))
    )
    # mapSet :: (x -> Either z y) -> Set x -> Either z (Set y)
    primitives[def_eithers.map_set.name] = prims.prim2(
        def_eithers.map_set.name, eithers.map_set, [_x, _y, _z],
        fun(x, prims.either(z, y)), prims.set_(x), prims.either(z, prims.set_(y))
    )
    primitives[def_eithers.partition_eithers.name] = prims.prim1(
        def_eithers.partition_eithers.name, eithers.partition_eithers, [_x, _y],
        prims.list_(prims.either(x, y)), prims.pair(prims.list_(x), prims.list_(y))
    )
    primitives[def_eithers.rights.name] = prims.prim1(
        def_eithers.rights.name, eithers.rights, [_x, _y],
        prims.list_(prims.either(x, y)), prims.list_(y)
    )

    return primitives


def register_lists_primitives() -> dict[Name, Primitive]:
    """Register all list primitive functions."""
    from hydra.overlay.python.lib import lists
    from hydra.lib import lists as def_lists

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
    primitives[def_lists.apply.name] = prims.prim2(
        def_lists.apply.name, lists.apply, [_a, _b],
        prims.list_(fun(a, b)), prims.list_(a), prims.list_(b)
    )
    # prim2: bind :: [a] -> (a -> [b]) -> [b]
    primitives[def_lists.bind.name] = prims.prim2(
        def_lists.bind.name, lists.bind, [_a, _b],
        prims.list_(a), fun(a, prims.list_(b)), prims.list_(b)
    )
    # prim1: concat :: [[a]] -> [a]
    primitives[def_lists.concat.name] = prims.prim1(
        def_lists.concat.name, lists.concat, [_a],
        prims.list_(prims.list_(a)), prims.list_(a)
    )
    # prim2: concat2 :: [a] -> [a] -> [a]
    primitives[def_lists.concat2.name] = prims.prim2(
        def_lists.concat2.name, lists.concat2, [_a],
        prims.list_(a), prims.list_(a), prims.list_(a)
    )
    # prim2: cons :: a -> [a] -> [a]
    primitives[def_lists.cons.name] = prims.prim2(
        def_lists.cons.name, lists.cons, [_a],
        a, prims.list_(a), prims.list_(a)
    )
    # prim2: drop :: Int32 -> [a] -> [a]
    primitives[def_lists.drop.name] = prims.prim2(
        def_lists.drop.name, lists.drop, [_a],
        prims.int32(), prims.list_(a), prims.list_(a)
    )
    # prim2: dropWhile :: (a -> Bool) -> [a] -> [a]
    primitives[def_lists.drop_while.name] = prims.prim2(
        def_lists.drop_while.name, lists.drop_while, [_a],
        fun(a, prims.boolean()), prims.list_(a), prims.list_(a)
    )
    # prim2: elem :: Eq a => a -> [a] -> Bool
    primitives[def_lists.elem.name] = prims.prim2(
        def_lists.elem.name, lists.elem, [_aEq],
        a, prims.list_(a), prims.boolean()
    )
    # prim2: filter :: (a -> Bool) -> [a] -> [a]
    primitives[def_lists.filter.name] = prims.prim2(
        def_lists.filter.name, lists.filter, [_a],
        fun(a, prims.boolean()), prims.list_(a), prims.list_(a)
    )
    # prim2: find :: (a -> Bool) -> [a] -> optional a
    primitives[def_lists.find.name] = prims.prim2(
        def_lists.find.name, lists.find, [_a],
        fun(a, prims.boolean()), prims.list_(a), prims.optional(a)
    )
    # prim3: foldl :: (b -> a -> b) -> b -> [a] -> b
    primitives[def_lists.foldl.name] = prims.prim3(
        def_lists.foldl.name, lambda f, init, xs: lists.foldl(lambda acc, el: f(acc)(el), init, xs), [_b, _a],
        fun(b, fun(a, b)), b, prims.list_(a), b
    )
    # prim3: foldr :: (a -> b -> b) -> b -> [a] -> b
    primitives[def_lists.foldr.name] = prims.prim3(
        def_lists.foldr.name, lambda f, init, xs: lists.foldr(lambda el, acc: f(el)(acc), init, xs), [_a, _b],
        fun(a, fun(b, b)), b, prims.list_(a), b
    )
    # prim1: group :: Eq a => [a] -> [[a]]
    primitives[def_lists.group.name] = prims.prim1(
        def_lists.group.name, lists.group, [_aEq],
        prims.list_(a), prims.list_(prims.list_(a))
    )
    # prim2: intercalate :: [a] -> [[a]] -> [a]
    primitives[def_lists.intercalate.name] = prims.prim2(
        def_lists.intercalate.name, lists.intercalate, [_a],
        prims.list_(a), prims.list_(prims.list_(a)), prims.list_(a)
    )
    # prim2: intersperse :: a -> [a] -> [a]
    primitives[def_lists.intersperse.name] = prims.prim2(
        def_lists.intersperse.name, lists.intersperse, [_a],
        a, prims.list_(a), prims.list_(a)
    )
    # prim1: length :: [a] -> Int32
    primitives[def_lists.length.name] = prims.prim1(
        def_lists.length.name, lists.length, [_a], prims.list_(a), prims.int32()
    )
    # prim2: map :: (a -> b) -> [a] -> [b]
    primitives[def_lists.map.name] = prims.prim2(
        def_lists.map.name, lists.map, [_a, _b],
        fun(a, b), prims.list_(a), prims.list_(b)
    )
    # prim2: maybeAt :: Int32 -> [a] -> optional a
    primitives[def_lists.maybe_at.name] = prims.prim2(
        def_lists.maybe_at.name, lists.maybe_at, [_a],
        prims.int32(), prims.list_(a), prims.optional(a)
    )
    # prim1: maybeHead :: [a] -> optional a
    primitives[def_lists.maybe_head.name] = prims.prim1(
        def_lists.maybe_head.name, lists.maybe_head, [_a],
        prims.list_(a), prims.optional(a)
    )
    # prim1: maybeInit :: [a] -> optional [a]
    primitives[def_lists.maybe_init.name] = prims.prim1(
        def_lists.maybe_init.name, lists.maybe_init, [_a],
        prims.list_(a), prims.optional(prims.list_(a))
    )
    # prim1: maybeLast :: [a] -> optional a
    primitives[def_lists.maybe_last.name] = prims.prim1(
        def_lists.maybe_last.name, lists.maybe_last, [_a],
        prims.list_(a), prims.optional(a)
    )
    # prim1: maybeTail :: [a] -> optional [a]
    primitives[def_lists.maybe_tail.name] = prims.prim1(
        def_lists.maybe_tail.name, lists.maybe_tail, [_a],
        prims.list_(a), prims.optional(prims.list_(a))
    )
    # prim1: nub :: Eq a => [a] -> [a]
    primitives[def_lists.nub.name] = prims.prim1(
        def_lists.nub.name, lists.nub, [_aEq],
        prims.list_(a), prims.list_(a)
    )
    # prim1: null :: [a] -> Bool
    primitives[def_lists.null.name] = prims.prim1(
        def_lists.null.name, lists.null, [_a],
        prims.list_(a), prims.boolean()
    )
    # prim2: partition :: (a -> Bool) -> [a] -> ([a], [a])
    primitives[def_lists.partition.name] = prims.prim2(
        def_lists.partition.name, lists.partition, [_a],
        fun(a, prims.boolean()), prims.list_(a), prims.pair(prims.list_(a), prims.list_(a))
    )
    # prim1: pure :: a -> [a]
    primitives[def_lists.pure.name] = prims.prim1(
        def_lists.pure.name, lists.pure, [_a],
        a, prims.list_(a)
    )
    # prim2: replicate :: Int32 -> a -> [a]
    primitives[def_lists.replicate.name] = prims.prim2(
        def_lists.replicate.name, lists.replicate, [_a],
        prims.int32(), a, prims.list_(a)
    )
    # prim1: reverse :: [a] -> [a]
    primitives[def_lists.reverse.name] = prims.prim1(
        def_lists.reverse.name, lists.reverse, [_a], prims.list_(a), prims.list_(a)
    )
    # prim1: singleton :: a -> [a]
    primitives[def_lists.singleton.name] = prims.prim1(
        def_lists.singleton.name, lists.singleton, [_a],
        a, prims.list_(a)
    )
    # prim1: sort :: Ord a => [a] -> [a]
    primitives[def_lists.sort.name] = prims.prim1(
        def_lists.sort.name, lists.sort, [_aOrd],
        prims.list_(a), prims.list_(a)
    )
    # prim2: sortOn :: Ord b => (a -> b) -> [a] -> [a]
    primitives[def_lists.sort_on.name] = prims.prim2(
        def_lists.sort_on.name, lists.sort_on, [_a, _bOrd],
        fun(a, b), prims.list_(a), prims.list_(a)
    )
    # prim2: span :: (a -> Bool) -> [a] -> ([a], [a])
    primitives[def_lists.span.name] = prims.prim2(
        def_lists.span.name, lists.span, [_a],
        fun(a, prims.boolean()), prims.list_(a), prims.pair(prims.list_(a), prims.list_(a))
    )
    # prim2: take :: Int32 -> [a] -> [a]
    primitives[def_lists.take.name] = prims.prim2(
        def_lists.take.name, lists.take, [_a],
        prims.int32(), prims.list_(a), prims.list_(a)
    )
    # prim1: transpose :: [[a]] -> [[a]]
    primitives[def_lists.transpose.name] = prims.prim1(
        def_lists.transpose.name, lists.transpose, [_a],
        prims.list_(prims.list_(a)), prims.list_(prims.list_(a))
    )
    # prim1: uncons :: [a] -> optional (a, [a])
    primitives[def_lists.uncons.name] = prims.prim1(
        def_lists.uncons.name, lists.uncons, [_a],
        prims.list_(a), prims.optional(prims.pair(a, prims.list_(a)))
    )
    # prim2: zip :: [a] -> [b] -> [(a, b)]
    primitives[def_lists.zip.name] = prims.prim2(
        def_lists.zip.name, lists.zip, [_a, _b],
        prims.list_(a), prims.list_(b), prims.list_(prims.pair(a, b))
    )
    # prim3: zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    primitives[def_lists.zip_with.name] = prims.prim3(
        def_lists.zip_with.name, lambda f, xs, ys: lists.zip_with(lambda a, b: f(a)(b), xs, ys), [_a, _b, _c],
        fun(a, fun(b, c)), prims.list_(a), prims.list_(b), prims.list_(c)
    )

    return primitives


def register_logic_primitives() -> dict[Name, Primitive]:
    """Register all logic primitive functions."""
    from hydra.overlay.python.lib import logic
    from hydra.lib import logic as def_logic

    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")
    _a = prims.v("a")

    primitives[def_logic.and_.name] = prims.prim2(
        def_logic.and_.name, logic.and_, [], prims.boolean(), prims.boolean(), prims.boolean()
    )
    primitives[def_logic.if_else.name] = prims.prim3(
        def_logic.if_else.name, logic.if_else, [_a],
        prims.boolean(), a, a, a,
        lazy_args=[1, 2]
    )
    primitives[def_logic.not_.name] = prims.prim1(
        def_logic.not_.name, logic.not_, [], prims.boolean(), prims.boolean()
    )
    primitives[def_logic.or_.name] = prims.prim2(
        def_logic.or_.name, logic.or_, [], prims.boolean(), prims.boolean(), prims.boolean()
    )

    return primitives


def register_maps_primitives() -> dict[Name, Primitive]:
    """Register all map primitive functions."""
    from hydra.overlay.python.lib import maps
    from hydra.lib import maps as def_maps

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

    # prim3: alter :: Ord k => (optional v -> optional v) -> k -> Map k v -> Map k v
    primitives[def_maps.alter.name] = prims.prim3(
        def_maps.alter.name, maps.alter, [_v, _kOrd],
        fun(prims.optional(v), prims.optional(v)), k, map_kv, map_kv
    )
    # prim3: bimap :: (Ord k1, Ord k2) => (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
    primitives[def_maps.bimap.name] = prims.prim3(
        def_maps.bimap.name, maps.bimap, [_k1Ord, _k2Ord, _v1, _v2],
        fun(k1, k2), fun(v1, v2), prims.map_(k1, v1), prims.map_(k2, v2)
    )
    # prim2: delete :: Ord k => k -> Map k v -> Map k v
    primitives[def_maps.delete.name] = prims.prim2(
        def_maps.delete.name, maps.delete, [_kOrd, _v],
        k, map_kv, map_kv
    )
    # prim1: elems :: Ord k => Map k v -> [v]
    primitives[def_maps.elems.name] = prims.prim1(
        def_maps.elems.name, maps.elems, [_kOrd, _v],
        map_kv, prims.list_(v)
    )
    # prim0: empty :: Ord k => Map k v
    primitives[def_maps.empty.name] = prims.prim0(
        def_maps.empty.name, maps.empty, [_kOrd, _v],
        map_kv
    )
    # prim2: filter :: Ord k => (v -> Bool) -> Map k v -> Map k v
    primitives[def_maps.filter.name] = prims.prim2(
        def_maps.filter.name, maps.filter, [_v, _kOrd],
        fun(v, prims.boolean()), map_kv, map_kv
    )
    # prim2: filterWithKey :: Ord k => (k -> v -> Bool) -> Map k v -> Map k v
    primitives[def_maps.filter_with_key.name] = prims.prim2(
        def_maps.filter_with_key.name, lambda f, m: maps.filter_with_key(lambda k, v: f(k)(v), m), [_kOrd, _v],
        fun(k, fun(v, prims.boolean())), map_kv, map_kv
    )
    # prim3: findWithDefault :: Ord k => v -> k -> Map k v -> v
    primitives[def_maps.find_with_default.name] = prims.prim3(
        def_maps.find_with_default.name, maps.find_with_default, [_v, _kOrd],
        v, k, map_kv, v,
        lazy_args=[0]
    )
    # prim1: fromList :: Ord k => [(k, v)] -> Map k v
    primitives[def_maps.from_list.name] = prims.prim1(
        def_maps.from_list.name, maps.from_list, [_kOrd, _v],
        prims.list_(prims.pair(k, v)), map_kv
    )
    # prim3: insert :: Ord k => k -> v -> Map k v -> Map k v
    primitives[def_maps.insert.name] = prims.prim3(
        def_maps.insert.name, maps.insert, [_kOrd, _v],
        k, v, map_kv, map_kv
    )
    # prim1: keys :: Ord k => Map k v -> [k]
    primitives[def_maps.keys.name] = prims.prim1(
        def_maps.keys.name, maps.keys, [_kOrd, _v],
        map_kv, prims.list_(k)
    )
    # prim2: lookup :: Ord k => k -> Map k v -> optional v
    primitives[def_maps.lookup.name] = prims.prim2(
        def_maps.lookup.name, maps.lookup, [_kOrd, _v],
        k, map_kv, prims.optional(v)
    )
    # prim2: map :: Ord k => (v1 -> v2) -> Map k v1 -> Map k v2
    primitives[def_maps.map.name] = prims.prim2(
        def_maps.map.name, maps.map, [_v1, _v2, _kOrd],
        fun(v1, v2), prims.map_(k, v1), prims.map_(k, v2)
    )
    # prim2: mapKeys :: (Ord k1, Ord k2) => (k1 -> k2) -> Map k1 v -> Map k2 v
    primitives[def_maps.map_keys.name] = prims.prim2(
        def_maps.map_keys.name, maps.map_keys, [_k1Ord, _k2Ord, _v],
        fun(k1, k2), prims.map_(k1, v), prims.map_(k2, v)
    )
    # prim2: member :: Ord k => k -> Map k v -> Bool
    primitives[def_maps.member.name] = prims.prim2(
        def_maps.member.name, maps.member, [_kOrd, _v],
        k, map_kv, prims.boolean()
    )
    # prim1: null :: Ord k => Map k v -> Bool
    primitives[def_maps.null.name] = prims.prim1(
        def_maps.null.name, maps.null, [_kOrd, _v],
        map_kv, prims.boolean()
    )
    # prim2: singleton :: Ord k => k -> v -> Map k v
    primitives[def_maps.singleton.name] = prims.prim2(
        def_maps.singleton.name, maps.singleton, [_kOrd, _v],
        k, v, map_kv
    )
    # prim1: size :: Ord k => Map k v -> Int32
    primitives[def_maps.size.name] = prims.prim1(
        def_maps.size.name, maps.size, [_kOrd, _v],
        map_kv, prims.int32()
    )
    # prim1: toList :: Ord k => Map k v -> [(k, v)]
    primitives[def_maps.to_list.name] = prims.prim1(
        def_maps.to_list.name, maps.to_list, [_kOrd, _v],
        map_kv, prims.list_(prims.pair(k, v))
    )
    # prim2: union :: Ord k => Map k v -> Map k v -> Map k v
    primitives[def_maps.union.name] = prims.prim2(
        def_maps.union.name, maps.union, [_kOrd, _v],
        map_kv, map_kv, map_kv
    )

    return primitives


def register_math_primitives() -> dict[Name, Primitive]:
    """Register all math primitive functions."""
    from hydra.overlay.python.lib import math
    from hydra.lib import math as def_math

    primitives: dict[Name, Primitive] = {}

    # Int32 primitives
    primitives[def_math.abs.name] = prims.prim1(
        def_math.abs.name, math.abs_, [], prims.int32(), prims.int32()
    )
    primitives[def_math.add.name] = prims.prim2(
        def_math.add.name, math.add, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[def_math.add_float64.name] = prims.prim2(
        def_math.add_float64.name, math.add_float64, [], prims.float64(), prims.float64(), prims.float64()
    )
    primitives[def_math.even.name] = prims.prim1(
        def_math.even.name, math.even, [], prims.int32(), prims.boolean()
    )
    primitives[def_math.mul.name] = prims.prim2(
        def_math.mul.name, math.mul, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[def_math.mul_float64.name] = prims.prim2(
        def_math.mul_float64.name, math.mul_float64, [], prims.float64(), prims.float64(), prims.float64()
    )
    primitives[def_math.negate.name] = prims.prim1(
        def_math.negate.name, math.negate, [], prims.int32(), prims.int32()
    )
    primitives[def_math.negate_float64.name] = prims.prim1(
        def_math.negate_float64.name, math.negate_float64, [], prims.float64(), prims.float64()
    )
    primitives[def_math.odd.name] = prims.prim1(
        def_math.odd.name, math.odd, [], prims.int32(), prims.boolean()
    )
    primitives[def_math.range_.name] = prims.prim2(
        def_math.range_.name, math.range_, [], prims.int32(), prims.int32(), prims.list_(prims.int32())
    )
    primitives[def_math.signum.name] = prims.prim1(
        def_math.signum.name, math.signum, [], prims.int32(), prims.int32()
    )
    primitives[def_math.sub.name] = prims.prim2(
        def_math.sub.name, math.sub, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[def_math.sub_float64.name] = prims.prim2(
        def_math.sub_float64.name, math.sub_float64, [], prims.float64(), prims.float64(), prims.float64()
    )
    primitives[def_math.max.name] = prims.prim2(
        def_math.max.name, math.max, [], prims.int32(), prims.int32(), prims.int32()
    )
    primitives[def_math.maybe_div.name] = prims.prim2(
        def_math.maybe_div.name, math.maybe_div, [], prims.int32(), prims.int32(), prims.optional(prims.int32())
    )
    primitives[def_math.maybe_mod.name] = prims.prim2(
        def_math.maybe_mod.name, math.maybe_mod, [], prims.int32(), prims.int32(), prims.optional(prims.int32())
    )
    primitives[def_math.maybe_pred.name] = prims.prim1(
        def_math.maybe_pred.name, math.maybe_pred, [], prims.int32(), prims.optional(prims.int32())
    )
    primitives[def_math.maybe_rem.name] = prims.prim2(
        def_math.maybe_rem.name, math.maybe_rem, [], prims.int32(), prims.int32(), prims.optional(prims.int32())
    )
    primitives[def_math.maybe_succ.name] = prims.prim1(
        def_math.maybe_succ.name, math.maybe_succ, [], prims.int32(), prims.optional(prims.int32())
    )
    primitives[def_math.min.name] = prims.prim2(
        def_math.min.name, math.min, [], prims.int32(), prims.int32(), prims.int32()
    )

    # Float64 primitives
    primitives[def_math.acos.name] = prims.prim1(
        def_math.acos.name, math.acos, [], prims.float64(), prims.float64()
    )
    primitives[def_math.acosh.name] = prims.prim1(
        def_math.acosh.name, math.acosh, [], prims.float64(), prims.float64()
    )
    primitives[def_math.asin.name] = prims.prim1(
        def_math.asin.name, math.asin, [], prims.float64(), prims.float64()
    )
    primitives[def_math.asinh.name] = prims.prim1(
        def_math.asinh.name, math.asinh, [], prims.float64(), prims.float64()
    )
    primitives[def_math.atan.name] = prims.prim1(
        def_math.atan.name, math.atan, [], prims.float64(), prims.float64()
    )
    primitives[def_math.atan2.name] = prims.prim2(
        def_math.atan2.name, math.atan2, [], prims.float64(), prims.float64(), prims.float64()
    )
    primitives[def_math.atanh.name] = prims.prim1(
        def_math.atanh.name, math.atanh, [], prims.float64(), prims.float64()
    )
    primitives[def_math.ceiling.name] = prims.prim1(
        def_math.ceiling.name, math.ceiling, [], prims.float64(), prims.float64()
    )
    primitives[def_math.cos.name] = prims.prim1(
        def_math.cos.name, math.cos, [], prims.float64(), prims.float64()
    )
    primitives[def_math.cosh.name] = prims.prim1(
        def_math.cosh.name, math.cosh, [], prims.float64(), prims.float64()
    )
    primitives[def_math.e.name] = prims.prim0(
        def_math.e.name, math.e, [], prims.float64()
    )
    primitives[def_math.exp.name] = prims.prim1(
        def_math.exp.name, math.exp, [], prims.float64(), prims.float64()
    )
    primitives[def_math.floor.name] = prims.prim1(
        def_math.floor.name, math.floor, [], prims.float64(), prims.float64()
    )
    primitives[def_math.log.name] = prims.prim1(
        def_math.log.name, math.log, [], prims.float64(), prims.float64()
    )
    primitives[def_math.log_base.name] = prims.prim2(
        def_math.log_base.name, math.log_base, [], prims.float64(), prims.float64(), prims.float64()
    )
    primitives[def_math.pi.name] = prims.prim0(
        def_math.pi.name, math.pi, [], prims.float64()
    )
    primitives[def_math.pow.name] = prims.prim2(
        def_math.pow.name, math.pow_, [], prims.float64(), prims.float64(), prims.float64()
    )
    primitives[def_math.round.name] = prims.prim1(
        def_math.round.name, math.round_, [], prims.float64(), prims.float64()
    )
    primitives[def_math.round_float32.name] = prims.prim2(
        def_math.round_float32.name, math.round_float32, [], prims.int32(), prims.float32(), prims.float32()
    )
    primitives[def_math.round_float64.name] = prims.prim2(
        def_math.round_float64.name, math.round_float64, [], prims.int32(), prims.float64(), prims.float64()
    )
    primitives[def_math.sin.name] = prims.prim1(
        def_math.sin.name, math.sin, [], prims.float64(), prims.float64()
    )
    primitives[def_math.sinh.name] = prims.prim1(
        def_math.sinh.name, math.sinh, [], prims.float64(), prims.float64()
    )
    primitives[def_math.sqrt.name] = prims.prim1(
        def_math.sqrt.name, math.sqrt, [], prims.float64(), prims.float64()
    )
    primitives[def_math.tan.name] = prims.prim1(
        def_math.tan.name, math.tan, [], prims.float64(), prims.float64()
    )
    primitives[def_math.tanh.name] = prims.prim1(
        def_math.tanh.name, math.tanh, [], prims.float64(), prims.float64()
    )
    primitives[def_math.truncate.name] = prims.prim1(
        def_math.truncate.name, math.truncate, [], prims.float64(), prims.float64()
    )

    return primitives


def register_optionals_primitives() -> dict[Name, Primitive]:
    """Register all optional primitive functions."""
    from hydra.overlay.python.lib import optionals
    from hydra.lib import optionals as def_optionals

    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")
    b = prims.variable("b")
    c = prims.variable("c")
    _a = prims.v("a")
    _b = prims.v("b")
    _c = prims.v("c")

    # apply :: optional (a -> b) -> optional a -> optional b
    primitives[def_optionals.apply.name] = prims.prim2(
        def_optionals.apply.name, optionals.apply, [_a, _b],
        prims.optional(fun(a, b)), prims.optional(a), prims.optional(b)
    )
    # bind :: optional a -> (a -> optional b) -> optional b
    primitives[def_optionals.bind.name] = prims.prim2(
        def_optionals.bind.name, optionals.bind, [_a, _b],
        prims.optional(a), fun(a, prims.optional(b)), prims.optional(b)
    )
    # cases :: optional a -> b -> (a -> b) -> b
    primitives[def_optionals.cases.name] = prims.prim3(
        def_optionals.cases.name, optionals.cases, [_a, _b],
        prims.optional(a), b, fun(a, b), b,
        lazy_args=[1]
    )
    # cat :: [optional a] -> [a]
    primitives[def_optionals.cat.name] = prims.prim1(
        def_optionals.cat.name, optionals.cat, [_a],
        prims.list_(prims.optional(a)), prims.list_(a)
    )
    # compose :: (a -> optional b) -> (b -> optional c) -> a -> optional c
    primitives[def_optionals.compose.name] = prims.prim3(
        def_optionals.compose.name, optionals.compose, [_a, _b, _c],
        fun(a, prims.optional(b)), fun(b, prims.optional(c)),
        a, prims.optional(c)
    )
    primitives[def_optionals.from_optional.name] = prims.prim2(
        def_optionals.from_optional.name, optionals.from_optional, [_a],
        a, prims.optional(a), a,
        lazy_args=[0]
    )
    primitives[def_optionals.is_given.name] = prims.prim1(
        def_optionals.is_given.name, optionals.is_given, [_a],
        prims.optional(a), prims.boolean()
    )
    primitives[def_optionals.is_none.name] = prims.prim1(
        def_optionals.is_none.name, optionals.is_none, [_a],
        prims.optional(a), prims.boolean()
    )
    # map :: (a -> b) -> optional a -> optional b
    primitives[def_optionals.map.name] = prims.prim2(
        def_optionals.map.name, optionals.map, [_a, _b],
        fun(a, b), prims.optional(a), prims.optional(b)
    )
    # mapOptional :: (a -> optional b) -> [a] -> [b]
    primitives[def_optionals.map_optional.name] = prims.prim2(
        def_optionals.map_optional.name, optionals.map_optional, [_a, _b],
        fun(a, prims.optional(b)), prims.list_(a), prims.list_(b)
    )
    primitives[def_optionals.pure.name] = prims.prim1(
        def_optionals.pure.name, optionals.pure, [_a],
        a, prims.optional(a)
    )
    # toList :: optional a -> [a]
    primitives[def_optionals.to_list.name] = prims.prim1(
        def_optionals.to_list.name, optionals.to_list, [_a],
        prims.optional(a), prims.list_(a)
    )

    return primitives


def register_sets_primitives() -> dict[Name, Primitive]:
    """Register all set primitive functions."""
    from hydra.overlay.python.lib import sets
    from hydra.lib import sets as def_sets

    primitives: dict[Name, Primitive] = {}

    a = prims.variable("a")
    b = prims.variable("b")
    _aOrd = prims.v_ord("a")
    _bOrd = prims.v_ord("b")

    primitives[def_sets.delete.name] = prims.prim2(
        def_sets.delete.name, sets.delete, [_aOrd],
        a, prims.set_(a), prims.set_(a)
    )
    primitives[def_sets.difference.name] = prims.prim2(
        def_sets.difference.name, sets.difference, [_aOrd],
        prims.set_(a), prims.set_(a), prims.set_(a)
    )
    # prim0: empty :: Ord a => Set a
    primitives[def_sets.empty.name] = prims.prim0(
        def_sets.empty.name, sets.empty, [_aOrd],
        prims.set_(a)
    )
    primitives[def_sets.from_list.name] = prims.prim1(
        def_sets.from_list.name, sets.from_list, [_aOrd],
        prims.list_(a), prims.set_(a)
    )
    primitives[def_sets.insert.name] = prims.prim2(
        def_sets.insert.name, sets.insert, [_aOrd],
        a, prims.set_(a), prims.set_(a)
    )
    primitives[def_sets.intersection.name] = prims.prim2(
        def_sets.intersection.name, sets.intersection, [_aOrd],
        prims.set_(a), prims.set_(a), prims.set_(a)
    )
    primitives[def_sets.map.name] = prims.prim2(
        def_sets.map.name, sets.map, [_aOrd, _bOrd],
        fun(a, b), prims.set_(a), prims.set_(b)
    )
    primitives[def_sets.member.name] = prims.prim2(
        def_sets.member.name, sets.member, [_aOrd],
        a, prims.set_(a), prims.boolean()
    )
    primitives[def_sets.null.name] = prims.prim1(
        def_sets.null.name, sets.null, [_aOrd],
        prims.set_(a), prims.boolean()
    )
    primitives[def_sets.singleton.name] = prims.prim1(
        def_sets.singleton.name, sets.singleton, [_aOrd],
        a, prims.set_(a)
    )
    primitives[def_sets.size.name] = prims.prim1(
        def_sets.size.name, sets.size, [_aOrd],
        prims.set_(a), prims.int32()
    )
    primitives[def_sets.to_list.name] = prims.prim1(
        def_sets.to_list.name, sets.to_list, [_aOrd],
        prims.set_(a), prims.list_(a)
    )
    primitives[def_sets.union.name] = prims.prim2(
        def_sets.union.name, sets.union, [_aOrd],
        prims.set_(a), prims.set_(a), prims.set_(a)
    )
    primitives[def_sets.unions.name] = prims.prim1(
        def_sets.unions.name, sets.unions, [_aOrd],
        prims.list_(prims.set_(a)), prims.set_(a)
    )

    return primitives


def register_regex_primitives() -> dict[Name, Primitive]:
    """Register all regex primitive functions."""
    from hydra.overlay.python.lib import regex
    from hydra.lib import regex as def_regex

    primitives: dict[Name, Primitive] = {}

    primitives[def_regex.find.name] = prims.prim2(
        def_regex.find.name, regex.find, [], prims.string(), prims.string(), prims.optional(prims.string())
    )
    primitives[def_regex.find_all.name] = prims.prim2(
        def_regex.find_all.name, regex.find_all, [], prims.string(), prims.string(), prims.list_(prims.string())
    )
    primitives[def_regex.matches.name] = prims.prim2(
        def_regex.matches.name, regex.matches, [], prims.string(), prims.string(), prims.boolean()
    )
    primitives[def_regex.replace.name] = prims.prim3(
        def_regex.replace.name, regex.replace, [], prims.string(), prims.string(), prims.string(), prims.string()
    )
    primitives[def_regex.replace_all.name] = prims.prim3(
        def_regex.replace_all.name, regex.replace_all, [], prims.string(), prims.string(), prims.string(), prims.string()
    )
    primitives[def_regex.split.name] = prims.prim2(
        def_regex.split.name, regex.split, [], prims.string(), prims.string(), prims.list_(prims.string())
    )

    return primitives


def register_strings_primitives() -> dict[Name, Primitive]:
    """Register all string primitive functions."""
    from hydra.overlay.python.lib import strings
    from hydra.lib import strings as def_strings

    primitives: dict[Name, Primitive] = {}

    primitives[def_strings.cat.name] = prims.prim1(
        def_strings.cat.name, strings.cat, [], prims.list_(prims.string()), prims.string()
    )
    primitives[def_strings.cat2.name] = prims.prim2(
        def_strings.cat2.name, strings.cat2, [], prims.string(), prims.string(), prims.string()
    )
    primitives[def_strings.from_list.name] = prims.prim1(
        def_strings.from_list.name, strings.from_list, [], prims.list_(prims.int32()), prims.string()
    )
    primitives[def_strings.intercalate.name] = prims.prim2(
        def_strings.intercalate.name, strings.intercalate, [], prims.string(), prims.list_(prims.string()), prims.string()
    )
    primitives[def_strings.length.name] = prims.prim1(
        def_strings.length.name, strings.length, [], prims.string(), prims.int32()
    )
    primitives[def_strings.lines.name] = prims.prim1(
        def_strings.lines.name, strings.lines, [], prims.string(), prims.list_(prims.string())
    )
    primitives[def_strings.maybe_char_at.name] = prims.prim2(
        def_strings.maybe_char_at.name, strings.maybe_char_at, [], prims.int32(), prims.string(), prims.optional(prims.int32())
    )
    primitives[def_strings.null.name] = prims.prim1(
        def_strings.null.name, strings.null, [], prims.string(), prims.boolean()
    )
    primitives[def_strings.split_on.name] = prims.prim2(
        def_strings.split_on.name, strings.split_on, [], prims.string(), prims.string(),
        prims.list_(prims.string())
    )
    primitives[def_strings.to_list.name] = prims.prim1(
        def_strings.to_list.name, strings.to_list, [], prims.string(), prims.list_(prims.int32())
    )
    primitives[def_strings.to_lower.name] = prims.prim1(
        def_strings.to_lower.name, strings.to_lower, [], prims.string(), prims.string()
    )
    primitives[def_strings.to_upper.name] = prims.prim1(
        def_strings.to_upper.name, strings.to_upper, [], prims.string(), prims.string()
    )
    primitives[def_strings.unlines.name] = prims.prim1(
        def_strings.unlines.name, strings.unlines, [], prims.list_(prims.string()), prims.string()
    )

    return primitives


def register_text_primitives() -> dict[Name, Primitive]:
    """Register all text primitive functions (UTF-8 codecs). For #494."""
    from hydra.overlay.python.lib import text
    from hydra.lib import text as def_text

    primitives: dict[Name, Primitive] = {}

    # decodeUtf8 :: binary -> Either string string
    primitives[def_text.decode_utf8.name] = prims.prim1(
        def_text.decode_utf8.name, text.decode_utf8, [],
        prims.binary(), prims.either(prims.string(), prims.string())
    )
    # encodeUtf8 :: string -> binary
    primitives[def_text.encode_utf8.name] = prims.prim1(
        def_text.encode_utf8.name, text.encode_utf8, [],
        prims.string(), prims.binary()
    )

    return primitives


def register_literals_primitives() -> dict[Name, Primitive]:
    """Register all literals primitive functions."""
    from hydra.overlay.python.lib import literals
    from hydra.lib import literals as def_literals

    primitives: dict[Name, Primitive] = {}

    # Conversion primitives
    primitives[def_literals.bigint_to_decimal.name] = prims.prim1(
        def_literals.bigint_to_decimal.name, literals.bigint_to_decimal, [],
        prims.bigint(), prims.decimal()
    )
    primitives[def_literals.bigint_to_int8.name] = prims.prim1(
        def_literals.bigint_to_int8.name, literals.bigint_to_int8, [],
        prims.bigint(), prims.int8()
    )
    primitives[def_literals.bigint_to_int16.name] = prims.prim1(
        def_literals.bigint_to_int16.name, literals.bigint_to_int16, [],
        prims.bigint(), prims.int16()
    )
    primitives[def_literals.bigint_to_int32.name] = prims.prim1(
        def_literals.bigint_to_int32.name, literals.bigint_to_int32, [],
        prims.bigint(), prims.int32()
    )
    primitives[def_literals.bigint_to_int64.name] = prims.prim1(
        def_literals.bigint_to_int64.name, literals.bigint_to_int64, [],
        prims.bigint(), prims.int64()
    )
    primitives[def_literals.bigint_to_uint8.name] = prims.prim1(
        def_literals.bigint_to_uint8.name, literals.bigint_to_uint8, [],
        prims.bigint(), prims.uint8()
    )
    primitives[def_literals.bigint_to_uint16.name] = prims.prim1(
        def_literals.bigint_to_uint16.name, literals.bigint_to_uint16, [],
        prims.bigint(), prims.uint16()
    )
    primitives[def_literals.bigint_to_uint32.name] = prims.prim1(
        def_literals.bigint_to_uint32.name, literals.bigint_to_uint32, [],
        prims.bigint(), prims.uint32()
    )
    primitives[def_literals.bigint_to_uint64.name] = prims.prim1(
        def_literals.bigint_to_uint64.name, literals.bigint_to_uint64, [],
        prims.bigint(), prims.uint64()
    )
    primitives[def_literals.binary_to_bytes.name] = prims.prim1(
        def_literals.binary_to_bytes.name, literals.binary_to_bytes, [],
        prims.binary(), prims.list_(prims.int32())
    )
    primitives[def_literals.binary_to_string.name] = prims.prim1(
        def_literals.binary_to_string.name, literals.binary_to_string, [],
        prims.binary(), prims.string()
    )
    primitives[def_literals.decimal_to_bigint.name] = prims.prim1(
        def_literals.decimal_to_bigint.name, literals.decimal_to_bigint, [],
        prims.decimal(), prims.bigint()
    )
    primitives[def_literals.decimal_to_float32.name] = prims.prim1(
        def_literals.decimal_to_float32.name, literals.decimal_to_float32, [],
        prims.decimal(), prims.float32()
    )
    primitives[def_literals.decimal_to_float64.name] = prims.prim1(
        def_literals.decimal_to_float64.name, literals.decimal_to_float64, [],
        prims.decimal(), prims.float64()
    )
    primitives[def_literals.float32_to_decimal.name] = prims.prim1(
        def_literals.float32_to_decimal.name, literals.float32_to_decimal, [],
        prims.float32(), prims.decimal()
    )
    primitives[def_literals.float32_to_float64.name] = prims.prim1(
        def_literals.float32_to_float64.name, literals.float32_to_float64, [],
        prims.float32(), prims.float64()
    )
    primitives[def_literals.float64_to_decimal.name] = prims.prim1(
        def_literals.float64_to_decimal.name, literals.float64_to_decimal, [],
        prims.float64(), prims.decimal()
    )
    primitives[def_literals.float64_to_float32.name] = prims.prim1(
        def_literals.float64_to_float32.name, literals.float64_to_float32, [],
        prims.float64(), prims.float32()
    )
    primitives[def_literals.int8_to_bigint.name] = prims.prim1(
        def_literals.int8_to_bigint.name, literals.int8_to_bigint, [],
        prims.int8(), prims.bigint()
    )
    primitives[def_literals.int16_to_bigint.name] = prims.prim1(
        def_literals.int16_to_bigint.name, literals.int16_to_bigint, [],
        prims.int16(), prims.bigint()
    )
    primitives[def_literals.int32_to_bigint.name] = prims.prim1(
        def_literals.int32_to_bigint.name, literals.int32_to_bigint, [],
        prims.int32(), prims.bigint()
    )
    primitives[def_literals.int64_to_bigint.name] = prims.prim1(
        def_literals.int64_to_bigint.name, literals.int64_to_bigint, [],
        prims.int64(), prims.bigint()
    )

    # Read primitives
    primitives[def_literals.read_bigint.name] = prims.prim1(
        def_literals.read_bigint.name, literals.read_bigint, [],
        prims.string(), prims.optional(prims.bigint())
    )
    primitives[def_literals.read_boolean.name] = prims.prim1(
        def_literals.read_boolean.name, literals.read_boolean, [],
        prims.string(), prims.optional(prims.boolean())
    )
    primitives[def_literals.read_decimal.name] = prims.prim1(
        def_literals.read_decimal.name, literals.read_decimal, [],
        prims.string(), prims.optional(prims.decimal())
    )
    primitives[def_literals.read_float32.name] = prims.prim1(
        def_literals.read_float32.name, literals.read_float32, [],
        prims.string(), prims.optional(prims.float32())
    )
    primitives[def_literals.read_float64.name] = prims.prim1(
        def_literals.read_float64.name, literals.read_float64, [],
        prims.string(), prims.optional(prims.float64())
    )
    primitives[def_literals.read_int8.name] = prims.prim1(
        def_literals.read_int8.name, literals.read_int8, [],
        prims.string(), prims.optional(prims.int8())
    )
    primitives[def_literals.read_int16.name] = prims.prim1(
        def_literals.read_int16.name, literals.read_int16, [],
        prims.string(), prims.optional(prims.int16())
    )
    primitives[def_literals.read_int32.name] = prims.prim1(
        def_literals.read_int32.name, literals.read_int32, [],
        prims.string(), prims.optional(prims.int32())
    )
    primitives[def_literals.read_int64.name] = prims.prim1(
        def_literals.read_int64.name, literals.read_int64, [],
        prims.string(), prims.optional(prims.int64())
    )
    primitives[def_literals.read_string.name] = prims.prim1(
        def_literals.read_string.name, literals.read_string, [],
        prims.string(), prims.optional(prims.string())
    )
    primitives[def_literals.read_uint8.name] = prims.prim1(
        def_literals.read_uint8.name, literals.read_uint8, [],
        prims.string(), prims.optional(prims.uint8())
    )
    primitives[def_literals.read_uint16.name] = prims.prim1(
        def_literals.read_uint16.name, literals.read_uint16, [],
        prims.string(), prims.optional(prims.uint16())
    )
    primitives[def_literals.read_uint32.name] = prims.prim1(
        def_literals.read_uint32.name, literals.read_uint32, [],
        prims.string(), prims.optional(prims.uint32())
    )
    primitives[def_literals.read_uint64.name] = prims.prim1(
        def_literals.read_uint64.name, literals.read_uint64, [],
        prims.string(), prims.optional(prims.uint64())
    )

    # Show primitives
    primitives[def_literals.show_bigint.name] = prims.prim1(
        def_literals.show_bigint.name, literals.show_bigint, [],
        prims.bigint(), prims.string()
    )
    primitives[def_literals.show_boolean.name] = prims.prim1(
        def_literals.show_boolean.name, literals.show_boolean, [],
        prims.boolean(), prims.string()
    )
    primitives[def_literals.show_decimal.name] = prims.prim1(
        def_literals.show_decimal.name, literals.show_decimal, [],
        prims.decimal(), prims.string()
    )
    primitives[def_literals.show_float32.name] = prims.prim1(
        def_literals.show_float32.name, literals.show_float32, [],
        prims.float32(), prims.string()
    )
    primitives[def_literals.show_float64.name] = prims.prim1(
        def_literals.show_float64.name, literals.show_float64, [],
        prims.float64(), prims.string()
    )
    primitives[def_literals.show_int8.name] = prims.prim1(
        def_literals.show_int8.name, literals.show_int8, [],
        prims.int8(), prims.string()
    )
    primitives[def_literals.show_int16.name] = prims.prim1(
        def_literals.show_int16.name, literals.show_int16, [],
        prims.int16(), prims.string()
    )
    primitives[def_literals.show_int32.name] = prims.prim1(
        def_literals.show_int32.name, literals.show_int32, [],
        prims.int32(), prims.string()
    )
    primitives[def_literals.show_int64.name] = prims.prim1(
        def_literals.show_int64.name, literals.show_int64, [],
        prims.int64(), prims.string()
    )
    primitives[def_literals.show_uint8.name] = prims.prim1(
        def_literals.show_uint8.name, literals.show_uint8, [],
        prims.uint8(), prims.string()
    )
    primitives[def_literals.show_uint16.name] = prims.prim1(
        def_literals.show_uint16.name, literals.show_uint16, [],
        prims.uint16(), prims.string()
    )
    primitives[def_literals.show_uint32.name] = prims.prim1(
        def_literals.show_uint32.name, literals.show_uint32, [],
        prims.uint32(), prims.string()
    )
    primitives[def_literals.show_uint64.name] = prims.prim1(
        def_literals.show_uint64.name, literals.show_uint64, [],
        prims.uint64(), prims.string()
    )
    primitives[def_literals.show_string.name] = prims.prim1(
        def_literals.show_string.name, literals.show_string, [],
        prims.string(), prims.string()
    )
    primitives[def_literals.string_to_binary.name] = prims.prim1(
        def_literals.string_to_binary.name, literals.string_to_binary, [],
        prims.string(), prims.binary()
    )
    primitives[def_literals.uint8_to_bigint.name] = prims.prim1(
        def_literals.uint8_to_bigint.name, literals.uint8_to_bigint, [],
        prims.uint8(), prims.bigint()
    )
    primitives[def_literals.uint16_to_bigint.name] = prims.prim1(
        def_literals.uint16_to_bigint.name, literals.uint16_to_bigint, [],
        prims.uint16(), prims.bigint()
    )
    primitives[def_literals.uint32_to_bigint.name] = prims.prim1(
        def_literals.uint32_to_bigint.name, literals.uint32_to_bigint, [],
        prims.uint32(), prims.bigint()
    )
    primitives[def_literals.uint64_to_bigint.name] = prims.prim1(
        def_literals.uint64_to_bigint.name, literals.uint64_to_bigint, [],
        prims.uint64(), prims.bigint()
    )

    return primitives


def register_pairs_primitives() -> dict[Name, Primitive]:
    """Register all pairs primitive functions."""
    from hydra.overlay.python.lib import pairs
    from hydra.lib import pairs as def_pairs

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
    primitives[def_pairs.bimap.name] = prims.prim3(
        def_pairs.bimap.name, pairs.bimap, [_a, _b, _c, _d],
        fun(a, c), fun(b, d), prims.pair(a, b), prims.pair(c, d)
    )
    primitives[def_pairs.first.name] = prims.prim1(
        def_pairs.first.name, pairs.first, [_a, _b],
        prims.pair(a, b), a
    )
    primitives[def_pairs.second.name] = prims.prim1(
        def_pairs.second.name, pairs.second, [_a, _b],
        prims.pair(a, b), b
    )

    return primitives


def standard_library() -> dict[Name, Primitive]:
    """Get all standard library primitives."""
    primitives: dict[Name, Primitive] = {}
    primitives.update(register_chars_primitives())
    primitives.update(register_effects_primitives())
    primitives.update(register_eithers_primitives())
    primitives.update(register_equality_primitives())
    primitives.update(register_files_primitives())
    primitives.update(register_hashing_primitives())
    primitives.update(register_lists_primitives())
    primitives.update(register_literals_primitives())
    primitives.update(register_logic_primitives())
    primitives.update(register_maps_primitives())
    primitives.update(register_math_primitives())
    primitives.update(register_optionals_primitives())
    primitives.update(register_pairs_primitives())
    primitives.update(register_regex_primitives())
    primitives.update(register_sets_primitives())
    primitives.update(register_strings_primitives())
    primitives.update(register_system_primitives())
    primitives.update(register_text_primitives())
    return primitives
