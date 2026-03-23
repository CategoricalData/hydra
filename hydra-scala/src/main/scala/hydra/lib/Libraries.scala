package hydra.lib

import hydra.core.*
import hydra.graph.Primitive

/** Registry of all primitive functions available in Hydra-Scala.
  * For bootstrapping (code generation only), implementations are stubs.
  */
object Libraries:
  private val stubImpl: hydra.context.Context => hydra.graph.Graph => Seq[Term] => Either[hydra.context.InContext[hydra.errors.Error], Term] =
    _ => _ => _ => Left(hydra.context.InContext(hydra.errors.Error.other("stub primitive"), hydra.context.Context(Seq.empty, Seq.empty, Map.empty)))

  private def mkPrim(name: String, ts: TypeScheme): Primitive =
    Primitive(name, ts, stubImpl)

  // Type construction helpers
  private def tVar(n: String): Type = Type.variable(n)
  private def tFun(d: Type, c: Type): Type = Type.function(FunctionType(d, c))
  private def tList(t: Type): Type = Type.list(t)
  private def tSet(t: Type): Type = Type.set(t)
  private def tMap(k: Type, v: Type): Type = Type.map(MapType(k, v))
  private def tOpt(t: Type): Type = Type.maybe(t)
  private def tEither(l: Type, r: Type): Type = Type.either(EitherType(l, r))
  private def tPair(a: Type, b: Type): Type = Type.pair(PairType(a, b))
  private val tString: Type = Type.literal(LiteralType.string)
  private val tBool: Type = Type.literal(LiteralType.boolean)
  private val tBinary: Type = Type.literal(LiteralType.binary)
  private val tInt8: Type = Type.literal(LiteralType.integer(IntegerType.int8))
  private val tInt16: Type = Type.literal(LiteralType.integer(IntegerType.int16))
  private val tInt32: Type = Type.literal(LiteralType.integer(IntegerType.int32))
  private val tInt64: Type = Type.literal(LiteralType.integer(IntegerType.int64))
  private val tUint8: Type = Type.literal(LiteralType.integer(IntegerType.uint8))
  private val tUint16: Type = Type.literal(LiteralType.integer(IntegerType.uint16))
  private val tUint32: Type = Type.literal(LiteralType.integer(IntegerType.uint32))
  private val tUint64: Type = Type.literal(LiteralType.integer(IntegerType.uint64))
  private val tBigint: Type = Type.literal(LiteralType.integer(IntegerType.bigint))
  private val tFloat32: Type = Type.literal(LiteralType.float(FloatType.float32))
  private val tFloat64: Type = Type.literal(LiteralType.float(FloatType.float64))
  private val tBigfloat: Type = Type.literal(LiteralType.float(FloatType.bigfloat))
  private val tUnit: Type = Type.unit
  private val tComparison: Type = Type.wrap(Type.variable("hydra.util.Comparison"))

  private def tScheme(vars: Seq[String], t: Type): TypeScheme = TypeScheme(vars, t, None)
  private def tMono(t: Type): TypeScheme = tScheme(Seq.empty, t)

  private def tSchemeConstrained(vars: Seq[(String, Seq[String])], t: Type): TypeScheme =
    val varNames = vars.map(_._1)
    val constraints = vars.collect { case (name, classes) if classes.nonEmpty =>
      name -> TypeVariableMetadata(classes.toSet)
    }.toMap
    TypeScheme(varNames, t, if constraints.isEmpty then None else Some(constraints))

  // ===== Chars primitives =====

  private def charsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.chars"
    Map(
      s"$ns.isAlphaNum" -> mkPrim(s"$ns.isAlphaNum", tMono(tFun(tInt32, tBool))),
      s"$ns.isLower" -> mkPrim(s"$ns.isLower", tMono(tFun(tInt32, tBool))),
      s"$ns.isSpace" -> mkPrim(s"$ns.isSpace", tMono(tFun(tInt32, tBool))),
      s"$ns.isUpper" -> mkPrim(s"$ns.isUpper", tMono(tFun(tInt32, tBool))),
      s"$ns.toLower" -> mkPrim(s"$ns.toLower", tMono(tFun(tInt32, tInt32))),
      s"$ns.toUpper" -> mkPrim(s"$ns.toUpper", tMono(tFun(tInt32, tInt32))),
    )

  // ===== Equality primitives =====

  private def equalityPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.equality"
    val x = tVar("x")
    val xOrd = Seq(("x", Seq("ordering")))
    val xEq = Seq(("x", Seq("equality")))
    val xPlain = Seq(("x", Seq.empty))
    Map(
      s"$ns.compare" -> mkPrim(s"$ns.compare", tSchemeConstrained(xOrd, tFun(x, tFun(x, tComparison)))),
      s"$ns.equal" -> mkPrim(s"$ns.equal", tSchemeConstrained(xEq, tFun(x, tFun(x, tBool)))),
      s"$ns.identity" -> mkPrim(s"$ns.identity", tSchemeConstrained(xPlain, tFun(x, x))),
      s"$ns.gt" -> mkPrim(s"$ns.gt", tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool)))),
      s"$ns.gte" -> mkPrim(s"$ns.gte", tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool)))),
      s"$ns.lt" -> mkPrim(s"$ns.lt", tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool)))),
      s"$ns.lte" -> mkPrim(s"$ns.lte", tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool)))),
      s"$ns.max" -> mkPrim(s"$ns.max", tSchemeConstrained(xOrd, tFun(x, tFun(x, x)))),
      s"$ns.min" -> mkPrim(s"$ns.min", tSchemeConstrained(xOrd, tFun(x, tFun(x, x)))),
    )

  // ===== Eithers primitives =====

  private def eithersPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.eithers"
    val x = tVar("x")
    val y = tVar("y")
    val z = tVar("z")
    val w = tVar("w")
    Map(
      // bind :: Either x y -> (y -> Either x z) -> Either x z
      s"$ns.bind" -> mkPrim(s"$ns.bind", tScheme(Seq("x", "y", "z"),
        tFun(tEither(x, y), tFun(tFun(y, tEither(x, z)), tEither(x, z))))),
      // bimap :: (x -> z) -> (y -> w) -> Either x y -> Either z w
      s"$ns.bimap" -> mkPrim(s"$ns.bimap", tScheme(Seq("x", "y", "z", "w"),
        tFun(tFun(x, z), tFun(tFun(y, w), tFun(tEither(x, y), tEither(z, w)))))),
      // either :: (x -> z) -> (y -> z) -> Either x y -> z
      s"$ns.either" -> mkPrim(s"$ns.either", tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, z), tFun(tFun(y, z), tFun(tEither(x, y), z))))),
      // foldl :: (x -> y -> Either z x) -> x -> [y] -> Either z x
      s"$ns.foldl" -> mkPrim(s"$ns.foldl", tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tFun(y, tEither(z, x))), tFun(x, tFun(tList(y), tEither(z, x)))))),
      // fromLeft :: x -> Either x y -> x
      s"$ns.fromLeft" -> mkPrim(s"$ns.fromLeft", tScheme(Seq("x", "y"),
        tFun(x, tFun(tEither(x, y), x)))),
      // fromRight :: y -> Either x y -> y
      s"$ns.fromRight" -> mkPrim(s"$ns.fromRight", tScheme(Seq("x", "y"),
        tFun(y, tFun(tEither(x, y), y)))),
      // isLeft :: Either x y -> Bool
      s"$ns.isLeft" -> mkPrim(s"$ns.isLeft", tScheme(Seq("x", "y"),
        tFun(tEither(x, y), tBool))),
      // isRight :: Either x y -> Bool
      s"$ns.isRight" -> mkPrim(s"$ns.isRight", tScheme(Seq("x", "y"),
        tFun(tEither(x, y), tBool))),
      // lefts :: [Either x y] -> [x]
      s"$ns.lefts" -> mkPrim(s"$ns.lefts", tScheme(Seq("x", "y"),
        tFun(tList(tEither(x, y)), tList(x)))),
      // map :: (x -> y) -> Either z x -> Either z y
      s"$ns.map" -> mkPrim(s"$ns.map", tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, y), tFun(tEither(z, x), tEither(z, y))))),
      // mapList :: (x -> Either z y) -> [x] -> Either z [y]
      s"$ns.mapList" -> mkPrim(s"$ns.mapList", tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tEither(z, y)), tFun(tList(x), tEither(z, tList(y)))))),
      // mapMaybe :: (x -> Either z y) -> Maybe x -> Either z (Maybe y)
      s"$ns.mapMaybe" -> mkPrim(s"$ns.mapMaybe", tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tEither(z, y)), tFun(tOpt(x), tEither(z, tOpt(y)))))),
      // mapSet :: (x -> Either z y) -> Set x -> Either z (Set y)
      s"$ns.mapSet" -> mkPrim(s"$ns.mapSet", tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tEither(z, y)), tFun(tSet(x), tEither(z, tSet(y)))))),
      // partitionEithers :: [Either x y] -> ([x], [y])
      s"$ns.partitionEithers" -> mkPrim(s"$ns.partitionEithers", tScheme(Seq("x", "y"),
        tFun(tList(tEither(x, y)), tPair(tList(x), tList(y))))),
      // rights :: [Either x y] -> [y]
      s"$ns.rights" -> mkPrim(s"$ns.rights", tScheme(Seq("x", "y"),
        tFun(tList(tEither(x, y)), tList(y)))),
    )

  // ===== Lists primitives =====

  private def listsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.lists"
    val a = tVar("a")
    val b = tVar("b")
    val c = tVar("c")
    val aEq = Seq(("a", Seq("equality")))
    val aOrd = Seq(("a", Seq("ordering")))
    val bOrd = Seq(("b", Seq("ordering")))
    Map(
      // apply :: [a -> b] -> [a] -> [b]
      s"$ns.apply" -> mkPrim(s"$ns.apply", tScheme(Seq("a", "b"),
        tFun(tList(tFun(a, b)), tFun(tList(a), tList(b))))),
      // at :: Int32 -> [a] -> a
      s"$ns.at" -> mkPrim(s"$ns.at", tScheme(Seq("a"),
        tFun(tInt32, tFun(tList(a), a)))),
      // bind :: [a] -> (a -> [b]) -> [b]
      s"$ns.bind" -> mkPrim(s"$ns.bind", tScheme(Seq("a", "b"),
        tFun(tList(a), tFun(tFun(a, tList(b)), tList(b))))),
      // concat :: [[a]] -> [a]
      s"$ns.concat" -> mkPrim(s"$ns.concat", tScheme(Seq("a"),
        tFun(tList(tList(a)), tList(a)))),
      // concat2 :: [a] -> [a] -> [a]
      s"$ns.concat2" -> mkPrim(s"$ns.concat2", tScheme(Seq("a"),
        tFun(tList(a), tFun(tList(a), tList(a))))),
      // cons :: a -> [a] -> [a]
      s"$ns.cons" -> mkPrim(s"$ns.cons", tScheme(Seq("a"),
        tFun(a, tFun(tList(a), tList(a))))),
      // drop :: Int32 -> [a] -> [a]
      s"$ns.drop" -> mkPrim(s"$ns.drop", tScheme(Seq("a"),
        tFun(tInt32, tFun(tList(a), tList(a))))),
      // dropWhile :: (a -> Bool) -> [a] -> [a]
      s"$ns.dropWhile" -> mkPrim(s"$ns.dropWhile", tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tList(a))))),
      // elem :: Eq a => a -> [a] -> Bool
      s"$ns.elem" -> mkPrim(s"$ns.elem", tSchemeConstrained(aEq,
        tFun(a, tFun(tList(a), tBool)))),
      // filter :: (a -> Bool) -> [a] -> [a]
      s"$ns.filter" -> mkPrim(s"$ns.filter", tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tList(a))))),
      // find :: (a -> Bool) -> [a] -> Maybe a
      s"$ns.find" -> mkPrim(s"$ns.find", tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tOpt(a))))),
      // foldl :: (b -> a -> b) -> b -> [a] -> b
      s"$ns.foldl" -> mkPrim(s"$ns.foldl", tScheme(Seq("b", "a"),
        tFun(tFun(b, tFun(a, b)), tFun(b, tFun(tList(a), b))))),
      // foldr :: (a -> b -> b) -> b -> [a] -> b
      s"$ns.foldr" -> mkPrim(s"$ns.foldr", tScheme(Seq("a", "b"),
        tFun(tFun(a, tFun(b, b)), tFun(b, tFun(tList(a), b))))),
      // group :: Eq a => [a] -> [[a]]
      s"$ns.group" -> mkPrim(s"$ns.group", tSchemeConstrained(aEq,
        tFun(tList(a), tList(tList(a))))),
      // head :: [a] -> a
      s"$ns.head" -> mkPrim(s"$ns.head", tScheme(Seq("a"),
        tFun(tList(a), a))),
      // init :: [a] -> [a]
      s"$ns.init" -> mkPrim(s"$ns.init", tScheme(Seq("a"),
        tFun(tList(a), tList(a)))),
      // intercalate :: [a] -> [[a]] -> [a]
      s"$ns.intercalate" -> mkPrim(s"$ns.intercalate", tScheme(Seq("a"),
        tFun(tList(a), tFun(tList(tList(a)), tList(a))))),
      // intersperse :: a -> [a] -> [a]
      s"$ns.intersperse" -> mkPrim(s"$ns.intersperse", tScheme(Seq("a"),
        tFun(a, tFun(tList(a), tList(a))))),
      // last :: [a] -> a
      s"$ns.last" -> mkPrim(s"$ns.last", tScheme(Seq("a"),
        tFun(tList(a), a))),
      // length :: [a] -> Int32
      s"$ns.length" -> mkPrim(s"$ns.length", tScheme(Seq("a"),
        tFun(tList(a), tInt32))),
      // map :: (a -> b) -> [a] -> [b]
      s"$ns.map" -> mkPrim(s"$ns.map", tScheme(Seq("a", "b"),
        tFun(tFun(a, b), tFun(tList(a), tList(b))))),
      // nub :: Eq a => [a] -> [a]
      s"$ns.nub" -> mkPrim(s"$ns.nub", tSchemeConstrained(aEq,
        tFun(tList(a), tList(a)))),
      // null :: [a] -> Bool
      s"$ns.null" -> mkPrim(s"$ns.null", tScheme(Seq("a"),
        tFun(tList(a), tBool))),
      // partition :: (a -> Bool) -> [a] -> ([a], [a])
      s"$ns.partition" -> mkPrim(s"$ns.partition", tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tPair(tList(a), tList(a)))))),
      // pure :: a -> [a]
      s"$ns.pure" -> mkPrim(s"$ns.pure", tScheme(Seq("a"),
        tFun(a, tList(a)))),
      // replicate :: Int32 -> a -> [a]
      s"$ns.replicate" -> mkPrim(s"$ns.replicate", tScheme(Seq("a"),
        tFun(tInt32, tFun(a, tList(a))))),
      // reverse :: [a] -> [a]
      s"$ns.reverse" -> mkPrim(s"$ns.reverse", tScheme(Seq("a"),
        tFun(tList(a), tList(a)))),
      // safeHead :: [a] -> Maybe a
      s"$ns.safeHead" -> mkPrim(s"$ns.safeHead", tScheme(Seq("a"),
        tFun(tList(a), tOpt(a)))),
      // singleton :: a -> [a]
      s"$ns.singleton" -> mkPrim(s"$ns.singleton", tScheme(Seq("a"),
        tFun(a, tList(a)))),
      // sort :: Ord a => [a] -> [a]
      s"$ns.sort" -> mkPrim(s"$ns.sort", tSchemeConstrained(aOrd,
        tFun(tList(a), tList(a)))),
      // sortOn :: Ord b => (a -> b) -> [a] -> [a]
      s"$ns.sortOn" -> mkPrim(s"$ns.sortOn", tSchemeConstrained(Seq(("a", Seq.empty), ("b", Seq("ordering"))),
        tFun(tFun(a, b), tFun(tList(a), tList(a))))),
      // span :: (a -> Bool) -> [a] -> ([a], [a])
      s"$ns.span" -> mkPrim(s"$ns.span", tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tPair(tList(a), tList(a)))))),
      // tail :: [a] -> [a]
      s"$ns.tail" -> mkPrim(s"$ns.tail", tScheme(Seq("a"),
        tFun(tList(a), tList(a)))),
      // take :: Int32 -> [a] -> [a]
      s"$ns.take" -> mkPrim(s"$ns.take", tScheme(Seq("a"),
        tFun(tInt32, tFun(tList(a), tList(a))))),
      // transpose :: [[a]] -> [[a]]
      s"$ns.transpose" -> mkPrim(s"$ns.transpose", tScheme(Seq("a"),
        tFun(tList(tList(a)), tList(tList(a))))),
      // zip :: [a] -> [b] -> [(a, b)]
      s"$ns.zip" -> mkPrim(s"$ns.zip", tScheme(Seq("a", "b"),
        tFun(tList(a), tFun(tList(b), tList(tPair(a, b)))))),
      // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
      s"$ns.zipWith" -> mkPrim(s"$ns.zipWith", tScheme(Seq("a", "b", "c"),
        tFun(tFun(a, tFun(b, c)), tFun(tList(a), tFun(tList(b), tList(c)))))),
    )

  // ===== Logic primitives =====

  private def logicPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.logic"
    val a = tVar("a")
    Map(
      s"$ns.and" -> mkPrim(s"$ns.and", tMono(tFun(tBool, tFun(tBool, tBool)))),
      s"$ns.ifElse" -> mkPrim(s"$ns.ifElse", tScheme(Seq("a"),
        tFun(tBool, tFun(a, tFun(a, a))))),
      s"$ns.not" -> mkPrim(s"$ns.not", tMono(tFun(tBool, tBool))),
      s"$ns.or" -> mkPrim(s"$ns.or", tMono(tFun(tBool, tFun(tBool, tBool)))),
    )

  // ===== Maps primitives =====

  private def mapsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.maps"
    val k = tVar("k")
    val k1 = tVar("k1")
    val k2 = tVar("k2")
    val v = tVar("v")
    val v1 = tVar("v1")
    val v2 = tVar("v2")
    val mapKV = tMap(k, v)
    Map(
      // alter :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
      s"$ns.alter" -> mkPrim(s"$ns.alter", tSchemeConstrained(Seq(("v", Seq.empty), ("k", Seq("ordering"))),
        tFun(tFun(tOpt(v), tOpt(v)), tFun(k, tFun(mapKV, mapKV))))),
      // bimap :: (Ord k1, Ord k2) => (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
      s"$ns.bimap" -> mkPrim(s"$ns.bimap", tSchemeConstrained(Seq(("k1", Seq("ordering")), ("k2", Seq("ordering")), ("v1", Seq.empty), ("v2", Seq.empty)),
        tFun(tFun(k1, k2), tFun(tFun(v1, v2), tFun(tMap(k1, v1), tMap(k2, v2)))))),
      // delete :: Ord k => k -> Map k v -> Map k v
      s"$ns.delete" -> mkPrim(s"$ns.delete", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(mapKV, mapKV)))),
      // elems :: Ord k => Map k v -> [v]
      s"$ns.elems" -> mkPrim(s"$ns.elems", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tList(v)))),
      // empty :: Ord k => Map k v
      s"$ns.empty" -> mkPrim(s"$ns.empty", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        mapKV)),
      // filter :: Ord k => (v -> Bool) -> Map k v -> Map k v
      s"$ns.filter" -> mkPrim(s"$ns.filter", tSchemeConstrained(Seq(("v", Seq.empty), ("k", Seq("ordering"))),
        tFun(tFun(v, tBool), tFun(mapKV, mapKV)))),
      // filterWithKey :: Ord k => (k -> v -> Bool) -> Map k v -> Map k v
      s"$ns.filterWithKey" -> mkPrim(s"$ns.filterWithKey", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(tFun(k, tFun(v, tBool)), tFun(mapKV, mapKV)))),
      // findWithDefault :: Ord k => v -> k -> Map k v -> v
      s"$ns.findWithDefault" -> mkPrim(s"$ns.findWithDefault", tSchemeConstrained(Seq(("v", Seq.empty), ("k", Seq("ordering"))),
        tFun(v, tFun(k, tFun(mapKV, v))))),
      // fromList :: Ord k => [(k, v)] -> Map k v
      s"$ns.fromList" -> mkPrim(s"$ns.fromList", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(tList(tPair(k, v)), mapKV))),
      // insert :: Ord k => k -> v -> Map k v -> Map k v
      s"$ns.insert" -> mkPrim(s"$ns.insert", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(v, tFun(mapKV, mapKV))))),
      // keys :: Ord k => Map k v -> [k]
      s"$ns.keys" -> mkPrim(s"$ns.keys", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tList(k)))),
      // lookup :: Ord k => k -> Map k v -> Maybe v
      s"$ns.lookup" -> mkPrim(s"$ns.lookup", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(mapKV, tOpt(v))))),
      // map :: Ord k => (v1 -> v2) -> Map k v1 -> Map k v2
      s"$ns.map" -> mkPrim(s"$ns.map", tSchemeConstrained(Seq(("v1", Seq.empty), ("v2", Seq.empty), ("k", Seq("ordering"))),
        tFun(tFun(v1, v2), tFun(tMap(k, v1), tMap(k, v2))))),
      // mapKeys :: (Ord k1, Ord k2) => (k1 -> k2) -> Map k1 v -> Map k2 v
      s"$ns.mapKeys" -> mkPrim(s"$ns.mapKeys", tSchemeConstrained(Seq(("k1", Seq("ordering")), ("k2", Seq("ordering")), ("v", Seq.empty)),
        tFun(tFun(k1, k2), tFun(tMap(k1, v), tMap(k2, v))))),
      // member :: Ord k => k -> Map k v -> Bool
      s"$ns.member" -> mkPrim(s"$ns.member", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(mapKV, tBool)))),
      // null :: Ord k => Map k v -> Bool
      s"$ns.null" -> mkPrim(s"$ns.null", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tBool))),
      // singleton :: Ord k => k -> v -> Map k v
      s"$ns.singleton" -> mkPrim(s"$ns.singleton", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(v, mapKV)))),
      // size :: Ord k => Map k v -> Int32
      s"$ns.size" -> mkPrim(s"$ns.size", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tInt32))),
      // toList :: Ord k => Map k v -> [(k, v)]
      s"$ns.toList" -> mkPrim(s"$ns.toList", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tList(tPair(k, v))))),
      // union :: Ord k => Map k v -> Map k v -> Map k v
      s"$ns.union" -> mkPrim(s"$ns.union", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tFun(mapKV, mapKV)))),
    )

  // ===== Math primitives =====

  private def mathPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.math"
    Map(
      // Int32 primitives
      s"$ns.abs" -> mkPrim(s"$ns.abs", tMono(tFun(tInt32, tInt32))),
      s"$ns.add" -> mkPrim(s"$ns.add", tMono(tFun(tInt32, tFun(tInt32, tInt32)))),
      s"$ns.div" -> mkPrim(s"$ns.div", tMono(tFun(tInt32, tFun(tInt32, tInt32)))),
      s"$ns.even" -> mkPrim(s"$ns.even", tMono(tFun(tInt32, tBool))),
      s"$ns.mod" -> mkPrim(s"$ns.mod", tMono(tFun(tInt32, tFun(tInt32, tInt32)))),
      s"$ns.mul" -> mkPrim(s"$ns.mul", tMono(tFun(tInt32, tFun(tInt32, tInt32)))),
      s"$ns.negate" -> mkPrim(s"$ns.negate", tMono(tFun(tInt32, tInt32))),
      s"$ns.odd" -> mkPrim(s"$ns.odd", tMono(tFun(tInt32, tBool))),
      s"$ns.pred" -> mkPrim(s"$ns.pred", tMono(tFun(tInt32, tInt32))),
      s"$ns.range" -> mkPrim(s"$ns.range", tMono(tFun(tInt32, tFun(tInt32, tList(tInt32))))),
      s"$ns.rem" -> mkPrim(s"$ns.rem", tMono(tFun(tInt32, tFun(tInt32, tInt32)))),
      s"$ns.signum" -> mkPrim(s"$ns.signum", tMono(tFun(tInt32, tInt32))),
      s"$ns.sub" -> mkPrim(s"$ns.sub", tMono(tFun(tInt32, tFun(tInt32, tInt32)))),
      s"$ns.succ" -> mkPrim(s"$ns.succ", tMono(tFun(tInt32, tInt32))),
      s"$ns.max" -> mkPrim(s"$ns.max", tMono(tFun(tInt32, tFun(tInt32, tInt32)))),
      s"$ns.min" -> mkPrim(s"$ns.min", tMono(tFun(tInt32, tFun(tInt32, tInt32)))),
      // Float64 primitives
      s"$ns.acos" -> mkPrim(s"$ns.acos", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.acosh" -> mkPrim(s"$ns.acosh", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.asin" -> mkPrim(s"$ns.asin", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.asinh" -> mkPrim(s"$ns.asinh", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.atan" -> mkPrim(s"$ns.atan", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.atan2" -> mkPrim(s"$ns.atan2", tMono(tFun(tFloat64, tFun(tFloat64, tFloat64)))),
      s"$ns.atanh" -> mkPrim(s"$ns.atanh", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.ceiling" -> mkPrim(s"$ns.ceiling", tMono(tFun(tFloat64, tBigint))),
      s"$ns.cos" -> mkPrim(s"$ns.cos", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.cosh" -> mkPrim(s"$ns.cosh", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.e" -> mkPrim(s"$ns.e", tMono(tFloat64)),
      s"$ns.exp" -> mkPrim(s"$ns.exp", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.floor" -> mkPrim(s"$ns.floor", tMono(tFun(tFloat64, tBigint))),
      s"$ns.log" -> mkPrim(s"$ns.log", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.logBase" -> mkPrim(s"$ns.logBase", tMono(tFun(tFloat64, tFun(tFloat64, tFloat64)))),
      s"$ns.pi" -> mkPrim(s"$ns.pi", tMono(tFloat64)),
      s"$ns.pow" -> mkPrim(s"$ns.pow", tMono(tFun(tFloat64, tFun(tFloat64, tFloat64)))),
      s"$ns.round" -> mkPrim(s"$ns.round", tMono(tFun(tFloat64, tBigint))),
      s"$ns.roundBigfloat" -> mkPrim(s"$ns.roundBigfloat", tMono(tFun(tInt32, tFun(tBigfloat, tBigfloat)))),
      s"$ns.roundFloat32" -> mkPrim(s"$ns.roundFloat32", tMono(tFun(tInt32, tFun(tFloat32, tFloat32)))),
      s"$ns.roundFloat64" -> mkPrim(s"$ns.roundFloat64", tMono(tFun(tInt32, tFun(tFloat64, tFloat64)))),
      s"$ns.sin" -> mkPrim(s"$ns.sin", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.sinh" -> mkPrim(s"$ns.sinh", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.sqrt" -> mkPrim(s"$ns.sqrt", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.tan" -> mkPrim(s"$ns.tan", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.tanh" -> mkPrim(s"$ns.tanh", tMono(tFun(tFloat64, tFloat64))),
      s"$ns.truncate" -> mkPrim(s"$ns.truncate", tMono(tFun(tFloat64, tBigint))),
    )

  // ===== Maybes primitives =====

  private def maybesPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.maybes"
    val a = tVar("a")
    val b = tVar("b")
    val c = tVar("c")
    Map(
      // apply :: Maybe (a -> b) -> Maybe a -> Maybe b
      s"$ns.apply" -> mkPrim(s"$ns.apply", tScheme(Seq("a", "b"),
        tFun(tOpt(tFun(a, b)), tFun(tOpt(a), tOpt(b))))),
      // bind :: Maybe a -> (a -> Maybe b) -> Maybe b
      s"$ns.bind" -> mkPrim(s"$ns.bind", tScheme(Seq("a", "b"),
        tFun(tOpt(a), tFun(tFun(a, tOpt(b)), tOpt(b))))),
      // cases :: Maybe a -> b -> (a -> b) -> b
      s"$ns.cases" -> mkPrim(s"$ns.cases", tScheme(Seq("a", "b"),
        tFun(tOpt(a), tFun(b, tFun(tFun(a, b), b))))),
      // cat :: [Maybe a] -> [a]
      s"$ns.cat" -> mkPrim(s"$ns.cat", tScheme(Seq("a"),
        tFun(tList(tOpt(a)), tList(a)))),
      // compose :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
      s"$ns.compose" -> mkPrim(s"$ns.compose", tScheme(Seq("a", "b", "c"),
        tFun(tFun(a, tOpt(b)), tFun(tFun(b, tOpt(c)), tFun(a, tOpt(c)))))),
      // fromJust :: Maybe a -> a
      s"$ns.fromJust" -> mkPrim(s"$ns.fromJust", tScheme(Seq("a"),
        tFun(tOpt(a), a))),
      // fromMaybe :: a -> Maybe a -> a
      s"$ns.fromMaybe" -> mkPrim(s"$ns.fromMaybe", tScheme(Seq("a"),
        tFun(a, tFun(tOpt(a), a)))),
      // isJust :: Maybe a -> Bool
      s"$ns.isJust" -> mkPrim(s"$ns.isJust", tScheme(Seq("a"),
        tFun(tOpt(a), tBool))),
      // isNothing :: Maybe a -> Bool
      s"$ns.isNothing" -> mkPrim(s"$ns.isNothing", tScheme(Seq("a"),
        tFun(tOpt(a), tBool))),
      // map :: (a -> b) -> Maybe a -> Maybe b
      s"$ns.map" -> mkPrim(s"$ns.map", tScheme(Seq("a", "b"),
        tFun(tFun(a, b), tFun(tOpt(a), tOpt(b))))),
      // mapMaybe :: (a -> Maybe b) -> [a] -> [b]
      s"$ns.mapMaybe" -> mkPrim(s"$ns.mapMaybe", tScheme(Seq("a", "b"),
        tFun(tFun(a, tOpt(b)), tFun(tList(a), tList(b))))),
      // maybe :: b -> (a -> b) -> Maybe a -> b
      s"$ns.maybe" -> mkPrim(s"$ns.maybe", tScheme(Seq("b", "a"),
        tFun(b, tFun(tFun(a, b), tFun(tOpt(a), b))))),
      // pure :: a -> Maybe a
      s"$ns.pure" -> mkPrim(s"$ns.pure", tScheme(Seq("a"),
        tFun(a, tOpt(a)))),
      // toList :: Maybe a -> [a]
      s"$ns.toList" -> mkPrim(s"$ns.toList", tScheme(Seq("a"),
        tFun(tOpt(a), tList(a)))),
    )

  // ===== Sets primitives =====

  private def setsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.sets"
    val a = tVar("a")
    val b = tVar("b")
    val aOrd = Seq(("a", Seq("ordering")))
    Map(
      // delete :: Ord a => a -> Set a -> Set a
      s"$ns.delete" -> mkPrim(s"$ns.delete", tSchemeConstrained(aOrd,
        tFun(a, tFun(tSet(a), tSet(a))))),
      // difference :: Ord a => Set a -> Set a -> Set a
      s"$ns.difference" -> mkPrim(s"$ns.difference", tSchemeConstrained(aOrd,
        tFun(tSet(a), tFun(tSet(a), tSet(a))))),
      // empty :: Ord a => Set a
      s"$ns.empty" -> mkPrim(s"$ns.empty", tSchemeConstrained(aOrd,
        tSet(a))),
      // fromList :: Ord a => [a] -> Set a
      s"$ns.fromList" -> mkPrim(s"$ns.fromList", tSchemeConstrained(aOrd,
        tFun(tList(a), tSet(a)))),
      // insert :: Ord a => a -> Set a -> Set a
      s"$ns.insert" -> mkPrim(s"$ns.insert", tSchemeConstrained(aOrd,
        tFun(a, tFun(tSet(a), tSet(a))))),
      // intersection :: Ord a => Set a -> Set a -> Set a
      s"$ns.intersection" -> mkPrim(s"$ns.intersection", tSchemeConstrained(aOrd,
        tFun(tSet(a), tFun(tSet(a), tSet(a))))),
      // map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
      s"$ns.map" -> mkPrim(s"$ns.map", tSchemeConstrained(Seq(("a", Seq("ordering")), ("b", Seq("ordering"))),
        tFun(tFun(a, b), tFun(tSet(a), tSet(b))))),
      // member :: Ord a => a -> Set a -> Bool
      s"$ns.member" -> mkPrim(s"$ns.member", tSchemeConstrained(aOrd,
        tFun(a, tFun(tSet(a), tBool)))),
      // null :: Ord a => Set a -> Bool
      s"$ns.null" -> mkPrim(s"$ns.null", tSchemeConstrained(aOrd,
        tFun(tSet(a), tBool))),
      // singleton :: Ord a => a -> Set a
      s"$ns.singleton" -> mkPrim(s"$ns.singleton", tSchemeConstrained(aOrd,
        tFun(a, tSet(a)))),
      // size :: Ord a => Set a -> Int32
      s"$ns.size" -> mkPrim(s"$ns.size", tSchemeConstrained(aOrd,
        tFun(tSet(a), tInt32))),
      // toList :: Ord a => Set a -> [a]
      s"$ns.toList" -> mkPrim(s"$ns.toList", tSchemeConstrained(aOrd,
        tFun(tSet(a), tList(a)))),
      // union :: Ord a => Set a -> Set a -> Set a
      s"$ns.union" -> mkPrim(s"$ns.union", tSchemeConstrained(aOrd,
        tFun(tSet(a), tFun(tSet(a), tSet(a))))),
      // unions :: Ord a => [Set a] -> Set a
      s"$ns.unions" -> mkPrim(s"$ns.unions", tSchemeConstrained(aOrd,
        tFun(tList(tSet(a)), tSet(a)))),
    )

  // ===== Regex primitives =====

  private def regexPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.regex"
    Map(
      s"$ns.find" -> mkPrim(s"$ns.find", tMono(tFun(tString, tFun(tString, tOpt(tString))))),
      s"$ns.findAll" -> mkPrim(s"$ns.findAll", tMono(tFun(tString, tFun(tString, tList(tString))))),
      s"$ns.matches" -> mkPrim(s"$ns.matches", tMono(tFun(tString, tFun(tString, tBool)))),
      s"$ns.replace" -> mkPrim(s"$ns.replace", tMono(tFun(tString, tFun(tString, tFun(tString, tString))))),
      s"$ns.replaceAll" -> mkPrim(s"$ns.replaceAll", tMono(tFun(tString, tFun(tString, tFun(tString, tString))))),
      s"$ns.split" -> mkPrim(s"$ns.split", tMono(tFun(tString, tFun(tString, tList(tString))))),
    )

  // ===== Strings primitives =====

  private def stringsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.strings"
    Map(
      s"$ns.cat" -> mkPrim(s"$ns.cat", tMono(tFun(tList(tString), tString))),
      s"$ns.cat2" -> mkPrim(s"$ns.cat2", tMono(tFun(tString, tFun(tString, tString)))),
      s"$ns.charAt" -> mkPrim(s"$ns.charAt", tMono(tFun(tInt32, tFun(tString, tInt32)))),
      s"$ns.fromList" -> mkPrim(s"$ns.fromList", tMono(tFun(tList(tInt32), tString))),
      s"$ns.intercalate" -> mkPrim(s"$ns.intercalate", tMono(tFun(tString, tFun(tList(tString), tString)))),
      s"$ns.length" -> mkPrim(s"$ns.length", tMono(tFun(tString, tInt32))),
      s"$ns.lines" -> mkPrim(s"$ns.lines", tMono(tFun(tString, tList(tString)))),
      s"$ns.null" -> mkPrim(s"$ns.null", tMono(tFun(tString, tBool))),
      s"$ns.splitOn" -> mkPrim(s"$ns.splitOn", tMono(tFun(tString, tFun(tString, tList(tString))))),
      s"$ns.toList" -> mkPrim(s"$ns.toList", tMono(tFun(tString, tList(tInt32)))),
      s"$ns.toLower" -> mkPrim(s"$ns.toLower", tMono(tFun(tString, tString))),
      s"$ns.toUpper" -> mkPrim(s"$ns.toUpper", tMono(tFun(tString, tString))),
      s"$ns.unlines" -> mkPrim(s"$ns.unlines", tMono(tFun(tList(tString), tString))),
    )

  // ===== Literals primitives =====

  private def literalsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.literals"
    Map(
      // Conversion primitives
      s"$ns.bigfloatToBigint" -> mkPrim(s"$ns.bigfloatToBigint", tMono(tFun(tBigfloat, tBigint))),
      s"$ns.bigfloatToFloat32" -> mkPrim(s"$ns.bigfloatToFloat32", tMono(tFun(tBigfloat, tFloat32))),
      s"$ns.bigfloatToFloat64" -> mkPrim(s"$ns.bigfloatToFloat64", tMono(tFun(tBigfloat, tFloat64))),
      s"$ns.bigintToBigfloat" -> mkPrim(s"$ns.bigintToBigfloat", tMono(tFun(tBigint, tBigfloat))),
      s"$ns.bigintToInt8" -> mkPrim(s"$ns.bigintToInt8", tMono(tFun(tBigint, tInt8))),
      s"$ns.bigintToInt16" -> mkPrim(s"$ns.bigintToInt16", tMono(tFun(tBigint, tInt16))),
      s"$ns.bigintToInt32" -> mkPrim(s"$ns.bigintToInt32", tMono(tFun(tBigint, tInt32))),
      s"$ns.bigintToInt64" -> mkPrim(s"$ns.bigintToInt64", tMono(tFun(tBigint, tInt64))),
      s"$ns.bigintToUint8" -> mkPrim(s"$ns.bigintToUint8", tMono(tFun(tBigint, tUint8))),
      s"$ns.bigintToUint16" -> mkPrim(s"$ns.bigintToUint16", tMono(tFun(tBigint, tUint16))),
      s"$ns.bigintToUint32" -> mkPrim(s"$ns.bigintToUint32", tMono(tFun(tBigint, tUint32))),
      s"$ns.bigintToUint64" -> mkPrim(s"$ns.bigintToUint64", tMono(tFun(tBigint, tUint64))),
      s"$ns.binaryToBytes" -> mkPrim(s"$ns.binaryToBytes", tMono(tFun(tBinary, tList(tInt32)))),
      s"$ns.binaryToString" -> mkPrim(s"$ns.binaryToString", tMono(tFun(tBinary, tString))),
      s"$ns.float32ToBigfloat" -> mkPrim(s"$ns.float32ToBigfloat", tMono(tFun(tFloat32, tBigfloat))),
      s"$ns.float64ToBigfloat" -> mkPrim(s"$ns.float64ToBigfloat", tMono(tFun(tFloat64, tBigfloat))),
      s"$ns.int8ToBigint" -> mkPrim(s"$ns.int8ToBigint", tMono(tFun(tInt8, tBigint))),
      s"$ns.int16ToBigint" -> mkPrim(s"$ns.int16ToBigint", tMono(tFun(tInt16, tBigint))),
      s"$ns.int32ToBigint" -> mkPrim(s"$ns.int32ToBigint", tMono(tFun(tInt32, tBigint))),
      s"$ns.int64ToBigint" -> mkPrim(s"$ns.int64ToBigint", tMono(tFun(tInt64, tBigint))),
      // Read primitives
      s"$ns.readBigfloat" -> mkPrim(s"$ns.readBigfloat", tMono(tFun(tString, tOpt(tBigfloat)))),
      s"$ns.readBigint" -> mkPrim(s"$ns.readBigint", tMono(tFun(tString, tOpt(tBigint)))),
      s"$ns.readBoolean" -> mkPrim(s"$ns.readBoolean", tMono(tFun(tString, tOpt(tBool)))),
      s"$ns.readFloat32" -> mkPrim(s"$ns.readFloat32", tMono(tFun(tString, tOpt(tFloat32)))),
      s"$ns.readFloat64" -> mkPrim(s"$ns.readFloat64", tMono(tFun(tString, tOpt(tFloat64)))),
      s"$ns.readInt8" -> mkPrim(s"$ns.readInt8", tMono(tFun(tString, tOpt(tInt8)))),
      s"$ns.readInt16" -> mkPrim(s"$ns.readInt16", tMono(tFun(tString, tOpt(tInt16)))),
      s"$ns.readInt32" -> mkPrim(s"$ns.readInt32", tMono(tFun(tString, tOpt(tInt32)))),
      s"$ns.readInt64" -> mkPrim(s"$ns.readInt64", tMono(tFun(tString, tOpt(tInt64)))),
      s"$ns.readString" -> mkPrim(s"$ns.readString", tMono(tFun(tString, tOpt(tString)))),
      s"$ns.readUint8" -> mkPrim(s"$ns.readUint8", tMono(tFun(tString, tOpt(tUint8)))),
      s"$ns.readUint16" -> mkPrim(s"$ns.readUint16", tMono(tFun(tString, tOpt(tUint16)))),
      s"$ns.readUint32" -> mkPrim(s"$ns.readUint32", tMono(tFun(tString, tOpt(tUint32)))),
      s"$ns.readUint64" -> mkPrim(s"$ns.readUint64", tMono(tFun(tString, tOpt(tUint64)))),
      // Show primitives
      s"$ns.showBigfloat" -> mkPrim(s"$ns.showBigfloat", tMono(tFun(tBigfloat, tString))),
      s"$ns.showBigint" -> mkPrim(s"$ns.showBigint", tMono(tFun(tBigint, tString))),
      s"$ns.showBoolean" -> mkPrim(s"$ns.showBoolean", tMono(tFun(tBool, tString))),
      s"$ns.showFloat32" -> mkPrim(s"$ns.showFloat32", tMono(tFun(tFloat32, tString))),
      s"$ns.showFloat64" -> mkPrim(s"$ns.showFloat64", tMono(tFun(tFloat64, tString))),
      s"$ns.showInt8" -> mkPrim(s"$ns.showInt8", tMono(tFun(tInt8, tString))),
      s"$ns.showInt16" -> mkPrim(s"$ns.showInt16", tMono(tFun(tInt16, tString))),
      s"$ns.showInt32" -> mkPrim(s"$ns.showInt32", tMono(tFun(tInt32, tString))),
      s"$ns.showInt64" -> mkPrim(s"$ns.showInt64", tMono(tFun(tInt64, tString))),
      s"$ns.showUint8" -> mkPrim(s"$ns.showUint8", tMono(tFun(tUint8, tString))),
      s"$ns.showUint16" -> mkPrim(s"$ns.showUint16", tMono(tFun(tUint16, tString))),
      s"$ns.showUint32" -> mkPrim(s"$ns.showUint32", tMono(tFun(tUint32, tString))),
      s"$ns.showUint64" -> mkPrim(s"$ns.showUint64", tMono(tFun(tUint64, tString))),
      s"$ns.showString" -> mkPrim(s"$ns.showString", tMono(tFun(tString, tString))),
      s"$ns.stringToBinary" -> mkPrim(s"$ns.stringToBinary", tMono(tFun(tString, tBinary))),
      s"$ns.uint8ToBigint" -> mkPrim(s"$ns.uint8ToBigint", tMono(tFun(tUint8, tBigint))),
      s"$ns.uint16ToBigint" -> mkPrim(s"$ns.uint16ToBigint", tMono(tFun(tUint16, tBigint))),
      s"$ns.uint32ToBigint" -> mkPrim(s"$ns.uint32ToBigint", tMono(tFun(tUint32, tBigint))),
      s"$ns.uint64ToBigint" -> mkPrim(s"$ns.uint64ToBigint", tMono(tFun(tUint64, tBigint))),
    )

  // ===== Pairs primitives =====

  private def pairsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.pairs"
    val a = tVar("a")
    val b = tVar("b")
    val c = tVar("c")
    val d = tVar("d")
    Map(
      // bimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
      s"$ns.bimap" -> mkPrim(s"$ns.bimap", tScheme(Seq("a", "b", "c", "d"),
        tFun(tFun(a, c), tFun(tFun(b, d), tFun(tPair(a, b), tPair(c, d)))))),
      // first :: (a, b) -> a
      s"$ns.first" -> mkPrim(s"$ns.first", tScheme(Seq("a", "b"),
        tFun(tPair(a, b), a))),
      // second :: (a, b) -> b
      s"$ns.second" -> mkPrim(s"$ns.second", tScheme(Seq("a", "b"),
        tFun(tPair(a, b), b))),
    )

  /** All standard primitives. */
  def standardPrimitives(): Map[String, Primitive] =
    charsPrimitives() ++
    equalityPrimitives() ++
    eithersPrimitives() ++
    listsPrimitives() ++
    literalsPrimitives() ++
    logicPrimitives() ++
    mapsPrimitives() ++
    mathPrimitives() ++
    maybesPrimitives() ++
    pairsPrimitives() ++
    regexPrimitives() ++
    setsPrimitives() ++
    stringsPrimitives()
