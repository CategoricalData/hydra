package hydra.overlay.scala.dsl

import hydra.core.*

/**
 * A domain-specific language for constructing Hydra types in Scala.
 * Mirrors Hydra.Dsl.Types (Haskell), hydra.dsl.types (Python),
 * and hydra.overlay.java.dsl.Types (Java).
 */
object Types:

  val PLACEHOLDER_NAME: Name = "_Placeholder"

  // ===== Annotations =====

  def annot(ann: Term, base: Type): Type =
    Type.annotated(AnnotatedType(base, ann))

  def annot(ann: Map[Name, Term], base: Type): Type =
    annot(Terms.annotationMapAsTerm(ann), base)

  def annot(key: Name, value: Term, base: Type): Type =
    annot(Map(key -> value), base)

  // ===== Type application =====

  def apply(lhs: Type, rhs: Type): Type =
    Type.application(ApplicationType(lhs, rhs))

  def apply(lhs: Type, rhs: Type*): Type =
    rhs.foldLeft(lhs)((acc, r) => apply(acc, r))

  def applys(t: Type, ts: Seq[Type]): Type =
    ts.foldLeft(t)((acc, r) => apply(acc, r))

  /** Apply a type to multiple arguments where ts.head is the function and ts.tail are arguments. */
  def applyMany(ts: Seq[Type]): Type =
    if ts.isEmpty then throw new IllegalArgumentException("applyMany requires at least one type")
    else ts.tail.foldLeft(ts.head)((acc, r) => apply(acc, r))

  // ===== Literal types =====

  def literal(ltype: LiteralType): Type = Type.literal(ltype)

  def binary: Type = literal(LiteralType.binary)
  def boolean_ : Type = literal(LiteralType.boolean)
  def decimal: Type = literal(LiteralType.decimal)
  def string: Type = literal(LiteralType.string)

  // ===== Integer types =====

  def integer(itype: IntegerType): Type = literal(LiteralType.integer(itype))
  def int8: Type = integer(IntegerType.int8)
  def int16: Type = integer(IntegerType.int16)
  def int32: Type = integer(IntegerType.int32)
  def int64: Type = integer(IntegerType.int64)
  def bigint: Type = integer(IntegerType.bigint)
  def uint8: Type = integer(IntegerType.uint8)
  def uint16: Type = integer(IntegerType.uint16)
  def uint32: Type = integer(IntegerType.uint32)
  def uint64: Type = integer(IntegerType.uint64)

  // ===== Floating-point types =====

  def float_(ftype: FloatType): Type = literal(LiteralType.float(ftype))
  def float32: Type = float_(FloatType.float32)
  def float64: Type = float_(FloatType.float64)

  /** Non-negative int32 — alias for int32, intended for semantic annotation. */
  def nonNegativeInt32: Type = int32

  // ===== Function types =====

  def function(dom: Type, cod: Type): Type =
    Type.function(FunctionType(dom, cod))

  def function(dom: String, cod: Type): Type = function(`var`(dom), cod)
  def function(dom: Type, cod: String): Type = function(dom, `var`(cod))
  def function(dom: String, cod: String): Type = function(`var`(dom), `var`(cod))

  /** N-ary curried function: function(a, b, c, d) = a -> b -> c -> d. */
  def function(dom: Type, cod: Type, more: Type*): Type =
    if more.isEmpty then function(dom, cod)
    else function(dom, function(cod, more.head, more.tail*))

  def functionMany(ts: Seq[Type]): Type =
    if ts.size < 2 then throw new IllegalArgumentException("functionMany requires at least 2 types")
    else
      val reversed = ts.reverse
      reversed.tail.foldLeft(reversed.head)((acc, t) => function(t, acc))

  // ===== Universal quantification (forall) =====

  def forall(v: String, body: Type): Type =
    Type.forall(ForallType(Terms.name(v), body))

  def forall(v1: String, v2: String, body: Type): Type =
    forall(v1, forall(v2, body))

  def forall(v1: String, v2: String, v3: String, body: Type): Type =
    forall(v1, forall(v2, forall(v3, body)))

  def foralls(vs: Seq[String], body: Type): Type =
    vs.foldRight(body)((v, acc) => forall(v, acc))

  // ===== Type schemes =====

  def mono(body: Type): TypeScheme =
    TypeScheme(Seq.empty, body, None)

  def poly(vs: Seq[String], body: Type): TypeScheme =
    TypeScheme(vs.map(Terms.name), body, None)

  /** Convert a Set of class identifier Names into a list of TypeClassConstraint.simple values. */
  def toConstraints(classes: Set[Name]): Seq[TypeClassConstraint] =
    classes.toSeq.map(n => TypeClassConstraint.simple(n))

  def polyConstrained(vsWithConstraints: Map[String, Set[Name]], body: Type): TypeScheme =
    val vars = vsWithConstraints.keys.toSeq.map(Terms.name)
    val constraintMap: Map[Name, TypeVariableConstraints] =
      vsWithConstraints.collect {
        case (v, cs) if cs.nonEmpty =>
          Terms.name(v) -> TypeVariableConstraints(toConstraints(cs))
      }
    TypeScheme(vars, body, Some(constraintMap))

  val ORD: Set[Name] = Set("ordering")
  val EQ:  Set[Name] = Set("equality")
  val NONE: Set[Name] = Set.empty

  def schemeOrd(v: String, body: Type): TypeScheme = constrained1(v, ORD, body)
  def schemeEq(v: String, body: Type): TypeScheme = constrained1(v, EQ, body)

  def constrained1(v1: String, c1: Set[Name], body: Type): TypeScheme =
    var cm: Map[Name, TypeVariableConstraints] = Map.empty
    if c1.nonEmpty then cm = cm + (Terms.name(v1) -> TypeVariableConstraints(toConstraints(c1)))
    TypeScheme(Seq(Terms.name(v1)), body, Some(cm))

  // constrained2/3/4 are currently unused by the hydra-scala coder sources (kept for
  // cross-language parity with the Java/Python DSLs — see #553 audit). Confirm before deleting.
  def constrained2(v1: String, c1: Set[Name], v2: String, c2: Set[Name], body: Type): TypeScheme =
    var cm: Map[Name, TypeVariableConstraints] = Map.empty
    if c1.nonEmpty then cm = cm + (Terms.name(v1) -> TypeVariableConstraints(toConstraints(c1)))
    if c2.nonEmpty then cm = cm + (Terms.name(v2) -> TypeVariableConstraints(toConstraints(c2)))
    TypeScheme(Seq(Terms.name(v1), Terms.name(v2)), body, Some(cm))

  def constrained3(v1: String, c1: Set[Name], v2: String, c2: Set[Name],
                   v3: String, c3: Set[Name], body: Type): TypeScheme =
    var cm: Map[Name, TypeVariableConstraints] = Map.empty
    if c1.nonEmpty then cm = cm + (Terms.name(v1) -> TypeVariableConstraints(toConstraints(c1)))
    if c2.nonEmpty then cm = cm + (Terms.name(v2) -> TypeVariableConstraints(toConstraints(c2)))
    if c3.nonEmpty then cm = cm + (Terms.name(v3) -> TypeVariableConstraints(toConstraints(c3)))
    TypeScheme(Seq(Terms.name(v1), Terms.name(v2), Terms.name(v3)), body, Some(cm))

  def constrained4(v1: String, c1: Set[Name], v2: String, c2: Set[Name],
                   v3: String, c3: Set[Name], v4: String, c4: Set[Name], body: Type): TypeScheme =
    var cm: Map[Name, TypeVariableConstraints] = Map.empty
    if c1.nonEmpty then cm = cm + (Terms.name(v1) -> TypeVariableConstraints(toConstraints(c1)))
    if c2.nonEmpty then cm = cm + (Terms.name(v2) -> TypeVariableConstraints(toConstraints(c2)))
    if c3.nonEmpty then cm = cm + (Terms.name(v3) -> TypeVariableConstraints(toConstraints(c3)))
    if c4.nonEmpty then cm = cm + (Terms.name(v4) -> TypeVariableConstraints(toConstraints(c4)))
    TypeScheme(Seq(Terms.name(v1), Terms.name(v2), Terms.name(v3), Terms.name(v4)), body, Some(cm))

  def scheme(body: Type): TypeScheme = mono(body)

  def scheme(v: String, body: Type): TypeScheme =
    TypeScheme(Seq(Terms.name(v)), body, None)

  def scheme(v1: String, v2: String, body: Type): TypeScheme =
    TypeScheme(Seq(Terms.name(v1), Terms.name(v2)), body, None)

  def scheme(v1: String, v2: String, v3: String, body: Type): TypeScheme =
    TypeScheme(Seq(Terms.name(v1), Terms.name(v2), Terms.name(v3)), body, None)

  def scheme(v1: String, v2: String, v3: String, v4: String, body: Type): TypeScheme =
    TypeScheme(Seq(Terms.name(v1), Terms.name(v2), Terms.name(v3), Terms.name(v4)), body, None)

  // ===== Collection types =====

  def list(elements: Type): Type = Type.list(elements)
  def list(elements: String): Type = list(`var`(elements))

  def map(keys: Type, values: Type): Type = Type.map(MapType(keys, values))
  def map(keys: String, values: String): Type = map(`var`(keys), `var`(values))

  def maybe(elements: Type): Type = Type.optional(elements)
  def maybe(elements: String): Type = maybe(`var`(elements))

  def optional(elements: Type): Type = maybe(elements)
  def optional(elements: String): Type = maybe(elements)

  def set(elements: Type): Type = Type.set(elements)
  def set(elements: String): Type = set(`var`(elements))

  // ===== Pair types =====

  def pair(first: Type, second: Type): Type = Type.pair(PairType(first, second))

  /** N-ary product as right-nested pairs. */
  def productSeq(types: Seq[Type]): Type =
    if types.isEmpty then unit
    else if types.size == 1 then types.head
    else if types.size == 2 then pair(types(0), types(1))
    else pair(types.head, productSeq(types.tail))

  def product(types: Type*): Type = productSeq(types)

  // ===== Either types =====

  def either(left: Type, right: Type): Type = Type.either(EitherType(left, right))
  def either_(left: Type, right: Type): Type = either(left, right)

  // ===== Record and union types =====

  def field(n: String, t: Type): FieldType = FieldType(Terms.name(n), t)

  def record(fields: FieldType*): Type = recordWithName(PLACEHOLDER_NAME, fields*)
  def recordSeq(fields: Seq[FieldType]): Type = Type.record(fields)

  /** Record-type constructor. The tname argument is ignored by the kernel constructor
   *  (Type.record is a Seq[FieldType] alone) but kept on the API for parity with the
   *  Haskell/Java/Python DSL signatures. */
  def recordWithName(tname: Name, fields: FieldType*): Type = Type.record(fields)
  def recordWithNameSeq(tname: Name, fields: Seq[FieldType]): Type = Type.record(fields)

  def unit: Type = Type.unit

  /** Enumerated type from variant names (each carries unit). */
  def enum_(names: String*): Type =
    union(names.map(n => field(n, unit))*)

  def union(fields: FieldType*): Type = Type.union(fields)
  def unionSeq(fields: Seq[FieldType]): Type = Type.union(fields)
  def union(tname: Name, fields: FieldType*): Type = Type.union(fields)

  // ===== Wrapped types =====

  def wrap(t: Type): Type = wrapWithName(PLACEHOLDER_NAME, t)
  def wrapWithName(tname: Name, t: Type): Type = Type.wrap(t)

  // ===== Type variables =====

  def `var`(n: String): Type = Type.variable(Terms.name(n))
  def variable(n: Name): Type = Type.variable(n)

  // ===== Binding-as-Type coercion (mirrors Haskell's AsType typeclass) =====

  def use(b: Binding): Type = Type.variable(b.name)

  def field(n: String, b: Binding): FieldType = field(n, use(b))
  def list(elements: Binding): Type = list(use(elements))
  def map(keys: Binding, values: Binding): Type = map(use(keys), use(values))
  def map(keys: Binding, values: Type): Type = map(use(keys), values)
  def map(keys: Type, values: Binding): Type = map(keys, use(values))
  def optional(elements: Binding): Type = optional(use(elements))
  def set(elements: Binding): Type = set(use(elements))
  def either_(left: Binding, right: Binding): Type = either(use(left), use(right))
  def either_(left: Binding, right: Type): Type = either(use(left), right)
  def either_(left: Type, right: Binding): Type = either(left, use(right))
  def pair(first: Binding, second: Binding): Type = pair(use(first), use(second))
  def pair(first: Binding, second: Type): Type = pair(use(first), second)
  def pair(first: Type, second: Binding): Type = pair(first, use(second))
  def function(dom: Binding, cod: Binding): Type = function(use(dom), use(cod))
  def function(dom: Binding, cod: Type): Type = function(use(dom), cod)
  def function(dom: Type, cod: Binding): Type = function(dom, use(cod))
  def wrap(b: Binding): Type = wrap(use(b))

end Types
