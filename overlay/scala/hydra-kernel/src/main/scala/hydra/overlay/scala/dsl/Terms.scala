package hydra.overlay.scala.dsl

import hydra.core.*

import _root_.java.math.BigInteger

/**
 * A domain-specific language for constructing Hydra terms in Scala.
 * Mirrors Hydra.Dsl.Terms (Haskell), hydra.dsl.terms (Python),
 * and hydra.overlay.java.dsl.Terms (Java). Scala stays slim because
 * hydra.core.Name is a String alias, the kernel constructors are
 * Scala 3 enums, and Scala native Option/Seq/Map/Set/Tuple2/Either
 * remove the wrapper layers Java needs.
 */
object Terms:

  val IGNORED_VARIABLE: Name = "_"

  /** Create a Name from a String. Since Name is `type Name = String`, this is identity. */
  def name(s: String): Name = s

  // ===== Annotations =====

  /** Attach an annotation Term to a term. */
  def annot(ann: Term, base: Term): Term =
    Term.annotated(AnnotatedTerm(base, ann))

  /** Attach a description annotation. */
  def annot(description: String, base: Term): Term =
    annot(name("description"), string(description), base)

  /** Attach a single-pair annotation map to a term. */
  def annot(key: Name, value: Term, base: Term): Term =
    annot(annotationMapAsTerm(Map(key -> value)), base)

  /** Attach a Map annotation, wrapping it as a TermMap (#386). */
  def annot(ann: Map[Name, Term], base: Term): Term =
    annot(annotationMapAsTerm(ann), base)

  /** Wrap a Map[Name, Term] as a Term.map, the conventional annotation shape. */
  def annotationMapAsTerm(ann: Map[Name, Term]): Term =
    val entries: Map[Term, Term] = ann.map((k, v) => Term.variable(k) -> v)
    Term.map(entries)

  /** Argument-flipped alias for annot. */
  def annotated(base: Term, ann: Map[Name, Term]): Term = annot(ann, base)

  // ===== Application =====

  def apply(func: Term, arg: Term): Term =
    Term.application(Application(func, arg))

  def apply(func: Term, args: Term*): Term =
    args.foldLeft(func)((acc, a) => apply(acc, a))

  def applyAll(func: Term, args: Seq[Term]): Term =
    args.foldLeft(func)((acc, a) => apply(acc, a))

  def app(func: Term, args: Term*): Term = apply(func, args*)
  def app(func: String, args: Term*): Term = apply(`var`(func), args*)

  // ===== Literals =====

  def literal(value: Literal): Term = Term.literal(value)

  def binary(value: String): Term = literal(Literal.binary(value))
  def boolean_(value: Boolean): Term = literal(Literal.boolean(value))
  def decimal(value: BigDecimal): Term = literal(Literal.decimal(value))
  def false_ : Term = boolean_(false)
  def true_ : Term = boolean_(true)
  def string(value: String): Term = literal(Literal.string(value))

  // ===== Integer terms =====

  def integer(value: IntegerValue): Term = literal(Literal.integer(value))
  def int8(value: Byte): Term = integer(IntegerValue.int8(value))
  def int16(value: Short): Term = integer(IntegerValue.int16(value))
  def int32(value: Int): Term = integer(IntegerValue.int32(value))
  def int64(value: Long): Term = integer(IntegerValue.int64(value))
  def bigint(value: BigInt): Term = integer(IntegerValue.bigint(value))
  def bigint(value: BigInteger): Term = bigint(BigInt(value))
  def uint8(value: Byte): Term = integer(IntegerValue.uint8(value))
  def uint16(value: Int): Term = integer(IntegerValue.uint16(value))
  def uint32(value: Long): Term = integer(IntegerValue.uint32(value))
  def uint64(value: BigInt): Term = integer(IntegerValue.uint64(value))

  // ===== Floating-point terms =====

  def float_(value: FloatValue): Term = literal(Literal.float(value))
  def float32(value: Float): Term = float_(FloatValue.float32(value))
  def float64(value: Double): Term = float_(FloatValue.float64(value))

  // ===== Character + comparison =====

  /** Character as int32 (ord value). */
  def char_(value: Char): Term = int32(value.toInt)

  /** Construct a Comparison value as an injection into hydra.mantle.Comparison. */
  def comparison(c: hydra.util.Comparison): Term =
    val variant: String = c match
      case hydra.util.Comparison.equalTo    => "equalTo"
      case hydra.util.Comparison.lessThan   => "lessThan"
      case hydra.util.Comparison.greaterThan => "greaterThan"
    injectUnit("hydra.mantle.Comparison", variant)

  // ===== Function terms =====

  def lambda(param: String, body: Term): Term =
    Term.lambda(Lambda(name(param), None, body))

  def lambda(p1: String, p2: String, body: Term): Term =
    lambda(p1, lambda(p2, body))

  def lambda(p1: String, p2: String, p3: String, body: Term): Term =
    lambda(p1, lambda(p2, lambda(p3, body)))

  def lambdas(params: Seq[String], body: Term): Term =
    params.foldRight(body)((p, acc) => lambda(p, acc))

  def lambdaTyped(param: String, dom: Type, body: Term): Term =
    Term.lambda(Lambda(name(param), Some(dom), body))

  def identity: Term = lambda("x_", `var`("x_"))

  def constant(value: Term): Term = lambda(IGNORED_VARIABLE, value)

  /** compose f g = \x -> f (g x). */
  def compose(f: Term, g: Term): Term =
    lambda("arg_", apply(f, apply(g, `var`("arg_"))))

  def primitive(primName: Name): Term = Term.variable(primName)

  // ===== Elimination terms =====

  def project(recordName: Name, fieldName: Name): Term =
    Term.project(Projection(recordName, fieldName))

  def unwrap(wrapName: Name): Term = Term.unwrap(wrapName)

  def first: Term = primitive("hydra.lib.pairs.first")
  def second: Term = primitive("hydra.lib.pairs.second")

  // ===== Pattern matching =====

  def match_(typeName: Name, defaultCase: Option[Term], fields: Field*): Term =
    Term.cases(CaseStatement(typeName, defaultCase, fieldsToAlternatives(fields)))

  def matchSeq(typeName: Name, defaultCase: Option[Term], fields: Seq[Field]): Term =
    Term.cases(CaseStatement(typeName, defaultCase, fieldsToAlternatives(fields)))

  /** Convert case Fields to CaseAlternatives. */
  def fieldsToAlternatives(fields: Seq[Field]): Seq[CaseAlternative] =
    fields.map(f => CaseAlternative(f.name, f.term))

  // ===== Let bindings =====

  def let_(varName: String, defined: Term, body: Term): Term =
    Term.let(Let(Seq(Binding(name(varName), defined, None)), body))

  /** Multi-binding let from a list of let-binding Fields. */
  def lets(bindings: Seq[Field], body: Term): Term =
    val bs = bindings.map(f => Binding(f.name, f.term, None))
    Term.let(Let(bs, body))

  def letsTyped(bindings: Seq[Binding], body: Term): Term =
    Term.let(Let(bindings, body))

  // ===== Field construction =====

  def field(name: Name, term: Term): Field = Field(name, term)
  def fieldVar(name: Name, varName: String): Field = Field(name, `var`(varName))

  // ===== Collection terms =====

  def list(elements: Term*): Term = Term.list(elements)
  def listSeq(elements: Seq[Term]): Term = Term.list(elements)

  def listOfStrings(elements: Seq[String]): Term = listSeq(elements.map(string))

  def map(value: Map[Term, Term]): Term = Term.map(value)
  def set(value: Set[Term]): Term = Term.set(value)

  // ===== Optional terms =====

  def optional(maybeTerm: Option[Term]): Term = Term.optional(maybeTerm)
  def nothing: Term = optional(None)
  def just(elem: Term): Term = optional(Some(elem))
  def just(varName: String): Term = just(`var`(varName))

  // ===== Pair + tuple terms =====

  def pair(a: Term, b: Term): Term = Term.pair((a, b))
  def tuple2(a: Term, b: Term): Term = pair(a, b)

  /** N-tuple via right-nested pairs. Empty = unit; single = element; 2 = pair. */
  def tuple(elements: Term*): Term = tupleSeq(elements)
  def tupleSeq(elements: Seq[Term]): Term =
    if elements.isEmpty then unit
    else if elements.size == 1 then elements.head
    else if elements.size == 2 then pair(elements(0), elements(1))
    else pair(elements.head, tupleSeq(elements.tail))

  def product(elements: Term*): Term = tupleSeq(elements)
  def productSeq(elements: Seq[Term]): Term = tupleSeq(elements)

  def triple(a: Term, b: Term, c: Term): Term = pair(a, pair(b, c))
  def tuple3(a: Term, b: Term, c: Term): Term = triple(a, b, c)

  // tuple4/5 are currently unused by the hydra-scala coder sources (kept for cross-language
  // parity with the Java/Python DSLs — see #553 audit). Confirm before deleting.
  def tuple4(a: Term, b: Term, c: Term, d: Term): Term = pair(a, pair(b, pair(c, d)))
  def tuple5(a: Term, b: Term, c: Term, d: Term, e: Term): Term =
    pair(a, pair(b, pair(c, pair(d, e))))

  // ===== Either terms =====

  def left(term: Term): Term = Term.either(scala.util.Left[Term, Term](term))
  def right(term: Term): Term = Term.either(scala.util.Right[Term, Term](term))

  // ===== Record terms =====

  def record(typeName: Name, fields: Field*): Term =
    Term.record(Record(typeName, fields))

  def recordSeq(typeName: Name, fields: Seq[Field]): Term =
    Term.record(Record(typeName, fields))

  // ===== Unit =====

  def unit: Term = Term.unit

  // ===== Union terms (injections) =====

  def inject(typeName: Name, field: Field): Term =
    Term.inject(Injection(typeName, field))

  def inject(typeName: Name, fieldName: Name, term: Term): Term =
    inject(typeName, Field(fieldName, term))

  /** Unit-valued injection (enum-style variant). */
  def injectUnit(typeName: Name, fieldName: Name): Term =
    inject(typeName, Field(fieldName, unit))

  // ===== Wrapped terms =====

  def wrap(wrapName: Name, term: Term): Term =
    Term.wrap(WrappedTerm(wrapName, term))

  // ===== Variables =====

  def `var`(n: String): Term = Term.variable(name(n))
  def variable(n: Name): Term = Term.variable(n)

  // ===== Type abstraction and application =====

  def tylam(v: String, body: Term): Term =
    Term.typeLambda(TypeLambda(name(v), body))

  def tylams(vars: Seq[String], body: Term): Term =
    vars.foldRight(body)((v, acc) => tylam(v, acc))

  def typeLambda(vars: Seq[Name], body: Term): Term =
    vars.foldRight(body)((v, acc) => Term.typeLambda(TypeLambda(v, acc)))

  def tyapp(term: Term, typ: Type): Term =
    Term.typeApplication(TypeApplicationTerm(term, typ))

  def tyapps(term: Term, types: Type*): Term =
    types.foldLeft(term)((acc, t) => tyapp(acc, t))

  def tyappsSeq(term: Term, types: Seq[Type]): Term =
    types.foldLeft(term)((acc, t) => tyapp(acc, t))

  def typeApplication(term: Term, types: Seq[Type]): Term = tyappsSeq(term, types)

end Terms
