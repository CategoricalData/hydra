package hydra.overlay.scala.dsl

import hydra.core.*
import hydra.packaging.{Definition, EntityMetadata, Module, ModuleName, TermDefinition}
import hydra.typed.{TypedBinding, TypedTerm}
import hydra.typing.TermSignature

import _root_.java.math.BigInteger

/**
 * Term-level phantom-typed DSL. Scala analogue of
 * hydra.dsl.meta.Phantoms (Java) and hydra.dsl.meta.phantoms (Python).
 *
 * Because hydra.typed.TypedTerm[A] is a `type` alias for hydra.core.Term in
 * Scala, every TypedTerm[A] IS a Term — no wrapper, no .value extraction,
 * no `tterm`/`unTTerm` conversions. The phantom parameter A still flows
 * through method signatures so source files can lean on type inference.
 *
 * Scala lazy val replaces Java's Defs/Def deferred-supplier pattern. The
 * source modules use `lazy val foo: TypedTerm[T] = ...` directly; forward
 * references between definitions resolve at first access.
 */
object Phantoms:

  // ---- Variables and lambdas ----

  /** Reference a variable by Name (Name = String, so a String literal works too). */
  def `var`[A](name: Name): TypedTerm[A] = Terms.variable(name)

  /** Single-parameter lambda. */
  def lambda[A](param: String, body: TypedTerm[?]): TypedTerm[A] =
    Terms.lambda(param, body)

  def lambda[A](p1: String, p2: String, body: TypedTerm[?]): TypedTerm[A] =
    lambda(p1, lambda[Any](p2, body))

  def lambda[A](p1: String, p2: String, p3: String, body: TypedTerm[?]): TypedTerm[A] =
    lambda(p1, lambda[Any](p2, lambda[Any](p3, body)))

  /** List-of-params variant. */
  def lambda[A](params: Seq[String], body: TypedTerm[?]): TypedTerm[A] =
    params.foldRight(body)((p, acc) => Terms.lambda(p, acc))

  /** Typed single-parameter lambda. */
  def lamTyped[A](param: String, paramType: Type, body: TypedTerm[?]): TypedTerm[A] =
    Terms.lambdaTyped(param, paramType, body)

  /** `\_ -> body` — unused parameter "_". */
  def constant[A](body: TypedTerm[?]): TypedTerm[A] = Terms.lambda("_", body)

  // ---- Application ----

  /** Apply a function to an argument. */
  def apply[A](fun: TypedTerm[?], arg: TypedTerm[?]): TypedTerm[A] =
    Terms.apply(fun, arg)

  /** Variadic application — apply(f, a1, a2, ..., an). */
  def apply[A](fun: TypedTerm[?], args: TypedTerm[?]*): TypedTerm[A] =
    args.foldLeft(fun: Term)((acc, a) => Terms.apply(acc, a))

  /** Reference a primitive / globally-named function by FQN — shorthand for `var`. */
  inline def prim[A](name: String): TypedTerm[A] = `var`(name)

  /** Apply a primitive by name to one argument. */
  inline def applyP[A](primName: String, a: TypedTerm[?]): TypedTerm[A] =
    apply(prim(primName), a)

  /** Apply a primitive by name to two curried arguments. */
  inline def applyP[A](primName: String, a: TypedTerm[?], b: TypedTerm[?]): TypedTerm[A] =
    apply(apply(prim(primName), a), b)

  /** Apply a primitive by name to three curried arguments. */
  inline def applyP[A](primName: String, a: TypedTerm[?], b: TypedTerm[?], c: TypedTerm[?]): TypedTerm[A] =
    apply(apply(apply(prim(primName), a), b), c)

  /** Apply a primitive by name to four curried arguments. */
  inline def applyP[A](primName: String, a: TypedTerm[?], b: TypedTerm[?], c: TypedTerm[?], d: TypedTerm[?]): TypedTerm[A] =
    apply(apply(apply(apply(prim(primName), a), b), c), d)

  /** Apply a primitive by name to five curried arguments. */
  inline def applyP[A](primName: String, a: TypedTerm[?], b: TypedTerm[?], c: TypedTerm[?], d: TypedTerm[?], e: TypedTerm[?]): TypedTerm[A] =
    apply(apply(apply(apply(apply(prim(primName), a), b), c), d), e)

  /** Apply a primitive by name to six curried arguments. */
  inline def applyP[A](primName: String, a: TypedTerm[?], b: TypedTerm[?], c: TypedTerm[?], d: TypedTerm[?], e: TypedTerm[?], f: TypedTerm[?]): TypedTerm[A] =
    apply(apply(apply(apply(apply(apply(prim(primName), a), b), c), d), e), f)

  /** Apply a primitive by name to seven curried arguments. */
  inline def applyP[A](primName: String, a: TypedTerm[?], b: TypedTerm[?], c: TypedTerm[?], d: TypedTerm[?], e: TypedTerm[?], f: TypedTerm[?], g: TypedTerm[?]): TypedTerm[A] =
    apply(apply(apply(apply(apply(apply(apply(prim(primName), a), b), c), d), e), f), g)

  /** Apply a primitive by name to eight curried arguments. */
  inline def applyP[A](primName: String, a: TypedTerm[?], b: TypedTerm[?], c: TypedTerm[?], d: TypedTerm[?], e: TypedTerm[?], f: TypedTerm[?], g: TypedTerm[?], h: TypedTerm[?]): TypedTerm[A] =
    apply(apply(apply(apply(apply(apply(apply(apply(prim(primName), a), b), c), d), e), f), g), h)

  // ---- Let bindings ----

  /** Single-binding let. */
  def let[A](name: String, value: TypedTerm[?], body: TypedTerm[?]): TypedTerm[A] =
    Terms.lets(Seq(Terms.field(name, value)), body)

  /** Multi-binding let. */
  def let[A](bindings: Seq[Field], body: TypedTerm[?]): TypedTerm[A] =
    Terms.lets(bindings, body)

  /** Build a let with an explicit TypeScheme attached to each binding. */
  def letTyped[A](name: String, value: TypedTerm[?], scheme: TypeScheme, body: TypedTerm[?]): TypedTerm[A] =
    val b = Binding(name, value, Some(scheme))
    Term.let(Let(Seq(b), body))

  /** Build a let-binding field: name -> value. */
  def field(name: Name, value: TypedTerm[?]): Field = Terms.field(name, value)

  // ---- Literals ----

  def string(s: String): TypedTerm[String] = Terms.string(s)
  def int32(i: Int): TypedTerm[Int] = Terms.int32(i)
  def int64(l: Long): TypedTerm[Long] = Terms.int64(l)
  def bigint(b: BigInt): TypedTerm[BigInt] = Terms.bigint(b)
  def bigint(b: BigInteger): TypedTerm[BigInt] = Terms.bigint(b)
  def bigint(b: Long): TypedTerm[BigInt] = Terms.bigint(BigInt(b))
  def bool(b: Boolean): TypedTerm[Boolean] = Terms.boolean_(b)

  /**
   * Construct a meta-level Term encoding a Boolean literal. Mirrors Haskell
   * DSL's `MetaTerms.true` (= `booleanLift . TypedTerm . Terms.boolean`).
   *
   * Returns a Hydra `Inject(hydra.core.Term, literal, Inject(hydra.core.Literal,
   * boolean, Literal(Boolean(b))))` expression — NOT a raw `Literal(Boolean(b))`.
   * The outer `Inject` has typeName `hydra.core.Term`, so Hydra inference
   * assigns the whole expression Hydra-type `Term`, which is what callers
   * expecting `Term` (e.g. the second arg to `just` in a `Optional Term`
   * predicate) need.
   */
  def metaBool(b: Boolean): TypedTerm[Term] =
    Terms.inject("hydra.core.Term", "literal",
      Terms.inject("hydra.core.Literal", "boolean",
        Terms.boolean_(b))).asInstanceOf[TypedTerm[Term]]
  def float64(d: Double): TypedTerm[Double] = Terms.float64(d)
  def float32(f: Float): TypedTerm[Float] = Terms.float32(f)

  // ---- Unit, optional, either, pair, lists ----

  def unit[A]: TypedTerm[A] = Terms.unit

  def just[A](x: TypedTerm[A]): TypedTerm[Option[A]] = Terms.just(x)
  def nothing[A]: TypedTerm[Option[A]] = Terms.nothing

  def right[A](x: TypedTerm[?]): TypedTerm[A] = Terms.right(x)
  def left[A](x: TypedTerm[?]): TypedTerm[A] = Terms.left(x)

  def pair[A, B](a: TypedTerm[A], b: TypedTerm[B]): TypedTerm[(A, B)] = Terms.pair(a, b)

  def list[A](elems: TypedTerm[? <: A]*): TypedTerm[Seq[A]] =
    Terms.list(elems.map(_.asInstanceOf[Term])*)

  def listSeq[A](elems: Seq[TypedTerm[? <: A]]): TypedTerm[Seq[A]] =
    Terms.list(elems.map(_.asInstanceOf[Term])*)

  // ---- Records, injections, projections, wraps ----

  def record[A](typeName: Name, fields: Field*): TypedTerm[A] =
    Terms.record(typeName, fields*)

  def recordSeq[A](typeName: Name, fields: Seq[Field]): TypedTerm[A] =
    Terms.recordSeq(typeName, fields)

  def inject[A](typeName: Name, fieldName: Name, value: TypedTerm[?]): TypedTerm[A] =
    Terms.inject(typeName, fieldName, value)

  /** Variant case carrying unit. */
  def injectUnit[A](typeName: Name, fieldName: Name): TypedTerm[A] =
    Terms.injectUnit(typeName, fieldName)

  /** Field-projection function (not yet applied). */
  def project[A](typeName: Name, fieldName: Name): TypedTerm[A] =
    Terms.project(typeName, fieldName)

  /** project(T, F) @@ var(x) — the common projection + application pattern. */
  def proj[A](typeName: Name, fieldName: Name, varName: String): TypedTerm[A] =
    apply(project(typeName, fieldName), `var`(varName))

  def projTerm[A](typeName: Name, fieldName: Name, term: TypedTerm[?]): TypedTerm[A] =
    apply(project(typeName, fieldName), term)

  def wrap[A](typeName: Name, body: TypedTerm[?]): TypedTerm[A] =
    Terms.wrap(typeName, body)

  def unwrap[A](typeName: Name): TypedTerm[A] = Terms.unwrap(typeName)

  // ---- Pattern matching ----

  /** Pattern match on a union (no default). */
  def cases[A](typeName: Name, arg: TypedTerm[?], branches: Field*): TypedTerm[A] =
    val matchTerm = Terms.match_(typeName, None, branches*)
    Terms.apply(matchTerm, arg)

  /** Pattern match on a union with a default branch. */
  def casesWithDefault[A](typeName: Name, arg: TypedTerm[?],
                          defaultBranch: TypedTerm[?], branches: Field*): TypedTerm[A] =
    val matchTerm = Terms.match_(typeName, Some(defaultBranch), branches*)
    Terms.apply(matchTerm, arg)

  /** Match function (not yet applied). */
  def `match`[A](typeName: Name, branches: Field*): TypedTerm[A] =
    Terms.match_(typeName, None, branches*)

  def matchWithDefault[A](typeName: Name, defaultBranch: TypedTerm[?],
                          branches: Field*): TypedTerm[A] =
    Terms.match_(typeName, Some(defaultBranch), branches*)

  // ---- Type application / type lambda ----

  def tyapp[A](term: TypedTerm[?], typ: Type): TypedTerm[A] =
    Terms.tyapp(term, typ)

  def tyapps[A](term: TypedTerm[?], types: Type*): TypedTerm[A] =
    Terms.tyapps(term, types*)

  def typeLambda[A](vars: Seq[String], body: TypedTerm[?]): TypedTerm[A] =
    val names = vars.map(Terms.name)
    Terms.typeLambda(names, body)

  def typeLambda[A](v: String, body: TypedTerm[?]): TypedTerm[A] =
    typeLambda(Seq(v), body)

  // ---- Annotations / documentation ----

  /** Attach a description annotation to a term. */
  def doc[A](description: String, term: TypedTerm[A]): TypedTerm[A] =
    Terms.annot(description, term)

  // ---- Definitions ----

  /** Build a TypedBinding for a definition in a module. */
  def definitionInModule[A](mod: Module, localName: String, term: TypedTerm[A]): TypedBinding[A] =
    val fqName: Name = mod.name + "." + localName
    TypedBinding(fqName, term)

  /** Convert a TypedBinding to a Definition (no explicit type scheme). */
  def toDefinition[A](tb: TypedBinding[A]): Definition =
    Definition.term(TermDefinition(tb.name, None, None, tb.term))

  /** Build a Definition directly: namespace + localName + term, no TypedBinding. */
  def `def`[A](ns: ModuleName, localName: String, term: TypedTerm[A]): Definition =
    val fqName: Name = ns + "." + localName
    Definition.term(TermDefinition(fqName, None, None, term))

  /** Build a Definition with a pre-computed TypeScheme. */
  def defTyped[A](ns: ModuleName, localName: String, term: TypedTerm[A], ts: TypeScheme): Definition =
    val fqName: Name = ns + "." + localName
    val signature: TermSignature = hydra.scoping.typeSchemeToTermSignature(ts)
    Definition.term(TermDefinition(fqName, None, Some(signature), term))

end Phantoms
