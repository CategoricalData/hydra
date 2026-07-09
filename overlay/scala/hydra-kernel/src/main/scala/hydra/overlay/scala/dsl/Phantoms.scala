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
 *
 * This does not mean no completeness checking is needed, though: see
 * hydra.overlay.scala.dsl.meta.Defs.checkComplete, which verifies every
 * `lazy val` def is present in its module's DEFINITIONS list (via JVM
 * reflection, since Scala has no dedicated Def marker type to filter on).
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

  /**
   * Build a local-reference function for a namespace: `makeLocal(ns)(localName)` produces the
   * fully-qualified `Name` `ns.localName`, suitable for either `v(...)` (a bare variable
   * reference) or `applyP(...)` (a primitive/function application) — both take a `Name`/`String`.
   * Scala analogue of Java's `ref(Def)` and Python's `make_local`. Use for recursive, forward, or
   * cross-module references within/between source modules, in place of fully-spelled FQN strings.
   *
   * Usage:
   * {{{
   *   private val local = makeLocal(NS)
   *   ...
   *   applyP(local("rewriteTerm"), v("rewrite"), v("term"))
   *   v(local("someOtherDef"))
   * }}}
   */
  def makeLocal(ns: ModuleName): String => Name =
    val prefix = ns + "."
    localName => prefix + localName

  /** Apply a primitive by name to one or more curried arguments. */
  def applyP[A](primName: String, a: TypedTerm[?], rest: TypedTerm[?]*): TypedTerm[A] =
    apply(prim(primName), a +: rest*)

  // ---- Let bindings ----

  /** Single-binding let. */
  def let[A](name: String, value: TypedTerm[?], body: TypedTerm[?]): TypedTerm[A] =
    Terms.lets(Seq(Terms.field(name, value)), body)

  /** Multi-binding let. */
  def let[A](bindings: Seq[Field], body: TypedTerm[?]): TypedTerm[A] =
    Terms.lets(bindings, body)

  /** Multi-binding let — varargs form, for when bindings are listed inline rather than as a Seq. */
  def binds[A](b1: Field, b2: Field, rest: Field*)(body: TypedTerm[?]): TypedTerm[A] =
    Terms.lets(b1 +: b2 +: rest, body)

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

  /** 2-arg string concat — Haskell's (++) operator on strings desugars to this. */
  def cat2(a: TypedTerm[String], b: TypedTerm[String]): TypedTerm[String] =
    applyP("hydra.lib.strings.cat2", a, b)

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

  /**
   * Copy-with-update: build a `record typeName` whose fields are, in the order of `allFields`,
   * projected verbatim from `var(baseVar)` — except for those named in `overrides`, which supply
   * their own value. Collapses the common "reconstruct the whole record to change one field"
   * pattern (every unchanged field would otherwise be hand-written as
   * `field(F, proj(typeName, F, baseVar))`).
   *
   * An override may still reference the base for the field it replaces, e.g. to add to a
   * collection field: `field(varRenames, Maps.insert(k, v, proj(typeName, varRenames, "aliases")))`.
   *
   * `allFields` must list every field of the type in declaration order.
   */
  def recordWith[A](typeName: Name, baseVar: String, allFields: Seq[Name], overrides: Field*): TypedTerm[A] =
    val overrideByName = scala.collection.mutable.LinkedHashMap.from(overrides.map(ov => ov.name -> ov))
    val fields = allFields.map { f =>
      overrideByName.remove(f) match
        case Some(ov) => ov
        case None => field(f, proj(typeName, f, baseVar))
    }
    if overrideByName.nonEmpty then
      throw new IllegalArgumentException(
        s"recordWith override(s) not in allFields for $typeName: ${overrideByName.keys.mkString(", ")}")
    recordSeq(typeName, fields)

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

  /**
   * Begin a fluent definition: `define(NS, "name").doc("...").lam("x").lam("y").to(body)`.
   *
   * Reads top-to-bottom (declaration order) instead of the inside-out
   * `` `def`(NS, "name", doc("...", lambda("x", lambda("y", body)))) `` nesting. The terminal
   * [[DefBuilder.to]] yields the same [[Definition]] the flat form would: it composes the recorded
   * doc + lambda parameters around the body as `doc(description, lambda([params], body))`,
   * omitting the `doc`/`lambda` wrappers when none were specified. Scala's `lazy val` (used at
   * call sites for the enclosing `def`) already handles forward/cross-references, so — unlike
   * Java's `Supplier`-deferred `Def` — the body here is taken eagerly, by-name.
   */
  def define(ns: ModuleName, localName: String): DefBuilder =
    DefBuilder(ns, localName, None, Seq.empty)

  /**
   * Fluent builder for a [[Definition]]. Records an optional doc description and zero or more
   * lambda parameters, then [[to]] closes over the body to produce the [[Definition]]. See
   * [[define]].
   */
  final case class DefBuilder(ns: ModuleName, localName: String, description: Option[String], params: Seq[String]):

    /** Attach a doc description, wrapping the eventual body in `doc(description, ...)`. */
    def doc(description: String): DefBuilder =
      copy(description = Some(description))

    /** Add one lambda parameter (applied outermost-first, matching `lambda("x", lambda("y", ...))`). */
    def lam(param: String): DefBuilder =
      copy(params = params :+ param)

    /** Add several lambda parameters in order (equivalent to chained [[lam]] calls). */
    def lams(ps: String*): DefBuilder =
      copy(params = params ++ ps)

    /** Close over the body and produce the [[Definition]]. */
    def to[A](body: => TypedTerm[?]): Definition =
      val t: TypedTerm[?] = if params.isEmpty then body else lambda(params, body)
      val wrapped: TypedTerm[?] = description match
        case Some(d) => Phantoms.doc(d, t)
        case None => t
      `def`(ns, localName, wrapped)

  end DefBuilder

  /** Build a Definition with a pre-computed TypeScheme. */
  def defTyped[A](ns: ModuleName, localName: String, term: TypedTerm[A], ts: TypeScheme): Definition =
    val fqName: Name = ns + "." + localName
    val signature: TermSignature = hydra.scoping.typeSchemeToTermSignature(ts)
    Definition.term(TermDefinition(fqName, None, Some(signature), term))

end Phantoms
