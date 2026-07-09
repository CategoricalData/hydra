package hydra.overlay.scala.dsl

import hydra.core.*
import hydra.packaging.{Definition, ModuleDependency, ModuleName, TermDefinition, TypeDefinition}
import hydra.typing.TermSignature

/**
 * Hand-written Scala DSL helpers for assembling Hydra type and term
 * definitions in Scala-language source modules (Coder.scala, Serde.scala, etc.).
 * Mirror of Haskell's Hydra.Dsl.Java.Helpers and Java's hydra.dsl.java.Helpers.
 *
 * For the deferred-body Def wrapper that Java uses, Scala uses `lazy val`
 * directly in source modules — no equivalent wrapper type is needed. A
 * registration-completeness checker still exists, though, for the same
 * reason Java's does: see hydra.overlay.scala.dsl.meta.Defs.checkComplete.
 */
object Helpers:

  /** Hydra type annotation key for human-readable docs. */
  val DESCRIPTION: Name = "description"

  /** Attach a "description" annotation to a Type. */
  def doc(description: String, base: Type): Type =
    Types.annot(DESCRIPTION, Terms.string(description), base)

  /** Attach a "description" annotation to a Term. */
  def docTerm(description: String, base: Term): Term =
    Terms.annot(description, base)

  /** TypeVariable reference for a fully-qualified name. */
  def typeref(ns: ModuleName, local: String): Type =
    Types.variable(ns + "." + local)

  /** Build a type Definition in the given namespace. */
  def typeDef(ns: ModuleName, localName: String, typ: Type): Definition =
    val fqName: Name = ns + "." + localName
    val ts = TypeScheme(Seq.empty, typ, None)
    Definition.`type`(TypeDefinition(fqName, None, ts))

  /** Build a term Definition with no type scheme (inference fills it in). */
  def termDef(ns: ModuleName, localName: String, term: Term): Definition =
    val fqName: Name = ns + "." + localName
    Definition.term(TermDefinition(fqName, None, None, term))

  /** Build a term Definition with a pre-computed TypeScheme. */
  def termDefTyped(ns: ModuleName, localName: String, term: Term, ts: TypeScheme): Definition =
    val fqName: Name = ns + "." + localName
    val signature: TermSignature = hydra.scoping.typeSchemeToTermSignature(ts)
    Definition.term(TermDefinition(fqName, None, Some(signature), term))

  /** Build a TypeScheme from a variables list and a body type. */
  def typeScheme(variables: Seq[Name], body: Type): TypeScheme =
    TypeScheme(variables, body, None)

  /** ModuleDependency on the given module, without a package qualifier. */
  def unqualifiedDep(module: ModuleName): ModuleDependency =
    ModuleDependency(module, None)

  /** Wrap a sequence of ModuleNames as unqualified ModuleDependencies. */
  def unqualifiedDeps(modules: ModuleName*): Seq[ModuleDependency] =
    modules.map(unqualifiedDep)

  /**
   * Namespaces of all kernel type modules, in the exact order produced by the Haskell pipeline
   * (`Hydra.Sources.Kernel.Types.All.kernelTypesModuleNames`) — keep this order so generated JSON
   * dependency lists stay byte-identical to the Haskell baseline.
   */
  val kernelTypesModuleNames: Seq[ModuleName] = Seq(
    "hydra.paths", "hydra.ast", "hydra.coders", "hydra.core", "hydra.docs",
    "hydra.error.checking", "hydra.error.core", "hydra.error.file",
    "hydra.error.packaging", "hydra.error.system",
    "hydra.errors", "hydra.file", "hydra.graph", "hydra.json.model",
    "hydra.packaging", "hydra.parsing",
    "hydra.query", "hydra.relational", "hydra.system", "hydra.tabular",
    "hydra.testing", "hydra.time", "hydra.topology", "hydra.typed",
    "hydra.typing", "hydra.util", "hydra.validation", "hydra.variants")

end Helpers
