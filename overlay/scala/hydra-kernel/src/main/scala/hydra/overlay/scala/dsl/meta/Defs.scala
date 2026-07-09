package hydra.overlay.scala.dsl.meta

import hydra.packaging.Definition

/**
 * Meta-DSL helper for verifying module def-registration completeness.
 *
 * Mirrors the role of Java's Defs.checkComplete (overlay/java/hydra-kernel/.../dsl/meta/Defs.java)
 * and Python's check_complete (overlay/python/hydra-kernel/.../dsl/meta/defs.py): each Hydra
 * definition in a Scala source module is authored as a `lazy val fooDef: Definition`, then
 * separately registered in the module's `DEFINITIONS: Seq[Definition]` list. Omitting a def from
 * that list silently drops it from the generated module. checkComplete catches this via JVM
 * reflection: it finds every zero-arg, `Definition`-returning method declared on the module
 * object's class, invokes it, and confirms the result is present in `registered`.
 *
 * Unlike Java, Scala has no dedicated `Def` wrapper type to filter fields by (see Helpers.scala's
 * and Phantoms.scala's doc comments: `lazy val` replaces that need) — every def-producing binding
 * is directly typed `Definition`, so the filter is by declared/return type rather than by a marker
 * class. Unlike Python (module-level function introspection), Scala's unit of authoring is an
 * `object`'s `lazy val` members, reflected via the object's own class.
 */
object Defs:

  /**
   * Verify every zero-arg, Definition-returning method declared on `moduleObject`'s class is
   * present in `registered`.
   *
   * `moduleObject` is the source module (e.g. `Utils`, passed as `Utils` — a Scala `object`
   * reference, whose runtime class is the singleton class reflection operates on).
   * `registered` is the module's own assembly list (its `DEFINITIONS`). Raises
   * `IllegalStateException` naming any orphaned method.
   */
  def checkComplete(moduleObject: AnyRef, registered: Seq[Definition]): Unit =
    val registeredSet: Set[Definition] = registered.toSet
    val definitionClass = classOf[Definition]

    val orphans = moduleObject.getClass.getDeclaredMethods.toSeq
      .filter(m => m.getParameterCount == 0 && definitionClass.isAssignableFrom(m.getReturnType))
      .filter(m => !(m.getName == "DEFINITIONS" || m.getName == "module_"))
      .flatMap { m =>
        m.setAccessible(true)
        m.invoke(moduleObject) match
          case d: Definition if !registeredSet.contains(d) => Some(m.getName)
          case _ => None
      }
      .sorted

    if orphans.nonEmpty then
      throw new IllegalStateException(
        s"checkComplete: ${moduleObject.getClass.getSimpleName.stripSuffix("$")} declares " +
          s"def-producing member(s) missing from its DEFINITIONS list: ${orphans.mkString(", ")}. " +
          "Add each to the module's DEFINITIONS list, or remove the unused member.")
