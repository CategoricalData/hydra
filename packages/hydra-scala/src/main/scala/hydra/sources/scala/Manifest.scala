package hydra.sources.scala

import hydra.packaging.Module

/**
 * Package manifest for hydra-scala.
 *
 * Owns the Scala coder DSL sources.
 *
 * In-progress migration (#509): Syntax, Language, Serde, Utils translated.
 * Coder still defined in the Haskell DSL.
 * Order matches Haskell Manifest.hs `mainModules`: Coder/Language/Serde/Syntax/Utils.
 */
object Manifest:

  val mainModules: Seq[Module] = Seq(
    Coder.module_,
    Language.module_,
    Serde.module_,
    Syntax.module_,
    Utils.module_)

  val mainDslModules: Seq[Module] = Seq(
    Syntax.module_)

  val mainEncodingModules: Seq[Module] = Seq.empty

  val testModules: Seq[Module] = Seq.empty

end Manifest
