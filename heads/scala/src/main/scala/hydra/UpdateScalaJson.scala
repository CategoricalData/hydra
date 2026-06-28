package hydra

import hydra.core.{Name, Type}
import hydra.packaging.{Module, ModuleName}

import _root_.java.nio.charset.StandardCharsets
import _root_.java.nio.file.{Files, Paths}

/**
 * Update `dist/json/hydra-scala/` from the Scala DSL sources in
 * `packages/hydra-scala/src/main/scala/hydra/sources/scala/`. Mirror of the
 * Java driver `heads/java/src/main/java/hydra/UpdateJavaJson.java`.
 *
 * Pipeline:
 *   1. Load the kernel universe (and hydra-java + hydra-jvm so cross-package
 *      references resolve) from `dist/json/hydra-{kernel,jvm,java}/`.
 *   2. Collect the Scala DSL source modules' `module_` static fields from
 *      `hydra.sources.scala.*`.
 *   3. Run `hydra.codegen.inferModulesGiven(universe + sources, sources)` to
 *      get typed Modules — adds TermSignatures, typed let bindings, explicit
 *      `tyapp` wrappers. Without this the JSON output diverges from the
 *      Haskell baseline by exactly the inference annotations.
 *   4. Encode each inferred Module to JSON via `hydra.codegen.moduleToJson`.
 *   5. Write each output to `dist/json/hydra-scala/src/main/json/hydra/scala/<name>.json`.
 *
 * Phase-2 caveat (#509): this driver currently handles only the Scala source
 * files that have been translated to Scala (Syntax, Language, Manifest as of
 * 2026-06-27); the rest (Coder, Serde, Utils) are still produced by the
 * Haskell DSL pass. Until all 6 source files are translated, this driver and
 * the Haskell DSL pass coexist — DO NOT delete any baseline JSON file the
 * Haskell pass still writes.
 *
 * Usage (run via sbt from packages/hydra-scala/):
 *   sbt "runMain hydra.updateScalaJson"
 */
@main def updateScalaJson(): Unit =
  // Resolve worktree root from cwd. sbt typically runs from packages/hydra-scala/,
  // but we accept any descendant of the root.
  val cwd = Paths.get("").toAbsolutePath
  val worktreeRoot: _root_.java.nio.file.Path =
    Iterator.iterate(cwd)(_.getParent).takeWhile(_ != null)
      .find(p => Files.exists(p.resolve("CLAUDE.md")))
      .getOrElse(throw new RuntimeException(s"Could not find worktree root above $cwd"))

  val distJsonRoot = worktreeRoot.resolve("dist/json").toString
  val outDir = worktreeRoot.resolve("dist/json/hydra-scala/src/main/json")
  System.err.println(s"Hydra root:  $worktreeRoot")
  System.err.println(s"dist/json:   $distJsonRoot")
  System.err.println(s"output dir:  $outDir")

  // 1. Load the kernel + jvm + java universes — every module these expose
  //    needs to be present when we run inference, since the Scala sources
  //    reference hydra.core, hydra.coders, hydra.variants, hydra.util, etc.
  System.err.println()
  System.err.println("Loading universe ...")
  var t0 = System.nanoTime()
  val schemaMap0: Map[Name, Type] = Generation.bootstrapSchemaMap()
  val kernelJsonDir = worktreeRoot.resolve("dist/json/hydra-kernel/src/main/json").toString
  val kernelMainNs: Seq[ModuleName] = Generation.readManifestField(kernelJsonDir, "mainModules")
  // mainDslModules lists the type-defining modules whose DSL wrappers are
  // emitted; the wrappers themselves live at hydra.dsl.<X>. Prefix to load,
  // but only for wrappers that physically exist on disk (some kernel types
  // don't get DSL wrappers).
  val kernelDslNs: Seq[ModuleName] =
    Generation.readManifestField(kernelJsonDir, "mainDslModules")
      .map(ns => "hydra.dsl." + ns.stripPrefix("hydra."))
      .filter(ns =>
        Files.exists(Paths.get(kernelJsonDir,
          hydra.codegen.moduleNameToPath(ns) + ".json")))
  val kernelNs: Seq[ModuleName] = (kernelMainNs ++ kernelDslNs).distinct
  val kernelMods: Seq[Module] = Generation.loadModulesFromJson(kernelJsonDir, schemaMap0, kernelNs)
  val jvmJsonDir = worktreeRoot.resolve("dist/json/hydra-jvm/src/main/json").toString
  val jvmNs: Seq[ModuleName] =
    if Files.exists(Paths.get(jvmJsonDir, "manifest.json")) then
      Generation.readManifestField(jvmJsonDir, "mainModules")
    else Seq.empty
  val jvmMods: Seq[Module] =
    if jvmNs.nonEmpty then Generation.loadModulesFromJson(jvmJsonDir, schemaMap0, jvmNs) else Seq.empty
  val javaJsonDir = worktreeRoot.resolve("dist/json/hydra-java/src/main/json").toString
  val javaNs: Seq[ModuleName] =
    if Files.exists(Paths.get(javaJsonDir, "manifest.json")) then
      Generation.readManifestField(javaJsonDir, "mainModules")
    else Seq.empty
  val javaMods: Seq[Module] =
    if javaNs.nonEmpty then Generation.loadModulesFromJson(javaJsonDir, schemaMap0, javaNs) else Seq.empty
  // Load hydra-scala's existing DSL wrappers (hydra.dsl.scala.syntax etc.) so
  // Coder can reference ScalaSyntax.* generated accessors at inference time.
  val scalaPkgJsonDir = worktreeRoot.resolve("dist/json/hydra-scala/src/main/json").toString
  val scalaDslNs: Seq[ModuleName] =
    if Files.exists(Paths.get(scalaPkgJsonDir, "manifest.json")) then
      Generation.readManifestField(scalaPkgJsonDir, "mainDslModules")
        .map(ns => "hydra.dsl." + ns.stripPrefix("hydra."))
        .filter(ns => Files.exists(Paths.get(scalaPkgJsonDir,
          hydra.codegen.moduleNameToPath(ns) + ".json")))
    else Seq.empty
  val scalaDslMods: Seq[Module] =
    if scalaDslNs.nonEmpty then Generation.loadModulesFromJson(scalaPkgJsonDir, schemaMap0, scalaDslNs) else Seq.empty

  val universeBase: Seq[Module] = kernelMods ++ jvmMods ++ javaMods ++ scalaDslMods
  System.err.println(f"  ${kernelMods.size} kernel + ${jvmMods.size} jvm + ${javaMods.size} java + ${scalaDslMods.size} scalaDsl = ${universeBase.size} universe modules (${(System.nanoTime() - t0) / 1e9}%.1fs)")

  // 2. Load the Scala DSL source modules.
  System.err.println()
  System.err.println("Loading Scala DSL source modules ...")
  val sources: Seq[Module] = Seq(
    hydra.sources.scala.Syntax.module_,
    hydra.sources.scala.Language.module_,
    hydra.sources.scala.Utils.module_,
    hydra.sources.scala.Serde.module_,
    hydra.sources.scala.Coder.module_)
  for m <- sources do
    System.err.println(s"  ${m.name}: ${m.definitions.size} definitions")

  // 3. Run inference (universe + sources, infer only sources).
  System.err.println()
  System.err.println("Running inference ...")
  t0 = System.nanoTime()
  val bsGraph = Generation.bootstrapGraph()
  val ctx = hydra.typing.InferenceContext(0, Seq.empty)
  val universePlusSources: Seq[Module] = universeBase ++ sources
  val inferred: Seq[Module] = hydra.codegen.inferModulesGiven(ctx)(bsGraph)(universePlusSources)(sources) match
    case Left(err) =>
      System.err.println(s"ERROR: inference failed: $err")
      System.exit(2)
      throw new RuntimeException("unreachable")
    case Right(ms) => ms
  System.err.println(f"  inferred ${inferred.size} modules (${(System.nanoTime() - t0) / 1e9}%.1fs)")

  // 4 + 5. Encode each Module to JSON, write into dist/json/hydra-scala/.
  System.err.println()
  System.err.println("Writing JSON ...")
  t0 = System.nanoTime()
  val graph = hydra.codegen.modulesToGraph(bsGraph)(universePlusSources)(universePlusSources)
  val schemaMap: Map[Name, Type] = hydra.codegen.buildSchemaMap(graph)
  Files.createDirectories(outDir)
  var nWritten = 0
  var nUnchanged = 0
  for m <- inferred do
    val jsonStr: String = hydra.codegen.moduleToJson(schemaMap)(m) match
      case Left(err) =>
        System.err.println(s"ERROR: encode failed for ${m.name}: $err")
        System.exit(3)
        throw new RuntimeException("unreachable")
      case Right(s) => s
    val relPath = hydra.codegen.moduleNameToPath(m.name) + ".json"
    val filePath = outDir.resolve(relPath)
    Files.createDirectories(filePath.getParent)
    val newContent = if jsonStr.endsWith("\n") then jsonStr else jsonStr + "\n"
    val unchanged =
      Files.exists(filePath) &&
        new String(Files.readAllBytes(filePath), StandardCharsets.UTF_8) == newContent
    if unchanged then
      nUnchanged += 1
    else
      Files.write(filePath, newContent.getBytes(StandardCharsets.UTF_8))
      nWritten += 1
      System.err.println(s"  WROTE   $relPath")

  System.err.println()
  System.err.println(f"Done: $nWritten written, $nUnchanged unchanged (${(System.nanoTime() - t0) / 1e9}%.1fs)")
