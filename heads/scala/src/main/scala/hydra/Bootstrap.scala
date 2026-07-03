package hydra

import hydra.core.Type

import _root_.java.io.File

/**
 * Bootstrapping entry point: loads Hydra modules from JSON and generates
 * code for a target language. Demonstrates that Scala can independently
 * regenerate Hydra from a language-independent JSON representation.
 *
 * Usage:
 *   scala hydra.Bootstrap --target <lang> --json-dir <dist/json root> [OPTIONS]
 *
 * Options:
 *   --output <dir>         Output base directory (default: /tmp/hydra-bootstrapping-demo)
 *   --include-coders       Also load coder packages (hydra-java/python/scala/lisp)
 *   --include-tests        Also load and generate kernel test modules
 *   --kernel-only          Only generate kernel modules (exclude coder packages)
 *   --types-only           Only generate type-defining modules
 *   --ext-json-dir <dir>   Legacy flag; ignored under the per-package layout.
 */
@main def bootstrap(args: String*): Unit =
  // Coder packages loaded on top of the kernel + Haskell baseline when
  // --include-coders is set.
  val coderPackages = Seq("hydra-java", "hydra-python", "hydra-scala", "hydra-lisp")

  var target: Option[String] = None
  var jsonDirArg: Option[String] = None
  var outBase = "/tmp/hydra-bootstrapping-demo"
  var includeCoders = false
  var includeTests = false
  var typesOnly = false
  var kernelOnly = false

  var i = 0
  while i < args.length do
    args(i) match
      case "--target" => i += 1; target = Some(args(i))
      case "--json-dir" => i += 1; jsonDirArg = Some(args(i))
      case "--ext-json-dir" => i += 1  // legacy flag, skip the value too
      case "--output" => i += 1; outBase = args(i)
      case "--include-coders" => includeCoders = true
      case "--include-tests" => includeTests = true
      case "--types-only" => typesOnly = true
      case "--kernel-only" => kernelOnly = true
      case other => System.err.println(s"Unknown option: $other")
    i += 1

  if target.isEmpty || jsonDirArg.isEmpty then
    println("Usage: scala hydra.Bootstrap --target <lang> --json-dir <dist/json root> [OPTIONS]")
    println()
    println("Options:")
    println("  --output <dir>         Output base directory")
    println("  --include-coders       Also load coder package modules")
    println("  --include-tests        Also load and generate kernel test modules")
    println("  --kernel-only          Only generate kernel modules (exclude coder packages)")
    println("  --types-only           Only generate type-defining modules")
    println("  --ext-json-dir <dir>   Legacy flag; ignored under the per-package layout")
    System.exit(1)

  val tgt = target.get

  // Backward compatibility: accept an old-style --json-dir ending in
  // <pkg>/src/main/json and strip down to the dist/json root.
  val distJsonRoot = BootstrapHelpers.legacyJsonDirToRoot(jsonDirArg.get)
  val targetCap = tgt.capitalize
  val outDir = outBase + File.separator + "scala-to-" + tgt

  println("==========================================")
  println(s"Mapping JSON to $targetCap (via Scala host)")
  println("==========================================")
  println(s"  Host language:   Scala")
  println(s"  Target language: $targetCap")
  println(s"  JSON root:       $distJsonRoot")
  println(s"  Output:          $outDir")
  println(s"  Include coders:  $includeCoders")
  println(s"  Include tests:   $includeTests")
  if typesOnly then println("  Filter:          types only")
  if kernelOnly then println("  Filter:          kernel only")
  println("==========================================")
  println()

  val totalStart = System.currentTimeMillis()

  // Step 1: Build schema map (shared across all module loads)
  println("Step 1: Building schema map...")
  var stepStart = System.currentTimeMillis()
  val schemaMap = Generation.bootstrapSchemaMap()
  println(s"  Schema map has ${schemaMap.size} types.")
  println(s"  Time: ${Generation.formatTime(System.currentTimeMillis() - stepStart)}")
  println()

  // Step 2: Load baseline packages (hydra-kernel + hydra-haskell).
  println("Step 2: Loading baseline main modules from JSON...")
  stepStart = System.currentTimeMillis()
  val kernelMods = BootstrapHelpers.loadPackageMain(distJsonRoot, "hydra-kernel", schemaMap)
  val haskellMods = BootstrapHelpers.loadPackageMain(distJsonRoot, "hydra-haskell", schemaMap)
  val baselineMods = kernelMods ++ haskellMods
  var stepTime = System.currentTimeMillis() - stepStart
  val totalDefs = baselineMods.map(_.definitions.size).sum
  println(s"  Loaded ${baselineMods.size} baseline modules ($totalDefs definitions).")
  println(s"  Time: ${Generation.formatTime(stepTime)}")
  println()

  val kernelNsSet = baselineMods.map(_.name).toSet

  // Step 3: Optionally load coder packages.
  var coderMods: Seq[hydra.packaging.Module] = Seq.empty
  if includeCoders then
    println("Step 3: Loading coder package modules from JSON...")
    stepStart = System.currentTimeMillis()
    coderMods = coderPackages.flatMap(pkg => BootstrapHelpers.loadPackageMain(distJsonRoot, pkg, schemaMap))
    stepTime = System.currentTimeMillis() - stepStart
    println(s"  Loaded ${coderMods.size} coder modules.")
    println(s"  Time: ${Generation.formatTime(stepTime)}")
    println()
  else
    println("Step 3: Skipping coder packages")
    println()

  var allMainMods = baselineMods ++ coderMods

  // Apply filters
  var modsToGenerate = allMainMods
  if kernelOnly then
    val before = modsToGenerate.size
    modsToGenerate = modsToGenerate.filter(m => kernelNsSet.contains(m.name))
    allMainMods = allMainMods.filter(m => kernelNsSet.contains(m.name))
    println(s"Filtering to kernel modules... $before -> ${modsToGenerate.size}")
    println()

  // Generate main modules
  val outMain = outDir + File.separator + "src/main"
  println(s"Mapping ${modsToGenerate.size} modules to $targetCap...")
  println(s"  Universe: ${allMainMods.size} modules")
  println(s"  Output: $outMain")
  println()

  stepStart = System.currentTimeMillis()

  val fileCount = tgt match
    case "haskell" => Generation.writeHaskell(outMain + "/haskell", allMainMods, modsToGenerate)
    case "java" => Generation.writeJava(outMain + "/java", allMainMods, modsToGenerate)
    case "python" => Generation.writePython(outMain + "/python", allMainMods, modsToGenerate)
    case "scala" => Generation.writeScala(outMain + "/scala", allMainMods, modsToGenerate)
    case "clojure" => Generation.writeLispDialect(outMain + "/clojure", "clojure", "clj", allMainMods, modsToGenerate)
    case "scheme" => Generation.writeLispDialect(outMain + "/scheme", "scheme", "scm", allMainMods, modsToGenerate)
    case "common-lisp" => Generation.writeLispDialect(outMain + "/common-lisp", "commonLisp", "lisp", allMainMods, modsToGenerate)
    case "emacs-lisp" => Generation.writeLispDialect(outMain + "/emacs-lisp", "emacsLisp", "el", allMainMods, modsToGenerate)
    case other =>
      println(s"Unknown target: $other")
      System.exit(1)
      0

  stepTime = System.currentTimeMillis() - stepStart
  println(s"  Generated $fileCount files.")
  println(s"  Time: ${Generation.formatTime(stepTime)}")
  println()

  // #473 Step 0 — lib pass + redirect (see project_473_self_host_lib_pass_gap / the Java/Python host
  // drivers + bootstrap-from-json/Main.hs). The hydra.lib.* primitive IMPLEMENTATIONS live at
  // hydra.<lang>.lib.*; hydra.lib.* is free for the generated PrimitiveDefinition def-modules. When the
  // Scala host generates a target that consumes def-modules (everything except haskell, which uses the
  // registry), it must (1) emit the hydra.lib.* def-modules from their LOWERED form (lib pass, now), and
  // (2) redirect generated consumer call-sites to hydra.<lang>.lib.* (redirect, LAST — after the test
  // pass below also writes into outMain/<target> and outTest/<target>).
  if tgt != "haskell" then
    BootstrapHelpers.runLibPass(tgt, outMain + File.separator + tgt, allMainMods, modsToGenerate)

  // Optionally load and generate test modules. Test modules live under
  // hydra-kernel/src/test/json/, and the kernel's main manifest lists them.
  var testFileCount = 0
  if includeTests then
    val kernelMainDir = BootstrapHelpers.packageMainDir(distJsonRoot, "hydra-kernel")
    val testJsonDir = distJsonRoot + File.separator + "hydra-kernel" +
      File.separator + "src" + File.separator + "test" + File.separator + "json"
    println("Loading test modules from JSON...")
    println(s"  Source: $testJsonDir")
    stepStart = System.currentTimeMillis()
    val testNamespaces = Generation.readManifestField(kernelMainDir, "testModules")
    val testMods = Generation.loadModulesFromJson(testJsonDir, schemaMap, testNamespaces)
    stepTime = System.currentTimeMillis() - stepStart
    val testBindings = testMods.map(_.definitions.size).sum
    println(s"  Loaded ${testMods.size} test modules ($testBindings bindings).")
    println(s"  Time: ${Generation.formatTime(stepTime)}")
    println()

    val allUniverse = allMainMods ++ testMods

    // Filter skip-emit test namespaces (e.g. hydra.test.testEnv): these are
    // type-only stubs in the DSL whose hand-written per-language counterparts
    // are the source of truth. Emitting them would overwrite hand-written code
    // that registers primitives for the test graph. Mirrors
    // testSkipEmitModuleNames in Hydra.Sources.Test.All and the equivalent
    // filter in heads/python/.../bootstrap.py.
    val testSkipEmit = Set("hydra.test.testEnv")
    val testModsToEmit = testMods.filter(m => !testSkipEmit.contains(m.name))

    val outTest = outDir + File.separator + "src/test"

    println(s"Mapping test modules to $targetCap...")
    println(s"  Universe: ${allUniverse.size} modules")
    println(s"  Generating: ${testModsToEmit.size} test modules")
    println(s"  Output: $outTest")
    println()

    stepStart = System.currentTimeMillis()

    testFileCount = tgt match
      case "haskell" => Generation.writeHaskell(outTest + "/haskell", allUniverse, testModsToEmit)
      case "java" => Generation.writeJava(outTest + "/java", allUniverse, testModsToEmit)
      case "python" => Generation.writePython(outTest + "/python", allUniverse, testModsToEmit)
      case "scala" => Generation.writeScala(outTest + "/scala", allUniverse, testModsToEmit)
      case "clojure" => Generation.writeLispDialect(outTest + "/clojure", "clojure", "clj", allUniverse, testModsToEmit)
      case "scheme" => Generation.writeLispDialect(outTest + "/scheme", "scheme", "scm", allUniverse, testModsToEmit)
      case "common-lisp" => Generation.writeLispDialect(outTest + "/common-lisp", "commonLisp", "lisp", allUniverse, testModsToEmit)
      case "emacs-lisp" => Generation.writeLispDialect(outTest + "/emacs-lisp", "emacsLisp", "el", allUniverse, testModsToEmit)
      case _ => 0

    stepTime = System.currentTimeMillis() - stepStart
    println(s"  Generated $testFileCount test files.")
    println(s"  Time: ${Generation.formatTime(stepTime)}")
    println()

  // #473 redirect — run LAST, over every generated dir (main + test), so consumer call-sites written by
  // any pass have their hydra.lib.* impl references rewritten to hydra.<lang>.lib.*. See the lib-pass note.
  if tgt != "haskell" then
    BootstrapHelpers.redirectLibCalls(tgt, outMain + File.separator + tgt)
    if includeTests then
      BootstrapHelpers.redirectLibCalls(tgt, outDir + File.separator + "src/test" + File.separator + tgt)

  val totalTime = System.currentTimeMillis() - totalStart
  val testStr = if includeTests then s" + $testFileCount test" else ""
  println("==========================================")
  println(s"Done: $fileCount main$testStr files")
  println(s"  Output: $outDir")
  println(s"  Total time: ${Generation.formatTime(totalTime)}")
  println("==========================================")
end bootstrap

/** Helpers for walking the per-package dist/json/ layout. */
object BootstrapHelpers:

  /** The hydra.lib.* sub-namespaces whose primitives get def-modules + impl relocation (#473). */
  private val libSubs = Seq("chars", "eithers", "equality", "hashing", "lists", "literals", "logic", "maps",
    "math", "optionals", "pairs", "regex", "sets", "strings")

  private def isLibModule(m: hydra.packaging.Module): Boolean =
    m.name.startsWith("hydra.lib.")

  /** #473 lib pass: emit the hydra.lib.* PrimitiveDefinition def-modules from their LOWERED form.
   *  Mirrors genForDirLib in bootstrap-from-json/Main.hs. The lib modules are filtered from
   *  modsToGenerate and lowered; the universe lowers ONLY the lib modules so a lib
   *  default-implementation referencing another primitive resolves to the primitive, not a lowered
   *  binding. */
  def runLibPass(target: String, langDir: String, allMainMods: Seq[hydra.packaging.Module],
      modsToGenerate: Seq[hydra.packaging.Module]): Unit =
    val libMods = modsToGenerate.filter(isLibModule).map(hydra.codegen.lowerPrimitiveDefinitions)
    if libMods.isEmpty then return
    val libUniverse = allMainMods.map(m => if isLibModule(m) then hydra.codegen.lowerPrimitiveDefinitions(m) else m)
    println(s"Lib pass: emitting ${libMods.size} hydra.lib.* definition modules to $target...")
    target match
      case "java" => Generation.writeJava(langDir, libUniverse, libMods)
      case "python" => Generation.writePython(langDir, libUniverse, libMods)
      case "scala" => Generation.writeScala(langDir, libUniverse, libMods)
      case "clojure" => Generation.writeLispDialect(langDir, "clojure", "clj", libUniverse, libMods)
      case "scheme" => Generation.writeLispDialect(langDir, "scheme", "scm", libUniverse, libMods)
      case "common-lisp" => Generation.writeLispDialect(langDir, "commonLisp", "lisp", libUniverse, libMods)
      case "emacs-lisp" => Generation.writeLispDialect(langDir, "emacsLisp", "el", libUniverse, libMods)
      case _ => ()

  /** #473 redirect: rewrite generated CONSUMER references from hydra.lib.<sub> to the relocated
   *  hydra.<lang>.lib.<sub> impl namespace. Per-dialect shapes mirror bootstrap-from-json/Main.hs:
   *  dotted (scala/clojure), R7RS space-form (scheme), flat-symbol (common-lisp/emacs). Primitive
   *  NAME strings ("hydra.lib...") and the hand-written registry are left untouched. No-op for java. */
  def redirectLibCalls(target: String, langDir: String): Unit =
    val dir = new File(langDir)
    if !dir.isDirectory then return
    val files = allFilesUnder(dir)
    target match
      case "scala" =>
        for f <- files if !isLibDefFile(f) do
          val s = readFile(f)
          if s.contains("hydra.lib.") then
            val out = redirectDotted(s, "overlay.scala")
            if out != s then writeFile(f, out)
      case "clojure" =>
        for f <- files if !isLibDefFile(f) do
          val s = readFile(f)
          if s.contains("hydra.lib.") then
            val out = redirectDotted(s, "overlay.clojure")
            if out != s then writeFile(f, out)
      case "scheme" =>
        for f <- files do
          val s = readFile(f)
          if s.contains("(hydra lib ") then
            var out = s
            for sub <- libSubs do out = out.replace(s"(hydra lib $sub)", s"(hydra overlay scheme lib $sub)")
            if out != s then writeFile(f, out)
      case "common-lisp" =>
        for f <- files do
          val s = readFile(f)
          if s.contains("hydra_lib_") || s.contains(":hydra.lib.") then
            var out = s
            for sub <- libSubs do
              out = out.replace(s"hydra_lib_${sub}_", s"hydra_overlay_common_lisp_lib_${sub}_")
              out = out.replace(s" :hydra.lib.$sub", "")
            if out != s then writeFile(f, out)
      case "emacs-lisp" =>
        for f <- files do
          val s = readFile(f)
          if s.contains("hydra_lib_") || s.contains(":hydra.lib.") then
            var out = s
            for sub <- libSubs do
              out = out.replace(s"hydra_lib_${sub}_", s"hydra_overlay_emacs_lisp_lib_${sub}_")
              out = out.replace(s" :hydra.lib.$sub", "")
            if out != s then writeFile(f, out)
      case _ => () // java + haskell: no redirect

  /** Dotted-language redirect (scala/clojure), protecting quoted primitive-NAME strings. */
  private def redirectDotted(s: String, langSeg: String): String =
    val sentinel = "@@HYDRA_LIB_NAME@@" // improbable token; never appears in generated source
    var out = s.replace("\"hydra.lib.", "\"" + sentinel)
    for sub <- libSubs do
      val old = "hydra.lib." + sub
      val nw = "hydra." + langSeg + ".lib." + sub
      out = out.replace(old + ".", nw + ".")
      out = out.replace(old + ";", nw + ";")
      out = out.replace(old + "\n", nw + "\n")
      out = out.replace(old + " ", nw + " ")
      out = out.replace(old + "}", nw + "}")
    out.replace(sentinel, "hydra.lib.")

  /** Files under `hydra/lib/` must NOT be redirected: the lib pass emits the def-modules there and they
   *  must keep their canonical `package hydra.lib.*` (redirecting the package decl would relocate the
   *  def-module on top of the hand-written impl at hydra.overlay.scala.lib.* and shadow its generic
   *  methods with the def-module's `lazy val`s — "does not take type parameters"). The Haskell driver
   *  keeps these canonical by running the lib pass with NO redirect.
   *  Consumers that need redirecting live everywhere EXCEPT hydra/lib/. */
  private def isLibDefFile(f: File): Boolean =
    f.getPath.replace(File.separatorChar, '/').contains("/hydra/lib/")

  private def allFilesUnder(dir: File): Seq[File] =
    val here = Option(dir.listFiles()).getOrElse(Array.empty[File]).toSeq
    here.flatMap(f => if f.isDirectory then allFilesUnder(f) else Seq(f))

  private def readFile(f: File): String =
    new String(_root_.java.nio.file.Files.readAllBytes(f.toPath), _root_.java.nio.charset.StandardCharsets.UTF_8)

  private def writeFile(f: File, content: String): Unit =
    _root_.java.nio.file.Files.write(f.toPath, content.getBytes(_root_.java.nio.charset.StandardCharsets.UTF_8))

  /** Return the JSON directory for a package's main modules. */
  def packageMainDir(root: String, pkg: String): String =
    root + File.separator + pkg + File.separator + "src" +
      File.separator + "main" + File.separator + "json"

  /** Read a manifest field, returning empty if the manifest or field is missing. */
  def readManifestFieldOrEmpty(pkgDir: String, fieldName: String): Seq[String] =
    val manifestFile = new File(pkgDir + File.separator + "manifest.json")
    if !manifestFile.exists() then Seq.empty
    else
      try Generation.readManifestField(pkgDir, fieldName)
      catch case _: RuntimeException => Seq.empty

  /** Load a package's mainModules from its manifest. */
  def loadPackageMain(root: String, pkg: String,
      schemaMap: Map[String, Type]): Seq[hydra.packaging.Module] =
    val pkgDir = packageMainDir(root, pkg)
    val allNs = readManifestFieldOrEmpty(pkgDir, "mainModules")
    if allNs.isEmpty then Seq.empty
    else
      println(s"  $pkg: ${allNs.size} modules from $pkgDir")
      Generation.loadModulesFromJson(pkgDir, schemaMap, allNs)

  /** Map a legacy --json-dir value (e.g. ".../dist/json/hydra-kernel/src/main/json")
   *  to the dist-json root (".../dist/json"). If the path does not match the
   *  expected shape, return unchanged. */
  def legacyJsonDirToRoot(path: String): String =
    val sep = File.separator
    val trimmed = if path.endsWith(sep) then path.dropRight(sep.length) else path
    val parts = trimmed.split(_root_.java.util.regex.Pattern.quote(sep)).toSeq
    if parts.length >= 4 && parts.takeRight(3) == Seq("src", "main", "json") then
      parts.dropRight(4).mkString(sep) match
        case "" => "."
        case s => s
    else path
end BootstrapHelpers
