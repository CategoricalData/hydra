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

  val kernelNsSet = kernelMods.map(_.namespace).toSet

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
    modsToGenerate = modsToGenerate.filter(m => kernelNsSet.contains(m.namespace))
    allMainMods = allMainMods.filter(m => kernelNsSet.contains(m.namespace))
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
    case other =>
      println(s"Unknown target: $other")
      System.exit(1)
      0

  stepTime = System.currentTimeMillis() - stepStart
  println(s"  Generated $fileCount files.")
  println(s"  Time: ${Generation.formatTime(stepTime)}")
  println()

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
    val outTest = outDir + File.separator + "src/test"

    println(s"Mapping test modules to $targetCap...")
    println(s"  Universe: ${allUniverse.size} modules")
    println(s"  Generating: ${testMods.size} test modules")
    println(s"  Output: $outTest")
    println()

    stepStart = System.currentTimeMillis()

    testFileCount = tgt match
      case "haskell" => Generation.writeHaskell(outTest + "/haskell", allUniverse, testMods)
      case "java" => Generation.writeJava(outTest + "/java", allUniverse, testMods)
      case "python" => Generation.writePython(outTest + "/python", allUniverse, testMods)
      case "scala" => Generation.writeScala(outTest + "/scala", allUniverse, testMods)
      case _ => 0

    stepTime = System.currentTimeMillis() - stepStart
    println(s"  Generated $testFileCount test files.")
    println(s"  Time: ${Generation.formatTime(stepTime)}")
    println()

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

  /** Load a package's mainModules + evalLibModules from its manifest. */
  def loadPackageMain(root: String, pkg: String,
      schemaMap: Map[String, Type]): Seq[hydra.packaging.Module] =
    val pkgDir = packageMainDir(root, pkg)
    val mainNs = readManifestFieldOrEmpty(pkgDir, "mainModules")
    val evalNs = readManifestFieldOrEmpty(pkgDir, "evalLibModules")
    val allNs = mainNs ++ evalNs
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
