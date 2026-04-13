package hydra

import _root_.java.io.File

/**
 * Bootstrapping entry point: loads Hydra modules from JSON and generates
 * code for a target language. Demonstrates that Scala can independently
 * regenerate Hydra from a language-independent JSON representation.
 *
 * Usage:
 *   scala hydra.Bootstrap --target <haskell|java|python> --json-dir <path> [OPTIONS]
 *
 * Options:
 *   --output <dir>         Output base directory (default: /tmp/hydra-bootstrapping-demo)
 *   --include-coders       Also load and generate ext coder modules
 *   --include-tests        Also load and generate kernel test modules
 *   --ext-json-dir <dir>   Directory containing ext JSON modules (for --include-coders)
 *   --kernel-only          Only generate kernel modules (exclude hydra.*)
 *   --types-only           Only generate type-defining modules
 */
@main def bootstrap(args: String*): Unit =
  var target: Option[String] = None
  var jsonDir: Option[String] = None
  var extJsonDir: Option[String] = None
  var outBase = "/tmp/hydra-bootstrapping-demo"
  var includeCoders = false
  var includeTests = false
  var typesOnly = false
  var kernelOnly = false

  var i = 0
  while i < args.length do
    args(i) match
      case "--target" => i += 1; target = Some(args(i))
      case "--json-dir" => i += 1; jsonDir = Some(args(i))
      case "--ext-json-dir" => i += 1; extJsonDir = Some(args(i))
      case "--output" => i += 1; outBase = args(i)
      case "--include-coders" => includeCoders = true
      case "--include-tests" => includeTests = true
      case "--types-only" => typesOnly = true
      case "--kernel-only" => kernelOnly = true
      case other => System.err.println(s"Unknown option: $other")
    i += 1

  if target.isEmpty || jsonDir.isEmpty then
    println("Usage: scala hydra.Bootstrap --target <haskell|java|python> --json-dir <path> [OPTIONS]")
    println()
    println("Options:")
    println("  --output <dir>         Output base directory")
    println("  --include-coders       Also load and generate ext coder modules")
    println("  --include-tests        Also load and generate kernel test modules")
    println("  --ext-json-dir <dir>   Directory containing ext JSON modules (for --include-coders)")
    println("  --kernel-only          Only generate kernel modules (exclude hydra.*)")
    println("  --types-only           Only generate type-defining modules")
    System.exit(1)

  if includeCoders && extJsonDir.isEmpty then
    println("Error: --include-coders requires --ext-json-dir")
    System.exit(1)

  val tgt = target.get
  val jDir = jsonDir.get
  val targetCap = tgt.capitalize
  val outDir = outBase + File.separator + "scala-to-" + tgt

  println("==========================================")
  println(s"Mapping JSON to $targetCap (via Scala host)")
  println("==========================================")
  println(s"  Host language:   Scala")
  println(s"  Target language: $targetCap")
  println(s"  JSON directory:  $jDir")
  println(s"  Output:          $outDir")
  println(s"  Include coders:  $includeCoders")
  println(s"  Include tests:   $includeTests")
  if typesOnly then println("  Filter:          types only")
  if kernelOnly then println("  Filter:          kernel only")
  println("==========================================")
  println()

  val totalStart = System.currentTimeMillis()

  // Step 1: Build schema map
  println("Step 1: Building schema map...")
  var stepStart = System.currentTimeMillis()
  val schemaMap = Generation.bootstrapSchemaMap()
  println(s"  Schema map has ${schemaMap.size} types.")
  println(s"  Time: ${Generation.formatTime(System.currentTimeMillis() - stepStart)}")
  println()

  // Step 2: Load main + eval lib modules from JSON
  println("Step 2: Loading main modules from JSON...")
  println(s"  Source: $jDir")
  stepStart = System.currentTimeMillis()
  val mainNamespaces = Generation.readManifestField(jDir, "mainModules")
  val evalLibNamespaces = Generation.readManifestField(jDir, "evalLibModules")
  val allKernelNamespaces = mainNamespaces ++ evalLibNamespaces
  val mainMods = Generation.loadModulesFromJson(jDir, schemaMap, allKernelNamespaces)
  var stepTime = System.currentTimeMillis() - stepStart
  val totalDefs = mainMods.map(_.definitions.size).sum
  println(s"  Loaded ${mainMods.size} modules ($totalDefs definitions).")
  println(s"  Time: ${Generation.formatTime(stepTime)}")
  println()

  // Step 3: Optionally load ext coder modules
  var coderMods: Seq[hydra.packaging.Module] = Seq.empty
  if includeCoders then
    println("Step 3: Loading hydra-ext coder modules from JSON...")
    val coderNamespaces = Generation.readManifestField(extJsonDir.get, "hydraBootstrapCoderModules")
    val kernelNsSet = allKernelNamespaces.toSet
    val extCoderNamespaces = coderNamespaces.filterNot(kernelNsSet.contains)
    stepStart = System.currentTimeMillis()
    coderMods = Generation.loadModulesFromJson(extJsonDir.get, schemaMap, extCoderNamespaces)
    stepTime = System.currentTimeMillis() - stepStart
    println(s"  Loaded ${coderMods.size} modules.")
    println(s"  Time: ${Generation.formatTime(stepTime)}")
    println()
  else
    println("Step 3: Skipping ext coder modules")
    println()

  var allMainMods = mainMods ++ coderMods

  // Apply filters
  var modsToGenerate = allMainMods
  if kernelOnly then
    val before = modsToGenerate.size
    val kernelNsStrings = allKernelNamespaces.map(_.value).toSet
    modsToGenerate = modsToGenerate.filter(m => kernelNsStrings.contains(m.namespace.value))
    allMainMods = allMainMods.filter(m => kernelNsStrings.contains(m.namespace.value))
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

  // Optionally load and generate test modules
  var testFileCount = 0
  if includeTests then
    val testJsonDir = jDir.replace("src/main/json", "src/test/json")
    println("Loading test modules from JSON...")
    println(s"  Source: $testJsonDir")
    stepStart = System.currentTimeMillis()
    val testNamespaces = Generation.readManifestField(jDir, "testModules")
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
