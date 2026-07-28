package hydra

import hydra.testing.*

/**
 * Shared, non-DSL-registration logic for Hydra ScalaTest test-suite runners: skip-tag
 * checking, the effectful-test temp dir, and benchmark JSON rendering. Shared by
 * TestSuiteRunner (hydra-kernel) and BuildTestSuiteRunner (hydra-build) — #547.
 *
 * `test(...)` registration itself stays per-class: ScalaTest's AnyFunSuite registers
 * tests as a side effect of trait construction, so it does not compose cleanly as a
 * returned value the way the Java/Python walkers do.
 */
trait HydraTestGroupSupport {

  protected def shouldSkip(tc: TestCaseWithMetadata): Boolean =
    tc.tags.contains("disabled") || tc.tags.contains("disabledForPython") || tc.tags.contains("disabledForScala")

  // Canonical root directory for effectful (file I/O) test cases. Must match the testDir
  // constant in Hydra.Sources.Test.Lib.Files and the per-host runners (Haskell, Java).
  // Hard-coded *nix path for now (configurable later, #494).
  protected val EffectfulTestDir: String = "/tmp/hydra-testing"

  protected def prepareEffectfulTempDir(): Unit = {
    val dir = _root_.java.nio.file.Paths.get(EffectfulTestDir)
    if (_root_.java.nio.file.Files.exists(dir)) {
      val walk = _root_.java.nio.file.Files.walk(dir)
      try {
        walk.sorted(_root_.java.util.Comparator.reverseOrder())
          .forEach(p => _root_.java.nio.file.Files.delete(p))
      } finally walk.close()
    }
    _root_.java.nio.file.Files.createDirectories(dir)
  }

  // ---- Benchmark JSON writer (matches the JSON shape used by other heads) ----

  protected def writeBenchmarkJson(outputPath: String, allTests: TestGroup, benchmarkResults: _root_.scala.collection.Map[String, Double]): Unit = {
    val json = buildBenchmarkJson(allTests, benchmarkResults)
    val writer = new _root_.java.io.FileWriter(outputPath)
    try writer.write(json)
    finally writer.close()
    println("Benchmark results written to " + outputPath)
  }

  private def buildBenchmarkJson(root: TestGroup, benchmarkResults: _root_.scala.collection.Map[String, Double]): String = {
    val sb = new StringBuilder
    sb.append("{\n")
    sb.append("  \"metadata\": {\n")
    sb.append("    \"language\": \"scala\"\n")
    sb.append("  },\n")
    sb.append("  \"groups\": [\n")
    sb.append(renderGroup("    ", root.name, root, benchmarkResults))
    sb.append("\n  ],\n")
    val (passed, skipped) = countCases(root)
    val totalTime = benchmarkResults.getOrElse(root.name, 0.0)
    sb.append("  \"summary\": {\n")
    sb.append(s"""    "totalPassed": $passed,\n""")
    sb.append("    \"totalFailed\": 0,\n")
    sb.append(s"""    "totalSkipped": $skipped,\n""")
    sb.append(s"""    "totalTimeMs": $totalTime\n""")
    sb.append("  }\n")
    sb.append("}\n")
    sb.toString
  }

  private def renderGroup(indent: String, path: String, g: TestGroup, benchmarkResults: _root_.scala.collection.Map[String, Double]): String = {
    val sb = new StringBuilder
    val timeMs = benchmarkResults.getOrElse(path, 0.0)
    sb.append(indent).append("{\n")
    sb.append(indent).append("  \"name\": ").append("\"").append(g.name).append("\",\n")
    sb.append(indent).append("  \"time_ms\": ").append(timeMs).append(",\n")
    sb.append(indent).append("  \"subgroups\": [")
    if (g.subgroups.isEmpty) {
      sb.append("]")
    } else {
      sb.append("\n")
      val parts = g.subgroups.map { sub =>
        renderGroup(indent + "    ", path + "/" + sub.name, sub, benchmarkResults)
      }
      sb.append(parts.mkString(",\n"))
      sb.append("\n").append(indent).append("  ]")
    }
    sb.append("\n").append(indent).append("}")
    sb.toString
  }

  protected def countCases(g: TestGroup): (Int, Int) = {
    var passed = 0
    var skipped = 0
    for (c <- g.cases) {
      if (shouldSkip(c)) skipped += 1
      else passed += 1
    }
    for (sub <- g.subgroups) {
      val (p, s) = countCases(sub)
      passed += p
      skipped += s
    }
    (passed, skipped)
  }
}
