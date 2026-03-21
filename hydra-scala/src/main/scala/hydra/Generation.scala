package hydra

import hydra.core.{Binding, Name, Term, TypeScheme}
import hydra.graph.{Graph, Primitive}
import hydra.json.model.Value
import hydra.module.{Definition, Module, Namespace}

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/**
 * I/O wrapper for Hydra code generation in Scala.
 * Provides file I/O around the pure Either-based functions in CodeGeneration.
 */
object Generation:

  /** Create an empty graph with standard primitives (the bootstrap graph). */
  def bootstrapGraph(): Graph =
    val primitives: Map[Name, Primitive] = hydra.lib.Libraries.standardPrimitives.map { prim =>
      prim.name -> prim.toNative
    }.toMap
    Graph(
      boundTerms = Map.empty,
      boundTypes = Map.empty,
      classConstraints = Map.empty,
      lambdaVariables = Set.empty,
      metadata = Map.empty,
      primitives = primitives,
      schemaTypes = Map.empty,
      typeVariables = Set.empty)

  /** Parse a JSON file into a Hydra JSON value. */
  def parseJsonFile(path: String): Value =
    val content = new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8)
    SimpleJsonParser(content).parseValue()

  /**
   * Create the schema map used for type-directed JSON decoding.
   * This is the subset of kernel types needed to decode modules from JSON.
   */
  def bootstrapSchemaMap(): Map[Name, hydra.core.Type] =
    hydra.schemas.Schemas.bootstrapSchemaMap

  /**
   * Load modules from a JSON directory using type-directed decoding.
   */
  def loadModulesFromJson(
      failOnError: Boolean,
      jsonDir: String,
      schemaMap: Map[Name, hydra.core.Type],
      namespaces: Seq[Namespace]): Seq[Module] =
    namespaces.flatMap { ns =>
      val filePath = jsonDir + File.separator +
        ns.value.replace('.', File.separatorChar) + ".json"
      val file = new File(filePath)
      if (!file.exists()) {
        if (failOnError) throw new RuntimeException(s"JSON file not found: $filePath")
        else { System.err.println(s"  Warning: skipping missing module: ${ns.value}"); Seq.empty }
      } else {
        try {
          val jsonValue = parseJsonFile(filePath)
          hydra.json.bootstrap.JsonBootstrap.decodeModule(schemaMap)(jsonValue) match {
            case Right(mod) => Seq(mod)
            case Left(err) =>
              if (failOnError) throw new RuntimeException(s"Failed to decode ${ns.value}: $err")
              else { System.err.println(s"  Warning: failed to decode ${ns.value}: $err"); Seq.empty }
          }
        } catch {
          case e: Exception =>
            if (failOnError) throw e
            else { System.err.println(s"  Warning: error loading ${ns.value}: ${e.getMessage}"); Seq.empty }
        }
      }
    }

  /** Read namespace list from a JSON manifest file. */
  def readManifestField(jsonDir: String, fieldName: String): Seq[Namespace] =
    val manifestPath = jsonDir + File.separator + "manifest.json"
    val manifestJson = parseJsonFile(manifestPath)
    manifestJson match {
      case Value.`object`(m) =>
        m.get(fieldName) match {
          case Some(Value.array(items)) =>
            items.collect { case Value.string(s) => Namespace(s) }
          case _ => throw new RuntimeException(s"manifest field '$fieldName' not found or not an array")
        }
      case _ => throw new RuntimeException("manifest.json is not an object")
    }

  /** Filter to kernel-only modules (exclude hydra.ext.*) */
  def filterKernelModules(mods: Seq[Module]): Seq[Module] =
    mods.filter(m => !m.namespace.value.startsWith("hydra.ext."))

  /** Filter to type-only modules */
  def filterTypeModules(mods: Seq[Module]): Seq[Module] =
    mods.filter(m => hydra.annotations.Annotations.isNativeType(m.elements.headOption.getOrElse(
      return Seq.empty)))
    mods // TODO: proper type module detection

  /** Generate source files and write them to disk. */
  def generateSources(
      coder: Module => Seq[Definition] => hydra.context.Context => Graph => Either[hydra.context.InContext[hydra.error.Error], Map[String, String]],
      language: hydra.coders.Language,
      doInfer: Boolean,
      doExpand: Boolean,
      doHoistCaseStatements: Boolean,
      doHoistPolymorphicLetBindings: Boolean,
      basePath: String,
      universe: Seq[Module],
      modsToGenerate: Seq[Module]): Int =
    val cx = hydra.context.Context(Seq.empty, Seq.empty, Map.empty)
    val bsGraph = bootstrapGraph()
    hydra.codeGeneration.CodeGeneration.generateSourceFiles(
      coder)(language)(doInfer)(doExpand)(doHoistCaseStatements)(doHoistPolymorphicLetBindings)(
      bsGraph)(universe)(modsToGenerate)(cx) match {
      case Left(ic) =>
        throw new RuntimeException(s"Code generation failed: ${ic.`object`}")
      case Right(pairs) =>
        var count = 0
        for ((filePath, content) <- pairs) {
          val fullPath = Paths.get(basePath, filePath)
          Files.createDirectories(fullPath.getParent)
          Files.write(fullPath, content.getBytes(StandardCharsets.UTF_8))
          count += 1
        }
        count
    }

  /** Generate Java source files from modules. */
  def writeJava(basePath: String, universe: Seq[Module], mods: Seq[Module]): Int =
    generateSources(
      mod => defs => cx => g => hydra.ext.java.coder.Coder.moduleToJava(mod)(defs)(cx)(g),
      hydra.ext.java.language.Language.javaLanguage,
      doInfer = false, doExpand = true, doHoistCaseStatements = false, doHoistPolymorphicLetBindings = true,
      basePath, universe, mods)

  /** Generate Python source files from modules. */
  def writePython(basePath: String, universe: Seq[Module], mods: Seq[Module]): Int =
    generateSources(
      mod => defs => cx => g => hydra.ext.python.coder.Coder.moduleToPython(mod)(defs)(cx)(g),
      hydra.ext.python.language.Language.pythonLanguage,
      doInfer = false, doExpand = true, doHoistCaseStatements = true, doHoistPolymorphicLetBindings = false,
      basePath, universe, mods)

  /** Generate Haskell source files from modules. */
  def writeHaskell(basePath: String, universe: Seq[Module], mods: Seq[Module]): Int =
    generateSources(
      mod => defs => cx => g => hydra.ext.haskell.coder.Coder.moduleToHaskell(mod)(defs)(cx)(g),
      hydra.ext.haskell.language.Language.haskellLanguage,
      doInfer = false, doExpand = false, doHoistCaseStatements = false, doHoistPolymorphicLetBindings = false,
      basePath, universe, mods)

  /** Format elapsed time for display. */
  def formatTime(millis: Long): String =
    if millis < 1000 then s"${millis}ms"
    else if millis < 60000 then f"${millis / 1000.0}%.1fs"
    else s"${millis / 60000}m ${(millis % 60000) / 1000}s"

  /**
   * Simple recursive descent JSON parser that produces hydra.json.model.Value objects.
   */
  private class SimpleJsonParser(input: String):
    private var pos = 0

    def parseValue(): Value =
      skipWhitespace()
      if pos >= input.length then throw new RuntimeException("Unexpected end of JSON")
      input.charAt(pos) match
        case '{' => parseObject()
        case '[' => parseArray()
        case '"' => Value.string(parseRawString())
        case 't' | 'f' => parseBoolean()
        case 'n' => parseNull()
        case c if c == '-' || c.isDigit => parseNumber()
        case c => throw new RuntimeException(s"Unexpected character '$c' at position $pos")

    private def parseObject(): Value =
      expect('{')
      skipWhitespace()
      var map = Map.empty[String, Value]
      if pos < input.length && input.charAt(pos) != '}' then
        map = parseKeyValue(map)
        while pos < input.length && input.charAt(pos) == ',' do
          pos += 1
          map = parseKeyValue(map)
      expect('}')
      Value.`object`(map)

    private def parseKeyValue(map: Map[String, Value]): Map[String, Value] =
      skipWhitespace()
      val key = parseRawString()
      skipWhitespace()
      expect(':')
      val v = parseValue()
      map + (key -> v)

    private def parseArray(): Value =
      expect('[')
      skipWhitespace()
      var list = Seq.empty[Value]
      if pos < input.length && input.charAt(pos) != ']' then
        list = list :+ parseValue()
        while pos < input.length && input.charAt(pos) == ',' do
          pos += 1
          list = list :+ parseValue()
      expect(']')
      Value.array(list)

    private def parseBoolean(): Value =
      if input.startsWith("true", pos) then { pos += 4; Value.boolean(true) }
      else if input.startsWith("false", pos) then { pos += 5; Value.boolean(false) }
      else throw new RuntimeException(s"Expected boolean at position $pos")

    private def parseNull(): Value =
      if input.startsWith("null", pos) then { pos += 4; Value.array(Seq.empty) } // null -> empty array
      else throw new RuntimeException(s"Expected null at position $pos")

    private def parseNumber(): Value =
      val start = pos
      if pos < input.length && input.charAt(pos) == '-' then pos += 1
      while pos < input.length && input.charAt(pos).isDigit do pos += 1
      if pos < input.length && input.charAt(pos) == '.' then
        pos += 1
        while pos < input.length && input.charAt(pos).isDigit do pos += 1
      if pos < input.length && (input.charAt(pos) == 'e' || input.charAt(pos) == 'E') then
        pos += 1
        if pos < input.length && (input.charAt(pos) == '+' || input.charAt(pos) == '-') then pos += 1
        while pos < input.length && input.charAt(pos).isDigit do pos += 1
      val numStr = input.substring(start, pos)
      Value.number(BigDecimal(numStr))

    private def parseRawString(): String =
      expect('"')
      val sb = new StringBuilder
      while pos < input.length && input.charAt(pos) != '"' do
        val c = input.charAt(pos)
        if c == '\\' then
          pos += 1
          if pos >= input.length then throw new RuntimeException("Unexpected end of string escape")
          input.charAt(pos) match
            case '"' => sb.append('"')
            case '\\' => sb.append('\\')
            case '/' => sb.append('/')
            case 'b' => sb.append('\b')
            case 'f' => sb.append('\f')
            case 'n' => sb.append('\n')
            case 'r' => sb.append('\r')
            case 't' => sb.append('\t')
            case 'u' =>
              pos += 1
              val hex = input.substring(pos, pos + 4)
              sb.append(Integer.parseInt(hex, 16).toChar)
              pos += 3 // will be incremented below
            case other => sb.append(other)
        else sb.append(c)
        pos += 1
      expect('"')
      sb.toString

    private def expect(c: Char): Unit =
      skipWhitespace()
      if pos >= input.length || input.charAt(pos) != c then
        throw new RuntimeException(s"Expected '$c' at position $pos")
      pos += 1

    private def skipWhitespace(): Unit =
      while pos < input.length && input.charAt(pos).isWhitespace do pos += 1
