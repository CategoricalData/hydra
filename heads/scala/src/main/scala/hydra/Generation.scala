package hydra

import hydra.core.*
import hydra.graph.{Graph, Primitive}
import hydra.json.model.Value
import hydra.packaging.{Module, Namespace, Definition}

import _root_.java.io.File
import _root_.java.nio.charset.StandardCharsets
import _root_.java.nio.file.{Files, Paths}

/**
 * I/O wrapper for Hydra code generation in Scala.
 * Provides file I/O around the pure Either-based functions in CodeGeneration.
 */
object Generation:

  /** Create an empty graph with standard primitives (the bootstrap graph). */
  def bootstrapGraph(): Graph =
    val primitives: Map[String, Primitive] = hydra.lib.Libraries.standardPrimitives()
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
   * Build a schema map for type-directed JSON decoding from the bootstrap type map.
   * Converts System F types (with foralls and annotations) to plain types for JSON decoding.
   */
  def bootstrapSchemaMap(): Map[String, Type] =
    hydra.json.bootstrap.typesByName.map { (name, typ) =>
      val ts = hydra.scoping.fTypeToTypeScheme(typ)
      name -> hydra.strip.deannotateTypeRecursive(ts.`type`)
    }

  /**
   * Decode a single module from a JSON value.
   */
  /**
   * Decode binary literals from base64. JSON stores binary as base64 strings.
   * The generic decode/core module extracts the raw string without decoding.
   * This post-processor walks terms to apply stringToBinary to binary literals.
   */
  private def decodeBinaryLiterals(term: Term): Term =
    decodeBinaryLiteralsDepth(term, 100)

  private def decodeBinaryLiteralsDepth(term: Term, depth: Int): Term =
    if depth <= 0 then return term
    val d = depth - 1
    term match
      case Term.literal(Literal.binary(b)) =>
        Term.literal(Literal.binary(hydra.lib.literals.stringToBinary(b)))
      case Term.application(app) =>
        Term.application(Application(decodeBinaryLiteralsDepth(app.function, d), decodeBinaryLiteralsDepth(app.argument, d)))
      case Term.lambda(lam) =>
        Term.lambda(Lambda(lam.parameter, lam.domain, decodeBinaryLiteralsDepth(lam.body, d)))
      case Term.let(lt) =>
        Term.let(Let(lt.bindings.map(b => Binding(b.name, decodeBinaryLiteralsDepth(b.term, d), b.`type`)),
          decodeBinaryLiteralsDepth(lt.body, d)))
      case Term.list(elems) => Term.list(elems.map(e => decodeBinaryLiteralsDepth(e, d)))
      case Term.record(rec) =>
        Term.record(Record(rec.typeName, rec.fields.map(f => Field(f.name, decodeBinaryLiteralsDepth(f.term, d)))))
      case Term.inject(inj) =>
        Term.inject(Injection(inj.typeName, Field(inj.field.name, decodeBinaryLiteralsDepth(inj.field.term, d))))
      case Term.annotated(ann) =>
        Term.annotated(AnnotatedTerm(decodeBinaryLiteralsDepth(ann.body, d), ann.annotation))
      case Term.wrap(w) =>
        Term.wrap(WrappedTerm(w.typeName, decodeBinaryLiteralsDepth(w.body, d)))
      case other => other

  private def decodeBinaryInModule(mod: Module): Module =
    Module(mod.description, mod.namespace, mod.termDependencies, mod.typeDependencies, mod.definitions.map {
      case Definition.term(td) =>
        Definition.term(hydra.packaging.TermDefinition(td.name, decodeBinaryLiterals(td.term), td.`type`))
      case other => other
    })

  def decodeModuleFromJson(bsGraph: Graph, schemaMap: Map[String, Type], jsonVal: Value): Module =
    val modName: String = "hydra.packaging.Module"
    val modType: Type = Type.variable(modName)
    hydra.json.decode.fromJson(schemaMap)(modName)(modType)(jsonVal) match
      case Left(err) => throw new RuntimeException(s"JSON decode error: $err")
      case Right(term) =>
        hydra.decode.packaging.module(bsGraph)(term) match
          case Left(err) => throw new RuntimeException(s"Module decode error: $err")
          case Right(mod) => mod

  /**
   * Load modules from a JSON directory using type-directed decoding.
   */
  def loadModulesFromJson(jsonDir: String, schemaMap: Map[String, Type],
      namespaces: Seq[String]): Seq[Module] =
    val bsGraph = bootstrapGraph()
    namespaces.flatMap { ns =>
      val filePath = jsonDir + File.separator +
        hydra.codegen.namespaceToPath(ns) + ".json"
      val file = new File(filePath)
      if !file.exists() then
        throw new RuntimeException(s"Missing module file: $filePath")
      else
        val jsonValue = parseJsonFile(filePath)
        val mod = decodeModuleFromJson(bsGraph, schemaMap, jsonValue)
        println(s"  Loaded: $ns (${mod.definitions.size} definitions)")
        Seq(mod)
    }

  /** Read namespace list from a JSON manifest file. */
  def readManifestField(jsonDir: String, fieldName: String): Seq[String] =
    val manifestPath = jsonDir + File.separator + "manifest.json"
    val manifestJson = parseJsonFile(manifestPath)
    manifestJson match
      case Value.`object`(m) =>
        m.get(fieldName) match
          case Some(Value.array(items)) =>
            items.collect { case Value.string(s) => s }
          case _ => throw new RuntimeException(s"manifest field '$fieldName' not found or not an array")
      case _ => throw new RuntimeException("manifest.json is not an object")

  /** Filter to kernel-only modules (exclude hydra.*) */
  def filterKernelModules(mods: Seq[Module]): Seq[Module] =
    mods.filter(m => !m.namespace.startsWith("hydra.") && !m.namespace.startsWith("hydra.json.yaml."))

  /** Generate source files and write them to disk. Returns number of files written. */
  def generateSources(
      coder: Module => Seq[Definition] => hydra.context.Context => Graph =>
        Either[hydra.errors.Error, Map[String, String]],
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
    hydra.codegen.generateSourceFiles(
      coder)(language)(doInfer)(doExpand)(doHoistCaseStatements)(doHoistPolymorphicLetBindings)(
      bsGraph)(universe)(modsToGenerate)(cx) match
      case Left(err) =>
        throw new RuntimeException(s"Code generation failed: $err")
      case Right(pairs) =>
        var count = 0
        for (filePath, content) <- pairs do
          val fullPath = Paths.get(basePath, filePath)
          Files.createDirectories(fullPath.getParent)
          Files.write(fullPath, content.getBytes(StandardCharsets.UTF_8))
          count += 1
        count

  /** Generate Java source files from modules. */
  def writeJava(basePath: String, universe: Seq[Module], mods: Seq[Module]): Int =
    generateSources(
      mod => defs => cx => g => hydra.java.coder.moduleToJava(mod)(defs)(cx)(g),
      hydra.java.language.javaLanguage,
      doInfer = false, doExpand = true, doHoistCaseStatements = false, doHoistPolymorphicLetBindings = true,
      basePath, universe, mods)

  /** Generate Python source files from modules. */
  def writePython(basePath: String, universe: Seq[Module], mods: Seq[Module]): Int =
    generateSources(
      mod => defs => cx => g => hydra.python.coder.moduleToPython(mod)(defs)(cx)(g),
      hydra.python.language.pythonLanguage,
      doInfer = false, doExpand = true, doHoistCaseStatements = true, doHoistPolymorphicLetBindings = false,
      basePath, universe, mods)

  /** Generate Scala source files from modules. */
  def writeScala(basePath: String, universe: Seq[Module], mods: Seq[Module]): Int =
    generateSources(
      mod => defs => cx => g => hydra.scala.coder.moduleToScala(mod)(defs)(cx)(g),
      hydra.scala.language.scalaLanguage,
      doInfer = false, doExpand = true, doHoistCaseStatements = false, doHoistPolymorphicLetBindings = false,
      basePath, universe, mods)

  /** Generate Haskell source files from modules. */
  def writeHaskell(basePath: String, universe: Seq[Module], mods: Seq[Module]): Int =
    generateSources(
      mod => defs => cx => g => hydra.haskell.coder.moduleToHaskell(mod)(defs)(cx)(g),
      hydra.haskell.language.haskellLanguage,
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
      if input.startsWith("null", pos) then { pos += 4; Value.`null` }
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
      Value.number(BigDecimal(input.substring(start, pos)))

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
              pos += 3
            case other => sb.append(other)
        else sb.append(c)
        pos += 1
      expect('"')
      sb.toString

    private def expect(c: Char): Unit =
      skipWhitespace()
      if pos >= input.length || input.charAt(pos) != c then
        throw new RuntimeException(s"Expected '$c' at position $pos, got '${if pos < input.length then input.charAt(pos) else "EOF"}'")
      pos += 1

    private def skipWhitespace(): Unit =
      while pos < input.length && input.charAt(pos).isWhitespace do pos += 1
