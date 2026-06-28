package hydra.sources.scala

import hydra.overlay.scala.dsl.{Helpers, Phantoms}
import hydra.overlay.scala.dsl.Phantoms.{`var` => v, applyP, lambda, let, field, string, int32, bool, list, nothing, just, doc, constant, cases, casesWithDefault, project, unwrap, wrap, inject}
import hydra.packaging.{Definition, EntityMetadata, Module, ModuleName}
import hydra.typed.TypedTerm
import hydra.core.{Field, Injection, Literal, Term, WrappedTerm}

import hydra.dsl.scala.syntax as ScalaSyntax
import hydra.dsl.{ast => AstDsl, core => CoreDsl, errors => ErrorsDsl, graph => GraphDsl, packaging => PackagingDsl, typing => TypingDsl, util => UtilDsl}

/**
 * Scala code generator: converts Hydra modules to Scala source code.
 *
 * STATUS (2026-06-27, #509): translation in progress. Simpler "extract"/"to*"
 * helpers translated; complex encode* functions still TBD.
 */
object Coder:

  val NS: ModuleName = "hydra.scala.coder"

  /** Dependencies — matches Haskell Coder.hs line 75-76 exactly. */
  private val DEPS: Seq[ModuleName] = Seq(
    "hydra.scala.utils",
    "hydra.scala.serde",
    "hydra.formatting",
    "hydra.names",
    "hydra.scoping",
    "hydra.strip",
    "hydra.variables",
    "hydra.analysis",
    "hydra.environment",
    "hydra.predicates",
    "hydra.resolution",
    "hydra.show.core",
    "hydra.annotations",
    "hydra.constants",
    "hydra.inference",
    "hydra.sorting",
    "hydra.arity",
    "hydra.serialization",
    "hydra.reduction",
    "hydra.scala.syntax",
    "hydra.scala.language",
    "hydra.paths",
    "hydra.ast", "hydra.coders", "hydra.core",
    "hydra.error.checking", "hydra.error.core", "hydra.error.file",
    "hydra.error.packaging", "hydra.error.system",
    "hydra.errors", "hydra.file", "hydra.graph", "hydra.json.model",
    "hydra.packaging", "hydra.parsing",
    "hydra.query", "hydra.relational", "hydra.system", "hydra.tabular",
    "hydra.testing", "hydra.time", "hydra.topology", "hydra.typed",
    "hydra.typing", "hydra.util", "hydra.validation", "hydra.variants")

  // ===== Local helpers =====

  private def cat2(a: TypedTerm[String], b: TypedTerm[String]): TypedTerm[String] =
    applyP("hydra.lib.strings.cat2", a, b)

  private val emptyList: TypedTerm[Any] = list[Any]()

  private def stref(s: TypedTerm[String]): TypedTerm[Any] =
    inject("hydra.scala.syntax.Type", "ref",
      inject("hydra.scala.syntax.RefType", "name",
        ScalaSyntax.nameType(s)))

  private def errorOther(msg: TypedTerm[String]): TypedTerm[Any] =
    applyP("hydra.dsl.errors.errorOther",
      applyP("hydra.dsl.errors.otherError", msg))

  /** Apply a TypedTerm function to N curried arguments (term-level apply chain). */
  private def applyN(fn: TypedTerm[Any], args: TypedTerm[Any]*): TypedTerm[Any] =
    args.foldLeft[TypedTerm[Any]](fn)((acc, a) => Phantoms.apply(acc, a))

  /** errorLeft builds `Left(Error.other(OtherError("msg")))` directly as kernel Terms,
   *  avoiding the inference engine re-typing each layer. Error is a union (inject),
   *  OtherError is a wrap (wrap). */
  private def errorLeft(msg: String): TypedTerm[Any] =
    Term.either(scala.util.Left[Term, Term](
      Term.inject(Injection("hydra.errors.Error",
        Field("other",
          Term.wrap(WrappedTerm("hydra.errors.OtherError",
            Term.literal(Literal.string(msg))))))))).asInstanceOf[TypedTerm[Any]]

  // mkLazyVal helper
  private def mkLazyVal(vname: TypedTerm[String], mdecltpe: TypedTerm[Any], rhs: TypedTerm[Any]): TypedTerm[Any] =
    inject("hydra.scala.syntax.Stat", "defn",
      inject("hydra.scala.syntax.Defn", "val",
        ScalaSyntax.valDefn(list(inject("hydra.scala.syntax.Mod", "lazy", Phantoms.unit)))(list(inject("hydra.scala.syntax.Pat", "var",
            ScalaSyntax.varPat(ScalaSyntax.nameData(ScalaSyntax.predefString(vname))))))(mdecltpe)(rhs)))

  // ===== Simple functions (no complex nesting) =====

  // dropDomains
  private val dropDomainsBody = lambda("n", lambda("t",
    applyP("hydra.lib.logic.ifElse",
      applyP("hydra.lib.equality.lte", v("n"), int32(0)),
      v("t"),
      casesWithDefault("hydra.core.Type",
        applyP("hydra.strip.deannotateType", v("t")),
        v("t"),
        field("function", lambda("ft",
          applyP("hydra.scala.coder.dropDomains",
            applyP("hydra.lib.math.sub", v("n"), int32(1)),
            CoreDsl.functionTypeCodomain(v("ft"))))),
        field("forall", lambda("fa",
          applyP("hydra.scala.coder.dropDomains",
            v("n"),
            CoreDsl.forallTypeBody(v("fa")))))))))

  lazy val dropDomainsDef: Definition =
    Phantoms.`def`(NS, "dropDomains",
      doc("Drop N domain types from a function type, returning the remaining type", dropDomainsBody))

  // extractBody
  private val extractBodyBody = lambda("t",
    casesWithDefault("hydra.core.Term",
      applyP("hydra.strip.deannotateAndDetypeTerm", v("t")),
      v("t"),
      field("lambda", lambda("lam",
        applyP("hydra.scala.coder.extractBody", CoreDsl.lambdaBody(v("lam"))))),
      field("typeLambda", lambda("tl",
        applyP("hydra.scala.coder.extractBody", CoreDsl.typeLambdaBody(v("tl"))))),
      field("typeApplication", lambda("ta",
        applyP("hydra.scala.coder.extractBody", CoreDsl.typeApplicationTermBody(v("ta"))))),
      field("let", lambda("lt",
        applyP("hydra.scala.coder.extractBody", CoreDsl.letBody(v("lt")))))))

  lazy val extractBodyDef: Definition =
    Phantoms.`def`(NS, "extractBody",
      doc("Extract the innermost body from a term", extractBodyBody))

  // extractCodomain
  private val extractCodomainBody = lambda("t",
    casesWithDefault("hydra.core.Type",
      applyP("hydra.strip.deannotateType", v("t")),
      v("t"),
      field("function", lambda("ft",
        applyP("hydra.scala.coder.extractCodomain", CoreDsl.functionTypeCodomain(v("ft"))))),
      field("forall", lambda("fa",
        applyP("hydra.scala.coder.extractCodomain", CoreDsl.forallTypeBody(v("fa")))))))

  lazy val extractCodomainDef: Definition =
    Phantoms.`def`(NS, "extractCodomain",
      doc("Extract the final return type from a function type", extractCodomainBody))

  // extractDomains
  private val extractDomainsBody = lambda("t",
    casesWithDefault("hydra.core.Type",
      applyP("hydra.strip.deannotateType", v("t")),
      emptyList,
      field("function", lambda("ft",
        applyP("hydra.lib.lists.cons",
          CoreDsl.functionTypeDomain(v("ft")),
          applyP("hydra.scala.coder.extractDomains",
            CoreDsl.functionTypeCodomain(v("ft")))))),
      field("forall", lambda("fa",
        applyP("hydra.scala.coder.extractDomains", CoreDsl.forallTypeBody(v("fa")))))))

  lazy val extractDomainsDef: Definition =
    Phantoms.`def`(NS, "extractDomains",
      doc("Extract domain types from a function type", extractDomainsBody))

  // extractLetBindings
  private val extractLetBindingsBody = lambda("t",
    casesWithDefault("hydra.core.Term",
      applyP("hydra.strip.deannotateAndDetypeTerm", v("t")),
      emptyList,
      field("lambda", lambda("lam",
        applyP("hydra.scala.coder.extractLetBindings", CoreDsl.lambdaBody(v("lam"))))),
      field("typeLambda", lambda("tl",
        applyP("hydra.scala.coder.extractLetBindings", CoreDsl.typeLambdaBody(v("tl"))))),
      field("typeApplication", lambda("ta",
        applyP("hydra.scala.coder.extractLetBindings", CoreDsl.typeApplicationTermBody(v("ta"))))),
      field("let", lambda("lt",
        applyP("hydra.lib.lists.concat2",
          CoreDsl.letBindings(v("lt")),
          applyP("hydra.scala.coder.extractLetBindings", CoreDsl.letBody(v("lt"))))))))

  lazy val extractLetBindingsDef: Definition =
    Phantoms.`def`(NS, "extractLetBindings",
      doc("Extract let bindings from a term", extractLetBindingsBody))

  // extractParams
  private val extractParamsBody = lambda("t",
    casesWithDefault("hydra.core.Term",
      applyP("hydra.strip.deannotateAndDetypeTerm", v("t")),
      emptyList,
      field("lambda", lambda("lam",
        applyP("hydra.lib.lists.cons",
          CoreDsl.lambdaParameter(v("lam")),
          applyP("hydra.scala.coder.extractParams", CoreDsl.lambdaBody(v("lam")))))),
      field("typeLambda", lambda("tl",
        applyP("hydra.scala.coder.extractParams", CoreDsl.typeLambdaBody(v("tl"))))),
      field("typeApplication", lambda("ta",
        applyP("hydra.scala.coder.extractParams", CoreDsl.typeApplicationTermBody(v("ta"))))),
      field("let", lambda("lt",
        applyP("hydra.scala.coder.extractParams", CoreDsl.letBody(v("lt")))))))

  lazy val extractParamsDef: Definition =
    Phantoms.`def`(NS, "extractParams",
      doc("Extract parameter names from a term", extractParamsBody))

  // typeParamToTypeVar
  private val typeParamToTypeVarBody = lambda("tp",
    let(Seq(
      field("n", ScalaSyntax.paramTypeName(v("tp"))),
      field("s",
        casesWithDefault("hydra.scala.syntax.Name",
          v("n"), string(""),
          field("value", lambda("v", v("v")))))),
      inject("hydra.scala.syntax.Type", "var",
        ScalaSyntax.varType(ScalaSyntax.nameType(v("s"))))))

  lazy val typeParamToTypeVarDef: Definition =
    Phantoms.`def`(NS, "typeParamToTypeVar",
      doc("Convert a type parameter to a type variable reference", typeParamToTypeVarBody))

  // toElImport
  private val toElImportBody = lambda("ns",
    inject("hydra.scala.syntax.Stat", "importExport",
      inject("hydra.scala.syntax.ImportExportStat", "import",
        ScalaSyntax.`import`(list(ScalaSyntax.importer(inject("hydra.scala.syntax.RefData", "name",
              ScalaSyntax.nameData(ScalaSyntax.predefString(applyP("hydra.lib.strings.intercalate",
                    string("."),
                    applyP("hydra.lib.strings.splitOn",
                      string("."),
                      PackagingDsl.unModuleName(v("ns"))))))))(list(inject("hydra.scala.syntax.Importee", "wildcard", Phantoms.unit))))))))

  lazy val toElImportDef: Definition =
    Phantoms.`def`(NS, "toElImport",
      doc("Create an element import statement", toElImportBody))

  // toPrimImport
  private val toPrimImportBody = lambda("ns",
    inject("hydra.scala.syntax.Stat", "importExport",
      inject("hydra.scala.syntax.ImportExportStat", "import",
        ScalaSyntax.`import`(list(ScalaSyntax.importer(inject("hydra.scala.syntax.RefData", "name",
              ScalaSyntax.nameData(ScalaSyntax.predefString(applyP("hydra.lib.strings.intercalate",
                    string("."),
                    applyP("hydra.lib.strings.splitOn",
                      string("."),
                      PackagingDsl.unModuleName(v("ns"))))))))(emptyList))))))

  lazy val toPrimImportDef: Definition =
    Phantoms.`def`(NS, "toPrimImport",
      doc("Create a primitive import statement", toPrimImportBody))

  // ===== Mid-complexity definitions =====

  // encodeTypedParam
  private val encodeTypedParamBody = lambda("cx", lambda("g", lambda("pair",
    let(Seq(
      field("pname",
        applyP("hydra.scala.utils.scalaEscapeName",
          applyP("hydra.names.localNameOf",
            applyP("hydra.lib.pairs.first", v("pair"))))),
      field("pdom", applyP("hydra.lib.pairs.second", v("pair")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("pdom")),
        lambda("sdom",
          Phantoms.right(
            ScalaSyntax.paramData(emptyList)(inject("hydra.scala.syntax.Name", "value", v("pname")))(just(v("sdom")))(nothing))))))))

  lazy val encodeTypedParamDef: Definition =
    Phantoms.`def`(NS, "encodeTypedParam",
      doc("Encode a parameter with its type annotation", encodeTypedParamBody))

  // fieldToParam
  private val fieldToParamBody = lambda("cx", lambda("g", lambda("ft",
    let(Seq(
      field("fname",
        applyP("hydra.scala.utils.scalaEscapeName",
          CoreDsl.unName(CoreDsl.fieldTypeName(v("ft"))))),
      field("ftyp", CoreDsl.fieldTypeType(v("ft")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("ftyp")),
        lambda("sftyp",
          Phantoms.right(
            ScalaSyntax.paramData(emptyList)(inject("hydra.scala.syntax.Name", "value", v("fname")))(just(v("sftyp")))(nothing))))))))

  lazy val fieldToParamDef: Definition =
    Phantoms.`def`(NS, "fieldToParam",
      doc("Convert a field type to a Scala parameter", fieldToParamBody))

  // encodeUntypeApplicationTerm
  private val encodeUntypeApplicationTermBody = lambda("cx", lambda("g", lambda("term",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.inference.inferInGraphContext", v("cx"), v("g"), v("term")),
      lambda("result",
        applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"),
          TypingDsl.inferenceResultTerm(v("result"))))))))

  lazy val encodeUntypeApplicationTermDef: Definition =
    Phantoms.`def`(NS, "encodeUntypeApplicationTerm",
      doc("Encode an untyped application term by first inferring types", encodeUntypeApplicationTermBody))

  // applyVar — see Coder.hs:112-123
  // Default arm uses Phantoms.apply (direct Term.application) to dodge an
  // inference confusion with the curried CoreDsl.application chain.
  private val applyVarBody = lambda("fterm", lambda("avar",
    let(Seq(field("v", CoreDsl.unName(v("avar")))),
      casesWithDefault("hydra.core.Term",
        applyP("hydra.strip.deannotateAndDetypeTerm", v("fterm")),
        CoreDsl.termApplication(CoreDsl.application(v("fterm"))(CoreDsl.termVariable(v("avar")))),
        field("lambda", lambda("lam",
          let(Seq(
            field("lamParam", CoreDsl.lambdaParameter(v("lam"))),
            field("lamBody", CoreDsl.lambdaBody(v("lam")))),
            applyP("hydra.lib.logic.ifElse",
              applyP("hydra.variables.isFreeVariableInTerm", v("lamParam"), v("lamBody")),
              v("lamBody"),
              applyP("hydra.variables.substituteVariable",
                v("lamParam"), v("avar"), v("lamBody"))))))))))

  lazy val applyVarDef: Definition =
    Phantoms.`def`(NS, "applyVar",
      doc("Apply a variable to a term, performing substitution for lambdas", applyVarBody))

  // stripWrapEliminations — see Coder.hs:1329-1352
  // unwrap(value) → value;  unwrap(x)(arg) → x(arg)
  private val stripWrapInnerArm = lambda("innerApp",
    let(Seq(
      field("innerFun", CoreDsl.applicationFunction(v("innerApp"))),
      field("innerArg", CoreDsl.applicationArgument(v("innerApp")))),
      casesWithDefault("hydra.core.Term",
        applyP("hydra.strip.deannotateAndDetypeTerm", v("innerFun")),
        v("t"),
        field("unwrap", constant(
          applyP("hydra.scala.coder.stripWrapEliminations",
            CoreDsl.termApplication(CoreDsl.application(v("innerArg"))(v("appArg")))))))))

  private val stripWrapApplicationArm = lambda("app",
    let(Seq(
      field("appFun", CoreDsl.applicationFunction(v("app"))),
      field("appArg", CoreDsl.applicationArgument(v("app")))),
      casesWithDefault("hydra.core.Term",
        applyP("hydra.strip.deannotateAndDetypeTerm", v("appFun")),
        v("t"),
        field("unwrap", constant(
          applyP("hydra.scala.coder.stripWrapEliminations", v("appArg")))),
        field("application", stripWrapInnerArm))))

  private val stripWrapEliminationsBody = lambda("t",
    casesWithDefault("hydra.core.Term",
      applyP("hydra.strip.deannotateAndDetypeTerm", v("t")),
      v("t"),
      field("application", stripWrapApplicationArm)))

  lazy val stripWrapEliminationsDef: Definition =
    Phantoms.`def`(NS, "stripWrapEliminations",
      doc("Strip wrap eliminations from terms (newtypes are erased in Scala)", stripWrapEliminationsBody))

  // findImports
  private val findImportsBody = lambda("cx", lambda("g", lambda("mod",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.analysis.moduleDependencyModuleNames",
        v("cx"), v("g"), bool(false), bool(false), bool(true), bool(false), v("mod")),
      lambda("elImps",
        applyP("hydra.lib.eithers.bind",
          applyP("hydra.analysis.moduleDependencyModuleNames",
            v("cx"), v("g"), bool(false), bool(true), bool(false), bool(false), v("mod")),
          lambda("primImps",
            Phantoms.right(
              applyP("hydra.lib.lists.concat",
                list(
                  applyP("hydra.lib.lists.map",
                    v("hydra.scala.coder.toElImport"),
                    applyP("hydra.lib.sets.toList", v("elImps"))),
                  applyP("hydra.lib.lists.map",
                    v("hydra.scala.coder.toPrimImport"),
                    applyP("hydra.lib.sets.toList", v("primImps")))))))))))))

  lazy val findImportsDef: Definition =
    Phantoms.`def`(NS, "findImports",
      doc("Find import statements for the module", findImportsBody))

  // moduleToScala
  private val moduleToScalaBody = lambda("mod", lambda("defs", lambda("cx", lambda("g",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.constructModule", v("cx"), v("g"), v("mod"), v("defs")),
      lambda("pkg",
        let(Seq(
          field("s",
            applyP("hydra.serialization.printExpr",
              applyP("hydra.serialization.parenthesize",
                applyP("hydra.scala.serde.pkgToExpr", v("pkg")))))),
          Phantoms.right(
            applyP("hydra.lib.maps.singleton",
              applyP("hydra.names.moduleNameToFilePath",
                UtilDsl.caseConventionCamel,
                wrap("hydra.file.FileExtension", string("scala")),
                PackagingDsl.moduleName(v("mod"))),
              v("s"))))))))))

  lazy val moduleToScalaDef: Definition =
    Phantoms.`def`(NS, "moduleToScala",
      doc("Convert a Hydra module to Scala source code", moduleToScalaBody))

  // constructModule
  private val constructModuleBody = lambda("cx", lambda("g", lambda("mod", lambda("defs",
    let(Seq(
      field("partitioned", applyP("hydra.environment.partitionDefinitions", v("defs"))),
      field("typeDefs", applyP("hydra.lib.pairs.first", v("partitioned"))),
      field("termDefs", applyP("hydra.lib.pairs.second", v("partitioned"))),
      field("nsName",
        PackagingDsl.unModuleName(PackagingDsl.moduleName(v("mod")))),
      field("pname",
        ScalaSyntax.nameData(ScalaSyntax.predefString(applyP("hydra.lib.strings.intercalate",
              string("."),
              applyP("hydra.lib.strings.splitOn", string("."), v("nsName")))))),
      field("pref", inject("hydra.scala.syntax.RefData", "name", v("pname")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.lib.eithers.mapList",
          lambda("td", applyP("hydra.scala.coder.encodeTypeDefinition", v("cx"), v("g"), v("td"))),
          v("typeDefs")),
        lambda("typeDeclStats",
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.lib.eithers.mapList",
              lambda("td", applyP("hydra.scala.coder.encodeTermDefinition", v("cx"), v("g"), v("td"))),
              v("termDefs")),
            lambda("termDeclStats",
              applyP("hydra.lib.eithers.bind",
                applyP("hydra.scala.coder.findImports", v("cx"), v("g"), v("mod")),
                lambda("imports",
                  Phantoms.right(
                    ScalaSyntax.pkg(v("pname"))(v("pref"))(applyP("hydra.lib.lists.concat",
                        list(v("imports"), v("typeDeclStats"), v("termDeclStats"))))))))))))))))

  lazy val constructModuleDef: Definition =
    Phantoms.`def`(NS, "constructModule",
      doc("Construct a Scala package from a Hydra module and its definitions", constructModuleBody))

  // ===== Complex encode* definitions =====

  // encodeLiteral — Coder.hs:441-461
  private val encodeLiteralBody = lambda("cx", lambda("g", lambda("av",
    casesWithDefault("hydra.core.Literal",
      v("av"),
      errorLeft("unexpected literal"),
      field("binary", lambda("b",
        Phantoms.right(inject("hydra.scala.syntax.Lit", "bytes",
          applyP("hydra.lib.literals.binaryToBytes", v("b")))))),
      field("boolean", lambda("b",
        Phantoms.right(inject("hydra.scala.syntax.Lit", "boolean", v("b"))))),
      field("decimal", lambda("d",
        Phantoms.right(inject("hydra.scala.syntax.Lit", "string",
          applyP("hydra.lib.literals.showDecimal", v("d")))))),
      field("float", lambda("fv",
        casesWithDefault("hydra.core.FloatValue", v("fv"),
          errorLeft("unexpected float value"),
          field("float32", lambda("f",
            Phantoms.right(inject("hydra.scala.syntax.Lit", "float", v("f"))))),
          field("float64", lambda("f",
            Phantoms.right(inject("hydra.scala.syntax.Lit", "double", v("f")))))))),
      field("integer", lambda("iv",
        casesWithDefault("hydra.core.IntegerValue", v("iv"),
          errorLeft("unexpected integer value"),
          field("bigint", lambda("i",
            Phantoms.right(inject("hydra.scala.syntax.Lit", "long",
              applyP("hydra.lib.literals.bigintToInt64", v("i")))))),
          field("int8", lambda("i",
            Phantoms.right(inject("hydra.scala.syntax.Lit", "byte", v("i"))))),
          field("int16", lambda("i",
            Phantoms.right(inject("hydra.scala.syntax.Lit", "short", v("i"))))),
          field("int32", lambda("i",
            Phantoms.right(inject("hydra.scala.syntax.Lit", "int", v("i"))))),
          field("int64", lambda("i",
            Phantoms.right(inject("hydra.scala.syntax.Lit", "long", v("i"))))),
          field("uint8", lambda("i",
            Phantoms.right(inject("hydra.scala.syntax.Lit", "byte",
              applyP("hydra.lib.literals.bigintToInt8",
                applyP("hydra.lib.literals.uint8ToBigint", v("i"))))))),
          field("uint16", lambda("i",
            Phantoms.right(inject("hydra.scala.syntax.Lit", "int",
              applyP("hydra.lib.literals.bigintToInt32",
                applyP("hydra.lib.literals.uint16ToBigint", v("i"))))))),
          field("uint32", lambda("i",
            Phantoms.right(inject("hydra.scala.syntax.Lit", "long",
              applyP("hydra.lib.literals.bigintToInt64",
                applyP("hydra.lib.literals.uint32ToBigint", v("i"))))))),
          field("uint64", lambda("i",
            Phantoms.right(inject("hydra.scala.syntax.Lit", "long",
              applyP("hydra.lib.literals.bigintToInt64",
                applyP("hydra.lib.literals.uint64ToBigint", v("i")))))))))),
      field("string", lambda("s",
        Phantoms.right(inject("hydra.scala.syntax.Lit", "string", v("s")))))))))

  lazy val encodeLiteralDef: Definition =
    Phantoms.`def`(NS, "encodeLiteral",
      doc("Encode a literal value as a Scala literal", encodeLiteralBody))

  // findDomain — Coder.hs:1236-1243
  // getTypeE helper (Coder.hs:1289-1293)
  /** getTypeE wraps annotation.getType, converting DecodingError to Error.
   *  Uses a direct kernel-Term construction for the error wrapping to avoid
   *  the inference engine choking on the curried `otherError ∘ errorOther` chain. */
  private def getTypeE(g: TypedTerm[Any], ann: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.lib.eithers.bimap",
      lambda("__de",
        // Build Error.other(OtherError(unDecodingError(__de))) directly as a Term.
        // The unDecodingError call returns TypedTerm[String]; we wrap it in a kernel-
        // level OtherError-wrap + Error.other-inject.
        Term.inject(Injection("hydra.errors.Error",
          Field("other",
            Term.wrap(WrappedTerm("hydra.errors.OtherError",
              ErrorsDsl.unDecodingError(v("__de")))))))),
      lambda("__a", v("__a")),
      applyP("hydra.annotations.getType", g, ann))

  private val findDomainTypeCases = lambda("t",
    casesWithDefault("hydra.core.Type",
      applyP("hydra.strip.deannotateType", v("t")),
      errorLeft("expected a function type"),
      field("function", lambda("ft",
        Phantoms.right(CoreDsl.functionTypeDomain(v("ft")))))))

  private val findDomainOptCases = lambda("r",
    applyP("hydra.lib.optionals.cases",
      v("r"),
      errorLeft("expected a typed term"),
      findDomainTypeCases))

  private val findDomainBody = lambda("cx", lambda("g", lambda("meta",
    applyP("hydra.lib.eithers.bind",
      getTypeE(v("g"), v("meta")),
      findDomainOptCases))))

  lazy val findDomainDef: Definition =
    Phantoms.`def`(NS, "findDomain",
      doc("Find the domain type from annotations", findDomainBody))

  // findSdom — Coder.hs:1259-1286
  private val findSdomFunctionArm = lambda("ft",
    let(Seq(field("dom", CoreDsl.functionTypeDomain(v("ft")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("dom")),
        lambda("sdom", Phantoms.right(just(v("sdom")))))))

  private val findSdomForallInner = lambda("ft2",
    let(Seq(field("dom2", CoreDsl.functionTypeDomain(v("ft2")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("dom2")),
        lambda("sdom2", Phantoms.right(just(v("sdom2")))))))

  private val findSdomForallArm = lambda("fa",
    casesWithDefault("hydra.core.Type",
      applyP("hydra.strip.deannotateType", CoreDsl.forallTypeBody(v("fa"))),
      Phantoms.right(nothing),
      field("function", findSdomForallInner)))

  private val findSdomTypeCases = lambda("t",
    casesWithDefault("hydra.core.Type",
      applyP("hydra.strip.deannotateType", v("t")),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("t")),
        lambda("st", Phantoms.right(just(v("st"))))),
      field("function", findSdomFunctionArm),
      field("forall", findSdomForallArm)))

  private val findSdomOptCases = lambda("mtyp",
    applyP("hydra.lib.optionals.cases",
      v("mtyp"),
      Phantoms.right(nothing),
      findSdomTypeCases))

  private val findSdomBody = lambda("cx", lambda("g", lambda("meta",
    applyP("hydra.lib.eithers.bind",
      getTypeE(v("g"), v("meta")),
      findSdomOptCases))))

  lazy val findSdomDef: Definition =
    Phantoms.`def`(NS, "findSdom",
      doc("Find the Scala domain type for a function from annotations", findSdomBody))

  // ===== fieldToEnumCase — Coder.hs:1177-1216 =====

  private val fieldToEnumCaseFnameExpr =
    applyP("hydra.scala.utils.scalaEscapeEnumCaseName",
      CoreDsl.unName(CoreDsl.fieldTypeName(v("ft"))))

  private val fieldToEnumCaseFtypExpr =
    CoreDsl.fieldTypeType(v("ft"))

  private val fieldToEnumCaseCaseNameExpr =
    ScalaSyntax.nameData(ScalaSyntax.predefString(v("fname")))

  private val fieldToEnumCaseIsUnitExpr =
    casesWithDefault("hydra.core.Type",
      applyP("hydra.strip.deannotateType", v("ftyp")),
      bool(false),
      field("unit", constant(bool(true))),
      field("record", lambda("rt",
        applyP("hydra.lib.equality.equal",
          applyP("hydra.lib.lists.length", v("rt")),
          int32(0)))))

  private val fieldToEnumCaseParentTypeApplied =
    inject("hydra.scala.syntax.Type", "apply",
      ScalaSyntax.applyType(stref(v("parentName")))(applyP("hydra.lib.lists.map",
          v("hydra.scala.coder.typeParamToTypeVar"),
          v("tparams"))))

  private val fieldToEnumCaseParentTypeExpr =
    applyP("hydra.lib.logic.ifElse",
      applyP("hydra.lib.lists.null", v("tparams")),
      stref(v("parentName")),
      fieldToEnumCaseParentTypeApplied)

  private val fieldToEnumCaseValueParam =
    ScalaSyntax.paramData(emptyList)(inject("hydra.scala.syntax.Name", "value", string("value")))(just(v("sftyp")))(nothing)

  private val fieldToEnumCaseParamssList =
    list(
      applyP("hydra.lib.logic.ifElse",
        v("isUnit"),
        emptyList,
        list(fieldToEnumCaseValueParam)))

  private val fieldToEnumCasePrimaryCtor =
    ScalaSyntax.primaryCtor(emptyList)(inject("hydra.scala.syntax.Name", "value", string("")))(fieldToEnumCaseParamssList)

  private val fieldToEnumCaseInitsList =
    list(
      ScalaSyntax.init(v("parentType"))(inject("hydra.scala.syntax.Name", "value", string("")))(emptyList))

  private val fieldToEnumCaseEnumCaseDefnExpr =
    ScalaSyntax.enumCaseDefn(emptyList)(v("caseName"))(emptyList)(fieldToEnumCasePrimaryCtor)(fieldToEnumCaseInitsList)

  private val fieldToEnumCaseSftypBindBody = lambda("sftyp",
    Phantoms.right(
      inject("hydra.scala.syntax.Stat", "defn",
        inject("hydra.scala.syntax.Defn", "enumCase", fieldToEnumCaseEnumCaseDefnExpr))))

  private val fieldToEnumCaseLetBody =
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("ftyp")),
      fieldToEnumCaseSftypBindBody)

  private val fieldToEnumCaseBody = lambda("cx", lambda("g", lambda("parentName", lambda("tparams", lambda("ft",
    let(Seq(
      field("fname", fieldToEnumCaseFnameExpr),
      field("ftyp", fieldToEnumCaseFtypExpr),
      field("caseName", fieldToEnumCaseCaseNameExpr),
      field("isUnit", fieldToEnumCaseIsUnitExpr),
      field("parentType", fieldToEnumCaseParentTypeExpr)),
      fieldToEnumCaseLetBody))))))

  lazy val fieldToEnumCaseDef: Definition =
    Phantoms.`def`(NS, "fieldToEnumCase",
      doc("Convert a field type to a Scala enum case", fieldToEnumCaseBody))

  // ===== encodeType — Coder.hs:836-966 =====

  // Literal-type arms — small enough to inline as a single map
  private val encodeTypeLitFloat = lambda("ft",
    casesWithDefault("hydra.core.FloatType",
      v("ft"),
      errorLeft("unsupported float type"),
      field("float32", constant(Phantoms.right(stref(string("scala.Float"))))),
      field("float64", constant(Phantoms.right(stref(string("scala.Double")))))))

  private val encodeTypeLitInteger = lambda("it",
    casesWithDefault("hydra.core.IntegerType",
      v("it"),
      errorLeft("unsupported integer type"),
      field("bigint", constant(Phantoms.right(stref(string("scala.math.BigInt"))))),
      field("int8", constant(Phantoms.right(stref(string("scala.Byte"))))),
      field("int16", constant(Phantoms.right(stref(string("scala.Short"))))),
      field("int32", constant(Phantoms.right(stref(string("scala.Int"))))),
      field("int64", constant(Phantoms.right(stref(string("scala.Long"))))),
      field("uint8", constant(Phantoms.right(stref(string("scala.Byte"))))),
      field("uint16", constant(Phantoms.right(stref(string("scala.Int"))))),
      field("uint32", constant(Phantoms.right(stref(string("scala.Long"))))),
      field("uint64", constant(Phantoms.right(stref(string("scala.math.BigInt")))))))

  private val encodeTypeLitArm = lambda("lt",
    casesWithDefault("hydra.core.LiteralType",
      v("lt"),
      errorLeft("unsupported literal type"),
      field("binary", constant(Phantoms.right(
        applyP("hydra.scala.utils.stapply",
          stref(string("scala.Array")),
          list(stref(string("scala.Byte"))))))),
      field("boolean", constant(Phantoms.right(stref(string("scala.Boolean"))))),
      field("decimal", constant(Phantoms.right(stref(string("scala.math.BigDecimal"))))),
      field("float", encodeTypeLitFloat),
      field("integer", encodeTypeLitInteger),
      field("string", constant(Phantoms.right(stref(string("scala.Predef.String")))))))

  // application arm: collect curried type args into a flat list, encode base + args, stapply
  private val encodeTypeCollectArgsRecurse = lambda("at2",
    let(Seq(
      field("f2", CoreDsl.applicationTypeFunction(v("at2"))),
      field("a2", CoreDsl.applicationTypeArgument(v("at2")))),
      applyP("hydra.scala.coder.encodeType_collectArgs",
        v("f2"),
        applyP("hydra.lib.lists.cons", v("a2"), v("acc")))))

  // NOTE: The Haskell version uses a lets-bound recursive helper "collectTypeArgs".
  // We approximate by inlining the recursion via a self-referencing FQN call,
  // matching the JSON output structure. The actual recursion shape mirrors the
  // Haskell `let "collectTypeArgs" = ... in collectTypeArgs @@ base @@ acc`.
  // Simpler: just do a 1-level decomposition for now (most type apps are unary).

  private val encodeTypeApplicationArm = lambda("at",
    Phantoms.let(Seq(
      field("collectTypeArgs",
        lambda("t2", lambda("acc",
          casesWithDefault("hydra.core.Type",
            applyP("hydra.strip.deannotateType", v("t2")),
            Phantoms.pair(v("t2"), v("acc")),
            field("application", lambda("at2",
              Phantoms.let(Seq(
                field("f2", CoreDsl.applicationTypeFunction(v("at2"))),
                field("a2", CoreDsl.applicationTypeArgument(v("at2")))),
                applyN(v("collectTypeArgs"),
                  v("f2"),
                  applyP("hydra.lib.lists.cons", v("a2"), v("acc")))))))))),
      field("collected",
        applyN(v("collectTypeArgs"),
          CoreDsl.typeApplication(v("at")),
          emptyList)),
      field("baseFun", applyP("hydra.lib.pairs.first", v("collected"))),
      field("allArgs", applyP("hydra.lib.pairs.second", v("collected")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("baseFun")),
        lambda("sfun",
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.lib.eithers.mapList",
              lambda("a", applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("a"))),
              v("allArgs")),
            lambda("sargs",
              Phantoms.right(
                applyP("hydra.scala.utils.stapply", v("sfun"), v("sargs")))))))))

  private val encodeTypeEitherArm = lambda("et",
    let(Seq(
      field("lt", CoreDsl.eitherTypeLeft(v("et"))),
      field("rt", CoreDsl.eitherTypeRight(v("et")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("lt")),
        lambda("slt",
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("rt")),
            lambda("srt",
              Phantoms.right(
                applyP("hydra.scala.utils.stapply2",
                  stref(string("Either")), v("slt"), v("srt")))))))))

  private val encodeTypeFunctionArm = lambda("ft",
    let(Seq(
      field("dom", CoreDsl.functionTypeDomain(v("ft"))),
      field("cod", CoreDsl.functionTypeCodomain(v("ft")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("dom")),
        lambda("sdom",
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("cod")),
            lambda("scod",
              Phantoms.right(
                inject("hydra.scala.syntax.Type", "function",
                  ScalaSyntax.functionType(list(v("sdom")))(v("scod"))))))))))

  private val encodeTypeMapArm = lambda("mt",
    let(Seq(
      field("kt", CoreDsl.mapTypeKeys(v("mt"))),
      field("vt", CoreDsl.mapTypeValues(v("mt")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("kt")),
        lambda("skt",
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("vt")),
            lambda("svt",
              Phantoms.right(
                applyP("hydra.scala.utils.stapply2",
                  stref(string("scala.collection.immutable.Map")),
                  v("skt"), v("svt")))))))))

  private val encodeTypePairArm = lambda("pt",
    let(Seq(
      field("ft", CoreDsl.pairTypeFirst(v("pt"))),
      field("st", CoreDsl.pairTypeSecond(v("pt")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("ft")),
        lambda("sft",
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("st")),
            lambda("sst",
              Phantoms.right(
                applyP("hydra.scala.utils.stapply2",
                  stref(string("Tuple2")),
                  v("sft"), v("sst")))))))))

  private val encodeTypeForallArm = lambda("ft",
    let(Seq(
      field("v", CoreDsl.forallTypeParameter(v("ft"))),
      field("body", CoreDsl.forallTypeBody(v("ft")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("body")),
        lambda("sbody",
          Phantoms.right(
            inject("hydra.scala.syntax.Type", "lambda",
              ScalaSyntax.lambdaType(list(applyP("hydra.scala.utils.stparam", v("v"))))(v("sbody"))))))))

  private val encodeTypeVariableArm = lambda("v",
    let(Seq(
      field("rawName", CoreDsl.unName(v("v"))),
      field("typeName",
        applyP("hydra.lib.logic.ifElse",
          applyP("hydra.lib.lists.elem",
            int32(46),
            applyP("hydra.lib.strings.toList", v("rawName"))),
          v("rawName"),
          applyP("hydra.formatting.capitalize", v("rawName"))))),
      Phantoms.right(
        inject("hydra.scala.syntax.Type", "var",
          ScalaSyntax.varType(ScalaSyntax.nameType(v("typeName")))))))

  private val encodeTypeBody = lambda("cx", lambda("g", lambda("t",
    casesWithDefault("hydra.core.Type",
      applyP("hydra.strip.deannotateType", v("t")),
      errorLeft("unsupported type"),
      field("application", encodeTypeApplicationArm),
      field("unit", constant(Phantoms.right(stref(string("Unit"))))),
      field("either", encodeTypeEitherArm),
      field("effect", lambda("et",
        applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("et")))),
      field("function", encodeTypeFunctionArm),
      field("list", lambda("lt",
        applyP("hydra.lib.eithers.bind",
          applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("lt")),
          lambda("slt",
            Phantoms.right(
              applyP("hydra.scala.utils.stapply1",
                stref(string("scala.collection.immutable.Seq")),
                v("slt"))))))),
      field("literal", encodeTypeLitArm),
      field("map", encodeTypeMapArm),
      field("optional", lambda("ot",
        applyP("hydra.lib.eithers.bind",
          applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("ot")),
          lambda("sot",
            Phantoms.right(
              applyP("hydra.scala.utils.stapply1",
                stref(string("scala.Option")),
                v("sot"))))))),
      field("pair", encodeTypePairArm),
      field("record", constant(errorLeft("unexpected anonymous record type"))),
      field("set", lambda("st",
        applyP("hydra.lib.eithers.bind",
          applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("st")),
          lambda("sst",
            Phantoms.right(
              applyP("hydra.scala.utils.stapply1",
                stref(string("scala.collection.immutable.Set")),
                v("sst"))))))),
      field("union", constant(errorLeft("unexpected anonymous union type"))),
      field("wrap", constant(errorLeft("unexpected anonymous wrap type"))),
      field("forall", encodeTypeForallArm),
      field("variable", encodeTypeVariableArm)))))

  lazy val encodeTypeDef: Definition =
    Phantoms.`def`(NS, "encodeType",
      doc("Encode a Hydra type as a Scala type", encodeTypeBody))

  // ===== encodeTermDefinition — Coder.hs:787-834 =====
  // Routes between encodeComplexTermDef (function types) and lazy val (others).

  private val encodeTermDefIsFunctionTypeForall = lambda("fa",
    casesWithDefault("hydra.core.Type",
      applyP("hydra.strip.deannotateType", CoreDsl.forallTypeBody(v("fa"))),
      bool(false),
      field("function", constant(bool(true)))))

  private val encodeTermDefIsFunctionType =
    casesWithDefault("hydra.core.Type",
      applyP("hydra.strip.deannotateType", v("typ'")),
      bool(false),
      field("function", constant(bool(true))),
      field("forall", encodeTermDefIsFunctionTypeForall))

  private val encodeTermDefSimpleVal =
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("typ'")),
      lambda("stype",
        applyP("hydra.lib.eithers.bind",
          applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("term")),
          lambda("rhs",
            Phantoms.right(mkLazyVal(v("lname"), just(v("stype")), v("rhs")))))))

  private val encodeTermDefZeroParamDef =
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("typ'")),
      lambda("stype",
        applyP("hydra.lib.eithers.bind",
          applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("term")),
          lambda("rhs",
            let(Seq(field("tparams",
              applyP("hydra.lib.lists.map",
                lambda("tv", applyP("hydra.scala.utils.stparam", v("tv"))),
                v("freeTypeVarsInTyp")))),
              Phantoms.right(
                inject("hydra.scala.syntax.Stat", "defn",
                  inject("hydra.scala.syntax.Defn", "def",
                    ScalaSyntax.defDefn(emptyList)(ScalaSyntax.nameData(ScalaSyntax.predefString(v("lname"))))(v("tparams"))(emptyList)(just(v("stype")))(v("rhs"))))))))))

  private val encodeTermDefBranch =
    applyP("hydra.lib.logic.ifElse",
      v("isFunctionType"),
      applyP("hydra.scala.coder.encodeComplexTermDef",
        v("cx"), v("g"), v("lname"), v("term"), v("typ'")),
      applyP("hydra.lib.logic.ifElse",
        applyP("hydra.lib.lists.null", v("freeTypeVarsInTyp")),
        encodeTermDefSimpleVal,
        encodeTermDefZeroParamDef))

  private val encodeTermDefinitionBody = lambda("cx", lambda("g", lambda("td",
    let(Seq(
      field("name", PackagingDsl.termDefinitionName(v("td"))),
      field("term", PackagingDsl.termDefinitionBody(v("td"))),
      field("lname",
        applyP("hydra.scala.utils.scalaEscapeName",
          applyP("hydra.names.localNameOf", v("name")))),
      field("typ'",
        applyP("hydra.lib.optionals.cases",
          applyP("hydra.lib.optionals.map",
            v("hydra.scoping.termSignatureToTypeScheme"),
            PackagingDsl.termDefinitionSignature(v("td"))),
          CoreDsl.typeVariable(wrap("hydra.core.Name", string("hydra.core.Unit"))),
          project("hydra.core.TypeScheme", "body"))),
      field("isFunctionType", encodeTermDefIsFunctionType),
      field("freeTypeVarsInTyp",
        applyP("hydra.lib.lists.filter",
          lambda("v",
            applyP("hydra.lib.logic.not",
              applyP("hydra.lib.lists.elem",
                int32(46),
                applyP("hydra.lib.strings.toList", CoreDsl.unName(v("v")))))),
          applyP("hydra.lib.sets.toList",
            applyP("hydra.variables.freeVariablesInType", v("typ'")))))),
      encodeTermDefBranch))))

  lazy val encodeTermDefinitionDef: Definition =
    Phantoms.`def`(NS, "encodeTermDefinition",
      doc("Encode a term definition as a Scala statement", encodeTermDefinitionBody))

  // ===== encodeTypeDefinition — Coder.hs:968-1088 =====
  // Has 3 where-helpers: recordTypeCase, unionTypeCase, defaultTypeCase.
  // Plus a local stparam.

  private val emptyTemplate =
    ScalaSyntax.template(emptyList)(emptyList)(wrap("hydra.scala.syntax.Self", Phantoms.unit))(emptyList)

  private val encodeTypeDefStparam = lambda("__v",
    let(Seq(
      field("vn", applyP("hydra.formatting.capitalize", CoreDsl.unName(v("__v"))))),
      ScalaSyntax.paramType(emptyList)(inject("hydra.scala.syntax.Name", "value", v("vn")))(emptyList)(emptyList)(emptyList)(emptyList)))

  private val recordTypeCase =
    lambda("tname", lambda("tparams", lambda("cx", lambda("g", lambda("rt",
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.lib.eithers.mapList",
          lambda("f", applyP("hydra.scala.coder.fieldToParam", v("cx"), v("g"), v("f"))),
          v("rt")),
        lambda("params",
          Phantoms.right(
            inject("hydra.scala.syntax.Stat", "defn",
              inject("hydra.scala.syntax.Defn", "class",
                ScalaSyntax.classDefn(list(inject("hydra.scala.syntax.Mod", "case", Phantoms.unit)))(v("tname"))(v("tparams"))(ScalaSyntax.primaryCtor(emptyList)(inject("hydra.scala.syntax.Name", "value", string("")))(list(v("params"))))(emptyTemplate)))))))))))

  private val unionTypeCase =
    lambda("tname", lambda("lname", lambda("tparams", lambda("cx", lambda("g", lambda("rt",
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.lib.eithers.mapList",
          lambda("f", applyP("hydra.scala.coder.fieldToEnumCase",
            v("cx"), v("g"), v("lname"), v("tparams"), v("f"))),
          v("rt")),
        lambda("cases",
          Phantoms.right(
            inject("hydra.scala.syntax.Stat", "defn",
              inject("hydra.scala.syntax.Defn", "enum",
                ScalaSyntax.enumDefn(emptyList)(v("tname"))(v("tparams"))(ScalaSyntax.primaryCtor(emptyList)(inject("hydra.scala.syntax.Name", "value", string("")))(emptyList))(ScalaSyntax.template(emptyList)(emptyList)(wrap("hydra.scala.syntax.Self", Phantoms.unit))(v("cases"))))))))))))))

  // wrap arm
  private val encodeTypeDefWrapArm = lambda("wt",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("wt")),
      lambda("styp",
        Phantoms.right(
          inject("hydra.scala.syntax.Stat", "defn",
            inject("hydra.scala.syntax.Defn", "type",
              ScalaSyntax.typeDefn(emptyList)(v("tname"))(v("tparams"))(v("styp"))))))))

  // forall inner cases on innerBody
  private val encodeTypeDefForallInnerWrap = lambda("wt2",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("wt2")),
      lambda("styp",
        Phantoms.right(
          inject("hydra.scala.syntax.Stat", "defn",
            inject("hydra.scala.syntax.Defn", "type",
              ScalaSyntax.typeDefn(emptyList)(v("tname"))(v("allTparams"))(v("styp"))))))))

  // defaultTypeCase: try encodeType; on success or failure, wrap result as a type alias
  // defaultTypeCase: build mkAlias as a let-bound helper (matches Haskell DSL where-binding),
  // then dispatch via Eithers.either with constant-failure and identity-success paths.
  private val defaultTypeCase = lambda("lname2", lambda("tparams2", lambda("cx2", lambda("g2", lambda("typ2",
    Phantoms.let("mkAlias",
      lambda("styp",
        Phantoms.right(
          inject("hydra.scala.syntax.Stat", "defn",
            inject("hydra.scala.syntax.Defn", "type",
              ScalaSyntax.typeDefn(emptyList)(ScalaSyntax.nameType(v("lname2")))(v("tparams2"))(v("styp")))))),
      applyP("hydra.lib.eithers.either",
        constant(applyN(v("mkAlias"), applyP("hydra.scala.utils.stref", string("Any")))),
        v("mkAlias"),
        applyP("hydra.scala.coder.encodeType", v("cx2"), v("g2"), v("typ2")))))))))

  private val encodeTypeDefForallArm = lambda("ft",
    let(Seq(
      field("forallBody", CoreDsl.forallTypeBody(v("ft"))),
      field("forallParam", CoreDsl.forallTypeParameter(v("ft"))),
      field("collectForallParams",
        lambda("t", lambda("acc",
          casesWithDefault("hydra.core.Type",
            applyP("hydra.strip.deannotateType", v("t")),
            Phantoms.pair(v("acc"), v("t")),
            field("forall", lambda("ft2",
              applyN(v("collectForallParams"),
                CoreDsl.forallTypeBody(v("ft2")),
                applyP("hydra.lib.lists.cons",
                  CoreDsl.forallTypeParameter(v("ft2")),
                  v("acc"))))))))),
      field("collected",
        applyN(v("collectForallParams"), v("forallBody"), list(v("forallParam")))),
      field("allForallParams",
        applyP("hydra.lib.lists.reverse",
          applyP("hydra.lib.pairs.first", v("collected")))),
      field("innerBody", applyP("hydra.lib.pairs.second", v("collected"))),
      field("allTparams",
        applyP("hydra.lib.lists.map",
          encodeTypeDefStparam,
          v("allForallParams")))),
      casesWithDefault("hydra.core.Type",
        applyP("hydra.strip.deannotateType", v("innerBody")),
        applyN(defaultTypeCase, v("lname"), v("allTparams"), v("cx"), v("g"), v("innerBody")),
        field("record", lambda("rt2",
          applyN(recordTypeCase, v("tname"), v("allTparams"), v("cx"), v("g"), v("rt2")))),
        field("union", lambda("rt2",
          applyN(unionTypeCase, v("tname"), v("lname"), v("allTparams"), v("cx"), v("g"), v("rt2")))),
        field("wrap", encodeTypeDefForallInnerWrap))))

  private val encodeTypeDefinitionBody = lambda("cx", lambda("g", lambda("td",
    let(Seq(
      field("name", PackagingDsl.typeDefinitionName(v("td"))),
      field("typ",
        CoreDsl.typeSchemeBody(PackagingDsl.typeDefinitionBody(v("td")))),
      field("lname", applyP("hydra.names.localNameOf", v("name"))),
      field("tname", ScalaSyntax.nameType(v("lname"))),
      field("dname",
        ScalaSyntax.nameData(ScalaSyntax.predefString(v("lname")))),
      field("freeVars",
        applyP("hydra.lib.lists.filter",
          lambda("v",
            applyP("hydra.lib.logic.not",
              applyP("hydra.lib.lists.elem",
                int32(46),
                applyP("hydra.lib.strings.toList", CoreDsl.unName(v("v")))))),
          applyP("hydra.lib.sets.toList",
            applyP("hydra.variables.freeVariablesInType", v("typ"))))),
      field("tparams",
        applyP("hydra.lib.lists.map", encodeTypeDefStparam, v("freeVars")))),
      casesWithDefault("hydra.core.Type",
        applyP("hydra.strip.deannotateType", v("typ")),
        applyN(defaultTypeCase, v("lname"), v("tparams"), v("cx"), v("g"), v("typ")),
        field("forall", encodeTypeDefForallArm),
        field("record", lambda("rt",
          applyN(recordTypeCase, v("tname"), v("tparams"), v("cx"), v("g"), v("rt")))),
        field("union", lambda("rt",
          applyN(unionTypeCase, v("tname"), v("lname"), v("tparams"), v("cx"), v("g"), v("rt")))),
        field("wrap", encodeTypeDefWrapArm))))))

  lazy val encodeTypeDefinitionDef: Definition =
    Phantoms.`def`(NS, "encodeTypeDefinition",
      doc("Encode a type definition as a Scala statement", encodeTypeDefinitionBody))

  // ===== encodeLetBinding — Coder.hs:405-438, full translation =====

  private val encodeLetBindingIsFnForall = lambda("fa",
    casesWithDefault("hydra.core.Type",
      applyP("hydra.strip.deannotateType", CoreDsl.forallTypeBody(v("fa"))),
      bool(false),
      field("function", constant(bool(true)))))

  private val encodeLetBindingIsFnTsArm = lambda("ts",
    casesWithDefault("hydra.core.Type",
      applyP("hydra.strip.deannotateType", CoreDsl.typeSchemeBody(v("ts"))),
      bool(false),
      field("function", constant(bool(true))),
      field("forall", encodeLetBindingIsFnForall)))

  private val encodeLetBindingNoTsBranch =
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("bterm")),
      lambda("srhs",
        Phantoms.right(mkLazyVal(v("bname"), nothing, v("srhs")))))

  private val encodeLetBindingMonoValBranch =
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("bterm")),
      lambda("srhs",
        applyP("hydra.lib.eithers.bind",
          applyP("hydra.scala.coder.encodeType", v("cx"), v("g"),
            CoreDsl.typeSchemeBody(v("ts"))),
          lambda("styp",
            Phantoms.right(mkLazyVal(v("bname"), just(v("styp")), v("srhs")))))))

  private val encodeLetBindingTsBranch = lambda("ts",
    let(Seq(
      field("newVars",
        applyP("hydra.lib.lists.filter",
          lambda("v",
            applyP("hydra.lib.logic.not",
              applyP("hydra.lib.sets.member",
                v("v"), v("outerTypeVars")))),
          CoreDsl.typeSchemeVariables(v("ts")))),
      field("useDef",
        applyP("hydra.lib.logic.or",
          v("isFn"),
          applyP("hydra.lib.logic.not",
            applyP("hydra.lib.lists.null", v("newVars")))))),
      applyP("hydra.lib.logic.ifElse",
        v("useDef"),
        applyP("hydra.scala.coder.encodeLocalDef",
          v("cx"), v("g"), v("outerTypeVars"), v("bname"), v("bterm"),
          CoreDsl.typeSchemeBody(v("ts"))),
        encodeLetBindingMonoValBranch)))

  private val encodeLetBindingBody = lambda("cx", lambda("g", lambda("outerTypeVars", lambda("b",
    let(Seq(
      field("bname",
        applyP("hydra.scala.utils.scalaEscapeName",
          CoreDsl.unName(CoreDsl.bindingName(v("b"))))),
      field("bterm", CoreDsl.bindingTerm(v("b"))),
      field("mts",
        applyP("hydra.lib.optionals.cases",
          CoreDsl.bindingTypeScheme(v("b")),
          applyP("hydra.lib.maps.lookup",
            CoreDsl.bindingName(v("b")),
            GraphDsl.graphBoundTypes(v("g"))),
          lambda("ts", just(v("ts"))))),
      field("isFn",
        applyP("hydra.lib.optionals.cases",
          v("mts"),
          bool(false),
          encodeLetBindingIsFnTsArm))),
      applyP("hydra.lib.optionals.cases",
        v("mts"),
        encodeLetBindingNoTsBranch,
        encodeLetBindingTsBranch))))))

  lazy val encodeLetBindingDef: Definition =
    Phantoms.`def`(NS, "encodeLetBinding",
      doc("Encode a let binding as a val or def declaration. outerTypeVars are type params from the enclosing scope.", encodeLetBindingBody))

  // ===== encodeLocalDef — Coder.hs:463-517, full translation =====
  //
  // Translates the Haskell `lets [...] $ Eithers.bind ... ("sparams" ~> ...)` shape
  // using a sequential chain of 1-binding lets — each binding's body is its
  // continuation. This guarantees that every `v("X")` reference lies lexically
  // inside the let that binds X (Hydra inference uses SCC analysis on let-rec
  // groups; one binding per let avoids any reordering surprises).

  private val encodeLocalDefBody = lambda("cx", lambda("g", lambda("outerTypeVars",
    lambda("lname", lambda("term", lambda("typ",
      let(Seq(
        field("freeTypeVars",
          applyP("hydra.lib.lists.filter",
            lambda("v",
              applyP("hydra.lib.logic.and",
                applyP("hydra.lib.logic.not",
                  applyP("hydra.lib.lists.elem",
                    int32(46),
                    applyP("hydra.lib.strings.toList", CoreDsl.unName(v("v"))))),
                applyP("hydra.lib.logic.not",
                  applyP("hydra.lib.sets.member",
                    v("v"), v("outerTypeVars"))))),
            applyP("hydra.lib.sets.toList",
              applyP("hydra.variables.freeVariablesInType", v("typ"))))),
        field("doms", applyP("hydra.scala.coder.extractDomains", v("typ"))),
        field("paramNames", applyP("hydra.scala.coder.extractParams", v("term"))),
        field("paramCount",
          applyP("hydra.lib.math.min",
            applyP("hydra.lib.lists.length", v("paramNames")),
            applyP("hydra.lib.lists.length", v("doms")))),
        field("cod",
          applyP("hydra.scala.coder.dropDomains", v("paramCount"), v("typ"))),
        field("zippedParams",
          applyP("hydra.lib.lists.zip",
            applyP("hydra.lib.lists.take", v("paramCount"), v("paramNames")),
            applyP("hydra.lib.lists.take", v("paramCount"), v("doms")))),
        field("letBindings",
          applyP("hydra.scala.coder.extractLetBindings", v("term"))),
        field("tparams",
          applyP("hydra.lib.lists.map",
            lambda("tv", applyP("hydra.scala.utils.stparam", v("tv"))),
            v("freeTypeVars"))),
        field("allTypeVars",
          applyP("hydra.lib.sets.union",
            v("outerTypeVars"),
            applyP("hydra.lib.sets.fromList", v("freeTypeVars")))),
        field("gWithTypeVars",
          GraphDsl.graph(GraphDsl.graphBoundTerms(v("g")))(
            GraphDsl.graphBoundTypes(v("g")))(
            GraphDsl.graphClassConstraints(v("g")))(
            GraphDsl.graphLambdaVariables(v("g")))(
            GraphDsl.graphMetadata(v("g")))(
            GraphDsl.graphPrimitives(v("g")))(
            GraphDsl.graphSchemaTypes(v("g")))(
            applyP("hydra.lib.sets.union",
              v("allTypeVars"),
              GraphDsl.graphTypeVariables(v("g")))))),
      // Body of the multi-binding outer let: the four-stage Eithers.bind chain.
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.lib.eithers.mapList",
          applyP("hydra.scala.coder.encodeTypedParam",
            v("cx"), v("gWithTypeVars")),
          v("zippedParams")),
        lambda("sparams",
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.scala.coder.encodeTerm",
              v("cx"), v("gWithTypeVars"),
              applyP("hydra.scala.coder.extractBody", v("term"))),
            lambda("sbody",
              applyP("hydra.lib.eithers.bind",
                applyP("hydra.scala.coder.encodeType",
                  v("cx"), v("gWithTypeVars"), v("cod")),
                lambda("scod",
                  let(Seq(field("gForLets",
                    applyP("hydra.lib.logic.ifElse",
                      applyP("hydra.lib.lists.null", v("letBindings")),
                      v("gWithTypeVars"),
                      applyP("hydra.scoping.extendGraphForLet",
                        lambda("g", lambda("b",
                          applyP("hydra.lib.logic.ifElse",
                            applyP("hydra.predicates.isComplexBinding", v("g"), v("b")),
                            just(Phantoms.metaBool(true)),
                            nothing))),
                        v("gWithTypeVars"),
                        CoreDsl.let(v("letBindings"))(
                          CoreDsl.termVariable(wrap("hydra.core.Name", string("dummy")))))))),
                  applyP("hydra.lib.eithers.bind",
                    applyP("hydra.lib.eithers.mapList",
                      applyP("hydra.scala.coder.encodeLetBinding",
                        v("cx"), v("gForLets"), v("allTypeVars")),
                      v("letBindings")),
                    lambda("sbindings",
                      let(Seq(field("defBody",
                        applyP("hydra.lib.logic.ifElse",
                          applyP("hydra.lib.lists.null", v("sbindings")),
                          v("sbody"),
                          inject("hydra.scala.syntax.Data", "block",
                            ScalaSyntax.blockData(
                              applyP("hydra.lib.lists.concat2",
                                v("sbindings"),
                                list(inject("hydra.scala.syntax.Stat", "term", v("sbody"))))))))),
                      Phantoms.right(
                        inject("hydra.scala.syntax.Stat", "defn",
                          inject("hydra.scala.syntax.Defn", "def",
                            ScalaSyntax.defDefn(emptyList)(
                              ScalaSyntax.nameData(ScalaSyntax.predefString(v("lname"))))(
                              v("tparams"))(
                              applyP("hydra.lib.lists.map",
                                lambda("p", list(v("p"))),
                                v("sparams")))(
                              just(v("scod")))(
                              v("defBody"))))))))))))))))))))))

  lazy val encodeLocalDefDef: Definition =
    Phantoms.`def`(NS, "encodeLocalDef",
      doc("Encode a local def. outerTypeVars are type params already in scope (don't redeclare them).", encodeLocalDefBody))

  // ===== encodeComplexTermDef — Coder.hs:232-286, full translation =====
  // Similar shape to encodeLocalDef but lacks outerTypeVars handling. Uses nested
  // 1-binding lets for unambiguous scoping.

  private val encodeComplexTermDefBody = lambda("cx", lambda("g", lambda("lname", lambda("term", lambda("typ",
    Phantoms.let(Seq(
      field("doms", applyP("hydra.scala.coder.extractDomains", v("typ"))),
      field("paramNames", applyP("hydra.scala.coder.extractParams", v("term"))),
      field("paramCount",
        applyP("hydra.lib.math.min",
          applyP("hydra.lib.lists.length", v("paramNames")),
          applyP("hydra.lib.lists.length", v("doms")))),
      field("cod",
        applyP("hydra.scala.coder.dropDomains", v("paramCount"), v("typ"))),
      field("zippedParams",
        applyP("hydra.lib.lists.zip",
          applyP("hydra.lib.lists.take", v("paramCount"), v("paramNames")),
          applyP("hydra.lib.lists.take", v("paramCount"), v("doms")))),
      field("freeTypeVars",
        applyP("hydra.lib.lists.filter",
          lambda("v",
            applyP("hydra.lib.logic.not",
              applyP("hydra.lib.lists.elem",
                int32(46),
                applyP("hydra.lib.strings.toList", CoreDsl.unName(v("v")))))),
          applyP("hydra.lib.sets.toList",
            applyP("hydra.variables.freeVariablesInType", v("typ"))))),
      field("tparams",
        applyP("hydra.lib.lists.map",
          lambda("tv", applyP("hydra.scala.utils.stparam", v("tv"))),
          v("freeTypeVars"))),
      field("letBindings", applyP("hydra.scala.coder.extractLetBindings", v("term"))),
      field("gWithTypeVars",
        GraphDsl.graph(GraphDsl.graphBoundTerms(v("g")))(
          GraphDsl.graphBoundTypes(v("g")))(
          GraphDsl.graphClassConstraints(v("g")))(
          GraphDsl.graphLambdaVariables(v("g")))(
          GraphDsl.graphMetadata(v("g")))(
          GraphDsl.graphPrimitives(v("g")))(
          GraphDsl.graphSchemaTypes(v("g")))(
          applyP("hydra.lib.sets.union",
            applyP("hydra.lib.sets.fromList", v("freeTypeVars")),
            GraphDsl.graphTypeVariables(v("g")))))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.lib.eithers.mapList",
          applyP("hydra.scala.coder.encodeTypedParam",
            v("cx"), v("gWithTypeVars")),
          v("zippedParams")),
        lambda("sparams",
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.scala.coder.encodeTerm",
              v("cx"), v("gWithTypeVars"),
              applyP("hydra.scala.coder.extractBody", v("term"))),
            lambda("sbody",
              applyP("hydra.lib.eithers.bind",
                applyP("hydra.scala.coder.encodeType",
                  v("cx"), v("g"), v("cod")),
                lambda("scod",
                  Phantoms.let("gForLets",
                    applyP("hydra.lib.logic.ifElse",
                      applyP("hydra.lib.lists.null", v("letBindings")),
                      v("gWithTypeVars"),
                      applyP("hydra.scoping.extendGraphForLet",
                        lambda("g", lambda("b",
                          applyP("hydra.lib.logic.ifElse",
                            applyP("hydra.predicates.isComplexBinding", v("g"), v("b")),
                            just(Phantoms.metaBool(true)),
                            nothing))),
                        v("gWithTypeVars"),
                        CoreDsl.let(v("letBindings"))(
                          CoreDsl.termVariable(wrap("hydra.core.Name", string("dummy")))))),
                    applyP("hydra.lib.eithers.bind",
                      applyP("hydra.lib.eithers.mapList",
                        applyP("hydra.scala.coder.encodeLetBinding",
                          v("cx"), v("gForLets"),
                          applyP("hydra.lib.sets.fromList", v("freeTypeVars"))),
                        v("letBindings")),
                      lambda("sbindings",
                        Phantoms.let("defBody",
                          applyP("hydra.lib.logic.ifElse",
                            applyP("hydra.lib.lists.null", v("sbindings")),
                            v("sbody"),
                            inject("hydra.scala.syntax.Data", "block",
                              ScalaSyntax.blockData(
                                applyP("hydra.lib.lists.concat2",
                                  v("sbindings"),
                                  list(inject("hydra.scala.syntax.Stat", "term", v("sbody"))))))),
                          Phantoms.right(
                            inject("hydra.scala.syntax.Stat", "defn",
                              inject("hydra.scala.syntax.Defn", "def",
                                ScalaSyntax.defDefn(emptyList)(
                                  ScalaSyntax.nameData(ScalaSyntax.predefString(v("lname"))))(
                                  v("tparams"))(
                                  applyP("hydra.lib.lists.map",
                                    lambda("p", list(v("p"))),
                                    v("sparams")))(
                                  just(v("scod")))(
                                  v("defBody")))))))))))))))))))))

  lazy val encodeComplexTermDefDef: Definition =
    Phantoms.`def`(NS, "encodeComplexTermDef",
      doc("Encode a complex term definition with proper parameter types from the type signature", encodeComplexTermDefBody))

  // ===== encodeCase — Coder.hs:175-230, full translation =====
  // Each sub-term extracted as a private val. Vals defined bottom-up: dependents follow dependencies.

  private val encodeCaseIsUnitLambdaArm = lambda("lam",
    Phantoms.let(Seq(
      field("lamParam", CoreDsl.lambdaParameter(v("lam"))),
      field("lamBody", CoreDsl.lambdaBody(v("lam"))),
      field("domIsUnit",
        applyP("hydra.lib.optionals.cases",
          CoreDsl.lambdaDomain(v("lam")),
          Phantoms.bool(false),
          lambda("dom",
            applyP("hydra.lib.equality.equal", v("dom"), CoreDsl.typeUnit)))),
      field("bodyIgnoresParam",
        applyP("hydra.variables.isFreeVariableInTerm", v("lamParam"), v("lamBody")))),
      applyP("hydra.lib.logic.or", v("domIsUnit"), v("bodyIgnoresParam"))))

  private val encodeCaseIsUnitFromTerm =
    casesWithDefault("hydra.core.Term",
      applyP("hydra.strip.deannotateAndDetypeTerm", v("fterm")),
      Phantoms.bool(false),
      field("lambda", encodeCaseIsUnitLambdaArm),
      field("record", lambda("r",
        applyP("hydra.lib.equality.equal",
          applyP("hydra.lib.lists.length", CoreDsl.recordFields(v("r"))),
          int32(0)))),
      field("unit", constant(Phantoms.bool(true))))

  private val encodeCaseIsUnitFromType =
    casesWithDefault("hydra.core.Type",
      applyP("hydra.strip.deannotateType", v("dom")),
      Phantoms.bool(false),
      field("unit", constant(Phantoms.bool(true))),
      field("record", lambda("rt",
        applyP("hydra.lib.equality.equal",
          applyP("hydra.lib.lists.length", v("rt")),
          int32(0)))))

  private val encodeCaseIsUnitValue =
    applyP("hydra.lib.optionals.cases",
      applyP("hydra.lib.maps.lookup", v("fname"), v("ftypes")),
      encodeCaseIsUnitFromTerm,
      lambda("dom", encodeCaseIsUnitFromType))

  private val encodeCaseShortTypeName =
    applyP("hydra.lib.optionals.fromOptional",
      string("x"),
      applyP("hydra.lib.lists.maybeLast",
        applyP("hydra.lib.strings.splitOn",
          string("."),
          applyP("hydra.lib.optionals.cases",
            v("sn"),
            string("x"),
            lambda("n", CoreDsl.unName(v("n")))))))

  private val encodeCaseLamParamSuffix =
    casesWithDefault("hydra.core.Term",
      applyP("hydra.strip.deannotateAndDetypeTerm", v("fterm")),
      string(""),
      field("lambda", lambda("lam",
        Phantoms.let(Seq(
          field("rawName", CoreDsl.unName(CoreDsl.lambdaParameter(v("lam")))),
          field("safeName",
            applyP("hydra.lib.strings.fromList",
              applyP("hydra.lib.lists.map",
                lambda("c",
                  applyP("hydra.lib.logic.ifElse",
                    applyP("hydra.lib.equality.equal", v("c"), int32(39)),
                    int32(95),
                    v("c"))),
                applyP("hydra.lib.strings.toList", v("rawName")))))),
          applyP("hydra.lib.strings.cat2", string("_"), v("safeName"))))))

  private val encodeCaseDomainIsUnit =
    casesWithDefault("hydra.core.Term",
      applyP("hydra.strip.deannotateAndDetypeTerm", v("fterm")),
      Phantoms.bool(true),
      field("lambda", lambda("lam",
        applyP("hydra.lib.optionals.cases",
          CoreDsl.lambdaDomain(v("lam")),
          Phantoms.bool(true),
          lambda("dom",
            applyP("hydra.lib.equality.equal", v("dom"), CoreDsl.typeUnit))))))

  private val encodeCasePatArgs =
    applyP("hydra.lib.logic.ifElse",
      v("isUnit"),
      applyP("hydra.lib.logic.ifElse",
        v("domainIsUnit"),
        emptyList,
        list(inject("hydra.scala.syntax.Pat", "wildcard", Phantoms.unit))),
      list(applyP("hydra.scala.utils.svar", v("v"))))

  private val encodeCasePat =
    inject("hydra.scala.syntax.Pat", "extract",
      ScalaSyntax.extractPat(
        applyP("hydra.scala.utils.sname",
          applyP("hydra.scala.utils.qualifyUnionFieldName",
            string("MATCHED."), v("sn"), v("fname"))))(
        v("patArgs")))

  private val encodeCaseBindAndReturn =
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("applied")),
      lambda("body",
        Phantoms.right(
          ScalaSyntax.`case`(v("pat"))(nothing)(v("body")))))

  private val encodeCaseVName =
    CoreDsl.name(
      applyP("hydra.lib.strings.cat",
        list(string("v_"), v("shortTypeName"), string("_"),
          CoreDsl.unName(v("fname")), v("lamParamSuffix"))))

  private val encodeCaseBody = lambda("cx", lambda("g", lambda("ftypes", lambda("sn", lambda("f",
    Phantoms.let(Seq(
      field("fname", CoreDsl.caseAlternativeName(v("f"))),
      field("fterm", CoreDsl.caseAlternativeHandler(v("f"))),
      field("isUnit", encodeCaseIsUnitValue),
      field("shortTypeName", encodeCaseShortTypeName),
      field("lamParamSuffix", encodeCaseLamParamSuffix),
      field("v", encodeCaseVName),
      field("domainIsUnit", encodeCaseDomainIsUnit),
      field("patArgs", encodeCasePatArgs),
      field("pat", encodeCasePat),
      field("applied", applyP("hydra.scala.coder.applyVar", v("fterm"), v("v")))),
      encodeCaseBindAndReturn))))))

  lazy val encodeCaseDef: Definition =
    Phantoms.`def`(NS, "encodeCase",
      doc("Encode a case branch", encodeCaseBody))

  // ===== encodeFunction — Coder.hs:288-403, full translation =====

  // Lambda-arm: encode body, then encode (or skip) the parameter type annotation.
  private val encodeFunctionMdomComputation =
    Phantoms.let(Seq(
      field("freeVars",
        applyP("hydra.variables.freeVariablesInType", v("dom"))),
      field("unqualifiedFreeVars",
        applyP("hydra.lib.sets.fromList",
          applyP("hydra.lib.lists.filter",
            lambda("n",
              applyP("hydra.lib.logic.not",
                applyP("hydra.lib.lists.elem",
                  int32(46),
                  applyP("hydra.lib.strings.toList", CoreDsl.unName(v("n")))))),
            applyP("hydra.lib.sets.toList", v("freeVars"))))),
      field("unresolvedVars",
        applyP("hydra.lib.sets.difference",
          v("unqualifiedFreeVars"),
          GraphDsl.graphTypeVariables(v("g"))))),
      applyP("hydra.lib.logic.ifElse",
        applyP("hydra.lib.sets.null", v("unresolvedVars")),
        just(v("dom")),
        nothing))

  private val encodeFunctionSbodySdomBinder = lambda("sbody",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.lib.optionals.cases",
        v("mdom"),
        applyP("hydra.scala.coder.findSdom", v("cx"), v("g"), v("meta")),
        lambda("dom",
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("dom")),
            lambda("sdom", Phantoms.right(just(v("sdom"))))))),
      lambda("sdom",
        Phantoms.right(
          applyP("hydra.scala.utils.slambda",
            v("v"), v("sbody"), v("sdom"))))))

  private val encodeFunctionLambdaArm = lambda("lam",
    Phantoms.let(Seq(
      field("param", CoreDsl.lambdaParameter(v("lam"))),
      field("v", applyP("hydra.scala.utils.scalaEscapeName", CoreDsl.unName(v("param")))),
      field("body", CoreDsl.lambdaBody(v("lam"))),
      field("rawMdom", CoreDsl.lambdaDomain(v("lam"))),
      field("mdom",
        applyP("hydra.lib.optionals.bind", v("rawMdom"),
          lambda("dom", encodeFunctionMdomComputation)))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("body")),
        encodeFunctionSbodySdomBinder)))

  // Unwrap-arm
  private val encodeFunctionUnwrapNoArg =
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.findSdom", v("cx"), v("g"), v("meta")),
      lambda("sdom",
        Phantoms.right(
          applyP("hydra.scala.utils.slambda",
            string("x"),
            applyP("hydra.scala.utils.sname", string("x")),
            v("sdom")))))

  private val encodeFunctionUnwrapArm = lambda("name",
    applyP("hydra.lib.optionals.cases",
      v("arg"),
      encodeFunctionUnwrapNoArg,
      lambda("a",
        applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("a")))))

  // Project-arm
  private val encodeFunctionProjectUnapplied =
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.lib.eithers.either",
        constant(Phantoms.right(nothing)),
        lambda("msdom", Phantoms.right(v("msdom"))),
        applyP("hydra.scala.coder.findSdom", v("cx"), v("g"), v("meta"))),
      lambda("msdom",
        Phantoms.right(
          applyP("hydra.scala.utils.slambda",
            v("pv"),
            inject("hydra.scala.syntax.Data", "ref",
              inject("hydra.scala.syntax.RefData", "select",
                ScalaSyntax.selectData(
                  applyP("hydra.scala.utils.sname", v("pv")))(
                  ScalaSyntax.nameData(ScalaSyntax.predefString(v("fname")))))),
            v("msdom")))))

  private val encodeFunctionProjectApplied = lambda("a",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("a")),
      lambda("sa",
        Phantoms.right(
          inject("hydra.scala.syntax.Data", "ref",
            inject("hydra.scala.syntax.RefData", "select",
              ScalaSyntax.selectData(v("sa"))(
                ScalaSyntax.nameData(ScalaSyntax.predefString(v("fname"))))))))))

  private val encodeFunctionProjectArm = lambda("proj",
    Phantoms.let(Seq(
      field("fname",
        applyP("hydra.scala.utils.scalaEscapeName",
          CoreDsl.unName(CoreDsl.projectionFieldName(v("proj"))))),
      field("typeName", CoreDsl.projectionTypeName(v("proj"))),
      field("pv", string("x"))),
      applyP("hydra.lib.optionals.cases",
        v("arg"),
        encodeFunctionProjectUnapplied,
        encodeFunctionProjectApplied)))

  // Cases-arm
  private val encodeFunctionCasesFieldCases =
    applyP("hydra.lib.eithers.mapList",
      lambda("f",
        applyP("hydra.scala.coder.encodeCase",
          v("cx"), v("g"), v("ftypes"), v("sn"), v("f"))),
      v("cases"))

  private val encodeFunctionCasesAddDefaultThen = lambda("fieldCases",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.lib.optionals.cases",
        v("dflt"),
        Phantoms.right(v("fieldCases")),
        lambda("dfltTerm",
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("dfltTerm")),
            lambda("sdflt",
              Phantoms.right(
                applyP("hydra.lib.lists.concat2",
                  v("fieldCases"),
                  list(ScalaSyntax.`case`(
                    inject("hydra.scala.syntax.Pat", "wildcard", Phantoms.unit))(
                    nothing)(v("sdflt"))))))))),
      lambda("scases",
        applyP("hydra.lib.optionals.cases",
          v("arg"),
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.scala.coder.findSdom", v("cx"), v("g"), v("meta")),
            lambda("sdom",
              Phantoms.right(
                applyP("hydra.scala.utils.slambda",
                  v("v"),
                  inject("hydra.scala.syntax.Data", "match",
                    ScalaSyntax.matchData(
                      applyP("hydra.scala.utils.sname", v("v")))(
                      v("scases"))),
                  v("sdom"))))),
          lambda("a",
            applyP("hydra.lib.eithers.bind",
              applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("a")),
              lambda("sa",
                Phantoms.right(
                  inject("hydra.scala.syntax.Data", "match",
                    ScalaSyntax.matchData(v("sa"))(v("scases")))))))))))

  private val encodeFunctionCasesArm = lambda("cs",
    Phantoms.let(Seq(
      field("v", string("v")),
      field("tname", CoreDsl.caseStatementTypeName(v("cs"))),
      field("dom", CoreDsl.typeVariable(v("tname"))),
      field("sn", applyP("hydra.scala.utils.nameOfType", v("g"), v("dom"))),
      field("cases", CoreDsl.caseStatementCases(v("cs"))),
      field("dflt", CoreDsl.caseStatementDefault(v("cs"))),
      field("ftypes",
        applyP("hydra.lib.eithers.either",
          constant(v("hydra.lib.maps.empty")),
          lambda("x_", v("x_")),
          applyP("hydra.resolution.fieldTypes", v("cx"), v("g"), v("dom"))))),
      applyP("hydra.lib.eithers.bind",
        encodeFunctionCasesFieldCases,
        encodeFunctionCasesAddDefaultThen)))

  private val encodeFunctionBody = lambda("cx", lambda("g", lambda("meta", lambda("funTerm", lambda("arg",
    casesWithDefault("hydra.core.Term",
      applyP("hydra.strip.deannotateAndDetypeTerm", v("funTerm")),
      errorLeft("unsupported function"),
      field("lambda", encodeFunctionLambdaArm),
      field("unwrap", encodeFunctionUnwrapArm),
      field("project", encodeFunctionProjectArm),
      field("cases", encodeFunctionCasesArm)))))))

  lazy val encodeFunctionDef: Definition =
    Phantoms.`def`(NS, "encodeFunction",
      doc("Encode a Hydra function-valued term (lambda, project, cases, or unwrap) as a Scala expression", encodeFunctionBody))

  // ===== encodeTerm — Coder.hs:519-785, full translation =====
  // Largest function in the module: ~15 arms over the Term union. Each arm
  // extracted as a named private val. Some arms delegate to encodeFunction.

  private val encodeTermLiteralArm = lambda("v",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.encodeLiteral", v("cx"), v("g"), v("v")),
      lambda("slit",
        Phantoms.let("litData", inject("hydra.scala.syntax.Data", "lit", v("slit")),
          casesWithDefault("hydra.core.Literal",
            v("v"),
            Phantoms.right(v("litData")),
            field("decimal", constant(Phantoms.right(
              applyP("hydra.scala.utils.sapply",
                applyP("hydra.scala.utils.sname", string("BigDecimal")),
                list(v("litData")))))),
            field("integer", lambda("iv",
              casesWithDefault("hydra.core.IntegerValue",
                v("iv"),
                Phantoms.right(v("litData")),
                field("bigint", lambda("bi",
                  Phantoms.right(
                    applyP("hydra.scala.utils.sapply",
                      applyP("hydra.scala.utils.sname", string("BigInt")),
                      list(inject("hydra.scala.syntax.Data", "lit",
                        inject("hydra.scala.syntax.Lit", "string",
                          applyP("hydra.lib.literals.showBigint", v("bi"))))))))),
                field("uint64", lambda("ui",
                  Phantoms.right(
                    applyP("hydra.scala.utils.sapply",
                      applyP("hydra.scala.utils.sname", string("BigInt")),
                      list(inject("hydra.scala.syntax.Data", "lit",
                        inject("hydra.scala.syntax.Lit", "string",
                          applyP("hydra.lib.literals.showBigint",
                            applyP("hydra.lib.literals.uint64ToBigint", v("ui"))))))))))))))))))

  // Variable arm: produce a properly-qualified Scala name.
  private val encodeTermVariableArm = lambda("v",
    Phantoms.let(Seq(
      field("fullName", CoreDsl.unName(v("v"))),
      field("localName", applyP("hydra.names.localNameOf", v("v"))),
      field("parts", applyP("hydra.lib.strings.splitOn", string("."), v("fullName"))),
      field("numParts", applyP("hydra.lib.lists.length", v("parts"))),
      field("escaped",
        applyP("hydra.lib.logic.ifElse",
          applyP("hydra.lib.equality.lte", v("numParts"), int32(1)),
          applyP("hydra.scala.utils.scalaEscapeName", v("fullName")),
          applyP("hydra.lib.logic.ifElse",
            applyP("hydra.lib.equality.equal", v("numParts"), int32(2)),
            applyP("hydra.lib.strings.cat2",
              applyP("hydra.lib.optionals.fromOptional",
                v("fullName"),
                applyP("hydra.lib.lists.maybeHead", v("parts"))),
              applyP("hydra.lib.strings.cat2",
                string("."),
                applyP("hydra.scala.utils.scalaEscapeName", v("localName")))),
            applyP("hydra.lib.strings.intercalate",
              string("."),
              applyP("hydra.lib.lists.concat2",
                applyP("hydra.lib.lists.take",
                  applyP("hydra.lib.math.sub", v("numParts"), int32(1)),
                  v("parts")),
                list(applyP("hydra.scala.utils.scalaEscapeName", v("localName"))))))))),
      Phantoms.right(applyP("hydra.scala.utils.sname", v("escaped")))))

  private val encodeTermUnitArm = constant(
    Phantoms.right(
      inject("hydra.scala.syntax.Data", "lit",
        inject("hydra.scala.syntax.Lit", "unit", Phantoms.unit))))

  // Helper: delegate to encodeFunction for lambda/project/cases/unwrap arms.
  private def encodeTermFunDelegate(argTerm: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.scala.coder.encodeFunction",
      v("cx"), v("g"),
      applyP("hydra.annotations.termAnnotationInternal", argTerm),
      argTerm,
      nothing)

  private val encodeTermLambdaArm = constant(encodeTermFunDelegate(v("term")))
  private val encodeTermProjectArm = constant(encodeTermFunDelegate(v("term")))
  private val encodeTermCasesArm = constant(encodeTermFunDelegate(v("term")))
  private val encodeTermUnwrapArm = constant(encodeTermFunDelegate(v("term")))

  private val encodeTermAnnotatedArm = lambda("at",
    applyP("hydra.scala.coder.encodeTerm",
      v("cx"), v("g"),
      CoreDsl.annotatedTermBody(v("at"))))

  private val encodeTermTypeLambdaArm = lambda("tl",
    applyP("hydra.scala.coder.encodeTerm",
      v("cx"),
      applyP("hydra.scoping.extendGraphForTypeLambda", v("g"), v("tl")),
      CoreDsl.typeLambdaBody(v("tl"))))

  private val encodeTermWrapArm = lambda("wt",
    applyP("hydra.scala.coder.encodeTerm",
      v("cx"), v("g"),
      CoreDsl.wrappedTermBody(v("wt"))))

  private val encodeTermListArm = lambda("els",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.lib.eithers.mapList",
        lambda("e", applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("e"))),
        v("els")),
      lambda("sels",
        Phantoms.right(
          applyP("hydra.scala.utils.sapply",
            applyP("hydra.scala.utils.sname", string("Seq")),
            v("sels"))))))

  private val encodeTermSetArm = lambda("s",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.lib.eithers.mapList",
        lambda("e", applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("e"))),
        applyP("hydra.lib.sets.toList", v("s"))),
      lambda("sels",
        Phantoms.right(
          applyP("hydra.scala.utils.sapply",
            applyP("hydra.scala.utils.sname", string("scala.collection.immutable.Set")),
            v("sels"))))))

  private val encodeTermPairArm = lambda("p",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.encodeTerm",
        v("cx"), v("g"),
        applyP("hydra.lib.pairs.first", v("p"))),
      lambda("sf",
        applyP("hydra.lib.eithers.bind",
          applyP("hydra.scala.coder.encodeTerm",
            v("cx"), v("g"),
            applyP("hydra.lib.pairs.second", v("p"))),
          lambda("ss",
            Phantoms.right(
              applyP("hydra.scala.utils.sapply",
                applyP("hydra.scala.utils.sname", string("Tuple2")),
                list(v("sf"), v("ss")))))))))

  private val encodeTermOptionalArm = lambda("m",
    applyP("hydra.lib.optionals.cases",
      v("m"),
      Phantoms.right(applyP("hydra.scala.utils.sname", string("None"))),
      lambda("t",
        applyP("hydra.lib.eithers.bind",
          applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("t")),
          lambda("s",
            Phantoms.right(
              applyP("hydra.scala.utils.sapply",
                applyP("hydra.scala.utils.sname", string("Some")),
                list(v("s")))))))))

  private val encodeTermEitherArm = lambda("e",
    applyP("hydra.lib.eithers.either",
      lambda("l",
        applyP("hydra.lib.eithers.bind",
          applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("l")),
          lambda("sl",
            Phantoms.right(
              applyP("hydra.scala.utils.sapply",
                applyP("hydra.scala.utils.sname", string("Left")),
                list(v("sl"))))))),
      lambda("r",
        applyP("hydra.lib.eithers.bind",
          applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("r")),
          lambda("sr",
            Phantoms.right(
              applyP("hydra.scala.utils.sapply",
                applyP("hydra.scala.utils.sname", string("Right")),
                list(v("sr"))))))),
      v("e")))

  private val encodeTermMapArm = lambda("m",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.lib.eithers.mapList",
        lambda("kv",
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.scala.coder.encodeTerm",
              v("cx"), v("g"),
              applyP("hydra.lib.pairs.first", v("kv"))),
            lambda("sk",
              applyP("hydra.lib.eithers.bind",
                applyP("hydra.scala.coder.encodeTerm",
                  v("cx"), v("g"),
                  applyP("hydra.lib.pairs.second", v("kv"))),
                lambda("sv",
                  Phantoms.right(
                    applyP("hydra.scala.utils.sassign", v("sk"), v("sv")))))))),
        applyP("hydra.lib.maps.toList", v("m"))),
      lambda("spairs",
        Phantoms.right(
          applyP("hydra.scala.utils.sapply",
            applyP("hydra.scala.utils.sname", string("Map")),
            v("spairs"))))))

  private val encodeTermRecordArm = lambda("rec",
    Phantoms.let(Seq(
      field("rname", CoreDsl.recordTypeName(v("rec"))),
      field("fields", CoreDsl.recordFields(v("rec"))),
      field("n", applyP("hydra.scala.utils.scalaTypeName", Phantoms.bool(true), v("rname")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.lib.eithers.mapList",
          lambda("f",
            applyP("hydra.scala.coder.encodeTerm",
              v("cx"), v("g"), CoreDsl.fieldTerm(v("f")))),
          v("fields")),
        lambda("args",
          Phantoms.right(
            applyP("hydra.scala.utils.sapply",
              applyP("hydra.scala.utils.sname", v("n")),
              v("args")))))))

  private val encodeTermInjectArm = lambda("inj",
    Phantoms.let(Seq(
      field("sn", CoreDsl.injectionTypeName(v("inj"))),
      field("fn", CoreDsl.fieldName(CoreDsl.injectionField(v("inj")))),
      field("ft", CoreDsl.fieldTerm(CoreDsl.injectionField(v("inj")))),
      field("lhs",
        applyP("hydra.scala.utils.sname",
          applyP("hydra.scala.utils.qualifyUnionFieldName",
            string("UNION."), just(v("sn")), v("fn")))),
      field("unionFtypes",
        applyP("hydra.lib.eithers.either",
          constant(v("hydra.lib.maps.empty")),
          lambda("x_", v("x_")),
          applyP("hydra.resolution.fieldTypes",
            v("cx"), v("g"),
            CoreDsl.typeVariable(v("sn")))))),
      applyP("hydra.lib.logic.ifElse",
        applyP("hydra.lib.optionals.cases",
          applyP("hydra.lib.maps.lookup", v("fn"), v("unionFtypes")),
          casesWithDefault("hydra.core.Term",
            applyP("hydra.strip.deannotateAndDetypeTerm", v("ft")),
            Phantoms.bool(false),
            field("unit", constant(Phantoms.bool(true))),
            field("record", lambda("rec",
              applyP("hydra.lib.equality.equal",
                applyP("hydra.lib.lists.length", CoreDsl.recordFields(v("rec"))),
                int32(0))))),
          lambda("dom",
            casesWithDefault("hydra.core.Type",
              applyP("hydra.strip.deannotateType", v("dom")),
              Phantoms.bool(false),
              field("unit", constant(Phantoms.bool(true))),
              field("record", lambda("rt",
                applyP("hydra.lib.equality.equal",
                  applyP("hydra.lib.lists.length", v("rt")),
                  int32(0))))))),
        Phantoms.right(v("lhs")),
        applyP("hydra.lib.eithers.bind",
          applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("ft")),
          lambda("sarg",
            Phantoms.right(
              applyP("hydra.scala.utils.sapply",
                v("lhs"),
                list(v("sarg")))))))))

  // Let arm
  private val encodeTermLetArm = lambda("lt",
    Phantoms.let(Seq(
      field("bindings", CoreDsl.letBindings(v("lt"))),
      field("body", CoreDsl.letBody(v("lt"))),
      field("gLet",
        applyP("hydra.scoping.extendGraphForLet",
          lambda("g", lambda("b",
            applyP("hydra.lib.logic.ifElse",
              applyP("hydra.predicates.isComplexBinding", v("g"), v("b")),
              just(Phantoms.metaBool(true)),
              nothing))),
          v("g"),
          v("lt")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.lib.eithers.mapList",
          applyP("hydra.scala.coder.encodeLetBinding",
            v("cx"), v("gLet"),
            GraphDsl.graphTypeVariables(v("gLet"))),
          v("bindings")),
        lambda("sbindings",
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.scala.coder.encodeTerm",
              v("cx"), v("gLet"), v("body")),
            lambda("sbody",
              Phantoms.right(
                inject("hydra.scala.syntax.Data", "block",
                  ScalaSyntax.blockData(
                    applyP("hydra.lib.lists.concat2",
                      v("sbindings"),
                      list(inject("hydra.scala.syntax.Stat", "term", v("sbody")))))))))))))

  // Application arm — full translation with beta-reduction optimizations for
  // Application(Cases(cs), v) → match patterns and Application(Project(f), arg) → arg.f.

  // Default: encode as plain Scala function application.
  private val encodeTermApplicationDefault =
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("fun")),
      lambda("sfun",
        applyP("hydra.lib.eithers.bind",
          applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("arg")),
          lambda("sarg",
            Phantoms.right(
              applyP("hydra.scala.utils.sapply", v("sfun"), list(v("sarg"))))))))

  // Project arm: emit `arg.fname` (RefData.select).
  private val encodeTermApplicationProjectArm = lambda("proj",
    Phantoms.let("fname",
      applyP("hydra.scala.utils.scalaEscapeName",
        CoreDsl.unName(CoreDsl.projectionFieldName(v("proj")))),
      applyP("hydra.lib.eithers.bind",
        applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("arg")),
        lambda("sarg",
          Phantoms.right(
            inject("hydra.scala.syntax.Data", "ref",
              inject("hydra.scala.syntax.RefData", "select",
                ScalaSyntax.selectData(v("sarg"))(
                  ScalaSyntax.nameData(ScalaSyntax.predefString(v("fname")))))))))))

  // Cases arm: beta-reduce to `arg match { cs }` via encodeFunction.
  private val encodeTermApplicationCasesArm = constant(
    applyP("hydra.scala.coder.encodeFunction",
      v("cx"), v("g"),
      applyP("hydra.annotations.termAnnotationInternal", v("fun")),
      v("fun"),
      just(v("arg"))))

  // Lambda arm: check if body is Application(Cases(cs), lamParam). If so, beta-reduce.
  // Inner inner case for Application(Cases, lamParam) detection.
  private val encodeTermApplicationLambdaInnerInnerCasesArm = constant(
    applyP("hydra.scala.coder.encodeFunction",
      v("cx"), v("g"),
      applyP("hydra.annotations.termAnnotationInternal", v("innerFun")),
      v("innerFun"),
      just(v("arg"))))

  // Inner dispatch on lamBody's structure.
  private val encodeTermApplicationLambdaInnerInnerDispatch =
    casesWithDefault("hydra.core.Term",
      applyP("hydra.strip.deannotateAndDetypeTerm", v("innerFun")),
      encodeTermApplicationDefault,
      field("cases", encodeTermApplicationLambdaInnerInnerCasesArm))

  // For lambda body of Application kind: inspect inner application's fun.
  private val encodeTermApplicationLambdaInnerArm = lambda("innerApp",
    Phantoms.let("innerFun", CoreDsl.applicationFunction(v("innerApp")),
      encodeTermApplicationLambdaInnerInnerDispatch))

  private val encodeTermApplicationLambdaInnerDispatch =
    casesWithDefault("hydra.core.Term",
      applyP("hydra.strip.deannotateAndDetypeTerm", v("lamBody")),
      encodeTermApplicationDefault,
      field("application", encodeTermApplicationLambdaInnerArm))

  private val encodeTermApplicationLambdaArm = lambda("lam",
    Phantoms.let("lamBody", CoreDsl.lambdaBody(v("lam")),
      encodeTermApplicationLambdaInnerDispatch))

  private val encodeTermApplicationFunDispatch =
    casesWithDefault("hydra.core.Term",
      applyP("hydra.strip.deannotateAndDetypeTerm", v("fun")),
      encodeTermApplicationDefault,
      field("lambda", encodeTermApplicationLambdaArm),
      field("project", encodeTermApplicationProjectArm),
      field("cases", encodeTermApplicationCasesArm))

  private val encodeTermApplicationArm = lambda("app",
    Phantoms.let(Seq(
      field("fun", CoreDsl.applicationFunction(v("app"))),
      field("arg", CoreDsl.applicationArgument(v("app")))),
      encodeTermApplicationFunDispatch))

  // typeApplication arm — full translation. Two recursive lambdas (collectTypeArgs and
  // collectTypeLambdas) are let-rec bound; Hydra's let is recursive by default, so the
  // lambda body can reference its own let-bound name.

  private val encodeTermTypeAppVariableInner = lambda("pname",
    applyP("hydra.lib.eithers.bind",
      applyP("hydra.lib.eithers.mapList",
        lambda("targ", applyP("hydra.scala.coder.encodeType", v("cx"), v("g"), v("targ"))),
        v("typeArgs")),
      lambda("stypeArgs",
        applyP("hydra.lib.optionals.cases",
          applyP("hydra.lib.maps.lookup", v("pname"),
            GraphDsl.graphPrimitives(v("g"))),
          applyP("hydra.lib.eithers.bind",
            applyP("hydra.scala.coder.encodeTerm",
              v("cx"), v("g"), v("substitutedBody")),
            lambda("svar",
              Phantoms.right(
                applyP("hydra.scala.utils.sapplyTypes",
                  v("svar"), v("stypeArgs"))))),
          lambda("_prim",
            Phantoms.right(
              applyP("hydra.scala.utils.sapplyTypes",
                applyP("hydra.scala.utils.sprim", v("pname")),
                v("stypeArgs"))))))))

  private val encodeTermTypeAppInnerDispatch =
    casesWithDefault("hydra.core.Term",
      applyP("hydra.strip.deannotateTerm", v("substitutedBody")),
      applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("substitutedBody")),
      field("project", constant(
        applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("substitutedBody")))),
      field("cases", constant(
        applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("substitutedBody")))),
      field("unwrap", constant(
        applyP("hydra.scala.coder.encodeTerm", v("cx"), v("g"), v("substitutedBody")))),
      field("variable", encodeTermTypeAppVariableInner))

  private val encodeTermTypeApplicationArm = lambda("ta",
    Phantoms.let(Seq(
      field("collectTypeArgs",
        lambda("t", lambda("acc",
          casesWithDefault("hydra.core.Term",
            applyP("hydra.strip.deannotateTerm", v("t")),
            Phantoms.pair(v("acc"), v("t")),
            field("typeApplication", lambda("ta2",
              applyN(v("collectTypeArgs"),
                CoreDsl.typeApplicationTermBody(v("ta2")),
                applyP("hydra.lib.lists.cons",
                  CoreDsl.typeApplicationTermType(v("ta2")),
                  v("acc"))))))))),
      field("collected",
        applyN(v("collectTypeArgs"),
          CoreDsl.typeApplicationTermBody(v("ta")),
          list(CoreDsl.typeApplicationTermType(v("ta"))))),
      field("typeArgs", applyP("hydra.lib.pairs.first", v("collected"))),
      field("innerTerm", applyP("hydra.lib.pairs.second", v("collected"))),
      field("collectTypeLambdas",
        lambda("t", lambda("acc",
          casesWithDefault("hydra.core.Term",
            applyP("hydra.strip.deannotateTerm", v("t")),
            Phantoms.pair(v("acc"), v("t")),
            field("typeLambda", lambda("tl",
              applyN(v("collectTypeLambdas"),
                CoreDsl.typeLambdaBody(v("tl")),
                applyP("hydra.lib.lists.cons",
                  CoreDsl.typeLambdaParameter(v("tl")),
                  v("acc"))))))))),
      field("tlCollected",
        applyN(v("collectTypeLambdas"), v("innerTerm"), emptyList)),
      field("typeParams", applyP("hydra.lib.pairs.first", v("tlCollected"))),
      field("bodyAfterTypeLambdas", applyP("hydra.lib.pairs.second", v("tlCollected"))),
      field("substitutedBody", v("bodyAfterTypeLambdas"))),
      encodeTermTypeAppInnerDispatch))

  // Field order matches Haskell DSL's Coder.hs:524-785 (cases ordering matters for
  // generated Scala match-case structure).
  private val encodeTermCases = casesWithDefault("hydra.core.Term",
    applyP("hydra.strip.deannotateTerm", v("term")),
    errorLeft("unexpected term"),
    field("typeApplication", encodeTermTypeApplicationArm),
    field("typeLambda", encodeTermTypeLambdaArm),
    field("application", encodeTermApplicationArm),
    field("lambda", encodeTermLambdaArm),
    field("project", encodeTermProjectArm),
    field("cases", encodeTermCasesArm),
    field("unwrap", encodeTermUnwrapArm),
    field("list", encodeTermListArm),
    field("literal", encodeTermLiteralArm),
    field("map", encodeTermMapArm),
    field("wrap", encodeTermWrapArm),
    field("optional", encodeTermOptionalArm),
    field("record", encodeTermRecordArm),
    field("set", encodeTermSetArm),
    field("inject", encodeTermInjectArm),
    field("variable", encodeTermVariableArm),
    field("annotated", encodeTermAnnotatedArm),
    field("either", encodeTermEitherArm),
    field("pair", encodeTermPairArm),
    field("unit", encodeTermUnitArm),
    field("let", encodeTermLetArm))

  private val encodeTermBody = lambda("cx", lambda("g", lambda("term0",
    Phantoms.let("term", applyP("hydra.scala.coder.stripWrapEliminations", v("term0")),
      encodeTermCases))))

  lazy val encodeTermDef: Definition =
    Phantoms.`def`(NS, "encodeTerm",
      doc("Encode a Hydra term as a Scala expression", encodeTermBody))

  // ===== Module assembly — order matches Haskell `definitions` list =====

  val DEFINITIONS: Seq[Definition] = Seq(
    applyVarDef,
    constructModuleDef,
    dropDomainsDef,
    encodeCaseDef,
    encodeComplexTermDefDef,
    encodeFunctionDef,
    encodeLetBindingDef,
    encodeLiteralDef,
    encodeLocalDefDef,
    encodeTermDef,
    encodeTermDefinitionDef,
    encodeTypeDef,
    encodeTypeDefinitionDef,
    encodeTypedParamDef,
    encodeUntypeApplicationTermDef,
    extractBodyDef,
    extractCodomainDef,
    extractDomainsDef,
    extractLetBindingsDef,
    extractParamsDef,
    fieldToEnumCaseDef,
    fieldToParamDef,
    findDomainDef,
    findImportsDef,
    findSdomDef,
    moduleToScalaDef,
    stripWrapEliminationsDef,
    toElImportDef,
    toPrimImportDef,
    typeParamToTypeVarDef)

  val module_ : Module = Module(
    name = NS,
    metadata = Some(EntityMetadata(
      description = Some("Scala code generator: converts Hydra modules to Scala source code"),
      comments = Seq.empty,
      seeAlso = Seq.empty,
      lifecycle = None)),
    dependencies = DEPS.map(Helpers.unqualifiedDep),
    definitions = DEFINITIONS)

end Coder
