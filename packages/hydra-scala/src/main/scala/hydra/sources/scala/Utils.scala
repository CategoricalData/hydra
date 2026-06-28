package hydra.sources.scala

import hydra.overlay.scala.dsl.{Helpers, Phantoms}
import hydra.overlay.scala.dsl.Phantoms.{`var` => v, prim, applyP, lambda, let, field, string, int32, bool, list, nothing, just, doc, casesWithDefault, project, unwrap, wrap}
import hydra.packaging.{Definition, EntityMetadata, Module, ModuleName}
import hydra.typed.TypedTerm

import hydra.dsl.scala.syntax as ScalaSyntax
import hydra.dsl.{core => CoreDsl, packaging => PackagingDsl, util => UtilDsl}

/**
 * Utility functions for constructing Scala AST nodes.
 */
object Utils:

  val NS: ModuleName = "hydra.scala.utils"

  /** Dependencies: matches Haskell `[scalaLanguage, names, formatting] ++ (scalaSyntax : kernelTypesModuleNames)`.
   *  kernelTypesModuleNames starts with hydra.paths; baseline ordering preserved exactly. */
  private val DEPS: Seq[ModuleName] = Seq(
    "hydra.scala.language",
    "hydra.names",
    "hydra.formatting",
    "hydra.scala.syntax",
    "hydra.paths",
    "hydra.ast", "hydra.coders", "hydra.core",
    "hydra.error.checking", "hydra.error.core", "hydra.error.file",
    "hydra.error.packaging", "hydra.error.system",
    "hydra.errors", "hydra.file", "hydra.graph", "hydra.json.model",
    "hydra.packaging", "hydra.parsing",
    "hydra.query", "hydra.relational", "hydra.system", "hydra.tabular",
    "hydra.testing", "hydra.time", "hydra.topology", "hydra.typed",
    "hydra.typing", "hydra.util", "hydra.validation", "hydra.variants")

  // ===== Local helpers — shorthand FQN references =====

  // Local re-export: references the scalaReservedWords binding in THIS module
  // (which itself proxies hydra.scala.language.scalaReservedWords). Matches the
  // Haskell DSL's `scalaReservedWordsRef` which is a local TypedTermDefinition.
  private val scalaReservedWordsRefVar: TypedTerm[Set[String]] =
    v("hydra.scala.utils.scalaReservedWords")

  /** 2-arg string concat — Haskell's (++) operator on strings desugars to this. */
  private def cat2(a: TypedTerm[String], b: TypedTerm[String]): TypedTerm[String] =
    applyP("hydra.lib.strings.cat2", a, b)

  // ===== Definitions (alphabetical by camelCase, matching Haskell file order) =====

  lazy val nameOfTypeDef: Definition =
    val body = lambda("cx", lambda("t",
      casesWithDefault("hydra.core.Type",
        applyP("hydra.strip.deannotateType", v("t")),
        nothing,
        field("variable", lambda("name", just(v("name")))),
        field("forall", lambda("ft",
          applyP("hydra.scala.utils.nameOfType",
            v("cx"),
            CoreDsl.forallTypeBody(v("ft"))))))))
    Phantoms.`def`(NS, "nameOfType",
      doc("Extract the name from a type, if it is a named type", body))

  lazy val qualifyUnionFieldNameDef: Definition =
    val body = lambda("dlft", lambda("sname", lambda("fname",
      cat2(
        applyP("hydra.lib.optionals.cases",
          v("sname"),
          v("dlft"),
          lambda("n",
            cat2(
              applyP("hydra.scala.utils.scalaTypeName", bool(true), v("n")),
              string(".")))),
        applyP("hydra.scala.utils.scalaEscapeName",
          CoreDsl.unName(v("fname")))))))
    Phantoms.`def`(NS, "qualifyUnionFieldName",
      doc("Qualify a union field name, optionally prefixing with the Scala type name", body))

  lazy val sapplyDef: Definition =
    val body = lambda("fun", lambda("args",
      ScalaSyntax.dataApply(ScalaSyntax.applyData(v("fun"))(v("args")))))
    Phantoms.`def`(NS, "sapply",
      doc("Apply a Scala data expression to a list of arguments", body))

  lazy val sapplyTypesDef: Definition =
    val body = lambda("fun", lambda("typeArgs",
      let(Seq(
        field("typeToStr", lambda("t",
          applyP("hydra.scala.utils.typeToString", v("t")))),
        field("typeStrings",
          applyP("hydra.lib.lists.map", v("typeToStr"), v("typeArgs"))),
        field("typeArgStr",
          applyP("hydra.lib.strings.cat",
            list(
              string("["),
              applyP("hydra.lib.strings.intercalate", string(", "), v("typeStrings")),
              string("]"))))),
        casesWithDefault("hydra.scala.syntax.Data",
          v("fun"), v("fun"),
          field("ref", lambda("ref",
            casesWithDefault("hydra.scala.syntax.RefData",
              v("ref"), v("fun"),
              field("name", lambda("dn",
                let(Seq(
                  field("nameStr", ScalaSyntax.nameDataValue(v("dn"))),
                  field("rawName", ScalaSyntax.unPredefString(v("nameStr")))),
                  applyP("hydra.scala.utils.sname",
                    cat2(v("rawName"), v("typeArgStr")))))),
              field("select", lambda("sel",
                let(Seq(
                  field("qual", ScalaSyntax.selectDataQual(v("sel"))),
                  field("selName", ScalaSyntax.selectDataName(v("sel"))),
                  field("nameStr", ScalaSyntax.nameDataValue(v("selName"))),
                  field("rawName", ScalaSyntax.unPredefString(v("nameStr")))),
                  ScalaSyntax.dataRef(ScalaSyntax.refDataSelect(
                    ScalaSyntax.selectData(v("qual"))(
                      ScalaSyntax.nameData(
                        ScalaSyntax.predefString(
                          cat2(v("rawName"), v("typeArgStr")))))))))))))))))
    Phantoms.`def`(NS, "sapplyTypes",
      doc("Apply explicit type parameters to a Scala expression (e.g. f[A, B])", body))

  lazy val sassignDef: Definition =
    val body = lambda("lhs", lambda("rhs",
      ScalaSyntax.dataAssign(ScalaSyntax.assignData(v("lhs"))(v("rhs")))))
    Phantoms.`def`(NS, "sassign",
      doc("Create a Scala assignment expression", body))

  lazy val scalaEscapeEnumCaseNameDef: Definition =
    val body = lambda("s",
      let(Seq(
        field("renamed",
          applyP("hydra.lib.logic.ifElse",
            applyP("hydra.lib.equality.equal", v("s"), string("values")),
            string("values_"),
            v("s")))),
        applyP("hydra.scala.utils.scalaEscapeName", v("renamed"))))
    Phantoms.`def`(NS, "scalaEscapeEnumCaseName",
      doc("Like scalaEscapeName, but also renames 'values' to 'values_' to avoid conflict with Scala 3 enum's synthesized values() method", body))

  lazy val scalaEscapeNameDef: Definition =
    // Compose intermediate sub-expressions with vals to keep the structure readable.
    val sanitized: TypedTerm[String] =
      applyP("hydra.lib.strings.fromList",
        applyP("hydra.lib.lists.map",
          lambda("c",
            applyP("hydra.lib.logic.ifElse",
              applyP("hydra.lib.equality.equal", v("c"), int32(39)),
              int32(95),
              v("c"))),
          applyP("hydra.lib.strings.toList", v("s"))))

    val sanitized2: TypedTerm[String] =
      applyP("hydra.lib.logic.ifElse",
        applyP("hydra.lib.equality.equal", v("sanitized"), string("_")),
        string("_x"),
        v("sanitized"))

    val sanitized3: TypedTerm[String] =
      applyP("hydra.lib.logic.ifElse",
        applyP("hydra.lib.equality.equal", v("sanitized2"), string("toString")),
        string("toString_"),
        v("sanitized2"))

    // lastChar = optionals.fromOptional 0 (maybeCharAt (length s3 - 1) s3)
    val lastChar: TypedTerm[Int] =
      applyP("hydra.lib.optionals.fromOptional",
        int32(0),
        applyP("hydra.lib.strings.maybeCharAt",
          applyP("hydra.lib.math.sub",
            applyP("hydra.lib.strings.length", v("sanitized3")),
            int32(1)),
          v("sanitized3")))

    val endsWithUnderscore: TypedTerm[Boolean] =
      applyP("hydra.lib.logic.and",
        applyP("hydra.lib.equality.gt",
          applyP("hydra.lib.strings.length", v("sanitized3")),
          int32(0)),
        applyP("hydra.lib.equality.equal", lastChar, int32(95)))

    val needsBackticks: TypedTerm[Boolean] =
      applyP("hydra.lib.logic.or",
        applyP("hydra.lib.sets.member", v("sanitized3"), scalaReservedWordsRefVar),
        endsWithUnderscore)

    val body = lambda("s",
      let(Seq(
        field("sanitized", sanitized),
        field("sanitized2", sanitized2),
        field("sanitized3", sanitized3),
        field("needsBackticks", needsBackticks)),
        applyP("hydra.lib.logic.ifElse",
          v("needsBackticks"),
          applyP("hydra.lib.strings.cat",
            list(string("`"), v("sanitized3"), string("`"))),
          v("sanitized3"))))
    Phantoms.`def`(NS, "scalaEscapeName",
      doc("Sanitize a name for Scala: escape reserved words, replace invalid characters", body))

  lazy val scalaReservedWordsRefDef: Definition =
    val body = v("hydra.scala.language.scalaReservedWords")
    Phantoms.`def`(NS, "scalaReservedWords",
      doc("Reference to scalaReservedWords from the language module", body))

  lazy val scalaTypeNameDef: Definition =
    val body = lambda("qualify", lambda("name",
      applyP("hydra.lib.logic.ifElse",
        applyP("hydra.lib.logic.or",
          v("qualify"),
          applyP("hydra.lib.sets.member",
            applyP("hydra.names.localNameOf", v("name")),
            scalaReservedWordsRefVar)),
        CoreDsl.unName(v("name")),
        applyP("hydra.names.localNameOf", v("name")))))
    Phantoms.`def`(NS, "scalaTypeName",
      doc("Convert a Hydra name to a Scala type name", body))

  lazy val slambdaDef: Definition =
    val body = lambda("v", lambda("body", lambda("sdom",
      ScalaSyntax.dataFunction(ScalaSyntax.functionData(
        list(
          ScalaSyntax.paramData(list[Any]())(  // mods
            ScalaSyntax.nameValue(v("v")))(
            v("sdom"))(
            nothing)))(  // default
        v("body"))))))
    Phantoms.`def`(NS, "slambda",
      doc("Create a Scala lambda (function) expression", body))

  lazy val snameDef: Definition =
    val body = lambda("s",
      ScalaSyntax.dataRef(ScalaSyntax.refDataName(
        ScalaSyntax.nameData(ScalaSyntax.predefString(v("s"))))))
    Phantoms.`def`(NS, "sname",
      doc("Create a Scala name reference", body))

  lazy val sprimDef: Definition =
    val body = lambda("name",
      let(Seq(
        field("qname", applyP("hydra.names.qualifyName", v("name"))),
        field("prefix",
          PackagingDsl.unModuleName(
            applyP("hydra.lib.optionals.fromOptional",
              wrap("hydra.packaging.ModuleName", string("")),
              UtilDsl.qualifiedNameModuleName(v("qname"))))),
        field("local",
          applyP("hydra.scala.utils.scalaEscapeName",
            UtilDsl.qualifiedNameLocal(v("qname"))))),
        applyP("hydra.scala.utils.sname",
          cat2(cat2(v("prefix"), string(".")), v("local")))))
    Phantoms.`def`(NS, "sprim",
      doc("Create a Scala primitive reference from a Hydra name", body))

  lazy val stapplyDef: Definition =
    val body = lambda("t", lambda("args",
      ScalaSyntax.typeApply(ScalaSyntax.applyType(v("t"))(v("args")))))
    Phantoms.`def`(NS, "stapply",
      doc("Apply a Scala type to a list of type arguments", body))

  lazy val stapply1Def: Definition =
    val body = lambda("t1", lambda("t2",
      applyP("hydra.scala.utils.stapply", v("t1"), list(v("t2")))))
    Phantoms.`def`(NS, "stapply1",
      doc("Apply a Scala type to one type argument", body))

  lazy val stapply2Def: Definition =
    val body = lambda("t1", lambda("t2", lambda("t3",
      applyP("hydra.scala.utils.stapply", v("t1"), list(v("t2"), v("t3"))))))
    Phantoms.`def`(NS, "stapply2",
      doc("Apply a Scala type to two type arguments", body))

  lazy val stparamDef: Definition =
    val body = lambda("name",
      let(Seq(
        field("v",
          applyP("hydra.formatting.capitalize",
            CoreDsl.unName(v("name"))))),
        ScalaSyntax.paramType(
          list[Any]())(  // mods
          ScalaSyntax.nameValue(v("v")))(
          list[Any]())(  // tparams
          list[Any]())(  // tbounds
          list[Any]())(  // vbounds
          list[Any]())))  // cbounds
    Phantoms.`def`(NS, "stparam",
      doc("Create a Scala type parameter from a Hydra name, capitalizing to avoid collision with value params", body))

  lazy val strefDef: Definition =
    val body = lambda("s",
      ScalaSyntax.typeRef(ScalaSyntax.refTypeName(ScalaSyntax.nameType(v("s")))))
    Phantoms.`def`(NS, "stref",
      doc("Create a Scala type reference by name", body))

  lazy val svarDef: Definition =
    val body = lambda("name",
      let(Seq(
        field("v", CoreDsl.unName(v("name")))),
        ScalaSyntax.patVar(
          ScalaSyntax.varPat(
            ScalaSyntax.nameData(
              ScalaSyntax.predefString(v("v")))))))
    Phantoms.`def`(NS, "svar",
      doc("Create a Scala pattern variable", body))

  lazy val typeToStringDef: Definition =
    val body = lambda("t",
      casesWithDefault("hydra.scala.syntax.Type",
        v("t"), string("Any"),
        field("ref", lambda("tr",
          casesWithDefault("hydra.scala.syntax.RefType",
            v("tr"), string("Any"),
            field("name", lambda("tn",
              ScalaSyntax.nameTypeValue(v("tn"))))))),
        field("var", lambda("tv",
          ScalaSyntax.nameTypeValue(
            ScalaSyntax.varTypeName(v("tv"))))),
        field("function", lambda("fn",
          let(Seq(
            field("params",
              applyP("hydra.lib.lists.map",
                v("hydra.scala.utils.typeToString"),
                ScalaSyntax.functionTypeParams(v("fn")))),
            field("res",
              applyP("hydra.scala.utils.typeToString",
                ScalaSyntax.functionTypeRes(v("fn"))))),
            applyP("hydra.lib.strings.cat",
              list(
                string("("),
                applyP("hydra.lib.strings.intercalate", string(", "), v("params")),
                string(") => "),
                v("res")))))),
        field("apply", lambda("ta",
          let(Seq(
            field("base",
              applyP("hydra.scala.utils.typeToString",
                ScalaSyntax.applyTypeTpe(v("ta")))),
            field("argStrs",
              applyP("hydra.lib.lists.map",
                v("hydra.scala.utils.typeToString"),
                ScalaSyntax.applyTypeArgs(v("ta"))))),
            applyP("hydra.lib.strings.cat",
              list(
                v("base"),
                string("["),
                applyP("hydra.lib.strings.intercalate", string(", "), v("argStrs")),
                string("]"))))))))
    Phantoms.`def`(NS, "typeToString",
      doc("Convert a Scala type to its string representation", body))

  // ===== Module assembly =====

  val DEFINITIONS: Seq[Definition] = Seq(
    nameOfTypeDef,
    qualifyUnionFieldNameDef,
    sapplyDef,
    sapplyTypesDef,
    sassignDef,
    scalaEscapeEnumCaseNameDef,
    scalaEscapeNameDef,
    scalaReservedWordsRefDef,
    scalaTypeNameDef,
    slambdaDef,
    snameDef,
    sprimDef,
    stapplyDef,
    stapply1Def,
    stapply2Def,
    stparamDef,
    strefDef,
    svarDef,
    typeToStringDef)

  val module_ : Module = Module(
    name = NS,
    metadata = Some(EntityMetadata(
      description = Some("Utility functions for constructing Scala AST nodes"),
      comments = Seq.empty,
      seeAlso = Seq.empty,
      lifecycle = None)),
    dependencies = DEPS.map(Helpers.unqualifiedDep),
    definitions = DEFINITIONS)

end Utils
