package hydra.sources.scala

import hydra.overlay.scala.dsl.{Helpers, Phantoms}
import hydra.overlay.scala.dsl.meta.Defs
import hydra.overlay.scala.dsl.Phantoms.{`var` => v, prim, applyP, lambda, let, field, string, int32, bool, list, nothing, just, doc, casesWithDefault, project, unwrap, wrap, makeLocal, define, cat2}
import hydra.packaging.{Definition, EntityMetadata, Module, ModuleName}
import hydra.typed.TypedTerm

import hydra.dsl.scala.syntax as ScalaSyntax
import hydra.dsl.{core => CoreDsl, packaging => PackagingDsl, util => UtilDsl}

/**
 * Utility functions for constructing Scala AST nodes.
 */
object Utils:

  val NS: ModuleName = "hydra.scala.utils"

  /** Dependencies: matches Haskell `[scalaLanguage, names, formatting] ++ (scalaSyntax : kernelTypesModuleNames)`. */
  private val DEPS: Seq[ModuleName] =
    Seq("hydra.scala.language", "hydra.names", "hydra.formatting", "hydra.scala.syntax")
      ++ Helpers.kernelTypesModuleNames

  // ===== Local helpers — shorthand FQN references =====

  private val local = makeLocal(NS)
  private val localLanguage = makeLocal("hydra.scala.language")

  // Local re-export: references the scalaReservedWords binding in THIS module
  // (which itself proxies hydra.scala.language.scalaReservedWords). Matches the
  // Haskell DSL's `scalaReservedWordsRef` which is a local TypedTermDefinition.
  private val scalaReservedWordsRefVar: TypedTerm[Set[String]] =
    v(local("scalaReservedWords"))

  // ===== Definitions (alphabetical by camelCase, matching Haskell file order) =====

  lazy val nameOfTypeDef: Definition =
    define(NS, "nameOfType").doc("Extract the name from a type, if it is a named type")
      .lam("cx").lam("t").to(
        casesWithDefault("hydra.core.Type",
          applyP("hydra.strip.deannotateType", v("t")),
          nothing,
          field("variable", lambda("name", just(v("name")))),
          field("forall", lambda("ft",
            applyP(local("nameOfType"),
              v("cx"),
              CoreDsl.forallTypeBody(v("ft")))))))

  lazy val qualifyUnionFieldNameDef: Definition =
    define(NS, "qualifyUnionFieldName").doc("Qualify a union field name, optionally prefixing with the Scala type name")
      .lam("dlft").lam("sname").lam("fname").to(
        cat2(
          applyP("hydra.lib.optionals.cases",
            v("sname"),
            v("dlft"),
            lambda("n",
              cat2(
                applyP(local("scalaTypeName"), bool(true), v("n")),
                string(".")))),
          applyP(local("scalaEscapeName"),
            CoreDsl.unName(v("fname")))))

  lazy val sapplyDef: Definition =
    define(NS, "sapply").doc("Apply a Scala data expression to a list of arguments")
      .lam("fun").lam("args").to(
        ScalaSyntax.dataApply(ScalaSyntax.applyData(v("fun"))(v("args"))))

  lazy val sapplyTypesDef: Definition =
    define(NS, "sapplyTypes").doc("Apply explicit type parameters to a Scala expression (e.g. f[A, B]); a no-op for an empty type-arg list (#589)")
      .lam("fun").lam("typeArgs").to(
      applyP("hydra.lib.logic.ifElse",
        applyP("hydra.lib.lists.null", v("typeArgs")),
        v("fun"),
        let(Seq(
          field("typeToStr", lambda("t",
            applyP(local("typeToString"), v("t")))),
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
                    applyP(local("sname"),
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

  lazy val sassignDef: Definition =
    define(NS, "sassign").doc("Create a Scala assignment expression")
      .lam("lhs").lam("rhs").to(
        ScalaSyntax.dataAssign(ScalaSyntax.assignData(v("lhs"))(v("rhs"))))

  lazy val scalaEscapeEnumCaseNameDef: Definition =
    define(NS, "scalaEscapeEnumCaseName")
      .doc("Like scalaEscapeName, but also renames 'values' to 'values_' to avoid conflict with Scala 3 enum's synthesized values() method")
      .lam("s").to(
        let(Seq(
          field("renamed",
            applyP("hydra.lib.logic.ifElse",
              applyP("hydra.lib.equality.equal", v("s"), string("values")),
              string("values_"),
              v("s")))),
          applyP(local("scalaEscapeName"), v("renamed"))))

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

    define(NS, "scalaEscapeName").doc("Sanitize a name for Scala: escape reserved words, replace invalid characters")
      .lam("s").to(
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

  lazy val scalaReservedWordsRefDef: Definition =
    define(NS, "scalaReservedWords").doc("Reference to scalaReservedWords from the language module")
      .to(v(localLanguage("scalaReservedWords")))

  lazy val scalaTypeNameDef: Definition =
    define(NS, "scalaTypeName").doc("Convert a Hydra name to a Scala type name")
      .lam("qualify").lam("name").to(
        applyP("hydra.lib.logic.ifElse",
          applyP("hydra.lib.logic.or",
            v("qualify"),
            applyP("hydra.lib.sets.member",
              applyP("hydra.names.localNameOf", v("name")),
              scalaReservedWordsRefVar)),
          CoreDsl.unName(v("name")),
          applyP("hydra.names.localNameOf", v("name"))))

  lazy val slambdaDef: Definition =
    define(NS, "slambda").doc("Create a Scala lambda (function) expression")
      .lam("v").lam("body").lam("sdom").to(
        ScalaSyntax.dataFunction(ScalaSyntax.functionData(
          list(
            ScalaSyntax.paramData(list[Any]())(  // mods
              ScalaSyntax.nameValue(v("v")))(
              v("sdom"))(
              nothing)))(  // default
          v("body"))))

  lazy val snameDef: Definition =
    define(NS, "sname").doc("Create a Scala name reference")
      .lam("s").to(
        ScalaSyntax.dataRef(ScalaSyntax.refDataName(
          ScalaSyntax.nameData(ScalaSyntax.predefString(v("s"))))))

  lazy val sprimDef: Definition =
    define(NS, "sprim").doc("Create a Scala primitive reference from a Hydra name")
      .lam("name").to(
        let(Seq(
          field("qname", applyP("hydra.names.qualifyName", v("name"))),
          field("prefix",
            PackagingDsl.unModuleName(
              applyP("hydra.lib.optionals.fromOptional",
                wrap("hydra.packaging.ModuleName", string("")),
                UtilDsl.qualifiedNameModuleName(v("qname"))))),
          field("local",
            applyP(local("scalaEscapeName"),
              UtilDsl.qualifiedNameLocal(v("qname"))))),
          applyP(local("sname"),
            cat2(cat2(v("prefix"), string(".")), v("local")))))

  lazy val stapplyDef: Definition =
    define(NS, "stapply").doc("Apply a Scala type to a list of type arguments")
      .lam("t").lam("args").to(
        ScalaSyntax.typeApply(ScalaSyntax.applyType(v("t"))(v("args"))))

  lazy val stapply1Def: Definition =
    define(NS, "stapply1").doc("Apply a Scala type to one type argument")
      .lam("t1").lam("t2").to(
        applyP(local("stapply"), v("t1"), list(v("t2"))))

  lazy val stapply2Def: Definition =
    define(NS, "stapply2").doc("Apply a Scala type to two type arguments")
      .lam("t1").lam("t2").lam("t3").to(
        applyP(local("stapply"), v("t1"), list(v("t2"), v("t3"))))

  lazy val stparamDef: Definition =
    define(NS, "stparam").doc("Create a Scala type parameter from a Hydra name, capitalizing to avoid collision with value params")
      .lam("name").to(
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

  lazy val strefDef: Definition =
    define(NS, "stref").doc("Create a Scala type reference by name")
      .lam("s").to(
        ScalaSyntax.typeRef(ScalaSyntax.refTypeName(ScalaSyntax.nameType(v("s")))))

  lazy val svarDef: Definition =
    define(NS, "svar").doc("Create a Scala pattern variable")
      .lam("name").to(
        let(Seq(
          field("v", CoreDsl.unName(v("name")))),
          ScalaSyntax.patVar(
            ScalaSyntax.varPat(
              ScalaSyntax.nameData(
                ScalaSyntax.predefString(v("v")))))))

  lazy val typeToStringDef: Definition =
    define(NS, "typeToString").doc("Convert a Scala type to its string representation")
      .lam("t").to(
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
                  v(local("typeToString")),
                  ScalaSyntax.functionTypeParams(v("fn")))),
              field("res",
                applyP(local("typeToString"),
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
                applyP(local("typeToString"),
                  ScalaSyntax.applyTypeTpe(v("ta")))),
              field("argStrs",
                applyP("hydra.lib.lists.map",
                  v(local("typeToString")),
                  ScalaSyntax.applyTypeArgs(v("ta"))))),
              applyP("hydra.lib.strings.cat",
                list(
                  v("base"),
                  string("["),
                  applyP("hydra.lib.strings.intercalate", string(", "), v("argStrs")),
                  string("]"))))))))

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

  Defs.checkComplete(this, DEFINITIONS)

end Utils
