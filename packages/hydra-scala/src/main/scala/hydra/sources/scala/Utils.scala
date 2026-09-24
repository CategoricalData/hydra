package hydra.sources.scala

import hydra.core.overlay.scala.dsl.{Helpers, Phantoms}
import hydra.core.overlay.scala.dsl.meta.Defs
import hydra.core.overlay.scala.dsl.Phantoms.{`var` => v, prim, applyP, lambda, let, field, string, int32, bool, list, nothing, just, doc, matchWithDefault, project, unwrap, wrap, makeLocal, define, cat2}
import hydra.core.packaging.{Definition, EntityMetadata, Module, ModuleName}
import hydra.core.typed.TypedTerm

import hydra.scala.dsl.syntax as ScalaSyntax
import hydra.core.dsl.{core => CoreDsl, packaging => PackagingDsl, util => UtilDsl}

/**
 * Utility functions for constructing Scala AST nodes.
 */
object Utils:

  val NS: ModuleName = "hydra.scala.utils"

  /** Dependencies: matches Haskell `[scalaLanguage, names, formatting] ++ (scalaSyntax : kernelTypesModuleNames)`. */
  private val DEPS: Seq[ModuleName] =
    Seq("hydra.scala.language", "hydra.core.names", "hydra.core.formatting", "hydra.scala.syntax")
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
        matchWithDefault("hydra.core.model.Type",
          applyP("hydra.core.strip.deannotateType", v("t")),
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
          applyP("hydra.core.lib.optionals.match",
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
      applyP("hydra.core.lib.logic.ifElse",
        applyP("hydra.core.lib.lists.isEmpty", v("typeArgs")),
        v("fun"),
        let(Seq(
          field("typeToStr", lambda("t",
            applyP(local("typeToString"), v("t")))),
          field("typeStrings",
            applyP("hydra.core.lib.lists.map", v("typeToStr"), v("typeArgs"))),
          field("typeArgStr",
            applyP("hydra.core.lib.strings.concat",
              list(
                string("["),
                applyP("hydra.core.lib.strings.join", string(", "), v("typeStrings")),
                string("]"))))),
          matchWithDefault("hydra.scala.syntax.Data",
            v("fun"), v("fun"),
            field("ref", lambda("ref",
              matchWithDefault("hydra.scala.syntax.RefData",
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
            applyP("hydra.core.lib.logic.ifElse",
              applyP("hydra.core.lib.equality.equal", v("s"), string("values")),
              string("values_"),
              v("s")))),
          applyP(local("scalaEscapeName"), v("renamed"))))

  lazy val scalaEscapeNameDef: Definition =
    // Compose intermediate sub-expressions with vals to keep the structure readable.
    val sanitized: TypedTerm[String] =
      applyP("hydra.core.lib.strings.fromList",
        applyP("hydra.core.lib.lists.map",
          lambda("c",
            applyP("hydra.core.lib.logic.ifElse",
              applyP("hydra.core.lib.equality.equal", v("c"), int32(39)),
              int32(95),
              v("c"))),
          applyP("hydra.core.lib.strings.toList", v("s"))))

    val sanitized2: TypedTerm[String] =
      applyP("hydra.core.lib.logic.ifElse",
        applyP("hydra.core.lib.equality.equal", v("sanitized"), string("_")),
        string("_x"),
        v("sanitized"))

    val sanitized3: TypedTerm[String] =
      applyP("hydra.core.lib.logic.ifElse",
        applyP("hydra.core.lib.equality.equal", v("sanitized2"), string("toString")),
        string("toString_"),
        v("sanitized2"))

    // lastChar = optionals.withDefault 0 (charAt (length s3 - 1) s3)
    val lastChar: TypedTerm[Int] =
      applyP("hydra.core.lib.optionals.withDefault",
        int32(0),
        applyP("hydra.core.lib.strings.charAt",
          applyP("hydra.core.lib.math.sub",
            applyP("hydra.core.lib.strings.length", v("sanitized3")),
            int32(1)),
          v("sanitized3")))

    val endsWithUnderscore: TypedTerm[Boolean] =
      applyP("hydra.core.lib.logic.and",
        applyP("hydra.core.lib.ordering.gt",
          applyP("hydra.core.lib.strings.length", v("sanitized3")),
          int32(0)),
        applyP("hydra.core.lib.equality.equal", lastChar, int32(95)))

    val needsBackticks: TypedTerm[Boolean] =
      applyP("hydra.core.lib.logic.or",
        applyP("hydra.core.lib.sets.member", v("sanitized3"), scalaReservedWordsRefVar),
        endsWithUnderscore)

    define(NS, "scalaEscapeName").doc("Sanitize a name for Scala: escape reserved words, replace invalid characters")
      .lam("s").to(
        let(Seq(
          field("sanitized", sanitized),
          field("sanitized2", sanitized2),
          field("sanitized3", sanitized3),
          field("needsBackticks", needsBackticks)),
          applyP("hydra.core.lib.logic.ifElse",
            v("needsBackticks"),
            applyP("hydra.core.lib.strings.concat",
              list(string("`"), v("sanitized3"), string("`"))),
            v("sanitized3"))))

  lazy val scalaReservedWordsRefDef: Definition =
    define(NS, "scalaReservedWords").doc("Reference to scalaReservedWords from the language module")
      .to(v(localLanguage("scalaReservedWords")))

  lazy val scalaTypeNameDef: Definition =
    define(NS, "scalaTypeName").doc("Convert a Hydra name to a Scala type name")
      .lam("qualify").lam("name").to(
        applyP("hydra.core.lib.logic.ifElse",
          applyP("hydra.core.lib.logic.or",
            v("qualify"),
            applyP("hydra.core.lib.sets.member",
              applyP("hydra.core.names.localNameOf", v("name")),
              scalaReservedWordsRefVar)),
          CoreDsl.unName(v("name")),
          applyP("hydra.core.names.localNameOf", v("name"))))

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
    define(NS, "sprim").doc("Create a Scala primitive reference from a Hydra name, redirecting"
      + " hydra.core.lib.<sub>.<local> to hydra.core.overlay.scala.lib.<sub>.<local> when <sub> has an overlay"
      + " implementation on this host (#630 -- the on-disk overlaySubs existence signal). Unlike"
      + " toPrimImport (which redirects a bare hydra.core.lib.<sub> MODULE name, always exactly 4"
      + " segments post-#729), a primitive reference is hydra.core.lib.<sub>.<local> -- at least 5 segments,"
      + " since it also carries the primitive's own local name -- so the sub to check against overlaySubs"
      + " is parts[3] alone, not the whole post-prefix tail (#635, #729).")
      .lam("overlaySubs").lam("name").to(
        let(Seq(
          field("raw", CoreDsl.unName(v("name"))),
          field("parts", applyP("hydra.core.lib.strings.splitOn", string("."), v("raw"))),
          field("sub", applyP("hydra.core.lib.strings.join", string("."),
            applyP("hydra.core.lib.lists.drop", int32(3), v("parts")))),
          field("subHead",
            applyP("hydra.core.lib.optionals.withDefault", string(""),
              applyP("hydra.core.lib.lists.at", int32(3), v("parts")))),
          field("redirectedRaw",
            applyP("hydra.core.lib.logic.ifElse",
              applyP("hydra.core.lib.logic.and",
                applyP("hydra.core.lib.logic.and",
                  applyP("hydra.core.lib.ordering.gte", applyP("hydra.core.lib.lists.length", v("parts")), int32(5)),
                  applyP("hydra.core.lib.equality.equal",
                    applyP("hydra.core.lib.lists.take", int32(3), v("parts")),
                    list(string("hydra"), string("core"), string("lib")))),
                applyP("hydra.core.lib.sets.member", v("subHead"), v("overlaySubs"))),
              applyP("hydra.core.lib.strings.concat2", string("hydra.core.overlay.scala.lib."), v("sub")),
              v("raw"))),
          field("redirectedName", wrap("hydra.core.model.Name", v("redirectedRaw"))),
          field("qname", applyP("hydra.core.names.qualifyName", v("redirectedName"))),
          field("prefix",
            PackagingDsl.unModuleName(
              applyP("hydra.core.lib.optionals.withDefault",
                wrap("hydra.core.packaging.ModuleName", string("")),
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
            applyP("hydra.core.formatting.capitalize",
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
        matchWithDefault("hydra.scala.syntax.Type",
          v("t"), string("Any"),
          field("ref", lambda("tr",
            matchWithDefault("hydra.scala.syntax.RefType",
              v("tr"), string("Any"),
              field("name", lambda("tn",
                ScalaSyntax.nameTypeValue(v("tn"))))))),
          field("var", lambda("tv",
            ScalaSyntax.nameTypeValue(
              ScalaSyntax.varTypeName(v("tv"))))),
          field("function", lambda("fn",
            let(Seq(
              field("params",
                applyP("hydra.core.lib.lists.map",
                  v(local("typeToString")),
                  ScalaSyntax.functionTypeParams(v("fn")))),
              field("res",
                applyP(local("typeToString"),
                  ScalaSyntax.functionTypeRes(v("fn"))))),
              applyP("hydra.core.lib.strings.concat",
                list(
                  string("("),
                  applyP("hydra.core.lib.strings.join", string(", "), v("params")),
                  string(") => "),
                  v("res")))))),
          field("apply", lambda("ta",
            let(Seq(
              field("base",
                applyP(local("typeToString"),
                  ScalaSyntax.applyTypeTpe(v("ta")))),
              field("argStrs",
                applyP("hydra.core.lib.lists.map",
                  v(local("typeToString")),
                  ScalaSyntax.applyTypeArgs(v("ta"))))),
              applyP("hydra.core.lib.strings.concat",
                list(
                  v("base"),
                  string("["),
                  applyP("hydra.core.lib.strings.join", string(", "), v("argStrs")),
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
      lifecycle = None,
      provisions = Seq.empty)),
    dependencies = DEPS.map(Helpers.unqualifiedDep),
    definitions = DEFINITIONS)

  Defs.checkComplete(this, DEFINITIONS)

end Utils
