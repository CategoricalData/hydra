package hydra.sources.scala

import hydra.overlay.scala.dsl.{Helpers, Phantoms}
import hydra.overlay.scala.dsl.Phantoms.{`var` => v, prim, applyP, lambda, let, field, string, int32, list, nothing, just, doc, casesWithDefault, cases, project, unwrap, wrap, constant, makeLocal, define, cat2}
import hydra.packaging.{Definition, EntityMetadata, Module, ModuleName}
import hydra.typed.TypedTerm

import hydra.dsl.scala.syntax as ScalaSyntax
import hydra.dsl.{ast => AstDsl}

/**
 * Serialization functions for converting Scala AST to abstract expressions.
 *
 * Style note: each big sub-expression is bound to a `private val` to keep
 * nesting shallow and paren-balancing trivial. Don't inline complex bodies.
 */
object Serde:

  val NS: ModuleName = "hydra.scala.serde"

  /** Dependencies match Haskell `[Serialization.ns, jvmSerdeNs] ++ (ScalaSyntax.ns:kernelTypesModuleNames)`. */
  private val DEPS: Seq[ModuleName] =
    Seq("hydra.serialization", "hydra.jvm.serde", "hydra.scala.syntax") ++ Helpers.kernelTypesModuleNames

  // ===== Shorthand helpers (kept inline-small) =====

  private val local = makeLocal(NS)

  private def cst(t: TypedTerm[String]): TypedTerm[Any] =
    applyP("hydra.serialization.cst", t)

  private def cstS(s: String): TypedTerm[Any] = cst(string(s))

  private def spaceSep(elems: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.serialization.spaceSep", elems)

  private def noSep(elems: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.serialization.noSep", elems)

  private def newlineSep(elems: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.serialization.newlineSep", elems)

  private def doubleNewlineSep(elems: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.serialization.doubleNewlineSep", elems)

  private def commaSep(style: TypedTerm[Any], elems: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.serialization.commaSep", style, elems)

  private def parenList(elems: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.serialization.parenListAdaptive", elems)

  private def bracketList(style: TypedTerm[Any], elems: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.serialization.bracketList", style, elems)

  private def curlyBracesList(msymb: TypedTerm[Any], style: TypedTerm[Any], elems: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.serialization.curlyBracesList", msymb, style, elems)

  private def curlyBlock(style: TypedTerm[Any], e: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.serialization.curlyBlock", style, e)

  private def ifx(op: TypedTerm[Any], lhs: TypedTerm[Any], rhs: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.serialization.ifx", op, lhs, rhs)

  private def expressionLength(e: TypedTerm[Any]): TypedTerm[Int] =
    applyP("hydra.serialization.expressionLength", e)

  private def inlineStyle: TypedTerm[Any] = v("hydra.serialization.inlineStyle")
  private def fullBlockStyle: TypedTerm[Any] = v("hydra.serialization.fullBlockStyle")

  private def termToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("termToExpr"), t)

  private def typeToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("typeToExpr"), t)

  private def patToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("patToExpr"), t)

  private def statToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("statToExpr"), t)

  private def caseToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("caseToExpr"), t)

  private def dataParamToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("dataParamToExpr"), t)

  private def dataNameToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("dataNameToExpr"), t)

  private def typeNameToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("typeNameToExpr"), t)

  private def typeParamToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("typeParamToExpr"), t)

  private def nameToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("nameToExpr"), t)

  private def modToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("modToExpr"), t)

  private def initToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("initToExpr"), t)

  private def importerToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("importerToExpr"), t)

  private def importExportStatToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("importExportStatToExpr"), t)

  private def defnToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("defnToExpr"), t)

  private def dataRefToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("dataRefToExpr"), t)

  private def dataSelectToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("dataSelectToExpr"), t)

  private def dataFunctionToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("dataFunctionToExpr"), t)

  private def litToExprCall(t: TypedTerm[Any]): TypedTerm[Any] =
    applyP(local("litToExpr"), t)

  private def map[A, B](fn: TypedTerm[A], xs: TypedTerm[Seq[A]]): TypedTerm[Seq[B]] =
    applyP("hydra.lib.lists.map", fn, xs)

  private def listNull(xs: TypedTerm[Any]): TypedTerm[Boolean] =
    applyP("hydra.lib.lists.null", xs)

  private def listConcat(xss: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.lib.lists.concat", xss)

  private def listLength(xs: TypedTerm[Any]): TypedTerm[Int] =
    applyP("hydra.lib.lists.length", xs)

  private def listMaybeHead(xs: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.lib.lists.maybeHead", xs)

  private def optPure(x: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.lib.optionals.pure", x)

  private def optCat(xs: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.lib.optionals.cat", xs)

  private def optMap(fn: TypedTerm[Any], xo: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.lib.optionals.map", fn, xo)

  private def optCases(xo: TypedTerm[Any], dflt: TypedTerm[Any], fn: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.lib.optionals.cases", xo, dflt, fn)

  private def optFromOptional(dflt: TypedTerm[Any], xo: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.lib.optionals.fromOptional", dflt, xo)

  private def ifElse(cond: TypedTerm[Boolean], thn: TypedTerm[Any], els: TypedTerm[Any]): TypedTerm[Any] =
    applyP("hydra.lib.logic.ifElse", cond, thn, els)

  private def eq[A](a: TypedTerm[A], b: TypedTerm[A]): TypedTerm[Boolean] =
    applyP("hydra.lib.equality.equal", a, b)

  private def gt(a: TypedTerm[Int], b: TypedTerm[Int]): TypedTerm[Boolean] =
    applyP("hydra.lib.equality.gt", a, b)

  // ===== Definitions (alphabetical, matching Haskell source ordering) =====

  // ---- caseToExpr ----
  private val caseToExprBody = lambda("c",
    let(Seq(
      field("pat", ScalaSyntax.casePat(v("c"))),
      field("term", ScalaSyntax.caseBody(v("c")))),
      spaceSep(list(
        cstS("case"),
        patToExprCall(v("pat")),
        cstS("=>"),
        termToExprCall(v("term"))))))

  lazy val caseToExprDef: Definition =
    define(NS, "caseToExpr").doc("Convert a case clause to an expression").to(caseToExprBody)

  // ---- dataFunctionToExpr ----
  private val dataFunctionToExprBody = lambda("f",
    let(Seq(
      field("params", ScalaSyntax.functionDataParams(v("f"))),
      field("body", ScalaSyntax.functionDataBody(v("f"))),
      field("bodyExpr", termToExprCall(v("body"))),
      field("bodyLen", expressionLength(v("bodyExpr")))),
      ifElse(
        gt(v("bodyLen"), int32(60)),
        noSep(list(
          parenList(map(v(local("dataParamToExpr")), v("params"))),
          cstS(" =>\n  "),
          v("bodyExpr"))),
        spaceSep(list(
          parenList(map(v(local("dataParamToExpr")), v("params"))),
          cstS("=>"),
          v("bodyExpr"))))))

  lazy val dataFunctionToExprDef: Definition =
    define(NS, "dataFunctionToExpr").doc("Convert a function-data lambda to an expression").to(dataFunctionToExprBody)

  // ---- dataNameToExpr ----
  private val dataNameToExprBody = lambda("dn",
    cst(ScalaSyntax.unPredefString(ScalaSyntax.nameDataValue(v("dn")))))

  lazy val dataNameToExprDef: Definition =
    define(NS, "dataNameToExpr").doc("Convert a data name to an expression").to(dataNameToExprBody)

  // ---- dataParamToExpr ----
  private val dataParamToExprBody = lambda("dp",
    let(Seq(
      field("name", ScalaSyntax.paramDataName(v("dp"))),
      field("stype", ScalaSyntax.paramDataDecltpe(v("dp")))),
      noSep(optCat(list(
        optPure(nameToExprCall(v("name"))),
        optMap(
          lambda("t", spaceSep(list(cstS(":"), typeToExprCall(v("t"))))),
          v("stype")))))))

  lazy val dataParamToExprDef: Definition =
    define(NS, "dataParamToExpr").doc("Convert a data parameter to an expression").to(dataParamToExprBody)

  // ---- dataRefToExpr ----
  private val dataRefToExprBody = lambda("ref",
    cases("hydra.scala.syntax.RefData", v("ref"),
      field("name", lambda("name", dataNameToExprCall(v("name")))),
      field("select", lambda("sel", dataSelectToExprCall(v("sel"))))))

  lazy val dataRefToExprDef: Definition =
    define(NS, "dataRefToExpr").doc("Convert a data reference to an expression").to(dataRefToExprBody)

  // ---- dataSelectToExpr ----
  private val dataSelectToExprBody = lambda("sel",
    let(Seq(
      field("arg", ScalaSyntax.selectDataQual(v("sel"))),
      field("name", ScalaSyntax.selectDataName(v("sel")))),
      ifx(
        v(local("dotOp")),
        termToExprCall(v("arg")),
        termToExprCall(ScalaSyntax.dataRef(ScalaSyntax.refDataName(v("name")))))))

  lazy val dataSelectToExprDef: Definition =
    define(NS, "dataSelectToExpr").doc("Convert a data select to an expression").to(dataSelectToExprBody)

  // ---- defnToExpr — six-arm cases. Each arm built as its own val. ----

  // def arm — outer let, then 3 sequenced single-binding lets to match
  // Haskell's `lets [...] $ "bodyExpr" <~ ... $ "defSig" <~ ... $ "bodyLen" <~ ... $ body` shape.
  private val defDefnArmInnerBody =
    ifElse(
      gt(v("bodyLen"), int32(80)),
      noSep(list(v("defSig"), cstS("\n  "), v("bodyExpr"))),
      spaceSep(list(v("defSig"), v("bodyExpr"))))

  private val defDefnArmAfterBodyLen = defDefnArmInnerBody

  private val defDefnArmAfterDefSig =
    let(Seq(field("bodyLen", expressionLength(v("bodyExpr")))),
      defDefnArmAfterBodyLen)

  private val defDefnArmAfterBodyExpr =
    let(Seq(field("defSig",
        spaceSep(list(cstS("def"), v("nameAndParams"), cstS("="))))),
      defDefnArmAfterDefSig)

  private val defDefnArmOuterBody =
    let(Seq(field("bodyExpr", termToExprCall(v("body")))),
      defDefnArmAfterBodyExpr)

  private val defDefnArmBody = lambda("dd",
    let(Seq(
      field("name", ScalaSyntax.defDefnName(v("dd"))),
      field("tparams", ScalaSyntax.defDefnTparams(v("dd"))),
      field("paramss", ScalaSyntax.defDefnParamss(v("dd"))),
      field("scod", ScalaSyntax.defDefnDecltpe(v("dd"))),
      field("body", ScalaSyntax.defDefnBody(v("dd"))),
      field("tparamsExpr",
        ifElse(listNull(v("tparams")), nothing,
          optPure(bracketList(inlineStyle, map(v(local("typeParamToExpr")), v("tparams")))))),
      field("scodExpr",
        optMap(
          lambda("t", spaceSep(list(cstS(":"), typeToExprCall(v("t"))))),
          v("scod"))),
      field("paramssExprs",
        map(
          lambda("ps", parenList(map(v(local("dataParamToExpr")), v("ps")))),
          v("paramss"))),
      field("nameAndParams",
        noSep(optCat(listConcat(list(
          list(optPure(dataNameToExprCall(v("name")))),
          list(v("tparamsExpr")),
          map(lambda("pe", optPure(v("pe"))), v("paramssExprs")),
          list(v("scodExpr")))))))),
      defDefnArmOuterBody))

  // type arm
  private val typeDefnArmBody = lambda("dt",
    let(Seq(
      field("name", ScalaSyntax.typeDefnName(v("dt"))),
      field("tparams", ScalaSyntax.typeDefnTparams(v("dt"))),
      field("body", ScalaSyntax.typeDefnBody(v("dt")))),
      spaceSep(optCat(list(
        optPure(cstS("type")),
        optPure(typeNameToExprCall(v("name"))),
        ifElse(listNull(v("tparams")), nothing,
          optPure(bracketList(inlineStyle, map(v(local("typeParamToExpr")), v("tparams"))))),
        optPure(cstS("=")),
        optPure(typeToExprCall(v("body"))))))))

  // val arm
  private val valDefnNameStr =
    optFromOptional(string(""),
      optMap(
        lambda("firstPat",
          let(Seq(
            field("patName",
              cases("hydra.scala.syntax.Pat", v("firstPat"),
                field("var", lambda("pv", ScalaSyntax.varPatName(v("pv"))))))),
            ScalaSyntax.unPredefString(ScalaSyntax.nameDataValue(v("patName"))))),
        listMaybeHead(v("pats"))))

  private val valDefnArmBody = lambda("dv",
    let(Seq(
      field("mods", ScalaSyntax.valDefnMods(v("dv"))),
      field("pats", ScalaSyntax.valDefnPats(v("dv"))),
      field("typ", ScalaSyntax.valDefnDecltpe(v("dv"))),
      field("rhs", ScalaSyntax.valDefnRhs(v("dv"))),
      field("nameStr", valDefnNameStr),
      field("nameAndType",
        optCases(v("typ"),
          cst(v("nameStr")),
          lambda("t", spaceSep(list(
            cst(cat2(v("nameStr"), string(":"))),
            typeToExprCall(v("t"))))))),
      field("valKeyword",
        ifElse(listNull(v("mods")), string("val"), string("lazy val")))),
      spaceSep(list(
        cst(v("valKeyword")),
        v("nameAndType"),
        cstS("="),
        termToExprCall(v("rhs"))))))

  // class arm
  private val classDefnArmBody = lambda("dc",
    let(Seq(
      field("mods", ScalaSyntax.classDefnMods(v("dc"))),
      field("name", ScalaSyntax.classDefnName(v("dc"))),
      field("tparams", ScalaSyntax.classDefnTparams(v("dc"))),
      field("ctor", ScalaSyntax.classDefnCtor(v("dc"))),
      field("paramss", ScalaSyntax.primaryCtorParamss(v("ctor"))),
      field("tparamsExpr",
        ifElse(listNull(v("tparams")), nothing,
          optPure(bracketList(inlineStyle, map(v(local("typeParamToExpr")), v("tparams")))))),
      field("paramsExpr",
        ifElse(listNull(v("paramss")), nothing,
          optPure(parenList(map(v(local("dataParamToExpr")), listConcat(v("paramss"))))))),
      field("nameAndParams",
        noSep(optCat(list(
          optPure(typeNameToExprCall(v("name"))),
          v("tparamsExpr"),
          v("paramsExpr")))))),
      spaceSep(listConcat(list(
        map(v(local("modToExpr")), v("mods")),
        list(cstS("class"), v("nameAndParams")))))))

  // enum arm
  private val enumDefnArmBody = lambda("de",
    let(Seq(
      field("name", ScalaSyntax.enumDefnName(v("de"))),
      field("tparams", ScalaSyntax.enumDefnTparams(v("de"))),
      field("template", ScalaSyntax.enumDefnTemplate(v("de"))),
      field("stats", ScalaSyntax.templateStats(v("template"))),
      field("enumHeader",
        spaceSep(list(
          cstS("enum"),
          noSep(optCat(list(
            optPure(typeNameToExprCall(v("name"))),
            ifElse(listNull(v("tparams")), nothing,
              optPure(bracketList(inlineStyle, map(v(local("typeParamToExpr")), v("tparams")))))))),
          cstS(":")))),
      field("enumCases",
        map(
          lambda("s", spaceSep(list(cstS("  "), statToExprCall(v("s"))))),
          v("stats")))),
      newlineSep(listConcat(list(list(v("enumHeader")), v("enumCases"))))))

  // enumCase arm
  private val enumCaseDefnArmBody = lambda("dec",
    let(Seq(
      field("name", ScalaSyntax.enumCaseDefnName(v("dec"))),
      field("ctor", ScalaSyntax.enumCaseDefnCtor(v("dec"))),
      field("inits", ScalaSyntax.enumCaseDefnInits(v("dec"))),
      field("paramss", ScalaSyntax.primaryCtorParamss(v("ctor"))),
      field("allParams", listConcat(v("paramss"))),
      field("params",
        ifElse(listNull(v("allParams")),
          cstS(""),
          parenList(map(v(local("dataParamToExpr")), v("allParams"))))),
      field("extendsClause",
        ifElse(listNull(v("inits")),
          cstS(""),
          spaceSep(list(
            cstS("extends"),
            commaSep(inlineStyle, map(v(local("initToExpr")), v("inits")))))))),
      spaceSep(list(
        cstS("case"),
        noSep(list(dataNameToExprCall(v("name")), v("params"))),
        v("extendsClause")))))

  private val defnToExprBody = lambda("def",
    cases("hydra.scala.syntax.Defn", v("def"),
      field("def", defDefnArmBody),
      field("type", typeDefnArmBody),
      field("val", valDefnArmBody),
      field("class", classDefnArmBody),
      field("enum", enumDefnArmBody),
      field("enumCase", enumCaseDefnArmBody)))

  lazy val defnToExprDef: Definition =
    define(NS, "defnToExpr").doc("Convert a definition to an expression").to(defnToExprBody)

  // ---- dotOp ----
  private val dotOpBody =
    AstDsl.op(AstDsl.symbol(string(".")))(
      AstDsl.padding(AstDsl.wsNone)(AstDsl.wsNone))(
      AstDsl.precedence(int32(0)))(
      AstDsl.associativityLeft)

  lazy val dotOpDef: Definition =
    define(NS, "dotOp").doc("The dot operator for member access").to(dotOpBody)

  // ---- functionArrowOp ----
  private val functionArrowOpBody =
    applyP("hydra.serialization.op",
      string("=>"),
      applyP("hydra.lib.math.negate", int32(1)),
      AstDsl.associativityRight)

  lazy val functionArrowOpDef: Definition =
    define(NS, "functionArrowOp").doc("The function arrow operator (=>)").to(functionArrowOpBody)

  // ---- importExportStatToExpr ----
  private val importExportStatToExprBody = lambda("ie",
    cases("hydra.scala.syntax.ImportExportStat", v("ie"),
      field("import", lambda("imp",
        let(Seq(field("importers", ScalaSyntax.importImporters(v("imp")))),
          newlineSep(map(v(local("importerToExpr")), v("importers"))))))))

  lazy val importExportStatToExprDef: Definition =
    define(NS, "importExportStatToExpr").doc("Convert an import/export statement to an expression").to(importExportStatToExprBody)

  // ---- importerToExpr ----

  // refName helper: cases on RefData ref, returning the wrapped name string
  private val importerRefName =
    cases("hydra.scala.syntax.RefData", v("ref"),
      field("name", lambda("dn",
        ScalaSyntax.unPredefString(ScalaSyntax.nameDataValue(v("dn"))))))

  // DEAD CODE (#553 audit): importeeRenderer and importerForImportees below are unreferenced —
  // only importerForImporteesInlined (below) is actually used. __importee_renderer doesn't
  // correspond to any registered Definition; this was an earlier attempt superseded by the
  // inlined version per the NOTE below. Candidate for deletion in a follow-up; left in place here
  // since removal wasn't in scope for this pass.

  // Per-importee renderer: matches wildcard or name, returns an Expr
  private val importeeRenderer = lambda("it",
    cases("hydra.scala.syntax.Importee", v("it"),
      field("wildcard", constant(cstS("*"))),
      field("name", lambda("in",
        cst(
          cases("hydra.scala.syntax.Name",
            ScalaSyntax.nameImporteeName(v("in")),
            field("value", lambda("s", v("s")))))))))

  // forImportees: 3-way ifElse — empty / single / many
  private val importerForImportees =
    ifElse(
      listNull(v("importees")),
      cstS(""),
      ifElse(
        eq(listLength(v("importees")), int32(1)),
        optFromOptional(cstS(""),
          optMap(
            lambda("firstImp",
              noSep(list(cstS("."), applyP(local("__importee_renderer"), v("firstImp"))))),
            listMaybeHead(v("importees")))),
        noSep(list(
          cstS("."),
          curlyBracesList(nothing, inlineStyle,
            map(v(local("__importee_renderer")), v("importees")))))))

  // NOTE: importeeRenderer is bound as a private val here, but the let-body
  // can't reference Scala-level vals at term time — it needs to be inlined or
  // bound via a let. Simplest: inline the renderer at both call sites.
  private val importerForImporteesInlined =
    ifElse(
      listNull(v("importees")),
      cstS(""),
      ifElse(
        eq(listLength(v("importees")), int32(1)),
        optFromOptional(cstS(""),
          optMap(
            lambda("firstImp",
              noSep(list(
                cstS("."),
                cases("hydra.scala.syntax.Importee", v("firstImp"),
                  field("wildcard", constant(cstS("*"))),
                  field("name", lambda("in",
                    cst(
                      cases("hydra.scala.syntax.Name",
                        ScalaSyntax.nameImporteeName(v("in")),
                        field("value", lambda("s", v("s")))))))) ))),
            listMaybeHead(v("importees")))),
        noSep(list(
          cstS("."),
          curlyBracesList(nothing, inlineStyle,
            map(
              lambda("it",
                cases("hydra.scala.syntax.Importee", v("it"),
                  field("wildcard", constant(cstS("*"))),
                  field("name", lambda("in",
                    cst(
                      cases("hydra.scala.syntax.Name",
                        ScalaSyntax.nameImporteeName(v("in")),
                        field("value", lambda("s", v("s")))))))) ),
              v("importees")))))))

  private val importerToExprBody = lambda("imp",
    let(Seq(
      field("ref", ScalaSyntax.importerRef(v("imp"))),
      field("importees", ScalaSyntax.importerImportees(v("imp"))),
      field("refName", importerRefName),
      field("forImportees", importerForImporteesInlined)),
      spaceSep(list(
        cstS("import"),
        noSep(list(
          cst(v("refName")),
          v("forImportees")))))))

  lazy val importerToExprDef: Definition =
    define(NS, "importerToExpr").doc("Convert an importer to an expression").to(importerToExprBody)

  // ---- initToExpr ----
  private val initToExprBody = lambda("init",
    typeToExprCall(ScalaSyntax.initTpe(v("init"))))

  lazy val initToExprDef: Definition =
    define(NS, "initToExpr").doc("Convert an init to an expression").to(initToExprBody)

  // ---- litToExpr ----
  private val litToExprBody = lambda("lit",
    casesWithDefault("hydra.scala.syntax.Lit",
      v("lit"),
      cstS("TODO:literal"),
      field("boolean", lambda("b",
        cst(ifElse(v("b"), string("true"), string("false"))))),
      field("byte", lambda("i",
        cst(cat2(applyP("hydra.lib.literals.showInt8", v("i")), string(".toByte"))))),
      field("short", lambda("i",
        cst(cat2(applyP("hydra.lib.literals.showInt16", v("i")), string(".toShort"))))),
      field("int", lambda("i",
        cst(applyP("hydra.lib.literals.showInt32", v("i"))))),
      field("long", lambda("i",
        cst(cat2(applyP("hydra.lib.literals.showInt64", v("i")), string("L"))))),
      field("float", lambda("f",
        cst(applyP(local("scalaFloatLiteralText"),
          string("Float"), string("f"),
          applyP("hydra.lib.literals.showFloat32", v("f")))))),
      field("double", lambda("f",
        cst(applyP(local("scalaFloatLiteralText"),
          string("Double"), string(""),
          applyP("hydra.lib.literals.showFloat64", v("f")))))),
      field("unit", constant(cstS("()"))),
      field("string", lambda("s",
        cst(cat2(string("\""), cat2(
          applyP("hydra.jvm.serde.escapeJavaString", v("s")),
          string("\"")))))),
      field("bytes", lambda("bs",
        cst(cat2(string("Array[Byte]("),
          cat2(
            applyP("hydra.lib.strings.intercalate",
              string(", "),
              map(
                lambda("b", cat2(applyP("hydra.lib.literals.showInt32", v("b")), string(".toByte"))),
                v("bs"))),
            string(")"))))))))

  lazy val litToExprDef: Definition =
    define(NS, "litToExpr").doc("Convert a literal to an expression").to(litToExprBody)

  // ---- matchOp ----
  private val matchOpBody =
    AstDsl.op(AstDsl.symbol(string("match")))(
      AstDsl.padding(AstDsl.wsSpace)(AstDsl.wsBreakAndIndent(string("  "))))(
      AstDsl.precedence(int32(0)))(
      AstDsl.associativityNone)

  lazy val matchOpDef: Definition =
    define(NS, "matchOp").doc("The match operator").to(matchOpBody)

  // ---- modToExpr ----
  private val modToExprBody = lambda("m",
    cases("hydra.scala.syntax.Mod", v("m"),
      field("case", constant(cstS("case"))),
      field("sealed", constant(cstS("sealed"))),
      field("abstract", constant(cstS("abstract"))),
      field("final", constant(cstS("final"))),
      field("override", constant(cstS("override"))),
      field("implicit", constant(cstS("implicit"))),
      field("lazy", constant(cstS("lazy"))),
      field("private", lambda("_", cstS("private"))),
      field("protected", lambda("_", cstS("protected")))))

  lazy val modToExprDef: Definition =
    define(NS, "modToExpr").doc("Convert a modifier to an expression").to(modToExprBody)

  // ---- nameToExpr ----
  private val nameToExprBody = lambda("name",
    cases("hydra.scala.syntax.Name", v("name"),
      field("value", lambda("s", cst(v("s"))))))

  lazy val nameToExprDef: Definition =
    define(NS, "nameToExpr").doc("Convert a name to an expression").to(nameToExprBody)

  // ---- patToExpr ----
  private val patExtractArm = lambda("pe",
    let(Seq(
      field("fun", ScalaSyntax.extractPatFun(v("pe"))),
      field("args", ScalaSyntax.extractPatArgs(v("pe")))),
      ifElse(
        listNull(v("args")),
        termToExprCall(v("fun")),
        noSep(list(
          termToExprCall(v("fun")),
          parenList(map(v(local("patToExpr")), v("args"))))))))

  private val patToExprBody = lambda("pat",
    cases("hydra.scala.syntax.Pat", v("pat"),
      field("extract", patExtractArm),
      field("var", lambda("pv",
        dataNameToExprCall(ScalaSyntax.varPatName(v("pv"))))),
      field("wildcard", constant(cstS("_")))))

  lazy val patToExprDef: Definition =
    define(NS, "patToExpr").doc("Convert a pattern to an expression").to(patToExprBody)

  // ---- pkgToExpr ----
  private val pkgToExprBody = lambda("pkg",
    let(Seq(
      field("name", ScalaSyntax.pkgName(v("pkg"))),
      field("stats", ScalaSyntax.pkgStats(v("pkg"))),
      field("package",
        spaceSep(list(cstS("package"), dataNameToExprCall(v("name")))))),
      doubleNewlineSep(listConcat(list(
        list(v("package")),
        map(v(local("statToExpr")), v("stats")))))))

  lazy val pkgToExprDef: Definition =
    define(NS, "pkgToExpr").doc("Convert a package to an expression").to(pkgToExprBody)

  // ---- scalaFloatLiteralText ----
  private val scalaFloatLiteralTextBody = lambda("prefix", lambda("suffix", lambda("s",
    ifElse(
      eq(v("s"), string("NaN")),
      cat2(v("prefix"), string(".NaN")),
      ifElse(
        eq(v("s"), string("Infinity")),
        cat2(v("prefix"), string(".PositiveInfinity")),
        ifElse(
          eq(v("s"), string("-Infinity")),
          cat2(v("prefix"), string(".NegativeInfinity")),
          cat2(v("s"), v("suffix"))))))))

  lazy val scalaFloatLiteralTextDef: Definition =
    define(NS, "scalaFloatLiteralText").to(scalaFloatLiteralTextBody)

  // ---- statToExpr ----
  private val statToExprBody = lambda("stat",
    cases("hydra.scala.syntax.Stat", v("stat"),
      field("term", lambda("t", termToExprCall(v("t")))),
      field("defn", lambda("def", defnToExprCall(v("def")))),
      field("importExport", lambda("ie", importExportStatToExprCall(v("ie"))))))

  lazy val statToExprDef: Definition =
    define(NS, "statToExpr").doc("Convert a statement to an expression").to(statToExprBody)

  // ---- termToExpr — 8-arm cases on Data ----

  private val termApplyArm = lambda("app",
    let(Seq(
      field("fun", ScalaSyntax.applyDataFun(v("app"))),
      field("args", ScalaSyntax.applyDataArgs(v("app")))),
      noSep(list(
        termToExprCall(v("fun")),
        parenList(map(v(local("termToExpr")), v("args")))))))

  private val termAssignArm = lambda("a",
    let(Seq(
      field("lhs", ScalaSyntax.assignDataLhs(v("a"))),
      field("rhs", ScalaSyntax.assignDataRhs(v("a")))),
      spaceSep(list(
        termToExprCall(v("lhs")),
        cstS("->"),
        termToExprCall(v("rhs"))))))

  private val termTupleArm = lambda("tup",
    parenList(map(v(local("termToExpr")),
      ScalaSyntax.tupleDataArgs(v("tup")))))

  private val termMatchArm = lambda("m",
    let(Seq(
      field("expr", ScalaSyntax.matchDataExpr(v("m"))),
      field("mCases", ScalaSyntax.matchDataCases(v("m")))),
      ifx(
        v(local("matchOp")),
        termToExprCall(v("expr")),
        newlineSep(map(v(local("caseToExpr")), v("mCases"))))))

  private val termBlockArm = lambda("blk",
    let(Seq(field("stats", ScalaSyntax.blockDataStats(v("blk")))),
      curlyBlock(fullBlockStyle,
        newlineSep(map(v(local("statToExpr")), v("stats"))))))

  private val termToExprBody = lambda("term",
    cases("hydra.scala.syntax.Data", v("term"),
      field("lit", lambda("lit", litToExprCall(v("lit")))),
      field("ref", lambda("ref", dataRefToExprCall(v("ref")))),
      field("apply", termApplyArm),
      field("assign", termAssignArm),
      field("tuple", termTupleArm),
      field("match", termMatchArm),
      field("function", lambda("f", dataFunctionToExprCall(v("f")))),
      field("block", termBlockArm)))

  lazy val termToExprDef: Definition =
    define(NS, "termToExpr").doc("Convert a term to an expression").to(termToExprBody)

  // ---- typeNameToExpr ----
  private val typeNameToExprBody = lambda("tn",
    cst(ScalaSyntax.nameTypeValue(v("tn"))))

  lazy val typeNameToExprDef: Definition =
    define(NS, "typeNameToExpr").doc("Convert a type name to an expression").to(typeNameToExprBody)

  // ---- typeParamToExpr ----
  private val typeParamToExprBody = lambda("tp",
    nameToExprCall(ScalaSyntax.paramTypeName(v("tp"))))

  lazy val typeParamToExprDef: Definition =
    define(NS, "typeParamToExpr").doc("Convert a type parameter to an expression").to(typeParamToExprBody)

  // ---- typeToExpr ----

  private val typeRefArm = lambda("tr",
    cases("hydra.scala.syntax.RefType", v("tr"),
      field("name", lambda("name", typeNameToExprCall(v("name"))))))

  private val typeApplyArm = lambda("ta",
    let(Seq(
      field("fun", ScalaSyntax.applyTypeTpe(v("ta"))),
      field("args", ScalaSyntax.applyTypeArgs(v("ta")))),
      noSep(list(
        typeToExprCall(v("fun")),
        bracketList(inlineStyle, map(v(local("typeToExpr")), v("args")))))))

  private val typeFunctionArm = lambda("tf",
    let(Seq(
      field("cod", ScalaSyntax.functionTypeRes(v("tf"))),
      field("dom",
        optFromOptional(v("cod"),
          listMaybeHead(ScalaSyntax.functionTypeParams(v("tf")))))),
      ifx(
        v(local("functionArrowOp")),
        typeToExprCall(v("dom")),
        typeToExprCall(v("cod")))))

  private val typeLambdaArm = lambda("tl",
    let(Seq(
      field("params", ScalaSyntax.lambdaTypeTparams(v("tl"))),
      field("body", ScalaSyntax.lambdaTypeTpe(v("tl")))),
      noSep(list(
        typeToExprCall(v("body")),
        bracketList(inlineStyle, map(v(local("typeParamToExpr")), v("params")))))))

  private val typeVarArm = lambda("tv",
    typeNameToExprCall(ScalaSyntax.varTypeName(v("tv"))))

  private val typeToExprBody = lambda("typ",
    cases("hydra.scala.syntax.Type", v("typ"),
      field("ref", typeRefArm),
      field("apply", typeApplyArm),
      field("function", typeFunctionArm),
      field("lambda", typeLambdaArm),
      field("var", typeVarArm)))

  lazy val typeToExprDef: Definition =
    define(NS, "typeToExpr").doc("Convert a type to an expression").to(typeToExprBody)

  // ===== Module assembly — order matches Haskell `definitions` list =====

  val DEFINITIONS: Seq[Definition] = Seq(
    dotOpDef,
    functionArrowOpDef,
    matchOpDef,
    scalaFloatLiteralTextDef,
    caseToExprDef,
    dataFunctionToExprDef,
    dataNameToExprDef,
    dataParamToExprDef,
    dataRefToExprDef,
    dataSelectToExprDef,
    defnToExprDef,
    importExportStatToExprDef,
    importerToExprDef,
    initToExprDef,
    litToExprDef,
    modToExprDef,
    nameToExprDef,
    patToExprDef,
    pkgToExprDef,
    statToExprDef,
    termToExprDef,
    typeToExprDef,
    typeNameToExprDef,
    typeParamToExprDef)

  val module_ : Module = Module(
    name = NS,
    metadata = Some(EntityMetadata(
      description = Some("Serialization functions for converting Scala AST to abstract expressions"),
      comments = Seq.empty,
      seeAlso = Seq.empty,
      lifecycle = None)),
    dependencies = DEPS.map(Helpers.unqualifiedDep),
    definitions = DEFINITIONS)

end Serde
