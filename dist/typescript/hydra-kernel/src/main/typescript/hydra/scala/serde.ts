// Note: this is an automatically generated file. Do not edit.

/**
 * Serialization functions for converting Scala AST to abstract expressions
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Graph from "../graph.js";
import * as JavaSerde from "../java/serde.js";
import * as JsonModel from "../json/model.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMath from "../lib/math.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ScalaSyntax from "./syntax.js";
import * as Serialization from "../serialization.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const dotOp: Ast.Op = ({
    symbol: ".",
    padding: ({
    left: ({ tag: "none" }),
    right: ({ tag: "none" })
  }),
    precedence: 0,
    associativity: ({ tag: "left" })
  });

export const functionArrowOp: Ast.Op = Serialization.op("=>")(LibMath.negate(1))(({ tag: "right" }));

export const matchOp: Ast.Op = ({
    symbol: "match",
    padding: ({
    left: ({ tag: "space" }),
    right: ({ tag: "breakAndIndent", value: "  " })
  }),
    precedence: 0,
    associativity: ({ tag: "none" })
  });

export function scalaFloatLiteralText(prefix: string): ((x: string) => ((x: string) => string)) {
  return ((suffix: string) => ((s: string) => LibLogic.ifElse(LibEquality.equal(s)("NaN"))(LibStrings.cat2(prefix)(".NaN"))(LibLogic.ifElse(LibEquality.equal(s)("Infinity"))(LibStrings.cat2(prefix)(".PositiveInfinity"))(LibLogic.ifElse(LibEquality.equal(s)("-Infinity"))(LibStrings.cat2(prefix)(".NegativeInfinity"))(LibStrings.cat2(s)(suffix))))));
}

export function writeCase(c: ScalaSyntax.Case): Ast.Expr {
  return (() => {
  const pat = ((_x) => _x.pat)(c);
  const term = ((_x) => _x.body)(c);
  return Serialization.spaceSep([Serialization.cst("case"), writePat(pat), Serialization.cst("=>"), writeTerm(term)]);
})();
}

export function writeData_FunctionData(ft: ScalaSyntax.Data_FunctionData): Ast.Expr {
  return (() => {
  const _m = ft;
  switch (_m.tag) {
    case "function": return ((f: ScalaSyntax.Data_Function) => (() => {
  const params = ((_x) => _x.params)(f);
  const body = ((_x) => _x.body)(f);
  const bodyExpr = writeTerm(body);
  const bodyLen = Serialization.expressionLength(bodyExpr);
  return LibLogic.ifElse(LibEquality.gt(bodyLen)(60))(Serialization.noSep([Serialization.parenList(false)(LibLists.map(writeData_Param)(params)), Serialization.cst(" =>\n  "), bodyExpr]))(Serialization.spaceSep([Serialization.parenList(false)(LibLists.map(writeData_Param)(params)), Serialization.cst("=>"), bodyExpr]));
})())((_m as any).value);
  }
})();
}

export function writeData_Name(dn: ScalaSyntax.Data_Name): Ast.Expr {
  return Serialization.cst(((_x) => _x)(((_x) => _x.value)(dn)));
}

export function writeData_Param(dp: ScalaSyntax.Data_Param): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(dp);
  const stype = ((_x) => _x.decltpe)(dp);
  return Serialization.noSep(LibMaybes.cat([LibMaybes.pure(writeName(name)), LibMaybes.map(((t: ScalaSyntax.Type) => Serialization.spaceSep([Serialization.cst(":"), writeType(t)])))(stype)]));
})();
}

export function writeData_Ref(ref: ScalaSyntax.Data_Ref): Ast.Expr {
  return (() => {
  const _m = ref;
  switch (_m.tag) {
    case "name": return ((name: ScalaSyntax.Data_Name) => writeData_Name(name))((_m as any).value);
    case "select": return ((sel: ScalaSyntax.Data_Select) => writeData_Select(sel))((_m as any).value);
  }
})();
}

export function writeData_Select(sel: ScalaSyntax.Data_Select): Ast.Expr {
  return (() => {
  const arg = ((_x) => _x.qual)(sel);
  const name = ((_x) => _x.name)(sel);
  return Serialization.ifx(dotOp)(writeTerm(arg))(writeTerm(({ tag: "ref", value: ({ tag: "name", value: name }) })));
})();
}

export function writeDefn(def: ScalaSyntax.Defn): Ast.Expr {
  return (() => {
  const _m = def;
  switch (_m.tag) {
    case "def": return ((dd: ScalaSyntax.Defn_Def) => (() => {
  const name = ((_x) => _x.name)(dd);
  const tparams = ((_x) => _x.tparams)(dd);
  const paramss = ((_x) => _x.paramss)(dd);
  const scod = ((_x) => _x.decltpe)(dd);
  const body = ((_x) => _x.body)(dd);
  const tparamsExpr = LibLogic.ifElse(LibLists.null_(tparams))(null)(LibMaybes.pure(Serialization.bracketList(Serialization.inlineStyle)(LibLists.map(writeType_Param)(tparams))));
  const scodExpr = LibMaybes.map(((t: ScalaSyntax.Type) => Serialization.spaceSep([Serialization.cst(":"), writeType(t)])))(scod);
  const paramssExprs = LibLists.map(((ps: ReadonlyArray<ScalaSyntax.Data_Param>) => Serialization.parenList(false)(LibLists.map(writeData_Param)(ps))))(paramss);
  const nameAndParams = Serialization.noSep(LibMaybes.cat(LibLists.concat([[LibMaybes.pure(writeData_Name(name))], [tparamsExpr], LibLists.map(((pe: Ast.Expr) => LibMaybes.pure(pe)))(paramssExprs), [scodExpr]])));
  return (() => {
  const bodyExpr = writeTerm(body);
  return (() => {
  const defSig = Serialization.spaceSep([Serialization.cst("def"), nameAndParams, Serialization.cst("=")]);
  return (() => {
  const bodyLen = Serialization.expressionLength(bodyExpr);
  return LibLogic.ifElse(LibEquality.gt(bodyLen)(80))(Serialization.noSep([defSig, Serialization.cst("\n  "), bodyExpr]))(Serialization.spaceSep([defSig, bodyExpr]));
})();
})();
})();
})())((_m as any).value);
    case "type": return ((dt: ScalaSyntax.Defn_Type) => (() => {
  const name = ((_x) => _x.name)(dt);
  const tparams = ((_x) => _x.tparams)(dt);
  const body = ((_x) => _x.body)(dt);
  return Serialization.spaceSep(LibMaybes.cat([LibMaybes.pure(Serialization.cst("type")), LibMaybes.pure(writeType_Name(name)), LibLogic.ifElse(LibLists.null_(tparams))(null)(LibMaybes.pure(Serialization.bracketList(Serialization.inlineStyle)(LibLists.map(writeType_Param)(tparams)))), LibMaybes.pure(Serialization.cst("=")), LibMaybes.pure(writeType(body))]));
})())((_m as any).value);
    case "val": return ((dv: ScalaSyntax.Defn_Val) => (() => {
  const mods = ((_x) => _x.mods)(dv);
  const pats = ((_x) => _x.pats)(dv);
  const typ = ((_x) => _x.decltpe)(dv);
  const rhs = ((_x) => _x.rhs)(dv);
  const firstPat = LibLists.head(pats);
  const patName = (() => {
  const _m = firstPat;
  switch (_m.tag) {
    case "var": return ((pv: ScalaSyntax.Pat_Var) => ((_x) => _x.name)(pv))((_m as any).value);
  }
})();
  const nameStr = ((_x) => _x)(((_x) => _x.value)(patName));
  const nameAndType = LibMaybes.maybe(Serialization.cst(nameStr))(((t: ScalaSyntax.Type) => Serialization.spaceSep([Serialization.cst(LibStrings.cat2(nameStr)(":")), writeType(t)])))(typ);
  const valKeyword = LibLogic.ifElse(LibLists.null_(mods))("val")("lazy val");
  return Serialization.spaceSep([Serialization.cst(valKeyword), nameAndType, Serialization.cst("="), writeTerm(rhs)]);
})())((_m as any).value);
    case "class": return ((dc: ScalaSyntax.Defn_Class) => (() => {
  const mods = ((_x) => _x.mods)(dc);
  const name = ((_x) => _x.name)(dc);
  const tparams = ((_x) => _x.tparams)(dc);
  const ctor = ((_x) => _x.ctor)(dc);
  const paramss = ((_x) => _x.paramss)(ctor);
  const tparamsExpr = LibLogic.ifElse(LibLists.null_(tparams))(null)(LibMaybes.pure(Serialization.bracketList(Serialization.inlineStyle)(LibLists.map(writeType_Param)(tparams))));
  const paramsExpr = LibLogic.ifElse(LibLists.null_(paramss))(null)(LibMaybes.pure(Serialization.parenList(false)(LibLists.map(writeData_Param)(LibLists.concat(paramss)))));
  const nameAndParams = Serialization.noSep(LibMaybes.cat([LibMaybes.pure(writeType_Name(name)), tparamsExpr, paramsExpr]));
  return Serialization.spaceSep(LibLists.concat([LibLists.map(writeMod)(mods), [Serialization.cst("class"), nameAndParams]]));
})())((_m as any).value);
    case "enum": return ((de: ScalaSyntax.Defn_Enum) => (() => {
  const name = ((_x) => _x.name)(de);
  const tparams = ((_x) => _x.tparams)(de);
  const template = ((_x) => _x.template)(de);
  const stats = ((_x) => _x.stats)(template);
  const enumHeader = Serialization.spaceSep([Serialization.cst("enum"), Serialization.noSep(LibMaybes.cat([LibMaybes.pure(writeType_Name(name)), LibLogic.ifElse(LibLists.null_(tparams))(null)(LibMaybes.pure(Serialization.bracketList(Serialization.inlineStyle)(LibLists.map(writeType_Param)(tparams))))])), Serialization.cst(":")]);
  const enumCases = LibLists.map(((s: ScalaSyntax.Stat) => Serialization.spaceSep([Serialization.cst("  "), writeStat(s)])))(stats);
  return Serialization.newlineSep(LibLists.concat([[enumHeader], enumCases]));
})())((_m as any).value);
    case "enumCase": return ((dec: ScalaSyntax.Defn_EnumCase) => (() => {
  const name = ((_x) => _x.name)(dec);
  const ctor = ((_x) => _x.ctor)(dec);
  const inits = ((_x) => _x.inits)(dec);
  const paramss = ((_x) => _x.paramss)(ctor);
  const allParams = LibLists.concat(paramss);
  const params = LibLogic.ifElse(LibLists.null_(allParams))(Serialization.cst(""))(Serialization.parenList(false)(LibLists.map(writeData_Param)(allParams)));
  const extendsClause = LibLogic.ifElse(LibLists.null_(inits))(Serialization.cst(""))(Serialization.spaceSep([Serialization.cst("extends"), Serialization.commaSep(Serialization.inlineStyle)(LibLists.map(writeInit)(inits))]));
  return Serialization.spaceSep([Serialization.cst("case"), Serialization.noSep([writeData_Name(name), params]), extendsClause]);
})())((_m as any).value);
  }
})();
}

export function writeImportExportStat(ie: ScalaSyntax.ImportExportStat): Ast.Expr {
  return (() => {
  const _m = ie;
  switch (_m.tag) {
    case "import": return ((imp: ScalaSyntax.Import) => (() => {
  const importers = ((_x) => _x.importers)(imp);
  return Serialization.newlineSep(LibLists.map(writeImporter)(importers));
})())((_m as any).value);
  }
})();
}

export function writeImporter(imp: ScalaSyntax.Importer): Ast.Expr {
  return (() => {
  const ref = ((_x) => _x.ref)(imp);
  const importees = ((_x) => _x.importees)(imp);
  const refName = (() => {
  const _m = ref;
  switch (_m.tag) {
    case "name": return ((dn: ScalaSyntax.Data_Name) => ((_x) => _x)(((_x) => _x.value)(dn)))((_m as any).value);
  }
})();
  const forImportees = LibLogic.ifElse(LibLists.null_(importees))(Serialization.cst(""))(LibLogic.ifElse(LibEquality.equal(LibLists.length(importees))(1))(Serialization.noSep([Serialization.cst("."), (() => {
  const _m = LibLists.head(importees);
  switch (_m.tag) {
    case "wildcard": return ((_: void) => Serialization.cst("*"))((_m as any).value);
    case "name": return ((in_: ScalaSyntax.Importee_Name) => Serialization.cst((() => {
  const _m = ((_x) => _x.name)(in_);
  switch (_m.tag) {
    case "value": return ((s: string) => s)((_m as any).value);
  }
})()))((_m as any).value);
  }
})()]))(Serialization.noSep([Serialization.cst("."), Serialization.curlyBracesList(null)(Serialization.inlineStyle)(LibLists.map(((it: ScalaSyntax.Importee) => (() => {
  const _m = it;
  switch (_m.tag) {
    case "wildcard": return ((_: void) => Serialization.cst("*"))((_m as any).value);
    case "name": return ((in_: ScalaSyntax.Importee_Name) => Serialization.cst((() => {
  const _m = ((_x) => _x.name)(in_);
  switch (_m.tag) {
    case "value": return ((s: string) => s)((_m as any).value);
  }
})()))((_m as any).value);
  }
})()))(importees))])));
  return Serialization.spaceSep([Serialization.cst("import"), Serialization.noSep([Serialization.cst(refName), forImportees])]);
})();
}

export function writeInit(init: ScalaSyntax.Init): Ast.Expr {
  return writeType(((_x) => _x.tpe)(init));
}

export function writeLit(lit: ScalaSyntax.Lit): Ast.Expr {
  return (() => {
  const _m = lit;
  switch (_m.tag) {
    case "boolean": return ((b: boolean) => Serialization.cst(LibLogic.ifElse(b)("true")("false")))((_m as any).value);
    case "byte": return ((i: number) => Serialization.cst(LibStrings.cat2(LibLiterals.showInt8(i))(".toByte")))((_m as any).value);
    case "short": return ((i: bigint) => Serialization.cst(LibStrings.cat2(LibLiterals.showInt16(i))(".toShort")))((_m as any).value);
    case "int": return ((i: number) => Serialization.cst(LibLiterals.showInt32(i)))((_m as any).value);
    case "long": return ((i: bigint) => Serialization.cst(LibStrings.cat2(LibLiterals.showInt64(i))("L")))((_m as any).value);
    case "float": return ((f: number) => Serialization.cst(scalaFloatLiteralText("Float")("f")(LibLiterals.showFloat32(f))))((_m as any).value);
    case "double": return ((f: number) => Serialization.cst(scalaFloatLiteralText("Double")("")(LibLiterals.showFloat64(f))))((_m as any).value);
    case "unit": return ((_: void) => Serialization.cst("()"))((_m as any).value);
    case "string": return ((s: string) => Serialization.cst(LibStrings.cat2("\"")(LibStrings.cat2(JavaSerde.escapeJavaString(s))("\""))))((_m as any).value);
    case "bytes": return ((bs: ReadonlyArray<number>) => Serialization.cst(LibStrings.cat2("Array[Byte](")(LibStrings.cat2(LibStrings.intercalate(", ")(LibLists.map(((b: number) => LibStrings.cat2(LibLiterals.showInt32(b))(".toByte")))(bs)))(")"))))((_m as any).value);
    default: return Serialization.cst("TODO:literal")(_m);
  }
})();
}

export function writeMod(m: ScalaSyntax.Mod): Ast.Expr {
  return (() => {
  const _m = m;
  switch (_m.tag) {
    case "case": return ((_: void) => Serialization.cst("case"))((_m as any).value);
    case "sealed": return ((_: void) => Serialization.cst("sealed"))((_m as any).value);
    case "abstract": return ((_: void) => Serialization.cst("abstract"))((_m as any).value);
    case "final": return ((_: void) => Serialization.cst("final"))((_m as any).value);
    case "override": return ((_: void) => Serialization.cst("override"))((_m as any).value);
    case "implicit": return ((_: void) => Serialization.cst("implicit"))((_m as any).value);
    case "lazy": return ((_: void) => Serialization.cst("lazy"))((_m as any).value);
    case "private": return ((_: ScalaSyntax.Mod_Private) => Serialization.cst("private"))((_m as any).value);
    case "protected": return ((_: ScalaSyntax.Mod_Protected) => Serialization.cst("protected"))((_m as any).value);
  }
})();
}

export function writeName(name: ScalaSyntax.Name): Ast.Expr {
  return (() => {
  const _m = name;
  switch (_m.tag) {
    case "value": return ((s: string) => Serialization.cst(s))((_m as any).value);
  }
})();
}

export function writePat(pat: ScalaSyntax.Pat): Ast.Expr {
  return (() => {
  const _m = pat;
  switch (_m.tag) {
    case "extract": return ((pe: ScalaSyntax.Pat_Extract) => (() => {
  const fun = ((_x) => _x.fun)(pe);
  const args = ((_x) => _x.args)(pe);
  return LibLogic.ifElse(LibLists.null_(args))(writeTerm(fun))(Serialization.noSep([writeTerm(fun), Serialization.parenList(false)(LibLists.map(writePat)(args))]));
})())((_m as any).value);
    case "var": return ((pv: ScalaSyntax.Pat_Var) => writeData_Name(((_x) => _x.name)(pv)))((_m as any).value);
    case "wildcard": return ((_: void) => Serialization.cst("_"))((_m as any).value);
  }
})();
}

export function writePkg(pkg: ScalaSyntax.Pkg): Ast.Expr {
  return (() => {
  const name = ((_x) => _x.name)(pkg);
  const stats = ((_x) => _x.stats)(pkg);
  const package_ = Serialization.spaceSep([Serialization.cst("package"), writeData_Name(name)]);
  return Serialization.doubleNewlineSep(LibLists.concat([[package_], LibLists.map(writeStat)(stats)]));
})();
}

export function writeStat(stat: ScalaSyntax.Stat): Ast.Expr {
  return (() => {
  const _m = stat;
  switch (_m.tag) {
    case "term": return ((t: ScalaSyntax.Data) => writeTerm(t))((_m as any).value);
    case "defn": return ((def: ScalaSyntax.Defn) => writeDefn(def))((_m as any).value);
    case "importExport": return ((ie: ScalaSyntax.ImportExportStat) => writeImportExportStat(ie))((_m as any).value);
  }
})();
}

export function writeTerm(term: ScalaSyntax.Data): Ast.Expr {
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "lit": return ((lit: ScalaSyntax.Lit) => writeLit(lit))((_m as any).value);
    case "ref": return ((ref: ScalaSyntax.Data_Ref) => writeData_Ref(ref))((_m as any).value);
    case "apply": return ((app: ScalaSyntax.Data_Apply) => (() => {
  const fun = ((_x) => _x.fun)(app);
  const args = ((_x) => _x.args)(app);
  return Serialization.noSep([writeTerm(fun), Serialization.parenList(false)(LibLists.map(writeTerm)(args))]);
})())((_m as any).value);
    case "assign": return ((a: ScalaSyntax.Data_Assign) => (() => {
  const lhs = ((_x) => _x.lhs)(a);
  const rhs = ((_x) => _x.rhs)(a);
  return Serialization.spaceSep([writeTerm(lhs), Serialization.cst("->"), writeTerm(rhs)]);
})())((_m as any).value);
    case "tuple": return ((tup: ScalaSyntax.Data_Tuple) => Serialization.parenList(false)(LibLists.map(writeTerm)(((_x) => _x.args)(tup))))((_m as any).value);
    case "match": return ((m: ScalaSyntax.Data_Match) => (() => {
  const expr = ((_x) => _x.expr)(m);
  const mCases = ((_x) => _x.cases)(m);
  return Serialization.ifx(matchOp)(writeTerm(expr))(Serialization.newlineSep(LibLists.map(writeCase)(mCases)));
})())((_m as any).value);
    case "functionData": return ((ft: ScalaSyntax.Data_FunctionData) => writeData_FunctionData(ft))((_m as any).value);
    case "block": return ((blk: ScalaSyntax.Data_Block) => (() => {
  const stats = ((_x) => _x.stats)(blk);
  return Serialization.curlyBlock(Serialization.fullBlockStyle)(Serialization.newlineSep(LibLists.map(writeStat)(stats)));
})())((_m as any).value);
  }
})();
}

export function writeType(typ: ScalaSyntax.Type): Ast.Expr {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "ref": return ((tr: ScalaSyntax.Type_Ref) => (() => {
  const _m = tr;
  switch (_m.tag) {
    case "name": return ((name: ScalaSyntax.Type_Name) => writeType_Name(name))((_m as any).value);
  }
})())((_m as any).value);
    case "apply": return ((ta: ScalaSyntax.Type_Apply) => (() => {
  const fun = ((_x) => _x.tpe)(ta);
  const args = ((_x) => _x.args)(ta);
  return Serialization.noSep([writeType(fun), Serialization.bracketList(Serialization.inlineStyle)(LibLists.map(writeType)(args))]);
})())((_m as any).value);
    case "functionType": return ((ft: ScalaSyntax.Type_FunctionType) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "function": return ((tf: ScalaSyntax.Type_Function) => (() => {
  const dom = LibLists.head(((_x) => _x.params)(tf));
  const cod = ((_x) => _x.res)(tf);
  return Serialization.ifx(functionArrowOp)(writeType(dom))(writeType(cod));
})())((_m as any).value);
  }
})())((_m as any).value);
    case "lambda": return ((tl: ScalaSyntax.Type_Lambda) => (() => {
  const params = ((_x) => _x.tparams)(tl);
  const body = ((_x) => _x.tpe)(tl);
  return Serialization.noSep([writeType(body), Serialization.bracketList(Serialization.inlineStyle)(LibLists.map(writeType_Param)(params))]);
})())((_m as any).value);
    case "var": return ((tv: ScalaSyntax.Type_Var) => writeType_Name(((_x) => _x.name)(tv)))((_m as any).value);
  }
})();
}

export function writeType_Name(tn: ScalaSyntax.Type_Name): Ast.Expr {
  return Serialization.cst(((_x) => _x.value)(tn));
}

export function writeType_Param(tp: ScalaSyntax.Type_Param): Ast.Expr {
  return writeName(((_x) => _x.name)(tp));
}
