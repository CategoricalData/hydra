// Note: this is an automatically generated file. Do not edit.

/**
 * Python utilities for constructing Python syntax trees
 */



import * as Analysis from "../analysis.js";
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
import * as JsonModel from "../json/model.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as PythonEnvironment from "./environment.js";
import * as PythonNames from "./names.js";
import * as PythonSerde from "./serde.js";
import * as PythonSyntax from "./syntax.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Serialization from "../serialization.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function annotatedExpression(mcomment: string | null): ((x: PythonSyntax.Expression) => PythonSyntax.Expression) {
  return ((expr: PythonSyntax.Expression) => LibMaybes.maybe(expr)(((c: string) => pyPrimaryToPyExpression(primaryWithExpressionSlices(pyNameToPyPrimary("Annotated"))([expr, doubleQuotedString(c)]))))(mcomment));
}

export function annotatedStatement(mcomment: string | null): ((x: PythonSyntax.Statement) => PythonSyntax.Statement) {
  return ((stmt: PythonSyntax.Statement) => LibMaybes.maybe(stmt)(((c: string) => ({ tag: "annotated", value: ({
    comment: c,
    statement: stmt
  }) })))(mcomment));
}

export function assignment(name: PythonSyntax.Name): ((x: PythonSyntax.AnnotatedRhs) => PythonSyntax.Statement) {
  return ((rhs: PythonSyntax.AnnotatedRhs) => pyAssignmentToPyStatement(({ tag: "untyped", value: ({
    targets: [pyNameToPyStarTarget(name)],
    rhs: rhs,
    typeComment: null
  }) })));
}

export function assignmentStatement(name: PythonSyntax.Name): ((x: PythonSyntax.Expression) => PythonSyntax.Statement) {
  return ((expr: PythonSyntax.Expression) => assignment(name)(pyExpressionToPyAnnotatedRhs(expr)));
}

export function castTo(pytype: PythonSyntax.Expression): ((x: PythonSyntax.Expression) => PythonSyntax.Expression) {
  return ((pyexpr: PythonSyntax.Expression) => functionCall(pyNameToPyPrimary("cast"))([pytype, pyexpr]));
}

export function commentStatement(s: string): PythonSyntax.Statement {
  return pyExpressionToPyStatement(tripleQuotedString(s));
}

export function decodePyComparisonToPyAwaitPrimary(c: PythonSyntax.Comparison): PythonSyntax.Primary | null {
  return (() => {
  const rhs = ((_x) => _x.rhs)(c);
  const lhs = ((_x) => _x.lhs)(c);
  const orLhs = ((_x) => _x.lhs)(lhs);
  const orRhs = ((_x) => _x.rhs)(lhs);
  const xorLhs = ((_x) => _x.lhs)(orRhs);
  const xorRhs = ((_x) => _x.rhs)(orRhs);
  const andLhs = ((_x) => _x.lhs)(xorRhs);
  const andRhs = ((_x) => _x.rhs)(xorRhs);
  const shiftLhs = ((_x) => _x.lhs)(andRhs);
  const shiftRhs = ((_x) => _x.rhs)(andRhs);
  const sumLhs = ((_x) => _x.lhs)(shiftRhs);
  const sumRhs = ((_x) => _x.rhs)(shiftRhs);
  const termLhs = ((_x) => _x.lhs)(sumRhs);
  const termRhs = ((_x) => _x.rhs)(sumRhs);
  return LibLogic.ifElse(LibLogic.not(LibLists.null_(rhs)))(null)(LibLogic.ifElse(LibMaybes.isJust(orLhs))(null)(LibLogic.ifElse(LibMaybes.isJust(xorLhs))(null)(LibLogic.ifElse(LibMaybes.isJust(andLhs))(null)(LibLogic.ifElse(LibMaybes.isJust(shiftLhs))(null)(LibLogic.ifElse(LibMaybes.isJust(sumLhs))(null)(LibLogic.ifElse(LibMaybes.isJust(termLhs))(null)((() => {
  const _m = termRhs;
  switch (_m.tag) {
    case "simple": return ((power: PythonSyntax.Power) => decodePyPowerToPyPrimary(power))((_m as any).value);
    default: return null(_m);
  }
})())))))));
})();
}

export function decodePyConjunctionToPyPrimary(c: PythonSyntax.Conjunction): PythonSyntax.Primary | null {
  return (() => {
  const inversions = ((_x) => _x)(c);
  return LibLogic.ifElse(LibEquality.equal(LibLists.length(inversions))(1))(decodePyInversionToPyPrimary(LibLists.head(inversions)))(null);
})();
}

export function decodePyExpressionToPyPrimary(e: PythonSyntax.Expression): PythonSyntax.Primary | null {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "simple": return ((disj: PythonSyntax.Disjunction) => (() => {
  const conjunctions = ((_x) => _x)(disj);
  return LibLogic.ifElse(LibEquality.equal(LibLists.length(conjunctions))(1))(decodePyConjunctionToPyPrimary(LibLists.head(conjunctions)))(null);
})())((_m as any).value);
    default: return null(_m);
  }
})();
}

export function decodePyInversionToPyPrimary(i: PythonSyntax.Inversion): PythonSyntax.Primary | null {
  return (() => {
  const _m = i;
  switch (_m.tag) {
    case "simple": return ((comparison: PythonSyntax.Comparison) => decodePyComparisonToPyAwaitPrimary(comparison))((_m as any).value);
    default: return null(_m);
  }
})();
}

export function decodePyPowerToPyPrimary(p: PythonSyntax.Power): PythonSyntax.Primary | null {
  return (() => {
  const lhs = ((_x) => _x.lhs)(p);
  const await_ = ((_x) => _x.await)(lhs);
  const prim = ((_x) => _x.primary)(lhs);
  return LibLogic.ifElse(await_)(null)(prim);
})();
}

export function dottedAssignmentStatement(obj: PythonSyntax.Name): ((x: PythonSyntax.Name) => ((x: PythonSyntax.Expression) => PythonSyntax.Statement)) {
  return ((attr: PythonSyntax.Name) => ((expr: PythonSyntax.Expression) => (() => {
  const target = ({ tag: "unstarred", value: ({ tag: "project", value: ({
    primary: ({ tag: "atom", value: ({ tag: "name", value: obj }) }),
    name: attr
  }) }) });
  return pyAssignmentToPyStatement(({ tag: "untyped", value: ({
    targets: [target],
    rhs: pyExpressionToPyAnnotatedRhs(expr),
    typeComment: null
  }) }));
})()));
}

export function doubleQuotedString(s: string): PythonSyntax.Expression {
  return stringToPyExpression(({ tag: "double" }))(s);
}

export function findNamespaces(focusNs: Packaging.Namespace): ((x: ReadonlyArray<Packaging.Definition>) => Packaging.Namespaces<PythonSyntax.DottedName>) {
  return ((defs: ReadonlyArray<Packaging.Definition>) => (() => {
  const coreNs = "hydra.core";
  const namespaces = Analysis.namespacesForDefinitions(PythonNames.encodeNamespace)(focusNs)(defs);
  return LibLogic.ifElse(LibEquality.equal(((_x) => _x)(LibPairs.first(((_x) => _x.focus)(namespaces))))(((_x) => _x)(coreNs)))(namespaces)(({
    focus: ((_x) => _x.focus)(namespaces),
    mapping: LibMaps.insert(coreNs)(PythonNames.encodeNamespace(coreNs))(((_x) => _x.mapping)(namespaces))
  }));
})());
}

export function functionCall(func: PythonSyntax.Primary): ((x: ReadonlyArray<PythonSyntax.Expression>) => PythonSyntax.Expression) {
  return ((args: ReadonlyArray<PythonSyntax.Expression>) => pyPrimaryToPyExpression(primaryWithRhs(func)(({ tag: "call", value: pyExpressionsToPyArgs(args) }))));
}

export const getItemParams: PythonSyntax.Parameters = ({ tag: "paramNoDefault", value: ({
    paramNoDefault: [({
    param: ({
    name: "cls",
    annotation: null
  }),
    typeComment: null
  }), ({
    param: ({
    name: "item",
    annotation: null
  }),
    typeComment: null
  })],
    paramWithDefault: [],
    starEtc: null
  }) });

export function indentedBlock(mcomment: string | null): ((x: ReadonlyArray<ReadonlyArray<PythonSyntax.Statement>>) => PythonSyntax.Block) {
  return ((stmts: ReadonlyArray<ReadonlyArray<PythonSyntax.Statement>>) => (() => {
  const commentGroup = LibMaybes.maybe([])(((s: string) => [commentStatement(s)]))(mcomment);
  const groups = LibLists.filter(((g: ReadonlyArray<PythonSyntax.Statement>) => LibLogic.not(LibLists.null_(g))))(LibLists.cons(commentGroup)(stmts));
  return LibLogic.ifElse(LibLists.null_(groups))(({ tag: "indented", value: [[({ tag: "simple", value: [pyExpressionToPySimpleStatement(pyAtomToPyExpression(({ tag: "ellipsis" })))] })]] }))(({ tag: "indented", value: groups }));
})());
}

export function nameAndParams(pyName: PythonSyntax.Name): ((x: ReadonlyArray<PythonSyntax.Expression>) => PythonSyntax.Expression) {
  return ((params: ReadonlyArray<PythonSyntax.Expression>) => primaryAndParams(pyNameToPyPrimary(pyName))(params));
}

export function newtypeStatement(name: PythonSyntax.Name): ((x: string | null) => ((x: PythonSyntax.Expression) => PythonSyntax.Statement)) {
  return ((mcomment: string | null) => ((expr: PythonSyntax.Expression) => annotatedStatement(mcomment)(assignmentStatement(name)(functionCall(({ tag: "simple", value: ({ tag: "name", value: "NewType" }) }))([doubleQuotedString(((_x) => _x)(name)), expr])))));
}

export function orExpression(prims: ReadonlyArray<PythonSyntax.Primary>): PythonSyntax.Expression {
  return (() => {
  const build = ((prev: PythonSyntax.BitwiseOr | null) => ((ps: ReadonlyArray<PythonSyntax.Primary>) => LibLogic.ifElse(LibLists.null_(LibLists.tail(ps)))(({
    lhs: prev,
    rhs: pyPrimaryToPyBitwiseXor(LibLists.head(ps))
  }))(build(({
    lhs: prev,
    rhs: pyPrimaryToPyBitwiseXor(LibLists.head(ps))
  }))(LibLists.tail(ps)))));
  return pyBitwiseOrToPyExpression(build(null)(prims));
})();
}

export function primaryAndParams(prim: PythonSyntax.Primary): ((x: ReadonlyArray<PythonSyntax.Expression>) => PythonSyntax.Expression) {
  return ((params: ReadonlyArray<PythonSyntax.Expression>) => pyPrimaryToPyExpression(primaryWithExpressionSlices(prim)(params)));
}

export function primaryWithExpressionSlices(prim: PythonSyntax.Primary): ((x: ReadonlyArray<PythonSyntax.Expression>) => PythonSyntax.Primary) {
  return ((exprs: ReadonlyArray<PythonSyntax.Expression>) => primaryWithSlices(prim)(pyExpressionToPySlice(LibLists.head(exprs)))(LibLists.map(((e: PythonSyntax.Expression) => ({ tag: "slice", value: pyExpressionToPySlice(e) })))(LibLists.tail(exprs))));
}

export function primaryWithRhs(prim: PythonSyntax.Primary): ((x: PythonSyntax.PrimaryRhs) => PythonSyntax.Primary) {
  return ((rhs: PythonSyntax.PrimaryRhs) => ({ tag: "compound", value: ({
    primary: prim,
    rhs: rhs
  }) }));
}

export function primaryWithSlices(prim: PythonSyntax.Primary): ((x: PythonSyntax.Slice) => ((x: ReadonlyArray<PythonSyntax.SliceOrStarredExpression>) => PythonSyntax.Primary)) {
  return ((first: PythonSyntax.Slice) => ((rest: ReadonlyArray<PythonSyntax.SliceOrStarredExpression>) => primaryWithRhs(prim)(({ tag: "slices", value: ({
    head: first,
    tail: rest
  }) }))));
}

export function projectFromExpression(exp: PythonSyntax.Expression): ((x: PythonSyntax.Name) => PythonSyntax.Expression) {
  return ((name: PythonSyntax.Name) => (() => {
  const prim = ({ tag: "simple", value: ({ tag: "group", value: ({ tag: "expression", value: ({ tag: "simple", value: exp }) }) }) });
  return pyPrimaryToPyExpression(({ tag: "compound", value: ({
    primary: prim,
    rhs: ({ tag: "project", value: name })
  }) }));
})());
}

export function pyAssignmentToPyStatement(a: PythonSyntax.Assignment): PythonSyntax.Statement {
  return pySimpleStatementToPyStatement(({ tag: "assignment", value: a }));
}

export function pyAtomToPyExpression(atom: PythonSyntax.Atom): PythonSyntax.Expression {
  return pyPrimaryToPyExpression(({ tag: "simple", value: atom }));
}

export function pyBitwiseOrToPyConjunction(bor: PythonSyntax.BitwiseOr): PythonSyntax.Conjunction {
  return [({ tag: "simple", value: ({
    lhs: bor,
    rhs: []
  }) })];
}

export function pyBitwiseOrToPyExpression(bor: PythonSyntax.BitwiseOr): PythonSyntax.Expression {
  return pyConjunctionToPyExpression(pyBitwiseOrToPyConjunction(bor));
}

export function pyClassDefinitionToPyStatement(cd: PythonSyntax.ClassDefinition): PythonSyntax.Statement {
  return ({ tag: "compound", value: ({ tag: "classDef", value: cd }) });
}

export function pyClosedPatternToPyPatterns(p: PythonSyntax.ClosedPattern): PythonSyntax.Patterns {
  return ({ tag: "pattern", value: ({ tag: "or", value: [p] }) });
}

export function pyConjunctionToPyExpression(conj: PythonSyntax.Conjunction): PythonSyntax.Expression {
  return ({ tag: "simple", value: [conj] });
}

export function pyExpressionToBitwiseOr(e: PythonSyntax.Expression): PythonSyntax.BitwiseOr {
  return ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "group", value: ({ tag: "expression", value: ({ tag: "simple", value: e }) }) }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  });
}

export function pyExpressionToDisjunction(e: PythonSyntax.Expression): PythonSyntax.Disjunction {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "simple": return ((disj: PythonSyntax.Disjunction) => disj)((_m as any).value);
    default: return [pyPrimaryToPyConjunction(({ tag: "simple", value: ({ tag: "group", value: ({ tag: "expression", value: ({ tag: "simple", value: e }) }) }) }))](_m);
  }
})();
}

export function pyExpressionToPyAnnotatedRhs(expr: PythonSyntax.Expression): PythonSyntax.AnnotatedRhs {
  return ({ tag: "star", value: [({ tag: "simple", value: expr })] });
}

export function pyExpressionToPyPrimary(e: PythonSyntax.Expression): PythonSyntax.Primary {
  return LibMaybes.maybe(({ tag: "simple", value: ({ tag: "group", value: ({ tag: "expression", value: ({ tag: "simple", value: e }) }) }) }))(((prim: PythonSyntax.Primary) => prim))(decodePyExpressionToPyPrimary(e));
}

export function pyExpressionToPySimpleStatement(expr: PythonSyntax.Expression): PythonSyntax.SimpleStatement {
  return ({ tag: "starExpressions", value: [({ tag: "simple", value: expr })] });
}

export function pyExpressionToPySlice(expr: PythonSyntax.Expression): PythonSyntax.Slice {
  return ({ tag: "named", value: ({ tag: "simple", value: expr }) });
}

export function pyExpressionToPyStarNamedExpression(expr: PythonSyntax.Expression): PythonSyntax.StarNamedExpression {
  return ({ tag: "simple", value: ({ tag: "simple", value: expr }) });
}

export function pyExpressionToPyStatement(expr: PythonSyntax.Expression): PythonSyntax.Statement {
  return pySimpleStatementToPyStatement(pyExpressionToPySimpleStatement(expr));
}

export function pyExpressionsToPyArgs(exprs: ReadonlyArray<PythonSyntax.Expression>): PythonSyntax.Args {
  return ({
    positional: LibLists.map(((e: PythonSyntax.Expression) => ({ tag: "expression", value: e })))(exprs),
    kwargOrStarred: [],
    kwargOrDoubleStarred: []
  });
}

export function pyList(exprs: ReadonlyArray<PythonSyntax.Expression>): PythonSyntax.List {
  return LibLists.map(pyExpressionToPyStarNamedExpression)(exprs);
}

export function pyNameToPyExpression(name: PythonSyntax.Name): PythonSyntax.Expression {
  return pyPrimaryToPyExpression(pyNameToPyPrimary(name));
}

export function pyNameToPyNamedExpression(name: PythonSyntax.Name): PythonSyntax.NamedExpression {
  return ({ tag: "simple", value: pyNameToPyExpression(name) });
}

export function pyNameToPyPrimary(name: PythonSyntax.Name): PythonSyntax.Primary {
  return ({ tag: "simple", value: ({ tag: "name", value: name }) });
}

export function pyNameToPyStarTarget(name: PythonSyntax.Name): PythonSyntax.StarTarget {
  return ({ tag: "unstarred", value: ({ tag: "atom", value: ({ tag: "name", value: name }) }) });
}

export function pyNameToPyTypeParameter(name: PythonSyntax.Name): PythonSyntax.TypeParameter {
  return ({ tag: "simple", value: ({
    name: name,
    bound: null,
    default: null
  }) });
}

export const pyNone: PythonSyntax.Name = "None";

export function pyPrimaryToPyBitwiseOr(prim: PythonSyntax.Primary): PythonSyntax.BitwiseOr {
  return ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: prim
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  });
}

export function pyPrimaryToPyBitwiseXor(prim: PythonSyntax.Primary): PythonSyntax.BitwiseXor {
  return ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: prim
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  });
}

export function pyPrimaryToPyConjunction(prim: PythonSyntax.Primary): PythonSyntax.Conjunction {
  return pyBitwiseOrToPyConjunction(pyPrimaryToPyBitwiseOr(prim));
}

export function pyPrimaryToPyExpression(prim: PythonSyntax.Primary): PythonSyntax.Expression {
  return pyConjunctionToPyExpression(pyPrimaryToPyConjunction(prim));
}

export function pyPrimaryToPySlice(prim: PythonSyntax.Primary): PythonSyntax.Slice {
  return pyExpressionToPySlice(pyPrimaryToPyExpression(prim));
}

export function pySimpleStatementToPyStatement(s: PythonSyntax.SimpleStatement): PythonSyntax.Statement {
  return ({ tag: "simple", value: [s] });
}

export function raiseAssertionError(msg: string): PythonSyntax.Statement {
  return pySimpleStatementToPyStatement(({ tag: "raise", value: ({
    expression: functionCall(({ tag: "simple", value: ({ tag: "name", value: "AssertionError" }) }))([doubleQuotedString(msg)]),
    from: null
  }) }));
}

export function raiseTypeError(msg: string): PythonSyntax.Statement {
  return pySimpleStatementToPyStatement(({ tag: "raise", value: ({
    expression: functionCall(({ tag: "simple", value: ({ tag: "name", value: "TypeError" }) }))([doubleQuotedString(msg)]),
    from: null
  }) }));
}

export function returnSingle(expr: PythonSyntax.Expression): PythonSyntax.Statement {
  return pySimpleStatementToPyStatement(({ tag: "return", value: [({ tag: "simple", value: expr })] }));
}

export const selfOnlyParams: PythonSyntax.Parameters = ({ tag: "paramNoDefault", value: ({
    paramNoDefault: [({
    param: ({
    name: "self",
    annotation: null
  }),
    typeComment: null
  })],
    paramWithDefault: [],
    starEtc: null
  }) });

export const selfOtherParams: PythonSyntax.Parameters = ({ tag: "paramNoDefault", value: ({
    paramNoDefault: [({
    param: ({
    name: "self",
    annotation: null
  }),
    typeComment: null
  }), ({
    param: ({
    name: "other",
    annotation: null
  }),
    typeComment: null
  })],
    paramWithDefault: [],
    starEtc: null
  }) });

export function singleQuotedString(s: string): PythonSyntax.Expression {
  return stringToPyExpression(({ tag: "single" }))(s);
}

export function stringToPyExpression(style: PythonSyntax.QuoteStyle): ((x: string) => PythonSyntax.Expression) {
  return ((s: string) => pyAtomToPyExpression(({ tag: "string", value: ({
    value: s,
    quoteStyle: style
  }) })));
}

export const targetPythonVersion: PythonEnvironment.PythonVersion = ({ tag: "python310" });

export function tripleQuotedString(s: string): PythonSyntax.Expression {
  return stringToPyExpression(({ tag: "triple" }))(s);
}

export function typeAliasStatement(name: PythonSyntax.Name): ((x: ReadonlyArray<PythonSyntax.TypeParameter>) => ((x: string | null) => ((x: PythonSyntax.Expression) => PythonSyntax.Statement))) {
  return ((tparams: ReadonlyArray<PythonSyntax.TypeParameter>) => ((mcomment: string | null) => ((tyexpr: PythonSyntax.Expression) => annotatedStatement(mcomment)(pySimpleStatementToPyStatement(({ tag: "typeAlias", value: ({
    name: name,
    typeParams: tparams,
    expression: tyexpr
  }) }))))));
}

export function typeAliasStatement310<t0>(name: PythonSyntax.Name): ((x: t0) => ((x: string | null) => ((x: PythonSyntax.Expression) => PythonSyntax.Statement))) {
  return ((_tparams: t0) => ((mcomment: string | null) => ((tyexpr: PythonSyntax.Expression) => (() => {
  const quotedExpr = doubleQuotedString(Serialization.printExpr(PythonSerde.encodeExpression(tyexpr)));
  return annotatedStatement(mcomment)(pyAssignmentToPyStatement(({ tag: "typed", value: ({
    lhs: ({ tag: "name", value: name }),
    type: ({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: "TypeAlias" }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] }),
    rhs: pyExpressionToPyAnnotatedRhs(quotedExpr)
  }) })));
})())));
}

export function unionTypeClassStatements310(name: PythonSyntax.Name): ((x: string | null) => ((x: PythonSyntax.Expression) => ((x: ReadonlyArray<PythonSyntax.Statement>) => ReadonlyArray<PythonSyntax.Statement>))) {
  return ((mcomment: string | null) => ((tyexpr: PythonSyntax.Expression) => ((extraStmts: ReadonlyArray<PythonSyntax.Statement>) => (() => {
  const nameStr = ((_x) => _x)(name);
  return (() => {
  const metaName = LibStrings.cat2(LibStrings.cat2("_")(nameStr))("Meta");
  return (() => {
  const docString = Serialization.printExpr(PythonSerde.encodeExpression(tyexpr));
  return (() => {
  const returnObject = pySimpleStatementToPyStatement(({ tag: "return", value: [({ tag: "simple", value: ({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: "object" }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] }) })] }));
  return (() => {
  const getItemMethod = ({ tag: "compound", value: ({ tag: "function", value: ({
    decorators: null,
    raw: ({
    async: false,
    name: "__getitem__",
    typeParams: [],
    params: getItemParams,
    returnType: null,
    funcTypeComment: null,
    block: indentedBlock(null)([[returnObject]])
  })
  }) }) });
  return (() => {
  const metaClass = pyClassDefinitionToPyStatement(({
    decorators: null,
    name: metaName,
    typeParams: [],
    arguments: pyExpressionsToPyArgs([({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: "type" }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] })]),
    body: indentedBlock(null)([[getItemMethod]])
  }));
  return (() => {
  const docStmt = pyExpressionToPyStatement(tripleQuotedString(docString));
  return (() => {
  const bodyGroups = LibLogic.ifElse(LibLists.null_(extraStmts))((() => {
  const passStmt = pySimpleStatementToPyStatement(({ tag: "pass" }));
  return [[docStmt], [passStmt]];
})())([[docStmt], extraStmts]);
  return (() => {
  const metaclassArg = ({
    name: "metaclass",
    value: ({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: metaName }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] })
  });
  return (() => {
  const unionClass = annotatedStatement(mcomment)(pyClassDefinitionToPyStatement(({
    decorators: null,
    name: name,
    typeParams: [],
    arguments: ({
    positional: [],
    kwargOrStarred: [({ tag: "kwarg", value: metaclassArg })],
    kwargOrDoubleStarred: []
  }),
    body: indentedBlock(null)(bodyGroups)
  })));
  return [metaClass, unionClass];
})();
})();
})();
})();
})();
})();
})();
})();
})();
})())));
}

export function unitVariantMethods(className: PythonSyntax.Name): ReadonlyArray<PythonSyntax.Statement> {
  return (() => {
  const classNameStr = ((_x) => _x)(className);
  return (() => {
  const slotsStmt = assignmentStatement("__slots__")(pyPrimaryToPyExpression(({ tag: "simple", value: ({ tag: "tuple", value: [] }) })));
  return (() => {
  const returnIsinstance = pySimpleStatementToPyStatement(({ tag: "return", value: [({ tag: "simple", value: functionCall(({ tag: "simple", value: ({ tag: "name", value: "isinstance" }) }))([({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: "other" }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] }), ({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: className }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] })]) })] }));
  return (() => {
  const eqMethod = ({ tag: "compound", value: ({ tag: "function", value: ({
    decorators: null,
    raw: ({
    async: false,
    name: "__eq__",
    typeParams: [],
    params: selfOtherParams,
    returnType: null,
    funcTypeComment: null,
    block: indentedBlock(null)([[returnIsinstance]])
  })
  }) }) });
  return (() => {
  const returnHash = pySimpleStatementToPyStatement(({ tag: "return", value: [({ tag: "simple", value: functionCall(({ tag: "simple", value: ({ tag: "name", value: "hash" }) }))([doubleQuotedString(classNameStr)]) })] }));
  return (() => {
  const hashMethod = ({ tag: "compound", value: ({ tag: "function", value: ({
    decorators: null,
    raw: ({
    async: false,
    name: "__hash__",
    typeParams: [],
    params: selfOnlyParams,
    returnType: null,
    funcTypeComment: null,
    block: indentedBlock(null)([[returnHash]])
  })
  }) }) });
  return [slotsStmt, eqMethod, hashMethod];
})();
})();
})();
})();
})();
})();
}
