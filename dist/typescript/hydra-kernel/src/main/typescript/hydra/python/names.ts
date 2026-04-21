// Note: this is an automatically generated file. Do not edit.

/**
 * Python naming utilities: encoding Hydra names as Python names
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
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibStrings from "../lib/strings.js";
import * as Names from "../names.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as PythonEnvironment from "./environment.js";
import * as PythonLanguage from "./language.js";
import * as PythonSerde from "./serde.js";
import * as PythonSyntax from "./syntax.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function encodeConstantForFieldName<t0, t1>(env: t0): ((x: t1) => ((x: Core.Name) => PythonSyntax.Name)) {
  return ((tname: t1) => ((fname: Core.Name) => Formatting.convertCase(({ tag: "camel" }))(({ tag: "upperSnake" }))(((_x) => _x)(fname))));
}

export function encodeConstantForTypeName<t0, t1>(env: t0): ((x: t1) => PythonSyntax.Name) {
  return ((tname: t1) => "TYPE_");
}

export function encodeEnumValue(v1: PythonEnvironment.PythonEnvironment): ((x: Core.Name) => PythonSyntax.Name) {
  return ((v2: Core.Name) => encodeName(false)(({ tag: "upperSnake" }))(v1)(v2));
}

export function encodeFieldName(env: PythonEnvironment.PythonEnvironment): ((x: Core.Name) => PythonSyntax.Name) {
  return ((fname: Core.Name) => encodeName(false)(({ tag: "lowerSnake" }))(env)(fname));
}

export function encodeName(isQualified: boolean): ((x: Util.CaseConvention) => ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => PythonSyntax.Name))) {
  return ((conv: Util.CaseConvention) => ((env: PythonEnvironment.PythonEnvironment) => ((name: Core.Name) => (() => {
  const namespaces = ((_x) => _x.namespaces)(env);
  const focusPair = ((_x) => _x.focus)(namespaces);
  const focusNs = LibPairs.first(focusPair);
  const boundVars = LibPairs.second(((_x) => _x.boundTypeVariables)(env));
  const qualName = Names.qualifyName(name);
  const mns = ((_x) => _x.namespace)(qualName);
  const local = ((_x) => _x.local)(qualName);
  const pyLocal = sanitizePythonName(Formatting.convertCase(({ tag: "camel" }))(conv)(local));
  const pyNs = ((nsVal: Packaging.Namespace) => LibStrings.intercalate(".")(LibLists.map(((v1: string) => Formatting.convertCase(({ tag: "camel" }))(({ tag: "lowerSnake" }))(v1)))(LibStrings.splitOn(".")(((_x) => _x)(nsVal)))));
  return LibLogic.ifElse(isQualified)(LibMaybes.maybe(LibLogic.ifElse(LibEquality.equal(mns)(focusNs))(LibLogic.ifElse(useFutureAnnotations)(pyLocal)(PythonSerde.escapePythonString(true)(pyLocal)))(LibMaybes.maybe(pyLocal)(((nsVal: Packaging.Namespace) => LibStrings.cat2(pyNs(nsVal))(LibStrings.cat2(".")(pyLocal))))(mns)))(((n: PythonSyntax.Name) => n))(LibMaps.lookup(name)(boundVars)))(pyLocal);
})())));
}

export function encodeNameQualified(env: PythonEnvironment.PythonEnvironment): ((x: Core.Name) => PythonSyntax.Name) {
  return ((name: Core.Name) => (() => {
  const namespaces = ((_x) => _x.namespaces)(env);
  const focusPair = ((_x) => _x.focus)(namespaces);
  const focusNs = LibPairs.first(focusPair);
  const boundVars = LibPairs.second(((_x) => _x.boundTypeVariables)(env));
  const qualName = Names.qualifyName(name);
  const mns = ((_x) => _x.namespace)(qualName);
  const local = ((_x) => _x.local)(qualName);
  return LibMaybes.maybe(LibLogic.ifElse(LibEquality.equal(mns)(focusNs))(LibLogic.ifElse(useFutureAnnotations)(local)(PythonSerde.escapePythonString(true)(local)))(LibStrings.intercalate(".")(LibLists.map(sanitizePythonName)(LibStrings.splitOn(".")(((_x) => _x)(name))))))(((n: PythonSyntax.Name) => n))(LibMaps.lookup(name)(boundVars));
})());
}

export function encodeNamespace(nsVal: Packaging.Namespace): PythonSyntax.DottedName {
  return LibLists.map(((part: string) => Formatting.convertCase(({ tag: "camel" }))(({ tag: "lowerSnake" }))(part)))(LibStrings.splitOn(".")(((_x) => _x)(nsVal)));
}

export function encodeTypeVariable(name: Core.Name): PythonSyntax.Name {
  return Formatting.capitalize(((_x) => _x)(name));
}

export function sanitizePythonName(v1: string): string {
  return Formatting.sanitizeWithUnderscores(PythonLanguage.pythonReservedWords)(v1);
}

export function termVariableReference(v1: PythonEnvironment.PythonEnvironment): ((x: Core.Name) => PythonSyntax.Expression) {
  return ((v2: Core.Name) => variableReference(({ tag: "lowerSnake" }))(false)(v1)(v2));
}

export function typeVariableReference(v1: PythonEnvironment.PythonEnvironment): ((x: Core.Name) => PythonSyntax.Expression) {
  return ((v2: Core.Name) => variableReference(({ tag: "pascal" }))(false)(v1)(v2));
}

export const useFutureAnnotations: boolean = true;

export function variableReference(conv: Util.CaseConvention): ((x: boolean) => ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => PythonSyntax.Expression))) {
  return ((quoted: boolean) => ((env: PythonEnvironment.PythonEnvironment) => ((name: Core.Name) => (() => {
  const pyName = encodeName(true)(conv)(env)(name);
  const unquoted = ({ tag: "simple", value: [[({ tag: "simple", value: ({
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
    primary: ({ tag: "simple", value: ({ tag: "name", value: pyName }) })
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
  }) })]] });
  const namespaces = ((_x) => _x.namespaces)(env);
  const focusPair = ((_x) => _x.focus)(namespaces);
  const focusNs = LibPairs.first(focusPair);
  const mns = Names.namespaceOf(name);
  const sameNamespace = LibMaybes.maybe(false)(((ns: Packaging.Namespace) => LibEquality.equal(ns)(focusNs)))(mns);
  return LibLogic.ifElse(LibLogic.and(quoted)(sameNamespace))(({ tag: "simple", value: [[({ tag: "simple", value: ({
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
    primary: ({ tag: "simple", value: ({ tag: "string", value: ({
    value: ((_x) => _x)(pyName),
    quoteStyle: ({ tag: "double" })
  }) }) })
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
  }) })]] }))(unquoted);
})())));
}

export function variantName(isQualified: boolean): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => ((x: Core.Name) => PythonSyntax.Name))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((tname: Core.Name) => ((fname: Core.Name) => encodeName(isQualified)(({ tag: "pascal" }))(env)(LibStrings.cat2(((_x) => _x)(tname))(Formatting.capitalize(((_x) => _x)(fname)))))));
}
