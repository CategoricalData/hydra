// Note: this is an automatically generated file. Do not edit.

/**
 * Shared utility functions for test code generation codecs
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
import * as Inference from "../inference.js";
import * as JsonModel from "../json/model.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ShowErrors from "../show/errors.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function inferTerm(g: Graph.Graph): ((x: Core.Term) => string | Core.Term) {
  return ((term: Core.Term) => LibEithers.bimap(((e: Errors.Error) => ShowErrors.error(e)))(((x: Typing.InferenceResult) => ((_x) => _x.term)(x)))(Inference.inferInGraphContext(Lexical.emptyContext)(g)(term)));
}

export function inferTestCase<t0, t1>(g: t0): ((x: Testing.TestCaseWithMetadata) => t1 | Testing.TestCaseWithMetadata) {
  return ((tcm: Testing.TestCaseWithMetadata) => (() => {
  const name_ = ((_x) => _x.name)(tcm);
  const tcase = ((_x) => _x.case)(tcm);
  const desc = ((_x) => _x.description)(tcm);
  const tags_ = ((_x) => _x.tags)(tcm);
  return LibEithers.map(((inferredCase: Testing.TestCase) => ({
    name: name_,
    case: inferredCase,
    description: desc,
    tags: tags_
  })))(({ tag: "right", value: tcase }));
})());
}

export function inferTestGroupTerms<t0, t1>(g: t0): ((x: Testing.TestGroup) => t1 | Testing.TestGroup) {
  return ((tg: Testing.TestGroup) => (() => {
  const name_ = ((_x) => _x.name)(tg);
  const desc = ((_x) => _x.description)(tg);
  const subgroups = ((_x) => _x.subgroups)(tg);
  const cases_ = ((_x) => _x.cases)(tg);
  return LibEithers.bind(LibEithers.mapList(((sg: Testing.TestGroup) => inferTestGroupTerms(g)(sg)))(subgroups))(((inferredSubgroups: ReadonlyArray<Testing.TestGroup>) => LibEithers.map(((inferredCases: ReadonlyArray<Testing.TestCaseWithMetadata>) => ({
    name: name_,
    description: desc,
    subgroups: inferredSubgroups,
    cases: inferredCases
  })))(LibEithers.mapList(((tc: Testing.TestCaseWithMetadata) => inferTestCase(g)(tc)))(cases_))));
})());
}
