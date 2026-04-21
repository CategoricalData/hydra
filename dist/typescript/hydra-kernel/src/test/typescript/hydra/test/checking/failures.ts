// Note: this is an automatically generated file. Do not edit.

/**
 * Type checking failure test cases
 */



import * as Ast from "../../ast.js";
import * as Classes from "../../classes.js";
import * as Coders from "../../coders.js";
import * as Context from "../../context.js";
import * as Core from "../../core.js";
import * as ErrorChecking from "../../error/checking.js";
import * as ErrorCore from "../../error/core.js";
import * as ErrorPackaging from "../../error/packaging.js";
import * as Errors from "../../errors.js";
import * as Graph from "../../graph.js";
import * as JsonModel from "../../json/model.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as Rewriting from "../../rewriting.js";
import * as Tabular from "../../tabular.js";
import * as TestTestGraph from "../testGraph.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "Failures",
    description: null,
    subgroups: [failOnUntypedTests],
    cases: []
  });

export const failOnUntypedTests: Testing.TestGroup = ({
    name: "Fail on untyped (pre-inference) terms",
    description: null,
    subgroups: [untypedLambdasTests],
    cases: []
  });

export const untypedLambdasTests: Testing.TestGroup = ({
    name: "Untyped lambdas",
    description: null,
    subgroups: [],
    cases: []
  });
