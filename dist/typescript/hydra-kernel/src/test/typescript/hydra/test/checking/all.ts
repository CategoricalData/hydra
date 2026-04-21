// Note: this is an automatically generated file. Do not edit.

/**
 * Hydra's type checking test suite
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
import * as TestCheckingAdvanced from "./advanced.js";
import * as TestCheckingAlgebraicTypes from "./algebraicTypes.js";
import * as TestCheckingCollections from "./collections.js";
import * as TestCheckingFailures from "./failures.js";
import * as TestCheckingFundamentals from "./fundamentals.js";
import * as TestCheckingNominalTypes from "./nominalTypes.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "checking",
    description: null,
    subgroups: [TestCheckingAdvanced.allTests, TestCheckingAlgebraicTypes.allTests, TestCheckingCollections.allTests, TestCheckingFailures.allTests, TestCheckingFundamentals.allTests, TestCheckingNominalTypes.allTests],
    cases: []
  });
