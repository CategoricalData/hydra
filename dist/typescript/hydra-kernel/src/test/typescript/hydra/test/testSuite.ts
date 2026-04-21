// Note: this is an automatically generated file. Do not edit.

/**
 * Hydra's common test suite, which is designed to run identically in each Hydra implementation; the criterion for a true Hydra implementation is that all test cases pass.
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
import * as JsonModel from "../json/model.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Tabular from "../tabular.js";
import * as TestAnnotations from "./annotations.js";
import * as TestCheckingAll from "./checking/all.js";
import * as TestDependencies from "./dependencies.js";
import * as TestDifferentiation from "./differentiation.js";
import * as TestEtaExpansion from "./etaExpansion.js";
import * as TestFormatting from "./formatting.js";
import * as TestHoistingAll from "./hoisting/all.js";
import * as TestInferenceAll from "./inference/all.js";
import * as TestJsonRoundtrip from "./json/roundtrip.js";
import * as TestJsonWriter from "./json/writer.js";
import * as TestLibChars from "./lib/chars.js";
import * as TestLibEithers from "./lib/eithers.js";
import * as TestLibEquality from "./lib/equality.js";
import * as TestLibLists from "./lib/lists.js";
import * as TestLibLiterals from "./lib/literals.js";
import * as TestLibLogic from "./lib/logic.js";
import * as TestLibMaps from "./lib/maps.js";
import * as TestLibMath from "./lib/math.js";
import * as TestLibMaybes from "./lib/maybes.js";
import * as TestLibPairs from "./lib/pairs.js";
import * as TestLibRegex from "./lib/regex.js";
import * as TestLibSets from "./lib/sets.js";
import * as TestLibStrings from "./lib/strings.js";
import * as TestReduction from "./reduction.js";
import * as TestRewriting from "./rewriting.js";
import * as TestSerialization from "./serialization.js";
import * as TestSorting from "./sorting.js";
import * as TestStrip from "./strip.js";
import * as TestSubstitution from "./substitution.js";
import * as TestUnification from "./unification.js";
import * as TestValidateAll from "./validate/all.js";
import * as TestVariables from "./variables.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const allTests: Testing.TestGroup = ({
    name: "common",
    description: null,
    subgroups: [TestLibChars.allTests, TestLibEithers.allTests, TestLibEquality.allTests, TestLibLists.allTests, TestLibLiterals.allTests, TestLibLogic.allTests, TestLibMaps.allTests, TestLibMath.allTests, TestLibMaybes.allTests, TestLibPairs.allTests, TestLibRegex.allTests, TestLibSets.allTests, TestLibStrings.allTests, TestAnnotations.allTests, TestCheckingAll.allTests, TestDependencies.allTests, TestDifferentiation.allTests, TestEtaExpansion.allTests, TestFormatting.allTests, TestHoistingAll.allTests, TestInferenceAll.allTests, TestJsonRoundtrip.allTests, TestJsonWriter.allTests, TestReduction.allTests, TestRewriting.allTests, TestSerialization.allTests, TestSorting.allTests, TestStrip.allTests, TestSubstitution.allTests, TestUnification.allTests, TestValidateAll.allTests, TestVariables.allTests],
    cases: []
  });
