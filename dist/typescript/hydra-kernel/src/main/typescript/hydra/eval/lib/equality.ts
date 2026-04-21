// Note: this is an automatically generated file. Do not edit.

/**
 * Evaluation-level implementations of Equality functions for the Hydra interpreter.
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
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export function identity<t0, t1, t2, t3>(cx: t0): ((x: t1) => ((x: t2) => t3 | t2)) {
  return ((g: t1) => ((x: t2) => ({ tag: "right", value: x })));
}

export function max<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => t2 | Core.Term))) {
  return ((g: t1) => ((x: Core.Term) => ((y: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.gte" }),
    argument: x
  }) }),
    argument: y
  }) })
  }) }),
    argument: x
  }) }),
    argument: y
  }) }) }))));
}

export function min<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => t2 | Core.Term))) {
  return ((g: t1) => ((x: Core.Term) => ((y: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.equality.lte" }),
    argument: x
  }) }),
    argument: y
  }) })
  }) }),
    argument: x
  }) }),
    argument: y
  }) }) }))));
}
