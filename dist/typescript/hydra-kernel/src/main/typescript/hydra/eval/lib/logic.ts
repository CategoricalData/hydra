// Note: this is an automatically generated file. Do not edit.

/**
 * Evaluation-level implementations of Logic functions for the Hydra interpreter.
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

export function and<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => t2 | Core.Term))) {
  return ((g: t1) => ((a: Core.Term) => ((b: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: a
  }) }),
    argument: b
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }) }))));
}

export function not<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => t2 | Core.Term)) {
  return ((g: t1) => ((a: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: a
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: false }) })
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }) })));
}

export function or<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => t2 | Core.Term))) {
  return ((g: t1) => ((a: Core.Term) => ((b: Core.Term) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: a
  }) }),
    argument: ({ tag: "literal", value: ({ tag: "boolean", value: true }) })
  }) }),
    argument: b
  }) }) }))));
}
