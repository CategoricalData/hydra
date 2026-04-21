// Note: this is an automatically generated file. Do not edit.

/**
 * Evaluation-level implementations of Set functions for the Hydra interpreter.
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
import * as ExtractCore from "../../extract/core.js";
import * as Graph from "../../graph.js";
import * as JsonModel from "../../json/model.js";
import * as LibEithers from "../../lib/eithers.js";
import * as LibLists from "../../lib/lists.js";
import * as LibSets from "../../lib/sets.js";
import * as Packaging from "../../packaging.js";
import * as Parsing from "../../parsing.js";
import * as Paths from "../../paths.js";
import * as Phantoms from "../../phantoms.js";
import * as Query from "../../query.js";
import * as Relational from "../../relational.js";
import * as ShowCore from "../../show/core.js";
import * as Tabular from "../../tabular.js";
import * as Testing from "../../testing.js";
import * as Topology from "../../topology.js";
import * as Typing from "../../typing.js";
import * as Util from "../../util.js";
import * as Variants from "../../variants.js";

export function difference<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((set1Term: Core.Term) => ((set2Term: Core.Term) => LibEithers.bind(ExtractCore.set(g)(set1Term))(((elements: ReadonlySet<Core.Term>) => ({ tag: "right", value: LibLists.foldl(((acc: Core.Term) => ((el: Core.Term) => ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.member" }),
    argument: el
  }) }),
    argument: set2Term
  }) })
  }) }),
    argument: acc
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.insert" }),
    argument: el
  }) }),
    argument: acc
  }) })
  }) }))))(({ tag: "set", value: LibSets.fromList([]) }))(LibSets.toList(elements)) }))))));
}

export function intersection<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((set1Term: Core.Term) => ((set2Term: Core.Term) => LibEithers.bind(ExtractCore.set(g)(set1Term))(((elements: ReadonlySet<Core.Term>) => ({ tag: "right", value: LibLists.foldl(((acc: Core.Term) => ((el: Core.Term) => ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.logic.ifElse" }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.member" }),
    argument: el
  }) }),
    argument: set2Term
  }) })
  }) }),
    argument: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.insert" }),
    argument: el
  }) }),
    argument: acc
  }) })
  }) }),
    argument: acc
  }) }))))(({ tag: "set", value: LibSets.fromList([]) }))(LibSets.toList(elements)) }))))));
}

export function map<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((fun: Core.Term) => ((setTerm: Core.Term) => LibEithers.bind(ExtractCore.set(g)(setTerm))(((elements: ReadonlySet<Core.Term>) => ({ tag: "right", value: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.fromList" }),
    argument: ({ tag: "list", value: LibLists.map(((el: Core.Term) => ({ tag: "application", value: ({
    function: fun,
    argument: el
  }) })))(LibSets.toList(elements)) })
  }) }) }))))));
}

export function union<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Term) => Errors.Error | Core.Term))) {
  return ((g: Graph.Graph) => ((set1Term: Core.Term) => ((set2Term: Core.Term) => LibEithers.bind(ExtractCore.set(g)(set1Term))(((elements: ReadonlySet<Core.Term>) => ({ tag: "right", value: LibLists.foldl(((acc: Core.Term) => ((el: Core.Term) => ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.insert" }),
    argument: el
  }) }),
    argument: acc
  }) }))))(set2Term)(LibSets.toList(elements)) }))))));
}

export function unions<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Term)) {
  return ((g: Graph.Graph) => ((listTerm: Core.Term) => LibEithers.bind(ExtractCore.list(g)(listTerm))(((elements: ReadonlyArray<Core.Term>) => ({ tag: "right", value: LibLists.foldl(((acc: Core.Term) => ((s: Core.Term) => ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.lib.sets.union" }),
    argument: acc
  }) }),
    argument: s
  }) }))))(({ tag: "set", value: LibSets.fromList([]) }))(elements) })))));
}
