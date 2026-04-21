// Note: this is an automatically generated file. Do not edit.

/**
 * Language constraints for Hydra Core
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as LibSets from "./lib/sets.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Reflect from "./reflect.js";
import * as Relational from "./relational.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export const hydraLanguage: Coders.Language = (() => {
  const eliminationVariants = LibSets.fromList(Reflect.eliminationVariants);
  const literalVariants = LibSets.fromList(Reflect.literalVariants);
  const floatTypes = LibSets.fromList(Reflect.floatTypes);
  const functionVariants = LibSets.fromList(Reflect.functionVariants);
  const integerTypes = LibSets.fromList(Reflect.integerTypes);
  const termVariants = LibSets.fromList(Reflect.termVariants);
  const typeVariants = LibSets.fromList(Reflect.typeVariants);
  const types = ((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    default: return true(_m);
  }
})());
  return ({
    name: "hydra.core",
    constraints: ({
    eliminationVariants: eliminationVariants,
    literalVariants: literalVariants,
    floatTypes: floatTypes,
    functionVariants: functionVariants,
    integerTypes: integerTypes,
    termVariants: termVariants,
    typeVariants: typeVariants,
    types: types
  })
  });
})();
