// Note: this is an automatically generated file. Do not edit.

/**
 * String representations of hydra.typing types
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
import * as LibLists from "../lib/lists.js";
import * as LibMaps from "../lib/maps.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ShowCore from "./core.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function typeConstraint(tc: Typing.TypeConstraint): string {
  return (() => {
  const ltyp = ((_x) => _x.left)(tc);
  const rtyp = ((_x) => _x.right)(tc);
  return LibStrings.cat([ShowCore.type(ltyp), "≡", ShowCore.type(rtyp)]);
})();
}

export function typeSubst(ts: Typing.TypeSubst): string {
  return (() => {
  const subst = ((_x) => _x)(ts);
  const pairs = LibMaps.toList(subst);
  const showPair = ((pair: readonly [Core.Name, Core.Type]) => (() => {
  const name = ((_x) => _x)(LibPairs.first(pair));
  const typ = LibPairs.second(pair);
  return LibStrings.cat([name, "↦", ShowCore.type(typ)]);
})());
  const pairStrs = LibLists.map(showPair)(pairs);
  return LibStrings.cat(["{", LibStrings.intercalate(",")(pairStrs), "}"]);
})();
}
