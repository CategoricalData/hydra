// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.phantoms
 */



import * as Core from "../core.js";
import * as EncodeCore from "./core.js";
import * as Phantoms from "../phantoms.js";

export function tBinding<t0, t1>(a: t0): ((x: Phantoms.TBinding<t1>) => Core.Term) {
  return ((x: Phantoms.TBinding<t1>) => ({ tag: "record", value: ({
    typeName: "hydra.phantoms.TBinding",
    fields: [({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  }), ({
    name: "term",
    term: tTerm(a)(((_x) => _x.term)(x))
  })]
  }) }));
}

export function tTerm<t0, t1>(a: t0): ((x: Phantoms.TTerm<t1>) => Core.Term) {
  return ((x: Phantoms.TTerm<t1>) => ({ tag: "wrap", value: ({
    typeName: "hydra.phantoms.TTerm",
    body: EncodeCore.term(((_x) => _x)(x))
  }) }));
}

export function tTermDefinition<t0, t1>(a: t0): ((x: Phantoms.TTermDefinition<t1>) => Core.Term) {
  return ((x: Phantoms.TTermDefinition<t1>) => ({ tag: "record", value: ({
    typeName: "hydra.phantoms.TTermDefinition",
    fields: [({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  }), ({
    name: "term",
    term: tTerm(a)(((_x) => _x.term)(x))
  })]
  }) }));
}
