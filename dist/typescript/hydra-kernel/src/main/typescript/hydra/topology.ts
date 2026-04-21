// Note: this is an automatically generated file. Do not edit.

/**
 * A model for simple graphs as adjacency lists
 */



import * as Core from "./core.js";

export type Graph = ReadonlyMap<Vertex, ReadonlyArray<Vertex>>;

export interface OrderingIsomorphism<a> {
  readonly encode: ((x: ReadonlyArray<a>) => ReadonlyArray<a>);
  readonly decode: ((x: ReadonlyArray<a>) => ReadonlyArray<a>);
}

export interface TarjanState {
  readonly counter: number;
  readonly indices: ReadonlyMap<Vertex, number>;
  readonly lowLinks: ReadonlyMap<Vertex, number>;
  readonly stack: ReadonlyArray<Vertex>;
  readonly onStack: ReadonlySet<Vertex>;
  readonly sccs: ReadonlyArray<ReadonlyArray<Vertex>>;
}

export type Vertex = number;
