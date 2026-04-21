// Note: this is an automatically generated file. Do not edit.

/**
 * Parser combinator types for text parsing
 */



import * as Core from "./core.js";

export interface ParseError {
  readonly message: string;
  readonly remainder: string;
}

export type ParseResult<a> =
  | { readonly tag: "success"; readonly value: ParseSuccess<a> }
  | { readonly tag: "failure"; readonly value: ParseError };

export interface ParseSuccess<a> {
  readonly value: a;
  readonly remainder: string;
}

export type Parser<a> = ((x: string) => ParseResult<a>) & { readonly __brand: "Parser" };
