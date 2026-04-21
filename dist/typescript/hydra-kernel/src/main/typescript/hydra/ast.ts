// Note: this is an automatically generated file. Do not edit.

/**
 * A model which provides a common syntax tree for Hydra serializers
 */



import * as Core from "./core.js";

export type Associativity =
  | { readonly tag: "none" }
  | { readonly tag: "left" }
  | { readonly tag: "right" }
  | { readonly tag: "both" };

export interface BlockStyle {
  readonly indent: string | null;
  readonly newlineBeforeContent: boolean;
  readonly newlineAfterContent: boolean;
}

export interface BracketExpr {
  readonly brackets: Brackets;
  readonly enclosed: Expr;
  readonly style: BlockStyle;
}

export interface Brackets {
  readonly open: Symbol;
  readonly close: Symbol;
}

export type Expr =
  | { readonly tag: "const"; readonly value: Symbol }
  | { readonly tag: "indent"; readonly value: IndentedExpression }
  | { readonly tag: "op"; readonly value: OpExpr }
  | { readonly tag: "brackets"; readonly value: BracketExpr }
  | { readonly tag: "seq"; readonly value: SeqExpr };

export interface IndentedExpression {
  readonly style: IndentStyle;
  readonly expr: Expr;
}

export type IndentStyle =
  | { readonly tag: "allLines"; readonly value: string }
  | { readonly tag: "subsequentLines"; readonly value: string };

export interface Op {
  readonly symbol: Symbol;
  readonly padding: Padding;
  readonly precedence: Precedence;
  readonly associativity: Associativity;
}

export interface OpExpr {
  readonly op: Op;
  readonly lhs: Expr;
  readonly rhs: Expr;
}

export interface Padding {
  readonly left: Ws;
  readonly right: Ws;
}

export type Precedence = number & { readonly __brand: "Precedence" };

export interface SeqExpr {
  readonly op: Op;
  readonly elements: ReadonlyArray<Expr>;
}

export type Symbol = string & { readonly __brand: "Symbol" };

export type Ws =
  | { readonly tag: "none" }
  | { readonly tag: "space" }
  | { readonly tag: "break" }
  | { readonly tag: "breakAndIndent"; readonly value: string }
  | { readonly tag: "doubleBreak" };
