// Note: this is an automatically generated file. Do not edit.

/**
 * A Shex model. Based on the BNF at:
 *   https://github.com/shexSpec/grammar/blob/master/bnf
 */



export interface ShexDoc {
  readonly listOfDirective: ReadonlyArray<Directive>;
  readonly sequence: ShexDoc_Sequence_Option | null;
  readonly prefixDecl: PrefixDecl;
}

export interface ShexDoc_Sequence_Option {
  readonly alts: ShexDoc_Sequence_Option_Alts;
  readonly listOfStatement: ReadonlyArray<Statement>;
}

export type ShexDoc_Sequence_Option_Alts =
  | { readonly tag: "notStartAction"; readonly value: NotStartAction }
  | { readonly tag: "startActions"; readonly value: StartActions };

export type Directive =
  | { readonly tag: "baseDecl"; readonly value: BaseDecl }
  | { readonly tag: "prefixDecl"; readonly value: PrefixDecl };

export type BaseDecl = IriRef & { readonly __brand: "BaseDecl" };

export interface PrefixDecl {
  readonly pnameNs: PnameNs;
  readonly iriRef: IriRef;
}

export type NotStartAction =
  | { readonly tag: "start"; readonly value: ShapeExpression }
  | { readonly tag: "shapeExprDecl"; readonly value: NotStartAction_ShapeExprDecl };

export interface NotStartAction_ShapeExprDecl {
  readonly shapeExprLabel: ShapeExprLabel;
  readonly alts: NotStartAction_ShapeExprDecl_Alts;
}

export type NotStartAction_ShapeExprDecl_Alts =
  | { readonly tag: "shapeExpression"; readonly value: ShapeExpression }
  | { readonly tag: "eXTERNAL" };

export type StartActions = ReadonlyArray<CodeDecl> & { readonly __brand: "StartActions" };

export type Statement =
  | { readonly tag: "directive"; readonly value: Directive }
  | { readonly tag: "notStartAction"; readonly value: NotStartAction };

export type ShapeExpression = ShapeOr & { readonly __brand: "ShapeExpression" };

export type InlineShapeExpression = InlineShapeOr & { readonly __brand: "InlineShapeExpression" };

export interface ShapeOr {
  readonly shapeAnd: ShapeAnd;
  readonly listOfSequence: ReadonlyArray<ShapeAnd>;
}

export interface InlineShapeOr {
  readonly shapeAnd: ShapeAnd;
  readonly listOfSequence: ReadonlyArray<InlineShapeAnd>;
}

export interface ShapeAnd {
  readonly shapeNot: ShapeNot;
  readonly listOfSequence: ReadonlyArray<ShapeNot>;
}

export interface InlineShapeAnd {
  readonly inlineShapeNot: InlineShapeNot;
  readonly listOfSequence: ReadonlyArray<InlineShapeNot>;
}

export interface ShapeNot {
  readonly nOT: void | null;
  readonly shapeAtom: ShapeAtom;
}

export interface InlineShapeNot {
  readonly nOT: void | null;
  readonly inlineShapeAtom: InlineShapeAtom;
}

export type ShapeAtom =
  | { readonly tag: "sequence"; readonly value: ShapeAtom_Sequence }
  | { readonly tag: "shapeOrRef"; readonly value: ShapeOrRef }
  | { readonly tag: "sequence2"; readonly value: ShapeExpression }
  | { readonly tag: "period" };

export interface ShapeAtom_Sequence {
  readonly nodeConstraint: NodeConstraint;
  readonly shapeOrRef: ShapeOrRef | null;
}

export type InlineShapeAtom =
  | { readonly tag: "sequence"; readonly value: InlineShapeAtom_Sequence }
  | { readonly tag: "sequence2"; readonly value: InlineShapeAtom_Sequence2 }
  | { readonly tag: "sequence3"; readonly value: ShapeExpression }
  | { readonly tag: "period" };

export interface InlineShapeAtom_Sequence {
  readonly nodeConstraint: NodeConstraint;
  readonly inlineShapeOrRef: InlineShapeOrRef | null;
}

export interface InlineShapeAtom_Sequence2 {
  readonly inlineShapeOrRef: InlineShapeOrRef;
  readonly nodeConstraint: NodeConstraint | null;
}

export type ShapeOrRef =
  | { readonly tag: "shapeDefinition"; readonly value: ShapeDefinition }
  | { readonly tag: "atpNameLn"; readonly value: AtpNameLn }
  | { readonly tag: "atpNameNs"; readonly value: AtpNameNs }
  | { readonly tag: "sequence"; readonly value: ShapeExprLabel };

export type InlineShapeOrRef =
  | { readonly tag: "inlineShapeDefinition"; readonly value: InlineShapeDefinition }
  | { readonly tag: "atpNameLn"; readonly value: AtpNameLn }
  | { readonly tag: "atpNameNs"; readonly value: AtpNameNs }
  | { readonly tag: "sequence"; readonly value: ShapeExprLabel };

export type NodeConstraint =
  | { readonly tag: "sequence"; readonly value: ReadonlyArray<XsFacet> }
  | { readonly tag: "sequence2"; readonly value: NodeConstraint_Sequence2 }
  | { readonly tag: "sequence3"; readonly value: NodeConstraint_Sequence3 }
  | { readonly tag: "sequence4"; readonly value: NodeConstraint_Sequence4 }
  | { readonly tag: "sequence5"; readonly value: NodeConstraint_Sequence5 }
  | { readonly tag: "listOfXsFacet"; readonly value: ReadonlyArray<XsFacet> };

export interface NodeConstraint_Sequence2 {
  readonly nonLiteralKind: NonLiteralKind;
  readonly listOfStringFacet: ReadonlyArray<StringFacet>;
}

export interface NodeConstraint_Sequence3 {
  readonly datatype: Datatype;
  readonly listOfXsFacet: ReadonlyArray<XsFacet>;
}

export interface NodeConstraint_Sequence4 {
  readonly valueSet: ValueSet;
  readonly listOfXsFacet: ReadonlyArray<XsFacet>;
}

export interface NodeConstraint_Sequence5 {
  readonly valueSet: ValueSet;
  readonly listOfXsFacet: ReadonlyArray<XsFacet>;
}

export type NonLiteralKind =
  | { readonly tag: "iRI" }
  | { readonly tag: "bNODE" }
  | { readonly tag: "nONLITERAL" };

export type XsFacet =
  | { readonly tag: "stringFacet"; readonly value: StringFacet }
  | { readonly tag: "numericFacet"; readonly value: NumericFacet };

export type StringFacet =
  | { readonly tag: "sequence"; readonly value: StringFacet_Sequence }
  | { readonly tag: "regexp"; readonly value: Regexp };

export interface StringFacet_Sequence {
  readonly stringLength: StringLength;
  readonly integer: Integer;
}

export type StringLength =
  | { readonly tag: "lENGTH" }
  | { readonly tag: "mINLENGTH" }
  | { readonly tag: "mAXLENGTH" };

export type NumericFacet =
  | { readonly tag: "sequence"; readonly value: NumericFacet_Sequence }
  | { readonly tag: "sequence2"; readonly value: NumericFacet_Sequence2 };

export interface NumericFacet_Sequence {
  readonly numericRange: NumericRange;
  readonly numericLiteral: NumericLiteral;
}

export interface NumericFacet_Sequence2 {
  readonly numericLength: NumericLength;
  readonly integer: Integer;
}

export type NumericRange =
  | { readonly tag: "mININCLUSIVE" }
  | { readonly tag: "mINEXCLUSIVE" }
  | { readonly tag: "mAXINCLUSIVE" }
  | { readonly tag: "mAXEXCLUSIVE" };

export type NumericLength =
  | { readonly tag: "tOTALDIGITS" }
  | { readonly tag: "fRACTIONDIGITS" };

export interface ShapeDefinition {
  readonly listOfAlts: ReadonlyArray<ShapeDefinition_ListOfAlts_Elmt>;
  readonly tripleExpression: TripleExpression | null;
  readonly listOfAnnotation: ReadonlyArray<Annotation>;
  readonly semanticActions: SemanticActions;
}

export type ShapeDefinition_ListOfAlts_Elmt =
  | { readonly tag: "includeSet"; readonly value: IncludeSet }
  | { readonly tag: "extraPropertySet"; readonly value: ExtraPropertySet }
  | { readonly tag: "cLOSED" };

export interface InlineShapeDefinition {
  readonly listOfAlts: ReadonlyArray<InlineShapeDefinition_ListOfAlts_Elmt>;
  readonly tripleExpression: TripleExpression | null;
}

export type InlineShapeDefinition_ListOfAlts_Elmt =
  | { readonly tag: "includeSet"; readonly value: IncludeSet }
  | { readonly tag: "extraPropertySet"; readonly value: ExtraPropertySet }
  | { readonly tag: "cLOSED" };

export type ExtraPropertySet = ReadonlyArray<Predicate> & { readonly __brand: "ExtraPropertySet" };

export type TripleExpression = OneOfTripleExpr & { readonly __brand: "TripleExpression" };

export type OneOfTripleExpr =
  | { readonly tag: "groupTripleExpr"; readonly value: GroupTripleExpr }
  | { readonly tag: "multiElementOneOf"; readonly value: MultiElementOneOf };

export interface MultiElementOneOf {
  readonly groupTripleExpr: GroupTripleExpr;
  readonly listOfSequence: ReadonlyArray<GroupTripleExpr>;
}

export type InnerTripleExpr =
  | { readonly tag: "multiElementGroup"; readonly value: MultiElementGroup }
  | { readonly tag: "multiElementOneOf"; readonly value: MultiElementOneOf };

export type GroupTripleExpr =
  | { readonly tag: "singleElementGroup"; readonly value: SingleElementGroup }
  | { readonly tag: "multiElementGroup"; readonly value: MultiElementGroup };

export interface SingleElementGroup {
  readonly unaryTripleExpr: UnaryTripleExpr;
  readonly semi: void | null;
}

export interface MultiElementGroup {
  readonly unaryTripleExpr: UnaryTripleExpr;
  readonly listOfSequence: ReadonlyArray<UnaryTripleExpr>;
  readonly semi: void | null;
}

export type UnaryTripleExpr =
  | { readonly tag: "sequence"; readonly value: UnaryTripleExpr_Sequence }
  | { readonly tag: "include"; readonly value: Include };

export interface UnaryTripleExpr_Sequence {
  readonly sequence: TripleExprLabel | null;
  readonly alts: UnaryTripleExpr_Sequence_Alts;
}

export type UnaryTripleExpr_Sequence_Alts =
  | { readonly tag: "tripleConstraint"; readonly value: TripleConstraint }
  | { readonly tag: "bracketedTripleExpr"; readonly value: BracketedTripleExpr };

export interface BracketedTripleExpr {
  readonly innerTripleExpr: InnerTripleExpr;
  readonly cardinality: Cardinality | null;
  readonly listOfAnnotation: ReadonlyArray<Annotation>;
  readonly semanticActions: SemanticActions;
}

export interface TripleConstraint {
  readonly senseFlags: SenseFlags | null;
  readonly predicate: Predicate;
  readonly inlineShapeExpression: InlineShapeExpression;
  readonly cardinality: Cardinality | null;
  readonly listOfAnnotation: ReadonlyArray<Annotation>;
  readonly semanticActions: SemanticActions;
}

export type Cardinality =
  | { readonly tag: "ast" }
  | { readonly tag: "plus" }
  | { readonly tag: "quest" }
  | { readonly tag: "repeatRange"; readonly value: RepeatRange };

export type SenseFlags = void & { readonly __brand: "SenseFlags" };

export type ValueSet = ReadonlyArray<ValueSetValue> & { readonly __brand: "ValueSet" };

export type ValueSetValue =
  | { readonly tag: "iriRange"; readonly value: IriRange }
  | { readonly tag: "literal"; readonly value: Literal };

export type IriRange =
  | { readonly tag: "sequence"; readonly value: IriRange_Sequence }
  | { readonly tag: "sequence2"; readonly value: ReadonlyArray<Exclusion> };

export interface IriRange_Sequence {
  readonly iri: Iri;
  readonly sequence: ReadonlyArray<Exclusion> | null;
}

export type Exclusion = Iri & { readonly __brand: "Exclusion" };

export type Include = TripleExprLabel & { readonly __brand: "Include" };

export interface Annotation {
  readonly predicate: Predicate;
  readonly alts: Annotation_Alts;
}

export type Annotation_Alts =
  | { readonly tag: "iri"; readonly value: Iri }
  | { readonly tag: "literal"; readonly value: Literal };

export type SemanticActions = ReadonlyArray<CodeDecl> & { readonly __brand: "SemanticActions" };

export interface CodeDecl {
  readonly iri: Iri;
  readonly alts: CodeDecl_Alts;
}

export type CodeDecl_Alts =
  | { readonly tag: "code"; readonly value: Code }
  | { readonly tag: "percnt" };

export type Literal =
  | { readonly tag: "rdfLiteral"; readonly value: RdfLiteral }
  | { readonly tag: "numericLiteral"; readonly value: NumericLiteral }
  | { readonly tag: "booleanLiteral"; readonly value: BooleanLiteral };

export type Predicate =
  | { readonly tag: "iri"; readonly value: Iri }
  | { readonly tag: "rdfType"; readonly value: RdfType };

export type Datatype = Iri & { readonly __brand: "Datatype" };

export type ShapeExprLabel =
  | { readonly tag: "iri"; readonly value: Iri }
  | { readonly tag: "blankNode"; readonly value: BlankNode };

export type TripleExprLabel =
  | { readonly tag: "iri"; readonly value: Iri }
  | { readonly tag: "blankNode"; readonly value: BlankNode };

export type NumericLiteral =
  | { readonly tag: "integer"; readonly value: Integer }
  | { readonly tag: "decimal"; readonly value: Decimal }
  | { readonly tag: "double"; readonly value: Double };

export interface RdfLiteral {
  readonly string: String;
  readonly alts: RdfLiteral_Alts_Option | null;
}

export type RdfLiteral_Alts_Option =
  | { readonly tag: "langTag"; readonly value: LangTag }
  | { readonly tag: "sequence"; readonly value: Datatype };

export type BooleanLiteral =
  | { readonly tag: "true" }
  | { readonly tag: "false" };

export type String =
  | { readonly tag: "stringLiteral1"; readonly value: StringLiteral1 }
  | { readonly tag: "stringLiteralLong1"; readonly value: StringLiteralLong1 }
  | { readonly tag: "stringLiteral2"; readonly value: StringLiteral2 }
  | { readonly tag: "stringLiteralLong2"; readonly value: StringLiteralLong2 };

export type Iri =
  | { readonly tag: "iriRef"; readonly value: IriRef }
  | { readonly tag: "prefixedName"; readonly value: PrefixedName };

export type PrefixedName =
  | { readonly tag: "pnameLn"; readonly value: PnameLn }
  | { readonly tag: "pnameNs"; readonly value: PnameNs };

export type BlankNode = BlankNodeLabel & { readonly __brand: "BlankNode" };

export type IncludeSet = ReadonlyArray<ShapeExprLabel> & { readonly __brand: "IncludeSet" };

export type Code = ReadonlyArray<Code_Elmt> & { readonly __brand: "Code" };

export type Code_Elmt =
  | { readonly tag: "regex"; readonly value: string }
  | { readonly tag: "sequence"; readonly value: string }
  | { readonly tag: "uchar"; readonly value: Uchar };

export interface RepeatRange {
  readonly integer: Integer;
  readonly sequence: RepeatRange_Sequence_Option_Option_Option | null | null | null;
}

export type RepeatRange_Sequence_Option_Option_Option =
  | { readonly tag: "integer"; readonly value: Integer }
  | { readonly tag: "ast" };

export type RdfType = void & { readonly __brand: "RdfType" };

export type IriRef = ReadonlyArray<IriRef_Elmt> & { readonly __brand: "IriRef" };

export type IriRef_Elmt =
  | { readonly tag: "regex"; readonly value: string }
  | { readonly tag: "uchar"; readonly value: Uchar };

export type PnameNs = PnPrefix | null & { readonly __brand: "PnameNs" };

export interface PnameLn {
  readonly pnameNs: PnameNs;
  readonly pnLocal: PnLocal;
}

export type AtpNameNs = PnPrefix | null & { readonly __brand: "AtpNameNs" };

export interface AtpNameLn {
  readonly pnameNs: PnameNs;
  readonly pnLocal: PnLocal;
}

export interface Regexp {
  readonly listOfAlts: ReadonlyArray<Regexp_ListOfAlts_Elmt>;
  readonly listOfRegex: ReadonlyArray<string>;
}

export type Regexp_ListOfAlts_Elmt =
  | { readonly tag: "regex"; readonly value: string }
  | { readonly tag: "sequence"; readonly value: string }
  | { readonly tag: "uchar"; readonly value: Uchar };

export interface BlankNodeLabel {
  readonly alts: BlankNodeLabel_Alts;
  readonly listOfAlts: ReadonlyArray<BlankNodeLabel_ListOfAlts_Option_Elmt> | null;
  readonly pnChars: PnChars;
}

export type BlankNodeLabel_Alts =
  | { readonly tag: "pnCharsU"; readonly value: PnCharsU }
  | { readonly tag: "regex"; readonly value: string };

export type BlankNodeLabel_ListOfAlts_Option_Elmt =
  | { readonly tag: "pnChars"; readonly value: PnChars }
  | { readonly tag: "period" };

export type LangTag = string & { readonly __brand: "LangTag" };

export type Integer = string & { readonly __brand: "Integer" };

export type Decimal = string & { readonly __brand: "Decimal" };

export type Double = string & { readonly __brand: "Double" };

export type StringLiteral1 = ReadonlyArray<StringLiteral1_Elmt> & { readonly __brand: "StringLiteral1" };

export type StringLiteral1_Elmt =
  | { readonly tag: "regex"; readonly value: string }
  | { readonly tag: "echar"; readonly value: Echar }
  | { readonly tag: "uchar"; readonly value: Uchar };

export type StringLiteral2 = ReadonlyArray<StringLiteral2_Elmt> & { readonly __brand: "StringLiteral2" };

export type StringLiteral2_Elmt =
  | { readonly tag: "regex"; readonly value: string }
  | { readonly tag: "echar"; readonly value: Echar }
  | { readonly tag: "uchar"; readonly value: Uchar };

export type StringLiteralLong1 = ReadonlyArray<StringLiteralLong1_Elmt> & { readonly __brand: "StringLiteralLong1" };

export type StringLiteralLong1_Elmt =
  | { readonly tag: "sequence"; readonly value: StringLiteralLong1_Elmt_Sequence }
  | { readonly tag: "echar"; readonly value: Echar }
  | { readonly tag: "uchar"; readonly value: Uchar };

export interface StringLiteralLong1_Elmt_Sequence {
  readonly alts: StringLiteralLong1_Elmt_Sequence_Alts_Option | null;
  readonly regex: string;
}

export type StringLiteralLong1_Elmt_Sequence_Alts_Option =
  | { readonly tag: "apos" }
  | { readonly tag: "sequence"; readonly value: StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence };

export interface StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence {

}

export type StringLiteralLong2 = ReadonlyArray<StringLiteralLong2_Elmt> & { readonly __brand: "StringLiteralLong2" };

export type StringLiteralLong2_Elmt =
  | { readonly tag: "sequence"; readonly value: StringLiteralLong2_Elmt_Sequence }
  | { readonly tag: "echar"; readonly value: Echar }
  | { readonly tag: "uchar"; readonly value: Uchar };

export interface StringLiteralLong2_Elmt_Sequence {
  readonly alts: StringLiteralLong2_Elmt_Sequence_Alts_Option | null;
  readonly regex: string;
}

export type StringLiteralLong2_Elmt_Sequence_Alts_Option =
  | { readonly tag: "quot" }
  | { readonly tag: "sequence"; readonly value: StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence };

export interface StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence {

}

export type Uchar =
  | { readonly tag: "sequence"; readonly value: Uchar_Sequence }
  | { readonly tag: "sequence2"; readonly value: Uchar_Sequence2 };

export interface Uchar_Sequence {
  readonly hex: Hex;
  readonly hex2: Hex;
  readonly hex3: Hex;
  readonly hex4: Hex;
}

export interface Uchar_Sequence2 {
  readonly hex: Hex;
  readonly hex2: Hex;
  readonly hex3: Hex;
  readonly hex4: Hex;
  readonly hex5: Hex;
  readonly hex6: Hex;
  readonly hex7: Hex;
  readonly hex8: Hex;
}

export type Echar = string & { readonly __brand: "Echar" };

export type PnCharsBase =
  | { readonly tag: "regex"; readonly value: string }
  | { readonly tag: "regex2"; readonly value: string };

export type PnCharsU =
  | { readonly tag: "pnCharsBase"; readonly value: PnCharsBase }
  | { readonly tag: "lowbar" };

export type PnChars =
  | { readonly tag: "pnCharsU"; readonly value: PnCharsU }
  | { readonly tag: "minus" }
  | { readonly tag: "regex"; readonly value: string };

export interface PnPrefix {
  readonly pnCharsBase: PnCharsBase;
  readonly sequence: PnPrefix_Sequence_Option | null;
}

export interface PnPrefix_Sequence_Option {
  readonly alts: PnPrefix_Sequence_Option_Alts;
  readonly pnChars: PnChars;
}

export type PnPrefix_Sequence_Option_Alts =
  | { readonly tag: "pnChars"; readonly value: PnChars }
  | { readonly tag: "period" };

export interface PnLocal {
  readonly alts: PnLocal_Alts;
  readonly sequence: PnLocal_Sequence_Option | null;
}

export type PnLocal_Alts =
  | { readonly tag: "pnCharsU"; readonly value: PnCharsU }
  | { readonly tag: "colon" }
  | { readonly tag: "regex"; readonly value: string }
  | { readonly tag: "plx"; readonly value: Plx };

export interface PnLocal_Sequence_Option {
  readonly listOfAlts: ReadonlyArray<PnLocal_Sequence_Option_ListOfAlts_Elmt>;
  readonly alts: PnLocal_Sequence_Option_Alts;
}

export type PnLocal_Sequence_Option_ListOfAlts_Elmt =
  | { readonly tag: "pnChars"; readonly value: PnChars }
  | { readonly tag: "period" }
  | { readonly tag: "colon" }
  | { readonly tag: "plx"; readonly value: Plx };

export type PnLocal_Sequence_Option_Alts =
  | { readonly tag: "pnChars"; readonly value: PnChars }
  | { readonly tag: "colon" }
  | { readonly tag: "plx"; readonly value: Plx };

export type Plx =
  | { readonly tag: "percent"; readonly value: Percent }
  | { readonly tag: "pnLocalEsc"; readonly value: PnLocalEsc };

export interface Percent {
  readonly hex: Hex;
  readonly hex2: Hex;
}

export type Hex = string & { readonly __brand: "Hex" };

export type PnLocalEsc = string & { readonly __brand: "PnLocalEsc" };
