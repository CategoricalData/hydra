module Hydra.Sources.Shex.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T


ns :: ModuleName
ns = ModuleName "hydra.shex.syntax"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [],
            moduleMetadata = descriptionMetadata (Just ("A Shex model. Based on the BNF at:\n" ++
        "  https://github.com/shexSpec/grammar/blob/master/bnf"))}
  where
    definitions = [
      shexDoc,
      shexDocSequenceOption,
      shexDocSequenceOptionAlts,
      directive,
      baseDecl,
      prefixDecl,
      notStartAction,
      notStartActionShapeExprDecl,
      notStartActionShapeExprDeclAlts,
      startActions,
      statement,
      shapeExpression,
      inlineShapeExpression,
      shapeOr,
      inlineShapeOr,
      shapeAnd,
      inlineShapeAnd,
      shapeNot,
      inlineShapeNot,
      shapeAtom,
      shapeAtomSequence,
      inlineShapeAtom,
      inlineShapeAtomSequence,
      inlineShapeAtomSequence2,
      shapeOrRef,
      inlineShapeOrRef,
      nodeConstraint,
      nodeConstraintSequence2,
      nodeConstraintSequence3,
      nodeConstraintSequence4,
      nodeConstraintSequence5,
      nonLiteralKind,
      xsFacet,
      stringFacet,
      stringFacetSequence,
      stringLength,
      numericFacet,
      numericFacetSequence,
      numericFacetSequence2,
      numericRange,
      numericLength,
      shapeDefinition,
      shapeDefinitionListOfAltsElmt,
      inlineShapeDefinition,
      inlineShapeDefinitionListOfAltsElmt,
      extraPropertySet,
      tripleExpression,
      oneOfTripleExpr,
      multiElementOneOf,
      innerTripleExpr,
      groupTripleExpr,
      singleElementGroup,
      multiElementGroup,
      unaryTripleExpr,
      unaryTripleExprSequence,
      unaryTripleExprSequenceAlts,
      bracketedTripleExpr,
      tripleConstraint,
      cardinality,
      senseFlags,
      valueSet,
      valueSetValue,
      iriRange,
      iriRangeSequence,
      exclusion,
      include,
      annotation,
      annotationAlts,
      semanticActions,
      codeDecl,
      codeDeclAlts,
      literal,
      predicate,
      datatype_,
      shapeExprLabel,
      tripleExprLabel,
      numericLiteral,
      rdfLiteral,
      rdfLiteralAltsOption,
      booleanLiteral,
      string_,
      iri,
      prefixedName,
      blankNode,
      includeSet,
      code,
      codeElmt,
      repeatRange,
      repeatRangeSequenceOptionOptionOption,
      rdfType,
      iriRef,
      iriRefElmt,
      pnameNs,
      pnameLn,
      atpNameNs,
      atpNameLn,
      regexp,
      regexpListOfAltsElmt,
      blankNodeLabel,
      blankNodeLabelAlts,
      blankNodeLabelListOfAltsOptionElmt,
      langTag,
      integer_,
      decimal,
      double_,
      stringLiteral1,
      stringLiteral1Elmt,
      stringLiteral2,
      stringLiteral2Elmt,
      stringLiteralLong1,
      stringLiteralLong1Elmt,
      stringLiteralLong1ElmtSequence,
      stringLiteralLong1ElmtSequenceAltsOption,
      stringLiteralLong1ElmtSequenceAltsOptionSequence,
      stringLiteralLong2,
      stringLiteralLong2Elmt,
      stringLiteralLong2ElmtSequence,
      stringLiteralLong2ElmtSequenceAltsOption,
      stringLiteralLong2ElmtSequenceAltsOptionSequence,
      uchar,
      ucharSequence,
      ucharSequence2,
      echar,
      pnCharsBase,
      pnCharsU,
      pnChars,
      pnPrefix,
      pnPrefixSequenceOption,
      pnPrefixSequenceOptionAlts,
      pnLocal,
      pnLocalAlts,
      pnLocalSequenceOption,
      pnLocalSequenceOptionListOfAltsElmt,
      pnLocalSequenceOptionAlts,
      plx,
      percent,
      hex,
      pnLocalEsc]

-- [51] Annotation ::= '//' Predicate (Iri | Literal)
annotation :: TypeDefinition
annotation = define "Annotation" $
  T.record [
    "Predicate">: shex "Predicate",
    "alts">: shex "AnnotationAlts"]

annotationAlts :: TypeDefinition
annotationAlts = define "AnnotationAlts" $
  T.union [
    "Iri">: shex "Iri",
    "Literal">: shex "Literal"]

-- [62] AtpNameLn ::= '@' PnameNs PnLocal
atpNameLn :: TypeDefinition
atpNameLn = define "AtpNameLn" $
  T.record [
    "PnameNs">: shex "PnameNs",
    "PnLocal">: shex "PnLocal"]

-- [61] AtpNameNs ::= '@' PnPrefix? ':'
atpNameNs :: TypeDefinition
atpNameNs = define "AtpNameNs" $ T.wrap $ T.optional $ shex "PnPrefix"

-- [3] BaseDecl ::= "BASE" IriRef
baseDecl :: TypeDefinition
baseDecl = define "BaseDecl" $ T.wrap $ shex "IriRef"

-- [138s] BlankNode ::= BlankNodeLabel
blankNode :: TypeDefinition
blankNode = define "BlankNode" $ T.wrap $ shex "BlankNodeLabel"

-- [142s] BlankNodeLabel
blankNodeLabel :: TypeDefinition
blankNodeLabel = define "BlankNodeLabel" $
  T.record [
    "alts">: shex "BlankNodeLabelAlts",
    "ListOfAlts">: T.optional (T.list (shex "BlankNodeLabelListOfAltsOptionElmt")),
    "PnChars">: shex "PnChars"]

blankNodeLabelAlts :: TypeDefinition
blankNodeLabelAlts = define "BlankNodeLabelAlts" $
  T.union [
    "PnCharsU">: shex "PnCharsU",
    "regex">: T.string]

blankNodeLabelListOfAltsOptionElmt :: TypeDefinition
blankNodeLabelListOfAltsOptionElmt = define "BlankNodeLabelListOfAltsOptionElmt" $
  T.union [
    "PnChars">: shex "PnChars",
    "Period">: T.unit]

-- [134s] BooleanLiteral ::= 'true' | 'false'
booleanLiteral :: TypeDefinition
booleanLiteral = define "BooleanLiteral" $
  T.union [
    "True">: T.unit,
    "False">: T.unit]

-- [41] BracketedTripleExpr
bracketedTripleExpr :: TypeDefinition
bracketedTripleExpr = define "BracketedTripleExpr" $
  T.record [
    "InnerTripleExpr">: shex "InnerTripleExpr",
    "Cardinality">: T.optional (shex "Cardinality"),
    "listOfAnnotation">: T.list (shex "Annotation"),
    "SemanticActions">: shex "SemanticActions"]

-- [44] Cardinality ::= '*' | '+' | '?' | RepeatRange
cardinality :: TypeDefinition
cardinality = define "Cardinality" $
  T.union [
    "Ast">: T.unit,
    "Plus">: T.unit,
    "Quest">: T.unit,
    "RepeatRange">: shex "RepeatRange"]

-- [58] Code
code :: TypeDefinition
code = define "Code" $ T.wrap $ T.list $ shex "CodeElmt"

-- [53] CodeDecl ::= '%' Iri (Code | "%")
codeDecl :: TypeDefinition
codeDecl = define "CodeDecl" $
  T.record [
    "Iri">: shex "Iri",
    "alts">: shex "CodeDeclAlts"]

codeDeclAlts :: TypeDefinition
codeDeclAlts = define "CodeDeclAlts" $
  T.union [
    "Code">: shex "Code",
    "Percnt">: T.unit]

codeElmt :: TypeDefinition
codeElmt = define "CodeElmt" $
  T.union [
    "regex">: T.string,
    "sequence">: T.string,
    "Uchar">: shex "Uchar"]

-- [55] Datatype ::= Iri
datatype_ :: TypeDefinition
datatype_ = define "Datatype" $ T.wrap $ shex "Iri"

-- [20t] Decimal
decimal :: TypeDefinition
decimal = define "Decimal" $ T.wrap T.string

-- [2] Directive ::= BaseDecl | PrefixDecl
directive :: TypeDefinition
directive = define "Directive" $
  T.union [
    "BaseDecl">: shex "BaseDecl",
    "PrefixDecl">: shex "PrefixDecl"]

-- [21t] Double
double_ :: TypeDefinition
double_ = define "Double" $ T.wrap T.string

-- [160s] Echar ::= '\\' [tbnrf\\\"\']
echar :: TypeDefinition
echar = define "Echar" $ T.wrap T.string

-- [49] Exclusion ::= '-' Iri '~'?
exclusion :: TypeDefinition
exclusion = define "Exclusion" $ T.wrap $ shex "Iri"

-- [32] ExtraPropertySet ::= "EXTRA" Predicate+
extraPropertySet :: TypeDefinition
extraPropertySet = define "ExtraPropertySet" $ T.wrap $ T.list $ shex "Predicate"

-- [37] GroupTripleExpr ::= SingleElementGroup | MultiElementGroup
groupTripleExpr :: TypeDefinition
groupTripleExpr = define "GroupTripleExpr" $
  T.union [
    "SingleElementGroup">: shex "SingleElementGroup",
    "MultiElementGroup">: shex "MultiElementGroup"]

-- [172s] Hex ::= [0-9] | [A-F] | [a-f]
hex :: TypeDefinition
hex = define "Hex" $ T.wrap T.string

-- [50] Include ::= '&' TripleExprLabel
include :: TypeDefinition
include = define "Include" $ T.wrap $ shex "TripleExprLabel"

-- [57] IncludeSet ::= '&' ShapeExprLabel+
includeSet :: TypeDefinition
includeSet = define "IncludeSet" $ T.wrap $ T.list $ shex "ShapeExprLabel"

-- [15] InlineShapeAnd ::= InlineShapeNot ("AND" InlineShapeNot)*
inlineShapeAnd :: TypeDefinition
inlineShapeAnd = define "InlineShapeAnd" $
  T.record [
    "InlineShapeNot">: shex "InlineShapeNot",
    "listOfSequence">: T.list (shex "InlineShapeNot")]

-- [19] InlineShapeAtom
inlineShapeAtom :: TypeDefinition
inlineShapeAtom = define "InlineShapeAtom" $
  T.union [
    "seq">: shex "InlineShapeAtomSequence",
    "seq2">: shex "InlineShapeAtomSequence2",
    "sequence3">: shex "ShapeExpression",
    "Period">: T.unit]

inlineShapeAtomSequence :: TypeDefinition
inlineShapeAtomSequence = define "InlineShapeAtomSequence" $
  T.record [
    "NodeConstraint">: shex "NodeConstraint",
    "InlineShapeOrRef">: T.optional (shex "InlineShapeOrRef")]

inlineShapeAtomSequence2 :: TypeDefinition
inlineShapeAtomSequence2 = define "InlineShapeAtomSequence2" $
  T.record [
    "InlineShapeOrRef">: shex "InlineShapeOrRef",
    "NodeConstraint">: T.optional (shex "NodeConstraint")]

-- [31] InlineShapeDefinition
inlineShapeDefinition :: TypeDefinition
inlineShapeDefinition = define "InlineShapeDefinition" $
  T.record [
    "listOfAlts">: T.list (shex "InlineShapeDefinitionListOfAltsElmt"),
    "TripleExpression">: T.optional (shex "TripleExpression")]

inlineShapeDefinitionListOfAltsElmt :: TypeDefinition
inlineShapeDefinitionListOfAltsElmt = define "InlineShapeDefinitionListOfAltsElmt" $
  T.union [
    "IncludeSet">: shex "IncludeSet",
    "ExtraPropertySet">: shex "ExtraPropertySet",
    "CLOSED">: T.unit]

-- [11] InlineShapeExpression ::= InlineShapeOr
inlineShapeExpression :: TypeDefinition
inlineShapeExpression = define "InlineShapeExpression" $ T.wrap $ shex "InlineShapeOr"

-- [17] InlineShapeNot ::= "NOT"? InlineShapeAtom
inlineShapeNot :: TypeDefinition
inlineShapeNot = define "InlineShapeNot" $
  T.record [
    "NOT">: T.optional T.unit,
    "InlineShapeAtom">: shex "InlineShapeAtom"]

-- [13] InlineShapeOr ::= InlineShapeAnd ("OR" InlineShapeAnd)*
inlineShapeOr :: TypeDefinition
inlineShapeOr = define "InlineShapeOr" $
  T.record [
    "ShapeAnd">: shex "ShapeAnd",
    "listOfSequence">: T.list (shex "InlineShapeAnd")]

-- [21] InlineShapeOrRef
inlineShapeOrRef :: TypeDefinition
inlineShapeOrRef = define "InlineShapeOrRef" $
  T.union [
    "InlineShapeDefinition">: shex "InlineShapeDefinition",
    "AtpNameLn">: shex "AtpNameLn",
    "AtpNameNs">: shex "AtpNameNs",
    "sequence">: shex "ShapeExprLabel"]

-- [36] InnerTripleExpr ::= MultiElementGroup | MultiElementOneOf
innerTripleExpr :: TypeDefinition
innerTripleExpr = define "InnerTripleExpr" $
  T.union [
    "MultiElementGroup">: shex "MultiElementGroup",
    "MultiElementOneOf">: shex "MultiElementOneOf"]

-- [19t] Integer
integer_ :: TypeDefinition
integer_ = define "Integer" $ T.wrap T.string

-- [136s] Iri ::= IriRef | PrefixedName
iri :: TypeDefinition
iri = define "Iri" $
  T.union [
    "IriRef">: shex "IriRef",
    "PrefixedName">: shex "PrefixedName"]

-- [48] IriRange
iriRange :: TypeDefinition
iriRange = define "IriRange" $
  T.union [
    "seq">: shex "IriRangeSequence",
    "sequence2">: T.list (shex "Exclusion")]

iriRangeSequence :: TypeDefinition
iriRangeSequence = define "IriRangeSequence" $
  T.record [
    "Iri">: shex "Iri",
    "Sequence">: T.optional (T.list (shex "Exclusion"))]

-- [18t] IriRef
iriRef :: TypeDefinition
iriRef = define "IriRef" $ T.wrap $ T.list $ shex "IriRefElmt"

iriRefElmt :: TypeDefinition
iriRefElmt = define "IriRefElmt" $
  T.union [
    "regex">: T.string,
    "Uchar">: shex "Uchar"]

-- [145s] LangTag
langTag :: TypeDefinition
langTag = define "LangTag" $ T.wrap T.string

-- [13t] Literal ::= RdfLiteral | NumericLiteral | BooleanLiteral
literal :: TypeDefinition
literal = define "Literal" $
  T.union [
    "RdfLiteral">: shex "RdfLiteral",
    "NumericLiteral">: shex "NumericLiteral",
    "BooleanLiteral">: shex "BooleanLiteral"]

-- [39] MultiElementGroup ::= UnaryTripleExpr (';' UnaryTripleExpr)+ ';'?
multiElementGroup :: TypeDefinition
multiElementGroup = define "MultiElementGroup" $
  T.record [
    "UnaryTripleExpr">: shex "UnaryTripleExpr",
    "listOfSequence">: T.list (shex "UnaryTripleExpr"),
    "Semi">: T.optional T.unit]

-- [35] MultiElementOneOf ::= GroupTripleExpr ('|' GroupTripleExpr)+
multiElementOneOf :: TypeDefinition
multiElementOneOf = define "MultiElementOneOf" $
  T.record [
    "GroupTripleExpr">: shex "GroupTripleExpr",
    "listOfSequence">: T.list (shex "GroupTripleExpr")]

-- [22] NodeConstraint
nodeConstraint :: TypeDefinition
nodeConstraint = define "NodeConstraint" $
  T.union [
    "sequence">: T.list (shex "XsFacet"),
    "seq2">: shex "NodeConstraintSequence2",
    "seq3">: shex "NodeConstraintSequence3",
    "seq4">: shex "NodeConstraintSequence4",
    "seq5">: shex "NodeConstraintSequence5",
    "listOfXsFacet">: T.list (shex "XsFacet")]

nodeConstraintSequence2 :: TypeDefinition
nodeConstraintSequence2 = define "NodeConstraintSequence2" $
  T.record [
    "NonLiteralKind">: shex "NonLiteralKind",
    "listOfStringFacet">: T.list (shex "StringFacet")]

nodeConstraintSequence3 :: TypeDefinition
nodeConstraintSequence3 = define "NodeConstraintSequence3" $
  T.record [
    "Datatype">: shex "Datatype",
    "listOfXsFacet">: T.list (shex "XsFacet")]

nodeConstraintSequence4 :: TypeDefinition
nodeConstraintSequence4 = define "NodeConstraintSequence4" $
  T.record [
    "ValueSet">: shex "ValueSet",
    "listOfXsFacet">: T.list (shex "XsFacet")]

nodeConstraintSequence5 :: TypeDefinition
nodeConstraintSequence5 = define "NodeConstraintSequence5" $
  T.record [
    "ValueSet">: shex "ValueSet",
    "listOfXsFacet">: T.list (shex "XsFacet")]

-- [23] NonLiteralKind ::= "IRI" | "BNODE" | "NONLITERAL"
nonLiteralKind :: TypeDefinition
nonLiteralKind = define "NonLiteralKind" $
  T.union [
    "IRI">: T.unit,
    "BNODE">: T.unit,
    "NONLITERAL">: T.unit]

-- [5] NotStartAction ::= start | shapeExprDecl
notStartAction :: TypeDefinition
notStartAction = define "NotStartAction" $
  T.union [
    "start">: shex "ShapeExpression",
    "declaration">: shex "NotStartActionShapeExprDecl"]

notStartActionShapeExprDecl :: TypeDefinition
notStartActionShapeExprDecl = define "NotStartActionShapeExprDecl" $
  T.record [
    "ShapeExprLabel">: shex "ShapeExprLabel",
    "alts">: shex "NotStartActionShapeExprDeclAlts"]

notStartActionShapeExprDeclAlts :: TypeDefinition
notStartActionShapeExprDeclAlts = define "NotStartActionShapeExprDeclAlts" $
  T.union [
    "ShapeExpression">: shex "ShapeExpression",
    "EXTERNAL">: T.unit]

-- [27] NumericFacet
numericFacet :: TypeDefinition
numericFacet = define "NumericFacet" $
  T.union [
    "seq">: shex "NumericFacetSequence",
    "seq2">: shex "NumericFacetSequence2"]

numericFacetSequence :: TypeDefinition
numericFacetSequence = define "NumericFacetSequence" $
  T.record [
    "NumericRange">: shex "NumericRange",
    "NumericLiteral">: shex "NumericLiteral"]

numericFacetSequence2 :: TypeDefinition
numericFacetSequence2 = define "NumericFacetSequence2" $
  T.record [
    "NumericLength">: shex "NumericLength",
    "Integer">: shex "Integer"]

-- [29] NumericLength
numericLength :: TypeDefinition
numericLength = define "NumericLength" $
  T.union [
    "TOTALDIGITS">: T.unit,
    "FRACTIONDIGITS">: T.unit]

-- [16t] NumericLiteral ::= Integer | Decimal | Double
numericLiteral :: TypeDefinition
numericLiteral = define "NumericLiteral" $
  T.union [
    "Integer">: shex "Integer",
    "Decimal">: shex "Decimal",
    "Double">: shex "Double"]

-- [28] NumericRange
numericRange :: TypeDefinition
numericRange = define "NumericRange" $
  T.union [
    "MININCLUSIVE">: T.unit,
    "MINEXCLUSIVE">: T.unit,
    "MAXINCLUSIVE">: T.unit,
    "MAXEXCLUSIVE">: T.unit]

-- [34] OneOfTripleExpr ::= GroupTripleExpr | MultiElementOneOf
oneOfTripleExpr :: TypeDefinition
oneOfTripleExpr = define "OneOfTripleExpr" $
  T.union [
    "GroupTripleExpr">: shex "GroupTripleExpr",
    "MultiElementOneOf">: shex "MultiElementOneOf"]

-- [171s] Percent ::= '%' Hex Hex
percent :: TypeDefinition
percent = define "Percent" $
  T.record [
    "Hex">: shex "Hex",
    "Hex2">: shex "Hex"]

-- [170s] Plx ::= Percent | PnLocalEsc
plx :: TypeDefinition
plx = define "Plx" $
  T.union [
    "Percent">: shex "Percent",
    "PnLocalEsc">: shex "PnLocalEsc"]

-- [167s] PnChars
pnChars :: TypeDefinition
pnChars = define "PnChars" $
  T.union [
    "PnCharsU">: shex "PnCharsU",
    "Minus">: T.unit,
    "regex">: T.string]

-- [164s] PnCharsBase
pnCharsBase :: TypeDefinition
pnCharsBase = define "PnCharsBase" $
  T.union [
    "regex">: T.string,
    "regex2">: T.string]

-- [165s] PnCharsU ::= PnCharsBase | '_'
pnCharsU :: TypeDefinition
pnCharsU = define "PnCharsU" $
  T.union [
    "PnCharsBase">: shex "PnCharsBase",
    "Lowbar">: T.unit]

-- [169s] PnLocal
pnLocal :: TypeDefinition
pnLocal = define "PnLocal" $
  T.record [
    "alts">: shex "PnLocalAlts",
    "Sequence">: T.optional (shex "PnLocalSequenceOption")]

-- [173s] PnLocalEsc
pnLocalEsc :: TypeDefinition
pnLocalEsc = define "PnLocalEsc" $ T.wrap T.string

pnLocalAlts :: TypeDefinition
pnLocalAlts = define "PnLocalAlts" $
  T.union [
    "PnCharsU">: shex "PnCharsU",
    "Colon">: T.unit,
    "regex">: T.string,
    "Plx">: shex "Plx"]

pnLocalSequenceOption :: TypeDefinition
pnLocalSequenceOption = define "PnLocalSequenceOption" $
  T.record [
    "listOfAlts">: T.list (shex "PnLocalSequenceOptionListOfAltsElmt"),
    "alts">: shex "PnLocalSequenceOptionAlts"]

pnLocalSequenceOptionAlts :: TypeDefinition
pnLocalSequenceOptionAlts = define "PnLocalSequenceOptionAlts" $
  T.union [
    "PnChars">: shex "PnChars",
    "Colon">: T.unit,
    "Plx">: shex "Plx"]

pnLocalSequenceOptionListOfAltsElmt :: TypeDefinition
pnLocalSequenceOptionListOfAltsElmt = define "PnLocalSequenceOptionListOfAltsElmt" $
  T.union [
    "PnChars">: shex "PnChars",
    "Period">: T.unit,
    "Colon">: T.unit,
    "Plx">: shex "Plx"]

-- [168s] PnPrefix
pnPrefix :: TypeDefinition
pnPrefix = define "PnPrefix" $
  T.record [
    "PnCharsBase">: shex "PnCharsBase",
    "Sequence">: T.optional (shex "PnPrefixSequenceOption")]

pnPrefixSequenceOption :: TypeDefinition
pnPrefixSequenceOption = define "PnPrefixSequenceOption" $
  T.record [
    "alts">: shex "PnPrefixSequenceOptionAlts",
    "PnChars">: shex "PnChars"]

pnPrefixSequenceOptionAlts :: TypeDefinition
pnPrefixSequenceOptionAlts = define "PnPrefixSequenceOptionAlts" $
  T.union [
    "PnChars">: shex "PnChars",
    "Period">: T.unit]

-- [141s] PnameLn ::= PnameNs PnLocal
pnameLn :: TypeDefinition
pnameLn = define "PnameLn" $
  T.record [
    "PnameNs">: shex "PnameNs",
    "PnLocal">: shex "PnLocal"]

-- [140s] PnameNs ::= PnPrefix? ':'
pnameNs :: TypeDefinition
pnameNs = define "PnameNs" $ T.wrap $ T.optional $ shex "PnPrefix"

-- [54] Predicate ::= Iri | RdfType
predicate :: TypeDefinition
predicate = define "Predicate" $
  T.union [
    "Iri">: shex "Iri",
    "RdfType">: shex "RdfType"]

-- [4] PrefixDecl ::= "PREFIX" PnameNs IriRef
prefixDecl :: TypeDefinition
prefixDecl = define "PrefixDecl" $
  T.record [
    "PnameNs">: shex "PnameNs",
    "IriRef">: shex "IriRef"]

-- [137s] PrefixedName ::= PnameLn | PnameNs
prefixedName :: TypeDefinition
prefixedName = define "PrefixedName" $
  T.union [
    "PnameLn">: shex "PnameLn",
    "PnameNs">: shex "PnameNs"]

-- [129s] RdfLiteral ::= String (LangTag | '^^' Datatype)?
rdfLiteral :: TypeDefinition
rdfLiteral = define "RdfLiteral" $
  T.record [
    "String">: shex "String",
    "Alts">: T.optional (shex "RdfLiteralAltsOption")]

rdfLiteralAltsOption :: TypeDefinition
rdfLiteralAltsOption = define "RdfLiteralAltsOption" $
  T.union [
    "LangTag">: shex "LangTag",
    "sequence">: shex "Datatype"]

-- [60] RdfType ::= 'a'
rdfType :: TypeDefinition
rdfType = define "RdfType" $ T.wrap T.unit

-- [63] Regexp
regexp :: TypeDefinition
regexp = define "Regexp" $
  T.record [
    "listOfAlts">: T.list (shex "RegexpListOfAltsElmt"),
    "listOfRegex">: T.list T.string]

regexpListOfAltsElmt :: TypeDefinition
regexpListOfAltsElmt = define "RegexpListOfAltsElmt" $
  T.union [
    "regex">: T.string,
    "sequence">: T.string,
    "Uchar">: shex "Uchar"]

-- [59] RepeatRange
repeatRange :: TypeDefinition
repeatRange = define "RepeatRange" $
  T.record [
    "Integer">: shex "Integer",
    "Sequence">: T.optional (T.optional (T.optional (shex "RepeatRangeSequenceOptionOptionOption")))]

repeatRangeSequenceOptionOptionOption :: TypeDefinition
repeatRangeSequenceOptionOptionOption = define "RepeatRangeSequenceOptionOptionOption" $
  T.union [
    "Integer">: shex "Integer",
    "Ast">: T.unit]

-- [52] SemanticActions ::= CodeDecl*
semanticActions :: TypeDefinition
semanticActions = define "SemanticActions" $ T.wrap $ T.list $ shex "CodeDecl"

-- [45] SenseFlags ::= '^'
senseFlags :: TypeDefinition
senseFlags = define "SenseFlags" $ T.wrap T.unit

-- [14] ShapeAnd ::= ShapeNot ("AND" ShapeNot)*
shapeAnd :: TypeDefinition
shapeAnd = define "ShapeAnd" $
  T.record [
    "ShapeNot">: shex "ShapeNot",
    "listOfSequence">: T.list (shex "ShapeNot")]

-- [18] ShapeAtom
shapeAtom :: TypeDefinition
shapeAtom = define "ShapeAtom" $
  T.union [
    "seq">: shex "ShapeAtomSequence",
    "ShapeOrRef">: shex "ShapeOrRef",
    "sequence2">: shex "ShapeExpression",
    "Period">: T.unit]

shapeAtomSequence :: TypeDefinition
shapeAtomSequence = define "ShapeAtomSequence" $
  T.record [
    "NodeConstraint">: shex "NodeConstraint",
    "ShapeOrRef">: T.optional (shex "ShapeOrRef")]

-- [30] ShapeDefinition
shapeDefinition :: TypeDefinition
shapeDefinition = define "ShapeDefinition" $
  T.record [
    "listOfAlts">: T.list (shex "ShapeDefinitionListOfAltsElmt"),
    "TripleExpression">: T.optional (shex "TripleExpression"),
    "listOfAnnotation">: T.list (shex "Annotation"),
    "SemanticActions">: shex "SemanticActions"]

shapeDefinitionListOfAltsElmt :: TypeDefinition
shapeDefinitionListOfAltsElmt = define "ShapeDefinitionListOfAltsElmt" $
  T.union [
    "IncludeSet">: shex "IncludeSet",
    "ExtraPropertySet">: shex "ExtraPropertySet",
    "CLOSED">: T.unit]

-- [56] ShapeExprLabel ::= Iri | BlankNode
shapeExprLabel :: TypeDefinition
shapeExprLabel = define "ShapeExprLabel" $
  T.union [
    "Iri">: shex "Iri",
    "BlankNode">: shex "BlankNode"]

-- [10] ShapeExpression ::= ShapeOr
shapeExpression :: TypeDefinition
shapeExpression = define "ShapeExpression" $ T.wrap $ shex "ShapeOr"

-- [16] ShapeNot ::= "NOT"? ShapeAtom
shapeNot :: TypeDefinition
shapeNot = define "ShapeNot" $
  T.record [
    "NOT">: T.optional T.unit,
    "ShapeAtom">: shex "ShapeAtom"]

-- [12] ShapeOr ::= ShapeAnd ("OR" ShapeAnd)*
shapeOr :: TypeDefinition
shapeOr = define "ShapeOr" $
  T.record [
    "ShapeAnd">: shex "ShapeAnd",
    "listOfSequence">: T.list (shex "ShapeAnd")]

-- [20] ShapeOrRef
shapeOrRef :: TypeDefinition
shapeOrRef = define "ShapeOrRef" $
  T.union [
    "ShapeDefinition">: shex "ShapeDefinition",
    "AtpNameLn">: shex "AtpNameLn",
    "AtpNameNs">: shex "AtpNameNs",
    "sequence">: shex "ShapeExprLabel"]

shex :: String -> Type
shex = typeref ns

-- [1] ShexDoc ::= Directive* ((NotStartAction | StartActions) Statement*)?
shexDoc :: TypeDefinition
shexDoc = define "ShexDoc" $
  T.record [
    "listOfDirective">: T.list (shex "Directive"),
    "Sequence">: T.optional (shex "ShexDocSequenceOption"),
    "PrefixDecl">: shex "PrefixDecl"]

shexDocSequenceOption :: TypeDefinition
shexDocSequenceOption = define "ShexDocSequenceOption" $
  T.record [
    "alts">: shex "ShexDocSequenceOptionAlts",
    "listOfStatement">: T.list (shex "Statement")]

shexDocSequenceOptionAlts :: TypeDefinition
shexDocSequenceOptionAlts = define "ShexDocSequenceOptionAlts" $
  T.union [
    "NotStartAction">: shex "NotStartAction",
    "StartActions">: shex "StartActions"]

-- [38] SingleElementGroup ::= UnaryTripleExpr ';'?
singleElementGroup :: TypeDefinition
singleElementGroup = define "SingleElementGroup" $
  T.record [
    "UnaryTripleExpr">: shex "UnaryTripleExpr",
    "Semi">: T.optional T.unit]

-- [7] StartActions ::= CodeDecl+
startActions :: TypeDefinition
startActions = define "StartActions" $ T.wrap $ T.list $ shex "CodeDecl"

-- [8] Statement ::= Directive | NotStartAction
statement :: TypeDefinition
statement = define "Statement" $
  T.union [
    "Directive">: shex "Directive",
    "NotStartAction">: shex "NotStartAction"]

-- [25] StringFacet ::= StringLength Integer | Regexp
stringFacet :: TypeDefinition
stringFacet = define "StringFacet" $
  T.union [
    "seq">: shex "StringFacetSequence",
    "Regexp">: shex "Regexp"]

stringFacetSequence :: TypeDefinition
stringFacetSequence = define "StringFacetSequence" $
  T.record [
    "StringLength">: shex "StringLength",
    "Integer">: shex "Integer"]

-- [26] StringLength ::= "LENGTH" | "MINLENGTH" | "MAXLENGTH"
stringLength :: TypeDefinition
stringLength = define "StringLength" $
  T.union [
    "LENGTH">: T.unit,
    "MINLENGTH">: T.unit,
    "MAXLENGTH">: T.unit]

-- [156s] StringLiteral1
stringLiteral1 :: TypeDefinition
stringLiteral1 = define "StringLiteral1" $ T.wrap $ T.list $ shex "StringLiteral1Elmt"

stringLiteral1Elmt :: TypeDefinition
stringLiteral1Elmt = define "StringLiteral1Elmt" $
  T.union [
    "regex">: T.string,
    "Echar">: shex "Echar",
    "Uchar">: shex "Uchar"]

-- [157s] StringLiteral2
stringLiteral2 :: TypeDefinition
stringLiteral2 = define "StringLiteral2" $ T.wrap $ T.list $ shex "StringLiteral2Elmt"

stringLiteral2Elmt :: TypeDefinition
stringLiteral2Elmt = define "StringLiteral2Elmt" $
  T.union [
    "regex">: T.string,
    "Echar">: shex "Echar",
    "Uchar">: shex "Uchar"]

-- [158s] StringLiteralLong1
stringLiteralLong1 :: TypeDefinition
stringLiteralLong1 = define "StringLiteralLong1" $ T.wrap $ T.list $ shex "StringLiteralLong1Elmt"

stringLiteralLong1Elmt :: TypeDefinition
stringLiteralLong1Elmt = define "StringLiteralLong1Elmt" $
  T.union [
    "seq">: shex "StringLiteralLong1ElmtSequence",
    "Echar">: shex "Echar",
    "Uchar">: shex "Uchar"]

stringLiteralLong1ElmtSequence :: TypeDefinition
stringLiteralLong1ElmtSequence = define "StringLiteralLong1ElmtSequence" $
  T.record [
    "Alts">: T.optional (shex "StringLiteralLong1ElmtSequenceAltsOption"),
    "regex">: T.string]

stringLiteralLong1ElmtSequenceAltsOption :: TypeDefinition
stringLiteralLong1ElmtSequenceAltsOption = define "StringLiteralLong1ElmtSequenceAltsOption" $
  T.union [
    "Apos">: T.unit,
    "seq">: shex "StringLiteralLong1ElmtSequenceAltsOptionSequence"]

stringLiteralLong1ElmtSequenceAltsOptionSequence :: TypeDefinition
stringLiteralLong1ElmtSequenceAltsOptionSequence = define "StringLiteralLong1ElmtSequenceAltsOptionSequence" $
  T.unit

-- [159s] StringLiteralLong2
stringLiteralLong2 :: TypeDefinition
stringLiteralLong2 = define "StringLiteralLong2" $ T.wrap $ T.list $ shex "StringLiteralLong2Elmt"

stringLiteralLong2Elmt :: TypeDefinition
stringLiteralLong2Elmt = define "StringLiteralLong2Elmt" $
  T.union [
    "seq">: shex "StringLiteralLong2ElmtSequence",
    "Echar">: shex "Echar",
    "Uchar">: shex "Uchar"]

stringLiteralLong2ElmtSequence :: TypeDefinition
stringLiteralLong2ElmtSequence = define "StringLiteralLong2ElmtSequence" $
  T.record [
    "Alts">: T.optional (shex "StringLiteralLong2ElmtSequenceAltsOption"),
    "regex">: T.string]

stringLiteralLong2ElmtSequenceAltsOption :: TypeDefinition
stringLiteralLong2ElmtSequenceAltsOption = define "StringLiteralLong2ElmtSequenceAltsOption" $
  T.union [
    "Quot">: T.unit,
    "seq">: shex "StringLiteralLong2ElmtSequenceAltsOptionSequence"]

stringLiteralLong2ElmtSequenceAltsOptionSequence :: TypeDefinition
stringLiteralLong2ElmtSequenceAltsOptionSequence = define "StringLiteralLong2ElmtSequenceAltsOptionSequence" $
  T.unit

-- [135s] String
string_ :: TypeDefinition
string_ = define "String" $
  T.union [
    "StringLiteral1">: shex "StringLiteral1",
    "StringLiteralLong1">: shex "StringLiteralLong1",
    "StringLiteral2">: shex "StringLiteral2",
    "StringLiteralLong2">: shex "StringLiteralLong2"]

-- [43] TripleConstraint
tripleConstraint :: TypeDefinition
tripleConstraint = define "TripleConstraint" $
  T.record [
    "SenseFlags">: T.optional (shex "SenseFlags"),
    "Predicate">: shex "Predicate",
    "InlineShapeExpression">: shex "InlineShapeExpression",
    "Cardinality">: T.optional (shex "Cardinality"),
    "listOfAnnotation">: T.list (shex "Annotation"),
    "SemanticActions">: shex "SemanticActions"]

-- [42] TripleExprLabel ::= Iri | BlankNode
tripleExprLabel :: TypeDefinition
tripleExprLabel = define "TripleExprLabel" $
  T.union [
    "Iri">: shex "Iri",
    "BlankNode">: shex "BlankNode"]

-- [33] TripleExpression ::= OneOfTripleExpr
tripleExpression :: TypeDefinition
tripleExpression = define "TripleExpression" $ T.wrap $ shex "OneOfTripleExpr"

-- [26t] Uchar
uchar :: TypeDefinition
uchar = define "Uchar" $
  T.union [
    "seq">: shex "UcharSequence",
    "seq2">: shex "UcharSequence2"]

ucharSequence :: TypeDefinition
ucharSequence = define "UcharSequence" $
  T.record [
    "Hex">: shex "Hex",
    "Hex2">: shex "Hex",
    "Hex3">: shex "Hex",
    "Hex4">: shex "Hex"]

ucharSequence2 :: TypeDefinition
ucharSequence2 = define "UcharSequence2" $
  T.record [
    "Hex">: shex "Hex",
    "Hex2">: shex "Hex",
    "Hex3">: shex "Hex",
    "Hex4">: shex "Hex",
    "Hex5">: shex "Hex",
    "Hex6">: shex "Hex",
    "Hex7">: shex "Hex",
    "Hex8">: shex "Hex"]

-- [40] UnaryTripleExpr
unaryTripleExpr :: TypeDefinition
unaryTripleExpr = define "UnaryTripleExpr" $
  T.union [
    "seq">: shex "UnaryTripleExprSequence",
    "Include">: shex "Include"]

unaryTripleExprSequence :: TypeDefinition
unaryTripleExprSequence = define "UnaryTripleExprSequence" $
  T.record [
    "Sequence">: T.optional (shex "TripleExprLabel"),
    "alts">: shex "UnaryTripleExprSequenceAlts"]

unaryTripleExprSequenceAlts :: TypeDefinition
unaryTripleExprSequenceAlts = define "UnaryTripleExprSequenceAlts" $
  T.union [
    "TripleConstraint">: shex "TripleConstraint",
    "BracketedTripleExpr">: shex "BracketedTripleExpr"]

-- [46] ValueSet ::= '[' ValueSetValue* ']'
valueSet :: TypeDefinition
valueSet = define "ValueSet" $ T.wrap $ T.list $ shex "ValueSetValue"

-- [47] ValueSetValue ::= IriRange | Literal
valueSetValue :: TypeDefinition
valueSetValue = define "ValueSetValue" $
  T.union [
    "IriRange">: shex "IriRange",
    "Literal">: shex "Literal"]

-- [24] XsFacet ::= StringFacet | NumericFacet
xsFacet :: TypeDefinition
xsFacet = define "XsFacet" $
  T.union [
    "StringFacet">: shex "StringFacet",
    "NumericFacet">: shex "NumericFacet"]
