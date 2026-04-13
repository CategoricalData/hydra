module Hydra.Sources.Shex.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T


ns :: Namespace
ns = Namespace "hydra.shex.syntax"

define :: String -> Type -> Binding
define = defineType ns

shex :: String -> Type
shex = typeref ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [] [] $
    Just ("A Shex model. Based on the BNF at:\n" ++
        "  https://github.com/shexSpec/grammar/blob/master/bnf")
  where
    definitions = [
      shexDoc,
      shexDoc_Sequence_Option,
      shexDoc_Sequence_Option_Alts,
      directive,
      baseDecl,
      prefixDecl,
      notStartAction,
      notStartAction_ShapeExprDecl,
      notStartAction_ShapeExprDecl_Alts,
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
      shapeAtom_Sequence,
      inlineShapeAtom,
      inlineShapeAtom_Sequence,
      inlineShapeAtom_Sequence2,
      shapeOrRef,
      inlineShapeOrRef,
      nodeConstraint,
      nodeConstraint_Sequence2,
      nodeConstraint_Sequence3,
      nodeConstraint_Sequence4,
      nodeConstraint_Sequence5,
      nonLiteralKind,
      xsFacet,
      stringFacet,
      stringFacet_Sequence,
      stringLength,
      numericFacet,
      numericFacet_Sequence,
      numericFacet_Sequence2,
      numericRange,
      numericLength,
      shapeDefinition,
      shapeDefinition_ListOfAlts_Elmt,
      inlineShapeDefinition,
      inlineShapeDefinition_ListOfAlts_Elmt,
      extraPropertySet,
      tripleExpression,
      oneOfTripleExpr,
      multiElementOneOf,
      innerTripleExpr,
      groupTripleExpr,
      singleElementGroup,
      multiElementGroup,
      unaryTripleExpr,
      unaryTripleExpr_Sequence,
      unaryTripleExpr_Sequence_Alts,
      bracketedTripleExpr,
      tripleConstraint,
      cardinality,
      senseFlags,
      valueSet,
      valueSetValue,
      iriRange,
      iriRange_Sequence,
      exclusion,
      include,
      annotation,
      annotation_Alts,
      semanticActions,
      codeDecl,
      codeDecl_Alts,
      literal,
      predicate,
      datatype_,
      shapeExprLabel,
      tripleExprLabel,
      numericLiteral,
      rdfLiteral,
      rdfLiteral_Alts_Option,
      booleanLiteral,
      string_,
      iri,
      prefixedName,
      blankNode,
      includeSet,
      code,
      code_Elmt,
      repeatRange,
      repeatRange_Sequence_Option_Option_Option,
      rdfType,
      iriRef,
      iriRef_Elmt,
      pnameNs,
      pnameLn,
      atpNameNs,
      atpNameLn,
      regexp,
      regexp_ListOfAlts_Elmt,
      blankNodeLabel,
      blankNodeLabel_Alts,
      blankNodeLabel_ListOfAlts_Option_Elmt,
      langTag,
      integer_,
      decimal,
      double_,
      stringLiteral1,
      stringLiteral1_Elmt,
      stringLiteral2,
      stringLiteral2_Elmt,
      stringLiteralLong1,
      stringLiteralLong1_Elmt,
      stringLiteralLong1_Elmt_Sequence,
      stringLiteralLong1_Elmt_Sequence_Alts_Option,
      stringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence,
      stringLiteralLong2,
      stringLiteralLong2_Elmt,
      stringLiteralLong2_Elmt_Sequence,
      stringLiteralLong2_Elmt_Sequence_Alts_Option,
      stringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence,
      uchar,
      uchar_Sequence,
      uchar_Sequence2,
      echar,
      pnCharsBase,
      pnCharsU,
      pnChars,
      pnPrefix,
      pnPrefix_Sequence_Option,
      pnPrefix_Sequence_Option_Alts,
      pnLocal,
      pnLocal_Alts,
      pnLocal_Sequence_Option,
      pnLocal_Sequence_Option_ListOfAlts_Elmt,
      pnLocal_Sequence_Option_Alts,
      plx,
      percent,
      hex,
      pnLocalEsc]

-- [1] ShexDoc ::= Directive* ((NotStartAction | StartActions) Statement*)?
shexDoc :: Binding
shexDoc = define "ShexDoc" $
  T.record [
    "listOfDirective">: T.list (shex "Directive"),
    "Sequence">: T.maybe (shex "ShexDoc_Sequence_Option"),
    "PrefixDecl">: shex "PrefixDecl"]

shexDoc_Sequence_Option :: Binding
shexDoc_Sequence_Option = define "ShexDoc_Sequence_Option" $
  T.record [
    "alts">: shex "ShexDoc_Sequence_Option_Alts",
    "listOfStatement">: T.list (shex "Statement")]

shexDoc_Sequence_Option_Alts :: Binding
shexDoc_Sequence_Option_Alts = define "ShexDoc_Sequence_Option_Alts" $
  T.union [
    "NotStartAction">: shex "NotStartAction",
    "StartActions">: shex "StartActions"]

-- [2] Directive ::= BaseDecl | PrefixDecl
directive :: Binding
directive = define "Directive" $
  T.union [
    "BaseDecl">: shex "BaseDecl",
    "PrefixDecl">: shex "PrefixDecl"]

-- [3] BaseDecl ::= "BASE" IriRef
baseDecl :: Binding
baseDecl = define "BaseDecl" $ T.wrap $ shex "IriRef"

-- [4] PrefixDecl ::= "PREFIX" PnameNs IriRef
prefixDecl :: Binding
prefixDecl = define "PrefixDecl" $
  T.record [
    "PnameNs">: shex "PnameNs",
    "IriRef">: shex "IriRef"]

-- [5] NotStartAction ::= start | shapeExprDecl
notStartAction :: Binding
notStartAction = define "NotStartAction" $
  T.union [
    "start">: shex "ShapeExpression",
    "shapeExprDecl">: shex "NotStartAction_ShapeExprDecl"]

notStartAction_ShapeExprDecl :: Binding
notStartAction_ShapeExprDecl = define "NotStartAction_ShapeExprDecl" $
  T.record [
    "ShapeExprLabel">: shex "ShapeExprLabel",
    "alts">: shex "NotStartAction_ShapeExprDecl_Alts"]

notStartAction_ShapeExprDecl_Alts :: Binding
notStartAction_ShapeExprDecl_Alts = define "NotStartAction_ShapeExprDecl_Alts" $
  T.union [
    "ShapeExpression">: shex "ShapeExpression",
    "EXTERNAL">: T.unit]

-- [7] StartActions ::= CodeDecl+
startActions :: Binding
startActions = define "StartActions" $ T.wrap $ T.list $ shex "CodeDecl"

-- [8] Statement ::= Directive | NotStartAction
statement :: Binding
statement = define "Statement" $
  T.union [
    "Directive">: shex "Directive",
    "NotStartAction">: shex "NotStartAction"]

-- [10] ShapeExpression ::= ShapeOr
shapeExpression :: Binding
shapeExpression = define "ShapeExpression" $ T.wrap $ shex "ShapeOr"

-- [11] InlineShapeExpression ::= InlineShapeOr
inlineShapeExpression :: Binding
inlineShapeExpression = define "InlineShapeExpression" $ T.wrap $ shex "InlineShapeOr"

-- [12] ShapeOr ::= ShapeAnd ("OR" ShapeAnd)*
shapeOr :: Binding
shapeOr = define "ShapeOr" $
  T.record [
    "ShapeAnd">: shex "ShapeAnd",
    "listOfSequence">: T.list (shex "ShapeAnd")]

-- [13] InlineShapeOr ::= InlineShapeAnd ("OR" InlineShapeAnd)*
inlineShapeOr :: Binding
inlineShapeOr = define "InlineShapeOr" $
  T.record [
    "ShapeAnd">: shex "ShapeAnd",
    "listOfSequence">: T.list (shex "InlineShapeAnd")]

-- [14] ShapeAnd ::= ShapeNot ("AND" ShapeNot)*
shapeAnd :: Binding
shapeAnd = define "ShapeAnd" $
  T.record [
    "ShapeNot">: shex "ShapeNot",
    "listOfSequence">: T.list (shex "ShapeNot")]

-- [15] InlineShapeAnd ::= InlineShapeNot ("AND" InlineShapeNot)*
inlineShapeAnd :: Binding
inlineShapeAnd = define "InlineShapeAnd" $
  T.record [
    "InlineShapeNot">: shex "InlineShapeNot",
    "listOfSequence">: T.list (shex "InlineShapeNot")]

-- [16] ShapeNot ::= "NOT"? ShapeAtom
shapeNot :: Binding
shapeNot = define "ShapeNot" $
  T.record [
    "NOT">: T.maybe T.unit,
    "ShapeAtom">: shex "ShapeAtom"]

-- [17] InlineShapeNot ::= "NOT"? InlineShapeAtom
inlineShapeNot :: Binding
inlineShapeNot = define "InlineShapeNot" $
  T.record [
    "NOT">: T.maybe T.unit,
    "InlineShapeAtom">: shex "InlineShapeAtom"]

-- [18] ShapeAtom
shapeAtom :: Binding
shapeAtom = define "ShapeAtom" $
  T.union [
    "sequence">: shex "ShapeAtom_Sequence",
    "ShapeOrRef">: shex "ShapeOrRef",
    "sequence2">: shex "ShapeExpression",
    "Period">: T.unit]

shapeAtom_Sequence :: Binding
shapeAtom_Sequence = define "ShapeAtom_Sequence" $
  T.record [
    "NodeConstraint">: shex "NodeConstraint",
    "ShapeOrRef">: T.maybe (shex "ShapeOrRef")]

-- [19] InlineShapeAtom
inlineShapeAtom :: Binding
inlineShapeAtom = define "InlineShapeAtom" $
  T.union [
    "sequence">: shex "InlineShapeAtom_Sequence",
    "sequence2">: shex "InlineShapeAtom_Sequence2",
    "sequence3">: shex "ShapeExpression",
    "Period">: T.unit]

inlineShapeAtom_Sequence :: Binding
inlineShapeAtom_Sequence = define "InlineShapeAtom_Sequence" $
  T.record [
    "NodeConstraint">: shex "NodeConstraint",
    "InlineShapeOrRef">: T.maybe (shex "InlineShapeOrRef")]

inlineShapeAtom_Sequence2 :: Binding
inlineShapeAtom_Sequence2 = define "InlineShapeAtom_Sequence2" $
  T.record [
    "InlineShapeOrRef">: shex "InlineShapeOrRef",
    "NodeConstraint">: T.maybe (shex "NodeConstraint")]

-- [20] ShapeOrRef
shapeOrRef :: Binding
shapeOrRef = define "ShapeOrRef" $
  T.union [
    "ShapeDefinition">: shex "ShapeDefinition",
    "AtpNameLn">: shex "AtpNameLn",
    "AtpNameNs">: shex "AtpNameNs",
    "sequence">: shex "ShapeExprLabel"]

-- [21] InlineShapeOrRef
inlineShapeOrRef :: Binding
inlineShapeOrRef = define "InlineShapeOrRef" $
  T.union [
    "InlineShapeDefinition">: shex "InlineShapeDefinition",
    "AtpNameLn">: shex "AtpNameLn",
    "AtpNameNs">: shex "AtpNameNs",
    "sequence">: shex "ShapeExprLabel"]

-- [22] NodeConstraint
nodeConstraint :: Binding
nodeConstraint = define "NodeConstraint" $
  T.union [
    "sequence">: T.list (shex "XsFacet"),
    "sequence2">: shex "NodeConstraint_Sequence2",
    "sequence3">: shex "NodeConstraint_Sequence3",
    "sequence4">: shex "NodeConstraint_Sequence4",
    "sequence5">: shex "NodeConstraint_Sequence5",
    "listOfXsFacet">: T.list (shex "XsFacet")]

nodeConstraint_Sequence2 :: Binding
nodeConstraint_Sequence2 = define "NodeConstraint_Sequence2" $
  T.record [
    "NonLiteralKind">: shex "NonLiteralKind",
    "listOfStringFacet">: T.list (shex "StringFacet")]

nodeConstraint_Sequence3 :: Binding
nodeConstraint_Sequence3 = define "NodeConstraint_Sequence3" $
  T.record [
    "Datatype">: shex "Datatype",
    "listOfXsFacet">: T.list (shex "XsFacet")]

nodeConstraint_Sequence4 :: Binding
nodeConstraint_Sequence4 = define "NodeConstraint_Sequence4" $
  T.record [
    "ValueSet">: shex "ValueSet",
    "listOfXsFacet">: T.list (shex "XsFacet")]

nodeConstraint_Sequence5 :: Binding
nodeConstraint_Sequence5 = define "NodeConstraint_Sequence5" $
  T.record [
    "ValueSet">: shex "ValueSet",
    "listOfXsFacet">: T.list (shex "XsFacet")]

-- [23] NonLiteralKind ::= "IRI" | "BNODE" | "NONLITERAL"
nonLiteralKind :: Binding
nonLiteralKind = define "NonLiteralKind" $
  T.union [
    "IRI">: T.unit,
    "BNODE">: T.unit,
    "NONLITERAL">: T.unit]

-- [24] XsFacet ::= StringFacet | NumericFacet
xsFacet :: Binding
xsFacet = define "XsFacet" $
  T.union [
    "StringFacet">: shex "StringFacet",
    "NumericFacet">: shex "NumericFacet"]

-- [25] StringFacet ::= StringLength Integer | Regexp
stringFacet :: Binding
stringFacet = define "StringFacet" $
  T.union [
    "sequence">: shex "StringFacet_Sequence",
    "Regexp">: shex "Regexp"]

stringFacet_Sequence :: Binding
stringFacet_Sequence = define "StringFacet_Sequence" $
  T.record [
    "StringLength">: shex "StringLength",
    "Integer">: shex "Integer"]

-- [26] StringLength ::= "LENGTH" | "MINLENGTH" | "MAXLENGTH"
stringLength :: Binding
stringLength = define "StringLength" $
  T.union [
    "LENGTH">: T.unit,
    "MINLENGTH">: T.unit,
    "MAXLENGTH">: T.unit]

-- [27] NumericFacet
numericFacet :: Binding
numericFacet = define "NumericFacet" $
  T.union [
    "sequence">: shex "NumericFacet_Sequence",
    "sequence2">: shex "NumericFacet_Sequence2"]

numericFacet_Sequence :: Binding
numericFacet_Sequence = define "NumericFacet_Sequence" $
  T.record [
    "NumericRange">: shex "NumericRange",
    "NumericLiteral">: shex "NumericLiteral"]

numericFacet_Sequence2 :: Binding
numericFacet_Sequence2 = define "NumericFacet_Sequence2" $
  T.record [
    "NumericLength">: shex "NumericLength",
    "Integer">: shex "Integer"]

-- [28] NumericRange
numericRange :: Binding
numericRange = define "NumericRange" $
  T.union [
    "MININCLUSIVE">: T.unit,
    "MINEXCLUSIVE">: T.unit,
    "MAXINCLUSIVE">: T.unit,
    "MAXEXCLUSIVE">: T.unit]

-- [29] NumericLength
numericLength :: Binding
numericLength = define "NumericLength" $
  T.union [
    "TOTALDIGITS">: T.unit,
    "FRACTIONDIGITS">: T.unit]

-- [30] ShapeDefinition
shapeDefinition :: Binding
shapeDefinition = define "ShapeDefinition" $
  T.record [
    "listOfAlts">: T.list (shex "ShapeDefinition_ListOfAlts_Elmt"),
    "TripleExpression">: T.maybe (shex "TripleExpression"),
    "listOfAnnotation">: T.list (shex "Annotation"),
    "SemanticActions">: shex "SemanticActions"]

shapeDefinition_ListOfAlts_Elmt :: Binding
shapeDefinition_ListOfAlts_Elmt = define "ShapeDefinition_ListOfAlts_Elmt" $
  T.union [
    "IncludeSet">: shex "IncludeSet",
    "ExtraPropertySet">: shex "ExtraPropertySet",
    "CLOSED">: T.unit]

-- [31] InlineShapeDefinition
inlineShapeDefinition :: Binding
inlineShapeDefinition = define "InlineShapeDefinition" $
  T.record [
    "listOfAlts">: T.list (shex "InlineShapeDefinition_ListOfAlts_Elmt"),
    "TripleExpression">: T.maybe (shex "TripleExpression")]

inlineShapeDefinition_ListOfAlts_Elmt :: Binding
inlineShapeDefinition_ListOfAlts_Elmt = define "InlineShapeDefinition_ListOfAlts_Elmt" $
  T.union [
    "IncludeSet">: shex "IncludeSet",
    "ExtraPropertySet">: shex "ExtraPropertySet",
    "CLOSED">: T.unit]

-- [32] ExtraPropertySet ::= "EXTRA" Predicate+
extraPropertySet :: Binding
extraPropertySet = define "ExtraPropertySet" $ T.wrap $ T.list $ shex "Predicate"

-- [33] TripleExpression ::= OneOfTripleExpr
tripleExpression :: Binding
tripleExpression = define "TripleExpression" $ T.wrap $ shex "OneOfTripleExpr"

-- [34] OneOfTripleExpr ::= GroupTripleExpr | MultiElementOneOf
oneOfTripleExpr :: Binding
oneOfTripleExpr = define "OneOfTripleExpr" $
  T.union [
    "GroupTripleExpr">: shex "GroupTripleExpr",
    "MultiElementOneOf">: shex "MultiElementOneOf"]

-- [35] MultiElementOneOf ::= GroupTripleExpr ('|' GroupTripleExpr)+
multiElementOneOf :: Binding
multiElementOneOf = define "MultiElementOneOf" $
  T.record [
    "GroupTripleExpr">: shex "GroupTripleExpr",
    "listOfSequence">: T.list (shex "GroupTripleExpr")]

-- [36] InnerTripleExpr ::= MultiElementGroup | MultiElementOneOf
innerTripleExpr :: Binding
innerTripleExpr = define "InnerTripleExpr" $
  T.union [
    "MultiElementGroup">: shex "MultiElementGroup",
    "MultiElementOneOf">: shex "MultiElementOneOf"]

-- [37] GroupTripleExpr ::= SingleElementGroup | MultiElementGroup
groupTripleExpr :: Binding
groupTripleExpr = define "GroupTripleExpr" $
  T.union [
    "SingleElementGroup">: shex "SingleElementGroup",
    "MultiElementGroup">: shex "MultiElementGroup"]

-- [38] SingleElementGroup ::= UnaryTripleExpr ';'?
singleElementGroup :: Binding
singleElementGroup = define "SingleElementGroup" $
  T.record [
    "UnaryTripleExpr">: shex "UnaryTripleExpr",
    "Semi">: T.maybe T.unit]

-- [39] MultiElementGroup ::= UnaryTripleExpr (';' UnaryTripleExpr)+ ';'?
multiElementGroup :: Binding
multiElementGroup = define "MultiElementGroup" $
  T.record [
    "UnaryTripleExpr">: shex "UnaryTripleExpr",
    "listOfSequence">: T.list (shex "UnaryTripleExpr"),
    "Semi">: T.maybe T.unit]

-- [40] UnaryTripleExpr
unaryTripleExpr :: Binding
unaryTripleExpr = define "UnaryTripleExpr" $
  T.union [
    "sequence">: shex "UnaryTripleExpr_Sequence",
    "Include">: shex "Include"]

unaryTripleExpr_Sequence :: Binding
unaryTripleExpr_Sequence = define "UnaryTripleExpr_Sequence" $
  T.record [
    "Sequence">: T.maybe (shex "TripleExprLabel"),
    "alts">: shex "UnaryTripleExpr_Sequence_Alts"]

unaryTripleExpr_Sequence_Alts :: Binding
unaryTripleExpr_Sequence_Alts = define "UnaryTripleExpr_Sequence_Alts" $
  T.union [
    "TripleConstraint">: shex "TripleConstraint",
    "BracketedTripleExpr">: shex "BracketedTripleExpr"]

-- [41] BracketedTripleExpr
bracketedTripleExpr :: Binding
bracketedTripleExpr = define "BracketedTripleExpr" $
  T.record [
    "InnerTripleExpr">: shex "InnerTripleExpr",
    "Cardinality">: T.maybe (shex "Cardinality"),
    "listOfAnnotation">: T.list (shex "Annotation"),
    "SemanticActions">: shex "SemanticActions"]

-- [43] TripleConstraint
tripleConstraint :: Binding
tripleConstraint = define "TripleConstraint" $
  T.record [
    "SenseFlags">: T.maybe (shex "SenseFlags"),
    "Predicate">: shex "Predicate",
    "InlineShapeExpression">: shex "InlineShapeExpression",
    "Cardinality">: T.maybe (shex "Cardinality"),
    "listOfAnnotation">: T.list (shex "Annotation"),
    "SemanticActions">: shex "SemanticActions"]

-- [44] Cardinality ::= '*' | '+' | '?' | RepeatRange
cardinality :: Binding
cardinality = define "Cardinality" $
  T.union [
    "Ast">: T.unit,
    "Plus">: T.unit,
    "Quest">: T.unit,
    "RepeatRange">: shex "RepeatRange"]

-- [45] SenseFlags ::= '^'
senseFlags :: Binding
senseFlags = define "SenseFlags" $ T.wrap T.unit

-- [46] ValueSet ::= '[' ValueSetValue* ']'
valueSet :: Binding
valueSet = define "ValueSet" $ T.wrap $ T.list $ shex "ValueSetValue"

-- [47] ValueSetValue ::= IriRange | Literal
valueSetValue :: Binding
valueSetValue = define "ValueSetValue" $
  T.union [
    "IriRange">: shex "IriRange",
    "Literal">: shex "Literal"]

-- [48] IriRange
iriRange :: Binding
iriRange = define "IriRange" $
  T.union [
    "sequence">: shex "IriRange_Sequence",
    "sequence2">: T.list (shex "Exclusion")]

iriRange_Sequence :: Binding
iriRange_Sequence = define "IriRange_Sequence" $
  T.record [
    "Iri">: shex "Iri",
    "Sequence">: T.maybe (T.list (shex "Exclusion"))]

-- [49] Exclusion ::= '-' Iri '~'?
exclusion :: Binding
exclusion = define "Exclusion" $ T.wrap $ shex "Iri"

-- [50] Include ::= '&' TripleExprLabel
include :: Binding
include = define "Include" $ T.wrap $ shex "TripleExprLabel"

-- [51] Annotation ::= '//' Predicate (Iri | Literal)
annotation :: Binding
annotation = define "Annotation" $
  T.record [
    "Predicate">: shex "Predicate",
    "alts">: shex "Annotation_Alts"]

annotation_Alts :: Binding
annotation_Alts = define "Annotation_Alts" $
  T.union [
    "Iri">: shex "Iri",
    "Literal">: shex "Literal"]

-- [52] SemanticActions ::= CodeDecl*
semanticActions :: Binding
semanticActions = define "SemanticActions" $ T.wrap $ T.list $ shex "CodeDecl"

-- [53] CodeDecl ::= '%' Iri (Code | "%")
codeDecl :: Binding
codeDecl = define "CodeDecl" $
  T.record [
    "Iri">: shex "Iri",
    "alts">: shex "CodeDecl_Alts"]

codeDecl_Alts :: Binding
codeDecl_Alts = define "CodeDecl_Alts" $
  T.union [
    "Code">: shex "Code",
    "Percnt">: T.unit]

-- [13t] Literal ::= RdfLiteral | NumericLiteral | BooleanLiteral
literal :: Binding
literal = define "Literal" $
  T.union [
    "RdfLiteral">: shex "RdfLiteral",
    "NumericLiteral">: shex "NumericLiteral",
    "BooleanLiteral">: shex "BooleanLiteral"]

-- [54] Predicate ::= Iri | RdfType
predicate :: Binding
predicate = define "Predicate" $
  T.union [
    "Iri">: shex "Iri",
    "RdfType">: shex "RdfType"]

-- [55] Datatype ::= Iri
datatype_ :: Binding
datatype_ = define "Datatype" $ T.wrap $ shex "Iri"

-- [56] ShapeExprLabel ::= Iri | BlankNode
shapeExprLabel :: Binding
shapeExprLabel = define "ShapeExprLabel" $
  T.union [
    "Iri">: shex "Iri",
    "BlankNode">: shex "BlankNode"]

-- [42] TripleExprLabel ::= Iri | BlankNode
tripleExprLabel :: Binding
tripleExprLabel = define "TripleExprLabel" $
  T.union [
    "Iri">: shex "Iri",
    "BlankNode">: shex "BlankNode"]

-- [16t] NumericLiteral ::= Integer | Decimal | Double
numericLiteral :: Binding
numericLiteral = define "NumericLiteral" $
  T.union [
    "Integer">: shex "Integer",
    "Decimal">: shex "Decimal",
    "Double">: shex "Double"]

-- [129s] RdfLiteral ::= String (LangTag | '^^' Datatype)?
rdfLiteral :: Binding
rdfLiteral = define "RdfLiteral" $
  T.record [
    "String">: shex "String",
    "Alts">: T.maybe (shex "RdfLiteral_Alts_Option")]

rdfLiteral_Alts_Option :: Binding
rdfLiteral_Alts_Option = define "RdfLiteral_Alts_Option" $
  T.union [
    "LangTag">: shex "LangTag",
    "sequence">: shex "Datatype"]

-- [134s] BooleanLiteral ::= 'true' | 'false'
booleanLiteral :: Binding
booleanLiteral = define "BooleanLiteral" $
  T.union [
    "True">: T.unit,
    "False">: T.unit]

-- [135s] String
string_ :: Binding
string_ = define "String" $
  T.union [
    "StringLiteral1">: shex "StringLiteral1",
    "StringLiteralLong1">: shex "StringLiteralLong1",
    "StringLiteral2">: shex "StringLiteral2",
    "StringLiteralLong2">: shex "StringLiteralLong2"]

-- [136s] Iri ::= IriRef | PrefixedName
iri :: Binding
iri = define "Iri" $
  T.union [
    "IriRef">: shex "IriRef",
    "PrefixedName">: shex "PrefixedName"]

-- [137s] PrefixedName ::= PnameLn | PnameNs
prefixedName :: Binding
prefixedName = define "PrefixedName" $
  T.union [
    "PnameLn">: shex "PnameLn",
    "PnameNs">: shex "PnameNs"]

-- [138s] BlankNode ::= BlankNodeLabel
blankNode :: Binding
blankNode = define "BlankNode" $ T.wrap $ shex "BlankNodeLabel"

-- [57] IncludeSet ::= '&' ShapeExprLabel+
includeSet :: Binding
includeSet = define "IncludeSet" $ T.wrap $ T.list $ shex "ShapeExprLabel"

-- [58] Code
code :: Binding
code = define "Code" $ T.wrap $ T.list $ shex "Code_Elmt"

code_Elmt :: Binding
code_Elmt = define "Code_Elmt" $
  T.union [
    "regex">: T.string,
    "sequence">: T.string,
    "Uchar">: shex "Uchar"]

-- [59] RepeatRange
repeatRange :: Binding
repeatRange = define "RepeatRange" $
  T.record [
    "Integer">: shex "Integer",
    "Sequence">: T.maybe (T.maybe (T.maybe (shex "RepeatRange_Sequence_Option_Option_Option")))]

repeatRange_Sequence_Option_Option_Option :: Binding
repeatRange_Sequence_Option_Option_Option = define "RepeatRange_Sequence_Option_Option_Option" $
  T.union [
    "Integer">: shex "Integer",
    "Ast">: T.unit]

-- [60] RdfType ::= 'a'
rdfType :: Binding
rdfType = define "RdfType" $ T.wrap T.unit

-- [18t] IriRef
iriRef :: Binding
iriRef = define "IriRef" $ T.wrap $ T.list $ shex "IriRef_Elmt"

iriRef_Elmt :: Binding
iriRef_Elmt = define "IriRef_Elmt" $
  T.union [
    "regex">: T.string,
    "Uchar">: shex "Uchar"]

-- [140s] PnameNs ::= PnPrefix? ':'
pnameNs :: Binding
pnameNs = define "PnameNs" $ T.wrap $ T.maybe $ shex "PnPrefix"

-- [141s] PnameLn ::= PnameNs PnLocal
pnameLn :: Binding
pnameLn = define "PnameLn" $
  T.record [
    "PnameNs">: shex "PnameNs",
    "PnLocal">: shex "PnLocal"]

-- [61] AtpNameNs ::= '@' PnPrefix? ':'
atpNameNs :: Binding
atpNameNs = define "AtpNameNs" $ T.wrap $ T.maybe $ shex "PnPrefix"

-- [62] AtpNameLn ::= '@' PnameNs PnLocal
atpNameLn :: Binding
atpNameLn = define "AtpNameLn" $
  T.record [
    "PnameNs">: shex "PnameNs",
    "PnLocal">: shex "PnLocal"]

-- [63] Regexp
regexp :: Binding
regexp = define "Regexp" $
  T.record [
    "listOfAlts">: T.list (shex "Regexp_ListOfAlts_Elmt"),
    "listOfRegex">: T.list T.string]

regexp_ListOfAlts_Elmt :: Binding
regexp_ListOfAlts_Elmt = define "Regexp_ListOfAlts_Elmt" $
  T.union [
    "regex">: T.string,
    "sequence">: T.string,
    "Uchar">: shex "Uchar"]

-- [142s] BlankNodeLabel
blankNodeLabel :: Binding
blankNodeLabel = define "BlankNodeLabel" $
  T.record [
    "alts">: shex "BlankNodeLabel_Alts",
    "ListOfAlts">: T.maybe (T.list (shex "BlankNodeLabel_ListOfAlts_Option_Elmt")),
    "PnChars">: shex "PnChars"]

blankNodeLabel_Alts :: Binding
blankNodeLabel_Alts = define "BlankNodeLabel_Alts" $
  T.union [
    "PnCharsU">: shex "PnCharsU",
    "regex">: T.string]

blankNodeLabel_ListOfAlts_Option_Elmt :: Binding
blankNodeLabel_ListOfAlts_Option_Elmt = define "BlankNodeLabel_ListOfAlts_Option_Elmt" $
  T.union [
    "PnChars">: shex "PnChars",
    "Period">: T.unit]

-- [145s] LangTag
langTag :: Binding
langTag = define "LangTag" $ T.wrap T.string

-- [19t] Integer
integer_ :: Binding
integer_ = define "Integer" $ T.wrap T.string

-- [20t] Decimal
decimal :: Binding
decimal = define "Decimal" $ T.wrap T.string

-- [21t] Double
double_ :: Binding
double_ = define "Double" $ T.wrap T.string

-- [156s] StringLiteral1
stringLiteral1 :: Binding
stringLiteral1 = define "StringLiteral1" $ T.wrap $ T.list $ shex "StringLiteral1_Elmt"

stringLiteral1_Elmt :: Binding
stringLiteral1_Elmt = define "StringLiteral1_Elmt" $
  T.union [
    "regex">: T.string,
    "Echar">: shex "Echar",
    "Uchar">: shex "Uchar"]

-- [157s] StringLiteral2
stringLiteral2 :: Binding
stringLiteral2 = define "StringLiteral2" $ T.wrap $ T.list $ shex "StringLiteral2_Elmt"

stringLiteral2_Elmt :: Binding
stringLiteral2_Elmt = define "StringLiteral2_Elmt" $
  T.union [
    "regex">: T.string,
    "Echar">: shex "Echar",
    "Uchar">: shex "Uchar"]

-- [158s] StringLiteralLong1
stringLiteralLong1 :: Binding
stringLiteralLong1 = define "StringLiteralLong1" $ T.wrap $ T.list $ shex "StringLiteralLong1_Elmt"

stringLiteralLong1_Elmt :: Binding
stringLiteralLong1_Elmt = define "StringLiteralLong1_Elmt" $
  T.union [
    "sequence">: shex "StringLiteralLong1_Elmt_Sequence",
    "Echar">: shex "Echar",
    "Uchar">: shex "Uchar"]

stringLiteralLong1_Elmt_Sequence :: Binding
stringLiteralLong1_Elmt_Sequence = define "StringLiteralLong1_Elmt_Sequence" $
  T.record [
    "Alts">: T.maybe (shex "StringLiteralLong1_Elmt_Sequence_Alts_Option"),
    "regex">: T.string]

stringLiteralLong1_Elmt_Sequence_Alts_Option :: Binding
stringLiteralLong1_Elmt_Sequence_Alts_Option = define "StringLiteralLong1_Elmt_Sequence_Alts_Option" $
  T.union [
    "Apos">: T.unit,
    "sequence">: shex "StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence"]

stringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence :: Binding
stringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence = define "StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence" $
  T.record []

-- [159s] StringLiteralLong2
stringLiteralLong2 :: Binding
stringLiteralLong2 = define "StringLiteralLong2" $ T.wrap $ T.list $ shex "StringLiteralLong2_Elmt"

stringLiteralLong2_Elmt :: Binding
stringLiteralLong2_Elmt = define "StringLiteralLong2_Elmt" $
  T.union [
    "sequence">: shex "StringLiteralLong2_Elmt_Sequence",
    "Echar">: shex "Echar",
    "Uchar">: shex "Uchar"]

stringLiteralLong2_Elmt_Sequence :: Binding
stringLiteralLong2_Elmt_Sequence = define "StringLiteralLong2_Elmt_Sequence" $
  T.record [
    "Alts">: T.maybe (shex "StringLiteralLong2_Elmt_Sequence_Alts_Option"),
    "regex">: T.string]

stringLiteralLong2_Elmt_Sequence_Alts_Option :: Binding
stringLiteralLong2_Elmt_Sequence_Alts_Option = define "StringLiteralLong2_Elmt_Sequence_Alts_Option" $
  T.union [
    "Quot">: T.unit,
    "sequence">: shex "StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence"]

stringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence :: Binding
stringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence = define "StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence" $
  T.record []

-- [26t] Uchar
uchar :: Binding
uchar = define "Uchar" $
  T.union [
    "sequence">: shex "Uchar_Sequence",
    "sequence2">: shex "Uchar_Sequence2"]

uchar_Sequence :: Binding
uchar_Sequence = define "Uchar_Sequence" $
  T.record [
    "Hex">: shex "Hex",
    "Hex2">: shex "Hex",
    "Hex3">: shex "Hex",
    "Hex4">: shex "Hex"]

uchar_Sequence2 :: Binding
uchar_Sequence2 = define "Uchar_Sequence2" $
  T.record [
    "Hex">: shex "Hex",
    "Hex2">: shex "Hex",
    "Hex3">: shex "Hex",
    "Hex4">: shex "Hex",
    "Hex5">: shex "Hex",
    "Hex6">: shex "Hex",
    "Hex7">: shex "Hex",
    "Hex8">: shex "Hex"]

-- [160s] Echar ::= '\\' [tbnrf\\\"\']
echar :: Binding
echar = define "Echar" $ T.wrap T.string

-- [164s] PnCharsBase
pnCharsBase :: Binding
pnCharsBase = define "PnCharsBase" $
  T.union [
    "regex">: T.string,
    "regex2">: T.string]

-- [165s] PnCharsU ::= PnCharsBase | '_'
pnCharsU :: Binding
pnCharsU = define "PnCharsU" $
  T.union [
    "PnCharsBase">: shex "PnCharsBase",
    "Lowbar">: T.unit]

-- [167s] PnChars
pnChars :: Binding
pnChars = define "PnChars" $
  T.union [
    "PnCharsU">: shex "PnCharsU",
    "Minus">: T.unit,
    "regex">: T.string]

-- [168s] PnPrefix
pnPrefix :: Binding
pnPrefix = define "PnPrefix" $
  T.record [
    "PnCharsBase">: shex "PnCharsBase",
    "Sequence">: T.maybe (shex "PnPrefix_Sequence_Option")]

pnPrefix_Sequence_Option :: Binding
pnPrefix_Sequence_Option = define "PnPrefix_Sequence_Option" $
  T.record [
    "alts">: shex "PnPrefix_Sequence_Option_Alts",
    "PnChars">: shex "PnChars"]

pnPrefix_Sequence_Option_Alts :: Binding
pnPrefix_Sequence_Option_Alts = define "PnPrefix_Sequence_Option_Alts" $
  T.union [
    "PnChars">: shex "PnChars",
    "Period">: T.unit]

-- [169s] PnLocal
pnLocal :: Binding
pnLocal = define "PnLocal" $
  T.record [
    "alts">: shex "PnLocal_Alts",
    "Sequence">: T.maybe (shex "PnLocal_Sequence_Option")]

pnLocal_Alts :: Binding
pnLocal_Alts = define "PnLocal_Alts" $
  T.union [
    "PnCharsU">: shex "PnCharsU",
    "Colon">: T.unit,
    "regex">: T.string,
    "Plx">: shex "Plx"]

pnLocal_Sequence_Option :: Binding
pnLocal_Sequence_Option = define "PnLocal_Sequence_Option" $
  T.record [
    "listOfAlts">: T.list (shex "PnLocal_Sequence_Option_ListOfAlts_Elmt"),
    "alts">: shex "PnLocal_Sequence_Option_Alts"]

pnLocal_Sequence_Option_ListOfAlts_Elmt :: Binding
pnLocal_Sequence_Option_ListOfAlts_Elmt = define "PnLocal_Sequence_Option_ListOfAlts_Elmt" $
  T.union [
    "PnChars">: shex "PnChars",
    "Period">: T.unit,
    "Colon">: T.unit,
    "Plx">: shex "Plx"]

pnLocal_Sequence_Option_Alts :: Binding
pnLocal_Sequence_Option_Alts = define "PnLocal_Sequence_Option_Alts" $
  T.union [
    "PnChars">: shex "PnChars",
    "Colon">: T.unit,
    "Plx">: shex "Plx"]

-- [170s] Plx ::= Percent | PnLocalEsc
plx :: Binding
plx = define "Plx" $
  T.union [
    "Percent">: shex "Percent",
    "PnLocalEsc">: shex "PnLocalEsc"]

-- [171s] Percent ::= '%' Hex Hex
percent :: Binding
percent = define "Percent" $
  T.record [
    "Hex">: shex "Hex",
    "Hex2">: shex "Hex"]

-- [172s] Hex ::= [0-9] | [A-F] | [a-f]
hex :: Binding
hex = define "Hex" $ T.wrap T.string

-- [173s] PnLocalEsc
pnLocalEsc :: Binding
pnLocalEsc = define "PnLocalEsc" $ T.wrap T.string
