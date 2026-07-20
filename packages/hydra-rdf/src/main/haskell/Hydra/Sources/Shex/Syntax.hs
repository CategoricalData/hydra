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
    -- Alphabetical order by local type name, per the definition-ordering style guide
    -- (Validate.Packaging.checkDefinitionOrdering has no section-boundary awareness).
    definitions = [
      annotation,
      annotationAlts,
      atpNameLn,
      atpNameNs,
      baseDecl,
      blankNode,
      blankNodeLabel,
      blankNodeLabelAlts,
      blankNodeLabelListOfAltsOptionElmt,
      booleanLiteral,
      bracketedTripleExpr,
      cardinality,
      code,
      codeDecl,
      codeDeclAlts,
      codeElmt,
      datatype_,
      decimal,
      directive,
      double_,
      echar,
      exclusion,
      extraPropertySet,
      groupTripleExpr,
      hex,
      include,
      includeSet,
      inlineShapeAnd,
      inlineShapeAtom,
      inlineShapeAtomSequence,
      inlineShapeAtomSequence2,
      inlineShapeDefinition,
      inlineShapeDefinitionListOfAltsElmt,
      inlineShapeExpression,
      inlineShapeNot,
      inlineShapeOr,
      inlineShapeOrRef,
      innerTripleExpr,
      integer_,
      iri,
      iriRange,
      iriRangeSequence,
      iriRef,
      iriRefElmt,
      langTag,
      literal,
      multiElementGroup,
      multiElementOneOf,
      nodeConstraint,
      nodeConstraintSequence2,
      nodeConstraintSequence3,
      nodeConstraintSequence4,
      nodeConstraintSequence5,
      nonLiteralKind,
      notStartAction,
      notStartActionShapeExprDecl,
      notStartActionShapeExprDeclAlts,
      numericFacet,
      numericFacetSequence,
      numericFacetSequence2,
      numericLength,
      numericLiteral,
      numericRange,
      oneOfTripleExpr,
      percent,
      plx,
      pnChars,
      pnCharsBase,
      pnCharsU,
      pnLocal,
      pnLocalAlts,
      pnLocalEsc,
      pnLocalSequenceOption,
      pnLocalSequenceOptionAlts,
      pnLocalSequenceOptionListOfAltsElmt,
      pnPrefix,
      pnPrefixSequenceOption,
      pnPrefixSequenceOptionAlts,
      pnameLn,
      pnameNs,
      predicate,
      prefixDecl,
      prefixedName,
      rdfLiteral,
      rdfLiteralAltsOption,
      rdfType,
      regexp,
      regexpListOfAltsElmt,
      repeatRange,
      repeatRangeSequenceOptionOptionOption,
      semanticActions,
      senseFlags,
      shapeAnd,
      shapeAtom,
      shapeAtomSequence,
      shapeDefinition,
      shapeDefinitionListOfAltsElmt,
      shapeExprLabel,
      shapeExpression,
      shapeNot,
      shapeOr,
      shapeOrRef,
      shexDoc,
      shexDocSequenceOption,
      shexDocSequenceOptionAlts,
      singleElementGroup,
      startActions,
      statement,
      string_,
      stringFacet,
      stringFacetSequence,
      stringLength,
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
      tripleConstraint,
      tripleExprLabel,
      tripleExpression,
      uchar,
      ucharSequence,
      ucharSequence2,
      unaryTripleExpr,
      unaryTripleExprSequence,
      unaryTripleExprSequenceAlts,
      valueSet,
      valueSetValue,
      xsFacet]

-- [51] Annotation ::= '//' Predicate (Iri | Literal)
annotation :: TypeDefinition
annotation = define "Annotation" $
  doc "ShEx grammar production: Annotation ::= '//' Predicate (Iri | Literal)" $
  T.record [
    "Predicate">: shex "Predicate",
    "alts">: shex "AnnotationAlts"]

annotationAlts :: TypeDefinition
annotationAlts = define "AnnotationAlts" $
  doc "A synthetic Hydra type representing the alternatives of the ShEx Annotation production" $
  T.union [
    "Iri">: shex "Iri",
    "Literal">: shex "Literal"]

-- [62] AtpNameLn ::= '@' PnameNs PnLocal
atpNameLn :: TypeDefinition
atpNameLn = define "AtpNameLn" $
  doc "ShEx grammar production: AtpNameLn ::= '@' PnameNs PnLocal" $
  T.record [
    "PnameNs">: shex "PnameNs",
    "PnLocal">: shex "PnLocal"]

-- [61] AtpNameNs ::= '@' PnPrefix? ':'
atpNameNs :: TypeDefinition
atpNameNs = define "AtpNameNs" $
  doc "ShEx grammar production: AtpNameNs ::= '@' PnPrefix? ':'" $
  T.wrap $ T.optional $ shex "PnPrefix"

-- [3] BaseDecl ::= "BASE" IriRef
baseDecl :: TypeDefinition
baseDecl = define "BaseDecl" $
  doc "ShEx grammar production: BaseDecl ::= 'BASE' IriRef" $
  T.wrap $ shex "IriRef"

-- [138s] BlankNode ::= BlankNodeLabel
blankNode :: TypeDefinition
blankNode = define "BlankNode" $
  doc "ShEx grammar production: BlankNode ::= BlankNodeLabel" $
  T.wrap $ shex "BlankNodeLabel"

-- [142s] BlankNodeLabel
blankNodeLabel :: TypeDefinition
blankNodeLabel = define "BlankNodeLabel" $
  doc "A component of the ShEx BlankNodeLabel production" $
  T.record [
    "alts">: shex "BlankNodeLabelAlts",
    "ListOfAlts">: T.optional (T.list (shex "BlankNodeLabelListOfAltsOptionElmt")),
    "PnChars">: shex "PnChars"]

blankNodeLabelAlts :: TypeDefinition
blankNodeLabelAlts = define "BlankNodeLabelAlts" $
  doc "A synthetic Hydra type representing the alternatives of the ShEx BlankNodeLabel production" $
  T.union [
    "PnCharsU">: shex "PnCharsU",
    "regex">: T.string]

blankNodeLabelListOfAltsOptionElmt :: TypeDefinition
blankNodeLabelListOfAltsOptionElmt = define "BlankNodeLabelListOfAltsOptionElmt" $
  doc "A synthetic Hydra type representing one alternative of an optional list element within the ShEx BlankNodeLabel production" $
  T.union [
    "PnChars">: shex "PnChars",
    "Period">: T.unit]

-- [134s] BooleanLiteral ::= 'true' | 'false'
booleanLiteral :: TypeDefinition
booleanLiteral = define "BooleanLiteral" $
  doc "ShEx grammar production: BooleanLiteral ::= 'true' | 'false'" $
  T.union [
    "True">: T.unit,
    "False">: T.unit]

-- [41] BracketedTripleExpr
bracketedTripleExpr :: TypeDefinition
bracketedTripleExpr = define "BracketedTripleExpr" $
  doc "A component of the ShEx BracketedTripleExpr production" $
  T.record [
    "InnerTripleExpr">: shex "InnerTripleExpr",
    "Cardinality">: T.optional (shex "Cardinality"),
    "listOfAnnotation">: T.list (shex "Annotation"),
    "SemanticActions">: shex "SemanticActions"]

-- [44] Cardinality ::= '*' | '+' | '?' | RepeatRange
cardinality :: TypeDefinition
cardinality = define "Cardinality" $
  doc "ShEx grammar production: Cardinality ::= '*' | '+' | '?' | RepeatRange" $
  T.union [
    "Ast">: T.unit,
    "Plus">: T.unit,
    "Quest">: T.unit,
    "RepeatRange">: shex "RepeatRange"]

-- [58] Code
code :: TypeDefinition
code = define "Code" $
  doc "A component of the ShEx Code production" $
  T.wrap $ T.list $ shex "CodeElmt"

-- [53] CodeDecl ::= '%' Iri (Code | "%")
codeDecl :: TypeDefinition
codeDecl = define "CodeDecl" $
  doc "ShEx grammar production: CodeDecl ::= '%' Iri (Code | '%')" $
  T.record [
    "Iri">: shex "Iri",
    "alts">: shex "CodeDeclAlts"]

codeDeclAlts :: TypeDefinition
codeDeclAlts = define "CodeDeclAlts" $
  doc "A synthetic Hydra type representing the alternatives of the ShEx CodeDecl production" $
  T.union [
    "Code">: shex "Code",
    "Percnt">: T.unit]

codeElmt :: TypeDefinition
codeElmt = define "CodeElmt" $
  doc "A synthetic Hydra type representing one element of the ShEx Code production" $
  T.union [
    "regex">: T.string,
    "sequence">: T.string,
    "Uchar">: shex "Uchar"]

-- [55] Datatype ::= Iri
datatype_ :: TypeDefinition
datatype_ = define "Datatype" $
  doc "ShEx grammar production: Datatype ::= Iri" $
  T.wrap $ shex "Iri"

-- [20t] Decimal
decimal :: TypeDefinition
decimal = define "Decimal" $
  doc "A component of the ShEx Decimal production" $
  T.wrap T.string

-- [2] Directive ::= BaseDecl | PrefixDecl
directive :: TypeDefinition
directive = define "Directive" $
  doc "ShEx grammar production: Directive ::= BaseDecl | PrefixDecl" $
  T.union [
    "BaseDecl">: shex "BaseDecl",
    "PrefixDecl">: shex "PrefixDecl"]

-- [21t] Double
double_ :: TypeDefinition
double_ = define "Double" $
  doc "A component of the ShEx Double production" $
  T.wrap T.string

-- [160s] Echar ::= '\\' [tbnrf\\\"\']
echar :: TypeDefinition
echar = define "Echar" $
  doc "ShEx grammar production: Echar ::= '\\' [tbnrf\\\'\']" $
  T.wrap T.string

-- [49] Exclusion ::= '-' Iri '~'?
exclusion :: TypeDefinition
exclusion = define "Exclusion" $
  doc "ShEx grammar production: Exclusion ::= '-' Iri '~'?" $
  T.wrap $ shex "Iri"

-- [32] ExtraPropertySet ::= "EXTRA" Predicate+
extraPropertySet :: TypeDefinition
extraPropertySet = define "ExtraPropertySet" $
  doc "ShEx grammar production: ExtraPropertySet ::= 'EXTRA' Predicate+" $
  T.wrap $ T.list $ shex "Predicate"

-- [37] GroupTripleExpr ::= SingleElementGroup | MultiElementGroup
groupTripleExpr :: TypeDefinition
groupTripleExpr = define "GroupTripleExpr" $
  doc "ShEx grammar production: GroupTripleExpr ::= SingleElementGroup | MultiElementGroup" $
  T.union [
    "SingleElementGroup">: shex "SingleElementGroup",
    "MultiElementGroup">: shex "MultiElementGroup"]

-- [172s] Hex ::= [0-9] | [A-F] | [a-f]
hex :: TypeDefinition
hex = define "Hex" $
  doc "ShEx grammar production: Hex ::= [0-9] | [A-F] | [a-f]" $
  T.wrap T.string

-- [50] Include ::= '&' TripleExprLabel
include :: TypeDefinition
include = define "Include" $
  doc "ShEx grammar production: Include ::= '&' TripleExprLabel" $
  T.wrap $ shex "TripleExprLabel"

-- [57] IncludeSet ::= '&' ShapeExprLabel+
includeSet :: TypeDefinition
includeSet = define "IncludeSet" $
  doc "ShEx grammar production: IncludeSet ::= '&' ShapeExprLabel+" $
  T.wrap $ T.list $ shex "ShapeExprLabel"

-- [15] InlineShapeAnd ::= InlineShapeNot ("AND" InlineShapeNot)*
inlineShapeAnd :: TypeDefinition
inlineShapeAnd = define "InlineShapeAnd" $
  doc "ShEx grammar production: InlineShapeAnd ::= InlineShapeNot ('AND' InlineShapeNot)*" $
  T.record [
    "InlineShapeNot">: shex "InlineShapeNot",
    "listOfSequence">: T.list (shex "InlineShapeNot")]

-- [19] InlineShapeAtom
inlineShapeAtom :: TypeDefinition
inlineShapeAtom = define "InlineShapeAtom" $
  doc "A component of the ShEx InlineShapeAtom production" $
  T.union [
    "seq">: shex "InlineShapeAtomSequence",
    "seq2">: shex "InlineShapeAtomSequence2",
    "sequence3">: shex "ShapeExpression",
    "Period">: T.unit]

inlineShapeAtomSequence :: TypeDefinition
inlineShapeAtomSequence = define "InlineShapeAtomSequence" $
  doc "A synthetic Hydra type representing a sequence within the ShEx InlineShapeAtom production" $
  T.record [
    "NodeConstraint">: shex "NodeConstraint",
    "InlineShapeOrRef">: T.optional (shex "InlineShapeOrRef")]

inlineShapeAtomSequence2 :: TypeDefinition
inlineShapeAtomSequence2 = define "InlineShapeAtomSequence2" $
  doc "A synthetic Hydra type representing a secondary sequence within the ShEx InlineShapeAtom production" $
  T.record [
    "InlineShapeOrRef">: shex "InlineShapeOrRef",
    "NodeConstraint">: T.optional (shex "NodeConstraint")]

-- [31] InlineShapeDefinition
inlineShapeDefinition :: TypeDefinition
inlineShapeDefinition = define "InlineShapeDefinition" $
  doc "A component of the ShEx InlineShapeDefinition production" $
  T.record [
    "listOfAlts">: T.list (shex "InlineShapeDefinitionListOfAltsElmt"),
    "TripleExpression">: T.optional (shex "TripleExpression")]

inlineShapeDefinitionListOfAltsElmt :: TypeDefinition
inlineShapeDefinitionListOfAltsElmt = define "InlineShapeDefinitionListOfAltsElmt" $
  doc "A synthetic Hydra type representing one element of the ShEx InlineShapeDefinitionListOfAlts production" $
  T.union [
    "IncludeSet">: shex "IncludeSet",
    "ExtraPropertySet">: shex "ExtraPropertySet",
    "CLOSED">: T.unit]

-- [11] InlineShapeExpression ::= InlineShapeOr
inlineShapeExpression :: TypeDefinition
inlineShapeExpression = define "InlineShapeExpression" $
  doc "ShEx grammar production: InlineShapeExpression ::= InlineShapeOr" $
  T.wrap $ shex "InlineShapeOr"

-- [17] InlineShapeNot ::= "NOT"? InlineShapeAtom
inlineShapeNot :: TypeDefinition
inlineShapeNot = define "InlineShapeNot" $
  doc "ShEx grammar production: InlineShapeNot ::= 'NOT'? InlineShapeAtom" $
  T.record [
    "NOT">: T.optional T.unit,
    "InlineShapeAtom">: shex "InlineShapeAtom"]

-- [13] InlineShapeOr ::= InlineShapeAnd ("OR" InlineShapeAnd)*
inlineShapeOr :: TypeDefinition
inlineShapeOr = define "InlineShapeOr" $
  doc "ShEx grammar production: InlineShapeOr ::= InlineShapeAnd ('OR' InlineShapeAnd)*" $
  T.record [
    "ShapeAnd">: shex "ShapeAnd",
    "listOfSequence">: T.list (shex "InlineShapeAnd")]

-- [21] InlineShapeOrRef
inlineShapeOrRef :: TypeDefinition
inlineShapeOrRef = define "InlineShapeOrRef" $
  doc "A component of the ShEx InlineShapeOrRef production" $
  T.union [
    "InlineShapeDefinition">: shex "InlineShapeDefinition",
    "AtpNameLn">: shex "AtpNameLn",
    "AtpNameNs">: shex "AtpNameNs",
    "sequence">: shex "ShapeExprLabel"]

-- [36] InnerTripleExpr ::= MultiElementGroup | MultiElementOneOf
innerTripleExpr :: TypeDefinition
innerTripleExpr = define "InnerTripleExpr" $
  doc "ShEx grammar production: InnerTripleExpr ::= MultiElementGroup | MultiElementOneOf" $
  T.union [
    "MultiElementGroup">: shex "MultiElementGroup",
    "MultiElementOneOf">: shex "MultiElementOneOf"]

-- [19t] Integer
integer_ :: TypeDefinition
integer_ = define "Integer" $
  doc "A component of the ShEx Integer production" $
  T.wrap T.string

-- [136s] Iri ::= IriRef | PrefixedName
iri :: TypeDefinition
iri = define "Iri" $
  doc "ShEx grammar production: Iri ::= IriRef | PrefixedName" $
  T.union [
    "IriRef">: shex "IriRef",
    "PrefixedName">: shex "PrefixedName"]

-- [48] IriRange
iriRange :: TypeDefinition
iriRange = define "IriRange" $
  doc "A component of the ShEx IriRange production" $
  T.union [
    "seq">: shex "IriRangeSequence",
    "sequence2">: T.list (shex "Exclusion")]

iriRangeSequence :: TypeDefinition
iriRangeSequence = define "IriRangeSequence" $
  doc "A synthetic Hydra type representing a sequence within the ShEx IriRange production" $
  T.record [
    "Iri">: shex "Iri",
    "Sequence">: T.optional (T.list (shex "Exclusion"))]

-- [18t] IriRef
iriRef :: TypeDefinition
iriRef = define "IriRef" $
  doc "A component of the ShEx IriRef production" $
  T.wrap $ T.list $ shex "IriRefElmt"

iriRefElmt :: TypeDefinition
iriRefElmt = define "IriRefElmt" $
  doc "A synthetic Hydra type representing one element of the ShEx IriRef production" $
  T.union [
    "regex">: T.string,
    "Uchar">: shex "Uchar"]

-- [145s] LangTag
langTag :: TypeDefinition
langTag = define "LangTag" $
  doc "A component of the ShEx LangTag production" $
  T.wrap T.string

-- [13t] Literal ::= RdfLiteral | NumericLiteral | BooleanLiteral
literal :: TypeDefinition
literal = define "Literal" $
  doc "ShEx grammar production: Literal ::= RdfLiteral | NumericLiteral | BooleanLiteral" $
  T.union [
    "RdfLiteral">: shex "RdfLiteral",
    "NumericLiteral">: shex "NumericLiteral",
    "BooleanLiteral">: shex "BooleanLiteral"]

-- [39] MultiElementGroup ::= UnaryTripleExpr (';' UnaryTripleExpr)+ ';'?
multiElementGroup :: TypeDefinition
multiElementGroup = define "MultiElementGroup" $
  doc "ShEx grammar production: MultiElementGroup ::= UnaryTripleExpr (';' UnaryTripleExpr)+ ';'?" $
  T.record [
    "UnaryTripleExpr">: shex "UnaryTripleExpr",
    "listOfSequence">: T.list (shex "UnaryTripleExpr"),
    "Semi">: T.optional T.unit]

-- [35] MultiElementOneOf ::= GroupTripleExpr ('|' GroupTripleExpr)+
multiElementOneOf :: TypeDefinition
multiElementOneOf = define "MultiElementOneOf" $
  doc "ShEx grammar production: MultiElementOneOf ::= GroupTripleExpr ('|' GroupTripleExpr)+" $
  T.record [
    "GroupTripleExpr">: shex "GroupTripleExpr",
    "listOfSequence">: T.list (shex "GroupTripleExpr")]

-- [22] NodeConstraint
nodeConstraint :: TypeDefinition
nodeConstraint = define "NodeConstraint" $
  doc "A component of the ShEx NodeConstraint production" $
  T.union [
    "sequence">: T.list (shex "XsFacet"),
    "seq2">: shex "NodeConstraintSequence2",
    "seq3">: shex "NodeConstraintSequence3",
    "seq4">: shex "NodeConstraintSequence4",
    "seq5">: shex "NodeConstraintSequence5",
    "listOfXsFacet">: T.list (shex "XsFacet")]

nodeConstraintSequence2 :: TypeDefinition
nodeConstraintSequence2 = define "NodeConstraintSequence2" $
  doc "A synthetic Hydra type representing a secondary sequence within the ShEx NodeConstraint production" $
  T.record [
    "NonLiteralKind">: shex "NonLiteralKind",
    "listOfStringFacet">: T.list (shex "StringFacet")]

nodeConstraintSequence3 :: TypeDefinition
nodeConstraintSequence3 = define "NodeConstraintSequence3" $
  doc "A synthetic Hydra type representing a tertiary sequence within the ShEx NodeConstraint production" $
  T.record [
    "Datatype">: shex "Datatype",
    "listOfXsFacet">: T.list (shex "XsFacet")]

nodeConstraintSequence4 :: TypeDefinition
nodeConstraintSequence4 = define "NodeConstraintSequence4" $
  doc "A synthetic Hydra type representing a quaternary sequence within the ShEx NodeConstraint production" $
  T.record [
    "ValueSet">: shex "ValueSet",
    "listOfXsFacet">: T.list (shex "XsFacet")]

nodeConstraintSequence5 :: TypeDefinition
nodeConstraintSequence5 = define "NodeConstraintSequence5" $
  doc "A synthetic Hydra type representing a quinary sequence within the ShEx NodeConstraint production" $
  T.record [
    "ValueSet">: shex "ValueSet",
    "listOfXsFacet">: T.list (shex "XsFacet")]

-- [23] NonLiteralKind ::= "IRI" | "BNODE" | "NONLITERAL"
nonLiteralKind :: TypeDefinition
nonLiteralKind = define "NonLiteralKind" $
  doc "ShEx grammar production: NonLiteralKind ::= 'IRI' | 'BNODE' | 'NONLITERAL'" $
  T.union [
    "IRI">: T.unit,
    "BNODE">: T.unit,
    "NONLITERAL">: T.unit]

-- [5] NotStartAction ::= start | shapeExprDecl
notStartAction :: TypeDefinition
notStartAction = define "NotStartAction" $
  doc "ShEx grammar production: NotStartAction ::= start | shapeExprDecl" $
  T.union [
    "start">: shex "ShapeExpression",
    "declaration">: shex "NotStartActionShapeExprDecl"]

notStartActionShapeExprDecl :: TypeDefinition
notStartActionShapeExprDecl = define "NotStartActionShapeExprDecl" $
  doc "A component of the ShEx NotStartActionShapeExprDecl production" $
  T.record [
    "ShapeExprLabel">: shex "ShapeExprLabel",
    "alts">: shex "NotStartActionShapeExprDeclAlts"]

notStartActionShapeExprDeclAlts :: TypeDefinition
notStartActionShapeExprDeclAlts = define "NotStartActionShapeExprDeclAlts" $
  doc "A synthetic Hydra type representing the alternatives of the ShEx NotStartActionShapeExprDecl production" $
  T.union [
    "ShapeExpression">: shex "ShapeExpression",
    "EXTERNAL">: T.unit]

-- [27] NumericFacet
numericFacet :: TypeDefinition
numericFacet = define "NumericFacet" $
  doc "A component of the ShEx NumericFacet production" $
  T.union [
    "seq">: shex "NumericFacetSequence",
    "seq2">: shex "NumericFacetSequence2"]

numericFacetSequence :: TypeDefinition
numericFacetSequence = define "NumericFacetSequence" $
  doc "A synthetic Hydra type representing a sequence within the ShEx NumericFacet production" $
  T.record [
    "NumericRange">: shex "NumericRange",
    "NumericLiteral">: shex "NumericLiteral"]

numericFacetSequence2 :: TypeDefinition
numericFacetSequence2 = define "NumericFacetSequence2" $
  doc "A synthetic Hydra type representing a secondary sequence within the ShEx NumericFacet production" $
  T.record [
    "NumericLength">: shex "NumericLength",
    "Integer">: shex "Integer"]

-- [29] NumericLength
numericLength :: TypeDefinition
numericLength = define "NumericLength" $
  doc "A component of the ShEx NumericLength production" $
  T.union [
    "TOTALDIGITS">: T.unit,
    "FRACTIONDIGITS">: T.unit]

-- [16t] NumericLiteral ::= Integer | Decimal | Double
numericLiteral :: TypeDefinition
numericLiteral = define "NumericLiteral" $
  doc "ShEx grammar production: NumericLiteral ::= Integer | Decimal | Double" $
  T.union [
    "Integer">: shex "Integer",
    "Decimal">: shex "Decimal",
    "Double">: shex "Double"]

-- [28] NumericRange
numericRange :: TypeDefinition
numericRange = define "NumericRange" $
  doc "A component of the ShEx NumericRange production" $
  T.union [
    "MININCLUSIVE">: T.unit,
    "MINEXCLUSIVE">: T.unit,
    "MAXINCLUSIVE">: T.unit,
    "MAXEXCLUSIVE">: T.unit]

-- [34] OneOfTripleExpr ::= GroupTripleExpr | MultiElementOneOf
oneOfTripleExpr :: TypeDefinition
oneOfTripleExpr = define "OneOfTripleExpr" $
  doc "ShEx grammar production: OneOfTripleExpr ::= GroupTripleExpr | MultiElementOneOf" $
  T.union [
    "GroupTripleExpr">: shex "GroupTripleExpr",
    "MultiElementOneOf">: shex "MultiElementOneOf"]

-- [171s] Percent ::= '%' Hex Hex
percent :: TypeDefinition
percent = define "Percent" $
  doc "ShEx grammar production: Percent ::= '%' Hex Hex" $
  T.record [
    "Hex">: shex "Hex",
    "Hex2">: shex "Hex"]

-- [170s] Plx ::= Percent | PnLocalEsc
plx :: TypeDefinition
plx = define "Plx" $
  doc "ShEx grammar production: Plx ::= Percent | PnLocalEsc" $
  T.union [
    "Percent">: shex "Percent",
    "PnLocalEsc">: shex "PnLocalEsc"]

-- [167s] PnChars
pnChars :: TypeDefinition
pnChars = define "PnChars" $
  doc "A component of the ShEx PnChars production" $
  T.union [
    "PnCharsU">: shex "PnCharsU",
    "Minus">: T.unit,
    "regex">: T.string]

-- [164s] PnCharsBase
pnCharsBase :: TypeDefinition
pnCharsBase = define "PnCharsBase" $
  doc "A component of the ShEx PnCharsBase production" $
  T.union [
    "regex">: T.string,
    "regex2">: T.string]

-- [165s] PnCharsU ::= PnCharsBase | '_'
pnCharsU :: TypeDefinition
pnCharsU = define "PnCharsU" $
  doc "ShEx grammar production: PnCharsU ::= PnCharsBase | '_'" $
  T.union [
    "PnCharsBase">: shex "PnCharsBase",
    "Lowbar">: T.unit]

-- [169s] PnLocal
pnLocal :: TypeDefinition
pnLocal = define "PnLocal" $
  doc "A component of the ShEx PnLocal production" $
  T.record [
    "alts">: shex "PnLocalAlts",
    "Sequence">: T.optional (shex "PnLocalSequenceOption")]

-- [173s] PnLocalEsc
pnLocalEsc :: TypeDefinition
pnLocalEsc = define "PnLocalEsc" $
  doc "A component of the ShEx PnLocalEsc production" $
  T.wrap T.string

pnLocalAlts :: TypeDefinition
pnLocalAlts = define "PnLocalAlts" $
  doc "A synthetic Hydra type representing the alternatives of the ShEx PnLocal production" $
  T.union [
    "PnCharsU">: shex "PnCharsU",
    "Colon">: T.unit,
    "regex">: T.string,
    "Plx">: shex "Plx"]

pnLocalSequenceOption :: TypeDefinition
pnLocalSequenceOption = define "PnLocalSequenceOption" $
  doc "A synthetic Hydra type representing an optional group within the ShEx PnLocal production" $
  T.record [
    "listOfAlts">: T.list (shex "PnLocalSequenceOptionListOfAltsElmt"),
    "alts">: shex "PnLocalSequenceOptionAlts"]

pnLocalSequenceOptionAlts :: TypeDefinition
pnLocalSequenceOptionAlts = define "PnLocalSequenceOptionAlts" $
  doc "A synthetic Hydra type representing the alternatives of an optional group within the ShEx PnLocal production" $
  T.union [
    "PnChars">: shex "PnChars",
    "Colon">: T.unit,
    "Plx">: shex "Plx"]

pnLocalSequenceOptionListOfAltsElmt :: TypeDefinition
pnLocalSequenceOptionListOfAltsElmt = define "PnLocalSequenceOptionListOfAltsElmt" $
  doc "A synthetic Hydra type representing one alternative of an optional repeated group within the ShEx PnLocal production" $
  T.union [
    "PnChars">: shex "PnChars",
    "Period">: T.unit,
    "Colon">: T.unit,
    "Plx">: shex "Plx"]

-- [168s] PnPrefix
pnPrefix :: TypeDefinition
pnPrefix = define "PnPrefix" $
  doc "A component of the ShEx PnPrefix production" $
  T.record [
    "PnCharsBase">: shex "PnCharsBase",
    "Sequence">: T.optional (shex "PnPrefixSequenceOption")]

pnPrefixSequenceOption :: TypeDefinition
pnPrefixSequenceOption = define "PnPrefixSequenceOption" $
  doc "A synthetic Hydra type representing an optional group within the ShEx PnPrefix production" $
  T.record [
    "alts">: shex "PnPrefixSequenceOptionAlts",
    "PnChars">: shex "PnChars"]

pnPrefixSequenceOptionAlts :: TypeDefinition
pnPrefixSequenceOptionAlts = define "PnPrefixSequenceOptionAlts" $
  doc "A synthetic Hydra type representing the alternatives of an optional group within the ShEx PnPrefix production" $
  T.union [
    "PnChars">: shex "PnChars",
    "Period">: T.unit]

-- [141s] PnameLn ::= PnameNs PnLocal
pnameLn :: TypeDefinition
pnameLn = define "PnameLn" $
  doc "ShEx grammar production: PnameLn ::= PnameNs PnLocal" $
  T.record [
    "PnameNs">: shex "PnameNs",
    "PnLocal">: shex "PnLocal"]

-- [140s] PnameNs ::= PnPrefix? ':'
pnameNs :: TypeDefinition
pnameNs = define "PnameNs" $
  doc "ShEx grammar production: PnameNs ::= PnPrefix? ':'" $
  T.wrap $ T.optional $ shex "PnPrefix"

-- [54] Predicate ::= Iri | RdfType
predicate :: TypeDefinition
predicate = define "Predicate" $
  doc "ShEx grammar production: Predicate ::= Iri | RdfType" $
  T.union [
    "Iri">: shex "Iri",
    "RdfType">: shex "RdfType"]

-- [4] PrefixDecl ::= "PREFIX" PnameNs IriRef
prefixDecl :: TypeDefinition
prefixDecl = define "PrefixDecl" $
  doc "ShEx grammar production: PrefixDecl ::= 'PREFIX' PnameNs IriRef" $
  T.record [
    "PnameNs">: shex "PnameNs",
    "IriRef">: shex "IriRef"]

-- [137s] PrefixedName ::= PnameLn | PnameNs
prefixedName :: TypeDefinition
prefixedName = define "PrefixedName" $
  doc "ShEx grammar production: PrefixedName ::= PnameLn | PnameNs" $
  T.union [
    "PnameLn">: shex "PnameLn",
    "PnameNs">: shex "PnameNs"]

-- [129s] RdfLiteral ::= String (LangTag | '^^' Datatype)?
rdfLiteral :: TypeDefinition
rdfLiteral = define "RdfLiteral" $
  doc "ShEx grammar production: RdfLiteral ::= String (LangTag | '^^' Datatype)?" $
  T.record [
    "String">: shex "String",
    "Alts">: T.optional (shex "RdfLiteralAltsOption")]

rdfLiteralAltsOption :: TypeDefinition
rdfLiteralAltsOption = define "RdfLiteralAltsOption" $
  doc "A synthetic Hydra type representing an optional alternative within the ShEx RdfLiteral production" $
  T.union [
    "LangTag">: shex "LangTag",
    "sequence">: shex "Datatype"]

-- [60] RdfType ::= 'a'
rdfType :: TypeDefinition
rdfType = define "RdfType" $
  doc "ShEx grammar production: RdfType ::= 'a'" $
  T.wrap T.unit

-- [63] Regexp
regexp :: TypeDefinition
regexp = define "Regexp" $
  doc "A component of the ShEx Regexp production" $
  T.record [
    "listOfAlts">: T.list (shex "RegexpListOfAltsElmt"),
    "listOfRegex">: T.list T.string]

regexpListOfAltsElmt :: TypeDefinition
regexpListOfAltsElmt = define "RegexpListOfAltsElmt" $
  doc "A synthetic Hydra type representing one element of the ShEx RegexpListOfAlts production" $
  T.union [
    "regex">: T.string,
    "sequence">: T.string,
    "Uchar">: shex "Uchar"]

-- [59] RepeatRange
repeatRange :: TypeDefinition
repeatRange = define "RepeatRange" $
  doc "A component of the ShEx RepeatRange production" $
  T.record [
    "Integer">: shex "Integer",
    "Sequence">: T.optional (T.optional (T.optional (shex "RepeatRangeSequenceOptionOptionOption")))]

repeatRangeSequenceOptionOptionOption :: TypeDefinition
repeatRangeSequenceOptionOptionOption = define "RepeatRangeSequenceOptionOptionOption" $
  doc "A synthetic Hydra type representing a nested optional group within the ShEx RepeatRange production" $
  T.union [
    "Integer">: shex "Integer",
    "Ast">: T.unit]

-- [52] SemanticActions ::= CodeDecl*
semanticActions :: TypeDefinition
semanticActions = define "SemanticActions" $
  doc "ShEx grammar production: SemanticActions ::= CodeDecl*" $
  T.wrap $ T.list $ shex "CodeDecl"

-- [45] SenseFlags ::= '^'
senseFlags :: TypeDefinition
senseFlags = define "SenseFlags" $
  doc "ShEx grammar production: SenseFlags ::= '^'" $
  T.wrap T.unit

-- [14] ShapeAnd ::= ShapeNot ("AND" ShapeNot)*
shapeAnd :: TypeDefinition
shapeAnd = define "ShapeAnd" $
  doc "ShEx grammar production: ShapeAnd ::= ShapeNot ('AND' ShapeNot)*" $
  T.record [
    "ShapeNot">: shex "ShapeNot",
    "listOfSequence">: T.list (shex "ShapeNot")]

-- [18] ShapeAtom
shapeAtom :: TypeDefinition
shapeAtom = define "ShapeAtom" $
  doc "A component of the ShEx ShapeAtom production" $
  T.union [
    "seq">: shex "ShapeAtomSequence",
    "ShapeOrRef">: shex "ShapeOrRef",
    "sequence2">: shex "ShapeExpression",
    "Period">: T.unit]

shapeAtomSequence :: TypeDefinition
shapeAtomSequence = define "ShapeAtomSequence" $
  doc "A synthetic Hydra type representing a sequence within the ShEx ShapeAtom production" $
  T.record [
    "NodeConstraint">: shex "NodeConstraint",
    "ShapeOrRef">: T.optional (shex "ShapeOrRef")]

-- [30] ShapeDefinition
shapeDefinition :: TypeDefinition
shapeDefinition = define "ShapeDefinition" $
  doc "A component of the ShEx ShapeDefinition production" $
  T.record [
    "listOfAlts">: T.list (shex "ShapeDefinitionListOfAltsElmt"),
    "TripleExpression">: T.optional (shex "TripleExpression"),
    "listOfAnnotation">: T.list (shex "Annotation"),
    "SemanticActions">: shex "SemanticActions"]

shapeDefinitionListOfAltsElmt :: TypeDefinition
shapeDefinitionListOfAltsElmt = define "ShapeDefinitionListOfAltsElmt" $
  doc "A synthetic Hydra type representing one element of the ShEx ShapeDefinitionListOfAlts production" $
  T.union [
    "IncludeSet">: shex "IncludeSet",
    "ExtraPropertySet">: shex "ExtraPropertySet",
    "CLOSED">: T.unit]

-- [56] ShapeExprLabel ::= Iri | BlankNode
shapeExprLabel :: TypeDefinition
shapeExprLabel = define "ShapeExprLabel" $
  doc "ShEx grammar production: ShapeExprLabel ::= Iri | BlankNode" $
  T.union [
    "Iri">: shex "Iri",
    "BlankNode">: shex "BlankNode"]

-- [10] ShapeExpression ::= ShapeOr
shapeExpression :: TypeDefinition
shapeExpression = define "ShapeExpression" $
  doc "ShEx grammar production: ShapeExpression ::= ShapeOr" $
  T.wrap $ shex "ShapeOr"

-- [16] ShapeNot ::= "NOT"? ShapeAtom
shapeNot :: TypeDefinition
shapeNot = define "ShapeNot" $
  doc "ShEx grammar production: ShapeNot ::= 'NOT'? ShapeAtom" $
  T.record [
    "NOT">: T.optional T.unit,
    "ShapeAtom">: shex "ShapeAtom"]

-- [12] ShapeOr ::= ShapeAnd ("OR" ShapeAnd)*
shapeOr :: TypeDefinition
shapeOr = define "ShapeOr" $
  doc "ShEx grammar production: ShapeOr ::= ShapeAnd ('OR' ShapeAnd)*" $
  T.record [
    "ShapeAnd">: shex "ShapeAnd",
    "listOfSequence">: T.list (shex "ShapeAnd")]

-- [20] ShapeOrRef
shapeOrRef :: TypeDefinition
shapeOrRef = define "ShapeOrRef" $
  doc "A component of the ShEx ShapeOrRef production" $
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
  doc "ShEx grammar production: ShexDoc ::= Directive* ((NotStartAction | StartActions) Statement*)?" $
  T.record [
    "listOfDirective">: T.list (shex "Directive"),
    "Sequence">: T.optional (shex "ShexDocSequenceOption"),
    "PrefixDecl">: shex "PrefixDecl"]

shexDocSequenceOption :: TypeDefinition
shexDocSequenceOption = define "ShexDocSequenceOption" $
  doc "A synthetic Hydra type representing an optional group within the ShEx ShexDoc production" $
  T.record [
    "alts">: shex "ShexDocSequenceOptionAlts",
    "listOfStatement">: T.list (shex "Statement")]

shexDocSequenceOptionAlts :: TypeDefinition
shexDocSequenceOptionAlts = define "ShexDocSequenceOptionAlts" $
  doc "A synthetic Hydra type representing the alternatives of an optional group within the ShEx ShexDoc production" $
  T.union [
    "NotStartAction">: shex "NotStartAction",
    "StartActions">: shex "StartActions"]

-- [38] SingleElementGroup ::= UnaryTripleExpr ';'?
singleElementGroup :: TypeDefinition
singleElementGroup = define "SingleElementGroup" $
  doc "ShEx grammar production: SingleElementGroup ::= UnaryTripleExpr ';'?" $
  T.record [
    "UnaryTripleExpr">: shex "UnaryTripleExpr",
    "Semi">: T.optional T.unit]

-- [7] StartActions ::= CodeDecl+
startActions :: TypeDefinition
startActions = define "StartActions" $
  doc "ShEx grammar production: StartActions ::= CodeDecl+" $
  T.wrap $ T.list $ shex "CodeDecl"

-- [8] Statement ::= Directive | NotStartAction
statement :: TypeDefinition
statement = define "Statement" $
  doc "ShEx grammar production: Statement ::= Directive | NotStartAction" $
  T.union [
    "Directive">: shex "Directive",
    "NotStartAction">: shex "NotStartAction"]

-- [25] StringFacet ::= StringLength Integer | Regexp
stringFacet :: TypeDefinition
stringFacet = define "StringFacet" $
  doc "ShEx grammar production: StringFacet ::= StringLength Integer | Regexp" $
  T.union [
    "seq">: shex "StringFacetSequence",
    "Regexp">: shex "Regexp"]

stringFacetSequence :: TypeDefinition
stringFacetSequence = define "StringFacetSequence" $
  doc "A synthetic Hydra type representing a sequence within the ShEx StringFacet production" $
  T.record [
    "StringLength">: shex "StringLength",
    "Integer">: shex "Integer"]

-- [26] StringLength ::= "LENGTH" | "MINLENGTH" | "MAXLENGTH"
stringLength :: TypeDefinition
stringLength = define "StringLength" $
  doc "ShEx grammar production: StringLength ::= 'LENGTH' | 'MINLENGTH' | 'MAXLENGTH'" $
  T.union [
    "LENGTH">: T.unit,
    "MINLENGTH">: T.unit,
    "MAXLENGTH">: T.unit]

-- [156s] StringLiteral1
stringLiteral1 :: TypeDefinition
stringLiteral1 = define "StringLiteral1" $
  doc "A component of the ShEx StringLiteral1 production" $
  T.wrap $ T.list $ shex "StringLiteral1Elmt"

stringLiteral1Elmt :: TypeDefinition
stringLiteral1Elmt = define "StringLiteral1Elmt" $
  doc "A synthetic Hydra type representing one element of the ShEx StringLiteral1 production" $
  T.union [
    "regex">: T.string,
    "Echar">: shex "Echar",
    "Uchar">: shex "Uchar"]

-- [157s] StringLiteral2
stringLiteral2 :: TypeDefinition
stringLiteral2 = define "StringLiteral2" $
  doc "A component of the ShEx StringLiteral2 production" $
  T.wrap $ T.list $ shex "StringLiteral2Elmt"

stringLiteral2Elmt :: TypeDefinition
stringLiteral2Elmt = define "StringLiteral2Elmt" $
  doc "A synthetic Hydra type representing one element of the ShEx StringLiteral2 production" $
  T.union [
    "regex">: T.string,
    "Echar">: shex "Echar",
    "Uchar">: shex "Uchar"]

-- [158s] StringLiteralLong1
stringLiteralLong1 :: TypeDefinition
stringLiteralLong1 = define "StringLiteralLong1" $
  doc "A component of the ShEx StringLiteralLong1 production" $
  T.wrap $ T.list $ shex "StringLiteralLong1Elmt"

stringLiteralLong1Elmt :: TypeDefinition
stringLiteralLong1Elmt = define "StringLiteralLong1Elmt" $
  doc "A synthetic Hydra type representing one element of the ShEx StringLiteralLong1 production" $
  T.union [
    "seq">: shex "StringLiteralLong1ElmtSequence",
    "Echar">: shex "Echar",
    "Uchar">: shex "Uchar"]

stringLiteralLong1ElmtSequence :: TypeDefinition
stringLiteralLong1ElmtSequence = define "StringLiteralLong1ElmtSequence" $
  doc "A synthetic Hydra type representing a sequence within an element of the ShEx StringLiteralLong1 production" $
  T.record [
    "Alts">: T.optional (shex "StringLiteralLong1ElmtSequenceAltsOption"),
    "regex">: T.string]

stringLiteralLong1ElmtSequenceAltsOption :: TypeDefinition
stringLiteralLong1ElmtSequenceAltsOption = define "StringLiteralLong1ElmtSequenceAltsOption" $
  doc "A synthetic Hydra type representing an optional alternative of an element of the ShEx StringLiteralLong1 production" $
  T.union [
    "Apos">: T.unit,
    "seq">: shex "StringLiteralLong1ElmtSequenceAltsOptionSequence"]

stringLiteralLong1ElmtSequenceAltsOptionSequence :: TypeDefinition
stringLiteralLong1ElmtSequenceAltsOptionSequence = define "StringLiteralLong1ElmtSequenceAltsOptionSequence" $
  doc "A synthetic Hydra type representing a sequence within an optional alternative of an element of the ShEx StringLiteralLong1 production" $
  T.unit

-- [159s] StringLiteralLong2
stringLiteralLong2 :: TypeDefinition
stringLiteralLong2 = define "StringLiteralLong2" $
  doc "A component of the ShEx StringLiteralLong2 production" $
  T.wrap $ T.list $ shex "StringLiteralLong2Elmt"

stringLiteralLong2Elmt :: TypeDefinition
stringLiteralLong2Elmt = define "StringLiteralLong2Elmt" $
  doc "A synthetic Hydra type representing one element of the ShEx StringLiteralLong2 production" $
  T.union [
    "seq">: shex "StringLiteralLong2ElmtSequence",
    "Echar">: shex "Echar",
    "Uchar">: shex "Uchar"]

stringLiteralLong2ElmtSequence :: TypeDefinition
stringLiteralLong2ElmtSequence = define "StringLiteralLong2ElmtSequence" $
  doc "A synthetic Hydra type representing a sequence within an element of the ShEx StringLiteralLong2 production" $
  T.record [
    "Alts">: T.optional (shex "StringLiteralLong2ElmtSequenceAltsOption"),
    "regex">: T.string]

stringLiteralLong2ElmtSequenceAltsOption :: TypeDefinition
stringLiteralLong2ElmtSequenceAltsOption = define "StringLiteralLong2ElmtSequenceAltsOption" $
  doc "A synthetic Hydra type representing an optional alternative of an element of the ShEx StringLiteralLong2 production" $
  T.union [
    "Quot">: T.unit,
    "seq">: shex "StringLiteralLong2ElmtSequenceAltsOptionSequence"]

stringLiteralLong2ElmtSequenceAltsOptionSequence :: TypeDefinition
stringLiteralLong2ElmtSequenceAltsOptionSequence = define "StringLiteralLong2ElmtSequenceAltsOptionSequence" $
  doc "A synthetic Hydra type representing a sequence within an optional alternative of an element of the ShEx StringLiteralLong2 production" $
  T.unit

-- [135s] String
string_ :: TypeDefinition
string_ = define "String" $
  doc "A component of the ShEx String production" $
  T.union [
    "StringLiteral1">: shex "StringLiteral1",
    "StringLiteralLong1">: shex "StringLiteralLong1",
    "StringLiteral2">: shex "StringLiteral2",
    "StringLiteralLong2">: shex "StringLiteralLong2"]

-- [43] TripleConstraint
tripleConstraint :: TypeDefinition
tripleConstraint = define "TripleConstraint" $
  doc "A component of the ShEx TripleConstraint production" $
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
  doc "ShEx grammar production: TripleExprLabel ::= Iri | BlankNode" $
  T.union [
    "Iri">: shex "Iri",
    "BlankNode">: shex "BlankNode"]

-- [33] TripleExpression ::= OneOfTripleExpr
tripleExpression :: TypeDefinition
tripleExpression = define "TripleExpression" $
  doc "ShEx grammar production: TripleExpression ::= OneOfTripleExpr" $
  T.wrap $ shex "OneOfTripleExpr"

-- [26t] Uchar
uchar :: TypeDefinition
uchar = define "Uchar" $
  doc "A component of the ShEx Uchar production" $
  T.union [
    "seq">: shex "UcharSequence",
    "seq2">: shex "UcharSequence2"]

ucharSequence :: TypeDefinition
ucharSequence = define "UcharSequence" $
  doc "A synthetic Hydra type representing a sequence within the ShEx Uchar production" $
  T.record [
    "Hex">: shex "Hex",
    "Hex2">: shex "Hex",
    "Hex3">: shex "Hex",
    "Hex4">: shex "Hex"]

ucharSequence2 :: TypeDefinition
ucharSequence2 = define "UcharSequence2" $
  doc "A synthetic Hydra type representing a secondary sequence within the ShEx Uchar production" $
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
  doc "A component of the ShEx UnaryTripleExpr production" $
  T.union [
    "seq">: shex "UnaryTripleExprSequence",
    "Include">: shex "Include"]

unaryTripleExprSequence :: TypeDefinition
unaryTripleExprSequence = define "UnaryTripleExprSequence" $
  doc "A synthetic Hydra type representing a sequence within the ShEx UnaryTripleExpr production" $
  T.record [
    "Sequence">: T.optional (shex "TripleExprLabel"),
    "alts">: shex "UnaryTripleExprSequenceAlts"]

unaryTripleExprSequenceAlts :: TypeDefinition
unaryTripleExprSequenceAlts = define "UnaryTripleExprSequenceAlts" $
  doc "A synthetic Hydra type representing the alternatives of the ShEx UnaryTripleExprSequence production" $
  T.union [
    "TripleConstraint">: shex "TripleConstraint",
    "BracketedTripleExpr">: shex "BracketedTripleExpr"]

-- [46] ValueSet ::= '[' ValueSetValue* ']'
valueSet :: TypeDefinition
valueSet = define "ValueSet" $
  doc "ShEx grammar production: ValueSet ::= '[' ValueSetValue* ']'" $
  T.wrap $ T.list $ shex "ValueSetValue"

-- [47] ValueSetValue ::= IriRange | Literal
valueSetValue :: TypeDefinition
valueSetValue = define "ValueSetValue" $
  doc "ShEx grammar production: ValueSetValue ::= IriRange | Literal" $
  T.union [
    "IriRange">: shex "IriRange",
    "Literal">: shex "Literal"]

-- [24] XsFacet ::= StringFacet | NumericFacet
xsFacet :: TypeDefinition
xsFacet = define "XsFacet" $
  doc "ShEx grammar production: XsFacet ::= StringFacet | NumericFacet" $
  T.union [
    "StringFacet">: shex "StringFacet",
    "NumericFacet">: shex "NumericFacet"]
