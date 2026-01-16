-- Note: this is an automatically generated file. Do not edit.

-- | A Shex model. Based on the BNF at:
-- |   https://github.com/shexSpec/grammar/blob/master/bnf

module Hydra.Ext.Io.Shex.Syntax where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

data ShexDoc = 
  ShexDoc {
    shexDocListOfDirective :: [Directive],
    shexDocSequence :: (Maybe ShexDoc_Sequence_Option),
    shexDocPrefixDecl :: PrefixDecl}
  deriving (Eq, Ord, Read, Show)

_ShexDoc = (Core.Name "hydra.ext.io.shex.syntax.ShexDoc")

_ShexDoc_listOfDirective = (Core.Name "listOfDirective")

_ShexDoc_Sequence = (Core.Name "Sequence")

_ShexDoc_PrefixDecl = (Core.Name "PrefixDecl")

data ShexDoc_Sequence_Option = 
  ShexDoc_Sequence_Option {
    shexDoc_Sequence_OptionAlts :: ShexDoc_Sequence_Option_Alts,
    shexDoc_Sequence_OptionListOfStatement :: [Statement]}
  deriving (Eq, Ord, Read, Show)

_ShexDoc_Sequence_Option = (Core.Name "hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option")

_ShexDoc_Sequence_Option_alts = (Core.Name "alts")

_ShexDoc_Sequence_Option_listOfStatement = (Core.Name "listOfStatement")

data ShexDoc_Sequence_Option_Alts = 
  ShexDoc_Sequence_Option_AltsNotStartAction NotStartAction |
  ShexDoc_Sequence_Option_AltsStartActions StartActions
  deriving (Eq, Ord, Read, Show)

_ShexDoc_Sequence_Option_Alts = (Core.Name "hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option_Alts")

_ShexDoc_Sequence_Option_Alts_NotStartAction = (Core.Name "NotStartAction")

_ShexDoc_Sequence_Option_Alts_StartActions = (Core.Name "StartActions")

data Directive = 
  DirectiveBaseDecl BaseDecl |
  DirectivePrefixDecl PrefixDecl
  deriving (Eq, Ord, Read, Show)

_Directive = (Core.Name "hydra.ext.io.shex.syntax.Directive")

_Directive_BaseDecl = (Core.Name "BaseDecl")

_Directive_PrefixDecl = (Core.Name "PrefixDecl")

newtype BaseDecl = 
  BaseDecl {
    unBaseDecl :: IriRef}
  deriving (Eq, Ord, Read, Show)

_BaseDecl = (Core.Name "hydra.ext.io.shex.syntax.BaseDecl")

data PrefixDecl = 
  PrefixDecl {
    prefixDeclPnameNs :: PnameNs,
    prefixDeclIriRef :: IriRef}
  deriving (Eq, Ord, Read, Show)

_PrefixDecl = (Core.Name "hydra.ext.io.shex.syntax.PrefixDecl")

_PrefixDecl_PnameNs = (Core.Name "PnameNs")

_PrefixDecl_IriRef = (Core.Name "IriRef")

data NotStartAction = 
  NotStartActionStart ShapeExpression |
  NotStartActionShapeExprDecl NotStartAction_ShapeExprDecl
  deriving (Eq, Ord, Read, Show)

_NotStartAction = (Core.Name "hydra.ext.io.shex.syntax.NotStartAction")

_NotStartAction_start = (Core.Name "start")

_NotStartAction_shapeExprDecl = (Core.Name "shapeExprDecl")

data NotStartAction_ShapeExprDecl = 
  NotStartAction_ShapeExprDecl {
    notStartAction_ShapeExprDeclShapeExprLabel :: ShapeExprLabel,
    notStartAction_ShapeExprDeclAlts :: NotStartAction_ShapeExprDecl_Alts}
  deriving (Eq, Ord, Read, Show)

_NotStartAction_ShapeExprDecl = (Core.Name "hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl")

_NotStartAction_ShapeExprDecl_ShapeExprLabel = (Core.Name "ShapeExprLabel")

_NotStartAction_ShapeExprDecl_alts = (Core.Name "alts")

data NotStartAction_ShapeExprDecl_Alts = 
  NotStartAction_ShapeExprDecl_AltsShapeExpression ShapeExpression |
  NotStartAction_ShapeExprDecl_AltsEXTERNAL 
  deriving (Eq, Ord, Read, Show)

_NotStartAction_ShapeExprDecl_Alts = (Core.Name "hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl_Alts")

_NotStartAction_ShapeExprDecl_Alts_ShapeExpression = (Core.Name "ShapeExpression")

_NotStartAction_ShapeExprDecl_Alts_EXTERNAL = (Core.Name "EXTERNAL")

newtype StartActions = 
  StartActions {
    unStartActions :: [CodeDecl]}
  deriving (Eq, Ord, Read, Show)

_StartActions = (Core.Name "hydra.ext.io.shex.syntax.StartActions")

data Statement = 
  StatementDirective Directive |
  StatementNotStartAction NotStartAction
  deriving (Eq, Ord, Read, Show)

_Statement = (Core.Name "hydra.ext.io.shex.syntax.Statement")

_Statement_Directive = (Core.Name "Directive")

_Statement_NotStartAction = (Core.Name "NotStartAction")

newtype ShapeExpression = 
  ShapeExpression {
    unShapeExpression :: ShapeOr}
  deriving (Eq, Ord, Read, Show)

_ShapeExpression = (Core.Name "hydra.ext.io.shex.syntax.ShapeExpression")

newtype InlineShapeExpression = 
  InlineShapeExpression {
    unInlineShapeExpression :: InlineShapeOr}
  deriving (Eq, Ord, Read, Show)

_InlineShapeExpression = (Core.Name "hydra.ext.io.shex.syntax.InlineShapeExpression")

data ShapeOr = 
  ShapeOr {
    shapeOrShapeAnd :: ShapeAnd,
    shapeOrListOfSequence :: [ShapeAnd]}
  deriving (Eq, Ord, Read, Show)

_ShapeOr = (Core.Name "hydra.ext.io.shex.syntax.ShapeOr")

_ShapeOr_ShapeAnd = (Core.Name "ShapeAnd")

_ShapeOr_listOfSequence = (Core.Name "listOfSequence")

data InlineShapeOr = 
  InlineShapeOr {
    inlineShapeOrShapeAnd :: ShapeAnd,
    inlineShapeOrListOfSequence :: [InlineShapeAnd]}
  deriving (Eq, Ord, Read, Show)

_InlineShapeOr = (Core.Name "hydra.ext.io.shex.syntax.InlineShapeOr")

_InlineShapeOr_ShapeAnd = (Core.Name "ShapeAnd")

_InlineShapeOr_listOfSequence = (Core.Name "listOfSequence")

data ShapeAnd = 
  ShapeAnd {
    shapeAndShapeNot :: ShapeNot,
    shapeAndListOfSequence :: [ShapeNot]}
  deriving (Eq, Ord, Read, Show)

_ShapeAnd = (Core.Name "hydra.ext.io.shex.syntax.ShapeAnd")

_ShapeAnd_ShapeNot = (Core.Name "ShapeNot")

_ShapeAnd_listOfSequence = (Core.Name "listOfSequence")

data InlineShapeAnd = 
  InlineShapeAnd {
    inlineShapeAndInlineShapeNot :: InlineShapeNot,
    inlineShapeAndListOfSequence :: [InlineShapeNot]}
  deriving (Eq, Ord, Read, Show)

_InlineShapeAnd = (Core.Name "hydra.ext.io.shex.syntax.InlineShapeAnd")

_InlineShapeAnd_InlineShapeNot = (Core.Name "InlineShapeNot")

_InlineShapeAnd_listOfSequence = (Core.Name "listOfSequence")

data ShapeNot = 
  ShapeNot {
    shapeNotNOT :: (Maybe ()),
    shapeNotShapeAtom :: ShapeAtom}
  deriving (Eq, Ord, Read, Show)

_ShapeNot = (Core.Name "hydra.ext.io.shex.syntax.ShapeNot")

_ShapeNot_NOT = (Core.Name "NOT")

_ShapeNot_ShapeAtom = (Core.Name "ShapeAtom")

data InlineShapeNot = 
  InlineShapeNot {
    inlineShapeNotNOT :: (Maybe ()),
    inlineShapeNotInlineShapeAtom :: InlineShapeAtom}
  deriving (Eq, Ord, Read, Show)

_InlineShapeNot = (Core.Name "hydra.ext.io.shex.syntax.InlineShapeNot")

_InlineShapeNot_NOT = (Core.Name "NOT")

_InlineShapeNot_InlineShapeAtom = (Core.Name "InlineShapeAtom")

data ShapeAtom = 
  ShapeAtomSequence ShapeAtom_Sequence |
  ShapeAtomShapeOrRef ShapeOrRef |
  ShapeAtomSequence2 ShapeExpression |
  ShapeAtomPeriod 
  deriving (Eq, Ord, Read, Show)

_ShapeAtom = (Core.Name "hydra.ext.io.shex.syntax.ShapeAtom")

_ShapeAtom_sequence = (Core.Name "sequence")

_ShapeAtom_ShapeOrRef = (Core.Name "ShapeOrRef")

_ShapeAtom_sequence2 = (Core.Name "sequence2")

_ShapeAtom_Period = (Core.Name "Period")

data ShapeAtom_Sequence = 
  ShapeAtom_Sequence {
    shapeAtom_SequenceNodeConstraint :: NodeConstraint,
    shapeAtom_SequenceShapeOrRef :: (Maybe ShapeOrRef)}
  deriving (Eq, Ord, Read, Show)

_ShapeAtom_Sequence = (Core.Name "hydra.ext.io.shex.syntax.ShapeAtom_Sequence")

_ShapeAtom_Sequence_NodeConstraint = (Core.Name "NodeConstraint")

_ShapeAtom_Sequence_ShapeOrRef = (Core.Name "ShapeOrRef")

data InlineShapeAtom = 
  InlineShapeAtomSequence InlineShapeAtom_Sequence |
  InlineShapeAtomSequence2 InlineShapeAtom_Sequence2 |
  InlineShapeAtomSequence3 ShapeExpression |
  InlineShapeAtomPeriod 
  deriving (Eq, Ord, Read, Show)

_InlineShapeAtom = (Core.Name "hydra.ext.io.shex.syntax.InlineShapeAtom")

_InlineShapeAtom_sequence = (Core.Name "sequence")

_InlineShapeAtom_sequence2 = (Core.Name "sequence2")

_InlineShapeAtom_sequence3 = (Core.Name "sequence3")

_InlineShapeAtom_Period = (Core.Name "Period")

data InlineShapeAtom_Sequence = 
  InlineShapeAtom_Sequence {
    inlineShapeAtom_SequenceNodeConstraint :: NodeConstraint,
    inlineShapeAtom_SequenceInlineShapeOrRef :: (Maybe InlineShapeOrRef)}
  deriving (Eq, Ord, Read, Show)

_InlineShapeAtom_Sequence = (Core.Name "hydra.ext.io.shex.syntax.InlineShapeAtom_Sequence")

_InlineShapeAtom_Sequence_NodeConstraint = (Core.Name "NodeConstraint")

_InlineShapeAtom_Sequence_InlineShapeOrRef = (Core.Name "InlineShapeOrRef")

data InlineShapeAtom_Sequence2 = 
  InlineShapeAtom_Sequence2 {
    inlineShapeAtom_Sequence2InlineShapeOrRef :: InlineShapeOrRef,
    inlineShapeAtom_Sequence2NodeConstraint :: (Maybe NodeConstraint)}
  deriving (Eq, Ord, Read, Show)

_InlineShapeAtom_Sequence2 = (Core.Name "hydra.ext.io.shex.syntax.InlineShapeAtom_Sequence2")

_InlineShapeAtom_Sequence2_InlineShapeOrRef = (Core.Name "InlineShapeOrRef")

_InlineShapeAtom_Sequence2_NodeConstraint = (Core.Name "NodeConstraint")

data ShapeOrRef = 
  ShapeOrRefShapeDefinition ShapeDefinition |
  ShapeOrRefAtpNameLn AtpNameLn |
  ShapeOrRefAtpNameNs AtpNameNs |
  ShapeOrRefSequence ShapeExprLabel
  deriving (Eq, Ord, Read, Show)

_ShapeOrRef = (Core.Name "hydra.ext.io.shex.syntax.ShapeOrRef")

_ShapeOrRef_ShapeDefinition = (Core.Name "ShapeDefinition")

_ShapeOrRef_AtpNameLn = (Core.Name "AtpNameLn")

_ShapeOrRef_AtpNameNs = (Core.Name "AtpNameNs")

_ShapeOrRef_sequence = (Core.Name "sequence")

data InlineShapeOrRef = 
  InlineShapeOrRefInlineShapeDefinition InlineShapeDefinition |
  InlineShapeOrRefAtpNameLn AtpNameLn |
  InlineShapeOrRefAtpNameNs AtpNameNs |
  InlineShapeOrRefSequence ShapeExprLabel
  deriving (Eq, Ord, Read, Show)

_InlineShapeOrRef = (Core.Name "hydra.ext.io.shex.syntax.InlineShapeOrRef")

_InlineShapeOrRef_InlineShapeDefinition = (Core.Name "InlineShapeDefinition")

_InlineShapeOrRef_AtpNameLn = (Core.Name "AtpNameLn")

_InlineShapeOrRef_AtpNameNs = (Core.Name "AtpNameNs")

_InlineShapeOrRef_sequence = (Core.Name "sequence")

data NodeConstraint = 
  NodeConstraintSequence [XsFacet] |
  NodeConstraintSequence2 NodeConstraint_Sequence2 |
  NodeConstraintSequence3 NodeConstraint_Sequence3 |
  NodeConstraintSequence4 NodeConstraint_Sequence4 |
  NodeConstraintSequence5 NodeConstraint_Sequence5 |
  NodeConstraintListOfXsFacet [XsFacet]
  deriving (Eq, Ord, Read, Show)

_NodeConstraint = (Core.Name "hydra.ext.io.shex.syntax.NodeConstraint")

_NodeConstraint_sequence = (Core.Name "sequence")

_NodeConstraint_sequence2 = (Core.Name "sequence2")

_NodeConstraint_sequence3 = (Core.Name "sequence3")

_NodeConstraint_sequence4 = (Core.Name "sequence4")

_NodeConstraint_sequence5 = (Core.Name "sequence5")

_NodeConstraint_listOfXsFacet = (Core.Name "listOfXsFacet")

data NodeConstraint_Sequence2 = 
  NodeConstraint_Sequence2 {
    nodeConstraint_Sequence2NonLiteralKind :: NonLiteralKind,
    nodeConstraint_Sequence2ListOfStringFacet :: [StringFacet]}
  deriving (Eq, Ord, Read, Show)

_NodeConstraint_Sequence2 = (Core.Name "hydra.ext.io.shex.syntax.NodeConstraint_Sequence2")

_NodeConstraint_Sequence2_NonLiteralKind = (Core.Name "NonLiteralKind")

_NodeConstraint_Sequence2_listOfStringFacet = (Core.Name "listOfStringFacet")

data NodeConstraint_Sequence3 = 
  NodeConstraint_Sequence3 {
    nodeConstraint_Sequence3Datatype :: Datatype,
    nodeConstraint_Sequence3ListOfXsFacet :: [XsFacet]}
  deriving (Eq, Ord, Read, Show)

_NodeConstraint_Sequence3 = (Core.Name "hydra.ext.io.shex.syntax.NodeConstraint_Sequence3")

_NodeConstraint_Sequence3_Datatype = (Core.Name "Datatype")

_NodeConstraint_Sequence3_listOfXsFacet = (Core.Name "listOfXsFacet")

data NodeConstraint_Sequence4 = 
  NodeConstraint_Sequence4 {
    nodeConstraint_Sequence4ValueSet :: ValueSet,
    nodeConstraint_Sequence4ListOfXsFacet :: [XsFacet]}
  deriving (Eq, Ord, Read, Show)

_NodeConstraint_Sequence4 = (Core.Name "hydra.ext.io.shex.syntax.NodeConstraint_Sequence4")

_NodeConstraint_Sequence4_ValueSet = (Core.Name "ValueSet")

_NodeConstraint_Sequence4_listOfXsFacet = (Core.Name "listOfXsFacet")

data NodeConstraint_Sequence5 = 
  NodeConstraint_Sequence5 {
    nodeConstraint_Sequence5ValueSet :: ValueSet,
    nodeConstraint_Sequence5ListOfXsFacet :: [XsFacet]}
  deriving (Eq, Ord, Read, Show)

_NodeConstraint_Sequence5 = (Core.Name "hydra.ext.io.shex.syntax.NodeConstraint_Sequence5")

_NodeConstraint_Sequence5_ValueSet = (Core.Name "ValueSet")

_NodeConstraint_Sequence5_listOfXsFacet = (Core.Name "listOfXsFacet")

data NonLiteralKind = 
  NonLiteralKindIRI  |
  NonLiteralKindBNODE  |
  NonLiteralKindNONLITERAL 
  deriving (Eq, Ord, Read, Show)

_NonLiteralKind = (Core.Name "hydra.ext.io.shex.syntax.NonLiteralKind")

_NonLiteralKind_IRI = (Core.Name "IRI")

_NonLiteralKind_BNODE = (Core.Name "BNODE")

_NonLiteralKind_NONLITERAL = (Core.Name "NONLITERAL")

data XsFacet = 
  XsFacetStringFacet StringFacet |
  XsFacetNumericFacet NumericFacet
  deriving (Eq, Ord, Read, Show)

_XsFacet = (Core.Name "hydra.ext.io.shex.syntax.XsFacet")

_XsFacet_StringFacet = (Core.Name "StringFacet")

_XsFacet_NumericFacet = (Core.Name "NumericFacet")

data StringFacet = 
  StringFacetSequence StringFacet_Sequence |
  StringFacetRegexp Regexp
  deriving (Eq, Ord, Read, Show)

_StringFacet = (Core.Name "hydra.ext.io.shex.syntax.StringFacet")

_StringFacet_sequence = (Core.Name "sequence")

_StringFacet_Regexp = (Core.Name "Regexp")

data StringFacet_Sequence = 
  StringFacet_Sequence {
    stringFacet_SequenceStringLength :: StringLength,
    stringFacet_SequenceInteger :: Integer_}
  deriving (Eq, Ord, Read, Show)

_StringFacet_Sequence = (Core.Name "hydra.ext.io.shex.syntax.StringFacet_Sequence")

_StringFacet_Sequence_StringLength = (Core.Name "StringLength")

_StringFacet_Sequence_Integer = (Core.Name "Integer")

data StringLength = 
  StringLengthLENGTH  |
  StringLengthMINLENGTH  |
  StringLengthMAXLENGTH 
  deriving (Eq, Ord, Read, Show)

_StringLength = (Core.Name "hydra.ext.io.shex.syntax.StringLength")

_StringLength_LENGTH = (Core.Name "LENGTH")

_StringLength_MINLENGTH = (Core.Name "MINLENGTH")

_StringLength_MAXLENGTH = (Core.Name "MAXLENGTH")

data NumericFacet = 
  NumericFacetSequence NumericFacet_Sequence |
  NumericFacetSequence2 NumericFacet_Sequence2
  deriving (Eq, Ord, Read, Show)

_NumericFacet = (Core.Name "hydra.ext.io.shex.syntax.NumericFacet")

_NumericFacet_sequence = (Core.Name "sequence")

_NumericFacet_sequence2 = (Core.Name "sequence2")

data NumericFacet_Sequence = 
  NumericFacet_Sequence {
    numericFacet_SequenceNumericRange :: NumericRange,
    numericFacet_SequenceNumericLiteral :: NumericLiteral}
  deriving (Eq, Ord, Read, Show)

_NumericFacet_Sequence = (Core.Name "hydra.ext.io.shex.syntax.NumericFacet_Sequence")

_NumericFacet_Sequence_NumericRange = (Core.Name "NumericRange")

_NumericFacet_Sequence_NumericLiteral = (Core.Name "NumericLiteral")

data NumericFacet_Sequence2 = 
  NumericFacet_Sequence2 {
    numericFacet_Sequence2NumericLength :: NumericLength,
    numericFacet_Sequence2Integer :: Integer_}
  deriving (Eq, Ord, Read, Show)

_NumericFacet_Sequence2 = (Core.Name "hydra.ext.io.shex.syntax.NumericFacet_Sequence2")

_NumericFacet_Sequence2_NumericLength = (Core.Name "NumericLength")

_NumericFacet_Sequence2_Integer = (Core.Name "Integer")

data NumericRange = 
  NumericRangeMININCLUSIVE  |
  NumericRangeMINEXCLUSIVE  |
  NumericRangeMAXINCLUSIVE  |
  NumericRangeMAXEXCLUSIVE 
  deriving (Eq, Ord, Read, Show)

_NumericRange = (Core.Name "hydra.ext.io.shex.syntax.NumericRange")

_NumericRange_MININCLUSIVE = (Core.Name "MININCLUSIVE")

_NumericRange_MINEXCLUSIVE = (Core.Name "MINEXCLUSIVE")

_NumericRange_MAXINCLUSIVE = (Core.Name "MAXINCLUSIVE")

_NumericRange_MAXEXCLUSIVE = (Core.Name "MAXEXCLUSIVE")

data NumericLength = 
  NumericLengthTOTALDIGITS  |
  NumericLengthFRACTIONDIGITS 
  deriving (Eq, Ord, Read, Show)

_NumericLength = (Core.Name "hydra.ext.io.shex.syntax.NumericLength")

_NumericLength_TOTALDIGITS = (Core.Name "TOTALDIGITS")

_NumericLength_FRACTIONDIGITS = (Core.Name "FRACTIONDIGITS")

data ShapeDefinition = 
  ShapeDefinition {
    shapeDefinitionListOfAlts :: [ShapeDefinition_ListOfAlts_Elmt],
    shapeDefinitionTripleExpression :: (Maybe TripleExpression),
    shapeDefinitionListOfAnnotation :: [Annotation],
    shapeDefinitionSemanticActions :: SemanticActions}
  deriving (Eq, Ord, Read, Show)

_ShapeDefinition = (Core.Name "hydra.ext.io.shex.syntax.ShapeDefinition")

_ShapeDefinition_listOfAlts = (Core.Name "listOfAlts")

_ShapeDefinition_TripleExpression = (Core.Name "TripleExpression")

_ShapeDefinition_listOfAnnotation = (Core.Name "listOfAnnotation")

_ShapeDefinition_SemanticActions = (Core.Name "SemanticActions")

data ShapeDefinition_ListOfAlts_Elmt = 
  ShapeDefinition_ListOfAlts_ElmtIncludeSet IncludeSet |
  ShapeDefinition_ListOfAlts_ElmtExtraPropertySet ExtraPropertySet |
  ShapeDefinition_ListOfAlts_ElmtCLOSED 
  deriving (Eq, Ord, Read, Show)

_ShapeDefinition_ListOfAlts_Elmt = (Core.Name "hydra.ext.io.shex.syntax.ShapeDefinition_ListOfAlts_Elmt")

_ShapeDefinition_ListOfAlts_Elmt_IncludeSet = (Core.Name "IncludeSet")

_ShapeDefinition_ListOfAlts_Elmt_ExtraPropertySet = (Core.Name "ExtraPropertySet")

_ShapeDefinition_ListOfAlts_Elmt_CLOSED = (Core.Name "CLOSED")

data InlineShapeDefinition = 
  InlineShapeDefinition {
    inlineShapeDefinitionListOfAlts :: [InlineShapeDefinition_ListOfAlts_Elmt],
    inlineShapeDefinitionTripleExpression :: (Maybe TripleExpression)}
  deriving (Eq, Ord, Read, Show)

_InlineShapeDefinition = (Core.Name "hydra.ext.io.shex.syntax.InlineShapeDefinition")

_InlineShapeDefinition_listOfAlts = (Core.Name "listOfAlts")

_InlineShapeDefinition_TripleExpression = (Core.Name "TripleExpression")

data InlineShapeDefinition_ListOfAlts_Elmt = 
  InlineShapeDefinition_ListOfAlts_ElmtIncludeSet IncludeSet |
  InlineShapeDefinition_ListOfAlts_ElmtExtraPropertySet ExtraPropertySet |
  InlineShapeDefinition_ListOfAlts_ElmtCLOSED 
  deriving (Eq, Ord, Read, Show)

_InlineShapeDefinition_ListOfAlts_Elmt = (Core.Name "hydra.ext.io.shex.syntax.InlineShapeDefinition_ListOfAlts_Elmt")

_InlineShapeDefinition_ListOfAlts_Elmt_IncludeSet = (Core.Name "IncludeSet")

_InlineShapeDefinition_ListOfAlts_Elmt_ExtraPropertySet = (Core.Name "ExtraPropertySet")

_InlineShapeDefinition_ListOfAlts_Elmt_CLOSED = (Core.Name "CLOSED")

newtype ExtraPropertySet = 
  ExtraPropertySet {
    unExtraPropertySet :: [Predicate]}
  deriving (Eq, Ord, Read, Show)

_ExtraPropertySet = (Core.Name "hydra.ext.io.shex.syntax.ExtraPropertySet")

newtype TripleExpression = 
  TripleExpression {
    unTripleExpression :: OneOfTripleExpr}
  deriving (Eq, Ord, Read, Show)

_TripleExpression = (Core.Name "hydra.ext.io.shex.syntax.TripleExpression")

data OneOfTripleExpr = 
  OneOfTripleExprGroupTripleExpr GroupTripleExpr |
  OneOfTripleExprMultiElementOneOf MultiElementOneOf
  deriving (Eq, Ord, Read, Show)

_OneOfTripleExpr = (Core.Name "hydra.ext.io.shex.syntax.OneOfTripleExpr")

_OneOfTripleExpr_GroupTripleExpr = (Core.Name "GroupTripleExpr")

_OneOfTripleExpr_MultiElementOneOf = (Core.Name "MultiElementOneOf")

data MultiElementOneOf = 
  MultiElementOneOf {
    multiElementOneOfGroupTripleExpr :: GroupTripleExpr,
    multiElementOneOfListOfSequence :: [GroupTripleExpr]}
  deriving (Eq, Ord, Read, Show)

_MultiElementOneOf = (Core.Name "hydra.ext.io.shex.syntax.MultiElementOneOf")

_MultiElementOneOf_GroupTripleExpr = (Core.Name "GroupTripleExpr")

_MultiElementOneOf_listOfSequence = (Core.Name "listOfSequence")

data InnerTripleExpr = 
  InnerTripleExprMultiElementGroup MultiElementGroup |
  InnerTripleExprMultiElementOneOf MultiElementOneOf
  deriving (Eq, Ord, Read, Show)

_InnerTripleExpr = (Core.Name "hydra.ext.io.shex.syntax.InnerTripleExpr")

_InnerTripleExpr_MultiElementGroup = (Core.Name "MultiElementGroup")

_InnerTripleExpr_MultiElementOneOf = (Core.Name "MultiElementOneOf")

data GroupTripleExpr = 
  GroupTripleExprSingleElementGroup SingleElementGroup |
  GroupTripleExprMultiElementGroup MultiElementGroup
  deriving (Eq, Ord, Read, Show)

_GroupTripleExpr = (Core.Name "hydra.ext.io.shex.syntax.GroupTripleExpr")

_GroupTripleExpr_SingleElementGroup = (Core.Name "SingleElementGroup")

_GroupTripleExpr_MultiElementGroup = (Core.Name "MultiElementGroup")

data SingleElementGroup = 
  SingleElementGroup {
    singleElementGroupUnaryTripleExpr :: UnaryTripleExpr,
    singleElementGroupSemi :: (Maybe ())}
  deriving (Eq, Ord, Read, Show)

_SingleElementGroup = (Core.Name "hydra.ext.io.shex.syntax.SingleElementGroup")

_SingleElementGroup_UnaryTripleExpr = (Core.Name "UnaryTripleExpr")

_SingleElementGroup_Semi = (Core.Name "Semi")

data MultiElementGroup = 
  MultiElementGroup {
    multiElementGroupUnaryTripleExpr :: UnaryTripleExpr,
    multiElementGroupListOfSequence :: [UnaryTripleExpr],
    multiElementGroupSemi :: (Maybe ())}
  deriving (Eq, Ord, Read, Show)

_MultiElementGroup = (Core.Name "hydra.ext.io.shex.syntax.MultiElementGroup")

_MultiElementGroup_UnaryTripleExpr = (Core.Name "UnaryTripleExpr")

_MultiElementGroup_listOfSequence = (Core.Name "listOfSequence")

_MultiElementGroup_Semi = (Core.Name "Semi")

data UnaryTripleExpr = 
  UnaryTripleExprSequence UnaryTripleExpr_Sequence |
  UnaryTripleExprInclude Include
  deriving (Eq, Ord, Read, Show)

_UnaryTripleExpr = (Core.Name "hydra.ext.io.shex.syntax.UnaryTripleExpr")

_UnaryTripleExpr_sequence = (Core.Name "sequence")

_UnaryTripleExpr_Include = (Core.Name "Include")

data UnaryTripleExpr_Sequence = 
  UnaryTripleExpr_Sequence {
    unaryTripleExpr_SequenceSequence :: (Maybe TripleExprLabel),
    unaryTripleExpr_SequenceAlts :: UnaryTripleExpr_Sequence_Alts}
  deriving (Eq, Ord, Read, Show)

_UnaryTripleExpr_Sequence = (Core.Name "hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence")

_UnaryTripleExpr_Sequence_Sequence = (Core.Name "Sequence")

_UnaryTripleExpr_Sequence_alts = (Core.Name "alts")

data UnaryTripleExpr_Sequence_Alts = 
  UnaryTripleExpr_Sequence_AltsTripleConstraint TripleConstraint |
  UnaryTripleExpr_Sequence_AltsBracketedTripleExpr BracketedTripleExpr
  deriving (Eq, Ord, Read, Show)

_UnaryTripleExpr_Sequence_Alts = (Core.Name "hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence_Alts")

_UnaryTripleExpr_Sequence_Alts_TripleConstraint = (Core.Name "TripleConstraint")

_UnaryTripleExpr_Sequence_Alts_BracketedTripleExpr = (Core.Name "BracketedTripleExpr")

data BracketedTripleExpr = 
  BracketedTripleExpr {
    bracketedTripleExprInnerTripleExpr :: InnerTripleExpr,
    bracketedTripleExprCardinality :: (Maybe Cardinality),
    bracketedTripleExprListOfAnnotation :: [Annotation],
    bracketedTripleExprSemanticActions :: SemanticActions}
  deriving (Eq, Ord, Read, Show)

_BracketedTripleExpr = (Core.Name "hydra.ext.io.shex.syntax.BracketedTripleExpr")

_BracketedTripleExpr_InnerTripleExpr = (Core.Name "InnerTripleExpr")

_BracketedTripleExpr_Cardinality = (Core.Name "Cardinality")

_BracketedTripleExpr_listOfAnnotation = (Core.Name "listOfAnnotation")

_BracketedTripleExpr_SemanticActions = (Core.Name "SemanticActions")

data TripleConstraint = 
  TripleConstraint {
    tripleConstraintSenseFlags :: (Maybe SenseFlags),
    tripleConstraintPredicate :: Predicate,
    tripleConstraintInlineShapeExpression :: InlineShapeExpression,
    tripleConstraintCardinality :: (Maybe Cardinality),
    tripleConstraintListOfAnnotation :: [Annotation],
    tripleConstraintSemanticActions :: SemanticActions}
  deriving (Eq, Ord, Read, Show)

_TripleConstraint = (Core.Name "hydra.ext.io.shex.syntax.TripleConstraint")

_TripleConstraint_SenseFlags = (Core.Name "SenseFlags")

_TripleConstraint_Predicate = (Core.Name "Predicate")

_TripleConstraint_InlineShapeExpression = (Core.Name "InlineShapeExpression")

_TripleConstraint_Cardinality = (Core.Name "Cardinality")

_TripleConstraint_listOfAnnotation = (Core.Name "listOfAnnotation")

_TripleConstraint_SemanticActions = (Core.Name "SemanticActions")

data Cardinality = 
  CardinalityAst  |
  CardinalityPlus  |
  CardinalityQuest  |
  CardinalityRepeatRange RepeatRange
  deriving (Eq, Ord, Read, Show)

_Cardinality = (Core.Name "hydra.ext.io.shex.syntax.Cardinality")

_Cardinality_Ast = (Core.Name "Ast")

_Cardinality_Plus = (Core.Name "Plus")

_Cardinality_Quest = (Core.Name "Quest")

_Cardinality_RepeatRange = (Core.Name "RepeatRange")

newtype SenseFlags = 
  SenseFlags {
    unSenseFlags :: ()}
  deriving (Eq, Ord, Read, Show)

_SenseFlags = (Core.Name "hydra.ext.io.shex.syntax.SenseFlags")

newtype ValueSet = 
  ValueSet {
    unValueSet :: [ValueSetValue]}
  deriving (Eq, Ord, Read, Show)

_ValueSet = (Core.Name "hydra.ext.io.shex.syntax.ValueSet")

data ValueSetValue = 
  ValueSetValueIriRange IriRange |
  ValueSetValueLiteral Literal
  deriving (Eq, Ord, Read, Show)

_ValueSetValue = (Core.Name "hydra.ext.io.shex.syntax.ValueSetValue")

_ValueSetValue_IriRange = (Core.Name "IriRange")

_ValueSetValue_Literal = (Core.Name "Literal")

data IriRange = 
  IriRangeSequence IriRange_Sequence |
  IriRangeSequence2 [Exclusion]
  deriving (Eq, Ord, Read, Show)

_IriRange = (Core.Name "hydra.ext.io.shex.syntax.IriRange")

_IriRange_sequence = (Core.Name "sequence")

_IriRange_sequence2 = (Core.Name "sequence2")

data IriRange_Sequence = 
  IriRange_Sequence {
    iriRange_SequenceIri :: Iri,
    iriRange_SequenceSequence :: (Maybe [Exclusion])}
  deriving (Eq, Ord, Read, Show)

_IriRange_Sequence = (Core.Name "hydra.ext.io.shex.syntax.IriRange_Sequence")

_IriRange_Sequence_Iri = (Core.Name "Iri")

_IriRange_Sequence_Sequence = (Core.Name "Sequence")

newtype Exclusion = 
  Exclusion {
    unExclusion :: Iri}
  deriving (Eq, Ord, Read, Show)

_Exclusion = (Core.Name "hydra.ext.io.shex.syntax.Exclusion")

newtype Include = 
  Include {
    unInclude :: TripleExprLabel}
  deriving (Eq, Ord, Read, Show)

_Include = (Core.Name "hydra.ext.io.shex.syntax.Include")

data Annotation = 
  Annotation {
    annotationPredicate :: Predicate,
    annotationAlts :: Annotation_Alts}
  deriving (Eq, Ord, Read, Show)

_Annotation = (Core.Name "hydra.ext.io.shex.syntax.Annotation")

_Annotation_Predicate = (Core.Name "Predicate")

_Annotation_alts = (Core.Name "alts")

data Annotation_Alts = 
  Annotation_AltsIri Iri |
  Annotation_AltsLiteral Literal
  deriving (Eq, Ord, Read, Show)

_Annotation_Alts = (Core.Name "hydra.ext.io.shex.syntax.Annotation_Alts")

_Annotation_Alts_Iri = (Core.Name "Iri")

_Annotation_Alts_Literal = (Core.Name "Literal")

newtype SemanticActions = 
  SemanticActions {
    unSemanticActions :: [CodeDecl]}
  deriving (Eq, Ord, Read, Show)

_SemanticActions = (Core.Name "hydra.ext.io.shex.syntax.SemanticActions")

data CodeDecl = 
  CodeDecl {
    codeDeclIri :: Iri,
    codeDeclAlts :: CodeDecl_Alts}
  deriving (Eq, Ord, Read, Show)

_CodeDecl = (Core.Name "hydra.ext.io.shex.syntax.CodeDecl")

_CodeDecl_Iri = (Core.Name "Iri")

_CodeDecl_alts = (Core.Name "alts")

data CodeDecl_Alts = 
  CodeDecl_AltsCode Code |
  CodeDecl_AltsPercnt 
  deriving (Eq, Ord, Read, Show)

_CodeDecl_Alts = (Core.Name "hydra.ext.io.shex.syntax.CodeDecl_Alts")

_CodeDecl_Alts_Code = (Core.Name "Code")

_CodeDecl_Alts_Percnt = (Core.Name "Percnt")

data Literal = 
  LiteralRdfLiteral RdfLiteral |
  LiteralNumericLiteral NumericLiteral |
  LiteralBooleanLiteral BooleanLiteral
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra.ext.io.shex.syntax.Literal")

_Literal_RdfLiteral = (Core.Name "RdfLiteral")

_Literal_NumericLiteral = (Core.Name "NumericLiteral")

_Literal_BooleanLiteral = (Core.Name "BooleanLiteral")

data Predicate = 
  PredicateIri Iri |
  PredicateRdfType RdfType
  deriving (Eq, Ord, Read, Show)

_Predicate = (Core.Name "hydra.ext.io.shex.syntax.Predicate")

_Predicate_Iri = (Core.Name "Iri")

_Predicate_RdfType = (Core.Name "RdfType")

newtype Datatype = 
  Datatype {
    unDatatype :: Iri}
  deriving (Eq, Ord, Read, Show)

_Datatype = (Core.Name "hydra.ext.io.shex.syntax.Datatype")

data ShapeExprLabel = 
  ShapeExprLabelIri Iri |
  ShapeExprLabelBlankNode BlankNode
  deriving (Eq, Ord, Read, Show)

_ShapeExprLabel = (Core.Name "hydra.ext.io.shex.syntax.ShapeExprLabel")

_ShapeExprLabel_Iri = (Core.Name "Iri")

_ShapeExprLabel_BlankNode = (Core.Name "BlankNode")

data TripleExprLabel = 
  TripleExprLabelIri Iri |
  TripleExprLabelBlankNode BlankNode
  deriving (Eq, Ord, Read, Show)

_TripleExprLabel = (Core.Name "hydra.ext.io.shex.syntax.TripleExprLabel")

_TripleExprLabel_Iri = (Core.Name "Iri")

_TripleExprLabel_BlankNode = (Core.Name "BlankNode")

data NumericLiteral = 
  NumericLiteralInteger Integer_ |
  NumericLiteralDecimal Decimal |
  NumericLiteralDouble Double_
  deriving (Eq, Ord, Read, Show)

_NumericLiteral = (Core.Name "hydra.ext.io.shex.syntax.NumericLiteral")

_NumericLiteral_Integer = (Core.Name "Integer")

_NumericLiteral_Decimal = (Core.Name "Decimal")

_NumericLiteral_Double = (Core.Name "Double")

data RdfLiteral = 
  RdfLiteral {
    rdfLiteralString :: String_,
    rdfLiteralAlts :: (Maybe RdfLiteral_Alts_Option)}
  deriving (Eq, Ord, Read, Show)

_RdfLiteral = (Core.Name "hydra.ext.io.shex.syntax.RdfLiteral")

_RdfLiteral_String = (Core.Name "String")

_RdfLiteral_Alts = (Core.Name "Alts")

data RdfLiteral_Alts_Option = 
  RdfLiteral_Alts_OptionLangTag LangTag |
  RdfLiteral_Alts_OptionSequence Datatype
  deriving (Eq, Ord, Read, Show)

_RdfLiteral_Alts_Option = (Core.Name "hydra.ext.io.shex.syntax.RdfLiteral_Alts_Option")

_RdfLiteral_Alts_Option_LangTag = (Core.Name "LangTag")

_RdfLiteral_Alts_Option_sequence = (Core.Name "sequence")

data BooleanLiteral = 
  BooleanLiteralTrue  |
  BooleanLiteralFalse 
  deriving (Eq, Ord, Read, Show)

_BooleanLiteral = (Core.Name "hydra.ext.io.shex.syntax.BooleanLiteral")

_BooleanLiteral_True = (Core.Name "True")

_BooleanLiteral_False = (Core.Name "False")

data String_ = 
  StringStringLiteral1 StringLiteral1 |
  StringStringLiteralLong1 StringLiteralLong1 |
  StringStringLiteral2 StringLiteral2 |
  StringStringLiteralLong2 StringLiteralLong2
  deriving (Eq, Ord, Read, Show)

_String = (Core.Name "hydra.ext.io.shex.syntax.String")

_String_StringLiteral1 = (Core.Name "StringLiteral1")

_String_StringLiteralLong1 = (Core.Name "StringLiteralLong1")

_String_StringLiteral2 = (Core.Name "StringLiteral2")

_String_StringLiteralLong2 = (Core.Name "StringLiteralLong2")

data Iri = 
  IriIriRef IriRef |
  IriPrefixedName PrefixedName
  deriving (Eq, Ord, Read, Show)

_Iri = (Core.Name "hydra.ext.io.shex.syntax.Iri")

_Iri_IriRef = (Core.Name "IriRef")

_Iri_PrefixedName = (Core.Name "PrefixedName")

data PrefixedName = 
  PrefixedNamePnameLn PnameLn |
  PrefixedNamePnameNs PnameNs
  deriving (Eq, Ord, Read, Show)

_PrefixedName = (Core.Name "hydra.ext.io.shex.syntax.PrefixedName")

_PrefixedName_PnameLn = (Core.Name "PnameLn")

_PrefixedName_PnameNs = (Core.Name "PnameNs")

newtype BlankNode = 
  BlankNode {
    unBlankNode :: BlankNodeLabel}
  deriving (Eq, Ord, Read, Show)

_BlankNode = (Core.Name "hydra.ext.io.shex.syntax.BlankNode")

newtype IncludeSet = 
  IncludeSet {
    unIncludeSet :: [ShapeExprLabel]}
  deriving (Eq, Ord, Read, Show)

_IncludeSet = (Core.Name "hydra.ext.io.shex.syntax.IncludeSet")

newtype Code = 
  Code {
    unCode :: [Code_Elmt]}
  deriving (Eq, Ord, Read, Show)

_Code = (Core.Name "hydra.ext.io.shex.syntax.Code")

data Code_Elmt = 
  Code_ElmtRegex String |
  Code_ElmtSequence String |
  Code_ElmtUchar Uchar
  deriving (Eq, Ord, Read, Show)

_Code_Elmt = (Core.Name "hydra.ext.io.shex.syntax.Code_Elmt")

_Code_Elmt_regex = (Core.Name "regex")

_Code_Elmt_sequence = (Core.Name "sequence")

_Code_Elmt_Uchar = (Core.Name "Uchar")

data RepeatRange = 
  RepeatRange {
    repeatRangeInteger :: Integer_,
    repeatRangeSequence :: (Maybe (Maybe (Maybe RepeatRange_Sequence_Option_Option_Option)))}
  deriving (Eq, Ord, Read, Show)

_RepeatRange = (Core.Name "hydra.ext.io.shex.syntax.RepeatRange")

_RepeatRange_Integer = (Core.Name "Integer")

_RepeatRange_Sequence = (Core.Name "Sequence")

data RepeatRange_Sequence_Option_Option_Option = 
  RepeatRange_Sequence_Option_Option_OptionInteger Integer_ |
  RepeatRange_Sequence_Option_Option_OptionAst 
  deriving (Eq, Ord, Read, Show)

_RepeatRange_Sequence_Option_Option_Option = (Core.Name "hydra.ext.io.shex.syntax.RepeatRange_Sequence_Option_Option_Option")

_RepeatRange_Sequence_Option_Option_Option_Integer = (Core.Name "Integer")

_RepeatRange_Sequence_Option_Option_Option_Ast = (Core.Name "Ast")

newtype RdfType = 
  RdfType {
    unRdfType :: ()}
  deriving (Eq, Ord, Read, Show)

_RdfType = (Core.Name "hydra.ext.io.shex.syntax.RdfType")

newtype IriRef = 
  IriRef {
    unIriRef :: [IriRef_Elmt]}
  deriving (Eq, Ord, Read, Show)

_IriRef = (Core.Name "hydra.ext.io.shex.syntax.IriRef")

data IriRef_Elmt = 
  IriRef_ElmtRegex String |
  IriRef_ElmtUchar Uchar
  deriving (Eq, Ord, Read, Show)

_IriRef_Elmt = (Core.Name "hydra.ext.io.shex.syntax.IriRef_Elmt")

_IriRef_Elmt_regex = (Core.Name "regex")

_IriRef_Elmt_Uchar = (Core.Name "Uchar")

newtype PnameNs = 
  PnameNs {
    unPnameNs :: (Maybe PnPrefix)}
  deriving (Eq, Ord, Read, Show)

_PnameNs = (Core.Name "hydra.ext.io.shex.syntax.PnameNs")

data PnameLn = 
  PnameLn {
    pnameLnPnameNs :: PnameNs,
    pnameLnPnLocal :: PnLocal}
  deriving (Eq, Ord, Read, Show)

_PnameLn = (Core.Name "hydra.ext.io.shex.syntax.PnameLn")

_PnameLn_PnameNs = (Core.Name "PnameNs")

_PnameLn_PnLocal = (Core.Name "PnLocal")

newtype AtpNameNs = 
  AtpNameNs {
    unAtpNameNs :: (Maybe PnPrefix)}
  deriving (Eq, Ord, Read, Show)

_AtpNameNs = (Core.Name "hydra.ext.io.shex.syntax.AtpNameNs")

data AtpNameLn = 
  AtpNameLn {
    atpNameLnPnameNs :: PnameNs,
    atpNameLnPnLocal :: PnLocal}
  deriving (Eq, Ord, Read, Show)

_AtpNameLn = (Core.Name "hydra.ext.io.shex.syntax.AtpNameLn")

_AtpNameLn_PnameNs = (Core.Name "PnameNs")

_AtpNameLn_PnLocal = (Core.Name "PnLocal")

data Regexp = 
  Regexp {
    regexpListOfAlts :: [Regexp_ListOfAlts_Elmt],
    regexpListOfRegex :: [String]}
  deriving (Eq, Ord, Read, Show)

_Regexp = (Core.Name "hydra.ext.io.shex.syntax.Regexp")

_Regexp_listOfAlts = (Core.Name "listOfAlts")

_Regexp_listOfRegex = (Core.Name "listOfRegex")

data Regexp_ListOfAlts_Elmt = 
  Regexp_ListOfAlts_ElmtRegex String |
  Regexp_ListOfAlts_ElmtSequence String |
  Regexp_ListOfAlts_ElmtUchar Uchar
  deriving (Eq, Ord, Read, Show)

_Regexp_ListOfAlts_Elmt = (Core.Name "hydra.ext.io.shex.syntax.Regexp_ListOfAlts_Elmt")

_Regexp_ListOfAlts_Elmt_regex = (Core.Name "regex")

_Regexp_ListOfAlts_Elmt_sequence = (Core.Name "sequence")

_Regexp_ListOfAlts_Elmt_Uchar = (Core.Name "Uchar")

data BlankNodeLabel = 
  BlankNodeLabel {
    blankNodeLabelAlts :: BlankNodeLabel_Alts,
    blankNodeLabelListOfAlts :: (Maybe [BlankNodeLabel_ListOfAlts_Option_Elmt]),
    blankNodeLabelPnChars :: PnChars}
  deriving (Eq, Ord, Read, Show)

_BlankNodeLabel = (Core.Name "hydra.ext.io.shex.syntax.BlankNodeLabel")

_BlankNodeLabel_alts = (Core.Name "alts")

_BlankNodeLabel_ListOfAlts = (Core.Name "ListOfAlts")

_BlankNodeLabel_PnChars = (Core.Name "PnChars")

data BlankNodeLabel_Alts = 
  BlankNodeLabel_AltsPnCharsU PnCharsU |
  BlankNodeLabel_AltsRegex String
  deriving (Eq, Ord, Read, Show)

_BlankNodeLabel_Alts = (Core.Name "hydra.ext.io.shex.syntax.BlankNodeLabel_Alts")

_BlankNodeLabel_Alts_PnCharsU = (Core.Name "PnCharsU")

_BlankNodeLabel_Alts_regex = (Core.Name "regex")

data BlankNodeLabel_ListOfAlts_Option_Elmt = 
  BlankNodeLabel_ListOfAlts_Option_ElmtPnChars PnChars |
  BlankNodeLabel_ListOfAlts_Option_ElmtPeriod 
  deriving (Eq, Ord, Read, Show)

_BlankNodeLabel_ListOfAlts_Option_Elmt = (Core.Name "hydra.ext.io.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt")

_BlankNodeLabel_ListOfAlts_Option_Elmt_PnChars = (Core.Name "PnChars")

_BlankNodeLabel_ListOfAlts_Option_Elmt_Period = (Core.Name "Period")

newtype LangTag = 
  LangTag {
    unLangTag :: String}
  deriving (Eq, Ord, Read, Show)

_LangTag = (Core.Name "hydra.ext.io.shex.syntax.LangTag")

newtype Integer_ = 
  Integer_ {
    unInteger :: String}
  deriving (Eq, Ord, Read, Show)

_Integer = (Core.Name "hydra.ext.io.shex.syntax.Integer")

newtype Decimal = 
  Decimal {
    unDecimal :: String}
  deriving (Eq, Ord, Read, Show)

_Decimal = (Core.Name "hydra.ext.io.shex.syntax.Decimal")

newtype Double_ = 
  Double_ {
    unDouble :: String}
  deriving (Eq, Ord, Read, Show)

_Double = (Core.Name "hydra.ext.io.shex.syntax.Double")

newtype StringLiteral1 = 
  StringLiteral1 {
    unStringLiteral1 :: [StringLiteral1_Elmt]}
  deriving (Eq, Ord, Read, Show)

_StringLiteral1 = (Core.Name "hydra.ext.io.shex.syntax.StringLiteral1")

data StringLiteral1_Elmt = 
  StringLiteral1_ElmtRegex String |
  StringLiteral1_ElmtEchar Echar |
  StringLiteral1_ElmtUchar Uchar
  deriving (Eq, Ord, Read, Show)

_StringLiteral1_Elmt = (Core.Name "hydra.ext.io.shex.syntax.StringLiteral1_Elmt")

_StringLiteral1_Elmt_regex = (Core.Name "regex")

_StringLiteral1_Elmt_Echar = (Core.Name "Echar")

_StringLiteral1_Elmt_Uchar = (Core.Name "Uchar")

newtype StringLiteral2 = 
  StringLiteral2 {
    unStringLiteral2 :: [StringLiteral2_Elmt]}
  deriving (Eq, Ord, Read, Show)

_StringLiteral2 = (Core.Name "hydra.ext.io.shex.syntax.StringLiteral2")

data StringLiteral2_Elmt = 
  StringLiteral2_ElmtRegex String |
  StringLiteral2_ElmtEchar Echar |
  StringLiteral2_ElmtUchar Uchar
  deriving (Eq, Ord, Read, Show)

_StringLiteral2_Elmt = (Core.Name "hydra.ext.io.shex.syntax.StringLiteral2_Elmt")

_StringLiteral2_Elmt_regex = (Core.Name "regex")

_StringLiteral2_Elmt_Echar = (Core.Name "Echar")

_StringLiteral2_Elmt_Uchar = (Core.Name "Uchar")

newtype StringLiteralLong1 = 
  StringLiteralLong1 {
    unStringLiteralLong1 :: [StringLiteralLong1_Elmt]}
  deriving (Eq, Ord, Read, Show)

_StringLiteralLong1 = (Core.Name "hydra.ext.io.shex.syntax.StringLiteralLong1")

data StringLiteralLong1_Elmt = 
  StringLiteralLong1_ElmtSequence StringLiteralLong1_Elmt_Sequence |
  StringLiteralLong1_ElmtEchar Echar |
  StringLiteralLong1_ElmtUchar Uchar
  deriving (Eq, Ord, Read, Show)

_StringLiteralLong1_Elmt = (Core.Name "hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt")

_StringLiteralLong1_Elmt_sequence = (Core.Name "sequence")

_StringLiteralLong1_Elmt_Echar = (Core.Name "Echar")

_StringLiteralLong1_Elmt_Uchar = (Core.Name "Uchar")

data StringLiteralLong1_Elmt_Sequence = 
  StringLiteralLong1_Elmt_Sequence {
    stringLiteralLong1_Elmt_SequenceAlts :: (Maybe StringLiteralLong1_Elmt_Sequence_Alts_Option),
    stringLiteralLong1_Elmt_SequenceRegex :: String}
  deriving (Eq, Ord, Read, Show)

_StringLiteralLong1_Elmt_Sequence = (Core.Name "hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence")

_StringLiteralLong1_Elmt_Sequence_Alts = (Core.Name "Alts")

_StringLiteralLong1_Elmt_Sequence_regex = (Core.Name "regex")

data StringLiteralLong1_Elmt_Sequence_Alts_Option = 
  StringLiteralLong1_Elmt_Sequence_Alts_OptionApos  |
  StringLiteralLong1_Elmt_Sequence_Alts_OptionSequence StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence
  deriving (Eq, Ord, Read, Show)

_StringLiteralLong1_Elmt_Sequence_Alts_Option = (Core.Name "hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option")

_StringLiteralLong1_Elmt_Sequence_Alts_Option_Apos = (Core.Name "Apos")

_StringLiteralLong1_Elmt_Sequence_Alts_Option_sequence = (Core.Name "sequence")

data StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence = 
  StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence {}
  deriving (Eq, Ord, Read, Show)

_StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence = (Core.Name "hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence")

newtype StringLiteralLong2 = 
  StringLiteralLong2 {
    unStringLiteralLong2 :: [StringLiteralLong2_Elmt]}
  deriving (Eq, Ord, Read, Show)

_StringLiteralLong2 = (Core.Name "hydra.ext.io.shex.syntax.StringLiteralLong2")

data StringLiteralLong2_Elmt = 
  StringLiteralLong2_ElmtSequence StringLiteralLong2_Elmt_Sequence |
  StringLiteralLong2_ElmtEchar Echar |
  StringLiteralLong2_ElmtUchar Uchar
  deriving (Eq, Ord, Read, Show)

_StringLiteralLong2_Elmt = (Core.Name "hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt")

_StringLiteralLong2_Elmt_sequence = (Core.Name "sequence")

_StringLiteralLong2_Elmt_Echar = (Core.Name "Echar")

_StringLiteralLong2_Elmt_Uchar = (Core.Name "Uchar")

data StringLiteralLong2_Elmt_Sequence = 
  StringLiteralLong2_Elmt_Sequence {
    stringLiteralLong2_Elmt_SequenceAlts :: (Maybe StringLiteralLong2_Elmt_Sequence_Alts_Option),
    stringLiteralLong2_Elmt_SequenceRegex :: String}
  deriving (Eq, Ord, Read, Show)

_StringLiteralLong2_Elmt_Sequence = (Core.Name "hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence")

_StringLiteralLong2_Elmt_Sequence_Alts = (Core.Name "Alts")

_StringLiteralLong2_Elmt_Sequence_regex = (Core.Name "regex")

data StringLiteralLong2_Elmt_Sequence_Alts_Option = 
  StringLiteralLong2_Elmt_Sequence_Alts_OptionQuot  |
  StringLiteralLong2_Elmt_Sequence_Alts_OptionSequence StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence
  deriving (Eq, Ord, Read, Show)

_StringLiteralLong2_Elmt_Sequence_Alts_Option = (Core.Name "hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option")

_StringLiteralLong2_Elmt_Sequence_Alts_Option_Quot = (Core.Name "Quot")

_StringLiteralLong2_Elmt_Sequence_Alts_Option_sequence = (Core.Name "sequence")

data StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence = 
  StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence {}
  deriving (Eq, Ord, Read, Show)

_StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence = (Core.Name "hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence")

data Uchar = 
  UcharSequence Uchar_Sequence |
  UcharSequence2 Uchar_Sequence2
  deriving (Eq, Ord, Read, Show)

_Uchar = (Core.Name "hydra.ext.io.shex.syntax.Uchar")

_Uchar_sequence = (Core.Name "sequence")

_Uchar_sequence2 = (Core.Name "sequence2")

data Uchar_Sequence = 
  Uchar_Sequence {
    uchar_SequenceHex :: Hex,
    uchar_SequenceHex2 :: Hex,
    uchar_SequenceHex3 :: Hex,
    uchar_SequenceHex4 :: Hex}
  deriving (Eq, Ord, Read, Show)

_Uchar_Sequence = (Core.Name "hydra.ext.io.shex.syntax.Uchar_Sequence")

_Uchar_Sequence_Hex = (Core.Name "Hex")

_Uchar_Sequence_Hex2 = (Core.Name "Hex2")

_Uchar_Sequence_Hex3 = (Core.Name "Hex3")

_Uchar_Sequence_Hex4 = (Core.Name "Hex4")

data Uchar_Sequence2 = 
  Uchar_Sequence2 {
    uchar_Sequence2Hex :: Hex,
    uchar_Sequence2Hex2 :: Hex,
    uchar_Sequence2Hex3 :: Hex,
    uchar_Sequence2Hex4 :: Hex,
    uchar_Sequence2Hex5 :: Hex,
    uchar_Sequence2Hex6 :: Hex,
    uchar_Sequence2Hex7 :: Hex,
    uchar_Sequence2Hex8 :: Hex}
  deriving (Eq, Ord, Read, Show)

_Uchar_Sequence2 = (Core.Name "hydra.ext.io.shex.syntax.Uchar_Sequence2")

_Uchar_Sequence2_Hex = (Core.Name "Hex")

_Uchar_Sequence2_Hex2 = (Core.Name "Hex2")

_Uchar_Sequence2_Hex3 = (Core.Name "Hex3")

_Uchar_Sequence2_Hex4 = (Core.Name "Hex4")

_Uchar_Sequence2_Hex5 = (Core.Name "Hex5")

_Uchar_Sequence2_Hex6 = (Core.Name "Hex6")

_Uchar_Sequence2_Hex7 = (Core.Name "Hex7")

_Uchar_Sequence2_Hex8 = (Core.Name "Hex8")

newtype Echar = 
  Echar {
    unEchar :: String}
  deriving (Eq, Ord, Read, Show)

_Echar = (Core.Name "hydra.ext.io.shex.syntax.Echar")

data PnCharsBase = 
  PnCharsBaseRegex String |
  PnCharsBaseRegex2 String
  deriving (Eq, Ord, Read, Show)

_PnCharsBase = (Core.Name "hydra.ext.io.shex.syntax.PnCharsBase")

_PnCharsBase_regex = (Core.Name "regex")

_PnCharsBase_regex2 = (Core.Name "regex2")

data PnCharsU = 
  PnCharsUPnCharsBase PnCharsBase |
  PnCharsULowbar 
  deriving (Eq, Ord, Read, Show)

_PnCharsU = (Core.Name "hydra.ext.io.shex.syntax.PnCharsU")

_PnCharsU_PnCharsBase = (Core.Name "PnCharsBase")

_PnCharsU_Lowbar = (Core.Name "Lowbar")

data PnChars = 
  PnCharsPnCharsU PnCharsU |
  PnCharsMinus  |
  PnCharsRegex String
  deriving (Eq, Ord, Read, Show)

_PnChars = (Core.Name "hydra.ext.io.shex.syntax.PnChars")

_PnChars_PnCharsU = (Core.Name "PnCharsU")

_PnChars_Minus = (Core.Name "Minus")

_PnChars_regex = (Core.Name "regex")

data PnPrefix = 
  PnPrefix {
    pnPrefixPnCharsBase :: PnCharsBase,
    pnPrefixSequence :: (Maybe PnPrefix_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_PnPrefix = (Core.Name "hydra.ext.io.shex.syntax.PnPrefix")

_PnPrefix_PnCharsBase = (Core.Name "PnCharsBase")

_PnPrefix_Sequence = (Core.Name "Sequence")

data PnPrefix_Sequence_Option = 
  PnPrefix_Sequence_Option {
    pnPrefix_Sequence_OptionAlts :: PnPrefix_Sequence_Option_Alts,
    pnPrefix_Sequence_OptionPnChars :: PnChars}
  deriving (Eq, Ord, Read, Show)

_PnPrefix_Sequence_Option = (Core.Name "hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option")

_PnPrefix_Sequence_Option_alts = (Core.Name "alts")

_PnPrefix_Sequence_Option_PnChars = (Core.Name "PnChars")

data PnPrefix_Sequence_Option_Alts = 
  PnPrefix_Sequence_Option_AltsPnChars PnChars |
  PnPrefix_Sequence_Option_AltsPeriod 
  deriving (Eq, Ord, Read, Show)

_PnPrefix_Sequence_Option_Alts = (Core.Name "hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option_Alts")

_PnPrefix_Sequence_Option_Alts_PnChars = (Core.Name "PnChars")

_PnPrefix_Sequence_Option_Alts_Period = (Core.Name "Period")

data PnLocal = 
  PnLocal {
    pnLocalAlts :: PnLocal_Alts,
    pnLocalSequence :: (Maybe PnLocal_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_PnLocal = (Core.Name "hydra.ext.io.shex.syntax.PnLocal")

_PnLocal_alts = (Core.Name "alts")

_PnLocal_Sequence = (Core.Name "Sequence")

data PnLocal_Alts = 
  PnLocal_AltsPnCharsU PnCharsU |
  PnLocal_AltsColon  |
  PnLocal_AltsRegex String |
  PnLocal_AltsPlx Plx
  deriving (Eq, Ord, Read, Show)

_PnLocal_Alts = (Core.Name "hydra.ext.io.shex.syntax.PnLocal_Alts")

_PnLocal_Alts_PnCharsU = (Core.Name "PnCharsU")

_PnLocal_Alts_Colon = (Core.Name "Colon")

_PnLocal_Alts_regex = (Core.Name "regex")

_PnLocal_Alts_Plx = (Core.Name "Plx")

data PnLocal_Sequence_Option = 
  PnLocal_Sequence_Option {
    pnLocal_Sequence_OptionListOfAlts :: [PnLocal_Sequence_Option_ListOfAlts_Elmt],
    pnLocal_Sequence_OptionAlts :: PnLocal_Sequence_Option_Alts}
  deriving (Eq, Ord, Read, Show)

_PnLocal_Sequence_Option = (Core.Name "hydra.ext.io.shex.syntax.PnLocal_Sequence_Option")

_PnLocal_Sequence_Option_listOfAlts = (Core.Name "listOfAlts")

_PnLocal_Sequence_Option_alts = (Core.Name "alts")

data PnLocal_Sequence_Option_ListOfAlts_Elmt = 
  PnLocal_Sequence_Option_ListOfAlts_ElmtPnChars PnChars |
  PnLocal_Sequence_Option_ListOfAlts_ElmtPeriod  |
  PnLocal_Sequence_Option_ListOfAlts_ElmtColon  |
  PnLocal_Sequence_Option_ListOfAlts_ElmtPlx Plx
  deriving (Eq, Ord, Read, Show)

_PnLocal_Sequence_Option_ListOfAlts_Elmt = (Core.Name "hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt")

_PnLocal_Sequence_Option_ListOfAlts_Elmt_PnChars = (Core.Name "PnChars")

_PnLocal_Sequence_Option_ListOfAlts_Elmt_Period = (Core.Name "Period")

_PnLocal_Sequence_Option_ListOfAlts_Elmt_Colon = (Core.Name "Colon")

_PnLocal_Sequence_Option_ListOfAlts_Elmt_Plx = (Core.Name "Plx")

data PnLocal_Sequence_Option_Alts = 
  PnLocal_Sequence_Option_AltsPnChars PnChars |
  PnLocal_Sequence_Option_AltsColon  |
  PnLocal_Sequence_Option_AltsPlx Plx
  deriving (Eq, Ord, Read, Show)

_PnLocal_Sequence_Option_Alts = (Core.Name "hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_Alts")

_PnLocal_Sequence_Option_Alts_PnChars = (Core.Name "PnChars")

_PnLocal_Sequence_Option_Alts_Colon = (Core.Name "Colon")

_PnLocal_Sequence_Option_Alts_Plx = (Core.Name "Plx")

data Plx = 
  PlxPercent Percent |
  PlxPnLocalEsc PnLocalEsc
  deriving (Eq, Ord, Read, Show)

_Plx = (Core.Name "hydra.ext.io.shex.syntax.Plx")

_Plx_Percent = (Core.Name "Percent")

_Plx_PnLocalEsc = (Core.Name "PnLocalEsc")

data Percent = 
  Percent {
    percentHex :: Hex,
    percentHex2 :: Hex}
  deriving (Eq, Ord, Read, Show)

_Percent = (Core.Name "hydra.ext.io.shex.syntax.Percent")

_Percent_Hex = (Core.Name "Hex")

_Percent_Hex2 = (Core.Name "Hex2")

newtype Hex = 
  Hex {
    unHex :: String}
  deriving (Eq, Ord, Read, Show)

_Hex = (Core.Name "hydra.ext.io.shex.syntax.Hex")

newtype PnLocalEsc = 
  PnLocalEsc {
    unPnLocalEsc :: String}
  deriving (Eq, Ord, Read, Show)

_PnLocalEsc = (Core.Name "hydra.ext.io.shex.syntax.PnLocalEsc")
