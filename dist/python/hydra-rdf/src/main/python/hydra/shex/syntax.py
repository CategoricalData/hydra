# Note: this is an automatically generated file. Do not edit.

r"""A Shex model. Based on the BNF at:
  https://github.com/shexSpec/grammar/blob/master/bnf."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class ShexDoc:
    list_of_directive: frozenlist[Directive]
    sequence: Maybe[ShexDoc_Sequence_Option]
    prefix_decl: PrefixDecl

    TYPE_ = hydra.core.Name("hydra.shex.syntax.ShexDoc")
    LIST_OF_DIRECTIVE = hydra.core.Name("listOfDirective")
    SEQUENCE = hydra.core.Name("Sequence")
    PREFIX_DECL = hydra.core.Name("PrefixDecl")

@dataclass(frozen=True)
class ShexDoc_Sequence_Option:
    alts: ShexDoc_Sequence_Option_Alts
    list_of_statement: frozenlist[Statement]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.ShexDoc_Sequence_Option")
    ALTS = hydra.core.Name("alts")
    LIST_OF_STATEMENT = hydra.core.Name("listOfStatement")

class ShexDoc_Sequence_Option_AltsNotStartAction(Node["NotStartAction"]):
    ...

class ShexDoc_Sequence_Option_AltsStartActions(Node["StartActions"]):
    ...

class _ShexDoc_Sequence_Option_AltsMeta(type):
    def __getitem__(cls, item):
        return object

class ShexDoc_Sequence_Option_Alts(metaclass=_ShexDoc_Sequence_Option_AltsMeta):
    r"""ShexDoc_Sequence_Option_AltsNotStartAction | ShexDoc_Sequence_Option_AltsStartActions"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.ShexDoc_Sequence_Option_Alts")
    NOT_START_ACTION = hydra.core.Name("NotStartAction")
    START_ACTIONS = hydra.core.Name("StartActions")

class DirectiveBaseDecl(Node["BaseDecl"]):
    ...

class DirectivePrefixDecl(Node["PrefixDecl"]):
    ...

class _DirectiveMeta(type):
    def __getitem__(cls, item):
        return object

class Directive(metaclass=_DirectiveMeta):
    r"""DirectiveBaseDecl | DirectivePrefixDecl"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Directive")
    BASE_DECL = hydra.core.Name("BaseDecl")
    PREFIX_DECL = hydra.core.Name("PrefixDecl")

class BaseDecl(Node["IriRef"]):
    ...

BaseDecl.TYPE_ = hydra.core.Name("hydra.shex.syntax.BaseDecl")

@dataclass(frozen=True)
class PrefixDecl:
    pname_ns: PnameNs
    iri_ref: IriRef

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PrefixDecl")
    PNAME_NS = hydra.core.Name("PnameNs")
    IRI_REF = hydra.core.Name("IriRef")

class NotStartActionStart(Node["ShapeExpression"]):
    ...

class NotStartActionShapeExprDecl(Node["NotStartAction_ShapeExprDecl"]):
    ...

class _NotStartActionMeta(type):
    def __getitem__(cls, item):
        return object

class NotStartAction(metaclass=_NotStartActionMeta):
    r"""NotStartActionStart | NotStartActionShapeExprDecl"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.NotStartAction")
    START = hydra.core.Name("start")
    SHAPE_EXPR_DECL = hydra.core.Name("shapeExprDecl")

@dataclass(frozen=True)
class NotStartAction_ShapeExprDecl:
    shape_expr_label: ShapeExprLabel
    alts: NotStartAction_ShapeExprDecl_Alts

    TYPE_ = hydra.core.Name("hydra.shex.syntax.NotStartAction_ShapeExprDecl")
    SHAPE_EXPR_LABEL = hydra.core.Name("ShapeExprLabel")
    ALTS = hydra.core.Name("alts")

class NotStartAction_ShapeExprDecl_AltsShapeExpression(Node["ShapeExpression"]):
    ...

class NotStartAction_ShapeExprDecl_AltsEXTERNAL:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, NotStartAction_ShapeExprDecl_AltsEXTERNAL)
    def __hash__(self):
        return hash("NotStartAction_ShapeExprDecl_AltsEXTERNAL")

class _NotStartAction_ShapeExprDecl_AltsMeta(type):
    def __getitem__(cls, item):
        return object

class NotStartAction_ShapeExprDecl_Alts(metaclass=_NotStartAction_ShapeExprDecl_AltsMeta):
    r"""NotStartAction_ShapeExprDecl_AltsShapeExpression | NotStartAction_ShapeExprDecl_AltsEXTERNAL"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.NotStartAction_ShapeExprDecl_Alts")
    SHAPE_EXPRESSION = hydra.core.Name("ShapeExpression")
    E_X_T_E_R_N_A_L = hydra.core.Name("EXTERNAL")

class StartActions(Node["frozenlist[CodeDecl]"]):
    ...

StartActions.TYPE_ = hydra.core.Name("hydra.shex.syntax.StartActions")

class StatementDirective(Node["Directive"]):
    ...

class StatementNotStartAction(Node["NotStartAction"]):
    ...

class _StatementMeta(type):
    def __getitem__(cls, item):
        return object

class Statement(metaclass=_StatementMeta):
    r"""StatementDirective | StatementNotStartAction"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Statement")
    DIRECTIVE = hydra.core.Name("Directive")
    NOT_START_ACTION = hydra.core.Name("NotStartAction")

class ShapeExpression(Node["ShapeOr"]):
    ...

ShapeExpression.TYPE_ = hydra.core.Name("hydra.shex.syntax.ShapeExpression")

class InlineShapeExpression(Node["InlineShapeOr"]):
    ...

InlineShapeExpression.TYPE_ = hydra.core.Name("hydra.shex.syntax.InlineShapeExpression")

@dataclass(frozen=True)
class ShapeOr:
    shape_and: ShapeAnd
    list_of_sequence: frozenlist[ShapeAnd]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.ShapeOr")
    SHAPE_AND = hydra.core.Name("ShapeAnd")
    LIST_OF_SEQUENCE = hydra.core.Name("listOfSequence")

@dataclass(frozen=True)
class InlineShapeOr:
    shape_and: ShapeAnd
    list_of_sequence: frozenlist[InlineShapeAnd]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.InlineShapeOr")
    SHAPE_AND = hydra.core.Name("ShapeAnd")
    LIST_OF_SEQUENCE = hydra.core.Name("listOfSequence")

@dataclass(frozen=True)
class ShapeAnd:
    shape_not: ShapeNot
    list_of_sequence: frozenlist[ShapeNot]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.ShapeAnd")
    SHAPE_NOT = hydra.core.Name("ShapeNot")
    LIST_OF_SEQUENCE = hydra.core.Name("listOfSequence")

@dataclass(frozen=True)
class InlineShapeAnd:
    inline_shape_not: InlineShapeNot
    list_of_sequence: frozenlist[InlineShapeNot]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.InlineShapeAnd")
    INLINE_SHAPE_NOT = hydra.core.Name("InlineShapeNot")
    LIST_OF_SEQUENCE = hydra.core.Name("listOfSequence")

@dataclass(frozen=True)
class ShapeNot:
    n_o_t: Maybe[None]
    shape_atom: ShapeAtom

    TYPE_ = hydra.core.Name("hydra.shex.syntax.ShapeNot")
    N_O_T = hydra.core.Name("NOT")
    SHAPE_ATOM = hydra.core.Name("ShapeAtom")

@dataclass(frozen=True)
class InlineShapeNot:
    n_o_t: Maybe[None]
    inline_shape_atom: InlineShapeAtom

    TYPE_ = hydra.core.Name("hydra.shex.syntax.InlineShapeNot")
    N_O_T = hydra.core.Name("NOT")
    INLINE_SHAPE_ATOM = hydra.core.Name("InlineShapeAtom")

class ShapeAtomSequence(Node["ShapeAtom_Sequence"]):
    ...

class ShapeAtomShapeOrRef(Node["ShapeOrRef"]):
    ...

class ShapeAtomSequence2(Node["ShapeExpression"]):
    ...

class ShapeAtomPeriod:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ShapeAtomPeriod)
    def __hash__(self):
        return hash("ShapeAtomPeriod")

class _ShapeAtomMeta(type):
    def __getitem__(cls, item):
        return object

class ShapeAtom(metaclass=_ShapeAtomMeta):
    r"""ShapeAtomSequence | ShapeAtomShapeOrRef | ShapeAtomSequence2 | ShapeAtomPeriod"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.ShapeAtom")
    SEQUENCE = hydra.core.Name("sequence")
    SHAPE_OR_REF = hydra.core.Name("ShapeOrRef")
    SEQUENCE2 = hydra.core.Name("sequence2")
    PERIOD = hydra.core.Name("Period")

@dataclass(frozen=True)
class ShapeAtom_Sequence:
    node_constraint: NodeConstraint
    shape_or_ref: Maybe[ShapeOrRef]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.ShapeAtom_Sequence")
    NODE_CONSTRAINT = hydra.core.Name("NodeConstraint")
    SHAPE_OR_REF = hydra.core.Name("ShapeOrRef")

class InlineShapeAtomSequence(Node["InlineShapeAtom_Sequence"]):
    ...

class InlineShapeAtomSequence2(Node["InlineShapeAtom_Sequence2"]):
    ...

class InlineShapeAtomSequence3(Node["ShapeExpression"]):
    ...

class InlineShapeAtomPeriod:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InlineShapeAtomPeriod)
    def __hash__(self):
        return hash("InlineShapeAtomPeriod")

class _InlineShapeAtomMeta(type):
    def __getitem__(cls, item):
        return object

class InlineShapeAtom(metaclass=_InlineShapeAtomMeta):
    r"""InlineShapeAtomSequence | InlineShapeAtomSequence2 | InlineShapeAtomSequence3 | InlineShapeAtomPeriod"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.InlineShapeAtom")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")
    SEQUENCE3 = hydra.core.Name("sequence3")
    PERIOD = hydra.core.Name("Period")

@dataclass(frozen=True)
class InlineShapeAtom_Sequence:
    node_constraint: NodeConstraint
    inline_shape_or_ref: Maybe[InlineShapeOrRef]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.InlineShapeAtom_Sequence")
    NODE_CONSTRAINT = hydra.core.Name("NodeConstraint")
    INLINE_SHAPE_OR_REF = hydra.core.Name("InlineShapeOrRef")

@dataclass(frozen=True)
class InlineShapeAtom_Sequence2:
    inline_shape_or_ref: InlineShapeOrRef
    node_constraint: Maybe[NodeConstraint]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.InlineShapeAtom_Sequence2")
    INLINE_SHAPE_OR_REF = hydra.core.Name("InlineShapeOrRef")
    NODE_CONSTRAINT = hydra.core.Name("NodeConstraint")

class ShapeOrRefShapeDefinition(Node["ShapeDefinition"]):
    ...

class ShapeOrRefAtpNameLn(Node["AtpNameLn"]):
    ...

class ShapeOrRefAtpNameNs(Node["AtpNameNs"]):
    ...

class ShapeOrRefSequence(Node["ShapeExprLabel"]):
    ...

class _ShapeOrRefMeta(type):
    def __getitem__(cls, item):
        return object

class ShapeOrRef(metaclass=_ShapeOrRefMeta):
    r"""ShapeOrRefShapeDefinition | ShapeOrRefAtpNameLn | ShapeOrRefAtpNameNs | ShapeOrRefSequence"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.ShapeOrRef")
    SHAPE_DEFINITION = hydra.core.Name("ShapeDefinition")
    ATP_NAME_LN = hydra.core.Name("AtpNameLn")
    ATP_NAME_NS = hydra.core.Name("AtpNameNs")
    SEQUENCE = hydra.core.Name("sequence")

class InlineShapeOrRefInlineShapeDefinition(Node["InlineShapeDefinition"]):
    ...

class InlineShapeOrRefAtpNameLn(Node["AtpNameLn"]):
    ...

class InlineShapeOrRefAtpNameNs(Node["AtpNameNs"]):
    ...

class InlineShapeOrRefSequence(Node["ShapeExprLabel"]):
    ...

class _InlineShapeOrRefMeta(type):
    def __getitem__(cls, item):
        return object

class InlineShapeOrRef(metaclass=_InlineShapeOrRefMeta):
    r"""InlineShapeOrRefInlineShapeDefinition | InlineShapeOrRefAtpNameLn | InlineShapeOrRefAtpNameNs | InlineShapeOrRefSequence"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.InlineShapeOrRef")
    INLINE_SHAPE_DEFINITION = hydra.core.Name("InlineShapeDefinition")
    ATP_NAME_LN = hydra.core.Name("AtpNameLn")
    ATP_NAME_NS = hydra.core.Name("AtpNameNs")
    SEQUENCE = hydra.core.Name("sequence")

class NodeConstraintSequence(Node["frozenlist[XsFacet]"]):
    ...

class NodeConstraintSequence2(Node["NodeConstraint_Sequence2"]):
    ...

class NodeConstraintSequence3(Node["NodeConstraint_Sequence3"]):
    ...

class NodeConstraintSequence4(Node["NodeConstraint_Sequence4"]):
    ...

class NodeConstraintSequence5(Node["NodeConstraint_Sequence5"]):
    ...

class NodeConstraintListOfXsFacet(Node["frozenlist[XsFacet]"]):
    ...

class _NodeConstraintMeta(type):
    def __getitem__(cls, item):
        return object

class NodeConstraint(metaclass=_NodeConstraintMeta):
    r"""NodeConstraintSequence | NodeConstraintSequence2 | NodeConstraintSequence3 | NodeConstraintSequence4 | NodeConstraintSequence5 | NodeConstraintListOfXsFacet"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.NodeConstraint")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")
    SEQUENCE3 = hydra.core.Name("sequence3")
    SEQUENCE4 = hydra.core.Name("sequence4")
    SEQUENCE5 = hydra.core.Name("sequence5")
    LIST_OF_XS_FACET = hydra.core.Name("listOfXsFacet")

@dataclass(frozen=True)
class NodeConstraint_Sequence2:
    non_literal_kind: NonLiteralKind
    list_of_string_facet: frozenlist[StringFacet]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.NodeConstraint_Sequence2")
    NON_LITERAL_KIND = hydra.core.Name("NonLiteralKind")
    LIST_OF_STRING_FACET = hydra.core.Name("listOfStringFacet")

@dataclass(frozen=True)
class NodeConstraint_Sequence3:
    datatype: Datatype
    list_of_xs_facet: frozenlist[XsFacet]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.NodeConstraint_Sequence3")
    DATATYPE = hydra.core.Name("Datatype")
    LIST_OF_XS_FACET = hydra.core.Name("listOfXsFacet")

@dataclass(frozen=True)
class NodeConstraint_Sequence4:
    value_set: ValueSet
    list_of_xs_facet: frozenlist[XsFacet]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.NodeConstraint_Sequence4")
    VALUE_SET = hydra.core.Name("ValueSet")
    LIST_OF_XS_FACET = hydra.core.Name("listOfXsFacet")

@dataclass(frozen=True)
class NodeConstraint_Sequence5:
    value_set: ValueSet
    list_of_xs_facet: frozenlist[XsFacet]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.NodeConstraint_Sequence5")
    VALUE_SET = hydra.core.Name("ValueSet")
    LIST_OF_XS_FACET = hydra.core.Name("listOfXsFacet")

class NonLiteralKind(Enum):
    I_R_I = hydra.core.Name("IRI")

    B_N_O_D_E = hydra.core.Name("BNODE")

    N_O_N_L_I_T_E_R_A_L = hydra.core.Name("NONLITERAL")

NonLiteralKind.TYPE_ = hydra.core.Name("hydra.shex.syntax.NonLiteralKind")

class XsFacetStringFacet(Node["StringFacet"]):
    ...

class XsFacetNumericFacet(Node["NumericFacet"]):
    ...

class _XsFacetMeta(type):
    def __getitem__(cls, item):
        return object

class XsFacet(metaclass=_XsFacetMeta):
    r"""XsFacetStringFacet | XsFacetNumericFacet"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.XsFacet")
    STRING_FACET = hydra.core.Name("StringFacet")
    NUMERIC_FACET = hydra.core.Name("NumericFacet")

class StringFacetSequence(Node["StringFacet_Sequence"]):
    ...

class StringFacetRegexp(Node["Regexp"]):
    ...

class _StringFacetMeta(type):
    def __getitem__(cls, item):
        return object

class StringFacet(metaclass=_StringFacetMeta):
    r"""StringFacetSequence | StringFacetRegexp"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.StringFacet")
    SEQUENCE = hydra.core.Name("sequence")
    REGEXP = hydra.core.Name("Regexp")

@dataclass(frozen=True)
class StringFacet_Sequence:
    string_length: StringLength
    integer: Integer

    TYPE_ = hydra.core.Name("hydra.shex.syntax.StringFacet_Sequence")
    STRING_LENGTH = hydra.core.Name("StringLength")
    INTEGER = hydra.core.Name("Integer")

class StringLength(Enum):
    L_E_N_G_T_H = hydra.core.Name("LENGTH")

    M_I_N_L_E_N_G_T_H = hydra.core.Name("MINLENGTH")

    M_A_X_L_E_N_G_T_H = hydra.core.Name("MAXLENGTH")

StringLength.TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLength")

class NumericFacetSequence(Node["NumericFacet_Sequence"]):
    ...

class NumericFacetSequence2(Node["NumericFacet_Sequence2"]):
    ...

class _NumericFacetMeta(type):
    def __getitem__(cls, item):
        return object

class NumericFacet(metaclass=_NumericFacetMeta):
    r"""NumericFacetSequence | NumericFacetSequence2"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.NumericFacet")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class NumericFacet_Sequence:
    numeric_range: NumericRange
    numeric_literal: NumericLiteral

    TYPE_ = hydra.core.Name("hydra.shex.syntax.NumericFacet_Sequence")
    NUMERIC_RANGE = hydra.core.Name("NumericRange")
    NUMERIC_LITERAL = hydra.core.Name("NumericLiteral")

@dataclass(frozen=True)
class NumericFacet_Sequence2:
    numeric_length: NumericLength
    integer: Integer

    TYPE_ = hydra.core.Name("hydra.shex.syntax.NumericFacet_Sequence2")
    NUMERIC_LENGTH = hydra.core.Name("NumericLength")
    INTEGER = hydra.core.Name("Integer")

class NumericRange(Enum):
    M_I_N_I_N_C_L_U_S_I_V_E = hydra.core.Name("MININCLUSIVE")

    M_I_N_E_X_C_L_U_S_I_V_E = hydra.core.Name("MINEXCLUSIVE")

    M_A_X_I_N_C_L_U_S_I_V_E = hydra.core.Name("MAXINCLUSIVE")

    M_A_X_E_X_C_L_U_S_I_V_E = hydra.core.Name("MAXEXCLUSIVE")

NumericRange.TYPE_ = hydra.core.Name("hydra.shex.syntax.NumericRange")

class NumericLength(Enum):
    T_O_T_A_L_D_I_G_I_T_S = hydra.core.Name("TOTALDIGITS")

    F_R_A_C_T_I_O_N_D_I_G_I_T_S = hydra.core.Name("FRACTIONDIGITS")

NumericLength.TYPE_ = hydra.core.Name("hydra.shex.syntax.NumericLength")

@dataclass(frozen=True)
class ShapeDefinition:
    list_of_alts: frozenlist[ShapeDefinition_ListOfAlts_Elmt]
    triple_expression: Maybe[TripleExpression]
    list_of_annotation: frozenlist[Annotation]
    semantic_actions: SemanticActions

    TYPE_ = hydra.core.Name("hydra.shex.syntax.ShapeDefinition")
    LIST_OF_ALTS = hydra.core.Name("listOfAlts")
    TRIPLE_EXPRESSION = hydra.core.Name("TripleExpression")
    LIST_OF_ANNOTATION = hydra.core.Name("listOfAnnotation")
    SEMANTIC_ACTIONS = hydra.core.Name("SemanticActions")

class ShapeDefinition_ListOfAlts_ElmtIncludeSet(Node["IncludeSet"]):
    ...

class ShapeDefinition_ListOfAlts_ElmtExtraPropertySet(Node["ExtraPropertySet"]):
    ...

class ShapeDefinition_ListOfAlts_ElmtCLOSED:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ShapeDefinition_ListOfAlts_ElmtCLOSED)
    def __hash__(self):
        return hash("ShapeDefinition_ListOfAlts_ElmtCLOSED")

class _ShapeDefinition_ListOfAlts_ElmtMeta(type):
    def __getitem__(cls, item):
        return object

class ShapeDefinition_ListOfAlts_Elmt(metaclass=_ShapeDefinition_ListOfAlts_ElmtMeta):
    r"""ShapeDefinition_ListOfAlts_ElmtIncludeSet | ShapeDefinition_ListOfAlts_ElmtExtraPropertySet | ShapeDefinition_ListOfAlts_ElmtCLOSED"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.ShapeDefinition_ListOfAlts_Elmt")
    INCLUDE_SET = hydra.core.Name("IncludeSet")
    EXTRA_PROPERTY_SET = hydra.core.Name("ExtraPropertySet")
    C_L_O_S_E_D = hydra.core.Name("CLOSED")

@dataclass(frozen=True)
class InlineShapeDefinition:
    list_of_alts: frozenlist[InlineShapeDefinition_ListOfAlts_Elmt]
    triple_expression: Maybe[TripleExpression]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.InlineShapeDefinition")
    LIST_OF_ALTS = hydra.core.Name("listOfAlts")
    TRIPLE_EXPRESSION = hydra.core.Name("TripleExpression")

class InlineShapeDefinition_ListOfAlts_ElmtIncludeSet(Node["IncludeSet"]):
    ...

class InlineShapeDefinition_ListOfAlts_ElmtExtraPropertySet(Node["ExtraPropertySet"]):
    ...

class InlineShapeDefinition_ListOfAlts_ElmtCLOSED:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InlineShapeDefinition_ListOfAlts_ElmtCLOSED)
    def __hash__(self):
        return hash("InlineShapeDefinition_ListOfAlts_ElmtCLOSED")

class _InlineShapeDefinition_ListOfAlts_ElmtMeta(type):
    def __getitem__(cls, item):
        return object

class InlineShapeDefinition_ListOfAlts_Elmt(metaclass=_InlineShapeDefinition_ListOfAlts_ElmtMeta):
    r"""InlineShapeDefinition_ListOfAlts_ElmtIncludeSet | InlineShapeDefinition_ListOfAlts_ElmtExtraPropertySet | InlineShapeDefinition_ListOfAlts_ElmtCLOSED"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.InlineShapeDefinition_ListOfAlts_Elmt")
    INCLUDE_SET = hydra.core.Name("IncludeSet")
    EXTRA_PROPERTY_SET = hydra.core.Name("ExtraPropertySet")
    C_L_O_S_E_D = hydra.core.Name("CLOSED")

class ExtraPropertySet(Node["frozenlist[Predicate]"]):
    ...

ExtraPropertySet.TYPE_ = hydra.core.Name("hydra.shex.syntax.ExtraPropertySet")

class TripleExpression(Node["OneOfTripleExpr"]):
    ...

TripleExpression.TYPE_ = hydra.core.Name("hydra.shex.syntax.TripleExpression")

class OneOfTripleExprGroupTripleExpr(Node["GroupTripleExpr"]):
    ...

class OneOfTripleExprMultiElementOneOf(Node["MultiElementOneOf"]):
    ...

class _OneOfTripleExprMeta(type):
    def __getitem__(cls, item):
        return object

class OneOfTripleExpr(metaclass=_OneOfTripleExprMeta):
    r"""OneOfTripleExprGroupTripleExpr | OneOfTripleExprMultiElementOneOf"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.OneOfTripleExpr")
    GROUP_TRIPLE_EXPR = hydra.core.Name("GroupTripleExpr")
    MULTI_ELEMENT_ONE_OF = hydra.core.Name("MultiElementOneOf")

@dataclass(frozen=True)
class MultiElementOneOf:
    group_triple_expr: GroupTripleExpr
    list_of_sequence: frozenlist[GroupTripleExpr]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.MultiElementOneOf")
    GROUP_TRIPLE_EXPR = hydra.core.Name("GroupTripleExpr")
    LIST_OF_SEQUENCE = hydra.core.Name("listOfSequence")

class InnerTripleExprMultiElementGroup(Node["MultiElementGroup"]):
    ...

class InnerTripleExprMultiElementOneOf(Node["MultiElementOneOf"]):
    ...

class _InnerTripleExprMeta(type):
    def __getitem__(cls, item):
        return object

class InnerTripleExpr(metaclass=_InnerTripleExprMeta):
    r"""InnerTripleExprMultiElementGroup | InnerTripleExprMultiElementOneOf"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.InnerTripleExpr")
    MULTI_ELEMENT_GROUP = hydra.core.Name("MultiElementGroup")
    MULTI_ELEMENT_ONE_OF = hydra.core.Name("MultiElementOneOf")

class GroupTripleExprSingleElementGroup(Node["SingleElementGroup"]):
    ...

class GroupTripleExprMultiElementGroup(Node["MultiElementGroup"]):
    ...

class _GroupTripleExprMeta(type):
    def __getitem__(cls, item):
        return object

class GroupTripleExpr(metaclass=_GroupTripleExprMeta):
    r"""GroupTripleExprSingleElementGroup | GroupTripleExprMultiElementGroup"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.GroupTripleExpr")
    SINGLE_ELEMENT_GROUP = hydra.core.Name("SingleElementGroup")
    MULTI_ELEMENT_GROUP = hydra.core.Name("MultiElementGroup")

@dataclass(frozen=True)
class SingleElementGroup:
    unary_triple_expr: UnaryTripleExpr
    semi: Maybe[None]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.SingleElementGroup")
    UNARY_TRIPLE_EXPR = hydra.core.Name("UnaryTripleExpr")
    SEMI = hydra.core.Name("Semi")

@dataclass(frozen=True)
class MultiElementGroup:
    unary_triple_expr: UnaryTripleExpr
    list_of_sequence: frozenlist[UnaryTripleExpr]
    semi: Maybe[None]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.MultiElementGroup")
    UNARY_TRIPLE_EXPR = hydra.core.Name("UnaryTripleExpr")
    LIST_OF_SEQUENCE = hydra.core.Name("listOfSequence")
    SEMI = hydra.core.Name("Semi")

class UnaryTripleExprSequence(Node["UnaryTripleExpr_Sequence"]):
    ...

class UnaryTripleExprInclude(Node["Include"]):
    ...

class _UnaryTripleExprMeta(type):
    def __getitem__(cls, item):
        return object

class UnaryTripleExpr(metaclass=_UnaryTripleExprMeta):
    r"""UnaryTripleExprSequence | UnaryTripleExprInclude"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.UnaryTripleExpr")
    SEQUENCE = hydra.core.Name("sequence")
    INCLUDE = hydra.core.Name("Include")

@dataclass(frozen=True)
class UnaryTripleExpr_Sequence:
    sequence: Maybe[TripleExprLabel]
    alts: UnaryTripleExpr_Sequence_Alts

    TYPE_ = hydra.core.Name("hydra.shex.syntax.UnaryTripleExpr_Sequence")
    SEQUENCE = hydra.core.Name("Sequence")
    ALTS = hydra.core.Name("alts")

class UnaryTripleExpr_Sequence_AltsTripleConstraint(Node["TripleConstraint"]):
    ...

class UnaryTripleExpr_Sequence_AltsBracketedTripleExpr(Node["BracketedTripleExpr"]):
    ...

class _UnaryTripleExpr_Sequence_AltsMeta(type):
    def __getitem__(cls, item):
        return object

class UnaryTripleExpr_Sequence_Alts(metaclass=_UnaryTripleExpr_Sequence_AltsMeta):
    r"""UnaryTripleExpr_Sequence_AltsTripleConstraint | UnaryTripleExpr_Sequence_AltsBracketedTripleExpr"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.UnaryTripleExpr_Sequence_Alts")
    TRIPLE_CONSTRAINT = hydra.core.Name("TripleConstraint")
    BRACKETED_TRIPLE_EXPR = hydra.core.Name("BracketedTripleExpr")

@dataclass(frozen=True)
class BracketedTripleExpr:
    inner_triple_expr: InnerTripleExpr
    cardinality: Maybe[Cardinality]
    list_of_annotation: frozenlist[Annotation]
    semantic_actions: SemanticActions

    TYPE_ = hydra.core.Name("hydra.shex.syntax.BracketedTripleExpr")
    INNER_TRIPLE_EXPR = hydra.core.Name("InnerTripleExpr")
    CARDINALITY = hydra.core.Name("Cardinality")
    LIST_OF_ANNOTATION = hydra.core.Name("listOfAnnotation")
    SEMANTIC_ACTIONS = hydra.core.Name("SemanticActions")

@dataclass(frozen=True)
class TripleConstraint:
    sense_flags: Maybe[SenseFlags]
    predicate: Predicate
    inline_shape_expression: InlineShapeExpression
    cardinality: Maybe[Cardinality]
    list_of_annotation: frozenlist[Annotation]
    semantic_actions: SemanticActions

    TYPE_ = hydra.core.Name("hydra.shex.syntax.TripleConstraint")
    SENSE_FLAGS = hydra.core.Name("SenseFlags")
    PREDICATE = hydra.core.Name("Predicate")
    INLINE_SHAPE_EXPRESSION = hydra.core.Name("InlineShapeExpression")
    CARDINALITY = hydra.core.Name("Cardinality")
    LIST_OF_ANNOTATION = hydra.core.Name("listOfAnnotation")
    SEMANTIC_ACTIONS = hydra.core.Name("SemanticActions")

class CardinalityAst:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, CardinalityAst)
    def __hash__(self):
        return hash("CardinalityAst")

class CardinalityPlus:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, CardinalityPlus)
    def __hash__(self):
        return hash("CardinalityPlus")

class CardinalityQuest:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, CardinalityQuest)
    def __hash__(self):
        return hash("CardinalityQuest")

class CardinalityRepeatRange(Node["RepeatRange"]):
    ...

class _CardinalityMeta(type):
    def __getitem__(cls, item):
        return object

class Cardinality(metaclass=_CardinalityMeta):
    r"""CardinalityAst | CardinalityPlus | CardinalityQuest | CardinalityRepeatRange"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Cardinality")
    AST = hydra.core.Name("Ast")
    PLUS = hydra.core.Name("Plus")
    QUEST = hydra.core.Name("Quest")
    REPEAT_RANGE = hydra.core.Name("RepeatRange")

class SenseFlags(Node[None]):
    ...

SenseFlags.TYPE_ = hydra.core.Name("hydra.shex.syntax.SenseFlags")

class ValueSet(Node["frozenlist[ValueSetValue]"]):
    ...

ValueSet.TYPE_ = hydra.core.Name("hydra.shex.syntax.ValueSet")

class ValueSetValueIriRange(Node["IriRange"]):
    ...

class ValueSetValueLiteral(Node["Literal"]):
    ...

class _ValueSetValueMeta(type):
    def __getitem__(cls, item):
        return object

class ValueSetValue(metaclass=_ValueSetValueMeta):
    r"""ValueSetValueIriRange | ValueSetValueLiteral"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.ValueSetValue")
    IRI_RANGE = hydra.core.Name("IriRange")
    LITERAL = hydra.core.Name("Literal")

class IriRangeSequence(Node["IriRange_Sequence"]):
    ...

class IriRangeSequence2(Node["frozenlist[Exclusion]"]):
    ...

class _IriRangeMeta(type):
    def __getitem__(cls, item):
        return object

class IriRange(metaclass=_IriRangeMeta):
    r"""IriRangeSequence | IriRangeSequence2"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.IriRange")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class IriRange_Sequence:
    iri: Iri
    sequence: Maybe[frozenlist[Exclusion]]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.IriRange_Sequence")
    IRI = hydra.core.Name("Iri")
    SEQUENCE = hydra.core.Name("Sequence")

class Exclusion(Node["Iri"]):
    ...

Exclusion.TYPE_ = hydra.core.Name("hydra.shex.syntax.Exclusion")

class Include(Node["TripleExprLabel"]):
    ...

Include.TYPE_ = hydra.core.Name("hydra.shex.syntax.Include")

@dataclass(frozen=True)
class Annotation:
    predicate: Predicate
    alts: Annotation_Alts

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Annotation")
    PREDICATE = hydra.core.Name("Predicate")
    ALTS = hydra.core.Name("alts")

class Annotation_AltsIri(Node["Iri"]):
    ...

class Annotation_AltsLiteral(Node["Literal"]):
    ...

class _Annotation_AltsMeta(type):
    def __getitem__(cls, item):
        return object

class Annotation_Alts(metaclass=_Annotation_AltsMeta):
    r"""Annotation_AltsIri | Annotation_AltsLiteral"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Annotation_Alts")
    IRI = hydra.core.Name("Iri")
    LITERAL = hydra.core.Name("Literal")

class SemanticActions(Node["frozenlist[CodeDecl]"]):
    ...

SemanticActions.TYPE_ = hydra.core.Name("hydra.shex.syntax.SemanticActions")

@dataclass(frozen=True)
class CodeDecl:
    iri: Iri
    alts: CodeDecl_Alts

    TYPE_ = hydra.core.Name("hydra.shex.syntax.CodeDecl")
    IRI = hydra.core.Name("Iri")
    ALTS = hydra.core.Name("alts")

class CodeDecl_AltsCode(Node["Code"]):
    ...

class CodeDecl_AltsPercnt:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, CodeDecl_AltsPercnt)
    def __hash__(self):
        return hash("CodeDecl_AltsPercnt")

class _CodeDecl_AltsMeta(type):
    def __getitem__(cls, item):
        return object

class CodeDecl_Alts(metaclass=_CodeDecl_AltsMeta):
    r"""CodeDecl_AltsCode | CodeDecl_AltsPercnt"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.CodeDecl_Alts")
    CODE = hydra.core.Name("Code")
    PERCNT = hydra.core.Name("Percnt")

class LiteralRdfLiteral(Node["RdfLiteral"]):
    ...

class LiteralNumericLiteral(Node["NumericLiteral"]):
    ...

class LiteralBooleanLiteral(Node["BooleanLiteral"]):
    ...

class _LiteralMeta(type):
    def __getitem__(cls, item):
        return object

class Literal(metaclass=_LiteralMeta):
    r"""LiteralRdfLiteral | LiteralNumericLiteral | LiteralBooleanLiteral"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Literal")
    RDF_LITERAL = hydra.core.Name("RdfLiteral")
    NUMERIC_LITERAL = hydra.core.Name("NumericLiteral")
    BOOLEAN_LITERAL = hydra.core.Name("BooleanLiteral")

class PredicateIri(Node["Iri"]):
    ...

class PredicateRdfType(Node["RdfType"]):
    ...

class _PredicateMeta(type):
    def __getitem__(cls, item):
        return object

class Predicate(metaclass=_PredicateMeta):
    r"""PredicateIri | PredicateRdfType"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Predicate")
    IRI = hydra.core.Name("Iri")
    RDF_TYPE = hydra.core.Name("RdfType")

class Datatype(Node["Iri"]):
    ...

Datatype.TYPE_ = hydra.core.Name("hydra.shex.syntax.Datatype")

class ShapeExprLabelIri(Node["Iri"]):
    ...

class ShapeExprLabelBlankNode(Node["BlankNode"]):
    ...

class _ShapeExprLabelMeta(type):
    def __getitem__(cls, item):
        return object

class ShapeExprLabel(metaclass=_ShapeExprLabelMeta):
    r"""ShapeExprLabelIri | ShapeExprLabelBlankNode"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.ShapeExprLabel")
    IRI = hydra.core.Name("Iri")
    BLANK_NODE = hydra.core.Name("BlankNode")

class TripleExprLabelIri(Node["Iri"]):
    ...

class TripleExprLabelBlankNode(Node["BlankNode"]):
    ...

class _TripleExprLabelMeta(type):
    def __getitem__(cls, item):
        return object

class TripleExprLabel(metaclass=_TripleExprLabelMeta):
    r"""TripleExprLabelIri | TripleExprLabelBlankNode"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.TripleExprLabel")
    IRI = hydra.core.Name("Iri")
    BLANK_NODE = hydra.core.Name("BlankNode")

class NumericLiteralInteger(Node["Integer"]):
    ...

class NumericLiteralDecimal(Node["Decimal"]):
    ...

class NumericLiteralDouble(Node["Double"]):
    ...

class _NumericLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class NumericLiteral(metaclass=_NumericLiteralMeta):
    r"""NumericLiteralInteger | NumericLiteralDecimal | NumericLiteralDouble"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.NumericLiteral")
    INTEGER = hydra.core.Name("Integer")
    DECIMAL = hydra.core.Name("Decimal")
    DOUBLE = hydra.core.Name("Double")

@dataclass(frozen=True)
class RdfLiteral:
    string: String
    alts: Maybe[RdfLiteral_Alts_Option]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.RdfLiteral")
    STRING = hydra.core.Name("String")
    ALTS = hydra.core.Name("Alts")

class RdfLiteral_Alts_OptionLangTag(Node["LangTag"]):
    ...

class RdfLiteral_Alts_OptionSequence(Node["Datatype"]):
    ...

class _RdfLiteral_Alts_OptionMeta(type):
    def __getitem__(cls, item):
        return object

class RdfLiteral_Alts_Option(metaclass=_RdfLiteral_Alts_OptionMeta):
    r"""RdfLiteral_Alts_OptionLangTag | RdfLiteral_Alts_OptionSequence"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.RdfLiteral_Alts_Option")
    LANG_TAG = hydra.core.Name("LangTag")
    SEQUENCE = hydra.core.Name("sequence")

class BooleanLiteral(Enum):
    TRUE = hydra.core.Name("True")

    FALSE = hydra.core.Name("False")

BooleanLiteral.TYPE_ = hydra.core.Name("hydra.shex.syntax.BooleanLiteral")

class StringStringLiteral1(Node["StringLiteral1"]):
    ...

class StringStringLiteralLong1(Node["StringLiteralLong1"]):
    ...

class StringStringLiteral2(Node["StringLiteral2"]):
    ...

class StringStringLiteralLong2(Node["StringLiteralLong2"]):
    ...

class _StringMeta(type):
    def __getitem__(cls, item):
        return object

class String(metaclass=_StringMeta):
    r"""StringStringLiteral1 | StringStringLiteralLong1 | StringStringLiteral2 | StringStringLiteralLong2"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.String")
    STRING_LITERAL1 = hydra.core.Name("StringLiteral1")
    STRING_LITERAL_LONG1 = hydra.core.Name("StringLiteralLong1")
    STRING_LITERAL2 = hydra.core.Name("StringLiteral2")
    STRING_LITERAL_LONG2 = hydra.core.Name("StringLiteralLong2")

class IriIriRef(Node["IriRef"]):
    ...

class IriPrefixedName(Node["PrefixedName"]):
    ...

class _IriMeta(type):
    def __getitem__(cls, item):
        return object

class Iri(metaclass=_IriMeta):
    r"""IriIriRef | IriPrefixedName"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Iri")
    IRI_REF = hydra.core.Name("IriRef")
    PREFIXED_NAME = hydra.core.Name("PrefixedName")

class PrefixedNamePnameLn(Node["PnameLn"]):
    ...

class PrefixedNamePnameNs(Node["PnameNs"]):
    ...

class _PrefixedNameMeta(type):
    def __getitem__(cls, item):
        return object

class PrefixedName(metaclass=_PrefixedNameMeta):
    r"""PrefixedNamePnameLn | PrefixedNamePnameNs"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PrefixedName")
    PNAME_LN = hydra.core.Name("PnameLn")
    PNAME_NS = hydra.core.Name("PnameNs")

class BlankNode(Node["BlankNodeLabel"]):
    ...

BlankNode.TYPE_ = hydra.core.Name("hydra.shex.syntax.BlankNode")

class IncludeSet(Node["frozenlist[ShapeExprLabel]"]):
    ...

IncludeSet.TYPE_ = hydra.core.Name("hydra.shex.syntax.IncludeSet")

class Code(Node["frozenlist[Code_Elmt]"]):
    ...

Code.TYPE_ = hydra.core.Name("hydra.shex.syntax.Code")

class Code_ElmtRegex(Node[str]):
    ...

class Code_ElmtSequence(Node[str]):
    ...

class Code_ElmtUchar(Node["Uchar"]):
    ...

class _Code_ElmtMeta(type):
    def __getitem__(cls, item):
        return object

class Code_Elmt(metaclass=_Code_ElmtMeta):
    r"""Code_ElmtRegex | Code_ElmtSequence | Code_ElmtUchar"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Code_Elmt")
    REGEX = hydra.core.Name("regex")
    SEQUENCE = hydra.core.Name("sequence")
    UCHAR = hydra.core.Name("Uchar")

@dataclass(frozen=True)
class RepeatRange:
    integer: Integer
    sequence: Maybe[Maybe[Maybe[RepeatRange_Sequence_Option_Option_Option]]]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.RepeatRange")
    INTEGER = hydra.core.Name("Integer")
    SEQUENCE = hydra.core.Name("Sequence")

class RepeatRange_Sequence_Option_Option_OptionInteger(Node["Integer"]):
    ...

class RepeatRange_Sequence_Option_Option_OptionAst:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, RepeatRange_Sequence_Option_Option_OptionAst)
    def __hash__(self):
        return hash("RepeatRange_Sequence_Option_Option_OptionAst")

class _RepeatRange_Sequence_Option_Option_OptionMeta(type):
    def __getitem__(cls, item):
        return object

class RepeatRange_Sequence_Option_Option_Option(metaclass=_RepeatRange_Sequence_Option_Option_OptionMeta):
    r"""RepeatRange_Sequence_Option_Option_OptionInteger | RepeatRange_Sequence_Option_Option_OptionAst"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.RepeatRange_Sequence_Option_Option_Option")
    INTEGER = hydra.core.Name("Integer")
    AST = hydra.core.Name("Ast")

class RdfType(Node[None]):
    ...

RdfType.TYPE_ = hydra.core.Name("hydra.shex.syntax.RdfType")

class IriRef(Node["frozenlist[IriRef_Elmt]"]):
    ...

IriRef.TYPE_ = hydra.core.Name("hydra.shex.syntax.IriRef")

class IriRef_ElmtRegex(Node[str]):
    ...

class IriRef_ElmtUchar(Node["Uchar"]):
    ...

class _IriRef_ElmtMeta(type):
    def __getitem__(cls, item):
        return object

class IriRef_Elmt(metaclass=_IriRef_ElmtMeta):
    r"""IriRef_ElmtRegex | IriRef_ElmtUchar"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.IriRef_Elmt")
    REGEX = hydra.core.Name("regex")
    UCHAR = hydra.core.Name("Uchar")

class PnameNs(Node["Maybe[PnPrefix]"]):
    ...

PnameNs.TYPE_ = hydra.core.Name("hydra.shex.syntax.PnameNs")

@dataclass(frozen=True)
class PnameLn:
    pname_ns: PnameNs
    pn_local: PnLocal

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PnameLn")
    PNAME_NS = hydra.core.Name("PnameNs")
    PN_LOCAL = hydra.core.Name("PnLocal")

class AtpNameNs(Node["Maybe[PnPrefix]"]):
    ...

AtpNameNs.TYPE_ = hydra.core.Name("hydra.shex.syntax.AtpNameNs")

@dataclass(frozen=True)
class AtpNameLn:
    pname_ns: PnameNs
    pn_local: PnLocal

    TYPE_ = hydra.core.Name("hydra.shex.syntax.AtpNameLn")
    PNAME_NS = hydra.core.Name("PnameNs")
    PN_LOCAL = hydra.core.Name("PnLocal")

@dataclass(frozen=True)
class Regexp:
    list_of_alts: frozenlist[Regexp_ListOfAlts_Elmt]
    list_of_regex: frozenlist[str]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Regexp")
    LIST_OF_ALTS = hydra.core.Name("listOfAlts")
    LIST_OF_REGEX = hydra.core.Name("listOfRegex")

class Regexp_ListOfAlts_ElmtRegex(Node[str]):
    ...

class Regexp_ListOfAlts_ElmtSequence(Node[str]):
    ...

class Regexp_ListOfAlts_ElmtUchar(Node["Uchar"]):
    ...

class _Regexp_ListOfAlts_ElmtMeta(type):
    def __getitem__(cls, item):
        return object

class Regexp_ListOfAlts_Elmt(metaclass=_Regexp_ListOfAlts_ElmtMeta):
    r"""Regexp_ListOfAlts_ElmtRegex | Regexp_ListOfAlts_ElmtSequence | Regexp_ListOfAlts_ElmtUchar"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Regexp_ListOfAlts_Elmt")
    REGEX = hydra.core.Name("regex")
    SEQUENCE = hydra.core.Name("sequence")
    UCHAR = hydra.core.Name("Uchar")

@dataclass(frozen=True)
class BlankNodeLabel:
    alts: BlankNodeLabel_Alts
    list_of_alts: Maybe[frozenlist[BlankNodeLabel_ListOfAlts_Option_Elmt]]
    pn_chars: PnChars

    TYPE_ = hydra.core.Name("hydra.shex.syntax.BlankNodeLabel")
    ALTS = hydra.core.Name("alts")
    LIST_OF_ALTS = hydra.core.Name("ListOfAlts")
    PN_CHARS = hydra.core.Name("PnChars")

class BlankNodeLabel_AltsPnCharsU(Node["PnCharsU"]):
    ...

class BlankNodeLabel_AltsRegex(Node[str]):
    ...

class _BlankNodeLabel_AltsMeta(type):
    def __getitem__(cls, item):
        return object

class BlankNodeLabel_Alts(metaclass=_BlankNodeLabel_AltsMeta):
    r"""BlankNodeLabel_AltsPnCharsU | BlankNodeLabel_AltsRegex"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.BlankNodeLabel_Alts")
    PN_CHARS_U = hydra.core.Name("PnCharsU")
    REGEX = hydra.core.Name("regex")

class BlankNodeLabel_ListOfAlts_Option_ElmtPnChars(Node["PnChars"]):
    ...

class BlankNodeLabel_ListOfAlts_Option_ElmtPeriod:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, BlankNodeLabel_ListOfAlts_Option_ElmtPeriod)
    def __hash__(self):
        return hash("BlankNodeLabel_ListOfAlts_Option_ElmtPeriod")

class _BlankNodeLabel_ListOfAlts_Option_ElmtMeta(type):
    def __getitem__(cls, item):
        return object

class BlankNodeLabel_ListOfAlts_Option_Elmt(metaclass=_BlankNodeLabel_ListOfAlts_Option_ElmtMeta):
    r"""BlankNodeLabel_ListOfAlts_Option_ElmtPnChars | BlankNodeLabel_ListOfAlts_Option_ElmtPeriod"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt")
    PN_CHARS = hydra.core.Name("PnChars")
    PERIOD = hydra.core.Name("Period")

class LangTag(Node[str]):
    ...

LangTag.TYPE_ = hydra.core.Name("hydra.shex.syntax.LangTag")

class Integer(Node[str]):
    ...

Integer.TYPE_ = hydra.core.Name("hydra.shex.syntax.Integer")

class Decimal(Node[str]):
    ...

Decimal.TYPE_ = hydra.core.Name("hydra.shex.syntax.Decimal")

class Double(Node[str]):
    ...

Double.TYPE_ = hydra.core.Name("hydra.shex.syntax.Double")

class StringLiteral1(Node["frozenlist[StringLiteral1_Elmt]"]):
    ...

StringLiteral1.TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteral1")

class StringLiteral1_ElmtRegex(Node[str]):
    ...

class StringLiteral1_ElmtEchar(Node["Echar"]):
    ...

class StringLiteral1_ElmtUchar(Node["Uchar"]):
    ...

class _StringLiteral1_ElmtMeta(type):
    def __getitem__(cls, item):
        return object

class StringLiteral1_Elmt(metaclass=_StringLiteral1_ElmtMeta):
    r"""StringLiteral1_ElmtRegex | StringLiteral1_ElmtEchar | StringLiteral1_ElmtUchar"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteral1_Elmt")
    REGEX = hydra.core.Name("regex")
    ECHAR = hydra.core.Name("Echar")
    UCHAR = hydra.core.Name("Uchar")

class StringLiteral2(Node["frozenlist[StringLiteral2_Elmt]"]):
    ...

StringLiteral2.TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteral2")

class StringLiteral2_ElmtRegex(Node[str]):
    ...

class StringLiteral2_ElmtEchar(Node["Echar"]):
    ...

class StringLiteral2_ElmtUchar(Node["Uchar"]):
    ...

class _StringLiteral2_ElmtMeta(type):
    def __getitem__(cls, item):
        return object

class StringLiteral2_Elmt(metaclass=_StringLiteral2_ElmtMeta):
    r"""StringLiteral2_ElmtRegex | StringLiteral2_ElmtEchar | StringLiteral2_ElmtUchar"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteral2_Elmt")
    REGEX = hydra.core.Name("regex")
    ECHAR = hydra.core.Name("Echar")
    UCHAR = hydra.core.Name("Uchar")

class StringLiteralLong1(Node["frozenlist[StringLiteralLong1_Elmt]"]):
    ...

StringLiteralLong1.TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteralLong1")

class StringLiteralLong1_ElmtSequence(Node["StringLiteralLong1_Elmt_Sequence"]):
    ...

class StringLiteralLong1_ElmtEchar(Node["Echar"]):
    ...

class StringLiteralLong1_ElmtUchar(Node["Uchar"]):
    ...

class _StringLiteralLong1_ElmtMeta(type):
    def __getitem__(cls, item):
        return object

class StringLiteralLong1_Elmt(metaclass=_StringLiteralLong1_ElmtMeta):
    r"""StringLiteralLong1_ElmtSequence | StringLiteralLong1_ElmtEchar | StringLiteralLong1_ElmtUchar"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteralLong1_Elmt")
    SEQUENCE = hydra.core.Name("sequence")
    ECHAR = hydra.core.Name("Echar")
    UCHAR = hydra.core.Name("Uchar")

@dataclass(frozen=True)
class StringLiteralLong1_Elmt_Sequence:
    alts: Maybe[StringLiteralLong1_Elmt_Sequence_Alts_Option]
    regex: str

    TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteralLong1_Elmt_Sequence")
    ALTS = hydra.core.Name("Alts")
    REGEX = hydra.core.Name("regex")

class StringLiteralLong1_Elmt_Sequence_Alts_OptionApos:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, StringLiteralLong1_Elmt_Sequence_Alts_OptionApos)
    def __hash__(self):
        return hash("StringLiteralLong1_Elmt_Sequence_Alts_OptionApos")

class StringLiteralLong1_Elmt_Sequence_Alts_OptionSequence(Node["StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence"]):
    ...

class _StringLiteralLong1_Elmt_Sequence_Alts_OptionMeta(type):
    def __getitem__(cls, item):
        return object

class StringLiteralLong1_Elmt_Sequence_Alts_Option(metaclass=_StringLiteralLong1_Elmt_Sequence_Alts_OptionMeta):
    r"""StringLiteralLong1_Elmt_Sequence_Alts_OptionApos | StringLiteralLong1_Elmt_Sequence_Alts_OptionSequence"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option")
    APOS = hydra.core.Name("Apos")
    SEQUENCE = hydra.core.Name("sequence")

@dataclass(frozen=True)
class StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence:
    TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence")

class StringLiteralLong2(Node["frozenlist[StringLiteralLong2_Elmt]"]):
    ...

StringLiteralLong2.TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteralLong2")

class StringLiteralLong2_ElmtSequence(Node["StringLiteralLong2_Elmt_Sequence"]):
    ...

class StringLiteralLong2_ElmtEchar(Node["Echar"]):
    ...

class StringLiteralLong2_ElmtUchar(Node["Uchar"]):
    ...

class _StringLiteralLong2_ElmtMeta(type):
    def __getitem__(cls, item):
        return object

class StringLiteralLong2_Elmt(metaclass=_StringLiteralLong2_ElmtMeta):
    r"""StringLiteralLong2_ElmtSequence | StringLiteralLong2_ElmtEchar | StringLiteralLong2_ElmtUchar"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteralLong2_Elmt")
    SEQUENCE = hydra.core.Name("sequence")
    ECHAR = hydra.core.Name("Echar")
    UCHAR = hydra.core.Name("Uchar")

@dataclass(frozen=True)
class StringLiteralLong2_Elmt_Sequence:
    alts: Maybe[StringLiteralLong2_Elmt_Sequence_Alts_Option]
    regex: str

    TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteralLong2_Elmt_Sequence")
    ALTS = hydra.core.Name("Alts")
    REGEX = hydra.core.Name("regex")

class StringLiteralLong2_Elmt_Sequence_Alts_OptionQuot:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, StringLiteralLong2_Elmt_Sequence_Alts_OptionQuot)
    def __hash__(self):
        return hash("StringLiteralLong2_Elmt_Sequence_Alts_OptionQuot")

class StringLiteralLong2_Elmt_Sequence_Alts_OptionSequence(Node["StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence"]):
    ...

class _StringLiteralLong2_Elmt_Sequence_Alts_OptionMeta(type):
    def __getitem__(cls, item):
        return object

class StringLiteralLong2_Elmt_Sequence_Alts_Option(metaclass=_StringLiteralLong2_Elmt_Sequence_Alts_OptionMeta):
    r"""StringLiteralLong2_Elmt_Sequence_Alts_OptionQuot | StringLiteralLong2_Elmt_Sequence_Alts_OptionSequence"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option")
    QUOT = hydra.core.Name("Quot")
    SEQUENCE = hydra.core.Name("sequence")

@dataclass(frozen=True)
class StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence:
    TYPE_ = hydra.core.Name("hydra.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence")

class UcharSequence(Node["Uchar_Sequence"]):
    ...

class UcharSequence2(Node["Uchar_Sequence2"]):
    ...

class _UcharMeta(type):
    def __getitem__(cls, item):
        return object

class Uchar(metaclass=_UcharMeta):
    r"""UcharSequence | UcharSequence2"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Uchar")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class Uchar_Sequence:
    hex: Hex
    hex2: Hex
    hex3: Hex
    hex4: Hex

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Uchar_Sequence")
    HEX = hydra.core.Name("Hex")
    HEX2 = hydra.core.Name("Hex2")
    HEX3 = hydra.core.Name("Hex3")
    HEX4 = hydra.core.Name("Hex4")

@dataclass(frozen=True)
class Uchar_Sequence2:
    hex: Hex
    hex2: Hex
    hex3: Hex
    hex4: Hex
    hex5: Hex
    hex6: Hex
    hex7: Hex
    hex8: Hex

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Uchar_Sequence2")
    HEX = hydra.core.Name("Hex")
    HEX2 = hydra.core.Name("Hex2")
    HEX3 = hydra.core.Name("Hex3")
    HEX4 = hydra.core.Name("Hex4")
    HEX5 = hydra.core.Name("Hex5")
    HEX6 = hydra.core.Name("Hex6")
    HEX7 = hydra.core.Name("Hex7")
    HEX8 = hydra.core.Name("Hex8")

class Echar(Node[str]):
    ...

Echar.TYPE_ = hydra.core.Name("hydra.shex.syntax.Echar")

class PnCharsBaseRegex(Node[str]):
    ...

class PnCharsBaseRegex2(Node[str]):
    ...

class _PnCharsBaseMeta(type):
    def __getitem__(cls, item):
        return object

class PnCharsBase(metaclass=_PnCharsBaseMeta):
    r"""PnCharsBaseRegex | PnCharsBaseRegex2"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PnCharsBase")
    REGEX = hydra.core.Name("regex")
    REGEX2 = hydra.core.Name("regex2")

class PnCharsUPnCharsBase(Node["PnCharsBase"]):
    ...

class PnCharsULowbar:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PnCharsULowbar)
    def __hash__(self):
        return hash("PnCharsULowbar")

class _PnCharsUMeta(type):
    def __getitem__(cls, item):
        return object

class PnCharsU(metaclass=_PnCharsUMeta):
    r"""PnCharsUPnCharsBase | PnCharsULowbar"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PnCharsU")
    PN_CHARS_BASE = hydra.core.Name("PnCharsBase")
    LOWBAR = hydra.core.Name("Lowbar")

class PnCharsPnCharsU(Node["PnCharsU"]):
    ...

class PnCharsMinus:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PnCharsMinus)
    def __hash__(self):
        return hash("PnCharsMinus")

class PnCharsRegex(Node[str]):
    ...

class _PnCharsMeta(type):
    def __getitem__(cls, item):
        return object

class PnChars(metaclass=_PnCharsMeta):
    r"""PnCharsPnCharsU | PnCharsMinus | PnCharsRegex"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PnChars")
    PN_CHARS_U = hydra.core.Name("PnCharsU")
    MINUS = hydra.core.Name("Minus")
    REGEX = hydra.core.Name("regex")

@dataclass(frozen=True)
class PnPrefix:
    pn_chars_base: PnCharsBase
    sequence: Maybe[PnPrefix_Sequence_Option]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PnPrefix")
    PN_CHARS_BASE = hydra.core.Name("PnCharsBase")
    SEQUENCE = hydra.core.Name("Sequence")

@dataclass(frozen=True)
class PnPrefix_Sequence_Option:
    alts: PnPrefix_Sequence_Option_Alts
    pn_chars: PnChars

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PnPrefix_Sequence_Option")
    ALTS = hydra.core.Name("alts")
    PN_CHARS = hydra.core.Name("PnChars")

class PnPrefix_Sequence_Option_AltsPnChars(Node["PnChars"]):
    ...

class PnPrefix_Sequence_Option_AltsPeriod:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PnPrefix_Sequence_Option_AltsPeriod)
    def __hash__(self):
        return hash("PnPrefix_Sequence_Option_AltsPeriod")

class _PnPrefix_Sequence_Option_AltsMeta(type):
    def __getitem__(cls, item):
        return object

class PnPrefix_Sequence_Option_Alts(metaclass=_PnPrefix_Sequence_Option_AltsMeta):
    r"""PnPrefix_Sequence_Option_AltsPnChars | PnPrefix_Sequence_Option_AltsPeriod"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PnPrefix_Sequence_Option_Alts")
    PN_CHARS = hydra.core.Name("PnChars")
    PERIOD = hydra.core.Name("Period")

@dataclass(frozen=True)
class PnLocal:
    alts: PnLocal_Alts
    sequence: Maybe[PnLocal_Sequence_Option]

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PnLocal")
    ALTS = hydra.core.Name("alts")
    SEQUENCE = hydra.core.Name("Sequence")

class PnLocal_AltsPnCharsU(Node["PnCharsU"]):
    ...

class PnLocal_AltsColon:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PnLocal_AltsColon)
    def __hash__(self):
        return hash("PnLocal_AltsColon")

class PnLocal_AltsRegex(Node[str]):
    ...

class PnLocal_AltsPlx(Node["Plx"]):
    ...

class _PnLocal_AltsMeta(type):
    def __getitem__(cls, item):
        return object

class PnLocal_Alts(metaclass=_PnLocal_AltsMeta):
    r"""PnLocal_AltsPnCharsU | PnLocal_AltsColon | PnLocal_AltsRegex | PnLocal_AltsPlx"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PnLocal_Alts")
    PN_CHARS_U = hydra.core.Name("PnCharsU")
    COLON = hydra.core.Name("Colon")
    REGEX = hydra.core.Name("regex")
    PLX = hydra.core.Name("Plx")

@dataclass(frozen=True)
class PnLocal_Sequence_Option:
    list_of_alts: frozenlist[PnLocal_Sequence_Option_ListOfAlts_Elmt]
    alts: PnLocal_Sequence_Option_Alts

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PnLocal_Sequence_Option")
    LIST_OF_ALTS = hydra.core.Name("listOfAlts")
    ALTS = hydra.core.Name("alts")

class PnLocal_Sequence_Option_ListOfAlts_ElmtPnChars(Node["PnChars"]):
    ...

class PnLocal_Sequence_Option_ListOfAlts_ElmtPeriod:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PnLocal_Sequence_Option_ListOfAlts_ElmtPeriod)
    def __hash__(self):
        return hash("PnLocal_Sequence_Option_ListOfAlts_ElmtPeriod")

class PnLocal_Sequence_Option_ListOfAlts_ElmtColon:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PnLocal_Sequence_Option_ListOfAlts_ElmtColon)
    def __hash__(self):
        return hash("PnLocal_Sequence_Option_ListOfAlts_ElmtColon")

class PnLocal_Sequence_Option_ListOfAlts_ElmtPlx(Node["Plx"]):
    ...

class _PnLocal_Sequence_Option_ListOfAlts_ElmtMeta(type):
    def __getitem__(cls, item):
        return object

class PnLocal_Sequence_Option_ListOfAlts_Elmt(metaclass=_PnLocal_Sequence_Option_ListOfAlts_ElmtMeta):
    r"""PnLocal_Sequence_Option_ListOfAlts_ElmtPnChars | PnLocal_Sequence_Option_ListOfAlts_ElmtPeriod | PnLocal_Sequence_Option_ListOfAlts_ElmtColon | PnLocal_Sequence_Option_ListOfAlts_ElmtPlx"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt")
    PN_CHARS = hydra.core.Name("PnChars")
    PERIOD = hydra.core.Name("Period")
    COLON = hydra.core.Name("Colon")
    PLX = hydra.core.Name("Plx")

class PnLocal_Sequence_Option_AltsPnChars(Node["PnChars"]):
    ...

class PnLocal_Sequence_Option_AltsColon:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PnLocal_Sequence_Option_AltsColon)
    def __hash__(self):
        return hash("PnLocal_Sequence_Option_AltsColon")

class PnLocal_Sequence_Option_AltsPlx(Node["Plx"]):
    ...

class _PnLocal_Sequence_Option_AltsMeta(type):
    def __getitem__(cls, item):
        return object

class PnLocal_Sequence_Option_Alts(metaclass=_PnLocal_Sequence_Option_AltsMeta):
    r"""PnLocal_Sequence_Option_AltsPnChars | PnLocal_Sequence_Option_AltsColon | PnLocal_Sequence_Option_AltsPlx"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.PnLocal_Sequence_Option_Alts")
    PN_CHARS = hydra.core.Name("PnChars")
    COLON = hydra.core.Name("Colon")
    PLX = hydra.core.Name("Plx")

class PlxPercent(Node["Percent"]):
    ...

class PlxPnLocalEsc(Node["PnLocalEsc"]):
    ...

class _PlxMeta(type):
    def __getitem__(cls, item):
        return object

class Plx(metaclass=_PlxMeta):
    r"""PlxPercent | PlxPnLocalEsc"""

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Plx")
    PERCENT = hydra.core.Name("Percent")
    PN_LOCAL_ESC = hydra.core.Name("PnLocalEsc")

@dataclass(frozen=True)
class Percent:
    hex: Hex
    hex2: Hex

    TYPE_ = hydra.core.Name("hydra.shex.syntax.Percent")
    HEX = hydra.core.Name("Hex")
    HEX2 = hydra.core.Name("Hex2")

class Hex(Node[str]):
    ...

Hex.TYPE_ = hydra.core.Name("hydra.shex.syntax.Hex")

class PnLocalEsc(Node[str]):
    ...

PnLocalEsc.TYPE_ = hydra.core.Name("hydra.shex.syntax.PnLocalEsc")
