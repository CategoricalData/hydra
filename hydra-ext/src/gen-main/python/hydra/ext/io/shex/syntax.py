# Note: this is an automatically generated file. Do not edit.

r"""A Shex model. Based on the BNF at:
  https://github.com/shexSpec/grammar/blob/master/bnf."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.core

annotation = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("Predicate"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Predicate")))), hydra.core.FieldType(hydra.core.Name("alts"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Annotation_Alts")))))))

annotation_alts = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("Iri"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Iri")))), hydra.core.FieldType(hydra.core.Name("Literal"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Literal")))))))

atp_name_ln = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("PnameNs"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnameNs")))), hydra.core.FieldType(hydra.core.Name("PnLocal"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal")))))))

atp_name_ns = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnPrefix")))))))

base_decl = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.IriRef")))))

blank_node = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.BlankNodeLabel")))))

blank_node_label = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("alts"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.BlankNodeLabel_Alts")))), hydra.core.FieldType(hydra.core.Name("ListOfAlts"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt")))))))), hydra.core.FieldType(hydra.core.Name("PnChars"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnChars")))))))

blank_node_label_alts = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("PnCharsU"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnCharsU")))), hydra.core.FieldType(hydra.core.Name("regex"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))))

blank_node_label_list_of_alts_option_elmt = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("PnChars"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnChars")))), hydra.core.FieldType(hydra.core.Name("Period"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

boolean_literal = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("True"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("False"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

bracketed_triple_expr = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("InnerTripleExpr"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.InnerTripleExpr")))), hydra.core.FieldType(hydra.core.Name("Cardinality"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Cardinality")))))), hydra.core.FieldType(hydra.core.Name("listOfAnnotation"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Annotation")))))), hydra.core.FieldType(hydra.core.Name("SemanticActions"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.SemanticActions")))))))

cardinality = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("Ast"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("Plus"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("Quest"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("RepeatRange"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.RepeatRange")))))))

code = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Code_Elmt")))))))

code_decl = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("Iri"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Iri")))), hydra.core.FieldType(hydra.core.Name("alts"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.CodeDecl_Alts")))))))

code_decl_alts = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("Code"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Code")))), hydra.core.FieldType(hydra.core.Name("Percnt"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

code_elmt = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("regex"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("Uchar"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))))))

datatype = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Iri")))))

decimal = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))

directive = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("BaseDecl"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.BaseDecl")))), hydra.core.FieldType(hydra.core.Name("PrefixDecl"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PrefixDecl")))))))

double = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))

echar = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))

exclusion = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Iri")))))

extra_property_set = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Predicate")))))))

group_triple_expr = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("SingleElementGroup"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.SingleElementGroup")))), hydra.core.FieldType(hydra.core.Name("MultiElementGroup"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.MultiElementGroup")))))))

hex = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))

include = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.TripleExprLabel")))))

include_set = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExprLabel")))))))

inline_shape_and = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("InlineShapeNot"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeNot")))), hydra.core.FieldType(hydra.core.Name("listOfSequence"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeNot")))))))))

inline_shape_atom = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeAtom_Sequence")))), hydra.core.FieldType(hydra.core.Name("sequence2"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeAtom_Sequence2")))), hydra.core.FieldType(hydra.core.Name("sequence3"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExpression")))), hydra.core.FieldType(hydra.core.Name("Period"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

inline_shape_atom_sequence = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("NodeConstraint"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint")))), hydra.core.FieldType(hydra.core.Name("InlineShapeOrRef"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeOrRef")))))))))

inline_shape_atom_sequence2 = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("InlineShapeOrRef"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeOrRef")))), hydra.core.FieldType(hydra.core.Name("NodeConstraint"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint")))))))))

inline_shape_definition = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("listOfAlts"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeDefinition_ListOfAlts_Elmt")))))), hydra.core.FieldType(hydra.core.Name("TripleExpression"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.TripleExpression")))))))))

inline_shape_definition_list_of_alts_elmt = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("IncludeSet"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.IncludeSet")))), hydra.core.FieldType(hydra.core.Name("ExtraPropertySet"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ExtraPropertySet")))), hydra.core.FieldType(hydra.core.Name("CLOSED"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

inline_shape_expression = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeOr")))))

inline_shape_not = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("NOT"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeUnit())))), hydra.core.FieldType(hydra.core.Name("InlineShapeAtom"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeAtom")))))))

inline_shape_or = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("ShapeAnd"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeAnd")))), hydra.core.FieldType(hydra.core.Name("listOfSequence"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeAnd")))))))))

inline_shape_or_ref = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("InlineShapeDefinition"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeDefinition")))), hydra.core.FieldType(hydra.core.Name("AtpNameLn"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.AtpNameLn")))), hydra.core.FieldType(hydra.core.Name("AtpNameNs"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.AtpNameNs")))), hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExprLabel")))))))

inner_triple_expr = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("MultiElementGroup"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.MultiElementGroup")))), hydra.core.FieldType(hydra.core.Name("MultiElementOneOf"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.MultiElementOneOf")))))))

integer = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))

iri = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("IriRef"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.IriRef")))), hydra.core.FieldType(hydra.core.Name("PrefixedName"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PrefixedName")))))))

iri_range = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.IriRange_Sequence")))), hydra.core.FieldType(hydra.core.Name("sequence2"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Exclusion")))))))))

iri_range_sequence = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("Iri"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Iri")))), hydra.core.FieldType(hydra.core.Name("Sequence"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Exclusion")))))))))))

iri_ref = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.IriRef_Elmt")))))))

iri_ref_elmt = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("regex"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("Uchar"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))))))

lang_tag = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))

literal = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("RdfLiteral"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.RdfLiteral")))), hydra.core.FieldType(hydra.core.Name("NumericLiteral"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NumericLiteral")))), hydra.core.FieldType(hydra.core.Name("BooleanLiteral"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.BooleanLiteral")))))))

multi_element_group = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("UnaryTripleExpr"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.UnaryTripleExpr")))), hydra.core.FieldType(hydra.core.Name("listOfSequence"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.UnaryTripleExpr")))))), hydra.core.FieldType(hydra.core.Name("Semi"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeUnit())))))))

multi_element_one_of = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("GroupTripleExpr"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.GroupTripleExpr")))), hydra.core.FieldType(hydra.core.Name("listOfSequence"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.GroupTripleExpr")))))))))

node_constraint = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.XsFacet")))))), hydra.core.FieldType(hydra.core.Name("sequence2"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint_Sequence2")))), hydra.core.FieldType(hydra.core.Name("sequence3"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint_Sequence3")))), hydra.core.FieldType(hydra.core.Name("sequence4"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint_Sequence4")))), hydra.core.FieldType(hydra.core.Name("sequence5"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint_Sequence5")))), hydra.core.FieldType(hydra.core.Name("listOfXsFacet"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.XsFacet")))))))))

node_constraint_sequence2 = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("NonLiteralKind"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NonLiteralKind")))), hydra.core.FieldType(hydra.core.Name("listOfStringFacet"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringFacet")))))))))

node_constraint_sequence3 = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("Datatype"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Datatype")))), hydra.core.FieldType(hydra.core.Name("listOfXsFacet"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.XsFacet")))))))))

node_constraint_sequence4 = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("ValueSet"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ValueSet")))), hydra.core.FieldType(hydra.core.Name("listOfXsFacet"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.XsFacet")))))))))

node_constraint_sequence5 = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("ValueSet"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ValueSet")))), hydra.core.FieldType(hydra.core.Name("listOfXsFacet"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.XsFacet")))))))))

non_literal_kind = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("IRI"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("BNODE"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("NONLITERAL"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

not_start_action = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("start"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExpression")))), hydra.core.FieldType(hydra.core.Name("shapeExprDecl"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl")))))))

not_start_action_shape_expr_decl = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("ShapeExprLabel"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExprLabel")))), hydra.core.FieldType(hydra.core.Name("alts"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl_Alts")))))))

not_start_action_shape_expr_decl_alts = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("ShapeExpression"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExpression")))), hydra.core.FieldType(hydra.core.Name("EXTERNAL"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

numeric_facet = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NumericFacet_Sequence")))), hydra.core.FieldType(hydra.core.Name("sequence2"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NumericFacet_Sequence2")))))))

numeric_facet_sequence = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("NumericRange"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NumericRange")))), hydra.core.FieldType(hydra.core.Name("NumericLiteral"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NumericLiteral")))))))

numeric_facet_sequence2 = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("NumericLength"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NumericLength")))), hydra.core.FieldType(hydra.core.Name("Integer"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Integer")))))))

numeric_length = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("TOTALDIGITS"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("FRACTIONDIGITS"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

numeric_literal = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("Integer"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Integer")))), hydra.core.FieldType(hydra.core.Name("Decimal"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Decimal")))), hydra.core.FieldType(hydra.core.Name("Double"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Double")))))))

numeric_range = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("MININCLUSIVE"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("MINEXCLUSIVE"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("MAXINCLUSIVE"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("MAXEXCLUSIVE"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

one_of_triple_expr = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("GroupTripleExpr"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.GroupTripleExpr")))), hydra.core.FieldType(hydra.core.Name("MultiElementOneOf"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.MultiElementOneOf")))))))

percent = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("Hex"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))), hydra.core.FieldType(hydra.core.Name("Hex2"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))))))

plx = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("Percent"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Percent")))), hydra.core.FieldType(hydra.core.Name("PnLocalEsc"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnLocalEsc")))))))

pn_chars = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("PnCharsU"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnCharsU")))), hydra.core.FieldType(hydra.core.Name("Minus"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("regex"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))))

pn_chars_base = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("regex"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("regex2"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))))

pn_chars_u = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("PnCharsBase"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnCharsBase")))), hydra.core.FieldType(hydra.core.Name("Lowbar"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

pn_local = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("alts"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal_Alts")))), hydra.core.FieldType(hydra.core.Name("Sequence"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal_Sequence_Option")))))))))

pn_local_esc = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))

pn_local_alts = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("PnCharsU"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnCharsU")))), hydra.core.FieldType(hydra.core.Name("Colon"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("regex"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("Plx"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Plx")))))))

pn_local_sequence_option = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("listOfAlts"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt")))))), hydra.core.FieldType(hydra.core.Name("alts"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_Alts")))))))

pn_local_sequence_option_alts = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("PnChars"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnChars")))), hydra.core.FieldType(hydra.core.Name("Colon"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("Plx"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Plx")))))))

pn_local_sequence_option_list_of_alts_elmt = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("PnChars"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnChars")))), hydra.core.FieldType(hydra.core.Name("Period"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("Colon"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("Plx"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Plx")))))))

pn_prefix = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("PnCharsBase"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnCharsBase")))), hydra.core.FieldType(hydra.core.Name("Sequence"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option")))))))))

pn_prefix_sequence_option = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("alts"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option_Alts")))), hydra.core.FieldType(hydra.core.Name("PnChars"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnChars")))))))

pn_prefix_sequence_option_alts = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("PnChars"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnChars")))), hydra.core.FieldType(hydra.core.Name("Period"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

pname_ln = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("PnameNs"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnameNs")))), hydra.core.FieldType(hydra.core.Name("PnLocal"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal")))))))

pname_ns = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnPrefix")))))))

predicate = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("Iri"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Iri")))), hydra.core.FieldType(hydra.core.Name("RdfType"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.RdfType")))))))

prefix_decl = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("PnameNs"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnameNs")))), hydra.core.FieldType(hydra.core.Name("IriRef"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.IriRef")))))))

prefixed_name = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("PnameLn"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnameLn")))), hydra.core.FieldType(hydra.core.Name("PnameNs"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PnameNs")))))))

rdf_literal = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("String"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.String")))), hydra.core.FieldType(hydra.core.Name("Alts"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.RdfLiteral_Alts_Option")))))))))

rdf_literal_alts_option = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("LangTag"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.LangTag")))), hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Datatype")))))))

rdf_type = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeUnit())))

regexp = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("listOfAlts"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Regexp_ListOfAlts_Elmt")))))), hydra.core.FieldType(hydra.core.Name("listOfRegex"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))))))

regexp_list_of_alts_elmt = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("regex"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("Uchar"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))))))

repeat_range = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("Integer"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Integer")))), hydra.core.FieldType(hydra.core.Name("Sequence"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.RepeatRange_Sequence_Option_Option_Option")))))))))))))

repeat_range_sequence_option_option_option = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("Integer"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Integer")))), hydra.core.FieldType(hydra.core.Name("Ast"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

semantic_actions = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.CodeDecl")))))))

sense_flags = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeUnit())))

shape_and = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("ShapeNot"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeNot")))), hydra.core.FieldType(hydra.core.Name("listOfSequence"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeNot")))))))))

shape_atom = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeAtom_Sequence")))), hydra.core.FieldType(hydra.core.Name("ShapeOrRef"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeOrRef")))), hydra.core.FieldType(hydra.core.Name("sequence2"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExpression")))), hydra.core.FieldType(hydra.core.Name("Period"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

shape_atom_sequence = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("NodeConstraint"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint")))), hydra.core.FieldType(hydra.core.Name("ShapeOrRef"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeOrRef")))))))))

shape_definition = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("listOfAlts"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeDefinition_ListOfAlts_Elmt")))))), hydra.core.FieldType(hydra.core.Name("TripleExpression"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.TripleExpression")))))), hydra.core.FieldType(hydra.core.Name("listOfAnnotation"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Annotation")))))), hydra.core.FieldType(hydra.core.Name("SemanticActions"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.SemanticActions")))))))

shape_definition_list_of_alts_elmt = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("IncludeSet"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.IncludeSet")))), hydra.core.FieldType(hydra.core.Name("ExtraPropertySet"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ExtraPropertySet")))), hydra.core.FieldType(hydra.core.Name("CLOSED"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

shape_expr_label = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("Iri"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Iri")))), hydra.core.FieldType(hydra.core.Name("BlankNode"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.BlankNode")))))))

shape_expression = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeOr")))))

shape_not = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("NOT"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeUnit())))), hydra.core.FieldType(hydra.core.Name("ShapeAtom"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeAtom")))))))

shape_or = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("ShapeAnd"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeAnd")))), hydra.core.FieldType(hydra.core.Name("listOfSequence"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeAnd")))))))))

shape_or_ref = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("ShapeDefinition"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeDefinition")))), hydra.core.FieldType(hydra.core.Name("AtpNameLn"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.AtpNameLn")))), hydra.core.FieldType(hydra.core.Name("AtpNameNs"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.AtpNameNs")))), hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExprLabel")))))))

shex_doc = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("listOfDirective"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Directive")))))), hydra.core.FieldType(hydra.core.Name("Sequence"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option")))))), hydra.core.FieldType(hydra.core.Name("PrefixDecl"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.PrefixDecl")))))))

shex_doc_sequence_option = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("alts"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option_Alts")))), hydra.core.FieldType(hydra.core.Name("listOfStatement"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Statement")))))))))

shex_doc_sequence_option_alts = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("NotStartAction"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NotStartAction")))), hydra.core.FieldType(hydra.core.Name("StartActions"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StartActions")))))))

single_element_group = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("UnaryTripleExpr"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.UnaryTripleExpr")))), hydra.core.FieldType(hydra.core.Name("Semi"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeUnit())))))))

start_actions = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.CodeDecl")))))))

statement = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("Directive"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Directive")))), hydra.core.FieldType(hydra.core.Name("NotStartAction"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NotStartAction")))))))

string = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("StringLiteral1"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteral1")))), hydra.core.FieldType(hydra.core.Name("StringLiteralLong1"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong1")))), hydra.core.FieldType(hydra.core.Name("StringLiteral2"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteral2")))), hydra.core.FieldType(hydra.core.Name("StringLiteralLong2"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong2")))))))

string_facet = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringFacet_Sequence")))), hydra.core.FieldType(hydra.core.Name("Regexp"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Regexp")))))))

string_facet_sequence = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("StringLength"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLength")))), hydra.core.FieldType(hydra.core.Name("Integer"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Integer")))))))

string_length = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("LENGTH"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("MINLENGTH"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("MAXLENGTH"), cast(hydra.core.Type, hydra.core.TypeUnit())))))

string_literal1 = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteral1_Elmt")))))))

string_literal1_elmt = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("regex"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("Echar"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Echar")))), hydra.core.FieldType(hydra.core.Name("Uchar"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))))))

string_literal2 = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteral2_Elmt")))))))

string_literal2_elmt = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("regex"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("Echar"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Echar")))), hydra.core.FieldType(hydra.core.Name("Uchar"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))))))

string_literal_long1 = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt")))))))

string_literal_long1_elmt = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence")))), hydra.core.FieldType(hydra.core.Name("Echar"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Echar")))), hydra.core.FieldType(hydra.core.Name("Uchar"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))))))

string_literal_long1_elmt_sequence = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("Alts"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option")))))), hydra.core.FieldType(hydra.core.Name("regex"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))))

string_literal_long1_elmt_sequence_alts_option = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("Apos"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence")))))))

@lru_cache(1)
def string_literal_long1_elmt_sequence_alts_option_sequence() -> hydra.core.Type:
    return cast(hydra.core.Type, hydra.core.TypeRecord(()))

string_literal_long2 = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt")))))))

string_literal_long2_elmt = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence")))), hydra.core.FieldType(hydra.core.Name("Echar"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Echar")))), hydra.core.FieldType(hydra.core.Name("Uchar"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))))))

string_literal_long2_elmt_sequence = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("Alts"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option")))))), hydra.core.FieldType(hydra.core.Name("regex"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))))

string_literal_long2_elmt_sequence_alts_option = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("Quot"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence")))))))

@lru_cache(1)
def string_literal_long2_elmt_sequence_alts_option_sequence() -> hydra.core.Type:
    return cast(hydra.core.Type, hydra.core.TypeRecord(()))

triple_constraint = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("SenseFlags"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.SenseFlags")))))), hydra.core.FieldType(hydra.core.Name("Predicate"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Predicate")))), hydra.core.FieldType(hydra.core.Name("InlineShapeExpression"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeExpression")))), hydra.core.FieldType(hydra.core.Name("Cardinality"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Cardinality")))))), hydra.core.FieldType(hydra.core.Name("listOfAnnotation"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Annotation")))))), hydra.core.FieldType(hydra.core.Name("SemanticActions"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.SemanticActions")))))))

triple_expr_label = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("Iri"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Iri")))), hydra.core.FieldType(hydra.core.Name("BlankNode"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.BlankNode")))))))

triple_expression = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.OneOfTripleExpr")))))

uchar = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Uchar_Sequence")))), hydra.core.FieldType(hydra.core.Name("sequence2"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Uchar_Sequence2")))))))

uchar_sequence = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("Hex"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))), hydra.core.FieldType(hydra.core.Name("Hex2"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))), hydra.core.FieldType(hydra.core.Name("Hex3"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))), hydra.core.FieldType(hydra.core.Name("Hex4"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))))))

uchar_sequence2 = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("Hex"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))), hydra.core.FieldType(hydra.core.Name("Hex2"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))), hydra.core.FieldType(hydra.core.Name("Hex3"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))), hydra.core.FieldType(hydra.core.Name("Hex4"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))), hydra.core.FieldType(hydra.core.Name("Hex5"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))), hydra.core.FieldType(hydra.core.Name("Hex6"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))), hydra.core.FieldType(hydra.core.Name("Hex7"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))), hydra.core.FieldType(hydra.core.Name("Hex8"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))))))

unary_triple_expr = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("sequence"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence")))), hydra.core.FieldType(hydra.core.Name("Include"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Include")))))))

unary_triple_expr_sequence = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("Sequence"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.TripleExprLabel")))))), hydra.core.FieldType(hydra.core.Name("alts"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence_Alts")))))))

unary_triple_expr_sequence_alts = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("TripleConstraint"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.TripleConstraint")))), hydra.core.FieldType(hydra.core.Name("BracketedTripleExpr"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.BracketedTripleExpr")))))))

value_set = cast(hydra.core.Type, hydra.core.TypeWrap(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.ValueSetValue")))))))

value_set_value = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("IriRange"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.IriRange")))), hydra.core.FieldType(hydra.core.Name("Literal"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.Literal")))))))

xs_facet = cast(hydra.core.Type, hydra.core.TypeUnion((hydra.core.FieldType(hydra.core.Name("StringFacet"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.StringFacet")))), hydra.core.FieldType(hydra.core.Name("NumericFacet"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.ext.io.shex.syntax.NumericFacet")))))))
