# Note: this is an automatically generated file. Do not edit.

r"""A model for Coq core and extensions. Based on the Coq 8.15 grammar:
  https://coq.github.io/doc/v8.15/refman/language/core/basic.html#essential-vocabulary
  Extended with Vernacular commands for complete .v file generation."""

from __future__ import annotations
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class AnnotatedApplication:
    annot: QualidAnnotated
    terms: frozenlist[Term1]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.AnnotatedApplication")
    ANNOT = hydra.core.Name("annot")
    TERMS = hydra.core.Name("terms")

class ApplicationNormal(Node["NormalApplication"]):
    ...

class ApplicationAnnotated(Node["AnnotatedApplication"]):
    ...

class _ApplicationMeta(type):
    def __getitem__(cls, item):
        return object

class Application(metaclass=_ApplicationMeta):
    r"""ApplicationNormal | ApplicationAnnotated"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Application")
    NORMAL = hydra.core.Name("normal")
    ANNOTATED = hydra.core.Name("annotated")

class ArgIdent(Node["IdentArg"]):
    ...

class ArgNatural(Node["NaturalArg"]):
    ...

class ArgTerm(Node["Term1"]):
    ...

class _ArgMeta(type):
    def __getitem__(cls, item):
        return object

class Arg(metaclass=_ArgMeta):
    r"""ArgIdent | ArgNatural | ArgTerm"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Arg")
    IDENT = hydra.core.Name("ident")
    NATURAL = hydra.core.Name("natural")
    TERM = hydra.core.Name("term")

class BinderName(Node["Name"]):
    ...

class BinderType(Node["TypeBinders"]):
    ...

class BinderTerm(Node["LetBinder"]):
    ...

class BinderImplicit(Node["ImplicitBinders"]):
    ...

class BinderGeneralizing(Node["GeneralizingBinder"]):
    ...

class BinderPattern(Node["Pattern0"]):
    ...

class _BinderMeta(type):
    def __getitem__(cls, item):
        return object

class Binder(metaclass=_BinderMeta):
    r"""BinderName | BinderType | BinderTerm | BinderImplicit | BinderGeneralizing | BinderPattern"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Binder")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")
    TERM = hydra.core.Name("term")
    IMPLICIT = hydra.core.Name("implicit")
    GENERALIZING = hydra.core.Name("generalizing")
    PATTERN = hydra.core.Name("pattern")

@dataclass(frozen=True)
class CaseItem:
    term: Term100
    as_: Maybe[Name]
    in_: Maybe[Pattern]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.CaseItem")
    TERM = hydra.core.Name("term")
    AS = hydra.core.Name("as")
    IN = hydra.core.Name("in")

@dataclass(frozen=True)
class Cofix:
    body: CofixBody
    qual: Maybe[CofixQual]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Cofix")
    BODY = hydra.core.Name("body")
    QUAL = hydra.core.Name("qual")

@dataclass(frozen=True)
class CofixBody:
    ident: Ident
    binders: frozenlist[Binder]
    type: Maybe[Type]
    term: Term

    TYPE_ = hydra.core.Name("hydra.coq.syntax.CofixBody")
    IDENT = hydra.core.Name("ident")
    BINDERS = hydra.core.Name("binders")
    TYPE = hydra.core.Name("type")
    TERM = hydra.core.Name("term")

class CofixQualIn(Node["Term"]):
    ...

class CofixQualWith(Node["CofixWith"]):
    ...

class _CofixQualMeta(type):
    def __getitem__(cls, item):
        return object

class CofixQual(metaclass=_CofixQualMeta):
    r"""CofixQualIn | CofixQualWith"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.CofixQual")
    IN = hydra.core.Name("in")
    WITH = hydra.core.Name("with")

@dataclass(frozen=True)
class CofixWith:
    with_: frozenlist[CofixBody]
    for_: Maybe[Ident]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.CofixWith")
    WITH = hydra.core.Name("with")
    FOR = hydra.core.Name("for")

@dataclass(frozen=True)
class Equation:
    pattern: frozenlist[frozenlist[Pattern]]
    term: Term

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Equation")
    PATTERN = hydra.core.Name("pattern")
    TERM = hydra.core.Name("term")

@dataclass(frozen=True)
class ExistentialVariable:
    ident: Ident
    variant: ExistentialVariableVariant

    TYPE_ = hydra.core.Name("hydra.coq.syntax.ExistentialVariable")
    IDENT = hydra.core.Name("ident")
    VARIANT = hydra.core.Name("variant")

class ExistentialVariableVariantPlaceholder:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ExistentialVariableVariantPlaceholder)
    def __hash__(self):
        return hash("ExistentialVariableVariantPlaceholder")

class ExistentialVariableVariantInside1:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ExistentialVariableVariantInside1)
    def __hash__(self):
        return hash("ExistentialVariableVariantInside1")

class ExistentialVariableVariantInside2:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ExistentialVariableVariantInside2)
    def __hash__(self):
        return hash("ExistentialVariableVariantInside2")

class ExistentialVariableVariantOutside(Node["Maybe[IdentArg]"]):
    ...

class _ExistentialVariableVariantMeta(type):
    def __getitem__(cls, item):
        return object

class ExistentialVariableVariant(metaclass=_ExistentialVariableVariantMeta):
    r"""ExistentialVariableVariantPlaceholder | ExistentialVariableVariantInside1 | ExistentialVariableVariantInside2 | ExistentialVariableVariantOutside"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.ExistentialVariableVariant")
    PLACEHOLDER = hydra.core.Name("placeholder")
    INSIDE1 = hydra.core.Name("inside1")
    INSIDE2 = hydra.core.Name("inside2")
    OUTSIDE = hydra.core.Name("outside")

class FieldIdent(Node["Ident"]):
    ...

FieldIdent.TYPE_ = hydra.core.Name("hydra.coq.syntax.FieldIdent")

class FixDecl(Node["Fix_Decl"]):
    ...

class FixQual(Node["Maybe[Fix_Qual]"]):
    ...

class _FixMeta(type):
    def __getitem__(cls, item):
        return object

class Fix(metaclass=_FixMeta):
    r"""FixDecl | FixQual"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Fix")
    DECL = hydra.core.Name("decl")
    QUAL = hydra.core.Name("qual")

class FixAnnotStruct(Node["Ident"]):
    ...

class FixAnnotWf(Node["FixAnnot_Wf"]):
    ...

class FixAnnotMeasure(Node["FixAnnot_Measure"]):
    ...

class _FixAnnotMeta(type):
    def __getitem__(cls, item):
        return object

class FixAnnot(metaclass=_FixAnnotMeta):
    r"""FixAnnotStruct | FixAnnotWf | FixAnnotMeasure"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.FixAnnot")
    STRUCT = hydra.core.Name("struct")
    WF = hydra.core.Name("wf")
    MEASURE = hydra.core.Name("measure")

@dataclass(frozen=True)
class FixAnnot_Measure:
    term: OneTerm
    ident: Maybe[Ident]
    term2: Maybe[OneTerm]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.FixAnnot_Measure")
    TERM = hydra.core.Name("term")
    IDENT = hydra.core.Name("ident")
    TERM2 = hydra.core.Name("term2")

@dataclass(frozen=True)
class FixAnnot_Wf:
    term: OneTerm
    ident: Ident

    TYPE_ = hydra.core.Name("hydra.coq.syntax.FixAnnot_Wf")
    TERM = hydra.core.Name("term")
    IDENT = hydra.core.Name("ident")

@dataclass(frozen=True)
class Fix_Decl:
    ident: Ident
    binders: frozenlist[Binder]
    annot: Maybe[FixAnnot]
    type: Maybe[Type]
    term: Term

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Fix_Decl")
    IDENT = hydra.core.Name("ident")
    BINDERS = hydra.core.Name("binders")
    ANNOT = hydra.core.Name("annot")
    TYPE = hydra.core.Name("type")
    TERM = hydra.core.Name("term")

class Fix_QualIn(Node["Term"]):
    ...

class Fix_QualWith(Node["FixWith"]):
    ...

class _Fix_QualMeta(type):
    def __getitem__(cls, item):
        return object

class Fix_Qual(metaclass=_Fix_QualMeta):
    r"""Fix_QualIn | Fix_QualWith"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Fix_Qual")
    IN = hydra.core.Name("in")
    WITH = hydra.core.Name("with")

@dataclass(frozen=True)
class FixWith:
    decls: frozenlist[Fix_Decl]
    for_: Maybe[Ident]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.FixWith")
    DECLS = hydra.core.Name("decls")
    FOR = hydra.core.Name("for")

@dataclass(frozen=True)
class Forall:
    binders: OpenBinders
    type: Type

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Forall")
    BINDERS = hydra.core.Name("binders")
    TYPE = hydra.core.Name("type")

class ForallOrFunForall(Node["Forall"]):
    ...

class ForallOrFunFun(Node["Fun"]):
    ...

class _ForallOrFunMeta(type):
    def __getitem__(cls, item):
        return object

class ForallOrFun(metaclass=_ForallOrFunMeta):
    r"""ForallOrFunForall | ForallOrFunFun"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.ForallOrFun")
    FORALL = hydra.core.Name("forall")
    FUN = hydra.core.Name("fun")

@dataclass(frozen=True)
class Fun:
    binders: OpenBinders
    body: Term

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Fun")
    BINDERS = hydra.core.Name("binders")
    BODY = hydra.core.Name("body")

class GeneralizingBinderExplicit(Node["TypeclassConstraint"]):
    r"""Terms surrounded by `( ) introduce their free variables as explicit arguments"""

class GeneralizingBinderImplicitMaximallyInserted(Node["TypeclassConstraint"]):
    r"""Terms surrounded by `{ } introduce their free variables as maximally inserted implicit arguments"""

class GeneralizingBinderImplicitNonMaximallyInserted(Node["TypeclassConstraint"]):
    r"""Terms surrounded by `[ ] introduce them as non-maximally inserted implicit arguments"""

class _GeneralizingBinderMeta(type):
    def __getitem__(cls, item):
        return object

class GeneralizingBinder(metaclass=_GeneralizingBinderMeta):
    r"""GeneralizingBinderExplicit | GeneralizingBinderImplicitMaximallyInserted | GeneralizingBinderImplicitNonMaximallyInserted"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.GeneralizingBinder")
    EXPLICIT = hydra.core.Name("explicit")
    IMPLICIT_MAXIMALLY_INSERTED = hydra.core.Name("implicitMaximallyInserted")
    IMPLICIT_NON_MAXIMALLY_INSERTED = hydra.core.Name("implicitNonMaximallyInserted")

class Ident(Node["String"]):
    ...

Ident.TYPE_ = hydra.core.Name("hydra.coq.syntax.Ident")

@dataclass(frozen=True)
class IdentArg:
    ident: Ident
    term: Term

    TYPE_ = hydra.core.Name("hydra.coq.syntax.IdentArg")
    IDENT = hydra.core.Name("ident")
    TERM = hydra.core.Name("term")

@dataclass(frozen=True)
class If:
    r"""Pattern match on boolean values."""

    condition: Term
    return_as: Maybe[ReturnAs]
    then: Term
    else_: Term

    TYPE_ = hydra.core.Name("hydra.coq.syntax.If")
    CONDITION = hydra.core.Name("condition")
    RETURN_AS = hydra.core.Name("returnAs")
    THEN = hydra.core.Name("then")
    ELSE = hydra.core.Name("else")

class ImplicitBindersMaximallyInserted(Node["TypeBinders"]):
    r"""The first form, with curly braces, makes name a maximally inserted implicit argument"""

class ImplicitBindersNonMaximallyInserted(Node["TypeBinders"]):
    r"""The second form, with square brackets, makes name a non-maximally inserted implicit argument."""

class _ImplicitBindersMeta(type):
    def __getitem__(cls, item):
        return object

# In the context of a function definition, these forms specify that name is an implicit argument.
class ImplicitBinders(metaclass=_ImplicitBindersMeta):
    r"""ImplicitBindersMaximallyInserted | ImplicitBindersNonMaximallyInserted"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.ImplicitBinders")
    MAXIMALLY_INSERTED = hydra.core.Name("maximallyInserted")
    NON_MAXIMALLY_INSERTED = hydra.core.Name("nonMaximallyInserted")

@dataclass(frozen=True)
class Let:
    r"""A let-in definition."""

    bindings: LetBindings
    in_: Term

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Let")
    BINDINGS = hydra.core.Name("bindings")
    IN = hydra.core.Name("in")

@dataclass(frozen=True)
class LetBinder:
    r"""Some constructions allow the binding of a variable to value. This is called a 'let-binder'."""

    name: Name
    type: Maybe[Type]
    term: Term

    TYPE_ = hydra.core.Name("hydra.coq.syntax.LetBinder")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")
    TERM = hydra.core.Name("term")

class LetBindingsNamed(Node["LetNamed"]):
    ...

class LetBindingsDestructuring(Node["LetDestructuring"]):
    ...

class _LetBindingsMeta(type):
    def __getitem__(cls, item):
        return object

class LetBindings(metaclass=_LetBindingsMeta):
    r"""LetBindingsNamed | LetBindingsDestructuring"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.LetBindings")
    NAMED = hydra.core.Name("named")
    DESTRUCTURING = hydra.core.Name("destructuring")

@dataclass(frozen=True)
class LetNamed:
    binder: LetBinder
    binders: frozenlist[Binder]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.LetNamed")
    BINDER = hydra.core.Name("binder")
    BINDERS = hydra.core.Name("binders")

class LetDestructuringVariant1(Node["LetDestructuring_Variant1"]):
    ...

class LetDestructuringVariant2(Node["LetDestructuring_Variant2"]):
    ...

class LetDestructuringVariant3(Node["LetDestructuring_Variant3"]):
    ...

class _LetDestructuringMeta(type):
    def __getitem__(cls, item):
        return object

class LetDestructuring(metaclass=_LetDestructuringMeta):
    r"""LetDestructuringVariant1 | LetDestructuringVariant2 | LetDestructuringVariant3"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.LetDestructuring")
    VARIANT1 = hydra.core.Name("variant1")
    VARIANT2 = hydra.core.Name("variant2")
    VARIANT3 = hydra.core.Name("variant3")

@dataclass(frozen=True)
class LetDestructuring_Variant1:
    names: frozenlist[Name]
    return_as: Maybe[ReturnAs]
    term: Term

    TYPE_ = hydra.core.Name("hydra.coq.syntax.LetDestructuring_Variant1")
    NAMES = hydra.core.Name("names")
    RETURN_AS = hydra.core.Name("returnAs")
    TERM = hydra.core.Name("term")

@dataclass(frozen=True)
class LetDestructuring_Variant2:
    pattern: Pattern
    term: Term
    return_: Maybe[Term100]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.LetDestructuring_Variant2")
    PATTERN = hydra.core.Name("pattern")
    TERM = hydra.core.Name("term")
    RETURN = hydra.core.Name("return")

@dataclass(frozen=True)
class LetDestructuring_Variant3:
    pattern1: Pattern
    pattern2: Pattern
    term: Term
    return_: Term100

    TYPE_ = hydra.core.Name("hydra.coq.syntax.LetDestructuring_Variant3")
    PATTERN1 = hydra.core.Name("pattern1")
    PATTERN2 = hydra.core.Name("pattern2")
    TERM = hydra.core.Name("term")
    RETURN = hydra.core.Name("return")

@dataclass(frozen=True)
class Match:
    case_items: frozenlist[CaseItem]
    return_: Maybe[Term100]
    pipe: bool
    equations: frozenlist[Equation]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Match")
    CASE_ITEMS = hydra.core.Name("caseItems")
    RETURN = hydra.core.Name("return")
    PIPE = hydra.core.Name("pipe")
    EQUATIONS = hydra.core.Name("equations")

class Name(Node["Maybe[Ident]"]):
    ...

Name.TYPE_ = hydra.core.Name("hydra.coq.syntax.Name")

class Natural(Node[int]):
    r"""A non-negative arbitrary-precision integer."""

Natural.TYPE_ = hydra.core.Name("hydra.coq.syntax.Natural")

@dataclass(frozen=True)
class NaturalArg:
    natural: Natural
    term: Term

    TYPE_ = hydra.core.Name("hydra.coq.syntax.NaturalArg")
    NATURAL = hydra.core.Name("natural")
    TERM = hydra.core.Name("term")

@dataclass(frozen=True)
class NormalApplication:
    lhs: Term1
    rhs: frozenlist[Arg]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.NormalApplication")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

class Number(Node[Decimal]):
    ...

Number.TYPE_ = hydra.core.Name("hydra.coq.syntax.Number")

class OneTermExplicit(Node["QualidAnnotated"]):
    ...

class OneTermTerm1(Node["Term1"]):
    ...

class _OneTermMeta(type):
    def __getitem__(cls, item):
        return object

class OneTerm(metaclass=_OneTermMeta):
    r"""OneTermExplicit | OneTermTerm1"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.OneTerm")
    EXPLICIT = hydra.core.Name("explicit")
    TERM1 = hydra.core.Name("term1")

class OpenBindersType(Node["TypeBinders"]):
    ...

class OpenBindersBinders(Node["frozenlist[Binder]"]):
    ...

class _OpenBindersMeta(type):
    def __getitem__(cls, item):
        return object

class OpenBinders(metaclass=_OpenBindersMeta):
    r"""OpenBindersType | OpenBindersBinders"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.OpenBinders")
    TYPE = hydra.core.Name("type")
    BINDERS = hydra.core.Name("binders")

class PatternPattern(Node["Pattern10"]):
    ...

class PatternTerm(Node["Maybe[Term]"]):
    ...

class _PatternMeta(type):
    def __getitem__(cls, item):
        return object

class Pattern(metaclass=_PatternMeta):
    r"""PatternPattern | PatternTerm"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Pattern")
    PATTERN = hydra.core.Name("pattern")
    TERM = hydra.core.Name("term")

class Pattern0Qualid(Node["Qualid"]):
    ...

class Pattern0QualIdAndPattern(Node["QualidAndPattern"]):
    ...

class Pattern0Placeholder:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, Pattern0Placeholder)
    def __hash__(self):
        return hash("Pattern0Placeholder")

class Pattern0Parens(Node["frozenlist[Pattern]"]):
    ...

class Pattern0Number(Node["Number"]):
    ...

class Pattern0String(Node["String"]):
    ...

class _Pattern0Meta(type):
    def __getitem__(cls, item):
        return object

class Pattern0(metaclass=_Pattern0Meta):
    r"""Pattern0Qualid | Pattern0QualIdAndPattern | Pattern0Placeholder | Pattern0Parens | Pattern0Number | Pattern0String"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Pattern0")
    QUALID = hydra.core.Name("qualid")
    QUAL_ID_AND_PATTERN = hydra.core.Name("qualIdAndPattern")
    PLACEHOLDER = hydra.core.Name("placeholder")
    PARENS = hydra.core.Name("parens")
    NUMBER = hydra.core.Name("number")
    STRING = hydra.core.Name("string")

@dataclass(frozen=True)
class Pattern1:
    pattern: Pattern0
    scope: Maybe[ScopeKey]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Pattern1")
    PATTERN = hydra.core.Name("pattern")
    SCOPE = hydra.core.Name("scope")

class Pattern10As(Node["Pattern10_As"]):
    ...

class Pattern10Patterns(Node["Pattern10_Patterns"]):
    ...

class Pattern10Qualiid(Node["Pattern10_Qualid"]):
    ...

class _Pattern10Meta(type):
    def __getitem__(cls, item):
        return object

class Pattern10(metaclass=_Pattern10Meta):
    r"""Pattern10As | Pattern10Patterns | Pattern10Qualiid"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Pattern10")
    AS = hydra.core.Name("as")
    PATTERNS = hydra.core.Name("patterns")
    QUALIID = hydra.core.Name("qualiid")

@dataclass(frozen=True)
class Pattern10_As:
    pattern: Pattern1
    as_: Name

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Pattern10_As")
    PATTERN = hydra.core.Name("pattern")
    AS = hydra.core.Name("as")

@dataclass(frozen=True)
class Pattern10_Patterns:
    pattern: Pattern1
    patterns: frozenlist[Pattern1]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Pattern10_Patterns")
    PATTERN = hydra.core.Name("pattern")
    PATTERNS = hydra.core.Name("patterns")

@dataclass(frozen=True)
class Pattern10_Qualid:
    qualid: Qualid
    patterns: frozenlist[Pattern1]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Pattern10_Qualid")
    QUALID = hydra.core.Name("qualid")
    PATTERNS = hydra.core.Name("patterns")

class PrimitiveNotationsNumber(Node["Number"]):
    ...

class PrimitiveNotationsString(Node["String"]):
    ...

class _PrimitiveNotationsMeta(type):
    def __getitem__(cls, item):
        return object

class PrimitiveNotations(metaclass=_PrimitiveNotationsMeta):
    r"""PrimitiveNotationsNumber | PrimitiveNotationsString"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.PrimitiveNotations")
    NUMBER = hydra.core.Name("number")
    STRING = hydra.core.Name("string")

@dataclass(frozen=True)
class Qualid:
    r"""A qualified identifier."""

    id: Ident
    field_ids: frozenlist[FieldIdent]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Qualid")
    ID = hydra.core.Name("id")
    FIELD_IDS = hydra.core.Name("fieldIds")

@dataclass(frozen=True)
class QualidAndPattern:
    qualid: Qualid
    pattern: Pattern

    TYPE_ = hydra.core.Name("hydra.coq.syntax.QualidAndPattern")
    QUALID = hydra.core.Name("qualid")
    PATTERN = hydra.core.Name("pattern")

@dataclass(frozen=True)
class QualidAnnotated:
    qualid: Qualid
    univ_annot: Maybe[UnivAnnot]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.QualidAnnotated")
    QUALID = hydra.core.Name("qualid")
    UNIV_ANNOT = hydra.core.Name("univAnnot")

@dataclass(frozen=True)
class ReturnAs:
    as_: Maybe[Name]
    return_: Term100

    TYPE_ = hydra.core.Name("hydra.coq.syntax.ReturnAs")
    AS = hydra.core.Name("as")
    RETURN = hydra.core.Name("return")

class ScopeKey(Node["Ident"]):
    ...

ScopeKey.TYPE_ = hydra.core.Name("hydra.coq.syntax.ScopeKey")

class SortSet:
    r"""The sort 𝖲𝖾𝗍 intends to be the type of small sets."""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SortSet)
    def __hash__(self):
        return hash("SortSet")

class SortProp:
    r"""The sort 𝖯𝗋𝗈𝗉 intends to be the type of logical propositions."""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SortProp)
    def __hash__(self):
        return hash("SortProp")

class SortSProp:
    r"""The sort 𝖲𝖯𝗋𝗈𝗉 is like 𝖯𝗋𝗈𝗉 but the propositions in 𝖲𝖯𝗋𝗈𝗉 are known to have irrelevant proofs (all proofs are equal)."""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SortSProp)
    def __hash__(self):
        return hash("SortSProp")

class SortType:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SortType)
    def __hash__(self):
        return hash("SortType")

class SortTypeWithAnyUniverse:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SortTypeWithAnyUniverse)
    def __hash__(self):
        return hash("SortTypeWithAnyUniverse")

class SortTypeWithUniverse(Node["Universe"]):
    ...

class _SortMeta(type):
    def __getitem__(cls, item):
        return object

# The types of types are called sorts.
class Sort(metaclass=_SortMeta):
    r"""SortSet | SortProp | SortSProp | SortType | SortTypeWithAnyUniverse | SortTypeWithUniverse"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Sort")
    SET = hydra.core.Name("set")
    PROP = hydra.core.Name("prop")
    S_PROP = hydra.core.Name("sProp")
    TYPE = hydra.core.Name("type")
    TYPE_WITH_ANY_UNIVERSE = hydra.core.Name("typeWithAnyUniverse")
    TYPE_WITH_UNIVERSE = hydra.core.Name("typeWithUniverse")

class String(Node[str]):
    ...

String.TYPE_ = hydra.core.Name("hydra.coq.syntax.String")

class TermForallOrFun(Node["ForallOrFun"]):
    ...

class TermLet(Node["Let"]):
    ...

class TermIf(Node["If"]):
    ...

class TermFix(Node["Fix"]):
    ...

class TermCofix(Node["Cofix"]):
    ...

class TermTerm100(Node["Term100"]):
    ...

class _TermMeta(type):
    def __getitem__(cls, item):
        return object

class Term(metaclass=_TermMeta):
    r"""TermForallOrFun | TermLet | TermIf | TermFix | TermCofix | TermTerm100"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Term")
    FORALL_OR_FUN = hydra.core.Name("forallOrFun")
    LET = hydra.core.Name("let")
    IF = hydra.core.Name("if")
    FIX = hydra.core.Name("fix")
    COFIX = hydra.core.Name("cofix")
    TERM100 = hydra.core.Name("term100")

class Term0QualidAnnotated(Node["QualidAnnotated"]):
    ...

class Term0Sort(Node["Sort"]):
    ...

class Term0PrimitiveNotations(Node["PrimitiveNotations"]):
    ...

class Term0Evar(Node["ExistentialVariable"]):
    ...

class Term0Match(Node["Match"]):
    ...

class Term0Record:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, Term0Record)
    def __hash__(self):
        return hash("Term0Record")

class Term0Generalizing:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, Term0Generalizing)
    def __hash__(self):
        return hash("Term0Generalizing")

class Term0Ltac:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, Term0Ltac)
    def __hash__(self):
        return hash("Term0Ltac")

class Term0Parens(Node["Term"]):
    ...

class _Term0Meta(type):
    def __getitem__(cls, item):
        return object

class Term0(metaclass=_Term0Meta):
    r"""Term0QualidAnnotated | Term0Sort | Term0PrimitiveNotations | Term0Evar | Term0Match | Term0Record | Term0Generalizing | Term0Ltac | Term0Parens"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Term0")
    QUALID_ANNOTATED = hydra.core.Name("qualidAnnotated")
    SORT = hydra.core.Name("sort")
    PRIMITIVE_NOTATIONS = hydra.core.Name("primitiveNotations")
    EVAR = hydra.core.Name("evar")
    MATCH = hydra.core.Name("match")
    RECORD = hydra.core.Name("record")
    GENERALIZING = hydra.core.Name("generalizing")
    LTAC = hydra.core.Name("ltac")
    PARENS = hydra.core.Name("parens")

class Term1Projection:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, Term1Projection)
    def __hash__(self):
        return hash("Term1Projection")

class Term1Scope:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, Term1Scope)
    def __hash__(self):
        return hash("Term1Scope")

class Term1Term0(Node["Term0"]):
    ...

class _Term1Meta(type):
    def __getitem__(cls, item):
        return object

class Term1(metaclass=_Term1Meta):
    r"""Term1Projection | Term1Scope | Term1Term0"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Term1")
    PROJECTION = hydra.core.Name("projection")
    SCOPE = hydra.core.Name("scope")
    TERM0 = hydra.core.Name("term0")

class Term10Application(Node["Application"]):
    ...

class Term10OneTerm(Node["OneTerm"]):
    ...

class _Term10Meta(type):
    def __getitem__(cls, item):
        return object

class Term10(metaclass=_Term10Meta):
    r"""Term10Application | Term10OneTerm"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Term10")
    APPLICATION = hydra.core.Name("application")
    ONE_TERM = hydra.core.Name("oneTerm")

class Term100Cast(Node["TypeCast"]):
    ...

class Term100Term10(Node["Term10"]):
    ...

class _Term100Meta(type):
    def __getitem__(cls, item):
        return object

class Term100(metaclass=_Term100Meta):
    r"""Term100Cast | Term100Term10"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Term100")
    CAST = hydra.core.Name("cast")
    TERM10 = hydra.core.Name("term10")

class Type(Node["Term"]):
    ...

Type.TYPE_ = hydra.core.Name("hydra.coq.syntax.Type")

@dataclass(frozen=True)
class TypeCast:
    term: Term10
    type: Type
    operator: TypeCastOperator

    TYPE_ = hydra.core.Name("hydra.coq.syntax.TypeCast")
    TERM = hydra.core.Name("term")
    TYPE = hydra.core.Name("type")
    OPERATOR = hydra.core.Name("operator")

class TypeCastOperator(Enum):
    NORMAL = hydra.core.Name("normal")
    r"""The expression term10 : type is a type cast expression. It enforces the type of term10 to be type."""

    VM_COMPUTE = hydra.core.Name("vmCompute")
    r"""term10 <: type specifies that the virtual machine will be used to type check that term10 has type type (see vm_compute)."""

    NATIVE_COMPUTE = hydra.core.Name("nativeCompute")
    r"""term10 <<: type specifies that compilation to OCaml will be used to type check that term10 has type type (see native_compute)."""

TypeCastOperator.TYPE_ = hydra.core.Name("hydra.coq.syntax.TypeCastOperator")

@dataclass(frozen=True)
class TypeBinders:
    names: frozenlist[Name]
    type: Type

    TYPE_ = hydra.core.Name("hydra.coq.syntax.TypeBinders")
    NAMES = hydra.core.Name("names")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class TypeclassConstraint:
    name: Maybe[Name]
    generalizing: bool
    term: Term

    TYPE_ = hydra.core.Name("hydra.coq.syntax.TypeclassConstraint")
    NAME = hydra.core.Name("name")
    GENERALIZING = hydra.core.Name("generalizing")
    TERM = hydra.core.Name("term")

class UnivAnnot(Node["frozenlist[UniverseLevel]"]):
    ...

UnivAnnot.TYPE_ = hydra.core.Name("hydra.coq.syntax.UnivAnnot")

class UniverseMax(Node["frozenlist[Universe_Expr]"]):
    ...

class UniverseExpr(Node["Universe_Expr"]):
    ...

class _UniverseMeta(type):
    def __getitem__(cls, item):
        return object

class Universe(metaclass=_UniverseMeta):
    r"""UniverseMax | UniverseExpr"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Universe")
    MAX = hydra.core.Name("max")
    EXPR = hydra.core.Name("expr")

@dataclass(frozen=True)
class Universe_Expr:
    name: UniverseName
    number: Maybe[Natural]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Universe_Expr")
    NAME = hydra.core.Name("name")
    NUMBER = hydra.core.Name("number")

class UniverseLevelSet:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, UniverseLevelSet)
    def __hash__(self):
        return hash("UniverseLevelSet")

class UniverseLevelProp:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, UniverseLevelProp)
    def __hash__(self):
        return hash("UniverseLevelProp")

class UniverseLevelType:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, UniverseLevelType)
    def __hash__(self):
        return hash("UniverseLevelType")

class UniverseLevelIgnored:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, UniverseLevelIgnored)
    def __hash__(self):
        return hash("UniverseLevelIgnored")

class UniverseLevelQualid(Node["Qualid"]):
    ...

class _UniverseLevelMeta(type):
    def __getitem__(cls, item):
        return object

class UniverseLevel(metaclass=_UniverseLevelMeta):
    r"""UniverseLevelSet | UniverseLevelProp | UniverseLevelType | UniverseLevelIgnored | UniverseLevelQualid"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.UniverseLevel")
    SET = hydra.core.Name("set")
    PROP = hydra.core.Name("prop")
    TYPE = hydra.core.Name("type")
    IGNORED = hydra.core.Name("ignored")
    QUALID = hydra.core.Name("qualid")

class UniverseNameQualid(Node["Qualid"]):
    ...

class UniverseNameSet:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, UniverseNameSet)
    def __hash__(self):
        return hash("UniverseNameSet")

class UniverseNameProp:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, UniverseNameProp)
    def __hash__(self):
        return hash("UniverseNameProp")

class _UniverseNameMeta(type):
    def __getitem__(cls, item):
        return object

class UniverseName(metaclass=_UniverseNameMeta):
    r"""UniverseNameQualid | UniverseNameSet | UniverseNameProp"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.UniverseName")
    QUALID = hydra.core.Name("qualid")
    SET = hydra.core.Name("set")
    PROP = hydra.core.Name("prop")

@dataclass(frozen=True)
class AxiomDeclaration:
    r"""An Axiom declaration: `Axiom <name> : <type>.`."""

    name: Ident
    type: Type

    TYPE_ = hydra.core.Name("hydra.coq.syntax.AxiomDeclaration")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")

class Comment(Node[str]):
    r"""A Coq comment (* ... *)."""

Comment.TYPE_ = hydra.core.Name("hydra.coq.syntax.Comment")

@dataclass(frozen=True)
class Constructor:
    r"""A constructor in an Inductive definition."""

    name: Ident
    binders: frozenlist[Binder]
    type: Maybe[Type]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Constructor")
    NAME = hydra.core.Name("name")
    BINDERS = hydra.core.Name("binders")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class Definition:
    r"""A Definition or Let command: Definition name binders : type := term."""

    locality: Maybe[Locality]
    name: Ident
    binders: frozenlist[Binder]
    type: Maybe[Type]
    body: Term

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Definition")
    LOCALITY = hydra.core.Name("locality")
    NAME = hydra.core.Name("name")
    BINDERS = hydra.core.Name("binders")
    TYPE = hydra.core.Name("type")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Document:
    r"""A complete Coq .v file."""

    sentences: frozenlist[Sentence]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Document")
    SENTENCES = hydra.core.Name("sentences")

@dataclass(frozen=True)
class FixpointDefinition:
    r"""A Fixpoint command for recursive definitions."""

    locality: Maybe[Locality]
    name: Ident
    binders: frozenlist[Binder]
    annot: Maybe[FixAnnot]
    type: Maybe[Type]
    body: Term
    with_: frozenlist[Fix_Decl]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.FixpointDefinition")
    LOCALITY = hydra.core.Name("locality")
    NAME = hydra.core.Name("name")
    BINDERS = hydra.core.Name("binders")
    ANNOT = hydra.core.Name("annot")
    TYPE = hydra.core.Name("type")
    BODY = hydra.core.Name("body")
    WITH = hydra.core.Name("with")

class ImportQualification(Enum):
    r"""Qualification for Require/Import commands."""

    IMPORT = hydra.core.Name("import")

    EXPORT = hydra.core.Name("export")

ImportQualification.TYPE_ = hydra.core.Name("hydra.coq.syntax.ImportQualification")

@dataclass(frozen=True)
class InductiveBody:
    r"""A single body in an Inductive definition (supports mutual induction via 'with')."""

    name: Ident
    binders: frozenlist[Binder]
    type: Maybe[Type]
    constructors: frozenlist[Constructor]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.InductiveBody")
    NAME = hydra.core.Name("name")
    BINDERS = hydra.core.Name("binders")
    TYPE = hydra.core.Name("type")
    CONSTRUCTORS = hydra.core.Name("constructors")

@dataclass(frozen=True)
class InductiveDefinition:
    r"""An Inductive or CoInductive definition with one or more mutually inductive bodies."""

    locality: Maybe[Locality]
    coinductive: bool
    bodies: frozenlist[InductiveBody]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.InductiveDefinition")
    LOCALITY = hydra.core.Name("locality")
    COINDUCTIVE = hydra.core.Name("coinductive")
    BODIES = hydra.core.Name("bodies")

class Locality(Enum):
    r"""Local or Global qualifier for commands."""

    LOCAL = hydra.core.Name("local")

    GLOBAL = hydra.core.Name("global")

Locality.TYPE_ = hydra.core.Name("hydra.coq.syntax.Locality")

@dataclass(frozen=True)
class ModuleDefinition:
    r"""A Module ... End block."""

    name: Ident
    sentences: frozenlist[Sentence]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.ModuleDefinition")
    NAME = hydra.core.Name("name")
    SENTENCES = hydra.core.Name("sentences")

@dataclass(frozen=True)
class NotationDeclaration:
    r"""A Notation declaration."""

    notation: String
    definition: Term
    level: Maybe[Natural]
    associativity: Maybe[str]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.NotationDeclaration")
    NOTATION = hydra.core.Name("notation")
    DEFINITION = hydra.core.Name("definition")
    LEVEL = hydra.core.Name("level")
    ASSOCIATIVITY = hydra.core.Name("associativity")

@dataclass(frozen=True)
class RecordBody:
    r"""The body of a Record definition."""

    constructor: Maybe[Ident]
    fields: frozenlist[RecordField]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.RecordBody")
    CONSTRUCTOR = hydra.core.Name("constructor")
    FIELDS = hydra.core.Name("fields")

@dataclass(frozen=True)
class RecordDefinition:
    r"""A Record or Structure definition."""

    locality: Maybe[Locality]
    name: Ident
    binders: frozenlist[Binder]
    sort: Maybe[Sort]
    body: RecordBody

    TYPE_ = hydra.core.Name("hydra.coq.syntax.RecordDefinition")
    LOCALITY = hydra.core.Name("locality")
    NAME = hydra.core.Name("name")
    BINDERS = hydra.core.Name("binders")
    SORT = hydra.core.Name("sort")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class RecordField:
    r"""A field in a Record definition."""

    name: Ident
    type: Type

    TYPE_ = hydra.core.Name("hydra.coq.syntax.RecordField")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class RequireImport:
    r"""A Require Import/Export command."""

    from_: Maybe[Qualid]
    require: bool
    qualification: Maybe[ImportQualification]
    modules: frozenlist[Qualid]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.RequireImport")
    FROM = hydra.core.Name("from")
    REQUIRE = hydra.core.Name("require")
    QUALIFICATION = hydra.core.Name("qualification")
    MODULES = hydra.core.Name("modules")

@dataclass(frozen=True)
class SectionDefinition:
    r"""A Section ... End block."""

    name: Ident
    sentences: frozenlist[Sentence]

    TYPE_ = hydra.core.Name("hydra.coq.syntax.SectionDefinition")
    NAME = hydra.core.Name("name")
    SENTENCES = hydra.core.Name("sentences")

@dataclass(frozen=True)
class Sentence:
    r"""A top-level sentence in a Coq document, optionally preceded by a comment."""

    comment: Maybe[Comment]
    content: SentenceContent

    TYPE_ = hydra.core.Name("hydra.coq.syntax.Sentence")
    COMMENT = hydra.core.Name("comment")
    CONTENT = hydra.core.Name("content")

class SentenceContentAxiom(Node["AxiomDeclaration"]):
    ...

class SentenceContentDefinition(Node["Definition"]):
    ...

class SentenceContentFixpoint(Node["FixpointDefinition"]):
    ...

class SentenceContentInductive(Node["InductiveDefinition"]):
    ...

class SentenceContentModule(Node["ModuleDefinition"]):
    ...

class SentenceContentNotation(Node["NotationDeclaration"]):
    ...

class SentenceContentRecord(Node["RecordDefinition"]):
    ...

class SentenceContentRequireImport(Node["RequireImport"]):
    ...

class SentenceContentSection(Node["SectionDefinition"]):
    ...

class SentenceContentTheorem(Node["TheoremBody"]):
    ...

class _SentenceContentMeta(type):
    def __getitem__(cls, item):
        return object

# The content of a top-level sentence.
class SentenceContent(metaclass=_SentenceContentMeta):
    r"""SentenceContentAxiom | SentenceContentDefinition | SentenceContentFixpoint | SentenceContentInductive | SentenceContentModule | SentenceContentNotation | SentenceContentRecord | SentenceContentRequireImport | SentenceContentSection | SentenceContentTheorem"""

    TYPE_ = hydra.core.Name("hydra.coq.syntax.SentenceContent")
    AXIOM = hydra.core.Name("axiom")
    DEFINITION = hydra.core.Name("definition")
    FIXPOINT = hydra.core.Name("fixpoint")
    INDUCTIVE = hydra.core.Name("inductive")
    MODULE = hydra.core.Name("module")
    NOTATION = hydra.core.Name("notation")
    RECORD = hydra.core.Name("record")
    REQUIRE_IMPORT = hydra.core.Name("requireImport")
    SECTION = hydra.core.Name("section")
    THEOREM = hydra.core.Name("theorem")

@dataclass(frozen=True)
class TheoremBody:
    r"""A Theorem/Lemma/Proposition with a proof term."""

    kind: TheoremKind
    name: Ident
    binders: frozenlist[Binder]
    type: Type
    proof: Term

    TYPE_ = hydra.core.Name("hydra.coq.syntax.TheoremBody")
    KIND = hydra.core.Name("kind")
    NAME = hydra.core.Name("name")
    BINDERS = hydra.core.Name("binders")
    TYPE = hydra.core.Name("type")
    PROOF = hydra.core.Name("proof")

class TheoremKind(Enum):
    r"""The kind of theorem command."""

    THEOREM = hydra.core.Name("theorem")

    LEMMA = hydra.core.Name("lemma")

    PROPOSITION = hydra.core.Name("proposition")

    COROLLARY = hydra.core.Name("corollary")

    EXAMPLE = hydra.core.Name("example")

TheoremKind.TYPE_ = hydra.core.Name("hydra.coq.syntax.TheoremKind")
