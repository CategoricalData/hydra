# Note: this is an automatically generated file. Do not edit.

r"""A Scala syntax model based on Scalameta (https://scalameta.org)."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

class PredefString(Node[str]):
    ...

PredefString.TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.PredefString")

@dataclass(frozen=True)
class ScalaSymbol:
    name: str

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.ScalaSymbol")
    NAME = hydra.core.Name("name")

class TreeRef(Node["Ref"]):
    ...

class TreeStat(Node["Stat"]):
    ...

class TreeType(Node["Type"]):
    ...

class TreeBounds(Node["TypeBounds"]):
    ...

class TreePat(Node["Pat"]):
    ...

class TreeMember(Node["Member"]):
    ...

class TreeCtor(Node["Ctor"]):
    ...

class TreeTemplate(Node["Template"]):
    ...

class TreeMod(Node["Mod"]):
    ...

class TreeEnumerator(Node["Enumerator"]):
    ...

class TreeImporter(Node["Importer"]):
    ...

class TreeImportee(Node["Importee"]):
    ...

class TreeCaseTree(Node["CaseTree"]):
    ...

class TreeSource(Node["Source"]):
    ...

class TreeQuasi(Node["Quasi"]):
    ...

class _TreeMeta(type):
    def __getitem__(cls, item):
        return object

class Tree(metaclass=_TreeMeta):
    r"""TreeRef | TreeStat | TreeType | TreeBounds | TreePat | TreeMember | TreeCtor | TreeTemplate | TreeMod | TreeEnumerator | TreeImporter | TreeImportee | TreeCaseTree | TreeSource | TreeQuasi"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Tree")
    REF = hydra.core.Name("ref")
    STAT = hydra.core.Name("stat")
    TYPE = hydra.core.Name("type")
    BOUNDS = hydra.core.Name("bounds")
    PAT = hydra.core.Name("pat")
    MEMBER = hydra.core.Name("member")
    CTOR = hydra.core.Name("ctor")
    TEMPLATE = hydra.core.Name("template")
    MOD = hydra.core.Name("mod")
    ENUMERATOR = hydra.core.Name("enumerator")
    IMPORTER = hydra.core.Name("importer")
    IMPORTEE = hydra.core.Name("importee")
    CASE_TREE = hydra.core.Name("caseTree")
    SOURCE = hydra.core.Name("source")
    QUASI = hydra.core.Name("quasi")

class RefName(Node["Name"]):
    ...

class RefInit(Node["Init"]):
    ...

class _RefMeta(type):
    def __getitem__(cls, item):
        return object

class Ref(metaclass=_RefMeta):
    r"""RefName | RefInit"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Ref")
    NAME = hydra.core.Name("name")
    INIT = hydra.core.Name("init")

class StatTerm(Node["Data"]):
    ...

class StatDecl(Node["Decl"]):
    ...

class StatDefn(Node["Defn"]):
    ...

class StatImportExport(Node["ImportExportStat"]):
    ...

class _StatMeta(type):
    def __getitem__(cls, item):
        return object

class Stat(metaclass=_StatMeta):
    r"""StatTerm | StatDecl | StatDefn | StatImportExport"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Stat")
    TERM = hydra.core.Name("term")
    DECL = hydra.core.Name("decl")
    DEFN = hydra.core.Name("defn")
    IMPORT_EXPORT = hydra.core.Name("importExport")

class NameValue(Node[str]):
    ...

class NameAnonymous:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, NameAnonymous)
    def __hash__(self):
        return hash("NameAnonymous")

class NameIndeterminate(Node["PredefString"]):
    ...

class _NameMeta(type):
    def __getitem__(cls, item):
        return object

class Name(metaclass=_NameMeta):
    r"""NameValue | NameAnonymous | NameIndeterminate"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Name")
    VALUE = hydra.core.Name("value")
    ANONYMOUS = hydra.core.Name("anonymous")
    INDETERMINATE = hydra.core.Name("indeterminate")

class LitNull:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LitNull)
    def __hash__(self):
        return hash("LitNull")

class LitInt(Node[int]):
    ...

class LitDouble(Node[float]):
    ...

class LitFloat(Node[float]):
    ...

class LitByte(Node[int]):
    ...

class LitShort(Node[int]):
    ...

class LitChar(Node[int]):
    ...

class LitLong(Node[int]):
    ...

class LitBoolean(Node[bool]):
    ...

class LitUnit:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LitUnit)
    def __hash__(self):
        return hash("LitUnit")

class LitString(Node[str]):
    ...

class LitSymbol(Node["ScalaSymbol"]):
    ...

class _LitMeta(type):
    def __getitem__(cls, item):
        return object

class Lit(metaclass=_LitMeta):
    r"""LitNull | LitInt | LitDouble | LitFloat | LitByte | LitShort | LitChar | LitLong | LitBoolean | LitUnit | LitString | LitSymbol"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Lit")
    NULL = hydra.core.Name("null")
    INT = hydra.core.Name("int")
    DOUBLE = hydra.core.Name("double")
    FLOAT = hydra.core.Name("float")
    BYTE = hydra.core.Name("byte")
    SHORT = hydra.core.Name("short")
    CHAR = hydra.core.Name("char")
    LONG = hydra.core.Name("long")
    BOOLEAN = hydra.core.Name("boolean")
    UNIT = hydra.core.Name("unit")
    STRING = hydra.core.Name("string")
    SYMBOL = hydra.core.Name("symbol")

class DataLit(Node["Lit"]):
    ...

class DataRef(Node["Data_Ref"]):
    ...

class DataInterpolate(Node["Data_Interpolate"]):
    ...

class DataXml(Node["Data_Xml"]):
    ...

class DataApply(Node["Data_Apply"]):
    ...

class DataApplyUsing(Node["Data_ApplyUsing"]):
    ...

class DataApplyType(Node["Data_ApplyType"]):
    ...

class DataAssign(Node["Data_Assign"]):
    ...

class DataReturn(Node["Data_Return"]):
    ...

class DataThrow(Node["Data_Throw"]):
    ...

class DataAscribe(Node["Data_Ascribe"]):
    ...

class DataAnnotate(Node["Data_Annotate"]):
    ...

class DataTuple(Node["Data_Tuple"]):
    ...

class DataBlock(Node["Data_Block"]):
    ...

class DataEndMarker(Node["Data_EndMarker"]):
    ...

class DataIf(Node["Data_If"]):
    ...

class DataQuotedMacroExpr(Node["Data_QuotedMacroExpr"]):
    ...

class DataQuotedMacroType(Node["Data_QuotedMacroType"]):
    ...

class DataSplicedMacroExpr(Node["Data_SplicedMacroExpr"]):
    ...

class DataMatch(Node["Data_Match"]):
    ...

class DataTry(Node["Data_Try"]):
    ...

class DataTryWithHandler(Node["Data_TryWithHandler"]):
    ...

class DataFunctionData(Node["Data_FunctionData"]):
    ...

class DataPolyFunction(Node["Data_PolyFunction"]):
    ...

class DataPartialFunction(Node["Data_PartialFunction"]):
    ...

class DataWhile(Node["Data_While"]):
    ...

class DataDo(Node["Data_Do"]):
    ...

class DataFor(Node["Data_For"]):
    ...

class DataForYield(Node["Data_ForYield"]):
    ...

class DataNew(Node["Data_New"]):
    ...

class DataNewAnonymous(Node["Data_NewAnonymous"]):
    ...

class DataPlaceholder(Node["Data_Placeholder"]):
    ...

class DataEta(Node["Data_Eta"]):
    ...

class DataRepeated(Node["Data_Repeated"]):
    ...

class DataParam(Node["Data_Param"]):
    ...

class _DataMeta(type):
    def __getitem__(cls, item):
        return object

class Data(metaclass=_DataMeta):
    r"""DataLit | DataRef | DataInterpolate | DataXml | DataApply | DataApplyUsing | DataApplyType | DataAssign | DataReturn | DataThrow | DataAscribe | DataAnnotate | DataTuple | DataBlock | DataEndMarker | DataIf | DataQuotedMacroExpr | DataQuotedMacroType | DataSplicedMacroExpr | DataMatch | DataTry | DataTryWithHandler | DataFunctionData | DataPolyFunction | DataPartialFunction | DataWhile | DataDo | DataFor | DataForYield | DataNew | DataNewAnonymous | DataPlaceholder | DataEta | DataRepeated | DataParam"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data")
    LIT = hydra.core.Name("lit")
    REF = hydra.core.Name("ref")
    INTERPOLATE = hydra.core.Name("interpolate")
    XML = hydra.core.Name("xml")
    APPLY = hydra.core.Name("apply")
    APPLY_USING = hydra.core.Name("applyUsing")
    APPLY_TYPE = hydra.core.Name("applyType")
    ASSIGN = hydra.core.Name("assign")
    RETURN = hydra.core.Name("return")
    THROW = hydra.core.Name("throw")
    ASCRIBE = hydra.core.Name("ascribe")
    ANNOTATE = hydra.core.Name("annotate")
    TUPLE = hydra.core.Name("tuple")
    BLOCK = hydra.core.Name("block")
    END_MARKER = hydra.core.Name("endMarker")
    IF = hydra.core.Name("if")
    QUOTED_MACRO_EXPR = hydra.core.Name("quotedMacroExpr")
    QUOTED_MACRO_TYPE = hydra.core.Name("quotedMacroType")
    SPLICED_MACRO_EXPR = hydra.core.Name("splicedMacroExpr")
    MATCH = hydra.core.Name("match")
    TRY = hydra.core.Name("try")
    TRY_WITH_HANDLER = hydra.core.Name("tryWithHandler")
    FUNCTION_DATA = hydra.core.Name("functionData")
    POLY_FUNCTION = hydra.core.Name("polyFunction")
    PARTIAL_FUNCTION = hydra.core.Name("partialFunction")
    WHILE = hydra.core.Name("while")
    DO = hydra.core.Name("do")
    FOR = hydra.core.Name("for")
    FOR_YIELD = hydra.core.Name("forYield")
    NEW = hydra.core.Name("new")
    NEW_ANONYMOUS = hydra.core.Name("newAnonymous")
    PLACEHOLDER = hydra.core.Name("placeholder")
    ETA = hydra.core.Name("eta")
    REPEATED = hydra.core.Name("repeated")
    PARAM = hydra.core.Name("param")

class Data_RefThis(Node["Data_This"]):
    ...

class Data_RefSuper(Node["Data_Super"]):
    ...

class Data_RefName(Node["Data_Name"]):
    ...

class Data_RefAnonymous(Node["Data_Anonymous"]):
    ...

class Data_RefSelect(Node["Data_Select"]):
    ...

class Data_RefApplyUnary(Node["Data_ApplyUnary"]):
    ...

class _Data_RefMeta(type):
    def __getitem__(cls, item):
        return object

class Data_Ref(metaclass=_Data_RefMeta):
    r"""Data_RefThis | Data_RefSuper | Data_RefName | Data_RefAnonymous | Data_RefSelect | Data_RefApplyUnary"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Ref")
    THIS = hydra.core.Name("this")
    SUPER = hydra.core.Name("super")
    NAME = hydra.core.Name("name")
    ANONYMOUS = hydra.core.Name("anonymous")
    SELECT = hydra.core.Name("select")
    APPLY_UNARY = hydra.core.Name("applyUnary")

class Data_This(Node[None]):
    ...

Data_This.TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_This")

@dataclass(frozen=True)
class Data_Super:
    thisp: Name
    superp: Name

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Super")
    THISP = hydra.core.Name("thisp")
    SUPERP = hydra.core.Name("superp")

@dataclass(frozen=True)
class Data_Name:
    value: PredefString

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Name")
    VALUE = hydra.core.Name("value")

class Data_Anonymous(Node[None]):
    ...

Data_Anonymous.TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Anonymous")

@dataclass(frozen=True)
class Data_Select:
    qual: Data
    name: Data_Name

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Select")
    QUAL = hydra.core.Name("qual")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class Data_Interpolate:
    prefix: Data_Name
    parts: frozenlist[Lit]
    args: frozenlist[Data]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Interpolate")
    PREFIX = hydra.core.Name("prefix")
    PARTS = hydra.core.Name("parts")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class Data_Xml:
    parts: frozenlist[Lit]
    args: frozenlist[Data]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Xml")
    PARTS = hydra.core.Name("parts")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class Data_Apply:
    fun: Data
    args: frozenlist[Data]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Apply")
    FUN = hydra.core.Name("fun")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class Data_ApplyUsing:
    fun: Data
    targs: frozenlist[Data]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_ApplyUsing")
    FUN = hydra.core.Name("fun")
    TARGS = hydra.core.Name("targs")

@dataclass(frozen=True)
class Data_ApplyType:
    lhs: Data
    op: Data_Name
    targs: frozenlist[Type]
    args: frozenlist[Data]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_ApplyType")
    LHS = hydra.core.Name("lhs")
    OP = hydra.core.Name("op")
    TARGS = hydra.core.Name("targs")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class Data_ApplyInfix:
    lhs: Data
    op: Data_Name
    targs: frozenlist[Type]
    args: frozenlist[Data]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_ApplyInfix")
    LHS = hydra.core.Name("lhs")
    OP = hydra.core.Name("op")
    TARGS = hydra.core.Name("targs")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class Data_ApplyUnary:
    op: Data_Name
    arg: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_ApplyUnary")
    OP = hydra.core.Name("op")
    ARG = hydra.core.Name("arg")

@dataclass(frozen=True)
class Data_Assign:
    lhs: Data
    rhs: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Assign")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Data_Return:
    expr: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Return")
    EXPR = hydra.core.Name("expr")

@dataclass(frozen=True)
class Data_Throw:
    expr: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Throw")
    EXPR = hydra.core.Name("expr")

@dataclass(frozen=True)
class Data_Ascribe:
    expr: Data
    tpe: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Ascribe")
    EXPR = hydra.core.Name("expr")
    TPE = hydra.core.Name("tpe")

@dataclass(frozen=True)
class Data_Annotate:
    expr: Data
    annots: frozenlist[Mod_Annot]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Annotate")
    EXPR = hydra.core.Name("expr")
    ANNOTS = hydra.core.Name("annots")

@dataclass(frozen=True)
class Data_Tuple:
    args: frozenlist[Data]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Tuple")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class Data_Block:
    stats: frozenlist[Stat]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Block")
    STATS = hydra.core.Name("stats")

@dataclass(frozen=True)
class Data_EndMarker:
    name: Data_Name

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_EndMarker")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class Data_If:
    cond: Data
    thenp: Data
    elsep: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_If")
    COND = hydra.core.Name("cond")
    THENP = hydra.core.Name("thenp")
    ELSEP = hydra.core.Name("elsep")

@dataclass(frozen=True)
class Data_QuotedMacroExpr:
    body: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_QuotedMacroExpr")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Data_QuotedMacroType:
    tpe: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_QuotedMacroType")
    TPE = hydra.core.Name("tpe")

@dataclass(frozen=True)
class Data_SplicedMacroExpr:
    body: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_SplicedMacroExpr")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Data_Match:
    expr: Data
    cases: frozenlist[Case]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Match")
    EXPR = hydra.core.Name("expr")
    CASES = hydra.core.Name("cases")

@dataclass(frozen=True)
class Data_Try:
    expr: Data
    catchp: frozenlist[Case]
    finallyp: Maybe[Data]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Try")
    EXPR = hydra.core.Name("expr")
    CATCHP = hydra.core.Name("catchp")
    FINALLYP = hydra.core.Name("finallyp")

@dataclass(frozen=True)
class Data_TryWithHandler:
    expr: Data
    catchp: Data
    finallyp: Maybe[Data]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_TryWithHandler")
    EXPR = hydra.core.Name("expr")
    CATCHP = hydra.core.Name("catchp")
    FINALLYP = hydra.core.Name("finallyp")

class Data_FunctionDataContextFunction(Node["Data_ContextFunction"]):
    ...

class Data_FunctionDataFunction(Node["Data_Function"]):
    ...

class _Data_FunctionDataMeta(type):
    def __getitem__(cls, item):
        return object

class Data_FunctionData(metaclass=_Data_FunctionDataMeta):
    r"""Data_FunctionDataContextFunction | Data_FunctionDataFunction"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_FunctionData")
    CONTEXT_FUNCTION = hydra.core.Name("contextFunction")
    FUNCTION = hydra.core.Name("function")

@dataclass(frozen=True)
class Data_ContextFunction:
    params: frozenlist[Data_Param]
    body: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_ContextFunction")
    PARAMS = hydra.core.Name("params")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Data_Function:
    params: frozenlist[Data_Param]
    body: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Function")
    PARAMS = hydra.core.Name("params")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Data_PolyFunction:
    tparams: frozenlist[Type_Param]
    body: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_PolyFunction")
    TPARAMS = hydra.core.Name("tparams")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Data_PartialFunction:
    cases: frozenlist[Case]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_PartialFunction")
    CASES = hydra.core.Name("cases")

@dataclass(frozen=True)
class Data_While:
    expr: Data
    body: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_While")
    EXPR = hydra.core.Name("expr")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Data_Do:
    body: Data
    expr: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Do")
    BODY = hydra.core.Name("body")
    EXPR = hydra.core.Name("expr")

@dataclass(frozen=True)
class Data_For:
    enums: frozenlist[Enumerator]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_For")
    ENUMS = hydra.core.Name("enums")

@dataclass(frozen=True)
class Data_ForYield:
    enums: frozenlist[Enumerator]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_ForYield")
    ENUMS = hydra.core.Name("enums")

@dataclass(frozen=True)
class Data_New:
    init: Init

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_New")
    INIT = hydra.core.Name("init")

@dataclass(frozen=True)
class Data_NewAnonymous:
    templ: Template

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_NewAnonymous")
    TEMPL = hydra.core.Name("templ")

Data_Placeholder: TypeAlias = "None"

@dataclass(frozen=True)
class Data_Eta:
    expr: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Eta")
    EXPR = hydra.core.Name("expr")

@dataclass(frozen=True)
class Data_Repeated:
    expr: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Repeated")
    EXPR = hydra.core.Name("expr")

@dataclass(frozen=True)
class Data_Param:
    mods: frozenlist[Mod]
    name: Name
    decltpe: Maybe[Type]
    default: Maybe[Data]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Data_Param")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    DECLTPE = hydra.core.Name("decltpe")
    DEFAULT = hydra.core.Name("default")

class TypeRef(Node["Type_Ref"]):
    ...

class TypeAnonymousName(Node["Type_AnonymousName"]):
    ...

class TypeApply(Node["Type_Apply"]):
    ...

class TypeApplyInfix(Node["Type_ApplyInfix"]):
    ...

class TypeFunctionType(Node["Type_FunctionType"]):
    ...

class TypePolyFunction(Node["Type_PolyFunction"]):
    ...

class TypeImplicitFunction(Node["Type_ImplicitFunction"]):
    ...

class TypeTuple(Node["Type_Tuple"]):
    ...

class TypeWith(Node["Type_With"]):
    ...

class TypeAnd(Node["Type_And"]):
    ...

class TypeOr(Node["Type_Or"]):
    ...

class TypeRefine(Node["Type_Refine"]):
    ...

class TypeExistential(Node["Type_Existential"]):
    ...

class TypeAnnotate(Node["Type_Annotate"]):
    ...

class TypeLambda(Node["Type_Lambda"]):
    ...

class TypeMacro(Node["Type_Macro"]):
    ...

class TypeMethod(Node["Type_Method"]):
    ...

class TypePlaceholder(Node["Type_Placeholder"]):
    ...

class TypeByName(Node["Type_ByName"]):
    ...

class TypeRepeated(Node["Type_Repeated"]):
    ...

class TypeVar(Node["Type_Var"]):
    ...

class TypeTypedParam(Node["Type_TypedParam"]):
    ...

class TypeMatch(Node["Type_Match"]):
    ...

class _TypeMeta(type):
    def __getitem__(cls, item):
        return object

class Type(metaclass=_TypeMeta):
    r"""TypeRef | TypeAnonymousName | TypeApply | TypeApplyInfix | TypeFunctionType | TypePolyFunction | TypeImplicitFunction | TypeTuple | TypeWith | TypeAnd | TypeOr | TypeRefine | TypeExistential | TypeAnnotate | TypeLambda | TypeMacro | TypeMethod | TypePlaceholder | TypeByName | TypeRepeated | TypeVar | TypeTypedParam | TypeMatch"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type")
    REF = hydra.core.Name("ref")
    ANONYMOUS_NAME = hydra.core.Name("anonymousName")
    APPLY = hydra.core.Name("apply")
    APPLY_INFIX = hydra.core.Name("applyInfix")
    FUNCTION_TYPE = hydra.core.Name("functionType")
    POLY_FUNCTION = hydra.core.Name("polyFunction")
    IMPLICIT_FUNCTION = hydra.core.Name("implicitFunction")
    TUPLE = hydra.core.Name("tuple")
    WITH = hydra.core.Name("with")
    AND = hydra.core.Name("and")
    OR = hydra.core.Name("or")
    REFINE = hydra.core.Name("refine")
    EXISTENTIAL = hydra.core.Name("existential")
    ANNOTATE = hydra.core.Name("annotate")
    LAMBDA = hydra.core.Name("lambda")
    MACRO = hydra.core.Name("macro")
    METHOD = hydra.core.Name("method")
    PLACEHOLDER = hydra.core.Name("placeholder")
    BY_NAME = hydra.core.Name("byName")
    REPEATED = hydra.core.Name("repeated")
    VAR = hydra.core.Name("var")
    TYPED_PARAM = hydra.core.Name("typedParam")
    MATCH = hydra.core.Name("match")

class Type_RefName(Node["Type_Name"]):
    ...

class Type_RefSelect(Node["Type_Select"]):
    ...

class Type_RefProject(Node["Type_Project"]):
    ...

class Type_RefSingleton(Node["Type_Singleton"]):
    ...

class _Type_RefMeta(type):
    def __getitem__(cls, item):
        return object

class Type_Ref(metaclass=_Type_RefMeta):
    r"""Type_RefName | Type_RefSelect | Type_RefProject | Type_RefSingleton"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Ref")
    NAME = hydra.core.Name("name")
    SELECT = hydra.core.Name("select")
    PROJECT = hydra.core.Name("project")
    SINGLETON = hydra.core.Name("singleton")

@dataclass(frozen=True)
class Type_Name:
    value: str

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Name")
    VALUE = hydra.core.Name("value")

class Type_AnonymousName(Node[None]):
    ...

Type_AnonymousName.TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_AnonymousName")

@dataclass(frozen=True)
class Type_Select:
    qual: Data_Ref
    name: Type_Name

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Select")
    QUAL = hydra.core.Name("qual")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class Type_Project:
    qual: Type
    name: Type_Name

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Project")
    QUAL = hydra.core.Name("qual")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class Type_Singleton:
    ref: Data_Ref

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Singleton")
    REF = hydra.core.Name("ref")

@dataclass(frozen=True)
class Type_Apply:
    tpe: Type
    args: frozenlist[Type]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Apply")
    TPE = hydra.core.Name("tpe")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class Type_ApplyInfix:
    lhs: Type
    op: Type_Name
    rhs: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_ApplyInfix")
    LHS = hydra.core.Name("lhs")
    OP = hydra.core.Name("op")
    RHS = hydra.core.Name("rhs")

class Type_FunctionTypeFunction(Node["Type_Function"]):
    ...

class Type_FunctionTypeContextFunction(Node["Type_ContextFunction"]):
    ...

class _Type_FunctionTypeMeta(type):
    def __getitem__(cls, item):
        return object

class Type_FunctionType(metaclass=_Type_FunctionTypeMeta):
    r"""Type_FunctionTypeFunction | Type_FunctionTypeContextFunction"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_FunctionType")
    FUNCTION = hydra.core.Name("function")
    CONTEXT_FUNCTION = hydra.core.Name("contextFunction")

@dataclass(frozen=True)
class Type_Function:
    params: frozenlist[Type]
    res: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Function")
    PARAMS = hydra.core.Name("params")
    RES = hydra.core.Name("res")

@dataclass(frozen=True)
class Type_PolyFunction:
    tparams: frozenlist[Type_Param]
    tpe: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_PolyFunction")
    TPARAMS = hydra.core.Name("tparams")
    TPE = hydra.core.Name("tpe")

@dataclass(frozen=True)
class Type_ContextFunction:
    params: frozenlist[Type]
    res: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_ContextFunction")
    PARAMS = hydra.core.Name("params")
    RES = hydra.core.Name("res")

@dataclass(frozen=True)
class Type_ImplicitFunction:
    params: frozenlist[Type]
    res: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_ImplicitFunction")
    PARAMS = hydra.core.Name("params")
    RES = hydra.core.Name("res")

@dataclass(frozen=True)
class Type_Tuple:
    args: frozenlist[Type]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Tuple")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class Type_With:
    lhs: Type
    rhs: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_With")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Type_And:
    lhs: Type
    rhs: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_And")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Type_Or:
    lhs: Type
    rhs: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Or")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Type_Refine:
    tpe: Maybe[Type]
    stats: frozenlist[Stat]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Refine")
    TPE = hydra.core.Name("tpe")
    STATS = hydra.core.Name("stats")

@dataclass(frozen=True)
class Type_Existential:
    tpe: Type
    stats: frozenlist[Stat]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Existential")
    TPE = hydra.core.Name("tpe")
    STATS = hydra.core.Name("stats")

@dataclass(frozen=True)
class Type_Annotate:
    tpe: Type
    annots: frozenlist[Mod_Annot]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Annotate")
    TPE = hydra.core.Name("tpe")
    ANNOTS = hydra.core.Name("annots")

@dataclass(frozen=True)
class Type_Lambda:
    tparams: frozenlist[Type_Param]
    tpe: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Lambda")
    TPARAMS = hydra.core.Name("tparams")
    TPE = hydra.core.Name("tpe")

@dataclass(frozen=True)
class Type_Macro:
    body: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Macro")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Type_Method:
    paramss: frozenlist[frozenlist[Data_Param]]
    tpe: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Method")
    PARAMSS = hydra.core.Name("paramss")
    TPE = hydra.core.Name("tpe")

@dataclass(frozen=True)
class Type_Placeholder:
    bounds: TypeBounds

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Placeholder")
    BOUNDS = hydra.core.Name("bounds")

@dataclass(frozen=True)
class TypeBounds:
    lo: Maybe[Type]
    hi: Maybe[Type]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.TypeBounds")
    LO = hydra.core.Name("lo")
    HI = hydra.core.Name("hi")

@dataclass(frozen=True)
class Type_ByName:
    tpe: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_ByName")
    TPE = hydra.core.Name("tpe")

@dataclass(frozen=True)
class Type_Repeated:
    tpe: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Repeated")
    TPE = hydra.core.Name("tpe")

@dataclass(frozen=True)
class Type_Var:
    name: Type_Name

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Var")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class Type_TypedParam:
    name: Name
    typ: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_TypedParam")
    NAME = hydra.core.Name("name")
    TYP = hydra.core.Name("typ")

@dataclass(frozen=True)
class Type_Param:
    mods: frozenlist[Mod]
    name: Name
    tparams: frozenlist[Type_Param]
    tbounds: frozenlist[TypeBounds]
    vbounds: frozenlist[Type]
    cbounds: frozenlist[Type]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Param")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TPARAMS = hydra.core.Name("tparams")
    TBOUNDS = hydra.core.Name("tbounds")
    VBOUNDS = hydra.core.Name("vbounds")
    CBOUNDS = hydra.core.Name("cbounds")

@dataclass(frozen=True)
class Type_Match:
    tpe: Type
    cases: frozenlist[TypeCase]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Type_Match")
    TPE = hydra.core.Name("tpe")
    CASES = hydra.core.Name("cases")

class PatVar(Node["Pat_Var"]):
    ...

class PatWildcard:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PatWildcard)
    def __hash__(self):
        return hash("PatWildcard")

class PatSeqWildcard:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PatSeqWildcard)
    def __hash__(self):
        return hash("PatSeqWildcard")

class PatBind(Node["Pat_Bind"]):
    ...

class PatAlternative(Node["Pat_Alternative"]):
    ...

class PatTuple(Node["Pat_Tuple"]):
    ...

class PatRepeated(Node["Pat_Repeated"]):
    ...

class PatExtract(Node["Pat_Extract"]):
    ...

class PatExtractInfix(Node["Pat_ExtractInfix"]):
    ...

class PatInterpolate(Node["Pat_Interpolate"]):
    ...

class PatXml(Node["Pat_Xml"]):
    ...

class PatTyped(Node["Pat_Typed"]):
    ...

class PatMacro(Node["Pat_Macro"]):
    ...

class PatGiven(Node["Pat_Given"]):
    ...

class _PatMeta(type):
    def __getitem__(cls, item):
        return object

class Pat(metaclass=_PatMeta):
    r"""PatVar | PatWildcard | PatSeqWildcard | PatBind | PatAlternative | PatTuple | PatRepeated | PatExtract | PatExtractInfix | PatInterpolate | PatXml | PatTyped | PatMacro | PatGiven"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pat")
    VAR = hydra.core.Name("var")
    WILDCARD = hydra.core.Name("wildcard")
    SEQ_WILDCARD = hydra.core.Name("seqWildcard")
    BIND = hydra.core.Name("bind")
    ALTERNATIVE = hydra.core.Name("alternative")
    TUPLE = hydra.core.Name("tuple")
    REPEATED = hydra.core.Name("repeated")
    EXTRACT = hydra.core.Name("extract")
    EXTRACT_INFIX = hydra.core.Name("extractInfix")
    INTERPOLATE = hydra.core.Name("interpolate")
    XML = hydra.core.Name("xml")
    TYPED = hydra.core.Name("typed")
    MACRO = hydra.core.Name("macro")
    GIVEN = hydra.core.Name("given")

@dataclass(frozen=True)
class Pat_Var:
    name: Data_Name

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pat_Var")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class Pat_Bind:
    lhs: Pat
    rhs: Pat

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pat_Bind")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Pat_Alternative:
    lhs: Pat
    rhs: Pat

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pat_Alternative")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Pat_Tuple:
    args: frozenlist[Pat]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pat_Tuple")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class Pat_Repeated:
    name: Data_Name

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pat_Repeated")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class Pat_Extract:
    fun: Data
    args: frozenlist[Pat]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pat_Extract")
    FUN = hydra.core.Name("fun")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class Pat_ExtractInfix:
    lhs: Pat
    op: Data_Name
    rhs: frozenlist[Pat]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pat_ExtractInfix")
    LHS = hydra.core.Name("lhs")
    OP = hydra.core.Name("op")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Pat_Interpolate:
    prefix: Data_Name
    parts: frozenlist[Lit]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pat_Interpolate")
    PREFIX = hydra.core.Name("prefix")
    PARTS = hydra.core.Name("parts")

@dataclass(frozen=True)
class Pat_Xml:
    parts: frozenlist[Lit]
    args: frozenlist[Pat]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pat_Xml")
    PARTS = hydra.core.Name("parts")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class Pat_Typed:
    lhs: Pat
    rhs: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pat_Typed")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Pat_Macro:
    body: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pat_Macro")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Pat_Given:
    tpe: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pat_Given")
    TPE = hydra.core.Name("tpe")

class MemberTerm(Node["Member_Data"]):
    ...

class MemberType(Node["Member_Type"]):
    ...

class MemberTermParam(Node["Data_Param"]):
    ...

class MemberTypeParam(Node["Type_Param"]):
    ...

class MemberSelf(Node["Self"]):
    ...

class _MemberMeta(type):
    def __getitem__(cls, item):
        return object

class Member(metaclass=_MemberMeta):
    r"""MemberTerm | MemberType | MemberTermParam | MemberTypeParam | MemberSelf"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Member")
    TERM = hydra.core.Name("term")
    TYPE = hydra.core.Name("type")
    TERM_PARAM = hydra.core.Name("termParam")
    TYPE_PARAM = hydra.core.Name("typeParam")
    SELF = hydra.core.Name("self")

class Member_DataPkg(Node["Pkg"]):
    ...

class Member_DataObject(Node["Pkg_Object"]):
    ...

class _Member_DataMeta(type):
    def __getitem__(cls, item):
        return object

class Member_Data(metaclass=_Member_DataMeta):
    r"""Member_DataPkg | Member_DataObject"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Member_Data")
    PKG = hydra.core.Name("pkg")
    OBJECT = hydra.core.Name("object")

@dataclass(frozen=True)
class Member_Type:
    name: Type_Name

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Member_Type")
    NAME = hydra.core.Name("name")

class DeclVal(Node["Decl_Val"]):
    ...

class DeclVar(Node["Decl_Var"]):
    ...

class DeclDef(Node["Decl_Def"]):
    ...

class DeclType(Node["Decl_Type"]):
    ...

class DeclGiven(Node["Decl_Given"]):
    ...

class _DeclMeta(type):
    def __getitem__(cls, item):
        return object

class Decl(metaclass=_DeclMeta):
    r"""DeclVal | DeclVar | DeclDef | DeclType | DeclGiven"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Decl")
    VAL = hydra.core.Name("val")
    VAR = hydra.core.Name("var")
    DEF = hydra.core.Name("def")
    TYPE = hydra.core.Name("type")
    GIVEN = hydra.core.Name("given")

@dataclass(frozen=True)
class Decl_Val:
    mods: frozenlist[Mod]
    pats: frozenlist[Pat]
    decltpe: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Decl_Val")
    MODS = hydra.core.Name("mods")
    PATS = hydra.core.Name("pats")
    DECLTPE = hydra.core.Name("decltpe")

@dataclass(frozen=True)
class Decl_Var:
    mods: frozenlist[Mod]
    pats: frozenlist[Pat]
    decltpe: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Decl_Var")
    MODS = hydra.core.Name("mods")
    PATS = hydra.core.Name("pats")
    DECLTPE = hydra.core.Name("decltpe")

@dataclass(frozen=True)
class Decl_Def:
    mods: frozenlist[Mod]
    name: Data_Name
    tparams: frozenlist[Type_Param]
    paramss: frozenlist[frozenlist[Data_Param]]
    decltpe: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Decl_Def")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TPARAMS = hydra.core.Name("tparams")
    PARAMSS = hydra.core.Name("paramss")
    DECLTPE = hydra.core.Name("decltpe")

@dataclass(frozen=True)
class Decl_Type:
    mods: frozenlist[Mod]
    name: Type_Name
    tparams: frozenlist[Type_Param]
    bounds: TypeBounds

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Decl_Type")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TPARAMS = hydra.core.Name("tparams")
    BOUNDS = hydra.core.Name("bounds")

@dataclass(frozen=True)
class Decl_Given:
    mods: frozenlist[Mod]
    name: Data_Name
    tparams: frozenlist[Type_Param]
    sparams: frozenlist[frozenlist[Data_Param]]
    decltpe: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Decl_Given")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TPARAMS = hydra.core.Name("tparams")
    SPARAMS = hydra.core.Name("sparams")
    DECLTPE = hydra.core.Name("decltpe")

class DefnVal(Node["Defn_Val"]):
    ...

class DefnVar(Node["Defn_Var"]):
    ...

class DefnGiven(Node["Defn_Given"]):
    ...

class DefnEnum(Node["Defn_Enum"]):
    ...

class DefnEnumCase(Node["Defn_EnumCase"]):
    ...

class DefnRepeatedEnumCase(Node["Defn_RepeatedEnumCase"]):
    ...

class DefnGivenAlias(Node["Defn_GivenAlias"]):
    ...

class DefnExtensionGroup(Node["Defn_ExtensionGroup"]):
    ...

class DefnDef(Node["Defn_Def"]):
    ...

class DefnMacro(Node["Defn_Macro"]):
    ...

class DefnType(Node["Defn_Type"]):
    ...

class DefnClass(Node["Defn_Class"]):
    ...

class DefnTrait(Node["Defn_Trait"]):
    ...

class DefnObject(Node["Defn_Object"]):
    ...

class _DefnMeta(type):
    def __getitem__(cls, item):
        return object

class Defn(metaclass=_DefnMeta):
    r"""DefnVal | DefnVar | DefnGiven | DefnEnum | DefnEnumCase | DefnRepeatedEnumCase | DefnGivenAlias | DefnExtensionGroup | DefnDef | DefnMacro | DefnType | DefnClass | DefnTrait | DefnObject"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn")
    VAL = hydra.core.Name("val")
    VAR = hydra.core.Name("var")
    GIVEN = hydra.core.Name("given")
    ENUM = hydra.core.Name("enum")
    ENUM_CASE = hydra.core.Name("enumCase")
    REPEATED_ENUM_CASE = hydra.core.Name("repeatedEnumCase")
    GIVEN_ALIAS = hydra.core.Name("givenAlias")
    EXTENSION_GROUP = hydra.core.Name("extensionGroup")
    DEF = hydra.core.Name("def")
    MACRO = hydra.core.Name("macro")
    TYPE = hydra.core.Name("type")
    CLASS = hydra.core.Name("class")
    TRAIT = hydra.core.Name("trait")
    OBJECT = hydra.core.Name("object")

@dataclass(frozen=True)
class Defn_Val:
    mods: frozenlist[Mod]
    pats: frozenlist[Pat]
    decltpe: Maybe[Type]
    rhs: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_Val")
    MODS = hydra.core.Name("mods")
    PATS = hydra.core.Name("pats")
    DECLTPE = hydra.core.Name("decltpe")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Defn_Var:
    mods: frozenlist[Mod]
    pats: frozenlist[Pat]
    decltpe: Type
    rhs: Maybe[Data]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_Var")
    MODS = hydra.core.Name("mods")
    PATS = hydra.core.Name("pats")
    DECLTPE = hydra.core.Name("decltpe")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Defn_Given:
    mods: frozenlist[Mod]
    name: Name
    tparams: frozenlist[frozenlist[Type_Param]]
    sparams: frozenlist[frozenlist[Data_Param]]
    templ: Template

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_Given")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TPARAMS = hydra.core.Name("tparams")
    SPARAMS = hydra.core.Name("sparams")
    TEMPL = hydra.core.Name("templ")

@dataclass(frozen=True)
class Defn_Enum:
    mods: frozenlist[Mod]
    name: Type_Name
    tparams: frozenlist[Type_Param]
    ctor: Ctor_Primary
    template: Template

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_Enum")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TPARAMS = hydra.core.Name("tparams")
    CTOR = hydra.core.Name("ctor")
    TEMPLATE = hydra.core.Name("template")

@dataclass(frozen=True)
class Defn_EnumCase:
    mods: frozenlist[Mod]
    name: Data_Name
    tparams: frozenlist[Type_Param]
    ctor: Ctor_Primary
    inits: frozenlist[Init]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_EnumCase")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TPARAMS = hydra.core.Name("tparams")
    CTOR = hydra.core.Name("ctor")
    INITS = hydra.core.Name("inits")

@dataclass(frozen=True)
class Defn_RepeatedEnumCase:
    mods: frozenlist[Mod]
    cases: frozenlist[Data_Name]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_RepeatedEnumCase")
    MODS = hydra.core.Name("mods")
    CASES = hydra.core.Name("cases")

@dataclass(frozen=True)
class Defn_GivenAlias:
    mods: frozenlist[Mod]
    name: Name
    tparams: frozenlist[frozenlist[Type_Param]]
    sparams: frozenlist[frozenlist[Data_Param]]
    decltpe: Type
    body: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_GivenAlias")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TPARAMS = hydra.core.Name("tparams")
    SPARAMS = hydra.core.Name("sparams")
    DECLTPE = hydra.core.Name("decltpe")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Defn_ExtensionGroup:
    tparams: frozenlist[Type_Param]
    parmss: frozenlist[frozenlist[Data_Param]]
    body: Stat

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_ExtensionGroup")
    TPARAMS = hydra.core.Name("tparams")
    PARMSS = hydra.core.Name("parmss")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Defn_Def:
    mods: frozenlist[Mod]
    name: Data_Name
    tparams: frozenlist[Type_Param]
    paramss: frozenlist[frozenlist[Data_Param]]
    decltpe: Maybe[Type]
    body: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_Def")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TPARAMS = hydra.core.Name("tparams")
    PARAMSS = hydra.core.Name("paramss")
    DECLTPE = hydra.core.Name("decltpe")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Defn_Macro:
    mods: frozenlist[Mod]
    name: Data_Name
    tparams: frozenlist[Type_Param]
    paramss: frozenlist[frozenlist[Data_Param]]
    decltpe: Maybe[Type]
    body: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_Macro")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TPARAMS = hydra.core.Name("tparams")
    PARAMSS = hydra.core.Name("paramss")
    DECLTPE = hydra.core.Name("decltpe")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Defn_Type:
    mods: frozenlist[Mod]
    name: Type_Name
    tparams: frozenlist[Type_Param]
    body: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_Type")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TPARAMS = hydra.core.Name("tparams")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Defn_Class:
    mods: frozenlist[Mod]
    name: Type_Name
    tparams: frozenlist[Type_Param]
    ctor: Ctor_Primary
    template: Template

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_Class")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TPARAMS = hydra.core.Name("tparams")
    CTOR = hydra.core.Name("ctor")
    TEMPLATE = hydra.core.Name("template")

@dataclass(frozen=True)
class Defn_Trait:
    mods: frozenlist[Mod]
    name: Type_Name
    tparams: frozenlist[Type_Param]
    ctor: Ctor_Primary
    template: Template

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_Trait")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TPARAMS = hydra.core.Name("tparams")
    CTOR = hydra.core.Name("ctor")
    TEMPLATE = hydra.core.Name("template")

@dataclass(frozen=True)
class Defn_Object:
    name: Data_Name

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Defn_Object")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class Pkg:
    name: Data_Name
    ref: Data_Ref
    stats: frozenlist[Stat]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pkg")
    NAME = hydra.core.Name("name")
    REF = hydra.core.Name("ref")
    STATS = hydra.core.Name("stats")

@dataclass(frozen=True)
class Pkg_Object:
    mods: frozenlist[Mod]
    name: Data_Name
    template: Template

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Pkg_Object")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    TEMPLATE = hydra.core.Name("template")

class CtorPrimary(Node["Ctor_Primary"]):
    ...

class CtorSecondary(Node["Ctor_Secondary"]):
    ...

class _CtorMeta(type):
    def __getitem__(cls, item):
        return object

class Ctor(metaclass=_CtorMeta):
    r"""CtorPrimary | CtorSecondary"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Ctor")
    PRIMARY = hydra.core.Name("primary")
    SECONDARY = hydra.core.Name("secondary")

@dataclass(frozen=True)
class Ctor_Primary:
    mods: frozenlist[Mod]
    name: Name
    paramss: frozenlist[frozenlist[Data_Param]]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Ctor_Primary")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    PARAMSS = hydra.core.Name("paramss")

@dataclass(frozen=True)
class Ctor_Secondary:
    mods: frozenlist[Mod]
    name: Name
    paramss: frozenlist[frozenlist[Data_Param]]
    init: Init
    stats: frozenlist[Stat]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Ctor_Secondary")
    MODS = hydra.core.Name("mods")
    NAME = hydra.core.Name("name")
    PARAMSS = hydra.core.Name("paramss")
    INIT = hydra.core.Name("init")
    STATS = hydra.core.Name("stats")

@dataclass(frozen=True)
class Init:
    tpe: Type
    name: Name
    argss: frozenlist[frozenlist[Data]]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Init")
    TPE = hydra.core.Name("tpe")
    NAME = hydra.core.Name("name")
    ARGSS = hydra.core.Name("argss")

class Self(Node[None]):
    ...

Self.TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Self")

@dataclass(frozen=True)
class Template:
    early: frozenlist[Stat]
    inits: frozenlist[Init]
    self: Self
    stats: frozenlist[Stat]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Template")
    EARLY = hydra.core.Name("early")
    INITS = hydra.core.Name("inits")
    SELF = hydra.core.Name("self")
    STATS = hydra.core.Name("stats")

class ModAnnot(Node["Mod_Annot"]):
    ...

class ModPrivate(Node["Mod_Private"]):
    ...

class ModProtected(Node["Mod_Protected"]):
    ...

class ModImplicit:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModImplicit)
    def __hash__(self):
        return hash("ModImplicit")

class ModFinal:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModFinal)
    def __hash__(self):
        return hash("ModFinal")

class ModSealed:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModSealed)
    def __hash__(self):
        return hash("ModSealed")

class ModOpen:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModOpen)
    def __hash__(self):
        return hash("ModOpen")

class ModSuper:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModSuper)
    def __hash__(self):
        return hash("ModSuper")

class ModOverride:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModOverride)
    def __hash__(self):
        return hash("ModOverride")

class ModCase:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModCase)
    def __hash__(self):
        return hash("ModCase")

class ModAbstract:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModAbstract)
    def __hash__(self):
        return hash("ModAbstract")

class ModCovariant:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModCovariant)
    def __hash__(self):
        return hash("ModCovariant")

class ModContravariant:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModContravariant)
    def __hash__(self):
        return hash("ModContravariant")

class ModLazy:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModLazy)
    def __hash__(self):
        return hash("ModLazy")

class ModValParam:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModValParam)
    def __hash__(self):
        return hash("ModValParam")

class ModVarParam:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModVarParam)
    def __hash__(self):
        return hash("ModVarParam")

class ModInfix:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModInfix)
    def __hash__(self):
        return hash("ModInfix")

class ModInline:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModInline)
    def __hash__(self):
        return hash("ModInline")

class ModUsing:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModUsing)
    def __hash__(self):
        return hash("ModUsing")

class ModOpaque:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModOpaque)
    def __hash__(self):
        return hash("ModOpaque")

class ModTransparent:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ModTransparent)
    def __hash__(self):
        return hash("ModTransparent")

class _ModMeta(type):
    def __getitem__(cls, item):
        return object

class Mod(metaclass=_ModMeta):
    r"""ModAnnot | ModPrivate | ModProtected | ModImplicit | ModFinal | ModSealed | ModOpen | ModSuper | ModOverride | ModCase | ModAbstract | ModCovariant | ModContravariant | ModLazy | ModValParam | ModVarParam | ModInfix | ModInline | ModUsing | ModOpaque | ModTransparent"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Mod")
    ANNOT = hydra.core.Name("annot")
    PRIVATE = hydra.core.Name("private")
    PROTECTED = hydra.core.Name("protected")
    IMPLICIT = hydra.core.Name("implicit")
    FINAL = hydra.core.Name("final")
    SEALED = hydra.core.Name("sealed")
    OPEN = hydra.core.Name("open")
    SUPER = hydra.core.Name("super")
    OVERRIDE = hydra.core.Name("override")
    CASE = hydra.core.Name("case")
    ABSTRACT = hydra.core.Name("abstract")
    COVARIANT = hydra.core.Name("covariant")
    CONTRAVARIANT = hydra.core.Name("contravariant")
    LAZY = hydra.core.Name("lazy")
    VAL_PARAM = hydra.core.Name("valParam")
    VAR_PARAM = hydra.core.Name("varParam")
    INFIX = hydra.core.Name("infix")
    INLINE = hydra.core.Name("inline")
    USING = hydra.core.Name("using")
    OPAQUE = hydra.core.Name("opaque")
    TRANSPARENT = hydra.core.Name("transparent")

@dataclass(frozen=True)
class Mod_Annot:
    init: Init

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Mod_Annot")
    INIT = hydra.core.Name("init")

@dataclass(frozen=True)
class Mod_Private:
    within: Ref

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Mod_Private")
    WITHIN = hydra.core.Name("within")

@dataclass(frozen=True)
class Mod_Protected:
    within: Ref

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Mod_Protected")
    WITHIN = hydra.core.Name("within")

class EnumeratorGenerator(Node["Enumerator_Generator"]):
    ...

class EnumeratorCaseGenerator(Node["Enumerator_CaseGenerator"]):
    ...

class EnumeratorVal(Node["Enumerator_Val"]):
    ...

class EnumeratorGuard(Node["Enumerator_Guard"]):
    ...

class _EnumeratorMeta(type):
    def __getitem__(cls, item):
        return object

class Enumerator(metaclass=_EnumeratorMeta):
    r"""EnumeratorGenerator | EnumeratorCaseGenerator | EnumeratorVal | EnumeratorGuard"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Enumerator")
    GENERATOR = hydra.core.Name("generator")
    CASE_GENERATOR = hydra.core.Name("caseGenerator")
    VAL = hydra.core.Name("val")
    GUARD = hydra.core.Name("guard")

@dataclass(frozen=True)
class Enumerator_Generator:
    pat: Pat
    rhs: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Enumerator_Generator")
    PAT = hydra.core.Name("pat")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Enumerator_CaseGenerator:
    pat: Pat
    rhs: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Enumerator_CaseGenerator")
    PAT = hydra.core.Name("pat")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Enumerator_Val:
    pat: Pat
    rhs: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Enumerator_Val")
    PAT = hydra.core.Name("pat")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Enumerator_Guard:
    cond: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Enumerator_Guard")
    COND = hydra.core.Name("cond")

class ImportExportStatImport(Node["Import"]):
    ...

class ImportExportStatExport(Node["Export"]):
    ...

class _ImportExportStatMeta(type):
    def __getitem__(cls, item):
        return object

class ImportExportStat(metaclass=_ImportExportStatMeta):
    r"""ImportExportStatImport | ImportExportStatExport"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.ImportExportStat")
    IMPORT = hydra.core.Name("import")
    EXPORT = hydra.core.Name("export")

@dataclass(frozen=True)
class Import:
    importers: frozenlist[Importer]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Import")
    IMPORTERS = hydra.core.Name("importers")

@dataclass(frozen=True)
class Export:
    importers: frozenlist[Importer]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Export")
    IMPORTERS = hydra.core.Name("importers")

@dataclass(frozen=True)
class Importer:
    ref: Data_Ref
    importees: frozenlist[Importee]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Importer")
    REF = hydra.core.Name("ref")
    IMPORTEES = hydra.core.Name("importees")

class ImporteeWildcard:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ImporteeWildcard)
    def __hash__(self):
        return hash("ImporteeWildcard")

class ImporteeGiven(Node["Importee_Given"]):
    ...

class ImporteeGivenAll:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ImporteeGivenAll)
    def __hash__(self):
        return hash("ImporteeGivenAll")

class ImporteeName(Node["Importee_Name"]):
    ...

class ImporteeRename(Node["Importee_Rename"]):
    ...

class ImporteeUnimport(Node["Importee_Unimport"]):
    ...

class _ImporteeMeta(type):
    def __getitem__(cls, item):
        return object

class Importee(metaclass=_ImporteeMeta):
    r"""ImporteeWildcard | ImporteeGiven | ImporteeGivenAll | ImporteeName | ImporteeRename | ImporteeUnimport"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Importee")
    WILDCARD = hydra.core.Name("wildcard")
    GIVEN = hydra.core.Name("given")
    GIVEN_ALL = hydra.core.Name("givenAll")
    NAME = hydra.core.Name("name")
    RENAME = hydra.core.Name("rename")
    UNIMPORT = hydra.core.Name("unimport")

@dataclass(frozen=True)
class Importee_Given:
    tpe: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Importee_Given")
    TPE = hydra.core.Name("tpe")

@dataclass(frozen=True)
class Importee_Name:
    name: Name

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Importee_Name")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class Importee_Rename:
    name: Name
    rename: Name

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Importee_Rename")
    NAME = hydra.core.Name("name")
    RENAME = hydra.core.Name("rename")

@dataclass(frozen=True)
class Importee_Unimport:
    name: Name

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Importee_Unimport")
    NAME = hydra.core.Name("name")

class CaseTreeCase(Node["Case"]):
    ...

class CaseTreeTypeCase(Node["TypeCase"]):
    ...

class _CaseTreeMeta(type):
    def __getitem__(cls, item):
        return object

class CaseTree(metaclass=_CaseTreeMeta):
    r"""CaseTreeCase | CaseTreeTypeCase"""

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.CaseTree")
    CASE = hydra.core.Name("case")
    TYPE_CASE = hydra.core.Name("typeCase")

@dataclass(frozen=True)
class Case:
    pat: Pat
    cond: Maybe[Data]
    body: Data

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Case")
    PAT = hydra.core.Name("pat")
    COND = hydra.core.Name("cond")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class TypeCase:
    pat: Type
    body: Type

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.TypeCase")
    PAT = hydra.core.Name("pat")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Source:
    stats: frozenlist[Stat]

    TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Source")
    STATS = hydra.core.Name("stats")

class Quasi(Node[None]):
    ...

Quasi.TYPE_ = hydra.core.Name("hydra.ext.scala.syntax.Quasi")
