# Note: this is an automatically generated file. Do not edit.

r"""A subset of ANSI SQL:2003, capturing selected productions of the BNF grammar provided at https://ronsavage.github.io/SQL/sql-2003-2.bnf.html, which is based on the Final Committee Draft (FCD) of ISO/IEC 9075-2:2003."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

class ApproximateNumericLiteral(Node[str]):
    ...

ApproximateNumericLiteral.TYPE_ = hydra.core.Name("hydra.sql.syntax.ApproximateNumericLiteral")

class BinaryStringLiteral(Node[None]):
    ...

BinaryStringLiteral.TYPE_ = hydra.core.Name("hydra.sql.syntax.BinaryStringLiteral")

class CharacterStringLiteral(Node[str]):
    ...

CharacterStringLiteral.TYPE_ = hydra.core.Name("hydra.sql.syntax.CharacterStringLiteral")

class ColumnName(Node[str]):
    ...

ColumnName.TYPE_ = hydra.core.Name("hydra.sql.syntax.ColumnName")

class DateString(Node[None]):
    ...

DateString.TYPE_ = hydra.core.Name("hydra.sql.syntax.DateString")

class DomainName(Node[str]):
    ...

DomainName.TYPE_ = hydra.core.Name("hydra.sql.syntax.DomainName")

class ExactNumericLiteral(Node[str]):
    ...

ExactNumericLiteral.TYPE_ = hydra.core.Name("hydra.sql.syntax.ExactNumericLiteral")

class LeftBracketOrTrigraph(Node[str]):
    ...

LeftBracketOrTrigraph.TYPE_ = hydra.core.Name("hydra.sql.syntax.LeftBracketOrTrigraph")

class RightBracketOrTrigraph(Node[str]):
    ...

RightBracketOrTrigraph.TYPE_ = hydra.core.Name("hydra.sql.syntax.RightBracketOrTrigraph")

class NationalCharacterStringLiteral(Node[None]):
    ...

NationalCharacterStringLiteral.TYPE_ = hydra.core.Name("hydra.sql.syntax.NationalCharacterStringLiteral")

class PathResolvedUserDefinedTypeName(Node[str]):
    ...

PathResolvedUserDefinedTypeName.TYPE_ = hydra.core.Name("hydra.sql.syntax.PathResolvedUserDefinedTypeName")

class TableName(Node[str]):
    ...

TableName.TYPE_ = hydra.core.Name("hydra.sql.syntax.TableName")

class TimeString(Node[None]):
    ...

TimeString.TYPE_ = hydra.core.Name("hydra.sql.syntax.TimeString")

class TimestampLiteral(Node[None]):
    ...

TimestampLiteral.TYPE_ = hydra.core.Name("hydra.sql.syntax.TimestampLiteral")

class UnicodeCharacterStringLiteral(Node[None]):
    ...

UnicodeCharacterStringLiteral.TYPE_ = hydra.core.Name("hydra.sql.syntax.UnicodeCharacterStringLiteral")

class UnsignedInteger(Node[str]):
    ...

UnsignedInteger.TYPE_ = hydra.core.Name("hydra.sql.syntax.UnsignedInteger")

class ApproximateNumericTypeFloat(Node["Maybe[Precision]"]):
    ...

class ApproximateNumericTypeReal:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ApproximateNumericTypeReal)
    def __hash__(self):
        return hash("ApproximateNumericTypeReal")

class ApproximateNumericTypeDouble:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ApproximateNumericTypeDouble)
    def __hash__(self):
        return hash("ApproximateNumericTypeDouble")

class _ApproximateNumericTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ApproximateNumericType(metaclass=_ApproximateNumericTypeMeta):
    r"""ApproximateNumericTypeFloat | ApproximateNumericTypeReal | ApproximateNumericTypeDouble"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ApproximateNumericType")
    FLOAT = hydra.core.Name("float")
    REAL = hydra.core.Name("real")
    DOUBLE = hydra.core.Name("double")

class ArrayElement(Node["ValueExpression"]):
    ...

ArrayElement.TYPE_ = hydra.core.Name("hydra.sql.syntax.ArrayElement")

@dataclass(frozen=True)
class ArrayElementList:
    first: ArrayElement
    rest: frozenlist[ArrayElement]

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ArrayElementList")
    FIRST = hydra.core.Name("first")
    REST = hydra.core.Name("rest")

class ArrayElementReference(Node[None]):
    ...

ArrayElementReference.TYPE_ = hydra.core.Name("hydra.sql.syntax.ArrayElementReference")

class ArrayType(Node[None]):
    ...

ArrayType.TYPE_ = hydra.core.Name("hydra.sql.syntax.ArrayType")

class ArrayValueConstructorEnumeration(Node["ArrayValueConstructorByEnumeration"]):
    ...

class ArrayValueConstructorQuery(Node["ArrayValueConstructorByQuery"]):
    ...

class _ArrayValueConstructorMeta(type):
    def __getitem__(cls, item):
        return object

class ArrayValueConstructor(metaclass=_ArrayValueConstructorMeta):
    r"""ArrayValueConstructorEnumeration | ArrayValueConstructorQuery"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ArrayValueConstructor")
    ENUMERATION = hydra.core.Name("enumeration")
    QUERY = hydra.core.Name("query")

class ArrayValueConstructorByQuery(Node[None]):
    ...

ArrayValueConstructorByQuery.TYPE_ = hydra.core.Name("hydra.sql.syntax.ArrayValueConstructorByQuery")

@dataclass(frozen=True)
class ArrayValueConstructorByEnumeration:
    left_bracket_or_trigraph: LeftBracketOrTrigraph
    array_element_list: ArrayElementList
    right_bracket_or_trigraph: RightBracketOrTrigraph

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ArrayValueConstructorByEnumeration")
    LEFT_BRACKET_OR_TRIGRAPH = hydra.core.Name("LeftBracketOrTrigraph")
    ARRAY_ELEMENT_LIST = hydra.core.Name("ArrayElementList")
    RIGHT_BRACKET_OR_TRIGRAPH = hydra.core.Name("RightBracketOrTrigraph")

class ArrayValueExpression(Node[None]):
    ...

ArrayValueExpression.TYPE_ = hydra.core.Name("hydra.sql.syntax.ArrayValueExpression")

class AsSubqueryClause(Node[None]):
    ...

AsSubqueryClause.TYPE_ = hydra.core.Name("hydra.sql.syntax.AsSubqueryClause")

class AttributeOrMethodReference(Node[None]):
    ...

AttributeOrMethodReference.TYPE_ = hydra.core.Name("hydra.sql.syntax.AttributeOrMethodReference")

class BinaryLargeObjectStringTypeBinary(Node["Maybe[LargeObjectLength]"]):
    ...

class BinaryLargeObjectStringTypeBlob(Node["Maybe[LargeObjectLength]"]):
    ...

class _BinaryLargeObjectStringTypeMeta(type):
    def __getitem__(cls, item):
        return object

class BinaryLargeObjectStringType(metaclass=_BinaryLargeObjectStringTypeMeta):
    r"""BinaryLargeObjectStringTypeBinary | BinaryLargeObjectStringTypeBlob"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.BinaryLargeObjectStringType")
    BINARY = hydra.core.Name("binary")
    BLOB = hydra.core.Name("blob")

@dataclass(frozen=True)
class BooleanFactor:
    n_o_t: Maybe[None]
    boolean_test: BooleanTest

    TYPE_ = hydra.core.Name("hydra.sql.syntax.BooleanFactor")
    N_O_T = hydra.core.Name("NOT")
    BOOLEAN_TEST = hydra.core.Name("BooleanTest")

class BooleanLiteral(Enum):
    T_R_U_E = hydra.core.Name("TRUE")

    F_A_L_S_E = hydra.core.Name("FALSE")

    U_N_K_N_O_W_N = hydra.core.Name("UNKNOWN")

BooleanLiteral.TYPE_ = hydra.core.Name("hydra.sql.syntax.BooleanLiteral")

class BooleanPredicand(Node[None]):
    ...

BooleanPredicand.TYPE_ = hydra.core.Name("hydra.sql.syntax.BooleanPredicand")

class BooleanPrimaryPredicate(Node["Predicate"]):
    ...

class BooleanPrimaryPredicand(Node["BooleanPredicand"]):
    ...

class _BooleanPrimaryMeta(type):
    def __getitem__(cls, item):
        return object

class BooleanPrimary(metaclass=_BooleanPrimaryMeta):
    r"""BooleanPrimaryPredicate | BooleanPrimaryPredicand"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.BooleanPrimary")
    PREDICATE = hydra.core.Name("predicate")
    PREDICAND = hydra.core.Name("predicand")

class BooleanTermFactor(Node["BooleanFactor"]):
    ...

class BooleanTermAnd(Node["BooleanTerm_And"]):
    ...

class _BooleanTermMeta(type):
    def __getitem__(cls, item):
        return object

class BooleanTerm(metaclass=_BooleanTermMeta):
    r"""BooleanTermFactor | BooleanTermAnd"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.BooleanTerm")
    FACTOR = hydra.core.Name("factor")
    AND = hydra.core.Name("and")

@dataclass(frozen=True)
class BooleanTerm_And:
    lhs: BooleanTerm
    rhs: BooleanFactor

    TYPE_ = hydra.core.Name("hydra.sql.syntax.BooleanTerm_And")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class BooleanTest:
    boolean_primary: BooleanPrimary
    sequence: Maybe[BooleanTest_Sequence_Option]

    TYPE_ = hydra.core.Name("hydra.sql.syntax.BooleanTest")
    BOOLEAN_PRIMARY = hydra.core.Name("BooleanPrimary")
    SEQUENCE = hydra.core.Name("Sequence")

@dataclass(frozen=True)
class BooleanTest_Sequence_Option:
    n_o_t: Maybe[None]
    truth_value: TruthValue

    TYPE_ = hydra.core.Name("hydra.sql.syntax.BooleanTest_Sequence_Option")
    N_O_T = hydra.core.Name("NOT")
    TRUTH_VALUE = hydra.core.Name("TruthValue")

class BooleanType(Node[None]):
    ...

BooleanType.TYPE_ = hydra.core.Name("hydra.sql.syntax.BooleanType")

class BooleanValueExpressionTerm(Node["BooleanTerm"]):
    ...

class BooleanValueExpressionOr(Node["BooleanValueExpression_Or"]):
    ...

class _BooleanValueExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class BooleanValueExpression(metaclass=_BooleanValueExpressionMeta):
    r"""BooleanValueExpressionTerm | BooleanValueExpressionOr"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.BooleanValueExpression")
    TERM = hydra.core.Name("term")
    OR = hydra.core.Name("or")

@dataclass(frozen=True)
class BooleanValueExpression_Or:
    lhs: BooleanValueExpression
    rhs: BooleanTerm

    TYPE_ = hydra.core.Name("hydra.sql.syntax.BooleanValueExpression_Or")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

class CaseExpression(Node[None]):
    ...

CaseExpression.TYPE_ = hydra.core.Name("hydra.sql.syntax.CaseExpression")

class CastSpecification(Node[None]):
    ...

CastSpecification.TYPE_ = hydra.core.Name("hydra.sql.syntax.CastSpecification")

class CharacterSetSpecification(Node[None]):
    ...

CharacterSetSpecification.TYPE_ = hydra.core.Name("hydra.sql.syntax.CharacterSetSpecification")

class CharacterStringTypeCharacter(Node["Maybe[Length]"]):
    ...

class CharacterStringTypeChar(Node["Maybe[Length]"]):
    ...

class CharacterStringTypeCharacterVarying(Node["Length"]):
    ...

class CharacterStringTypeCharVarying(Node["Length"]):
    ...

class CharacterStringTypeVarchar(Node["Length"]):
    ...

class CharacterStringTypeCharacterLargeObject(Node["Maybe[LargeObjectLength]"]):
    ...

class CharacterStringTypeCharLargeObject(Node["Maybe[LargeObjectLength]"]):
    ...

class CharacterStringTypeClob(Node["Maybe[LargeObjectLength]"]):
    ...

class _CharacterStringTypeMeta(type):
    def __getitem__(cls, item):
        return object

class CharacterStringType(metaclass=_CharacterStringTypeMeta):
    r"""CharacterStringTypeCharacter | CharacterStringTypeChar | CharacterStringTypeCharacterVarying | CharacterStringTypeCharVarying | CharacterStringTypeVarchar | CharacterStringTypeCharacterLargeObject | CharacterStringTypeCharLargeObject | CharacterStringTypeClob"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.CharacterStringType")
    CHARACTER = hydra.core.Name("character")
    CHAR = hydra.core.Name("char")
    CHARACTER_VARYING = hydra.core.Name("characterVarying")
    CHAR_VARYING = hydra.core.Name("charVarying")
    VARCHAR = hydra.core.Name("varchar")
    CHARACTER_LARGE_OBJECT = hydra.core.Name("characterLargeObject")
    CHAR_LARGE_OBJECT = hydra.core.Name("charLargeObject")
    CLOB = hydra.core.Name("clob")

class CollateClause(Node[None]):
    ...

CollateClause.TYPE_ = hydra.core.Name("hydra.sql.syntax.CollateClause")

class CollectionTypeArray(Node["ArrayType"]):
    ...

class CollectionTypeMultiset(Node["MultisetType"]):
    ...

class _CollectionTypeMeta(type):
    def __getitem__(cls, item):
        return object

class CollectionType(metaclass=_CollectionTypeMeta):
    r"""CollectionTypeArray | CollectionTypeMultiset"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.CollectionType")
    ARRAY = hydra.core.Name("array")
    MULTISET = hydra.core.Name("multiset")

class CollectionValueConstructorArray(Node["ArrayValueConstructor"]):
    ...

class CollectionValueConstructorMultiset(Node["MultisetValueConstructor"]):
    ...

class _CollectionValueConstructorMeta(type):
    def __getitem__(cls, item):
        return object

class CollectionValueConstructor(metaclass=_CollectionValueConstructorMeta):
    r"""CollectionValueConstructorArray | CollectionValueConstructorMultiset"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.CollectionValueConstructor")
    ARRAY = hydra.core.Name("array")
    MULTISET = hydra.core.Name("multiset")

class CollectionValueExpressionArray(Node["ArrayValueExpression"]):
    ...

class CollectionValueExpressionMultiset(Node["MultisetValueExpression"]):
    ...

class _CollectionValueExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class CollectionValueExpression(metaclass=_CollectionValueExpressionMeta):
    r"""CollectionValueExpressionArray | CollectionValueExpressionMultiset"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.CollectionValueExpression")
    ARRAY = hydra.core.Name("array")
    MULTISET = hydra.core.Name("multiset")

class ColumnConstraintDefinition(Node[None]):
    ...

ColumnConstraintDefinition.TYPE_ = hydra.core.Name("hydra.sql.syntax.ColumnConstraintDefinition")

@dataclass(frozen=True)
class ColumnDefinition:
    name: ColumnName
    type_or_domain: Maybe[ColumnDefinition_TypeOrDomain_Option]
    ref_scope: Maybe[ReferenceScopeCheck]
    default_or_identity_or_generation: Maybe[ColumnDefinition_DefaultOrIdentityOrGeneration_Option]
    constraints: frozenlist[ColumnConstraintDefinition]
    collate: Maybe[CollateClause]

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ColumnDefinition")
    NAME = hydra.core.Name("name")
    TYPE_OR_DOMAIN = hydra.core.Name("typeOrDomain")
    REF_SCOPE = hydra.core.Name("refScope")
    DEFAULT_OR_IDENTITY_OR_GENERATION = hydra.core.Name("defaultOrIdentityOrGeneration")
    CONSTRAINTS = hydra.core.Name("constraints")
    COLLATE = hydra.core.Name("collate")

class ColumnDefinition_TypeOrDomain_OptionDataType(Node["DataType"]):
    ...

class ColumnDefinition_TypeOrDomain_OptionDomainName(Node["DomainName"]):
    ...

class _ColumnDefinition_TypeOrDomain_OptionMeta(type):
    def __getitem__(cls, item):
        return object

class ColumnDefinition_TypeOrDomain_Option(metaclass=_ColumnDefinition_TypeOrDomain_OptionMeta):
    r"""ColumnDefinition_TypeOrDomain_OptionDataType | ColumnDefinition_TypeOrDomain_OptionDomainName"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ColumnDefinition_TypeOrDomain_Option")
    DATA_TYPE = hydra.core.Name("DataType")
    DOMAIN_NAME = hydra.core.Name("DomainName")

class ColumnDefinition_DefaultOrIdentityOrGeneration_OptionDefaultClause(Node["DefaultClause"]):
    ...

class ColumnDefinition_DefaultOrIdentityOrGeneration_OptionIdentityColumnSpecification(Node["IdentityColumnSpecification"]):
    ...

class ColumnDefinition_DefaultOrIdentityOrGeneration_OptionGenerationClause(Node["GenerationClause"]):
    ...

class _ColumnDefinition_DefaultOrIdentityOrGeneration_OptionMeta(type):
    def __getitem__(cls, item):
        return object

class ColumnDefinition_DefaultOrIdentityOrGeneration_Option(metaclass=_ColumnDefinition_DefaultOrIdentityOrGeneration_OptionMeta):
    r"""ColumnDefinition_DefaultOrIdentityOrGeneration_OptionDefaultClause | ColumnDefinition_DefaultOrIdentityOrGeneration_OptionIdentityColumnSpecification | ColumnDefinition_DefaultOrIdentityOrGeneration_OptionGenerationClause"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option")
    DEFAULT_CLAUSE = hydra.core.Name("DefaultClause")
    IDENTITY_COLUMN_SPECIFICATION = hydra.core.Name("IdentityColumnSpecification")
    GENERATION_CLAUSE = hydra.core.Name("GenerationClause")

@dataclass(frozen=True)
class ColumnNameList:
    first: ColumnName
    rest: frozenlist[ColumnName]

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ColumnNameList")
    FIRST = hydra.core.Name("first")
    REST = hydra.core.Name("rest")

class ColumnOptions(Node[None]):
    ...

ColumnOptions.TYPE_ = hydra.core.Name("hydra.sql.syntax.ColumnOptions")

class ColumnReference(Node[None]):
    ...

ColumnReference.TYPE_ = hydra.core.Name("hydra.sql.syntax.ColumnReference")

class CommonValueExpressionNumeric(Node["NumericValueExpression"]):
    ...

class CommonValueExpressionString(Node["StringValueExpression"]):
    ...

class CommonValueExpressionDatetime(Node["DatetimeValueExpression"]):
    ...

class CommonValueExpressionInterval(Node["IntervalValueExpression"]):
    ...

class CommonValueExpressionUserDefined(Node["UserDefinedTypeValueExpression"]):
    ...

class CommonValueExpressionReference(Node["ReferenceValueExpression"]):
    ...

class CommonValueExpressionCollection(Node["CollectionValueExpression"]):
    ...

class _CommonValueExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class CommonValueExpression(metaclass=_CommonValueExpressionMeta):
    r"""CommonValueExpressionNumeric | CommonValueExpressionString | CommonValueExpressionDatetime | CommonValueExpressionInterval | CommonValueExpressionUserDefined | CommonValueExpressionReference | CommonValueExpressionCollection"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.CommonValueExpression")
    NUMERIC = hydra.core.Name("numeric")
    STRING = hydra.core.Name("string")
    DATETIME = hydra.core.Name("datetime")
    INTERVAL = hydra.core.Name("interval")
    USER_DEFINED = hydra.core.Name("userDefined")
    REFERENCE = hydra.core.Name("reference")
    COLLECTION = hydra.core.Name("collection")

class ContextuallyTypedRowValueExpressionSpecialCase(Node["RowValueSpecialCase"]):
    ...

class ContextuallyTypedRowValueExpressionConstructor(Node["ContextuallyTypedRowValueConstructor"]):
    ...

class _ContextuallyTypedRowValueExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ContextuallyTypedRowValueExpression(metaclass=_ContextuallyTypedRowValueExpressionMeta):
    r"""ContextuallyTypedRowValueExpressionSpecialCase | ContextuallyTypedRowValueExpressionConstructor"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ContextuallyTypedRowValueExpression")
    SPECIAL_CASE = hydra.core.Name("specialCase")
    CONSTRUCTOR = hydra.core.Name("constructor")

class ContextuallyTypedRowValueConstructor(Node[None]):
    ...

ContextuallyTypedRowValueConstructor.TYPE_ = hydra.core.Name("hydra.sql.syntax.ContextuallyTypedRowValueConstructor")

@dataclass(frozen=True)
class ContextuallyTypedRowValueExpressionList:
    first: ContextuallyTypedRowValueExpression
    rest: frozenlist[ContextuallyTypedRowValueExpression]

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ContextuallyTypedRowValueExpressionList")
    FIRST = hydra.core.Name("first")
    REST = hydra.core.Name("rest")

class ContextuallyTypedTableValueConstructor(Node["ContextuallyTypedRowValueExpressionList"]):
    ...

ContextuallyTypedTableValueConstructor.TYPE_ = hydra.core.Name("hydra.sql.syntax.ContextuallyTypedTableValueConstructor")

class DataTypePredefined(Node["PredefinedType"]):
    ...

class DataTypeRow(Node["RowType"]):
    ...

class DataTypeNamed(Node["PathResolvedUserDefinedTypeName"]):
    ...

class DataTypeReference(Node["ReferenceType"]):
    ...

class DataTypeCollection(Node["CollectionType"]):
    ...

class _DataTypeMeta(type):
    def __getitem__(cls, item):
        return object

class DataType(metaclass=_DataTypeMeta):
    r"""DataTypePredefined | DataTypeRow | DataTypeNamed | DataTypeReference | DataTypeCollection"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.DataType")
    PREDEFINED = hydra.core.Name("predefined")
    ROW = hydra.core.Name("row")
    NAMED = hydra.core.Name("named")
    REFERENCE = hydra.core.Name("reference")
    COLLECTION = hydra.core.Name("collection")

class DateLiteral(Node["DateString"]):
    ...

DateLiteral.TYPE_ = hydra.core.Name("hydra.sql.syntax.DateLiteral")

class DatetimeLiteralDate(Node["DateLiteral"]):
    ...

class DatetimeLiteralTime(Node["TimeLiteral"]):
    ...

class DatetimeLiteralTimestamp(Node["TimestampLiteral"]):
    ...

class _DatetimeLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class DatetimeLiteral(metaclass=_DatetimeLiteralMeta):
    r"""DatetimeLiteralDate | DatetimeLiteralTime | DatetimeLiteralTimestamp"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.DatetimeLiteral")
    DATE = hydra.core.Name("date")
    TIME = hydra.core.Name("time")
    TIMESTAMP = hydra.core.Name("timestamp")

class DatetimeType(Node[None]):
    ...

DatetimeType.TYPE_ = hydra.core.Name("hydra.sql.syntax.DatetimeType")

class DatetimeValueExpression(Node[None]):
    ...

DatetimeValueExpression.TYPE_ = hydra.core.Name("hydra.sql.syntax.DatetimeValueExpression")

class DefaultClause(Node[None]):
    ...

DefaultClause.TYPE_ = hydra.core.Name("hydra.sql.syntax.DefaultClause")

class ExactNumericTypeNumeric(Node["Maybe[ExactNumericType_Numeric_Option]"]):
    ...

class ExactNumericTypeDecimal(Node["Maybe[ExactNumericType_Decimal_Option]"]):
    ...

class ExactNumericTypeDec(Node["Maybe[ExactNumericType_Dec_Option]"]):
    ...

class ExactNumericTypeSmallint:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ExactNumericTypeSmallint)
    def __hash__(self):
        return hash("ExactNumericTypeSmallint")

class ExactNumericTypeInteger:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ExactNumericTypeInteger)
    def __hash__(self):
        return hash("ExactNumericTypeInteger")

class ExactNumericTypeInt:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ExactNumericTypeInt)
    def __hash__(self):
        return hash("ExactNumericTypeInt")

class ExactNumericTypeBigint:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ExactNumericTypeBigint)
    def __hash__(self):
        return hash("ExactNumericTypeBigint")

class _ExactNumericTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ExactNumericType(metaclass=_ExactNumericTypeMeta):
    r"""ExactNumericTypeNumeric | ExactNumericTypeDecimal | ExactNumericTypeDec | ExactNumericTypeSmallint | ExactNumericTypeInteger | ExactNumericTypeInt | ExactNumericTypeBigint"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ExactNumericType")
    NUMERIC = hydra.core.Name("numeric")
    DECIMAL = hydra.core.Name("decimal")
    DEC = hydra.core.Name("dec")
    SMALLINT = hydra.core.Name("smallint")
    INTEGER = hydra.core.Name("integer")
    INT = hydra.core.Name("int")
    BIGINT = hydra.core.Name("bigint")

@dataclass(frozen=True)
class ExactNumericType_Numeric_Option:
    precision: Precision
    sequence: Maybe[Scale]

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ExactNumericType_Numeric_Option")
    PRECISION = hydra.core.Name("Precision")
    SEQUENCE = hydra.core.Name("Sequence")

@dataclass(frozen=True)
class ExactNumericType_Decimal_Option:
    precision: Precision
    sequence: Maybe[Scale]

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ExactNumericType_Decimal_Option")
    PRECISION = hydra.core.Name("Precision")
    SEQUENCE = hydra.core.Name("Sequence")

@dataclass(frozen=True)
class ExactNumericType_Dec_Option:
    precision: Precision
    sequence: Maybe[Scale]

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ExactNumericType_Dec_Option")
    PRECISION = hydra.core.Name("Precision")
    SEQUENCE = hydra.core.Name("Sequence")

class FieldReference(Node[None]):
    ...

FieldReference.TYPE_ = hydra.core.Name("hydra.sql.syntax.FieldReference")

@dataclass(frozen=True)
class FromConstructor:
    columns: Maybe[InsertColumnList]
    override: Maybe[OverrideClause]
    values: ContextuallyTypedTableValueConstructor

    TYPE_ = hydra.core.Name("hydra.sql.syntax.FromConstructor")
    COLUMNS = hydra.core.Name("columns")
    OVERRIDE = hydra.core.Name("override")
    VALUES = hydra.core.Name("values")

class FromDefault(Node[None]):
    ...

FromDefault.TYPE_ = hydra.core.Name("hydra.sql.syntax.FromDefault")

class FromSubquery(Node[None]):
    ...

FromSubquery.TYPE_ = hydra.core.Name("hydra.sql.syntax.FromSubquery")

class GeneralLiteralString(Node["CharacterStringLiteral"]):
    ...

class GeneralLiteralNationalString(Node["NationalCharacterStringLiteral"]):
    ...

class GeneralLiteralUnicode(Node["UnicodeCharacterStringLiteral"]):
    ...

class GeneralLiteralBinary(Node["BinaryStringLiteral"]):
    ...

class GeneralLiteralDateTime(Node["DatetimeLiteral"]):
    ...

class GeneralLiteralInterval(Node["IntervalLiteral"]):
    ...

class GeneralLiteralBoolean(Node["BooleanLiteral"]):
    ...

class _GeneralLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class GeneralLiteral(metaclass=_GeneralLiteralMeta):
    r"""GeneralLiteralString | GeneralLiteralNationalString | GeneralLiteralUnicode | GeneralLiteralBinary | GeneralLiteralDateTime | GeneralLiteralInterval | GeneralLiteralBoolean"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.GeneralLiteral")
    STRING = hydra.core.Name("string")
    NATIONAL_STRING = hydra.core.Name("nationalString")
    UNICODE = hydra.core.Name("unicode")
    BINARY = hydra.core.Name("binary")
    DATE_TIME = hydra.core.Name("dateTime")
    INTERVAL = hydra.core.Name("interval")
    BOOLEAN = hydra.core.Name("boolean")

class GeneralValueSpecification(Node[None]):
    ...

GeneralValueSpecification.TYPE_ = hydra.core.Name("hydra.sql.syntax.GeneralValueSpecification")

class GenerationClause(Node[None]):
    ...

GenerationClause.TYPE_ = hydra.core.Name("hydra.sql.syntax.GenerationClause")

class GlobalOrLocal(Enum):
    GLOBAL = hydra.core.Name("global")

    LOCAL = hydra.core.Name("local")

GlobalOrLocal.TYPE_ = hydra.core.Name("hydra.sql.syntax.GlobalOrLocal")

class IdentityColumnSpecification(Node[None]):
    ...

IdentityColumnSpecification.TYPE_ = hydra.core.Name("hydra.sql.syntax.IdentityColumnSpecification")

class InsertColumnList(Node["ColumnNameList"]):
    ...

InsertColumnList.TYPE_ = hydra.core.Name("hydra.sql.syntax.InsertColumnList")

class InsertColumnsAndSourceSubquery(Node["FromSubquery"]):
    ...

class InsertColumnsAndSourceConstructor(Node["FromConstructor"]):
    ...

class InsertColumnsAndSourceDefault(Node["FromDefault"]):
    ...

class _InsertColumnsAndSourceMeta(type):
    def __getitem__(cls, item):
        return object

class InsertColumnsAndSource(metaclass=_InsertColumnsAndSourceMeta):
    r"""InsertColumnsAndSourceSubquery | InsertColumnsAndSourceConstructor | InsertColumnsAndSourceDefault"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.InsertColumnsAndSource")
    SUBQUERY = hydra.core.Name("subquery")
    CONSTRUCTOR = hydra.core.Name("constructor")
    DEFAULT = hydra.core.Name("default")

@dataclass(frozen=True)
class InsertStatement:
    target: InsertionTarget
    columns_and_source: InsertColumnsAndSource

    TYPE_ = hydra.core.Name("hydra.sql.syntax.InsertStatement")
    TARGET = hydra.core.Name("target")
    COLUMNS_AND_SOURCE = hydra.core.Name("columnsAndSource")

class InsertionTarget(Node["TableName"]):
    ...

InsertionTarget.TYPE_ = hydra.core.Name("hydra.sql.syntax.InsertionTarget")

class IntervalLiteral(Node[None]):
    ...

IntervalLiteral.TYPE_ = hydra.core.Name("hydra.sql.syntax.IntervalLiteral")

class IntervalType(Node[None]):
    ...

IntervalType.TYPE_ = hydra.core.Name("hydra.sql.syntax.IntervalType")

class IntervalValueExpression(Node[None]):
    ...

IntervalValueExpression.TYPE_ = hydra.core.Name("hydra.sql.syntax.IntervalValueExpression")

class LargeObjectLength(Node[None]):
    ...

LargeObjectLength.TYPE_ = hydra.core.Name("hydra.sql.syntax.LargeObjectLength")

class Length(Node["UnsignedInteger"]):
    ...

Length.TYPE_ = hydra.core.Name("hydra.sql.syntax.Length")

class LikeClause(Node[None]):
    ...

LikeClause.TYPE_ = hydra.core.Name("hydra.sql.syntax.LikeClause")

class MethodInvocation(Node[None]):
    ...

MethodInvocation.TYPE_ = hydra.core.Name("hydra.sql.syntax.MethodInvocation")

class MultisetElementReference(Node[None]):
    ...

MultisetElementReference.TYPE_ = hydra.core.Name("hydra.sql.syntax.MultisetElementReference")

class MultisetType(Node["DataType"]):
    ...

MultisetType.TYPE_ = hydra.core.Name("hydra.sql.syntax.MultisetType")

class MultisetValueConstructor(Node[None]):
    ...

MultisetValueConstructor.TYPE_ = hydra.core.Name("hydra.sql.syntax.MultisetValueConstructor")

class MultisetValueExpression(Node[None]):
    ...

MultisetValueExpression.TYPE_ = hydra.core.Name("hydra.sql.syntax.MultisetValueExpression")

class NationalCharacterStringType(Node[None]):
    ...

NationalCharacterStringType.TYPE_ = hydra.core.Name("hydra.sql.syntax.NationalCharacterStringType")

class NewSpecification(Node[None]):
    ...

NewSpecification.TYPE_ = hydra.core.Name("hydra.sql.syntax.NewSpecification")

class NextValueExpression(Node[None]):
    ...

NextValueExpression.TYPE_ = hydra.core.Name("hydra.sql.syntax.NextValueExpression")

class NumericTypeExact(Node["ExactNumericType"]):
    ...

class NumericTypeApproximate(Node["ApproximateNumericType"]):
    ...

class _NumericTypeMeta(type):
    def __getitem__(cls, item):
        return object

class NumericType(metaclass=_NumericTypeMeta):
    r"""NumericTypeExact | NumericTypeApproximate"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.NumericType")
    EXACT = hydra.core.Name("exact")
    APPROXIMATE = hydra.core.Name("approximate")

class NumericValueExpression(Node[None]):
    ...

NumericValueExpression.TYPE_ = hydra.core.Name("hydra.sql.syntax.NumericValueExpression")

class OverrideClause(Enum):
    O_V_E_R_R_I_D_I_N_GSP_U_S_E_RSP_V_A_L_U_E = hydra.core.Name("OVERRIDINGspUSERspVALUE")

    O_V_E_R_R_I_D_I_N_GSP_S_Y_S_T_E_MSP_V_A_L_U_E = hydra.core.Name("OVERRIDINGspSYSTEMspVALUE")

OverrideClause.TYPE_ = hydra.core.Name("hydra.sql.syntax.OverrideClause")

class ParenthesizedValueExpression(Node["ValueExpression"]):
    ...

ParenthesizedValueExpression.TYPE_ = hydra.core.Name("hydra.sql.syntax.ParenthesizedValueExpression")

class Precision(Node["UnsignedInteger"]):
    ...

Precision.TYPE_ = hydra.core.Name("hydra.sql.syntax.Precision")

class PredefinedTypeString(Node["PredefinedType_String"]):
    ...

class PredefinedTypeNationalString(Node["PredefinedType_NationalString"]):
    ...

class PredefinedTypeBlob(Node["BinaryLargeObjectStringType"]):
    ...

class PredefinedTypeNumeric(Node["NumericType"]):
    ...

class PredefinedTypeBoolean(Node["BooleanType"]):
    ...

class PredefinedTypeDatetime(Node["DatetimeType"]):
    ...

class PredefinedTypeInterval(Node["IntervalType"]):
    ...

class _PredefinedTypeMeta(type):
    def __getitem__(cls, item):
        return object

class PredefinedType(metaclass=_PredefinedTypeMeta):
    r"""PredefinedTypeString | PredefinedTypeNationalString | PredefinedTypeBlob | PredefinedTypeNumeric | PredefinedTypeBoolean | PredefinedTypeDatetime | PredefinedTypeInterval"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.PredefinedType")
    STRING = hydra.core.Name("string")
    NATIONAL_STRING = hydra.core.Name("nationalString")
    BLOB = hydra.core.Name("blob")
    NUMERIC = hydra.core.Name("numeric")
    BOOLEAN = hydra.core.Name("boolean")
    DATETIME = hydra.core.Name("datetime")
    INTERVAL = hydra.core.Name("interval")

@dataclass(frozen=True)
class PredefinedType_String:
    type: CharacterStringType
    characters: Maybe[CharacterSetSpecification]
    collate: Maybe[CollateClause]

    TYPE_ = hydra.core.Name("hydra.sql.syntax.PredefinedType_String")
    TYPE = hydra.core.Name("type")
    CHARACTERS = hydra.core.Name("characters")
    COLLATE = hydra.core.Name("collate")

@dataclass(frozen=True)
class PredefinedType_NationalString:
    type: NationalCharacterStringType
    collate: Maybe[CollateClause]

    TYPE_ = hydra.core.Name("hydra.sql.syntax.PredefinedType_NationalString")
    TYPE = hydra.core.Name("type")
    COLLATE = hydra.core.Name("collate")

class Predicate(Node[None]):
    ...

Predicate.TYPE_ = hydra.core.Name("hydra.sql.syntax.Predicate")

class QueryExpression(Node[None]):
    ...

QueryExpression.TYPE_ = hydra.core.Name("hydra.sql.syntax.QueryExpression")

class ReferenceScopeCheck(Node[None]):
    ...

ReferenceScopeCheck.TYPE_ = hydra.core.Name("hydra.sql.syntax.ReferenceScopeCheck")

class ReferenceType(Node[None]):
    ...

ReferenceType.TYPE_ = hydra.core.Name("hydra.sql.syntax.ReferenceType")

class RowType(Node[None]):
    ...

RowType.TYPE_ = hydra.core.Name("hydra.sql.syntax.RowType")

class RowValueSpecialCase(Node["NonparenthesizedValueExpressionPrimary"]):
    ...

RowValueSpecialCase.TYPE_ = hydra.core.Name("hydra.sql.syntax.RowValueSpecialCase")

class NonparenthesizedValueExpressionPrimaryUnsigned(Node["UnsignedValueSpecification"]):
    ...

class NonparenthesizedValueExpressionPrimaryColumn(Node["ColumnReference"]):
    ...

class NonparenthesizedValueExpressionPrimarySetFunction(Node["SetFunctionSpecification"]):
    ...

class NonparenthesizedValueExpressionPrimaryWindowFunction(Node["WindowFunction"]):
    ...

class NonparenthesizedValueExpressionPrimaryScalarSubquery(Node["ScalarSubquery"]):
    ...

class NonparenthesizedValueExpressionPrimaryCases(Node["CaseExpression"]):
    ...

class NonparenthesizedValueExpressionPrimaryCast(Node["CastSpecification"]):
    ...

class NonparenthesizedValueExpressionPrimaryField(Node["FieldReference"]):
    ...

class NonparenthesizedValueExpressionPrimarySubtype(Node["SubtypeTreatment"]):
    ...

class NonparenthesizedValueExpressionPrimaryMethod(Node["MethodInvocation"]):
    ...

class NonparenthesizedValueExpressionPrimaryStaticMethod(Node["StaticMethodInvocation"]):
    ...

class NonparenthesizedValueExpressionPrimaryNew(Node["NewSpecification"]):
    ...

class NonparenthesizedValueExpressionPrimaryAttributeOrMethod(Node["AttributeOrMethodReference"]):
    ...

class NonparenthesizedValueExpressionPrimaryReference(Node["ReferenceResolution"]):
    ...

class NonparenthesizedValueExpressionPrimaryCollection(Node["CollectionValueConstructor"]):
    ...

class NonparenthesizedValueExpressionPrimaryArrayElement(Node["ArrayElementReference"]):
    ...

class NonparenthesizedValueExpressionPrimaryMultisetElement(Node["MultisetElementReference"]):
    ...

class NonparenthesizedValueExpressionPrimaryRoutine(Node["RoutineInvocation"]):
    ...

class NonparenthesizedValueExpressionPrimaryNext(Node["NextValueExpression"]):
    ...

class _NonparenthesizedValueExpressionPrimaryMeta(type):
    def __getitem__(cls, item):
        return object

class NonparenthesizedValueExpressionPrimary(metaclass=_NonparenthesizedValueExpressionPrimaryMeta):
    r"""NonparenthesizedValueExpressionPrimaryUnsigned | NonparenthesizedValueExpressionPrimaryColumn | NonparenthesizedValueExpressionPrimarySetFunction | NonparenthesizedValueExpressionPrimaryWindowFunction | NonparenthesizedValueExpressionPrimaryScalarSubquery | NonparenthesizedValueExpressionPrimaryCases | NonparenthesizedValueExpressionPrimaryCast | NonparenthesizedValueExpressionPrimaryField | NonparenthesizedValueExpressionPrimarySubtype | NonparenthesizedValueExpressionPrimaryMethod | NonparenthesizedValueExpressionPrimaryStaticMethod | NonparenthesizedValueExpressionPrimaryNew | NonparenthesizedValueExpressionPrimaryAttributeOrMethod | NonparenthesizedValueExpressionPrimaryReference | NonparenthesizedValueExpressionPrimaryCollection | NonparenthesizedValueExpressionPrimaryArrayElement | NonparenthesizedValueExpressionPrimaryMultisetElement | NonparenthesizedValueExpressionPrimaryRoutine | NonparenthesizedValueExpressionPrimaryNext"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.NonparenthesizedValueExpressionPrimary")
    UNSIGNED = hydra.core.Name("unsigned")
    COLUMN = hydra.core.Name("column")
    SET_FUNCTION = hydra.core.Name("setFunction")
    WINDOW_FUNCTION = hydra.core.Name("windowFunction")
    SCALAR_SUBQUERY = hydra.core.Name("scalarSubquery")
    CASES = hydra.core.Name("cases")
    CAST = hydra.core.Name("cast")
    FIELD = hydra.core.Name("field")
    SUBTYPE = hydra.core.Name("subtype")
    METHOD = hydra.core.Name("method")
    STATIC_METHOD = hydra.core.Name("staticMethod")
    NEW = hydra.core.Name("new")
    ATTRIBUTE_OR_METHOD = hydra.core.Name("attributeOrMethod")
    REFERENCE = hydra.core.Name("reference")
    COLLECTION = hydra.core.Name("collection")
    ARRAY_ELEMENT = hydra.core.Name("arrayElement")
    MULTISET_ELEMENT = hydra.core.Name("multisetElement")
    ROUTINE = hydra.core.Name("routine")
    NEXT = hydra.core.Name("next")

class ReferenceResolution(Node[None]):
    ...

ReferenceResolution.TYPE_ = hydra.core.Name("hydra.sql.syntax.ReferenceResolution")

class ReferenceValueExpression(Node["ValueExpressionPrimary"]):
    ...

ReferenceValueExpression.TYPE_ = hydra.core.Name("hydra.sql.syntax.ReferenceValueExpression")

class RowValueExpression(Node[None]):
    ...

RowValueExpression.TYPE_ = hydra.core.Name("hydra.sql.syntax.RowValueExpression")

class RoutineInvocation(Node[None]):
    ...

RoutineInvocation.TYPE_ = hydra.core.Name("hydra.sql.syntax.RoutineInvocation")

class ScalarSubquery(Node["Subquery"]):
    ...

ScalarSubquery.TYPE_ = hydra.core.Name("hydra.sql.syntax.ScalarSubquery")

class Scale(Node["UnsignedInteger"]):
    ...

Scale.TYPE_ = hydra.core.Name("hydra.sql.syntax.Scale")

class SelfReferencingColumnSpecification(Node[None]):
    ...

SelfReferencingColumnSpecification.TYPE_ = hydra.core.Name("hydra.sql.syntax.SelfReferencingColumnSpecification")

class SetFunctionSpecification(Node[None]):
    ...

SetFunctionSpecification.TYPE_ = hydra.core.Name("hydra.sql.syntax.SetFunctionSpecification")

class StaticMethodInvocation(Node[None]):
    ...

StaticMethodInvocation.TYPE_ = hydra.core.Name("hydra.sql.syntax.StaticMethodInvocation")

class StringValueExpression(Node[None]):
    ...

StringValueExpression.TYPE_ = hydra.core.Name("hydra.sql.syntax.StringValueExpression")

class Subquery(Node["QueryExpression"]):
    ...

Subquery.TYPE_ = hydra.core.Name("hydra.sql.syntax.Subquery")

class SubtableClause(Node[None]):
    ...

SubtableClause.TYPE_ = hydra.core.Name("hydra.sql.syntax.SubtableClause")

class SubtypeTreatment(Node[None]):
    ...

SubtypeTreatment.TYPE_ = hydra.core.Name("hydra.sql.syntax.SubtypeTreatment")

class TableCommitAction(Enum):
    PRESERVE = hydra.core.Name("preserve")

    DELETE = hydra.core.Name("delete")

TableCommitAction.TYPE_ = hydra.core.Name("hydra.sql.syntax.TableCommitAction")

class TableConstraintDefinition(Node[None]):
    ...

TableConstraintDefinition.TYPE_ = hydra.core.Name("hydra.sql.syntax.TableConstraintDefinition")

class TableContentsSourceList(Node["TableElementList"]):
    ...

class TableContentsSourceSubtable(Node["TableContentsSource_Subtable"]):
    ...

class TableContentsSourceSubquery(Node["AsSubqueryClause"]):
    ...

class _TableContentsSourceMeta(type):
    def __getitem__(cls, item):
        return object

class TableContentsSource(metaclass=_TableContentsSourceMeta):
    r"""TableContentsSourceList | TableContentsSourceSubtable | TableContentsSourceSubquery"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.TableContentsSource")
    LIST = hydra.core.Name("list")
    SUBTABLE = hydra.core.Name("subtable")
    SUBQUERY = hydra.core.Name("subquery")

@dataclass(frozen=True)
class TableContentsSource_Subtable:
    type: PathResolvedUserDefinedTypeName
    subtable: Maybe[SubtableClause]
    elements: Maybe[TableElementList]

    TYPE_ = hydra.core.Name("hydra.sql.syntax.TableContentsSource_Subtable")
    TYPE = hydra.core.Name("type")
    SUBTABLE = hydra.core.Name("subtable")
    ELEMENTS = hydra.core.Name("elements")

@dataclass(frozen=True)
class TableDefinition:
    scope: Maybe[TableScope]
    name: TableName
    source: TableContentsSource
    commit_actions: Maybe[TableCommitAction]

    TYPE_ = hydra.core.Name("hydra.sql.syntax.TableDefinition")
    SCOPE = hydra.core.Name("scope")
    NAME = hydra.core.Name("name")
    SOURCE = hydra.core.Name("source")
    COMMIT_ACTIONS = hydra.core.Name("commitActions")

class TableElementColumn(Node["ColumnDefinition"]):
    ...

class TableElementTableConstraint(Node["TableConstraintDefinition"]):
    ...

class TableElementLike(Node["LikeClause"]):
    ...

class TableElementSelfReferencingColumn(Node["SelfReferencingColumnSpecification"]):
    ...

class TableElementColumOptions(Node["ColumnOptions"]):
    ...

class _TableElementMeta(type):
    def __getitem__(cls, item):
        return object

class TableElement(metaclass=_TableElementMeta):
    r"""TableElementColumn | TableElementTableConstraint | TableElementLike | TableElementSelfReferencingColumn | TableElementColumOptions"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.TableElement")
    COLUMN = hydra.core.Name("column")
    TABLE_CONSTRAINT = hydra.core.Name("tableConstraint")
    LIKE = hydra.core.Name("like")
    SELF_REFERENCING_COLUMN = hydra.core.Name("selfReferencingColumn")
    COLUM_OPTIONS = hydra.core.Name("columOptions")

@dataclass(frozen=True)
class TableElementList:
    first: TableElement
    rest: frozenlist[TableElement]

    TYPE_ = hydra.core.Name("hydra.sql.syntax.TableElementList")
    FIRST = hydra.core.Name("first")
    REST = hydra.core.Name("rest")

class TableScope(Node["GlobalOrLocal"]):
    ...

TableScope.TYPE_ = hydra.core.Name("hydra.sql.syntax.TableScope")

class TimeLiteral(Node["TimeString"]):
    ...

TimeLiteral.TYPE_ = hydra.core.Name("hydra.sql.syntax.TimeLiteral")

class TruthValue(Enum):
    T_R_U_E = hydra.core.Name("TRUE")

    F_A_L_S_E = hydra.core.Name("FALSE")

    U_N_K_N_O_W_N = hydra.core.Name("UNKNOWN")

TruthValue.TYPE_ = hydra.core.Name("hydra.sql.syntax.TruthValue")

class UnsignedLiteralNumeric(Node["UnsignedNumericLiteral"]):
    ...

class UnsignedLiteralGeneral(Node["GeneralLiteral"]):
    ...

class _UnsignedLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class UnsignedLiteral(metaclass=_UnsignedLiteralMeta):
    r"""UnsignedLiteralNumeric | UnsignedLiteralGeneral"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.UnsignedLiteral")
    NUMERIC = hydra.core.Name("numeric")
    GENERAL = hydra.core.Name("general")

class UnsignedNumericLiteralExact(Node["ExactNumericLiteral"]):
    ...

class UnsignedNumericLiteralApproximate(Node["ApproximateNumericLiteral"]):
    ...

class _UnsignedNumericLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class UnsignedNumericLiteral(metaclass=_UnsignedNumericLiteralMeta):
    r"""UnsignedNumericLiteralExact | UnsignedNumericLiteralApproximate"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.UnsignedNumericLiteral")
    EXACT = hydra.core.Name("exact")
    APPROXIMATE = hydra.core.Name("approximate")

class UnsignedValueSpecificationLiteral(Node["UnsignedLiteral"]):
    ...

class UnsignedValueSpecificationGeneral(Node["GeneralValueSpecification"]):
    ...

class _UnsignedValueSpecificationMeta(type):
    def __getitem__(cls, item):
        return object

class UnsignedValueSpecification(metaclass=_UnsignedValueSpecificationMeta):
    r"""UnsignedValueSpecificationLiteral | UnsignedValueSpecificationGeneral"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.UnsignedValueSpecification")
    LITERAL = hydra.core.Name("literal")
    GENERAL = hydra.core.Name("general")

class UserDefinedTypeValueExpression(Node["ValueExpressionPrimary"]):
    ...

UserDefinedTypeValueExpression.TYPE_ = hydra.core.Name("hydra.sql.syntax.UserDefinedTypeValueExpression")

class ValueExpressionCommon(Node["CommonValueExpression"]):
    ...

class ValueExpressionBoolean(Node["BooleanValueExpression"]):
    ...

class ValueExpressionRow(Node["RowValueExpression"]):
    ...

class _ValueExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ValueExpression(metaclass=_ValueExpressionMeta):
    r"""ValueExpressionCommon | ValueExpressionBoolean | ValueExpressionRow"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ValueExpression")
    COMMON = hydra.core.Name("common")
    BOOLEAN = hydra.core.Name("boolean")
    ROW = hydra.core.Name("row")

class ValueExpressionPrimaryParens(Node["ParenthesizedValueExpression"]):
    ...

class ValueExpressionPrimaryNoparens(Node["NonparenthesizedValueExpressionPrimary"]):
    ...

class _ValueExpressionPrimaryMeta(type):
    def __getitem__(cls, item):
        return object

class ValueExpressionPrimary(metaclass=_ValueExpressionPrimaryMeta):
    r"""ValueExpressionPrimaryParens | ValueExpressionPrimaryNoparens"""

    TYPE_ = hydra.core.Name("hydra.sql.syntax.ValueExpressionPrimary")
    PARENS = hydra.core.Name("parens")
    NOPARENS = hydra.core.Name("noparens")

class WindowFunction(Node[None]):
    ...

WindowFunction.TYPE_ = hydra.core.Name("hydra.sql.syntax.WindowFunction")
