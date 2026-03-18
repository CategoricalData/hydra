-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.org.ansi.sql.syntax

module Hydra.Dsl.Ext.Org.Ansi.Sql.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Ansi.Sql.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

approximateNumericLiteral :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ApproximateNumericLiteral
approximateNumericLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ApproximateNumericLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unApproximateNumericLiteral :: Phantoms.TTerm Syntax.ApproximateNumericLiteral -> Phantoms.TTerm String
unApproximateNumericLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ApproximateNumericLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryStringLiteral :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.BinaryStringLiteral
binaryStringLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BinaryStringLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unBinaryStringLiteral :: Phantoms.TTerm Syntax.BinaryStringLiteral -> Phantoms.TTerm ()
unBinaryStringLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.BinaryStringLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

characterStringLiteral :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.CharacterStringLiteral
characterStringLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterStringLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unCharacterStringLiteral :: Phantoms.TTerm Syntax.CharacterStringLiteral -> Phantoms.TTerm String
unCharacterStringLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterStringLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnName :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ColumnName
columnName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unColumnName :: Phantoms.TTerm Syntax.ColumnName -> Phantoms.TTerm String
unColumnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dateString :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.DateString
dateString x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DateString"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDateString :: Phantoms.TTerm Syntax.DateString -> Phantoms.TTerm ()
unDateString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.DateString")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

domainName :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.DomainName
domainName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DomainName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDomainName :: Phantoms.TTerm Syntax.DomainName -> Phantoms.TTerm String
unDomainName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.DomainName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exactNumericLiteral :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ExactNumericLiteral
exactNumericLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unExactNumericLiteral :: Phantoms.TTerm Syntax.ExactNumericLiteral -> Phantoms.TTerm String
unExactNumericLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

leftBracketOrTrigraph :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.LeftBracketOrTrigraph
leftBracketOrTrigraph x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.LeftBracketOrTrigraph"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unLeftBracketOrTrigraph :: Phantoms.TTerm Syntax.LeftBracketOrTrigraph -> Phantoms.TTerm String
unLeftBracketOrTrigraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.LeftBracketOrTrigraph")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rightBracketOrTrigraph :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.RightBracketOrTrigraph
rightBracketOrTrigraph x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.RightBracketOrTrigraph"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unRightBracketOrTrigraph :: Phantoms.TTerm Syntax.RightBracketOrTrigraph -> Phantoms.TTerm String
unRightBracketOrTrigraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.RightBracketOrTrigraph")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nationalCharacterStringLiteral :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.NationalCharacterStringLiteral
nationalCharacterStringLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NationalCharacterStringLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unNationalCharacterStringLiteral :: Phantoms.TTerm Syntax.NationalCharacterStringLiteral -> Phantoms.TTerm ()
unNationalCharacterStringLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.NationalCharacterStringLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pathResolvedUserDefinedTypeName :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.PathResolvedUserDefinedTypeName
pathResolvedUserDefinedTypeName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PathResolvedUserDefinedTypeName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPathResolvedUserDefinedTypeName :: Phantoms.TTerm Syntax.PathResolvedUserDefinedTypeName -> Phantoms.TTerm String
unPathResolvedUserDefinedTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.PathResolvedUserDefinedTypeName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableName :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.TableName
tableName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unTableName :: Phantoms.TTerm Syntax.TableName -> Phantoms.TTerm String
unTableName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.TableName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

timeString :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.TimeString
timeString x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TimeString"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unTimeString :: Phantoms.TTerm Syntax.TimeString -> Phantoms.TTerm ()
unTimeString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.TimeString")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

timestampLiteral :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.TimestampLiteral
timestampLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TimestampLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unTimestampLiteral :: Phantoms.TTerm Syntax.TimestampLiteral -> Phantoms.TTerm ()
unTimestampLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.TimestampLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unicodeCharacterStringLiteral :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.UnicodeCharacterStringLiteral
unicodeCharacterStringLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.UnicodeCharacterStringLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unUnicodeCharacterStringLiteral :: Phantoms.TTerm Syntax.UnicodeCharacterStringLiteral -> Phantoms.TTerm ()
unUnicodeCharacterStringLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.UnicodeCharacterStringLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unsignedInteger :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.UnsignedInteger
unsignedInteger x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.UnsignedInteger"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unUnsignedInteger :: Phantoms.TTerm Syntax.UnsignedInteger -> Phantoms.TTerm String
unUnsignedInteger x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.UnsignedInteger")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

approximateNumericTypeFloat :: Phantoms.TTerm (Maybe Syntax.Precision) -> Phantoms.TTerm Syntax.ApproximateNumericType
approximateNumericTypeFloat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ApproximateNumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

approximateNumericTypeReal :: Phantoms.TTerm Syntax.ApproximateNumericType
approximateNumericTypeReal =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ApproximateNumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "real"),
        Core.fieldTerm = Core.TermUnit}}))

approximateNumericTypeDouble :: Phantoms.TTerm Syntax.ApproximateNumericType
approximateNumericTypeDouble =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ApproximateNumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = Core.TermUnit}}))

arrayElement :: Phantoms.TTerm Syntax.ValueExpression -> Phantoms.TTerm Syntax.ArrayElement
arrayElement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unArrayElement :: Phantoms.TTerm Syntax.ArrayElement -> Phantoms.TTerm Syntax.ValueExpression
unArrayElement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayElementList :: Phantoms.TTerm Syntax.ArrayElement -> Phantoms.TTerm [Syntax.ArrayElement] -> Phantoms.TTerm Syntax.ArrayElementList
arrayElementList first rest =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElementList"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm rest)}]}))

arrayElementListFirst :: Phantoms.TTerm Syntax.ArrayElementList -> Phantoms.TTerm Syntax.ArrayElement
arrayElementListFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElementList"),
        Core.projectionField = (Core.Name "first")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayElementListRest :: Phantoms.TTerm Syntax.ArrayElementList -> Phantoms.TTerm [Syntax.ArrayElement]
arrayElementListRest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElementList"),
        Core.projectionField = (Core.Name "rest")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayElementListWithFirst :: Phantoms.TTerm Syntax.ArrayElementList -> Phantoms.TTerm Syntax.ArrayElement -> Phantoms.TTerm Syntax.ArrayElementList
arrayElementListWithFirst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElementList"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElementList"),
              Core.projectionField = (Core.Name "rest")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayElementListWithRest :: Phantoms.TTerm Syntax.ArrayElementList -> Phantoms.TTerm [Syntax.ArrayElement] -> Phantoms.TTerm Syntax.ArrayElementList
arrayElementListWithRest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElementList"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElementList"),
              Core.projectionField = (Core.Name "first")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arrayElementReference :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ArrayElementReference
arrayElementReference x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElementReference"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unArrayElementReference :: Phantoms.TTerm Syntax.ArrayElementReference -> Phantoms.TTerm ()
unArrayElementReference x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElementReference")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayType :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ArrayType
arrayType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unArrayType :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm ()
unArrayType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayType")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayValueConstructorEnumeration :: Phantoms.TTerm Syntax.ArrayValueConstructorByEnumeration -> Phantoms.TTerm Syntax.ArrayValueConstructor
arrayValueConstructorEnumeration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enumeration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayValueConstructorQuery :: Phantoms.TTerm Syntax.ArrayValueConstructorByQuery -> Phantoms.TTerm Syntax.ArrayValueConstructor
arrayValueConstructorQuery x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "query"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayValueConstructorByQuery :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ArrayValueConstructorByQuery
arrayValueConstructorByQuery x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByQuery"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unArrayValueConstructorByQuery :: Phantoms.TTerm Syntax.ArrayValueConstructorByQuery -> Phantoms.TTerm ()
unArrayValueConstructorByQuery x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByQuery")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayValueConstructorByEnumeration :: Phantoms.TTerm Syntax.LeftBracketOrTrigraph -> Phantoms.TTerm Syntax.ArrayElementList -> Phantoms.TTerm Syntax.RightBracketOrTrigraph -> Phantoms.TTerm Syntax.ArrayValueConstructorByEnumeration
arrayValueConstructorByEnumeration leftBracketOrTrigraph arrayElementList rightBracketOrTrigraph =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "LeftBracketOrTrigraph"),
          Core.fieldTerm = (Phantoms.unTTerm leftBracketOrTrigraph)},
        Core.Field {
          Core.fieldName = (Core.Name "ArrayElementList"),
          Core.fieldTerm = (Phantoms.unTTerm arrayElementList)},
        Core.Field {
          Core.fieldName = (Core.Name "RightBracketOrTrigraph"),
          Core.fieldTerm = (Phantoms.unTTerm rightBracketOrTrigraph)}]}))

arrayValueConstructorByEnumerationLeftBracketOrTrigraph :: Phantoms.TTerm Syntax.ArrayValueConstructorByEnumeration -> Phantoms.TTerm Syntax.LeftBracketOrTrigraph
arrayValueConstructorByEnumerationLeftBracketOrTrigraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration"),
        Core.projectionField = (Core.Name "LeftBracketOrTrigraph")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayValueConstructorByEnumerationArrayElementList :: Phantoms.TTerm Syntax.ArrayValueConstructorByEnumeration -> Phantoms.TTerm Syntax.ArrayElementList
arrayValueConstructorByEnumerationArrayElementList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration"),
        Core.projectionField = (Core.Name "ArrayElementList")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayValueConstructorByEnumerationRightBracketOrTrigraph :: Phantoms.TTerm Syntax.ArrayValueConstructorByEnumeration -> Phantoms.TTerm Syntax.RightBracketOrTrigraph
arrayValueConstructorByEnumerationRightBracketOrTrigraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration"),
        Core.projectionField = (Core.Name "RightBracketOrTrigraph")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrayValueConstructorByEnumerationWithLeftBracketOrTrigraph :: Phantoms.TTerm Syntax.ArrayValueConstructorByEnumeration -> Phantoms.TTerm Syntax.LeftBracketOrTrigraph -> Phantoms.TTerm Syntax.ArrayValueConstructorByEnumeration
arrayValueConstructorByEnumerationWithLeftBracketOrTrigraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "LeftBracketOrTrigraph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ArrayElementList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration"),
              Core.projectionField = (Core.Name "ArrayElementList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "RightBracketOrTrigraph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration"),
              Core.projectionField = (Core.Name "RightBracketOrTrigraph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayValueConstructorByEnumerationWithArrayElementList :: Phantoms.TTerm Syntax.ArrayValueConstructorByEnumeration -> Phantoms.TTerm Syntax.ArrayElementList -> Phantoms.TTerm Syntax.ArrayValueConstructorByEnumeration
arrayValueConstructorByEnumerationWithArrayElementList original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "LeftBracketOrTrigraph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration"),
              Core.projectionField = (Core.Name "LeftBracketOrTrigraph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ArrayElementList"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "RightBracketOrTrigraph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration"),
              Core.projectionField = (Core.Name "RightBracketOrTrigraph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrayValueConstructorByEnumerationWithRightBracketOrTrigraph :: Phantoms.TTerm Syntax.ArrayValueConstructorByEnumeration -> Phantoms.TTerm Syntax.RightBracketOrTrigraph -> Phantoms.TTerm Syntax.ArrayValueConstructorByEnumeration
arrayValueConstructorByEnumerationWithRightBracketOrTrigraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "LeftBracketOrTrigraph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration"),
              Core.projectionField = (Core.Name "LeftBracketOrTrigraph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ArrayElementList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration"),
              Core.projectionField = (Core.Name "ArrayElementList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "RightBracketOrTrigraph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arrayValueExpression :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ArrayValueExpression
arrayValueExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unArrayValueExpression :: Phantoms.TTerm Syntax.ArrayValueExpression -> Phantoms.TTerm ()
unArrayValueExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

asSubqueryClause :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.AsSubqueryClause
asSubqueryClause x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.AsSubqueryClause"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unAsSubqueryClause :: Phantoms.TTerm Syntax.AsSubqueryClause -> Phantoms.TTerm ()
unAsSubqueryClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.AsSubqueryClause")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

attributeOrMethodReference :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.AttributeOrMethodReference
attributeOrMethodReference x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.AttributeOrMethodReference"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unAttributeOrMethodReference :: Phantoms.TTerm Syntax.AttributeOrMethodReference -> Phantoms.TTerm ()
unAttributeOrMethodReference x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.AttributeOrMethodReference")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryLargeObjectStringTypeBinary :: Phantoms.TTerm (Maybe Syntax.LargeObjectLength) -> Phantoms.TTerm Syntax.BinaryLargeObjectStringType
binaryLargeObjectStringTypeBinary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BinaryLargeObjectStringType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binaryLargeObjectStringTypeBlob :: Phantoms.TTerm (Maybe Syntax.LargeObjectLength) -> Phantoms.TTerm Syntax.BinaryLargeObjectStringType
binaryLargeObjectStringTypeBlob x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BinaryLargeObjectStringType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blob"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

booleanFactor :: Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.BooleanTest -> Phantoms.TTerm Syntax.BooleanFactor
booleanFactor nOT booleanTest =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanFactor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NOT"),
          Core.fieldTerm = (Phantoms.unTTerm nOT)},
        Core.Field {
          Core.fieldName = (Core.Name "BooleanTest"),
          Core.fieldTerm = (Phantoms.unTTerm booleanTest)}]}))

booleanFactorNOT :: Phantoms.TTerm Syntax.BooleanFactor -> Phantoms.TTerm (Maybe ())
booleanFactorNOT x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanFactor"),
        Core.projectionField = (Core.Name "NOT")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

booleanFactorBooleanTest :: Phantoms.TTerm Syntax.BooleanFactor -> Phantoms.TTerm Syntax.BooleanTest
booleanFactorBooleanTest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanFactor"),
        Core.projectionField = (Core.Name "BooleanTest")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

booleanFactorWithNOT :: Phantoms.TTerm Syntax.BooleanFactor -> Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.BooleanFactor
booleanFactorWithNOT original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanFactor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NOT"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "BooleanTest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanFactor"),
              Core.projectionField = (Core.Name "BooleanTest")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

booleanFactorWithBooleanTest :: Phantoms.TTerm Syntax.BooleanFactor -> Phantoms.TTerm Syntax.BooleanTest -> Phantoms.TTerm Syntax.BooleanFactor
booleanFactorWithBooleanTest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanFactor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NOT"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanFactor"),
              Core.projectionField = (Core.Name "NOT")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "BooleanTest"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

booleanLiteralTRUE :: Phantoms.TTerm Syntax.BooleanLiteral
booleanLiteralTRUE =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "TRUE"),
        Core.fieldTerm = Core.TermUnit}}))

booleanLiteralFALSE :: Phantoms.TTerm Syntax.BooleanLiteral
booleanLiteralFALSE =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "FALSE"),
        Core.fieldTerm = Core.TermUnit}}))

booleanLiteralUNKNOWN :: Phantoms.TTerm Syntax.BooleanLiteral
booleanLiteralUNKNOWN =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "UNKNOWN"),
        Core.fieldTerm = Core.TermUnit}}))

booleanPredicand :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.BooleanPredicand
booleanPredicand x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanPredicand"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unBooleanPredicand :: Phantoms.TTerm Syntax.BooleanPredicand -> Phantoms.TTerm ()
unBooleanPredicand x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanPredicand")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

booleanPrimaryPredicate :: Phantoms.TTerm Syntax.Predicate -> Phantoms.TTerm Syntax.BooleanPrimary
booleanPrimaryPredicate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

booleanPrimaryPredicand :: Phantoms.TTerm Syntax.BooleanPredicand -> Phantoms.TTerm Syntax.BooleanPrimary
booleanPrimaryPredicand x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predicand"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

booleanTermFactor :: Phantoms.TTerm Syntax.BooleanFactor -> Phantoms.TTerm Syntax.BooleanTerm
booleanTermFactor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTerm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "factor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

booleanTermAnd :: Phantoms.TTerm Syntax.BooleanTerm_And -> Phantoms.TTerm Syntax.BooleanTerm
booleanTermAnd x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTerm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

booleanTerm_And :: Phantoms.TTerm Syntax.BooleanTerm -> Phantoms.TTerm Syntax.BooleanFactor -> Phantoms.TTerm Syntax.BooleanTerm_And
booleanTerm_And lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTerm_And"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

booleanTerm_AndLhs :: Phantoms.TTerm Syntax.BooleanTerm_And -> Phantoms.TTerm Syntax.BooleanTerm
booleanTerm_AndLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTerm_And"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

booleanTerm_AndRhs :: Phantoms.TTerm Syntax.BooleanTerm_And -> Phantoms.TTerm Syntax.BooleanFactor
booleanTerm_AndRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTerm_And"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

booleanTerm_AndWithLhs :: Phantoms.TTerm Syntax.BooleanTerm_And -> Phantoms.TTerm Syntax.BooleanTerm -> Phantoms.TTerm Syntax.BooleanTerm_And
booleanTerm_AndWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTerm_And"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTerm_And"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

booleanTerm_AndWithRhs :: Phantoms.TTerm Syntax.BooleanTerm_And -> Phantoms.TTerm Syntax.BooleanFactor -> Phantoms.TTerm Syntax.BooleanTerm_And
booleanTerm_AndWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTerm_And"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTerm_And"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

booleanTest :: Phantoms.TTerm Syntax.BooleanPrimary -> Phantoms.TTerm (Maybe Syntax.BooleanTest_Sequence_Option) -> Phantoms.TTerm Syntax.BooleanTest
booleanTest booleanPrimary sequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "BooleanPrimary"),
          Core.fieldTerm = (Phantoms.unTTerm booleanPrimary)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm sequence)}]}))

booleanTestBooleanPrimary :: Phantoms.TTerm Syntax.BooleanTest -> Phantoms.TTerm Syntax.BooleanPrimary
booleanTestBooleanPrimary x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest"),
        Core.projectionField = (Core.Name "BooleanPrimary")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

booleanTestSequence :: Phantoms.TTerm Syntax.BooleanTest -> Phantoms.TTerm (Maybe Syntax.BooleanTest_Sequence_Option)
booleanTestSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest"),
        Core.projectionField = (Core.Name "Sequence")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

booleanTestWithBooleanPrimary :: Phantoms.TTerm Syntax.BooleanTest -> Phantoms.TTerm Syntax.BooleanPrimary -> Phantoms.TTerm Syntax.BooleanTest
booleanTestWithBooleanPrimary original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "BooleanPrimary"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest"),
              Core.projectionField = (Core.Name "Sequence")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

booleanTestWithSequence :: Phantoms.TTerm Syntax.BooleanTest -> Phantoms.TTerm (Maybe Syntax.BooleanTest_Sequence_Option) -> Phantoms.TTerm Syntax.BooleanTest
booleanTestWithSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "BooleanPrimary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest"),
              Core.projectionField = (Core.Name "BooleanPrimary")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

booleanTest_Sequence_Option :: Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.TruthValue -> Phantoms.TTerm Syntax.BooleanTest_Sequence_Option
booleanTest_Sequence_Option nOT truthValue =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest_Sequence_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NOT"),
          Core.fieldTerm = (Phantoms.unTTerm nOT)},
        Core.Field {
          Core.fieldName = (Core.Name "TruthValue"),
          Core.fieldTerm = (Phantoms.unTTerm truthValue)}]}))

booleanTest_Sequence_OptionNOT :: Phantoms.TTerm Syntax.BooleanTest_Sequence_Option -> Phantoms.TTerm (Maybe ())
booleanTest_Sequence_OptionNOT x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest_Sequence_Option"),
        Core.projectionField = (Core.Name "NOT")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

booleanTest_Sequence_OptionTruthValue :: Phantoms.TTerm Syntax.BooleanTest_Sequence_Option -> Phantoms.TTerm Syntax.TruthValue
booleanTest_Sequence_OptionTruthValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest_Sequence_Option"),
        Core.projectionField = (Core.Name "TruthValue")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

booleanTest_Sequence_OptionWithNOT :: Phantoms.TTerm Syntax.BooleanTest_Sequence_Option -> Phantoms.TTerm (Maybe ()) -> Phantoms.TTerm Syntax.BooleanTest_Sequence_Option
booleanTest_Sequence_OptionWithNOT original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest_Sequence_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NOT"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "TruthValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest_Sequence_Option"),
              Core.projectionField = (Core.Name "TruthValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

booleanTest_Sequence_OptionWithTruthValue :: Phantoms.TTerm Syntax.BooleanTest_Sequence_Option -> Phantoms.TTerm Syntax.TruthValue -> Phantoms.TTerm Syntax.BooleanTest_Sequence_Option
booleanTest_Sequence_OptionWithTruthValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest_Sequence_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "NOT"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest_Sequence_Option"),
              Core.projectionField = (Core.Name "NOT")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "TruthValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

booleanType :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.BooleanType
booleanType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unBooleanType :: Phantoms.TTerm Syntax.BooleanType -> Phantoms.TTerm ()
unBooleanType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanType")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

booleanValueExpressionTerm :: Phantoms.TTerm Syntax.BooleanTerm -> Phantoms.TTerm Syntax.BooleanValueExpression
booleanValueExpressionTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

booleanValueExpressionOr :: Phantoms.TTerm Syntax.BooleanValueExpression_Or -> Phantoms.TTerm Syntax.BooleanValueExpression
booleanValueExpressionOr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

booleanValueExpression_Or :: Phantoms.TTerm Syntax.BooleanValueExpression -> Phantoms.TTerm Syntax.BooleanTerm -> Phantoms.TTerm Syntax.BooleanValueExpression_Or
booleanValueExpression_Or lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanValueExpression_Or"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

booleanValueExpression_OrLhs :: Phantoms.TTerm Syntax.BooleanValueExpression_Or -> Phantoms.TTerm Syntax.BooleanValueExpression
booleanValueExpression_OrLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanValueExpression_Or"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

booleanValueExpression_OrRhs :: Phantoms.TTerm Syntax.BooleanValueExpression_Or -> Phantoms.TTerm Syntax.BooleanTerm
booleanValueExpression_OrRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanValueExpression_Or"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

booleanValueExpression_OrWithLhs :: Phantoms.TTerm Syntax.BooleanValueExpression_Or -> Phantoms.TTerm Syntax.BooleanValueExpression -> Phantoms.TTerm Syntax.BooleanValueExpression_Or
booleanValueExpression_OrWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanValueExpression_Or"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanValueExpression_Or"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

booleanValueExpression_OrWithRhs :: Phantoms.TTerm Syntax.BooleanValueExpression_Or -> Phantoms.TTerm Syntax.BooleanTerm -> Phantoms.TTerm Syntax.BooleanValueExpression_Or
booleanValueExpression_OrWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanValueExpression_Or"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanValueExpression_Or"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

caseExpression :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.CaseExpression
caseExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CaseExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unCaseExpression :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm ()
unCaseExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.CaseExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

castSpecification :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.CastSpecification
castSpecification x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CastSpecification"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unCastSpecification :: Phantoms.TTerm Syntax.CastSpecification -> Phantoms.TTerm ()
unCastSpecification x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.CastSpecification")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

characterSetSpecification :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.CharacterSetSpecification
characterSetSpecification x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterSetSpecification"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unCharacterSetSpecification :: Phantoms.TTerm Syntax.CharacterSetSpecification -> Phantoms.TTerm ()
unCharacterSetSpecification x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterSetSpecification")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

characterStringTypeCharacter :: Phantoms.TTerm (Maybe Syntax.Length) -> Phantoms.TTerm Syntax.CharacterStringType
characterStringTypeCharacter x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterStringType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "character"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

characterStringTypeChar :: Phantoms.TTerm (Maybe Syntax.Length) -> Phantoms.TTerm Syntax.CharacterStringType
characterStringTypeChar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterStringType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

characterStringTypeCharacterVarying :: Phantoms.TTerm Syntax.Length -> Phantoms.TTerm Syntax.CharacterStringType
characterStringTypeCharacterVarying x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterStringType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "characterVarying"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

characterStringTypeCharVarying :: Phantoms.TTerm Syntax.Length -> Phantoms.TTerm Syntax.CharacterStringType
characterStringTypeCharVarying x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterStringType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "charVarying"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

characterStringTypeVarchar :: Phantoms.TTerm Syntax.Length -> Phantoms.TTerm Syntax.CharacterStringType
characterStringTypeVarchar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterStringType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "varchar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

characterStringTypeCharacterLargeObject :: Phantoms.TTerm (Maybe Syntax.LargeObjectLength) -> Phantoms.TTerm Syntax.CharacterStringType
characterStringTypeCharacterLargeObject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterStringType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "characterLargeObject"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

characterStringTypeCharLargeObject :: Phantoms.TTerm (Maybe Syntax.LargeObjectLength) -> Phantoms.TTerm Syntax.CharacterStringType
characterStringTypeCharLargeObject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterStringType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "charLargeObject"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

characterStringTypeClob :: Phantoms.TTerm (Maybe Syntax.LargeObjectLength) -> Phantoms.TTerm Syntax.CharacterStringType
characterStringTypeClob x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterStringType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "clob"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

collateClause :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.CollateClause
collateClause x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CollateClause"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unCollateClause :: Phantoms.TTerm Syntax.CollateClause -> Phantoms.TTerm ()
unCollateClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.CollateClause")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

collectionTypeArray :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.CollectionType
collectionTypeArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CollectionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

collectionTypeMultiset :: Phantoms.TTerm Syntax.MultisetType -> Phantoms.TTerm Syntax.CollectionType
collectionTypeMultiset x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CollectionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiset"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

collectionValueConstructorArray :: Phantoms.TTerm Syntax.ArrayValueConstructor -> Phantoms.TTerm Syntax.CollectionValueConstructor
collectionValueConstructorArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CollectionValueConstructor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

collectionValueConstructorMultiset :: Phantoms.TTerm Syntax.MultisetValueConstructor -> Phantoms.TTerm Syntax.CollectionValueConstructor
collectionValueConstructorMultiset x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CollectionValueConstructor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiset"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

collectionValueExpressionArray :: Phantoms.TTerm Syntax.ArrayValueExpression -> Phantoms.TTerm Syntax.CollectionValueExpression
collectionValueExpressionArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CollectionValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

collectionValueExpressionMultiset :: Phantoms.TTerm Syntax.MultisetValueExpression -> Phantoms.TTerm Syntax.CollectionValueExpression
collectionValueExpressionMultiset x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CollectionValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiset"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

columnConstraintDefinition :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ColumnConstraintDefinition
columnConstraintDefinition x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnConstraintDefinition"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unColumnConstraintDefinition :: Phantoms.TTerm Syntax.ColumnConstraintDefinition -> Phantoms.TTerm ()
unColumnConstraintDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnConstraintDefinition")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnDefinition :: Phantoms.TTerm Syntax.ColumnName -> Phantoms.TTerm (Maybe Syntax.ColumnDefinition_TypeOrDomain_Option) -> Phantoms.TTerm (Maybe Syntax.ReferenceScopeCheck) -> Phantoms.TTerm (Maybe Syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option) -> Phantoms.TTerm [Syntax.ColumnConstraintDefinition] -> Phantoms.TTerm (Maybe Syntax.CollateClause) -> Phantoms.TTerm Syntax.ColumnDefinition
columnDefinition name typeOrDomain refScope defaultOrIdentityOrGeneration constraints collate =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeOrDomain"),
          Core.fieldTerm = (Phantoms.unTTerm typeOrDomain)},
        Core.Field {
          Core.fieldName = (Core.Name "refScope"),
          Core.fieldTerm = (Phantoms.unTTerm refScope)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultOrIdentityOrGeneration"),
          Core.fieldTerm = (Phantoms.unTTerm defaultOrIdentityOrGeneration)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm constraints)},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Phantoms.unTTerm collate)}]}))

columnDefinitionName :: Phantoms.TTerm Syntax.ColumnDefinition -> Phantoms.TTerm Syntax.ColumnName
columnDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnDefinitionTypeOrDomain :: Phantoms.TTerm Syntax.ColumnDefinition -> Phantoms.TTerm (Maybe Syntax.ColumnDefinition_TypeOrDomain_Option)
columnDefinitionTypeOrDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
        Core.projectionField = (Core.Name "typeOrDomain")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnDefinitionRefScope :: Phantoms.TTerm Syntax.ColumnDefinition -> Phantoms.TTerm (Maybe Syntax.ReferenceScopeCheck)
columnDefinitionRefScope x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
        Core.projectionField = (Core.Name "refScope")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnDefinitionDefaultOrIdentityOrGeneration :: Phantoms.TTerm Syntax.ColumnDefinition -> Phantoms.TTerm (Maybe Syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option)
columnDefinitionDefaultOrIdentityOrGeneration x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
        Core.projectionField = (Core.Name "defaultOrIdentityOrGeneration")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnDefinitionConstraints :: Phantoms.TTerm Syntax.ColumnDefinition -> Phantoms.TTerm [Syntax.ColumnConstraintDefinition]
columnDefinitionConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
        Core.projectionField = (Core.Name "constraints")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnDefinitionCollate :: Phantoms.TTerm Syntax.ColumnDefinition -> Phantoms.TTerm (Maybe Syntax.CollateClause)
columnDefinitionCollate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
        Core.projectionField = (Core.Name "collate")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnDefinitionWithName :: Phantoms.TTerm Syntax.ColumnDefinition -> Phantoms.TTerm Syntax.ColumnName -> Phantoms.TTerm Syntax.ColumnDefinition
columnDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeOrDomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "typeOrDomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "refScope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "refScope")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultOrIdentityOrGeneration"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "defaultOrIdentityOrGeneration")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "collate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnDefinitionWithTypeOrDomain :: Phantoms.TTerm Syntax.ColumnDefinition -> Phantoms.TTerm (Maybe Syntax.ColumnDefinition_TypeOrDomain_Option) -> Phantoms.TTerm Syntax.ColumnDefinition
columnDefinitionWithTypeOrDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeOrDomain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "refScope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "refScope")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultOrIdentityOrGeneration"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "defaultOrIdentityOrGeneration")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "collate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnDefinitionWithRefScope :: Phantoms.TTerm Syntax.ColumnDefinition -> Phantoms.TTerm (Maybe Syntax.ReferenceScopeCheck) -> Phantoms.TTerm Syntax.ColumnDefinition
columnDefinitionWithRefScope original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeOrDomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "typeOrDomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "refScope"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultOrIdentityOrGeneration"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "defaultOrIdentityOrGeneration")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "collate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnDefinitionWithDefaultOrIdentityOrGeneration :: Phantoms.TTerm Syntax.ColumnDefinition -> Phantoms.TTerm (Maybe Syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option) -> Phantoms.TTerm Syntax.ColumnDefinition
columnDefinitionWithDefaultOrIdentityOrGeneration original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeOrDomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "typeOrDomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "refScope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "refScope")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultOrIdentityOrGeneration"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "collate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnDefinitionWithConstraints :: Phantoms.TTerm Syntax.ColumnDefinition -> Phantoms.TTerm [Syntax.ColumnConstraintDefinition] -> Phantoms.TTerm Syntax.ColumnDefinition
columnDefinitionWithConstraints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeOrDomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "typeOrDomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "refScope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "refScope")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultOrIdentityOrGeneration"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "defaultOrIdentityOrGeneration")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "collate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnDefinitionWithCollate :: Phantoms.TTerm Syntax.ColumnDefinition -> Phantoms.TTerm (Maybe Syntax.CollateClause) -> Phantoms.TTerm Syntax.ColumnDefinition
columnDefinitionWithCollate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeOrDomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "typeOrDomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "refScope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "refScope")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultOrIdentityOrGeneration"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "defaultOrIdentityOrGeneration")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

columnDefinition_TypeOrDomain_OptionDataType :: Phantoms.TTerm Syntax.DataType -> Phantoms.TTerm Syntax.ColumnDefinition_TypeOrDomain_Option
columnDefinition_TypeOrDomain_OptionDataType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition_TypeOrDomain_Option"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "DataType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

columnDefinition_TypeOrDomain_OptionDomainName :: Phantoms.TTerm Syntax.DomainName -> Phantoms.TTerm Syntax.ColumnDefinition_TypeOrDomain_Option
columnDefinition_TypeOrDomain_OptionDomainName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition_TypeOrDomain_Option"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "DomainName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

columnDefinition_DefaultOrIdentityOrGeneration_OptionDefaultClause :: Phantoms.TTerm Syntax.DefaultClause -> Phantoms.TTerm Syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option
columnDefinition_DefaultOrIdentityOrGeneration_OptionDefaultClause x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "DefaultClause"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

columnDefinition_DefaultOrIdentityOrGeneration_OptionIdentityColumnSpecification :: Phantoms.TTerm Syntax.IdentityColumnSpecification -> Phantoms.TTerm Syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option
columnDefinition_DefaultOrIdentityOrGeneration_OptionIdentityColumnSpecification x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "IdentityColumnSpecification"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

columnDefinition_DefaultOrIdentityOrGeneration_OptionGenerationClause :: Phantoms.TTerm Syntax.GenerationClause -> Phantoms.TTerm Syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option
columnDefinition_DefaultOrIdentityOrGeneration_OptionGenerationClause x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "GenerationClause"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

columnNameList :: Phantoms.TTerm Syntax.ColumnName -> Phantoms.TTerm [Syntax.ColumnName] -> Phantoms.TTerm Syntax.ColumnNameList
columnNameList first rest =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnNameList"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm rest)}]}))

columnNameListFirst :: Phantoms.TTerm Syntax.ColumnNameList -> Phantoms.TTerm Syntax.ColumnName
columnNameListFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnNameList"),
        Core.projectionField = (Core.Name "first")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnNameListRest :: Phantoms.TTerm Syntax.ColumnNameList -> Phantoms.TTerm [Syntax.ColumnName]
columnNameListRest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnNameList"),
        Core.projectionField = (Core.Name "rest")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnNameListWithFirst :: Phantoms.TTerm Syntax.ColumnNameList -> Phantoms.TTerm Syntax.ColumnName -> Phantoms.TTerm Syntax.ColumnNameList
columnNameListWithFirst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnNameList"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnNameList"),
              Core.projectionField = (Core.Name "rest")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnNameListWithRest :: Phantoms.TTerm Syntax.ColumnNameList -> Phantoms.TTerm [Syntax.ColumnName] -> Phantoms.TTerm Syntax.ColumnNameList
columnNameListWithRest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnNameList"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnNameList"),
              Core.projectionField = (Core.Name "first")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

columnOptions :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ColumnOptions
columnOptions x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnOptions"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unColumnOptions :: Phantoms.TTerm Syntax.ColumnOptions -> Phantoms.TTerm ()
unColumnOptions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnOptions")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnReference :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ColumnReference
columnReference x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnReference"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unColumnReference :: Phantoms.TTerm Syntax.ColumnReference -> Phantoms.TTerm ()
unColumnReference x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnReference")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

commonValueExpressionNumeric :: Phantoms.TTerm Syntax.NumericValueExpression -> Phantoms.TTerm Syntax.CommonValueExpression
commonValueExpressionNumeric x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CommonValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numeric"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonValueExpressionString :: Phantoms.TTerm Syntax.StringValueExpression -> Phantoms.TTerm Syntax.CommonValueExpression
commonValueExpressionString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CommonValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonValueExpressionDatetime :: Phantoms.TTerm Syntax.DatetimeValueExpression -> Phantoms.TTerm Syntax.CommonValueExpression
commonValueExpressionDatetime x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CommonValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datetime"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonValueExpressionInterval :: Phantoms.TTerm Syntax.IntervalValueExpression -> Phantoms.TTerm Syntax.CommonValueExpression
commonValueExpressionInterval x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CommonValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interval"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonValueExpressionUserDefined :: Phantoms.TTerm Syntax.UserDefinedTypeValueExpression -> Phantoms.TTerm Syntax.CommonValueExpression
commonValueExpressionUserDefined x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CommonValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "userDefined"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonValueExpressionReference :: Phantoms.TTerm Syntax.ReferenceValueExpression -> Phantoms.TTerm Syntax.CommonValueExpression
commonValueExpressionReference x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CommonValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commonValueExpressionCollection :: Phantoms.TTerm Syntax.CollectionValueExpression -> Phantoms.TTerm Syntax.CommonValueExpression
commonValueExpressionCollection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.CommonValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "collection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

contextuallyTypedRowValueExpressionSpecialCase :: Phantoms.TTerm Syntax.RowValueSpecialCase -> Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpression
contextuallyTypedRowValueExpressionSpecialCase x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "specialCase"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

contextuallyTypedRowValueExpressionConstructor :: Phantoms.TTerm Syntax.ContextuallyTypedRowValueConstructor -> Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpression
contextuallyTypedRowValueExpressionConstructor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

contextuallyTypedRowValueConstructor :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ContextuallyTypedRowValueConstructor
contextuallyTypedRowValueConstructor x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueConstructor"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unContextuallyTypedRowValueConstructor :: Phantoms.TTerm Syntax.ContextuallyTypedRowValueConstructor -> Phantoms.TTerm ()
unContextuallyTypedRowValueConstructor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueConstructor")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextuallyTypedRowValueExpressionList :: Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpression -> Phantoms.TTerm [Syntax.ContextuallyTypedRowValueExpression] -> Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpressionList
contextuallyTypedRowValueExpressionList first rest =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpressionList"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm rest)}]}))

contextuallyTypedRowValueExpressionListFirst :: Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpressionList -> Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpression
contextuallyTypedRowValueExpressionListFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpressionList"),
        Core.projectionField = (Core.Name "first")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextuallyTypedRowValueExpressionListRest :: Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpressionList -> Phantoms.TTerm [Syntax.ContextuallyTypedRowValueExpression]
contextuallyTypedRowValueExpressionListRest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpressionList"),
        Core.projectionField = (Core.Name "rest")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextuallyTypedRowValueExpressionListWithFirst :: Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpressionList -> Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpression -> Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpressionList
contextuallyTypedRowValueExpressionListWithFirst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpressionList"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpressionList"),
              Core.projectionField = (Core.Name "rest")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

contextuallyTypedRowValueExpressionListWithRest :: Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpressionList -> Phantoms.TTerm [Syntax.ContextuallyTypedRowValueExpression] -> Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpressionList
contextuallyTypedRowValueExpressionListWithRest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpressionList"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpressionList"),
              Core.projectionField = (Core.Name "first")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

contextuallyTypedTableValueConstructor :: Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpressionList -> Phantoms.TTerm Syntax.ContextuallyTypedTableValueConstructor
contextuallyTypedTableValueConstructor x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedTableValueConstructor"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unContextuallyTypedTableValueConstructor :: Phantoms.TTerm Syntax.ContextuallyTypedTableValueConstructor -> Phantoms.TTerm Syntax.ContextuallyTypedRowValueExpressionList
unContextuallyTypedTableValueConstructor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedTableValueConstructor")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataTypePredefined :: Phantoms.TTerm Syntax.PredefinedType -> Phantoms.TTerm Syntax.DataType
dataTypePredefined x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DataType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "predefined"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataTypeRow :: Phantoms.TTerm Syntax.RowType -> Phantoms.TTerm Syntax.DataType
dataTypeRow x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DataType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "row"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataTypeNamed :: Phantoms.TTerm Syntax.PathResolvedUserDefinedTypeName -> Phantoms.TTerm Syntax.DataType
dataTypeNamed x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DataType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataTypeReference :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm Syntax.DataType
dataTypeReference x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DataType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dataTypeCollection :: Phantoms.TTerm Syntax.CollectionType -> Phantoms.TTerm Syntax.DataType
dataTypeCollection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DataType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "collection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

dateLiteral :: Phantoms.TTerm Syntax.DateString -> Phantoms.TTerm Syntax.DateLiteral
dateLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DateLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDateLiteral :: Phantoms.TTerm Syntax.DateLiteral -> Phantoms.TTerm Syntax.DateString
unDateLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.DateLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datetimeLiteralDate :: Phantoms.TTerm Syntax.DateLiteral -> Phantoms.TTerm Syntax.DatetimeLiteral
datetimeLiteralDate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DatetimeLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "date"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

datetimeLiteralTime :: Phantoms.TTerm Syntax.TimeLiteral -> Phantoms.TTerm Syntax.DatetimeLiteral
datetimeLiteralTime x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DatetimeLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "time"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

datetimeLiteralTimestamp :: Phantoms.TTerm Syntax.TimestampLiteral -> Phantoms.TTerm Syntax.DatetimeLiteral
datetimeLiteralTimestamp x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DatetimeLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "timestamp"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

datetimeType :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.DatetimeType
datetimeType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DatetimeType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDatetimeType :: Phantoms.TTerm Syntax.DatetimeType -> Phantoms.TTerm ()
unDatetimeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.DatetimeType")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

datetimeValueExpression :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.DatetimeValueExpression
datetimeValueExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DatetimeValueExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDatetimeValueExpression :: Phantoms.TTerm Syntax.DatetimeValueExpression -> Phantoms.TTerm ()
unDatetimeValueExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.DatetimeValueExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

defaultClause :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.DefaultClause
defaultClause x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DefaultClause"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDefaultClause :: Phantoms.TTerm Syntax.DefaultClause -> Phantoms.TTerm ()
unDefaultClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.DefaultClause")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exactNumericTypeNumeric :: Phantoms.TTerm (Maybe Syntax.ExactNumericType_Numeric_Option) -> Phantoms.TTerm Syntax.ExactNumericType
exactNumericTypeNumeric x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numeric"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exactNumericTypeDecimal :: Phantoms.TTerm (Maybe Syntax.ExactNumericType_Decimal_Option) -> Phantoms.TTerm Syntax.ExactNumericType
exactNumericTypeDecimal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decimal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exactNumericTypeDec :: Phantoms.TTerm (Maybe Syntax.ExactNumericType_Dec_Option) -> Phantoms.TTerm Syntax.ExactNumericType
exactNumericTypeDec x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dec"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exactNumericTypeSmallint :: Phantoms.TTerm Syntax.ExactNumericType
exactNumericTypeSmallint =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "smallint"),
        Core.fieldTerm = Core.TermUnit}}))

exactNumericTypeInteger :: Phantoms.TTerm Syntax.ExactNumericType
exactNumericTypeInteger =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = Core.TermUnit}}))

exactNumericTypeInt :: Phantoms.TTerm Syntax.ExactNumericType
exactNumericTypeInt =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = Core.TermUnit}}))

exactNumericTypeBigint :: Phantoms.TTerm Syntax.ExactNumericType
exactNumericTypeBigint =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigint"),
        Core.fieldTerm = Core.TermUnit}}))

exactNumericType_Numeric_Option :: Phantoms.TTerm Syntax.Precision -> Phantoms.TTerm (Maybe Syntax.Scale) -> Phantoms.TTerm Syntax.ExactNumericType_Numeric_Option
exactNumericType_Numeric_Option precision sequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Numeric_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Precision"),
          Core.fieldTerm = (Phantoms.unTTerm precision)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm sequence)}]}))

exactNumericType_Numeric_OptionPrecision :: Phantoms.TTerm Syntax.ExactNumericType_Numeric_Option -> Phantoms.TTerm Syntax.Precision
exactNumericType_Numeric_OptionPrecision x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Numeric_Option"),
        Core.projectionField = (Core.Name "Precision")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exactNumericType_Numeric_OptionSequence :: Phantoms.TTerm Syntax.ExactNumericType_Numeric_Option -> Phantoms.TTerm (Maybe Syntax.Scale)
exactNumericType_Numeric_OptionSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Numeric_Option"),
        Core.projectionField = (Core.Name "Sequence")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exactNumericType_Numeric_OptionWithPrecision :: Phantoms.TTerm Syntax.ExactNumericType_Numeric_Option -> Phantoms.TTerm Syntax.Precision -> Phantoms.TTerm Syntax.ExactNumericType_Numeric_Option
exactNumericType_Numeric_OptionWithPrecision original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Numeric_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Precision"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Numeric_Option"),
              Core.projectionField = (Core.Name "Sequence")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

exactNumericType_Numeric_OptionWithSequence :: Phantoms.TTerm Syntax.ExactNumericType_Numeric_Option -> Phantoms.TTerm (Maybe Syntax.Scale) -> Phantoms.TTerm Syntax.ExactNumericType_Numeric_Option
exactNumericType_Numeric_OptionWithSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Numeric_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Precision"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Numeric_Option"),
              Core.projectionField = (Core.Name "Precision")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

exactNumericType_Decimal_Option :: Phantoms.TTerm Syntax.Precision -> Phantoms.TTerm (Maybe Syntax.Scale) -> Phantoms.TTerm Syntax.ExactNumericType_Decimal_Option
exactNumericType_Decimal_Option precision sequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Decimal_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Precision"),
          Core.fieldTerm = (Phantoms.unTTerm precision)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm sequence)}]}))

exactNumericType_Decimal_OptionPrecision :: Phantoms.TTerm Syntax.ExactNumericType_Decimal_Option -> Phantoms.TTerm Syntax.Precision
exactNumericType_Decimal_OptionPrecision x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Decimal_Option"),
        Core.projectionField = (Core.Name "Precision")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exactNumericType_Decimal_OptionSequence :: Phantoms.TTerm Syntax.ExactNumericType_Decimal_Option -> Phantoms.TTerm (Maybe Syntax.Scale)
exactNumericType_Decimal_OptionSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Decimal_Option"),
        Core.projectionField = (Core.Name "Sequence")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exactNumericType_Decimal_OptionWithPrecision :: Phantoms.TTerm Syntax.ExactNumericType_Decimal_Option -> Phantoms.TTerm Syntax.Precision -> Phantoms.TTerm Syntax.ExactNumericType_Decimal_Option
exactNumericType_Decimal_OptionWithPrecision original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Decimal_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Precision"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Decimal_Option"),
              Core.projectionField = (Core.Name "Sequence")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

exactNumericType_Decimal_OptionWithSequence :: Phantoms.TTerm Syntax.ExactNumericType_Decimal_Option -> Phantoms.TTerm (Maybe Syntax.Scale) -> Phantoms.TTerm Syntax.ExactNumericType_Decimal_Option
exactNumericType_Decimal_OptionWithSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Decimal_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Precision"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Decimal_Option"),
              Core.projectionField = (Core.Name "Precision")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

exactNumericType_Dec_Option :: Phantoms.TTerm Syntax.Precision -> Phantoms.TTerm (Maybe Syntax.Scale) -> Phantoms.TTerm Syntax.ExactNumericType_Dec_Option
exactNumericType_Dec_Option precision sequence =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Dec_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Precision"),
          Core.fieldTerm = (Phantoms.unTTerm precision)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm sequence)}]}))

exactNumericType_Dec_OptionPrecision :: Phantoms.TTerm Syntax.ExactNumericType_Dec_Option -> Phantoms.TTerm Syntax.Precision
exactNumericType_Dec_OptionPrecision x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Dec_Option"),
        Core.projectionField = (Core.Name "Precision")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exactNumericType_Dec_OptionSequence :: Phantoms.TTerm Syntax.ExactNumericType_Dec_Option -> Phantoms.TTerm (Maybe Syntax.Scale)
exactNumericType_Dec_OptionSequence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Dec_Option"),
        Core.projectionField = (Core.Name "Sequence")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exactNumericType_Dec_OptionWithPrecision :: Phantoms.TTerm Syntax.ExactNumericType_Dec_Option -> Phantoms.TTerm Syntax.Precision -> Phantoms.TTerm Syntax.ExactNumericType_Dec_Option
exactNumericType_Dec_OptionWithPrecision original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Dec_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Precision"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Dec_Option"),
              Core.projectionField = (Core.Name "Sequence")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

exactNumericType_Dec_OptionWithSequence :: Phantoms.TTerm Syntax.ExactNumericType_Dec_Option -> Phantoms.TTerm (Maybe Syntax.Scale) -> Phantoms.TTerm Syntax.ExactNumericType_Dec_Option
exactNumericType_Dec_OptionWithSequence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Dec_Option"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "Precision"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Dec_Option"),
              Core.projectionField = (Core.Name "Precision")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "Sequence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldReference :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.FieldReference
fieldReference x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FieldReference"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unFieldReference :: Phantoms.TTerm Syntax.FieldReference -> Phantoms.TTerm ()
unFieldReference x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.FieldReference")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fromConstructor :: Phantoms.TTerm (Maybe Syntax.InsertColumnList) -> Phantoms.TTerm (Maybe Syntax.OverrideClause) -> Phantoms.TTerm Syntax.ContextuallyTypedTableValueConstructor -> Phantoms.TTerm Syntax.FromConstructor
fromConstructor columns override values =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Phantoms.unTTerm columns)},
        Core.Field {
          Core.fieldName = (Core.Name "override"),
          Core.fieldTerm = (Phantoms.unTTerm override)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm values)}]}))

fromConstructorColumns :: Phantoms.TTerm Syntax.FromConstructor -> Phantoms.TTerm (Maybe Syntax.InsertColumnList)
fromConstructorColumns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor"),
        Core.projectionField = (Core.Name "columns")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fromConstructorOverride :: Phantoms.TTerm Syntax.FromConstructor -> Phantoms.TTerm (Maybe Syntax.OverrideClause)
fromConstructorOverride x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor"),
        Core.projectionField = (Core.Name "override")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fromConstructorValues :: Phantoms.TTerm Syntax.FromConstructor -> Phantoms.TTerm Syntax.ContextuallyTypedTableValueConstructor
fromConstructorValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor"),
        Core.projectionField = (Core.Name "values")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fromConstructorWithColumns :: Phantoms.TTerm Syntax.FromConstructor -> Phantoms.TTerm (Maybe Syntax.InsertColumnList) -> Phantoms.TTerm Syntax.FromConstructor
fromConstructorWithColumns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "override"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor"),
              Core.projectionField = (Core.Name "override")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor"),
              Core.projectionField = (Core.Name "values")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fromConstructorWithOverride :: Phantoms.TTerm Syntax.FromConstructor -> Phantoms.TTerm (Maybe Syntax.OverrideClause) -> Phantoms.TTerm Syntax.FromConstructor
fromConstructorWithOverride original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor"),
              Core.projectionField = (Core.Name "columns")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "override"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor"),
              Core.projectionField = (Core.Name "values")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fromConstructorWithValues :: Phantoms.TTerm Syntax.FromConstructor -> Phantoms.TTerm Syntax.ContextuallyTypedTableValueConstructor -> Phantoms.TTerm Syntax.FromConstructor
fromConstructorWithValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor"),
              Core.projectionField = (Core.Name "columns")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "override"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor"),
              Core.projectionField = (Core.Name "override")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fromDefault :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.FromDefault
fromDefault x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromDefault"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unFromDefault :: Phantoms.TTerm Syntax.FromDefault -> Phantoms.TTerm ()
unFromDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.FromDefault")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fromSubquery :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.FromSubquery
fromSubquery x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromSubquery"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unFromSubquery :: Phantoms.TTerm Syntax.FromSubquery -> Phantoms.TTerm ()
unFromSubquery x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.FromSubquery")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

generalLiteralString :: Phantoms.TTerm Syntax.CharacterStringLiteral -> Phantoms.TTerm Syntax.GeneralLiteral
generalLiteralString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.GeneralLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

generalLiteralNationalString :: Phantoms.TTerm Syntax.NationalCharacterStringLiteral -> Phantoms.TTerm Syntax.GeneralLiteral
generalLiteralNationalString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.GeneralLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nationalString"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

generalLiteralUnicode :: Phantoms.TTerm Syntax.UnicodeCharacterStringLiteral -> Phantoms.TTerm Syntax.GeneralLiteral
generalLiteralUnicode x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.GeneralLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unicode"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

generalLiteralBinary :: Phantoms.TTerm Syntax.BinaryStringLiteral -> Phantoms.TTerm Syntax.GeneralLiteral
generalLiteralBinary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.GeneralLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

generalLiteralDateTime :: Phantoms.TTerm Syntax.DatetimeLiteral -> Phantoms.TTerm Syntax.GeneralLiteral
generalLiteralDateTime x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.GeneralLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dateTime"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

generalLiteralInterval :: Phantoms.TTerm Syntax.IntervalLiteral -> Phantoms.TTerm Syntax.GeneralLiteral
generalLiteralInterval x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.GeneralLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interval"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

generalLiteralBoolean :: Phantoms.TTerm Syntax.BooleanLiteral -> Phantoms.TTerm Syntax.GeneralLiteral
generalLiteralBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.GeneralLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

generalValueSpecification :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.GeneralValueSpecification
generalValueSpecification x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.GeneralValueSpecification"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unGeneralValueSpecification :: Phantoms.TTerm Syntax.GeneralValueSpecification -> Phantoms.TTerm ()
unGeneralValueSpecification x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.GeneralValueSpecification")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

generationClause :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.GenerationClause
generationClause x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.GenerationClause"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unGenerationClause :: Phantoms.TTerm Syntax.GenerationClause -> Phantoms.TTerm ()
unGenerationClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.GenerationClause")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

globalOrLocalGlobal :: Phantoms.TTerm Syntax.GlobalOrLocal
globalOrLocalGlobal =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.GlobalOrLocal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "global"),
        Core.fieldTerm = Core.TermUnit}}))

globalOrLocalLocal :: Phantoms.TTerm Syntax.GlobalOrLocal
globalOrLocalLocal =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.GlobalOrLocal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "local"),
        Core.fieldTerm = Core.TermUnit}}))

identityColumnSpecification :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.IdentityColumnSpecification
identityColumnSpecification x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.IdentityColumnSpecification"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unIdentityColumnSpecification :: Phantoms.TTerm Syntax.IdentityColumnSpecification -> Phantoms.TTerm ()
unIdentityColumnSpecification x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.IdentityColumnSpecification")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

insertColumnList :: Phantoms.TTerm Syntax.ColumnNameList -> Phantoms.TTerm Syntax.InsertColumnList
insertColumnList x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertColumnList"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unInsertColumnList :: Phantoms.TTerm Syntax.InsertColumnList -> Phantoms.TTerm Syntax.ColumnNameList
unInsertColumnList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertColumnList")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

insertColumnsAndSourceSubquery :: Phantoms.TTerm Syntax.FromSubquery -> Phantoms.TTerm Syntax.InsertColumnsAndSource
insertColumnsAndSourceSubquery x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertColumnsAndSource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subquery"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

insertColumnsAndSourceConstructor :: Phantoms.TTerm Syntax.FromConstructor -> Phantoms.TTerm Syntax.InsertColumnsAndSource
insertColumnsAndSourceConstructor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertColumnsAndSource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

insertColumnsAndSourceDefault :: Phantoms.TTerm Syntax.FromDefault -> Phantoms.TTerm Syntax.InsertColumnsAndSource
insertColumnsAndSourceDefault x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertColumnsAndSource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

insertStatement :: Phantoms.TTerm Syntax.InsertionTarget -> Phantoms.TTerm Syntax.InsertColumnsAndSource -> Phantoms.TTerm Syntax.InsertStatement
insertStatement target columnsAndSource =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)},
        Core.Field {
          Core.fieldName = (Core.Name "columnsAndSource"),
          Core.fieldTerm = (Phantoms.unTTerm columnsAndSource)}]}))

insertStatementTarget :: Phantoms.TTerm Syntax.InsertStatement -> Phantoms.TTerm Syntax.InsertionTarget
insertStatementTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertStatement"),
        Core.projectionField = (Core.Name "target")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

insertStatementColumnsAndSource :: Phantoms.TTerm Syntax.InsertStatement -> Phantoms.TTerm Syntax.InsertColumnsAndSource
insertStatementColumnsAndSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertStatement"),
        Core.projectionField = (Core.Name "columnsAndSource")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

insertStatementWithTarget :: Phantoms.TTerm Syntax.InsertStatement -> Phantoms.TTerm Syntax.InsertionTarget -> Phantoms.TTerm Syntax.InsertStatement
insertStatementWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "columnsAndSource"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertStatement"),
              Core.projectionField = (Core.Name "columnsAndSource")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

insertStatementWithColumnsAndSource :: Phantoms.TTerm Syntax.InsertStatement -> Phantoms.TTerm Syntax.InsertColumnsAndSource -> Phantoms.TTerm Syntax.InsertStatement
insertStatementWithColumnsAndSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertStatement"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnsAndSource"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

insertionTarget :: Phantoms.TTerm Syntax.TableName -> Phantoms.TTerm Syntax.InsertionTarget
insertionTarget x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertionTarget"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unInsertionTarget :: Phantoms.TTerm Syntax.InsertionTarget -> Phantoms.TTerm Syntax.TableName
unInsertionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertionTarget")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

intervalLiteral :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.IntervalLiteral
intervalLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.IntervalLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unIntervalLiteral :: Phantoms.TTerm Syntax.IntervalLiteral -> Phantoms.TTerm ()
unIntervalLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.IntervalLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

intervalType :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.IntervalType
intervalType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.IntervalType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unIntervalType :: Phantoms.TTerm Syntax.IntervalType -> Phantoms.TTerm ()
unIntervalType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.IntervalType")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

intervalValueExpression :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.IntervalValueExpression
intervalValueExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.IntervalValueExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unIntervalValueExpression :: Phantoms.TTerm Syntax.IntervalValueExpression -> Phantoms.TTerm ()
unIntervalValueExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.IntervalValueExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

largeObjectLength :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.LargeObjectLength
largeObjectLength x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.LargeObjectLength"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unLargeObjectLength :: Phantoms.TTerm Syntax.LargeObjectLength -> Phantoms.TTerm ()
unLargeObjectLength x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.LargeObjectLength")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

length :: Phantoms.TTerm Syntax.UnsignedInteger -> Phantoms.TTerm Syntax.Length
length x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.Length"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unLength :: Phantoms.TTerm Syntax.Length -> Phantoms.TTerm Syntax.UnsignedInteger
unLength x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.Length")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

likeClause :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.LikeClause
likeClause x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.LikeClause"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unLikeClause :: Phantoms.TTerm Syntax.LikeClause -> Phantoms.TTerm ()
unLikeClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.LikeClause")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodInvocation :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.MethodInvocation
methodInvocation x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.MethodInvocation"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unMethodInvocation :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm ()
unMethodInvocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.MethodInvocation")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multisetElementReference :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.MultisetElementReference
multisetElementReference x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetElementReference"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unMultisetElementReference :: Phantoms.TTerm Syntax.MultisetElementReference -> Phantoms.TTerm ()
unMultisetElementReference x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetElementReference")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multisetType :: Phantoms.TTerm Syntax.DataType -> Phantoms.TTerm Syntax.MultisetType
multisetType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unMultisetType :: Phantoms.TTerm Syntax.MultisetType -> Phantoms.TTerm Syntax.DataType
unMultisetType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetType")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multisetValueConstructor :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.MultisetValueConstructor
multisetValueConstructor x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetValueConstructor"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unMultisetValueConstructor :: Phantoms.TTerm Syntax.MultisetValueConstructor -> Phantoms.TTerm ()
unMultisetValueConstructor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetValueConstructor")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multisetValueExpression :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.MultisetValueExpression
multisetValueExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetValueExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unMultisetValueExpression :: Phantoms.TTerm Syntax.MultisetValueExpression -> Phantoms.TTerm ()
unMultisetValueExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetValueExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nationalCharacterStringType :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.NationalCharacterStringType
nationalCharacterStringType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NationalCharacterStringType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unNationalCharacterStringType :: Phantoms.TTerm Syntax.NationalCharacterStringType -> Phantoms.TTerm ()
unNationalCharacterStringType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.NationalCharacterStringType")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

newSpecification :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.NewSpecification
newSpecification x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NewSpecification"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unNewSpecification :: Phantoms.TTerm Syntax.NewSpecification -> Phantoms.TTerm ()
unNewSpecification x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.NewSpecification")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nextValueExpression :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.NextValueExpression
nextValueExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NextValueExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unNextValueExpression :: Phantoms.TTerm Syntax.NextValueExpression -> Phantoms.TTerm ()
unNextValueExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.NextValueExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

numericTypeExact :: Phantoms.TTerm Syntax.ExactNumericType -> Phantoms.TTerm Syntax.NumericType
numericTypeExact x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exact"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericTypeApproximate :: Phantoms.TTerm Syntax.ApproximateNumericType -> Phantoms.TTerm Syntax.NumericType
numericTypeApproximate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NumericType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "approximate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericValueExpression :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.NumericValueExpression
numericValueExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NumericValueExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unNumericValueExpression :: Phantoms.TTerm Syntax.NumericValueExpression -> Phantoms.TTerm ()
unNumericValueExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.NumericValueExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

overrideClauseOVERRIDINGspUSERspVALUE :: Phantoms.TTerm Syntax.OverrideClause
overrideClauseOVERRIDINGspUSERspVALUE =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.OverrideClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "OVERRIDINGspUSERspVALUE"),
        Core.fieldTerm = Core.TermUnit}}))

overrideClauseOVERRIDINGspSYSTEMspVALUE :: Phantoms.TTerm Syntax.OverrideClause
overrideClauseOVERRIDINGspSYSTEMspVALUE =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.OverrideClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "OVERRIDINGspSYSTEMspVALUE"),
        Core.fieldTerm = Core.TermUnit}}))

parenthesizedValueExpression :: Phantoms.TTerm Syntax.ValueExpression -> Phantoms.TTerm Syntax.ParenthesizedValueExpression
parenthesizedValueExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ParenthesizedValueExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unParenthesizedValueExpression :: Phantoms.TTerm Syntax.ParenthesizedValueExpression -> Phantoms.TTerm Syntax.ValueExpression
unParenthesizedValueExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ParenthesizedValueExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

precision :: Phantoms.TTerm Syntax.UnsignedInteger -> Phantoms.TTerm Syntax.Precision
precision x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.Precision"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPrecision :: Phantoms.TTerm Syntax.Precision -> Phantoms.TTerm Syntax.UnsignedInteger
unPrecision x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.Precision")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

predefinedTypeString :: Phantoms.TTerm Syntax.PredefinedType_String -> Phantoms.TTerm Syntax.PredefinedType
predefinedTypeString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

predefinedTypeNationalString :: Phantoms.TTerm Syntax.PredefinedType_NationalString -> Phantoms.TTerm Syntax.PredefinedType
predefinedTypeNationalString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nationalString"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

predefinedTypeBlob :: Phantoms.TTerm Syntax.BinaryLargeObjectStringType -> Phantoms.TTerm Syntax.PredefinedType
predefinedTypeBlob x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "blob"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

predefinedTypeNumeric :: Phantoms.TTerm Syntax.NumericType -> Phantoms.TTerm Syntax.PredefinedType
predefinedTypeNumeric x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numeric"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

predefinedTypeBoolean :: Phantoms.TTerm Syntax.BooleanType -> Phantoms.TTerm Syntax.PredefinedType
predefinedTypeBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

predefinedTypeDatetime :: Phantoms.TTerm Syntax.DatetimeType -> Phantoms.TTerm Syntax.PredefinedType
predefinedTypeDatetime x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datetime"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

predefinedTypeInterval :: Phantoms.TTerm Syntax.IntervalType -> Phantoms.TTerm Syntax.PredefinedType
predefinedTypeInterval x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interval"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

predefinedType_String :: Phantoms.TTerm Syntax.CharacterStringType -> Phantoms.TTerm (Maybe Syntax.CharacterSetSpecification) -> Phantoms.TTerm (Maybe Syntax.CollateClause) -> Phantoms.TTerm Syntax.PredefinedType_String
predefinedType_String type_ characters collate =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "characters"),
          Core.fieldTerm = (Phantoms.unTTerm characters)},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Phantoms.unTTerm collate)}]}))

predefinedType_StringType :: Phantoms.TTerm Syntax.PredefinedType_String -> Phantoms.TTerm Syntax.CharacterStringType
predefinedType_StringType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

predefinedType_StringCharacters :: Phantoms.TTerm Syntax.PredefinedType_String -> Phantoms.TTerm (Maybe Syntax.CharacterSetSpecification)
predefinedType_StringCharacters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String"),
        Core.projectionField = (Core.Name "characters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

predefinedType_StringCollate :: Phantoms.TTerm Syntax.PredefinedType_String -> Phantoms.TTerm (Maybe Syntax.CollateClause)
predefinedType_StringCollate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String"),
        Core.projectionField = (Core.Name "collate")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

predefinedType_StringWithType :: Phantoms.TTerm Syntax.PredefinedType_String -> Phantoms.TTerm Syntax.CharacterStringType -> Phantoms.TTerm Syntax.PredefinedType_String
predefinedType_StringWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "characters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String"),
              Core.projectionField = (Core.Name "characters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String"),
              Core.projectionField = (Core.Name "collate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

predefinedType_StringWithCharacters :: Phantoms.TTerm Syntax.PredefinedType_String -> Phantoms.TTerm (Maybe Syntax.CharacterSetSpecification) -> Phantoms.TTerm Syntax.PredefinedType_String
predefinedType_StringWithCharacters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "characters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String"),
              Core.projectionField = (Core.Name "collate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

predefinedType_StringWithCollate :: Phantoms.TTerm Syntax.PredefinedType_String -> Phantoms.TTerm (Maybe Syntax.CollateClause) -> Phantoms.TTerm Syntax.PredefinedType_String
predefinedType_StringWithCollate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "characters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String"),
              Core.projectionField = (Core.Name "characters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

predefinedType_NationalString :: Phantoms.TTerm Syntax.NationalCharacterStringType -> Phantoms.TTerm (Maybe Syntax.CollateClause) -> Phantoms.TTerm Syntax.PredefinedType_NationalString
predefinedType_NationalString type_ collate =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_NationalString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Phantoms.unTTerm collate)}]}))

predefinedType_NationalStringType :: Phantoms.TTerm Syntax.PredefinedType_NationalString -> Phantoms.TTerm Syntax.NationalCharacterStringType
predefinedType_NationalStringType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_NationalString"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

predefinedType_NationalStringCollate :: Phantoms.TTerm Syntax.PredefinedType_NationalString -> Phantoms.TTerm (Maybe Syntax.CollateClause)
predefinedType_NationalStringCollate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_NationalString"),
        Core.projectionField = (Core.Name "collate")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

predefinedType_NationalStringWithType :: Phantoms.TTerm Syntax.PredefinedType_NationalString -> Phantoms.TTerm Syntax.NationalCharacterStringType -> Phantoms.TTerm Syntax.PredefinedType_NationalString
predefinedType_NationalStringWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_NationalString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_NationalString"),
              Core.projectionField = (Core.Name "collate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

predefinedType_NationalStringWithCollate :: Phantoms.TTerm Syntax.PredefinedType_NationalString -> Phantoms.TTerm (Maybe Syntax.CollateClause) -> Phantoms.TTerm Syntax.PredefinedType_NationalString
predefinedType_NationalStringWithCollate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_NationalString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_NationalString"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "collate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

predicate :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.Predicate
predicate x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.Predicate"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPredicate :: Phantoms.TTerm Syntax.Predicate -> Phantoms.TTerm ()
unPredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.Predicate")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

queryExpression :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.QueryExpression
queryExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.QueryExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unQueryExpression :: Phantoms.TTerm Syntax.QueryExpression -> Phantoms.TTerm ()
unQueryExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.QueryExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

referenceScopeCheck :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ReferenceScopeCheck
referenceScopeCheck x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceScopeCheck"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unReferenceScopeCheck :: Phantoms.TTerm Syntax.ReferenceScopeCheck -> Phantoms.TTerm ()
unReferenceScopeCheck x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceScopeCheck")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

referenceType :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ReferenceType
referenceType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unReferenceType :: Phantoms.TTerm Syntax.ReferenceType -> Phantoms.TTerm ()
unReferenceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceType")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rowType :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.RowType
rowType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.RowType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unRowType :: Phantoms.TTerm Syntax.RowType -> Phantoms.TTerm ()
unRowType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.RowType")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rowValueSpecialCase :: Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary -> Phantoms.TTerm Syntax.RowValueSpecialCase
rowValueSpecialCase x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.RowValueSpecialCase"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unRowValueSpecialCase :: Phantoms.TTerm Syntax.RowValueSpecialCase -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
unRowValueSpecialCase x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.RowValueSpecialCase")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nonparenthesizedValueExpressionPrimaryUnsigned :: Phantoms.TTerm Syntax.UnsignedValueSpecification -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryUnsigned x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unsigned"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryColumn :: Phantoms.TTerm Syntax.ColumnReference -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryColumn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "column"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimarySetFunction :: Phantoms.TTerm Syntax.SetFunctionSpecification -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimarySetFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "setFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryWindowFunction :: Phantoms.TTerm Syntax.WindowFunction -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryWindowFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "windowFunction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryScalarSubquery :: Phantoms.TTerm Syntax.ScalarSubquery -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryScalarSubquery x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scalarSubquery"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryCases :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryCases x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cases"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryCast :: Phantoms.TTerm Syntax.CastSpecification -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryCast x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cast"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryField :: Phantoms.TTerm Syntax.FieldReference -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryField x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "field"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimarySubtype :: Phantoms.TTerm Syntax.SubtypeTreatment -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimarySubtype x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subtype"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryMethod :: Phantoms.TTerm Syntax.MethodInvocation -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryMethod x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryStaticMethod :: Phantoms.TTerm Syntax.StaticMethodInvocation -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryStaticMethod x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "staticMethod"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryNew :: Phantoms.TTerm Syntax.NewSpecification -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryNew x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "new"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryAttributeOrMethod :: Phantoms.TTerm Syntax.AttributeOrMethodReference -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryAttributeOrMethod x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "attributeOrMethod"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryReference :: Phantoms.TTerm Syntax.ReferenceResolution -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryReference x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryCollection :: Phantoms.TTerm Syntax.CollectionValueConstructor -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryCollection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "collection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryArrayElement :: Phantoms.TTerm Syntax.ArrayElementReference -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryArrayElement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrayElement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryMultisetElement :: Phantoms.TTerm Syntax.MultisetElementReference -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryMultisetElement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multisetElement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryRoutine :: Phantoms.TTerm Syntax.RoutineInvocation -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryRoutine x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "routine"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nonparenthesizedValueExpressionPrimaryNext :: Phantoms.TTerm Syntax.NextValueExpression -> Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary
nonparenthesizedValueExpressionPrimaryNext x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "next"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

referenceResolution :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.ReferenceResolution
referenceResolution x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceResolution"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unReferenceResolution :: Phantoms.TTerm Syntax.ReferenceResolution -> Phantoms.TTerm ()
unReferenceResolution x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceResolution")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

referenceValueExpression :: Phantoms.TTerm Syntax.ValueExpressionPrimary -> Phantoms.TTerm Syntax.ReferenceValueExpression
referenceValueExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceValueExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unReferenceValueExpression :: Phantoms.TTerm Syntax.ReferenceValueExpression -> Phantoms.TTerm Syntax.ValueExpressionPrimary
unReferenceValueExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceValueExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rowValueExpression :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.RowValueExpression
rowValueExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.RowValueExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unRowValueExpression :: Phantoms.TTerm Syntax.RowValueExpression -> Phantoms.TTerm ()
unRowValueExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.RowValueExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

routineInvocation :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.RoutineInvocation
routineInvocation x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.RoutineInvocation"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unRoutineInvocation :: Phantoms.TTerm Syntax.RoutineInvocation -> Phantoms.TTerm ()
unRoutineInvocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.RoutineInvocation")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

scalarSubquery :: Phantoms.TTerm Syntax.Subquery -> Phantoms.TTerm Syntax.ScalarSubquery
scalarSubquery x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ScalarSubquery"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unScalarSubquery :: Phantoms.TTerm Syntax.ScalarSubquery -> Phantoms.TTerm Syntax.Subquery
unScalarSubquery x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.ScalarSubquery")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

scale :: Phantoms.TTerm Syntax.UnsignedInteger -> Phantoms.TTerm Syntax.Scale
scale x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.Scale"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unScale :: Phantoms.TTerm Syntax.Scale -> Phantoms.TTerm Syntax.UnsignedInteger
unScale x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.Scale")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selfReferencingColumnSpecification :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.SelfReferencingColumnSpecification
selfReferencingColumnSpecification x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.SelfReferencingColumnSpecification"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unSelfReferencingColumnSpecification :: Phantoms.TTerm Syntax.SelfReferencingColumnSpecification -> Phantoms.TTerm ()
unSelfReferencingColumnSpecification x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.SelfReferencingColumnSpecification")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

setFunctionSpecification :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.SetFunctionSpecification
setFunctionSpecification x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.SetFunctionSpecification"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unSetFunctionSpecification :: Phantoms.TTerm Syntax.SetFunctionSpecification -> Phantoms.TTerm ()
unSetFunctionSpecification x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.SetFunctionSpecification")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

staticMethodInvocation :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.StaticMethodInvocation
staticMethodInvocation x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.StaticMethodInvocation"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unStaticMethodInvocation :: Phantoms.TTerm Syntax.StaticMethodInvocation -> Phantoms.TTerm ()
unStaticMethodInvocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.StaticMethodInvocation")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringValueExpression :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.StringValueExpression
stringValueExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.StringValueExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unStringValueExpression :: Phantoms.TTerm Syntax.StringValueExpression -> Phantoms.TTerm ()
unStringValueExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.StringValueExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subquery :: Phantoms.TTerm Syntax.QueryExpression -> Phantoms.TTerm Syntax.Subquery
subquery x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.Subquery"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unSubquery :: Phantoms.TTerm Syntax.Subquery -> Phantoms.TTerm Syntax.QueryExpression
unSubquery x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.Subquery")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtableClause :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.SubtableClause
subtableClause x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.SubtableClause"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unSubtableClause :: Phantoms.TTerm Syntax.SubtableClause -> Phantoms.TTerm ()
unSubtableClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.SubtableClause")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

subtypeTreatment :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.SubtypeTreatment
subtypeTreatment x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.SubtypeTreatment"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unSubtypeTreatment :: Phantoms.TTerm Syntax.SubtypeTreatment -> Phantoms.TTerm ()
unSubtypeTreatment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.SubtypeTreatment")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableCommitActionPreserve :: Phantoms.TTerm Syntax.TableCommitAction
tableCommitActionPreserve =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableCommitAction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "preserve"),
        Core.fieldTerm = Core.TermUnit}}))

tableCommitActionDelete :: Phantoms.TTerm Syntax.TableCommitAction
tableCommitActionDelete =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableCommitAction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "delete"),
        Core.fieldTerm = Core.TermUnit}}))

tableConstraintDefinition :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.TableConstraintDefinition
tableConstraintDefinition x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableConstraintDefinition"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unTableConstraintDefinition :: Phantoms.TTerm Syntax.TableConstraintDefinition -> Phantoms.TTerm ()
unTableConstraintDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.TableConstraintDefinition")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableContentsSourceList :: Phantoms.TTerm Syntax.TableElementList -> Phantoms.TTerm Syntax.TableContentsSource
tableContentsSourceList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tableContentsSourceSubtable :: Phantoms.TTerm Syntax.TableContentsSource_Subtable -> Phantoms.TTerm Syntax.TableContentsSource
tableContentsSourceSubtable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subtable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tableContentsSourceSubquery :: Phantoms.TTerm Syntax.AsSubqueryClause -> Phantoms.TTerm Syntax.TableContentsSource
tableContentsSourceSubquery x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subquery"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tableContentsSource_Subtable :: Phantoms.TTerm Syntax.PathResolvedUserDefinedTypeName -> Phantoms.TTerm (Maybe Syntax.SubtableClause) -> Phantoms.TTerm (Maybe Syntax.TableElementList) -> Phantoms.TTerm Syntax.TableContentsSource_Subtable
tableContentsSource_Subtable type_ subtable elements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "subtable"),
          Core.fieldTerm = (Phantoms.unTTerm subtable)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm elements)}]}))

tableContentsSource_SubtableType :: Phantoms.TTerm Syntax.TableContentsSource_Subtable -> Phantoms.TTerm Syntax.PathResolvedUserDefinedTypeName
tableContentsSource_SubtableType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableContentsSource_SubtableSubtable :: Phantoms.TTerm Syntax.TableContentsSource_Subtable -> Phantoms.TTerm (Maybe Syntax.SubtableClause)
tableContentsSource_SubtableSubtable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable"),
        Core.projectionField = (Core.Name "subtable")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableContentsSource_SubtableElements :: Phantoms.TTerm Syntax.TableContentsSource_Subtable -> Phantoms.TTerm (Maybe Syntax.TableElementList)
tableContentsSource_SubtableElements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable"),
        Core.projectionField = (Core.Name "elements")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableContentsSource_SubtableWithType :: Phantoms.TTerm Syntax.TableContentsSource_Subtable -> Phantoms.TTerm Syntax.PathResolvedUserDefinedTypeName -> Phantoms.TTerm Syntax.TableContentsSource_Subtable
tableContentsSource_SubtableWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subtable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable"),
              Core.projectionField = (Core.Name "subtable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable"),
              Core.projectionField = (Core.Name "elements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tableContentsSource_SubtableWithSubtable :: Phantoms.TTerm Syntax.TableContentsSource_Subtable -> Phantoms.TTerm (Maybe Syntax.SubtableClause) -> Phantoms.TTerm Syntax.TableContentsSource_Subtable
tableContentsSource_SubtableWithSubtable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subtable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable"),
              Core.projectionField = (Core.Name "elements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tableContentsSource_SubtableWithElements :: Phantoms.TTerm Syntax.TableContentsSource_Subtable -> Phantoms.TTerm (Maybe Syntax.TableElementList) -> Phantoms.TTerm Syntax.TableContentsSource_Subtable
tableContentsSource_SubtableWithElements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subtable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable"),
              Core.projectionField = (Core.Name "subtable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tableDefinition :: Phantoms.TTerm (Maybe Syntax.TableScope) -> Phantoms.TTerm Syntax.TableName -> Phantoms.TTerm Syntax.TableContentsSource -> Phantoms.TTerm (Maybe Syntax.TableCommitAction) -> Phantoms.TTerm Syntax.TableDefinition
tableDefinition scope name source commitActions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm scope)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "commitActions"),
          Core.fieldTerm = (Phantoms.unTTerm commitActions)}]}))

tableDefinitionScope :: Phantoms.TTerm Syntax.TableDefinition -> Phantoms.TTerm (Maybe Syntax.TableScope)
tableDefinitionScope x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
        Core.projectionField = (Core.Name "scope")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableDefinitionName :: Phantoms.TTerm Syntax.TableDefinition -> Phantoms.TTerm Syntax.TableName
tableDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableDefinitionSource :: Phantoms.TTerm Syntax.TableDefinition -> Phantoms.TTerm Syntax.TableContentsSource
tableDefinitionSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
        Core.projectionField = (Core.Name "source")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableDefinitionCommitActions :: Phantoms.TTerm Syntax.TableDefinition -> Phantoms.TTerm (Maybe Syntax.TableCommitAction)
tableDefinitionCommitActions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
        Core.projectionField = (Core.Name "commitActions")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableDefinitionWithScope :: Phantoms.TTerm Syntax.TableDefinition -> Phantoms.TTerm (Maybe Syntax.TableScope) -> Phantoms.TTerm Syntax.TableDefinition
tableDefinitionWithScope original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "commitActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
              Core.projectionField = (Core.Name "commitActions")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tableDefinitionWithName :: Phantoms.TTerm Syntax.TableDefinition -> Phantoms.TTerm Syntax.TableName -> Phantoms.TTerm Syntax.TableDefinition
tableDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
              Core.projectionField = (Core.Name "scope")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "commitActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
              Core.projectionField = (Core.Name "commitActions")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tableDefinitionWithSource :: Phantoms.TTerm Syntax.TableDefinition -> Phantoms.TTerm Syntax.TableContentsSource -> Phantoms.TTerm Syntax.TableDefinition
tableDefinitionWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
              Core.projectionField = (Core.Name "scope")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "commitActions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
              Core.projectionField = (Core.Name "commitActions")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tableDefinitionWithCommitActions :: Phantoms.TTerm Syntax.TableDefinition -> Phantoms.TTerm (Maybe Syntax.TableCommitAction) -> Phantoms.TTerm Syntax.TableDefinition
tableDefinitionWithCommitActions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scope"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
              Core.projectionField = (Core.Name "scope")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "commitActions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tableElementColumn :: Phantoms.TTerm Syntax.ColumnDefinition -> Phantoms.TTerm Syntax.TableElement
tableElementColumn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "column"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tableElementTableConstraint :: Phantoms.TTerm Syntax.TableConstraintDefinition -> Phantoms.TTerm Syntax.TableElement
tableElementTableConstraint x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tableConstraint"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tableElementLike :: Phantoms.TTerm Syntax.LikeClause -> Phantoms.TTerm Syntax.TableElement
tableElementLike x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "like"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tableElementSelfReferencingColumn :: Phantoms.TTerm Syntax.SelfReferencingColumnSpecification -> Phantoms.TTerm Syntax.TableElement
tableElementSelfReferencingColumn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "selfReferencingColumn"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tableElementColumOptions :: Phantoms.TTerm Syntax.ColumnOptions -> Phantoms.TTerm Syntax.TableElement
tableElementColumOptions x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "columOptions"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tableElementList :: Phantoms.TTerm Syntax.TableElement -> Phantoms.TTerm [Syntax.TableElement] -> Phantoms.TTerm Syntax.TableElementList
tableElementList first rest =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElementList"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm rest)}]}))

tableElementListFirst :: Phantoms.TTerm Syntax.TableElementList -> Phantoms.TTerm Syntax.TableElement
tableElementListFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElementList"),
        Core.projectionField = (Core.Name "first")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableElementListRest :: Phantoms.TTerm Syntax.TableElementList -> Phantoms.TTerm [Syntax.TableElement]
tableElementListRest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElementList"),
        Core.projectionField = (Core.Name "rest")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableElementListWithFirst :: Phantoms.TTerm Syntax.TableElementList -> Phantoms.TTerm Syntax.TableElement -> Phantoms.TTerm Syntax.TableElementList
tableElementListWithFirst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElementList"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElementList"),
              Core.projectionField = (Core.Name "rest")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tableElementListWithRest :: Phantoms.TTerm Syntax.TableElementList -> Phantoms.TTerm [Syntax.TableElement] -> Phantoms.TTerm Syntax.TableElementList
tableElementListWithRest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElementList"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElementList"),
              Core.projectionField = (Core.Name "first")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tableScope :: Phantoms.TTerm Syntax.GlobalOrLocal -> Phantoms.TTerm Syntax.TableScope
tableScope x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableScope"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unTableScope :: Phantoms.TTerm Syntax.TableScope -> Phantoms.TTerm Syntax.GlobalOrLocal
unTableScope x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.TableScope")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

timeLiteral :: Phantoms.TTerm Syntax.TimeString -> Phantoms.TTerm Syntax.TimeLiteral
timeLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TimeLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unTimeLiteral :: Phantoms.TTerm Syntax.TimeLiteral -> Phantoms.TTerm Syntax.TimeString
unTimeLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.TimeLiteral")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

truthValueTRUE :: Phantoms.TTerm Syntax.TruthValue
truthValueTRUE =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TruthValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "TRUE"),
        Core.fieldTerm = Core.TermUnit}}))

truthValueFALSE :: Phantoms.TTerm Syntax.TruthValue
truthValueFALSE =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TruthValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "FALSE"),
        Core.fieldTerm = Core.TermUnit}}))

truthValueUNKNOWN :: Phantoms.TTerm Syntax.TruthValue
truthValueUNKNOWN =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TruthValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "UNKNOWN"),
        Core.fieldTerm = Core.TermUnit}}))

unsignedLiteralNumeric :: Phantoms.TTerm Syntax.UnsignedNumericLiteral -> Phantoms.TTerm Syntax.UnsignedLiteral
unsignedLiteralNumeric x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.UnsignedLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "numeric"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unsignedLiteralGeneral :: Phantoms.TTerm Syntax.GeneralLiteral -> Phantoms.TTerm Syntax.UnsignedLiteral
unsignedLiteralGeneral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.UnsignedLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "general"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unsignedNumericLiteralExact :: Phantoms.TTerm Syntax.ExactNumericLiteral -> Phantoms.TTerm Syntax.UnsignedNumericLiteral
unsignedNumericLiteralExact x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.UnsignedNumericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exact"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unsignedNumericLiteralApproximate :: Phantoms.TTerm Syntax.ApproximateNumericLiteral -> Phantoms.TTerm Syntax.UnsignedNumericLiteral
unsignedNumericLiteralApproximate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.UnsignedNumericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "approximate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unsignedValueSpecificationLiteral :: Phantoms.TTerm Syntax.UnsignedLiteral -> Phantoms.TTerm Syntax.UnsignedValueSpecification
unsignedValueSpecificationLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.UnsignedValueSpecification"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unsignedValueSpecificationGeneral :: Phantoms.TTerm Syntax.GeneralValueSpecification -> Phantoms.TTerm Syntax.UnsignedValueSpecification
unsignedValueSpecificationGeneral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.UnsignedValueSpecification"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "general"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

userDefinedTypeValueExpression :: Phantoms.TTerm Syntax.ValueExpressionPrimary -> Phantoms.TTerm Syntax.UserDefinedTypeValueExpression
userDefinedTypeValueExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.UserDefinedTypeValueExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unUserDefinedTypeValueExpression :: Phantoms.TTerm Syntax.UserDefinedTypeValueExpression -> Phantoms.TTerm Syntax.ValueExpressionPrimary
unUserDefinedTypeValueExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.UserDefinedTypeValueExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valueExpressionCommon :: Phantoms.TTerm Syntax.CommonValueExpression -> Phantoms.TTerm Syntax.ValueExpression
valueExpressionCommon x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "common"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueExpressionBoolean :: Phantoms.TTerm Syntax.BooleanValueExpression -> Phantoms.TTerm Syntax.ValueExpression
valueExpressionBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueExpressionRow :: Phantoms.TTerm Syntax.RowValueExpression -> Phantoms.TTerm Syntax.ValueExpression
valueExpressionRow x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ValueExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "row"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueExpressionPrimaryParens :: Phantoms.TTerm Syntax.ParenthesizedValueExpression -> Phantoms.TTerm Syntax.ValueExpressionPrimary
valueExpressionPrimaryParens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueExpressionPrimaryNoparens :: Phantoms.TTerm Syntax.NonparenthesizedValueExpressionPrimary -> Phantoms.TTerm Syntax.ValueExpressionPrimary
valueExpressionPrimaryNoparens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ValueExpressionPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noparens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

windowFunction :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.WindowFunction
windowFunction x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.WindowFunction"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unWindowFunction :: Phantoms.TTerm Syntax.WindowFunction -> Phantoms.TTerm ()
unWindowFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.ansi.sql.syntax.WindowFunction")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
