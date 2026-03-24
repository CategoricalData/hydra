package hydra.ext.haskell.syntax

import hydra.core.*

case class Alternative(pattern: hydra.ext.haskell.syntax.Pattern, rhs: hydra.ext.haskell.syntax.CaseRhs, binds: Option[hydra.ext.haskell.syntax.LocalBindings])

enum Assertion :
   case `class`(value: hydra.ext.haskell.syntax.ClassAssertion) extends Assertion
   case tuple(value: Seq[hydra.ext.haskell.syntax.Assertion]) extends Assertion

case class ClassAssertion(name: hydra.ext.haskell.syntax.Name, types: Seq[hydra.ext.haskell.syntax.Type])

type CaseRhs = hydra.ext.haskell.syntax.Expression

enum Constructor :
   case ordinary(value: hydra.ext.haskell.syntax.OrdinaryConstructor) extends Constructor
   case record(value: hydra.ext.haskell.syntax.RecordConstructor) extends Constructor

case class OrdinaryConstructor(name: hydra.ext.haskell.syntax.Name, fields: Seq[hydra.ext.haskell.syntax.Type])

case class RecordConstructor(name: hydra.ext.haskell.syntax.Name, fields: Seq[hydra.ext.haskell.syntax.FieldWithComments])

case class ConstructorWithComments(body: hydra.ext.haskell.syntax.Constructor, comments: Option[scala.Predef.String])

case class DataDeclaration(keyword: hydra.ext.haskell.syntax.DataOrNewtype, context: Seq[hydra.ext.haskell.syntax.Assertion], head: hydra.ext.haskell.syntax.DeclarationHead, constructors: Seq[hydra.ext.haskell.syntax.ConstructorWithComments], deriving: Seq[hydra.ext.haskell.syntax.Deriving])

enum DataOrNewtype :
   case data extends DataOrNewtype
   case newtype extends DataOrNewtype

case class DeclarationWithComments(body: hydra.ext.haskell.syntax.Declaration, comments: Option[scala.Predef.String])

enum Declaration :
   case data(value: hydra.ext.haskell.syntax.DataDeclaration) extends Declaration
   case `type`(value: hydra.ext.haskell.syntax.TypeDeclaration) extends Declaration
   case valueBinding(value: hydra.ext.haskell.syntax.ValueBinding) extends Declaration
   case typedBinding(value: hydra.ext.haskell.syntax.TypedBinding) extends Declaration

enum DeclarationHead :
   case application(value: hydra.ext.haskell.syntax.ApplicationDeclarationHead) extends DeclarationHead
   case parens(value: hydra.ext.haskell.syntax.DeclarationHead) extends DeclarationHead
   case simple(value: hydra.ext.haskell.syntax.Name) extends DeclarationHead

case class ApplicationDeclarationHead(function: hydra.ext.haskell.syntax.DeclarationHead, operand: hydra.ext.haskell.syntax.Variable)

type Deriving = Seq[hydra.ext.haskell.syntax.Name]

enum Export :
   case declaration(value: hydra.ext.haskell.syntax.ImportExportSpec) extends Export
   case module(value: hydra.ext.haskell.syntax.ModuleName) extends Export

enum Expression :
   case application(value: hydra.ext.haskell.syntax.ApplicationExpression) extends Expression
   case `case`(value: hydra.ext.haskell.syntax.CaseExpression) extends Expression
   case constructRecord(value: hydra.ext.haskell.syntax.ConstructRecordExpression) extends Expression
   case `do`(value: Seq[hydra.ext.haskell.syntax.Statement]) extends Expression
   case `if`(value: hydra.ext.haskell.syntax.IfExpression) extends Expression
   case infixApplication(value: hydra.ext.haskell.syntax.InfixApplicationExpression) extends Expression
   case literal(value: hydra.ext.haskell.syntax.Literal) extends Expression
   case lambda(value: hydra.ext.haskell.syntax.LambdaExpression) extends Expression
   case leftSection(value: hydra.ext.haskell.syntax.SectionExpression) extends Expression
   case let(value: hydra.ext.haskell.syntax.LetExpression) extends Expression
   case list(value: Seq[hydra.ext.haskell.syntax.Expression]) extends Expression
   case parens(value: hydra.ext.haskell.syntax.Expression) extends Expression
   case prefixApplication(value: hydra.ext.haskell.syntax.PrefixApplicationExpression) extends Expression
   case rightSection(value: hydra.ext.haskell.syntax.SectionExpression) extends Expression
   case tuple(value: Seq[hydra.ext.haskell.syntax.Expression]) extends Expression
   case typeSignature(value: hydra.ext.haskell.syntax.TypeSignatureExpression) extends Expression
   case updateRecord(value: hydra.ext.haskell.syntax.UpdateRecordExpression) extends Expression
   case variable(value: hydra.ext.haskell.syntax.Name) extends Expression

case class ApplicationExpression(function: hydra.ext.haskell.syntax.Expression, argument: hydra.ext.haskell.syntax.Expression)

case class CaseExpression(`case`: hydra.ext.haskell.syntax.Expression, alternatives: Seq[hydra.ext.haskell.syntax.Alternative])

case class ConstructRecordExpression(name: hydra.ext.haskell.syntax.Name, fields: Seq[hydra.ext.haskell.syntax.FieldUpdate])

case class IfExpression(condition: hydra.ext.haskell.syntax.Expression, `then`: hydra.ext.haskell.syntax.Expression, `else`: hydra.ext.haskell.syntax.Expression)

case class InfixApplicationExpression(lhs: hydra.ext.haskell.syntax.Expression, operator: hydra.ext.haskell.syntax.Operator, rhs: hydra.ext.haskell.syntax.Expression)

case class LambdaExpression(bindings: Seq[hydra.ext.haskell.syntax.Pattern], inner: hydra.ext.haskell.syntax.Expression)

case class LetExpression(bindings: Seq[hydra.ext.haskell.syntax.LocalBinding], inner: hydra.ext.haskell.syntax.Expression)

case class PrefixApplicationExpression(operator: hydra.ext.haskell.syntax.Operator, rhs: hydra.ext.haskell.syntax.Expression)

case class SectionExpression(operator: hydra.ext.haskell.syntax.Operator, expression: hydra.ext.haskell.syntax.Expression)

case class TypeSignatureExpression(inner: hydra.ext.haskell.syntax.Expression, `type`: hydra.ext.haskell.syntax.Type)

case class UpdateRecordExpression(inner: hydra.ext.haskell.syntax.Expression, fields: Seq[hydra.ext.haskell.syntax.FieldUpdate])

case class Field(name: hydra.ext.haskell.syntax.Name, `type`: hydra.ext.haskell.syntax.Type)

case class FieldWithComments(field: hydra.ext.haskell.syntax.Field, comments: Option[scala.Predef.String])

case class FieldUpdate(name: hydra.ext.haskell.syntax.Name, value: hydra.ext.haskell.syntax.Expression)

case class Import(qualified: Boolean, module: hydra.ext.haskell.syntax.ModuleName, as: Option[hydra.ext.haskell.syntax.ModuleName], spec: Option[hydra.ext.haskell.syntax.SpecImport])

enum SpecImport :
   case list(value: Seq[hydra.ext.haskell.syntax.ImportExportSpec]) extends SpecImport
   case hiding(value: Seq[hydra.ext.haskell.syntax.ImportExportSpec]) extends SpecImport

enum ImportModifier :
   case pattern extends ImportModifier
   case `type` extends ImportModifier

case class ImportExportSpec(modifier: Option[hydra.ext.haskell.syntax.ImportModifier], name: hydra.ext.haskell.syntax.Name, subspec: Option[hydra.ext.haskell.syntax.SubspecImportExportSpec])

enum SubspecImportExportSpec :
   case all extends SubspecImportExportSpec
   case list(value: Seq[hydra.ext.haskell.syntax.Name]) extends SubspecImportExportSpec

enum Literal :
   case char(value: Int) extends Literal
   case double(value: Double) extends Literal
   case float(value: Float) extends Literal
   case int(value: Int) extends Literal
   case integer(value: BigInt) extends Literal
   case string(value: scala.Predef.String) extends Literal

enum LocalBinding :
   case signature(value: hydra.ext.haskell.syntax.TypeSignature) extends LocalBinding
   case value(value: hydra.ext.haskell.syntax.ValueBinding) extends LocalBinding

type LocalBindings = Seq[hydra.ext.haskell.syntax.LocalBinding]

case class Module(head: Option[hydra.ext.haskell.syntax.ModuleHead], imports: Seq[hydra.ext.haskell.syntax.Import], declarations: Seq[hydra.ext.haskell.syntax.DeclarationWithComments])

case class ModuleHead(comments: Option[scala.Predef.String], name: hydra.ext.haskell.syntax.ModuleName, exports: Seq[hydra.ext.haskell.syntax.Export])

type ModuleName = scala.Predef.String

enum Name :
   case `implicit`(value: hydra.ext.haskell.syntax.QualifiedName) extends Name
   case normal(value: hydra.ext.haskell.syntax.QualifiedName) extends Name
   case parens(value: hydra.ext.haskell.syntax.QualifiedName) extends Name

type NamePart = scala.Predef.String

enum Operator :
   case backtick(value: hydra.ext.haskell.syntax.QualifiedName) extends Operator
   case normal(value: hydra.ext.haskell.syntax.QualifiedName) extends Operator

enum Pattern :
   case application(value: hydra.ext.haskell.syntax.ApplicationPattern) extends Pattern
   case as(value: hydra.ext.haskell.syntax.AsPattern) extends Pattern
   case list(value: Seq[hydra.ext.haskell.syntax.Pattern]) extends Pattern
   case literal(value: hydra.ext.haskell.syntax.Literal) extends Pattern
   case name(value: hydra.ext.haskell.syntax.Name) extends Pattern
   case parens(value: hydra.ext.haskell.syntax.Pattern) extends Pattern
   case record(value: hydra.ext.haskell.syntax.RecordPattern) extends Pattern
   case tuple(value: Seq[hydra.ext.haskell.syntax.Pattern]) extends Pattern
   case typed(value: hydra.ext.haskell.syntax.TypedPattern) extends Pattern
   case wildcard extends Pattern

case class ApplicationPattern(name: hydra.ext.haskell.syntax.Name, args: Seq[hydra.ext.haskell.syntax.Pattern])

case class AsPattern(name: hydra.ext.haskell.syntax.Name, inner: hydra.ext.haskell.syntax.Pattern)

case class RecordPattern(name: hydra.ext.haskell.syntax.Name, fields: Seq[hydra.ext.haskell.syntax.PatternField])

case class TypedPattern(inner: hydra.ext.haskell.syntax.Pattern, `type`: hydra.ext.haskell.syntax.Type)

case class PatternField(name: hydra.ext.haskell.syntax.Name, pattern: hydra.ext.haskell.syntax.Pattern)

case class QualifiedName(qualifiers: Seq[hydra.ext.haskell.syntax.NamePart], unqualified: hydra.ext.haskell.syntax.NamePart)

type RightHandSide = hydra.ext.haskell.syntax.Expression

type Statement = hydra.ext.haskell.syntax.Expression

enum Type :
   case application(value: hydra.ext.haskell.syntax.ApplicationType) extends Type
   case ctx(value: hydra.ext.haskell.syntax.ContextType) extends Type
   case function(value: hydra.ext.haskell.syntax.FunctionType) extends Type
   case infix(value: hydra.ext.haskell.syntax.InfixType) extends Type
   case list(value: hydra.ext.haskell.syntax.Type) extends Type
   case parens(value: hydra.ext.haskell.syntax.Type) extends Type
   case tuple(value: Seq[hydra.ext.haskell.syntax.Type]) extends Type
   case variable(value: hydra.ext.haskell.syntax.Name) extends Type

case class ApplicationType(context: hydra.ext.haskell.syntax.Type, argument: hydra.ext.haskell.syntax.Type)

case class ContextType(ctx: hydra.ext.haskell.syntax.Assertion, `type`: hydra.ext.haskell.syntax.Type)

case class FunctionType(domain: hydra.ext.haskell.syntax.Type, codomain: hydra.ext.haskell.syntax.Type)

case class InfixType(lhs: hydra.ext.haskell.syntax.Type, operator: hydra.ext.haskell.syntax.Operator, rhs: hydra.ext.haskell.syntax.Operator)

case class TypeDeclaration(name: hydra.ext.haskell.syntax.DeclarationHead, `type`: hydra.ext.haskell.syntax.Type)

case class TypeSignature(name: hydra.ext.haskell.syntax.Name, `type`: hydra.ext.haskell.syntax.Type)

case class TypedBinding(typeSignature: hydra.ext.haskell.syntax.TypeSignature, valueBinding: hydra.ext.haskell.syntax.ValueBinding)

enum ValueBinding :
   case simple(value: hydra.ext.haskell.syntax.SimpleValueBinding) extends ValueBinding

case class SimpleValueBinding(pattern: hydra.ext.haskell.syntax.Pattern, rhs: hydra.ext.haskell.syntax.RightHandSide, localBindings: Option[hydra.ext.haskell.syntax.LocalBindings])

type Variable = hydra.ext.haskell.syntax.Name
