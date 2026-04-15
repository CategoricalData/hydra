package hydra.haskell.syntax

import hydra.core.*

case class Alternative(pattern: hydra.haskell.syntax.Pattern, rhs: hydra.haskell.syntax.CaseRhs,
   binds: Option[hydra.haskell.syntax.LocalBindings])

enum Assertion :
   case `class`(value: hydra.haskell.syntax.ClassAssertion) extends Assertion
   case tuple(value: Seq[hydra.haskell.syntax.Assertion]) extends Assertion

case class ClassAssertion(name: hydra.haskell.syntax.Name, types: Seq[hydra.haskell.syntax.Type])

type CaseRhs = hydra.haskell.syntax.Expression

enum Constructor :
   case ordinary(value: hydra.haskell.syntax.OrdinaryConstructor) extends Constructor
   case record(value: hydra.haskell.syntax.RecordConstructor) extends Constructor

case class OrdinaryConstructor(name: hydra.haskell.syntax.Name, fields: Seq[hydra.haskell.syntax.Type])

case class RecordConstructor(name: hydra.haskell.syntax.Name, fields: Seq[hydra.haskell.syntax.FieldWithComments])

case class ConstructorWithComments(body: hydra.haskell.syntax.Constructor, comments: Option[scala.Predef.String])

case class DataDeclaration(keyword: hydra.haskell.syntax.DataOrNewtype, context: Seq[hydra.haskell.syntax.Assertion],
   head: hydra.haskell.syntax.DeclarationHead, constructors: Seq[hydra.haskell.syntax.ConstructorWithComments],
   deriving: Seq[hydra.haskell.syntax.Deriving])

enum DataOrNewtype :
   case data extends DataOrNewtype
   case newtype extends DataOrNewtype

case class DeclarationWithComments(body: hydra.haskell.syntax.Declaration, comments: Option[scala.Predef.String])

enum Declaration :
   case data(value: hydra.haskell.syntax.DataDeclaration) extends Declaration
   case `type`(value: hydra.haskell.syntax.TypeDeclaration) extends Declaration
   case valueBinding(value: hydra.haskell.syntax.ValueBinding) extends Declaration
   case typedBinding(value: hydra.haskell.syntax.TypedBinding) extends Declaration

enum DeclarationHead :
   case application(value: hydra.haskell.syntax.ApplicationDeclarationHead) extends DeclarationHead
   case parens(value: hydra.haskell.syntax.DeclarationHead) extends DeclarationHead
   case simple(value: hydra.haskell.syntax.Name) extends DeclarationHead

case class ApplicationDeclarationHead(function: hydra.haskell.syntax.DeclarationHead,
   operand: hydra.haskell.syntax.Variable)

type Deriving = Seq[hydra.haskell.syntax.Name]

enum Export :
   case declaration(value: hydra.haskell.syntax.ImportExportSpec) extends Export
   case module(value: hydra.haskell.syntax.ModuleName) extends Export

enum Expression :
   case application(value: hydra.haskell.syntax.ApplicationExpression) extends Expression
   case `case`(value: hydra.haskell.syntax.CaseExpression) extends Expression
   case constructRecord(value: hydra.haskell.syntax.ConstructRecordExpression) extends Expression
   case `do`(value: Seq[hydra.haskell.syntax.Statement]) extends Expression
   case `if`(value: hydra.haskell.syntax.IfExpression) extends Expression
   case infixApplication(value: hydra.haskell.syntax.InfixApplicationExpression) extends Expression
   case literal(value: hydra.haskell.syntax.Literal) extends Expression
   case lambda(value: hydra.haskell.syntax.LambdaExpression) extends Expression
   case leftSection(value: hydra.haskell.syntax.SectionExpression) extends Expression
   case let(value: hydra.haskell.syntax.LetExpression) extends Expression
   case list(value: Seq[hydra.haskell.syntax.Expression]) extends Expression
   case parens(value: hydra.haskell.syntax.Expression) extends Expression
   case prefixApplication(value: hydra.haskell.syntax.PrefixApplicationExpression) extends Expression
   case rightSection(value: hydra.haskell.syntax.SectionExpression) extends Expression
   case tuple(value: Seq[hydra.haskell.syntax.Expression]) extends Expression
   case typeSignature(value: hydra.haskell.syntax.TypeSignatureExpression) extends Expression
   case updateRecord(value: hydra.haskell.syntax.UpdateRecordExpression) extends Expression
   case variable(value: hydra.haskell.syntax.Name) extends Expression

case class ApplicationExpression(function: hydra.haskell.syntax.Expression, argument: hydra.haskell.syntax.Expression)

case class CaseExpression(`case`: hydra.haskell.syntax.Expression, alternatives: Seq[hydra.haskell.syntax.Alternative])

case class ConstructRecordExpression(name: hydra.haskell.syntax.Name, fields: Seq[hydra.haskell.syntax.FieldUpdate])

case class IfExpression(condition: hydra.haskell.syntax.Expression, `then`: hydra.haskell.syntax.Expression,
   `else`: hydra.haskell.syntax.Expression)

case class InfixApplicationExpression(lhs: hydra.haskell.syntax.Expression, operator: hydra.haskell.syntax.Operator,
   rhs: hydra.haskell.syntax.Expression)

case class LambdaExpression(bindings: Seq[hydra.haskell.syntax.Pattern], inner: hydra.haskell.syntax.Expression)

case class LetExpression(bindings: Seq[hydra.haskell.syntax.LocalBinding], inner: hydra.haskell.syntax.Expression)

case class PrefixApplicationExpression(operator: hydra.haskell.syntax.Operator, rhs: hydra.haskell.syntax.Expression)

case class SectionExpression(operator: hydra.haskell.syntax.Operator, expression: hydra.haskell.syntax.Expression)

case class TypeSignatureExpression(inner: hydra.haskell.syntax.Expression, `type`: hydra.haskell.syntax.Type)

case class UpdateRecordExpression(inner: hydra.haskell.syntax.Expression, fields: Seq[hydra.haskell.syntax.FieldUpdate])

case class Field(name: hydra.haskell.syntax.Name, `type`: hydra.haskell.syntax.Type)

case class FieldWithComments(field: hydra.haskell.syntax.Field, comments: Option[scala.Predef.String])

case class FieldUpdate(name: hydra.haskell.syntax.Name, value: hydra.haskell.syntax.Expression)

case class Import(qualified: Boolean, module: hydra.haskell.syntax.ModuleName, as: Option[hydra.haskell.syntax.ModuleName],
   spec: Option[hydra.haskell.syntax.SpecImport])

enum SpecImport :
   case list(value: Seq[hydra.haskell.syntax.ImportExportSpec]) extends SpecImport
   case hiding(value: Seq[hydra.haskell.syntax.ImportExportSpec]) extends SpecImport

enum ImportModifier :
   case pattern extends ImportModifier
   case `type` extends ImportModifier

case class ImportExportSpec(modifier: Option[hydra.haskell.syntax.ImportModifier],
   name: hydra.haskell.syntax.Name, subspec: Option[hydra.haskell.syntax.SubspecImportExportSpec])

enum SubspecImportExportSpec :
   case all extends SubspecImportExportSpec
   case list(value: Seq[hydra.haskell.syntax.Name]) extends SubspecImportExportSpec

enum Literal :
   case char(value: Int) extends Literal
   case double(value: Double) extends Literal
   case float(value: Float) extends Literal
   case int(value: Int) extends Literal
   case integer(value: BigInt) extends Literal
   case string(value: scala.Predef.String) extends Literal

enum LocalBinding :
   case signature(value: hydra.haskell.syntax.TypeSignature) extends LocalBinding
   case value(value: hydra.haskell.syntax.ValueBinding) extends LocalBinding

type LocalBindings = Seq[hydra.haskell.syntax.LocalBinding]

case class Module(head: Option[hydra.haskell.syntax.ModuleHead], imports: Seq[hydra.haskell.syntax.Import],
   declarations: Seq[hydra.haskell.syntax.DeclarationWithComments])

case class ModuleHead(comments: Option[scala.Predef.String], name: hydra.haskell.syntax.ModuleName,
   exports: Seq[hydra.haskell.syntax.Export])

type ModuleName = scala.Predef.String

enum Name :
   case `implicit`(value: hydra.haskell.syntax.QualifiedName) extends Name
   case normal(value: hydra.haskell.syntax.QualifiedName) extends Name
   case parens(value: hydra.haskell.syntax.QualifiedName) extends Name

type NamePart = scala.Predef.String

enum Operator :
   case backtick(value: hydra.haskell.syntax.QualifiedName) extends Operator
   case normal(value: hydra.haskell.syntax.QualifiedName) extends Operator

enum Pattern :
   case application(value: hydra.haskell.syntax.ApplicationPattern) extends Pattern
   case as(value: hydra.haskell.syntax.AsPattern) extends Pattern
   case list(value: Seq[hydra.haskell.syntax.Pattern]) extends Pattern
   case literal(value: hydra.haskell.syntax.Literal) extends Pattern
   case name(value: hydra.haskell.syntax.Name) extends Pattern
   case parens(value: hydra.haskell.syntax.Pattern) extends Pattern
   case record(value: hydra.haskell.syntax.RecordPattern) extends Pattern
   case tuple(value: Seq[hydra.haskell.syntax.Pattern]) extends Pattern
   case typed(value: hydra.haskell.syntax.TypedPattern) extends Pattern
   case wildcard extends Pattern

case class ApplicationPattern(name: hydra.haskell.syntax.Name, args: Seq[hydra.haskell.syntax.Pattern])

case class AsPattern(name: hydra.haskell.syntax.Name, inner: hydra.haskell.syntax.Pattern)

case class RecordPattern(name: hydra.haskell.syntax.Name, fields: Seq[hydra.haskell.syntax.PatternField])

case class TypedPattern(inner: hydra.haskell.syntax.Pattern, `type`: hydra.haskell.syntax.Type)

case class PatternField(name: hydra.haskell.syntax.Name, pattern: hydra.haskell.syntax.Pattern)

case class QualifiedName(qualifiers: Seq[hydra.haskell.syntax.NamePart], unqualified: hydra.haskell.syntax.NamePart)

type RightHandSide = hydra.haskell.syntax.Expression

type Statement = hydra.haskell.syntax.Expression

enum Type :
   case application(value: hydra.haskell.syntax.ApplicationType) extends Type
   case ctx(value: hydra.haskell.syntax.ContextType) extends Type
   case function(value: hydra.haskell.syntax.FunctionType) extends Type
   case infix(value: hydra.haskell.syntax.InfixType) extends Type
   case list(value: hydra.haskell.syntax.Type) extends Type
   case parens(value: hydra.haskell.syntax.Type) extends Type
   case tuple(value: Seq[hydra.haskell.syntax.Type]) extends Type
   case variable(value: hydra.haskell.syntax.Name) extends Type

case class ApplicationType(context: hydra.haskell.syntax.Type, argument: hydra.haskell.syntax.Type)

case class ContextType(ctx: hydra.haskell.syntax.Assertion, `type`: hydra.haskell.syntax.Type)

case class FunctionType(domain: hydra.haskell.syntax.Type, codomain: hydra.haskell.syntax.Type)

case class InfixType(lhs: hydra.haskell.syntax.Type, operator: hydra.haskell.syntax.Operator,
   rhs: hydra.haskell.syntax.Operator)

case class TypeDeclaration(name: hydra.haskell.syntax.DeclarationHead, `type`: hydra.haskell.syntax.Type)

case class TypeSignature(name: hydra.haskell.syntax.Name, `type`: hydra.haskell.syntax.Type)

case class TypedBinding(typeSignature: hydra.haskell.syntax.TypeSignature, valueBinding: hydra.haskell.syntax.ValueBinding)

enum ValueBinding :
   case simple(value: hydra.haskell.syntax.SimpleValueBinding) extends ValueBinding

case class SimpleValueBinding(pattern: hydra.haskell.syntax.Pattern, rhs: hydra.haskell.syntax.RightHandSide,
   localBindings: Option[hydra.haskell.syntax.LocalBindings])

type Variable = hydra.haskell.syntax.Name
