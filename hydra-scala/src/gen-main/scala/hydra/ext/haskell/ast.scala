package hydra.ext.haskell.ast

import hydra.core.*

case class Alternative(pattern: hydra.ext.haskell.ast.Pattern, rhs: hydra.ext.haskell.ast.CaseRhs, binds: Option[hydra.ext.haskell.ast.LocalBindings])

enum Assertion :
   case `class`(value: hydra.ext.haskell.ast.ClassAssertion) extends Assertion
   case tuple(value: Seq[hydra.ext.haskell.ast.Assertion]) extends Assertion

case class ClassAssertion(name: hydra.ext.haskell.ast.Name, types: Seq[hydra.ext.haskell.ast.Type])

type CaseRhs = hydra.ext.haskell.ast.Expression

enum Constructor :
   case ordinary(value: hydra.ext.haskell.ast.OrdinaryConstructor) extends Constructor
   case record(value: hydra.ext.haskell.ast.RecordConstructor) extends Constructor

case class OrdinaryConstructor(name: hydra.ext.haskell.ast.Name, fields: Seq[hydra.ext.haskell.ast.Type])

case class RecordConstructor(name: hydra.ext.haskell.ast.Name, fields: Seq[hydra.ext.haskell.ast.FieldWithComments])

case class ConstructorWithComments(body: hydra.ext.haskell.ast.Constructor, comments: Option[scala.Predef.String])

case class DataDeclaration(keyword: hydra.ext.haskell.ast.DataOrNewtype, context: Seq[hydra.ext.haskell.ast.Assertion], head: hydra.ext.haskell.ast.DeclarationHead, constructors: Seq[hydra.ext.haskell.ast.ConstructorWithComments], deriving: Seq[hydra.ext.haskell.ast.Deriving])

enum DataOrNewtype :
   case data extends DataOrNewtype
   case newtype extends DataOrNewtype

case class DeclarationWithComments(body: hydra.ext.haskell.ast.Declaration, comments: Option[scala.Predef.String])

enum Declaration :
   case data(value: hydra.ext.haskell.ast.DataDeclaration) extends Declaration
   case `type`(value: hydra.ext.haskell.ast.TypeDeclaration) extends Declaration
   case valueBinding(value: hydra.ext.haskell.ast.ValueBinding) extends Declaration
   case typedBinding(value: hydra.ext.haskell.ast.TypedBinding) extends Declaration

enum DeclarationHead :
   case application(value: hydra.ext.haskell.ast.ApplicationDeclarationHead) extends DeclarationHead
   case parens(value: hydra.ext.haskell.ast.DeclarationHead) extends DeclarationHead
   case simple(value: hydra.ext.haskell.ast.Name) extends DeclarationHead

case class ApplicationDeclarationHead(function: hydra.ext.haskell.ast.DeclarationHead, operand: hydra.ext.haskell.ast.Variable)

type Deriving = Seq[hydra.ext.haskell.ast.Name]

enum Export :
   case declaration(value: hydra.ext.haskell.ast.ImportExportSpec) extends Export
   case module(value: hydra.ext.haskell.ast.ModuleName) extends Export

enum Expression :
   case application(value: hydra.ext.haskell.ast.ApplicationExpression) extends Expression
   case `case`(value: hydra.ext.haskell.ast.CaseExpression) extends Expression
   case constructRecord(value: hydra.ext.haskell.ast.ConstructRecordExpression) extends Expression
   case `do`(value: Seq[hydra.ext.haskell.ast.Statement]) extends Expression
   case `if`(value: hydra.ext.haskell.ast.IfExpression) extends Expression
   case infixApplication(value: hydra.ext.haskell.ast.InfixApplicationExpression) extends Expression
   case literal(value: hydra.ext.haskell.ast.Literal) extends Expression
   case lambda(value: hydra.ext.haskell.ast.LambdaExpression) extends Expression
   case leftSection(value: hydra.ext.haskell.ast.SectionExpression) extends Expression
   case let(value: hydra.ext.haskell.ast.LetExpression) extends Expression
   case list(value: Seq[hydra.ext.haskell.ast.Expression]) extends Expression
   case parens(value: hydra.ext.haskell.ast.Expression) extends Expression
   case prefixApplication(value: hydra.ext.haskell.ast.PrefixApplicationExpression) extends Expression
   case rightSection(value: hydra.ext.haskell.ast.SectionExpression) extends Expression
   case tuple(value: Seq[hydra.ext.haskell.ast.Expression]) extends Expression
   case typeSignature(value: hydra.ext.haskell.ast.TypeSignatureExpression) extends Expression
   case updateRecord(value: hydra.ext.haskell.ast.UpdateRecordExpression) extends Expression
   case variable(value: hydra.ext.haskell.ast.Name) extends Expression

case class ApplicationExpression(function: hydra.ext.haskell.ast.Expression, argument: hydra.ext.haskell.ast.Expression)

case class CaseExpression(`case`: hydra.ext.haskell.ast.Expression, alternatives: Seq[hydra.ext.haskell.ast.Alternative])

case class ConstructRecordExpression(name: hydra.ext.haskell.ast.Name, fields: Seq[hydra.ext.haskell.ast.FieldUpdate])

case class IfExpression(condition: hydra.ext.haskell.ast.Expression, `then`: hydra.ext.haskell.ast.Expression, `else`: hydra.ext.haskell.ast.Expression)

case class InfixApplicationExpression(lhs: hydra.ext.haskell.ast.Expression, operator: hydra.ext.haskell.ast.Operator, rhs: hydra.ext.haskell.ast.Expression)

case class LambdaExpression(bindings: Seq[hydra.ext.haskell.ast.Pattern], inner: hydra.ext.haskell.ast.Expression)

case class LetExpression(bindings: Seq[hydra.ext.haskell.ast.LocalBinding], inner: hydra.ext.haskell.ast.Expression)

case class PrefixApplicationExpression(operator: hydra.ext.haskell.ast.Operator, rhs: hydra.ext.haskell.ast.Expression)

case class SectionExpression(operator: hydra.ext.haskell.ast.Operator, expression: hydra.ext.haskell.ast.Expression)

case class TypeSignatureExpression(inner: hydra.ext.haskell.ast.Expression, `type`: hydra.ext.haskell.ast.Type)

case class UpdateRecordExpression(inner: hydra.ext.haskell.ast.Expression, fields: Seq[hydra.ext.haskell.ast.FieldUpdate])

case class Field(name: hydra.ext.haskell.ast.Name, `type`: hydra.ext.haskell.ast.Type)

case class FieldWithComments(field: hydra.ext.haskell.ast.Field, comments: Option[scala.Predef.String])

case class FieldUpdate(name: hydra.ext.haskell.ast.Name, value: hydra.ext.haskell.ast.Expression)

case class Import(qualified: Boolean, module: hydra.ext.haskell.ast.ModuleName, as: Option[hydra.ext.haskell.ast.ModuleName], spec: Option[hydra.ext.haskell.ast.SpecImport])

enum SpecImport :
   case list(value: Seq[hydra.ext.haskell.ast.ImportExportSpec]) extends SpecImport
   case hiding(value: Seq[hydra.ext.haskell.ast.ImportExportSpec]) extends SpecImport

enum ImportModifier :
   case pattern extends ImportModifier
   case `type` extends ImportModifier

case class ImportExportSpec(modifier: Option[hydra.ext.haskell.ast.ImportModifier], name: hydra.ext.haskell.ast.Name, subspec: Option[hydra.ext.haskell.ast.SubspecImportExportSpec])

enum SubspecImportExportSpec :
   case all extends SubspecImportExportSpec
   case list(value: Seq[hydra.ext.haskell.ast.Name]) extends SubspecImportExportSpec

enum Literal :
   case char(value: Int) extends Literal
   case double(value: Double) extends Literal
   case float(value: Float) extends Literal
   case int(value: Int) extends Literal
   case integer(value: BigInt) extends Literal
   case string(value: scala.Predef.String) extends Literal

enum LocalBinding :
   case signature(value: hydra.ext.haskell.ast.TypeSignature) extends LocalBinding
   case value(value: hydra.ext.haskell.ast.ValueBinding) extends LocalBinding

type LocalBindings = Seq[hydra.ext.haskell.ast.LocalBinding]

case class Module(head: Option[hydra.ext.haskell.ast.ModuleHead], imports: Seq[hydra.ext.haskell.ast.Import], declarations: Seq[hydra.ext.haskell.ast.DeclarationWithComments])

case class ModuleHead(comments: Option[scala.Predef.String], name: hydra.ext.haskell.ast.ModuleName, exports: Seq[hydra.ext.haskell.ast.Export])

type ModuleName = scala.Predef.String

enum Name :
   case `implicit`(value: hydra.ext.haskell.ast.QualifiedName) extends Name
   case normal(value: hydra.ext.haskell.ast.QualifiedName) extends Name
   case parens(value: hydra.ext.haskell.ast.QualifiedName) extends Name

type NamePart = scala.Predef.String

enum Operator :
   case backtick(value: hydra.ext.haskell.ast.QualifiedName) extends Operator
   case normal(value: hydra.ext.haskell.ast.QualifiedName) extends Operator

enum Pattern :
   case application(value: hydra.ext.haskell.ast.ApplicationPattern) extends Pattern
   case as(value: hydra.ext.haskell.ast.AsPattern) extends Pattern
   case list(value: Seq[hydra.ext.haskell.ast.Pattern]) extends Pattern
   case literal(value: hydra.ext.haskell.ast.Literal) extends Pattern
   case name(value: hydra.ext.haskell.ast.Name) extends Pattern
   case parens(value: hydra.ext.haskell.ast.Pattern) extends Pattern
   case record(value: hydra.ext.haskell.ast.RecordPattern) extends Pattern
   case tuple(value: Seq[hydra.ext.haskell.ast.Pattern]) extends Pattern
   case typed(value: hydra.ext.haskell.ast.TypedPattern) extends Pattern
   case wildcard extends Pattern

case class ApplicationPattern(name: hydra.ext.haskell.ast.Name, args: Seq[hydra.ext.haskell.ast.Pattern])

case class AsPattern(name: hydra.ext.haskell.ast.Name, inner: hydra.ext.haskell.ast.Pattern)

case class RecordPattern(name: hydra.ext.haskell.ast.Name, fields: Seq[hydra.ext.haskell.ast.PatternField])

case class TypedPattern(inner: hydra.ext.haskell.ast.Pattern, `type`: hydra.ext.haskell.ast.Type)

case class PatternField(name: hydra.ext.haskell.ast.Name, pattern: hydra.ext.haskell.ast.Pattern)

case class QualifiedName(qualifiers: Seq[hydra.ext.haskell.ast.NamePart], unqualified: hydra.ext.haskell.ast.NamePart)

type RightHandSide = hydra.ext.haskell.ast.Expression

type Statement = hydra.ext.haskell.ast.Expression

enum Type :
   case application(value: hydra.ext.haskell.ast.ApplicationType) extends Type
   case ctx(value: hydra.ext.haskell.ast.ContextType) extends Type
   case function(value: hydra.ext.haskell.ast.FunctionType) extends Type
   case infix(value: hydra.ext.haskell.ast.InfixType) extends Type
   case list(value: hydra.ext.haskell.ast.Type) extends Type
   case parens(value: hydra.ext.haskell.ast.Type) extends Type
   case tuple(value: Seq[hydra.ext.haskell.ast.Type]) extends Type
   case variable(value: hydra.ext.haskell.ast.Name) extends Type

case class ApplicationType(context: hydra.ext.haskell.ast.Type, argument: hydra.ext.haskell.ast.Type)

case class ContextType(ctx: hydra.ext.haskell.ast.Assertion, `type`: hydra.ext.haskell.ast.Type)

case class FunctionType(domain: hydra.ext.haskell.ast.Type, codomain: hydra.ext.haskell.ast.Type)

case class InfixType(lhs: hydra.ext.haskell.ast.Type, operator: hydra.ext.haskell.ast.Operator, rhs: hydra.ext.haskell.ast.Operator)

case class TypeDeclaration(name: hydra.ext.haskell.ast.DeclarationHead, `type`: hydra.ext.haskell.ast.Type)

case class TypeSignature(name: hydra.ext.haskell.ast.Name, `type`: hydra.ext.haskell.ast.Type)

case class TypedBinding(typeSignature: hydra.ext.haskell.ast.TypeSignature, valueBinding: hydra.ext.haskell.ast.ValueBinding)

enum ValueBinding :
   case simple(value: hydra.ext.haskell.ast.SimpleValueBinding) extends ValueBinding

case class SimpleValueBinding(pattern: hydra.ext.haskell.ast.Pattern, rhs: hydra.ext.haskell.ast.RightHandSide, localBindings: Option[hydra.ext.haskell.ast.LocalBindings])

type Variable = hydra.ext.haskell.ast.Name
