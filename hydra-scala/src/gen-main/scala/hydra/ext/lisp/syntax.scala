package hydra.ext.lisp.syntax

import hydra.core.*

case class Program(dialect: hydra.ext.lisp.syntax.Dialect, module: Option[hydra.ext.lisp.syntax.ModuleDeclaration], imports: Seq[hydra.ext.lisp.syntax.ImportDeclaration], exports: Seq[hydra.ext.lisp.syntax.ExportDeclaration], forms: Seq[hydra.ext.lisp.syntax.TopLevelFormWithComments])

enum TopLevelForm :
   case function(value: hydra.ext.lisp.syntax.FunctionDefinition) extends TopLevelForm
   case variable(value: hydra.ext.lisp.syntax.VariableDefinition) extends TopLevelForm
   case constant(value: hydra.ext.lisp.syntax.ConstantDefinition) extends TopLevelForm
   case recordType(value: hydra.ext.lisp.syntax.RecordTypeDefinition) extends TopLevelForm
   case `macro`(value: hydra.ext.lisp.syntax.MacroDefinition) extends TopLevelForm
   case expression(value: hydra.ext.lisp.syntax.Expression) extends TopLevelForm

case class TopLevelFormWithComments(doc: Option[hydra.ext.lisp.syntax.Docstring], comment: Option[hydra.ext.lisp.syntax.Comment], form: hydra.ext.lisp.syntax.TopLevelForm)

case class FunctionDefinition(name: hydra.ext.lisp.syntax.Symbol, params: Seq[hydra.ext.lisp.syntax.Symbol], restParam: Option[hydra.ext.lisp.syntax.Symbol], doc: Option[hydra.ext.lisp.syntax.Docstring], typeHints: Seq[hydra.ext.lisp.syntax.TypeHint], body: Seq[hydra.ext.lisp.syntax.Expression])

case class VariableDefinition(name: hydra.ext.lisp.syntax.Symbol, value: hydra.ext.lisp.syntax.Expression, doc: Option[hydra.ext.lisp.syntax.Docstring])

case class ConstantDefinition(name: hydra.ext.lisp.syntax.Symbol, value: hydra.ext.lisp.syntax.Expression, doc: Option[hydra.ext.lisp.syntax.Docstring])

case class RecordTypeDefinition(name: hydra.ext.lisp.syntax.Symbol, fields: Seq[hydra.ext.lisp.syntax.FieldDefinition], doc: Option[hydra.ext.lisp.syntax.Docstring])

case class FieldDefinition(name: hydra.ext.lisp.syntax.Symbol, defaultValue: Option[hydra.ext.lisp.syntax.Expression])

case class MacroDefinition(name: hydra.ext.lisp.syntax.Symbol, params: Seq[hydra.ext.lisp.syntax.Symbol], restParam: Option[hydra.ext.lisp.syntax.Symbol], body: Seq[hydra.ext.lisp.syntax.Expression])

enum Expression :
   case application(value: hydra.ext.lisp.syntax.Application) extends Expression
   case lambda(value: hydra.ext.lisp.syntax.Lambda) extends Expression
   case let(value: hydra.ext.lisp.syntax.LetExpression) extends Expression
   case `if`(value: hydra.ext.lisp.syntax.IfExpression) extends Expression
   case cond(value: hydra.ext.lisp.syntax.CondExpression) extends Expression
   case `case`(value: hydra.ext.lisp.syntax.CaseExpression) extends Expression
   case and(value: hydra.ext.lisp.syntax.AndExpression) extends Expression
   case or(value: hydra.ext.lisp.syntax.OrExpression) extends Expression
   case not(value: hydra.ext.lisp.syntax.NotExpression) extends Expression
   case `do`(value: hydra.ext.lisp.syntax.DoExpression) extends Expression
   case begin(value: hydra.ext.lisp.syntax.BeginExpression) extends Expression
   case variable(value: hydra.ext.lisp.syntax.VariableReference) extends Expression
   case literal(value: hydra.ext.lisp.syntax.Literal) extends Expression
   case list(value: hydra.ext.lisp.syntax.ListLiteral) extends Expression
   case vector(value: hydra.ext.lisp.syntax.VectorLiteral) extends Expression
   case map(value: hydra.ext.lisp.syntax.MapLiteral) extends Expression
   case set(value: hydra.ext.lisp.syntax.SetLiteral) extends Expression
   case cons(value: hydra.ext.lisp.syntax.ConsExpression) extends Expression
   case dottedPair(value: hydra.ext.lisp.syntax.DottedPair) extends Expression
   case fieldAccess(value: hydra.ext.lisp.syntax.FieldAccess) extends Expression
   case typeAnnotation(value: hydra.ext.lisp.syntax.TypeAnnotation) extends Expression
   case quote(value: hydra.ext.lisp.syntax.QuoteExpression) extends Expression
   case quasiquote(value: hydra.ext.lisp.syntax.QuasiquoteExpression) extends Expression
   case unquote(value: hydra.ext.lisp.syntax.UnquoteExpression) extends Expression
   case splicingUnquote(value: hydra.ext.lisp.syntax.SplicingUnquoteExpression) extends Expression
   case sExpression(value: hydra.ext.lisp.syntax.SExpression) extends Expression

case class Application(function: hydra.ext.lisp.syntax.Expression, arguments: Seq[hydra.ext.lisp.syntax.Expression])

case class Lambda(name: Option[hydra.ext.lisp.syntax.Symbol], params: Seq[hydra.ext.lisp.syntax.Symbol], restParam: Option[hydra.ext.lisp.syntax.Symbol], body: Seq[hydra.ext.lisp.syntax.Expression])

case class VariableReference(name: hydra.ext.lisp.syntax.Symbol, functionNamespace: Boolean)

case class FieldAccess(recordType: hydra.ext.lisp.syntax.Symbol, field: hydra.ext.lisp.syntax.Symbol, target: hydra.ext.lisp.syntax.Expression)

case class TypeAnnotation(expression: hydra.ext.lisp.syntax.Expression, `type`: hydra.ext.lisp.syntax.TypeSpecifier)

case class IfExpression(condition: hydra.ext.lisp.syntax.Expression, `then`: hydra.ext.lisp.syntax.Expression, `else`: Option[hydra.ext.lisp.syntax.Expression])

case class CondExpression(clauses: Seq[hydra.ext.lisp.syntax.CondClause], default: Option[hydra.ext.lisp.syntax.Expression])

case class CondClause(condition: hydra.ext.lisp.syntax.Expression, body: hydra.ext.lisp.syntax.Expression)

case class CaseExpression(scrutinee: hydra.ext.lisp.syntax.Expression, clauses: Seq[hydra.ext.lisp.syntax.CaseClause], default: Option[hydra.ext.lisp.syntax.Expression])

case class CaseClause(keys: Seq[hydra.ext.lisp.syntax.Expression], body: hydra.ext.lisp.syntax.Expression)

case class AndExpression(expressions: Seq[hydra.ext.lisp.syntax.Expression])

case class OrExpression(expressions: Seq[hydra.ext.lisp.syntax.Expression])

case class NotExpression(expression: hydra.ext.lisp.syntax.Expression)

case class DoExpression(expressions: Seq[hydra.ext.lisp.syntax.Expression])

case class BeginExpression(expressions: Seq[hydra.ext.lisp.syntax.Expression])

case class QuoteExpression(body: hydra.ext.lisp.syntax.Expression)

case class QuasiquoteExpression(body: hydra.ext.lisp.syntax.Expression)

case class UnquoteExpression(body: hydra.ext.lisp.syntax.Expression)

case class SplicingUnquoteExpression(body: hydra.ext.lisp.syntax.Expression)

case class LetExpression(kind: hydra.ext.lisp.syntax.LetKind, bindings: Seq[hydra.ext.lisp.syntax.LetBinding], body: Seq[hydra.ext.lisp.syntax.Expression])

enum LetKind :
   case parallel extends LetKind
   case sequential extends LetKind
   case recursive extends LetKind

enum LetBinding :
   case simple(value: hydra.ext.lisp.syntax.SimpleBinding) extends LetBinding
   case destructuring(value: hydra.ext.lisp.syntax.DestructuringBinding) extends LetBinding

case class SimpleBinding(name: hydra.ext.lisp.syntax.Symbol, value: hydra.ext.lisp.syntax.Expression)

case class DestructuringBinding(pattern: hydra.ext.lisp.syntax.DestructuringPattern, value: hydra.ext.lisp.syntax.Expression)

enum DestructuringPattern :
   case sequential(value: Seq[hydra.ext.lisp.syntax.Symbol]) extends DestructuringPattern
   case associative(value: Seq[hydra.ext.lisp.syntax.Symbol]) extends DestructuringPattern
   case rest(value: Seq[hydra.ext.lisp.syntax.Symbol]) extends DestructuringPattern

enum Pattern :
   case constructor(value: hydra.ext.lisp.syntax.ConstructorPattern) extends Pattern
   case literal(value: hydra.ext.lisp.syntax.LiteralPattern) extends Pattern
   case wildcard(value: hydra.ext.lisp.syntax.WildcardPattern) extends Pattern
   case variable(value: hydra.ext.lisp.syntax.Symbol) extends Pattern

case class ConstructorPattern(constructor: hydra.ext.lisp.syntax.Symbol, arguments: Seq[hydra.ext.lisp.syntax.Pattern])

case class LiteralPattern(value: hydra.ext.lisp.syntax.Literal)

case class WildcardPattern()

enum Literal :
   case integer(value: hydra.ext.lisp.syntax.IntegerLiteral) extends Literal
   case float(value: hydra.ext.lisp.syntax.FloatLiteral) extends Literal
   case string(value: scala.Predef.String) extends Literal
   case character(value: hydra.ext.lisp.syntax.CharacterLiteral) extends Literal
   case boolean(value: Boolean) extends Literal
   case nil extends Literal
   case keyword(value: hydra.ext.lisp.syntax.Keyword) extends Literal
   case symbol(value: hydra.ext.lisp.syntax.Symbol) extends Literal

case class IntegerLiteral(value: BigInt, bigint: Boolean)

case class FloatLiteral(value: BigDecimal, precision: Option[scala.Predef.String])

case class CharacterLiteral(value: scala.Predef.String)

enum BooleanStyle :
   case trueFalse extends BooleanStyle
   case tNil extends BooleanStyle
   case hashTF extends BooleanStyle

enum NilStyle :
   case nil extends NilStyle
   case emptyList extends NilStyle

type Symbol = scala.Predef.String

case class Keyword(name: scala.Predef.String, namespace: Option[scala.Predef.String])

case class QualifiedSymbol(namespace: scala.Predef.String, name: scala.Predef.String)

type NamespaceName = scala.Predef.String

case class ListLiteral(elements: Seq[hydra.ext.lisp.syntax.Expression], quoted: Boolean)

case class VectorLiteral(elements: Seq[hydra.ext.lisp.syntax.Expression])

case class MapLiteral(entries: Seq[hydra.ext.lisp.syntax.MapEntry])

case class MapEntry(key: hydra.ext.lisp.syntax.Expression, value: hydra.ext.lisp.syntax.Expression)

case class SetLiteral(elements: Seq[hydra.ext.lisp.syntax.Expression])

case class ConsExpression(head: hydra.ext.lisp.syntax.Expression, tail: hydra.ext.lisp.syntax.Expression)

case class DottedPair(car: hydra.ext.lisp.syntax.Expression, cdr: hydra.ext.lisp.syntax.Expression)

case class TypeHint(name: hydra.ext.lisp.syntax.Symbol, `type`: hydra.ext.lisp.syntax.TypeSpecifier)

enum TypeSpecifier :
   case named(value: hydra.ext.lisp.syntax.Symbol) extends TypeSpecifier
   case list(value: hydra.ext.lisp.syntax.TypeSpecifier) extends TypeSpecifier
   case function(value: Seq[hydra.ext.lisp.syntax.TypeSpecifier]) extends TypeSpecifier
   case maybe(value: hydra.ext.lisp.syntax.TypeSpecifier) extends TypeSpecifier
   case map(value: Seq[hydra.ext.lisp.syntax.TypeSpecifier]) extends TypeSpecifier
   case set(value: hydra.ext.lisp.syntax.TypeSpecifier) extends TypeSpecifier
   case pair(value: Seq[hydra.ext.lisp.syntax.TypeSpecifier]) extends TypeSpecifier
   case either(value: Seq[hydra.ext.lisp.syntax.TypeSpecifier]) extends TypeSpecifier
   case unit extends TypeSpecifier

case class ModuleDeclaration(name: hydra.ext.lisp.syntax.NamespaceName, doc: Option[hydra.ext.lisp.syntax.Docstring])

case class ImportDeclaration(module: hydra.ext.lisp.syntax.NamespaceName, spec: hydra.ext.lisp.syntax.ImportSpec)

enum ImportSpec :
   case all extends ImportSpec
   case alias(value: hydra.ext.lisp.syntax.Symbol) extends ImportSpec
   case only(value: Seq[hydra.ext.lisp.syntax.Symbol]) extends ImportSpec
   case rename(value: Seq[Seq[hydra.ext.lisp.syntax.Symbol]]) extends ImportSpec

case class ExportDeclaration(symbols: Seq[hydra.ext.lisp.syntax.Symbol])

case class Comment(style: hydra.ext.lisp.syntax.CommentStyle, text: scala.Predef.String)

enum CommentStyle :
   case line extends CommentStyle
   case block extends CommentStyle
   case datum extends CommentStyle

type Docstring = scala.Predef.String

enum Dialect :
   case clojure extends Dialect
   case emacsLisp extends Dialect
   case commonLisp extends Dialect
   case scheme extends Dialect

enum SExpression :
   case atom(value: scala.Predef.String) extends SExpression
   case list(value: Seq[hydra.ext.lisp.syntax.SExpression]) extends SExpression
