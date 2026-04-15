package hydra.lisp.syntax

import hydra.core.*

case class Program(dialect: hydra.lisp.syntax.Dialect, module: Option[hydra.lisp.syntax.ModuleDeclaration],
   imports: Seq[hydra.lisp.syntax.ImportDeclaration], exports: Seq[hydra.lisp.syntax.ExportDeclaration],
   forms: Seq[hydra.lisp.syntax.TopLevelFormWithComments])

enum TopLevelForm :
   case function(value: hydra.lisp.syntax.FunctionDefinition) extends TopLevelForm
   case variable(value: hydra.lisp.syntax.VariableDefinition) extends TopLevelForm
   case constant(value: hydra.lisp.syntax.ConstantDefinition) extends TopLevelForm
   case recordType(value: hydra.lisp.syntax.RecordTypeDefinition) extends TopLevelForm
   case `macro`(value: hydra.lisp.syntax.MacroDefinition) extends TopLevelForm
   case expression(value: hydra.lisp.syntax.Expression) extends TopLevelForm

case class TopLevelFormWithComments(doc: Option[hydra.lisp.syntax.Docstring], comment: Option[hydra.lisp.syntax.Comment],
   form: hydra.lisp.syntax.TopLevelForm)

case class FunctionDefinition(name: hydra.lisp.syntax.Symbol, params: Seq[hydra.lisp.syntax.Symbol],
   restParam: Option[hydra.lisp.syntax.Symbol], doc: Option[hydra.lisp.syntax.Docstring],
   typeHints: Seq[hydra.lisp.syntax.TypeHint], body: Seq[hydra.lisp.syntax.Expression])

case class VariableDefinition(name: hydra.lisp.syntax.Symbol, value: hydra.lisp.syntax.Expression,
   doc: Option[hydra.lisp.syntax.Docstring])

case class ConstantDefinition(name: hydra.lisp.syntax.Symbol, value: hydra.lisp.syntax.Expression,
   doc: Option[hydra.lisp.syntax.Docstring])

case class RecordTypeDefinition(name: hydra.lisp.syntax.Symbol, fields: Seq[hydra.lisp.syntax.FieldDefinition],
   doc: Option[hydra.lisp.syntax.Docstring])

case class FieldDefinition(name: hydra.lisp.syntax.Symbol, defaultValue: Option[hydra.lisp.syntax.Expression])

case class MacroDefinition(name: hydra.lisp.syntax.Symbol, params: Seq[hydra.lisp.syntax.Symbol],
   restParam: Option[hydra.lisp.syntax.Symbol], body: Seq[hydra.lisp.syntax.Expression])

enum Expression :
   case application(value: hydra.lisp.syntax.Application) extends Expression
   case lambda(value: hydra.lisp.syntax.Lambda) extends Expression
   case let(value: hydra.lisp.syntax.LetExpression) extends Expression
   case `if`(value: hydra.lisp.syntax.IfExpression) extends Expression
   case cond(value: hydra.lisp.syntax.CondExpression) extends Expression
   case `case`(value: hydra.lisp.syntax.CaseExpression) extends Expression
   case and(value: hydra.lisp.syntax.AndExpression) extends Expression
   case or(value: hydra.lisp.syntax.OrExpression) extends Expression
   case not(value: hydra.lisp.syntax.NotExpression) extends Expression
   case `do`(value: hydra.lisp.syntax.DoExpression) extends Expression
   case begin(value: hydra.lisp.syntax.BeginExpression) extends Expression
   case variable(value: hydra.lisp.syntax.VariableReference) extends Expression
   case literal(value: hydra.lisp.syntax.Literal) extends Expression
   case list(value: hydra.lisp.syntax.ListLiteral) extends Expression
   case vector(value: hydra.lisp.syntax.VectorLiteral) extends Expression
   case map(value: hydra.lisp.syntax.MapLiteral) extends Expression
   case set(value: hydra.lisp.syntax.SetLiteral) extends Expression
   case cons(value: hydra.lisp.syntax.ConsExpression) extends Expression
   case dottedPair(value: hydra.lisp.syntax.DottedPair) extends Expression
   case fieldAccess(value: hydra.lisp.syntax.FieldAccess) extends Expression
   case typeAnnotation(value: hydra.lisp.syntax.TypeAnnotation) extends Expression
   case quote(value: hydra.lisp.syntax.QuoteExpression) extends Expression
   case quasiquote(value: hydra.lisp.syntax.QuasiquoteExpression) extends Expression
   case unquote(value: hydra.lisp.syntax.UnquoteExpression) extends Expression
   case splicingUnquote(value: hydra.lisp.syntax.SplicingUnquoteExpression) extends Expression
   case sExpression(value: hydra.lisp.syntax.SExpression) extends Expression

case class Application(function: hydra.lisp.syntax.Expression, arguments: Seq[hydra.lisp.syntax.Expression])

case class Lambda(name: Option[hydra.lisp.syntax.Symbol], params: Seq[hydra.lisp.syntax.Symbol],
   restParam: Option[hydra.lisp.syntax.Symbol], body: Seq[hydra.lisp.syntax.Expression])

case class VariableReference(name: hydra.lisp.syntax.Symbol, functionNamespace: Boolean)

case class FieldAccess(recordType: hydra.lisp.syntax.Symbol, field: hydra.lisp.syntax.Symbol,
   target: hydra.lisp.syntax.Expression)

case class TypeAnnotation(expression: hydra.lisp.syntax.Expression, `type`: hydra.lisp.syntax.TypeSpecifier)

case class IfExpression(condition: hydra.lisp.syntax.Expression, `then`: hydra.lisp.syntax.Expression,
   `else`: Option[hydra.lisp.syntax.Expression])

case class CondExpression(clauses: Seq[hydra.lisp.syntax.CondClause], default: Option[hydra.lisp.syntax.Expression])

case class CondClause(condition: hydra.lisp.syntax.Expression, body: hydra.lisp.syntax.Expression)

case class CaseExpression(scrutinee: hydra.lisp.syntax.Expression, clauses: Seq[hydra.lisp.syntax.CaseClause],
   default: Option[hydra.lisp.syntax.Expression])

case class CaseClause(keys: Seq[hydra.lisp.syntax.Expression], body: hydra.lisp.syntax.Expression)

case class AndExpression(expressions: Seq[hydra.lisp.syntax.Expression])

case class OrExpression(expressions: Seq[hydra.lisp.syntax.Expression])

case class NotExpression(expression: hydra.lisp.syntax.Expression)

case class DoExpression(expressions: Seq[hydra.lisp.syntax.Expression])

case class BeginExpression(expressions: Seq[hydra.lisp.syntax.Expression])

case class QuoteExpression(body: hydra.lisp.syntax.Expression)

case class QuasiquoteExpression(body: hydra.lisp.syntax.Expression)

case class UnquoteExpression(body: hydra.lisp.syntax.Expression)

case class SplicingUnquoteExpression(body: hydra.lisp.syntax.Expression)

case class LetExpression(kind: hydra.lisp.syntax.LetKind, bindings: Seq[hydra.lisp.syntax.LetBinding],
   body: Seq[hydra.lisp.syntax.Expression])

enum LetKind :
   case parallel extends LetKind
   case sequential extends LetKind
   case recursive extends LetKind

enum LetBinding :
   case simple(value: hydra.lisp.syntax.SimpleBinding) extends LetBinding
   case destructuring(value: hydra.lisp.syntax.DestructuringBinding) extends LetBinding

case class SimpleBinding(name: hydra.lisp.syntax.Symbol, value: hydra.lisp.syntax.Expression)

case class DestructuringBinding(pattern: hydra.lisp.syntax.DestructuringPattern, value: hydra.lisp.syntax.Expression)

enum DestructuringPattern :
   case sequential(value: Seq[hydra.lisp.syntax.Symbol]) extends DestructuringPattern
   case associative(value: Seq[hydra.lisp.syntax.Symbol]) extends DestructuringPattern
   case rest(value: Seq[hydra.lisp.syntax.Symbol]) extends DestructuringPattern

enum Pattern :
   case constructor(value: hydra.lisp.syntax.ConstructorPattern) extends Pattern
   case literal(value: hydra.lisp.syntax.LiteralPattern) extends Pattern
   case wildcard(value: hydra.lisp.syntax.WildcardPattern) extends Pattern
   case variable(value: hydra.lisp.syntax.Symbol) extends Pattern

case class ConstructorPattern(constructor: hydra.lisp.syntax.Symbol, arguments: Seq[hydra.lisp.syntax.Pattern])

case class LiteralPattern(value: hydra.lisp.syntax.Literal)

case class WildcardPattern()

enum Literal :
   case integer(value: hydra.lisp.syntax.IntegerLiteral) extends Literal
   case float(value: hydra.lisp.syntax.FloatLiteral) extends Literal
   case string(value: scala.Predef.String) extends Literal
   case character(value: hydra.lisp.syntax.CharacterLiteral) extends Literal
   case boolean(value: Boolean) extends Literal
   case nil extends Literal
   case keyword(value: hydra.lisp.syntax.Keyword) extends Literal
   case symbol(value: hydra.lisp.syntax.Symbol) extends Literal

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

case class ListLiteral(elements: Seq[hydra.lisp.syntax.Expression], quoted: Boolean)

case class VectorLiteral(elements: Seq[hydra.lisp.syntax.Expression])

case class MapLiteral(entries: Seq[hydra.lisp.syntax.MapEntry])

case class MapEntry(key: hydra.lisp.syntax.Expression, value: hydra.lisp.syntax.Expression)

case class SetLiteral(elements: Seq[hydra.lisp.syntax.Expression])

case class ConsExpression(head: hydra.lisp.syntax.Expression, tail: hydra.lisp.syntax.Expression)

case class DottedPair(car: hydra.lisp.syntax.Expression, cdr: hydra.lisp.syntax.Expression)

case class TypeHint(name: hydra.lisp.syntax.Symbol, `type`: hydra.lisp.syntax.TypeSpecifier)

enum TypeSpecifier :
   case named(value: hydra.lisp.syntax.Symbol) extends TypeSpecifier
   case list(value: hydra.lisp.syntax.TypeSpecifier) extends TypeSpecifier
   case function(value: Seq[hydra.lisp.syntax.TypeSpecifier]) extends TypeSpecifier
   case maybe(value: hydra.lisp.syntax.TypeSpecifier) extends TypeSpecifier
   case map(value: Seq[hydra.lisp.syntax.TypeSpecifier]) extends TypeSpecifier
   case set(value: hydra.lisp.syntax.TypeSpecifier) extends TypeSpecifier
   case pair(value: Seq[hydra.lisp.syntax.TypeSpecifier]) extends TypeSpecifier
   case either(value: Seq[hydra.lisp.syntax.TypeSpecifier]) extends TypeSpecifier
   case unit extends TypeSpecifier

case class ModuleDeclaration(name: hydra.lisp.syntax.NamespaceName, doc: Option[hydra.lisp.syntax.Docstring])

case class ImportDeclaration(module: hydra.lisp.syntax.NamespaceName, spec: hydra.lisp.syntax.ImportSpec)

enum ImportSpec :
   case all extends ImportSpec
   case alias(value: hydra.lisp.syntax.Symbol) extends ImportSpec
   case only(value: Seq[hydra.lisp.syntax.Symbol]) extends ImportSpec
   case rename(value: Seq[Seq[hydra.lisp.syntax.Symbol]]) extends ImportSpec

case class ExportDeclaration(symbols: Seq[hydra.lisp.syntax.Symbol])

case class Comment(style: hydra.lisp.syntax.CommentStyle, text: scala.Predef.String)

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
   case list(value: Seq[hydra.lisp.syntax.SExpression]) extends SExpression
