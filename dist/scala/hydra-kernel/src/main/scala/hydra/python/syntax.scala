package hydra.python.syntax

import hydra.core.*

case class AnnotatedStatement(comment: scala.Predef.String, statement: hydra.python.syntax.Statement)

type Module = Seq[Seq[hydra.python.syntax.Statement]]

enum QuoteStyle :
   case single extends QuoteStyle
   case double extends QuoteStyle
   case triple extends QuoteStyle

type Name = scala.Predef.String

enum Number :
   case integer(value: BigInt) extends Number
   case float(value: BigDecimal) extends Number

case class String(value: scala.Predef.String, quoteStyle: hydra.python.syntax.QuoteStyle)

type TypeComment = scala.Predef.String

type File = Seq[hydra.python.syntax.Statement]

type Interactive = hydra.python.syntax.Statement

type Eval = Seq[hydra.python.syntax.Expression]

case class FuncType(`type`: Seq[hydra.python.syntax.TypeExpression], body: hydra.python.syntax.Expression)

enum Statement :
   case compound(value: hydra.python.syntax.CompoundStatement) extends Statement
   case simple(value: Seq[hydra.python.syntax.SimpleStatement]) extends Statement
   case annotated(value: hydra.python.syntax.AnnotatedStatement) extends Statement

enum SimpleStatement :
   case assignment(value: hydra.python.syntax.Assignment) extends SimpleStatement
   case typeAlias(value: hydra.python.syntax.TypeAlias) extends SimpleStatement
   case starExpressions(value: Seq[hydra.python.syntax.StarExpression]) extends SimpleStatement
   case `return`(value: hydra.python.syntax.ReturnStatement) extends SimpleStatement
   case `import`(value: hydra.python.syntax.ImportStatement) extends SimpleStatement
   case raise(value: hydra.python.syntax.RaiseStatement) extends SimpleStatement
   case pass extends SimpleStatement
   case del(value: hydra.python.syntax.DelStatement) extends SimpleStatement
   case `yield`(value: hydra.python.syntax.YieldStatement) extends SimpleStatement
   case assert(value: hydra.python.syntax.AssertStatement) extends SimpleStatement
   case break extends SimpleStatement
   case continue extends SimpleStatement
   case global(value: Seq[hydra.python.syntax.Name]) extends SimpleStatement
   case nonlocal(value: Seq[hydra.python.syntax.Name]) extends SimpleStatement

enum CompoundStatement :
   case function(value: hydra.python.syntax.FunctionDefinition) extends CompoundStatement
   case `if`(value: hydra.python.syntax.IfStatement) extends CompoundStatement
   case classDef(value: hydra.python.syntax.ClassDefinition) extends CompoundStatement
   case `with`(value: hydra.python.syntax.WithStatement) extends CompoundStatement
   case `for`(value: hydra.python.syntax.ForStatement) extends CompoundStatement
   case `try`(value: hydra.python.syntax.TryStatement) extends CompoundStatement
   case `while`(value: hydra.python.syntax.WhileStatement) extends CompoundStatement
   case `match`(value: hydra.python.syntax.MatchStatement) extends CompoundStatement

enum Assignment :
   case typed(value: hydra.python.syntax.TypedAssignment) extends Assignment
   case untyped(value: hydra.python.syntax.UntypedAssignment) extends Assignment
   case aug(value: hydra.python.syntax.AugAssignment) extends Assignment

case class TypedAssignment(lhs: hydra.python.syntax.SingleTarget, `type`: hydra.python.syntax.Expression,
   rhs: Option[hydra.python.syntax.AnnotatedRhs])

case class UntypedAssignment(targets: Seq[hydra.python.syntax.StarTarget], rhs: hydra.python.syntax.AnnotatedRhs,
   typeComment: Option[hydra.python.syntax.TypeComment])

case class AugAssignment(lhs: hydra.python.syntax.SingleTarget, augassign: hydra.python.syntax.AugAssign,
   rhs: hydra.python.syntax.AnnotatedRhs)

enum AnnotatedRhs :
   case `yield`(value: hydra.python.syntax.YieldExpression) extends AnnotatedRhs
   case star(value: Seq[hydra.python.syntax.StarExpression]) extends AnnotatedRhs

enum AugAssign :
   case plusEqual extends AugAssign
   case minusEqual extends AugAssign
   case timesEqual extends AugAssign
   case atEqual extends AugAssign
   case slashEqual extends AugAssign
   case percentEqual extends AugAssign
   case ampersandEqual extends AugAssign
   case barEqual extends AugAssign
   case caretEqual extends AugAssign
   case leftShiftEqual extends AugAssign
   case rightShiftEqual extends AugAssign
   case starStarEqual extends AugAssign
   case doubleSlashEqual extends AugAssign

type ReturnStatement = Seq[hydra.python.syntax.StarExpression]

type RaiseStatement = Option[hydra.python.syntax.RaiseExpression]

case class RaiseExpression(expression: hydra.python.syntax.Expression, from: Option[hydra.python.syntax.Expression])

type DelStatement = hydra.python.syntax.DelTargets

type YieldStatement = hydra.python.syntax.YieldExpression

case class AssertStatement(expression1: hydra.python.syntax.Expression, expression2: Option[hydra.python.syntax.Expression])

enum ImportStatement :
   case name(value: hydra.python.syntax.ImportName) extends ImportStatement
   case from(value: hydra.python.syntax.ImportFrom) extends ImportStatement

type ImportName = Seq[hydra.python.syntax.DottedAsName]

case class ImportFrom(prefixes: Seq[hydra.python.syntax.RelativeImportPrefix], dottedName: Option[hydra.python.syntax.DottedName],
   targets: hydra.python.syntax.ImportFromTargets)

enum RelativeImportPrefix :
   case dot extends RelativeImportPrefix
   case ellipsis extends RelativeImportPrefix

enum ImportFromTargets :
   case simple(value: Seq[hydra.python.syntax.ImportFromAsName]) extends ImportFromTargets
   case parens(value: Seq[hydra.python.syntax.ImportFromAsName]) extends ImportFromTargets
   case star extends ImportFromTargets

case class ImportFromAsName(name: hydra.python.syntax.Name, as: Option[hydra.python.syntax.Name])

case class DottedAsName(name: hydra.python.syntax.DottedName, as: Option[hydra.python.syntax.Name])

type DottedName = Seq[hydra.python.syntax.Name]

enum Block :
   case indented(value: Seq[Seq[hydra.python.syntax.Statement]]) extends Block
   case simple(value: Seq[hydra.python.syntax.SimpleStatement]) extends Block

type Decorators = Seq[hydra.python.syntax.NamedExpression]

case class ClassDefinition(decorators: Option[hydra.python.syntax.Decorators], name: hydra.python.syntax.Name,
   typeParams: Seq[hydra.python.syntax.TypeParameter], arguments: Option[hydra.python.syntax.Args],
   body: hydra.python.syntax.Block)

case class FunctionDefinition(decorators: Option[hydra.python.syntax.Decorators],
   raw: hydra.python.syntax.FunctionDefRaw)

case class FunctionDefRaw(async: Boolean, name: hydra.python.syntax.Name, typeParams: Seq[hydra.python.syntax.TypeParameter],
   params: Option[hydra.python.syntax.Parameters], returnType: Option[hydra.python.syntax.Expression],
   funcTypeComment: Option[hydra.python.syntax.FuncTypeComment], block: hydra.python.syntax.Block)

enum Parameters :
   case slashNoDefault(value: hydra.python.syntax.SlashNoDefaultParameters) extends Parameters
   case slashWithDefault(value: hydra.python.syntax.SlashWithDefaultParameters) extends Parameters
   case paramNoDefault(value: hydra.python.syntax.ParamNoDefaultParameters) extends Parameters
   case paramWithDefault(value: hydra.python.syntax.ParamWithDefaultParameters) extends Parameters
   case starEtc(value: hydra.python.syntax.StarEtc) extends Parameters

case class SlashNoDefaultParameters(slash: hydra.python.syntax.SlashNoDefault, paramNoDefault: Seq[hydra.python.syntax.ParamNoDefault],
   paramWithDefault: Seq[hydra.python.syntax.ParamWithDefault], starEtc: Option[hydra.python.syntax.StarEtc])

case class SlashWithDefaultParameters(paramNoDefault: Seq[hydra.python.syntax.ParamNoDefault],
   paramWithDefault: Seq[hydra.python.syntax.ParamWithDefault], starEtc: Option[hydra.python.syntax.StarEtc])

case class ParamNoDefaultParameters(paramNoDefault: Seq[hydra.python.syntax.ParamNoDefault],
   paramWithDefault: Seq[hydra.python.syntax.ParamWithDefault], starEtc: Option[hydra.python.syntax.StarEtc])

case class ParamWithDefaultParameters(paramWithDefault: Seq[hydra.python.syntax.ParamWithDefault],
   starEtc: Option[hydra.python.syntax.StarEtc])

type SlashNoDefault = Seq[hydra.python.syntax.ParamNoDefault]

case class SlashWithDefault(paramNoDefault: Seq[hydra.python.syntax.ParamNoDefault],
   paramWithDefault: Seq[hydra.python.syntax.ParamWithDefault])

enum StarEtc :
   case starNoDefault(value: hydra.python.syntax.NoDefaultStarEtc) extends StarEtc
   case starNoDefaultStarAnnotation(value: hydra.python.syntax.NoDefaultStarAnnotationStarEtc) extends StarEtc
   case starComma(value: hydra.python.syntax.CommaStarEtc) extends StarEtc
   case keywords(value: hydra.python.syntax.Keywords) extends StarEtc

case class NoDefaultStarEtc(paramNoDefault: hydra.python.syntax.ParamNoDefault, paramMaybeDefault: Seq[hydra.python.syntax.ParamMaybeDefault],
   keywords: Option[hydra.python.syntax.Keywords])

case class NoDefaultStarAnnotationStarEtc(paramNoDefaultStarAnnotation: hydra.python.syntax.ParamNoDefaultStarAnnotation,
   paramMaybeDefault: Seq[hydra.python.syntax.ParamMaybeDefault], keywords: Option[hydra.python.syntax.Keywords])

case class CommaStarEtc(paramMaybeDefault: Seq[hydra.python.syntax.ParamMaybeDefault],
   keywords: Option[hydra.python.syntax.Keywords])

type Keywords = hydra.python.syntax.ParamNoDefault

case class ParamNoDefault(param: hydra.python.syntax.Param, typeComment: Option[hydra.python.syntax.TypeComment])

case class ParamNoDefaultStarAnnotation(paramStarAnnotation: hydra.python.syntax.ParamStarAnnotation,
   typeComment: Option[hydra.python.syntax.TypeComment])

case class ParamWithDefault(param: hydra.python.syntax.Param, default: hydra.python.syntax.Default,
   typeComment: Option[hydra.python.syntax.TypeComment])

case class ParamMaybeDefault(param: hydra.python.syntax.Param, default: Option[hydra.python.syntax.Default],
   typeComment: Option[hydra.python.syntax.TypeComment])

case class Param(name: hydra.python.syntax.Name, annotation: Option[hydra.python.syntax.Annotation])

case class ParamStarAnnotation(name: hydra.python.syntax.Name, annotation: hydra.python.syntax.StarAnnotation)

type Annotation = hydra.python.syntax.Expression

type StarAnnotation = hydra.python.syntax.StarExpression

type Default = hydra.python.syntax.Expression

case class IfStatement(condition: hydra.python.syntax.NamedExpression, body: hydra.python.syntax.Block,
   continuation: Option[hydra.python.syntax.IfTail])

enum IfTail :
   case elif(value: hydra.python.syntax.IfStatement) extends IfTail
   case `else`(value: hydra.python.syntax.Block) extends IfTail

case class WhileStatement(condition: hydra.python.syntax.NamedExpression, body: hydra.python.syntax.Block,
   `else`: Option[hydra.python.syntax.Block])

case class ForStatement(async: Boolean, targets: Seq[hydra.python.syntax.StarTarget],
   expressions: Seq[hydra.python.syntax.StarExpression], typeComment: Option[hydra.python.syntax.TypeComment],
   body: hydra.python.syntax.Block, `else`: Option[hydra.python.syntax.Block])

case class WithStatement(async: Boolean, items: Seq[hydra.python.syntax.WithItem],
   typeComment: Option[hydra.python.syntax.TypeComment], body: hydra.python.syntax.Block)

case class WithItem(expression: hydra.python.syntax.Expression, as: Option[hydra.python.syntax.StarTarget])

enum TryStatement :
   case `finally`(value: hydra.python.syntax.TryFinallyStatement) extends TryStatement
   case except(value: hydra.python.syntax.TryExceptStatement) extends TryStatement
   case exceptStar(value: hydra.python.syntax.TryExceptStarStatement) extends TryStatement

case class TryFinallyStatement(body: hydra.python.syntax.Block, `finally`: hydra.python.syntax.Block)

case class TryExceptStatement(body: hydra.python.syntax.Block, excepts: Seq[hydra.python.syntax.ExceptBlock],
   `else`: Option[hydra.python.syntax.Block], `finally`: Option[hydra.python.syntax.Block])

case class TryExceptStarStatement(body: hydra.python.syntax.Block, excepts: Seq[hydra.python.syntax.ExceptStarBlock],
   `else`: Option[hydra.python.syntax.Block], `finally`: Option[hydra.python.syntax.Block])

case class ExceptBlock(expression: Option[hydra.python.syntax.ExceptExpression], body: hydra.python.syntax.Block)

case class ExceptExpression(expression: hydra.python.syntax.Expression, as: Option[hydra.python.syntax.Name])

case class ExceptStarBlock(expression: hydra.python.syntax.Expression, as: Option[hydra.python.syntax.Name],
   body: hydra.python.syntax.Block)

case class MatchStatement(subject: hydra.python.syntax.SubjectExpression, cases: Seq[hydra.python.syntax.CaseBlock])

enum SubjectExpression :
   case tuple(value: Seq[hydra.python.syntax.StarNamedExpression]) extends SubjectExpression
   case simple(value: hydra.python.syntax.NamedExpression) extends SubjectExpression

case class CaseBlock(patterns: hydra.python.syntax.Patterns, guard: Option[hydra.python.syntax.Guard],
   body: hydra.python.syntax.Block)

type Guard = hydra.python.syntax.NamedExpression

enum Patterns :
   case sequence(value: hydra.python.syntax.OpenSequencePattern) extends Patterns
   case pattern(value: hydra.python.syntax.Pattern) extends Patterns

enum Pattern :
   case as(value: hydra.python.syntax.AsPattern) extends Pattern
   case or(value: hydra.python.syntax.OrPattern) extends Pattern

case class AsPattern(pattern: hydra.python.syntax.OrPattern, as: hydra.python.syntax.PatternCaptureTarget)

type OrPattern = Seq[hydra.python.syntax.ClosedPattern]

enum ClosedPattern :
   case literal(value: hydra.python.syntax.LiteralExpression) extends ClosedPattern
   case capture(value: hydra.python.syntax.CapturePattern) extends ClosedPattern
   case wildcard extends ClosedPattern
   case value(value: hydra.python.syntax.ValuePattern) extends ClosedPattern
   case group(value: hydra.python.syntax.GroupPattern) extends ClosedPattern
   case sequence(value: hydra.python.syntax.SequencePattern) extends ClosedPattern
   case mapping(value: hydra.python.syntax.MappingPattern) extends ClosedPattern
   case `class`(value: hydra.python.syntax.ClassPattern) extends ClosedPattern

enum LiteralExpression :
   case number(value: hydra.python.syntax.SignedNumber) extends LiteralExpression
   case complex(value: hydra.python.syntax.ComplexNumber) extends LiteralExpression
   case string(value: scala.Predef.String) extends LiteralExpression
   case none extends LiteralExpression
   case `true` extends LiteralExpression
   case `false` extends LiteralExpression

case class ComplexNumber(real: hydra.python.syntax.SignedRealNumber, plusOrMinus: hydra.python.syntax.PlusOrMinus,
   imaginary: hydra.python.syntax.ImaginaryNumber)

enum PlusOrMinus :
   case plus extends PlusOrMinus
   case minus extends PlusOrMinus

enum SignedNumber :
   case sign(value: hydra.python.syntax.PlusOrMinus) extends SignedNumber
   case number(value: hydra.python.syntax.Number) extends SignedNumber

enum SignedRealNumber :
   case sign(value: hydra.python.syntax.PlusOrMinus) extends SignedRealNumber
   case number(value: hydra.python.syntax.RealNumber) extends SignedRealNumber

type RealNumber = hydra.python.syntax.Number

type ImaginaryNumber = hydra.python.syntax.Number

type CapturePattern = hydra.python.syntax.PatternCaptureTarget

type PatternCaptureTarget = hydra.python.syntax.Name

type ValuePattern = hydra.python.syntax.Attribute

type Attribute = Seq[hydra.python.syntax.Name]

type NameOrAttribute = Seq[hydra.python.syntax.Name]

type GroupPattern = hydra.python.syntax.Pattern

enum SequencePattern :
   case list(value: Option[hydra.python.syntax.MaybeSequencePattern]) extends SequencePattern
   case tuple(value: Option[hydra.python.syntax.OpenSequencePattern]) extends SequencePattern

case class OpenSequencePattern(head: hydra.python.syntax.MaybeStarPattern, tail: Option[hydra.python.syntax.MaybeSequencePattern])

type MaybeSequencePattern = Seq[hydra.python.syntax.MaybeStarPattern]

enum MaybeStarPattern :
   case star(value: hydra.python.syntax.StarPattern) extends MaybeStarPattern
   case pattern(value: hydra.python.syntax.Pattern) extends MaybeStarPattern

enum StarPattern :
   case capture(value: hydra.python.syntax.PatternCaptureTarget) extends StarPattern
   case wildcard extends StarPattern

case class MappingPattern(items: Option[hydra.python.syntax.ItemsPattern], doubleStar: Option[hydra.python.syntax.DoubleStarPattern])

type ItemsPattern = Seq[hydra.python.syntax.KeyValuePattern]

case class KeyValuePattern(key: hydra.python.syntax.LiteralExpressionOrAttribute, value: hydra.python.syntax.Pattern)

enum LiteralExpressionOrAttribute :
   case literal(value: hydra.python.syntax.LiteralExpression) extends LiteralExpressionOrAttribute
   case attribute(value: hydra.python.syntax.Attribute) extends LiteralExpressionOrAttribute

type DoubleStarPattern = hydra.python.syntax.PatternCaptureTarget

case class ClassPattern(nameOrAttribute: hydra.python.syntax.NameOrAttribute, positionalPatterns: Option[hydra.python.syntax.PositionalPatterns],
   keywordPatterns: Option[hydra.python.syntax.KeywordPatterns])

type PositionalPatterns = Seq[hydra.python.syntax.Pattern]

type KeywordPatterns = Seq[hydra.python.syntax.KeywordPattern]

case class KeywordPattern(name: hydra.python.syntax.Name, pattern: hydra.python.syntax.Pattern)

case class TypeAlias(name: hydra.python.syntax.Name, typeParams: Seq[hydra.python.syntax.TypeParameter],
   expression: hydra.python.syntax.Expression)

enum TypeParameter :
   case simple(value: hydra.python.syntax.SimpleTypeParameter) extends TypeParameter
   case star(value: hydra.python.syntax.StarTypeParameter) extends TypeParameter
   case doubleStar(value: hydra.python.syntax.DoubleStarTypeParameter) extends TypeParameter

case class SimpleTypeParameter(name: hydra.python.syntax.Name, bound: Option[hydra.python.syntax.Expression],
   default: Option[hydra.python.syntax.Expression])

case class StarTypeParameter(name: hydra.python.syntax.Name, default: Option[hydra.python.syntax.StarExpression])

case class DoubleStarTypeParameter(name: hydra.python.syntax.Name, default: Option[hydra.python.syntax.Expression])

enum Expression :
   case conditional(value: hydra.python.syntax.Conditional) extends Expression
   case simple(value: hydra.python.syntax.Disjunction) extends Expression
   case lambda(value: hydra.python.syntax.Lambda) extends Expression

case class Conditional(body: hydra.python.syntax.Disjunction, `if`: hydra.python.syntax.Disjunction,
   `else`: hydra.python.syntax.Expression)

enum YieldExpression :
   case from(value: hydra.python.syntax.Expression) extends YieldExpression
   case simple(value: Seq[hydra.python.syntax.StarExpression]) extends YieldExpression

enum StarExpression :
   case star(value: hydra.python.syntax.BitwiseOr) extends StarExpression
   case simple(value: hydra.python.syntax.Expression) extends StarExpression

type StarNamedExpressions = Seq[hydra.python.syntax.StarNamedExpression]

enum StarNamedExpression :
   case star(value: hydra.python.syntax.BitwiseOr) extends StarNamedExpression
   case simple(value: hydra.python.syntax.NamedExpression) extends StarNamedExpression

case class AssignmentExpression(name: hydra.python.syntax.Name, expression: hydra.python.syntax.Expression)

enum NamedExpression :
   case assignment(value: hydra.python.syntax.AssignmentExpression) extends NamedExpression
   case simple(value: hydra.python.syntax.Expression) extends NamedExpression

type Disjunction = Seq[hydra.python.syntax.Conjunction]

type Conjunction = Seq[hydra.python.syntax.Inversion]

enum Inversion :
   case not(value: hydra.python.syntax.Inversion) extends Inversion
   case simple(value: hydra.python.syntax.Comparison) extends Inversion

case class Comparison(lhs: hydra.python.syntax.BitwiseOr, rhs: Seq[hydra.python.syntax.CompareOpBitwiseOrPair])

case class CompareOpBitwiseOrPair(operator: hydra.python.syntax.CompareOp, rhs: hydra.python.syntax.BitwiseOr)

enum CompareOp :
   case eq extends CompareOp
   case noteq extends CompareOp
   case lte extends CompareOp
   case lt extends CompareOp
   case gte extends CompareOp
   case gt extends CompareOp
   case notin extends CompareOp
   case in extends CompareOp
   case isnot extends CompareOp
   case is extends CompareOp

case class BitwiseOr(lhs: Option[hydra.python.syntax.BitwiseOr], rhs: hydra.python.syntax.BitwiseXor)

case class BitwiseXor(lhs: Option[hydra.python.syntax.BitwiseXor], rhs: hydra.python.syntax.BitwiseAnd)

case class BitwiseAnd(lhs: Option[hydra.python.syntax.BitwiseAnd], rhs: hydra.python.syntax.ShiftExpression)

case class ShiftExpression(lhs: Option[hydra.python.syntax.ShiftLhs], rhs: hydra.python.syntax.Sum)

case class ShiftLhs(operand: hydra.python.syntax.ShiftExpression, operator: hydra.python.syntax.ShiftOp)

enum ShiftOp :
   case left extends ShiftOp
   case right extends ShiftOp

case class Sum(lhs: Option[hydra.python.syntax.SumLhs], rhs: hydra.python.syntax.Term)

case class SumLhs(operand: hydra.python.syntax.Sum, operator: hydra.python.syntax.SumOp)

enum SumOp :
   case add extends SumOp
   case sub extends SumOp

case class Term(lhs: Option[hydra.python.syntax.TermLhs], rhs: hydra.python.syntax.Factor)

case class TermLhs(operand: hydra.python.syntax.Term, operator: hydra.python.syntax.TermOp)

enum TermOp :
   case mul extends TermOp
   case div extends TermOp
   case floordiv extends TermOp
   case mod extends TermOp
   case matmul extends TermOp

enum Factor :
   case positive(value: hydra.python.syntax.Factor) extends Factor
   case negative(value: hydra.python.syntax.Factor) extends Factor
   case complement(value: hydra.python.syntax.Factor) extends Factor
   case simple(value: hydra.python.syntax.Power) extends Factor

case class Power(lhs: hydra.python.syntax.AwaitPrimary, rhs: Option[hydra.python.syntax.Factor])

case class AwaitPrimary(await: Boolean, primary: hydra.python.syntax.Primary)

enum Primary :
   case simple(value: hydra.python.syntax.Atom) extends Primary
   case compound(value: hydra.python.syntax.PrimaryWithRhs) extends Primary

case class PrimaryWithRhs(primary: hydra.python.syntax.Primary, rhs: hydra.python.syntax.PrimaryRhs)

enum PrimaryRhs :
   case project(value: hydra.python.syntax.Name) extends PrimaryRhs
   case genexp(value: hydra.python.syntax.Genexp) extends PrimaryRhs
   case call(value: hydra.python.syntax.Args) extends PrimaryRhs
   case slices(value: hydra.python.syntax.Slices) extends PrimaryRhs

case class Slices(head: hydra.python.syntax.Slice, tail: Seq[hydra.python.syntax.SliceOrStarredExpression])

enum SliceOrStarredExpression :
   case slice(value: hydra.python.syntax.Slice) extends SliceOrStarredExpression
   case starred(value: hydra.python.syntax.StarredExpression) extends SliceOrStarredExpression

enum Slice :
   case named(value: hydra.python.syntax.NamedExpression) extends Slice
   case `slice_`(value: hydra.python.syntax.SliceExpression) extends Slice

case class SliceExpression(start: Option[hydra.python.syntax.Expression], stop: Option[hydra.python.syntax.Expression],
   step: Option[hydra.python.syntax.Expression])

enum Atom :
   case name(value: hydra.python.syntax.Name) extends Atom
   case `true` extends Atom
   case `false` extends Atom
   case none extends Atom
   case string(value: hydra.python.syntax.String) extends Atom
   case number(value: hydra.python.syntax.Number) extends Atom
   case tuple(value: hydra.python.syntax.Tuple) extends Atom
   case group(value: hydra.python.syntax.Group) extends Atom
   case genexp(value: hydra.python.syntax.Genexp) extends Atom
   case list(value: hydra.python.syntax.List) extends Atom
   case listcomp(value: hydra.python.syntax.Listcomp) extends Atom
   case dict(value: hydra.python.syntax.Dict) extends Atom
   case set(value: hydra.python.syntax.Set) extends Atom
   case dictcomp(value: hydra.python.syntax.Dictcomp) extends Atom
   case setcomp(value: hydra.python.syntax.Setcomp) extends Atom
   case ellipsis extends Atom

enum Group :
   case `yield`(value: hydra.python.syntax.YieldExpression) extends Group
   case expression(value: hydra.python.syntax.NamedExpression) extends Group

case class Lambda(params: hydra.python.syntax.LambdaParameters, body: hydra.python.syntax.Expression)

case class LambdaParameters(slashNoDefault: Option[hydra.python.syntax.LambdaSlashNoDefault],
   paramNoDefault: Seq[hydra.python.syntax.LambdaParamNoDefault], paramWithDefault: Seq[hydra.python.syntax.LambdaParamWithDefault],
   starEtc: Option[hydra.python.syntax.LambdaStarEtc])

case class LambdaSlashNoDefault(parameters: Seq[hydra.python.syntax.LambdaParamNoDefault])

case class LambdaSlashWithDefault(paramNoDefault: Seq[hydra.python.syntax.LambdaParamNoDefault],
   paramWithDefault: Seq[hydra.python.syntax.LambdaParamWithDefault])

enum LambdaStarEtc :
   case star(value: Boolean) extends LambdaStarEtc
   case paramNoDefault(value: hydra.python.syntax.LambdaParamNoDefault) extends LambdaStarEtc
   case paramMaybeDefault(value: Seq[hydra.python.syntax.LambdaParamMaybeDefault]) extends LambdaStarEtc
   case kwds(value: hydra.python.syntax.LambdaKwds) extends LambdaStarEtc

type LambdaKwds = hydra.python.syntax.LambdaParamNoDefault

type LambdaParamNoDefault = hydra.python.syntax.Name

case class LambdaParamWithDefault(param: hydra.python.syntax.Name, default: Option[hydra.python.syntax.Default])

case class LambdaParamMaybeDefault(param: hydra.python.syntax.Name, default: Option[hydra.python.syntax.Default])

type List = Seq[hydra.python.syntax.StarNamedExpression]

type Tuple = Seq[hydra.python.syntax.StarNamedExpression]

type Set = Seq[hydra.python.syntax.StarNamedExpression]

type Dict = Seq[hydra.python.syntax.DoubleStarredKvpair]

enum DoubleStarredKvpair :
   case starred(value: hydra.python.syntax.BitwiseOr) extends DoubleStarredKvpair
   case pair(value: hydra.python.syntax.Kvpair) extends DoubleStarredKvpair

case class Kvpair(key: hydra.python.syntax.Expression, value: hydra.python.syntax.Expression)

type ForIfClauses = Seq[hydra.python.syntax.ForIfClause]

case class ForIfClause(async: Boolean, targets: Seq[hydra.python.syntax.StarTarget],
   in: hydra.python.syntax.Disjunction, ifs: Seq[hydra.python.syntax.Disjunction])

case class Listcomp(expression: hydra.python.syntax.NamedExpression, forIfClauses: hydra.python.syntax.ForIfClauses)

case class Setcomp(expression: hydra.python.syntax.NamedExpression, forIfClauses: hydra.python.syntax.ForIfClauses)

case class Genexp(head: hydra.python.syntax.GenexpHead, tail: hydra.python.syntax.ForIfClauses)

enum GenexpHead :
   case assignment(value: hydra.python.syntax.AssignmentExpression) extends GenexpHead
   case expression(value: hydra.python.syntax.Expression) extends GenexpHead

case class Dictcomp(kvpair: hydra.python.syntax.Kvpair, forIfClauses: hydra.python.syntax.ForIfClauses)

case class Args(positional: Seq[hydra.python.syntax.PosArg], kwargOrStarred: Seq[hydra.python.syntax.KwargOrStarred],
   kwargOrDoubleStarred: Seq[hydra.python.syntax.KwargOrDoubleStarred])

enum PosArg :
   case starred(value: hydra.python.syntax.StarredExpression) extends PosArg
   case assignment(value: hydra.python.syntax.AssignmentExpression) extends PosArg
   case expression(value: hydra.python.syntax.Expression) extends PosArg

type StarredExpression = hydra.python.syntax.Expression

enum KwargOrStarred :
   case kwarg(value: hydra.python.syntax.Kwarg) extends KwargOrStarred
   case starred(value: hydra.python.syntax.StarredExpression) extends KwargOrStarred

case class Kwarg(name: hydra.python.syntax.Name, value: hydra.python.syntax.Expression)

enum KwargOrDoubleStarred :
   case kwarg(value: hydra.python.syntax.Kwarg) extends KwargOrDoubleStarred
   case doubleStarred(value: hydra.python.syntax.Expression) extends KwargOrDoubleStarred

type StarTargetsListSeq = Seq[hydra.python.syntax.StarTarget]

type StarTargetsTupleSeq = Seq[hydra.python.syntax.StarTarget]

enum StarTarget :
   case starred(value: hydra.python.syntax.StarTarget) extends StarTarget
   case unstarred(value: hydra.python.syntax.TargetWithStarAtom) extends StarTarget

enum TargetWithStarAtom :
   case project(value: hydra.python.syntax.TPrimaryAndName) extends TargetWithStarAtom
   case slices(value: hydra.python.syntax.TPrimaryAndSlices) extends TargetWithStarAtom
   case atom(value: hydra.python.syntax.StarAtom) extends TargetWithStarAtom

case class TPrimaryAndName(primary: hydra.python.syntax.TPrimary, name: hydra.python.syntax.Name)

case class TPrimaryAndSlices(primary: hydra.python.syntax.TPrimary, slices: hydra.python.syntax.Slices)

enum StarAtom :
   case name(value: hydra.python.syntax.Name) extends StarAtom
   case targetWithStarAtom(value: hydra.python.syntax.TargetWithStarAtom) extends StarAtom
   case starTargetsTupleSeq(value: Option[hydra.python.syntax.StarTargetsTupleSeq]) extends StarAtom
   case starTargetsListSeq(value: Option[hydra.python.syntax.StarTargetsListSeq]) extends StarAtom

enum SingleTarget :
   case subscriptAttributeTarget(value: hydra.python.syntax.SingleSubscriptAttributeTarget) extends SingleTarget
   case name(value: hydra.python.syntax.Name) extends SingleTarget
   case parens(value: hydra.python.syntax.SingleTarget) extends SingleTarget

enum SingleSubscriptAttributeTarget :
   case primaryAndName(value: hydra.python.syntax.TPrimaryAndName) extends SingleSubscriptAttributeTarget
   case primaryAndSlices(value: hydra.python.syntax.TPrimaryAndSlices) extends SingleSubscriptAttributeTarget

enum TPrimary :
   case primaryAndName(value: hydra.python.syntax.TPrimaryAndName) extends TPrimary
   case primaryAndSlices(value: hydra.python.syntax.TPrimaryAndSlices) extends TPrimary
   case primaryAndGenexp(value: hydra.python.syntax.TPrimaryAndGenexp) extends TPrimary
   case primaryAndArguments(value: hydra.python.syntax.TPrimaryAndArguments) extends TPrimary
   case atom(value: hydra.python.syntax.Atom) extends TPrimary

case class TPrimaryAndGenexp(primary: hydra.python.syntax.TPrimary, genexp: hydra.python.syntax.Genexp)

case class TPrimaryAndArguments(primary: hydra.python.syntax.TPrimary, arguments: Option[hydra.python.syntax.Args])

type DelTargets = Seq[hydra.python.syntax.DelTarget]

enum DelTarget :
   case primaryAndName(value: hydra.python.syntax.TPrimaryAndName) extends DelTarget
   case primaryAndSlices(value: hydra.python.syntax.TPrimaryAndSlices) extends DelTarget
   case delTAtom(value: hydra.python.syntax.DelTAtom) extends DelTarget

enum DelTAtom :
   case name(value: hydra.python.syntax.Name) extends DelTAtom
   case target(value: hydra.python.syntax.DelTarget) extends DelTAtom
   case targets(value: hydra.python.syntax.DelTargets) extends DelTAtom

enum TypeExpression :
   case expression(value: hydra.python.syntax.Expression) extends TypeExpression
   case starredExpression(value: hydra.python.syntax.Expression) extends TypeExpression
   case doubleStarredExpression(value: hydra.python.syntax.Expression) extends TypeExpression

type FuncTypeComment = hydra.python.syntax.TypeComment
