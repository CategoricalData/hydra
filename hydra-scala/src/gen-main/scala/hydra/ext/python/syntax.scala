package hydra.ext.python.syntax

import hydra.core.*

case class AnnotatedStatement(comment: scala.Predef.String, statement: hydra.ext.python.syntax.Statement)

type Module = Seq[Seq[hydra.ext.python.syntax.Statement]]

enum QuoteStyle :
   case single extends QuoteStyle
   case double extends QuoteStyle
   case triple extends QuoteStyle

type Name = scala.Predef.String

enum Number :
   case integer(value: BigInt) extends Number
   case float(value: BigDecimal) extends Number

case class String(value: scala.Predef.String, quoteStyle: hydra.ext.python.syntax.QuoteStyle)

type TypeComment = scala.Predef.String

type File = Seq[hydra.ext.python.syntax.Statement]

type Interactive = hydra.ext.python.syntax.Statement

type Eval = Seq[hydra.ext.python.syntax.Expression]

case class FuncType(`type`: Seq[hydra.ext.python.syntax.TypeExpression], body: hydra.ext.python.syntax.Expression)

enum Statement :
   case compound(value: hydra.ext.python.syntax.CompoundStatement) extends Statement
   case simple(value: Seq[hydra.ext.python.syntax.SimpleStatement]) extends Statement
   case annotated(value: hydra.ext.python.syntax.AnnotatedStatement) extends Statement

enum SimpleStatement :
   case assignment(value: hydra.ext.python.syntax.Assignment) extends SimpleStatement
   case typeAlias(value: hydra.ext.python.syntax.TypeAlias) extends SimpleStatement
   case starExpressions(value: Seq[hydra.ext.python.syntax.StarExpression]) extends SimpleStatement
   case `return`(value: hydra.ext.python.syntax.ReturnStatement) extends SimpleStatement
   case `import`(value: hydra.ext.python.syntax.ImportStatement) extends SimpleStatement
   case raise(value: hydra.ext.python.syntax.RaiseStatement) extends SimpleStatement
   case pass extends SimpleStatement
   case del(value: hydra.ext.python.syntax.DelStatement) extends SimpleStatement
   case `yield`(value: hydra.ext.python.syntax.YieldStatement) extends SimpleStatement
   case assert(value: hydra.ext.python.syntax.AssertStatement) extends SimpleStatement
   case break extends SimpleStatement
   case continue extends SimpleStatement
   case global(value: Seq[hydra.ext.python.syntax.Name]) extends SimpleStatement
   case nonlocal(value: Seq[hydra.ext.python.syntax.Name]) extends SimpleStatement

enum CompoundStatement :
   case function(value: hydra.ext.python.syntax.FunctionDefinition) extends CompoundStatement
   case `if`(value: hydra.ext.python.syntax.IfStatement) extends CompoundStatement
   case classDef(value: hydra.ext.python.syntax.ClassDefinition) extends CompoundStatement
   case `with`(value: hydra.ext.python.syntax.WithStatement) extends CompoundStatement
   case `for`(value: hydra.ext.python.syntax.ForStatement) extends CompoundStatement
   case `try`(value: hydra.ext.python.syntax.TryStatement) extends CompoundStatement
   case `while`(value: hydra.ext.python.syntax.WhileStatement) extends CompoundStatement
   case `match`(value: hydra.ext.python.syntax.MatchStatement) extends CompoundStatement

enum Assignment :
   case typed(value: hydra.ext.python.syntax.TypedAssignment) extends Assignment
   case untyped(value: hydra.ext.python.syntax.UntypedAssignment) extends Assignment
   case aug(value: hydra.ext.python.syntax.AugAssignment) extends Assignment

case class TypedAssignment(lhs: hydra.ext.python.syntax.SingleTarget, `type`: hydra.ext.python.syntax.Expression, rhs: Option[hydra.ext.python.syntax.AnnotatedRhs])

case class UntypedAssignment(targets: Seq[hydra.ext.python.syntax.StarTarget], rhs: hydra.ext.python.syntax.AnnotatedRhs, typeComment: Option[hydra.ext.python.syntax.TypeComment])

case class AugAssignment(lhs: hydra.ext.python.syntax.SingleTarget, augassign: hydra.ext.python.syntax.AugAssign, rhs: hydra.ext.python.syntax.AnnotatedRhs)

enum AnnotatedRhs :
   case `yield`(value: hydra.ext.python.syntax.YieldExpression) extends AnnotatedRhs
   case star(value: Seq[hydra.ext.python.syntax.StarExpression]) extends AnnotatedRhs

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

type ReturnStatement = Seq[hydra.ext.python.syntax.StarExpression]

type RaiseStatement = Option[hydra.ext.python.syntax.RaiseExpression]

case class RaiseExpression(expression: hydra.ext.python.syntax.Expression, from: Option[hydra.ext.python.syntax.Expression])

type DelStatement = hydra.ext.python.syntax.DelTargets

type YieldStatement = hydra.ext.python.syntax.YieldExpression

case class AssertStatement(expression1: hydra.ext.python.syntax.Expression, expression2: Option[hydra.ext.python.syntax.Expression])

enum ImportStatement :
   case name(value: hydra.ext.python.syntax.ImportName) extends ImportStatement
   case from(value: hydra.ext.python.syntax.ImportFrom) extends ImportStatement

type ImportName = Seq[hydra.ext.python.syntax.DottedAsName]

case class ImportFrom(prefixes: Seq[hydra.ext.python.syntax.RelativeImportPrefix], dottedName: Option[hydra.ext.python.syntax.DottedName], targets: hydra.ext.python.syntax.ImportFromTargets)

enum RelativeImportPrefix :
   case dot extends RelativeImportPrefix
   case ellipsis extends RelativeImportPrefix

enum ImportFromTargets :
   case simple(value: Seq[hydra.ext.python.syntax.ImportFromAsName]) extends ImportFromTargets
   case parens(value: Seq[hydra.ext.python.syntax.ImportFromAsName]) extends ImportFromTargets
   case star extends ImportFromTargets

case class ImportFromAsName(name: hydra.ext.python.syntax.Name, as: Option[hydra.ext.python.syntax.Name])

case class DottedAsName(name: hydra.ext.python.syntax.DottedName, as: Option[hydra.ext.python.syntax.Name])

type DottedName = Seq[hydra.ext.python.syntax.Name]

enum Block :
   case indented(value: Seq[Seq[hydra.ext.python.syntax.Statement]]) extends Block
   case simple(value: Seq[hydra.ext.python.syntax.SimpleStatement]) extends Block

type Decorators = Seq[hydra.ext.python.syntax.NamedExpression]

case class ClassDefinition(decorators: Option[hydra.ext.python.syntax.Decorators], name: hydra.ext.python.syntax.Name, typeParams: Seq[hydra.ext.python.syntax.TypeParameter], arguments: Option[hydra.ext.python.syntax.Args], body: hydra.ext.python.syntax.Block)

case class FunctionDefinition(decorators: Option[hydra.ext.python.syntax.Decorators], raw: hydra.ext.python.syntax.FunctionDefRaw)

case class FunctionDefRaw(async: Boolean, name: hydra.ext.python.syntax.Name, typeParams: Seq[hydra.ext.python.syntax.TypeParameter], params: Option[hydra.ext.python.syntax.Parameters], returnType: Option[hydra.ext.python.syntax.Expression], funcTypeComment: Option[hydra.ext.python.syntax.FuncTypeComment], block: hydra.ext.python.syntax.Block)

enum Parameters :
   case slashNoDefault(value: hydra.ext.python.syntax.SlashNoDefaultParameters) extends Parameters
   case slashWithDefault(value: hydra.ext.python.syntax.SlashWithDefaultParameters) extends Parameters
   case paramNoDefault(value: hydra.ext.python.syntax.ParamNoDefaultParameters) extends Parameters
   case paramWithDefault(value: hydra.ext.python.syntax.ParamWithDefaultParameters) extends Parameters
   case starEtc(value: hydra.ext.python.syntax.StarEtc) extends Parameters

case class SlashNoDefaultParameters(slash: hydra.ext.python.syntax.SlashNoDefault, paramNoDefault: Seq[hydra.ext.python.syntax.ParamNoDefault], paramWithDefault: Seq[hydra.ext.python.syntax.ParamWithDefault], starEtc: Option[hydra.ext.python.syntax.StarEtc])

case class SlashWithDefaultParameters(paramNoDefault: Seq[hydra.ext.python.syntax.ParamNoDefault], paramWithDefault: Seq[hydra.ext.python.syntax.ParamWithDefault], starEtc: Option[hydra.ext.python.syntax.StarEtc])

case class ParamNoDefaultParameters(paramNoDefault: Seq[hydra.ext.python.syntax.ParamNoDefault], paramWithDefault: Seq[hydra.ext.python.syntax.ParamWithDefault], starEtc: Option[hydra.ext.python.syntax.StarEtc])

case class ParamWithDefaultParameters(paramWithDefault: Seq[hydra.ext.python.syntax.ParamWithDefault], starEtc: Option[hydra.ext.python.syntax.StarEtc])

type SlashNoDefault = Seq[hydra.ext.python.syntax.ParamNoDefault]

case class SlashWithDefault(paramNoDefault: Seq[hydra.ext.python.syntax.ParamNoDefault], paramWithDefault: Seq[hydra.ext.python.syntax.ParamWithDefault])

enum StarEtc :
   case starNoDefault(value: hydra.ext.python.syntax.NoDefaultStarEtc) extends StarEtc
   case starNoDefaultStarAnnotation(value: hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc) extends StarEtc
   case starComma(value: hydra.ext.python.syntax.CommaStarEtc) extends StarEtc
   case keywords(value: hydra.ext.python.syntax.Keywords) extends StarEtc

case class NoDefaultStarEtc(paramNoDefault: hydra.ext.python.syntax.ParamNoDefault, paramMaybeDefault: Seq[hydra.ext.python.syntax.ParamMaybeDefault], keywords: Option[hydra.ext.python.syntax.Keywords])

case class NoDefaultStarAnnotationStarEtc(paramNoDefaultStarAnnotation: hydra.ext.python.syntax.ParamNoDefaultStarAnnotation, paramMaybeDefault: Seq[hydra.ext.python.syntax.ParamMaybeDefault], keywords: Option[hydra.ext.python.syntax.Keywords])

case class CommaStarEtc(paramMaybeDefault: Seq[hydra.ext.python.syntax.ParamMaybeDefault], keywords: Option[hydra.ext.python.syntax.Keywords])

type Keywords = hydra.ext.python.syntax.ParamNoDefault

case class ParamNoDefault(param: hydra.ext.python.syntax.Param, typeComment: Option[hydra.ext.python.syntax.TypeComment])

case class ParamNoDefaultStarAnnotation(paramStarAnnotation: hydra.ext.python.syntax.ParamStarAnnotation, typeComment: Option[hydra.ext.python.syntax.TypeComment])

case class ParamWithDefault(param: hydra.ext.python.syntax.Param, default: hydra.ext.python.syntax.Default, typeComment: Option[hydra.ext.python.syntax.TypeComment])

case class ParamMaybeDefault(param: hydra.ext.python.syntax.Param, default: Option[hydra.ext.python.syntax.Default], typeComment: Option[hydra.ext.python.syntax.TypeComment])

case class Param(name: hydra.ext.python.syntax.Name, annotation: Option[hydra.ext.python.syntax.Annotation])

case class ParamStarAnnotation(name: hydra.ext.python.syntax.Name, annotation: hydra.ext.python.syntax.StarAnnotation)

type Annotation = hydra.ext.python.syntax.Expression

type StarAnnotation = hydra.ext.python.syntax.StarExpression

type Default = hydra.ext.python.syntax.Expression

case class IfStatement(condition: hydra.ext.python.syntax.NamedExpression, body: hydra.ext.python.syntax.Block, continuation: Option[hydra.ext.python.syntax.IfTail])

enum IfTail :
   case elif(value: hydra.ext.python.syntax.IfStatement) extends IfTail
   case `else`(value: hydra.ext.python.syntax.Block) extends IfTail

case class WhileStatement(condition: hydra.ext.python.syntax.NamedExpression, body: hydra.ext.python.syntax.Block, `else`: Option[hydra.ext.python.syntax.Block])

case class ForStatement(async: Boolean, targets: Seq[hydra.ext.python.syntax.StarTarget], expressions: Seq[hydra.ext.python.syntax.StarExpression], typeComment: Option[hydra.ext.python.syntax.TypeComment], body: hydra.ext.python.syntax.Block, `else`: Option[hydra.ext.python.syntax.Block])

case class WithStatement(async: Boolean, items: Seq[hydra.ext.python.syntax.WithItem], typeComment: Option[hydra.ext.python.syntax.TypeComment], body: hydra.ext.python.syntax.Block)

case class WithItem(expression: hydra.ext.python.syntax.Expression, as: Option[hydra.ext.python.syntax.StarTarget])

enum TryStatement :
   case `finally`(value: hydra.ext.python.syntax.TryFinallyStatement) extends TryStatement
   case except(value: hydra.ext.python.syntax.TryExceptStatement) extends TryStatement
   case exceptStar(value: hydra.ext.python.syntax.TryExceptStarStatement) extends TryStatement

case class TryFinallyStatement(body: hydra.ext.python.syntax.Block, `finally`: hydra.ext.python.syntax.Block)

case class TryExceptStatement(body: hydra.ext.python.syntax.Block, excepts: Seq[hydra.ext.python.syntax.ExceptBlock], `else`: Option[hydra.ext.python.syntax.Block], `finally`: Option[hydra.ext.python.syntax.Block])

case class TryExceptStarStatement(body: hydra.ext.python.syntax.Block, excepts: Seq[hydra.ext.python.syntax.ExceptStarBlock], `else`: Option[hydra.ext.python.syntax.Block], `finally`: Option[hydra.ext.python.syntax.Block])

case class ExceptBlock(expression: Option[hydra.ext.python.syntax.ExceptExpression], body: hydra.ext.python.syntax.Block)

case class ExceptExpression(expression: hydra.ext.python.syntax.Expression, as: Option[hydra.ext.python.syntax.Name])

case class ExceptStarBlock(expression: hydra.ext.python.syntax.Expression, as: Option[hydra.ext.python.syntax.Name], body: hydra.ext.python.syntax.Block)

case class MatchStatement(subject: hydra.ext.python.syntax.SubjectExpression, cases: Seq[hydra.ext.python.syntax.CaseBlock])

enum SubjectExpression :
   case tuple(value: Seq[hydra.ext.python.syntax.StarNamedExpression]) extends SubjectExpression
   case simple(value: hydra.ext.python.syntax.NamedExpression) extends SubjectExpression

case class CaseBlock(patterns: hydra.ext.python.syntax.Patterns, guard: Option[hydra.ext.python.syntax.Guard], body: hydra.ext.python.syntax.Block)

type Guard = hydra.ext.python.syntax.NamedExpression

enum Patterns :
   case sequence(value: hydra.ext.python.syntax.OpenSequencePattern) extends Patterns
   case pattern(value: hydra.ext.python.syntax.Pattern) extends Patterns

enum Pattern :
   case as(value: hydra.ext.python.syntax.AsPattern) extends Pattern
   case or(value: hydra.ext.python.syntax.OrPattern) extends Pattern

case class AsPattern(pattern: hydra.ext.python.syntax.OrPattern, as: hydra.ext.python.syntax.PatternCaptureTarget)

type OrPattern = Seq[hydra.ext.python.syntax.ClosedPattern]

enum ClosedPattern :
   case literal(value: hydra.ext.python.syntax.LiteralExpression) extends ClosedPattern
   case capture(value: hydra.ext.python.syntax.CapturePattern) extends ClosedPattern
   case wildcard extends ClosedPattern
   case value(value: hydra.ext.python.syntax.ValuePattern) extends ClosedPattern
   case group(value: hydra.ext.python.syntax.GroupPattern) extends ClosedPattern
   case sequence(value: hydra.ext.python.syntax.SequencePattern) extends ClosedPattern
   case mapping(value: hydra.ext.python.syntax.MappingPattern) extends ClosedPattern
   case `class`(value: hydra.ext.python.syntax.ClassPattern) extends ClosedPattern

enum LiteralExpression :
   case number(value: hydra.ext.python.syntax.SignedNumber) extends LiteralExpression
   case complex(value: hydra.ext.python.syntax.ComplexNumber) extends LiteralExpression
   case string(value: scala.Predef.String) extends LiteralExpression
   case none extends LiteralExpression
   case `true` extends LiteralExpression
   case `false` extends LiteralExpression

case class ComplexNumber(real: hydra.ext.python.syntax.SignedRealNumber, plusOrMinus: hydra.ext.python.syntax.PlusOrMinus, imaginary: hydra.ext.python.syntax.ImaginaryNumber)

enum PlusOrMinus :
   case plus extends PlusOrMinus
   case minus extends PlusOrMinus

enum SignedNumber :
   case sign(value: hydra.ext.python.syntax.PlusOrMinus) extends SignedNumber
   case number(value: hydra.ext.python.syntax.Number) extends SignedNumber

enum SignedRealNumber :
   case sign(value: hydra.ext.python.syntax.PlusOrMinus) extends SignedRealNumber
   case number(value: hydra.ext.python.syntax.RealNumber) extends SignedRealNumber

type RealNumber = hydra.ext.python.syntax.Number

type ImaginaryNumber = hydra.ext.python.syntax.Number

type CapturePattern = hydra.ext.python.syntax.PatternCaptureTarget

type PatternCaptureTarget = hydra.ext.python.syntax.Name

type ValuePattern = hydra.ext.python.syntax.Attribute

type Attribute = Seq[hydra.ext.python.syntax.Name]

type NameOrAttribute = Seq[hydra.ext.python.syntax.Name]

type GroupPattern = hydra.ext.python.syntax.Pattern

enum SequencePattern :
   case list(value: Option[hydra.ext.python.syntax.MaybeSequencePattern]) extends SequencePattern
   case tuple(value: Option[hydra.ext.python.syntax.OpenSequencePattern]) extends SequencePattern

case class OpenSequencePattern(head: hydra.ext.python.syntax.MaybeStarPattern, tail: Option[hydra.ext.python.syntax.MaybeSequencePattern])

type MaybeSequencePattern = Seq[hydra.ext.python.syntax.MaybeStarPattern]

enum MaybeStarPattern :
   case star(value: hydra.ext.python.syntax.StarPattern) extends MaybeStarPattern
   case pattern(value: hydra.ext.python.syntax.Pattern) extends MaybeStarPattern

enum StarPattern :
   case capture(value: hydra.ext.python.syntax.PatternCaptureTarget) extends StarPattern
   case wildcard extends StarPattern

case class MappingPattern(items: Option[hydra.ext.python.syntax.ItemsPattern], doubleStar: Option[hydra.ext.python.syntax.DoubleStarPattern])

type ItemsPattern = Seq[hydra.ext.python.syntax.KeyValuePattern]

case class KeyValuePattern(key: hydra.ext.python.syntax.LiteralExpressionOrAttribute, value: hydra.ext.python.syntax.Pattern)

enum LiteralExpressionOrAttribute :
   case literal(value: hydra.ext.python.syntax.LiteralExpression) extends LiteralExpressionOrAttribute
   case attribute(value: hydra.ext.python.syntax.Attribute) extends LiteralExpressionOrAttribute

type DoubleStarPattern = hydra.ext.python.syntax.PatternCaptureTarget

case class ClassPattern(nameOrAttribute: hydra.ext.python.syntax.NameOrAttribute, positionalPatterns: Option[hydra.ext.python.syntax.PositionalPatterns], keywordPatterns: Option[hydra.ext.python.syntax.KeywordPatterns])

type PositionalPatterns = Seq[hydra.ext.python.syntax.Pattern]

type KeywordPatterns = Seq[hydra.ext.python.syntax.KeywordPattern]

case class KeywordPattern(name: hydra.ext.python.syntax.Name, pattern: hydra.ext.python.syntax.Pattern)

case class TypeAlias(name: hydra.ext.python.syntax.Name, typeParams: Seq[hydra.ext.python.syntax.TypeParameter], expression: hydra.ext.python.syntax.Expression)

enum TypeParameter :
   case simple(value: hydra.ext.python.syntax.SimpleTypeParameter) extends TypeParameter
   case star(value: hydra.ext.python.syntax.StarTypeParameter) extends TypeParameter
   case doubleStar(value: hydra.ext.python.syntax.DoubleStarTypeParameter) extends TypeParameter

case class SimpleTypeParameter(name: hydra.ext.python.syntax.Name, bound: Option[hydra.ext.python.syntax.Expression], default: Option[hydra.ext.python.syntax.Expression])

case class StarTypeParameter(name: hydra.ext.python.syntax.Name, default: Option[hydra.ext.python.syntax.StarExpression])

case class DoubleStarTypeParameter(name: hydra.ext.python.syntax.Name, default: Option[hydra.ext.python.syntax.Expression])

enum Expression :
   case conditional(value: hydra.ext.python.syntax.Conditional) extends Expression
   case simple(value: hydra.ext.python.syntax.Disjunction) extends Expression
   case lambda(value: hydra.ext.python.syntax.Lambda) extends Expression

case class Conditional(body: hydra.ext.python.syntax.Disjunction, `if`: hydra.ext.python.syntax.Disjunction, `else`: hydra.ext.python.syntax.Expression)

enum YieldExpression :
   case from(value: hydra.ext.python.syntax.Expression) extends YieldExpression
   case simple(value: Seq[hydra.ext.python.syntax.StarExpression]) extends YieldExpression

enum StarExpression :
   case star(value: hydra.ext.python.syntax.BitwiseOr) extends StarExpression
   case simple(value: hydra.ext.python.syntax.Expression) extends StarExpression

type StarNamedExpressions = Seq[hydra.ext.python.syntax.StarNamedExpression]

enum StarNamedExpression :
   case star(value: hydra.ext.python.syntax.BitwiseOr) extends StarNamedExpression
   case simple(value: hydra.ext.python.syntax.NamedExpression) extends StarNamedExpression

case class AssignmentExpression(name: hydra.ext.python.syntax.Name, expression: hydra.ext.python.syntax.Expression)

enum NamedExpression :
   case assignment(value: hydra.ext.python.syntax.AssignmentExpression) extends NamedExpression
   case simple(value: hydra.ext.python.syntax.Expression) extends NamedExpression

type Disjunction = Seq[hydra.ext.python.syntax.Conjunction]

type Conjunction = Seq[hydra.ext.python.syntax.Inversion]

enum Inversion :
   case not(value: hydra.ext.python.syntax.Inversion) extends Inversion
   case simple(value: hydra.ext.python.syntax.Comparison) extends Inversion

case class Comparison(lhs: hydra.ext.python.syntax.BitwiseOr, rhs: Seq[hydra.ext.python.syntax.CompareOpBitwiseOrPair])

case class CompareOpBitwiseOrPair(operator: hydra.ext.python.syntax.CompareOp, rhs: hydra.ext.python.syntax.BitwiseOr)

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

case class BitwiseOr(lhs: Option[hydra.ext.python.syntax.BitwiseOr], rhs: hydra.ext.python.syntax.BitwiseXor)

case class BitwiseXor(lhs: Option[hydra.ext.python.syntax.BitwiseXor], rhs: hydra.ext.python.syntax.BitwiseAnd)

case class BitwiseAnd(lhs: Option[hydra.ext.python.syntax.BitwiseAnd], rhs: hydra.ext.python.syntax.ShiftExpression)

case class ShiftExpression(lhs: Option[hydra.ext.python.syntax.ShiftLhs], rhs: hydra.ext.python.syntax.Sum)

case class ShiftLhs(operand: hydra.ext.python.syntax.ShiftExpression, operator: hydra.ext.python.syntax.ShiftOp)

enum ShiftOp :
   case left extends ShiftOp
   case right extends ShiftOp

case class Sum(lhs: Option[hydra.ext.python.syntax.SumLhs], rhs: hydra.ext.python.syntax.Term)

case class SumLhs(operand: hydra.ext.python.syntax.Sum, operator: hydra.ext.python.syntax.SumOp)

enum SumOp :
   case add extends SumOp
   case sub extends SumOp

case class Term(lhs: Option[hydra.ext.python.syntax.TermLhs], rhs: hydra.ext.python.syntax.Factor)

case class TermLhs(operand: hydra.ext.python.syntax.Term, operator: hydra.ext.python.syntax.TermOp)

enum TermOp :
   case mul extends TermOp
   case div extends TermOp
   case floordiv extends TermOp
   case mod extends TermOp
   case matmul extends TermOp

enum Factor :
   case positive(value: hydra.ext.python.syntax.Factor) extends Factor
   case negative(value: hydra.ext.python.syntax.Factor) extends Factor
   case complement(value: hydra.ext.python.syntax.Factor) extends Factor
   case simple(value: hydra.ext.python.syntax.Power) extends Factor

case class Power(lhs: hydra.ext.python.syntax.AwaitPrimary, rhs: Option[hydra.ext.python.syntax.Factor])

case class AwaitPrimary(await: Boolean, primary: hydra.ext.python.syntax.Primary)

enum Primary :
   case simple(value: hydra.ext.python.syntax.Atom) extends Primary
   case compound(value: hydra.ext.python.syntax.PrimaryWithRhs) extends Primary

case class PrimaryWithRhs(primary: hydra.ext.python.syntax.Primary, rhs: hydra.ext.python.syntax.PrimaryRhs)

enum PrimaryRhs :
   case project(value: hydra.ext.python.syntax.Name) extends PrimaryRhs
   case genexp(value: hydra.ext.python.syntax.Genexp) extends PrimaryRhs
   case call(value: hydra.ext.python.syntax.Args) extends PrimaryRhs
   case slices(value: hydra.ext.python.syntax.Slices) extends PrimaryRhs

case class Slices(head: hydra.ext.python.syntax.Slice, tail: Seq[hydra.ext.python.syntax.SliceOrStarredExpression])

enum SliceOrStarredExpression :
   case slice(value: hydra.ext.python.syntax.Slice) extends SliceOrStarredExpression
   case starred(value: hydra.ext.python.syntax.StarredExpression) extends SliceOrStarredExpression

enum Slice :
   case named(value: hydra.ext.python.syntax.NamedExpression) extends Slice
   case `slice_`(value: hydra.ext.python.syntax.SliceExpression) extends Slice

case class SliceExpression(start: Option[hydra.ext.python.syntax.Expression], stop: Option[hydra.ext.python.syntax.Expression], step: Option[hydra.ext.python.syntax.Expression])

enum Atom :
   case name(value: hydra.ext.python.syntax.Name) extends Atom
   case `true` extends Atom
   case `false` extends Atom
   case none extends Atom
   case string(value: hydra.ext.python.syntax.String) extends Atom
   case number(value: hydra.ext.python.syntax.Number) extends Atom
   case tuple(value: hydra.ext.python.syntax.Tuple) extends Atom
   case group(value: hydra.ext.python.syntax.Group) extends Atom
   case genexp(value: hydra.ext.python.syntax.Genexp) extends Atom
   case list(value: hydra.ext.python.syntax.List) extends Atom
   case listcomp(value: hydra.ext.python.syntax.Listcomp) extends Atom
   case dict(value: hydra.ext.python.syntax.Dict) extends Atom
   case set(value: hydra.ext.python.syntax.Set) extends Atom
   case dictcomp(value: hydra.ext.python.syntax.Dictcomp) extends Atom
   case setcomp(value: hydra.ext.python.syntax.Setcomp) extends Atom
   case ellipsis extends Atom

enum Group :
   case `yield`(value: hydra.ext.python.syntax.YieldExpression) extends Group
   case expression(value: hydra.ext.python.syntax.NamedExpression) extends Group

case class Lambda(params: hydra.ext.python.syntax.LambdaParameters, body: hydra.ext.python.syntax.Expression)

case class LambdaParameters(slashNoDefault: Option[hydra.ext.python.syntax.LambdaSlashNoDefault], paramNoDefault: Seq[hydra.ext.python.syntax.LambdaParamNoDefault], paramWithDefault: Seq[hydra.ext.python.syntax.LambdaParamWithDefault], starEtc: Option[hydra.ext.python.syntax.LambdaStarEtc])

case class LambdaSlashNoDefault(parameters: Seq[hydra.ext.python.syntax.LambdaParamNoDefault])

case class LambdaSlashWithDefault(paramNoDefault: Seq[hydra.ext.python.syntax.LambdaParamNoDefault], paramWithDefault: Seq[hydra.ext.python.syntax.LambdaParamWithDefault])

enum LambdaStarEtc :
   case star(value: Boolean) extends LambdaStarEtc
   case paramNoDefault(value: hydra.ext.python.syntax.LambdaParamNoDefault) extends LambdaStarEtc
   case paramMaybeDefault(value: Seq[hydra.ext.python.syntax.LambdaParamMaybeDefault]) extends LambdaStarEtc
   case kwds(value: hydra.ext.python.syntax.LambdaKwds) extends LambdaStarEtc

type LambdaKwds = hydra.ext.python.syntax.LambdaParamNoDefault

type LambdaParamNoDefault = hydra.ext.python.syntax.Name

case class LambdaParamWithDefault(param: hydra.ext.python.syntax.Name, default: Option[hydra.ext.python.syntax.Default])

case class LambdaParamMaybeDefault(param: hydra.ext.python.syntax.Name, default: Option[hydra.ext.python.syntax.Default])

type List = Seq[hydra.ext.python.syntax.StarNamedExpression]

type Tuple = Seq[hydra.ext.python.syntax.StarNamedExpression]

type Set = Seq[hydra.ext.python.syntax.StarNamedExpression]

type Dict = Seq[hydra.ext.python.syntax.DoubleStarredKvpair]

enum DoubleStarredKvpair :
   case starred(value: hydra.ext.python.syntax.BitwiseOr) extends DoubleStarredKvpair
   case pair(value: hydra.ext.python.syntax.Kvpair) extends DoubleStarredKvpair

case class Kvpair(key: hydra.ext.python.syntax.Expression, value: hydra.ext.python.syntax.Expression)

type ForIfClauses = Seq[hydra.ext.python.syntax.ForIfClause]

case class ForIfClause(async: Boolean, targets: Seq[hydra.ext.python.syntax.StarTarget], in: hydra.ext.python.syntax.Disjunction, ifs: Seq[hydra.ext.python.syntax.Disjunction])

case class Listcomp(expression: hydra.ext.python.syntax.NamedExpression, forIfClauses: hydra.ext.python.syntax.ForIfClauses)

case class Setcomp(expression: hydra.ext.python.syntax.NamedExpression, forIfClauses: hydra.ext.python.syntax.ForIfClauses)

case class Genexp(head: hydra.ext.python.syntax.GenexpHead, tail: hydra.ext.python.syntax.ForIfClauses)

enum GenexpHead :
   case assignment(value: hydra.ext.python.syntax.AssignmentExpression) extends GenexpHead
   case expression(value: hydra.ext.python.syntax.Expression) extends GenexpHead

case class Dictcomp(kvpair: hydra.ext.python.syntax.Kvpair, forIfClauses: hydra.ext.python.syntax.ForIfClauses)

case class Args(positional: Seq[hydra.ext.python.syntax.PosArg], kwargOrStarred: Seq[hydra.ext.python.syntax.KwargOrStarred], kwargOrDoubleStarred: Seq[hydra.ext.python.syntax.KwargOrDoubleStarred])

enum PosArg :
   case starred(value: hydra.ext.python.syntax.StarredExpression) extends PosArg
   case assignment(value: hydra.ext.python.syntax.AssignmentExpression) extends PosArg
   case expression(value: hydra.ext.python.syntax.Expression) extends PosArg

type StarredExpression = hydra.ext.python.syntax.Expression

enum KwargOrStarred :
   case kwarg(value: hydra.ext.python.syntax.Kwarg) extends KwargOrStarred
   case starred(value: hydra.ext.python.syntax.StarredExpression) extends KwargOrStarred

case class Kwarg(name: hydra.ext.python.syntax.Name, value: hydra.ext.python.syntax.Expression)

enum KwargOrDoubleStarred :
   case kwarg(value: hydra.ext.python.syntax.Kwarg) extends KwargOrDoubleStarred
   case doubleStarred(value: hydra.ext.python.syntax.Expression) extends KwargOrDoubleStarred

type StarTargetsListSeq = Seq[hydra.ext.python.syntax.StarTarget]

type StarTargetsTupleSeq = Seq[hydra.ext.python.syntax.StarTarget]

enum StarTarget :
   case starred(value: hydra.ext.python.syntax.StarTarget) extends StarTarget
   case unstarred(value: hydra.ext.python.syntax.TargetWithStarAtom) extends StarTarget

enum TargetWithStarAtom :
   case project(value: hydra.ext.python.syntax.TPrimaryAndName) extends TargetWithStarAtom
   case slices(value: hydra.ext.python.syntax.TPrimaryAndSlices) extends TargetWithStarAtom
   case atom(value: hydra.ext.python.syntax.StarAtom) extends TargetWithStarAtom

case class TPrimaryAndName(primary: hydra.ext.python.syntax.TPrimary, name: hydra.ext.python.syntax.Name)

case class TPrimaryAndSlices(primary: hydra.ext.python.syntax.TPrimary, slices: hydra.ext.python.syntax.Slices)

enum StarAtom :
   case name(value: hydra.ext.python.syntax.Name) extends StarAtom
   case targetWithStarAtom(value: hydra.ext.python.syntax.TargetWithStarAtom) extends StarAtom
   case starTargetsTupleSeq(value: Option[hydra.ext.python.syntax.StarTargetsTupleSeq]) extends StarAtom
   case starTargetsListSeq(value: Option[hydra.ext.python.syntax.StarTargetsListSeq]) extends StarAtom

enum SingleTarget :
   case subscriptAttributeTarget(value: hydra.ext.python.syntax.SingleSubscriptAttributeTarget) extends SingleTarget
   case name(value: hydra.ext.python.syntax.Name) extends SingleTarget
   case parens(value: hydra.ext.python.syntax.SingleTarget) extends SingleTarget

enum SingleSubscriptAttributeTarget :
   case primaryAndName(value: hydra.ext.python.syntax.TPrimaryAndName) extends SingleSubscriptAttributeTarget
   case primaryAndSlices(value: hydra.ext.python.syntax.TPrimaryAndSlices) extends SingleSubscriptAttributeTarget

enum TPrimary :
   case primaryAndName(value: hydra.ext.python.syntax.TPrimaryAndName) extends TPrimary
   case primaryAndSlices(value: hydra.ext.python.syntax.TPrimaryAndSlices) extends TPrimary
   case primaryAndGenexp(value: hydra.ext.python.syntax.TPrimaryAndGenexp) extends TPrimary
   case primaryAndArguments(value: hydra.ext.python.syntax.TPrimaryAndArguments) extends TPrimary
   case atom(value: hydra.ext.python.syntax.Atom) extends TPrimary

case class TPrimaryAndGenexp(primary: hydra.ext.python.syntax.TPrimary, genexp: hydra.ext.python.syntax.Genexp)

case class TPrimaryAndArguments(primary: hydra.ext.python.syntax.TPrimary, arguments: Option[hydra.ext.python.syntax.Args])

type DelTargets = Seq[hydra.ext.python.syntax.DelTarget]

enum DelTarget :
   case primaryAndName(value: hydra.ext.python.syntax.TPrimaryAndName) extends DelTarget
   case primaryAndSlices(value: hydra.ext.python.syntax.TPrimaryAndSlices) extends DelTarget
   case delTAtom(value: hydra.ext.python.syntax.DelTAtom) extends DelTarget

enum DelTAtom :
   case name(value: hydra.ext.python.syntax.Name) extends DelTAtom
   case target(value: hydra.ext.python.syntax.DelTarget) extends DelTAtom
   case targets(value: hydra.ext.python.syntax.DelTargets) extends DelTAtom

enum TypeExpression :
   case expression(value: hydra.ext.python.syntax.Expression) extends TypeExpression
   case starredExpression(value: hydra.ext.python.syntax.Expression) extends TypeExpression
   case doubleStarredExpression(value: hydra.ext.python.syntax.Expression) extends TypeExpression

type FuncTypeComment = hydra.ext.python.syntax.TypeComment
