package hydra.ext.python.utils

import hydra.ext.python.environment.*

import hydra.ext.python.syntax.*

import hydra.module.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.strings

def annotatedExpression(mcomment: Option[scala.Predef.String])(expr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Expression =
  hydra.lib.maybes.maybe[hydra.ext.python.syntax.Expression, scala.Predef.String](expr)((c: scala.Predef.String) =>
  hydra.ext.python.utils.pyPrimaryToPyExpression(hydra.ext.python.utils.primaryWithExpressionSlices(hydra.ext.python.utils.pyNameToPyPrimary("Annotated"))(Seq(expr, hydra.ext.python.utils.doubleQuotedString(c)))))(mcomment)

def annotatedStatement(mcomment: Option[scala.Predef.String])(stmt: hydra.ext.python.syntax.Statement): hydra.ext.python.syntax.Statement =
  hydra.lib.maybes.maybe[hydra.ext.python.syntax.Statement, scala.Predef.String](stmt)((c: scala.Predef.String) =>
  hydra.ext.python.syntax.Statement.annotated(hydra.ext.python.syntax.AnnotatedStatement(c, stmt)))(mcomment)

def assignment(name: hydra.ext.python.syntax.Name)(rhs: hydra.ext.python.syntax.AnnotatedRhs): hydra.ext.python.syntax.Statement =
  hydra.ext.python.utils.pyAssignmentToPyStatement(hydra.ext.python.syntax.Assignment.untyped(hydra.ext.python.syntax.UntypedAssignment(Seq(hydra.ext.python.utils.pyNameToPyStarTarget(name)), rhs, None)))

def assignmentStatement(name: hydra.ext.python.syntax.Name)(expr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Statement =
  hydra.ext.python.utils.assignment(name)(hydra.ext.python.utils.pyExpressionToPyAnnotatedRhs(expr))

def castTo(pytype: hydra.ext.python.syntax.Expression)(pyexpr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary("cast"))(Seq(pytype, pyexpr))

def commentStatement(s: scala.Predef.String): hydra.ext.python.syntax.Statement =
  hydra.ext.python.utils.pyExpressionToPyStatement(hydra.ext.python.utils.tripleQuotedString(s))

def decodePyComparisonToPyAwaitPrimary(c: hydra.ext.python.syntax.Comparison): Option[hydra.ext.python.syntax.Primary] =
  {
  lazy val rhs: Seq[hydra.ext.python.syntax.CompareOpBitwiseOrPair] = (c.rhs)
  lazy val lhs: hydra.ext.python.syntax.BitwiseOr = (c.lhs)
  lazy val orLhs: Option[hydra.ext.python.syntax.BitwiseOr] = (lhs.lhs)
  lazy val orRhs: hydra.ext.python.syntax.BitwiseXor = (lhs.rhs)
  lazy val xorLhs: Option[hydra.ext.python.syntax.BitwiseXor] = (orRhs.lhs)
  lazy val xorRhs: hydra.ext.python.syntax.BitwiseAnd = (orRhs.rhs)
  lazy val andLhs: Option[hydra.ext.python.syntax.BitwiseAnd] = (xorRhs.lhs)
  lazy val andRhs: hydra.ext.python.syntax.ShiftExpression = (xorRhs.rhs)
  lazy val shiftLhs: Option[hydra.ext.python.syntax.ShiftLhs] = (andRhs.lhs)
  lazy val shiftRhs: hydra.ext.python.syntax.Sum = (andRhs.rhs)
  lazy val sumLhs: Option[hydra.ext.python.syntax.SumLhs] = (shiftRhs.lhs)
  lazy val sumRhs: hydra.ext.python.syntax.Term = (shiftRhs.rhs)
  lazy val termLhs: Option[hydra.ext.python.syntax.TermLhs] = (sumRhs.lhs)
  lazy val termRhs: hydra.ext.python.syntax.Factor = (sumRhs.rhs)
  hydra.lib.logic.ifElse[Option[hydra.ext.python.syntax.Primary]](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.ext.python.syntax.CompareOpBitwiseOrPair](rhs)))(None)(hydra.lib.logic.ifElse[Option[hydra.ext.python.syntax.Primary]](hydra.lib.maybes.isJust[hydra.ext.python.syntax.BitwiseOr](orLhs))(None)(hydra.lib.logic.ifElse[Option[hydra.ext.python.syntax.Primary]](hydra.lib.maybes.isJust[hydra.ext.python.syntax.BitwiseXor](xorLhs))(None)(hydra.lib.logic.ifElse[Option[hydra.ext.python.syntax.Primary]](hydra.lib.maybes.isJust[hydra.ext.python.syntax.BitwiseAnd](andLhs))(None)(hydra.lib.logic.ifElse[Option[hydra.ext.python.syntax.Primary]](hydra.lib.maybes.isJust[hydra.ext.python.syntax.ShiftLhs](shiftLhs))(None)(hydra.lib.logic.ifElse[Option[hydra.ext.python.syntax.Primary]](hydra.lib.maybes.isJust[hydra.ext.python.syntax.SumLhs](sumLhs))(None)(hydra.lib.logic.ifElse[Option[hydra.ext.python.syntax.Primary]](hydra.lib.maybes.isJust[hydra.ext.python.syntax.TermLhs](termLhs))(None)(termRhs match
    case hydra.ext.python.syntax.Factor.simple(v_Factor_simple_power) => hydra.ext.python.utils.decodePyPowerToPyPrimary(v_Factor_simple_power)
    case _ => None)))))))
}

def decodePyConjunctionToPyPrimary(c: hydra.ext.python.syntax.Conjunction): Option[hydra.ext.python.syntax.Primary] =
  {
  lazy val inversions: Seq[hydra.ext.python.syntax.Inversion] = c
  hydra.lib.logic.ifElse[Option[hydra.ext.python.syntax.Primary]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.python.syntax.Inversion](inversions))(1))(hydra.ext.python.utils.decodePyInversionToPyPrimary(hydra.lib.lists.head[hydra.ext.python.syntax.Inversion](inversions)))(None)
}

def decodePyExpressionToPyPrimary(e: hydra.ext.python.syntax.Expression): Option[hydra.ext.python.syntax.Primary] =
  e match
  case hydra.ext.python.syntax.Expression.simple(v_Expression_simple_disj) => {
    lazy val conjunctions: Seq[hydra.ext.python.syntax.Conjunction] = v_Expression_simple_disj
    hydra.lib.logic.ifElse[Option[hydra.ext.python.syntax.Primary]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.python.syntax.Conjunction](conjunctions))(1))(hydra.ext.python.utils.decodePyConjunctionToPyPrimary(hydra.lib.lists.head[hydra.ext.python.syntax.Conjunction](conjunctions)))(None)
  }
  case _ => None

def decodePyInversionToPyPrimary(i: hydra.ext.python.syntax.Inversion): Option[hydra.ext.python.syntax.Primary] =
  i match
  case hydra.ext.python.syntax.Inversion.simple(v_Inversion_simple_comparison) => hydra.ext.python.utils.decodePyComparisonToPyAwaitPrimary(v_Inversion_simple_comparison)
  case _ => None

def decodePyPowerToPyPrimary(p: hydra.ext.python.syntax.Power): Option[hydra.ext.python.syntax.Primary] =
  {
  lazy val lhs: hydra.ext.python.syntax.AwaitPrimary = (p.lhs)
  lazy val await: Boolean = (lhs.await)
  lazy val prim: hydra.ext.python.syntax.Primary = (lhs.primary)
  hydra.lib.logic.ifElse[Option[hydra.ext.python.syntax.Primary]](await)(None)(Some(prim))
}

def dottedAssignmentStatement(obj: hydra.ext.python.syntax.Name)(attr: hydra.ext.python.syntax.Name)(expr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Statement =
  {
  lazy val target: hydra.ext.python.syntax.StarTarget = hydra.ext.python.syntax.StarTarget.unstarred(hydra.ext.python.syntax.TargetWithStarAtom.project(hydra.ext.python.syntax.TPrimaryAndName(hydra.ext.python.syntax.TPrimary.atom(hydra.ext.python.syntax.Atom.name(obj)), attr)))
  hydra.ext.python.utils.pyAssignmentToPyStatement(hydra.ext.python.syntax.Assignment.untyped(hydra.ext.python.syntax.UntypedAssignment(Seq(target), hydra.ext.python.utils.pyExpressionToPyAnnotatedRhs(expr), None)))
}

def doubleQuotedString(s: scala.Predef.String): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.stringToPyExpression(hydra.ext.python.syntax.QuoteStyle.double)(s)

def findNamespaces(focusNs: hydra.module.Namespace)(defs: Seq[hydra.module.Definition]): hydra.module.Namespaces[hydra.ext.python.syntax.DottedName] =
  {
  lazy val coreNs: hydra.module.Namespace = "hydra.core"
  lazy val namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName] = hydra.schemas.namespacesForDefinitions(hydra.ext.python.names.encodeNamespace)(focusNs)(defs)
  hydra.lib.logic.ifElse[hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]](hydra.lib.equality.equal[scala.Predef.String](hydra.lib.pairs.first[hydra.module.Namespace, hydra.ext.python.syntax.DottedName](namespaces.focus))(coreNs))(namespaces)(hydra.module.Namespaces(namespaces.focus, hydra.lib.maps.insert[hydra.module.Namespace, hydra.ext.python.syntax.DottedName](coreNs)(hydra.ext.python.names.encodeNamespace(coreNs))(namespaces.mapping)))
}

def functionCall(func: hydra.ext.python.syntax.Primary)(args: Seq[hydra.ext.python.syntax.Expression]): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.pyPrimaryToPyExpression(hydra.ext.python.utils.primaryWithRhs(func)(hydra.ext.python.syntax.PrimaryRhs.call(hydra.ext.python.utils.pyExpressionsToPyArgs(args))))

lazy val getItemParams: hydra.ext.python.syntax.Parameters = hydra.ext.python.syntax.Parameters.paramNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters(Seq(hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param("cls", None), None), hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param("item", None), None)), Seq(), None))

def indentedBlock(mcomment: Option[scala.Predef.String])(stmts: Seq[Seq[hydra.ext.python.syntax.Statement]]): hydra.ext.python.syntax.Block =
  {
  lazy val commentGroup: Seq[hydra.ext.python.syntax.Statement] = hydra.lib.maybes.maybe[Seq[hydra.ext.python.syntax.Statement], scala.Predef.String](Seq())((s: scala.Predef.String) => Seq(hydra.ext.python.utils.commentStatement(s)))(mcomment)
  lazy val groups: Seq[Seq[hydra.ext.python.syntax.Statement]] = hydra.lib.lists.filter[Seq[hydra.ext.python.syntax.Statement]]((g: Seq[hydra.ext.python.syntax.Statement]) =>
    hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.ext.python.syntax.Statement](g)))(hydra.lib.lists.cons[Seq[hydra.ext.python.syntax.Statement]](commentGroup)(stmts))
  hydra.lib.logic.ifElse[hydra.ext.python.syntax.Block](hydra.lib.lists.`null`[Seq[hydra.ext.python.syntax.Statement]](groups))(hydra.ext.python.syntax.Block.indented(Seq(Seq(hydra.ext.python.syntax.Statement.simple(Seq(hydra.ext.python.utils.pyExpressionToPySimpleStatement(hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.ellipsis))))))))(hydra.ext.python.syntax.Block.indented(groups))
}

def nameAndParams(pyName: hydra.ext.python.syntax.Name)(params: Seq[hydra.ext.python.syntax.Expression]): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.primaryAndParams(hydra.ext.python.utils.pyNameToPyPrimary(pyName))(params)

def newtypeStatement(name: hydra.ext.python.syntax.Name)(mcomment: Option[scala.Predef.String])(expr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Statement =
  hydra.ext.python.utils.annotatedStatement(mcomment)(hydra.ext.python.utils.assignmentStatement(name)(hydra.ext.python.utils.functionCall(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("NewType")))(Seq(hydra.ext.python.utils.doubleQuotedString(name), expr))))

def orExpression(prims: Seq[hydra.ext.python.syntax.Primary]): hydra.ext.python.syntax.Expression =
  {
  def build(prev: Option[hydra.ext.python.syntax.BitwiseOr])(ps: Seq[hydra.ext.python.syntax.Primary]): hydra.ext.python.syntax.BitwiseOr =
    hydra.lib.logic.ifElse[hydra.ext.python.syntax.BitwiseOr](hydra.lib.lists.`null`[hydra.ext.python.syntax.Primary](hydra.lib.lists.tail[hydra.ext.python.syntax.Primary](ps)))(hydra.ext.python.syntax.BitwiseOr(prev, hydra.ext.python.utils.pyPrimaryToPyBitwiseXor(hydra.lib.lists.head[hydra.ext.python.syntax.Primary](ps))))(build(Some(hydra.ext.python.syntax.BitwiseOr(prev, hydra.ext.python.utils.pyPrimaryToPyBitwiseXor(hydra.lib.lists.head[hydra.ext.python.syntax.Primary](ps)))))(hydra.lib.lists.tail[hydra.ext.python.syntax.Primary](ps)))
  hydra.ext.python.utils.pyBitwiseOrToPyExpression(build(None)(prims))
}

def primaryAndParams(prim: hydra.ext.python.syntax.Primary)(params: Seq[hydra.ext.python.syntax.Expression]): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.pyPrimaryToPyExpression(hydra.ext.python.utils.primaryWithExpressionSlices(prim)(params))

def primaryWithExpressionSlices(prim: hydra.ext.python.syntax.Primary)(exprs: Seq[hydra.ext.python.syntax.Expression]): hydra.ext.python.syntax.Primary =
  hydra.ext.python.utils.primaryWithSlices(prim)(hydra.ext.python.utils.pyExpressionToPySlice(hydra.lib.lists.head[hydra.ext.python.syntax.Expression](exprs)))(hydra.lib.lists.map[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.SliceOrStarredExpression]((e: hydra.ext.python.syntax.Expression) =>
  hydra.ext.python.syntax.SliceOrStarredExpression.slice(hydra.ext.python.utils.pyExpressionToPySlice(e)))(hydra.lib.lists.tail[hydra.ext.python.syntax.Expression](exprs)))

def primaryWithRhs(prim: hydra.ext.python.syntax.Primary)(rhs: hydra.ext.python.syntax.PrimaryRhs): hydra.ext.python.syntax.Primary =
  hydra.ext.python.syntax.Primary.compound(hydra.ext.python.syntax.PrimaryWithRhs(prim, rhs))

def primaryWithSlices(prim: hydra.ext.python.syntax.Primary)(first: hydra.ext.python.syntax.Slice)(rest: Seq[hydra.ext.python.syntax.SliceOrStarredExpression]): hydra.ext.python.syntax.Primary =
  hydra.ext.python.utils.primaryWithRhs(prim)(hydra.ext.python.syntax.PrimaryRhs.slices(hydra.ext.python.syntax.Slices(first, rest)))

def projectFromExpression(exp: hydra.ext.python.syntax.Expression)(name: hydra.ext.python.syntax.Name): hydra.ext.python.syntax.Expression =
  {
  lazy val prim: hydra.ext.python.syntax.Primary = hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.group(hydra.ext.python.syntax.Group.expression(hydra.ext.python.syntax.NamedExpression.simple(exp))))
  hydra.ext.python.utils.pyPrimaryToPyExpression(hydra.ext.python.syntax.Primary.compound(hydra.ext.python.syntax.PrimaryWithRhs(prim, hydra.ext.python.syntax.PrimaryRhs.project(name))))
}

def pyAssignmentToPyStatement(a: hydra.ext.python.syntax.Assignment): hydra.ext.python.syntax.Statement =
  hydra.ext.python.utils.pySimpleStatementToPyStatement(hydra.ext.python.syntax.SimpleStatement.assignment(a))

def pyAtomToPyExpression(atom: hydra.ext.python.syntax.Atom): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.pyPrimaryToPyExpression(hydra.ext.python.syntax.Primary.simple(atom))

def pyBitwiseOrToPyConjunction(bor: hydra.ext.python.syntax.BitwiseOr): hydra.ext.python.syntax.Conjunction =
  Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(bor, Seq())))

def pyBitwiseOrToPyExpression(bor: hydra.ext.python.syntax.BitwiseOr): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.pyConjunctionToPyExpression(hydra.ext.python.utils.pyBitwiseOrToPyConjunction(bor))

def pyClassDefinitionToPyStatement(cd: hydra.ext.python.syntax.ClassDefinition): hydra.ext.python.syntax.Statement =
  hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.classDef(cd))

def pyClosedPatternToPyPatterns(p: hydra.ext.python.syntax.ClosedPattern): hydra.ext.python.syntax.Patterns =
  hydra.ext.python.syntax.Patterns.pattern(hydra.ext.python.syntax.Pattern.or(Seq(p)))

def pyConjunctionToPyExpression(conj: hydra.ext.python.syntax.Conjunction): hydra.ext.python.syntax.Expression = hydra.ext.python.syntax.Expression.simple(Seq(conj))

def pyExpressionToBitwiseOr(e: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.BitwiseOr =
  hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.group(hydra.ext.python.syntax.Group.expression(hydra.ext.python.syntax.NamedExpression.simple(e))))), None))))))))

def pyExpressionToDisjunction(e: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Disjunction =
  e match
  case hydra.ext.python.syntax.Expression.simple(v_Expression_simple_disj) => v_Expression_simple_disj
  case _ => Seq(hydra.ext.python.utils.pyPrimaryToPyConjunction(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.group(hydra.ext.python.syntax.Group.expression(hydra.ext.python.syntax.NamedExpression.simple(e))))))

def pyExpressionToPyAnnotatedRhs(expr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.AnnotatedRhs =
  hydra.ext.python.syntax.AnnotatedRhs.star(Seq(hydra.ext.python.syntax.StarExpression.simple(expr)))

def pyExpressionToPyPrimary(e: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Primary =
  hydra.lib.maybes.maybe[hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.Primary](hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.group(hydra.ext.python.syntax.Group.expression(hydra.ext.python.syntax.NamedExpression.simple(e)))))((prim: hydra.ext.python.syntax.Primary) => prim)(hydra.ext.python.utils.decodePyExpressionToPyPrimary(e))

def pyExpressionToPySimpleStatement(expr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.SimpleStatement =
  hydra.ext.python.syntax.SimpleStatement.starExpressions(Seq(hydra.ext.python.syntax.StarExpression.simple(expr)))

def pyExpressionToPySlice(expr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Slice =
  hydra.ext.python.syntax.Slice.named(hydra.ext.python.syntax.NamedExpression.simple(expr))

def pyExpressionToPyStarNamedExpression(expr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.StarNamedExpression =
  hydra.ext.python.syntax.StarNamedExpression.simple(hydra.ext.python.syntax.NamedExpression.simple(expr))

def pyExpressionToPyStatement(expr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Statement =
  hydra.ext.python.utils.pySimpleStatementToPyStatement(hydra.ext.python.utils.pyExpressionToPySimpleStatement(expr))

def pyExpressionsToPyArgs(exprs: Seq[hydra.ext.python.syntax.Expression]): hydra.ext.python.syntax.Args =
  hydra.ext.python.syntax.Args(hydra.lib.lists.map[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.PosArg]((e: hydra.ext.python.syntax.Expression) => hydra.ext.python.syntax.PosArg.expression(e))(exprs), Seq(), Seq())

def pyList(exprs: Seq[hydra.ext.python.syntax.Expression]): hydra.ext.python.syntax.List =
  hydra.lib.lists.map[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.StarNamedExpression](hydra.ext.python.utils.pyExpressionToPyStarNamedExpression)(exprs)

def pyNameToPyExpression(name: hydra.ext.python.syntax.Name): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.pyPrimaryToPyExpression(hydra.ext.python.utils.pyNameToPyPrimary(name))

def pyNameToPyNamedExpression(name: hydra.ext.python.syntax.Name): hydra.ext.python.syntax.NamedExpression =
  hydra.ext.python.syntax.NamedExpression.simple(hydra.ext.python.utils.pyNameToPyExpression(name))

def pyNameToPyPrimary(name: hydra.ext.python.syntax.Name): hydra.ext.python.syntax.Primary = hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name(name))

def pyNameToPyStarTarget(name: hydra.ext.python.syntax.Name): hydra.ext.python.syntax.StarTarget =
  hydra.ext.python.syntax.StarTarget.unstarred(hydra.ext.python.syntax.TargetWithStarAtom.atom(hydra.ext.python.syntax.StarAtom.name(name)))

def pyNameToPyTypeParameter(name: hydra.ext.python.syntax.Name): hydra.ext.python.syntax.TypeParameter =
  hydra.ext.python.syntax.TypeParameter.simple(hydra.ext.python.syntax.SimpleTypeParameter(name, None, None))

lazy val pyNone: hydra.ext.python.syntax.Name = "None"

def pyPrimaryToPyBitwiseOr(prim: hydra.ext.python.syntax.Primary): hydra.ext.python.syntax.BitwiseOr =
  hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, prim), None))))))))

def pyPrimaryToPyBitwiseXor(prim: hydra.ext.python.syntax.Primary): hydra.ext.python.syntax.BitwiseXor =
  hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, prim), None)))))))

def pyPrimaryToPyConjunction(prim: hydra.ext.python.syntax.Primary): hydra.ext.python.syntax.Conjunction =
  hydra.ext.python.utils.pyBitwiseOrToPyConjunction(hydra.ext.python.utils.pyPrimaryToPyBitwiseOr(prim))

def pyPrimaryToPyExpression(prim: hydra.ext.python.syntax.Primary): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.pyConjunctionToPyExpression(hydra.ext.python.utils.pyPrimaryToPyConjunction(prim))

def pyPrimaryToPySlice(prim: hydra.ext.python.syntax.Primary): hydra.ext.python.syntax.Slice =
  hydra.ext.python.utils.pyExpressionToPySlice(hydra.ext.python.utils.pyPrimaryToPyExpression(prim))

def pySimpleStatementToPyStatement(s: hydra.ext.python.syntax.SimpleStatement): hydra.ext.python.syntax.Statement = hydra.ext.python.syntax.Statement.simple(Seq(s))

def raiseAssertionError(msg: scala.Predef.String): hydra.ext.python.syntax.Statement =
  hydra.ext.python.utils.pySimpleStatementToPyStatement(hydra.ext.python.syntax.SimpleStatement.raise(Some(hydra.ext.python.syntax.RaiseExpression(hydra.ext.python.utils.functionCall(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("AssertionError")))(Seq(hydra.ext.python.utils.doubleQuotedString(msg))), None))))

def raiseTypeError(msg: scala.Predef.String): hydra.ext.python.syntax.Statement =
  hydra.ext.python.utils.pySimpleStatementToPyStatement(hydra.ext.python.syntax.SimpleStatement.raise(Some(hydra.ext.python.syntax.RaiseExpression(hydra.ext.python.utils.functionCall(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("TypeError")))(Seq(hydra.ext.python.utils.doubleQuotedString(msg))), None))))

def returnSingle(expr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Statement =
  hydra.ext.python.utils.pySimpleStatementToPyStatement(hydra.ext.python.syntax.SimpleStatement.`return`(Seq(hydra.ext.python.syntax.StarExpression.simple(expr))))

lazy val selfOnlyParams: hydra.ext.python.syntax.Parameters = hydra.ext.python.syntax.Parameters.paramNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters(Seq(hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param("self", None), None)), Seq(), None))

lazy val selfOtherParams: hydra.ext.python.syntax.Parameters = hydra.ext.python.syntax.Parameters.paramNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters(Seq(hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param("self", None), None), hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param("other", None), None)), Seq(), None))

def singleQuotedString(s: scala.Predef.String): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.stringToPyExpression(hydra.ext.python.syntax.QuoteStyle.single)(s)

def stringToPyExpression(style: hydra.ext.python.syntax.QuoteStyle)(s: scala.Predef.String): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.string(hydra.ext.python.syntax.String(s, style)))

lazy val targetPythonVersion: hydra.ext.python.environment.PythonVersion = hydra.ext.python.environment.PythonVersion.python310

def tripleQuotedString(s: scala.Predef.String): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.stringToPyExpression(hydra.ext.python.syntax.QuoteStyle.triple)(s)

def typeAliasStatement(name: hydra.ext.python.syntax.Name)(tparams: Seq[hydra.ext.python.syntax.TypeParameter])(mcomment: Option[scala.Predef.String])(tyexpr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Statement =
  hydra.ext.python.utils.annotatedStatement(mcomment)(hydra.ext.python.utils.pySimpleStatementToPyStatement(hydra.ext.python.syntax.SimpleStatement.typeAlias(hydra.ext.python.syntax.TypeAlias(name, tparams, tyexpr))))

def typeAliasStatement310[T0](name: hydra.ext.python.syntax.Name)(_tparams: T0)(mcomment: Option[scala.Predef.String])(tyexpr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Statement =
  {
  lazy val quotedExpr: hydra.ext.python.syntax.Expression = hydra.ext.python.utils.doubleQuotedString(hydra.serialization.printExpr(hydra.ext.python.serde.encodeExpression(tyexpr)))
  hydra.ext.python.utils.annotatedStatement(mcomment)(hydra.ext.python.utils.pyAssignmentToPyStatement(hydra.ext.python.syntax.Assignment.typed(hydra.ext.python.syntax.TypedAssignment(hydra.ext.python.syntax.SingleTarget.name(name), hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("TypeAlias"))), None)))))))), Seq()))))), Some(hydra.ext.python.utils.pyExpressionToPyAnnotatedRhs(quotedExpr))))))
}

def unionTypeClassStatements310(name: hydra.ext.python.syntax.Name)(mcomment: Option[scala.Predef.String])(tyexpr: hydra.ext.python.syntax.Expression)(extraStmts: Seq[hydra.ext.python.syntax.Statement]): Seq[hydra.ext.python.syntax.Statement] =
  {
  lazy val nameStr: scala.Predef.String = name
  lazy val metaName: hydra.ext.python.syntax.Name = hydra.lib.strings.cat2(hydra.lib.strings.cat2("_")(nameStr))("Meta")
  lazy val docString: scala.Predef.String = hydra.serialization.printExpr(hydra.ext.python.serde.encodeExpression(tyexpr))
  lazy val returnObject: hydra.ext.python.syntax.Statement = hydra.ext.python.utils.pySimpleStatementToPyStatement(hydra.ext.python.syntax.SimpleStatement.`return`(Seq(hydra.ext.python.syntax.StarExpression.simple(hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("object"))), None)))))))), Seq())))))))))
  lazy val getItemMethod: hydra.ext.python.syntax.Statement = hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.function(hydra.ext.python.syntax.FunctionDefinition(None, hydra.ext.python.syntax.FunctionDefRaw(false, "__getitem__", Seq(), Some(hydra.ext.python.utils.getItemParams), None, None, hydra.ext.python.utils.indentedBlock(None)(Seq(Seq(returnObject)))))))
  lazy val metaClass: hydra.ext.python.syntax.Statement = hydra.ext.python.utils.pyClassDefinitionToPyStatement(hydra.ext.python.syntax.ClassDefinition(None, metaName, Seq(), Some(hydra.ext.python.utils.pyExpressionsToPyArgs(Seq(hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("type"))), None)))))))), Seq())))))))), hydra.ext.python.utils.indentedBlock(None)(Seq(Seq(getItemMethod)))))
  lazy val docStmt: hydra.ext.python.syntax.Statement = hydra.ext.python.utils.pyExpressionToPyStatement(hydra.ext.python.utils.tripleQuotedString(docString))
  lazy val bodyGroups: Seq[Seq[hydra.ext.python.syntax.Statement]] = hydra.lib.logic.ifElse[Seq[Seq[hydra.ext.python.syntax.Statement]]](hydra.lib.lists.`null`[hydra.ext.python.syntax.Statement](extraStmts))({
    lazy val passStmt: hydra.ext.python.syntax.Statement = hydra.ext.python.utils.pySimpleStatementToPyStatement(hydra.ext.python.syntax.SimpleStatement.pass)
    Seq(Seq(docStmt), Seq(passStmt))
  })(Seq(Seq(docStmt), extraStmts))
  lazy val metaclassArg: hydra.ext.python.syntax.Kwarg = hydra.ext.python.syntax.Kwarg("metaclass", hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name(metaName))), None)))))))), Seq()))))))
  lazy val unionClass: hydra.ext.python.syntax.Statement = hydra.ext.python.utils.annotatedStatement(mcomment)(hydra.ext.python.utils.pyClassDefinitionToPyStatement(hydra.ext.python.syntax.ClassDefinition(None, name, Seq(), Some(hydra.ext.python.syntax.Args(Seq(), Seq(hydra.ext.python.syntax.KwargOrStarred.kwarg(metaclassArg)), Seq())), hydra.ext.python.utils.indentedBlock(None)(bodyGroups))))
  Seq(metaClass, unionClass)
}

def unitVariantMethods(className: hydra.ext.python.syntax.Name): Seq[hydra.ext.python.syntax.Statement] =
  {
  lazy val classNameStr: scala.Predef.String = className
  lazy val slotsStmt: hydra.ext.python.syntax.Statement = hydra.ext.python.utils.assignmentStatement("__slots__")(hydra.ext.python.utils.pyPrimaryToPyExpression(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.tuple(Seq()))))
  lazy val returnIsinstance: hydra.ext.python.syntax.Statement = hydra.ext.python.utils.pySimpleStatementToPyStatement(hydra.ext.python.syntax.SimpleStatement.`return`(Seq(hydra.ext.python.syntax.StarExpression.simple(hydra.ext.python.utils.functionCall(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("isinstance")))(Seq(hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("other"))), None)))))))), Seq()))))), hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name(className))), None)))))))), Seq())))))))))))
  lazy val eqMethod: hydra.ext.python.syntax.Statement = hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.function(hydra.ext.python.syntax.FunctionDefinition(None, hydra.ext.python.syntax.FunctionDefRaw(false, "__eq__", Seq(), Some(hydra.ext.python.utils.selfOtherParams), None, None, hydra.ext.python.utils.indentedBlock(None)(Seq(Seq(returnIsinstance)))))))
  lazy val returnHash: hydra.ext.python.syntax.Statement = hydra.ext.python.utils.pySimpleStatementToPyStatement(hydra.ext.python.syntax.SimpleStatement.`return`(Seq(hydra.ext.python.syntax.StarExpression.simple(hydra.ext.python.utils.functionCall(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("hash")))(Seq(hydra.ext.python.utils.doubleQuotedString(classNameStr)))))))
  lazy val hashMethod: hydra.ext.python.syntax.Statement = hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.function(hydra.ext.python.syntax.FunctionDefinition(None, hydra.ext.python.syntax.FunctionDefRaw(false, "__hash__", Seq(), Some(hydra.ext.python.utils.selfOnlyParams), None, None, hydra.ext.python.utils.indentedBlock(None)(Seq(Seq(returnHash)))))))
  Seq(slotsStmt, eqMethod, hashMethod)
}
