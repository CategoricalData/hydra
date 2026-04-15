package hydra.python.utils

import hydra.packaging.*

import hydra.python.environment.*

import hydra.python.syntax.*

def annotatedExpression(mcomment: Option[scala.Predef.String])(expr: hydra.python.syntax.Expression): hydra.python.syntax.Expression =
  hydra.lib.maybes.maybe[hydra.python.syntax.Expression, scala.Predef.String](expr)((c: scala.Predef.String) =>
  hydra.python.utils.pyPrimaryToPyExpression(hydra.python.utils.primaryWithExpressionSlices(hydra.python.utils.pyNameToPyPrimary("Annotated"))(Seq(expr,
     hydra.python.utils.doubleQuotedString(c)))))(mcomment)

def annotatedStatement(mcomment: Option[scala.Predef.String])(stmt: hydra.python.syntax.Statement): hydra.python.syntax.Statement =
  hydra.lib.maybes.maybe[hydra.python.syntax.Statement, scala.Predef.String](stmt)((c: scala.Predef.String) =>
  hydra.python.syntax.Statement.annotated(hydra.python.syntax.AnnotatedStatement(c, stmt)))(mcomment)

def assignment(name: hydra.python.syntax.Name)(rhs: hydra.python.syntax.AnnotatedRhs): hydra.python.syntax.Statement =
  hydra.python.utils.pyAssignmentToPyStatement(hydra.python.syntax.Assignment.untyped(hydra.python.syntax.UntypedAssignment(Seq(hydra.python.utils.pyNameToPyStarTarget(name)),
     rhs, None)))

def assignmentStatement(name: hydra.python.syntax.Name)(expr: hydra.python.syntax.Expression): hydra.python.syntax.Statement =
  hydra.python.utils.assignment(name)(hydra.python.utils.pyExpressionToPyAnnotatedRhs(expr))

def castTo(pytype: hydra.python.syntax.Expression)(pyexpr: hydra.python.syntax.Expression): hydra.python.syntax.Expression =
  hydra.python.utils.functionCall(hydra.python.utils.pyNameToPyPrimary("cast"))(Seq(pytype, pyexpr))

def commentStatement(s: scala.Predef.String): hydra.python.syntax.Statement =
  hydra.python.utils.pyExpressionToPyStatement(hydra.python.utils.tripleQuotedString(s))

def decodePyComparisonToPyAwaitPrimary(c: hydra.python.syntax.Comparison): Option[hydra.python.syntax.Primary] =
  {
  lazy val rhs: Seq[hydra.python.syntax.CompareOpBitwiseOrPair] = (c.rhs)
  lazy val lhs: hydra.python.syntax.BitwiseOr = (c.lhs)
  lazy val orLhs: Option[hydra.python.syntax.BitwiseOr] = (lhs.lhs)
  lazy val orRhs: hydra.python.syntax.BitwiseXor = (lhs.rhs)
  lazy val xorLhs: Option[hydra.python.syntax.BitwiseXor] = (orRhs.lhs)
  lazy val xorRhs: hydra.python.syntax.BitwiseAnd = (orRhs.rhs)
  lazy val andLhs: Option[hydra.python.syntax.BitwiseAnd] = (xorRhs.lhs)
  lazy val andRhs: hydra.python.syntax.ShiftExpression = (xorRhs.rhs)
  lazy val shiftLhs: Option[hydra.python.syntax.ShiftLhs] = (andRhs.lhs)
  lazy val shiftRhs: hydra.python.syntax.Sum = (andRhs.rhs)
  lazy val sumLhs: Option[hydra.python.syntax.SumLhs] = (shiftRhs.lhs)
  lazy val sumRhs: hydra.python.syntax.Term = (shiftRhs.rhs)
  lazy val termLhs: Option[hydra.python.syntax.TermLhs] = (sumRhs.lhs)
  lazy val termRhs: hydra.python.syntax.Factor = (sumRhs.rhs)
  hydra.lib.logic.ifElse[Option[hydra.python.syntax.Primary]](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.python.syntax.CompareOpBitwiseOrPair](rhs)))(None)(hydra.lib.logic.ifElse[Option[hydra.python.syntax.Primary]](hydra.lib.maybes.isJust[hydra.python.syntax.BitwiseOr](orLhs))(None)(hydra.lib.logic.ifElse[Option[hydra.python.syntax.Primary]](hydra.lib.maybes.isJust[hydra.python.syntax.BitwiseXor](xorLhs))(None)(hydra.lib.logic.ifElse[Option[hydra.python.syntax.Primary]](hydra.lib.maybes.isJust[hydra.python.syntax.BitwiseAnd](andLhs))(None)(hydra.lib.logic.ifElse[Option[hydra.python.syntax.Primary]](hydra.lib.maybes.isJust[hydra.python.syntax.ShiftLhs](shiftLhs))(None)(hydra.lib.logic.ifElse[Option[hydra.python.syntax.Primary]](hydra.lib.maybes.isJust[hydra.python.syntax.SumLhs](sumLhs))(None)(hydra.lib.logic.ifElse[Option[hydra.python.syntax.Primary]](hydra.lib.maybes.isJust[hydra.python.syntax.TermLhs](termLhs))(None)(termRhs match
    case hydra.python.syntax.Factor.simple(v_Factor_simple_power) => hydra.python.utils.decodePyPowerToPyPrimary(v_Factor_simple_power)
    case _ => None)))))))
}

def decodePyConjunctionToPyPrimary(c: hydra.python.syntax.Conjunction): Option[hydra.python.syntax.Primary] =
  {
  lazy val inversions: Seq[hydra.python.syntax.Inversion] = c
  hydra.lib.logic.ifElse[Option[hydra.python.syntax.Primary]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.python.syntax.Inversion](inversions))(1))(hydra.python.utils.decodePyInversionToPyPrimary(hydra.lib.lists.head[hydra.python.syntax.Inversion](inversions)))(None)
}

def decodePyExpressionToPyPrimary(e: hydra.python.syntax.Expression): Option[hydra.python.syntax.Primary] =
  e match
  case hydra.python.syntax.Expression.simple(v_Expression_simple_disj) => {
    lazy val conjunctions: Seq[hydra.python.syntax.Conjunction] = v_Expression_simple_disj
    hydra.lib.logic.ifElse[Option[hydra.python.syntax.Primary]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.python.syntax.Conjunction](conjunctions))(1))(hydra.python.utils.decodePyConjunctionToPyPrimary(hydra.lib.lists.head[hydra.python.syntax.Conjunction](conjunctions)))(None)
  }
  case _ => None

def decodePyInversionToPyPrimary(i: hydra.python.syntax.Inversion): Option[hydra.python.syntax.Primary] =
  i match
  case hydra.python.syntax.Inversion.simple(v_Inversion_simple_comparison) => hydra.python.utils.decodePyComparisonToPyAwaitPrimary(v_Inversion_simple_comparison)
  case _ => None

def decodePyPowerToPyPrimary(p: hydra.python.syntax.Power): Option[hydra.python.syntax.Primary] =
  {
  lazy val lhs: hydra.python.syntax.AwaitPrimary = (p.lhs)
  lazy val await: Boolean = (lhs.await)
  lazy val prim: hydra.python.syntax.Primary = (lhs.primary)
  hydra.lib.logic.ifElse[Option[hydra.python.syntax.Primary]](await)(None)(Some(prim))
}

def dottedAssignmentStatement(obj: hydra.python.syntax.Name)(attr: hydra.python.syntax.Name)(expr: hydra.python.syntax.Expression): hydra.python.syntax.Statement =
  {
  lazy val target: hydra.python.syntax.StarTarget = hydra.python.syntax.StarTarget.unstarred(hydra.python.syntax.TargetWithStarAtom.project(hydra.python.syntax.TPrimaryAndName(hydra.python.syntax.TPrimary.atom(hydra.python.syntax.Atom.name(obj)),
     attr)))
  hydra.python.utils.pyAssignmentToPyStatement(hydra.python.syntax.Assignment.untyped(hydra.python.syntax.UntypedAssignment(Seq(target),
     hydra.python.utils.pyExpressionToPyAnnotatedRhs(expr), None)))
}

def doubleQuotedString(s: scala.Predef.String): hydra.python.syntax.Expression =
  hydra.python.utils.stringToPyExpression(hydra.python.syntax.QuoteStyle.double)(s)

def findNamespaces(focusNs: hydra.packaging.Namespace)(defs: Seq[hydra.packaging.Definition]): hydra.packaging.Namespaces[hydra.python.syntax.DottedName] =
  {
  lazy val coreNs: hydra.packaging.Namespace = "hydra.core"
  lazy val namespaces: hydra.packaging.Namespaces[hydra.python.syntax.DottedName] = hydra.analysis.namespacesForDefinitions(hydra.python.names.encodeNamespace)(focusNs)(defs)
  hydra.lib.logic.ifElse[hydra.packaging.Namespaces[hydra.python.syntax.DottedName]](hydra.lib.equality.equal[scala.Predef.String](hydra.lib.pairs.first[hydra.packaging.Namespace,
     hydra.python.syntax.DottedName](namespaces.focus))(coreNs))(namespaces)(hydra.packaging.Namespaces(namespaces.focus,
     hydra.lib.maps.insert[hydra.packaging.Namespace, hydra.python.syntax.DottedName](coreNs)(hydra.python.names.encodeNamespace(coreNs))(namespaces.mapping)))
}

def functionCall(func: hydra.python.syntax.Primary)(args: Seq[hydra.python.syntax.Expression]): hydra.python.syntax.Expression =
  hydra.python.utils.pyPrimaryToPyExpression(hydra.python.utils.primaryWithRhs(func)(hydra.python.syntax.PrimaryRhs.call(hydra.python.utils.pyExpressionsToPyArgs(args))))

lazy val getItemParams: hydra.python.syntax.Parameters = hydra.python.syntax.Parameters.paramNoDefault(hydra.python.syntax.ParamNoDefaultParameters(Seq(hydra.python.syntax.ParamNoDefault(hydra.python.syntax.Param("cls",
   None), None), hydra.python.syntax.ParamNoDefault(hydra.python.syntax.Param("item",
   None), None)), Seq(), None))

def indentedBlock(mcomment: Option[scala.Predef.String])(stmts: Seq[Seq[hydra.python.syntax.Statement]]): hydra.python.syntax.Block =
  {
  lazy val commentGroup: Seq[hydra.python.syntax.Statement] = hydra.lib.maybes.maybe[Seq[hydra.python.syntax.Statement],
     scala.Predef.String](Seq())((s: scala.Predef.String) => Seq(hydra.python.utils.commentStatement(s)))(mcomment)
  lazy val groups: Seq[Seq[hydra.python.syntax.Statement]] = hydra.lib.lists.filter[Seq[hydra.python.syntax.Statement]]((g: Seq[hydra.python.syntax.Statement]) =>
    hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.python.syntax.Statement](g)))(hydra.lib.lists.cons[Seq[hydra.python.syntax.Statement]](commentGroup)(stmts))
  hydra.lib.logic.ifElse[hydra.python.syntax.Block](hydra.lib.lists.`null`[Seq[hydra.python.syntax.Statement]](groups))(hydra.python.syntax.Block.indented(Seq(Seq(hydra.python.syntax.Statement.simple(Seq(hydra.python.utils.pyExpressionToPySimpleStatement(hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.ellipsis))))))))(hydra.python.syntax.Block.indented(groups))
}

def nameAndParams(pyName: hydra.python.syntax.Name)(params: Seq[hydra.python.syntax.Expression]): hydra.python.syntax.Expression =
  hydra.python.utils.primaryAndParams(hydra.python.utils.pyNameToPyPrimary(pyName))(params)

def newtypeStatement(name: hydra.python.syntax.Name)(mcomment: Option[scala.Predef.String])(expr: hydra.python.syntax.Expression): hydra.python.syntax.Statement =
  hydra.python.utils.annotatedStatement(mcomment)(hydra.python.utils.assignmentStatement(name)(hydra.python.utils.functionCall(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("NewType")))(Seq(hydra.python.utils.doubleQuotedString(name),
     expr))))

def orExpression(prims: Seq[hydra.python.syntax.Primary]): hydra.python.syntax.Expression =
  {
  def build(prev: Option[hydra.python.syntax.BitwiseOr])(ps: Seq[hydra.python.syntax.Primary]): hydra.python.syntax.BitwiseOr =
    hydra.lib.logic.ifElse[hydra.python.syntax.BitwiseOr](hydra.lib.lists.`null`[hydra.python.syntax.Primary](hydra.lib.lists.tail[hydra.python.syntax.Primary](ps)))(hydra.python.syntax.BitwiseOr(prev,
       hydra.python.utils.pyPrimaryToPyBitwiseXor(hydra.lib.lists.head[hydra.python.syntax.Primary](ps))))(build(Some(hydra.python.syntax.BitwiseOr(prev,
       hydra.python.utils.pyPrimaryToPyBitwiseXor(hydra.lib.lists.head[hydra.python.syntax.Primary](ps)))))(hydra.lib.lists.tail[hydra.python.syntax.Primary](ps)))
  hydra.python.utils.pyBitwiseOrToPyExpression(build(None)(prims))
}

def primaryAndParams(prim: hydra.python.syntax.Primary)(params: Seq[hydra.python.syntax.Expression]): hydra.python.syntax.Expression =
  hydra.python.utils.pyPrimaryToPyExpression(hydra.python.utils.primaryWithExpressionSlices(prim)(params))

def primaryWithExpressionSlices(prim: hydra.python.syntax.Primary)(exprs: Seq[hydra.python.syntax.Expression]): hydra.python.syntax.Primary =
  hydra.python.utils.primaryWithSlices(prim)(hydra.python.utils.pyExpressionToPySlice(hydra.lib.lists.head[hydra.python.syntax.Expression](exprs)))(hydra.lib.lists.map[hydra.python.syntax.Expression,
     hydra.python.syntax.SliceOrStarredExpression]((e: hydra.python.syntax.Expression) =>
  hydra.python.syntax.SliceOrStarredExpression.slice(hydra.python.utils.pyExpressionToPySlice(e)))(hydra.lib.lists.tail[hydra.python.syntax.Expression](exprs)))

def primaryWithRhs(prim: hydra.python.syntax.Primary)(rhs: hydra.python.syntax.PrimaryRhs): hydra.python.syntax.Primary =
  hydra.python.syntax.Primary.compound(hydra.python.syntax.PrimaryWithRhs(prim, rhs))

def primaryWithSlices(prim: hydra.python.syntax.Primary)(first: hydra.python.syntax.Slice)(rest: Seq[hydra.python.syntax.SliceOrStarredExpression]): hydra.python.syntax.Primary =
  hydra.python.utils.primaryWithRhs(prim)(hydra.python.syntax.PrimaryRhs.slices(hydra.python.syntax.Slices(first,
     rest)))

def projectFromExpression(exp: hydra.python.syntax.Expression)(name: hydra.python.syntax.Name): hydra.python.syntax.Expression =
  {
  lazy val prim: hydra.python.syntax.Primary = hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.group(hydra.python.syntax.Group.expression(hydra.python.syntax.NamedExpression.simple(exp))))
  hydra.python.utils.pyPrimaryToPyExpression(hydra.python.syntax.Primary.compound(hydra.python.syntax.PrimaryWithRhs(prim,
     hydra.python.syntax.PrimaryRhs.project(name))))
}

def pyAssignmentToPyStatement(a: hydra.python.syntax.Assignment): hydra.python.syntax.Statement =
  hydra.python.utils.pySimpleStatementToPyStatement(hydra.python.syntax.SimpleStatement.assignment(a))

def pyAtomToPyExpression(atom: hydra.python.syntax.Atom): hydra.python.syntax.Expression =
  hydra.python.utils.pyPrimaryToPyExpression(hydra.python.syntax.Primary.simple(atom))

def pyBitwiseOrToPyConjunction(bor: hydra.python.syntax.BitwiseOr): hydra.python.syntax.Conjunction =
  Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(bor, Seq())))

def pyBitwiseOrToPyExpression(bor: hydra.python.syntax.BitwiseOr): hydra.python.syntax.Expression =
  hydra.python.utils.pyConjunctionToPyExpression(hydra.python.utils.pyBitwiseOrToPyConjunction(bor))

def pyClassDefinitionToPyStatement(cd: hydra.python.syntax.ClassDefinition): hydra.python.syntax.Statement =
  hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.classDef(cd))

def pyClosedPatternToPyPatterns(p: hydra.python.syntax.ClosedPattern): hydra.python.syntax.Patterns = hydra.python.syntax.Patterns.pattern(hydra.python.syntax.Pattern.or(Seq(p)))

def pyConjunctionToPyExpression(conj: hydra.python.syntax.Conjunction): hydra.python.syntax.Expression = hydra.python.syntax.Expression.simple(Seq(conj))

def pyExpressionToBitwiseOr(e: hydra.python.syntax.Expression): hydra.python.syntax.BitwiseOr =
  hydra.python.syntax.BitwiseOr(None, hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None,
     hydra.python.syntax.ShiftExpression(None, hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None,
     hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
     hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.group(hydra.python.syntax.Group.expression(hydra.python.syntax.NamedExpression.simple(e))))),
     None))))))))

def pyExpressionToDisjunction(e: hydra.python.syntax.Expression): hydra.python.syntax.Disjunction =
  e match
  case hydra.python.syntax.Expression.simple(v_Expression_simple_disj) => v_Expression_simple_disj
  case _ => Seq(hydra.python.utils.pyPrimaryToPyConjunction(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.group(hydra.python.syntax.Group.expression(hydra.python.syntax.NamedExpression.simple(e))))))

def pyExpressionToPyAnnotatedRhs(expr: hydra.python.syntax.Expression): hydra.python.syntax.AnnotatedRhs =
  hydra.python.syntax.AnnotatedRhs.star(Seq(hydra.python.syntax.StarExpression.simple(expr)))

def pyExpressionToPyPrimary(e: hydra.python.syntax.Expression): hydra.python.syntax.Primary =
  hydra.lib.maybes.maybe[hydra.python.syntax.Primary, hydra.python.syntax.Primary](hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.group(hydra.python.syntax.Group.expression(hydra.python.syntax.NamedExpression.simple(e)))))((prim: hydra.python.syntax.Primary) => prim)(hydra.python.utils.decodePyExpressionToPyPrimary(e))

def pyExpressionToPySimpleStatement(expr: hydra.python.syntax.Expression): hydra.python.syntax.SimpleStatement =
  hydra.python.syntax.SimpleStatement.starExpressions(Seq(hydra.python.syntax.StarExpression.simple(expr)))

def pyExpressionToPySlice(expr: hydra.python.syntax.Expression): hydra.python.syntax.Slice =
  hydra.python.syntax.Slice.named(hydra.python.syntax.NamedExpression.simple(expr))

def pyExpressionToPyStarNamedExpression(expr: hydra.python.syntax.Expression): hydra.python.syntax.StarNamedExpression =
  hydra.python.syntax.StarNamedExpression.simple(hydra.python.syntax.NamedExpression.simple(expr))

def pyExpressionToPyStatement(expr: hydra.python.syntax.Expression): hydra.python.syntax.Statement =
  hydra.python.utils.pySimpleStatementToPyStatement(hydra.python.utils.pyExpressionToPySimpleStatement(expr))

def pyExpressionsToPyArgs(exprs: Seq[hydra.python.syntax.Expression]): hydra.python.syntax.Args =
  hydra.python.syntax.Args(hydra.lib.lists.map[hydra.python.syntax.Expression, hydra.python.syntax.PosArg]((e: hydra.python.syntax.Expression) => hydra.python.syntax.PosArg.expression(e))(exprs),
     Seq(), Seq())

def pyList(exprs: Seq[hydra.python.syntax.Expression]): hydra.python.syntax.List =
  hydra.lib.lists.map[hydra.python.syntax.Expression, hydra.python.syntax.StarNamedExpression](hydra.python.utils.pyExpressionToPyStarNamedExpression)(exprs)

def pyNameToPyExpression(name: hydra.python.syntax.Name): hydra.python.syntax.Expression =
  hydra.python.utils.pyPrimaryToPyExpression(hydra.python.utils.pyNameToPyPrimary(name))

def pyNameToPyNamedExpression(name: hydra.python.syntax.Name): hydra.python.syntax.NamedExpression =
  hydra.python.syntax.NamedExpression.simple(hydra.python.utils.pyNameToPyExpression(name))

def pyNameToPyPrimary(name: hydra.python.syntax.Name): hydra.python.syntax.Primary = hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name(name))

def pyNameToPyStarTarget(name: hydra.python.syntax.Name): hydra.python.syntax.StarTarget =
  hydra.python.syntax.StarTarget.unstarred(hydra.python.syntax.TargetWithStarAtom.atom(hydra.python.syntax.StarAtom.name(name)))

def pyNameToPyTypeParameter(name: hydra.python.syntax.Name): hydra.python.syntax.TypeParameter =
  hydra.python.syntax.TypeParameter.simple(hydra.python.syntax.SimpleTypeParameter(name, None, None))

lazy val pyNone: hydra.python.syntax.Name = "None"

def pyPrimaryToPyBitwiseOr(prim: hydra.python.syntax.Primary): hydra.python.syntax.BitwiseOr =
  hydra.python.syntax.BitwiseOr(None, hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None,
     hydra.python.syntax.ShiftExpression(None, hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None,
     hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
     prim), None))))))))

def pyPrimaryToPyBitwiseXor(prim: hydra.python.syntax.Primary): hydra.python.syntax.BitwiseXor =
  hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None, hydra.python.syntax.ShiftExpression(None,
     hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
     prim), None)))))))

def pyPrimaryToPyConjunction(prim: hydra.python.syntax.Primary): hydra.python.syntax.Conjunction =
  hydra.python.utils.pyBitwiseOrToPyConjunction(hydra.python.utils.pyPrimaryToPyBitwiseOr(prim))

def pyPrimaryToPyExpression(prim: hydra.python.syntax.Primary): hydra.python.syntax.Expression =
  hydra.python.utils.pyConjunctionToPyExpression(hydra.python.utils.pyPrimaryToPyConjunction(prim))

def pyPrimaryToPySlice(prim: hydra.python.syntax.Primary): hydra.python.syntax.Slice =
  hydra.python.utils.pyExpressionToPySlice(hydra.python.utils.pyPrimaryToPyExpression(prim))

def pySimpleStatementToPyStatement(s: hydra.python.syntax.SimpleStatement): hydra.python.syntax.Statement = hydra.python.syntax.Statement.simple(Seq(s))

def raiseAssertionError(msg: scala.Predef.String): hydra.python.syntax.Statement =
  hydra.python.utils.pySimpleStatementToPyStatement(hydra.python.syntax.SimpleStatement.raise(Some(hydra.python.syntax.RaiseExpression(hydra.python.utils.functionCall(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("AssertionError")))(Seq(hydra.python.utils.doubleQuotedString(msg))),
     None))))

def raiseTypeError(msg: scala.Predef.String): hydra.python.syntax.Statement =
  hydra.python.utils.pySimpleStatementToPyStatement(hydra.python.syntax.SimpleStatement.raise(Some(hydra.python.syntax.RaiseExpression(hydra.python.utils.functionCall(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("TypeError")))(Seq(hydra.python.utils.doubleQuotedString(msg))),
     None))))

def returnSingle(expr: hydra.python.syntax.Expression): hydra.python.syntax.Statement =
  hydra.python.utils.pySimpleStatementToPyStatement(hydra.python.syntax.SimpleStatement.`return`(Seq(hydra.python.syntax.StarExpression.simple(expr))))

lazy val selfOnlyParams: hydra.python.syntax.Parameters = hydra.python.syntax.Parameters.paramNoDefault(hydra.python.syntax.ParamNoDefaultParameters(Seq(hydra.python.syntax.ParamNoDefault(hydra.python.syntax.Param("self",
   None), None)), Seq(), None))

lazy val selfOtherParams: hydra.python.syntax.Parameters = hydra.python.syntax.Parameters.paramNoDefault(hydra.python.syntax.ParamNoDefaultParameters(Seq(hydra.python.syntax.ParamNoDefault(hydra.python.syntax.Param("self",
   None), None), hydra.python.syntax.ParamNoDefault(hydra.python.syntax.Param("other",
   None), None)), Seq(), None))

def singleQuotedString(s: scala.Predef.String): hydra.python.syntax.Expression =
  hydra.python.utils.stringToPyExpression(hydra.python.syntax.QuoteStyle.single)(s)

def stringToPyExpression(style: hydra.python.syntax.QuoteStyle)(s: scala.Predef.String): hydra.python.syntax.Expression =
  hydra.python.utils.pyAtomToPyExpression(hydra.python.syntax.Atom.string(hydra.python.syntax.String(s, style)))

lazy val targetPythonVersion: hydra.python.environment.PythonVersion = hydra.python.environment.PythonVersion.python310

def tripleQuotedString(s: scala.Predef.String): hydra.python.syntax.Expression =
  hydra.python.utils.stringToPyExpression(hydra.python.syntax.QuoteStyle.triple)(s)

def typeAliasStatement(name: hydra.python.syntax.Name)(tparams: Seq[hydra.python.syntax.TypeParameter])(mcomment: Option[scala.Predef.String])(tyexpr: hydra.python.syntax.Expression): hydra.python.syntax.Statement =
  hydra.python.utils.annotatedStatement(mcomment)(hydra.python.utils.pySimpleStatementToPyStatement(hydra.python.syntax.SimpleStatement.typeAlias(hydra.python.syntax.TypeAlias(name,
     tparams, tyexpr))))

def typeAliasStatement310[T0](name: hydra.python.syntax.Name)(_tparams: T0)(mcomment: Option[scala.Predef.String])(tyexpr: hydra.python.syntax.Expression): hydra.python.syntax.Statement =
  {
  lazy val quotedExpr: hydra.python.syntax.Expression = hydra.python.utils.doubleQuotedString(hydra.serialization.printExpr(hydra.python.serde.encodeExpression(tyexpr)))
  hydra.python.utils.annotatedStatement(mcomment)(hydra.python.utils.pyAssignmentToPyStatement(hydra.python.syntax.Assignment.typed(hydra.python.syntax.TypedAssignment(hydra.python.syntax.SingleTarget.name(name),
     hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
     hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None, hydra.python.syntax.ShiftExpression(None,
     hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
     hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("TypeAlias"))),
     None)))))))), Seq()))))), Some(hydra.python.utils.pyExpressionToPyAnnotatedRhs(quotedExpr))))))
}

def unionTypeClassStatements310(name: hydra.python.syntax.Name)(mcomment: Option[scala.Predef.String])(tyexpr: hydra.python.syntax.Expression)(extraStmts: Seq[hydra.python.syntax.Statement]): Seq[hydra.python.syntax.Statement] =
  {
  lazy val nameStr: scala.Predef.String = name
  lazy val metaName: hydra.python.syntax.Name = hydra.lib.strings.cat2(hydra.lib.strings.cat2("_")(nameStr))("Meta")
  lazy val docString: scala.Predef.String = hydra.serialization.printExpr(hydra.python.serde.encodeExpression(tyexpr))
  lazy val returnObject: hydra.python.syntax.Statement = hydra.python.utils.pySimpleStatementToPyStatement(hydra.python.syntax.SimpleStatement.`return`(Seq(hydra.python.syntax.StarExpression.simple(hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
     hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None, hydra.python.syntax.ShiftExpression(None,
     hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
     hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("object"))),
     None)))))))), Seq())))))))))
  lazy val getItemMethod: hydra.python.syntax.Statement = hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.function(hydra.python.syntax.FunctionDefinition(None,
     hydra.python.syntax.FunctionDefRaw(false, "__getitem__", Seq(), Some(hydra.python.utils.getItemParams),
     None, None, hydra.python.utils.indentedBlock(None)(Seq(Seq(returnObject)))))))
  lazy val metaClass: hydra.python.syntax.Statement = hydra.python.utils.pyClassDefinitionToPyStatement(hydra.python.syntax.ClassDefinition(None,
     metaName, Seq(), Some(hydra.python.utils.pyExpressionsToPyArgs(Seq(hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
     hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None, hydra.python.syntax.ShiftExpression(None,
     hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
     hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("type"))), None)))))))),
     Seq())))))))), hydra.python.utils.indentedBlock(None)(Seq(Seq(getItemMethod)))))
  lazy val docStmt: hydra.python.syntax.Statement = hydra.python.utils.pyExpressionToPyStatement(hydra.python.utils.tripleQuotedString(docString))
  lazy val bodyGroups: Seq[Seq[hydra.python.syntax.Statement]] = hydra.lib.logic.ifElse[Seq[Seq[hydra.python.syntax.Statement]]](hydra.lib.lists.`null`[hydra.python.syntax.Statement](extraStmts))({
    lazy val passStmt: hydra.python.syntax.Statement = hydra.python.utils.pySimpleStatementToPyStatement(hydra.python.syntax.SimpleStatement.pass)
    Seq(Seq(docStmt), Seq(passStmt))
  })(Seq(Seq(docStmt), extraStmts))
  lazy val metaclassArg: hydra.python.syntax.Kwarg = hydra.python.syntax.Kwarg("metaclass",
     hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
     hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None, hydra.python.syntax.ShiftExpression(None,
     hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
     hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name(metaName))),
     None)))))))), Seq()))))))
  lazy val unionClass: hydra.python.syntax.Statement = hydra.python.utils.annotatedStatement(mcomment)(hydra.python.utils.pyClassDefinitionToPyStatement(hydra.python.syntax.ClassDefinition(None,
     name, Seq(), Some(hydra.python.syntax.Args(Seq(), Seq(hydra.python.syntax.KwargOrStarred.kwarg(metaclassArg)),
     Seq())), hydra.python.utils.indentedBlock(None)(bodyGroups))))
  Seq(metaClass, unionClass)
}

def unitVariantMethods(className: hydra.python.syntax.Name): Seq[hydra.python.syntax.Statement] =
  {
  lazy val classNameStr: scala.Predef.String = className
  lazy val slotsStmt: hydra.python.syntax.Statement = hydra.python.utils.assignmentStatement("__slots__")(hydra.python.utils.pyPrimaryToPyExpression(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.tuple(Seq()))))
  lazy val returnIsinstance: hydra.python.syntax.Statement = hydra.python.utils.pySimpleStatementToPyStatement(hydra.python.syntax.SimpleStatement.`return`(Seq(hydra.python.syntax.StarExpression.simple(hydra.python.utils.functionCall(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("isinstance")))(Seq(hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
     hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None, hydra.python.syntax.ShiftExpression(None,
     hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
     hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("other"))),
     None)))))))), Seq()))))), hydra.python.syntax.Expression.simple(Seq(Seq(hydra.python.syntax.Inversion.simple(hydra.python.syntax.Comparison(hydra.python.syntax.BitwiseOr(None,
     hydra.python.syntax.BitwiseXor(None, hydra.python.syntax.BitwiseAnd(None, hydra.python.syntax.ShiftExpression(None,
     hydra.python.syntax.Sum(None, hydra.python.syntax.Term(None, hydra.python.syntax.Factor.simple(hydra.python.syntax.Power(hydra.python.syntax.AwaitPrimary(false,
     hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name(className))),
     None)))))))), Seq())))))))))))
  lazy val eqMethod: hydra.python.syntax.Statement = hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.function(hydra.python.syntax.FunctionDefinition(None,
     hydra.python.syntax.FunctionDefRaw(false, "__eq__", Seq(), Some(hydra.python.utils.selfOtherParams),
     None, None, hydra.python.utils.indentedBlock(None)(Seq(Seq(returnIsinstance)))))))
  lazy val returnHash: hydra.python.syntax.Statement = hydra.python.utils.pySimpleStatementToPyStatement(hydra.python.syntax.SimpleStatement.`return`(Seq(hydra.python.syntax.StarExpression.simple(hydra.python.utils.functionCall(hydra.python.syntax.Primary.simple(hydra.python.syntax.Atom.name("hash")))(Seq(hydra.python.utils.doubleQuotedString(classNameStr)))))))
  lazy val hashMethod: hydra.python.syntax.Statement = hydra.python.syntax.Statement.compound(hydra.python.syntax.CompoundStatement.function(hydra.python.syntax.FunctionDefinition(None,
     hydra.python.syntax.FunctionDefRaw(false, "__hash__", Seq(), Some(hydra.python.utils.selfOnlyParams),
     None, None, hydra.python.utils.indentedBlock(None)(Seq(Seq(returnHash)))))))
  Seq(slotsStmt, eqMethod, hashMethod)
}
