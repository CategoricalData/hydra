// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.utils;

/**
 * Python utilities for constructing Python syntax trees
 */
public interface Utils {
  static hydra.ext.python.helpers.PythonVersion targetPythonVersion() {
    return new hydra.ext.python.helpers.PythonVersion.Python310();
  }
  
  static hydra.ext.python.syntax.Name pyNone() {
    return new hydra.ext.python.syntax.Name("None");
  }
  
  static hydra.ext.python.syntax.Primary pyNameToPyPrimary(hydra.ext.python.syntax.Name name) {
    return new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(name));
  }
  
  static hydra.ext.python.syntax.BitwiseXor pyPrimaryToPyBitwiseXor(hydra.ext.python.syntax.Primary prim) {
    return new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, prim), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))));
  }
  
  static hydra.ext.python.syntax.BitwiseOr pyPrimaryToPyBitwiseOr(hydra.ext.python.syntax.Primary prim) {
    return new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, prim), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing())))))))));
  }
  
  static hydra.ext.python.syntax.Conjunction pyBitwiseOrToPyConjunction(hydra.ext.python.syntax.BitwiseOr bor) {
    return new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(bor, (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of())))));
  }
  
  static hydra.ext.python.syntax.Conjunction pyPrimaryToPyConjunction(hydra.ext.python.syntax.Primary prim) {
    return hydra.ext.python.utils.Utils.pyBitwiseOrToPyConjunction(hydra.ext.python.utils.Utils.pyPrimaryToPyBitwiseOr(prim));
  }
  
  static hydra.ext.python.syntax.Expression pyConjunctionToPyExpression(hydra.ext.python.syntax.Conjunction conj) {
    return new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(conj)));
  }
  
  static hydra.ext.python.syntax.Expression pyPrimaryToPyExpression(hydra.ext.python.syntax.Primary prim) {
    return hydra.ext.python.utils.Utils.pyConjunctionToPyExpression(hydra.ext.python.utils.Utils.pyPrimaryToPyConjunction(prim));
  }
  
  static hydra.ext.python.syntax.Expression pyAtomToPyExpression(hydra.ext.python.syntax.Atom atom) {
    return hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(new hydra.ext.python.syntax.Primary.Simple(atom));
  }
  
  static hydra.ext.python.syntax.Expression pyNameToPyExpression(hydra.ext.python.syntax.Name name) {
    return hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(hydra.ext.python.utils.Utils.pyNameToPyPrimary(name));
  }
  
  static hydra.ext.python.syntax.Statement pySimpleStatementToPyStatement(hydra.ext.python.syntax.SimpleStatement s) {
    return new hydra.ext.python.syntax.Statement.Simple(java.util.List.of(s));
  }
  
  static hydra.ext.python.syntax.SimpleStatement pyExpressionToPySimpleStatement(hydra.ext.python.syntax.Expression expr) {
    return new hydra.ext.python.syntax.SimpleStatement.StarExpressions(java.util.List.of(new hydra.ext.python.syntax.StarExpression.Simple(expr)));
  }
  
  static hydra.ext.python.syntax.Statement pyExpressionToPyStatement(hydra.ext.python.syntax.Expression expr) {
    return hydra.ext.python.utils.Utils.pySimpleStatementToPyStatement(hydra.ext.python.utils.Utils.pyExpressionToPySimpleStatement(expr));
  }
  
  static hydra.ext.python.syntax.AnnotatedRhs pyExpressionToPyAnnotatedRhs(hydra.ext.python.syntax.Expression expr) {
    return new hydra.ext.python.syntax.AnnotatedRhs.Star(java.util.List.of(new hydra.ext.python.syntax.StarExpression.Simple(expr)));
  }
  
  static hydra.ext.python.syntax.Slice pyExpressionToPySlice(hydra.ext.python.syntax.Expression expr) {
    return new hydra.ext.python.syntax.Slice.Named(new hydra.ext.python.syntax.NamedExpression.Simple(expr));
  }
  
  static hydra.ext.python.syntax.StarNamedExpression pyExpressionToPyStarNamedExpression(hydra.ext.python.syntax.Expression expr) {
    return new hydra.ext.python.syntax.StarNamedExpression.Simple(new hydra.ext.python.syntax.NamedExpression.Simple(expr));
  }
  
  static hydra.ext.python.syntax.Args pyExpressionsToPyArgs(java.util.List<hydra.ext.python.syntax.Expression> exprs) {
    return new hydra.ext.python.syntax.Args(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.PosArg>) (e -> new hydra.ext.python.syntax.PosArg.Expression(e)),
      exprs), (java.util.List<hydra.ext.python.syntax.KwargOrStarred>) (java.util.List.<hydra.ext.python.syntax.KwargOrStarred>of()), (java.util.List<hydra.ext.python.syntax.KwargOrDoubleStarred>) (java.util.List.<hydra.ext.python.syntax.KwargOrDoubleStarred>of()));
  }
  
  static hydra.ext.python.syntax.StarTarget pyNameToPyStarTarget(hydra.ext.python.syntax.Name name) {
    return new hydra.ext.python.syntax.StarTarget.Unstarred(new hydra.ext.python.syntax.TargetWithStarAtom.Atom(new hydra.ext.python.syntax.StarAtom.Name(name)));
  }
  
  static hydra.ext.python.syntax.TypeParameter pyNameToPyTypeParameter(hydra.ext.python.syntax.Name name) {
    return new hydra.ext.python.syntax.TypeParameter.Simple(new hydra.ext.python.syntax.SimpleTypeParameter(name, (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing())));
  }
  
  static hydra.ext.python.syntax.NamedExpression pyNameToPyNamedExpression(hydra.ext.python.syntax.Name name) {
    return new hydra.ext.python.syntax.NamedExpression.Simple(hydra.ext.python.utils.Utils.pyNameToPyExpression(name));
  }
  
  static hydra.ext.python.syntax.Statement pyAssignmentToPyStatement(hydra.ext.python.syntax.Assignment a) {
    return hydra.ext.python.utils.Utils.pySimpleStatementToPyStatement(new hydra.ext.python.syntax.SimpleStatement.Assignment(a));
  }
  
  static hydra.ext.python.syntax.Statement pyClassDefinitionToPyStatement(hydra.ext.python.syntax.ClassDefinition cd) {
    return new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.ClassDef(cd));
  }
  
  static hydra.ext.python.syntax.Patterns pyClosedPatternToPyPatterns(hydra.ext.python.syntax.ClosedPattern p) {
    return new hydra.ext.python.syntax.Patterns.Pattern(new hydra.ext.python.syntax.Pattern.Or(new hydra.ext.python.syntax.OrPattern(java.util.List.of(p))));
  }
  
  static hydra.ext.python.syntax.Primary primaryWithRhs(hydra.ext.python.syntax.Primary prim, hydra.ext.python.syntax.PrimaryRhs rhs) {
    return new hydra.ext.python.syntax.Primary.Compound(new hydra.ext.python.syntax.PrimaryWithRhs(prim, rhs));
  }
  
  static hydra.ext.python.syntax.Primary primaryWithSlices(hydra.ext.python.syntax.Primary prim, hydra.ext.python.syntax.Slice first, java.util.List<hydra.ext.python.syntax.SliceOrStarredExpression> rest) {
    return hydra.ext.python.utils.Utils.primaryWithRhs(
      prim,
      new hydra.ext.python.syntax.PrimaryRhs.Slices(new hydra.ext.python.syntax.Slices(first, rest)));
  }
  
  static hydra.ext.python.syntax.Primary primaryWithExpressionSlices(hydra.ext.python.syntax.Primary prim, java.util.List<hydra.ext.python.syntax.Expression> exprs) {
    return hydra.ext.python.utils.Utils.primaryWithSlices(
      prim,
      hydra.ext.python.utils.Utils.pyExpressionToPySlice(hydra.lib.lists.Head.apply(exprs)),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.SliceOrStarredExpression>) (e -> new hydra.ext.python.syntax.SliceOrStarredExpression.Slice(hydra.ext.python.utils.Utils.pyExpressionToPySlice(e))),
        hydra.lib.lists.Tail.apply(exprs)));
  }
  
  static hydra.ext.python.syntax.Expression functionCall(hydra.ext.python.syntax.Primary func, java.util.List<hydra.ext.python.syntax.Expression> args) {
    return hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(hydra.ext.python.utils.Utils.primaryWithRhs(
      func,
      new hydra.ext.python.syntax.PrimaryRhs.Call(hydra.ext.python.utils.Utils.pyExpressionsToPyArgs(args))));
  }
  
  static hydra.ext.python.syntax.Expression primaryAndParams(hydra.ext.python.syntax.Primary prim, java.util.List<hydra.ext.python.syntax.Expression> params) {
    return hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(hydra.ext.python.utils.Utils.primaryWithExpressionSlices(
      prim,
      params));
  }
  
  static hydra.ext.python.syntax.Expression nameAndParams(hydra.ext.python.syntax.Name pyName, java.util.List<hydra.ext.python.syntax.Expression> params) {
    return hydra.ext.python.utils.Utils.primaryAndParams(
      hydra.ext.python.utils.Utils.pyNameToPyPrimary(pyName),
      params);
  }
  
  static hydra.ext.python.syntax.Expression stringToPyExpression(hydra.ext.python.syntax.QuoteStyle style, String s) {
    return hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.String_(new hydra.ext.python.syntax.String_(s, style)));
  }
  
  static hydra.ext.python.syntax.Expression singleQuotedString(String s) {
    return hydra.ext.python.utils.Utils.stringToPyExpression(
      new hydra.ext.python.syntax.QuoteStyle.Single(),
      s);
  }
  
  static hydra.ext.python.syntax.Expression doubleQuotedString(String s) {
    return hydra.ext.python.utils.Utils.stringToPyExpression(
      new hydra.ext.python.syntax.QuoteStyle.Double_(),
      s);
  }
  
  static hydra.ext.python.syntax.Expression tripleQuotedString(String s) {
    return hydra.ext.python.utils.Utils.stringToPyExpression(
      new hydra.ext.python.syntax.QuoteStyle.Triple(),
      s);
  }
  
  static hydra.ext.python.syntax.Statement assignment(hydra.ext.python.syntax.Name name, hydra.ext.python.syntax.AnnotatedRhs rhs) {
    return hydra.ext.python.utils.Utils.pyAssignmentToPyStatement(new hydra.ext.python.syntax.Assignment.Untyped(new hydra.ext.python.syntax.UntypedAssignment(java.util.List.of(hydra.ext.python.utils.Utils.pyNameToPyStarTarget(name)), rhs, (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing()))));
  }
  
  static hydra.ext.python.syntax.Statement assignmentStatement(hydra.ext.python.syntax.Name name, hydra.ext.python.syntax.Expression expr) {
    return hydra.ext.python.utils.Utils.assignment(
      name,
      hydra.ext.python.utils.Utils.pyExpressionToPyAnnotatedRhs(expr));
  }
  
  static hydra.ext.python.syntax.Statement returnSingle(hydra.ext.python.syntax.Expression expr) {
    return hydra.ext.python.utils.Utils.pySimpleStatementToPyStatement(new hydra.ext.python.syntax.SimpleStatement.Return(new hydra.ext.python.syntax.ReturnStatement(java.util.List.of(new hydra.ext.python.syntax.StarExpression.Simple(expr)))));
  }
  
  static hydra.ext.python.syntax.Expression castTo(hydra.ext.python.syntax.Expression pytype, hydra.ext.python.syntax.Expression pyexpr) {
    return hydra.ext.python.utils.Utils.functionCall(
      hydra.ext.python.utils.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("cast")),
      java.util.List.of(
        pytype,
        pyexpr));
  }
  
  static hydra.ext.python.syntax.Expression projectFromExpression(hydra.ext.python.syntax.Expression exp, hydra.ext.python.syntax.Name name) {
    hydra.ext.python.syntax.Primary prim = new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Group(new hydra.ext.python.syntax.Group.Expression(new hydra.ext.python.syntax.NamedExpression.Simple(exp))));
    return hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(new hydra.ext.python.syntax.Primary.Compound(new hydra.ext.python.syntax.PrimaryWithRhs(prim, new hydra.ext.python.syntax.PrimaryRhs.Project(name))));
  }
  
  static hydra.ext.python.syntax.Statement annotatedStatement(hydra.util.Maybe<String> mcomment, hydra.ext.python.syntax.Statement stmt) {
    return hydra.lib.maybes.Maybe.apply(
      stmt,
      (java.util.function.Function<String, hydra.ext.python.syntax.Statement>) (c -> new hydra.ext.python.syntax.Statement.Annotated(new hydra.ext.python.syntax.AnnotatedStatement(c, stmt))),
      mcomment);
  }
  
  static hydra.ext.python.syntax.Expression annotatedExpression(hydra.util.Maybe<String> mcomment, hydra.ext.python.syntax.Expression expr) {
    return hydra.lib.maybes.Maybe.apply(
      expr,
      (java.util.function.Function<String, hydra.ext.python.syntax.Expression>) (c -> hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(hydra.ext.python.utils.Utils.primaryWithExpressionSlices(
        hydra.ext.python.utils.Utils.pyNameToPyPrimary(new hydra.ext.python.syntax.Name("Annotated")),
        java.util.List.of(
          expr,
          hydra.ext.python.utils.Utils.doubleQuotedString(c))))),
      mcomment);
  }
  
  static hydra.ext.python.syntax.Statement commentStatement(String s) {
    return hydra.ext.python.utils.Utils.pyExpressionToPyStatement(hydra.ext.python.utils.Utils.tripleQuotedString(s));
  }
  
  static hydra.ext.python.syntax.Statement raiseAssertionError(String msg) {
    return hydra.ext.python.utils.Utils.pySimpleStatementToPyStatement(new hydra.ext.python.syntax.SimpleStatement.Raise(new hydra.ext.python.syntax.RaiseStatement(hydra.util.Maybe.just(new hydra.ext.python.syntax.RaiseExpression(hydra.ext.python.utils.Utils.functionCall(
      new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("AssertionError"))),
      java.util.List.of(hydra.ext.python.utils.Utils.doubleQuotedString(msg))), (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()))))));
  }
  
  static hydra.ext.python.syntax.Statement raiseTypeError(String msg) {
    return hydra.ext.python.utils.Utils.pySimpleStatementToPyStatement(new hydra.ext.python.syntax.SimpleStatement.Raise(new hydra.ext.python.syntax.RaiseStatement(hydra.util.Maybe.just(new hydra.ext.python.syntax.RaiseExpression(hydra.ext.python.utils.Utils.functionCall(
      new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("TypeError"))),
      java.util.List.of(hydra.ext.python.utils.Utils.doubleQuotedString(msg))), (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()))))));
  }
  
  static hydra.ext.python.syntax.Statement newtypeStatement(hydra.ext.python.syntax.Name name, hydra.util.Maybe<String> mcomment, hydra.ext.python.syntax.Expression expr) {
    return hydra.ext.python.utils.Utils.annotatedStatement(
      mcomment,
      hydra.ext.python.utils.Utils.assignmentStatement(
        name,
        hydra.ext.python.utils.Utils.functionCall(
          new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("NewType"))),
          java.util.List.of(
            hydra.ext.python.utils.Utils.doubleQuotedString((name).value),
            expr))));
  }
  
  static hydra.ext.python.syntax.Statement typeAliasStatement(hydra.ext.python.syntax.Name name, java.util.List<hydra.ext.python.syntax.TypeParameter> tparams, hydra.util.Maybe<String> mcomment, hydra.ext.python.syntax.Expression tyexpr) {
    return hydra.ext.python.utils.Utils.annotatedStatement(
      mcomment,
      hydra.ext.python.utils.Utils.pySimpleStatementToPyStatement(new hydra.ext.python.syntax.SimpleStatement.TypeAlias(new hydra.ext.python.syntax.TypeAlias(name, tparams, tyexpr))));
  }
  
  static hydra.ext.python.syntax.List pyList(java.util.List<hydra.ext.python.syntax.Expression> exprs) {
    return new hydra.ext.python.syntax.List(hydra.lib.lists.Map.apply(
      hydra.ext.python.utils.Utils::pyExpressionToPyStarNamedExpression,
      exprs));
  }
  
  static hydra.util.Maybe<hydra.ext.python.syntax.Primary> decodePyPowerToPyPrimary(hydra.ext.python.syntax.Power p) {
    hydra.ext.python.syntax.AwaitPrimary lhs = (p).lhs;
    Boolean await = (lhs).await;
    hydra.ext.python.syntax.Primary prim = (lhs).primary;
    return hydra.lib.logic.IfElse.lazy(
      await,
      () -> (hydra.util.Maybe<hydra.ext.python.syntax.Primary>) (hydra.util.Maybe.<hydra.ext.python.syntax.Primary>nothing()),
      () -> hydra.util.Maybe.just(prim));
  }
  
  static hydra.util.Maybe<hydra.ext.python.syntax.Primary> decodePyComparisonToPyAwaitPrimary(hydra.ext.python.syntax.Comparison c) {
    hydra.ext.python.syntax.BitwiseOr lhs = (c).lhs;
    hydra.ext.python.syntax.BitwiseXor orRhs = (lhs).rhs;
    hydra.ext.python.syntax.BitwiseAnd xorRhs = (orRhs).rhs;
    hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd> andLhs = (xorRhs).lhs;
    hydra.ext.python.syntax.ShiftExpression andRhs = (xorRhs).rhs;
    hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr> orLhs = (lhs).lhs;
    java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair> rhs = (c).rhs;
    hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs> shiftLhs = (andRhs).lhs;
    hydra.ext.python.syntax.Sum shiftRhs = (andRhs).rhs;
    hydra.util.Maybe<hydra.ext.python.syntax.SumLhs> sumLhs = (shiftRhs).lhs;
    hydra.ext.python.syntax.Term sumRhs = (shiftRhs).rhs;
    hydra.util.Maybe<hydra.ext.python.syntax.TermLhs> termLhs = (sumRhs).lhs;
    hydra.ext.python.syntax.Factor termRhs = (sumRhs).rhs;
    hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor> xorLhs = (orRhs).lhs;
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(rhs)),
      () -> (hydra.util.Maybe<hydra.ext.python.syntax.Primary>) (hydra.util.Maybe.<hydra.ext.python.syntax.Primary>nothing()),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.maybes.IsJust.apply(orLhs),
        () -> (hydra.util.Maybe<hydra.ext.python.syntax.Primary>) (hydra.util.Maybe.<hydra.ext.python.syntax.Primary>nothing()),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.maybes.IsJust.apply(xorLhs),
          () -> (hydra.util.Maybe<hydra.ext.python.syntax.Primary>) (hydra.util.Maybe.<hydra.ext.python.syntax.Primary>nothing()),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.maybes.IsJust.apply(andLhs),
            () -> (hydra.util.Maybe<hydra.ext.python.syntax.Primary>) (hydra.util.Maybe.<hydra.ext.python.syntax.Primary>nothing()),
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.maybes.IsJust.apply(shiftLhs),
              () -> (hydra.util.Maybe<hydra.ext.python.syntax.Primary>) (hydra.util.Maybe.<hydra.ext.python.syntax.Primary>nothing()),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.maybes.IsJust.apply(sumLhs),
                () -> (hydra.util.Maybe<hydra.ext.python.syntax.Primary>) (hydra.util.Maybe.<hydra.ext.python.syntax.Primary>nothing()),
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.maybes.IsJust.apply(termLhs),
                  () -> (hydra.util.Maybe<hydra.ext.python.syntax.Primary>) (hydra.util.Maybe.<hydra.ext.python.syntax.Primary>nothing()),
                  () -> (termRhs).accept(new hydra.ext.python.syntax.Factor.PartialVisitor<>() {
                    @Override
                    public hydra.util.Maybe<hydra.ext.python.syntax.Primary> otherwise(hydra.ext.python.syntax.Factor instance) {
                      return (hydra.util.Maybe<hydra.ext.python.syntax.Primary>) (hydra.util.Maybe.<hydra.ext.python.syntax.Primary>nothing());
                    }
                    
                    @Override
                    public hydra.util.Maybe<hydra.ext.python.syntax.Primary> visit(hydra.ext.python.syntax.Factor.Simple power) {
                      return hydra.ext.python.utils.Utils.decodePyPowerToPyPrimary((power).value);
                    }
                  }))))))));
  }
  
  static hydra.util.Maybe<hydra.ext.python.syntax.Primary> decodePyInversionToPyPrimary(hydra.ext.python.syntax.Inversion i) {
    return (i).accept(new hydra.ext.python.syntax.Inversion.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.ext.python.syntax.Primary> otherwise(hydra.ext.python.syntax.Inversion instance) {
        return (hydra.util.Maybe<hydra.ext.python.syntax.Primary>) (hydra.util.Maybe.<hydra.ext.python.syntax.Primary>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.ext.python.syntax.Primary> visit(hydra.ext.python.syntax.Inversion.Simple comparison) {
        return hydra.ext.python.utils.Utils.decodePyComparisonToPyAwaitPrimary((comparison).value);
      }
    });
  }
  
  static hydra.util.Maybe<hydra.ext.python.syntax.Primary> decodePyConjunctionToPyPrimary(hydra.ext.python.syntax.Conjunction c) {
    java.util.List<hydra.ext.python.syntax.Inversion> inversions = (c).value;
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(inversions),
        1),
      () -> hydra.ext.python.utils.Utils.decodePyInversionToPyPrimary(hydra.lib.lists.Head.apply(inversions)),
      () -> (hydra.util.Maybe<hydra.ext.python.syntax.Primary>) (hydra.util.Maybe.<hydra.ext.python.syntax.Primary>nothing()));
  }
  
  static hydra.util.Maybe<hydra.ext.python.syntax.Primary> decodePyExpressionToPyPrimary(hydra.ext.python.syntax.Expression e) {
    return (e).accept(new hydra.ext.python.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.ext.python.syntax.Primary> otherwise(hydra.ext.python.syntax.Expression instance) {
        return (hydra.util.Maybe<hydra.ext.python.syntax.Primary>) (hydra.util.Maybe.<hydra.ext.python.syntax.Primary>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.ext.python.syntax.Primary> visit(hydra.ext.python.syntax.Expression.Simple disj) {
        java.util.List<hydra.ext.python.syntax.Conjunction> conjunctions = ((disj).value).value;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(conjunctions),
            1),
          () -> hydra.ext.python.utils.Utils.decodePyConjunctionToPyPrimary(hydra.lib.lists.Head.apply(conjunctions)),
          () -> (hydra.util.Maybe<hydra.ext.python.syntax.Primary>) (hydra.util.Maybe.<hydra.ext.python.syntax.Primary>nothing()));
      }
    });
  }
  
  static hydra.ext.python.syntax.Primary pyExpressionToPyPrimary(hydra.ext.python.syntax.Expression e) {
    return hydra.lib.maybes.Maybe.apply(
      new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Group(new hydra.ext.python.syntax.Group.Expression(new hydra.ext.python.syntax.NamedExpression.Simple(e)))),
      (java.util.function.Function<hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.Primary>) (prim -> prim),
      hydra.ext.python.utils.Utils.decodePyExpressionToPyPrimary(e));
  }
  
  static hydra.ext.python.syntax.Slice pyPrimaryToPySlice(hydra.ext.python.syntax.Primary prim) {
    return hydra.ext.python.utils.Utils.pyExpressionToPySlice(hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(prim));
  }
  
  static hydra.ext.python.syntax.Expression pyBitwiseOrToPyExpression(hydra.ext.python.syntax.BitwiseOr bor) {
    return hydra.ext.python.utils.Utils.pyConjunctionToPyExpression(hydra.ext.python.utils.Utils.pyBitwiseOrToPyConjunction(bor));
  }
  
  static hydra.ext.python.syntax.Block indentedBlock(hydra.util.Maybe<String> mcomment, java.util.List<java.util.List<hydra.ext.python.syntax.Statement>> stmts) {
    hydra.util.Lazy<java.util.List<hydra.ext.python.syntax.Statement>> commentGroup = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
      (java.util.List<hydra.ext.python.syntax.Statement>) (java.util.List.<hydra.ext.python.syntax.Statement>of()),
      (java.util.function.Function<String, java.util.List<hydra.ext.python.syntax.Statement>>) (s -> java.util.List.of(hydra.ext.python.utils.Utils.commentStatement(s))),
      mcomment));
    hydra.util.Lazy<java.util.List<java.util.List<hydra.ext.python.syntax.Statement>>> groups = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, Boolean>) (g -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(g))),
      hydra.lib.lists.Cons.apply(
        commentGroup.get(),
        stmts)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(groups.get()),
      () -> new hydra.ext.python.syntax.Block.Simple(java.util.List.of(hydra.ext.python.utils.Utils.pyExpressionToPySimpleStatement(hydra.ext.python.utils.Utils.pyAtomToPyExpression(new hydra.ext.python.syntax.Atom.Ellipsis())))),
      () -> new hydra.ext.python.syntax.Block.Indented(groups.get()));
  }
  
  static hydra.ext.python.syntax.Expression orExpression(java.util.List<hydra.ext.python.syntax.Primary> prims) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>, java.util.function.Function<java.util.List<hydra.ext.python.syntax.Primary>, hydra.ext.python.syntax.BitwiseOr>>> build = new java.util.concurrent.atomic.AtomicReference<>();
    build.set((java.util.function.Function<hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>, java.util.function.Function<java.util.List<hydra.ext.python.syntax.Primary>, hydra.ext.python.syntax.BitwiseOr>>) (prev -> (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Primary>, hydra.ext.python.syntax.BitwiseOr>) (ps -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(hydra.lib.lists.Tail.apply(ps)),
      () -> new hydra.ext.python.syntax.BitwiseOr(prev, hydra.ext.python.utils.Utils.pyPrimaryToPyBitwiseXor(hydra.lib.lists.Head.apply(ps))),
      () -> ((build.get()).apply(hydra.util.Maybe.just(new hydra.ext.python.syntax.BitwiseOr(prev, hydra.ext.python.utils.Utils.pyPrimaryToPyBitwiseXor(hydra.lib.lists.Head.apply(ps)))))).apply(hydra.lib.lists.Tail.apply(ps))))));
    return hydra.ext.python.utils.Utils.pyBitwiseOrToPyExpression(((build.get()).apply((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()))).apply(prims));
  }
  
  static <T0> hydra.ext.python.syntax.Statement typeAliasStatement310(hydra.ext.python.syntax.Name name, T0 _tparams, hydra.util.Maybe<String> mcomment, hydra.ext.python.syntax.Expression tyexpr) {
    hydra.ext.python.syntax.Expression quotedExpr = hydra.ext.python.utils.Utils.doubleQuotedString(hydra.serialization.Serialization.printExpr(hydra.ext.python.serde.Serde.encodeExpression(tyexpr)));
    return hydra.ext.python.utils.Utils.annotatedStatement(
      mcomment,
      hydra.ext.python.utils.Utils.pyAssignmentToPyStatement(new hydra.ext.python.syntax.Assignment.Typed(new hydra.ext.python.syntax.TypedAssignment(new hydra.ext.python.syntax.SingleTarget.Name(name), new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("TypeAlias")))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of())))))))), hydra.util.Maybe.just(hydra.ext.python.utils.Utils.pyExpressionToPyAnnotatedRhs(quotedExpr))))));
  }
  
  static hydra.ext.python.syntax.Parameters getItemParams() {
    return new hydra.ext.python.syntax.Parameters.ParamNoDefault(new hydra.ext.python.syntax.ParamNoDefaultParameters(java.util.List.of(
      new hydra.ext.python.syntax.ParamNoDefault(new hydra.ext.python.syntax.Param(new hydra.ext.python.syntax.Name("cls"), (hydra.util.Maybe<hydra.ext.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.ext.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing())),
      new hydra.ext.python.syntax.ParamNoDefault(new hydra.ext.python.syntax.Param(new hydra.ext.python.syntax.Name("item"), (hydra.util.Maybe<hydra.ext.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.ext.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing()))), (java.util.List<hydra.ext.python.syntax.ParamWithDefault>) (java.util.List.<hydra.ext.python.syntax.ParamWithDefault>of()), (hydra.util.Maybe<hydra.ext.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.StarEtc>nothing())));
  }
  
  static java.util.List<hydra.ext.python.syntax.Statement> unionTypeClassStatements310(hydra.ext.python.syntax.Name name, hydra.util.Maybe<String> mcomment, hydra.ext.python.syntax.Expression tyexpr) {
    String docString = hydra.serialization.Serialization.printExpr(hydra.ext.python.serde.Serde.encodeExpression(tyexpr));
    hydra.ext.python.syntax.Statement docStmt = hydra.ext.python.utils.Utils.pyExpressionToPyStatement(hydra.ext.python.utils.Utils.tripleQuotedString(docString));
    hydra.util.Lazy<hydra.ext.python.syntax.Statement> returnObject = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.pySimpleStatementToPyStatement(new hydra.ext.python.syntax.SimpleStatement.Return(new hydra.ext.python.syntax.ReturnStatement(java.util.List.of(new hydra.ext.python.syntax.StarExpression.Simple(new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("object")))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of()))))))))))))));
    hydra.util.Lazy<hydra.ext.python.syntax.Statement> getItemMethod = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Function(new hydra.ext.python.syntax.FunctionDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), new hydra.ext.python.syntax.FunctionDefRaw(false, new hydra.ext.python.syntax.Name("__getitem__"), (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.List.<hydra.ext.python.syntax.TypeParameter>of()), hydra.util.Maybe.just(hydra.ext.python.utils.Utils.getItemParams()), (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.FuncTypeComment>nothing()), hydra.ext.python.utils.Utils.indentedBlock(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      java.util.List.of(java.util.List.of(returnObject.get()))))))));
    String nameStr = (name).value;
    hydra.ext.python.syntax.Name metaName = new hydra.ext.python.syntax.Name(hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        "_",
        nameStr),
      "Meta"));
    hydra.util.Lazy<hydra.ext.python.syntax.Statement> metaClass = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.pyClassDefinitionToPyStatement(new hydra.ext.python.syntax.ClassDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), metaName, (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.List.<hydra.ext.python.syntax.TypeParameter>of()), hydra.util.Maybe.just(hydra.ext.python.utils.Utils.pyExpressionsToPyArgs(java.util.List.of(new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("type")))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of()))))))))))), hydra.ext.python.utils.Utils.indentedBlock(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      java.util.List.of(java.util.List.of(getItemMethod.get()))))));
    hydra.util.Lazy<hydra.ext.python.syntax.Kwarg> metaclassArg = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Kwarg(new hydra.ext.python.syntax.Name("metaclass"), new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(metaName))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of()))))))))));
    hydra.ext.python.syntax.Statement passStmt = hydra.ext.python.utils.Utils.pySimpleStatementToPyStatement(new hydra.ext.python.syntax.SimpleStatement.Pass());
    hydra.util.Lazy<hydra.ext.python.syntax.Statement> unionClass = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.annotatedStatement(
      mcomment,
      hydra.ext.python.utils.Utils.pyClassDefinitionToPyStatement(new hydra.ext.python.syntax.ClassDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), name, (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.List.<hydra.ext.python.syntax.TypeParameter>of()), hydra.util.Maybe.just(new hydra.ext.python.syntax.Args((java.util.List<hydra.ext.python.syntax.PosArg>) (java.util.List.<hydra.ext.python.syntax.PosArg>of()), java.util.List.of(new hydra.ext.python.syntax.KwargOrStarred.Kwarg(metaclassArg.get())), (java.util.List<hydra.ext.python.syntax.KwargOrDoubleStarred>) (java.util.List.<hydra.ext.python.syntax.KwargOrDoubleStarred>of()))), hydra.ext.python.utils.Utils.indentedBlock(
        (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
        java.util.List.of(
          java.util.List.of(docStmt),
          java.util.List.of(passStmt)))))));
    return java.util.List.of(
      metaClass.get(),
      unionClass.get());
  }
  
  static hydra.ext.python.syntax.Parameters selfOnlyParams() {
    return new hydra.ext.python.syntax.Parameters.ParamNoDefault(new hydra.ext.python.syntax.ParamNoDefaultParameters(java.util.List.of(new hydra.ext.python.syntax.ParamNoDefault(new hydra.ext.python.syntax.Param(new hydra.ext.python.syntax.Name("self"), (hydra.util.Maybe<hydra.ext.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.ext.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing()))), (java.util.List<hydra.ext.python.syntax.ParamWithDefault>) (java.util.List.<hydra.ext.python.syntax.ParamWithDefault>of()), (hydra.util.Maybe<hydra.ext.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.StarEtc>nothing())));
  }
  
  static hydra.ext.python.syntax.Parameters selfOtherParams() {
    return new hydra.ext.python.syntax.Parameters.ParamNoDefault(new hydra.ext.python.syntax.ParamNoDefaultParameters(java.util.List.of(
      new hydra.ext.python.syntax.ParamNoDefault(new hydra.ext.python.syntax.Param(new hydra.ext.python.syntax.Name("self"), (hydra.util.Maybe<hydra.ext.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.ext.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing())),
      new hydra.ext.python.syntax.ParamNoDefault(new hydra.ext.python.syntax.Param(new hydra.ext.python.syntax.Name("other"), (hydra.util.Maybe<hydra.ext.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.ext.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.ext.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.TypeComment>nothing()))), (java.util.List<hydra.ext.python.syntax.ParamWithDefault>) (java.util.List.<hydra.ext.python.syntax.ParamWithDefault>of()), (hydra.util.Maybe<hydra.ext.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.ext.python.syntax.StarEtc>nothing())));
  }
  
  static java.util.List<hydra.ext.python.syntax.Statement> unitVariantMethods(hydra.ext.python.syntax.Name className) {
    String classNameStr = (className).value;
    hydra.util.Lazy<hydra.ext.python.syntax.Statement> returnIsinstance = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.pySimpleStatementToPyStatement(new hydra.ext.python.syntax.SimpleStatement.Return(new hydra.ext.python.syntax.ReturnStatement(java.util.List.of(new hydra.ext.python.syntax.StarExpression.Simple(hydra.ext.python.utils.Utils.functionCall(
      new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("isinstance"))),
      java.util.List.of(
        new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("other")))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of())))))))),
        new hydra.ext.python.syntax.Expression.Simple(new hydra.ext.python.syntax.Disjunction(java.util.List.of(new hydra.ext.python.syntax.Conjunction(java.util.List.of(new hydra.ext.python.syntax.Inversion.Simple(new hydra.ext.python.syntax.Comparison(new hydra.ext.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseOr>nothing()), new hydra.ext.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseXor>nothing()), new hydra.ext.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.ext.python.syntax.BitwiseAnd>nothing()), new hydra.ext.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.ext.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.ShiftLhs>nothing()), new hydra.ext.python.syntax.Sum((hydra.util.Maybe<hydra.ext.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.SumLhs>nothing()), new hydra.ext.python.syntax.Term((hydra.util.Maybe<hydra.ext.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.ext.python.syntax.TermLhs>nothing()), new hydra.ext.python.syntax.Factor.Simple(new hydra.ext.python.syntax.Power(new hydra.ext.python.syntax.AwaitPrimary(false, new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(className))), (hydra.util.Maybe<hydra.ext.python.syntax.Factor>) (hydra.util.Maybe.<hydra.ext.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.ext.python.syntax.CompareOpBitwiseOrPair>) (java.util.List.<hydra.ext.python.syntax.CompareOpBitwiseOrPair>of()))))))))))))))));
    hydra.util.Lazy<hydra.ext.python.syntax.Statement> eqMethod = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Function(new hydra.ext.python.syntax.FunctionDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), new hydra.ext.python.syntax.FunctionDefRaw(false, new hydra.ext.python.syntax.Name("__eq__"), (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.List.<hydra.ext.python.syntax.TypeParameter>of()), hydra.util.Maybe.just(hydra.ext.python.utils.Utils.selfOtherParams()), (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.FuncTypeComment>nothing()), hydra.ext.python.utils.Utils.indentedBlock(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      java.util.List.of(java.util.List.of(returnIsinstance.get()))))))));
    hydra.ext.python.syntax.Statement returnHash = hydra.ext.python.utils.Utils.pySimpleStatementToPyStatement(new hydra.ext.python.syntax.SimpleStatement.Return(new hydra.ext.python.syntax.ReturnStatement(java.util.List.of(new hydra.ext.python.syntax.StarExpression.Simple(hydra.ext.python.utils.Utils.functionCall(
      new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Name(new hydra.ext.python.syntax.Name("hash"))),
      java.util.List.of(hydra.ext.python.utils.Utils.doubleQuotedString(classNameStr))))))));
    hydra.util.Lazy<hydra.ext.python.syntax.Statement> hashMethod = new hydra.util.Lazy<>(() -> new hydra.ext.python.syntax.Statement.Compound(new hydra.ext.python.syntax.CompoundStatement.Function(new hydra.ext.python.syntax.FunctionDefinition((hydra.util.Maybe<hydra.ext.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.ext.python.syntax.Decorators>nothing()), new hydra.ext.python.syntax.FunctionDefRaw(false, new hydra.ext.python.syntax.Name("__hash__"), (java.util.List<hydra.ext.python.syntax.TypeParameter>) (java.util.List.<hydra.ext.python.syntax.TypeParameter>of()), hydra.util.Maybe.just(hydra.ext.python.utils.Utils.selfOnlyParams()), (hydra.util.Maybe<hydra.ext.python.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.ext.python.syntax.FuncTypeComment>nothing()), hydra.ext.python.utils.Utils.indentedBlock(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      java.util.List.of(java.util.List.of(returnHash))))))));
    hydra.util.Lazy<hydra.ext.python.syntax.Statement> slotsStmt = new hydra.util.Lazy<>(() -> hydra.ext.python.utils.Utils.assignmentStatement(
      new hydra.ext.python.syntax.Name("__slots__"),
      hydra.ext.python.utils.Utils.pyPrimaryToPyExpression(new hydra.ext.python.syntax.Primary.Simple(new hydra.ext.python.syntax.Atom.Tuple(new hydra.ext.python.syntax.Tuple((java.util.List<hydra.ext.python.syntax.StarNamedExpression>) (java.util.List.<hydra.ext.python.syntax.StarNamedExpression>of())))))));
    return java.util.List.of(
      slotsStmt.get(),
      eqMethod.get(),
      hashMethod.get());
  }
  
  static hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> findNamespaces(hydra.module.Namespace focusNs, java.util.List<hydra.module.Definition> defs) {
    hydra.module.Namespace coreNs = new hydra.module.Namespace("hydra.core");
    hydra.util.Lazy<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>> namespaces = new hydra.util.Lazy<>(() -> hydra.schemas.Schemas.namespacesForDefinitions(
      hydra.ext.python.names.Names::encodeNamespace,
      focusNs,
      defs));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        (hydra.lib.pairs.First.apply(((java.util.function.Function<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>, hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>>) (projected -> projected.focus)).apply(namespaces.get()))).value,
        (coreNs).value),
      () -> namespaces.get(),
      () -> (hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>) (new hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>(((java.util.function.Function<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>, hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>>) (projected -> projected.focus)).apply(namespaces.get()), hydra.lib.maps.Insert.apply(
        coreNs,
        hydra.ext.python.names.Names.encodeNamespace(coreNs),
        ((java.util.function.Function<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>, java.util.Map<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>>) (projected -> projected.mapping)).apply(namespaces.get())))));
  }
}
