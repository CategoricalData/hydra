// Note: this is an automatically generated file. Do not edit.

package hydra.python;

/**
 * Python utilities for constructing Python syntax trees
 */
public interface Utils {
  static hydra.python.syntax.Expression annotatedExpression(hydra.util.Maybe<String> mcomment, hydra.python.syntax.Expression expr) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> expr,
      (java.util.function.Function<String, hydra.python.syntax.Expression>) (c -> hydra.python.Utils.pyPrimaryToPyExpression(hydra.python.Utils.primaryWithExpressionSlices(
        hydra.python.Utils.pyNameToPyPrimary(new hydra.python.syntax.Name("Annotated")),
        java.util.Arrays.asList(
          expr,
          hydra.python.Utils.doubleQuotedString(c))))),
      mcomment);
  }

  static hydra.python.syntax.Statement annotatedStatement(hydra.util.Maybe<String> mcomment, hydra.python.syntax.Statement stmt) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> stmt,
      (java.util.function.Function<String, hydra.python.syntax.Statement>) (c -> new hydra.python.syntax.Statement.Annotated(new hydra.python.syntax.AnnotatedStatement(c, stmt))),
      mcomment);
  }

  static hydra.python.syntax.Statement assignment(hydra.python.syntax.Name name, hydra.python.syntax.AnnotatedRhs rhs) {
    return hydra.python.Utils.pyAssignmentToPyStatement(new hydra.python.syntax.Assignment.Untyped(new hydra.python.syntax.UntypedAssignment(java.util.Arrays.asList(hydra.python.Utils.pyNameToPyStarTarget(name)), rhs, (hydra.util.Maybe<hydra.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.python.syntax.TypeComment>nothing()))));
  }

  static hydra.python.syntax.Statement assignmentStatement(hydra.python.syntax.Name name, hydra.python.syntax.Expression expr) {
    return hydra.python.Utils.assignment(
      name,
      hydra.python.Utils.pyExpressionToPyAnnotatedRhs(expr));
  }

  static hydra.python.syntax.Expression castTo(hydra.python.syntax.Expression pytype, hydra.python.syntax.Expression pyexpr) {
    return hydra.python.Utils.functionCall(
      hydra.python.Utils.pyNameToPyPrimary(new hydra.python.syntax.Name("cast")),
      java.util.Arrays.asList(
        pytype,
        pyexpr));
  }

  static hydra.python.syntax.Statement commentStatement(String s) {
    return hydra.python.Utils.pyExpressionToPyStatement(hydra.python.Utils.tripleQuotedString(s));
  }

  static hydra.util.Maybe<hydra.python.syntax.Primary> decodePyComparisonToPyAwaitPrimary(hydra.python.syntax.Comparison c) {
    hydra.python.syntax.BitwiseOr lhs = (c).lhs;
    hydra.python.syntax.BitwiseXor orRhs = (lhs).rhs;
    hydra.python.syntax.BitwiseAnd xorRhs = (orRhs).rhs;
    hydra.util.Maybe<hydra.python.syntax.BitwiseAnd> andLhs = (xorRhs).lhs;
    hydra.python.syntax.ShiftExpression andRhs = (xorRhs).rhs;
    hydra.util.Maybe<hydra.python.syntax.BitwiseOr> orLhs = (lhs).lhs;
    java.util.List<hydra.python.syntax.CompareOpBitwiseOrPair> rhs = (c).rhs;
    hydra.util.Maybe<hydra.python.syntax.ShiftLhs> shiftLhs = (andRhs).lhs;
    hydra.python.syntax.Sum shiftRhs = (andRhs).rhs;
    hydra.util.Maybe<hydra.python.syntax.SumLhs> sumLhs = (shiftRhs).lhs;
    hydra.python.syntax.Term sumRhs = (shiftRhs).rhs;
    hydra.util.Maybe<hydra.python.syntax.TermLhs> termLhs = (sumRhs).lhs;
    hydra.python.syntax.Factor termRhs = (sumRhs).rhs;
    hydra.util.Maybe<hydra.python.syntax.BitwiseXor> xorLhs = (orRhs).lhs;
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(rhs)),
      () -> (hydra.util.Maybe<hydra.python.syntax.Primary>) (hydra.util.Maybe.<hydra.python.syntax.Primary>nothing()),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.maybes.IsJust.apply(orLhs),
        () -> (hydra.util.Maybe<hydra.python.syntax.Primary>) (hydra.util.Maybe.<hydra.python.syntax.Primary>nothing()),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.maybes.IsJust.apply(xorLhs),
          () -> (hydra.util.Maybe<hydra.python.syntax.Primary>) (hydra.util.Maybe.<hydra.python.syntax.Primary>nothing()),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.maybes.IsJust.apply(andLhs),
            () -> (hydra.util.Maybe<hydra.python.syntax.Primary>) (hydra.util.Maybe.<hydra.python.syntax.Primary>nothing()),
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.maybes.IsJust.apply(shiftLhs),
              () -> (hydra.util.Maybe<hydra.python.syntax.Primary>) (hydra.util.Maybe.<hydra.python.syntax.Primary>nothing()),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.maybes.IsJust.apply(sumLhs),
                () -> (hydra.util.Maybe<hydra.python.syntax.Primary>) (hydra.util.Maybe.<hydra.python.syntax.Primary>nothing()),
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.maybes.IsJust.apply(termLhs),
                  () -> (hydra.util.Maybe<hydra.python.syntax.Primary>) (hydra.util.Maybe.<hydra.python.syntax.Primary>nothing()),
                  () -> (termRhs).accept(new hydra.python.syntax.Factor.PartialVisitor<>() {
                    @Override
                    public hydra.util.Maybe<hydra.python.syntax.Primary> otherwise(hydra.python.syntax.Factor instance) {
                      return (hydra.util.Maybe<hydra.python.syntax.Primary>) (hydra.util.Maybe.<hydra.python.syntax.Primary>nothing());
                    }

                    @Override
                    public hydra.util.Maybe<hydra.python.syntax.Primary> visit(hydra.python.syntax.Factor.Simple power) {
                      return hydra.python.Utils.decodePyPowerToPyPrimary((power).value);
                    }
                  }))))))));
  }

  static hydra.util.Maybe<hydra.python.syntax.Primary> decodePyConjunctionToPyPrimary(hydra.python.syntax.Conjunction c) {
    java.util.List<hydra.python.syntax.Inversion> inversions = (c).value;
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(inversions),
        1),
      () -> hydra.lib.maybes.Bind.apply(
        hydra.lib.lists.MaybeHead.apply(inversions),
        (java.util.function.Function<hydra.python.syntax.Inversion, hydra.util.Maybe<hydra.python.syntax.Primary>>) (i -> hydra.python.Utils.decodePyInversionToPyPrimary(i))),
      () -> (hydra.util.Maybe<hydra.python.syntax.Primary>) (hydra.util.Maybe.<hydra.python.syntax.Primary>nothing()));
  }

  static hydra.util.Maybe<hydra.python.syntax.Primary> decodePyExpressionToPyPrimary(hydra.python.syntax.Expression e) {
    return (e).accept(new hydra.python.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.python.syntax.Primary> otherwise(hydra.python.syntax.Expression instance) {
        return (hydra.util.Maybe<hydra.python.syntax.Primary>) (hydra.util.Maybe.<hydra.python.syntax.Primary>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.python.syntax.Primary> visit(hydra.python.syntax.Expression.Simple disj) {
        java.util.List<hydra.python.syntax.Conjunction> conjunctions = (disj).value.value;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(conjunctions),
            1),
          () -> hydra.lib.maybes.Bind.apply(
            hydra.lib.lists.MaybeHead.apply(conjunctions),
            (java.util.function.Function<hydra.python.syntax.Conjunction, hydra.util.Maybe<hydra.python.syntax.Primary>>) (c2 -> hydra.python.Utils.decodePyConjunctionToPyPrimary(c2))),
          () -> (hydra.util.Maybe<hydra.python.syntax.Primary>) (hydra.util.Maybe.<hydra.python.syntax.Primary>nothing()));
      }
    });
  }

  static hydra.util.Maybe<hydra.python.syntax.Primary> decodePyInversionToPyPrimary(hydra.python.syntax.Inversion i) {
    return (i).accept(new hydra.python.syntax.Inversion.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.python.syntax.Primary> otherwise(hydra.python.syntax.Inversion instance) {
        return (hydra.util.Maybe<hydra.python.syntax.Primary>) (hydra.util.Maybe.<hydra.python.syntax.Primary>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.python.syntax.Primary> visit(hydra.python.syntax.Inversion.Simple comparison) {
        return hydra.python.Utils.decodePyComparisonToPyAwaitPrimary((comparison).value);
      }
    });
  }

  static hydra.util.Maybe<hydra.python.syntax.Primary> decodePyPowerToPyPrimary(hydra.python.syntax.Power p) {
    hydra.python.syntax.AwaitPrimary lhs = (p).lhs;
    Boolean await = (lhs).await;
    hydra.python.syntax.Primary prim = (lhs).primary;
    return hydra.lib.logic.IfElse.lazy(
      await,
      () -> (hydra.util.Maybe<hydra.python.syntax.Primary>) (hydra.util.Maybe.<hydra.python.syntax.Primary>nothing()),
      () -> hydra.util.Maybe.just(prim));
  }

  static hydra.python.syntax.Statement dottedAssignmentStatement(hydra.python.syntax.Name obj, hydra.python.syntax.Name attr, hydra.python.syntax.Expression expr) {
    hydra.python.syntax.StarTarget target = new hydra.python.syntax.StarTarget.Unstarred(new hydra.python.syntax.TargetWithStarAtom.Project(new hydra.python.syntax.TPrimaryAndName(new hydra.python.syntax.TPrimary.Atom(new hydra.python.syntax.Atom.Name(obj)), attr)));
    return hydra.python.Utils.pyAssignmentToPyStatement(new hydra.python.syntax.Assignment.Untyped(new hydra.python.syntax.UntypedAssignment(java.util.Arrays.asList(target), hydra.python.Utils.pyExpressionToPyAnnotatedRhs(expr), (hydra.util.Maybe<hydra.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.python.syntax.TypeComment>nothing()))));
  }

  static hydra.python.syntax.Expression doubleQuotedString(String s) {
    return hydra.python.Utils.stringToPyExpression(
      new hydra.python.syntax.QuoteStyle.Double_(),
      s);
  }

  static hydra.packaging.Namespaces<hydra.python.syntax.DottedName> findNamespaces(hydra.packaging.Namespace focusNs, java.util.List<hydra.packaging.Definition> defs) {
    hydra.packaging.Namespace coreNs = new hydra.packaging.Namespace("hydra.core");
    hydra.util.Lazy<hydra.packaging.Namespaces<hydra.python.syntax.DottedName>> namespaces = new hydra.util.Lazy<>(() -> hydra.Analysis.namespacesForDefinitions(
      hydra.python.Names::encodeNamespace,
      focusNs,
      defs));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.pairs.First.apply(((java.util.function.Function<hydra.packaging.Namespaces<hydra.python.syntax.DottedName>, hydra.util.Pair<hydra.packaging.Namespace, hydra.python.syntax.DottedName>>) (projected -> projected.focus)).apply(namespaces.get())).value,
        (coreNs).value),
      () -> namespaces.get(),
      () -> (hydra.packaging.Namespaces<hydra.python.syntax.DottedName>) (new hydra.packaging.Namespaces<hydra.python.syntax.DottedName>(((java.util.function.Function<hydra.packaging.Namespaces<hydra.python.syntax.DottedName>, hydra.util.Pair<hydra.packaging.Namespace, hydra.python.syntax.DottedName>>) (projected -> projected.focus)).apply(namespaces.get()), hydra.lib.maps.Insert.apply(
        coreNs,
        hydra.python.Names.encodeNamespace(coreNs),
        ((java.util.function.Function<hydra.packaging.Namespaces<hydra.python.syntax.DottedName>, java.util.Map<hydra.packaging.Namespace, hydra.python.syntax.DottedName>>) (projected -> projected.mapping)).apply(namespaces.get())))));
  }

  static hydra.python.syntax.Expression functionCall(hydra.python.syntax.Primary func, java.util.List<hydra.python.syntax.Expression> args) {
    return hydra.python.Utils.pyPrimaryToPyExpression(hydra.python.Utils.primaryWithRhs(
      func,
      new hydra.python.syntax.PrimaryRhs.Call(hydra.python.Utils.pyExpressionsToPyArgs(args))));
  }

  static hydra.python.syntax.Parameters getItemParams() {
    return new hydra.python.syntax.Parameters.ParamNoDefault(new hydra.python.syntax.ParamNoDefaultParameters(java.util.Arrays.asList(
      new hydra.python.syntax.ParamNoDefault(new hydra.python.syntax.Param(new hydra.python.syntax.Name("cls"), (hydra.util.Maybe<hydra.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.python.syntax.TypeComment>nothing())),
      new hydra.python.syntax.ParamNoDefault(new hydra.python.syntax.Param(new hydra.python.syntax.Name("item"), (hydra.util.Maybe<hydra.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.python.syntax.TypeComment>nothing()))), (java.util.List<hydra.python.syntax.ParamWithDefault>) (java.util.Collections.<hydra.python.syntax.ParamWithDefault>emptyList()), (hydra.util.Maybe<hydra.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.python.syntax.StarEtc>nothing())));
  }

  static hydra.python.syntax.Block indentedBlock(hydra.util.Maybe<String> mcomment, java.util.List<java.util.List<hydra.python.syntax.Statement>> stmts) {
    hydra.util.Lazy<java.util.List<hydra.python.syntax.Statement>> commentGroup = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (java.util.List<hydra.python.syntax.Statement>) (java.util.Collections.<hydra.python.syntax.Statement>emptyList()),
      (java.util.function.Function<String, java.util.List<hydra.python.syntax.Statement>>) (s -> java.util.Arrays.asList(hydra.python.Utils.commentStatement(s))),
      mcomment));
    hydra.util.Lazy<java.util.List<java.util.List<hydra.python.syntax.Statement>>> groups = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<java.util.List<hydra.python.syntax.Statement>, Boolean>) (g -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(g))),
      hydra.lib.lists.Cons.apply(
        commentGroup.get(),
        stmts)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(groups.get()),
      () -> new hydra.python.syntax.Block.Indented(java.util.Arrays.asList(java.util.Arrays.asList(new hydra.python.syntax.Statement.Simple(java.util.Arrays.asList(hydra.python.Utils.pyExpressionToPySimpleStatement(hydra.python.Utils.pyAtomToPyExpression(new hydra.python.syntax.Atom.Ellipsis()))))))),
      () -> new hydra.python.syntax.Block.Indented(groups.get()));
  }

  static hydra.python.syntax.Expression nameAndParams(hydra.python.syntax.Name pyName, java.util.List<hydra.python.syntax.Expression> params) {
    return hydra.python.Utils.primaryAndParams(
      hydra.python.Utils.pyNameToPyPrimary(pyName),
      params);
  }

  static hydra.python.syntax.Statement newtypeStatement(hydra.python.syntax.Name name, hydra.util.Maybe<String> mcomment, hydra.python.syntax.Expression expr) {
    return hydra.python.Utils.annotatedStatement(
      mcomment,
      hydra.python.Utils.assignmentStatement(
        name,
        hydra.python.Utils.functionCall(
          new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Name(new hydra.python.syntax.Name("NewType"))),
          java.util.Arrays.asList(
            hydra.python.Utils.doubleQuotedString((name).value),
            expr))));
  }

  static hydra.python.syntax.Expression orExpression(java.util.List<hydra.python.syntax.Primary> prims) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.Maybe<hydra.python.syntax.BitwiseOr>, java.util.function.Function<java.util.List<hydra.python.syntax.Primary>, hydra.python.syntax.BitwiseOr>>> build = new java.util.concurrent.atomic.AtomicReference<>();
    build.set((java.util.function.Function<hydra.util.Maybe<hydra.python.syntax.BitwiseOr>, java.util.function.Function<java.util.List<hydra.python.syntax.Primary>, hydra.python.syntax.BitwiseOr>>) (prev -> (java.util.function.Function<java.util.List<hydra.python.syntax.Primary>, hydra.python.syntax.BitwiseOr>) (ps -> hydra.lib.maybes.Maybe.applyLazy(
      () -> new hydra.python.syntax.BitwiseOr(prev, hydra.python.Utils.pyPrimaryToPyBitwiseXor(new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Ellipsis()))),
      (java.util.function.Function<hydra.util.Pair<hydra.python.syntax.Primary, java.util.List<hydra.python.syntax.Primary>>, hydra.python.syntax.BitwiseOr>) (p -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(hydra.lib.pairs.Second.apply(p)),
        () -> new hydra.python.syntax.BitwiseOr(prev, hydra.python.Utils.pyPrimaryToPyBitwiseXor(hydra.lib.pairs.First.apply(p))),
        () -> build.get().apply(hydra.util.Maybe.just(new hydra.python.syntax.BitwiseOr(prev, hydra.python.Utils.pyPrimaryToPyBitwiseXor(hydra.lib.pairs.First.apply(p))))).apply(hydra.lib.pairs.Second.apply(p)))),
      hydra.lib.lists.Uncons.apply(ps)))));
    return hydra.python.Utils.pyBitwiseOrToPyExpression(build.get().apply((hydra.util.Maybe<hydra.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseOr>nothing())).apply(prims));
  }

  static hydra.python.syntax.Expression primaryAndParams(hydra.python.syntax.Primary prim, java.util.List<hydra.python.syntax.Expression> params) {
    return hydra.python.Utils.pyPrimaryToPyExpression(hydra.python.Utils.primaryWithExpressionSlices(
      prim,
      params));
  }

  static hydra.python.syntax.Primary primaryWithExpressionSlices(hydra.python.syntax.Primary prim, java.util.List<hydra.python.syntax.Expression> exprs) {
    return hydra.lib.maybes.FromMaybe.applyLazy(
      () -> prim,
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.python.syntax.Expression, java.util.List<hydra.python.syntax.Expression>>, hydra.python.syntax.Primary>) (p -> hydra.python.Utils.primaryWithSlices(
          prim,
          hydra.python.Utils.pyExpressionToPySlice(hydra.lib.pairs.First.apply(p)),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.python.syntax.Expression, hydra.python.syntax.SliceOrStarredExpression>) (e -> new hydra.python.syntax.SliceOrStarredExpression.Slice(hydra.python.Utils.pyExpressionToPySlice(e))),
            hydra.lib.pairs.Second.apply(p)))),
        hydra.lib.lists.Uncons.apply(exprs)));
  }

  static hydra.python.syntax.Primary primaryWithRhs(hydra.python.syntax.Primary prim, hydra.python.syntax.PrimaryRhs rhs) {
    return new hydra.python.syntax.Primary.Compound(new hydra.python.syntax.PrimaryWithRhs(prim, rhs));
  }

  static hydra.python.syntax.Primary primaryWithSlices(hydra.python.syntax.Primary prim, hydra.python.syntax.Slice first, java.util.List<hydra.python.syntax.SliceOrStarredExpression> rest) {
    return hydra.python.Utils.primaryWithRhs(
      prim,
      new hydra.python.syntax.PrimaryRhs.Slices(new hydra.python.syntax.Slices(first, rest)));
  }

  static hydra.python.syntax.Expression projectFromExpression(hydra.python.syntax.Expression exp, hydra.python.syntax.Name name) {
    hydra.python.syntax.Primary prim = new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Group(new hydra.python.syntax.Group.Expression(new hydra.python.syntax.NamedExpression.Simple(exp))));
    return hydra.python.Utils.pyPrimaryToPyExpression(new hydra.python.syntax.Primary.Compound(new hydra.python.syntax.PrimaryWithRhs(prim, new hydra.python.syntax.PrimaryRhs.Project(name))));
  }

  static hydra.python.syntax.Statement pyAssignmentToPyStatement(hydra.python.syntax.Assignment a) {
    return hydra.python.Utils.pySimpleStatementToPyStatement(new hydra.python.syntax.SimpleStatement.Assignment(a));
  }

  static hydra.python.syntax.Expression pyAtomToPyExpression(hydra.python.syntax.Atom atom) {
    return hydra.python.Utils.pyPrimaryToPyExpression(new hydra.python.syntax.Primary.Simple(atom));
  }

  static hydra.python.syntax.Conjunction pyBitwiseOrToPyConjunction(hydra.python.syntax.BitwiseOr bor) {
    return new hydra.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.python.syntax.Inversion.Simple(new hydra.python.syntax.Comparison(bor, (java.util.List<hydra.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.python.syntax.CompareOpBitwiseOrPair>emptyList())))));
  }

  static hydra.python.syntax.Expression pyBitwiseOrToPyExpression(hydra.python.syntax.BitwiseOr bor) {
    return hydra.python.Utils.pyConjunctionToPyExpression(hydra.python.Utils.pyBitwiseOrToPyConjunction(bor));
  }

  static hydra.python.syntax.Statement pyClassDefinitionToPyStatement(hydra.python.syntax.ClassDefinition cd) {
    return new hydra.python.syntax.Statement.Compound(new hydra.python.syntax.CompoundStatement.ClassDef(cd));
  }

  static hydra.python.syntax.Patterns pyClosedPatternToPyPatterns(hydra.python.syntax.ClosedPattern p) {
    return new hydra.python.syntax.Patterns.Pattern(new hydra.python.syntax.Pattern.Or(new hydra.python.syntax.OrPattern(java.util.Arrays.asList(p))));
  }

  static hydra.python.syntax.Expression pyConjunctionToPyExpression(hydra.python.syntax.Conjunction conj) {
    return new hydra.python.syntax.Expression.Simple(new hydra.python.syntax.Disjunction(java.util.Arrays.asList(conj)));
  }

  static hydra.python.syntax.BitwiseOr pyExpressionToBitwiseOr(hydra.python.syntax.Expression e) {
    return new hydra.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseOr>nothing()), new hydra.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseXor>nothing()), new hydra.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseAnd>nothing()), new hydra.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.python.syntax.ShiftLhs>nothing()), new hydra.python.syntax.Sum((hydra.util.Maybe<hydra.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.python.syntax.SumLhs>nothing()), new hydra.python.syntax.Term((hydra.util.Maybe<hydra.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.python.syntax.TermLhs>nothing()), new hydra.python.syntax.Factor.Simple(new hydra.python.syntax.Power(new hydra.python.syntax.AwaitPrimary(false, new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Group(new hydra.python.syntax.Group.Expression(new hydra.python.syntax.NamedExpression.Simple(e))))), (hydra.util.Maybe<hydra.python.syntax.Factor>) (hydra.util.Maybe.<hydra.python.syntax.Factor>nothing())))))))));
  }

  static hydra.python.syntax.Disjunction pyExpressionToDisjunction(hydra.python.syntax.Expression e) {
    return (e).accept(new hydra.python.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.python.syntax.Disjunction otherwise(hydra.python.syntax.Expression instance) {
        return new hydra.python.syntax.Disjunction(java.util.Arrays.asList(hydra.python.Utils.pyPrimaryToPyConjunction(new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Group(new hydra.python.syntax.Group.Expression(new hydra.python.syntax.NamedExpression.Simple(e)))))));
      }

      @Override
      public hydra.python.syntax.Disjunction visit(hydra.python.syntax.Expression.Simple disj) {
        return (disj).value;
      }
    });
  }

  static hydra.python.syntax.AnnotatedRhs pyExpressionToPyAnnotatedRhs(hydra.python.syntax.Expression expr) {
    return new hydra.python.syntax.AnnotatedRhs.Star(java.util.Arrays.asList(new hydra.python.syntax.StarExpression.Simple(expr)));
  }

  static hydra.python.syntax.Primary pyExpressionToPyPrimary(hydra.python.syntax.Expression e) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Group(new hydra.python.syntax.Group.Expression(new hydra.python.syntax.NamedExpression.Simple(e)))),
      (java.util.function.Function<hydra.python.syntax.Primary, hydra.python.syntax.Primary>) (prim -> prim),
      hydra.python.Utils.decodePyExpressionToPyPrimary(e));
  }

  static hydra.python.syntax.SimpleStatement pyExpressionToPySimpleStatement(hydra.python.syntax.Expression expr) {
    return new hydra.python.syntax.SimpleStatement.StarExpressions(java.util.Arrays.asList(new hydra.python.syntax.StarExpression.Simple(expr)));
  }

  static hydra.python.syntax.Slice pyExpressionToPySlice(hydra.python.syntax.Expression expr) {
    return new hydra.python.syntax.Slice.Named(new hydra.python.syntax.NamedExpression.Simple(expr));
  }

  static hydra.python.syntax.StarNamedExpression pyExpressionToPyStarNamedExpression(hydra.python.syntax.Expression expr) {
    return new hydra.python.syntax.StarNamedExpression.Simple(new hydra.python.syntax.NamedExpression.Simple(expr));
  }

  static hydra.python.syntax.Statement pyExpressionToPyStatement(hydra.python.syntax.Expression expr) {
    return hydra.python.Utils.pySimpleStatementToPyStatement(hydra.python.Utils.pyExpressionToPySimpleStatement(expr));
  }

  static hydra.python.syntax.Args pyExpressionsToPyArgs(java.util.List<hydra.python.syntax.Expression> exprs) {
    return new hydra.python.syntax.Args(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.python.syntax.Expression, hydra.python.syntax.PosArg>) (e -> new hydra.python.syntax.PosArg.Expression(e)),
      exprs), (java.util.List<hydra.python.syntax.KwargOrStarred>) (java.util.Collections.<hydra.python.syntax.KwargOrStarred>emptyList()), (java.util.List<hydra.python.syntax.KwargOrDoubleStarred>) (java.util.Collections.<hydra.python.syntax.KwargOrDoubleStarred>emptyList()));
  }

  static hydra.python.syntax.List pyList(java.util.List<hydra.python.syntax.Expression> exprs) {
    return new hydra.python.syntax.List(hydra.lib.lists.Map.apply(
      hydra.python.Utils::pyExpressionToPyStarNamedExpression,
      exprs));
  }

  static hydra.python.syntax.Expression pyNameToPyExpression(hydra.python.syntax.Name name) {
    return hydra.python.Utils.pyPrimaryToPyExpression(hydra.python.Utils.pyNameToPyPrimary(name));
  }

  static hydra.python.syntax.NamedExpression pyNameToPyNamedExpression(hydra.python.syntax.Name name) {
    return new hydra.python.syntax.NamedExpression.Simple(hydra.python.Utils.pyNameToPyExpression(name));
  }

  static hydra.python.syntax.Primary pyNameToPyPrimary(hydra.python.syntax.Name name) {
    return new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Name(name));
  }

  static hydra.python.syntax.StarTarget pyNameToPyStarTarget(hydra.python.syntax.Name name) {
    return new hydra.python.syntax.StarTarget.Unstarred(new hydra.python.syntax.TargetWithStarAtom.Atom(new hydra.python.syntax.StarAtom.Name(name)));
  }

  static hydra.python.syntax.TypeParameter pyNameToPyTypeParameter(hydra.python.syntax.Name name) {
    return new hydra.python.syntax.TypeParameter.Simple(new hydra.python.syntax.SimpleTypeParameter(name, (hydra.util.Maybe<hydra.python.syntax.Expression>) (hydra.util.Maybe.<hydra.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.python.syntax.Expression>) (hydra.util.Maybe.<hydra.python.syntax.Expression>nothing())));
  }

  static hydra.python.syntax.Name pyNone() {
    return new hydra.python.syntax.Name("None");
  }

  static hydra.python.syntax.BitwiseOr pyPrimaryToPyBitwiseOr(hydra.python.syntax.Primary prim) {
    return new hydra.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseOr>nothing()), new hydra.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseXor>nothing()), new hydra.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseAnd>nothing()), new hydra.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.python.syntax.ShiftLhs>nothing()), new hydra.python.syntax.Sum((hydra.util.Maybe<hydra.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.python.syntax.SumLhs>nothing()), new hydra.python.syntax.Term((hydra.util.Maybe<hydra.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.python.syntax.TermLhs>nothing()), new hydra.python.syntax.Factor.Simple(new hydra.python.syntax.Power(new hydra.python.syntax.AwaitPrimary(false, prim), (hydra.util.Maybe<hydra.python.syntax.Factor>) (hydra.util.Maybe.<hydra.python.syntax.Factor>nothing())))))))));
  }

  static hydra.python.syntax.BitwiseXor pyPrimaryToPyBitwiseXor(hydra.python.syntax.Primary prim) {
    return new hydra.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseXor>nothing()), new hydra.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseAnd>nothing()), new hydra.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.python.syntax.ShiftLhs>nothing()), new hydra.python.syntax.Sum((hydra.util.Maybe<hydra.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.python.syntax.SumLhs>nothing()), new hydra.python.syntax.Term((hydra.util.Maybe<hydra.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.python.syntax.TermLhs>nothing()), new hydra.python.syntax.Factor.Simple(new hydra.python.syntax.Power(new hydra.python.syntax.AwaitPrimary(false, prim), (hydra.util.Maybe<hydra.python.syntax.Factor>) (hydra.util.Maybe.<hydra.python.syntax.Factor>nothing()))))))));
  }

  static hydra.python.syntax.Conjunction pyPrimaryToPyConjunction(hydra.python.syntax.Primary prim) {
    return hydra.python.Utils.pyBitwiseOrToPyConjunction(hydra.python.Utils.pyPrimaryToPyBitwiseOr(prim));
  }

  static hydra.python.syntax.Expression pyPrimaryToPyExpression(hydra.python.syntax.Primary prim) {
    return hydra.python.Utils.pyConjunctionToPyExpression(hydra.python.Utils.pyPrimaryToPyConjunction(prim));
  }

  static hydra.python.syntax.Slice pyPrimaryToPySlice(hydra.python.syntax.Primary prim) {
    return hydra.python.Utils.pyExpressionToPySlice(hydra.python.Utils.pyPrimaryToPyExpression(prim));
  }

  static hydra.python.syntax.Statement pySimpleStatementToPyStatement(hydra.python.syntax.SimpleStatement s) {
    return new hydra.python.syntax.Statement.Simple(java.util.Arrays.asList(s));
  }

  static hydra.python.syntax.Statement raiseAssertionError(String msg) {
    return hydra.python.Utils.pySimpleStatementToPyStatement(new hydra.python.syntax.SimpleStatement.Raise(new hydra.python.syntax.RaiseStatement(hydra.util.Maybe.just(new hydra.python.syntax.RaiseExpression(hydra.python.Utils.functionCall(
      new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Name(new hydra.python.syntax.Name("AssertionError"))),
      java.util.Arrays.asList(hydra.python.Utils.doubleQuotedString(msg))), (hydra.util.Maybe<hydra.python.syntax.Expression>) (hydra.util.Maybe.<hydra.python.syntax.Expression>nothing()))))));
  }

  static hydra.python.syntax.Statement raiseTypeError(String msg) {
    return hydra.python.Utils.pySimpleStatementToPyStatement(new hydra.python.syntax.SimpleStatement.Raise(new hydra.python.syntax.RaiseStatement(hydra.util.Maybe.just(new hydra.python.syntax.RaiseExpression(hydra.python.Utils.functionCall(
      new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Name(new hydra.python.syntax.Name("TypeError"))),
      java.util.Arrays.asList(hydra.python.Utils.doubleQuotedString(msg))), (hydra.util.Maybe<hydra.python.syntax.Expression>) (hydra.util.Maybe.<hydra.python.syntax.Expression>nothing()))))));
  }

  static hydra.python.syntax.Statement returnSingle(hydra.python.syntax.Expression expr) {
    return hydra.python.Utils.pySimpleStatementToPyStatement(new hydra.python.syntax.SimpleStatement.Return(new hydra.python.syntax.ReturnStatement(java.util.Arrays.asList(new hydra.python.syntax.StarExpression.Simple(expr)))));
  }

  static hydra.python.syntax.Parameters selfOnlyParams() {
    return new hydra.python.syntax.Parameters.ParamNoDefault(new hydra.python.syntax.ParamNoDefaultParameters(java.util.Arrays.asList(new hydra.python.syntax.ParamNoDefault(new hydra.python.syntax.Param(new hydra.python.syntax.Name("self"), (hydra.util.Maybe<hydra.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.python.syntax.TypeComment>nothing()))), (java.util.List<hydra.python.syntax.ParamWithDefault>) (java.util.Collections.<hydra.python.syntax.ParamWithDefault>emptyList()), (hydra.util.Maybe<hydra.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.python.syntax.StarEtc>nothing())));
  }

  static hydra.python.syntax.Parameters selfOtherParams() {
    return new hydra.python.syntax.Parameters.ParamNoDefault(new hydra.python.syntax.ParamNoDefaultParameters(java.util.Arrays.asList(
      new hydra.python.syntax.ParamNoDefault(new hydra.python.syntax.Param(new hydra.python.syntax.Name("self"), (hydra.util.Maybe<hydra.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.python.syntax.TypeComment>nothing())),
      new hydra.python.syntax.ParamNoDefault(new hydra.python.syntax.Param(new hydra.python.syntax.Name("other"), (hydra.util.Maybe<hydra.python.syntax.Annotation>) (hydra.util.Maybe.<hydra.python.syntax.Annotation>nothing())), (hydra.util.Maybe<hydra.python.syntax.TypeComment>) (hydra.util.Maybe.<hydra.python.syntax.TypeComment>nothing()))), (java.util.List<hydra.python.syntax.ParamWithDefault>) (java.util.Collections.<hydra.python.syntax.ParamWithDefault>emptyList()), (hydra.util.Maybe<hydra.python.syntax.StarEtc>) (hydra.util.Maybe.<hydra.python.syntax.StarEtc>nothing())));
  }

  static hydra.python.syntax.Expression singleQuotedString(String s) {
    return hydra.python.Utils.stringToPyExpression(
      new hydra.python.syntax.QuoteStyle.Single(),
      s);
  }

  static hydra.python.syntax.Expression stringToPyExpression(hydra.python.syntax.QuoteStyle style, String s) {
    return hydra.python.Utils.pyAtomToPyExpression(new hydra.python.syntax.Atom.String_(new hydra.python.syntax.String_(s, style)));
  }

  static hydra.python.environment.PythonVersion targetPythonVersion() {
    return new hydra.python.environment.PythonVersion.Python310();
  }

  static hydra.python.syntax.Expression tripleQuotedString(String s) {
    return hydra.python.Utils.stringToPyExpression(
      new hydra.python.syntax.QuoteStyle.Triple(),
      s);
  }

  static hydra.python.syntax.Statement typeAliasStatement(hydra.python.syntax.Name name, java.util.List<hydra.python.syntax.TypeParameter> tparams, hydra.util.Maybe<String> mcomment, hydra.python.syntax.Expression tyexpr) {
    return hydra.python.Utils.annotatedStatement(
      mcomment,
      hydra.python.Utils.pySimpleStatementToPyStatement(new hydra.python.syntax.SimpleStatement.TypeAlias(new hydra.python.syntax.TypeAlias(name, tparams, tyexpr))));
  }

  static <T0> hydra.python.syntax.Statement typeAliasStatement310(hydra.python.syntax.Name name, T0 _tparams, hydra.util.Maybe<String> mcomment, hydra.python.syntax.Expression tyexpr) {
    hydra.python.syntax.Expression quotedExpr = hydra.python.Utils.doubleQuotedString(hydra.Serialization.printExpr(hydra.python.Serde.encodeExpression(tyexpr)));
    return hydra.python.Utils.annotatedStatement(
      mcomment,
      hydra.python.Utils.pyAssignmentToPyStatement(new hydra.python.syntax.Assignment.Typed(new hydra.python.syntax.TypedAssignment(new hydra.python.syntax.SingleTarget.Name(name), new hydra.python.syntax.Expression.Simple(new hydra.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.python.syntax.Inversion.Simple(new hydra.python.syntax.Comparison(new hydra.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseOr>nothing()), new hydra.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseXor>nothing()), new hydra.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseAnd>nothing()), new hydra.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.python.syntax.ShiftLhs>nothing()), new hydra.python.syntax.Sum((hydra.util.Maybe<hydra.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.python.syntax.SumLhs>nothing()), new hydra.python.syntax.Term((hydra.util.Maybe<hydra.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.python.syntax.TermLhs>nothing()), new hydra.python.syntax.Factor.Simple(new hydra.python.syntax.Power(new hydra.python.syntax.AwaitPrimary(false, new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Name(new hydra.python.syntax.Name("TypeAlias")))), (hydra.util.Maybe<hydra.python.syntax.Factor>) (hydra.util.Maybe.<hydra.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.python.syntax.CompareOpBitwiseOrPair>emptyList())))))))), hydra.util.Maybe.just(hydra.python.Utils.pyExpressionToPyAnnotatedRhs(quotedExpr))))));
  }

  static java.util.List<hydra.python.syntax.Statement> unionTypeClassStatements310(hydra.python.syntax.Name name, hydra.util.Maybe<String> mcomment, hydra.python.syntax.Expression tyexpr, java.util.List<hydra.python.syntax.Statement> extraStmts) {
    String docString = hydra.Serialization.printExpr(hydra.python.Serde.encodeExpression(tyexpr));
    hydra.python.syntax.Statement docStmt = hydra.python.Utils.pyExpressionToPyStatement(hydra.python.Utils.tripleQuotedString(docString));
    hydra.util.Lazy<java.util.List<java.util.List<hydra.python.syntax.Statement>>> bodyGroups = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(extraStmts),
      () -> ((java.util.function.Supplier<java.util.List<java.util.List<hydra.python.syntax.Statement>>>) (() -> {
        hydra.python.syntax.Statement passStmt = hydra.python.Utils.pySimpleStatementToPyStatement(new hydra.python.syntax.SimpleStatement.Pass());
        return java.util.Arrays.asList(
          java.util.Arrays.asList(docStmt),
          java.util.Arrays.asList(passStmt));
      })).get(),
      () -> java.util.Arrays.asList(
        java.util.Arrays.asList(docStmt),
        extraStmts)));
    hydra.util.Lazy<hydra.python.syntax.Statement> returnObject = new hydra.util.Lazy<>(() -> hydra.python.Utils.pySimpleStatementToPyStatement(new hydra.python.syntax.SimpleStatement.Return(new hydra.python.syntax.ReturnStatement(java.util.Arrays.asList(new hydra.python.syntax.StarExpression.Simple(new hydra.python.syntax.Expression.Simple(new hydra.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.python.syntax.Inversion.Simple(new hydra.python.syntax.Comparison(new hydra.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseOr>nothing()), new hydra.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseXor>nothing()), new hydra.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseAnd>nothing()), new hydra.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.python.syntax.ShiftLhs>nothing()), new hydra.python.syntax.Sum((hydra.util.Maybe<hydra.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.python.syntax.SumLhs>nothing()), new hydra.python.syntax.Term((hydra.util.Maybe<hydra.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.python.syntax.TermLhs>nothing()), new hydra.python.syntax.Factor.Simple(new hydra.python.syntax.Power(new hydra.python.syntax.AwaitPrimary(false, new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Name(new hydra.python.syntax.Name("object")))), (hydra.util.Maybe<hydra.python.syntax.Factor>) (hydra.util.Maybe.<hydra.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.python.syntax.CompareOpBitwiseOrPair>emptyList()))))))))))))));
    hydra.util.Lazy<hydra.python.syntax.Statement> getItemMethod = new hydra.util.Lazy<>(() -> new hydra.python.syntax.Statement.Compound(new hydra.python.syntax.CompoundStatement.Function(new hydra.python.syntax.FunctionDefinition((hydra.util.Maybe<hydra.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.python.syntax.Decorators>nothing()), new hydra.python.syntax.FunctionDefRaw(false, new hydra.python.syntax.Name("__getitem__"), (java.util.List<hydra.python.syntax.TypeParameter>) (java.util.Collections.<hydra.python.syntax.TypeParameter>emptyList()), hydra.util.Maybe.just(hydra.python.Utils.getItemParams()), (hydra.util.Maybe<hydra.python.syntax.Expression>) (hydra.util.Maybe.<hydra.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.python.syntax.FuncTypeComment>nothing()), hydra.python.Utils.indentedBlock(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      java.util.Arrays.asList(java.util.Arrays.asList(returnObject.get()))))))));
    String nameStr = (name).value;
    hydra.python.syntax.Name metaName = new hydra.python.syntax.Name(hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        "_",
        nameStr),
      "Meta"));
    hydra.util.Lazy<hydra.python.syntax.Statement> metaClass = new hydra.util.Lazy<>(() -> hydra.python.Utils.pyClassDefinitionToPyStatement(new hydra.python.syntax.ClassDefinition((hydra.util.Maybe<hydra.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.python.syntax.Decorators>nothing()), metaName, (java.util.List<hydra.python.syntax.TypeParameter>) (java.util.Collections.<hydra.python.syntax.TypeParameter>emptyList()), hydra.util.Maybe.just(hydra.python.Utils.pyExpressionsToPyArgs(java.util.Arrays.asList(new hydra.python.syntax.Expression.Simple(new hydra.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.python.syntax.Inversion.Simple(new hydra.python.syntax.Comparison(new hydra.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseOr>nothing()), new hydra.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseXor>nothing()), new hydra.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseAnd>nothing()), new hydra.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.python.syntax.ShiftLhs>nothing()), new hydra.python.syntax.Sum((hydra.util.Maybe<hydra.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.python.syntax.SumLhs>nothing()), new hydra.python.syntax.Term((hydra.util.Maybe<hydra.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.python.syntax.TermLhs>nothing()), new hydra.python.syntax.Factor.Simple(new hydra.python.syntax.Power(new hydra.python.syntax.AwaitPrimary(false, new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Name(new hydra.python.syntax.Name("type")))), (hydra.util.Maybe<hydra.python.syntax.Factor>) (hydra.util.Maybe.<hydra.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.python.syntax.CompareOpBitwiseOrPair>emptyList()))))))))))), hydra.python.Utils.indentedBlock(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      java.util.Arrays.asList(java.util.Arrays.asList(getItemMethod.get()))))));
    hydra.util.Lazy<hydra.python.syntax.Kwarg> metaclassArg = new hydra.util.Lazy<>(() -> new hydra.python.syntax.Kwarg(new hydra.python.syntax.Name("metaclass"), new hydra.python.syntax.Expression.Simple(new hydra.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.python.syntax.Inversion.Simple(new hydra.python.syntax.Comparison(new hydra.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseOr>nothing()), new hydra.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseXor>nothing()), new hydra.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseAnd>nothing()), new hydra.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.python.syntax.ShiftLhs>nothing()), new hydra.python.syntax.Sum((hydra.util.Maybe<hydra.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.python.syntax.SumLhs>nothing()), new hydra.python.syntax.Term((hydra.util.Maybe<hydra.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.python.syntax.TermLhs>nothing()), new hydra.python.syntax.Factor.Simple(new hydra.python.syntax.Power(new hydra.python.syntax.AwaitPrimary(false, new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Name(metaName))), (hydra.util.Maybe<hydra.python.syntax.Factor>) (hydra.util.Maybe.<hydra.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.python.syntax.CompareOpBitwiseOrPair>emptyList()))))))))));
    hydra.util.Lazy<hydra.python.syntax.Statement> unionClass = new hydra.util.Lazy<>(() -> hydra.python.Utils.annotatedStatement(
      mcomment,
      hydra.python.Utils.pyClassDefinitionToPyStatement(new hydra.python.syntax.ClassDefinition((hydra.util.Maybe<hydra.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.python.syntax.Decorators>nothing()), name, (java.util.List<hydra.python.syntax.TypeParameter>) (java.util.Collections.<hydra.python.syntax.TypeParameter>emptyList()), hydra.util.Maybe.just(new hydra.python.syntax.Args((java.util.List<hydra.python.syntax.PosArg>) (java.util.Collections.<hydra.python.syntax.PosArg>emptyList()), java.util.Arrays.asList(new hydra.python.syntax.KwargOrStarred.Kwarg(metaclassArg.get())), (java.util.List<hydra.python.syntax.KwargOrDoubleStarred>) (java.util.Collections.<hydra.python.syntax.KwargOrDoubleStarred>emptyList()))), hydra.python.Utils.indentedBlock(
        (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
        bodyGroups.get())))));
    return java.util.Arrays.asList(
      metaClass.get(),
      unionClass.get());
  }

  static java.util.List<hydra.python.syntax.Statement> unitVariantMethods(hydra.python.syntax.Name className) {
    String classNameStr = (className).value;
    hydra.util.Lazy<hydra.python.syntax.Statement> returnIsinstance = new hydra.util.Lazy<>(() -> hydra.python.Utils.pySimpleStatementToPyStatement(new hydra.python.syntax.SimpleStatement.Return(new hydra.python.syntax.ReturnStatement(java.util.Arrays.asList(new hydra.python.syntax.StarExpression.Simple(hydra.python.Utils.functionCall(
      new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Name(new hydra.python.syntax.Name("isinstance"))),
      java.util.Arrays.asList(
        new hydra.python.syntax.Expression.Simple(new hydra.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.python.syntax.Inversion.Simple(new hydra.python.syntax.Comparison(new hydra.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseOr>nothing()), new hydra.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseXor>nothing()), new hydra.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseAnd>nothing()), new hydra.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.python.syntax.ShiftLhs>nothing()), new hydra.python.syntax.Sum((hydra.util.Maybe<hydra.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.python.syntax.SumLhs>nothing()), new hydra.python.syntax.Term((hydra.util.Maybe<hydra.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.python.syntax.TermLhs>nothing()), new hydra.python.syntax.Factor.Simple(new hydra.python.syntax.Power(new hydra.python.syntax.AwaitPrimary(false, new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Name(new hydra.python.syntax.Name("other")))), (hydra.util.Maybe<hydra.python.syntax.Factor>) (hydra.util.Maybe.<hydra.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.python.syntax.CompareOpBitwiseOrPair>emptyList())))))))),
        new hydra.python.syntax.Expression.Simple(new hydra.python.syntax.Disjunction(java.util.Arrays.asList(new hydra.python.syntax.Conjunction(java.util.Arrays.asList(new hydra.python.syntax.Inversion.Simple(new hydra.python.syntax.Comparison(new hydra.python.syntax.BitwiseOr((hydra.util.Maybe<hydra.python.syntax.BitwiseOr>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseOr>nothing()), new hydra.python.syntax.BitwiseXor((hydra.util.Maybe<hydra.python.syntax.BitwiseXor>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseXor>nothing()), new hydra.python.syntax.BitwiseAnd((hydra.util.Maybe<hydra.python.syntax.BitwiseAnd>) (hydra.util.Maybe.<hydra.python.syntax.BitwiseAnd>nothing()), new hydra.python.syntax.ShiftExpression((hydra.util.Maybe<hydra.python.syntax.ShiftLhs>) (hydra.util.Maybe.<hydra.python.syntax.ShiftLhs>nothing()), new hydra.python.syntax.Sum((hydra.util.Maybe<hydra.python.syntax.SumLhs>) (hydra.util.Maybe.<hydra.python.syntax.SumLhs>nothing()), new hydra.python.syntax.Term((hydra.util.Maybe<hydra.python.syntax.TermLhs>) (hydra.util.Maybe.<hydra.python.syntax.TermLhs>nothing()), new hydra.python.syntax.Factor.Simple(new hydra.python.syntax.Power(new hydra.python.syntax.AwaitPrimary(false, new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Name(className))), (hydra.util.Maybe<hydra.python.syntax.Factor>) (hydra.util.Maybe.<hydra.python.syntax.Factor>nothing()))))))))), (java.util.List<hydra.python.syntax.CompareOpBitwiseOrPair>) (java.util.Collections.<hydra.python.syntax.CompareOpBitwiseOrPair>emptyList()))))))))))))))));
    hydra.util.Lazy<hydra.python.syntax.Statement> eqMethod = new hydra.util.Lazy<>(() -> new hydra.python.syntax.Statement.Compound(new hydra.python.syntax.CompoundStatement.Function(new hydra.python.syntax.FunctionDefinition((hydra.util.Maybe<hydra.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.python.syntax.Decorators>nothing()), new hydra.python.syntax.FunctionDefRaw(false, new hydra.python.syntax.Name("__eq__"), (java.util.List<hydra.python.syntax.TypeParameter>) (java.util.Collections.<hydra.python.syntax.TypeParameter>emptyList()), hydra.util.Maybe.just(hydra.python.Utils.selfOtherParams()), (hydra.util.Maybe<hydra.python.syntax.Expression>) (hydra.util.Maybe.<hydra.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.python.syntax.FuncTypeComment>nothing()), hydra.python.Utils.indentedBlock(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      java.util.Arrays.asList(java.util.Arrays.asList(returnIsinstance.get()))))))));
    hydra.python.syntax.Statement returnHash = hydra.python.Utils.pySimpleStatementToPyStatement(new hydra.python.syntax.SimpleStatement.Return(new hydra.python.syntax.ReturnStatement(java.util.Arrays.asList(new hydra.python.syntax.StarExpression.Simple(hydra.python.Utils.functionCall(
      new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Name(new hydra.python.syntax.Name("hash"))),
      java.util.Arrays.asList(hydra.python.Utils.doubleQuotedString(classNameStr))))))));
    hydra.util.Lazy<hydra.python.syntax.Statement> hashMethod = new hydra.util.Lazy<>(() -> new hydra.python.syntax.Statement.Compound(new hydra.python.syntax.CompoundStatement.Function(new hydra.python.syntax.FunctionDefinition((hydra.util.Maybe<hydra.python.syntax.Decorators>) (hydra.util.Maybe.<hydra.python.syntax.Decorators>nothing()), new hydra.python.syntax.FunctionDefRaw(false, new hydra.python.syntax.Name("__hash__"), (java.util.List<hydra.python.syntax.TypeParameter>) (java.util.Collections.<hydra.python.syntax.TypeParameter>emptyList()), hydra.util.Maybe.just(hydra.python.Utils.selfOnlyParams()), (hydra.util.Maybe<hydra.python.syntax.Expression>) (hydra.util.Maybe.<hydra.python.syntax.Expression>nothing()), (hydra.util.Maybe<hydra.python.syntax.FuncTypeComment>) (hydra.util.Maybe.<hydra.python.syntax.FuncTypeComment>nothing()), hydra.python.Utils.indentedBlock(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      java.util.Arrays.asList(java.util.Arrays.asList(returnHash))))))));
    hydra.util.Lazy<hydra.python.syntax.Statement> slotsStmt = new hydra.util.Lazy<>(() -> hydra.python.Utils.assignmentStatement(
      new hydra.python.syntax.Name("__slots__"),
      hydra.python.Utils.pyPrimaryToPyExpression(new hydra.python.syntax.Primary.Simple(new hydra.python.syntax.Atom.Tuple(new hydra.python.syntax.Tuple((java.util.List<hydra.python.syntax.StarNamedExpression>) (java.util.Collections.<hydra.python.syntax.StarNamedExpression>emptyList())))))));
    return java.util.Arrays.asList(
      slotsStmt.get(),
      eqMethod.get(),
      hashMethod.get());
  }
}
