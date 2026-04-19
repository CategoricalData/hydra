// Note: this is an automatically generated file. Do not edit.

package hydra.java;

/**
 * Java utilities for constructing Java syntax trees
 */
public interface Utils {
  static hydra.java.syntax.AdditiveExpression addExpressions(java.util.List<hydra.java.syntax.MultiplicativeExpression> exprs) {
    hydra.java.syntax.MultiplicativeExpression dummyMult = new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Primary(new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.Literal(new hydra.java.syntax.Literal.Integer_(new hydra.java.syntax.IntegerLiteral(new java.math.BigInteger("0")))))))));
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.java.syntax.AdditiveExpression, java.util.function.Function<hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.AdditiveExpression>>) (ae -> (java.util.function.Function<hydra.java.syntax.MultiplicativeExpression, hydra.java.syntax.AdditiveExpression>) (me -> new hydra.java.syntax.AdditiveExpression.Plus(new hydra.java.syntax.AdditiveExpression_Binary(ae, me)))),
      new hydra.java.syntax.AdditiveExpression.Unary(hydra.lib.maybes.FromMaybe.applyLazy(
        () -> dummyMult,
        hydra.lib.lists.MaybeHead.apply(exprs))),
      hydra.lib.lists.Drop.apply(
        1,
        exprs));
  }

  static hydra.java.environment.Aliases addInScopeVar(hydra.core.Name name, hydra.java.environment.Aliases aliases) {
    return new hydra.java.environment.Aliases((aliases).currentNamespace, (aliases).packages, (aliases).branchVars, (aliases).recursiveVars, (aliases).inScopeTypeParams, (aliases).polymorphicLocals, hydra.lib.sets.Insert.apply(
      name,
      (aliases).inScopeJavaVars), (aliases).varRenames, (aliases).lambdaVars, (aliases).typeVarSubst, (aliases).trustedTypeVars, (aliases).methodCodomain, (aliases).thunkedVars);
  }

  static hydra.java.environment.Aliases addInScopeVars(java.util.List<hydra.core.Name> names, hydra.java.environment.Aliases aliases) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.java.environment.Aliases, java.util.function.Function<hydra.core.Name, hydra.java.environment.Aliases>>) (a -> (java.util.function.Function<hydra.core.Name, hydra.java.environment.Aliases>) (n -> hydra.java.Utils.addInScopeVar(
        n,
        a))),
      aliases,
      names);
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> addJavaTypeParameter(hydra.java.syntax.ReferenceType rt, hydra.java.syntax.Type t, T0 cx) {
    return (t).accept(new hydra.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> visit(hydra.java.syntax.Type.Reference rt1) {
        return (rt1).value.accept(new hydra.java.syntax.ReferenceType.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> visit(hydra.java.syntax.ReferenceType.ClassOrInterface cit) {
            return (cit).value.accept(new hydra.java.syntax.ClassOrInterfaceType.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> visit(hydra.java.syntax.ClassOrInterfaceType.Class_ ct) {
                java.util.List<hydra.java.syntax.Annotation> anns = (ct).value.annotations;
                java.util.List<hydra.java.syntax.TypeArgument> args = (ct).value.arguments;
                hydra.java.syntax.TypeIdentifier id = (ct).value.identifier;
                hydra.java.syntax.ClassTypeQualifier qual = (ct).value.qualifier;
                return hydra.util.Either.<hydra.errors.Error_, hydra.java.syntax.Type>right(new hydra.java.syntax.Type.Reference(new hydra.java.syntax.ReferenceType.ClassOrInterface(new hydra.java.syntax.ClassOrInterfaceType.Class_(new hydra.java.syntax.ClassType(anns, qual, id, hydra.lib.lists.Concat2.apply(
                  args,
                  java.util.Arrays.asList(new hydra.java.syntax.TypeArgument.Reference(rt))))))));
              }

              @Override
              public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> visit(hydra.java.syntax.ClassOrInterfaceType.Interface ignored) {
                return hydra.util.Either.<hydra.errors.Error_, hydra.java.syntax.Type>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError("expected a Java class type")));
              }
            });
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> visit(hydra.java.syntax.ReferenceType.Variable tv) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.java.syntax.Type>right(hydra.java.Utils.javaTypeVariableToType((tv).value));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> visit(hydra.java.syntax.ReferenceType.Array ignored) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.java.syntax.Type>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError("expected a Java class or interface type, or a variable")));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> visit(hydra.java.syntax.Type.Primitive ignored) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.java.syntax.Type>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError("expected a reference type")));
      }
    });
  }

  static hydra.java.environment.Aliases addVarRename(hydra.core.Name original, hydra.core.Name renamed, hydra.java.environment.Aliases aliases) {
    return new hydra.java.environment.Aliases((aliases).currentNamespace, (aliases).packages, (aliases).branchVars, (aliases).recursiveVars, (aliases).inScopeTypeParams, (aliases).polymorphicLocals, (aliases).inScopeJavaVars, hydra.lib.maps.Insert.apply(
      original,
      renamed,
      (aliases).varRenames), (aliases).lambdaVars, (aliases).typeVarSubst, (aliases).trustedTypeVars, (aliases).methodCodomain, (aliases).thunkedVars);
  }

  static hydra.java.syntax.ExpressionName fieldExpression(hydra.java.syntax.Identifier varId, hydra.java.syntax.Identifier fieldId) {
    return new hydra.java.syntax.ExpressionName(hydra.util.Maybe.just(new hydra.java.syntax.AmbiguousName(java.util.Arrays.asList(varId))), fieldId);
  }

  static hydra.java.syntax.Expression fieldNameToJavaExpression(hydra.core.Name fname) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Name(hydra.java.Utils.javaIdentifierToJavaExpressionName(hydra.java.Utils.fieldNameToJavaIdentifier(fname)))))))))))))))))))))));
  }

  static hydra.java.syntax.Identifier fieldNameToJavaIdentifier(hydra.core.Name fname) {
    return hydra.java.Utils.javaIdentifier((fname).value);
  }

  static hydra.java.syntax.VariableDeclarator fieldNameToJavaVariableDeclarator(hydra.core.Name fname) {
    return hydra.java.Utils.javaVariableDeclarator(
      hydra.java.Utils.javaIdentifier((fname).value),
      (hydra.util.Maybe<hydra.java.syntax.VariableInitializer>) (hydra.util.Maybe.<hydra.java.syntax.VariableInitializer>nothing()));
  }

  static hydra.java.syntax.VariableDeclaratorId fieldNameToJavaVariableDeclaratorId(hydra.core.Name fname) {
    return hydra.java.Utils.javaVariableDeclaratorId(hydra.java.Utils.javaIdentifier((fname).value));
  }

  static hydra.java.syntax.BlockStatement finalVarDeclarationStatement(hydra.java.syntax.Identifier id, hydra.java.syntax.Expression rhs) {
    return new hydra.java.syntax.BlockStatement.LocalVariableDeclaration(new hydra.java.syntax.LocalVariableDeclarationStatement(new hydra.java.syntax.LocalVariableDeclaration(java.util.Arrays.asList(new hydra.java.syntax.VariableModifier.Final()), new hydra.java.syntax.LocalVariableType.Var(), java.util.Arrays.asList(hydra.java.Utils.javaVariableDeclarator(
      id,
      hydra.util.Maybe.just(new hydra.java.syntax.VariableInitializer.Expression(rhs)))))));
  }

  static hydra.java.environment.Aliases importAliasesForModule(hydra.packaging.Module mod) {
    return new hydra.java.environment.Aliases((mod).namespace, (java.util.Map<hydra.packaging.Namespace, hydra.java.syntax.PackageName>) ((java.util.Map<hydra.packaging.Namespace, hydra.java.syntax.PackageName>) (hydra.lib.maps.Empty.<hydra.packaging.Namespace, hydra.java.syntax.PackageName>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()));
  }

  static hydra.java.syntax.InterfaceMemberDeclaration interfaceMethodDeclaration(java.util.List<hydra.java.syntax.InterfaceMethodModifier> mods, java.util.List<hydra.java.syntax.TypeParameter> tparams, String methodName, java.util.List<hydra.java.syntax.FormalParameter> params, hydra.java.syntax.Result result, hydra.util.Maybe<java.util.List<hydra.java.syntax.BlockStatement>> stmts) {
    return new hydra.java.syntax.InterfaceMemberDeclaration.InterfaceMethod(new hydra.java.syntax.InterfaceMethodDeclaration(mods, hydra.java.Utils.javaMethodHeader(
      tparams,
      methodName,
      params,
      result), hydra.java.Utils.javaMethodBody(stmts)));
  }

  static Boolean isEscaped(String s) {
    return hydra.lib.equality.Equal.apply(
      hydra.lib.maybes.FromMaybe.applyLazy(
        () -> 0,
        hydra.lib.strings.MaybeCharAt.apply(
          0,
          s)),
      36);
  }

  static hydra.java.syntax.Expression javaAdditiveExpressionToJavaExpression(hydra.java.syntax.AdditiveExpression ae) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(ae))))))))))))))));
  }

  static hydra.java.syntax.Expression javaArrayCreation(hydra.java.syntax.PrimitiveTypeWithAnnotations primType, hydra.util.Maybe<hydra.java.syntax.ArrayInitializer> minit) {
    hydra.util.Lazy<hydra.java.syntax.ArrayInitializer> init_ = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
      minit,
      () -> new hydra.java.syntax.ArrayInitializer((java.util.List<java.util.List<hydra.java.syntax.VariableInitializer>>) (java.util.Collections.<java.util.List<hydra.java.syntax.VariableInitializer>>emptyList())),
      (java.util.function.Function<hydra.java.syntax.ArrayInitializer, hydra.java.syntax.ArrayInitializer>) (i -> i)));
    return hydra.java.Utils.javaPrimaryToJavaExpression(new hydra.java.syntax.Primary.ArrayCreation(new hydra.java.syntax.ArrayCreationExpression.PrimitiveArray(new hydra.java.syntax.ArrayCreationExpression_PrimitiveArray(primType, (java.util.List<hydra.java.syntax.Dims>) (java.util.Collections.<hydra.java.syntax.Dims>emptyList()), init_.get()))));
  }

  static hydra.java.syntax.ArrayInitializer javaArrayInitializer(java.util.List<hydra.java.syntax.Expression> exprs) {
    return new hydra.java.syntax.ArrayInitializer(java.util.Arrays.asList(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.java.syntax.Expression, hydra.java.syntax.VariableInitializer>) (e -> new hydra.java.syntax.VariableInitializer.Expression(e)),
      exprs)));
  }

  static hydra.java.syntax.Statement javaAssignmentStatement(hydra.java.syntax.LeftHandSide lhs, hydra.java.syntax.Expression rhs) {
    return new hydra.java.syntax.Statement.WithoutTrailing(new hydra.java.syntax.StatementWithoutTrailingSubstatement.Expression(new hydra.java.syntax.ExpressionStatement(new hydra.java.syntax.StatementExpression.Assignment(new hydra.java.syntax.Assignment(lhs, new hydra.java.syntax.AssignmentOperator.Simple(), rhs)))));
  }

  static hydra.java.syntax.Literal javaBoolean(Boolean b) {
    return new hydra.java.syntax.Literal.Boolean_(b);
  }

  static hydra.java.syntax.Expression javaBooleanExpression(Boolean b) {
    return hydra.java.Utils.javaPrimaryToJavaExpression(hydra.java.Utils.javaLiteralToJavaPrimary(hydra.java.Utils.javaBoolean(b)));
  }

  static hydra.java.syntax.Type javaBooleanType() {
    return hydra.java.Utils.javaPrimitiveTypeToJavaType(new hydra.java.syntax.PrimitiveType.Boolean_());
  }

  static hydra.java.syntax.PrimitiveTypeWithAnnotations javaBytePrimitiveType() {
    return new hydra.java.syntax.PrimitiveTypeWithAnnotations(new hydra.java.syntax.PrimitiveType.Numeric(new hydra.java.syntax.NumericType.Integral(new hydra.java.syntax.IntegralType.Byte_())), (java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList()));
  }

  static hydra.java.syntax.CastExpression javaCastExpression(hydra.java.syntax.ReferenceType rt, hydra.java.syntax.UnaryExpression expr) {
    return new hydra.java.syntax.CastExpression.NotPlusMinus(new hydra.java.syntax.CastExpression_NotPlusMinus(new hydra.java.syntax.CastExpression_RefAndBounds(rt, (java.util.List<hydra.java.syntax.AdditionalBound>) (java.util.Collections.<hydra.java.syntax.AdditionalBound>emptyList())), expr));
  }

  static hydra.java.syntax.Expression javaCastExpressionToJavaExpression(hydra.java.syntax.CastExpression ce) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Cast(ce))))))))))))))))))));
  }

  static hydra.java.syntax.CastExpression javaCastPrimitive(hydra.java.syntax.PrimitiveType pt, hydra.java.syntax.UnaryExpression expr) {
    return new hydra.java.syntax.CastExpression.Primitive(new hydra.java.syntax.CastExpression_Primitive(new hydra.java.syntax.PrimitiveTypeWithAnnotations(pt, (java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList())), expr));
  }

  static hydra.java.syntax.ClassDeclaration javaClassDeclaration(hydra.java.environment.Aliases aliases, java.util.List<hydra.java.syntax.TypeParameter> tparams, hydra.core.Name elName, java.util.List<hydra.java.syntax.ClassModifier> mods, hydra.util.Maybe<hydra.core.Name> supname, java.util.List<hydra.java.syntax.InterfaceType> impls, java.util.List<hydra.java.syntax.ClassBodyDeclarationWithComments> bodyDecls) {
    hydra.util.Lazy<hydra.util.Maybe<hydra.java.syntax.ClassType>> extends_ = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.java.syntax.ClassType>) (n -> hydra.java.Utils.nameToJavaClassType(
        aliases,
        true,
        (java.util.List<hydra.java.syntax.TypeArgument>) (java.util.Collections.<hydra.java.syntax.TypeArgument>emptyList()),
        n,
        (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))),
      supname));
    return new hydra.java.syntax.ClassDeclaration.Normal(new hydra.java.syntax.NormalClassDeclaration(mods, hydra.java.Utils.javaDeclName(elName), tparams, extends_.get(), impls, new hydra.java.syntax.ClassBody(bodyDecls)));
  }

  static hydra.java.syntax.ClassType javaClassType(java.util.List<hydra.java.syntax.ReferenceType> args, hydra.util.Maybe<hydra.java.syntax.PackageName> pkg, String id) {
    hydra.util.Lazy<hydra.java.syntax.ClassTypeQualifier> qual = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
      pkg,
      () -> new hydra.java.syntax.ClassTypeQualifier.None(),
      (java.util.function.Function<hydra.java.syntax.PackageName, hydra.java.syntax.ClassTypeQualifier>) (p -> new hydra.java.syntax.ClassTypeQualifier.Package_(p))));
    hydra.util.Lazy<java.util.List<hydra.java.syntax.TypeArgument>> targs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.java.syntax.ReferenceType, hydra.java.syntax.TypeArgument>) (rt -> new hydra.java.syntax.TypeArgument.Reference(rt)),
      args));
    return new hydra.java.syntax.ClassType((java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList()), qual.get(), hydra.java.Utils.javaTypeIdentifier(id), targs.get());
  }

  static hydra.java.syntax.Type javaClassTypeToJavaType(hydra.java.syntax.ClassType ct) {
    return new hydra.java.syntax.Type.Reference(new hydra.java.syntax.ReferenceType.ClassOrInterface(new hydra.java.syntax.ClassOrInterfaceType.Class_(ct)));
  }

  static hydra.java.syntax.Expression javaConditionalAndExpressionToJavaExpression(hydra.java.syntax.ConditionalAndExpression cae) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(cae)))));
  }

  static hydra.java.syntax.Expression javaConstructorCall(hydra.java.syntax.ClassOrInterfaceTypeToInstantiate ci, java.util.List<hydra.java.syntax.Expression> args, hydra.util.Maybe<hydra.java.syntax.ClassBody> mbody) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Primary(new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.ClassInstance(new hydra.java.syntax.ClassInstanceCreationExpression((hydra.util.Maybe<hydra.java.syntax.ClassInstanceCreationExpression_Qualifier>) (hydra.util.Maybe.<hydra.java.syntax.ClassInstanceCreationExpression_Qualifier>nothing()), new hydra.java.syntax.UnqualifiedClassInstanceCreationExpression((java.util.List<hydra.java.syntax.TypeArgument>) (java.util.Collections.<hydra.java.syntax.TypeArgument>emptyList()), ci, args, mbody)))))))))))))))))))))))));
  }

  static hydra.java.syntax.ClassOrInterfaceTypeToInstantiate javaConstructorName(hydra.java.syntax.Identifier id, hydra.util.Maybe<hydra.java.syntax.TypeArgumentsOrDiamond> targs) {
    return new hydra.java.syntax.ClassOrInterfaceTypeToInstantiate(java.util.Arrays.asList(new hydra.java.syntax.AnnotatedIdentifier((java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList()), id)), targs);
  }

  static hydra.java.syntax.TypeIdentifier javaDeclName(hydra.core.Name name) {
    return new hydra.java.syntax.TypeIdentifier(hydra.java.Utils.javaVariableName(name));
  }

  static hydra.java.syntax.CastExpression javaDoubleCastExpression(hydra.java.syntax.ReferenceType rawRt, hydra.java.syntax.ReferenceType targetRt, hydra.java.syntax.UnaryExpression expr) {
    hydra.java.syntax.Expression firstCast = hydra.java.Utils.javaCastExpressionToJavaExpression(hydra.java.Utils.javaCastExpression(
      rawRt,
      expr));
    return hydra.java.Utils.javaCastExpression(
      targetRt,
      hydra.java.Utils.javaExpressionToJavaUnaryExpression(firstCast));
  }

  static hydra.java.syntax.Expression javaDoubleCastExpressionToJavaExpression(hydra.java.syntax.ReferenceType rawRt, hydra.java.syntax.ReferenceType targetRt, hydra.java.syntax.UnaryExpression expr) {
    return hydra.java.Utils.javaCastExpressionToJavaExpression(hydra.java.Utils.javaDoubleCastExpression(
      rawRt,
      targetRt,
      expr));
  }

  static hydra.java.syntax.Statement javaEmptyStatement() {
    return new hydra.java.syntax.Statement.WithoutTrailing(new hydra.java.syntax.StatementWithoutTrailingSubstatement.Empty());
  }

  static hydra.java.syntax.Expression javaEqualityExpressionToJavaExpression(hydra.java.syntax.EqualityExpression ee) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(ee)))))))))))));
  }

  static hydra.java.syntax.InclusiveOrExpression javaEqualityExpressionToJavaInclusiveOrExpression(hydra.java.syntax.EqualityExpression ee) {
    return new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(ee))))));
  }

  static hydra.java.syntax.EqualityExpression javaEquals(hydra.java.syntax.EqualityExpression lhs, hydra.java.syntax.RelationalExpression rhs) {
    return new hydra.java.syntax.EqualityExpression.Equal(new hydra.java.syntax.EqualityExpression_Binary(lhs, rhs));
  }

  static hydra.java.syntax.EqualityExpression javaEqualsNull(hydra.java.syntax.EqualityExpression lhs) {
    return hydra.java.Utils.javaEquals(
      lhs,
      hydra.java.Utils.javaLiteralToJavaRelationalExpression(new hydra.java.syntax.Literal.Null()));
  }

  static hydra.java.syntax.Expression javaExpressionNameToJavaExpression(hydra.java.syntax.ExpressionName en) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Name(en)))))))))))))))))))));
  }

  static hydra.java.syntax.Primary javaExpressionToJavaPrimary(hydra.java.syntax.Expression e) {
    hydra.java.syntax.Primary fallback = new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.Parens(e));
    return (e).accept(new hydra.java.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.java.syntax.Primary otherwise(hydra.java.syntax.Expression instance) {
        return fallback;
      }

      @Override
      public hydra.java.syntax.Primary visit(hydra.java.syntax.Expression.Assignment ae) {
        return (ae).value.accept(new hydra.java.syntax.AssignmentExpression.PartialVisitor<>() {
          @Override
          public hydra.java.syntax.Primary otherwise(hydra.java.syntax.AssignmentExpression instance) {
            return fallback;
          }

          @Override
          public hydra.java.syntax.Primary visit(hydra.java.syntax.AssignmentExpression.Conditional ce) {
            return (ce).value.accept(new hydra.java.syntax.ConditionalExpression.PartialVisitor<>() {
              @Override
              public hydra.java.syntax.Primary otherwise(hydra.java.syntax.ConditionalExpression instance) {
                return fallback;
              }

              @Override
              public hydra.java.syntax.Primary visit(hydra.java.syntax.ConditionalExpression.Simple cor) {
                java.util.List<hydra.java.syntax.ConditionalAndExpression> cands = (cor).value.value;
                return hydra.lib.maybes.FromMaybe.applyLazy(
                  () -> fallback,
                  hydra.lib.maybes.Bind.apply(
                    hydra.lib.lists.MaybeHead.apply(cands),
                    (java.util.function.Function<hydra.java.syntax.ConditionalAndExpression, hydra.util.Maybe<hydra.java.syntax.Primary>>) (candHead -> {
                      java.util.List<hydra.java.syntax.InclusiveOrExpression> iors = (candHead).value;
                      return hydra.lib.maybes.Bind.apply(
                        hydra.lib.lists.MaybeHead.apply(iors),
                        (java.util.function.Function<hydra.java.syntax.InclusiveOrExpression, hydra.util.Maybe<hydra.java.syntax.Primary>>) (iorHead -> {
                          java.util.List<hydra.java.syntax.ExclusiveOrExpression> xors = (iorHead).value;
                          return hydra.lib.maybes.Bind.apply(
                            hydra.lib.lists.MaybeHead.apply(xors),
                            (java.util.function.Function<hydra.java.syntax.ExclusiveOrExpression, hydra.util.Maybe<hydra.java.syntax.Primary>>) (xorHead -> {
                              java.util.List<hydra.java.syntax.AndExpression> ands = (xorHead).value;
                              return hydra.lib.maybes.Bind.apply(
                                hydra.lib.lists.MaybeHead.apply(ands),
                                (java.util.function.Function<hydra.java.syntax.AndExpression, hydra.util.Maybe<hydra.java.syntax.Primary>>) (andHead -> {
                                  java.util.List<hydra.java.syntax.EqualityExpression> eqs = (andHead).value;
                                  return hydra.lib.maybes.Bind.apply(
                                    hydra.lib.lists.MaybeHead.apply(eqs),
                                    (java.util.function.Function<hydra.java.syntax.EqualityExpression, hydra.util.Maybe<hydra.java.syntax.Primary>>) (eqHead -> hydra.util.Maybe.just((eqHead).accept(new hydra.java.syntax.EqualityExpression.PartialVisitor<>() {
                                      @Override
                                      public hydra.java.syntax.Primary otherwise(hydra.java.syntax.EqualityExpression instance) {
                                        return fallback;
                                      }

                                      @Override
                                      public hydra.java.syntax.Primary visit(hydra.java.syntax.EqualityExpression.Unary rel) {
                                        return (rel).value.accept(new hydra.java.syntax.RelationalExpression.PartialVisitor<>() {
                                          @Override
                                          public hydra.java.syntax.Primary otherwise(hydra.java.syntax.RelationalExpression instance) {
                                            return fallback;
                                          }

                                          @Override
                                          public hydra.java.syntax.Primary visit(hydra.java.syntax.RelationalExpression.Simple shift) {
                                            return (shift).value.accept(new hydra.java.syntax.ShiftExpression.PartialVisitor<>() {
                                              @Override
                                              public hydra.java.syntax.Primary otherwise(hydra.java.syntax.ShiftExpression instance) {
                                                return fallback;
                                              }

                                              @Override
                                              public hydra.java.syntax.Primary visit(hydra.java.syntax.ShiftExpression.Unary add) {
                                                return (add).value.accept(new hydra.java.syntax.AdditiveExpression.PartialVisitor<>() {
                                                  @Override
                                                  public hydra.java.syntax.Primary otherwise(hydra.java.syntax.AdditiveExpression instance) {
                                                    return fallback;
                                                  }

                                                  @Override
                                                  public hydra.java.syntax.Primary visit(hydra.java.syntax.AdditiveExpression.Unary mul) {
                                                    return (mul).value.accept(new hydra.java.syntax.MultiplicativeExpression.PartialVisitor<>() {
                                                      @Override
                                                      public hydra.java.syntax.Primary otherwise(hydra.java.syntax.MultiplicativeExpression instance) {
                                                        return fallback;
                                                      }

                                                      @Override
                                                      public hydra.java.syntax.Primary visit(hydra.java.syntax.MultiplicativeExpression.Unary unary) {
                                                        return (unary).value.accept(new hydra.java.syntax.UnaryExpression.PartialVisitor<>() {
                                                          @Override
                                                          public hydra.java.syntax.Primary otherwise(hydra.java.syntax.UnaryExpression instance) {
                                                            return fallback;
                                                          }

                                                          @Override
                                                          public hydra.java.syntax.Primary visit(hydra.java.syntax.UnaryExpression.Other npm) {
                                                            return (npm).value.accept(new hydra.java.syntax.UnaryExpressionNotPlusMinus.PartialVisitor<>() {
                                                              @Override
                                                              public hydra.java.syntax.Primary otherwise(hydra.java.syntax.UnaryExpressionNotPlusMinus instance) {
                                                                return fallback;
                                                              }

                                                              @Override
                                                              public hydra.java.syntax.Primary visit(hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix pf) {
                                                                return (pf).value.accept(new hydra.java.syntax.PostfixExpression.PartialVisitor<>() {
                                                                  @Override
                                                                  public hydra.java.syntax.Primary otherwise(hydra.java.syntax.PostfixExpression instance) {
                                                                    return fallback;
                                                                  }

                                                                  @Override
                                                                  public hydra.java.syntax.Primary visit(hydra.java.syntax.PostfixExpression.Primary p) {
                                                                    return (p).value;
                                                                  }
                                                                });
                                                              }
                                                            });
                                                          }
                                                        });
                                                      }
                                                    });
                                                  }
                                                });
                                              }
                                            });
                                          }
                                        });
                                      }
                                    }))));
                                }));
                            }));
                        }));
                    })));
              }
            });
          }
        });
      }
    });
  }

  static hydra.java.syntax.UnaryExpression javaExpressionToJavaUnaryExpression(hydra.java.syntax.Expression e) {
    return new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Primary(new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.Parens(e)))));
  }

  static hydra.java.syntax.Expression javaFieldAccessToJavaExpression(hydra.java.syntax.FieldAccess fa) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Primary(new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.FieldAccess(fa)))))))))))))))))))))));
  }

  static hydra.java.syntax.Identifier javaIdentifier(String s) {
    return new hydra.java.syntax.Identifier(hydra.java.Utils.sanitizeJavaName(s));
  }

  static hydra.java.syntax.Expression javaIdentifierToJavaExpression(hydra.java.syntax.Identifier id) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Name(new hydra.java.syntax.ExpressionName((hydra.util.Maybe<hydra.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.java.syntax.AmbiguousName>nothing()), id))))))))))))))))))))));
  }

  static hydra.java.syntax.ExpressionName javaIdentifierToJavaExpressionName(hydra.java.syntax.Identifier id) {
    return new hydra.java.syntax.ExpressionName((hydra.util.Maybe<hydra.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.java.syntax.AmbiguousName>nothing()), id);
  }

  static hydra.java.syntax.RelationalExpression javaIdentifierToJavaRelationalExpression(hydra.java.syntax.Identifier id) {
    return new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Name(new hydra.java.syntax.ExpressionName((hydra.util.Maybe<hydra.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.java.syntax.AmbiguousName>nothing()), id))))))));
  }

  static hydra.java.syntax.UnaryExpression javaIdentifierToJavaUnaryExpression(hydra.java.syntax.Identifier id) {
    return new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Name(new hydra.java.syntax.ExpressionName((hydra.util.Maybe<hydra.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.java.syntax.AmbiguousName>nothing()), id))));
  }

  static hydra.java.syntax.RelationalExpression javaInstanceOf(hydra.java.syntax.RelationalExpression lhs, hydra.java.syntax.ReferenceType rhs) {
    return new hydra.java.syntax.RelationalExpression.Instanceof(new hydra.java.syntax.RelationalExpression_InstanceOf(lhs, rhs));
  }

  static hydra.java.syntax.Literal javaInt(java.math.BigInteger i) {
    return new hydra.java.syntax.Literal.Integer_(new hydra.java.syntax.IntegerLiteral(i));
  }

  static hydra.java.syntax.Expression javaIntExpression(java.math.BigInteger i) {
    return hydra.java.Utils.javaPrimaryToJavaExpression(hydra.java.Utils.javaLiteralToJavaPrimary(hydra.java.Utils.javaInt(i)));
  }

  static hydra.java.syntax.Type javaIntType() {
    return hydra.java.Utils.javaPrimitiveTypeToJavaType(new hydra.java.syntax.PrimitiveType.Numeric(new hydra.java.syntax.NumericType.Integral(new hydra.java.syntax.IntegralType.Int())));
  }

  static hydra.java.syntax.ClassBodyDeclaration javaInterfaceDeclarationToJavaClassBodyDeclaration(hydra.java.syntax.NormalInterfaceDeclaration nid) {
    return new hydra.java.syntax.ClassBodyDeclaration.ClassMember(new hydra.java.syntax.ClassMemberDeclaration.Interface(new hydra.java.syntax.InterfaceDeclaration.NormalInterface(nid)));
  }

  static hydra.java.syntax.Expression javaLambda(hydra.core.Name v, hydra.java.syntax.Expression body) {
    return new hydra.java.syntax.Expression.Lambda(new hydra.java.syntax.LambdaExpression(new hydra.java.syntax.LambdaParameters.Single(hydra.java.Utils.variableToJavaIdentifier(v)), new hydra.java.syntax.LambdaBody.Expression(body)));
  }

  static hydra.java.syntax.Expression javaLambdaFromBlock(hydra.core.Name v, hydra.java.syntax.Block block) {
    return new hydra.java.syntax.Expression.Lambda(new hydra.java.syntax.LambdaExpression(new hydra.java.syntax.LambdaParameters.Single(hydra.java.Utils.variableToJavaIdentifier(v)), new hydra.java.syntax.LambdaBody.Block(block)));
  }

  static hydra.java.syntax.Expression javaLiteralToJavaExpression(hydra.java.syntax.Literal lit) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Primary(new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.Literal(lit)))))))))))))))))))))));
  }

  static hydra.java.syntax.MultiplicativeExpression javaLiteralToJavaMultiplicativeExpression(hydra.java.syntax.Literal lit) {
    return new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Primary(new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.Literal(lit))))));
  }

  static hydra.java.syntax.Primary javaLiteralToJavaPrimary(hydra.java.syntax.Literal lit) {
    return new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.Literal(lit));
  }

  static hydra.java.syntax.RelationalExpression javaLiteralToJavaRelationalExpression(hydra.java.syntax.Literal lit) {
    return new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Primary(new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.Literal(lit)))))))));
  }

  static hydra.java.syntax.ClassBodyDeclaration javaMemberField(java.util.List<hydra.java.syntax.FieldModifier> mods, hydra.java.syntax.Type jt, hydra.java.syntax.VariableDeclarator v) {
    return new hydra.java.syntax.ClassBodyDeclaration.ClassMember(new hydra.java.syntax.ClassMemberDeclaration.Field(new hydra.java.syntax.FieldDeclaration(mods, new hydra.java.syntax.UnannType(jt), java.util.Arrays.asList(v))));
  }

  static hydra.java.syntax.MethodBody javaMethodBody(hydra.util.Maybe<java.util.List<hydra.java.syntax.BlockStatement>> mstmts) {
    return hydra.lib.maybes.Cases.applyLazy(
      mstmts,
      () -> new hydra.java.syntax.MethodBody.None(),
      (java.util.function.Function<java.util.List<hydra.java.syntax.BlockStatement>, hydra.java.syntax.MethodBody>) (stmts -> new hydra.java.syntax.MethodBody.Block(new hydra.java.syntax.Block(stmts))));
  }

  static hydra.java.syntax.ClassBodyDeclaration javaMethodDeclarationToJavaClassBodyDeclaration(hydra.java.syntax.MethodDeclaration md) {
    return new hydra.java.syntax.ClassBodyDeclaration.ClassMember(new hydra.java.syntax.ClassMemberDeclaration.Method(md));
  }

  static hydra.java.syntax.MethodHeader javaMethodHeader(java.util.List<hydra.java.syntax.TypeParameter> tparams, String methodName, java.util.List<hydra.java.syntax.FormalParameter> params, hydra.java.syntax.Result result) {
    return new hydra.java.syntax.MethodHeader(tparams, result, new hydra.java.syntax.MethodDeclarator(new hydra.java.syntax.Identifier(methodName), (hydra.util.Maybe<hydra.java.syntax.ReceiverParameter>) (hydra.util.Maybe.<hydra.java.syntax.ReceiverParameter>nothing()), params), (hydra.util.Maybe<hydra.java.syntax.Throws>) (hydra.util.Maybe.<hydra.java.syntax.Throws>nothing()));
  }

  static hydra.java.syntax.Expression javaMethodInvocationToJavaExpression(hydra.java.syntax.MethodInvocation mi) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Primary(new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.MethodInvocation(mi)))))))))))))))))))))));
  }

  static hydra.java.syntax.PostfixExpression javaMethodInvocationToJavaPostfixExpression(hydra.java.syntax.MethodInvocation mi) {
    return new hydra.java.syntax.PostfixExpression.Primary(new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.MethodInvocation(mi)));
  }

  static hydra.java.syntax.Primary javaMethodInvocationToJavaPrimary(hydra.java.syntax.MethodInvocation mi) {
    return new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.MethodInvocation(mi));
  }

  static hydra.java.syntax.Statement javaMethodInvocationToJavaStatement(hydra.java.syntax.MethodInvocation mi) {
    return new hydra.java.syntax.Statement.WithoutTrailing(new hydra.java.syntax.StatementWithoutTrailingSubstatement.Expression(new hydra.java.syntax.ExpressionStatement(new hydra.java.syntax.StatementExpression.MethodInvocation(mi))));
  }

  static hydra.java.syntax.RelationalExpression javaMultiplicativeExpressionToJavaRelationalExpression(hydra.java.syntax.MultiplicativeExpression me) {
    return new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(me)));
  }

  static hydra.java.syntax.PackageDeclaration javaPackageDeclaration(hydra.packaging.Namespace ns) {
    return new hydra.java.syntax.PackageDeclaration((java.util.List<hydra.java.syntax.PackageModifier>) (java.util.Collections.<hydra.java.syntax.PackageModifier>emptyList()), hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, hydra.java.syntax.Identifier>) (s -> new hydra.java.syntax.Identifier(s)),
      hydra.lib.strings.SplitOn.apply(
        ".",
        (ns).value)));
  }

  static hydra.java.syntax.EqualityExpression javaPostfixExpressionToJavaEqualityExpression(hydra.java.syntax.PostfixExpression pe) {
    return new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe)))))));
  }

  static hydra.java.syntax.Expression javaPostfixExpressionToJavaExpression(hydra.java.syntax.PostfixExpression pe) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe))))))))))))))))))));
  }

  static hydra.java.syntax.InclusiveOrExpression javaPostfixExpressionToJavaInclusiveOrExpression(hydra.java.syntax.PostfixExpression pe) {
    return new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe)))))))))))));
  }

  static hydra.java.syntax.RelationalExpression javaPostfixExpressionToJavaRelationalExpression(hydra.java.syntax.PostfixExpression pe) {
    return new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe))))));
  }

  static hydra.java.syntax.UnaryExpression javaPostfixExpressionToJavaUnaryExpression(hydra.java.syntax.PostfixExpression pe) {
    return new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe));
  }

  static hydra.java.syntax.Expression javaPrimaryToJavaExpression(hydra.java.syntax.Primary p) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Primary(p)))))))))))))))))))));
  }

  static hydra.java.syntax.UnaryExpression javaPrimaryToJavaUnaryExpression(hydra.java.syntax.Primary p) {
    return new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Primary(p)));
  }

  static hydra.java.syntax.Type javaPrimitiveTypeToJavaType(hydra.java.syntax.PrimitiveType pt) {
    return new hydra.java.syntax.Type.Primitive(new hydra.java.syntax.PrimitiveTypeWithAnnotations(pt, (java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList())));
  }

  static hydra.java.syntax.Type javaRefType(java.util.List<hydra.java.syntax.ReferenceType> args, hydra.util.Maybe<hydra.java.syntax.PackageName> pkg, String id) {
    return new hydra.java.syntax.Type.Reference(new hydra.java.syntax.ReferenceType.ClassOrInterface(new hydra.java.syntax.ClassOrInterfaceType.Class_(hydra.java.Utils.javaClassType(
      args,
      pkg,
      id))));
  }

  static hydra.java.syntax.ReferenceType javaReferenceTypeToRawType(hydra.java.syntax.ReferenceType rt) {
    return (rt).accept(new hydra.java.syntax.ReferenceType.PartialVisitor<>() {
      @Override
      public hydra.java.syntax.ReferenceType otherwise(hydra.java.syntax.ReferenceType instance) {
        return rt;
      }

      @Override
      public hydra.java.syntax.ReferenceType visit(hydra.java.syntax.ReferenceType.ClassOrInterface cit) {
        return (cit).value.accept(new hydra.java.syntax.ClassOrInterfaceType.PartialVisitor<>() {
          @Override
          public hydra.java.syntax.ReferenceType visit(hydra.java.syntax.ClassOrInterfaceType.Class_ ct) {
            java.util.List<hydra.java.syntax.Annotation> anns = (ct).value.annotations;
            hydra.java.syntax.TypeIdentifier id = (ct).value.identifier;
            hydra.java.syntax.ClassTypeQualifier qual = (ct).value.qualifier;
            return new hydra.java.syntax.ReferenceType.ClassOrInterface(new hydra.java.syntax.ClassOrInterfaceType.Class_(new hydra.java.syntax.ClassType(anns, qual, id, (java.util.List<hydra.java.syntax.TypeArgument>) (java.util.Collections.<hydra.java.syntax.TypeArgument>emptyList()))));
          }

          @Override
          public hydra.java.syntax.ReferenceType visit(hydra.java.syntax.ClassOrInterfaceType.Interface it) {
            hydra.java.syntax.ClassType ct = (it).value.value;
            java.util.List<hydra.java.syntax.Annotation> anns = (ct).annotations;
            hydra.java.syntax.TypeIdentifier id = (ct).identifier;
            hydra.java.syntax.ClassTypeQualifier qual = (ct).qualifier;
            return new hydra.java.syntax.ReferenceType.ClassOrInterface(new hydra.java.syntax.ClassOrInterfaceType.Interface(new hydra.java.syntax.InterfaceType(new hydra.java.syntax.ClassType(anns, qual, id, (java.util.List<hydra.java.syntax.TypeArgument>) (java.util.Collections.<hydra.java.syntax.TypeArgument>emptyList())))));
          }
        });
      }
    });
  }

  static hydra.java.syntax.EqualityExpression javaRelationalExpressionToJavaEqualityExpression(hydra.java.syntax.RelationalExpression re) {
    return new hydra.java.syntax.EqualityExpression.Unary(re);
  }

  static hydra.java.syntax.Expression javaRelationalExpressionToJavaExpression(hydra.java.syntax.RelationalExpression re) {
    return hydra.java.Utils.javaEqualityExpressionToJavaExpression(new hydra.java.syntax.EqualityExpression.Unary(re));
  }

  static hydra.java.syntax.UnaryExpression javaRelationalExpressionToJavaUnaryExpression(hydra.java.syntax.RelationalExpression re) {
    return new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Primary(new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.Parens(new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(re)))))))))))))))))));
  }

  static hydra.java.syntax.Statement javaReturnStatement(hydra.util.Maybe<hydra.java.syntax.Expression> mex) {
    return new hydra.java.syntax.Statement.WithoutTrailing(new hydra.java.syntax.StatementWithoutTrailingSubstatement.Return(new hydra.java.syntax.ReturnStatement(mex)));
  }

  static hydra.java.syntax.Block javaStatementsToBlock(java.util.List<hydra.java.syntax.Statement> stmts) {
    return new hydra.java.syntax.Block(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.java.syntax.Statement, hydra.java.syntax.BlockStatement>) (s -> new hydra.java.syntax.BlockStatement.Statement(s)),
      stmts));
  }

  static hydra.java.syntax.Literal javaString(String s) {
    return new hydra.java.syntax.Literal.String_(new hydra.java.syntax.StringLiteral(s));
  }

  static hydra.java.syntax.MultiplicativeExpression javaStringMultiplicativeExpression(String s) {
    return hydra.java.Utils.javaLiteralToJavaMultiplicativeExpression(hydra.java.Utils.javaString(s));
  }

  static hydra.java.syntax.Expression javaThis() {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(new hydra.java.syntax.UnaryExpression.Other(new hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.java.syntax.PostfixExpression.Primary(new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.This()))))))))))))))))))))));
  }

  static hydra.java.syntax.Statement javaThrowIllegalArgumentException(java.util.List<hydra.java.syntax.Expression> args) {
    return hydra.java.Utils.javaThrowStatement(hydra.java.Utils.javaConstructorCall(
      hydra.java.Utils.javaConstructorName(
        new hydra.java.syntax.Identifier("IllegalArgumentException"),
        (hydra.util.Maybe<hydra.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.java.syntax.TypeArgumentsOrDiamond>nothing())),
      args,
      (hydra.util.Maybe<hydra.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.java.syntax.ClassBody>nothing())));
  }

  static hydra.java.syntax.Statement javaThrowIllegalStateException(java.util.List<hydra.java.syntax.Expression> args) {
    return hydra.java.Utils.javaThrowStatement(hydra.java.Utils.javaConstructorCall(
      hydra.java.Utils.javaConstructorName(
        new hydra.java.syntax.Identifier("IllegalStateException"),
        (hydra.util.Maybe<hydra.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.java.syntax.TypeArgumentsOrDiamond>nothing())),
      args,
      (hydra.util.Maybe<hydra.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.java.syntax.ClassBody>nothing())));
  }

  static hydra.java.syntax.Statement javaThrowStatement(hydra.java.syntax.Expression e) {
    return new hydra.java.syntax.Statement.WithoutTrailing(new hydra.java.syntax.StatementWithoutTrailingSubstatement.Throw(new hydra.java.syntax.ThrowStatement(e)));
  }

  static hydra.java.syntax.Type javaTypeFromTypeName(hydra.java.environment.Aliases aliases, hydra.core.Name elName) {
    return hydra.java.Utils.javaTypeVariableToType(new hydra.java.syntax.TypeVariable((java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList()), hydra.java.Utils.nameToJavaTypeIdentifier(
      aliases,
      false,
      elName)));
  }

  static hydra.java.syntax.TypeIdentifier javaTypeIdentifier(String s) {
    return new hydra.java.syntax.TypeIdentifier(new hydra.java.syntax.Identifier(s));
  }

  static hydra.java.syntax.TypeArgument javaTypeIdentifierToJavaTypeArgument(hydra.java.syntax.TypeIdentifier id) {
    return new hydra.java.syntax.TypeArgument.Reference(new hydra.java.syntax.ReferenceType.Variable(new hydra.java.syntax.TypeVariable((java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList()), id)));
  }

  static hydra.java.syntax.TypeName javaTypeName(hydra.java.syntax.Identifier id) {
    return new hydra.java.syntax.TypeName(new hydra.java.syntax.TypeIdentifier(id), (hydra.util.Maybe<hydra.java.syntax.PackageOrTypeName>) (hydra.util.Maybe.<hydra.java.syntax.PackageOrTypeName>nothing()));
  }

  static hydra.java.syntax.TypeParameter javaTypeParameter(String v) {
    return new hydra.java.syntax.TypeParameter((java.util.List<hydra.java.syntax.TypeParameterModifier>) (java.util.Collections.<hydra.java.syntax.TypeParameterModifier>emptyList()), hydra.java.Utils.javaTypeIdentifier(v), (hydra.util.Maybe<hydra.java.syntax.TypeBound>) (hydra.util.Maybe.<hydra.java.syntax.TypeBound>nothing()));
  }

  static hydra.java.syntax.FormalParameter javaTypeToJavaFormalParameter(hydra.java.syntax.Type jt, hydra.core.Name fname) {
    return new hydra.java.syntax.FormalParameter.Simple(new hydra.java.syntax.FormalParameter_Simple((java.util.List<hydra.java.syntax.VariableModifier>) (java.util.Collections.<hydra.java.syntax.VariableModifier>emptyList()), new hydra.java.syntax.UnannType(jt), hydra.java.Utils.fieldNameToJavaVariableDeclaratorId(fname)));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.ReferenceType> javaTypeToJavaReferenceType(hydra.java.syntax.Type t, T0 cx) {
    return (t).accept(new hydra.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.ReferenceType> visit(hydra.java.syntax.Type.Reference rt) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.java.syntax.ReferenceType>right((rt).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.ReferenceType> visit(hydra.java.syntax.Type.Primitive ignored) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.java.syntax.ReferenceType>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError("expected a Java reference type")));
      }
    });
  }

  static hydra.java.syntax.Result javaTypeToJavaResult(hydra.java.syntax.Type jt) {
    return new hydra.java.syntax.Result.Type(new hydra.java.syntax.UnannType(jt));
  }

  static hydra.java.syntax.TypeArgument javaTypeToJavaTypeArgument(hydra.java.syntax.Type t) {
    return (t).accept(new hydra.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.java.syntax.TypeArgument visit(hydra.java.syntax.Type.Reference rt) {
        return new hydra.java.syntax.TypeArgument.Reference((rt).value);
      }

      @Override
      public hydra.java.syntax.TypeArgument visit(hydra.java.syntax.Type.Primitive ignored) {
        return new hydra.java.syntax.TypeArgument.Wildcard(new hydra.java.syntax.Wildcard((java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList()), (hydra.util.Maybe<hydra.java.syntax.WildcardBounds>) (hydra.util.Maybe.<hydra.java.syntax.WildcardBounds>nothing())));
      }
    });
  }

  static hydra.java.syntax.ReferenceType javaTypeVariable(String v) {
    return new hydra.java.syntax.ReferenceType.Variable(new hydra.java.syntax.TypeVariable((java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList()), hydra.java.Utils.javaTypeIdentifier(hydra.Formatting.capitalize(v))));
  }

  static hydra.java.syntax.Type javaTypeVariableToType(hydra.java.syntax.TypeVariable tv) {
    return new hydra.java.syntax.Type.Reference(new hydra.java.syntax.ReferenceType.Variable(tv));
  }

  static hydra.java.syntax.Expression javaUnaryExpressionToJavaExpression(hydra.java.syntax.UnaryExpression ue) {
    return new hydra.java.syntax.Expression.Assignment(new hydra.java.syntax.AssignmentExpression.Conditional(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.java.syntax.EqualityExpression.Unary(new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(ue))))))))))))))))));
  }

  static hydra.java.syntax.RelationalExpression javaUnaryExpressionToJavaRelationalExpression(hydra.java.syntax.UnaryExpression ue) {
    return new hydra.java.syntax.RelationalExpression.Simple(new hydra.java.syntax.ShiftExpression.Unary(new hydra.java.syntax.AdditiveExpression.Unary(new hydra.java.syntax.MultiplicativeExpression.Unary(ue))));
  }

  static hydra.java.syntax.VariableDeclarator javaVariableDeclarator(hydra.java.syntax.Identifier id, hydra.util.Maybe<hydra.java.syntax.VariableInitializer> minit) {
    return new hydra.java.syntax.VariableDeclarator(hydra.java.Utils.javaVariableDeclaratorId(id), minit);
  }

  static hydra.java.syntax.VariableDeclaratorId javaVariableDeclaratorId(hydra.java.syntax.Identifier id) {
    return new hydra.java.syntax.VariableDeclaratorId(id, (hydra.util.Maybe<hydra.java.syntax.Dims>) (hydra.util.Maybe.<hydra.java.syntax.Dims>nothing()));
  }

  static hydra.java.syntax.Identifier javaVariableName(hydra.core.Name name) {
    return hydra.java.Utils.javaIdentifier(hydra.Names.localNameOf(name));
  }

  static hydra.core.Name lookupJavaVarName(hydra.java.environment.Aliases aliases, hydra.core.Name name) {
    return hydra.lib.maybes.Cases.applyLazy(
      hydra.lib.maps.Lookup.apply(
        name,
        (aliases).varRenames),
      () -> name,
      (java.util.function.Function<hydra.core.Name, hydra.core.Name>) (renamed -> renamed));
  }

  static hydra.java.syntax.ClassBodyDeclaration makeConstructor(hydra.java.environment.Aliases aliases, hydra.core.Name elName, Boolean private_, java.util.List<hydra.java.syntax.FormalParameter> params, java.util.List<hydra.java.syntax.BlockStatement> stmts) {
    hydra.util.Lazy<hydra.java.syntax.ConstructorBody> body = new hydra.util.Lazy<>(() -> new hydra.java.syntax.ConstructorBody((hydra.util.Maybe<hydra.java.syntax.ExplicitConstructorInvocation>) (hydra.util.Maybe.<hydra.java.syntax.ExplicitConstructorInvocation>nothing()), stmts));
    hydra.java.syntax.SimpleTypeName nm = new hydra.java.syntax.SimpleTypeName(hydra.java.Utils.nameToJavaTypeIdentifier(
      aliases,
      false,
      elName));
    hydra.util.Lazy<hydra.java.syntax.ConstructorDeclarator> cons = new hydra.util.Lazy<>(() -> new hydra.java.syntax.ConstructorDeclarator((java.util.List<hydra.java.syntax.TypeParameter>) (java.util.Collections.<hydra.java.syntax.TypeParameter>emptyList()), nm, (hydra.util.Maybe<hydra.java.syntax.ReceiverParameter>) (hydra.util.Maybe.<hydra.java.syntax.ReceiverParameter>nothing()), params));
    hydra.util.Lazy<java.util.List<hydra.java.syntax.ConstructorModifier>> mods = new hydra.util.Lazy<>(() -> java.util.Arrays.asList(hydra.lib.logic.IfElse.lazy(
      private_,
      () -> new hydra.java.syntax.ConstructorModifier.Private(),
      () -> new hydra.java.syntax.ConstructorModifier.Public())));
    return new hydra.java.syntax.ClassBodyDeclaration.ConstructorDeclaration(new hydra.java.syntax.ConstructorDeclaration(mods.get(), cons.get(), (hydra.util.Maybe<hydra.java.syntax.Throws>) (hydra.util.Maybe.<hydra.java.syntax.Throws>nothing()), body.get()));
  }

  static hydra.java.syntax.ClassBodyDeclaration methodDeclaration(java.util.List<hydra.java.syntax.MethodModifier> mods, java.util.List<hydra.java.syntax.TypeParameter> tparams, java.util.List<hydra.java.syntax.Annotation> anns, String methodName, java.util.List<hydra.java.syntax.FormalParameter> params, hydra.java.syntax.Result result, hydra.util.Maybe<java.util.List<hydra.java.syntax.BlockStatement>> stmts) {
    return hydra.java.Utils.javaMethodDeclarationToJavaClassBodyDeclaration(new hydra.java.syntax.MethodDeclaration(anns, mods, hydra.java.Utils.javaMethodHeader(
      tparams,
      methodName,
      params,
      result), hydra.java.Utils.javaMethodBody(stmts)));
  }

  static hydra.java.syntax.MethodInvocation methodInvocation(hydra.util.Maybe<hydra.util.Either<hydra.java.syntax.ExpressionName, hydra.java.syntax.Primary>> lhs, hydra.java.syntax.Identifier methodName, java.util.List<hydra.java.syntax.Expression> args) {
    hydra.util.Lazy<hydra.java.syntax.MethodInvocation_Header> header = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
      lhs,
      () -> new hydra.java.syntax.MethodInvocation_Header.Simple(new hydra.java.syntax.MethodName(methodName)),
      (java.util.function.Function<hydra.util.Either<hydra.java.syntax.ExpressionName, hydra.java.syntax.Primary>, hydra.java.syntax.MethodInvocation_Header>) (either -> new hydra.java.syntax.MethodInvocation_Header.Complex(new hydra.java.syntax.MethodInvocation_Complex(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.java.syntax.ExpressionName, hydra.java.syntax.MethodInvocation_Variant>) (en -> new hydra.java.syntax.MethodInvocation_Variant.Expression(en)),
        (java.util.function.Function<hydra.java.syntax.Primary, hydra.java.syntax.MethodInvocation_Variant>) (p -> new hydra.java.syntax.MethodInvocation_Variant.Primary(p)),
        either), (java.util.List<hydra.java.syntax.TypeArgument>) (java.util.Collections.<hydra.java.syntax.TypeArgument>emptyList()), methodName)))));
    return new hydra.java.syntax.MethodInvocation(header.get(), args);
  }

  static hydra.java.syntax.MethodInvocation methodInvocationStatic(hydra.java.syntax.Identifier self, hydra.java.syntax.Identifier methodName, java.util.List<hydra.java.syntax.Expression> args) {
    return hydra.java.Utils.methodInvocation(
      hydra.util.Maybe.just(hydra.util.Either.<hydra.java.syntax.ExpressionName, hydra.java.syntax.Primary>left(hydra.java.Utils.javaIdentifierToJavaExpressionName(self))),
      methodName,
      args);
  }

  static hydra.java.syntax.MethodInvocation methodInvocationStaticWithTypeArgs(hydra.java.syntax.Identifier self, hydra.java.syntax.Identifier methodName, java.util.List<hydra.java.syntax.TypeArgument> targs, java.util.List<hydra.java.syntax.Expression> args) {
    hydra.java.syntax.MethodInvocation_Header header = new hydra.java.syntax.MethodInvocation_Header.Complex(new hydra.java.syntax.MethodInvocation_Complex(new hydra.java.syntax.MethodInvocation_Variant.Expression(hydra.java.Utils.javaIdentifierToJavaExpressionName(self)), targs, methodName));
    return new hydra.java.syntax.MethodInvocation(header, args);
  }

  static hydra.java.syntax.ClassType nameToJavaClassType(hydra.java.environment.Aliases aliases, Boolean qualify, java.util.List<hydra.java.syntax.TypeArgument> args, hydra.core.Name name, hydra.util.Maybe<String> mlocal) {
    hydra.util.Pair<hydra.java.syntax.TypeIdentifier, hydra.java.syntax.ClassTypeQualifier> result = hydra.java.Utils.nameToQualifiedJavaName(
      aliases,
      qualify,
      name,
      mlocal);
    hydra.util.Lazy<hydra.java.syntax.TypeIdentifier> id = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
    hydra.util.Lazy<hydra.java.syntax.ClassTypeQualifier> pkg = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
    return new hydra.java.syntax.ClassType((java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList()), pkg.get(), id.get(), args);
  }

  static hydra.java.syntax.Identifier nameToJavaName(hydra.java.environment.Aliases aliases, hydra.core.Name name) {
    hydra.packaging.QualifiedName qn = hydra.Names.qualifyName(name);
    String local = (qn).local;
    hydra.util.Maybe<hydra.packaging.Namespace> ns_ = (qn).namespace;
    return hydra.lib.logic.IfElse.lazy(
      hydra.java.Utils.isEscaped((name).value),
      () -> new hydra.java.syntax.Identifier(hydra.java.Utils.sanitizeJavaName(local)),
      () -> hydra.lib.maybes.Cases.applyLazy(
        ns_,
        () -> new hydra.java.syntax.Identifier(local),
        (java.util.function.Function<hydra.packaging.Namespace, hydra.java.syntax.Identifier>) (gname -> {
          hydra.util.Lazy<java.util.List<String>> parts = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
            hydra.lib.maps.Lookup.apply(
              gname,
              (aliases).packages),
            () -> hydra.lib.strings.SplitOn.apply(
              ".",
              (gname).value),
            (java.util.function.Function<hydra.java.syntax.PackageName, java.util.List<String>>) (pkgName -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.java.syntax.Identifier, String>) (i -> (i).value),
              (pkgName).value))));
          hydra.util.Lazy<java.util.List<String>> allParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
            parts.get(),
            java.util.Arrays.asList(hydra.java.Utils.sanitizeJavaName(local))));
          return new hydra.java.syntax.Identifier(hydra.lib.strings.Intercalate.apply(
            ".",
            allParts.get()));
        })));
  }

  static hydra.java.syntax.ReferenceType nameToJavaReferenceType(hydra.java.environment.Aliases aliases, Boolean qualify, java.util.List<hydra.java.syntax.TypeArgument> args, hydra.core.Name name, hydra.util.Maybe<String> mlocal) {
    return new hydra.java.syntax.ReferenceType.ClassOrInterface(new hydra.java.syntax.ClassOrInterfaceType.Class_(hydra.java.Utils.nameToJavaClassType(
      aliases,
      qualify,
      args,
      name,
      mlocal)));
  }

  static hydra.java.syntax.TypeIdentifier nameToJavaTypeIdentifier(hydra.java.environment.Aliases aliases, Boolean qualify, hydra.core.Name name) {
    return hydra.lib.pairs.First.apply(hydra.java.Utils.nameToQualifiedJavaName(
      aliases,
      qualify,
      name,
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
  }

  static hydra.util.Pair<hydra.java.syntax.TypeIdentifier, hydra.java.syntax.ClassTypeQualifier> nameToQualifiedJavaName(hydra.java.environment.Aliases aliases, Boolean qualify, hydra.core.Name name, hydra.util.Maybe<String> mlocal) {
    hydra.packaging.QualifiedName qn = hydra.Names.qualifyName(name);
    hydra.util.Maybe<hydra.packaging.Namespace> ns_ = (qn).namespace;
    hydra.util.Lazy<hydra.util.Maybe<hydra.java.syntax.PackageName>> alias = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
      ns_,
      () -> (hydra.util.Maybe<hydra.java.syntax.PackageName>) (hydra.util.Maybe.<hydra.java.syntax.PackageName>nothing()),
      (java.util.function.Function<hydra.packaging.Namespace, hydra.util.Maybe<hydra.java.syntax.PackageName>>) (n -> hydra.util.Maybe.just(hydra.lib.maybes.Cases.applyLazy(
        hydra.lib.maps.Lookup.apply(
          n,
          (aliases).packages),
        () -> hydra.java.Names.javaPackageName(hydra.lib.strings.SplitOn.apply(
          ".",
          (n).value)),
        (java.util.function.Function<hydra.java.syntax.PackageName, hydra.java.syntax.PackageName>) (id -> id))))));
    String local = (qn).local;
    hydra.util.Lazy<hydra.java.syntax.TypeIdentifier> jid = new hydra.util.Lazy<>(() -> hydra.java.Utils.javaTypeIdentifier(hydra.lib.maybes.Cases.applyLazy(
      mlocal,
      () -> hydra.java.Utils.sanitizeJavaName(local),
      (java.util.function.Function<String, String>) (l -> hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.java.Utils.sanitizeJavaName(local),
          "."),
        hydra.java.Utils.sanitizeJavaName(l))))));
    hydra.util.Lazy<hydra.java.syntax.ClassTypeQualifier> pkg = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      qualify,
      () -> hydra.lib.maybes.Cases.applyLazy(
        alias.get(),
        () -> new hydra.java.syntax.ClassTypeQualifier.None(),
        (java.util.function.Function<hydra.java.syntax.PackageName, hydra.java.syntax.ClassTypeQualifier>) (p -> new hydra.java.syntax.ClassTypeQualifier.Package_(p))),
      () -> new hydra.java.syntax.ClassTypeQualifier.None()));
    return (hydra.util.Pair<hydra.java.syntax.TypeIdentifier, hydra.java.syntax.ClassTypeQualifier>) ((hydra.util.Pair<hydra.java.syntax.TypeIdentifier, hydra.java.syntax.ClassTypeQualifier>) (new hydra.util.Pair<hydra.java.syntax.TypeIdentifier, hydra.java.syntax.ClassTypeQualifier>(jid.get(), pkg.get())));
  }

  static hydra.java.syntax.Annotation overrideAnnotation() {
    return new hydra.java.syntax.Annotation.Marker(new hydra.java.syntax.MarkerAnnotation(hydra.java.Utils.javaTypeName(new hydra.java.syntax.Identifier("Override"))));
  }

  static hydra.java.syntax.Result referenceTypeToResult(hydra.java.syntax.ReferenceType rt) {
    return hydra.java.Utils.javaTypeToJavaResult(new hydra.java.syntax.Type.Reference(rt));
  }

  static String sanitizeJavaName(String name) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.java.Utils.isEscaped(name),
      () -> hydra.java.Utils.unescape(name),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          name,
          "_"),
        () -> "ignored",
        () -> hydra.Formatting.sanitizeWithUnderscores(
          hydra.java.Language.reservedWords(),
          name)));
  }

  static hydra.java.syntax.Annotation suppressWarningsUncheckedAnnotation() {
    return new hydra.java.syntax.Annotation.SingleElement(new hydra.java.syntax.SingleElementAnnotation(hydra.java.Utils.javaTypeName(new hydra.java.syntax.Identifier("SuppressWarnings")), hydra.util.Maybe.just(new hydra.java.syntax.ElementValue.ConditionalExpression(new hydra.java.syntax.ConditionalExpression.Simple(new hydra.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(hydra.java.Utils.javaPostfixExpressionToJavaInclusiveOrExpression(new hydra.java.syntax.PostfixExpression.Primary(hydra.java.Utils.javaLiteralToJavaPrimary(hydra.java.Utils.javaString("unchecked")))))))))))));
  }

  static hydra.java.syntax.ClassBodyDeclaration toAcceptMethod(Boolean abstract_, java.util.List<hydra.java.syntax.TypeParameter> vtparams) {
    hydra.util.Lazy<java.util.List<hydra.java.syntax.Annotation>> anns = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      abstract_,
      () -> (java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList()),
      () -> java.util.Arrays.asList(hydra.java.Utils.overrideAnnotation())));
    hydra.java.syntax.Expression returnExpr = hydra.java.Utils.javaMethodInvocationToJavaExpression(hydra.java.Utils.methodInvocationStatic(
      new hydra.java.syntax.Identifier("visitor"),
      new hydra.java.syntax.Identifier(hydra.java.Names.visitMethodName()),
      java.util.Arrays.asList(hydra.java.Utils.javaThis())));
    hydra.util.Lazy<hydra.util.Maybe<java.util.List<hydra.java.syntax.BlockStatement>>> body = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      abstract_,
      () -> (hydra.util.Maybe<java.util.List<hydra.java.syntax.BlockStatement>>) (hydra.util.Maybe.<java.util.List<hydra.java.syntax.BlockStatement>>nothing()),
      () -> hydra.util.Maybe.just(java.util.Arrays.asList(new hydra.java.syntax.BlockStatement.Statement(hydra.java.Utils.javaReturnStatement(hydra.util.Maybe.just(returnExpr)))))));
    hydra.util.Lazy<java.util.List<hydra.java.syntax.MethodModifier>> mods = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      abstract_,
      () -> java.util.Arrays.asList(
        new hydra.java.syntax.MethodModifier.Public(),
        new hydra.java.syntax.MethodModifier.Abstract()),
      () -> java.util.Arrays.asList(new hydra.java.syntax.MethodModifier.Public())));
    hydra.util.Lazy<java.util.List<hydra.java.syntax.TypeArgument>> typeArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.java.syntax.TypeParameter, hydra.java.syntax.TypeArgument>) (tp -> new hydra.java.syntax.TypeArgument.Reference(hydra.java.Utils.typeParameterToReferenceType(tp))),
      vtparams));
    hydra.util.Lazy<hydra.java.syntax.Type> ref = new hydra.util.Lazy<>(() -> hydra.java.Utils.javaClassTypeToJavaType(new hydra.java.syntax.ClassType((java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList()), new hydra.java.syntax.ClassTypeQualifier.None(), hydra.java.Utils.javaTypeIdentifier(hydra.java.Names.visitorName()), hydra.lib.lists.Concat2.apply(
      typeArgs.get(),
      java.util.Arrays.asList(new hydra.java.syntax.TypeArgument.Reference(hydra.java.Utils.visitorTypeVariable()))))));
    hydra.java.syntax.FormalParameter param = hydra.java.Utils.javaTypeToJavaFormalParameter(
      ref.get(),
      new hydra.core.Name("visitor"));
    hydra.java.syntax.Result result = hydra.java.Utils.javaTypeToJavaResult(new hydra.java.syntax.Type.Reference(hydra.java.Utils.visitorTypeVariable()));
    java.util.List<hydra.java.syntax.TypeParameter> tparams = java.util.Arrays.asList(hydra.java.Utils.javaTypeParameter(hydra.java.Names.visitorReturnParameter()));
    return hydra.java.Utils.methodDeclaration(
      mods.get(),
      tparams,
      anns.get(),
      hydra.java.Names.acceptMethodName(),
      java.util.Arrays.asList(param),
      result,
      body.get());
  }

  static hydra.java.syntax.Statement toAssignStmt(hydra.core.Name fname) {
    hydra.java.syntax.Identifier id = hydra.java.Utils.fieldNameToJavaIdentifier(fname);
    hydra.java.syntax.LeftHandSide lhs = new hydra.java.syntax.LeftHandSide.FieldAccess(new hydra.java.syntax.FieldAccess(new hydra.java.syntax.FieldAccess_Qualifier.Primary(new hydra.java.syntax.Primary.NoNewArray(new hydra.java.syntax.PrimaryNoNewArrayExpression.This())), id));
    hydra.java.syntax.Expression rhs = hydra.java.Utils.fieldNameToJavaExpression(fname);
    return hydra.java.Utils.javaAssignmentStatement(
      lhs,
      rhs);
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> toJavaArrayType(hydra.java.syntax.Type t, T0 cx) {
    return (t).accept(new hydra.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> visit(hydra.java.syntax.Type.Reference rt) {
        return (rt).value.accept(new hydra.java.syntax.ReferenceType.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> visit(hydra.java.syntax.ReferenceType.ClassOrInterface cit) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.java.syntax.Type>right(new hydra.java.syntax.Type.Reference(new hydra.java.syntax.ReferenceType.Array(new hydra.java.syntax.ArrayType(new hydra.java.syntax.Dims(java.util.Arrays.asList((java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList()))), new hydra.java.syntax.ArrayType_Variant.ClassOrInterface((cit).value)))));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> visit(hydra.java.syntax.ReferenceType.Array at) {
            java.util.List<java.util.List<hydra.java.syntax.Annotation>> oldDims = (at).value.dims.value;
            hydra.util.Lazy<hydra.java.syntax.Dims> newDims = new hydra.util.Lazy<>(() -> new hydra.java.syntax.Dims(hydra.lib.lists.Concat2.apply(
              oldDims,
              java.util.Arrays.asList((java.util.List<hydra.java.syntax.Annotation>) (java.util.Collections.<hydra.java.syntax.Annotation>emptyList())))));
            hydra.java.syntax.ArrayType_Variant variant = (at).value.variant;
            return hydra.util.Either.<hydra.errors.Error_, hydra.java.syntax.Type>right(new hydra.java.syntax.Type.Reference(new hydra.java.syntax.ReferenceType.Array(new hydra.java.syntax.ArrayType(newDims.get(), variant))));
          }

          @Override
          public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> visit(hydra.java.syntax.ReferenceType.Variable ignored) {
            return hydra.util.Either.<hydra.errors.Error_, hydra.java.syntax.Type>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError("don't know how to make Java reference type into array type")));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.java.syntax.Type> visit(hydra.java.syntax.Type.Primitive ignored) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.java.syntax.Type>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError("don't know how to make Java type into array type")));
      }
    });
  }

  static hydra.java.syntax.ReferenceType typeParameterToReferenceType(hydra.java.syntax.TypeParameter tp) {
    return hydra.java.Utils.javaTypeVariable((tp).identifier.value.value);
  }

  static hydra.java.syntax.TypeArgument typeParameterToTypeArgument(hydra.java.syntax.TypeParameter tp) {
    return hydra.java.Utils.javaTypeIdentifierToJavaTypeArgument((tp).identifier);
  }

  static String unTypeParameter(hydra.java.syntax.TypeParameter tp) {
    return (tp).identifier.value.value;
  }

  static String unescape(String s) {
    return hydra.lib.strings.FromList.apply(hydra.lib.lists.Drop.apply(
      1,
      hydra.lib.strings.ToList.apply(s)));
  }

  static hydra.core.Name uniqueVarName(hydra.java.environment.Aliases aliases, hydra.core.Name name) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        name,
        (aliases).inScopeJavaVars),
      () -> hydra.java.Utils.uniqueVarName_go(
        aliases,
        (name).value,
        2),
      () -> name);
  }

  static hydra.core.Name uniqueVarName_go(hydra.java.environment.Aliases aliases, String base, Integer n) {
    hydra.core.Name candidate = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      base,
      hydra.lib.literals.ShowInt32.apply(n)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        candidate,
        (aliases).inScopeJavaVars),
      () -> hydra.java.Utils.uniqueVarName_go(
        aliases,
        base,
        hydra.lib.math.Add.apply(
          n,
          1)),
      () -> candidate);
  }

  static hydra.java.syntax.BlockStatement varDeclarationStatement(hydra.java.syntax.Identifier id, hydra.java.syntax.Expression rhs) {
    return new hydra.java.syntax.BlockStatement.LocalVariableDeclaration(new hydra.java.syntax.LocalVariableDeclarationStatement(new hydra.java.syntax.LocalVariableDeclaration((java.util.List<hydra.java.syntax.VariableModifier>) (java.util.Collections.<hydra.java.syntax.VariableModifier>emptyList()), new hydra.java.syntax.LocalVariableType.Var(), java.util.Arrays.asList(hydra.java.Utils.javaVariableDeclarator(
      id,
      hydra.util.Maybe.just(new hydra.java.syntax.VariableInitializer.Expression(rhs)))))));
  }

  static <T0> hydra.java.syntax.BlockStatement variableDeclarationStatement(T0 aliases, hydra.java.syntax.Type jtype, hydra.java.syntax.Identifier id, hydra.java.syntax.Expression rhs) {
    hydra.java.syntax.VariableInitializer init_ = new hydra.java.syntax.VariableInitializer.Expression(rhs);
    hydra.java.syntax.VariableDeclarator vdec = hydra.java.Utils.javaVariableDeclarator(
      id,
      hydra.util.Maybe.just(init_));
    return new hydra.java.syntax.BlockStatement.LocalVariableDeclaration(new hydra.java.syntax.LocalVariableDeclarationStatement(new hydra.java.syntax.LocalVariableDeclaration((java.util.List<hydra.java.syntax.VariableModifier>) (java.util.Collections.<hydra.java.syntax.VariableModifier>emptyList()), new hydra.java.syntax.LocalVariableType.Type(new hydra.java.syntax.UnannType(jtype)), java.util.Arrays.asList(vdec))));
  }

  static hydra.java.syntax.Identifier variableToJavaIdentifier(hydra.core.Name name) {
    String v = (name).value;
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        v,
        "_"),
      () -> new hydra.java.syntax.Identifier("ignored"),
      () -> new hydra.java.syntax.Identifier(hydra.java.Utils.sanitizeJavaName(v)));
  }

  static hydra.core.Name variantClassName(Boolean qualify, hydra.core.Name elName, hydra.core.Name fname) {
    String flocal = hydra.Formatting.capitalize((fname).value);
    hydra.packaging.QualifiedName qn = hydra.Names.qualifyName(elName);
    String local = (qn).local;
    hydra.util.Lazy<String> local1 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      qualify,
      () -> hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          local,
          "."),
        flocal),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          flocal,
          local),
        () -> hydra.lib.strings.Cat2.apply(
          flocal,
          "_"),
        () -> flocal)));
    hydra.util.Maybe<hydra.packaging.Namespace> ns_ = (qn).namespace;
    return hydra.Names.unqualifyName(new hydra.packaging.QualifiedName(ns_, local1.get()));
  }

  static hydra.java.syntax.ReferenceType visitorTypeVariable() {
    return hydra.java.Utils.javaTypeVariable("r");
  }
}
