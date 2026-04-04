// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java;

/**
 * Java utilities for constructing Java syntax trees
 */
public interface Utils {
  static hydra.ext.java.syntax.AdditiveExpression addExpressions(java.util.List<hydra.ext.java.syntax.MultiplicativeExpression> exprs) {
    hydra.util.Lazy<hydra.ext.java.syntax.AdditiveExpression> first = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.AdditiveExpression.Unary(hydra.lib.lists.Head.apply(exprs)));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.MultiplicativeExpression>> rest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(exprs));
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.ext.java.syntax.AdditiveExpression, java.util.function.Function<hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.AdditiveExpression>>) (ae -> (java.util.function.Function<hydra.ext.java.syntax.MultiplicativeExpression, hydra.ext.java.syntax.AdditiveExpression>) (me -> new hydra.ext.java.syntax.AdditiveExpression.Plus(new hydra.ext.java.syntax.AdditiveExpression_Binary(ae, me)))),
      first.get(),
      rest.get());
  }

  static hydra.ext.java.environment.Aliases addInScopeVar(hydra.core.Name name, hydra.ext.java.environment.Aliases aliases) {
    return new hydra.ext.java.environment.Aliases((aliases).currentNamespace, (aliases).packages, (aliases).branchVars, (aliases).recursiveVars, (aliases).inScopeTypeParams, (aliases).polymorphicLocals, hydra.lib.sets.Insert.apply(
      name,
      (aliases).inScopeJavaVars), (aliases).varRenames, (aliases).lambdaVars, (aliases).typeVarSubst, (aliases).trustedTypeVars, (aliases).methodCodomain, (aliases).thunkedVars);
  }

  static hydra.ext.java.environment.Aliases addInScopeVars(java.util.List<hydra.core.Name> names, hydra.ext.java.environment.Aliases aliases) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.ext.java.environment.Aliases, java.util.function.Function<hydra.core.Name, hydra.ext.java.environment.Aliases>>) (a -> (java.util.function.Function<hydra.core.Name, hydra.ext.java.environment.Aliases>) (n -> hydra.ext.java.Utils.addInScopeVar(
        n,
        a))),
      aliases,
      names);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> addJavaTypeParameter(hydra.ext.java.syntax.ReferenceType rt, hydra.ext.java.syntax.Type t, hydra.context.Context cx) {
    return (t).accept(new hydra.ext.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.Type.Reference rt1) {
        return (rt1).value.accept(new hydra.ext.java.syntax.ReferenceType.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ReferenceType.ClassOrInterface cit) {
            return (cit).value.accept(new hydra.ext.java.syntax.ClassOrInterfaceType.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ClassOrInterfaceType.Class_ ct) {
                java.util.List<hydra.ext.java.syntax.Annotation> anns = (ct).value.annotations;
                java.util.List<hydra.ext.java.syntax.TypeArgument> args = (ct).value.arguments;
                hydra.ext.java.syntax.TypeIdentifier id = (ct).value.identifier;
                hydra.ext.java.syntax.ClassTypeQualifier qual = (ct).value.qualifier;
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(new hydra.ext.java.syntax.ClassType(anns, qual, id, hydra.lib.lists.Concat2.apply(
                  args,
                  java.util.Arrays.asList(new hydra.ext.java.syntax.TypeArgument.Reference(rt))))))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ClassOrInterfaceType.Interface ignored) {
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("expected a Java class type")), cx)));
              }
            });
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ReferenceType.Variable tv) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaTypeVariableToType((tv).value));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ReferenceType.Array ignored) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("expected a Java class or interface type, or a variable")), cx)));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.Type.Primitive ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("expected a reference type")), cx)));
      }
    });
  }

  static hydra.ext.java.environment.Aliases addVarRename(hydra.core.Name original, hydra.core.Name renamed, hydra.ext.java.environment.Aliases aliases) {
    return new hydra.ext.java.environment.Aliases((aliases).currentNamespace, (aliases).packages, (aliases).branchVars, (aliases).recursiveVars, (aliases).inScopeTypeParams, (aliases).polymorphicLocals, (aliases).inScopeJavaVars, hydra.lib.maps.Insert.apply(
      original,
      renamed,
      (aliases).varRenames), (aliases).lambdaVars, (aliases).typeVarSubst, (aliases).trustedTypeVars, (aliases).methodCodomain, (aliases).thunkedVars);
  }

  static hydra.ext.java.syntax.ExpressionName fieldExpression(hydra.ext.java.syntax.Identifier varId, hydra.ext.java.syntax.Identifier fieldId) {
    return new hydra.ext.java.syntax.ExpressionName(hydra.util.Maybe.just(new hydra.ext.java.syntax.AmbiguousName(java.util.Arrays.asList(varId))), fieldId);
  }

  static hydra.ext.java.syntax.Expression fieldNameToJavaExpression(hydra.core.Name fname) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Name(hydra.ext.java.Utils.javaIdentifierToJavaExpressionName(hydra.ext.java.Utils.fieldNameToJavaIdentifier(fname)))))))))))))))))))))));
  }

  static hydra.ext.java.syntax.Identifier fieldNameToJavaIdentifier(hydra.core.Name fname) {
    return hydra.ext.java.Utils.javaIdentifier((fname).value);
  }

  static hydra.ext.java.syntax.VariableDeclarator fieldNameToJavaVariableDeclarator(hydra.core.Name fname) {
    return hydra.ext.java.Utils.javaVariableDeclarator(
      hydra.ext.java.Utils.javaIdentifier((fname).value),
      (hydra.util.Maybe<hydra.ext.java.syntax.VariableInitializer>) (hydra.util.Maybe.<hydra.ext.java.syntax.VariableInitializer>nothing()));
  }

  static hydra.ext.java.syntax.VariableDeclaratorId fieldNameToJavaVariableDeclaratorId(hydra.core.Name fname) {
    return hydra.ext.java.Utils.javaVariableDeclaratorId(hydra.ext.java.Utils.javaIdentifier((fname).value));
  }

  static hydra.ext.java.syntax.BlockStatement finalVarDeclarationStatement(hydra.ext.java.syntax.Identifier id, hydra.ext.java.syntax.Expression rhs) {
    return new hydra.ext.java.syntax.BlockStatement.LocalVariableDeclaration(new hydra.ext.java.syntax.LocalVariableDeclarationStatement(new hydra.ext.java.syntax.LocalVariableDeclaration(java.util.Arrays.asList(new hydra.ext.java.syntax.VariableModifier.Final()), new hydra.ext.java.syntax.LocalVariableType.Var(), java.util.Arrays.asList(hydra.ext.java.Utils.javaVariableDeclarator(
      id,
      hydra.util.Maybe.just(new hydra.ext.java.syntax.VariableInitializer.Expression(rhs)))))));
  }

  static hydra.ext.java.environment.Aliases importAliasesForModule(hydra.module.Module mod) {
    return new hydra.ext.java.environment.Aliases((mod).namespace, (java.util.Map<hydra.module.Namespace, hydra.ext.java.syntax.PackageName>) ((java.util.Map<hydra.module.Namespace, hydra.ext.java.syntax.PackageName>) (hydra.lib.maps.Empty.<hydra.module.Namespace, hydra.ext.java.syntax.PackageName>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()));
  }

  static hydra.ext.java.syntax.InterfaceMemberDeclaration interfaceMethodDeclaration(java.util.List<hydra.ext.java.syntax.InterfaceMethodModifier> mods, java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, String methodName, java.util.List<hydra.ext.java.syntax.FormalParameter> params, hydra.ext.java.syntax.Result result, hydra.util.Maybe<java.util.List<hydra.ext.java.syntax.BlockStatement>> stmts) {
    return new hydra.ext.java.syntax.InterfaceMemberDeclaration.InterfaceMethod(new hydra.ext.java.syntax.InterfaceMethodDeclaration(mods, hydra.ext.java.Utils.javaMethodHeader(
      tparams,
      methodName,
      params,
      result), hydra.ext.java.Utils.javaMethodBody(stmts)));
  }

  static Boolean isEscaped(String s) {
    return hydra.lib.equality.Equal.apply(
      hydra.lib.strings.CharAt.apply(
        0,
        s),
      36);
  }

  static hydra.ext.java.syntax.Expression javaAdditiveExpressionToJavaExpression(hydra.ext.java.syntax.AdditiveExpression ae) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(ae))))))))))))))));
  }

  static hydra.ext.java.syntax.Expression javaArrayCreation(hydra.ext.java.syntax.PrimitiveTypeWithAnnotations primType, hydra.util.Maybe<hydra.ext.java.syntax.ArrayInitializer> minit) {
    hydra.util.Lazy<hydra.ext.java.syntax.ArrayInitializer> init_ = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
      minit,
      () -> new hydra.ext.java.syntax.ArrayInitializer((java.util.List<java.util.List<hydra.ext.java.syntax.VariableInitializer>>) (java.util.Collections.<java.util.List<hydra.ext.java.syntax.VariableInitializer>>emptyList())),
      (java.util.function.Function<hydra.ext.java.syntax.ArrayInitializer, hydra.ext.java.syntax.ArrayInitializer>) (i -> i)));
    return hydra.ext.java.Utils.javaPrimaryToJavaExpression(new hydra.ext.java.syntax.Primary.ArrayCreation(new hydra.ext.java.syntax.ArrayCreationExpression.PrimitiveArray(new hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray(primType, (java.util.List<hydra.ext.java.syntax.Dims>) (java.util.Collections.<hydra.ext.java.syntax.Dims>emptyList()), init_.get()))));
  }

  static hydra.ext.java.syntax.ArrayInitializer javaArrayInitializer(java.util.List<hydra.ext.java.syntax.Expression> exprs) {
    return new hydra.ext.java.syntax.ArrayInitializer(java.util.Arrays.asList(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.VariableInitializer>) (e -> new hydra.ext.java.syntax.VariableInitializer.Expression(e)),
      exprs)));
  }

  static hydra.ext.java.syntax.Statement javaAssignmentStatement(hydra.ext.java.syntax.LeftHandSide lhs, hydra.ext.java.syntax.Expression rhs) {
    return new hydra.ext.java.syntax.Statement.WithoutTrailing(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Expression(new hydra.ext.java.syntax.ExpressionStatement(new hydra.ext.java.syntax.StatementExpression.Assignment(new hydra.ext.java.syntax.Assignment(lhs, new hydra.ext.java.syntax.AssignmentOperator.Simple(), rhs)))));
  }

  static hydra.ext.java.syntax.Literal javaBoolean(Boolean b) {
    return new hydra.ext.java.syntax.Literal.Boolean_(b);
  }

  static hydra.ext.java.syntax.Expression javaBooleanExpression(Boolean b) {
    return hydra.ext.java.Utils.javaPrimaryToJavaExpression(hydra.ext.java.Utils.javaLiteralToJavaPrimary(hydra.ext.java.Utils.javaBoolean(b)));
  }

  static hydra.ext.java.syntax.Type javaBooleanType() {
    return hydra.ext.java.Utils.javaPrimitiveTypeToJavaType(new hydra.ext.java.syntax.PrimitiveType.Boolean_());
  }

  static hydra.ext.java.syntax.PrimitiveTypeWithAnnotations javaBytePrimitiveType() {
    return new hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Byte_())), (java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList()));
  }

  static hydra.ext.java.syntax.CastExpression javaCastExpression(hydra.ext.java.syntax.ReferenceType rt, hydra.ext.java.syntax.UnaryExpression expr) {
    return new hydra.ext.java.syntax.CastExpression.NotPlusMinus(new hydra.ext.java.syntax.CastExpression_NotPlusMinus(new hydra.ext.java.syntax.CastExpression_RefAndBounds(rt, (java.util.List<hydra.ext.java.syntax.AdditionalBound>) (java.util.Collections.<hydra.ext.java.syntax.AdditionalBound>emptyList())), expr));
  }

  static hydra.ext.java.syntax.Expression javaCastExpressionToJavaExpression(hydra.ext.java.syntax.CastExpression ce) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Cast(ce))))))))))))))))))));
  }

  static hydra.ext.java.syntax.CastExpression javaCastPrimitive(hydra.ext.java.syntax.PrimitiveType pt, hydra.ext.java.syntax.UnaryExpression expr) {
    return new hydra.ext.java.syntax.CastExpression.Primitive(new hydra.ext.java.syntax.CastExpression_Primitive(new hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(pt, (java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList())), expr));
  }

  static hydra.ext.java.syntax.ClassDeclaration javaClassDeclaration(hydra.ext.java.environment.Aliases aliases, java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName, java.util.List<hydra.ext.java.syntax.ClassModifier> mods, hydra.util.Maybe<hydra.core.Name> supname, java.util.List<hydra.ext.java.syntax.InterfaceType> impls, java.util.List<hydra.ext.java.syntax.ClassBodyDeclarationWithComments> bodyDecls) {
    hydra.util.Lazy<hydra.util.Maybe<hydra.ext.java.syntax.ClassType>> extends_ = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.ClassType>) (n -> hydra.ext.java.Utils.nameToJavaClassType(
        aliases,
        true,
        (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.Collections.<hydra.ext.java.syntax.TypeArgument>emptyList()),
        n,
        (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))),
      supname));
    return new hydra.ext.java.syntax.ClassDeclaration.Normal(new hydra.ext.java.syntax.NormalClassDeclaration(mods, hydra.ext.java.Utils.javaDeclName(elName), tparams, extends_.get(), impls, new hydra.ext.java.syntax.ClassBody(bodyDecls)));
  }

  static hydra.ext.java.syntax.ClassType javaClassType(java.util.List<hydra.ext.java.syntax.ReferenceType> args, hydra.util.Maybe<hydra.ext.java.syntax.PackageName> pkg, String id) {
    hydra.util.Lazy<hydra.ext.java.syntax.ClassTypeQualifier> qual = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
      pkg,
      () -> new hydra.ext.java.syntax.ClassTypeQualifier.None(),
      (java.util.function.Function<hydra.ext.java.syntax.PackageName, hydra.ext.java.syntax.ClassTypeQualifier>) (p -> new hydra.ext.java.syntax.ClassTypeQualifier.Package_(p))));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.TypeArgument>> targs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.TypeArgument>) (rt -> new hydra.ext.java.syntax.TypeArgument.Reference(rt)),
      args));
    return new hydra.ext.java.syntax.ClassType((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList()), qual.get(), hydra.ext.java.Utils.javaTypeIdentifier(id), targs.get());
  }

  static hydra.ext.java.syntax.Type javaClassTypeToJavaType(hydra.ext.java.syntax.ClassType ct) {
    return new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(ct)));
  }

  static hydra.ext.java.syntax.Expression javaConditionalAndExpressionToJavaExpression(hydra.ext.java.syntax.ConditionalAndExpression cae) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(cae)))));
  }

  static hydra.ext.java.syntax.Expression javaConstructorCall(hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate ci, java.util.List<hydra.ext.java.syntax.Expression> args, hydra.util.Maybe<hydra.ext.java.syntax.ClassBody> mbody) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.ClassInstance(new hydra.ext.java.syntax.ClassInstanceCreationExpression((hydra.util.Maybe<hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier>nothing()), new hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression((java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.Collections.<hydra.ext.java.syntax.TypeArgument>emptyList()), ci, args, mbody)))))))))))))))))))))))));
  }

  static hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate javaConstructorName(hydra.ext.java.syntax.Identifier id, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond> targs) {
    return new hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate(java.util.Arrays.asList(new hydra.ext.java.syntax.AnnotatedIdentifier((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList()), id)), targs);
  }

  static hydra.ext.java.syntax.TypeIdentifier javaDeclName(hydra.core.Name name) {
    return new hydra.ext.java.syntax.TypeIdentifier(hydra.ext.java.Utils.javaVariableName(name));
  }

  static hydra.ext.java.syntax.CastExpression javaDoubleCastExpression(hydra.ext.java.syntax.ReferenceType rawRt, hydra.ext.java.syntax.ReferenceType targetRt, hydra.ext.java.syntax.UnaryExpression expr) {
    hydra.ext.java.syntax.Expression firstCast = hydra.ext.java.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.Utils.javaCastExpression(
      rawRt,
      expr));
    return hydra.ext.java.Utils.javaCastExpression(
      targetRt,
      hydra.ext.java.Utils.javaExpressionToJavaUnaryExpression(firstCast));
  }

  static hydra.ext.java.syntax.Expression javaDoubleCastExpressionToJavaExpression(hydra.ext.java.syntax.ReferenceType rawRt, hydra.ext.java.syntax.ReferenceType targetRt, hydra.ext.java.syntax.UnaryExpression expr) {
    return hydra.ext.java.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.Utils.javaDoubleCastExpression(
      rawRt,
      targetRt,
      expr));
  }

  static hydra.ext.java.syntax.Statement javaEmptyStatement() {
    return new hydra.ext.java.syntax.Statement.WithoutTrailing(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Empty());
  }

  static hydra.ext.java.syntax.Expression javaEqualityExpressionToJavaExpression(hydra.ext.java.syntax.EqualityExpression ee) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(ee)))))))))))));
  }

  static hydra.ext.java.syntax.InclusiveOrExpression javaEqualityExpressionToJavaInclusiveOrExpression(hydra.ext.java.syntax.EqualityExpression ee) {
    return new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(ee))))));
  }

  static hydra.ext.java.syntax.EqualityExpression javaEquals(hydra.ext.java.syntax.EqualityExpression lhs, hydra.ext.java.syntax.RelationalExpression rhs) {
    return new hydra.ext.java.syntax.EqualityExpression.Equal(new hydra.ext.java.syntax.EqualityExpression_Binary(lhs, rhs));
  }

  static hydra.ext.java.syntax.EqualityExpression javaEqualsNull(hydra.ext.java.syntax.EqualityExpression lhs) {
    return hydra.ext.java.Utils.javaEquals(
      lhs,
      hydra.ext.java.Utils.javaLiteralToJavaRelationalExpression(new hydra.ext.java.syntax.Literal.Null()));
  }

  static hydra.ext.java.syntax.Expression javaExpressionNameToJavaExpression(hydra.ext.java.syntax.ExpressionName en) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Name(en)))))))))))))))))))));
  }

  static hydra.ext.java.syntax.Primary javaExpressionToJavaPrimary(hydra.ext.java.syntax.Expression e) {
    hydra.ext.java.syntax.Primary fallback = new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.Parens(e));
    return (e).accept(new hydra.ext.java.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.ext.java.syntax.Primary otherwise(hydra.ext.java.syntax.Expression instance) {
        return fallback;
      }

      @Override
      public hydra.ext.java.syntax.Primary visit(hydra.ext.java.syntax.Expression.Assignment ae) {
        return (ae).value.accept(new hydra.ext.java.syntax.AssignmentExpression.PartialVisitor<>() {
          @Override
          public hydra.ext.java.syntax.Primary otherwise(hydra.ext.java.syntax.AssignmentExpression instance) {
            return fallback;
          }

          @Override
          public hydra.ext.java.syntax.Primary visit(hydra.ext.java.syntax.AssignmentExpression.Conditional ce) {
            return (ce).value.accept(new hydra.ext.java.syntax.ConditionalExpression.PartialVisitor<>() {
              @Override
              public hydra.ext.java.syntax.Primary otherwise(hydra.ext.java.syntax.ConditionalExpression instance) {
                return fallback;
              }

              @Override
              public hydra.ext.java.syntax.Primary visit(hydra.ext.java.syntax.ConditionalExpression.Simple cor) {
                java.util.List<hydra.ext.java.syntax.ConditionalAndExpression> cands = (cor).value.value;
                return hydra.lib.logic.IfElse.lazy(
                  hydra.lib.equality.Equal.apply(
                    hydra.lib.lists.Length.apply(cands),
                    1),
                  () -> ((java.util.function.Supplier<hydra.ext.java.syntax.Primary>) (() -> {
                    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.InclusiveOrExpression>> iors = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(cands).value);
                    return hydra.lib.logic.IfElse.lazy(
                      hydra.lib.equality.Equal.apply(
                        hydra.lib.lists.Length.apply(iors.get()),
                        1),
                      () -> ((java.util.function.Supplier<hydra.ext.java.syntax.Primary>) (() -> {
                        hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.ExclusiveOrExpression>> xors = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(iors.get()).value);
                        return hydra.lib.logic.IfElse.lazy(
                          hydra.lib.equality.Equal.apply(
                            hydra.lib.lists.Length.apply(xors.get()),
                            1),
                          () -> ((java.util.function.Supplier<hydra.ext.java.syntax.Primary>) (() -> {
                            hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.AndExpression>> ands = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(xors.get()).value);
                            return hydra.lib.logic.IfElse.lazy(
                              hydra.lib.equality.Equal.apply(
                                hydra.lib.lists.Length.apply(ands.get()),
                                1),
                              () -> ((java.util.function.Supplier<hydra.ext.java.syntax.Primary>) (() -> {
                                hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.EqualityExpression>> eqs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(ands.get()).value);
                                return hydra.lib.logic.IfElse.lazy(
                                  hydra.lib.equality.Equal.apply(
                                    hydra.lib.lists.Length.apply(eqs.get()),
                                    1),
                                  () -> hydra.lib.lists.Head.apply(eqs.get()).accept(new hydra.ext.java.syntax.EqualityExpression.PartialVisitor<>() {
                                    @Override
                                    public hydra.ext.java.syntax.Primary otherwise(hydra.ext.java.syntax.EqualityExpression instance) {
                                      return fallback;
                                    }

                                    @Override
                                    public hydra.ext.java.syntax.Primary visit(hydra.ext.java.syntax.EqualityExpression.Unary rel) {
                                      return (rel).value.accept(new hydra.ext.java.syntax.RelationalExpression.PartialVisitor<>() {
                                        @Override
                                        public hydra.ext.java.syntax.Primary otherwise(hydra.ext.java.syntax.RelationalExpression instance) {
                                          return fallback;
                                        }

                                        @Override
                                        public hydra.ext.java.syntax.Primary visit(hydra.ext.java.syntax.RelationalExpression.Simple shift) {
                                          return (shift).value.accept(new hydra.ext.java.syntax.ShiftExpression.PartialVisitor<>() {
                                            @Override
                                            public hydra.ext.java.syntax.Primary otherwise(hydra.ext.java.syntax.ShiftExpression instance) {
                                              return fallback;
                                            }

                                            @Override
                                            public hydra.ext.java.syntax.Primary visit(hydra.ext.java.syntax.ShiftExpression.Unary add) {
                                              return (add).value.accept(new hydra.ext.java.syntax.AdditiveExpression.PartialVisitor<>() {
                                                @Override
                                                public hydra.ext.java.syntax.Primary otherwise(hydra.ext.java.syntax.AdditiveExpression instance) {
                                                  return fallback;
                                                }

                                                @Override
                                                public hydra.ext.java.syntax.Primary visit(hydra.ext.java.syntax.AdditiveExpression.Unary mul) {
                                                  return (mul).value.accept(new hydra.ext.java.syntax.MultiplicativeExpression.PartialVisitor<>() {
                                                    @Override
                                                    public hydra.ext.java.syntax.Primary otherwise(hydra.ext.java.syntax.MultiplicativeExpression instance) {
                                                      return fallback;
                                                    }

                                                    @Override
                                                    public hydra.ext.java.syntax.Primary visit(hydra.ext.java.syntax.MultiplicativeExpression.Unary unary) {
                                                      return (unary).value.accept(new hydra.ext.java.syntax.UnaryExpression.PartialVisitor<>() {
                                                        @Override
                                                        public hydra.ext.java.syntax.Primary otherwise(hydra.ext.java.syntax.UnaryExpression instance) {
                                                          return fallback;
                                                        }

                                                        @Override
                                                        public hydra.ext.java.syntax.Primary visit(hydra.ext.java.syntax.UnaryExpression.Other npm) {
                                                          return (npm).value.accept(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.PartialVisitor<>() {
                                                            @Override
                                                            public hydra.ext.java.syntax.Primary otherwise(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus instance) {
                                                              return fallback;
                                                            }

                                                            @Override
                                                            public hydra.ext.java.syntax.Primary visit(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix pf) {
                                                              return (pf).value.accept(new hydra.ext.java.syntax.PostfixExpression.PartialVisitor<>() {
                                                                @Override
                                                                public hydra.ext.java.syntax.Primary otherwise(hydra.ext.java.syntax.PostfixExpression instance) {
                                                                  return fallback;
                                                                }

                                                                @Override
                                                                public hydra.ext.java.syntax.Primary visit(hydra.ext.java.syntax.PostfixExpression.Primary p) {
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
                                  }),
                                  () -> fallback);
                              })).get(),
                              () -> fallback);
                          })).get(),
                          () -> fallback);
                      })).get(),
                      () -> fallback);
                  })).get(),
                  () -> fallback);
              }
            });
          }
        });
      }
    });
  }

  static hydra.ext.java.syntax.UnaryExpression javaExpressionToJavaUnaryExpression(hydra.ext.java.syntax.Expression e) {
    return new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.Parens(e)))));
  }

  static hydra.ext.java.syntax.Expression javaFieldAccessToJavaExpression(hydra.ext.java.syntax.FieldAccess fa) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.FieldAccess(fa)))))))))))))))))))))));
  }

  static hydra.ext.java.syntax.Identifier javaIdentifier(String s) {
    return new hydra.ext.java.syntax.Identifier(hydra.ext.java.Utils.sanitizeJavaName(s));
  }

  static hydra.ext.java.syntax.Expression javaIdentifierToJavaExpression(hydra.ext.java.syntax.Identifier id) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Name(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), id))))))))))))))))))))));
  }

  static hydra.ext.java.syntax.ExpressionName javaIdentifierToJavaExpressionName(hydra.ext.java.syntax.Identifier id) {
    return new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), id);
  }

  static hydra.ext.java.syntax.RelationalExpression javaIdentifierToJavaRelationalExpression(hydra.ext.java.syntax.Identifier id) {
    return new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Name(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), id))))))));
  }

  static hydra.ext.java.syntax.UnaryExpression javaIdentifierToJavaUnaryExpression(hydra.ext.java.syntax.Identifier id) {
    return new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Name(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), id))));
  }

  static hydra.ext.java.syntax.RelationalExpression javaInstanceOf(hydra.ext.java.syntax.RelationalExpression lhs, hydra.ext.java.syntax.ReferenceType rhs) {
    return new hydra.ext.java.syntax.RelationalExpression.Instanceof(new hydra.ext.java.syntax.RelationalExpression_InstanceOf(lhs, rhs));
  }

  static hydra.ext.java.syntax.Literal javaInt(java.math.BigInteger i) {
    return new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(i));
  }

  static hydra.ext.java.syntax.Expression javaIntExpression(java.math.BigInteger i) {
    return hydra.ext.java.Utils.javaPrimaryToJavaExpression(hydra.ext.java.Utils.javaLiteralToJavaPrimary(hydra.ext.java.Utils.javaInt(i)));
  }

  static hydra.ext.java.syntax.Type javaIntType() {
    return hydra.ext.java.Utils.javaPrimitiveTypeToJavaType(new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Int())));
  }

  static hydra.ext.java.syntax.ClassBodyDeclaration javaInterfaceDeclarationToJavaClassBodyDeclaration(hydra.ext.java.syntax.NormalInterfaceDeclaration nid) {
    return new hydra.ext.java.syntax.ClassBodyDeclaration.ClassMember(new hydra.ext.java.syntax.ClassMemberDeclaration.Interface(new hydra.ext.java.syntax.InterfaceDeclaration.NormalInterface(nid)));
  }

  static hydra.ext.java.syntax.Expression javaLambda(hydra.core.Name v, hydra.ext.java.syntax.Expression body) {
    return new hydra.ext.java.syntax.Expression.Lambda(new hydra.ext.java.syntax.LambdaExpression(new hydra.ext.java.syntax.LambdaParameters.Single(hydra.ext.java.Utils.variableToJavaIdentifier(v)), new hydra.ext.java.syntax.LambdaBody.Expression(body)));
  }

  static hydra.ext.java.syntax.Expression javaLambdaFromBlock(hydra.core.Name v, hydra.ext.java.syntax.Block block) {
    return new hydra.ext.java.syntax.Expression.Lambda(new hydra.ext.java.syntax.LambdaExpression(new hydra.ext.java.syntax.LambdaParameters.Single(hydra.ext.java.Utils.variableToJavaIdentifier(v)), new hydra.ext.java.syntax.LambdaBody.Block(block)));
  }

  static hydra.ext.java.syntax.Expression javaLiteralToJavaExpression(hydra.ext.java.syntax.Literal lit) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.Literal(lit)))))))))))))))))))))));
  }

  static hydra.ext.java.syntax.MultiplicativeExpression javaLiteralToJavaMultiplicativeExpression(hydra.ext.java.syntax.Literal lit) {
    return new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.Literal(lit))))));
  }

  static hydra.ext.java.syntax.Primary javaLiteralToJavaPrimary(hydra.ext.java.syntax.Literal lit) {
    return new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.Literal(lit));
  }

  static hydra.ext.java.syntax.RelationalExpression javaLiteralToJavaRelationalExpression(hydra.ext.java.syntax.Literal lit) {
    return new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.Literal(lit)))))))));
  }

  static hydra.ext.java.syntax.ClassBodyDeclaration javaMemberField(java.util.List<hydra.ext.java.syntax.FieldModifier> mods, hydra.ext.java.syntax.Type jt, hydra.ext.java.syntax.VariableDeclarator v) {
    return new hydra.ext.java.syntax.ClassBodyDeclaration.ClassMember(new hydra.ext.java.syntax.ClassMemberDeclaration.Field(new hydra.ext.java.syntax.FieldDeclaration(mods, new hydra.ext.java.syntax.UnannType(jt), java.util.Arrays.asList(v))));
  }

  static hydra.ext.java.syntax.MethodBody javaMethodBody(hydra.util.Maybe<java.util.List<hydra.ext.java.syntax.BlockStatement>> mstmts) {
    return hydra.lib.maybes.Cases.applyLazy(
      mstmts,
      () -> new hydra.ext.java.syntax.MethodBody.None(),
      (java.util.function.Function<java.util.List<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.syntax.MethodBody>) (stmts -> new hydra.ext.java.syntax.MethodBody.Block(new hydra.ext.java.syntax.Block(stmts))));
  }

  static hydra.ext.java.syntax.ClassBodyDeclaration javaMethodDeclarationToJavaClassBodyDeclaration(hydra.ext.java.syntax.MethodDeclaration md) {
    return new hydra.ext.java.syntax.ClassBodyDeclaration.ClassMember(new hydra.ext.java.syntax.ClassMemberDeclaration.Method(md));
  }

  static hydra.ext.java.syntax.MethodHeader javaMethodHeader(java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, String methodName, java.util.List<hydra.ext.java.syntax.FormalParameter> params, hydra.ext.java.syntax.Result result) {
    return new hydra.ext.java.syntax.MethodHeader(tparams, result, new hydra.ext.java.syntax.MethodDeclarator(new hydra.ext.java.syntax.Identifier(methodName), (hydra.util.Maybe<hydra.ext.java.syntax.ReceiverParameter>) (hydra.util.Maybe.<hydra.ext.java.syntax.ReceiverParameter>nothing()), params), (hydra.util.Maybe<hydra.ext.java.syntax.Throws>) (hydra.util.Maybe.<hydra.ext.java.syntax.Throws>nothing()));
  }

  static hydra.ext.java.syntax.Expression javaMethodInvocationToJavaExpression(hydra.ext.java.syntax.MethodInvocation mi) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.MethodInvocation(mi)))))))))))))))))))))));
  }

  static hydra.ext.java.syntax.PostfixExpression javaMethodInvocationToJavaPostfixExpression(hydra.ext.java.syntax.MethodInvocation mi) {
    return new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.MethodInvocation(mi)));
  }

  static hydra.ext.java.syntax.Primary javaMethodInvocationToJavaPrimary(hydra.ext.java.syntax.MethodInvocation mi) {
    return new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.MethodInvocation(mi));
  }

  static hydra.ext.java.syntax.Statement javaMethodInvocationToJavaStatement(hydra.ext.java.syntax.MethodInvocation mi) {
    return new hydra.ext.java.syntax.Statement.WithoutTrailing(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Expression(new hydra.ext.java.syntax.ExpressionStatement(new hydra.ext.java.syntax.StatementExpression.MethodInvocation(mi))));
  }

  static hydra.ext.java.syntax.RelationalExpression javaMultiplicativeExpressionToJavaRelationalExpression(hydra.ext.java.syntax.MultiplicativeExpression me) {
    return new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(me)));
  }

  static hydra.ext.java.syntax.PackageDeclaration javaPackageDeclaration(hydra.module.Namespace ns) {
    return new hydra.ext.java.syntax.PackageDeclaration((java.util.List<hydra.ext.java.syntax.PackageModifier>) (java.util.Collections.<hydra.ext.java.syntax.PackageModifier>emptyList()), hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, hydra.ext.java.syntax.Identifier>) (s -> new hydra.ext.java.syntax.Identifier(s)),
      hydra.lib.strings.SplitOn.apply(
        ".",
        (ns).value)));
  }

  static hydra.ext.java.syntax.EqualityExpression javaPostfixExpressionToJavaEqualityExpression(hydra.ext.java.syntax.PostfixExpression pe) {
    return new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe)))))));
  }

  static hydra.ext.java.syntax.Expression javaPostfixExpressionToJavaExpression(hydra.ext.java.syntax.PostfixExpression pe) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe))))))))))))))))))));
  }

  static hydra.ext.java.syntax.InclusiveOrExpression javaPostfixExpressionToJavaInclusiveOrExpression(hydra.ext.java.syntax.PostfixExpression pe) {
    return new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe)))))))))))));
  }

  static hydra.ext.java.syntax.RelationalExpression javaPostfixExpressionToJavaRelationalExpression(hydra.ext.java.syntax.PostfixExpression pe) {
    return new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe))))));
  }

  static hydra.ext.java.syntax.UnaryExpression javaPostfixExpressionToJavaUnaryExpression(hydra.ext.java.syntax.PostfixExpression pe) {
    return new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(pe));
  }

  static hydra.ext.java.syntax.Expression javaPrimaryToJavaExpression(hydra.ext.java.syntax.Primary p) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(p)))))))))))))))))))));
  }

  static hydra.ext.java.syntax.UnaryExpression javaPrimaryToJavaUnaryExpression(hydra.ext.java.syntax.Primary p) {
    return new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(p)));
  }

  static hydra.ext.java.syntax.Type javaPrimitiveTypeToJavaType(hydra.ext.java.syntax.PrimitiveType pt) {
    return new hydra.ext.java.syntax.Type.Primitive(new hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(pt, (java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList())));
  }

  static hydra.ext.java.syntax.Type javaRefType(java.util.List<hydra.ext.java.syntax.ReferenceType> args, hydra.util.Maybe<hydra.ext.java.syntax.PackageName> pkg, String id) {
    return new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(hydra.ext.java.Utils.javaClassType(
      args,
      pkg,
      id))));
  }

  static hydra.ext.java.syntax.ReferenceType javaReferenceTypeToRawType(hydra.ext.java.syntax.ReferenceType rt) {
    return (rt).accept(new hydra.ext.java.syntax.ReferenceType.PartialVisitor<>() {
      @Override
      public hydra.ext.java.syntax.ReferenceType otherwise(hydra.ext.java.syntax.ReferenceType instance) {
        return rt;
      }

      @Override
      public hydra.ext.java.syntax.ReferenceType visit(hydra.ext.java.syntax.ReferenceType.ClassOrInterface cit) {
        return (cit).value.accept(new hydra.ext.java.syntax.ClassOrInterfaceType.PartialVisitor<>() {
          @Override
          public hydra.ext.java.syntax.ReferenceType visit(hydra.ext.java.syntax.ClassOrInterfaceType.Class_ ct) {
            java.util.List<hydra.ext.java.syntax.Annotation> anns = (ct).value.annotations;
            hydra.ext.java.syntax.TypeIdentifier id = (ct).value.identifier;
            hydra.ext.java.syntax.ClassTypeQualifier qual = (ct).value.qualifier;
            return new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(new hydra.ext.java.syntax.ClassType(anns, qual, id, (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.Collections.<hydra.ext.java.syntax.TypeArgument>emptyList()))));
          }

          @Override
          public hydra.ext.java.syntax.ReferenceType visit(hydra.ext.java.syntax.ClassOrInterfaceType.Interface it) {
            hydra.ext.java.syntax.ClassType ct = (it).value.value;
            java.util.List<hydra.ext.java.syntax.Annotation> anns = (ct).annotations;
            hydra.ext.java.syntax.TypeIdentifier id = (ct).identifier;
            hydra.ext.java.syntax.ClassTypeQualifier qual = (ct).qualifier;
            return new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Interface(new hydra.ext.java.syntax.InterfaceType(new hydra.ext.java.syntax.ClassType(anns, qual, id, (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.Collections.<hydra.ext.java.syntax.TypeArgument>emptyList())))));
          }
        });
      }
    });
  }

  static hydra.ext.java.syntax.EqualityExpression javaRelationalExpressionToJavaEqualityExpression(hydra.ext.java.syntax.RelationalExpression re) {
    return new hydra.ext.java.syntax.EqualityExpression.Unary(re);
  }

  static hydra.ext.java.syntax.Expression javaRelationalExpressionToJavaExpression(hydra.ext.java.syntax.RelationalExpression re) {
    return hydra.ext.java.Utils.javaEqualityExpressionToJavaExpression(new hydra.ext.java.syntax.EqualityExpression.Unary(re));
  }

  static hydra.ext.java.syntax.UnaryExpression javaRelationalExpressionToJavaUnaryExpression(hydra.ext.java.syntax.RelationalExpression re) {
    return new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.Parens(new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(re)))))))))))))))))));
  }

  static hydra.ext.java.syntax.Statement javaReturnStatement(hydra.util.Maybe<hydra.ext.java.syntax.Expression> mex) {
    return new hydra.ext.java.syntax.Statement.WithoutTrailing(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Return(new hydra.ext.java.syntax.ReturnStatement(mex)));
  }

  static hydra.ext.java.syntax.Block javaStatementsToBlock(java.util.List<hydra.ext.java.syntax.Statement> stmts) {
    return new hydra.ext.java.syntax.Block(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.java.syntax.Statement, hydra.ext.java.syntax.BlockStatement>) (s -> new hydra.ext.java.syntax.BlockStatement.Statement(s)),
      stmts));
  }

  static hydra.ext.java.syntax.Literal javaString(String s) {
    return new hydra.ext.java.syntax.Literal.String_(new hydra.ext.java.syntax.StringLiteral(s));
  }

  static hydra.ext.java.syntax.MultiplicativeExpression javaStringMultiplicativeExpression(String s) {
    return hydra.ext.java.Utils.javaLiteralToJavaMultiplicativeExpression(hydra.ext.java.Utils.javaString(s));
  }

  static hydra.ext.java.syntax.Expression javaThis() {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix(new hydra.ext.java.syntax.PostfixExpression.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.This()))))))))))))))))))))));
  }

  static hydra.ext.java.syntax.Statement javaThrowIllegalArgumentException(java.util.List<hydra.ext.java.syntax.Expression> args) {
    return hydra.ext.java.Utils.javaThrowStatement(hydra.ext.java.Utils.javaConstructorCall(
      hydra.ext.java.Utils.javaConstructorName(
        new hydra.ext.java.syntax.Identifier("IllegalArgumentException"),
        (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
      args,
      (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())));
  }

  static hydra.ext.java.syntax.Statement javaThrowIllegalStateException(java.util.List<hydra.ext.java.syntax.Expression> args) {
    return hydra.ext.java.Utils.javaThrowStatement(hydra.ext.java.Utils.javaConstructorCall(
      hydra.ext.java.Utils.javaConstructorName(
        new hydra.ext.java.syntax.Identifier("IllegalStateException"),
        (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
      args,
      (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())));
  }

  static hydra.ext.java.syntax.Statement javaThrowStatement(hydra.ext.java.syntax.Expression e) {
    return new hydra.ext.java.syntax.Statement.WithoutTrailing(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Throw(new hydra.ext.java.syntax.ThrowStatement(e)));
  }

  static hydra.ext.java.syntax.Type javaTypeFromTypeName(hydra.ext.java.environment.Aliases aliases, hydra.core.Name elName) {
    return hydra.ext.java.Utils.javaTypeVariableToType(new hydra.ext.java.syntax.TypeVariable((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList()), hydra.ext.java.Utils.nameToJavaTypeIdentifier(
      aliases,
      false,
      elName)));
  }

  static hydra.ext.java.syntax.TypeIdentifier javaTypeIdentifier(String s) {
    return new hydra.ext.java.syntax.TypeIdentifier(new hydra.ext.java.syntax.Identifier(s));
  }

  static hydra.ext.java.syntax.TypeArgument javaTypeIdentifierToJavaTypeArgument(hydra.ext.java.syntax.TypeIdentifier id) {
    return new hydra.ext.java.syntax.TypeArgument.Reference(new hydra.ext.java.syntax.ReferenceType.Variable(new hydra.ext.java.syntax.TypeVariable((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList()), id)));
  }

  static hydra.ext.java.syntax.TypeName javaTypeName(hydra.ext.java.syntax.Identifier id) {
    return new hydra.ext.java.syntax.TypeName(new hydra.ext.java.syntax.TypeIdentifier(id), (hydra.util.Maybe<hydra.ext.java.syntax.PackageOrTypeName>) (hydra.util.Maybe.<hydra.ext.java.syntax.PackageOrTypeName>nothing()));
  }

  static hydra.ext.java.syntax.TypeParameter javaTypeParameter(String v) {
    return new hydra.ext.java.syntax.TypeParameter((java.util.List<hydra.ext.java.syntax.TypeParameterModifier>) (java.util.Collections.<hydra.ext.java.syntax.TypeParameterModifier>emptyList()), hydra.ext.java.Utils.javaTypeIdentifier(v), (hydra.util.Maybe<hydra.ext.java.syntax.TypeBound>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeBound>nothing()));
  }

  static hydra.ext.java.syntax.FormalParameter javaTypeToJavaFormalParameter(hydra.ext.java.syntax.Type jt, hydra.core.Name fname) {
    return new hydra.ext.java.syntax.FormalParameter.Simple(new hydra.ext.java.syntax.FormalParameter_Simple((java.util.List<hydra.ext.java.syntax.VariableModifier>) (java.util.Collections.<hydra.ext.java.syntax.VariableModifier>emptyList()), new hydra.ext.java.syntax.UnannType(jt), hydra.ext.java.Utils.fieldNameToJavaVariableDeclaratorId(fname)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType> javaTypeToJavaReferenceType(hydra.ext.java.syntax.Type t, hydra.context.Context cx) {
    return (t).accept(new hydra.ext.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType> visit(hydra.ext.java.syntax.Type.Reference rt) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>right((rt).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType> visit(hydra.ext.java.syntax.Type.Primitive ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("expected a Java reference type")), cx)));
      }
    });
  }

  static hydra.ext.java.syntax.Result javaTypeToJavaResult(hydra.ext.java.syntax.Type jt) {
    return new hydra.ext.java.syntax.Result.Type(new hydra.ext.java.syntax.UnannType(jt));
  }

  static hydra.ext.java.syntax.TypeArgument javaTypeToJavaTypeArgument(hydra.ext.java.syntax.Type t) {
    return (t).accept(new hydra.ext.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.ext.java.syntax.TypeArgument visit(hydra.ext.java.syntax.Type.Reference rt) {
        return new hydra.ext.java.syntax.TypeArgument.Reference((rt).value);
      }

      @Override
      public hydra.ext.java.syntax.TypeArgument visit(hydra.ext.java.syntax.Type.Primitive ignored) {
        return new hydra.ext.java.syntax.TypeArgument.Wildcard(new hydra.ext.java.syntax.Wildcard((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList()), (hydra.util.Maybe<hydra.ext.java.syntax.WildcardBounds>) (hydra.util.Maybe.<hydra.ext.java.syntax.WildcardBounds>nothing())));
      }
    });
  }

  static hydra.ext.java.syntax.ReferenceType javaTypeVariable(String v) {
    return new hydra.ext.java.syntax.ReferenceType.Variable(new hydra.ext.java.syntax.TypeVariable((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList()), hydra.ext.java.Utils.javaTypeIdentifier(hydra.Formatting.capitalize(v))));
  }

  static hydra.ext.java.syntax.Type javaTypeVariableToType(hydra.ext.java.syntax.TypeVariable tv) {
    return new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.Variable(tv));
  }

  static hydra.ext.java.syntax.Expression javaUnaryExpressionToJavaExpression(hydra.ext.java.syntax.UnaryExpression ue) {
    return new hydra.ext.java.syntax.Expression.Assignment(new hydra.ext.java.syntax.AssignmentExpression.Conditional(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.InclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ExclusiveOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.AndExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.EqualityExpression.Unary(new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(ue))))))))))))))))));
  }

  static hydra.ext.java.syntax.RelationalExpression javaUnaryExpressionToJavaRelationalExpression(hydra.ext.java.syntax.UnaryExpression ue) {
    return new hydra.ext.java.syntax.RelationalExpression.Simple(new hydra.ext.java.syntax.ShiftExpression.Unary(new hydra.ext.java.syntax.AdditiveExpression.Unary(new hydra.ext.java.syntax.MultiplicativeExpression.Unary(ue))));
  }

  static hydra.ext.java.syntax.VariableDeclarator javaVariableDeclarator(hydra.ext.java.syntax.Identifier id, hydra.util.Maybe<hydra.ext.java.syntax.VariableInitializer> minit) {
    return new hydra.ext.java.syntax.VariableDeclarator(hydra.ext.java.Utils.javaVariableDeclaratorId(id), minit);
  }

  static hydra.ext.java.syntax.VariableDeclaratorId javaVariableDeclaratorId(hydra.ext.java.syntax.Identifier id) {
    return new hydra.ext.java.syntax.VariableDeclaratorId(id, (hydra.util.Maybe<hydra.ext.java.syntax.Dims>) (hydra.util.Maybe.<hydra.ext.java.syntax.Dims>nothing()));
  }

  static hydra.ext.java.syntax.Identifier javaVariableName(hydra.core.Name name) {
    return hydra.ext.java.Utils.javaIdentifier(hydra.Names.localNameOf(name));
  }

  static hydra.core.Name lookupJavaVarName(hydra.ext.java.environment.Aliases aliases, hydra.core.Name name) {
    return hydra.lib.maybes.Cases.applyLazy(
      hydra.lib.maps.Lookup.apply(
        name,
        (aliases).varRenames),
      () -> name,
      (java.util.function.Function<hydra.core.Name, hydra.core.Name>) (renamed -> renamed));
  }

  static hydra.ext.java.syntax.ClassBodyDeclaration makeConstructor(hydra.ext.java.environment.Aliases aliases, hydra.core.Name elName, Boolean private_, java.util.List<hydra.ext.java.syntax.FormalParameter> params, java.util.List<hydra.ext.java.syntax.BlockStatement> stmts) {
    hydra.util.Lazy<hydra.ext.java.syntax.ConstructorBody> body = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.ConstructorBody((hydra.util.Maybe<hydra.ext.java.syntax.ExplicitConstructorInvocation>) (hydra.util.Maybe.<hydra.ext.java.syntax.ExplicitConstructorInvocation>nothing()), stmts));
    hydra.ext.java.syntax.SimpleTypeName nm = new hydra.ext.java.syntax.SimpleTypeName(hydra.ext.java.Utils.nameToJavaTypeIdentifier(
      aliases,
      false,
      elName));
    hydra.util.Lazy<hydra.ext.java.syntax.ConstructorDeclarator> cons = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.ConstructorDeclarator((java.util.List<hydra.ext.java.syntax.TypeParameter>) (java.util.Collections.<hydra.ext.java.syntax.TypeParameter>emptyList()), nm, (hydra.util.Maybe<hydra.ext.java.syntax.ReceiverParameter>) (hydra.util.Maybe.<hydra.ext.java.syntax.ReceiverParameter>nothing()), params));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.ConstructorModifier>> mods = new hydra.util.Lazy<>(() -> java.util.Arrays.asList(hydra.lib.logic.IfElse.lazy(
      private_,
      () -> new hydra.ext.java.syntax.ConstructorModifier.Private(),
      () -> new hydra.ext.java.syntax.ConstructorModifier.Public())));
    return new hydra.ext.java.syntax.ClassBodyDeclaration.ConstructorDeclaration(new hydra.ext.java.syntax.ConstructorDeclaration(mods.get(), cons.get(), (hydra.util.Maybe<hydra.ext.java.syntax.Throws>) (hydra.util.Maybe.<hydra.ext.java.syntax.Throws>nothing()), body.get()));
  }

  static hydra.ext.java.syntax.ClassBodyDeclaration methodDeclaration(java.util.List<hydra.ext.java.syntax.MethodModifier> mods, java.util.List<hydra.ext.java.syntax.TypeParameter> tparams, java.util.List<hydra.ext.java.syntax.Annotation> anns, String methodName, java.util.List<hydra.ext.java.syntax.FormalParameter> params, hydra.ext.java.syntax.Result result, hydra.util.Maybe<java.util.List<hydra.ext.java.syntax.BlockStatement>> stmts) {
    return hydra.ext.java.Utils.javaMethodDeclarationToJavaClassBodyDeclaration(new hydra.ext.java.syntax.MethodDeclaration(anns, mods, hydra.ext.java.Utils.javaMethodHeader(
      tparams,
      methodName,
      params,
      result), hydra.ext.java.Utils.javaMethodBody(stmts)));
  }

  static hydra.ext.java.syntax.MethodInvocation methodInvocation(hydra.util.Maybe<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>> lhs, hydra.ext.java.syntax.Identifier methodName, java.util.List<hydra.ext.java.syntax.Expression> args) {
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> header = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
      lhs,
      () -> new hydra.ext.java.syntax.MethodInvocation_Header.Simple(new hydra.ext.java.syntax.MethodName(methodName)),
      (java.util.function.Function<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>, hydra.ext.java.syntax.MethodInvocation_Header>) (either -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.MethodInvocation_Variant>) (en -> new hydra.ext.java.syntax.MethodInvocation_Variant.Expression(en)),
        (java.util.function.Function<hydra.ext.java.syntax.Primary, hydra.ext.java.syntax.MethodInvocation_Variant>) (p -> new hydra.ext.java.syntax.MethodInvocation_Variant.Primary(p)),
        either), (java.util.List<hydra.ext.java.syntax.TypeArgument>) (java.util.Collections.<hydra.ext.java.syntax.TypeArgument>emptyList()), methodName)))));
    return new hydra.ext.java.syntax.MethodInvocation(header.get(), args);
  }

  static hydra.ext.java.syntax.MethodInvocation methodInvocationStatic(hydra.ext.java.syntax.Identifier self, hydra.ext.java.syntax.Identifier methodName, java.util.List<hydra.ext.java.syntax.Expression> args) {
    return hydra.ext.java.Utils.methodInvocation(
      hydra.util.Maybe.just(hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>left(hydra.ext.java.Utils.javaIdentifierToJavaExpressionName(self))),
      methodName,
      args);
  }

  static hydra.ext.java.syntax.MethodInvocation methodInvocationStaticWithTypeArgs(hydra.ext.java.syntax.Identifier self, hydra.ext.java.syntax.Identifier methodName, java.util.List<hydra.ext.java.syntax.TypeArgument> targs, java.util.List<hydra.ext.java.syntax.Expression> args) {
    hydra.ext.java.syntax.MethodInvocation_Header header = new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Expression(hydra.ext.java.Utils.javaIdentifierToJavaExpressionName(self)), targs, methodName));
    return new hydra.ext.java.syntax.MethodInvocation(header, args);
  }

  static hydra.ext.java.syntax.ClassType nameToJavaClassType(hydra.ext.java.environment.Aliases aliases, Boolean qualify, java.util.List<hydra.ext.java.syntax.TypeArgument> args, hydra.core.Name name, hydra.util.Maybe<String> mlocal) {
    hydra.util.Pair<hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier> result = hydra.ext.java.Utils.nameToQualifiedJavaName(
      aliases,
      qualify,
      name,
      mlocal);
    hydra.util.Lazy<hydra.ext.java.syntax.TypeIdentifier> id = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
    hydra.util.Lazy<hydra.ext.java.syntax.ClassTypeQualifier> pkg = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
    return new hydra.ext.java.syntax.ClassType((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList()), pkg.get(), id.get(), args);
  }

  static hydra.ext.java.syntax.Identifier nameToJavaName(hydra.ext.java.environment.Aliases aliases, hydra.core.Name name) {
    hydra.module.QualifiedName qn = hydra.Names.qualifyName(name);
    String local = (qn).local;
    hydra.util.Maybe<hydra.module.Namespace> ns_ = (qn).namespace;
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.java.Utils.isEscaped((name).value),
      () -> new hydra.ext.java.syntax.Identifier(hydra.ext.java.Utils.sanitizeJavaName(local)),
      () -> hydra.lib.maybes.Cases.applyLazy(
        ns_,
        () -> new hydra.ext.java.syntax.Identifier(local),
        (java.util.function.Function<hydra.module.Namespace, hydra.ext.java.syntax.Identifier>) (gname -> {
          hydra.util.Lazy<java.util.List<String>> parts = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
            hydra.lib.maps.Lookup.apply(
              gname,
              (aliases).packages),
            () -> hydra.lib.strings.SplitOn.apply(
              ".",
              (gname).value),
            (java.util.function.Function<hydra.ext.java.syntax.PackageName, java.util.List<String>>) (pkgName -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.ext.java.syntax.Identifier, String>) (i -> (i).value),
              (pkgName).value))));
          hydra.util.Lazy<java.util.List<String>> allParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
            parts.get(),
            java.util.Arrays.asList(hydra.ext.java.Utils.sanitizeJavaName(local))));
          return new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Intercalate.apply(
            ".",
            allParts.get()));
        })));
  }

  static hydra.ext.java.syntax.ReferenceType nameToJavaReferenceType(hydra.ext.java.environment.Aliases aliases, Boolean qualify, java.util.List<hydra.ext.java.syntax.TypeArgument> args, hydra.core.Name name, hydra.util.Maybe<String> mlocal) {
    return new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(hydra.ext.java.Utils.nameToJavaClassType(
      aliases,
      qualify,
      args,
      name,
      mlocal)));
  }

  static hydra.ext.java.syntax.TypeIdentifier nameToJavaTypeIdentifier(hydra.ext.java.environment.Aliases aliases, Boolean qualify, hydra.core.Name name) {
    return hydra.lib.pairs.First.apply(hydra.ext.java.Utils.nameToQualifiedJavaName(
      aliases,
      qualify,
      name,
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
  }

  static hydra.util.Pair<hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier> nameToQualifiedJavaName(hydra.ext.java.environment.Aliases aliases, Boolean qualify, hydra.core.Name name, hydra.util.Maybe<String> mlocal) {
    hydra.module.QualifiedName qn = hydra.Names.qualifyName(name);
    hydra.util.Maybe<hydra.module.Namespace> ns_ = (qn).namespace;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ext.java.syntax.PackageName>> alias = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
      ns_,
      () -> (hydra.util.Maybe<hydra.ext.java.syntax.PackageName>) (hydra.util.Maybe.<hydra.ext.java.syntax.PackageName>nothing()),
      (java.util.function.Function<hydra.module.Namespace, hydra.util.Maybe<hydra.ext.java.syntax.PackageName>>) (n -> hydra.util.Maybe.just(hydra.lib.maybes.Cases.applyLazy(
        hydra.lib.maps.Lookup.apply(
          n,
          (aliases).packages),
        () -> hydra.ext.java.Names.javaPackageName(hydra.lib.strings.SplitOn.apply(
          ".",
          (n).value)),
        (java.util.function.Function<hydra.ext.java.syntax.PackageName, hydra.ext.java.syntax.PackageName>) (id -> id))))));
    String local = (qn).local;
    hydra.util.Lazy<hydra.ext.java.syntax.TypeIdentifier> jid = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaTypeIdentifier(hydra.lib.maybes.Cases.applyLazy(
      mlocal,
      () -> hydra.ext.java.Utils.sanitizeJavaName(local),
      (java.util.function.Function<String, String>) (l -> hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.ext.java.Utils.sanitizeJavaName(local),
          "."),
        hydra.ext.java.Utils.sanitizeJavaName(l))))));
    hydra.util.Lazy<hydra.ext.java.syntax.ClassTypeQualifier> pkg = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      qualify,
      () -> hydra.lib.maybes.Cases.applyLazy(
        alias.get(),
        () -> new hydra.ext.java.syntax.ClassTypeQualifier.None(),
        (java.util.function.Function<hydra.ext.java.syntax.PackageName, hydra.ext.java.syntax.ClassTypeQualifier>) (p -> new hydra.ext.java.syntax.ClassTypeQualifier.Package_(p))),
      () -> new hydra.ext.java.syntax.ClassTypeQualifier.None()));
    return (hydra.util.Pair<hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier>) ((hydra.util.Pair<hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier>) (new hydra.util.Pair<hydra.ext.java.syntax.TypeIdentifier, hydra.ext.java.syntax.ClassTypeQualifier>(jid.get(), pkg.get())));
  }

  static hydra.ext.java.syntax.Annotation overrideAnnotation() {
    return new hydra.ext.java.syntax.Annotation.Marker(new hydra.ext.java.syntax.MarkerAnnotation(hydra.ext.java.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("Override"))));
  }

  static hydra.ext.java.syntax.Result referenceTypeToResult(hydra.ext.java.syntax.ReferenceType rt) {
    return hydra.ext.java.Utils.javaTypeToJavaResult(new hydra.ext.java.syntax.Type.Reference(rt));
  }

  static String sanitizeJavaName(String name) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.java.Utils.isEscaped(name),
      () -> hydra.ext.java.Utils.unescape(name),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          name,
          "_"),
        () -> "ignored",
        () -> hydra.Formatting.sanitizeWithUnderscores(
          hydra.ext.java.Language.reservedWords(),
          name)));
  }

  static hydra.ext.java.syntax.Annotation suppressWarningsUncheckedAnnotation() {
    return new hydra.ext.java.syntax.Annotation.SingleElement(new hydra.ext.java.syntax.SingleElementAnnotation(hydra.ext.java.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("SuppressWarnings")), hydra.util.Maybe.just(new hydra.ext.java.syntax.ElementValue.ConditionalExpression(new hydra.ext.java.syntax.ConditionalExpression.Simple(new hydra.ext.java.syntax.ConditionalOrExpression(java.util.Arrays.asList(new hydra.ext.java.syntax.ConditionalAndExpression(java.util.Arrays.asList(hydra.ext.java.Utils.javaPostfixExpressionToJavaInclusiveOrExpression(new hydra.ext.java.syntax.PostfixExpression.Primary(hydra.ext.java.Utils.javaLiteralToJavaPrimary(hydra.ext.java.Utils.javaString("unchecked")))))))))))));
  }

  static hydra.ext.java.syntax.ClassBodyDeclaration toAcceptMethod(Boolean abstract_, java.util.List<hydra.ext.java.syntax.TypeParameter> vtparams) {
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.Annotation>> anns = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      abstract_,
      () -> (java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList()),
      () -> java.util.Arrays.asList(hydra.ext.java.Utils.overrideAnnotation())));
    hydra.ext.java.syntax.Expression returnExpr = hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStatic(
      new hydra.ext.java.syntax.Identifier("visitor"),
      new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.visitMethodName()),
      java.util.Arrays.asList(hydra.ext.java.Utils.javaThis())));
    hydra.util.Lazy<hydra.util.Maybe<java.util.List<hydra.ext.java.syntax.BlockStatement>>> body = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      abstract_,
      () -> (hydra.util.Maybe<java.util.List<hydra.ext.java.syntax.BlockStatement>>) (hydra.util.Maybe.<java.util.List<hydra.ext.java.syntax.BlockStatement>>nothing()),
      () -> hydra.util.Maybe.just(java.util.Arrays.asList(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(returnExpr)))))));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.MethodModifier>> mods = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      abstract_,
      () -> java.util.Arrays.asList(
        new hydra.ext.java.syntax.MethodModifier.Public(),
        new hydra.ext.java.syntax.MethodModifier.Abstract()),
      () -> java.util.Arrays.asList(new hydra.ext.java.syntax.MethodModifier.Public())));
    hydra.util.Lazy<java.util.List<hydra.ext.java.syntax.TypeArgument>> typeArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument>) (tp -> new hydra.ext.java.syntax.TypeArgument.Reference(hydra.ext.java.Utils.typeParameterToReferenceType(tp))),
      vtparams));
    hydra.util.Lazy<hydra.ext.java.syntax.Type> ref = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaClassTypeToJavaType(new hydra.ext.java.syntax.ClassType((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList()), new hydra.ext.java.syntax.ClassTypeQualifier.None(), hydra.ext.java.Utils.javaTypeIdentifier(hydra.ext.java.Names.visitorName()), hydra.lib.lists.Concat2.apply(
      typeArgs.get(),
      java.util.Arrays.asList(new hydra.ext.java.syntax.TypeArgument.Reference(hydra.ext.java.Utils.visitorTypeVariable()))))));
    hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.Utils.javaTypeToJavaFormalParameter(
      ref.get(),
      new hydra.core.Name("visitor"));
    hydra.ext.java.syntax.Result result = hydra.ext.java.Utils.javaTypeToJavaResult(new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.Utils.visitorTypeVariable()));
    java.util.List<hydra.ext.java.syntax.TypeParameter> tparams = java.util.Arrays.asList(hydra.ext.java.Utils.javaTypeParameter(hydra.ext.java.Names.visitorReturnParameter()));
    return hydra.ext.java.Utils.methodDeclaration(
      mods.get(),
      tparams,
      anns.get(),
      hydra.ext.java.Names.acceptMethodName(),
      java.util.Arrays.asList(param),
      result,
      body.get());
  }

  static hydra.ext.java.syntax.Statement toAssignStmt(hydra.core.Name fname) {
    hydra.ext.java.syntax.Identifier id = hydra.ext.java.Utils.fieldNameToJavaIdentifier(fname);
    hydra.ext.java.syntax.LeftHandSide lhs = new hydra.ext.java.syntax.LeftHandSide.FieldAccess(new hydra.ext.java.syntax.FieldAccess(new hydra.ext.java.syntax.FieldAccess_Qualifier.Primary(new hydra.ext.java.syntax.Primary.NoNewArray(new hydra.ext.java.syntax.PrimaryNoNewArrayExpression.This())), id));
    hydra.ext.java.syntax.Expression rhs = hydra.ext.java.Utils.fieldNameToJavaExpression(fname);
    return hydra.ext.java.Utils.javaAssignmentStatement(
      lhs,
      rhs);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> toJavaArrayType(hydra.ext.java.syntax.Type t, hydra.context.Context cx) {
    return (t).accept(new hydra.ext.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.Type.Reference rt) {
        return (rt).value.accept(new hydra.ext.java.syntax.ReferenceType.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ReferenceType.ClassOrInterface cit) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.Array(new hydra.ext.java.syntax.ArrayType(new hydra.ext.java.syntax.Dims(java.util.Arrays.asList((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList()))), new hydra.ext.java.syntax.ArrayType_Variant.ClassOrInterface((cit).value)))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ReferenceType.Array at) {
            java.util.List<java.util.List<hydra.ext.java.syntax.Annotation>> oldDims = (at).value.dims.value;
            hydra.util.Lazy<hydra.ext.java.syntax.Dims> newDims = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Dims(hydra.lib.lists.Concat2.apply(
              oldDims,
              java.util.Arrays.asList((java.util.List<hydra.ext.java.syntax.Annotation>) (java.util.Collections.<hydra.ext.java.syntax.Annotation>emptyList())))));
            hydra.ext.java.syntax.ArrayType_Variant variant = (at).value.variant;
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.Array(new hydra.ext.java.syntax.ArrayType(newDims.get(), variant))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.ReferenceType.Variable ignored) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("don't know how to make Java reference type into array type")), cx)));
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.ext.java.syntax.Type.Primitive ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("don't know how to make Java type into array type")), cx)));
      }
    });
  }

  static hydra.ext.java.syntax.ReferenceType typeParameterToReferenceType(hydra.ext.java.syntax.TypeParameter tp) {
    return hydra.ext.java.Utils.javaTypeVariable((tp).identifier.value.value);
  }

  static hydra.ext.java.syntax.TypeArgument typeParameterToTypeArgument(hydra.ext.java.syntax.TypeParameter tp) {
    return hydra.ext.java.Utils.javaTypeIdentifierToJavaTypeArgument((tp).identifier);
  }

  static String unTypeParameter(hydra.ext.java.syntax.TypeParameter tp) {
    return (tp).identifier.value.value;
  }

  static String unescape(String s) {
    return hydra.lib.strings.FromList.apply(hydra.lib.lists.Tail.apply(hydra.lib.strings.ToList.apply(s)));
  }

  static hydra.core.Name uniqueVarName(hydra.ext.java.environment.Aliases aliases, hydra.core.Name name) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        name,
        (aliases).inScopeJavaVars),
      () -> hydra.ext.java.Utils.uniqueVarName_go(
        aliases,
        (name).value,
        2),
      () -> name);
  }

  static hydra.core.Name uniqueVarName_go(hydra.ext.java.environment.Aliases aliases, String base, Integer n) {
    hydra.core.Name candidate = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      base,
      hydra.lib.literals.ShowInt32.apply(n)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        candidate,
        (aliases).inScopeJavaVars),
      () -> hydra.ext.java.Utils.uniqueVarName_go(
        aliases,
        base,
        hydra.lib.math.Add.apply(
          n,
          1)),
      () -> candidate);
  }

  static hydra.ext.java.syntax.BlockStatement varDeclarationStatement(hydra.ext.java.syntax.Identifier id, hydra.ext.java.syntax.Expression rhs) {
    return new hydra.ext.java.syntax.BlockStatement.LocalVariableDeclaration(new hydra.ext.java.syntax.LocalVariableDeclarationStatement(new hydra.ext.java.syntax.LocalVariableDeclaration((java.util.List<hydra.ext.java.syntax.VariableModifier>) (java.util.Collections.<hydra.ext.java.syntax.VariableModifier>emptyList()), new hydra.ext.java.syntax.LocalVariableType.Var(), java.util.Arrays.asList(hydra.ext.java.Utils.javaVariableDeclarator(
      id,
      hydra.util.Maybe.just(new hydra.ext.java.syntax.VariableInitializer.Expression(rhs)))))));
  }

  static <T0> hydra.ext.java.syntax.BlockStatement variableDeclarationStatement(T0 aliases, hydra.ext.java.syntax.Type jtype, hydra.ext.java.syntax.Identifier id, hydra.ext.java.syntax.Expression rhs) {
    hydra.ext.java.syntax.VariableInitializer init_ = new hydra.ext.java.syntax.VariableInitializer.Expression(rhs);
    hydra.ext.java.syntax.VariableDeclarator vdec = hydra.ext.java.Utils.javaVariableDeclarator(
      id,
      hydra.util.Maybe.just(init_));
    return new hydra.ext.java.syntax.BlockStatement.LocalVariableDeclaration(new hydra.ext.java.syntax.LocalVariableDeclarationStatement(new hydra.ext.java.syntax.LocalVariableDeclaration((java.util.List<hydra.ext.java.syntax.VariableModifier>) (java.util.Collections.<hydra.ext.java.syntax.VariableModifier>emptyList()), new hydra.ext.java.syntax.LocalVariableType.Type(new hydra.ext.java.syntax.UnannType(jtype)), java.util.Arrays.asList(vdec))));
  }

  static hydra.ext.java.syntax.Identifier variableToJavaIdentifier(hydra.core.Name name) {
    String v = (name).value;
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        v,
        "_"),
      () -> new hydra.ext.java.syntax.Identifier("ignored"),
      () -> new hydra.ext.java.syntax.Identifier(hydra.ext.java.Utils.sanitizeJavaName(v)));
  }

  static hydra.core.Name variantClassName(Boolean qualify, hydra.core.Name elName, hydra.core.Name fname) {
    String flocal = hydra.Formatting.capitalize((fname).value);
    hydra.module.QualifiedName qn = hydra.Names.qualifyName(elName);
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
    hydra.util.Maybe<hydra.module.Namespace> ns_ = (qn).namespace;
    return hydra.Names.unqualifyName(new hydra.module.QualifiedName(ns_, local1.get()));
  }

  static hydra.ext.java.syntax.ReferenceType visitorTypeVariable() {
    return hydra.ext.java.Utils.javaTypeVariable("r");
  }
}
