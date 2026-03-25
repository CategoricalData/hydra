// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java;

/**
 * Java code generator: converts Hydra modules to Java source code
 */
public interface Coder {
  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> addComment(hydra.ext.java.syntax.ClassBodyDeclaration decl, hydra.core.FieldType field, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.Maybe<String>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>) (c -> new hydra.ext.java.syntax.ClassBodyDeclarationWithComments(decl, c)),
      hydra.CoderUtils.commentsFromFieldType(
        cx,
        g,
        field));
  }

  static <T0, T1> hydra.util.Either<T1, hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>> analyzeJavaFunction(hydra.ext.java.environment.JavaEnvironment env, hydra.core.Term term, hydra.context.Context cx, T0 g) {
    return hydra.CoderUtils.analyzeFunctionTerm(
      cx,
      hydra.ext.java.Coder::javaEnvGetGraph,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.ext.java.environment.JavaEnvironment, hydra.ext.java.environment.JavaEnvironment>>) (p0 -> p1 -> hydra.ext.java.Coder.javaEnvSetGraph(
        p0,
        p1)),
      env,
      term);
  }

  static hydra.core.Term annotateBodyWithCod(hydra.core.Type typ, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, hydra.core.Term> setAnn = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> hydra.Annotations.setTermAnnotation(
      hydra.Constants.key_type(),
      hydra.util.Maybe.just(hydra.encode.Core.type(typ)),
      t));
    return hydra.Rewriting.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (setAnn).apply(term);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication _ta) {
        return (setAnn).apply(term);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = (app).value.function;
        hydra.core.Term rhs = (app).value.argument;
        hydra.util.Lazy<hydra.core.Term> annotatedRhs = new hydra.util.Lazy<>(() -> hydra.Rewriting.deannotateTerm(rhs).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Term instance) {
            return rhs;
          }

          @Override
          public hydra.core.Term visit(hydra.core.Term.TypeApplication _ta2) {
            return hydra.ext.java.Coder.annotateBodyWithCod(
              hydra.ext.java.Coder.extractArgType(
                lhs,
                typ),
              rhs);
          }
        }));
        return (setAnn).apply(new hydra.core.Term.Application(new hydra.core.Application(lhs, annotatedRhs.get())));
      }
    });
  }

  static <T0, T1> hydra.util.Either<T1, hydra.util.ConsList<hydra.core.Term>> annotateLambdaArgs(hydra.core.Name cname, hydra.util.ConsList<hydra.core.Type> tApps, hydra.util.ConsList<hydra.core.Term> argTerms, T0 cx, hydra.graph.Graph g) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(tApps),
      () -> hydra.util.Either.<T1, hydra.util.ConsList<hydra.core.Term>>right(argTerms),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.Bind.apply(
          hydra.util.Either.<T1, hydra.util.Maybe<hydra.core.Binding>>right(hydra.Lexical.dereferenceElement(
            g,
            cname)),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, hydra.util.Either<T1, hydra.util.Maybe<hydra.core.TypeScheme>>>) (mel -> hydra.lib.maybes.Cases.applyLazy(
            mel,
            () -> hydra.util.Either.<T1, hydra.util.Maybe<hydra.core.TypeScheme>>right(hydra.lib.maybes.Map.apply(
              (java.util.function.Function<hydra.graph.Primitive, hydra.core.TypeScheme>) (prim -> (prim).type),
              hydra.lib.maps.Lookup.apply(
                cname,
                (g).primitives))),
            (java.util.function.Function<hydra.core.Binding, hydra.util.Either<T1, hydra.util.Maybe<hydra.core.TypeScheme>>>) (el -> hydra.util.Either.<T1, hydra.util.Maybe<hydra.core.TypeScheme>>right((el).type))))),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.util.Either<T1, hydra.util.ConsList<hydra.core.Term>>>) (mts -> hydra.lib.maybes.Cases.applyLazy(
          mts,
          () -> hydra.util.Either.<T1, hydra.util.ConsList<hydra.core.Term>>right(argTerms),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<T1, hydra.util.ConsList<hydra.core.Term>>>) (ts -> {
            hydra.core.Type schemeType = (ts).type;
            hydra.util.PersistentSet<hydra.core.Name> schemeTypeVars = hydra.ext.java.Coder.collectTypeVars(schemeType);
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> schemeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.sets.Member.apply(
                v,
                schemeTypeVars)),
              (ts).variables));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Or.apply(
                hydra.lib.lists.Null.apply(schemeVars.get()),
                hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
                  hydra.lib.lists.Length.apply(schemeVars.get()),
                  hydra.lib.lists.Length.apply(tApps)))),
              () -> hydra.util.Either.<T1, hydra.util.ConsList<hydra.core.Term>>right(argTerms),
              () -> ((java.util.function.Supplier<hydra.util.Either<T1, hydra.util.ConsList<hydra.core.Term>>>) (() -> {
                hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> subst = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
                  schemeVars.get(),
                  tApps)));
                return ((java.util.function.Supplier<hydra.util.Either<T1, hydra.util.ConsList<hydra.core.Term>>>) (() -> {
                  hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> expectedTypes = new hydra.util.Lazy<>(() -> hydra.ext.java.Coder.peelExpectedTypes(
                    subst.get(),
                    hydra.lib.lists.Length.apply(argTerms),
                    schemeType));
                  return hydra.util.Either.<T1, hydra.util.ConsList<hydra.core.Term>>right(hydra.lib.lists.ZipWith.apply(
                    (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Type, hydra.core.Term>>) (arg -> (java.util.function.Function<hydra.core.Type, hydra.core.Term>) (mExpected -> hydra.ext.java.Coder.propagateType(
                      mExpected,
                      arg))),
                    argTerms,
                    hydra.lib.lists.Concat2.apply(
                      expectedTypes.get(),
                      hydra.lib.lists.Replicate.apply(
                        hydra.lib.lists.Length.apply(argTerms),
                        new hydra.core.Type.Variable(new hydra.core.Name("unused"))))));
                })).get();
              })).get());
          })))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> applyCastIfSafe(hydra.ext.java.environment.Aliases aliases, hydra.core.Type castType, hydra.ext.java.syntax.Expression expr, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.util.PersistentSet<hydra.core.Name> castVars = hydra.ext.java.Coder.collectTypeVars(castType);
    hydra.util.PersistentSet<hydra.core.Name> inScope = (aliases).inScopeTypeParams;
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> javaTypeVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.Or.apply(
        hydra.lib.sets.Member.apply(
          v,
          inScope),
        hydra.ext.java.Coder.isLambdaBoundVariable(v))),
      hydra.lib.sets.ToList.apply(castVars))));
    hydra.util.PersistentSet<hydra.core.Name> trusted = (aliases).trustedTypeVars;
    hydra.util.Lazy<Boolean> isSafe = new hydra.util.Lazy<>(() -> hydra.lib.logic.Or.apply(
      hydra.lib.sets.Null.apply(trusted),
      hydra.lib.logic.Or.apply(
        hydra.lib.sets.Null.apply(javaTypeVars.get()),
        hydra.lib.sets.Null.apply(hydra.lib.sets.Difference.apply(
          javaTypeVars.get(),
          trusted)))));
    return hydra.lib.logic.IfElse.lazy(
      isSafe.get(),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.ext.java.Coder.encodeType(
          aliases,
          (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
          castType,
          cx,
          g),
        (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jtype -> hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Utils.javaTypeToJavaReferenceType(
            jtype,
            cx),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.Utils.javaCastExpression(
            rt,
            hydra.ext.java.Utils.javaExpressionToJavaUnaryExpression(expr)))))))),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(expr));
  }

  static hydra.ext.java.syntax.Expression applyJavaArg(hydra.ext.java.syntax.Expression expr, hydra.ext.java.syntax.Expression jarg) {
    return hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocation(
      hydra.util.Maybe.just(hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>right(hydra.ext.java.Utils.javaExpressionToJavaPrimary(expr))),
      new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.applyMethodName()),
      hydra.util.ConsList.of(jarg)));
  }

  static <T0, T1> hydra.util.Either<T1, hydra.core.Term> applyOvergenSubstToTermAnnotations(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> subst, hydra.core.Term term0, T0 cx, hydra.graph.Graph g) {
    return hydra.util.Either.<T1, hydra.core.Term>right(hydra.ext.java.Coder.applyOvergenSubstToTermAnnotations_go(
      subst,
      g,
      term0));
  }

  static hydra.core.Term applyOvergenSubstToTermAnnotations_go(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> subst, hydra.graph.Graph cx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return term;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> ann = (at).value.annotation;
        hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> ann_ = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
          hydra.lib.maps.Lookup.apply(
            hydra.Constants.key_type(),
            ann),
          () -> ann,
          (java.util.function.Function<hydra.core.Term, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>) (typeTerm -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>) (ignored -> ann),
            (java.util.function.Function<hydra.core.Type, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>) (t -> {
              hydra.core.Type t_ = hydra.ext.java.Coder.substituteTypeVarsWithTypes(
                subst,
                t);
              return hydra.lib.maps.Insert.apply(
                hydra.Constants.key_type(),
                hydra.encode.Core.type(t_),
                ann);
            }),
            hydra.decode.Core.type(
              cx,
              typeTerm)))));
        hydra.core.Term inner = (at).value.body;
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.ext.java.Coder.applyOvergenSubstToTermAnnotations_go(
          subst,
          cx,
          inner), ann_.get()));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Application app) {
        return new hydra.core.Term.Application(new hydra.core.Application(hydra.ext.java.Coder.applyOvergenSubstToTermAnnotations_go(
          subst,
          cx,
          (app).value.function), hydra.ext.java.Coder.applyOvergenSubstToTermAnnotations_go(
          subst,
          cx,
          (app).value.argument)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return term;
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda lam) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((lam).value.parameter, hydra.lib.maybes.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (d -> hydra.ext.java.Coder.substituteTypeVarsWithTypes(
                subst,
                d)),
              (lam).value.domain), hydra.ext.java.Coder.applyOvergenSubstToTermAnnotations_go(
              subst,
              cx,
              (lam).value.body))));
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Elimination elim) {
            return (elim).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.core.Term otherwise(hydra.core.Elimination instance) {
                return term;
              }

              @Override
              public hydra.core.Term visit(hydra.core.Elimination.Union cs) {
                return new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement((cs).value.typeName, hydra.lib.maybes.Map.apply(
                  (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (d -> hydra.ext.java.Coder.applyOvergenSubstToTermAnnotations_go(
                    subst,
                    cx,
                    d)),
                  (cs).value.default_), hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (fld -> new hydra.core.Field((fld).name, hydra.ext.java.Coder.applyOvergenSubstToTermAnnotations_go(
                    subst,
                    cx,
                    (fld).term))),
                  (cs).value.cases)))));
              }
            });
          }
        });
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, hydra.ext.java.Coder.applyOvergenSubstToTermAnnotations_go(
            subst,
            cx,
            (b).term), (b).type)),
          (lt).value.bindings), hydra.ext.java.Coder.applyOvergenSubstToTermAnnotations_go(
          subst,
          cx,
          (lt).value.body)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication ta) {
        return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(hydra.ext.java.Coder.applyOvergenSubstToTermAnnotations_go(
          subst,
          cx,
          (ta).value.body), hydra.ext.java.Coder.substituteTypeVarsWithTypes(
          subst,
          (ta).value.type)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda((tl).value.parameter, hydra.ext.java.Coder.applyOvergenSubstToTermAnnotations_go(
          subst,
          cx,
          (tl).value.body)));
      }
    });
  }

  static hydra.core.Type applySubstFull(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> s, hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable v) {
        return hydra.lib.maps.FindWithDefault.applyLazy(
          () -> t,
          (v).value,
          s);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Function ft) {
        return new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.ext.java.Coder.applySubstFull(
          s,
          (ft).value.domain), hydra.ext.java.Coder.applySubstFull(
          s,
          (ft).value.codomain)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Application at) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.ext.java.Coder.applySubstFull(
          s,
          (at).value.function), hydra.ext.java.Coder.applySubstFull(
          s,
          (at).value.argument)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.List inner) {
        return new hydra.core.Type.List(hydra.ext.java.Coder.applySubstFull(
          s,
          (inner).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Set inner) {
        return new hydra.core.Type.Set(hydra.ext.java.Coder.applySubstFull(
          s,
          (inner).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Maybe inner) {
        return new hydra.core.Type.Maybe(hydra.ext.java.Coder.applySubstFull(
          s,
          (inner).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Map mt) {
        return new hydra.core.Type.Map(new hydra.core.MapType(hydra.ext.java.Coder.applySubstFull(
          s,
          (mt).value.keys), hydra.ext.java.Coder.applySubstFull(
          s,
          (mt).value.values)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Pair pt) {
        return new hydra.core.Type.Pair(new hydra.core.PairType(hydra.ext.java.Coder.applySubstFull(
          s,
          (pt).value.first), hydra.ext.java.Coder.applySubstFull(
          s,
          (pt).value.second)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Either et) {
        return new hydra.core.Type.Either(new hydra.core.EitherType(hydra.ext.java.Coder.applySubstFull(
          s,
          (et).value.left), hydra.ext.java.Coder.applySubstFull(
          s,
          (et).value.right)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Type.Forall(new hydra.core.ForallType((ft).value.parameter, hydra.ext.java.Coder.applySubstFull(
          hydra.lib.maps.Delete.apply(
            (ft).value.parameter,
            s),
          (ft).value.body)));
      }
    });
  }

  static hydra.core.Type applySubstSimple(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> subst, hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable v) {
        return hydra.lib.maps.FindWithDefault.applyLazy(
          () -> t,
          (v).value,
          subst);
      }
    });
  }

  static hydra.ext.java.syntax.Expression arraysCompareExpr(String otherVar, String fname) {
    hydra.util.Lazy<hydra.ext.java.syntax.Expression> arg1 = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaExpressionNameToJavaExpression(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.Utils.sanitizeJavaName(fname)))));
    hydra.ext.java.syntax.Expression arg2 = hydra.ext.java.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.Utils.fieldExpression(
      hydra.ext.java.Utils.javaIdentifier(otherVar),
      hydra.ext.java.Utils.javaIdentifier(fname)));
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> header = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Type(hydra.ext.java.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("java.util.Arrays"))), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier("compare"))));
    return hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(header.get(), hydra.util.ConsList.of(
      arg1.get(),
      arg2)));
  }

  static hydra.ext.java.syntax.InclusiveOrExpression arraysEqualsClause(String tmpName, String fname) {
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> header = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Type(hydra.ext.java.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("java.util.Arrays"))), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.equalsMethodName()))));
    hydra.ext.java.syntax.Expression otherArg = hydra.ext.java.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.Utils.fieldExpression(
      hydra.ext.java.Utils.javaIdentifier(tmpName),
      hydra.ext.java.Utils.javaIdentifier(fname)));
    hydra.ext.java.syntax.Expression thisArg = hydra.ext.java.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.Utils.fieldExpression(
      new hydra.ext.java.syntax.Identifier("this"),
      hydra.ext.java.Utils.javaIdentifier(fname)));
    return hydra.ext.java.Utils.javaPostfixExpressionToJavaInclusiveOrExpression(hydra.ext.java.Utils.javaMethodInvocationToJavaPostfixExpression(new hydra.ext.java.syntax.MethodInvocation(header.get(), hydra.util.ConsList.of(
      thisArg,
      otherArg))));
  }

  static hydra.ext.java.syntax.ClassDeclaration augmentVariantClass(hydra.ext.java.environment.Aliases aliases, hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName, hydra.ext.java.syntax.ClassDeclaration cd) {
    return (cd).accept(new hydra.ext.java.syntax.ClassDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ext.java.syntax.ClassDeclaration otherwise(hydra.ext.java.syntax.ClassDeclaration instance) {
        return cd;
      }

      @Override
      public hydra.ext.java.syntax.ClassDeclaration visit(hydra.ext.java.syntax.ClassDeclaration.Normal ncd) {
        hydra.ext.java.syntax.ClassBodyDeclarationWithComments acceptDecl = hydra.ext.java.Coder.noComment(hydra.ext.java.Utils.toAcceptMethod(
          false,
          tparams));
        hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>> args = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument>) (tp -> hydra.ext.java.Utils.typeParameterToTypeArgument(tp)),
          tparams));
        hydra.util.Lazy<hydra.ext.java.syntax.ClassType> extendsPart = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.nameToJavaClassType(
          aliases,
          true,
          args.get(),
          elName,
          (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())));
        hydra.ext.java.syntax.ClassBody oldBody = (ncd).value.body;
        hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments> oldDecls = (oldBody).value;
        hydra.util.Lazy<hydra.ext.java.syntax.ClassBody> newBody = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.ClassBody(hydra.lib.lists.Concat2.apply(
          oldDecls,
          hydra.util.ConsList.of(acceptDecl))));
        hydra.util.ConsList<hydra.ext.java.syntax.ClassModifier> newMods = hydra.util.ConsList.of(
          new hydra.ext.java.syntax.ClassModifier.Public(),
          new hydra.ext.java.syntax.ClassModifier.Static(),
          new hydra.ext.java.syntax.ClassModifier.Final());
        return new hydra.ext.java.syntax.ClassDeclaration.Normal(new hydra.ext.java.syntax.NormalClassDeclaration(newMods, (ncd).value.identifier, tparams, hydra.util.Maybe.just(extendsPart.get()), (ncd).value.implements_, newBody.get()));
      }
    });
  }

  static Boolean bindingIsFunctionType(hydra.core.Binding b) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.Rewriting.deannotateTerm((b).term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Term instance) {
          return false;
        }

        @Override
        public Boolean visit(hydra.core.Term.Function _f) {
          return true;
        }
      }),
      (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> hydra.Rewriting.deannotateType((ts).type).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Type instance) {
          return false;
        }

        @Override
        public Boolean visit(hydra.core.Type.Function _ft) {
          return true;
        }

        @Override
        public Boolean visit(hydra.core.Type.Forall fa) {
          return hydra.Rewriting.deannotateType((fa).value.body).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public Boolean otherwise(hydra.core.Type instance) {
              return false;
            }

            @Override
            public Boolean visit(hydra.core.Type.Function _ft2) {
              return true;
            }
          });
        }
      })),
      (b).type);
  }

  static String bindingNameToFilePath(hydra.core.Name name) {
    hydra.module.QualifiedName qn = hydra.Names.qualifyName(name);
    String local = (qn).local;
    hydra.util.Maybe<hydra.module.Namespace> ns_ = (qn).namespace;
    String sanitized = hydra.Formatting.sanitizeWithUnderscores(
      hydra.ext.java.Language.reservedWords(),
      local);
    hydra.core.Name unq = hydra.Names.unqualifyName(new hydra.module.QualifiedName(ns_, sanitized));
    return hydra.CoderUtils.nameToFilePath(
      new hydra.util.CaseConvention.Camel(),
      new hydra.util.CaseConvention.Pascal(),
      new hydra.module.FileExtension("java"),
      unq);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>> bindingsToStatements(hydra.ext.java.environment.JavaEnvironment env, hydra.util.ConsList<hydra.core.Binding> bindings, hydra.context.Context cx, hydra.graph.Graph g0) {
    hydra.ext.java.environment.Aliases aliases = (env).aliases;
    hydra.util.ConsList<hydra.core.Binding> flatBindings = hydra.ext.java.Coder.dedupBindings(
      (aliases).inScopeJavaVars,
      hydra.ext.java.Coder.flattenBindings(bindings));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> bindingVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.core.Name>) (b -> (b).name),
      flatBindings)));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.util.PersistentSet<hydra.core.Name>>> allDeps = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, hydra.util.PersistentSet<hydra.core.Name>>>) (b -> {
        hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> deps = new hydra.util.Lazy<>(() -> hydra.lib.sets.Intersection.apply(
          bindingVars.get(),
          hydra.Rewriting.freeVariablesInTerm((b).term)));
        hydra.core.Name key = (b).name;
        return (hydra.util.Pair<hydra.core.Name, hydra.util.PersistentSet<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.PersistentSet<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.PersistentSet<hydra.core.Name>>(key, deps.get())));
      }),
      flatBindings)));
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.ConsList<hydra.core.Name>>> sorted = new hydra.util.Lazy<>(() -> hydra.Sorting.topologicalSortComponents(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.PersistentSet<hydra.core.Name>>, hydra.util.Pair<hydra.core.Name, hydra.util.ConsList<hydra.core.Name>>>) (entry -> {
        hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> deps = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(entry));
        hydra.util.Lazy<hydra.core.Name> key = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(entry));
        return (hydra.util.Pair<hydra.core.Name, hydra.util.ConsList<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.ConsList<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.ConsList<hydra.core.Name>>(key.get(), hydra.lib.sets.ToList.apply(deps.get()))));
      }),
      hydra.lib.maps.ToList.apply(allDeps.get()))));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> recursiveVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.ConsList<hydra.core.Name>>) (names -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(names),
          1),
        () -> ((java.util.function.Supplier<hydra.util.ConsList<hydra.core.Name>>) (() -> {
          hydra.util.Lazy<hydra.core.Name> singleName = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(names));
          return hydra.lib.maybes.Cases.applyLazy(
            hydra.lib.maps.Lookup.apply(
              singleName.get(),
              allDeps.get()),
            () -> (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()),
            (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.ConsList<hydra.core.Name>>) (deps -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                singleName.get(),
                deps),
              () -> hydra.util.ConsList.of(singleName.get()),
              () -> (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()))));
        })).get(),
        () -> names)),
      sorted.get()))));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> thunkedVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.util.ConsList<hydra.core.Name>>) (b -> {
        hydra.core.Name bname = (b).name;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.And.apply(
            hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
              bname,
              recursiveVars.get())),
            hydra.lib.logic.And.apply(
              hydra.ext.java.Coder.needsThunking((b).term),
              hydra.lib.logic.Not.apply(hydra.ext.java.Coder.bindingIsFunctionType(b)))),
          () -> hydra.util.ConsList.of(bname),
          () -> (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()));
      }),
      flatBindings))));
    hydra.util.Lazy<hydra.ext.java.environment.Aliases> aliasesExtended = new hydra.util.Lazy<>(() -> new hydra.ext.java.environment.Aliases((aliases).currentNamespace, (aliases).packages, (aliases).branchVars, hydra.lib.sets.Union.apply(
      (aliases).recursiveVars,
      recursiveVars.get()), (aliases).inScopeTypeParams, (aliases).polymorphicLocals, hydra.lib.sets.Union.apply(
      (aliases).inScopeJavaVars,
      bindingVars.get()), (aliases).varRenames, (aliases).lambdaVars, (aliases).typeVarSubst, (aliases).trustedTypeVars, (aliases).methodCodomain, hydra.lib.sets.Union.apply(
      (aliases).thunkedVars,
      thunkedVars.get())));
    hydra.graph.Graph g = (env).graph;
    hydra.graph.Graph gExtended = hydra.Rewriting.extendGraphForLet(
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (p0 -> p1 -> hydra.CoderUtils.bindingMetadata(
        p0,
        p1)),
      g,
      new hydra.core.Let(flatBindings, new hydra.core.Term.Variable(new hydra.core.Name("dummy"))));
    hydra.ext.java.environment.JavaEnvironment envExtended = new hydra.ext.java.environment.JavaEnvironment(aliasesExtended.get(), gExtended);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(bindings),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>>right((hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>((hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>) (hydra.util.ConsList.<hydra.ext.java.syntax.BlockStatement>empty()), envExtended)))),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (names -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>>) (n -> hydra.ext.java.Coder.toDeclInit(
                aliasesExtended.get(),
                gExtended,
                recursiveVars.get(),
                flatBindings,
                n,
                cx,
                g)),
              names),
            (java.util.function.Function<hydra.util.ConsList<hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (inits -> hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.MapList.apply(
                (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>>) (n -> hydra.ext.java.Coder.toDeclStatement(
                  envExtended,
                  aliasesExtended.get(),
                  gExtended,
                  recursiveVars.get(),
                  thunkedVars.get(),
                  flatBindings,
                  n,
                  cx,
                  g)),
                names),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (decls -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>right(hydra.lib.lists.Concat2.apply(
                hydra.lib.maybes.Cat.apply(inits),
                decls))))))),
          sorted.get()),
        (java.util.function.Function<hydra.util.ConsList<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>>>) (groups -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>>right((hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>(hydra.lib.lists.Concat.apply(groups), envExtended)))))));
  }

  static hydra.util.ConsList<hydra.core.Name> boundTypeVariables(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Annotated at) {
        return hydra.ext.java.Coder.boundTypeVariables((at).value.body);
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.lib.lists.Cons.apply(
          (ft).value.parameter,
          hydra.ext.java.Coder.boundTypeVariables((ft).value.body));
      }
    });
  }

  static <T0> hydra.util.PersistentMap<hydra.core.Name, T0> buildArgSubst(hydra.util.PersistentSet<hydra.core.Name> schemeVarSet, hydra.util.ConsList<hydra.core.Type> schemeDoms, hydra.util.ConsList<T0> argTypes) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Bind.apply(
      hydra.lib.lists.Zip.apply(
        schemeDoms,
        argTypes),
      (java.util.function.Function<hydra.util.Pair<hydra.core.Type, T0>, hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, T0>>>) (p -> {
        hydra.util.Lazy<hydra.core.Type> sdom = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
        return hydra.Rewriting.deannotateType(sdom.get()).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, T0>> otherwise(hydra.core.Type instance) {
            return (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, T0>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, T0>>empty());
          }

          @Override
          public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, T0>> visit(hydra.core.Type.Variable v) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                (v).value,
                schemeVarSet),
              () -> hydra.util.ConsList.of((hydra.util.Pair<hydra.core.Name, T0>) ((hydra.util.Pair<hydra.core.Name, T0>) (new hydra.util.Pair<hydra.core.Name, T0>((v).value, hydra.ext.java.Coder.<T0>buildArgSubst_argType(p))))),
              () -> (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, T0>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, T0>>empty()));
          }
        });
      })));
  }

  static <T0> T0 buildArgSubst_argType(hydra.util.Pair<hydra.core.Type, T0> p) {
    return hydra.lib.pairs.Second.apply(p);
  }

  static hydra.ext.java.syntax.Expression buildCurriedLambda(hydra.util.ConsList<hydra.core.Name> params, hydra.ext.java.syntax.Expression inner) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.ext.java.syntax.Expression, java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.Expression>>) (acc -> (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.Expression>) (p -> hydra.ext.java.Utils.javaLambda(
        p,
        acc))),
      inner,
      hydra.lib.lists.Reverse.apply(params));
  }

  static <T0, T1> hydra.util.Either<T1, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> buildSubstFromAnnotations(hydra.util.PersistentSet<hydra.core.Name> schemeVarSet, hydra.core.Term term, T0 cx, hydra.graph.Graph g) {
    return hydra.util.Either.<T1, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>right(hydra.ext.java.Coder.buildSubstFromAnnotations_go(
      schemeVarSet,
      g,
      term));
  }

  static hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> buildSubstFromAnnotations_go(hydra.util.PersistentSet<hydra.core.Name> schemeVarSet, hydra.graph.Graph g, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Term instance) {
        return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Annotated at) {
        hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> anns = (at).value.annotation;
        hydra.core.Term body = (at).value.body;
        hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> annSubst = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
          hydra.lib.maps.Lookup.apply(
            hydra.Constants.key_type(),
            anns),
          () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          (java.util.function.Function<hydra.core.Term, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (typeTerm -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (ignored -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()))),
            (java.util.function.Function<hydra.core.Type, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (annType -> hydra.Rewriting.deannotateTerm(body).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Term instance) {
                return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
              }

              @Override
              public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Function f) {
                return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
                  @Override
                  public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Function instance) {
                    return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
                  }

                  @Override
                  public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Function.Lambda lam) {
                    return hydra.lib.maybes.Cases.applyLazy(
                      (lam).value.domain,
                      () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
                      (java.util.function.Function<hydra.core.Type, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (dom -> hydra.Rewriting.deannotateType(annType).accept(new hydra.core.Type.PartialVisitor<>() {
                        @Override
                        public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
                          return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
                        }

                        @Override
                        public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Function ft) {
                          return hydra.ext.java.Coder.buildTypeVarSubst(
                            schemeVarSet,
                            (ft).value.domain,
                            dom);
                        }
                      })));
                  }
                });
              }
            })),
            hydra.decode.Core.type(
              g,
              typeTerm)))));
        hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> bodySubst = hydra.ext.java.Coder.buildSubstFromAnnotations_go(
          schemeVarSet,
          g,
          body);
        return hydra.lib.maps.Union.apply(
          annSubst.get(),
          bodySubst);
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Application app) {
        return hydra.lib.maps.Union.apply(
          hydra.ext.java.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            (app).value.function),
          hydra.ext.java.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            (app).value.argument));
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Function instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Function.Lambda lam) {
            return hydra.ext.java.Coder.buildSubstFromAnnotations_go(
              schemeVarSet,
              g,
              (lam).value.body);
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Function.Elimination elim) {
            return (elim).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Elimination instance) {
                return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
              }

              @Override
              public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Elimination.Union cs) {
                hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> caseSubsts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                  (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Field, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>>) (acc -> (java.util.function.Function<hydra.core.Field, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (fld -> hydra.lib.maps.Union.apply(
                    acc,
                    hydra.ext.java.Coder.buildSubstFromAnnotations_go(
                      schemeVarSet,
                      g,
                      (fld).term)))),
                  (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
                  (cs).value.cases));
                hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> defSubst = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
                  (cs).value.default_,
                  () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
                  (java.util.function.Function<hydra.core.Term, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (d -> hydra.ext.java.Coder.buildSubstFromAnnotations_go(
                    schemeVarSet,
                    g,
                    d))));
                return hydra.lib.maps.Union.apply(
                  defSubst.get(),
                  caseSubsts.get());
              }
            });
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Let lt) {
        hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> bindingSubst = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Binding, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>>) (acc -> (java.util.function.Function<hydra.core.Binding, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (b -> hydra.lib.maps.Union.apply(
            acc,
            hydra.ext.java.Coder.buildSubstFromAnnotations_go(
              schemeVarSet,
              g,
              (b).term)))),
          (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          (lt).value.bindings));
        return hydra.lib.maps.Union.apply(
          bindingSubst.get(),
          hydra.ext.java.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            (lt).value.body));
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.List terms) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (t -> hydra.lib.maps.Union.apply(
            acc,
            hydra.ext.java.Coder.buildSubstFromAnnotations_go(
              schemeVarSet,
              g,
              t)))),
          (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          (terms).value);
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Cases.applyLazy(
          (mt).value,
          () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          (java.util.function.Function<hydra.core.Term, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (t -> hydra.ext.java.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            t)));
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Pair p) {
        return hydra.lib.maps.Union.apply(
          hydra.ext.java.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            hydra.lib.pairs.First.apply((p).value)),
          hydra.ext.java.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            hydra.lib.pairs.Second.apply((p).value)));
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Record r) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Field, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>>) (acc -> (java.util.function.Function<hydra.core.Field, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (fld -> hydra.lib.maps.Union.apply(
            acc,
            hydra.ext.java.Coder.buildSubstFromAnnotations_go(
              schemeVarSet,
              g,
              (fld).term)))),
          (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          (r).value.fields);
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Set terms) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (t -> hydra.lib.maps.Union.apply(
            acc,
            hydra.ext.java.Coder.buildSubstFromAnnotations_go(
              schemeVarSet,
              g,
              t)))),
          (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          hydra.lib.sets.ToList.apply((terms).value));
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.ext.java.Coder.buildSubstFromAnnotations_go(
          schemeVarSet,
          g,
          (ta).value.body);
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.ext.java.Coder.buildSubstFromAnnotations_go(
          schemeVarSet,
          g,
          (tl).value.body);
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (t -> hydra.ext.java.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            t)),
          (java.util.function.Function<hydra.core.Term, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (t -> hydra.ext.java.Coder.buildSubstFromAnnotations_go(
            schemeVarSet,
            g,
            t)),
          (e).value);
      }
    });
  }

  static hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> buildTypeSubst(hydra.util.PersistentSet<hydra.core.Name> schemeVarSet, hydra.core.Type schemeType, hydra.core.Type actualType) {
    return hydra.ext.java.Coder.buildTypeSubst_go(
      schemeVarSet,
      hydra.Rewriting.deannotateType(schemeType),
      hydra.Rewriting.deannotateType(actualType));
  }

  static hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> buildTypeSubst_go(hydra.util.PersistentSet<hydra.core.Name> svs, hydra.core.Type st, hydra.core.Type at) {
    java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>> goSub = (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>>) (a -> (java.util.function.Function<hydra.core.Type, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>) (b -> hydra.ext.java.Coder.buildTypeSubst_go(
      svs,
      hydra.Rewriting.deannotateType(a),
      hydra.Rewriting.deannotateType(b))));
    return (st).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Variable v) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Member.apply(
            (v).value,
            svs),
          () -> hydra.lib.maps.Singleton.apply(
            (v).value,
            at),
          () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())));
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Function sft) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Function aft) {
            return hydra.lib.maps.Union.apply(
              (goSub).apply((sft).value.domain).apply((aft).value.domain),
              (goSub).apply((sft).value.codomain).apply((aft).value.codomain));
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Application sat) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Application aat) {
            return hydra.lib.maps.Union.apply(
              (goSub).apply((sat).value.function).apply((aat).value.function),
              (goSub).apply((sat).value.argument).apply((aat).value.argument));
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.List sl) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.List al) {
            return (goSub).apply((sl).value).apply((al).value);
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Set ss) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Set as_) {
            return (goSub).apply((ss).value).apply((as_).value);
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Maybe sm) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Maybe am) {
            return (goSub).apply((sm).value).apply((am).value);
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Map smt) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Map amt) {
            return hydra.lib.maps.Union.apply(
              (goSub).apply((smt).value.keys).apply((amt).value.keys),
              (goSub).apply((smt).value.values).apply((amt).value.values));
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Pair spt) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Pair apt) {
            return hydra.lib.maps.Union.apply(
              (goSub).apply((spt).value.first).apply((apt).value.first),
              (goSub).apply((spt).value.second).apply((apt).value.second));
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Either set_) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Either aet) {
            return hydra.lib.maps.Union.apply(
              (goSub).apply((set_).value.left).apply((aet).value.left),
              (goSub).apply((set_).value.right).apply((aet).value.right));
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Forall sfa) {
        return (at).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (goSub).apply((sfa).value.body).apply(at);
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> visit(hydra.core.Type.Forall afa) {
            return (goSub).apply((sfa).value.body).apply((afa).value.body);
          }
        });
      }
    });
  }

  static hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> buildTypeVarSubst(hydra.util.PersistentSet<hydra.core.Name> schemeVarSet, hydra.core.Type freshTyp, hydra.core.Type canonTyp) {
    return hydra.ext.java.Coder.buildTypeVarSubst_go(
      schemeVarSet,
      hydra.Rewriting.deannotateType(freshTyp),
      hydra.Rewriting.deannotateType(canonTyp));
  }

  static hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> buildTypeVarSubst_go(hydra.util.PersistentSet<hydra.core.Name> svs, hydra.core.Type ft, hydra.core.Type ct) {
    java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>> goSub = (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>>) (a -> (java.util.function.Function<hydra.core.Type, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (b -> hydra.ext.java.Coder.buildTypeVarSubst_go(
      svs,
      hydra.Rewriting.deannotateType(a),
      hydra.Rewriting.deannotateType(b))));
    return (ft).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Forall cfa) {
            return hydra.ext.java.Coder.buildTypeVarSubst_go(
              svs,
              ft,
              hydra.Rewriting.deannotateType((cfa).value.body));
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Variable fn) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Variable cn) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.And.apply(
                hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
                  (fn).value,
                  (cn).value)),
                hydra.lib.sets.Member.apply(
                  (cn).value,
                  svs)),
              () -> hydra.lib.maps.Singleton.apply(
                (fn).value,
                (cn).value),
              () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())));
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Function fft) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Function cft) {
            return hydra.lib.maps.Union.apply(
              (goSub).apply((fft).value.domain).apply((cft).value.domain),
              (goSub).apply((fft).value.codomain).apply((cft).value.codomain));
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Application fat) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Application cat) {
            return hydra.lib.maps.Union.apply(
              (goSub).apply((fat).value.function).apply((cat).value.function),
              (goSub).apply((fat).value.argument).apply((cat).value.argument));
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.List fl) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.List cl) {
            return (goSub).apply((fl).value).apply((cl).value);
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Set fs) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Set cs) {
            return (goSub).apply((fs).value).apply((cs).value);
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Maybe fm) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Maybe cm) {
            return (goSub).apply((fm).value).apply((cm).value);
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Map fmt) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Map cmt) {
            return hydra.lib.maps.Union.apply(
              (goSub).apply((fmt).value.keys).apply((cmt).value.keys),
              (goSub).apply((fmt).value.values).apply((cmt).value.values));
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Pair fpt) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Pair cpt) {
            return hydra.lib.maps.Union.apply(
              (goSub).apply((fpt).value.first).apply((cpt).value.first),
              (goSub).apply((fpt).value.second).apply((cpt).value.second));
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Either fet) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()));
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Either cet) {
            return hydra.lib.maps.Union.apply(
              (goSub).apply((fet).value.left).apply((cet).value.left),
              (goSub).apply((fet).value.right).apply((cet).value.right));
          }
        });
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Forall ffa) {
        return (ct).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
            return hydra.ext.java.Coder.buildTypeVarSubst_go(
              svs,
              hydra.Rewriting.deannotateType((ffa).value.body),
              ct);
          }

          @Override
          public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Forall cfa) {
            return (goSub).apply((ffa).value.body).apply((cfa).value.body);
          }
        });
      }
    });
  }

  static hydra.util.ConsList<hydra.ext.java.syntax.ClassModifier> classModsPublic() {
    return hydra.util.ConsList.of(new hydra.ext.java.syntax.ClassModifier.Public());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.environment.JavaSymbolClass> classifyDataReference(hydra.core.Name name, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.Binding>>right(hydra.Lexical.dereferenceElement(
        g,
        name)),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.environment.JavaSymbolClass>>) (mel -> hydra.lib.maybes.Cases.applyLazy(
        mel,
        () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.environment.JavaSymbolClass>right(new hydra.ext.java.environment.JavaSymbolClass.LocalVariable()),
        (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.environment.JavaSymbolClass>>) (el -> hydra.lib.maybes.Cases.applyLazy(
          (el).type,
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.environment.JavaSymbolClass>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
            "no type scheme for element ",
            (el).name.value))), cx))),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.environment.JavaSymbolClass>>) (ts -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.environment.JavaSymbolClass>right(hydra.ext.java.Coder.classifyDataTerm(
            ts,
            (el).term))))))));
  }

  static hydra.ext.java.environment.JavaSymbolClass classifyDataTerm(hydra.core.TypeScheme ts, hydra.core.Term term) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.Rewriting.isLambda(term),
      () -> ((java.util.function.Supplier<hydra.ext.java.environment.JavaSymbolClass>) (() -> {
        Integer n = hydra.ext.java.Coder.classifyDataTerm_countLambdaParams(term);
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Gt.apply(
            n,
            1),
          () -> new hydra.ext.java.environment.JavaSymbolClass.HoistedLambda(n),
          () -> new hydra.ext.java.environment.JavaSymbolClass.UnaryFunction());
      })).get(),
      () -> ((java.util.function.Supplier<hydra.ext.java.environment.JavaSymbolClass>) (() -> {
        hydra.util.Lazy<Boolean> hasTypeParams = new hydra.util.Lazy<>(() -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply((ts).variables)));
        return hydra.lib.logic.IfElse.lazy(
          hasTypeParams.get(),
          () -> ((java.util.function.Supplier<hydra.ext.java.environment.JavaSymbolClass>) (() -> {
            Integer n2 = hydra.ext.java.Coder.classifyDataTerm_countLambdaParams(hydra.ext.java.Coder.classifyDataTerm_stripTypeLambdas(term));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Gt.apply(
                n2,
                0),
              () -> new hydra.ext.java.environment.JavaSymbolClass.HoistedLambda(n2),
              () -> new hydra.ext.java.environment.JavaSymbolClass.NullaryFunction());
          })).get(),
          () -> new hydra.ext.java.environment.JavaSymbolClass.NullaryFunction());
      })).get());
  }

  static Integer classifyDataTerm_countLambdaParams(hydra.core.Term t) {
    return hydra.Rewriting.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Term instance) {
        return 0;
      }

      @Override
      public Integer visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public Integer otherwise(hydra.core.Function instance) {
            return 0;
          }

          @Override
          public Integer visit(hydra.core.Function.Lambda lam) {
            return hydra.lib.math.Add.apply(
              1,
              hydra.ext.java.Coder.classifyDataTerm_countLambdaParams((lam).value.body));
          }
        });
      }

      @Override
      public Integer visit(hydra.core.Term.Let lt) {
        return hydra.ext.java.Coder.classifyDataTerm_countLambdaParams((lt).value.body);
      }
    });
  }

  static hydra.core.Term classifyDataTerm_stripTypeLambdas(hydra.core.Term t) {
    return hydra.Rewriting.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return hydra.ext.java.Coder.classifyDataTerm_stripTypeLambdas((tl).value.body);
      }
    });
  }

  static <T0> hydra.ext.java.syntax.BlockStatement cmpDeclStatement(T0 aliases) {
    return hydra.ext.java.Utils.<T0>variableDeclarationStatement(
      aliases,
      hydra.ext.java.Utils.javaIntType(),
      hydra.ext.java.Utils.javaIdentifier("cmp"),
      hydra.ext.java.Utils.javaIntExpression(new java.math.BigInteger("0")));
  }

  static hydra.ext.java.syntax.Expression cmpNotZeroExpr() {
    hydra.util.Lazy<hydra.ext.java.syntax.EqualityExpression> lhs = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaRelationalExpressionToJavaEqualityExpression(hydra.ext.java.Utils.javaPostfixExpressionToJavaRelationalExpression(new hydra.ext.java.syntax.PostfixExpression.Name(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), hydra.ext.java.Utils.javaIdentifier("cmp"))))));
    hydra.ext.java.syntax.RelationalExpression rhs = hydra.ext.java.Utils.javaPostfixExpressionToJavaRelationalExpression(new hydra.ext.java.syntax.PostfixExpression.Primary(hydra.ext.java.Utils.javaLiteralToJavaPrimary(hydra.ext.java.Utils.javaInt(new java.math.BigInteger("0")))));
    return hydra.ext.java.Utils.javaEqualityExpressionToJavaExpression(new hydra.ext.java.syntax.EqualityExpression.NotEqual(new hydra.ext.java.syntax.EqualityExpression_Binary(lhs.get(), rhs)));
  }

  static hydra.util.ConsList<hydra.core.Name> collectForallParams(hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Forall fa) {
        return hydra.lib.lists.Cons.apply(
          (fa).value.parameter,
          hydra.ext.java.Coder.collectForallParams((fa).value.body));
      }
    });
  }

  static hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term> collectLambdaDomains(hydra.core.Term t) {
    return hydra.Rewriting.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), t)));
      }

      @Override
      public hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term> visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term> otherwise(hydra.core.Function instance) {
            return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), t)));
          }

          @Override
          public hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term> visit(hydra.core.Function.Lambda lam) {
            return hydra.lib.maybes.Cases.applyLazy(
              (lam).value.domain,
              () -> (hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), t))),
              (java.util.function.Function<hydra.core.Type, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>>) (dom -> {
                hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term> rest = hydra.ext.java.Coder.collectLambdaDomains((lam).value.body);
                return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term>(hydra.lib.lists.Cons.apply(
                  dom,
                  hydra.lib.pairs.First.apply(rest)), hydra.lib.pairs.Second.apply(rest))));
              }));
          }
        });
      }
    });
  }

  static hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>> collectTypeApps(hydra.core.Term t, hydra.util.ConsList<hydra.core.Type> acc) {
    return hydra.Rewriting.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>> otherwise(hydra.core.Term instance) {
        return (hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>>(hydra.Rewriting.deannotateTerm(t), acc)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.ext.java.Coder.collectTypeApps(
          (ta).value.body,
          hydra.lib.lists.Cons.apply(
            (ta).value.type,
            acc));
      }
    });
  }

  static hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>> collectTypeApps0(hydra.core.Term t, hydra.util.ConsList<hydra.core.Type> acc) {
    return hydra.Rewriting.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>> otherwise(hydra.core.Term instance) {
        return (hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>>(t, acc)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.ext.java.Coder.collectTypeApps0(
          (ta).value.body,
          hydra.lib.lists.Cons.apply(
            (ta).value.type,
            acc));
      }
    });
  }

  static hydra.util.PersistentSet<hydra.core.Name> collectTypeVars(hydra.core.Type typ) {
    return hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType(typ));
  }

  static hydra.util.PersistentSet<hydra.core.Name> collectTypeVars_go(hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.PersistentSet<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply());
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Variable name) {
        return hydra.lib.sets.Singleton.apply((name).value);
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Function ft) {
        return hydra.lib.sets.Union.apply(
          hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((ft).value.domain)),
          hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((ft).value.codomain)));
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Application at) {
        return hydra.lib.sets.Union.apply(
          hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((at).value.function)),
          hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((at).value.argument)));
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.List inner) {
        return hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((inner).value));
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Set inner) {
        return hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((inner).value));
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Maybe inner) {
        return hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((inner).value));
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Map mt) {
        return hydra.lib.sets.Union.apply(
          hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((mt).value.keys)),
          hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((mt).value.values)));
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.sets.Union.apply(
          hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((pt).value.first)),
          hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((pt).value.second)));
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Either et) {
        return hydra.lib.sets.Union.apply(
          hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((et).value.left)),
          hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((et).value.right)));
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.ext.java.Coder.collectTypeVars_go(hydra.Rewriting.deannotateType((ft).value.body));
      }
    });
  }

  static hydra.ext.java.syntax.Expression comparableCompareExpr(String otherVar, String fname) {
    hydra.ext.java.syntax.Expression arg = hydra.ext.java.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.Utils.fieldExpression(
      hydra.ext.java.Utils.javaIdentifier(otherVar),
      hydra.ext.java.Utils.javaIdentifier(fname)));
    hydra.ext.java.syntax.MethodInvocation_Variant castVar = new hydra.ext.java.syntax.MethodInvocation_Variant.Primary(hydra.ext.java.Utils.javaExpressionToJavaPrimary(hydra.ext.java.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.Utils.javaCastExpression(
      hydra.ext.java.Coder.javaComparableRefType(),
      hydra.ext.java.Utils.javaIdentifierToJavaUnaryExpression(new hydra.ext.java.syntax.Identifier(hydra.ext.java.Utils.sanitizeJavaName(fname)))))));
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> header = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(castVar, (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.compareToMethodName()))));
    return hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(header.get(), hydra.util.ConsList.of(arg)));
  }

  static hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement> compareAndReturnStmts(String otherVar, hydra.core.FieldType f) {
    return hydra.util.ConsList.of(
      new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaAssignmentStatement(
        new hydra.ext.java.syntax.LeftHandSide.ExpressionName(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), hydra.ext.java.Utils.javaIdentifier("cmp"))),
        hydra.ext.java.Coder.compareFieldExpr(
          otherVar,
          f))),
      new hydra.ext.java.syntax.BlockStatement.Statement(new hydra.ext.java.syntax.Statement.IfThen(new hydra.ext.java.syntax.IfThenStatement(hydra.ext.java.Coder.cmpNotZeroExpr(), hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.Utils.javaExpressionNameToJavaExpression(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), hydra.ext.java.Utils.javaIdentifier("cmp")))))))));
  }

  static hydra.ext.java.syntax.Expression compareFieldExpr(String otherVar, hydra.core.FieldType ft) {
    String fname = (ft).name.value;
    hydra.core.Type ftype = (ft).type;
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.java.Coder.isBinaryType(ftype),
      () -> hydra.ext.java.Coder.arraysCompareExpr(
        otherVar,
        fname),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.ext.java.Coder.isNonComparableType(ftype),
        () -> hydra.ext.java.Coder.hashCodeCompareExpr(
          otherVar,
          fname),
        () -> hydra.ext.java.Coder.comparableCompareExpr(
          otherVar,
          fname)));
  }

  static <T0> hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement> compareToBody(T0 aliases, String otherVar, hydra.util.ConsList<hydra.core.FieldType> fields) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(fields),
      () -> hydra.util.ConsList.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.Utils.javaIntExpression(new java.math.BigInteger("0")))))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(fields),
          1),
        () -> hydra.util.ConsList.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.Coder.compareFieldExpr(
          otherVar,
          hydra.lib.lists.Head.apply(fields)))))),
        () -> hydra.lib.lists.Concat2.apply(
          hydra.util.ConsList.of(hydra.ext.java.Coder.<T0>cmpDeclStatement(aliases)),
          hydra.lib.lists.Concat2.apply(
            hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.FieldType, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>) (f -> hydra.ext.java.Coder.compareAndReturnStmts(
                otherVar,
                f)),
              hydra.lib.lists.Init.apply(fields))),
            hydra.util.ConsList.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.Coder.compareFieldExpr(
              otherVar,
              hydra.lib.lists.Last.apply(fields))))))))));
  }

  static hydra.ext.java.syntax.InclusiveOrExpression compareToZeroClause(String tmpName, String fname) {
    hydra.ext.java.syntax.Expression compareToArg = hydra.ext.java.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.Utils.fieldExpression(
      hydra.ext.java.Utils.javaIdentifier(tmpName),
      hydra.ext.java.Utils.javaIdentifier(fname)));
    hydra.ext.java.syntax.MethodInvocation_Variant compareToVar = new hydra.ext.java.syntax.MethodInvocation_Variant.Expression(hydra.ext.java.Utils.fieldExpression(
      new hydra.ext.java.syntax.Identifier("this"),
      hydra.ext.java.Utils.javaIdentifier(fname)));
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> compareToHeader = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(compareToVar, (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.compareToMethodName()))));
    hydra.ext.java.syntax.EqualityExpression lhs = hydra.ext.java.Utils.javaRelationalExpressionToJavaEqualityExpression(hydra.ext.java.Utils.javaPostfixExpressionToJavaRelationalExpression(hydra.ext.java.Utils.javaMethodInvocationToJavaPostfixExpression(new hydra.ext.java.syntax.MethodInvocation(compareToHeader.get(), hydra.util.ConsList.of(compareToArg)))));
    hydra.ext.java.syntax.RelationalExpression rhs = hydra.ext.java.Utils.javaPostfixExpressionToJavaRelationalExpression(new hydra.ext.java.syntax.PostfixExpression.Primary(hydra.ext.java.Utils.javaLiteralToJavaPrimary(hydra.ext.java.Utils.javaInt(new java.math.BigInteger("0")))));
    return hydra.ext.java.Utils.javaEqualityExpressionToJavaInclusiveOrExpression(new hydra.ext.java.syntax.EqualityExpression.Equal(new hydra.ext.java.syntax.EqualityExpression_Binary(lhs, rhs)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> constantDecl(String javaName, hydra.ext.java.environment.Aliases aliases, hydra.core.Name name, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.ext.java.environment.JavaEnvironment env = new hydra.ext.java.environment.JavaEnvironment(aliases, g);
    hydra.util.ConsList<hydra.ext.java.syntax.FieldModifier> mods = hydra.util.ConsList.of(
      new hydra.ext.java.syntax.FieldModifier.Public(),
      new hydra.ext.java.syntax.FieldModifier.Static(),
      new hydra.ext.java.syntax.FieldModifier.Final());
    hydra.ext.java.syntax.Identifier nameName = hydra.ext.java.Utils.nameToJavaName(
      aliases,
      new hydra.core.Name("hydra.core.Name"));
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.java.Coder.encodeType(
        aliases,
        (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
        new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")),
        cx,
        g),
      (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (jt -> hydra.lib.eithers.Bind.apply(
        hydra.ext.java.Coder.encodeTerm(
          env,
          new hydra.core.Term.Literal(new hydra.core.Literal.String_((name).value)),
          cx,
          g),
        (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (arg -> {
          hydra.util.Lazy<hydra.ext.java.syntax.VariableInitializer> init = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.VariableInitializer.Expression(hydra.ext.java.Utils.javaConstructorCall(
            hydra.ext.java.Utils.javaConstructorName(
              nameName,
              (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
            hydra.util.ConsList.of(arg),
            (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing()))));
          hydra.ext.java.syntax.VariableDeclarator var = hydra.ext.java.Utils.javaVariableDeclarator(
            new hydra.ext.java.syntax.Identifier(javaName),
            hydra.util.Maybe.just(init.get()));
          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>right(hydra.ext.java.Coder.noComment(hydra.ext.java.Utils.javaMemberField(
            mods,
            jt,
            var)));
        }))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> constantDeclForFieldType(hydra.ext.java.environment.Aliases aliases, hydra.core.FieldType ftyp, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.core.Name name = (ftyp).name;
    String javaName = hydra.Formatting.nonAlnumToUnderscores(hydra.Formatting.convertCase(
      new hydra.util.CaseConvention.Camel(),
      new hydra.util.CaseConvention.UpperSnake(),
      (name).value));
    return hydra.ext.java.Coder.constantDecl(
      javaName,
      aliases,
      name,
      cx,
      g);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> constantDeclForTypeName(hydra.ext.java.environment.Aliases aliases, hydra.core.Name name, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.ext.java.Coder.constantDecl(
      "TYPE_",
      aliases,
      name,
      cx,
      g);
  }

  static hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit> constructElementsInterface(hydra.module.Module mod, hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMemberDeclaration> members) {
    hydra.ext.java.syntax.InterfaceBody body = new hydra.ext.java.syntax.InterfaceBody(members);
    hydra.module.Namespace ns = (mod).namespace;
    String className = hydra.ext.java.Coder.elementsClassName(ns);
    hydra.util.ConsList<hydra.ext.java.syntax.InterfaceModifier> mods = hydra.util.ConsList.of(new hydra.ext.java.syntax.InterfaceModifier.Public());
    hydra.util.Lazy<hydra.ext.java.syntax.TypeDeclaration> itf = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.TypeDeclaration.Interface(new hydra.ext.java.syntax.InterfaceDeclaration.NormalInterface(new hydra.ext.java.syntax.NormalInterfaceDeclaration(mods, hydra.ext.java.Utils.javaTypeIdentifier(className), (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()), (hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType>) (hydra.util.ConsList.<hydra.ext.java.syntax.InterfaceType>empty()), body))));
    hydra.ext.java.syntax.TypeDeclarationWithComments decl = new hydra.ext.java.syntax.TypeDeclarationWithComments(itf.get(), (mod).description);
    hydra.core.Name elName = hydra.ext.java.Coder.elementsQualifiedName(ns);
    hydra.util.Maybe<hydra.module.Namespace> parentNs = hydra.ext.java.Coder.namespaceParent(ns);
    hydra.util.Lazy<hydra.ext.java.syntax.PackageDeclaration> pkg = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
      parentNs,
      () -> hydra.ext.java.Utils.javaPackageDeclaration(ns),
      (java.util.function.Function<hydra.module.Namespace, hydra.ext.java.syntax.PackageDeclaration>) (pns -> hydra.ext.java.Utils.javaPackageDeclaration(pns))));
    return (hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>) ((hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>) (new hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>(elName, new hydra.ext.java.syntax.CompilationUnit.Ordinary(new hydra.ext.java.syntax.OrdinaryCompilationUnit(hydra.util.Maybe.just(pkg.get()), (hydra.util.ConsList<hydra.ext.java.syntax.ImportDeclaration>) (hydra.util.ConsList.<hydra.ext.java.syntax.ImportDeclaration>empty()), hydra.util.ConsList.of(decl))))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Type> correctCastType(hydra.core.Term innerBody, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.Type fallback, T0 cx, T1 g) {
    return hydra.Rewriting.deannotateTerm(innerBody).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T2, hydra.core.Type> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<T2, hydra.core.Type>right(fallback);
      }

      @Override
      public hydra.util.Either<T2, hydra.core.Type> visit(hydra.core.Term.Pair _p) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(typeArgs),
            2),
          () -> hydra.util.Either.<T2, hydra.core.Type>right(new hydra.core.Type.Pair(new hydra.core.PairType(hydra.lib.lists.Head.apply(typeArgs), hydra.lib.lists.Head.apply(hydra.lib.lists.Tail.apply(typeArgs))))),
          () -> hydra.util.Either.<T2, hydra.core.Type>right(fallback));
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>> correctTypeApps(T0 gr, hydra.core.Name name, hydra.util.ConsList<hydra.core.Term> args, hydra.util.ConsList<hydra.core.Type> fallbackTypeApps, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.Binding>>right(hydra.Lexical.dereferenceElement(
        g,
        name)),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>>) (mel -> hydra.lib.maybes.Cases.applyLazy(
        mel,
        () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>right(fallbackTypeApps),
        (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>>) (el -> hydra.lib.maybes.Cases.applyLazy(
          (el).type,
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>right(fallbackTypeApps),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>>) (ts -> {
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> allSchemeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.ext.java.Coder.isSimpleName(v)),
              (ts).variables));
            hydra.core.Type schemeType = (ts).type;
            Integer nParams = hydra.ext.java.Coder.countFunctionParams(schemeType);
            hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type> peeled = hydra.ext.java.Coder.peelDomainTypes(
              nParams,
              schemeType);
            hydra.util.Lazy<hydra.core.Type> calleeCod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(peeled));
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> calleeDoms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(peeled));
            hydra.util.PersistentSet<hydra.core.Name> schemeTypeVars = hydra.ext.java.Coder.collectTypeVars(schemeType);
            hydra.util.Lazy<hydra.util.ConsList<Boolean>> usedFlags = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.sets.Member.apply(
                v,
                schemeTypeVars)),
              allSchemeVars.get()));
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> usedSchemeVars = new hydra.util.Lazy<>(() -> hydra.ext.java.Coder.filterByFlags(
              allSchemeVars.get(),
              usedFlags.get()));
            hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> overgenSubst = hydra.ext.java.Coder.detectAccumulatorUnification(
              calleeDoms.get(),
              calleeCod.get(),
              usedSchemeVars.get());
            hydra.util.Lazy<hydra.util.ConsList<Boolean>> keepFlags = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.And.apply(
                hydra.lib.sets.Member.apply(
                  v,
                  schemeTypeVars),
                hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
                  v,
                  overgenSubst)))),
              allSchemeVars.get()));
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> filteredFallback0 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                hydra.lib.lists.Length.apply(allSchemeVars.get()),
                hydra.lib.lists.Length.apply(fallbackTypeApps)),
              () -> hydra.ext.java.Coder.filterByFlags(
                fallbackTypeApps,
                keepFlags.get()),
              () -> fallbackTypeApps));
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> filteredFallback = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.maps.Null.apply(overgenSubst),
              () -> filteredFallback0.get(),
              () -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> hydra.ext.java.Coder.substituteTypeVarsWithTypes(
                  overgenSubst,
                  t)),
                filteredFallback0.get())));
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> schemeVars = new hydra.util.Lazy<>(() -> hydra.ext.java.Coder.filterByFlags(
              allSchemeVars.get(),
              keepFlags.get()));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Or.apply(
                hydra.lib.lists.Null.apply(schemeVars.get()),
                hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
                  hydra.lib.lists.Length.apply(schemeVars.get()),
                  hydra.lib.lists.Length.apply(filteredFallback.get())))),
              () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>right(filteredFallback.get()),
              () -> hydra.ext.java.Coder.correctTypeAppsWithArgs(
                schemeVars.get(),
                filteredFallback.get(),
                schemeType,
                args,
                cx,
                g));
          }))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>> correctTypeAppsWithArgs(hydra.util.ConsList<hydra.core.Name> schemeVars, hydra.util.ConsList<hydra.core.Type> fallbackTypeApps, hydra.core.Type schemeType, hydra.util.ConsList<hydra.core.Term> args, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> irSubst = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      schemeVars,
      fallbackTypeApps)));
    hydra.util.Lazy<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>> peeled = new hydra.util.Lazy<>(() -> hydra.ext.java.Coder.peelDomainTypes(
      hydra.lib.lists.Length.apply(args),
      schemeType));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> schemeDoms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(peeled.get()));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> schemeVarSet = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(schemeVars));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.Type>>>) (arg -> hydra.lib.eithers.Bimap.apply(
          (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
          hydra.Annotations.getType(
            g,
            hydra.Annotations.termAnnotationInternal(arg)))),
        args),
      (java.util.function.Function<hydra.util.ConsList<hydra.util.Maybe<hydra.core.Type>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>>) (mArgTypes -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, Boolean>) (m -> hydra.lib.maybes.IsNothing.apply(m)),
          mArgTypes))),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>right(fallbackTypeApps),
        () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>>) (() -> {
          hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> argTypes = new hydra.util.Lazy<>(() -> hydra.lib.lists.Bind.apply(
            mArgTypes,
            (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.ConsList<hydra.core.Type>>) (m -> hydra.lib.maybes.Cases.applyLazy(
              m,
              () -> (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
              (java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>) (x -> hydra.lib.lists.Pure.apply(x))))));
          return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>>) (() -> {
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> irDoms = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (d -> hydra.ext.java.Coder.applySubstSimple(
                irSubst.get(),
                d)),
              schemeDoms.get()));
            return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>>) (() -> {
              hydra.util.Lazy<Boolean> domsMatch = new hydra.util.Lazy<>(() -> hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.core.Type>, Boolean>) (p -> hydra.lib.logic.Not.apply(hydra.ext.java.Coder.typesMatch(
                  hydra.Rewriting.deannotateType(hydra.lib.pairs.First.apply(p)),
                  hydra.Rewriting.deannotateType(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.lists.Zip.apply(
                  irDoms.get(),
                  argTypes.get()))));
              return hydra.lib.logic.IfElse.lazy(
                domsMatch.get(),
                () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>right(fallbackTypeApps),
                () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>right(hydra.ext.java.Coder.resolveTypeApps(
                  schemeVars,
                  fallbackTypeApps,
                  hydra.ext.java.Coder.buildArgSubst(
                    schemeVarSet.get(),
                    schemeDoms.get(),
                    argTypes.get()))));
            })).get();
          })).get();
        })).get())));
  }

  static Integer countFunctionParams(hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Type instance) {
        return 0;
      }

      @Override
      public Integer visit(hydra.core.Type.Function ft) {
        return hydra.lib.math.Add.apply(
          1,
          hydra.ext.java.Coder.countFunctionParams((ft).value.codomain));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration> declarationForRecordType(Boolean isInner, Boolean isSer, hydra.ext.java.environment.Aliases aliases, hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName, hydra.util.ConsList<hydra.core.FieldType> fields, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.ext.java.Coder.declarationForRecordType_(
      isInner,
      isSer,
      aliases,
      tparams,
      elName,
      (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing()),
      fields,
      cx,
      g);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration> declarationForRecordType_(Boolean isInner, Boolean isSer, hydra.ext.java.environment.Aliases aliases, hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName, hydra.util.Maybe<hydra.core.Name> parentName, hydra.util.ConsList<hydra.core.FieldType> fields, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclaration>>) (f -> hydra.ext.java.Coder.recordMemberVar(
          aliases,
          f,
          cx,
          g)),
        fields),
      (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclaration>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>>) (memberVars -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.ext.java.syntax.ClassBodyDeclaration, hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (p -> hydra.ext.java.Coder.addComment(
            hydra.lib.pairs.First.apply(p),
            hydra.lib.pairs.Second.apply(p),
            cx,
            g)),
          hydra.lib.lists.Zip.apply(
            memberVars,
            fields)),
        (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>>) (memberVars_ -> hydra.lib.eithers.Bind.apply(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Gt.apply(
              hydra.lib.lists.Length.apply(fields),
              1),
            () -> hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclaration>>) (f -> hydra.ext.java.Coder.recordWithMethod(
                aliases,
                elName,
                fields,
                f,
                cx,
                g)),
              fields),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclaration>>right((hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclaration>) (hydra.util.ConsList.<hydra.ext.java.syntax.ClassBodyDeclaration>empty()))),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclaration>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>>) (withMethods -> hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Coder.recordConstructor(
              aliases,
              elName,
              fields,
              cx,
              g),
            (java.util.function.Function<hydra.ext.java.syntax.ClassBodyDeclaration, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>>) (cons -> hydra.lib.eithers.Bind.apply(
              hydra.lib.logic.IfElse.lazy(
                isInner,
                () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>right((hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>) (hydra.util.ConsList.<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>empty())),
                () -> hydra.lib.eithers.Bind.apply(
                  hydra.ext.java.Coder.constantDeclForTypeName(
                    aliases,
                    elName,
                    cx,
                    g),
                  (java.util.function.Function<hydra.ext.java.syntax.ClassBodyDeclarationWithComments, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>>) (d -> hydra.lib.eithers.Bind.apply(
                    hydra.lib.eithers.MapList.apply(
                      (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (f -> hydra.ext.java.Coder.constantDeclForFieldType(
                        aliases,
                        f,
                        cx,
                        g)),
                      fields),
                    (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>>) (dfields -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>right(hydra.lib.lists.Cons.apply(
                      d,
                      dfields))))))),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>>) (tn -> {
                hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclaration>> comparableMethods = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
                  parentName,
                  () -> hydra.lib.logic.IfElse.lazy(
                    hydra.lib.logic.And.apply(
                      hydra.lib.logic.Not.apply(isInner),
                      isSer),
                    () -> hydra.util.ConsList.of(hydra.ext.java.Coder.recordCompareToMethod(
                      aliases,
                      tparams,
                      elName,
                      fields)),
                    () -> (hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclaration>) (hydra.util.ConsList.<hydra.ext.java.syntax.ClassBodyDeclaration>empty())),
                  (java.util.function.Function<hydra.core.Name, hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclaration>>) (pn -> hydra.lib.logic.IfElse.lazy(
                    isSer,
                    () -> hydra.util.ConsList.of(hydra.ext.java.Coder.variantCompareToMethod(
                      aliases,
                      tparams,
                      pn,
                      elName,
                      fields)),
                    () -> (hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclaration>) (hydra.util.ConsList.<hydra.ext.java.syntax.ClassBodyDeclaration>empty())))));
                hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>> bodyDecls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                  tn,
                  hydra.lib.lists.Concat2.apply(
                    memberVars_,
                    hydra.lib.lists.Map.apply(
                      (java.util.function.Function<hydra.ext.java.syntax.ClassBodyDeclaration, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>) (x -> hydra.ext.java.Coder.noComment(x)),
                      hydra.lib.lists.Concat2.apply(
                        hydra.util.ConsList.of(
                          cons,
                          hydra.ext.java.Coder.recordEqualsMethod(
                            aliases,
                            elName,
                            fields),
                          hydra.ext.java.Coder.recordHashCodeMethod(fields)),
                        hydra.lib.lists.Concat2.apply(
                          comparableMethods.get(),
                          withMethods))))));
                hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType>> ifaces = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                  isInner,
                  () -> hydra.ext.java.Coder.serializableTypes(isSer),
                  () -> hydra.ext.java.Coder.interfaceTypes(
                    isSer,
                    aliases,
                    tparams,
                    elName)));
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>right(hydra.ext.java.Utils.javaClassDeclaration(
                  aliases,
                  tparams,
                  elName,
                  hydra.ext.java.Coder.classModsPublic(),
                  (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing()),
                  ifaces.get(),
                  bodyDecls.get()));
              }))))))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration> declarationForUnionType(Boolean isSer, hydra.ext.java.environment.Aliases aliases, hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName, hydra.util.ConsList<hydra.core.FieldType> fields, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>>) (ft -> {
          hydra.core.Name fname = (ft).name;
          hydra.core.Type ftype = (ft).type;
          hydra.util.Lazy<hydra.util.ConsList<hydra.core.FieldType>> rfields = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
            hydra.Schemas.isUnitType(hydra.Rewriting.deannotateType(ftype)),
            () -> (hydra.util.ConsList<hydra.core.FieldType>) (hydra.util.ConsList.<hydra.core.FieldType>empty()),
            () -> hydra.util.ConsList.of(new hydra.core.FieldType(new hydra.core.Name("value"), hydra.Rewriting.deannotateType(ftype)))));
          hydra.core.Name varName = hydra.ext.java.Utils.variantClassName(
            false,
            elName,
            fname);
          return hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Coder.declarationForRecordType_(
              true,
              isSer,
              aliases,
              (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()),
              varName,
              hydra.lib.logic.IfElse.lazy(
                isSer,
                () -> hydra.util.Maybe.just(elName),
                () -> (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing())),
              rfields.get(),
              cx,
              g),
            (java.util.function.Function<hydra.ext.java.syntax.ClassDeclaration, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>>) (innerDecl -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>right(hydra.ext.java.Coder.augmentVariantClass(
              aliases,
              tparams,
              elName,
              innerDecl))));
        }),
        fields),
      (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ClassDeclaration>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>>) (variantClasses -> {
        hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclaration>> variantDecls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ext.java.syntax.ClassDeclaration, hydra.ext.java.syntax.ClassBodyDeclaration>) (vc -> new hydra.ext.java.syntax.ClassBodyDeclaration.ClassMember(new hydra.ext.java.syntax.ClassMemberDeclaration.Class_(vc))),
          variantClasses));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.ext.java.syntax.ClassBodyDeclaration, hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (pair -> hydra.ext.java.Coder.addComment(
              hydra.lib.pairs.First.apply(pair),
              hydra.lib.pairs.Second.apply(pair),
              cx,
              g)),
            hydra.lib.lists.Zip.apply(
              variantDecls.get(),
              fields)),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>>) (variantDecls_ -> {
            hydra.ext.java.syntax.ClassBodyDeclaration acceptDecl = hydra.ext.java.Utils.toAcceptMethod(
              true,
              tparams);
            hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMethodModifier> defaultMod = hydra.util.ConsList.of(new hydra.ext.java.syntax.InterfaceMethodModifier.Default());
            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>> typeArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument>) (tp -> hydra.ext.java.Utils.typeParameterToTypeArgument(tp)),
              tparams));
            hydra.util.Lazy<hydra.ext.java.syntax.FormalParameter> mainInstanceParam = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaTypeToJavaFormalParameter(
              hydra.ext.java.Utils.javaClassTypeToJavaType(hydra.ext.java.Utils.nameToJavaClassType(
                aliases,
                false,
                typeArgs.get(),
                elName,
                (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))),
              new hydra.core.Name("instance")));
            hydra.ext.java.syntax.Result resultR = hydra.ext.java.Utils.javaTypeToJavaResult(new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.Utils.visitorTypeVariable()));
            hydra.ext.java.syntax.BlockStatement throwStmt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaThrowIllegalStateException(hydra.util.ConsList.of(hydra.ext.java.Utils.javaAdditiveExpressionToJavaExpression(hydra.ext.java.Utils.addExpressions(hydra.util.ConsList.of(
              hydra.ext.java.Utils.javaStringMultiplicativeExpression("Non-exhaustive patterns when matching: "),
              new hydra.ext.java.syntax.MultiplicativeExpression.Unary(hydra.ext.java.Utils.javaIdentifierToJavaUnaryExpression(new hydra.ext.java.syntax.Identifier("instance")))))))));
            hydra.util.Lazy<hydra.ext.java.syntax.InterfaceMemberDeclaration> otherwiseDecl = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.interfaceMethodDeclaration(
              defaultMod,
              (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()),
              hydra.ext.java.Names.otherwiseMethodName(),
              hydra.util.ConsList.of(mainInstanceParam.get()),
              resultR,
              hydra.util.Maybe.just(hydra.util.ConsList.of(throwStmt))));
            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMemberDeclaration>> pvVisitMethods = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.FieldType, hydra.ext.java.syntax.InterfaceMemberDeclaration>) (ft -> {
                hydra.core.Name fname = (ft).name;
                hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation> mi = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.methodInvocation(
                  (hydra.util.Maybe<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>) (hydra.util.Maybe.<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>nothing()),
                  new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.otherwiseMethodName()),
                  hydra.util.ConsList.of(hydra.ext.java.Utils.javaIdentifierToJavaExpression(new hydra.ext.java.syntax.Identifier("instance")))));
                hydra.util.Lazy<hydra.ext.java.syntax.Type> varRef = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaClassTypeToJavaType(hydra.ext.java.Utils.nameToJavaClassType(
                  aliases,
                  false,
                  typeArgs.get(),
                  hydra.ext.java.Utils.variantClassName(
                    false,
                    elName,
                    fname),
                  (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
                hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.Utils.javaTypeToJavaFormalParameter(
                  varRef.get(),
                  new hydra.core.Name("instance"));
                hydra.ext.java.syntax.BlockStatement returnOtherwise = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.Utils.javaPrimaryToJavaExpression(hydra.ext.java.Utils.javaMethodInvocationToJavaPrimary(mi.get())))));
                return hydra.ext.java.Utils.interfaceMethodDeclaration(
                  defaultMod,
                  (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()),
                  hydra.ext.java.Names.visitMethodName(),
                  hydra.util.ConsList.of(param),
                  resultR,
                  hydra.util.Maybe.just(hydra.util.ConsList.of(returnOtherwise)));
              }),
              fields));
            hydra.util.Lazy<hydra.ext.java.syntax.InterfaceBody> pvBody = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.InterfaceBody(hydra.lib.lists.Concat2.apply(
              hydra.util.ConsList.of(otherwiseDecl.get()),
              pvVisitMethods.get())));
            hydra.util.Lazy<hydra.ext.java.syntax.ClassType> visitorClassType = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaClassType(
              hydra.lib.lists.Concat2.apply(
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.ReferenceType>) (tp -> hydra.ext.java.Utils.typeParameterToReferenceType(tp)),
                  tparams),
                hydra.util.ConsList.of(hydra.ext.java.Utils.visitorTypeVariable())),
              (hydra.util.Maybe<hydra.ext.java.syntax.PackageName>) (hydra.util.Maybe.<hydra.ext.java.syntax.PackageName>nothing()),
              hydra.ext.java.Names.visitorName()));
            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>> vtparams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
              tparams,
              hydra.util.ConsList.of(hydra.ext.java.Utils.javaTypeParameter(hydra.ext.java.Names.visitorReturnParameter()))));
            hydra.ext.java.syntax.ClassBodyDeclaration partialVisitor = hydra.ext.java.Utils.javaInterfaceDeclarationToJavaClassBodyDeclaration(new hydra.ext.java.syntax.NormalInterfaceDeclaration(hydra.util.ConsList.of(new hydra.ext.java.syntax.InterfaceModifier.Public()), new hydra.ext.java.syntax.TypeIdentifier(new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.partialVisitorName())), vtparams.get(), hydra.util.ConsList.of(new hydra.ext.java.syntax.InterfaceType(visitorClassType.get())), pvBody.get()));
            hydra.util.Lazy<hydra.ext.java.syntax.ClassBodyDeclaration> privateConst = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.makeConstructor(
              aliases,
              elName,
              true,
              (hydra.util.ConsList<hydra.ext.java.syntax.FormalParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.FormalParameter>empty()),
              (hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>) (hydra.util.ConsList.<hydra.ext.java.syntax.BlockStatement>empty())));
            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMemberDeclaration>> visitorMethods = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.FieldType, hydra.ext.java.syntax.InterfaceMemberDeclaration>) (ft -> {
                hydra.core.Name fname = (ft).name;
                hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>> typeArgs2 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument>) (tp -> hydra.ext.java.Utils.typeParameterToTypeArgument(tp)),
                  tparams));
                hydra.util.Lazy<hydra.ext.java.syntax.Type> varRef = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaClassTypeToJavaType(hydra.ext.java.Utils.nameToJavaClassType(
                  aliases,
                  false,
                  typeArgs2.get(),
                  hydra.ext.java.Utils.variantClassName(
                    false,
                    elName,
                    fname),
                  (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
                hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.Utils.javaTypeToJavaFormalParameter(
                  varRef.get(),
                  new hydra.core.Name("instance"));
                hydra.ext.java.syntax.Result resultR2 = hydra.ext.java.Utils.javaTypeToJavaResult(new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.Utils.visitorTypeVariable()));
                return hydra.ext.java.Utils.interfaceMethodDeclaration(
                  (hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMethodModifier>) (hydra.util.ConsList.<hydra.ext.java.syntax.InterfaceMethodModifier>empty()),
                  (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()),
                  hydra.ext.java.Names.visitMethodName(),
                  hydra.util.ConsList.of(param),
                  resultR,
                  (hydra.util.Maybe<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>) (hydra.util.Maybe.<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>nothing()));
              }),
              fields));
            hydra.ext.java.syntax.InterfaceBody visitorBody = new hydra.ext.java.syntax.InterfaceBody(visitorMethods.get());
            hydra.util.Lazy<hydra.ext.java.syntax.ClassBodyDeclaration> visitor = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaInterfaceDeclarationToJavaClassBodyDeclaration(new hydra.ext.java.syntax.NormalInterfaceDeclaration(hydra.util.ConsList.of(new hydra.ext.java.syntax.InterfaceModifier.Public()), new hydra.ext.java.syntax.TypeIdentifier(new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.visitorName())), vtparams.get(), (hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType>) (hydra.util.ConsList.<hydra.ext.java.syntax.InterfaceType>empty()), visitorBody)));
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.constantDeclForTypeName(
                aliases,
                elName,
                cx,
                g),
              (java.util.function.Function<hydra.ext.java.syntax.ClassBodyDeclarationWithComments, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>>) (tn0 -> hydra.lib.eithers.Bind.apply(
                hydra.lib.eithers.MapList.apply(
                  (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (ft -> hydra.ext.java.Coder.constantDeclForFieldType(
                    aliases,
                    ft,
                    cx,
                    g)),
                  fields),
                (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>>) (tn1 -> {
                  hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>> otherDecls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.ext.java.syntax.ClassBodyDeclaration, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>) (d -> hydra.ext.java.Coder.noComment(d)),
                    hydra.util.ConsList.of(
                      privateConst.get(),
                      acceptDecl,
                      visitor.get(),
                      partialVisitor)));
                  hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>> tn = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                    hydra.util.ConsList.of(tn0),
                    tn1));
                  hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>> bodyDecls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
                    tn.get(),
                    otherDecls.get(),
                    variantDecls_)));
                  hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.ClassModifier>> mods = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                    hydra.ext.java.Coder.classModsPublic(),
                    hydra.util.ConsList.of(new hydra.ext.java.syntax.ClassModifier.Abstract())));
                  return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>right(hydra.ext.java.Utils.javaClassDeclaration(
                    aliases,
                    tparams,
                    elName,
                    mods.get(),
                    (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing()),
                    hydra.ext.java.Coder.interfaceTypes(
                      isSer,
                      aliases,
                      tparams,
                      elName),
                    bodyDecls.get()));
                }))));
          }));
      }));
  }

  static hydra.util.Maybe<hydra.core.Type> decodeTypeFromTerm(hydra.core.Term term) {
    return hydra.Rewriting.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
        return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Union inj) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (inj).value.typeName,
            new hydra.core.Name("hydra.core.Type")),
          () -> ((java.util.function.Supplier<hydra.util.Maybe<hydra.core.Type>>) (() -> {
            String fname = (inj).value.field.name.value;
            return ((java.util.function.Supplier<hydra.util.Maybe<hydra.core.Type>>) (() -> {
              hydra.core.Term fterm = (inj).value.field.term;
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  fname,
                  "variable"),
                () -> (fterm).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                  }

                  @Override
                  public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Wrap wt) {
                    return (wt).value.body.accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                        return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                      }

                      @Override
                      public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Literal lit) {
                        return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                          @Override
                          public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Literal instance) {
                            return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                          }

                          @Override
                          public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Literal.String_ s) {
                            return hydra.util.Maybe.just(new hydra.core.Type.Variable(new hydra.core.Name((s).value)));
                          }
                        });
                      }
                    });
                  }
                }),
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.equality.Equal.apply(
                    fname,
                    "annotated"),
                  () -> (fterm).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                      return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                    }

                    @Override
                    public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Record rec) {
                      return hydra.lib.maybes.Bind.apply(
                        hydra.lib.lists.SafeHead.apply(hydra.lib.lists.Filter.apply(
                          (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                            (f).name,
                            new hydra.core.Name("body"))),
                          (rec).value.fields)),
                        (java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Type>>) (bodyField -> hydra.ext.java.Coder.decodeTypeFromTerm((bodyField).term)));
                    }
                  }),
                  () -> hydra.lib.logic.IfElse.lazy(
                    hydra.lib.equality.Equal.apply(
                      fname,
                      "application"),
                    () -> (fterm).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                        return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                      }

                      @Override
                      public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Record rec) {
                        return hydra.lib.maybes.Bind.apply(
                          hydra.lib.lists.SafeHead.apply(hydra.lib.lists.Filter.apply(
                            (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                              (f).name,
                              new hydra.core.Name("function"))),
                            (rec).value.fields)),
                          (java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Type>>) (funcField -> hydra.lib.maybes.Bind.apply(
                            hydra.ext.java.Coder.decodeTypeFromTerm((funcField).term),
                            (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (func -> hydra.lib.maybes.Bind.apply(
                              hydra.lib.lists.SafeHead.apply(hydra.lib.lists.Filter.apply(
                                (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                                  (f).name,
                                  new hydra.core.Name("argument"))),
                                (rec).value.fields)),
                              (java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Type>>) (argField -> hydra.lib.maybes.Map.apply(
                                (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (arg -> new hydra.core.Type.Application(new hydra.core.ApplicationType(func, arg))),
                                hydra.ext.java.Coder.decodeTypeFromTerm((argField).term))))))));
                      }
                    }),
                    () -> hydra.lib.logic.IfElse.lazy(
                      hydra.lib.equality.Equal.apply(
                        fname,
                        "function"),
                      () -> (fterm).accept(new hydra.core.Term.PartialVisitor<>() {
                        @Override
                        public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                          return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                        }

                        @Override
                        public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Record rec) {
                          return hydra.lib.maybes.Bind.apply(
                            hydra.lib.lists.SafeHead.apply(hydra.lib.lists.Filter.apply(
                              (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                                (f).name,
                                new hydra.core.Name("domain"))),
                              (rec).value.fields)),
                            (java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Type>>) (domField -> hydra.lib.maybes.Bind.apply(
                              hydra.ext.java.Coder.decodeTypeFromTerm((domField).term),
                              (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (dom -> hydra.lib.maybes.Bind.apply(
                                hydra.lib.lists.SafeHead.apply(hydra.lib.lists.Filter.apply(
                                  (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                                    (f).name,
                                    new hydra.core.Name("codomain"))),
                                  (rec).value.fields)),
                                (java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Type>>) (codField -> hydra.lib.maybes.Map.apply(
                                  (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (cod -> new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod))),
                                  hydra.ext.java.Coder.decodeTypeFromTerm((codField).term))))))));
                        }
                      }),
                      () -> hydra.lib.logic.IfElse.lazy(
                        hydra.lib.equality.Equal.apply(
                          fname,
                          "literal"),
                        () -> (fterm).accept(new hydra.core.Term.PartialVisitor<>() {
                          @Override
                          public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                            return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                          }

                          @Override
                          public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Union litInj) {
                            return hydra.lib.logic.IfElse.lazy(
                              hydra.lib.equality.Equal.apply(
                                (litInj).value.field.name.value,
                                "string"),
                              () -> hydra.util.Maybe.just(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
                              () -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()));
                          }
                        }),
                        () -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()))))));
            })).get();
          })).get(),
          () -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()));
      }
    });
  }

  static hydra.util.ConsList<hydra.core.Binding> dedupBindings(hydra.util.PersistentSet<hydra.core.Name> inScope, hydra.util.ConsList<hydra.core.Binding> bs) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(bs),
      () -> (hydra.util.ConsList<hydra.core.Binding>) (hydra.util.ConsList.<hydra.core.Binding>empty()),
      () -> ((java.util.function.Supplier<hydra.util.ConsList<hydra.core.Binding>>) (() -> {
        hydra.util.Lazy<hydra.core.Binding> b = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(bs));
        return ((java.util.function.Supplier<hydra.util.ConsList<hydra.core.Binding>>) (() -> {
          hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> rest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(bs));
          return ((java.util.function.Supplier<hydra.util.ConsList<hydra.core.Binding>>) (() -> {
            hydra.core.Name name = b.get().name;
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                name,
                inScope),
              () -> ((java.util.function.Supplier<hydra.util.ConsList<hydra.core.Binding>>) (() -> {
                hydra.core.Name newName = hydra.ext.java.Coder.freshJavaName(
                  name,
                  inScope);
                return ((java.util.function.Supplier<hydra.util.ConsList<hydra.core.Binding>>) (() -> {
                  hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> subst = new hydra.util.Lazy<>(() -> hydra.lib.maps.Singleton.apply(
                    name,
                    newName));
                  return ((java.util.function.Supplier<hydra.util.ConsList<hydra.core.Binding>>) (() -> {
                    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> rest2 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                      (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b2 -> new hydra.core.Binding((b2).name, hydra.Rewriting.substituteVariables(
                        subst.get(),
                        (b2).term), (b2).type)),
                      rest.get()));
                    return hydra.lib.lists.Cons.apply(
                      new hydra.core.Binding(newName, b.get().term, b.get().type),
                      hydra.ext.java.Coder.dedupBindings(
                        hydra.lib.sets.Insert.apply(
                          newName,
                          inScope),
                        rest2.get()));
                  })).get();
                })).get();
              })).get(),
              () -> hydra.lib.lists.Cons.apply(
                b.get(),
                hydra.ext.java.Coder.dedupBindings(
                  hydra.lib.sets.Insert.apply(
                    name,
                    inScope),
                  rest.get())));
          })).get();
        })).get();
      })).get());
  }

  static hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> detectAccumulatorUnification(hydra.util.ConsList<hydra.core.Type> doms, hydra.core.Type cod, hydra.util.ConsList<hydra.core.Name> tparams) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>> allPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Bind.apply(
      doms,
      (java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>>) (d -> hydra.ext.java.Coder.extractInOutPair(d))));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.util.ConsList<hydra.core.Name>>> groupedByInput = new hydra.util.Lazy<>(() -> hydra.ext.java.Coder.groupPairsByFirst(allPairs.get()));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> selfRefSubst = new hydra.util.Lazy<>(() -> hydra.ext.java.Coder.selfRefSubstitution(groupedByInput.get()));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> codSubst = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
      (java.util.function.Function<hydra.core.Name, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (cv -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.maps.Member.apply(
          cv,
          selfRefSubst.get()),
        () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
        () -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
          (java.util.function.Function<hydra.core.Name, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (refVar -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              cv,
              refVar),
            () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
            () -> hydra.lib.maps.Singleton.apply(
              cv,
              refVar))),
          hydra.ext.java.Coder.findSelfRefVar(groupedByInput.get())))),
      hydra.ext.java.Coder.findPairFirst(cod)));
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.Name>> codVar = new hydra.util.Lazy<>(() -> hydra.Rewriting.deannotateType(cod).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Type.Variable v) {
        return hydra.util.Maybe.just((v).value);
      }
    }));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> domVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Bind.apply(
      doms,
      (java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Name>>) (d -> hydra.lib.sets.ToList.apply(hydra.ext.java.Coder.collectTypeVars(d))))));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> danglingSubst = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
      (java.util.function.Function<hydra.core.Name, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>) (cv -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.sets.Member.apply(
          cv,
          domVars.get()),
        () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
        () -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
          (java.util.function.Function<hydra.core.Name, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>) (refVar -> hydra.lib.maps.Singleton.apply(
            cv,
            new hydra.core.Type.Variable(refVar))),
          hydra.ext.java.Coder.findSelfRefVar(groupedByInput.get())))),
      hydra.ext.java.Coder.findPairFirst(cod)));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> tparamSet = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(tparams));
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>> directPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Bind.apply(
      doms,
      (java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>>) (d -> hydra.ext.java.Coder.extractDirectReturn(
        tparamSet.get(),
        d))));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> directInputVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Name>, hydra.core.Name>) (p -> hydra.lib.pairs.First.apply(p)),
      directPairs.get())));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.util.ConsList<hydra.core.Name>>> groupedDirect = new hydra.util.Lazy<>(() -> hydra.ext.java.Coder.groupPairsByFirst(directPairs.get()));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> directRefSubst = new hydra.util.Lazy<>(() -> hydra.ext.java.Coder.directRefSubstitution(
      directInputVars.get(),
      codVar.get(),
      groupedDirect.get()));
    return hydra.lib.maps.Union.apply(
      hydra.lib.maps.Union.apply(
        hydra.lib.maps.Union.apply(
          hydra.ext.java.Coder.nameMapToTypeMap(selfRefSubst.get()),
          hydra.ext.java.Coder.nameMapToTypeMap(codSubst.get())),
        danglingSubst.get()),
      hydra.ext.java.Coder.nameMapToTypeMap(directRefSubst.get()));
  }

  static <T0> hydra.util.PersistentMap<T0, T0> directRefSubstitution(hydra.util.PersistentSet<T0> directInputVars, hydra.util.Maybe<T0> codVar, hydra.util.PersistentMap<T0, hydra.util.ConsList<T0>> grouped) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.PersistentMap<T0, T0>, java.util.function.Function<hydra.util.Pair<T0, hydra.util.ConsList<T0>>, hydra.util.PersistentMap<T0, T0>>>) (subst -> (java.util.function.Function<hydra.util.Pair<T0, hydra.util.ConsList<T0>>, hydra.util.PersistentMap<T0, T0>>) (entry -> hydra.ext.java.Coder.<T0>directRefSubstitution_processGroup(
        directInputVars,
        codVar,
        subst,
        hydra.lib.pairs.First.apply(entry),
        hydra.lib.pairs.Second.apply(entry)))),
      (hydra.util.PersistentMap<T0, T0>) ((hydra.util.PersistentMap<T0, T0>) (hydra.lib.maps.Empty.<T0, T0>apply())),
      hydra.lib.maps.ToList.apply(grouped));
  }

  static <T0> hydra.util.PersistentMap<T0, T0> directRefSubstitution_processGroup(hydra.util.PersistentSet<T0> directInputVars, hydra.util.Maybe<T0> codVar, hydra.util.PersistentMap<T0, T0> subst, T0 inVar, hydra.util.ConsList<T0> outVars) {
    hydra.util.Lazy<hydra.util.ConsList<T0>> safeNonSelfVars = new hydra.util.Lazy<>(() -> hydra.ext.java.Coder.<T0>directRefSubstitution_processGroup_safeNonSelfVars(
      codVar,
      directInputVars,
      hydra.ext.java.Coder.<T0>directRefSubstitution_processGroup_nonSelfVars(
        inVar,
        outVars)));
    hydra.util.Lazy<Integer> selfRefCount = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<T0, Boolean>) (v -> hydra.lib.equality.Equal.apply(
        v,
        inVar)),
      outVars)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Gte.apply(
          selfRefCount.get(),
          2),
        hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(safeNonSelfVars.get()))),
      () -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.util.PersistentMap<T0, T0>, java.util.function.Function<T0, hydra.util.PersistentMap<T0, T0>>>) (s -> (java.util.function.Function<T0, hydra.util.PersistentMap<T0, T0>>) (v -> hydra.lib.maps.Insert.apply(
          v,
          inVar,
          s))),
        subst,
        safeNonSelfVars.get()),
      () -> subst);
  }

  static <T0> hydra.util.ConsList<T0> directRefSubstitution_processGroup_nonSelfVars(T0 inVar, hydra.util.ConsList<T0> outVars) {
    return hydra.lib.lists.Filter.apply(
      (java.util.function.Function<T0, Boolean>) (v -> hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
        v,
        inVar))),
      outVars);
  }

  static <T0> hydra.util.ConsList<T0> directRefSubstitution_processGroup_safeNonSelfVars(hydra.util.Maybe<T0> codVar, hydra.util.PersistentSet<T0> directInputVars, hydra.util.ConsList<T0> nonSelfVars) {
    return hydra.lib.lists.Filter.apply(
      (java.util.function.Function<T0, Boolean>) (v -> hydra.lib.logic.And.apply(
        hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
          v,
          directInputVars)),
        hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          hydra.util.Maybe.just(v),
          codVar)))),
      nonSelfVars);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>> domTypeArgs(hydra.ext.java.environment.Aliases aliases, hydra.core.Type d, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.util.ConsList<hydra.core.Type> args = hydra.ext.java.Coder.extractTypeApplicationArgs(hydra.Rewriting.deannotateType(d));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(args)),
      () -> hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (t -> hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.encodeType(
            aliases,
            (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            t,
            cx,
            g),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (jt -> hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Utils.javaTypeToJavaReferenceType(
              jt,
              cx),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>right(new hydra.ext.java.syntax.TypeArgument.Reference(rt))))))),
        args),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>right(hydra.ext.java.Coder.javaTypeArgumentsForType(d)));
  }

  static hydra.ext.java.syntax.Identifier elementJavaIdentifier(Boolean isPrim, Boolean isMethod, hydra.ext.java.environment.Aliases aliases, hydra.core.Name name) {
    hydra.module.QualifiedName qn = hydra.Names.qualifyName(name);
    String local = (qn).local;
    hydra.util.Maybe<hydra.module.Namespace> ns_ = (qn).namespace;
    hydra.util.Lazy<String> sep = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      isMethod,
      () -> "::",
      () -> "."));
    return hydra.lib.logic.IfElse.lazy(
      isPrim,
      () -> new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.ext.java.Coder.elementJavaIdentifier_qualify(
            aliases,
            ns_,
            hydra.Formatting.capitalize(local)),
          "."),
        hydra.ext.java.Names.applyMethodName())),
      () -> hydra.lib.maybes.Cases.applyLazy(
        ns_,
        () -> new hydra.ext.java.syntax.Identifier(hydra.ext.java.Utils.sanitizeJavaName(local)),
        (java.util.function.Function<hydra.module.Namespace, hydra.ext.java.syntax.Identifier>) (n -> new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.ext.java.Coder.elementJavaIdentifier_qualify(
              aliases,
              hydra.ext.java.Coder.namespaceParent(n),
              hydra.ext.java.Coder.elementsClassName(n)),
            sep.get()),
          hydra.ext.java.Utils.sanitizeJavaName(local))))));
  }

  static String elementJavaIdentifier_qualify(hydra.ext.java.environment.Aliases aliases, hydra.util.Maybe<hydra.module.Namespace> mns, String s) {
    return hydra.ext.java.Utils.nameToJavaName(
      aliases,
      hydra.Names.unqualifyName(new hydra.module.QualifiedName(mns, s))).value;
  }

  static String elementsClassName(hydra.module.Namespace ns) {
    String nsStr = (ns).value;
    hydra.util.ConsList<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      nsStr);
    return hydra.Formatting.sanitizeWithUnderscores(
      hydra.ext.java.Language.reservedWords(),
      hydra.Formatting.capitalize(hydra.lib.lists.Last.apply(parts)));
  }

  static hydra.core.Name elementsQualifiedName(hydra.module.Namespace ns) {
    return hydra.Names.unqualifyName(new hydra.module.QualifiedName(hydra.ext.java.Coder.namespaceParent(ns), hydra.ext.java.Coder.elementsClassName(ns)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> encodeApplication(hydra.ext.java.environment.JavaEnvironment env, hydra.core.Application app, hydra.context.Context cx, hydra.graph.Graph g0) {
    hydra.ext.java.environment.Aliases aliases = (env).aliases;
    hydra.util.Lazy<hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>>> gathered = new hydra.util.Lazy<>(() -> hydra.CoderUtils.gatherArgsWithTypeApps(
      new hydra.core.Term.Application(app),
      (hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty()),
      (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> args = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(gathered.get())));
    hydra.util.Lazy<hydra.core.Term> fun = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered.get()));
    hydra.graph.Graph g = (env).graph;
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> typeApps = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(gathered.get())));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
        hydra.Annotations.getType(
          g,
          hydra.Annotations.termAnnotationInternal(fun.get()))),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (mfunTyp -> hydra.lib.eithers.Bind.apply(
        hydra.lib.maybes.Cases.applyLazy(
          mfunTyp,
          () -> hydra.CoderUtils.typeOfTerm(
            cx,
            g,
            fun.get()),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (t -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right(t))),
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (funTyp -> {
          Integer arity = hydra.Arity.typeArity(funTyp);
          hydra.core.Term deannotatedFun = hydra.Rewriting.deannotateTerm(fun.get());
          hydra.util.Lazy<hydra.util.Maybe<hydra.core.Name>> calleeName = new hydra.util.Lazy<>(() -> (deannotatedFun).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.core.Term instance) {
              return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
            }

            @Override
            public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Term.Function f) {
              return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
                @Override
                public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.core.Function instance) {
                  return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
                }

                @Override
                public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Function.Primitive n) {
                  return hydra.util.Maybe.just((n).value);
                }
              });
            }

            @Override
            public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Term.Variable n) {
              return hydra.util.Maybe.just((n).value);
            }
          }));
          return hydra.lib.eithers.Bind.apply(
            hydra.lib.maybes.Cases.applyLazy(
              calleeName.get(),
              () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Term>>right(args.get()),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Term>>>) (cname -> hydra.ext.java.Coder.annotateLambdaArgs(
                cname,
                typeApps.get(),
                args.get(),
                cx,
                g))),
            (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (annotatedArgs -> (deannotatedFun).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Term instance) {
                return hydra.ext.java.Coder.encodeApplication_fallback(
                  env,
                  aliases,
                  g,
                  typeApps.get(),
                  (app).function,
                  (app).argument,
                  cx,
                  g);
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Function f) {
                return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Function instance) {
                    return hydra.ext.java.Coder.encodeApplication_fallback(
                      env,
                      aliases,
                      g,
                      typeApps.get(),
                      (app).function,
                      (app).argument,
                      cx,
                      g);
                  }

                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Primitive name) {
                    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> hargs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Take.apply(
                      arity,
                      annotatedArgs));
                    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> rargs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Drop.apply(
                      arity,
                      annotatedArgs));
                    return hydra.lib.eithers.Bind.apply(
                      hydra.ext.java.Coder.functionCall(
                        env,
                        true,
                        (name).value,
                        hargs.get(),
                        (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
                        cx,
                        g),
                      (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (initialCall -> hydra.lib.eithers.Foldl.apply(
                        (java.util.function.Function<hydra.ext.java.syntax.Expression, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (h -> hydra.lib.eithers.Bind.apply(
                          hydra.ext.java.Coder.encodeTerm(
                            env,
                            h,
                            cx,
                            g),
                          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Coder.applyJavaArg(
                            acc,
                            jarg)))))),
                        initialCall,
                        rargs.get())));
                  }
                });
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Variable name) {
                return hydra.lib.logic.IfElse.lazy(
                  hydra.lib.logic.And.apply(
                    hydra.ext.java.Coder.isRecursiveVariable(
                      aliases,
                      (name).value),
                    hydra.lib.logic.Not.apply(hydra.ext.java.Coder.isLambdaBoundIn(
                      (name).value,
                      (aliases).lambdaVars))),
                  () -> hydra.ext.java.Coder.encodeApplication_fallback(
                    env,
                    aliases,
                    g,
                    typeApps.get(),
                    (app).function,
                    (app).argument,
                    cx,
                    g),
                  () -> hydra.lib.eithers.Bind.apply(
                    hydra.ext.java.Coder.classifyDataReference(
                      (name).value,
                      cx,
                      g),
                    (java.util.function.Function<hydra.ext.java.environment.JavaSymbolClass, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (symClass -> {
                      hydra.util.PersistentSet<hydra.core.Name> inScope = (aliases).inScopeTypeParams;
                      hydra.util.PersistentSet<hydra.core.Name> trusted = (aliases).trustedTypeVars;
                      hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> filteredTypeApps = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                        hydra.lib.logic.Or.apply(
                          hydra.lib.sets.Null.apply(trusted),
                          hydra.lib.sets.Null.apply(inScope)),
                        () -> (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
                        () -> ((java.util.function.Supplier<hydra.util.ConsList<hydra.core.Type>>) (() -> {
                          hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> allVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
                            (java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>) (t -> hydra.ext.java.Coder.collectTypeVars(t)),
                            typeApps.get())));
                          return hydra.lib.logic.IfElse.lazy(
                            hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.lib.sets.Difference.apply(
                              allVars.get(),
                              inScope))),
                            () -> (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
                            () -> hydra.lib.logic.IfElse.lazy(
                              hydra.lib.sets.Null.apply(hydra.lib.sets.Difference.apply(
                                allVars.get(),
                                trusted)),
                              () -> typeApps.get(),
                              () -> (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())));
                        })).get()));
                      Integer methodArity = (symClass).accept(new hydra.ext.java.environment.JavaSymbolClass.PartialVisitor<>() {
                        @Override
                        public Integer otherwise(hydra.ext.java.environment.JavaSymbolClass instance) {
                          return arity;
                        }

                        @Override
                        public Integer visit(hydra.ext.java.environment.JavaSymbolClass.HoistedLambda n) {
                          return (n).value;
                        }
                      });
                      hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> hargs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Take.apply(
                        methodArity,
                        annotatedArgs));
                      hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> rargs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Drop.apply(
                        methodArity,
                        annotatedArgs));
                      return hydra.lib.eithers.Bind.apply(
                        hydra.lib.logic.IfElse.lazy(
                          hydra.lib.lists.Null.apply(filteredTypeApps.get()),
                          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.core.Type>>right((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())),
                          () -> hydra.ext.java.Coder.correctTypeApps(
                            g,
                            (name).value,
                            hargs.get(),
                            filteredTypeApps.get(),
                            cx,
                            g)),
                        (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (safeTypeApps -> hydra.lib.eithers.Bind.apply(
                          hydra.ext.java.Coder.filterPhantomTypeArgs(
                            (name).value,
                            safeTypeApps,
                            cx,
                            g),
                          (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (finalTypeApps -> hydra.lib.eithers.Bind.apply(
                            hydra.ext.java.Coder.functionCall(
                              env,
                              false,
                              (name).value,
                              hargs.get(),
                              finalTypeApps,
                              cx,
                              g),
                            (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (initialCall -> hydra.lib.eithers.Foldl.apply(
                              (java.util.function.Function<hydra.ext.java.syntax.Expression, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (h -> hydra.lib.eithers.Bind.apply(
                                hydra.ext.java.Coder.encodeTerm(
                                  env,
                                  h,
                                  cx,
                                  g),
                                (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Coder.applyJavaArg(
                                  acc,
                                  jarg)))))),
                              initialCall,
                              rargs.get())))))));
                    })));
              }
            })));
        }))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> encodeApplication_fallback(hydra.ext.java.environment.JavaEnvironment env, hydra.ext.java.environment.Aliases aliases, hydra.graph.Graph gr, hydra.util.ConsList<hydra.core.Type> typeApps, hydra.core.Term lhs, hydra.core.Term rhs, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
        hydra.Annotations.getType(
          g,
          hydra.Annotations.termAnnotationInternal(lhs))),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (mt -> hydra.lib.eithers.Bind.apply(
        hydra.lib.maybes.Cases.applyLazy(
          mt,
          () -> hydra.CoderUtils.typeOfTerm(
            cx,
            g,
            lhs),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (typ -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right(typ))),
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (t -> hydra.Rewriting.deannotateTypeParameters(hydra.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Type instance) {
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.encodeTerm(
                env,
                lhs,
                cx,
                g),
              (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jfun -> hydra.lib.eithers.Bind.apply(
                hydra.ext.java.Coder.encodeTerm(
                  env,
                  rhs,
                  cx,
                  g),
                (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Coder.applyJavaArg(
                  jfun,
                  jarg))))));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Type.Function ft) {
            hydra.core.Type cod = (ft).value.codomain;
            hydra.core.Type dom = (ft).value.domain;
            return hydra.Rewriting.deannotateTerm(lhs).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Term instance) {
                return hydra.lib.eithers.Bind.apply(
                  hydra.ext.java.Coder.encodeTerm(
                    env,
                    lhs,
                    cx,
                    g),
                  (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jfun -> hydra.lib.eithers.Bind.apply(
                    hydra.ext.java.Coder.encodeTerm(
                      env,
                      rhs,
                      cx,
                      g),
                    (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Coder.applyJavaArg(
                      jfun,
                      jarg))))));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Function f) {
                return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Function instance) {
                    return hydra.lib.eithers.Bind.apply(
                      hydra.ext.java.Coder.encodeTerm(
                        env,
                        lhs,
                        cx,
                        g),
                      (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jfun -> hydra.lib.eithers.Bind.apply(
                        hydra.ext.java.Coder.encodeTerm(
                          env,
                          rhs,
                          cx,
                          g),
                        (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Coder.applyJavaArg(
                          jfun,
                          jarg))))));
                  }

                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Elimination e) {
                    return hydra.lib.eithers.Bind.apply(
                      hydra.ext.java.Coder.encodeTerm(
                        env,
                        rhs,
                        cx,
                        g),
                      (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jarg -> hydra.lib.eithers.Bind.apply(
                        hydra.lib.logic.IfElse.lazy(
                          hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.ext.java.Coder.javaTypeArgumentsForType(dom))),
                          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right(dom),
                          () -> hydra.lib.eithers.Bind.apply(
                            hydra.lib.eithers.Bimap.apply(
                              (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
                              (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
                              hydra.Annotations.getType(
                                g,
                                hydra.Annotations.termAnnotationInternal(rhs))),
                            (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (mrt -> hydra.lib.maybes.Cases.applyLazy(
                              mrt,
                              () -> hydra.lib.eithers.Bind.apply(
                                hydra.CoderUtils.typeOfTerm(
                                  cx,
                                  g,
                                  rhs),
                                (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right(hydra.lib.logic.IfElse.lazy(
                                  hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.ext.java.Coder.javaTypeArgumentsForType(rt))),
                                  () -> rt,
                                  () -> dom)))),
                              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right(hydra.lib.logic.IfElse.lazy(
                                hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.ext.java.Coder.javaTypeArgumentsForType(rt))),
                                () -> rt,
                                () -> dom))))))),
                        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (enrichedDom -> hydra.ext.java.Coder.encodeElimination(
                          env,
                          hydra.util.Maybe.just(jarg),
                          enrichedDom,
                          cod,
                          (e).value,
                          cx,
                          g)))));
                  }
                });
              }
            });
          }
        })))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>> encodeDefinitions(hydra.module.Module mod, hydra.util.ConsList<hydra.module.Definition> defs, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.ext.java.environment.Aliases aliases = hydra.ext.java.Utils.importAliasesForModule(mod);
    hydra.ext.java.environment.JavaEnvironment env = new hydra.ext.java.environment.JavaEnvironment(aliases, g);
    hydra.util.Pair<hydra.util.ConsList<hydra.module.TypeDefinition>, hydra.util.ConsList<hydra.module.TermDefinition>> partitioned = hydra.Schemas.partitionDefinitions(defs);
    hydra.util.Lazy<hydra.util.ConsList<hydra.module.TypeDefinition>> typeDefs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(partitioned));
    hydra.util.Lazy<hydra.util.ConsList<hydra.module.TypeDefinition>> nonTypedefDefs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.module.TypeDefinition, Boolean>) (td -> {
        hydra.core.Type typ = (td).type;
        return hydra.ext.java.Coder.isSerializableJavaType(typ);
      }),
      typeDefs.get()));
    hydra.ext.java.syntax.PackageDeclaration pkg = hydra.ext.java.Utils.javaPackageDeclaration((mod).namespace);
    hydra.util.Lazy<hydra.util.ConsList<hydra.module.TermDefinition>> termDefs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(partitioned));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.module.TypeDefinition, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>) (td -> hydra.ext.java.Coder.encodeTypeDefinition(
          pkg,
          aliases,
          td,
          cx,
          g)),
        nonTypedefDefs.get()),
      (java.util.function.Function<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>) (typeUnits -> hydra.lib.eithers.Bind.apply(
        hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(termDefs.get()),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>right((hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>empty())),
          () -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.module.TermDefinition, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (td -> hydra.ext.java.Coder.encodeTermDefinition(
                env,
                td,
                cx,
                g)),
              termDefs.get()),
            (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMemberDeclaration>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>>) (dataMembers -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>right(hydra.util.ConsList.of(hydra.ext.java.Coder.constructElementsInterface(
              mod,
              dataMembers)))))),
        (java.util.function.Function<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>) (termUnits -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>right(hydra.lib.maps.FromList.apply(hydra.lib.lists.Concat2.apply(
          typeUnits,
          termUnits)))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> encodeElimination(hydra.ext.java.environment.JavaEnvironment env, hydra.util.Maybe<hydra.ext.java.syntax.Expression> marg, hydra.core.Type dom, hydra.core.Type cod, hydra.core.Elimination elm, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.ext.java.environment.Aliases aliases = (env).aliases;
    return (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Elimination instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          "unexpected ",
          hydra.lib.strings.Cat2.apply(
            "elimination case",
            hydra.lib.strings.Cat2.apply(
              " in ",
              "encodeElimination"))))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Elimination.Record proj) {
        hydra.core.Name fname = (proj).value.field;
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.encodeType(
            aliases,
            (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            dom,
            cx,
            g),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jdom0 -> hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Utils.javaTypeToJavaReferenceType(
              jdom0,
              cx),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jdomr -> hydra.lib.maybes.Cases.applyLazy(
              marg,
              () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                hydra.core.Name projVar = new hydra.core.Name("projected");
                return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                  hydra.ext.java.syntax.Expression jbody = hydra.ext.java.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.Utils.fieldExpression(
                    hydra.ext.java.Utils.variableToJavaIdentifier(projVar),
                    hydra.ext.java.Utils.javaIdentifier((fname).value)));
                  return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaLambda(
                    projVar,
                    jbody));
                })).get();
              })).get(),
              (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jarg -> {
                hydra.ext.java.syntax.FieldAccess_Qualifier qual = new hydra.ext.java.syntax.FieldAccess_Qualifier.Primary(hydra.ext.java.Utils.javaExpressionToJavaPrimary(jarg));
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaFieldAccessToJavaExpression(new hydra.ext.java.syntax.FieldAccess(qual, hydra.ext.java.Utils.javaIdentifier((fname).value))));
              }))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Elimination.Union cs) {
        hydra.util.Maybe<hydra.core.Term> def_ = (cs).value.default_;
        hydra.util.ConsList<hydra.core.Field> fields = (cs).value.cases;
        hydra.core.Name tname = (cs).value.typeName;
        return hydra.lib.maybes.Cases.applyLazy(
          marg,
          () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
            hydra.core.Name uVar = new hydra.core.Name("u");
            return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
              hydra.core.Term typedLambda = new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(uVar, hydra.util.Maybe.just(dom), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(elm)), new hydra.core.Term.Variable(uVar))))));
              return hydra.ext.java.Coder.encodeTerm(
                env,
                typedLambda,
                cx,
                g);
            })).get();
          })).get(),
          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jarg -> {
            hydra.ext.java.syntax.Identifier consId = hydra.ext.java.Coder.innerClassRef(
              aliases,
              tname,
              hydra.ext.java.Names.partialVisitorName());
            hydra.core.Type effectiveCod = cod;
            hydra.ext.java.syntax.Primary prim = hydra.ext.java.Utils.javaExpressionToJavaPrimary(jarg);
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.encodeType(
                aliases,
                (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                effectiveCod,
                cx,
                g),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jcod -> hydra.lib.eithers.Bind.apply(
                hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                  jcod,
                  cx),
                (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (rt -> hydra.lib.eithers.Bind.apply(
                  hydra.ext.java.Coder.domTypeArgs(
                    aliases,
                    dom,
                    cx,
                    g),
                  (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (domArgs -> {
                    hydra.util.Lazy<hydra.ext.java.syntax.TypeArgumentsOrDiamond> targs = new hydra.util.Lazy<>(() -> hydra.ext.java.Coder.typeArgsOrDiamond(hydra.lib.lists.Concat2.apply(
                      domArgs,
                      hydra.util.ConsList.of(new hydra.ext.java.syntax.TypeArgument.Reference(rt)))));
                    return hydra.lib.eithers.Bind.apply(
                      hydra.lib.maybes.Cases.applyLazy(
                        def_,
                        () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>right((hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>) (hydra.util.ConsList.<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>empty())),
                        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>>) (d -> hydra.lib.eithers.Bind.apply(
                          hydra.ext.java.Coder.otherwiseBranch(
                            env,
                            aliases,
                            dom,
                            cod,
                            tname,
                            jcod,
                            domArgs,
                            d,
                            cx,
                            g),
                          (java.util.function.Function<hydra.ext.java.syntax.ClassBodyDeclarationWithComments, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>>) (b -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>right(hydra.util.ConsList.of(b)))))),
                      (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (otherwiseBranches -> hydra.lib.eithers.Bind.apply(
                        hydra.lib.eithers.MapList.apply(
                          (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (f -> hydra.ext.java.Coder.visitBranch(
                            env,
                            aliases,
                            dom,
                            tname,
                            jcod,
                            domArgs,
                            f,
                            cx,
                            g)),
                          fields),
                        (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ClassBodyDeclarationWithComments>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (visitBranches -> {
                          hydra.util.Lazy<hydra.ext.java.syntax.ClassBody> body = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.ClassBody(hydra.lib.lists.Concat2.apply(
                            otherwiseBranches,
                            visitBranches)));
                          hydra.util.Lazy<hydra.ext.java.syntax.Expression> visitor = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaConstructorCall(
                            hydra.ext.java.Utils.javaConstructorName(
                              consId,
                              hydra.util.Maybe.just(targs.get())),
                            (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()),
                            hydra.util.Maybe.just(body.get())));
                          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocation(
                            hydra.util.Maybe.just(hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>right(prim)),
                            new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.acceptMethodName()),
                            hydra.util.ConsList.of(visitor.get()))));
                        }))));
                  }))))));
          }));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Elimination.Wrap wrapName) {
        java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression> withArg = (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression>) (ja -> hydra.ext.java.Utils.javaFieldAccessToJavaExpression(new hydra.ext.java.syntax.FieldAccess(new hydra.ext.java.syntax.FieldAccess_Qualifier.Primary(hydra.ext.java.Utils.javaExpressionToJavaPrimary(ja)), hydra.ext.java.Utils.javaIdentifier(hydra.ext.java.Names.valueFieldName()))));
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.lib.maybes.Cases.applyLazy(
          marg,
          () -> ((java.util.function.Supplier<hydra.ext.java.syntax.Expression>) (() -> {
            hydra.core.Name wVar = new hydra.core.Name("wrapped");
            return ((java.util.function.Supplier<hydra.ext.java.syntax.Expression>) (() -> {
              hydra.ext.java.syntax.Expression wArg = hydra.ext.java.Utils.javaIdentifierToJavaExpression(hydra.ext.java.Utils.variableToJavaIdentifier(wVar));
              return hydra.ext.java.Utils.javaLambda(
                wVar,
                (withArg).apply(wArg));
            })).get();
          })).get(),
          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression>) (jarg -> (withArg).apply(jarg))));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> encodeFunction(hydra.ext.java.environment.JavaEnvironment env, hydra.core.Type dom, hydra.core.Type cod, hydra.core.Function fun, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.ext.java.environment.Aliases aliases = (env).aliases;
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Function instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Coder.encodeLiteral(new hydra.core.Literal.String_(hydra.lib.strings.Cat2.apply(
          "Unimplemented function variant: ",
          hydra.show.Core.function(fun)))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Elimination elm) {
        return hydra.ext.java.Coder.encodeElimination(
          env,
          (hydra.util.Maybe<hydra.ext.java.syntax.Expression>) (hydra.util.Maybe.<hydra.ext.java.syntax.Expression>nothing()),
          dom,
          cod,
          (elm).value,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Lambda lam) {
        return hydra.ext.java.Coder.withLambda(
          env,
          (lam).value,
          (java.util.function.Function<hydra.ext.java.environment.JavaEnvironment, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (env2 -> {
            hydra.core.Term body = (lam).value.body;
            hydra.core.Name lambdaVar = (lam).value.parameter;
            return hydra.Rewriting.deannotateTerm(body).accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Term instance) {
                return hydra.lib.eithers.Bind.apply(
                  hydra.ext.java.Coder.analyzeJavaFunction(
                    env2,
                    body,
                    cx,
                    g),
                  (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (fs -> {
                    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.ConsList<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
                    hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env3 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.ext.java.environment.JavaEnvironment>) (projected -> projected.environment)).apply(fs));
                    hydra.util.Lazy<hydra.core.Term> innerBody = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
                    return hydra.lib.eithers.Bind.apply(
                      hydra.ext.java.Coder.bindingsToStatements(
                        env3.get(),
                        bindings.get(),
                        cx,
                        g),
                      (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (bindResult -> {
                        hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> bindingStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
                        hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env4 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
                        return hydra.lib.eithers.Bind.apply(
                          hydra.ext.java.Coder.encodeTerm(
                            env4.get(),
                            innerBody.get(),
                            cx,
                            g),
                          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jbody -> {
                            hydra.util.Lazy<hydra.ext.java.syntax.Expression> lam1 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                              hydra.lib.lists.Null.apply(bindings.get()),
                              () -> hydra.ext.java.Utils.javaLambda(
                                lambdaVar,
                                jbody),
                              () -> ((java.util.function.Supplier<hydra.ext.java.syntax.Expression>) (() -> {
                                hydra.ext.java.syntax.BlockStatement returnSt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(jbody)));
                                return hydra.ext.java.Utils.javaLambdaFromBlock(
                                  lambdaVar,
                                  new hydra.ext.java.syntax.Block(hydra.lib.lists.Concat2.apply(
                                    bindingStmts.get(),
                                    hydra.util.ConsList.of(returnSt))));
                              })).get()));
                            return hydra.ext.java.Coder.applyCastIfSafe(
                              aliases,
                              new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod)),
                              lam1.get(),
                              cx,
                              g);
                          }));
                      }));
                  }));
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Function f2) {
                return (f2).value.accept(new hydra.core.Function.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Function instance) {
                    return hydra.lib.eithers.Bind.apply(
                      hydra.ext.java.Coder.analyzeJavaFunction(
                        env2,
                        body,
                        cx,
                        g),
                      (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (fs -> {
                        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.ConsList<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
                        hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env3 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.ext.java.environment.JavaEnvironment>) (projected -> projected.environment)).apply(fs));
                        hydra.util.Lazy<hydra.core.Term> innerBody = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
                        return hydra.lib.eithers.Bind.apply(
                          hydra.ext.java.Coder.bindingsToStatements(
                            env3.get(),
                            bindings.get(),
                            cx,
                            g),
                          (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (bindResult -> {
                            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> bindingStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
                            hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env4 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
                            return hydra.lib.eithers.Bind.apply(
                              hydra.ext.java.Coder.encodeTerm(
                                env4.get(),
                                innerBody.get(),
                                cx,
                                g),
                              (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jbody -> {
                                hydra.util.Lazy<hydra.ext.java.syntax.Expression> lam1 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                                  hydra.lib.lists.Null.apply(bindings.get()),
                                  () -> hydra.ext.java.Utils.javaLambda(
                                    lambdaVar,
                                    jbody),
                                  () -> ((java.util.function.Supplier<hydra.ext.java.syntax.Expression>) (() -> {
                                    hydra.ext.java.syntax.BlockStatement returnSt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(jbody)));
                                    return hydra.ext.java.Utils.javaLambdaFromBlock(
                                      lambdaVar,
                                      new hydra.ext.java.syntax.Block(hydra.lib.lists.Concat2.apply(
                                        bindingStmts.get(),
                                        hydra.util.ConsList.of(returnSt))));
                                  })).get()));
                                return hydra.ext.java.Coder.applyCastIfSafe(
                                  aliases,
                                  new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod)),
                                  lam1.get(),
                                  cx,
                                  g);
                              }));
                          }));
                      }));
                  }

                  @Override
                  public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Lambda innerLam) {
                    return hydra.Rewriting.deannotateType(cod).accept(new hydra.core.Type.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Type instance) {
                        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
                          "expected function type for lambda body, but got: ",
                          hydra.show.Core.type(cod)))), cx)));
                      }

                      @Override
                      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Type.Function ft) {
                        hydra.core.Type cod2 = (ft).value.codomain;
                        hydra.core.Type dom2 = (ft).value.domain;
                        return hydra.lib.eithers.Bind.apply(
                          hydra.ext.java.Coder.encodeFunction(
                            env2,
                            dom2,
                            cod2,
                            new hydra.core.Function.Lambda((innerLam).value),
                            cx,
                            g),
                          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (innerJavaLambda -> {
                            hydra.ext.java.syntax.Expression lam1 = hydra.ext.java.Utils.javaLambda(
                              lambdaVar,
                              innerJavaLambda);
                            return hydra.ext.java.Coder.applyCastIfSafe(
                              aliases,
                              new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod)),
                              lam1,
                              cx,
                              g);
                          }));
                      }
                    });
                  }
                });
              }
            });
          }));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Primitive name) {
        Integer arity = hydra.Arity.typeArity(new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod)));
        String classWithApply = hydra.ext.java.Coder.elementJavaIdentifier(
          true,
          false,
          aliases,
          (name).value).value;
        String suffix = hydra.lib.strings.Cat2.apply(
          ".",
          hydra.ext.java.Names.applyMethodName());
        hydra.util.Lazy<String> className = new hydra.util.Lazy<>(() -> hydra.lib.strings.FromList.apply(hydra.lib.lists.Take.apply(
          hydra.lib.math.Sub.apply(
            hydra.lib.strings.Length.apply(classWithApply),
            hydra.lib.strings.Length.apply(suffix)),
          hydra.lib.strings.ToList.apply(classWithApply))));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Lte.apply(
            arity,
            1),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaIdentifierToJavaExpression(new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
            className.get(),
            "::",
            hydra.ext.java.Names.applyMethodName()))))),
          () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> paramNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<Integer, hydra.core.Name>) (i -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                "p",
                hydra.lib.literals.ShowInt32.apply(i)))),
              hydra.lib.math.Range.apply(
                0,
                hydra.lib.math.Sub.apply(
                  arity,
                  1))));
            return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
              hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.Expression>> paramExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.Expression>) (p -> hydra.ext.java.Utils.javaIdentifierToJavaExpression(hydra.ext.java.Utils.variableToJavaIdentifier(p))),
                paramNames.get()));
              return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                hydra.ext.java.syntax.Identifier classId = new hydra.ext.java.syntax.Identifier(className.get());
                return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                  hydra.ext.java.syntax.Expression call = hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStatic(
                    classId,
                    new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.applyMethodName()),
                    paramExprs.get()));
                  return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                    hydra.ext.java.syntax.Expression curried = hydra.ext.java.Coder.buildCurriedLambda(
                      paramNames.get(),
                      call);
                    return hydra.lib.eithers.Bind.apply(
                      hydra.ext.java.Coder.encodeType(
                        aliases,
                        (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                        new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod)),
                        cx,
                        g),
                      (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jtype -> hydra.lib.eithers.Bind.apply(
                        hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                          jtype,
                          cx),
                        (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.Utils.javaCastExpression(
                          rt,
                          hydra.ext.java.Utils.javaExpressionToJavaUnaryExpression(curried))))))));
                  })).get();
                })).get();
              })).get();
            })).get();
          })).get());
      }
    });
  }

  static hydra.ext.java.syntax.Expression encodeLiteral(hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.Literal.Binary bs) {
        hydra.util.ConsList<Integer> byteValues = hydra.lib.literals.BinaryToBytes.apply((bs).value);
        return hydra.ext.java.Utils.javaArrayCreation(
          hydra.ext.java.Utils.javaBytePrimitiveType(),
          hydra.util.Maybe.just(hydra.ext.java.Utils.javaArrayInitializer(hydra.lib.lists.Map.apply(
            (java.util.function.Function<Integer, hydra.ext.java.syntax.Expression>) (w -> hydra.ext.java.Utils.javaLiteralToJavaExpression(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Int32ToBigint.apply(w))))),
            byteValues))));
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.Literal.Boolean_ b) {
        return hydra.ext.java.Coder.encodeLiteral_litExp(hydra.ext.java.Utils.javaBoolean((b).value));
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.Literal.Float_ f) {
        return hydra.ext.java.Coder.encodeLiteral_encodeFloat((f).value);
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.Literal.Integer_ i) {
        return hydra.ext.java.Coder.encodeLiteral_encodeInteger((i).value);
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.Literal.String_ s) {
        return hydra.ext.java.Coder.encodeLiteral_litExp(hydra.ext.java.Utils.javaString((s).value));
      }
    });
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.ext.java.syntax.Type> encodeLiteralType(hydra.core.LiteralType lt, T0 cx, T1 g) {
    return (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.LiteralType.Binary ignored) {
        return hydra.util.Either.<T2, hydra.ext.java.syntax.Type>right(new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.Array(new hydra.ext.java.syntax.ArrayType(new hydra.ext.java.syntax.Dims(hydra.util.ConsList.of((hydra.util.ConsList<hydra.ext.java.syntax.Annotation>) (hydra.util.ConsList.<hydra.ext.java.syntax.Annotation>empty()))), new hydra.ext.java.syntax.ArrayType_Variant.Primitive(new hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Byte_())), (hydra.util.ConsList<hydra.ext.java.syntax.Annotation>) (hydra.util.ConsList.<hydra.ext.java.syntax.Annotation>empty())))))));
      }

      @Override
      public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.LiteralType.Boolean_ ignored) {
        return hydra.ext.java.Coder.<T0, T1, T2>encodeLiteralType_simple(
          "Boolean",
          cx,
          g);
      }

      @Override
      public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.LiteralType.Float_ ft) {
        return (ft).value.accept(new hydra.core.FloatType.PartialVisitor<>() {
          @Override
          public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.FloatType.Bigfloat ignored) {
            return hydra.util.Either.<T2, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaRefType(
              (hydra.util.ConsList<hydra.ext.java.syntax.ReferenceType>) (hydra.util.ConsList.<hydra.ext.java.syntax.ReferenceType>empty()),
              hydra.util.Maybe.just(hydra.ext.java.Names.javaPackageName(hydra.util.ConsList.of(
                "java",
                "math"))),
              "BigDecimal"));
          }

          @Override
          public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.FloatType.Float32 ignored) {
            return hydra.ext.java.Coder.<T0, T1, T2>encodeLiteralType_simple(
              "Float",
              cx,
              g);
          }

          @Override
          public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.FloatType.Float64 ignored) {
            return hydra.ext.java.Coder.<T0, T1, T2>encodeLiteralType_simple(
              "Double",
              cx,
              g);
          }
        });
      }

      @Override
      public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.LiteralType.Integer_ it) {
        return (it).value.accept(new hydra.core.IntegerType.PartialVisitor<>() {
          @Override
          public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Bigint ignored) {
            return hydra.util.Either.<T2, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaRefType(
              (hydra.util.ConsList<hydra.ext.java.syntax.ReferenceType>) (hydra.util.ConsList.<hydra.ext.java.syntax.ReferenceType>empty()),
              hydra.util.Maybe.just(hydra.ext.java.Names.javaPackageName(hydra.util.ConsList.of(
                "java",
                "math"))),
              "BigInteger"));
          }

          @Override
          public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Int8 ignored) {
            return hydra.ext.java.Coder.<T0, T1, T2>encodeLiteralType_simple(
              "Byte",
              cx,
              g);
          }

          @Override
          public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Int16 ignored) {
            return hydra.ext.java.Coder.<T0, T1, T2>encodeLiteralType_simple(
              "Short",
              cx,
              g);
          }

          @Override
          public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Int32 ignored) {
            return hydra.ext.java.Coder.<T0, T1, T2>encodeLiteralType_simple(
              "Integer",
              cx,
              g);
          }

          @Override
          public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Int64 ignored) {
            return hydra.ext.java.Coder.<T0, T1, T2>encodeLiteralType_simple(
              "Long",
              cx,
              g);
          }

          @Override
          public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Uint8 ignored) {
            return hydra.ext.java.Coder.<T0, T1, T2>encodeLiteralType_simple(
              "Short",
              cx,
              g);
          }

          @Override
          public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Uint16 ignored) {
            return hydra.ext.java.Coder.<T0, T1, T2>encodeLiteralType_simple(
              "Character",
              cx,
              g);
          }

          @Override
          public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Uint32 ignored) {
            return hydra.ext.java.Coder.<T0, T1, T2>encodeLiteralType_simple(
              "Long",
              cx,
              g);
          }

          @Override
          public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.IntegerType.Uint64 ignored) {
            return hydra.util.Either.<T2, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaRefType(
              (hydra.util.ConsList<hydra.ext.java.syntax.ReferenceType>) (hydra.util.ConsList.<hydra.ext.java.syntax.ReferenceType>empty()),
              hydra.util.Maybe.just(hydra.ext.java.Names.javaPackageName(hydra.util.ConsList.of(
                "java",
                "math"))),
              "BigInteger"));
          }
        });
      }

      @Override
      public hydra.util.Either<T2, hydra.ext.java.syntax.Type> visit(hydra.core.LiteralType.String_ ignored) {
        return hydra.ext.java.Coder.<T0, T1, T2>encodeLiteralType_simple(
          "String",
          cx,
          g);
      }
    });
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.ext.java.syntax.Type> encodeLiteralType_simple(String n, T0 cx, T1 g) {
    return hydra.util.Either.<T2, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaRefType(
      (hydra.util.ConsList<hydra.ext.java.syntax.ReferenceType>) (hydra.util.ConsList.<hydra.ext.java.syntax.ReferenceType>empty()),
      (hydra.util.Maybe<hydra.ext.java.syntax.PackageName>) (hydra.util.Maybe.<hydra.ext.java.syntax.PackageName>nothing()),
      n));
  }

  static hydra.ext.java.syntax.Expression encodeLiteral_encodeFloat(hydra.core.FloatValue f) {
    return (f).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.FloatValue.Bigfloat v) {
        return hydra.ext.java.Utils.javaConstructorCall(
          hydra.ext.java.Utils.javaConstructorName(
            new hydra.ext.java.syntax.Identifier("java.math.BigDecimal"),
            (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
          hydra.util.ConsList.of(hydra.ext.java.Coder.encodeLiteral(new hydra.core.Literal.String_(hydra.lib.literals.ShowBigfloat.apply((v).value)))),
          (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing()));
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.FloatValue.Float32 v) {
        return hydra.ext.java.Coder.encodeLiteral_primCast(
          new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.FloatingPoint(new hydra.ext.java.syntax.FloatingPointType.Float_())),
          hydra.ext.java.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.FloatingPoint(new hydra.ext.java.syntax.FloatingPointLiteral(hydra.lib.literals.Float32ToBigfloat.apply((v).value)))));
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.FloatValue.Float64 v) {
        return hydra.ext.java.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.FloatingPoint(new hydra.ext.java.syntax.FloatingPointLiteral(hydra.lib.literals.Float64ToBigfloat.apply((v).value))));
      }
    });
  }

  static hydra.ext.java.syntax.Expression encodeLiteral_encodeInteger(hydra.core.IntegerValue i) {
    return (i).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Bigint v) {
        return hydra.ext.java.Utils.javaConstructorCall(
          hydra.ext.java.Utils.javaConstructorName(
            new hydra.ext.java.syntax.Identifier("java.math.BigInteger"),
            (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
          hydra.util.ConsList.of(hydra.ext.java.Coder.encodeLiteral(new hydra.core.Literal.String_(hydra.lib.literals.ShowBigint.apply((v).value)))),
          (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing()));
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Int8 v) {
        return hydra.ext.java.Coder.encodeLiteral_primCast(
          new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Byte_())),
          hydra.ext.java.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Int8ToBigint.apply((v).value)))));
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Int16 v) {
        return hydra.ext.java.Coder.encodeLiteral_primCast(
          new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Short_())),
          hydra.ext.java.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Int16ToBigint.apply((v).value)))));
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Int32 v) {
        return hydra.ext.java.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Int32ToBigint.apply((v).value))));
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Int64 v) {
        return hydra.ext.java.Coder.encodeLiteral_primCast(
          new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Long_())),
          hydra.ext.java.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Int64ToBigint.apply((v).value)))));
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Uint8 v) {
        return hydra.ext.java.Coder.encodeLiteral_primCast(
          new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Short_())),
          hydra.ext.java.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Uint8ToBigint.apply((v).value)))));
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Uint16 v) {
        return hydra.ext.java.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Character_((v).value));
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Uint32 v) {
        return hydra.ext.java.Coder.encodeLiteral_primCast(
          new hydra.ext.java.syntax.PrimitiveType.Numeric(new hydra.ext.java.syntax.NumericType.Integral(new hydra.ext.java.syntax.IntegralType.Long_())),
          hydra.ext.java.Coder.encodeLiteral_litExp(new hydra.ext.java.syntax.Literal.Integer_(new hydra.ext.java.syntax.IntegerLiteral(hydra.lib.literals.Uint32ToBigint.apply((v).value)))));
      }

      @Override
      public hydra.ext.java.syntax.Expression visit(hydra.core.IntegerValue.Uint64 v) {
        return hydra.ext.java.Utils.javaConstructorCall(
          hydra.ext.java.Utils.javaConstructorName(
            new hydra.ext.java.syntax.Identifier("java.math.BigInteger"),
            (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
          hydra.util.ConsList.of(hydra.ext.java.Coder.encodeLiteral(new hydra.core.Literal.String_(hydra.lib.literals.ShowBigint.apply(hydra.lib.literals.Uint64ToBigint.apply((v).value))))),
          (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing()));
      }
    });
  }

  static hydra.ext.java.syntax.Expression encodeLiteral_litExp(hydra.ext.java.syntax.Literal l) {
    return hydra.ext.java.Utils.javaLiteralToJavaExpression(l);
  }

  static hydra.ext.java.syntax.Expression encodeLiteral_primCast(hydra.ext.java.syntax.PrimitiveType pt, hydra.ext.java.syntax.Expression expr) {
    return hydra.ext.java.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.Utils.javaCastPrimitive(
      pt,
      hydra.ext.java.Utils.javaExpressionToJavaUnaryExpression(expr)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> encodeNullaryConstant(hydra.ext.java.environment.JavaEnvironment env, hydra.core.Type typ, hydra.core.Function fun, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.ext.java.environment.Aliases aliases = (env).aliases;
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Function instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          "unexpected ",
          hydra.lib.strings.Cat2.apply(
            "nullary function",
            hydra.lib.strings.Cat2.apply(
              " in ",
              hydra.show.Core.function(fun)))))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Function.Primitive name) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.encodeNullaryConstant_typeArgsFromReturnType(
            aliases,
            typ,
            cx,
            g),
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (targs -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(targs),
            () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
              hydra.ext.java.syntax.MethodInvocation_Header header = new hydra.ext.java.syntax.MethodInvocation_Header.Simple(new hydra.ext.java.syntax.MethodName(hydra.ext.java.Coder.elementJavaIdentifier(
                true,
                false,
                aliases,
                (name).value)));
              return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(header, (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()))));
            })).get(),
            () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
              String fullName = hydra.ext.java.Coder.elementJavaIdentifier(
                true,
                false,
                aliases,
                (name).value).value;
              return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                hydra.util.ConsList<String> parts = hydra.lib.strings.SplitOn.apply(
                  ".",
                  fullName);
                return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                  hydra.util.Lazy<hydra.ext.java.syntax.Identifier> className = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Intercalate.apply(
                    ".",
                    hydra.lib.lists.Init.apply(parts))));
                  return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                    hydra.util.Lazy<hydra.ext.java.syntax.Identifier> methodName = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Identifier(hydra.lib.lists.Last.apply(parts)));
                    return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStaticWithTypeArgs(
                      className.get(),
                      methodName.get(),
                      targs,
                      (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()))));
                  })).get();
                })).get();
              })).get();
            })).get())));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>> encodeNullaryConstant_typeArgsFromReturnType(hydra.ext.java.environment.Aliases aliases, hydra.core.Type t, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>right((hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>> visit(hydra.core.Type.Set st) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.encodeType(
            aliases,
            (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            (st).value,
            cx,
            g),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>) (jst -> hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Utils.javaTypeToJavaReferenceType(
              jst,
              cx),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>right(hydra.util.ConsList.of(new hydra.ext.java.syntax.TypeArgument.Reference(rt)))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>> visit(hydra.core.Type.List lt_) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.encodeType(
            aliases,
            (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            (lt_).value,
            cx,
            g),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>) (jlt -> hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Utils.javaTypeToJavaReferenceType(
              jlt,
              cx),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>right(hydra.util.ConsList.of(new hydra.ext.java.syntax.TypeArgument.Reference(rt)))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>> visit(hydra.core.Type.Maybe mt) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.encodeType(
            aliases,
            (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            (mt).value,
            cx,
            g),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>) (jmt -> hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Utils.javaTypeToJavaReferenceType(
              jmt,
              cx),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>right(hydra.util.ConsList.of(new hydra.ext.java.syntax.TypeArgument.Reference(rt)))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>> visit(hydra.core.Type.Map mp) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.encodeType(
            aliases,
            (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            (mp).value.keys,
            cx,
            g),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>) (jkt -> hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Utils.javaTypeToJavaReferenceType(
              jkt,
              cx),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>) (rk -> hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.encodeType(
                aliases,
                (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                (mp).value.values,
                cx,
                g),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>) (jvt -> hydra.lib.eithers.Bind.apply(
                hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                  jvt,
                  cx),
                (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>) (rv -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>right(hydra.util.ConsList.of(
                  new hydra.ext.java.syntax.TypeArgument.Reference(rk),
                  new hydra.ext.java.syntax.TypeArgument.Reference(rv)))))))))));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> encodeTerm(hydra.ext.java.environment.JavaEnvironment env, hydra.core.Term term, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.ext.java.Coder.encodeTermInternal(
      env,
      (hydra.util.ConsList<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>) (hydra.util.ConsList.<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>empty()),
      (hydra.util.ConsList<hydra.ext.java.syntax.Type>) (hydra.util.ConsList.<hydra.ext.java.syntax.Type>empty()),
      term,
      cx,
      g);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.InterfaceMemberDeclaration> encodeTermDefinition(hydra.ext.java.environment.JavaEnvironment env, hydra.module.TermDefinition tdef, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.core.Name name = (tdef).name;
    hydra.core.Term term0 = (tdef).term;
    hydra.core.Term term = hydra.Rewriting.unshadowVariables(term0);
    hydra.util.Lazy<hydra.core.TypeScheme> ts = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> new hydra.core.TypeScheme((hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Unit")), (hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (x -> x),
      (tdef).type));
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.java.Coder.analyzeJavaFunction(
        env,
        term,
        cx,
        g),
      (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (fs -> {
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.ConsList<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
        hydra.util.Lazy<hydra.core.Term> body = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> params = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.ConsList<hydra.core.Name>>) (projected -> projected.params)).apply(fs));
        hydra.util.Lazy<Integer> numParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(params.get()));
        hydra.core.Type schemeType = ts.get().type;
        hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type> peelResult = hydra.ext.java.Coder.peelDomainsAndCod(
          numParams.get(),
          schemeType);
        hydra.util.Lazy<hydra.core.Type> cod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(peelResult));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> doms = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.ConsList<hydra.core.Type>>) (projected -> projected.domains)).apply(fs));
        hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env2 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.ext.java.environment.JavaEnvironment>) (projected -> projected.environment)).apply(fs));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> schemeDoms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(peelResult));
        hydra.util.PersistentSet<hydra.core.Name> schemeTypeVars = hydra.ext.java.Coder.collectTypeVars(ts.get().type);
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> termVars = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.ConsList<hydra.core.Name>>) (projected -> projected.typeParams)).apply(fs));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> schemeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.ext.java.Coder.isSimpleName(v)),
          ts.get().variables));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> usedSchemeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.sets.Member.apply(
            v,
            schemeTypeVars)),
          schemeVars.get()));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> tparams = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(usedSchemeVars.get()),
          () -> termVars.get(),
          () -> usedSchemeVars.get()));
        hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> schemeVarSet = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(tparams.get()));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(tparams.get()),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>right((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()))),
            () -> hydra.ext.java.Coder.buildSubstFromAnnotations(
              schemeVarSet.get(),
              term,
              cx,
              g)),
          (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (typeVarSubst -> {
            hydra.ext.java.environment.Aliases aliases2base = env2.get().aliases;
            hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> overgenSubst = hydra.ext.java.Coder.detectAccumulatorUnification(
              schemeDoms.get(),
              cod.get(),
              tparams.get());
            hydra.util.Lazy<hydra.core.Type> fixedCod = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.maps.Null.apply(overgenSubst),
              () -> cod.get(),
              () -> hydra.ext.java.Coder.substituteTypeVarsWithTypes(
                overgenSubst,
                cod.get())));
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> fixedTparams = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.maps.Null.apply(overgenSubst),
              () -> tparams.get(),
              () -> hydra.lib.lists.Filter.apply(
                (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
                  v,
                  overgenSubst))),
                tparams.get())));
            hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> fixedSchemeVarSet = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(fixedTparams.get()));
            hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> overgenVarSubst = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>>) (entry -> {
                hydra.util.Lazy<hydra.core.Name> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(entry));
                hydra.util.Lazy<hydra.core.Type> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(entry));
                return v.get().accept(new hydra.core.Type.PartialVisitor<>() {
                  @Override
                  public hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                    return (hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>nothing());
                  }

                  @Override
                  public hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable n) {
                    return hydra.util.Maybe.just((hydra.util.Pair<hydra.core.Name, hydra.core.Name>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Name>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Name>(k.get(), (n).value))));
                  }
                });
              }),
              hydra.lib.maps.ToList.apply(overgenSubst)))));
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> fixedDoms = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.maps.Null.apply(overgenSubst),
              () -> schemeDoms.get(),
              () -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (d -> hydra.ext.java.Coder.substituteTypeVarsWithTypes(
                  overgenSubst,
                  d)),
                schemeDoms.get())));
            hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> trustedVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>) (d -> hydra.ext.java.Coder.collectTypeVars(d)),
              hydra.lib.lists.Concat2.apply(
                fixedDoms.get(),
                hydra.util.ConsList.of(fixedCod.get())))));
            hydra.util.Lazy<hydra.ext.java.environment.Aliases> aliases2 = new hydra.util.Lazy<>(() -> new hydra.ext.java.environment.Aliases((aliases2base).currentNamespace, (aliases2base).packages, (aliases2base).branchVars, (aliases2base).recursiveVars, fixedSchemeVarSet.get(), (aliases2base).polymorphicLocals, (aliases2base).inScopeJavaVars, (aliases2base).varRenames, hydra.lib.sets.Union.apply(
              (aliases2base).lambdaVars,
              hydra.lib.sets.FromList.apply(params.get())), hydra.lib.maps.Union.apply(
              overgenVarSubst.get(),
              typeVarSubst), hydra.lib.sets.Intersection.apply(
              trustedVars.get(),
              fixedSchemeVarSet.get()), hydra.util.Maybe.just(fixedCod.get()), (aliases2base).thunkedVars));
            hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
              () -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
              ts.get().constraints));
            hydra.ext.java.environment.JavaEnvironment env2WithTypeParams = new hydra.ext.java.environment.JavaEnvironment(aliases2.get(), env2.get().graph);
            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>> jparams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.TypeParameter>) (v -> hydra.ext.java.Utils.javaTypeParameter(hydra.Formatting.capitalize((v).value))),
              fixedTparams.get()));
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.bindingsToStatements(
                env2WithTypeParams,
                bindings.get(),
                cx,
                g),
              (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (bindResult -> {
                hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> bindingStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
                hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
                return hydra.lib.eithers.Bind.apply(
                  hydra.lib.logic.IfElse.lazy(
                    hydra.lib.maps.Null.apply(overgenSubst),
                    () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(body.get()),
                    () -> hydra.ext.java.Coder.applyOvergenSubstToTermAnnotations(
                      overgenSubst,
                      body.get(),
                      cx,
                      g)),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (body_ -> {
                    hydra.core.Term annotatedBody = hydra.ext.java.Coder.propagateTypesInAppChain(
                      fixedCod.get(),
                      fixedCod.get(),
                      body_);
                    return hydra.lib.eithers.Bind.apply(
                      hydra.lib.eithers.MapList.apply(
                        (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.core.Name>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.FormalParameter>>) (pair -> hydra.lib.eithers.Bind.apply(
                          hydra.ext.java.Coder.encodeType(
                            aliases2.get(),
                            (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                            hydra.lib.pairs.First.apply(pair),
                            cx,
                            g),
                          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.FormalParameter>>) (jdom -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.FormalParameter>right(hydra.ext.java.Utils.javaTypeToJavaFormalParameter(
                            jdom,
                            hydra.lib.pairs.Second.apply(pair)))))),
                        hydra.lib.lists.Zip.apply(
                          fixedDoms.get(),
                          params.get())),
                      (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.FormalParameter>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (jformalParams -> hydra.lib.eithers.Bind.apply(
                        hydra.ext.java.Coder.encodeType(
                          aliases2.get(),
                          (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                          fixedCod.get(),
                          cx,
                          g),
                        (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (jcod -> {
                          Boolean isTCO = false;
                          String jname = hydra.ext.java.Utils.sanitizeJavaName(hydra.Formatting.decapitalize(hydra.Names.localNameOf(name)));
                          hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMethodModifier> mods = hydra.util.ConsList.of(new hydra.ext.java.syntax.InterfaceMethodModifier.Static());
                          hydra.ext.java.syntax.Result result = hydra.ext.java.Utils.javaTypeToJavaResult(jcod);
                          return hydra.lib.eithers.Bind.apply(
                            hydra.lib.logic.IfElse.lazy(
                              isTCO,
                              () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (() -> {
                                String tcoSuffix = "_tco";
                                return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (() -> {
                                  hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> snapshotNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                                    (java.util.function.Function<hydra.core.Name, hydra.core.Name>) (p -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                                      (p).value,
                                      tcoSuffix))),
                                    params.get()));
                                  return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (() -> {
                                    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> tcoVarRenames = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
                                      params.get(),
                                      snapshotNames.get())));
                                    return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (() -> {
                                      hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> snapshotDecls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                                        (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Name>, hydra.ext.java.syntax.BlockStatement>) (pair -> hydra.ext.java.Utils.finalVarDeclarationStatement(
                                          hydra.ext.java.Utils.variableToJavaIdentifier(hydra.lib.pairs.Second.apply(pair)),
                                          hydra.ext.java.Utils.javaIdentifierToJavaExpression(hydra.ext.java.Utils.variableToJavaIdentifier(hydra.lib.pairs.First.apply(pair))))),
                                        hydra.lib.lists.Zip.apply(
                                          params.get(),
                                          snapshotNames.get())));
                                      return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (() -> {
                                        hydra.util.Lazy<hydra.core.Term> tcoBody = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                                          hydra.lib.lists.Null.apply(bindings.get()),
                                          () -> annotatedBody,
                                          () -> new hydra.core.Term.Let(new hydra.core.Let(bindings.get(), annotatedBody))));
                                        return hydra.lib.eithers.Bind.apply(
                                          hydra.ext.java.Coder.encodeTermTCO(
                                            env2WithTypeParams,
                                            name,
                                            params.get(),
                                            tcoVarRenames.get(),
                                            0,
                                            tcoBody.get(),
                                            cx,
                                            g),
                                          (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (tcoStmts -> {
                                            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> whileBodyStmts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                                              snapshotDecls.get(),
                                              tcoStmts));
                                            hydra.ext.java.syntax.Statement whileBodyBlock = new hydra.ext.java.syntax.Statement.WithoutTrailing(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Block(new hydra.ext.java.syntax.Block(whileBodyStmts.get())));
                                            hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> whileStmt = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.BlockStatement.Statement(new hydra.ext.java.syntax.Statement.While(new hydra.ext.java.syntax.WhileStatement(hydra.ext.java.Coder.<hydra.ext.java.syntax.Expression>encodeTermDefinition_noCond(), whileBodyBlock))));
                                            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>right(hydra.util.ConsList.of(whileStmt.get()));
                                          }));
                                      })).get();
                                    })).get();
                                  })).get();
                                })).get();
                              })).get(),
                              () -> hydra.lib.eithers.Bind.apply(
                                hydra.ext.java.Coder.encodeTerm(
                                  env3.get(),
                                  annotatedBody,
                                  cx,
                                  g),
                                (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (jbody -> {
                                  hydra.ext.java.syntax.BlockStatement returnSt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(jbody)));
                                  return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>right(hydra.lib.lists.Concat2.apply(
                                    bindingStmts.get(),
                                    hydra.util.ConsList.of(returnSt)));
                                }))),
                            (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (methodBody -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.InterfaceMemberDeclaration>right(hydra.ext.java.Utils.interfaceMethodDeclaration(
                              mods,
                              jparams.get(),
                              jname,
                              jformalParams,
                              result,
                              hydra.util.Maybe.just(methodBody)))));
                        }))));
                  }));
              }));
          }));
      }));
  }

  static <T0> hydra.util.Maybe<T0> encodeTermDefinition_noCond() {
    return (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> encodeTermInternal(hydra.ext.java.environment.JavaEnvironment env, hydra.util.ConsList<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> anns, hydra.util.ConsList<hydra.ext.java.syntax.Type> tyapps, hydra.core.Term term, hydra.context.Context cx, hydra.graph.Graph g0) {
    hydra.ext.java.environment.Aliases aliases = (env).aliases;
    hydra.graph.Graph g = (env).graph;
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>> encode = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (t -> hydra.ext.java.Coder.encodeTerm(
      env,
      t,
      cx,
      g));
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Coder.encodeLiteral(new hydra.core.Literal.String_("Unimplemented term variant")));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Annotated at) {
        return hydra.ext.java.Coder.encodeTermInternal(
          env,
          hydra.lib.lists.Cons.apply(
            (at).value.annotation,
            anns),
          tyapps,
          (at).value.body,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Application app) {
        return hydra.ext.java.Coder.encodeApplication(
          env,
          (app).value,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Either et) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(tyapps),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>right((hydra.util.Maybe<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>) (hydra.util.Maybe.<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>nothing())),
            () -> hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.takeTypeArgs(
                "either",
                2,
                tyapps,
                cx,
                g),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>>) (ta -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>right(hydra.util.Maybe.just(ta))))),
          (java.util.function.Function<hydra.util.Maybe<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (mtargs -> {
            hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> combinedAnns = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>>) (acc -> (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>) (m -> hydra.lib.maps.Union.apply(
                acc,
                m))),
              (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())),
              anns));
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.Bimap.apply(
                (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
                (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
                hydra.Annotations.getType(
                  g,
                  combinedAnns.get())),
              (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (mEitherType -> {
                hydra.util.Lazy<hydra.util.Maybe<hydra.util.Pair<hydra.core.Type, hydra.core.Type>>> branchTypes = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Bind.apply(
                  mEitherType,
                  (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.util.Pair<hydra.core.Type, hydra.core.Type>>>) (etyp -> hydra.Rewriting.deannotateType(etyp).accept(new hydra.core.Type.PartialVisitor<>() {
                    @Override
                    public hydra.util.Maybe<hydra.util.Pair<hydra.core.Type, hydra.core.Type>> otherwise(hydra.core.Type instance) {
                      return (hydra.util.Maybe<hydra.util.Pair<hydra.core.Type, hydra.core.Type>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.Type, hydra.core.Type>>nothing());
                    }

                    @Override
                    public hydra.util.Maybe<hydra.util.Pair<hydra.core.Type, hydra.core.Type>> visit(hydra.core.Type.Either et2) {
                      return hydra.util.Maybe.just((hydra.util.Pair<hydra.core.Type, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Type, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Type, hydra.core.Type>((et2).value.left, (et2).value.right))));
                    }
                  }))));
                java.util.function.Function<String, java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression>> eitherCall = (java.util.function.Function<String, java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression>>) (methodName -> (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression>) (expr -> hydra.lib.maybes.Cases.applyLazy(
                  mtargs,
                  () -> hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStatic(
                    new hydra.ext.java.syntax.Identifier("hydra.util.Either"),
                    new hydra.ext.java.syntax.Identifier(methodName),
                    hydra.util.ConsList.of(expr))),
                  (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>, hydra.ext.java.syntax.Expression>) (targs -> hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStaticWithTypeArgs(
                    new hydra.ext.java.syntax.Identifier("hydra.util.Either"),
                    new hydra.ext.java.syntax.Identifier(methodName),
                    targs,
                    hydra.util.ConsList.of(expr)))))));
                java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>> encodeWithType = (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>>) (branchType -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (t1 -> {
                  hydra.core.Term annotated = hydra.Annotations.setTermAnnotation(
                    hydra.Constants.key_type(),
                    hydra.util.Maybe.just(hydra.encode.Core.type(branchType)),
                    t1);
                  return hydra.ext.java.Coder.encodeTermInternal(
                    env,
                    anns,
                    (hydra.util.ConsList<hydra.ext.java.syntax.Type>) (hydra.util.ConsList.<hydra.ext.java.syntax.Type>empty()),
                    annotated,
                    cx,
                    g);
                }));
                return hydra.lib.eithers.Either.apply(
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (term1 -> hydra.lib.eithers.Bind.apply(
                    hydra.lib.maybes.Cases.applyLazy(
                      branchTypes.get(),
                      () -> (encode).apply(term1),
                      (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (bt -> (encodeWithType).apply(hydra.lib.pairs.First.apply(bt)).apply(term1))),
                    (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right((eitherCall).apply("left").apply(expr))))),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (term1 -> hydra.lib.eithers.Bind.apply(
                    hydra.lib.maybes.Cases.applyLazy(
                      branchTypes.get(),
                      () -> (encode).apply(term1),
                      (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (bt -> (encodeWithType).apply(hydra.lib.pairs.Second.apply(bt)).apply(term1))),
                    (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right((eitherCall).apply("right").apply(expr))))),
                  (et).value);
              }));
          }));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Function f) {
        hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> combinedAnns = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>>) (acc -> (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>) (m -> hydra.lib.maps.Union.apply(
            acc,
            m))),
          (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())),
          anns));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
            (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
            hydra.Annotations.getType(
              g,
              combinedAnns.get())),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (mt -> hydra.lib.eithers.Bind.apply(
            hydra.lib.maybes.Cases.applyLazy(
              mt,
              () -> hydra.lib.maybes.Cases.applyLazy(
                hydra.ext.java.Coder.tryInferFunctionType((f).value),
                () -> hydra.CoderUtils.typeOfTerm(
                  cx,
                  g,
                  term),
                (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (inferredType -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right(inferredType))),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (t -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right(t))),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (typ -> hydra.Rewriting.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Type instance) {
                return hydra.ext.java.Coder.encodeNullaryConstant(
                  env,
                  typ,
                  (f).value,
                  cx,
                  g);
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Type.Function ft) {
                return hydra.ext.java.Coder.encodeFunction(
                  env,
                  (ft).value.domain,
                  (ft).value.codomain,
                  (f).value,
                  cx,
                  g);
              }
            })))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Let lt) {
        hydra.util.ConsList<hydra.core.Binding> bindings = (lt).value.bindings;
        hydra.core.Term body = (lt).value.body;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(bindings),
          () -> hydra.ext.java.Coder.encodeTermInternal(
            env,
            anns,
            (hydra.util.ConsList<hydra.ext.java.syntax.Type>) (hydra.util.ConsList.<hydra.ext.java.syntax.Type>empty()),
            body,
            cx,
            g),
          () -> hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Coder.bindingsToStatements(
              env,
              bindings,
              cx,
              g),
            (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (bindResult -> {
              hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> bindingStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
              hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
              return hydra.lib.eithers.Bind.apply(
                hydra.ext.java.Coder.encodeTermInternal(
                  env2.get(),
                  anns,
                  (hydra.util.ConsList<hydra.ext.java.syntax.Type>) (hydra.util.ConsList.<hydra.ext.java.syntax.Type>empty()),
                  body,
                  cx,
                  g),
                (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jbody -> {
                  hydra.ext.java.environment.Aliases aliases2 = env2.get().aliases;
                  hydra.ext.java.syntax.BlockStatement returnSt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(jbody)));
                  hydra.util.Lazy<hydra.ext.java.syntax.Block> block = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Block(hydra.lib.lists.Concat2.apply(
                    bindingStmts.get(),
                    hydra.util.ConsList.of(returnSt))));
                  hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> combinedAnns = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                    (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>>) (acc -> (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>) (m -> hydra.lib.maps.Union.apply(
                      acc,
                      m))),
                    (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())),
                    anns));
                  hydra.graph.Graph g2 = env2.get().graph;
                  hydra.util.Lazy<hydra.ext.java.syntax.Expression> nullaryLambda = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Expression.Lambda(new hydra.ext.java.syntax.LambdaExpression(new hydra.ext.java.syntax.LambdaParameters.Tuple((hydra.util.ConsList<hydra.ext.java.syntax.LambdaParameters>) (hydra.util.ConsList.<hydra.ext.java.syntax.LambdaParameters>empty())), new hydra.ext.java.syntax.LambdaBody.Block(block.get()))));
                  return hydra.lib.eithers.Bind.apply(
                    hydra.lib.eithers.Bimap.apply(
                      (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
                      (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
                      hydra.Annotations.getType(
                        g,
                        combinedAnns.get())),
                    (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (mt -> hydra.lib.eithers.Bind.apply(
                      hydra.lib.maybes.Cases.applyLazy(
                        mt,
                        () -> hydra.CoderUtils.typeOfTerm(
                          cx,
                          g2,
                          body),
                        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (t -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right(t))),
                      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (letType -> hydra.lib.eithers.Bind.apply(
                        hydra.ext.java.Coder.encodeType(
                          aliases2,
                          (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                          letType,
                          cx,
                          g),
                        (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jLetType -> hydra.lib.eithers.Bind.apply(
                          hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                            jLetType,
                            cx),
                          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (rt -> {
                            hydra.ext.java.syntax.ReferenceType supplierRt = new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(hydra.ext.java.Utils.javaClassType(
                              hydra.util.ConsList.of(rt),
                              hydra.ext.java.Names.javaUtilFunctionPackageName(),
                              "Supplier")));
                            hydra.ext.java.syntax.Expression castExpr = hydra.ext.java.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.Utils.javaCastExpression(
                              supplierRt,
                              hydra.ext.java.Utils.javaExpressionToJavaUnaryExpression(nullaryLambda.get())));
                            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocation(
                              hydra.util.Maybe.just(hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>right(hydra.ext.java.Utils.javaExpressionToJavaPrimary(castExpr))),
                              new hydra.ext.java.syntax.Identifier("get"),
                              (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()))));
                          }))))))));
                }));
            })));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.List els) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply((els).value),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(tyapps),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStatic(
              new hydra.ext.java.syntax.Identifier("hydra.util.ConsList"),
              new hydra.ext.java.syntax.Identifier("empty"),
              (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty())))),
            () -> hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.takeTypeArgs(
                "list",
                1,
                tyapps,
                cx,
                g),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (targs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStaticWithTypeArgs(
                new hydra.ext.java.syntax.Identifier("hydra.util.ConsList"),
                new hydra.ext.java.syntax.Identifier("empty"),
                targs,
                (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()))))))),
          () -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              encode,
              (els).value),
            (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jels -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStatic(
              new hydra.ext.java.syntax.Identifier("hydra.util.ConsList"),
              new hydra.ext.java.syntax.Identifier("of"),
              jels))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Literal l) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Coder.encodeLiteral((l).value));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Map m) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.maps.Null.apply((m).value),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(tyapps),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStatic(
              new hydra.ext.java.syntax.Identifier("hydra.util.PersistentMap"),
              new hydra.ext.java.syntax.Identifier("empty"),
              (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty())))),
            () -> hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.takeTypeArgs(
                "map",
                2,
                tyapps,
                cx,
                g),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (targs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStaticWithTypeArgs(
                new hydra.ext.java.syntax.Identifier("hydra.util.PersistentMap"),
                new hydra.ext.java.syntax.Identifier("empty"),
                targs,
                (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()))))))),
          () -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              encode,
              hydra.lib.maps.Keys.apply((m).value)),
            (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jkeys -> hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.MapList.apply(
                encode,
                hydra.lib.maps.Elems.apply((m).value)),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jvals -> {
                hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.Expression>> pairExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.util.Pair<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression>, hydra.ext.java.syntax.Expression>) (kv -> hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStatic(
                    new hydra.ext.java.syntax.Identifier("hydra.util.PersistentMap"),
                    new hydra.ext.java.syntax.Identifier("entry"),
                    hydra.util.ConsList.of(
                      hydra.lib.pairs.First.apply(kv),
                      hydra.lib.pairs.Second.apply(kv))))),
                  hydra.lib.lists.Zip.apply(
                    jkeys,
                    jvals)));
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStatic(
                  new hydra.ext.java.syntax.Identifier("hydra.util.PersistentMap"),
                  new hydra.ext.java.syntax.Identifier("ofEntries"),
                  pairExprs.get())));
              })))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Cases.applyLazy(
          (mt).value,
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(tyapps),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStatic(
              new hydra.ext.java.syntax.Identifier("hydra.util.Maybe"),
              new hydra.ext.java.syntax.Identifier("nothing"),
              (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty())))),
            () -> hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.takeTypeArgs(
                "maybe",
                1,
                tyapps,
                cx,
                g),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (targs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStaticWithTypeArgs(
                new hydra.ext.java.syntax.Identifier("hydra.util.Maybe"),
                new hydra.ext.java.syntax.Identifier("nothing"),
                targs,
                (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()))))))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (term1 -> hydra.lib.eithers.Bind.apply(
            (encode).apply(term1),
            (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStatic(
              new hydra.ext.java.syntax.Identifier("hydra.util.Maybe"),
              new hydra.ext.java.syntax.Identifier("just"),
              hydra.util.ConsList.of(expr))))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Pair p) {
        return hydra.lib.eithers.Bind.apply(
          (encode).apply(hydra.lib.pairs.First.apply((p).value)),
          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jterm1 -> hydra.lib.eithers.Bind.apply(
            (encode).apply(hydra.lib.pairs.Second.apply((p).value)),
            (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jterm2 -> hydra.lib.eithers.Bind.apply(
              hydra.lib.logic.IfElse.lazy(
                hydra.lib.lists.Null.apply(tyapps),
                () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>right((hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
                () -> hydra.lib.eithers.Bind.apply(
                  hydra.lib.eithers.MapList.apply(
                    (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                      jt,
                      cx)),
                    tyapps),
                  (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ReferenceType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>>) (rts -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>right(hydra.util.Maybe.just(new hydra.ext.java.syntax.TypeArgumentsOrDiamond.Arguments(hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.TypeArgument>) (rt -> new hydra.ext.java.syntax.TypeArgument.Reference(rt)),
                    rts))))))),
              (java.util.function.Function<hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (mtargs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaConstructorCall(
                hydra.ext.java.Utils.javaConstructorName(
                  new hydra.ext.java.syntax.Identifier("hydra.util.Pair"),
                  mtargs),
                hydra.util.ConsList.of(
                  jterm1,
                  jterm2),
                (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())))))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Record rec) {
        hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> combinedAnnsRec = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>>) (acc -> (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>) (m -> hydra.lib.maps.Union.apply(
            acc,
            m))),
          (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())),
          anns));
        hydra.core.Name recName = (rec).value.typeName;
        hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> mRecordType = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.Type>>) (ignored -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
          (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (t -> hydra.util.Maybe.just(t)),
          hydra.Schemas.requireType(
            cx,
            g,
            recName)));
        hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> strippedRecTyp = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (recTyp -> hydra.ext.java.Coder.stripForalls(hydra.Rewriting.deannotateType(recTyp))),
          mRecordType.get()));
        hydra.util.Lazy<hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>> mFieldTypeMap = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Bind.apply(
          strippedRecTyp.get(),
          (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>>) (bodyTyp -> (bodyTyp).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> otherwise(hydra.core.Type instance) {
              return (hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>) (hydra.util.Maybe.<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>nothing());
            }

            @Override
            public hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> visit(hydra.core.Type.Record rt) {
              return hydra.util.Maybe.just(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (ft -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>((ft).name, (ft).type)))),
                (rt).value)));
            }
          }))));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
            (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
            hydra.Annotations.getType(
              g,
              combinedAnnsRec.get())),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (mAnnotType -> {
            hydra.util.Lazy<hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>> mTypeSubst = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Bind.apply(
              mAnnotType,
              (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>>) (annTyp -> hydra.lib.maybes.Bind.apply(
                mRecordType.get(),
                (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>>) (recTyp -> {
                  hydra.util.ConsList<hydra.core.Type> args = hydra.ext.java.Coder.extractTypeApplicationArgs(hydra.Rewriting.deannotateType(annTyp));
                  hydra.util.ConsList<hydra.core.Name> params = hydra.ext.java.Coder.collectForallParams(hydra.Rewriting.deannotateType(recTyp));
                  return hydra.lib.logic.IfElse.lazy(
                    hydra.lib.logic.Or.apply(
                      hydra.lib.lists.Null.apply(args),
                      hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
                        hydra.lib.lists.Length.apply(args),
                        hydra.lib.lists.Length.apply(params)))),
                    () -> (hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>) (hydra.util.Maybe.<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>nothing()),
                    () -> hydra.util.Maybe.just(hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
                      params,
                      args))));
                })))));
            java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>> encodeField = (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (fld -> hydra.lib.maybes.Cases.applyLazy(
              mFieldTypeMap.get(),
              () -> (encode).apply((fld).term),
              (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (ftmap -> {
                hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> mftyp = new hydra.util.Lazy<>(() -> hydra.lib.maps.Lookup.apply(
                  (fld).name,
                  ftmap));
                return hydra.lib.maybes.Cases.applyLazy(
                  mftyp.get(),
                  () -> (encode).apply((fld).term),
                  (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (ftyp -> {
                    hydra.util.Lazy<hydra.core.Type> resolvedType = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
                      mTypeSubst.get(),
                      () -> ftyp,
                      (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>, hydra.core.Type>) (subst -> hydra.ext.java.Coder.applySubstFull(
                        subst,
                        ftyp))));
                    hydra.core.Term annotatedFieldTerm = hydra.Annotations.setTermAnnotation(
                      hydra.Constants.key_type(),
                      hydra.util.Maybe.just(hydra.encode.Core.type(resolvedType.get())),
                      (fld).term);
                    return hydra.ext.java.Coder.encodeTermInternal(
                      env,
                      anns,
                      (hydra.util.ConsList<hydra.ext.java.syntax.Type>) (hydra.util.ConsList.<hydra.ext.java.syntax.Type>empty()),
                      annotatedFieldTerm,
                      cx,
                      g);
                  }));
              })));
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.MapList.apply(
                encodeField,
                (rec).value.fields),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (fieldExprs -> {
                hydra.ext.java.syntax.Identifier consId = hydra.ext.java.Utils.nameToJavaName(
                  aliases,
                  recName);
                return hydra.lib.eithers.Bind.apply(
                  hydra.lib.logic.IfElse.lazy(
                    hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(tyapps)),
                    () -> hydra.lib.eithers.Bind.apply(
                      hydra.lib.eithers.MapList.apply(
                        (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                          jt,
                          cx)),
                        tyapps),
                      (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ReferenceType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>>) (rts -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>right(hydra.util.Maybe.just(new hydra.ext.java.syntax.TypeArgumentsOrDiamond.Arguments(hydra.lib.lists.Map.apply(
                        (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.TypeArgument>) (rt -> new hydra.ext.java.syntax.TypeArgument.Reference(rt)),
                        rts)))))),
                    () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>>) (() -> {
                      hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> combinedAnns = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                        (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>>) (acc -> (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>) (m -> hydra.lib.maps.Union.apply(
                          acc,
                          m))),
                        (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())),
                        anns));
                      return hydra.lib.eithers.Bind.apply(
                        hydra.lib.eithers.Bimap.apply(
                          (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
                          (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
                          hydra.Annotations.getType(
                            g,
                            combinedAnns.get())),
                        (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>>) (mtyp -> hydra.lib.maybes.Cases.applyLazy(
                          mtyp,
                          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>right((hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
                          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>>) (annTyp -> {
                            hydra.util.ConsList<hydra.core.Type> typeArgs = hydra.ext.java.Coder.extractTypeApplicationArgs(hydra.Rewriting.deannotateType(annTyp));
                            return hydra.lib.logic.IfElse.lazy(
                              hydra.lib.lists.Null.apply(typeArgs),
                              () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>right((hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
                              () -> hydra.lib.eithers.Bind.apply(
                                hydra.lib.eithers.MapList.apply(
                                  (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (t -> hydra.lib.eithers.Bind.apply(
                                    hydra.ext.java.Coder.encodeType(
                                      aliases,
                                      (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                                      t,
                                      cx,
                                      g),
                                    (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                                      jt,
                                      cx)))),
                                  typeArgs),
                                (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ReferenceType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>>) (jTypeArgs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>>right(hydra.util.Maybe.just(new hydra.ext.java.syntax.TypeArgumentsOrDiamond.Arguments(hydra.lib.lists.Map.apply(
                                  (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.TypeArgument>) (rt -> new hydra.ext.java.syntax.TypeArgument.Reference(rt)),
                                  jTypeArgs)))))));
                          }))));
                    })).get()),
                  (java.util.function.Function<hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (mtargs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaConstructorCall(
                    hydra.ext.java.Utils.javaConstructorName(
                      consId,
                      mtargs),
                    fieldExprs,
                    (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())))));
              }));
          }));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Set s) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Null.apply((s).value),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(tyapps),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStatic(
              new hydra.ext.java.syntax.Identifier("hydra.util.PersistentSet"),
              new hydra.ext.java.syntax.Identifier("empty"),
              (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty())))),
            () -> hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.takeTypeArgs(
                "set",
                1,
                tyapps,
                cx,
                g),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (targs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStaticWithTypeArgs(
                new hydra.ext.java.syntax.Identifier("hydra.util.PersistentSet"),
                new hydra.ext.java.syntax.Identifier("empty"),
                targs,
                (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()))))))),
          () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> slist = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply((s).value));
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.MapList.apply(
                encode,
                slist.get()),
              (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jels -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStatic(
                new hydra.ext.java.syntax.Identifier("hydra.util.PersistentSet"),
                new hydra.ext.java.syntax.Identifier("of"),
                jels)))));
          })).get());
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.ext.java.Coder.withTypeLambda(
          env,
          (tl).value,
          (java.util.function.Function<hydra.ext.java.environment.JavaEnvironment, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (env2 -> {
            hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> combinedAnns = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>>) (acc -> (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>) (m -> hydra.lib.maps.Union.apply(
                acc,
                m))),
              (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())),
              anns));
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.Bimap.apply(
                (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
                (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
                hydra.Annotations.getType(
                  g,
                  combinedAnns.get())),
              (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (mtyp -> {
                hydra.util.Lazy<hydra.core.Term> annotatedBody = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cases.applyLazy(
                  mtyp,
                  () -> (tl).value.body,
                  (java.util.function.Function<hydra.core.Type, hydra.core.Term>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
                    @Override
                    public hydra.core.Term otherwise(hydra.core.Type instance) {
                      return (tl).value.body;
                    }

                    @Override
                    public hydra.core.Term visit(hydra.core.Type.Forall fa) {
                      return hydra.Annotations.setTermAnnotation(
                        hydra.Constants.key_type(),
                        hydra.util.Maybe.just(hydra.encode.Core.type((fa).value.body)),
                        (tl).value.body);
                    }
                  }))));
                return hydra.ext.java.Coder.encodeTerm(
                  env2,
                  annotatedBody.get(),
                  cx,
                  g);
              }));
          }));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Union inj) {
        hydra.core.Field injField = (inj).value.field;
        hydra.core.Name injFieldName = (injField).name;
        hydra.core.Name injTypeName = (inj).value.typeName;
        String typeId = hydra.ext.java.Utils.nameToJavaName(
          aliases,
          injTypeName).value;
        hydra.ext.java.syntax.Identifier consId = new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          typeId,
          ".",
          hydra.ext.java.Utils.sanitizeJavaName(hydra.Formatting.capitalize((injFieldName).value)))));
        hydra.core.Term injFieldTerm = (injField).term;
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.isFieldUnitType(
            injTypeName,
            injFieldName,
            cx,
            g),
          (java.util.function.Function<Boolean, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (fieldIsUnit -> hydra.lib.eithers.Bind.apply(
            hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Or.apply(
                hydra.Schemas.isUnitTerm(hydra.Rewriting.deannotateTerm(injFieldTerm)),
                fieldIsUnit),
              () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.Expression>>right((hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty())),
              () -> hydra.lib.eithers.Bind.apply(
                (encode).apply(injFieldTerm),
                (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.Expression>>>) (ex -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.Expression>>right(hydra.util.ConsList.of(ex))))),
            (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (args -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaConstructorCall(
              hydra.ext.java.Utils.javaConstructorName(
                consId,
                (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
              args,
              (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Variable name) {
        return hydra.ext.java.Coder.encodeVariable(
          env,
          (name).value,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Unit ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaLiteralToJavaExpression(new hydra.ext.java.syntax.Literal.Null()));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Wrap wt) {
        return hydra.lib.eithers.Bind.apply(
          (encode).apply((wt).value.body),
          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jarg -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaConstructorCall(
            hydra.ext.java.Utils.javaConstructorName(
              hydra.ext.java.Utils.nameToJavaName(
                aliases,
                (wt).value.typeName),
              (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
            hydra.util.ConsList.of(jarg),
            (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Type atyp = (ta).value.type;
        hydra.core.Term body = (ta).value.body;
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.encodeType(
            aliases,
            (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            atyp,
            cx,
            g),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jatyp -> {
            hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> combinedAnns = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>>) (acc -> (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>) (m -> hydra.lib.maps.Union.apply(
                acc,
                m))),
              (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())),
              anns));
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.Bimap.apply(
                (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
                (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
                hydra.Annotations.getType(
                  g,
                  combinedAnns.get())),
              (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (mtyp -> hydra.lib.eithers.Bind.apply(
                hydra.lib.maybes.Cases.applyLazy(
                  mtyp,
                  () -> hydra.CoderUtils.typeOfTerm(
                    cx,
                    g,
                    term),
                  (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (t -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right(t))),
                (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (typ -> {
                  hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>> collected0 = hydra.ext.java.Coder.collectTypeApps0(
                    body,
                    hydra.util.ConsList.of(atyp));
                  hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> allTypeArgs0 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(collected0));
                  hydra.util.Lazy<hydra.core.Term> innermostBody0 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(collected0));
                  return hydra.lib.eithers.Bind.apply(
                    hydra.ext.java.Coder.correctCastType(
                      innermostBody0.get(),
                      allTypeArgs0.get(),
                      typ,
                      cx,
                      g),
                    (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (correctedTyp -> {
                      hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Type>> collected = hydra.ext.java.Coder.collectTypeApps(
                        body,
                        hydra.util.ConsList.of(atyp));
                      hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> allTypeArgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(collected));
                      hydra.util.Lazy<hydra.core.Term> innermostBody = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(collected));
                      return innermostBody.get().accept(new hydra.core.Term.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.core.Term instance) {
                          return hydra.ext.java.Coder.typeAppFallbackCast(
                            env,
                            aliases,
                            anns,
                            tyapps,
                            jatyp,
                            body,
                            correctedTyp,
                            cx,
                            g);
                        }

                        @Override
                        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Variable varName) {
                          return hydra.lib.eithers.Bind.apply(
                            hydra.ext.java.Coder.classifyDataReference(
                              (varName).value,
                              cx,
                              g),
                            (java.util.function.Function<hydra.ext.java.environment.JavaSymbolClass, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (cls -> hydra.ext.java.Coder.typeAppNullaryOrHoisted(
                              env,
                              aliases,
                              anns,
                              tyapps,
                              jatyp,
                              body,
                              correctedTyp,
                              (varName).value,
                              cls,
                              allTypeArgs.get(),
                              cx,
                              g)));
                        }

                        @Override
                        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.core.Term.Either eitherTerm) {
                          return hydra.lib.logic.IfElse.lazy(
                            hydra.lib.equality.Equal.apply(
                              hydra.lib.lists.Length.apply(allTypeArgs.get()),
                              2),
                            () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                              hydra.util.Lazy<hydra.util.Pair<hydra.core.Type, hydra.core.Type>> eitherBranchTypes = new hydra.util.Lazy<>(() -> (hydra.util.Pair<hydra.core.Type, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Type, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Type, hydra.core.Type>(hydra.lib.lists.Head.apply(allTypeArgs.get()), hydra.lib.lists.Head.apply(hydra.lib.lists.Tail.apply(allTypeArgs.get()))))));
                              return hydra.lib.eithers.Bind.apply(
                                hydra.lib.eithers.MapList.apply(
                                  (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (t -> hydra.lib.eithers.Bind.apply(
                                    hydra.ext.java.Coder.encodeType(
                                      aliases,
                                      (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                                      t,
                                      cx,
                                      g),
                                    (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                                      jt,
                                      cx)))),
                                  allTypeArgs.get()),
                                (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.ReferenceType>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jTypeArgs -> {
                                  hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>> eitherTargs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                                    (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.TypeArgument>) (rt -> new hydra.ext.java.syntax.TypeArgument.Reference(rt)),
                                    jTypeArgs));
                                  java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>> encodeEitherBranch = (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>>) (branchType -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (t1 -> {
                                    hydra.core.Term annotated = hydra.Annotations.setTermAnnotation(
                                      hydra.Constants.key_type(),
                                      hydra.util.Maybe.just(hydra.encode.Core.type(branchType)),
                                      t1);
                                    return hydra.ext.java.Coder.encodeTermInternal(
                                      env,
                                      anns,
                                      (hydra.util.ConsList<hydra.ext.java.syntax.Type>) (hydra.util.ConsList.<hydra.ext.java.syntax.Type>empty()),
                                      annotated,
                                      cx,
                                      g);
                                  }));
                                  return hydra.lib.eithers.Either.apply(
                                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (term1 -> hydra.lib.eithers.Bind.apply(
                                      (encodeEitherBranch).apply(hydra.lib.pairs.First.apply(eitherBranchTypes.get())).apply(term1),
                                      (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStaticWithTypeArgs(
                                        new hydra.ext.java.syntax.Identifier("hydra.util.Either"),
                                        new hydra.ext.java.syntax.Identifier("left"),
                                        eitherTargs.get(),
                                        hydra.util.ConsList.of(expr))))))),
                                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (term1 -> hydra.lib.eithers.Bind.apply(
                                      (encodeEitherBranch).apply(hydra.lib.pairs.Second.apply(eitherBranchTypes.get())).apply(term1),
                                      (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStaticWithTypeArgs(
                                        new hydra.ext.java.syntax.Identifier("hydra.util.Either"),
                                        new hydra.ext.java.syntax.Identifier("right"),
                                        eitherTargs.get(),
                                        hydra.util.ConsList.of(expr))))))),
                                    (eitherTerm).value);
                                }));
                            })).get(),
                            () -> hydra.ext.java.Coder.typeAppFallbackCast(
                              env,
                              aliases,
                              anns,
                              tyapps,
                              jatyp,
                              body,
                              correctedTyp,
                              cx,
                              g));
                        }
                      });
                    }));
                }))));
          }));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> encodeTermTCO(hydra.ext.java.environment.JavaEnvironment env0, hydra.core.Name funcName, hydra.util.ConsList<hydra.core.Name> paramNames, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> tcoVarRenames, Integer tcoDepth, hydra.core.Term term, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.ext.java.environment.Aliases aliases0 = (env0).aliases;
    hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env = new hydra.util.Lazy<>(() -> new hydra.ext.java.environment.JavaEnvironment(new hydra.ext.java.environment.Aliases((aliases0).currentNamespace, (aliases0).packages, (aliases0).branchVars, (aliases0).recursiveVars, (aliases0).inScopeTypeParams, (aliases0).polymorphicLocals, (aliases0).inScopeJavaVars, hydra.lib.maps.Union.apply(
      tcoVarRenames,
      (aliases0).varRenames), (aliases0).lambdaVars, (aliases0).typeVarSubst, (aliases0).trustedTypeVars, (aliases0).methodCodomain, (aliases0).thunkedVars), (env0).graph));
    hydra.core.Term stripped = hydra.Rewriting.deannotateAndDetypeTerm(term);
    hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term> gathered = hydra.CoderUtils.gatherApplications(stripped);
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> gatherArgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered));
    hydra.util.Lazy<hydra.core.Term> gatherFun = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered));
    hydra.core.Term strippedFun = hydra.Rewriting.deannotateAndDetypeTerm(gatherFun.get());
    hydra.util.Lazy<Boolean> isSelfCall = new hydra.util.Lazy<>(() -> (strippedFun).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Term.Variable n) {
        return hydra.lib.equality.Equal.apply(
          (n).value,
          funcName);
      }
    }));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        isSelfCall.get(),
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(gatherArgs.get()),
          hydra.lib.lists.Length.apply(paramNames))),
      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (() -> {
        hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>> changePairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, Boolean>) (pair -> hydra.lib.logic.Not.apply(hydra.Rewriting.deannotateAndDetypeTerm(hydra.lib.pairs.Second.apply(pair)).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public Boolean otherwise(hydra.core.Term instance) {
              return false;
            }

            @Override
            public Boolean visit(hydra.core.Term.Variable n) {
              return hydra.lib.equality.Equal.apply(
                (n).value,
                hydra.lib.pairs.First.apply(pair));
            }
          }))),
          hydra.lib.lists.Zip.apply(
            paramNames,
            gatherArgs.get())));
        return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (() -> {
          hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> changedParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, hydra.core.Name>) ((java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, hydra.core.Name>) (hydra.lib.pairs.First::apply)),
            changePairs.get()));
          return hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (pair -> hydra.ext.java.Coder.encodeTerm(
                env.get(),
                hydra.lib.pairs.Second.apply(pair),
                cx,
                g)),
              changePairs.get()),
            (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (jChangedArgs -> {
              hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> assignments = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.Expression>, hydra.ext.java.syntax.BlockStatement>) (pair -> {
                  hydra.util.Lazy<hydra.ext.java.syntax.Expression> jArg = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
                  hydra.util.Lazy<hydra.core.Name> paramName = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
                  return new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaAssignmentStatement(
                    new hydra.ext.java.syntax.LeftHandSide.ExpressionName(hydra.ext.java.Utils.javaIdentifierToJavaExpressionName(hydra.ext.java.Utils.variableToJavaIdentifier(paramName.get()))),
                    jArg.get()));
                }),
                hydra.lib.lists.Zip.apply(
                  changedParams.get(),
                  jChangedArgs)));
              hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> continueStmt = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.BlockStatement.Statement(new hydra.ext.java.syntax.Statement.WithoutTrailing(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Continue(new hydra.ext.java.syntax.ContinueStatement((hydra.util.Maybe<hydra.ext.java.syntax.Identifier>) (hydra.util.Maybe.<hydra.ext.java.syntax.Identifier>nothing()))))));
              return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>right(hydra.lib.lists.Concat2.apply(
                assignments.get(),
                hydra.util.ConsList.of(continueStmt.get())));
            }));
        })).get();
      })).get(),
      () -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> otherwise(hydra.core.Term instance) {
          hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term> gathered2 = hydra.CoderUtils.gatherApplications(term);
          hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> args2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered2));
          hydra.util.Lazy<hydra.core.Term> body2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered2));
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              hydra.lib.lists.Length.apply(args2.get()),
              1),
            () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (() -> {
              hydra.util.Lazy<hydra.core.Term> arg = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(args2.get()));
              return hydra.Rewriting.deannotateAndDetypeTerm(body2.get()).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> otherwise(hydra.core.Term instance) {
                  return hydra.lib.eithers.Bind.apply(
                    hydra.ext.java.Coder.encodeTerm(
                      env.get(),
                      term,
                      cx,
                      g),
                    (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>right(hydra.util.ConsList.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(expr)))))));
                }

                @Override
                public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> visit(hydra.core.Term.Function f) {
                  return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> otherwise(hydra.core.Function instance) {
                      return hydra.lib.eithers.Bind.apply(
                        hydra.ext.java.Coder.encodeTerm(
                          env.get(),
                          term,
                          cx,
                          g),
                        (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>right(hydra.util.ConsList.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(expr)))))));
                    }

                    @Override
                    public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> visit(hydra.core.Function.Elimination e) {
                      return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> otherwise(hydra.core.Elimination instance) {
                          return hydra.lib.eithers.Bind.apply(
                            hydra.ext.java.Coder.encodeTerm(
                              env.get(),
                              term,
                              cx,
                              g),
                            (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>right(hydra.util.ConsList.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(expr)))))));
                        }

                        @Override
                        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> visit(hydra.core.Elimination.Union cs) {
                          hydra.ext.java.environment.Aliases aliases = env.get().aliases;
                          hydra.util.ConsList<hydra.core.Field> cases_ = (cs).value.cases;
                          hydra.util.Maybe<hydra.core.Term> dflt = (cs).value.default_;
                          hydra.core.Name tname = (cs).value.typeName;
                          return hydra.lib.eithers.Bind.apply(
                            hydra.ext.java.Coder.domTypeArgs(
                              aliases,
                              hydra.Schemas.nominalApplication(
                                tname,
                                (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())),
                              cx,
                              g),
                            (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (domArgs -> hydra.lib.eithers.Bind.apply(
                              hydra.ext.java.Coder.encodeTerm(
                                env.get(),
                                arg.get(),
                                cx,
                                g),
                              (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (jArgRaw -> {
                                hydra.util.Lazy<String> depthSuffix = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                                  hydra.lib.equality.Equal.apply(
                                    tcoDepth,
                                    0),
                                  () -> "",
                                  () -> hydra.lib.literals.ShowInt32.apply(tcoDepth)));
                                hydra.ext.java.syntax.Identifier matchVarId = hydra.ext.java.Utils.javaIdentifier(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
                                  "_tco_match_",
                                  hydra.Formatting.decapitalize(hydra.Names.localNameOf(tname)),
                                  depthSuffix.get())));
                                hydra.ext.java.syntax.Expression jArg = hydra.ext.java.Utils.javaIdentifierToJavaExpression(matchVarId);
                                hydra.ext.java.syntax.BlockStatement matchDecl = hydra.ext.java.Utils.varDeclarationStatement(
                                  matchVarId,
                                  jArgRaw);
                                return hydra.lib.eithers.Bind.apply(
                                  hydra.lib.eithers.MapList.apply(
                                    (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>>) (field -> {
                                      hydra.core.Name fieldName = (field).name;
                                      hydra.ext.java.syntax.ReferenceType variantRefType = hydra.ext.java.Utils.nameToJavaReferenceType(
                                        aliases,
                                        true,
                                        domArgs,
                                        tname,
                                        hydra.util.Maybe.just(hydra.Formatting.capitalize((fieldName).value)));
                                      return hydra.Rewriting.deannotateTerm((field).term).accept(new hydra.core.Term.PartialVisitor<>() {
                                        @Override
                                        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement> otherwise(hydra.core.Term instance) {
                                          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("TCO: case branch is not a lambda")), cx)));
                                        }

                                        @Override
                                        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement> visit(hydra.core.Term.Function f2) {
                                          return (f2).value.accept(new hydra.core.Function.PartialVisitor<>() {
                                            @Override
                                            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement> otherwise(hydra.core.Function instance) {
                                              return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("TCO: case branch is not a lambda")), cx)));
                                            }

                                            @Override
                                            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement> visit(hydra.core.Function.Lambda lam) {
                                              return hydra.ext.java.Coder.withLambda(
                                                env.get(),
                                                (lam).value,
                                                (java.util.function.Function<hydra.ext.java.environment.JavaEnvironment, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>>) (env2 -> {
                                                  hydra.core.Term branchBody = (lam).value.body;
                                                  hydra.ext.java.syntax.Expression castExpr = hydra.ext.java.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.Utils.javaCastExpression(
                                                    variantRefType,
                                                    hydra.ext.java.Utils.javaExpressionToJavaUnaryExpression(jArg)));
                                                  hydra.core.Name lambdaParam = (lam).value.parameter;
                                                  hydra.ext.java.environment.JavaEnvironment env3 = hydra.ext.java.Coder.insertBranchVar(
                                                    lambdaParam,
                                                    env2);
                                                  Boolean isBranchTailCall = hydra.CoderUtils.isTailRecursiveInTailPosition(
                                                    funcName,
                                                    branchBody);
                                                  hydra.ext.java.syntax.Identifier varId = hydra.ext.java.Utils.variableToJavaIdentifier(lambdaParam);
                                                  hydra.ext.java.syntax.BlockStatement localDecl = hydra.ext.java.Utils.varDeclarationStatement(
                                                    varId,
                                                    castExpr);
                                                  return hydra.lib.eithers.Bind.apply(
                                                    hydra.lib.logic.IfElse.lazy(
                                                      isBranchTailCall,
                                                      () -> hydra.ext.java.Coder.encodeTermTCO(
                                                        env3,
                                                        funcName,
                                                        paramNames,
                                                        tcoVarRenames,
                                                        hydra.lib.math.Add.apply(
                                                          tcoDepth,
                                                          1),
                                                        branchBody,
                                                        cx,
                                                        g),
                                                      () -> hydra.lib.eithers.Bind.apply(
                                                        hydra.ext.java.Coder.analyzeJavaFunction(
                                                          env3,
                                                          branchBody,
                                                          cx,
                                                          g),
                                                        (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (fs -> {
                                                          hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.ConsList<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
                                                          hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env4 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.ext.java.environment.JavaEnvironment>) (projected -> projected.environment)).apply(fs));
                                                          hydra.util.Lazy<hydra.core.Term> innerBody = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
                                                          return hydra.lib.eithers.Bind.apply(
                                                            hydra.ext.java.Coder.bindingsToStatements(
                                                              env4.get(),
                                                              bindings.get(),
                                                              cx,
                                                              g),
                                                            (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (bindResult -> {
                                                              hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> bindingStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
                                                              hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env5 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
                                                              return hydra.lib.eithers.Bind.apply(
                                                                hydra.ext.java.Coder.encodeTerm(
                                                                  env5.get(),
                                                                  innerBody.get(),
                                                                  cx,
                                                                  g),
                                                                (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (jret -> {
                                                                  hydra.ext.java.syntax.BlockStatement returnStmt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(jret)));
                                                                  return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>right(hydra.lib.lists.Concat2.apply(
                                                                    bindingStmts.get(),
                                                                    hydra.util.ConsList.of(returnStmt)));
                                                                }));
                                                            }));
                                                        }))),
                                                    (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>>) (bodyStmts -> {
                                                      hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> blockStmts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
                                                        localDecl,
                                                        bodyStmts));
                                                      hydra.ext.java.syntax.RelationalExpression relExpr = hydra.ext.java.Utils.javaInstanceOf(
                                                        hydra.ext.java.Utils.javaUnaryExpressionToJavaRelationalExpression(hydra.ext.java.Utils.javaExpressionToJavaUnaryExpression(jArg)),
                                                        variantRefType);
                                                      hydra.ext.java.syntax.Expression condExpr = hydra.ext.java.Utils.javaRelationalExpressionToJavaExpression(relExpr);
                                                      hydra.ext.java.syntax.Statement ifBody = new hydra.ext.java.syntax.Statement.WithoutTrailing(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Block(new hydra.ext.java.syntax.Block(blockStmts.get())));
                                                      return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>right(new hydra.ext.java.syntax.BlockStatement.Statement(new hydra.ext.java.syntax.Statement.IfThen(new hydra.ext.java.syntax.IfThenStatement(condExpr, ifBody))));
                                                    }));
                                                }));
                                            }
                                          });
                                        }
                                      });
                                    }),
                                    cases_),
                                  (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (ifBlocks -> hydra.lib.eithers.Bind.apply(
                                    hydra.lib.maybes.Cases.applyLazy(
                                      dflt,
                                      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>right(hydra.util.ConsList.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(jArg))))),
                                      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (d -> hydra.lib.eithers.Bind.apply(
                                        hydra.ext.java.Coder.encodeTerm(
                                          env.get(),
                                          d,
                                          cx,
                                          g),
                                        (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (dExpr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>right(hydra.util.ConsList.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(dExpr))))))))),
                                    (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (defaultStmt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>right(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
                                      hydra.util.ConsList.of(matchDecl),
                                      ifBlocks,
                                      defaultStmt)))))));
                              }))));
                        }
                      });
                    }
                  });
                }
              });
            })).get(),
            () -> hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.encodeTerm(
                env.get(),
                term,
                cx,
                g),
              (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (expr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>right(hydra.util.ConsList.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(expr))))))));
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> visit(hydra.core.Term.Let lt) {
          hydra.util.ConsList<hydra.core.Binding> letBindings = (lt).value.bindings;
          hydra.core.Term letBody = (lt).value.body;
          return hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Coder.bindingsToStatements(
              env.get(),
              letBindings,
              cx,
              g),
            (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (bindResult -> {
              hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> envAfterLet = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
              hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> letStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
              return hydra.lib.eithers.Bind.apply(
                hydra.ext.java.Coder.encodeTermTCO(
                  envAfterLet.get(),
                  funcName,
                  paramNames,
                  tcoVarRenames,
                  tcoDepth,
                  letBody,
                  cx,
                  g),
                (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>>) (tcoBodyStmts -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>>right(hydra.lib.lists.Concat2.apply(
                  letStmts.get(),
                  tcoBodyStmts))));
            }));
        }
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> encodeType(hydra.ext.java.environment.Aliases aliases, hydra.util.PersistentSet<hydra.core.Name> boundVars, hydra.core.Type t, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.util.PersistentSet<hydra.core.Name> inScopeTypeParams = (aliases).inScopeTypeParams;
    hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> typeVarSubst = (aliases).typeVarSubst;
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          "can't encode unsupported type in Java: ",
          hydra.show.Core.type(t)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Application at) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.encodeType(
            aliases,
            boundVars,
            (at).value.function,
            cx,
            g),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jlhs -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.encodeType(
                aliases,
                boundVars,
                (at).value.argument,
                cx,
                g),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                jt_,
                cx))),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jrhs -> hydra.ext.java.Utils.addJavaTypeParameter(
              jrhs,
              jlhs,
              cx)))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Function ft) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Coder.encodeType(
              aliases,
              boundVars,
              (ft).value.domain,
              cx,
              g),
            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
              jt_,
              cx))),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jdom -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.encodeType(
                aliases,
                boundVars,
                (ft).value.codomain,
                cx,
                g),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                jt_,
                cx))),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jcod -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaRefType(
              hydra.util.ConsList.of(
                jdom,
                jcod),
              hydra.ext.java.Names.javaUtilFunctionPackageName(),
              "Function"))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Forall fa) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.encodeType(
            aliases,
            hydra.lib.sets.Insert.apply(
              (fa).value.parameter,
              boundVars),
            (fa).value.body,
            cx,
            g),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jbody -> hydra.ext.java.Utils.addJavaTypeParameter(
            hydra.ext.java.Utils.javaTypeVariable((fa).value.parameter.value),
            jbody,
            cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.List et) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.encodeType(
            aliases,
            boundVars,
            (et).value,
            cx,
            g),
          (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jet -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.Bind.apply(
              hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(jet),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                jt_,
                cx))),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaRefType(
              hydra.util.ConsList.of(rt),
              hydra.ext.java.Names.hydraUtilPackageName(),
              "ConsList"))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Literal lt) {
        return hydra.ext.java.Coder.encodeLiteralType(
          (lt).value,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Either et) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Coder.encodeType(
              aliases,
              boundVars,
              (et).value.left,
              cx,
              g),
            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
              jt_,
              cx))),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jlt -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.encodeType(
                aliases,
                boundVars,
                (et).value.right,
                cx,
                g),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                jt_,
                cx))),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jrt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaRefType(
              hydra.util.ConsList.of(
                jlt,
                jrt),
              hydra.ext.java.Names.hydraUtilPackageName(),
              "Either"))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Map mt) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Coder.encodeType(
              aliases,
              boundVars,
              (mt).value.keys,
              cx,
              g),
            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
              jt_,
              cx))),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jkt -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.encodeType(
                aliases,
                boundVars,
                (mt).value.values,
                cx,
                g),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                jt_,
                cx))),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jvt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaRefType(
              hydra.util.ConsList.of(
                jkt,
                jvt),
              hydra.ext.java.Names.hydraUtilPackageName(),
              "PersistentMap"))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Coder.encodeType(
              aliases,
              boundVars,
              (pt).value.first,
              cx,
              g),
            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
              jt_,
              cx))),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jfirst -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.encodeType(
                aliases,
                boundVars,
                (pt).value.second,
                cx,
                g),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                jt_,
                cx))),
            (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jsecond -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaRefType(
              hydra.util.ConsList.of(
                jfirst,
                jsecond),
              hydra.ext.java.Names.hydraUtilPackageName(),
              "Pair"))))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Unit ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaRefType(
          (hydra.util.ConsList<hydra.ext.java.syntax.ReferenceType>) (hydra.util.ConsList.<hydra.ext.java.syntax.ReferenceType>empty()),
          hydra.ext.java.Names.javaLangPackageName(),
          "Void"));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Record rt) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply((rt).value),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaRefType(
            (hydra.util.ConsList<hydra.ext.java.syntax.ReferenceType>) (hydra.util.ConsList.<hydra.ext.java.syntax.ReferenceType>empty()),
            hydra.ext.java.Names.javaLangPackageName(),
            "Void")),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unexpected anonymous record type")), cx))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Maybe ot) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Coder.encodeType(
              aliases,
              boundVars,
              (ot).value,
              cx,
              g),
            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
              jt_,
              cx))),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jot -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaRefType(
            hydra.util.ConsList.of(jot),
            hydra.ext.java.Names.hydraUtilPackageName(),
            "Maybe"))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Set st) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Coder.encodeType(
              aliases,
              boundVars,
              (st).value,
              cx,
              g),
            (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ReferenceType>>) (jt_ -> hydra.ext.java.Utils.javaTypeToJavaReferenceType(
              jt_,
              cx))),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (jst -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(hydra.ext.java.Utils.javaRefType(
            hydra.util.ConsList.of(jst),
            hydra.ext.java.Names.hydraUtilPackageName(),
            "PersistentSet"))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Union ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unexpected anonymous union type")), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Variable name0) {
        hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
          () -> (name0).value,
          hydra.lib.maps.Lookup.apply(
            (name0).value,
            typeVarSubst)));
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.encodeType_resolveIfTypedef(
            aliases,
            boundVars,
            inScopeTypeParams,
            name.get(),
            cx,
            g),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (resolved -> hydra.lib.maybes.Cases.applyLazy(
            resolved,
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>right(hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Or.apply(
                hydra.lib.sets.Member.apply(
                  name.get(),
                  boundVars),
                hydra.lib.sets.Member.apply(
                  name.get(),
                  inScopeTypeParams)),
              () -> new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.Utils.javaTypeVariable(name.get().value)),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.ext.java.Coder.isLambdaBoundVariable(name.get()),
                () -> new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.Utils.javaTypeVariable(name.get().value)),
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.ext.java.Coder.isUnresolvedInferenceVar(name.get()),
                  () -> new hydra.ext.java.syntax.Type.Reference(new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(hydra.ext.java.Utils.javaClassType(
                    (hydra.util.ConsList<hydra.ext.java.syntax.ReferenceType>) (hydra.util.ConsList.<hydra.ext.java.syntax.ReferenceType>empty()),
                    hydra.ext.java.Names.javaLangPackageName(),
                    "Object")))),
                  () -> new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.Utils.nameToJavaReferenceType(
                    aliases,
                    true,
                    (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()),
                    name.get(),
                    (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))))))),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>>) (resolvedType -> hydra.ext.java.Coder.encodeType(
              aliases,
              boundVars,
              resolvedType,
              cx,
              g)))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type> visit(hydra.core.Type.Wrap ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Type>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("unexpected anonymous wrap type")), cx)));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>> encodeTypeDefinition(hydra.ext.java.syntax.PackageDeclaration pkg, hydra.ext.java.environment.Aliases aliases, hydra.module.TypeDefinition tdef, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.core.Type typ = (tdef).type;
    Boolean serializable = hydra.ext.java.Coder.isSerializableJavaType(typ);
    hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.ImportDeclaration>> imports = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      serializable,
      () -> hydra.util.ConsList.of(new hydra.ext.java.syntax.ImportDeclaration.SingleType(new hydra.ext.java.syntax.SingleTypeImportDeclaration(hydra.ext.java.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("java.io.Serializable"))))),
      () -> (hydra.util.ConsList<hydra.ext.java.syntax.ImportDeclaration>) (hydra.util.ConsList.<hydra.ext.java.syntax.ImportDeclaration>empty())));
    hydra.core.Name name = (tdef).name;
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.java.Coder.toClassDecl(
        false,
        serializable,
        aliases,
        (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()),
        name,
        typ,
        cx,
        g),
      (java.util.function.Function<hydra.ext.java.syntax.ClassDeclaration, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>) (decl -> hydra.lib.eithers.Bind.apply(
        hydra.Annotations.getTypeDescription(
          cx,
          g,
          typ),
        (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>>) (comment -> {
          hydra.ext.java.syntax.TypeDeclarationWithComments tdecl = new hydra.ext.java.syntax.TypeDeclarationWithComments(new hydra.ext.java.syntax.TypeDeclaration.Class_(decl), comment);
          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>>right((hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>) ((hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>) (new hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>(name, new hydra.ext.java.syntax.CompilationUnit.Ordinary(new hydra.ext.java.syntax.OrdinaryCompilationUnit(hydra.util.Maybe.just(pkg), imports.get(), hydra.util.ConsList.of(tdecl)))))));
        }))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.util.Maybe<hydra.core.Type>> encodeType_resolveIfTypedef(T0 aliases, hydra.util.PersistentSet<hydra.core.Name> boundVars, hydra.util.PersistentSet<hydra.core.Name> inScopeTypeParams, hydra.core.Name name, T1 cx, hydra.graph.Graph g) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Or.apply(
        hydra.lib.sets.Member.apply(
          name,
          boundVars),
        hydra.lib.sets.Member.apply(
          name,
          inScopeTypeParams)),
      () -> hydra.util.Either.<T2, hydra.util.Maybe<hydra.core.Type>>right((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.ext.java.Coder.isLambdaBoundVariable(name),
        () -> hydra.util.Either.<T2, hydra.util.Maybe<hydra.core.Type>>right((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
        () -> ((java.util.function.Supplier<hydra.util.Either<T2, hydra.util.Maybe<hydra.core.Type>>>) (() -> {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme> schemaTypes = (g).schemaTypes;
          return hydra.lib.maybes.Cases.applyLazy(
            hydra.lib.maps.Lookup.apply(
              name,
              schemaTypes),
            () -> hydra.util.Either.<T2, hydra.util.Maybe<hydra.core.Type>>right((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
            (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<T2, hydra.util.Maybe<hydra.core.Type>>>) (ts -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply((ts).variables)),
              () -> hydra.util.Either.<T2, hydra.util.Maybe<hydra.core.Type>>right((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
              () -> hydra.Rewriting.deannotateType((ts).type).accept(new hydra.core.Type.PartialVisitor<>() {
                @Override
                public hydra.util.Either<T2, hydra.util.Maybe<hydra.core.Type>> otherwise(hydra.core.Type instance) {
                  return hydra.util.Either.<T2, hydra.util.Maybe<hydra.core.Type>>right(hydra.util.Maybe.just((ts).type));
                }

                @Override
                public hydra.util.Either<T2, hydra.util.Maybe<hydra.core.Type>> visit(hydra.core.Type.Record ignored) {
                  return hydra.util.Either.<T2, hydra.util.Maybe<hydra.core.Type>>right((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()));
                }

                @Override
                public hydra.util.Either<T2, hydra.util.Maybe<hydra.core.Type>> visit(hydra.core.Type.Union ignored) {
                  return hydra.util.Either.<T2, hydra.util.Maybe<hydra.core.Type>>right((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()));
                }

                @Override
                public hydra.util.Either<T2, hydra.util.Maybe<hydra.core.Type>> visit(hydra.core.Type.Wrap ignored) {
                  return hydra.util.Either.<T2, hydra.util.Maybe<hydra.core.Type>>right((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()));
                }
              }))));
        })).get()));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> encodeVariable(hydra.ext.java.environment.JavaEnvironment env, hydra.core.Name name, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.ext.java.environment.Aliases aliases = (env).aliases;
    hydra.core.Name resolvedName = hydra.ext.java.Utils.lookupJavaVarName(
      aliases,
      name);
    hydra.ext.java.syntax.Identifier jid = hydra.ext.java.Utils.javaIdentifier((resolvedName).value);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        name,
        (aliases).branchVars),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaFieldAccessToJavaExpression(new hydra.ext.java.syntax.FieldAccess(new hydra.ext.java.syntax.FieldAccess_Qualifier.Primary(hydra.ext.java.Utils.javaExpressionToJavaPrimary(hydra.ext.java.Utils.javaIdentifierToJavaExpression(jid))), hydra.ext.java.Utils.javaIdentifier(hydra.ext.java.Names.valueFieldName())))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.And.apply(
          hydra.lib.equality.Equal.apply(
            name,
            new hydra.core.Name(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              hydra.ext.java.Names.instanceName(),
              "_",
              hydra.ext.java.Names.valueFieldName())))),
          hydra.ext.java.Coder.isRecursiveVariable(
            aliases,
            name)),
        () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
          hydra.ext.java.syntax.Expression instanceExpr = hydra.ext.java.Utils.javaIdentifierToJavaExpression(hydra.ext.java.Utils.javaIdentifier(hydra.ext.java.Names.instanceName()));
          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaFieldAccessToJavaExpression(new hydra.ext.java.syntax.FieldAccess(new hydra.ext.java.syntax.FieldAccess_Qualifier.Primary(hydra.ext.java.Utils.javaExpressionToJavaPrimary(instanceExpr)), hydra.ext.java.Utils.javaIdentifier(hydra.ext.java.Names.valueFieldName()))));
        })).get(),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.And.apply(
            hydra.ext.java.Coder.isRecursiveVariable(
              aliases,
              name),
            hydra.lib.logic.Not.apply(hydra.ext.java.Coder.isLambdaBoundIn(
              name,
              (aliases).lambdaVars))),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocation(
            hydra.util.Maybe.just(hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>left(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), jid))),
            new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.getMethodName()),
            (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty())))),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.logic.And.apply(
              hydra.lib.sets.Member.apply(
                name,
                (aliases).thunkedVars),
              hydra.lib.logic.Not.apply(hydra.ext.java.Coder.isLambdaBoundIn(
                name,
                (aliases).lambdaVars))),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocation(
              hydra.util.Maybe.just(hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>left(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), jid))),
              new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.getMethodName()),
              (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty())))),
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.ext.java.Coder.isLambdaBoundIn(
                name,
                (aliases).lambdaVars),
              () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                hydra.core.Name actualName = hydra.ext.java.Coder.findMatchingLambdaVar(
                  name,
                  (aliases).lambdaVars);
                return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                  hydra.core.Name resolvedActual = hydra.ext.java.Utils.lookupJavaVarName(
                    aliases,
                    actualName);
                  return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaIdentifierToJavaExpression(hydra.ext.java.Utils.variableToJavaIdentifier(resolvedActual)));
                })).get();
              })).get(),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.sets.Member.apply(
                  name,
                  (aliases).inScopeJavaVars),
                () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaIdentifierToJavaExpression(hydra.ext.java.Coder.elementJavaIdentifier(
                  false,
                  false,
                  aliases,
                  resolvedName))),
                () -> hydra.lib.eithers.Bind.apply(
                  hydra.ext.java.Coder.classifyDataReference(
                    name,
                    cx,
                    g),
                  (java.util.function.Function<hydra.ext.java.environment.JavaSymbolClass, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (cls -> (cls).accept(new hydra.ext.java.environment.JavaSymbolClass.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.environment.JavaSymbolClass.HoistedLambda arity) {
                      return hydra.ext.java.Coder.encodeVariable_hoistedLambdaCase(
                        aliases,
                        name,
                        (arity).value,
                        cx,
                        g);
                    }

                    @Override
                    public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.environment.JavaSymbolClass.LocalVariable ignored) {
                      return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaIdentifierToJavaExpression(hydra.ext.java.Coder.elementJavaIdentifier(
                        false,
                        false,
                        aliases,
                        resolvedName)));
                    }

                    @Override
                    public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.environment.JavaSymbolClass.Constant ignored) {
                      return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaIdentifierToJavaExpression(hydra.ext.java.Coder.elementJavaIdentifier(
                        false,
                        false,
                        aliases,
                        name)));
                    }

                    @Override
                    public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.environment.JavaSymbolClass.NullaryFunction ignored) {
                      return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocation(
                        (hydra.util.Maybe<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>) (hydra.util.Maybe.<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>nothing()),
                        hydra.ext.java.Coder.elementJavaIdentifier(
                          false,
                          false,
                          aliases,
                          name),
                        (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()))));
                    }

                    @Override
                    public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.environment.JavaSymbolClass.UnaryFunction ignored) {
                      return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaIdentifierToJavaExpression(hydra.ext.java.Coder.elementJavaIdentifier(
                        false,
                        true,
                        aliases,
                        name)));
                    }
                  })))))))));
  }

  static hydra.ext.java.syntax.Expression encodeVariable_buildCurried(hydra.util.ConsList<hydra.core.Name> params, hydra.ext.java.syntax.Expression inner) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(params),
      () -> inner,
      () -> hydra.ext.java.Utils.javaLambda(
        hydra.lib.lists.Head.apply(params),
        hydra.ext.java.Coder.encodeVariable_buildCurried(
          hydra.lib.lists.Tail.apply(params),
          inner)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> encodeVariable_hoistedLambdaCase(hydra.ext.java.environment.Aliases aliases, hydra.core.Name name, Integer arity, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> paramNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<Integer, hydra.core.Name>) (i -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
        "p",
        hydra.lib.literals.ShowInt32.apply(i)))),
      hydra.lib.math.Range.apply(
        0,
        hydra.lib.math.Sub.apply(
          arity,
          1))));
    hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.Expression>> paramExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.Expression>) (pn -> hydra.ext.java.Utils.javaIdentifierToJavaExpression(hydra.ext.java.Utils.variableToJavaIdentifier(pn))),
      paramNames.get()));
    hydra.util.Lazy<hydra.ext.java.syntax.Expression> call = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocation(
      (hydra.util.Maybe<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>) (hydra.util.Maybe.<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>nothing()),
      hydra.ext.java.Coder.elementJavaIdentifier(
        false,
        false,
        aliases,
        name),
      paramExprs.get())));
    hydra.ext.java.syntax.Expression lam = hydra.ext.java.Coder.encodeVariable_buildCurried(
      paramNames.get(),
      call.get());
    return hydra.lib.eithers.Bind.apply(
      hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.Binding>>right(hydra.Lexical.dereferenceElement(
        g,
        name)),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (mel -> hydra.lib.maybes.Cases.applyLazy(
        mel,
        () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(lam),
        (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (el -> hydra.lib.maybes.Cases.applyLazy(
          (el).type,
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(lam),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (ts -> {
            hydra.core.Type typ = (ts).type;
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.encodeType(
                aliases,
                (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                typ,
                cx,
                g),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jtype -> hydra.lib.eithers.Bind.apply(
                hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                  jtype,
                  cx),
                (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.Utils.javaCastExpression(
                  rt,
                  hydra.ext.java.Utils.javaExpressionToJavaUnaryExpression(lam))))))));
          }))))));
  }

  static hydra.ext.java.syntax.InclusiveOrExpression eqClause(String tmpName, hydra.core.FieldType ft) {
    String fname = (ft).name.value;
    hydra.core.Type ftype = (ft).type;
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.java.Coder.isBinaryType(ftype),
      () -> hydra.ext.java.Coder.arraysEqualsClause(
        tmpName,
        fname),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.ext.java.Coder.isBigNumericType(ftype),
        () -> hydra.ext.java.Coder.compareToZeroClause(
          tmpName,
          fname),
        () -> hydra.ext.java.Coder.equalsClause(
          tmpName,
          fname)));
  }

  static hydra.ext.java.syntax.InclusiveOrExpression equalsClause(String tmpName, String fname) {
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> header = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Type(hydra.ext.java.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("java.util.Objects"))), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.equalsMethodName()))));
    hydra.ext.java.syntax.Expression otherArg = hydra.ext.java.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.Utils.fieldExpression(
      hydra.ext.java.Utils.javaIdentifier(tmpName),
      hydra.ext.java.Utils.javaIdentifier(fname)));
    hydra.ext.java.syntax.Expression thisArg = hydra.ext.java.Utils.javaExpressionNameToJavaExpression(hydra.ext.java.Utils.fieldExpression(
      new hydra.ext.java.syntax.Identifier("this"),
      hydra.ext.java.Utils.javaIdentifier(fname)));
    return hydra.ext.java.Utils.javaPostfixExpressionToJavaInclusiveOrExpression(hydra.ext.java.Utils.javaMethodInvocationToJavaPostfixExpression(new hydra.ext.java.syntax.MethodInvocation(header.get(), hydra.util.ConsList.of(
      thisArg,
      otherArg))));
  }

  static <T0> hydra.core.Type extractArgType(T0 _lhs, hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return typ;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Application at1) {
        return (at1).value.function.accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.core.Type otherwise(hydra.core.Type instance) {
            return typ;
          }

          @Override
          public hydra.core.Type visit(hydra.core.Type.Application _at2) {
            return (at1).value.argument;
          }
        });
      }
    });
  }

  static hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> extractDirectReturn(hydra.util.PersistentSet<hydra.core.Name> tparamSet, hydra.core.Type t) {
    return hydra.ext.java.Coder.extractDirectReturn_go(
      tparamSet,
      t);
  }

  static hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> extractDirectReturn_go(hydra.util.PersistentSet<hydra.core.Name> tparamSet, hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
        return (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Function ft) {
        hydra.core.Type cod = (ft).value.codomain;
        hydra.core.Type dom = hydra.Rewriting.deannotateType((ft).value.domain);
        return (dom).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
            return hydra.ext.java.Coder.extractDirectReturn_go(
              tparamSet,
              cod);
          }

          @Override
          public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable inVar) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                (inVar).value,
                tparamSet),
              () -> hydra.Rewriting.deannotateType(cod).accept(new hydra.core.Type.PartialVisitor<>() {
                @Override
                public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                  return (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>empty());
                }

                @Override
                public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Function ft2) {
                  hydra.core.Type midArg = hydra.Rewriting.deannotateType((ft2).value.domain);
                  hydra.core.Type retPart = hydra.Rewriting.deannotateType((ft2).value.codomain);
                  return (midArg).accept(new hydra.core.Type.PartialVisitor<>() {
                    @Override
                    public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                      return (retPart).accept(new hydra.core.Type.PartialVisitor<>() {
                        @Override
                        public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                          return (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>empty());
                        }

                        @Override
                        public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable outVar) {
                          return hydra.lib.logic.IfElse.lazy(
                            hydra.lib.sets.Member.apply(
                              (outVar).value,
                              tparamSet),
                            () -> hydra.util.ConsList.of((hydra.util.Pair<hydra.core.Name, hydra.core.Name>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Name>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Name>((inVar).value, (outVar).value)))),
                            () -> (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>empty()));
                        }
                      });
                    }

                    @Override
                    public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable midVar) {
                      return hydra.lib.logic.IfElse.lazy(
                        hydra.lib.sets.Member.apply(
                          (midVar).value,
                          tparamSet),
                        () -> (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>empty()),
                        () -> (retPart).accept(new hydra.core.Type.PartialVisitor<>() {
                          @Override
                          public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                            return (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>empty());
                          }

                          @Override
                          public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable outVar) {
                            return hydra.lib.logic.IfElse.lazy(
                              hydra.lib.sets.Member.apply(
                                (outVar).value,
                                tparamSet),
                              () -> hydra.util.ConsList.of((hydra.util.Pair<hydra.core.Name, hydra.core.Name>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Name>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Name>((inVar).value, (outVar).value)))),
                              () -> (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>empty()));
                          }
                        }));
                    }
                  });
                }
              }),
              () -> hydra.ext.java.Coder.extractDirectReturn_go(
                tparamSet,
                cod));
          }
        });
      }
    });
  }

  static hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> extractInOutPair(hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
        return (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Function ft) {
        return hydra.Rewriting.deannotateType((ft).value.domain).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
            return (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>empty());
          }

          @Override
          public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable inVar) {
            hydra.core.Type retType = hydra.ext.java.Coder.unwrapReturnType((ft).value.codomain);
            return hydra.Rewriting.deannotateType(retType).accept(new hydra.core.Type.PartialVisitor<>() {
              @Override
              public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                return (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>empty());
              }

              @Override
              public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Pair pt) {
                return hydra.Rewriting.deannotateType((pt).value.first).accept(new hydra.core.Type.PartialVisitor<>() {
                  @Override
                  public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> otherwise(hydra.core.Type instance) {
                    return (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>) (hydra.util.ConsList.<hydra.util.Pair<hydra.core.Name, hydra.core.Name>>empty());
                  }

                  @Override
                  public hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Name>> visit(hydra.core.Type.Variable outVar) {
                    return hydra.util.ConsList.of((hydra.util.Pair<hydra.core.Name, hydra.core.Name>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Name>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Name>((inVar).value, (outVar).value))));
                  }
                });
              }
            });
          }
        });
      }
    });
  }

  static hydra.util.ConsList<hydra.core.Type> extractTypeApplicationArgs(hydra.core.Type typ) {
    return hydra.lib.lists.Reverse.apply(hydra.ext.java.Coder.extractTypeApplicationArgs_go(typ));
  }

  static hydra.util.ConsList<hydra.core.Type> extractTypeApplicationArgs_go(hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.core.Type> visit(hydra.core.Type.Application at) {
        return hydra.lib.lists.Cons.apply(
          (at).value.argument,
          hydra.ext.java.Coder.extractTypeApplicationArgs_go((at).value.function));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.FormalParameter> fieldTypeToFormalParam(hydra.ext.java.environment.Aliases aliases, hydra.core.FieldType ft, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.java.Coder.encodeType(
        aliases,
        (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
        (ft).type,
        cx,
        g),
      (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.FormalParameter>>) (jt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.FormalParameter>right(hydra.ext.java.Utils.javaTypeToJavaFormalParameter(
        jt,
        (ft).name))));
  }

  static <T0> hydra.util.ConsList<T0> filterByFlags(hydra.util.ConsList<T0> xs, hydra.util.ConsList<Boolean> flags) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<T0, Boolean>, T0>) (p -> hydra.lib.pairs.First.apply(p)),
      hydra.lib.lists.Filter.apply(
        (java.util.function.Function<hydra.util.Pair<T0, Boolean>, Boolean>) (p -> hydra.lib.pairs.Second.apply(p)),
        hydra.lib.lists.Zip.apply(
          xs,
          flags)));
  }

  static <T0, T1> hydra.util.Either<T1, hydra.util.ConsList<hydra.core.Type>> filterPhantomTypeArgs(hydra.core.Name calleeName, hydra.util.ConsList<hydra.core.Type> allTypeArgs, T0 cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.util.Either.<T1, hydra.util.Maybe<hydra.core.Binding>>right(hydra.Lexical.dereferenceElement(
        g,
        calleeName)),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, hydra.util.Either<T1, hydra.util.ConsList<hydra.core.Type>>>) (mel -> hydra.lib.maybes.Cases.applyLazy(
        mel,
        () -> hydra.util.Either.<T1, hydra.util.ConsList<hydra.core.Type>>right(allTypeArgs),
        (java.util.function.Function<hydra.core.Binding, hydra.util.Either<T1, hydra.util.ConsList<hydra.core.Type>>>) (el -> hydra.lib.maybes.Cases.applyLazy(
          (el).type,
          () -> hydra.util.Either.<T1, hydra.util.ConsList<hydra.core.Type>>right(allTypeArgs),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<T1, hydra.util.ConsList<hydra.core.Type>>>) (ts -> {
            hydra.core.Type schemeType = (ts).type;
            Integer nParams = hydra.ext.java.Coder.countFunctionParams(schemeType);
            hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type> peeled = hydra.ext.java.Coder.peelDomainTypes(
              nParams,
              schemeType);
            hydra.util.Lazy<hydra.core.Type> calleeCod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(peeled));
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> calleeDoms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(peeled));
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> schemeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.ext.java.Coder.isSimpleName(v)),
              (ts).variables));
            hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> overgenSubst = hydra.ext.java.Coder.detectAccumulatorUnification(
              calleeDoms.get(),
              calleeCod.get(),
              schemeVars.get());
            hydra.util.PersistentSet<hydra.core.Name> schemeTypeVars = hydra.ext.java.Coder.collectTypeVars((ts).type);
            hydra.util.Lazy<hydra.util.ConsList<Boolean>> keepFlags = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.And.apply(
                hydra.lib.sets.Member.apply(
                  v,
                  schemeTypeVars),
                hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
                  v,
                  overgenSubst)))),
              schemeVars.get()));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
                hydra.lib.lists.Length.apply(schemeVars.get()),
                hydra.lib.lists.Length.apply(allTypeArgs))),
              () -> hydra.util.Either.<T1, hydra.util.ConsList<hydra.core.Type>>right(allTypeArgs),
              () -> hydra.util.Either.<T1, hydra.util.ConsList<hydra.core.Type>>right(hydra.ext.java.Coder.filterPhantomTypeArgs_filterAndApply(
                allTypeArgs,
                keepFlags.get(),
                overgenSubst)));
          }))))));
  }

  static hydra.util.ConsList<hydra.core.Type> filterPhantomTypeArgs_filterAndApply(hydra.util.ConsList<hydra.core.Type> allTypeArgs, hydra.util.ConsList<Boolean> keepFlags, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> overgenSubst) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> filtered = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.core.Type, Boolean>, hydra.core.Type>) (p -> hydra.lib.pairs.First.apply(p)),
      hydra.lib.lists.Filter.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.Type, Boolean>, Boolean>) (p -> hydra.lib.pairs.Second.apply(p)),
        hydra.lib.lists.Zip.apply(
          allTypeArgs,
          keepFlags))));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.maps.Null.apply(overgenSubst)),
      () -> hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> hydra.ext.java.Coder.substituteTypeVarsWithTypes(
          overgenSubst,
          t)),
        filtered.get()),
      () -> filtered.get());
  }

  static hydra.core.Name findMatchingLambdaVar(hydra.core.Name name, hydra.util.PersistentSet<hydra.core.Name> lambdaVars) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        name,
        lambdaVars),
      () -> name,
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.ext.java.Coder.isLambdaBoundIn_isQualified(name),
        () -> hydra.lib.maybes.FromMaybe.applyLazy(
          () -> name,
          hydra.lib.lists.Find.apply(
            (java.util.function.Function<hydra.core.Name, Boolean>) (lv -> hydra.lib.logic.And.apply(
              hydra.ext.java.Coder.isLambdaBoundIn_isQualified(lv),
              hydra.lib.equality.Equal.apply(
                hydra.Names.localNameOf(lv),
                hydra.Names.localNameOf(name)))),
            hydra.lib.sets.ToList.apply(lambdaVars))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Member.apply(
            new hydra.core.Name(hydra.Names.localNameOf(name)),
            lambdaVars),
          () -> new hydra.core.Name(hydra.Names.localNameOf(name)),
          () -> name)));
  }

  static hydra.util.Maybe<hydra.core.Name> findPairFirst(hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Type.Pair pt) {
        return hydra.Rewriting.deannotateType((pt).value.first).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.core.Type instance) {
            return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Type.Variable v) {
            return hydra.util.Maybe.just((v).value);
          }
        });
      }
    });
  }

  static <T0> hydra.util.Maybe<T0> findSelfRefVar(hydra.util.PersistentMap<T0, hydra.util.ConsList<T0>> grouped) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T0>>>> selfRefs = new hydra.util.Lazy<>(() -> hydra.ext.java.Coder.<T0>findSelfRefVar_selfRefs(grouped));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(selfRefs.get()),
      () -> (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),
      () -> hydra.util.Maybe.just(hydra.lib.pairs.First.apply(hydra.lib.lists.Head.apply(selfRefs.get()))));
  }

  static <T0> hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T0>>> findSelfRefVar_selfRefs(hydra.util.PersistentMap<T0, hydra.util.ConsList<T0>> grouped) {
    return hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.util.Pair<T0, hydra.util.ConsList<T0>>, Boolean>) (entry -> hydra.lib.lists.Elem.apply(
        hydra.lib.pairs.First.apply(entry),
        hydra.lib.pairs.Second.apply(entry))),
      hydra.lib.maps.ToList.apply(grouped));
  }

  static hydra.util.ConsList<java.math.BigInteger> first20Primes() {
    return hydra.util.ConsList.of(
      new java.math.BigInteger("2"),
      new java.math.BigInteger("3"),
      new java.math.BigInteger("5"),
      new java.math.BigInteger("7"),
      new java.math.BigInteger("11"),
      new java.math.BigInteger("13"),
      new java.math.BigInteger("17"),
      new java.math.BigInteger("19"),
      new java.math.BigInteger("23"),
      new java.math.BigInteger("29"),
      new java.math.BigInteger("31"),
      new java.math.BigInteger("37"),
      new java.math.BigInteger("41"),
      new java.math.BigInteger("43"),
      new java.math.BigInteger("47"),
      new java.math.BigInteger("53"),
      new java.math.BigInteger("59"),
      new java.math.BigInteger("61"),
      new java.math.BigInteger("67"),
      new java.math.BigInteger("71"));
  }

  static hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term> flattenApps(hydra.core.Term t, hydra.util.ConsList<hydra.core.Term> acc) {
    return hydra.Rewriting.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>(acc, t)));
      }

      @Override
      public hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term> visit(hydra.core.Term.Application app) {
        return hydra.ext.java.Coder.flattenApps(
          (app).value.function,
          hydra.lib.lists.Cons.apply(
            (app).value.argument,
            acc));
      }
    });
  }

  static hydra.util.ConsList<hydra.core.Binding> flattenBindings(hydra.util.ConsList<hydra.core.Binding> bindings) {
    return hydra.lib.lists.Bind.apply(
      bindings,
      (java.util.function.Function<hydra.core.Binding, hydra.util.ConsList<hydra.core.Binding>>) (b -> hydra.Rewriting.deannotateTerm((b).term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.ConsList<hydra.core.Binding> otherwise(hydra.core.Term instance) {
          return hydra.util.ConsList.of(b);
        }

        @Override
        public hydra.util.ConsList<hydra.core.Binding> visit(hydra.core.Term.Let lt) {
          return hydra.lib.lists.Concat2.apply(
            hydra.ext.java.Coder.flattenBindings((lt).value.bindings),
            hydra.util.ConsList.of(new hydra.core.Binding((b).name, (lt).value.body, (b).type)));
        }
      })));
  }

  static hydra.core.Name freshJavaName(hydra.core.Name base, hydra.util.PersistentSet<hydra.core.Name> avoid) {
    return hydra.ext.java.Coder.freshJavaName_go(
      base,
      avoid,
      2);
  }

  static hydra.core.Name freshJavaName_go(hydra.core.Name base, hydra.util.PersistentSet<hydra.core.Name> avoid, Integer i) {
    hydra.core.Name candidate = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      (base).value,
      hydra.lib.literals.ShowInt32.apply(i)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        candidate,
        avoid),
      () -> hydra.ext.java.Coder.freshJavaName_go(
        base,
        avoid,
        hydra.lib.math.Add.apply(
          i,
          1)),
      () -> candidate);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> functionCall(hydra.ext.java.environment.JavaEnvironment env, Boolean isPrim, hydra.core.Name name, hydra.util.ConsList<hydra.core.Term> args, hydra.util.ConsList<hydra.core.Type> typeApps, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.ext.java.environment.Aliases aliases = (env).aliases;
    Boolean isLambdaBound = hydra.ext.java.Coder.isLambdaBoundIn(
      name,
      (aliases).lambdaVars);
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (arg -> hydra.ext.java.Coder.encodeTerm(
          env,
          arg,
          cx,
          g)),
        args),
      (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jargs0 -> {
        hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>> wrapResult = hydra.ext.java.Coder.wrapLazyArguments(
          name,
          jargs0);
        hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.Expression>> jargs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(wrapResult));
        hydra.util.Lazy<hydra.util.Maybe<String>> mMethodOverride = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(wrapResult));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.Or.apply(
            hydra.ext.java.Coder.isLocalVariable(name),
            isLambdaBound),
          () -> hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Coder.encodeVariable(
              env,
              name,
              cx,
              g),
            (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (baseExpr -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<hydra.ext.java.syntax.Expression, java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression>>) (acc -> (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression>) (jarg -> hydra.ext.java.Coder.applyJavaArg(
                acc,
                jarg))),
              baseExpr,
              jargs.get())))),
          () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
            java.util.function.Function<hydra.ext.java.syntax.Identifier, hydra.ext.java.syntax.Identifier> overrideMethodName = (java.util.function.Function<hydra.ext.java.syntax.Identifier, hydra.ext.java.syntax.Identifier>) (jid -> hydra.lib.maybes.Cases.applyLazy(
              mMethodOverride.get(),
              () -> jid,
              (java.util.function.Function<String, hydra.ext.java.syntax.Identifier>) (m -> {
                String s = (jid).value;
                return new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat2.apply(
                  hydra.lib.strings.FromList.apply(hydra.lib.lists.Take.apply(
                    hydra.lib.math.Sub.apply(
                      hydra.lib.strings.Length.apply(s),
                      hydra.lib.strings.Length.apply(hydra.ext.java.Names.applyMethodName())),
                    hydra.lib.strings.ToList.apply(s))),
                  m));
              })));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(typeApps),
              () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                hydra.ext.java.syntax.MethodInvocation_Header header = new hydra.ext.java.syntax.MethodInvocation_Header.Simple(new hydra.ext.java.syntax.MethodName((overrideMethodName).apply(hydra.ext.java.Coder.elementJavaIdentifier(
                  isPrim,
                  false,
                  aliases,
                  name))));
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(header, jargs.get())));
              })).get(),
              () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                hydra.module.QualifiedName qn = hydra.Names.qualifyName(name);
                return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                  hydra.util.Maybe<hydra.module.Namespace> mns = (qn).namespace;
                  return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                    String localName = (qn).local;
                    return hydra.lib.maybes.Cases.applyLazy(
                      mns,
                      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (() -> {
                        hydra.ext.java.syntax.MethodInvocation_Header header = new hydra.ext.java.syntax.MethodInvocation_Header.Simple(new hydra.ext.java.syntax.MethodName((overrideMethodName).apply(hydra.ext.java.Coder.elementJavaIdentifier(
                          isPrim,
                          false,
                          aliases,
                          name))));
                        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(header, jargs.get())));
                      })).get(),
                      (java.util.function.Function<hydra.module.Namespace, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (ns_ -> {
                        hydra.ext.java.syntax.Identifier classId = hydra.ext.java.Utils.nameToJavaName(
                          aliases,
                          hydra.ext.java.Coder.elementsQualifiedName(ns_));
                        hydra.util.Lazy<hydra.ext.java.syntax.Identifier> methodId = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                          isPrim,
                          () -> (overrideMethodName).apply(new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat2.apply(
                            hydra.ext.java.Utils.nameToJavaName(
                              aliases,
                              hydra.Names.unqualifyName(new hydra.module.QualifiedName(hydra.util.Maybe.just(ns_), hydra.Formatting.capitalize(localName)))).value,
                            hydra.lib.strings.Cat2.apply(
                              ".",
                              hydra.ext.java.Names.applyMethodName())))),
                          () -> new hydra.ext.java.syntax.Identifier(hydra.ext.java.Utils.sanitizeJavaName(localName))));
                        return hydra.lib.eithers.Bind.apply(
                          hydra.lib.eithers.MapList.apply(
                            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (t -> hydra.lib.eithers.Bind.apply(
                              hydra.ext.java.Coder.encodeType(
                                aliases,
                                (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                                t,
                                cx,
                                g),
                              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (jt -> hydra.lib.eithers.Bind.apply(
                                hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                                  jt,
                                  cx),
                                (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>right(new hydra.ext.java.syntax.TypeArgument.Reference(rt))))))),
                            typeApps),
                          (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jTypeArgs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStaticWithTypeArgs(
                            classId,
                            methodId.get(),
                            jTypeArgs,
                            jargs.get())))));
                      }));
                  })).get();
                })).get();
              })).get());
          })).get());
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type> getCodomain(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> ann, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.core.FunctionType, hydra.core.Type>) (ft -> (ft).codomain),
      hydra.ext.java.Coder.getFunctionType(
        ann,
        cx,
        g));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.FunctionType> getFunctionType(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> ann, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.Error_>>) (_de -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError((_de).value)), cx))),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (_a -> _a),
        hydra.Annotations.getType(
          g,
          ann)),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.FunctionType>>) (mt -> hydra.lib.maybes.Cases.applyLazy(
        mt,
        () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.FunctionType>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("type annotation is required for function and elimination terms in Java")), cx))),
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.FunctionType>>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.FunctionType> otherwise(hydra.core.Type instance) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.FunctionType>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
              "expected function type, got: ",
              hydra.show.Core.type(t)))), cx)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.FunctionType> visit(hydra.core.Type.Function ft) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.FunctionType>right((ft).value);
          }
        })))));
  }

  static <T0, T1> hydra.util.PersistentMap<T0, hydra.util.ConsList<T1>> groupPairsByFirst(hydra.util.ConsList<hydra.util.Pair<T0, T1>> pairs) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.PersistentMap<T0, hydra.util.ConsList<T1>>, java.util.function.Function<hydra.util.Pair<T0, T1>, hydra.util.PersistentMap<T0, hydra.util.ConsList<T1>>>>) (m -> (java.util.function.Function<hydra.util.Pair<T0, T1>, hydra.util.PersistentMap<T0, hydra.util.ConsList<T1>>>) (p -> {
        hydra.util.Lazy<T1> v = new hydra.util.Lazy<>(() -> hydra.ext.java.Coder.<T0, T1>groupPairsByFirst_v(p));
        return hydra.lib.maps.Alter.apply(
          (java.util.function.Function<hydra.util.Maybe<hydra.util.ConsList<T1>>, hydra.util.Maybe<hydra.util.ConsList<T1>>>) (mv -> hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Maybe.just(hydra.util.ConsList.of(v.get())),
            (java.util.function.Function<hydra.util.ConsList<T1>, hydra.util.Maybe<hydra.util.ConsList<T1>>>) (vs -> hydra.util.Maybe.just(hydra.lib.lists.Concat2.apply(
              vs,
              hydra.util.ConsList.of(v.get())))),
            mv)),
          hydra.ext.java.Coder.<T0, T1>groupPairsByFirst_k(p),
          m);
      })),
      (hydra.util.PersistentMap<T0, hydra.util.ConsList<T1>>) ((hydra.util.PersistentMap<T0, hydra.util.ConsList<T1>>) (hydra.lib.maps.Empty.<T0, hydra.util.ConsList<T1>>apply())),
      pairs);
  }

  static <T0, T1> T0 groupPairsByFirst_k(hydra.util.Pair<T0, T1> p) {
    return hydra.lib.pairs.First.apply(p);
  }

  static <T0, T1> T1 groupPairsByFirst_v(hydra.util.Pair<T0, T1> p) {
    return hydra.lib.pairs.Second.apply(p);
  }

  static hydra.ext.java.syntax.Expression hashCodeCompareExpr(String otherVar, String fname) {
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation_Header> header = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Type(hydra.ext.java.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("Integer"))), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier("compare"))));
    hydra.util.Lazy<hydra.ext.java.syntax.Expression> otherHashCode = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Expression(hydra.ext.java.Utils.fieldExpression(
      hydra.ext.java.Utils.javaIdentifier(otherVar),
      hydra.ext.java.Utils.javaIdentifier(fname))), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.hashCodeMethodName()))), (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()))));
    hydra.util.Lazy<hydra.ext.java.syntax.Expression> thisHashCode = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Expression(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.Utils.sanitizeJavaName(fname)))), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.hashCodeMethodName()))), (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()))));
    return hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(header.get(), hydra.util.ConsList.of(
      thisHashCode.get(),
      otherHashCode.get())));
  }

  static hydra.ext.java.syntax.MultiplicativeExpression hashCodeMultPair(java.math.BigInteger i, hydra.core.Name fname) {
    String fnameStr = (fname).value;
    hydra.ext.java.syntax.MultiplicativeExpression lhs = new hydra.ext.java.syntax.MultiplicativeExpression.Unary(hydra.ext.java.Utils.javaPrimaryToJavaUnaryExpression(hydra.ext.java.Utils.javaLiteralToJavaPrimary(hydra.ext.java.Utils.javaInt(i))));
    hydra.util.Lazy<hydra.ext.java.syntax.UnaryExpression> rhs = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaPostfixExpressionToJavaUnaryExpression(hydra.ext.java.Utils.javaMethodInvocationToJavaPostfixExpression(new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Type(hydra.ext.java.Utils.javaTypeName(new hydra.ext.java.syntax.Identifier("java.util.Objects"))), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.hashCodeMethodName()))), hydra.util.ConsList.of(hydra.ext.java.Utils.javaExpressionNameToJavaExpression(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.Utils.sanitizeJavaName(fnameStr)))))))));
    return new hydra.ext.java.syntax.MultiplicativeExpression.Times(new hydra.ext.java.syntax.MultiplicativeExpression_Binary(lhs, rhs.get()));
  }

  static hydra.ext.java.syntax.Identifier innerClassRef(hydra.ext.java.environment.Aliases aliases, hydra.core.Name name, String local) {
    String id = hydra.ext.java.Utils.nameToJavaName(
      aliases,
      name).value;
    return new hydra.ext.java.syntax.Identifier(hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        id,
        "."),
      local));
  }

  static hydra.ext.java.environment.JavaEnvironment insertBranchVar(hydra.core.Name name, hydra.ext.java.environment.JavaEnvironment env) {
    hydra.ext.java.environment.Aliases aliases = (env).aliases;
    return new hydra.ext.java.environment.JavaEnvironment(new hydra.ext.java.environment.Aliases((aliases).currentNamespace, (aliases).packages, hydra.lib.sets.Insert.apply(
      name,
      (aliases).branchVars), (aliases).recursiveVars, (aliases).inScopeTypeParams, (aliases).polymorphicLocals, (aliases).inScopeJavaVars, (aliases).varRenames, (aliases).lambdaVars, (aliases).typeVarSubst, (aliases).trustedTypeVars, (aliases).methodCodomain, (aliases).thunkedVars), (env).graph);
  }

  static hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType> interfaceTypes(Boolean isSer, hydra.ext.java.environment.Aliases aliases, hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName) {
    hydra.util.Lazy<hydra.ext.java.syntax.TypeArgument> selfTypeArg = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.TypeArgument.Reference(hydra.ext.java.Utils.nameToJavaReferenceType(
      aliases,
      false,
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument>) (tp_ -> hydra.ext.java.Utils.typeParameterToTypeArgument(tp_)),
        tparams),
      elName,
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
    hydra.util.Lazy<hydra.ext.java.syntax.InterfaceType> javaComparableType = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.InterfaceType(new hydra.ext.java.syntax.ClassType((hydra.util.ConsList<hydra.ext.java.syntax.Annotation>) (hydra.util.ConsList.<hydra.ext.java.syntax.Annotation>empty()), new hydra.ext.java.syntax.ClassTypeQualifier.None(), hydra.ext.java.Utils.javaTypeIdentifier("Comparable"), hydra.util.ConsList.of(selfTypeArg.get()))));
    hydra.util.Lazy<hydra.ext.java.syntax.InterfaceType> javaSerializableType = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.InterfaceType(new hydra.ext.java.syntax.ClassType((hydra.util.ConsList<hydra.ext.java.syntax.Annotation>) (hydra.util.ConsList.<hydra.ext.java.syntax.Annotation>empty()), new hydra.ext.java.syntax.ClassTypeQualifier.None(), hydra.ext.java.Utils.javaTypeIdentifier("Serializable"), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()))));
    return hydra.lib.logic.IfElse.lazy(
      isSer,
      () -> hydra.util.ConsList.of(
        javaSerializableType.get(),
        javaComparableType.get()),
      () -> (hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType>) (hydra.util.ConsList.<hydra.ext.java.syntax.InterfaceType>empty()));
  }

  static Boolean isBigNumericType(hydra.core.Type typ) {
    return hydra.Rewriting.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Type.Literal lt) {
        return (lt).value.accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.LiteralType instance) {
            return false;
          }

          @Override
          public Boolean visit(hydra.core.LiteralType.Float_ ft) {
            return (ft).value.accept(new hydra.core.FloatType.PartialVisitor<>() {
              @Override
              public Boolean otherwise(hydra.core.FloatType instance) {
                return false;
              }

              @Override
              public Boolean visit(hydra.core.FloatType.Bigfloat ignored) {
                return true;
              }
            });
          }

          @Override
          public Boolean visit(hydra.core.LiteralType.Integer_ it) {
            return (it).value.accept(new hydra.core.IntegerType.PartialVisitor<>() {
              @Override
              public Boolean otherwise(hydra.core.IntegerType instance) {
                return false;
              }

              @Override
              public Boolean visit(hydra.core.IntegerType.Bigint ignored) {
                return true;
              }
            });
          }
        });
      }
    });
  }

  static Boolean isBinaryType(hydra.core.Type typ) {
    return hydra.Rewriting.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Type.Literal lt) {
        return (lt).value.accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.LiteralType instance) {
            return false;
          }

          @Override
          public Boolean visit(hydra.core.LiteralType.Binary ignored) {
            return true;
          }
        });
      }
    });
  }

  static <T0, T1> hydra.util.Either<T1, Boolean> isFieldUnitType(hydra.core.Name typeName, hydra.core.Name fieldName, T0 cx, hydra.graph.Graph g) {
    hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme> schemaTypes = (g).schemaTypes;
    return hydra.lib.maybes.Cases.applyLazy(
      hydra.lib.maps.Lookup.apply(
        typeName,
        schemaTypes),
      () -> hydra.util.Either.<T1, Boolean>right(false),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<T1, Boolean>>) (ts -> hydra.Rewriting.deannotateType((ts).type).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.Either<T1, Boolean> otherwise(hydra.core.Type instance) {
          return hydra.util.Either.<T1, Boolean>right(false);
        }

        @Override
        public hydra.util.Either<T1, Boolean> visit(hydra.core.Type.Union rt) {
          return hydra.util.Either.<T1, Boolean>right(hydra.lib.maybes.Cases.applyLazy(
            hydra.lib.lists.Find.apply(
              (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.equality.Equal.apply(
                (ft).name,
                fieldName)),
              (rt).value),
            () -> false,
            (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.Schemas.isUnitType(hydra.Rewriting.deannotateType((ft).type)))));
        }
      })));
  }

  static Boolean isLambdaBoundIn(hydra.core.Name name, hydra.util.PersistentSet<hydra.core.Name> lambdaVars) {
    return hydra.lib.logic.Or.apply(
      hydra.lib.sets.Member.apply(
        name,
        lambdaVars),
      hydra.lib.logic.Or.apply(
        hydra.lib.logic.And.apply(
          hydra.ext.java.Coder.isLambdaBoundIn_isQualified(name),
          hydra.lib.maybes.IsJust.apply(hydra.lib.lists.Find.apply(
            (java.util.function.Function<hydra.core.Name, Boolean>) (lv -> hydra.lib.logic.And.apply(
              hydra.ext.java.Coder.isLambdaBoundIn_isQualified(lv),
              hydra.lib.equality.Equal.apply(
                hydra.Names.localNameOf(lv),
                hydra.Names.localNameOf(name)))),
            hydra.lib.sets.ToList.apply(lambdaVars)))),
        hydra.lib.logic.And.apply(
          hydra.lib.logic.Not.apply(hydra.ext.java.Coder.isLambdaBoundIn_isQualified(name)),
          hydra.lib.sets.Member.apply(
            new hydra.core.Name(hydra.Names.localNameOf(name)),
            lambdaVars))));
  }

  static Boolean isLambdaBoundIn_isQualified(hydra.core.Name n) {
    return hydra.lib.maybes.IsJust.apply(hydra.Names.qualifyName(n).namespace);
  }

  static Boolean isLambdaBoundVariable(hydra.core.Name name) {
    String v = (name).value;
    return hydra.lib.equality.Lte.apply(
      hydra.lib.strings.Length.apply(v),
      4);
  }

  static Boolean isLocalVariable(hydra.core.Name name) {
    return hydra.lib.maybes.IsNothing.apply(hydra.Names.qualifyName(name).namespace);
  }

  static Boolean isNonComparableType(hydra.core.Type typ) {
    return hydra.Rewriting.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Type.Either ignored) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Type.Function ignored) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Type.Unit ignored) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Type.Literal lt) {
        return (lt).value.accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.LiteralType instance) {
            return false;
          }

          @Override
          public Boolean visit(hydra.core.LiteralType.Binary ignored) {
            return true;
          }
        });
      }

      @Override
      public Boolean visit(hydra.core.Type.Forall ft) {
        return hydra.ext.java.Coder.isNonComparableType((ft).value.body);
      }
    });
  }

  static Boolean isRecursiveVariable(hydra.ext.java.environment.Aliases aliases, hydra.core.Name name) {
    return hydra.lib.sets.Member.apply(
      name,
      (aliases).recursiveVars);
  }

  static Boolean isSerializableJavaType(hydra.core.Type typ) {
    return hydra.Schemas.isNominalType(typ);
  }

  static Boolean isSimpleName(hydra.core.Name name) {
    return hydra.lib.equality.Equal.apply(
      hydra.lib.lists.Length.apply(hydra.lib.strings.SplitOn.apply(
        ".",
        (name).value)),
      1);
  }

  static Boolean isUnresolvedInferenceVar(hydra.core.Name name) {
    hydra.util.ConsList<Integer> chars = hydra.lib.strings.ToList.apply((name).value);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(chars),
      () -> false,
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Head.apply(chars),
          116)),
        () -> false,
        () -> ((java.util.function.Supplier<Boolean>) (() -> {
          hydra.util.Lazy<hydra.util.ConsList<Integer>> rest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(chars));
          return hydra.lib.logic.And.apply(
            hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(rest.get())),
            hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
              (java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.Not.apply(hydra.ext.java.Coder.isUnresolvedInferenceVar_isDigit(c))),
              rest.get())));
        })).get()));
  }

  static Boolean isUnresolvedInferenceVar_isDigit(Integer c) {
    return hydra.lib.logic.And.apply(
      hydra.lib.equality.Gte.apply(
        c,
        48),
      hydra.lib.equality.Lte.apply(
        c,
        57));
  }

  static hydra.ext.java.environment.JavaFeatures java11Features() {
    return new hydra.ext.java.environment.JavaFeatures(true);
  }

  static hydra.ext.java.environment.JavaFeatures java8Features() {
    return new hydra.ext.java.environment.JavaFeatures(false);
  }

  static hydra.ext.java.syntax.ReferenceType javaComparableRefType() {
    return new hydra.ext.java.syntax.ReferenceType.ClassOrInterface(new hydra.ext.java.syntax.ClassOrInterfaceType.Class_(new hydra.ext.java.syntax.ClassType((hydra.util.ConsList<hydra.ext.java.syntax.Annotation>) (hydra.util.ConsList.<hydra.ext.java.syntax.Annotation>empty()), new hydra.ext.java.syntax.ClassTypeQualifier.None(), hydra.ext.java.Utils.javaTypeIdentifier("Comparable"), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()))));
  }

  static hydra.graph.Graph javaEnvGetGraph(hydra.ext.java.environment.JavaEnvironment env) {
    return (env).graph;
  }

  static hydra.ext.java.environment.JavaEnvironment javaEnvSetGraph(hydra.graph.Graph g, hydra.ext.java.environment.JavaEnvironment env) {
    return new hydra.ext.java.environment.JavaEnvironment((env).aliases, g);
  }

  static hydra.ext.java.environment.JavaFeatures javaFeatures() {
    return hydra.ext.java.Coder.java11Features();
  }

  static String javaIdentifierToString(hydra.ext.java.syntax.Identifier id) {
    return (id).value;
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>> javaTypeArgumentsForNamedType(hydra.core.Name tname, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Schemas.requireType(
        cx,
        g,
        tname),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>>) (typ -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>right(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument>) (tp_ -> hydra.ext.java.Utils.typeParameterToTypeArgument(tp_)),
        hydra.ext.java.Coder.javaTypeParametersForType(typ)))));
  }

  static hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument> javaTypeArgumentsForType(hydra.core.Type typ) {
    return hydra.lib.lists.Reverse.apply(hydra.lib.lists.Map.apply(
      hydra.ext.java.Utils::typeParameterToTypeArgument,
      hydra.ext.java.Coder.javaTypeParametersForType(typ)));
  }

  static hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter> javaTypeParametersForType(hydra.core.Type typ) {
    hydra.util.ConsList<hydra.core.Name> boundVars = hydra.ext.java.Coder.javaTypeParametersForType_bvars(typ);
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> freeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.ext.java.Coder.isLambdaBoundVariable(v)),
      hydra.lib.sets.ToList.apply(hydra.Rewriting.freeVariablesInType(typ))));
    java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.TypeParameter> toParam = (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.TypeParameter>) (name -> hydra.ext.java.Utils.javaTypeParameter(hydra.Formatting.capitalize((name).value)));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> vars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Nub.apply(hydra.lib.lists.Concat2.apply(
      boundVars,
      freeVars.get())));
    return hydra.lib.lists.Map.apply(
      toParam,
      vars.get());
  }

  static hydra.util.ConsList<hydra.core.Name> javaTypeParametersForType_bvars(hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.lib.lists.Cons.apply(
          (ft).value.parameter,
          hydra.ext.java.Coder.javaTypeParametersForType_bvars((ft).value.body));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<String, String>> moduleToJava(hydra.module.Module mod, hydra.util.ConsList<hydra.module.Definition> defs, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.java.Coder.encodeDefinitions(
        mod,
        defs,
        cx,
        g),
      (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<String, String>>>) (units -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<String, String>>right(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.ext.java.syntax.CompilationUnit>, hydra.util.Pair<String, String>>) (entry -> {
          hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(entry));
          hydra.util.Lazy<hydra.ext.java.syntax.CompilationUnit> unit = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(entry));
          return (hydra.util.Pair<String, String>) ((hydra.util.Pair<String, String>) (new hydra.util.Pair<String, String>(hydra.ext.java.Coder.bindingNameToFilePath(name.get()), hydra.Serialization.printExpr(hydra.Serialization.parenthesize(hydra.ext.java.Serde.writeCompilationUnit(unit.get()))))));
        }),
        hydra.lib.maps.ToList.apply(units))))));
  }

  static <T0> hydra.util.PersistentMap<T0, hydra.core.Type> nameMapToTypeMap(hydra.util.PersistentMap<T0, hydra.core.Name> m) {
    return hydra.lib.maps.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (v -> new hydra.core.Type.Variable(v)),
      m);
  }

  static hydra.util.Maybe<hydra.module.Namespace> namespaceParent(hydra.module.Namespace ns) {
    hydra.util.ConsList<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      (ns).value);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(hydra.lib.lists.Init.apply(parts)),
      () -> (hydra.util.Maybe<hydra.module.Namespace>) (hydra.util.Maybe.<hydra.module.Namespace>nothing()),
      () -> hydra.util.Maybe.just(new hydra.module.Namespace(hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Init.apply(parts)))));
  }

  static Boolean needsThunking(hydra.core.Term t) {
    return hydra.Rewriting.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (b -> (java.util.function.Function<hydra.core.Term, Boolean>) (st -> hydra.lib.logic.Or.apply(
            b,
            hydra.ext.java.Coder.needsThunking(st)))),
          false,
          hydra.Rewriting.subterms(t));
      }

      @Override
      public Boolean visit(hydra.core.Term.Let _lt) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Term.TypeApplication _ta) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Term.TypeLambda _tl) {
        return true;
      }
    });
  }

  static hydra.ext.java.syntax.ClassBodyDeclarationWithComments noComment(hydra.ext.java.syntax.ClassBodyDeclaration decl) {
    return new hydra.ext.java.syntax.ClassBodyDeclarationWithComments(decl, (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> otherwiseBranch(hydra.ext.java.environment.JavaEnvironment env, hydra.ext.java.environment.Aliases aliases, hydra.core.Type dom, hydra.core.Type cod, hydra.core.Name tname, hydra.ext.java.syntax.Type jcod, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument> targs, hydra.core.Term d, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.util.ConsList<hydra.ext.java.syntax.Annotation> anns = hydra.util.ConsList.of(hydra.ext.java.Utils.overrideAnnotation());
    hydra.util.Lazy<hydra.ext.java.syntax.Type> jdom = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.Utils.nameToJavaReferenceType(
      aliases,
      true,
      targs,
      tname,
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
    hydra.util.ConsList<hydra.ext.java.syntax.MethodModifier> mods = hydra.util.ConsList.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.Utils.javaTypeToJavaFormalParameter(
      jdom.get(),
      new hydra.core.Name("instance"));
    hydra.ext.java.syntax.Result result = new hydra.ext.java.syntax.Result.Type(new hydra.ext.java.syntax.UnannType(jcod));
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.java.Coder.analyzeJavaFunction(
        env,
        d,
        cx,
        g),
      (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (fs -> {
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.ConsList<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
        hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env2 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.ext.java.environment.JavaEnvironment>) (projected -> projected.environment)).apply(fs));
        hydra.util.Lazy<hydra.core.Term> rawBody = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
        hydra.core.Term innerBody = hydra.ext.java.Coder.annotateBodyWithCod(
          cod,
          rawBody.get());
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Coder.bindingsToStatements(
            env2.get(),
            bindings.get(),
            cx,
            g),
          (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (bindResult -> {
            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> bindingStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
            hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.encodeTerm(
                env3.get(),
                innerBody,
                cx,
                g),
              (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (jret -> {
                hydra.ext.java.syntax.BlockStatement returnStmt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(jret)));
                hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> allStmts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                  bindingStmts.get(),
                  hydra.util.ConsList.of(returnStmt)));
                return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>right(hydra.ext.java.Coder.noComment(hydra.ext.java.Utils.methodDeclaration(
                  mods,
                  (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()),
                  anns,
                  hydra.ext.java.Names.otherwiseMethodName(),
                  hydra.util.ConsList.of(param),
                  result,
                  hydra.util.Maybe.just(allStmts.get()))));
              }));
          }));
      }));
  }

  static hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type> peelDomainTypes(Integer n, hydra.core.Type t) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Lte.apply(
        n,
        0),
      () -> (hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), t))),
      () -> hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type> otherwise(hydra.core.Type instance) {
          return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), t)));
        }

        @Override
        public hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Function ft) {
          hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type> rest = hydra.ext.java.Coder.peelDomainTypes(
            hydra.lib.math.Sub.apply(
              n,
              1),
            (ft).value.codomain);
          return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Cons.apply(
            (ft).value.domain,
            hydra.lib.pairs.First.apply(rest)), hydra.lib.pairs.Second.apply(rest))));
        }
      }));
  }

  static hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type> peelDomainsAndCod(Integer n, hydra.core.Type t) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Lte.apply(
        n,
        0),
      () -> (hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), t))),
      () -> hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type> otherwise(hydra.core.Type instance) {
          return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), t)));
        }

        @Override
        public hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type> visit(hydra.core.Type.Function ft) {
          hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type> rest = hydra.ext.java.Coder.peelDomainsAndCod(
            hydra.lib.math.Sub.apply(
              n,
              1),
            (ft).value.codomain);
          return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Type>(hydra.lib.lists.Cons.apply(
            (ft).value.domain,
            hydra.lib.pairs.First.apply(rest)), hydra.lib.pairs.Second.apply(rest))));
        }
      }));
  }

  static hydra.util.ConsList<hydra.core.Type> peelExpectedTypes(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> subst, Integer n, hydra.core.Type t) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        n,
        0),
      () -> (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
      () -> hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.ConsList<hydra.core.Type> otherwise(hydra.core.Type instance) {
          return (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty());
        }

        @Override
        public hydra.util.ConsList<hydra.core.Type> visit(hydra.core.Type.Function ft) {
          return hydra.lib.lists.Cons.apply(
            hydra.ext.java.Coder.applySubstFull(
              subst,
              (ft).value.domain),
            hydra.ext.java.Coder.peelExpectedTypes(
              subst,
              hydra.lib.math.Sub.apply(
                n,
                1),
              (ft).value.codomain));
        }
      }));
  }

  static hydra.core.Term propagateType(hydra.core.Type typ, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, hydra.core.Term> setTypeAnn = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> hydra.Annotations.setTermAnnotation(
      hydra.Constants.key_type(),
      hydra.util.Maybe.just(hydra.encode.Core.type(typ)),
      t));
    return hydra.Rewriting.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (setTypeAnn).apply(term);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return (setTypeAnn).apply(term);
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda lam) {
            hydra.core.Term annotated = (setTypeAnn).apply(term);
            return hydra.Rewriting.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
              @Override
              public hydra.core.Term otherwise(hydra.core.Type instance) {
                return annotated;
              }

              @Override
              public hydra.core.Term visit(hydra.core.Type.Function ft) {
                return hydra.ext.java.Coder.propagateType_propagateIntoLambda(
                  (ft).value.codomain,
                  annotated);
              }
            });
          }
        });
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return (setTypeAnn).apply(hydra.ext.java.Coder.propagateType_rebuildLet(
          term,
          (lt).value.bindings,
          hydra.ext.java.Coder.propagateType(
            typ,
            (lt).value.body)));
      }
    });
  }

  static hydra.core.Term propagateType_propagateIntoLambda(hydra.core.Type cod, hydra.core.Term t) {
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.ext.java.Coder.propagateType_propagateIntoLambda(
          cod,
          (at).value.body), (at).value.annotation));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return t;
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda lam) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((lam).value.parameter, (lam).value.domain, hydra.ext.java.Coder.propagateType(
              cod,
              (lam).value.body))));
          }
        });
      }
    });
  }

  static hydra.core.Term propagateType_rebuildLet(hydra.core.Term t, hydra.util.ConsList<hydra.core.Binding> bindings, hydra.core.Term newBody) {
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.ext.java.Coder.propagateType_rebuildLet(
          (at).value.body,
          bindings,
          newBody), (at).value.annotation));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Let _lt) {
        return new hydra.core.Term.Let(new hydra.core.Let(bindings, newBody));
      }
    });
  }

  static hydra.core.Term propagateTypesInAppChain(hydra.core.Type fixedCod, hydra.core.Type resultType, hydra.core.Term t) {
    hydra.util.Lazy<hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>> flattened = new hydra.util.Lazy<>(() -> hydra.ext.java.Coder.flattenApps(
      t,
      (hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> args = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(flattened.get()));
    hydra.util.Lazy<hydra.core.Term> fun = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(flattened.get()));
    hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.core.Term> lambdaDomsResult = hydra.ext.java.Coder.collectLambdaDomains(fun.get());
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> lambdaDoms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(lambdaDomsResult));
    hydra.util.Lazy<Integer> nArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(args.get()));
    hydra.util.Lazy<Integer> nLambdaDoms = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(lambdaDoms.get()));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Gt.apply(
          nLambdaDoms.get(),
          0),
        hydra.lib.equality.Gt.apply(
          nArgs.get(),
          0)),
      () -> ((java.util.function.Supplier<hydra.core.Term>) (() -> {
        hydra.util.Lazy<hydra.core.Type> bodyRetType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.ext.java.Coder.peelDomainsAndCod(
          hydra.lib.math.Sub.apply(
            nLambdaDoms.get(),
            nArgs.get()),
          resultType)));
        return ((java.util.function.Supplier<hydra.core.Term>) (() -> {
          hydra.util.Lazy<hydra.core.Type> funType = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (c -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (d -> new hydra.core.Type.Function(new hydra.core.FunctionType(d, c)))),
            bodyRetType.get(),
            hydra.lib.lists.Reverse.apply(lambdaDoms.get())));
          return ((java.util.function.Supplier<hydra.core.Term>) (() -> {
            hydra.core.Term annotatedFun = hydra.Annotations.setTermAnnotation(
              hydra.Constants.key_type(),
              hydra.util.Maybe.just(hydra.encode.Core.type(funType.get())),
              fun.get());
            return hydra.ext.java.Coder.rebuildApps(
              annotatedFun,
              args.get(),
              funType.get());
          })).get();
        })).get();
      })).get(),
      () -> hydra.Rewriting.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return hydra.Annotations.setTermAnnotation(
            hydra.Constants.key_type(),
            hydra.util.Maybe.just(hydra.encode.Core.type(resultType)),
            t);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Application app) {
          hydra.core.Term lhs = (app).value.function;
          hydra.util.Lazy<hydra.core.Term> annotatedLhs = new hydra.util.Lazy<>(() -> hydra.Rewriting.deannotateTerm(lhs).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.core.Term otherwise(hydra.core.Term instance) {
              return lhs;
            }

            @Override
            public hydra.core.Term visit(hydra.core.Term.Function fn) {
              return (fn).value.accept(new hydra.core.Function.PartialVisitor<>() {
                @Override
                public hydra.core.Term otherwise(hydra.core.Function instance) {
                  return lhs;
                }

                @Override
                public hydra.core.Term visit(hydra.core.Function.Elimination elim) {
                  return (elim).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                    @Override
                    public hydra.core.Term otherwise(hydra.core.Elimination instance) {
                      return lhs;
                    }

                    @Override
                    public hydra.core.Term visit(hydra.core.Elimination.Union cs) {
                      hydra.util.Lazy<hydra.core.Type> dom = new hydra.util.Lazy<>(() -> hydra.Schemas.nominalApplication(
                        (cs).value.typeName,
                        (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())));
                      hydra.core.Type ft = new hydra.core.Type.Function(new hydra.core.FunctionType(dom.get(), fixedCod));
                      return hydra.Annotations.setTermAnnotation(
                        hydra.Constants.key_type(),
                        hydra.util.Maybe.just(hydra.encode.Core.type(ft)),
                        lhs);
                    }
                  });
                }
              });
            }
          }));
          hydra.core.Term rhs = (app).value.argument;
          return hydra.Annotations.setTermAnnotation(
            hydra.Constants.key_type(),
            hydra.util.Maybe.just(hydra.encode.Core.type(resultType)),
            new hydra.core.Term.Application(new hydra.core.Application(annotatedLhs.get(), rhs)));
        }
      }));
  }

  static hydra.core.Term rebuildApps(hydra.core.Term f, hydra.util.ConsList<hydra.core.Term> args, hydra.core.Type fType) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(args),
      () -> f,
      () -> hydra.Rewriting.deannotateType(fType).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Type instance) {
          return hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (a -> new hydra.core.Term.Application(new hydra.core.Application(acc, a)))),
            f,
            args);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Type.Function ft) {
          hydra.util.Lazy<hydra.core.Term> arg = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(args));
          hydra.core.Term app = new hydra.core.Term.Application(new hydra.core.Application(f, arg.get()));
          hydra.core.Type remainingType = (ft).value.codomain;
          hydra.core.Term annotatedApp = hydra.Annotations.setTermAnnotation(
            hydra.Constants.key_type(),
            hydra.util.Maybe.just(hydra.encode.Core.type(remainingType)),
            app);
          hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> rest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(args));
          return hydra.ext.java.Coder.rebuildApps(
            annotatedApp,
            rest.get(),
            remainingType);
        }
      }));
  }

  static <T0> hydra.ext.java.syntax.ClassBodyDeclaration recordCompareToMethod(hydra.ext.java.environment.Aliases aliases, T0 tparams, hydra.core.Name elName, hydra.util.ConsList<hydra.core.FieldType> fields) {
    hydra.util.ConsList<hydra.ext.java.syntax.Annotation> anns = hydra.util.ConsList.of(
      hydra.ext.java.Utils.overrideAnnotation(),
      hydra.ext.java.Utils.suppressWarningsUncheckedAnnotation());
    hydra.util.ConsList<hydra.ext.java.syntax.MethodModifier> mods = hydra.util.ConsList.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.Utils.javaTypeToJavaFormalParameter(
      hydra.ext.java.Utils.javaTypeFromTypeName(
        aliases,
        elName),
      new hydra.core.Name(hydra.ext.java.Names.otherInstanceName()));
    hydra.ext.java.syntax.Result result = hydra.ext.java.Utils.javaTypeToJavaResult(hydra.ext.java.Utils.javaIntType());
    return hydra.ext.java.Utils.methodDeclaration(
      mods,
      (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()),
      anns,
      hydra.ext.java.Names.compareToMethodName(),
      hydra.util.ConsList.of(param),
      result,
      hydra.util.Maybe.just(hydra.ext.java.Coder.compareToBody(
        aliases,
        hydra.ext.java.Names.otherInstanceName(),
        fields)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclaration> recordConstructor(hydra.ext.java.environment.Aliases aliases, hydra.core.Name elName, hydra.util.ConsList<hydra.core.FieldType> fields, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> assignStmts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.ext.java.syntax.BlockStatement>) (f -> new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.toAssignStmt((f).name))),
      fields));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.FormalParameter>>) (f -> hydra.ext.java.Coder.fieldTypeToFormalParam(
          aliases,
          f,
          cx,
          g)),
        fields),
      (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.FormalParameter>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclaration>>) (params -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclaration>right(hydra.ext.java.Utils.makeConstructor(
        aliases,
        elName,
        false,
        params,
        assignStmts.get()))));
  }

  static hydra.ext.java.syntax.ClassBodyDeclaration recordEqualsMethod(hydra.ext.java.environment.Aliases aliases, hydra.core.Name elName, hydra.util.ConsList<hydra.core.FieldType> fields) {
    hydra.util.ConsList<hydra.ext.java.syntax.Annotation> anns = hydra.util.ConsList.of(hydra.ext.java.Utils.overrideAnnotation());
    String tmpName = "o";
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> castStmt = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.variableDeclarationStatement(
      aliases,
      hydra.ext.java.Utils.javaTypeFromTypeName(
        aliases,
        elName),
      hydra.ext.java.Utils.javaIdentifier(tmpName),
      hydra.ext.java.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.Utils.javaCastExpression(
        hydra.ext.java.Utils.nameToJavaReferenceType(
          aliases,
          false,
          (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()),
          elName,
          (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
        hydra.ext.java.Utils.javaIdentifierToJavaUnaryExpression(new hydra.ext.java.syntax.Identifier(hydra.ext.java.Utils.sanitizeJavaName(hydra.ext.java.Names.otherInstanceName())))))));
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> instanceOfStmt = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.BlockStatement.Statement(new hydra.ext.java.syntax.Statement.IfThen(new hydra.ext.java.syntax.IfThenStatement(hydra.ext.java.Utils.javaUnaryExpressionToJavaExpression(new hydra.ext.java.syntax.UnaryExpression.Other(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Not(hydra.ext.java.Utils.javaRelationalExpressionToJavaUnaryExpression(hydra.ext.java.Utils.javaInstanceOf(
      hydra.ext.java.Utils.javaIdentifierToJavaRelationalExpression(hydra.ext.java.Utils.javaIdentifier(hydra.ext.java.Names.otherInstanceName())),
      hydra.ext.java.Utils.nameToJavaReferenceType(
        aliases,
        false,
        (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()),
        elName,
        (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))))))), hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.Utils.javaBooleanExpression(false)))))));
    hydra.util.ConsList<hydra.ext.java.syntax.MethodModifier> mods = hydra.util.ConsList.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.util.Lazy<hydra.ext.java.syntax.FormalParameter> param = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaTypeToJavaFormalParameter(
      hydra.ext.java.Utils.javaRefType(
        (hydra.util.ConsList<hydra.ext.java.syntax.ReferenceType>) (hydra.util.ConsList.<hydra.ext.java.syntax.ReferenceType>empty()),
        (hydra.util.Maybe<hydra.ext.java.syntax.PackageName>) (hydra.util.Maybe.<hydra.ext.java.syntax.PackageName>nothing()),
        "Object"),
      new hydra.core.Name(hydra.ext.java.Names.otherInstanceName())));
    hydra.ext.java.syntax.Result result = hydra.ext.java.Utils.javaTypeToJavaResult(hydra.ext.java.Utils.javaBooleanType());
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> returnAllFieldsEqual = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(fields),
      () -> hydra.ext.java.Utils.javaBooleanExpression(true),
      () -> hydra.ext.java.Utils.javaConditionalAndExpressionToJavaExpression(new hydra.ext.java.syntax.ConditionalAndExpression(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.ext.java.syntax.InclusiveOrExpression>) (f -> hydra.ext.java.Coder.eqClause(
          tmpName,
          f)),
        fields))))))));
    return hydra.ext.java.Utils.methodDeclaration(
      mods,
      (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()),
      anns,
      hydra.ext.java.Names.equalsMethodName(),
      hydra.util.ConsList.of(param.get()),
      result,
      hydra.util.Maybe.just(hydra.util.ConsList.of(
        instanceOfStmt.get(),
        castStmt.get(),
        returnAllFieldsEqual.get())));
  }

  static hydra.ext.java.syntax.ClassBodyDeclaration recordHashCodeMethod(hydra.util.ConsList<hydra.core.FieldType> fields) {
    hydra.util.ConsList<hydra.ext.java.syntax.Annotation> anns = hydra.util.ConsList.of(hydra.ext.java.Utils.overrideAnnotation());
    hydra.util.ConsList<hydra.ext.java.syntax.MethodModifier> mods = hydra.util.ConsList.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.ext.java.syntax.Result result = hydra.ext.java.Utils.javaTypeToJavaResult(hydra.ext.java.Utils.javaIntType());
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> returnSum = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.BlockStatement.Statement(hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(fields),
      () -> hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.Utils.javaIntExpression(new java.math.BigInteger("0")))),
      () -> hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.Utils.javaAdditiveExpressionToJavaExpression(hydra.ext.java.Utils.addExpressions(hydra.lib.lists.ZipWith.apply(
        (java.util.function.Function<java.math.BigInteger, java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.MultiplicativeExpression>>) (p0 -> p1 -> hydra.ext.java.Coder.hashCodeMultPair(
          p0,
          p1)),
        hydra.ext.java.Coder.first20Primes(),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, hydra.core.Name>) (f -> (f).name),
          fields)))))))));
    return hydra.ext.java.Utils.methodDeclaration(
      mods,
      (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()),
      anns,
      hydra.ext.java.Names.hashCodeMethodName(),
      (hydra.util.ConsList<hydra.ext.java.syntax.FormalParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.FormalParameter>empty()),
      result,
      hydra.util.Maybe.just(hydra.util.ConsList.of(returnSum.get())));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclaration> recordMemberVar(hydra.ext.java.environment.Aliases aliases, hydra.core.FieldType ft, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.core.Name fname = (ft).name;
    hydra.core.Type ftype = (ft).type;
    hydra.util.ConsList<hydra.ext.java.syntax.FieldModifier> mods = hydra.util.ConsList.of(
      new hydra.ext.java.syntax.FieldModifier.Public(),
      new hydra.ext.java.syntax.FieldModifier.Final());
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.java.Coder.encodeType(
        aliases,
        (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
        ftype,
        cx,
        g),
      (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclaration>>) (jt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclaration>right(hydra.ext.java.Utils.javaMemberField(
        mods,
        jt,
        hydra.ext.java.Utils.fieldNameToJavaVariableDeclarator(fname)))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclaration> recordWithMethod(hydra.ext.java.environment.Aliases aliases, hydra.core.Name elName, hydra.util.ConsList<hydra.core.FieldType> fields, hydra.core.FieldType field, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.ext.java.syntax.Identifier consId = new hydra.ext.java.syntax.Identifier(hydra.ext.java.Utils.sanitizeJavaName(hydra.Names.localNameOf(elName)));
    hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.Expression>> fieldArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.ext.java.syntax.Expression>) (f -> hydra.ext.java.Utils.fieldNameToJavaExpression((f).name)),
      fields));
    String methodName = hydra.lib.strings.Cat2.apply(
      "with",
      hydra.Formatting.nonAlnumToUnderscores(hydra.Formatting.capitalize((field).name.value)));
    hydra.util.ConsList<hydra.ext.java.syntax.MethodModifier> mods = hydra.util.ConsList.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.util.Lazy<hydra.ext.java.syntax.Result> result = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.referenceTypeToResult(hydra.ext.java.Utils.nameToJavaReferenceType(
      aliases,
      false,
      (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()),
      elName,
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> returnStmt = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.Utils.javaConstructorCall(
      hydra.ext.java.Utils.javaConstructorName(
        consId,
        (hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond>) (hydra.util.Maybe.<hydra.ext.java.syntax.TypeArgumentsOrDiamond>nothing())),
      fieldArgs.get(),
      (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing()))))));
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.java.Coder.fieldTypeToFormalParam(
        aliases,
        field,
        cx,
        g),
      (java.util.function.Function<hydra.ext.java.syntax.FormalParameter, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclaration>>) (param -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclaration>right(hydra.ext.java.Utils.methodDeclaration(
        mods,
        (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()),
        hydra.ext.java.Coder.<hydra.ext.java.syntax.Annotation>recordWithMethod_anns(),
        methodName,
        hydra.util.ConsList.of(param),
        result.get(),
        hydra.util.Maybe.just(hydra.util.ConsList.of(returnStmt.get()))))));
  }

  static <T0> hydra.util.ConsList<T0> recordWithMethod_anns() {
    return (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty());
  }

  static hydra.util.ConsList<hydra.core.Type> resolveTypeApps(hydra.util.ConsList<hydra.core.Name> schemeVars, hydra.util.ConsList<hydra.core.Type> fallbackTypeApps, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> argSubst) {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> resolvedVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply(argSubst)));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> unresolvedVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
        v,
        resolvedVars.get()))),
      schemeVars));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Type>> usedTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maps.Elems.apply(argSubst)));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> unusedIrTypes = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Type, Boolean>) (t -> hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
        t,
        usedTypes.get()))),
      fallbackTypeApps));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> remainingSubst = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      unresolvedVars.get(),
      unusedIrTypes.get())));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> fullSubst = new hydra.util.Lazy<>(() -> hydra.lib.maps.Union.apply(
      argSubst,
      remainingSubst.get()));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (v -> hydra.lib.maps.FindWithDefault.applyLazy(
        () -> new hydra.core.Type.Variable(v),
        v,
        fullSubst.get())),
      schemeVars);
  }

  static <T0> hydra.util.PersistentMap<T0, T0> selfRefSubstitution(hydra.util.PersistentMap<T0, hydra.util.ConsList<T0>> grouped) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.PersistentMap<T0, T0>, java.util.function.Function<hydra.util.Pair<T0, hydra.util.ConsList<T0>>, hydra.util.PersistentMap<T0, T0>>>) (subst -> (java.util.function.Function<hydra.util.Pair<T0, hydra.util.ConsList<T0>>, hydra.util.PersistentMap<T0, T0>>) (entry -> hydra.ext.java.Coder.<T0>selfRefSubstitution_processGroup(
        subst,
        hydra.lib.pairs.First.apply(entry),
        hydra.lib.pairs.Second.apply(entry)))),
      (hydra.util.PersistentMap<T0, T0>) ((hydra.util.PersistentMap<T0, T0>) (hydra.lib.maps.Empty.<T0, T0>apply())),
      hydra.lib.maps.ToList.apply(grouped));
  }

  static <T0> hydra.util.PersistentMap<T0, T0> selfRefSubstitution_processGroup(hydra.util.PersistentMap<T0, T0> subst, T0 inVar, hydra.util.ConsList<T0> outVars) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Elem.apply(
        inVar,
        outVars),
      () -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.util.PersistentMap<T0, T0>, java.util.function.Function<T0, hydra.util.PersistentMap<T0, T0>>>) (s -> (java.util.function.Function<T0, hydra.util.PersistentMap<T0, T0>>) (v -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            v,
            inVar),
          () -> s,
          () -> hydra.lib.maps.Insert.apply(
            v,
            inVar,
            s)))),
        subst,
        outVars),
      () -> subst);
  }

  static hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType> serializableTypes(Boolean isSer) {
    hydra.util.Lazy<hydra.ext.java.syntax.InterfaceType> javaSerializableType = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.InterfaceType(new hydra.ext.java.syntax.ClassType((hydra.util.ConsList<hydra.ext.java.syntax.Annotation>) (hydra.util.ConsList.<hydra.ext.java.syntax.Annotation>empty()), new hydra.ext.java.syntax.ClassTypeQualifier.None(), hydra.ext.java.Utils.javaTypeIdentifier("Serializable"), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()))));
    return hydra.lib.logic.IfElse.lazy(
      isSer,
      () -> hydra.util.ConsList.of(javaSerializableType.get()),
      () -> (hydra.util.ConsList<hydra.ext.java.syntax.InterfaceType>) (hydra.util.ConsList.<hydra.ext.java.syntax.InterfaceType>empty()));
  }

  static hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMemberDeclaration> splitConstantInitializer(hydra.ext.java.syntax.InterfaceMemberDeclaration member) {
    return (member).accept(new hydra.ext.java.syntax.InterfaceMemberDeclaration.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMemberDeclaration> otherwise(hydra.ext.java.syntax.InterfaceMemberDeclaration instance) {
        return hydra.util.ConsList.of(member);
      }

      @Override
      public hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMemberDeclaration> visit(hydra.ext.java.syntax.InterfaceMemberDeclaration.Constant cd) {
        return hydra.lib.lists.Bind.apply(
          (cd).value.variables,
          (java.util.function.Function<hydra.ext.java.syntax.VariableDeclarator, hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (v1 -> hydra.ext.java.Coder.splitConstantInitializer_splitVar(
            (cd).value.modifiers,
            (cd).value.type,
            v1)));
      }
    });
  }

  static hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMemberDeclaration> splitConstantInitializer_splitVar(hydra.util.ConsList<hydra.ext.java.syntax.ConstantModifier> mods, hydra.ext.java.syntax.UnannType utype, hydra.ext.java.syntax.VariableDeclarator vd) {
    hydra.util.Maybe<hydra.ext.java.syntax.VariableInitializer> mInit = (vd).initializer;
    hydra.ext.java.syntax.VariableDeclaratorId vid = (vd).id;
    return hydra.lib.maybes.Cases.applyLazy(
      mInit,
      () -> hydra.util.ConsList.of(new hydra.ext.java.syntax.InterfaceMemberDeclaration.Constant(new hydra.ext.java.syntax.ConstantDeclaration(mods, utype, hydra.util.ConsList.of(vd)))),
      (java.util.function.Function<hydra.ext.java.syntax.VariableInitializer, hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMemberDeclaration>>) (init_ -> (init_).accept(new hydra.ext.java.syntax.VariableInitializer.PartialVisitor<>() {
        @Override
        public hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMemberDeclaration> otherwise(hydra.ext.java.syntax.VariableInitializer instance) {
          return hydra.util.ConsList.of(new hydra.ext.java.syntax.InterfaceMemberDeclaration.Constant(new hydra.ext.java.syntax.ConstantDeclaration(mods, utype, hydra.util.ConsList.of(vd))));
        }

        @Override
        public hydra.util.ConsList<hydra.ext.java.syntax.InterfaceMemberDeclaration> visit(hydra.ext.java.syntax.VariableInitializer.Expression expr) {
          String varName = hydra.ext.java.Coder.javaIdentifierToString((vid).identifier);
          String helperName = hydra.lib.strings.Cat2.apply(
            "_init_",
            varName);
          hydra.util.Lazy<hydra.ext.java.syntax.Expression> callExpr = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocation(
            (hydra.util.Maybe<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>) (hydra.util.Maybe.<hydra.util.Either<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>>nothing()),
            new hydra.ext.java.syntax.Identifier(helperName),
            (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()))));
          hydra.ext.java.syntax.InterfaceMemberDeclaration field = new hydra.ext.java.syntax.InterfaceMemberDeclaration.Constant(new hydra.ext.java.syntax.ConstantDeclaration(mods, utype, hydra.util.ConsList.of(new hydra.ext.java.syntax.VariableDeclarator(vid, hydra.util.Maybe.just(new hydra.ext.java.syntax.VariableInitializer.Expression(callExpr.get()))))));
          hydra.ext.java.syntax.Result resultType = new hydra.ext.java.syntax.Result.Type(utype);
          hydra.ext.java.syntax.BlockStatement returnSt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just((expr).value)));
          hydra.util.Lazy<hydra.ext.java.syntax.InterfaceMemberDeclaration> helper = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.interfaceMethodDeclaration(
            hydra.util.ConsList.of(
              new hydra.ext.java.syntax.InterfaceMethodModifier.Static(),
              new hydra.ext.java.syntax.InterfaceMethodModifier.Private()),
            (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()),
            helperName,
            (hydra.util.ConsList<hydra.ext.java.syntax.FormalParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.FormalParameter>empty()),
            resultType,
            hydra.util.Maybe.just(hydra.util.ConsList.of(returnSt))));
          return hydra.util.ConsList.of(
            field,
            helper.get());
        }
      })));
  }

  static hydra.core.Type stripForalls(hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall fa) {
        return hydra.ext.java.Coder.stripForalls((fa).value.body);
      }
    });
  }

  static hydra.core.Type substituteTypeVarsWithTypes(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> subst, hydra.core.Type t) {
    return hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
      subst,
      hydra.Rewriting.deannotateType(t));
  }

  static hydra.core.Type substituteTypeVarsWithTypes_go(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> subst, hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable v) {
        return hydra.lib.maybes.Cases.applyLazy(
          hydra.lib.maps.Lookup.apply(
            (v).value,
            subst),
          () -> t,
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (rep -> rep));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Function ft) {
        return new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (ft).value.domain), hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (ft).value.codomain)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Application at) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (at).value.function), hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (at).value.argument)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.List inner) {
        return new hydra.core.Type.List(hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (inner).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Set inner) {
        return new hydra.core.Type.Set(hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (inner).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Maybe inner) {
        return new hydra.core.Type.Maybe(hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (inner).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Map mt) {
        return new hydra.core.Type.Map(new hydra.core.MapType(hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (mt).value.keys), hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (mt).value.values)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Pair pt) {
        return new hydra.core.Type.Pair(new hydra.core.PairType(hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (pt).value.first), hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (pt).value.second)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Either et) {
        return new hydra.core.Type.Either(new hydra.core.EitherType(hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (et).value.left), hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (et).value.right)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Type.Forall(new hydra.core.ForallType((ft).value.parameter, hydra.ext.java.Coder.substituteTypeVarsWithTypes_go(
          subst,
          (ft).value.body)));
      }
    });
  }

  static hydra.ext.java.syntax.Expression tagCmpNotZeroExpr() {
    hydra.util.Lazy<hydra.ext.java.syntax.EqualityExpression> lhs = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaRelationalExpressionToJavaEqualityExpression(hydra.ext.java.Utils.javaPostfixExpressionToJavaRelationalExpression(new hydra.ext.java.syntax.PostfixExpression.Name(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), hydra.ext.java.Utils.javaIdentifier("tagCmp"))))));
    hydra.ext.java.syntax.RelationalExpression rhs = hydra.ext.java.Utils.javaPostfixExpressionToJavaRelationalExpression(new hydra.ext.java.syntax.PostfixExpression.Primary(hydra.ext.java.Utils.javaLiteralToJavaPrimary(hydra.ext.java.Utils.javaInt(new java.math.BigInteger("0")))));
    return hydra.ext.java.Utils.javaEqualityExpressionToJavaExpression(new hydra.ext.java.syntax.EqualityExpression.NotEqual(new hydra.ext.java.syntax.EqualityExpression_Binary(lhs.get(), rhs)));
  }

  static hydra.ext.java.syntax.Expression tagCompareExpr() {
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation> otherGetClass = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Expression(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.otherInstanceName()))), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier("getClass"))), (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty())));
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation> otherGetName = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Primary(hydra.ext.java.Utils.javaMethodInvocationToJavaPrimary(otherGetClass.get())), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier("getName"))), (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty())));
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation> thisGetClass = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Primary(hydra.ext.java.Utils.javaExpressionToJavaPrimary(hydra.ext.java.Utils.javaThis())), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier("getClass"))), (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty())));
    hydra.util.Lazy<hydra.ext.java.syntax.MethodInvocation> thisGetName = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Primary(hydra.ext.java.Utils.javaMethodInvocationToJavaPrimary(thisGetClass.get())), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier("getName"))), (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty())));
    return hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(new hydra.ext.java.syntax.MethodInvocation(new hydra.ext.java.syntax.MethodInvocation_Header.Complex(new hydra.ext.java.syntax.MethodInvocation_Complex(new hydra.ext.java.syntax.MethodInvocation_Variant.Primary(hydra.ext.java.Utils.javaMethodInvocationToJavaPrimary(thisGetName.get())), (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()), new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.compareToMethodName()))), hydra.util.ConsList.of(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(otherGetName.get()))));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>> takeTypeArgs(String label, Integer n, hydra.util.ConsList<hydra.ext.java.syntax.Type> tyapps, hydra.context.Context cx, T0 g) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Lt.apply(
        hydra.lib.lists.Length.apply(tyapps),
        n),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
        "needed type arguments for ",
        label,
        ", found too few")))), cx))),
      () -> hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (jt -> hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Utils.javaTypeToJavaReferenceType(
            jt,
            cx),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>right(new hydra.ext.java.syntax.TypeArgument.Reference(rt))))),
        hydra.lib.lists.Take.apply(
          n,
          tyapps)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration> toClassDecl(Boolean isInner, Boolean isSer, hydra.ext.java.environment.Aliases aliases, hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter> tparams, hydra.core.Name elName, hydra.core.Type t, hydra.context.Context cx, hydra.graph.Graph g) {
    java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>> wrap = (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration>>) (t_ -> hydra.ext.java.Coder.declarationForRecordType(
      isInner,
      isSer,
      aliases,
      tparams,
      elName,
      hydra.util.ConsList.of(new hydra.core.FieldType(new hydra.core.Name("value"), hydra.Rewriting.deannotateType(t_))),
      cx,
      g));
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration> otherwise(hydra.core.Type instance) {
        return (wrap).apply(t);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration> visit(hydra.core.Type.Record rt) {
        return hydra.ext.java.Coder.declarationForRecordType(
          isInner,
          isSer,
          aliases,
          tparams,
          elName,
          (rt).value,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration> visit(hydra.core.Type.Union rt) {
        return hydra.ext.java.Coder.declarationForUnionType(
          isSer,
          aliases,
          tparams,
          elName,
          (rt).value,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration> visit(hydra.core.Type.Forall fa) {
        hydra.core.Type body = (fa).value.body;
        hydra.core.Name v = (fa).value.parameter;
        hydra.ext.java.syntax.TypeParameter param = hydra.ext.java.Utils.javaTypeParameter(hydra.Formatting.capitalize((v).value));
        return hydra.ext.java.Coder.toClassDecl(
          false,
          isSer,
          aliases,
          hydra.lib.lists.Concat2.apply(
            tparams,
            hydra.util.ConsList.of(param)),
          elName,
          body,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassDeclaration> visit(hydra.core.Type.Wrap wt) {
        return hydra.ext.java.Coder.declarationForRecordType(
          isInner,
          isSer,
          aliases,
          tparams,
          elName,
          hydra.util.ConsList.of(new hydra.core.FieldType(new hydra.core.Name("value"), (wt).value)),
          cx,
          g);
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>> toDeclInit(hydra.ext.java.environment.Aliases aliasesExt, hydra.graph.Graph gExt, hydra.util.PersistentSet<hydra.core.Name> recursiveVars, hydra.util.ConsList<hydra.core.Binding> flatBindings, hydra.core.Name name, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        name,
        recursiveVars),
      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>>) (() -> {
        hydra.util.Lazy<hydra.core.Binding> binding = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
            (b).name,
            name)),
          flatBindings)));
        return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>>) (() -> {
          hydra.core.Term value = binding.get().term;
          return hydra.lib.eithers.Bind.apply(
            hydra.lib.maybes.Cases.applyLazy(
              binding.get().type,
              () -> hydra.CoderUtils.typeOfTerm(
                cx,
                gExt,
                value),
              (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (ts -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right((ts).type))),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>>) (typ -> hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.encodeType(
                aliasesExt,
                (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                typ,
                cx,
                g),
              (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>>) (jtype -> {
                hydra.ext.java.syntax.Identifier arid = new hydra.ext.java.syntax.Identifier("java.util.concurrent.atomic.AtomicReference");
                hydra.util.Lazy<hydra.ext.java.syntax.AnnotatedIdentifier> aid = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.AnnotatedIdentifier((hydra.util.ConsList<hydra.ext.java.syntax.Annotation>) (hydra.util.ConsList.<hydra.ext.java.syntax.Annotation>empty()), arid));
                hydra.ext.java.syntax.Identifier id = hydra.ext.java.Utils.variableToJavaIdentifier(name);
                return hydra.lib.eithers.Bind.apply(
                  hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                    jtype,
                    cx),
                  (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>>) (rt -> {
                    hydra.ext.java.syntax.PackageName pkg = hydra.ext.java.Names.javaPackageName(hydra.util.ConsList.of(
                      "java",
                      "util",
                      "concurrent",
                      "atomic"));
                    hydra.ext.java.syntax.Type artype = hydra.ext.java.Utils.javaRefType(
                      hydra.util.ConsList.of(rt),
                      hydra.util.Maybe.just(pkg),
                      "AtomicReference");
                    hydra.ext.java.syntax.TypeArgumentsOrDiamond targs = hydra.ext.java.Coder.typeArgsOrDiamond(hydra.util.ConsList.of(new hydra.ext.java.syntax.TypeArgument.Reference(rt)));
                    hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate ci = new hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate(hydra.util.ConsList.of(aid.get()), hydra.util.Maybe.just(targs));
                    hydra.util.Lazy<hydra.ext.java.syntax.Expression> body = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaConstructorCall(
                      ci,
                      (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()),
                      (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())));
                    return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>right(hydra.util.Maybe.just(hydra.ext.java.Utils.variableDeclarationStatement(
                      aliasesExt,
                      artype,
                      id,
                      body.get())));
                  }));
              }))));
        })).get();
      })).get(),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>>right((hydra.util.Maybe<hydra.ext.java.syntax.BlockStatement>) (hydra.util.Maybe.<hydra.ext.java.syntax.BlockStatement>nothing())));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement> toDeclStatement(hydra.ext.java.environment.JavaEnvironment envExt, hydra.ext.java.environment.Aliases aliasesExt, hydra.graph.Graph gExt, hydra.util.PersistentSet<hydra.core.Name> recursiveVars, hydra.util.PersistentSet<hydra.core.Name> thunkedVars, hydra.util.ConsList<hydra.core.Binding> flatBindings, hydra.core.Name name, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.util.Lazy<hydra.core.Binding> binding = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
        (b).name,
        name)),
      flatBindings)));
    hydra.core.Term value = binding.get().term;
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.maybes.Cases.applyLazy(
        binding.get().type,
        () -> hydra.CoderUtils.typeOfTerm(
          cx,
          gExt,
          value),
        (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (ts -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right((ts).type))),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>>) (typ -> hydra.lib.eithers.Bind.apply(
        hydra.ext.java.Coder.encodeType(
          aliasesExt,
          (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
          typ,
          cx,
          g),
        (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>>) (jtype -> {
          hydra.core.Term annotatedValue = hydra.Annotations.setTermAnnotation(
            hydra.Constants.key_type(),
            hydra.util.Maybe.just(hydra.encode.Core.type(typ)),
            value);
          hydra.ext.java.syntax.Identifier id = hydra.ext.java.Utils.variableToJavaIdentifier(name);
          return hydra.lib.eithers.Bind.apply(
            hydra.ext.java.Coder.encodeTerm(
              envExt,
              annotatedValue,
              cx,
              g),
            (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>>) (rhs -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.sets.Member.apply(
                name,
                recursiveVars),
              () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>right(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaMethodInvocationToJavaStatement(hydra.ext.java.Utils.methodInvocation(
                hydra.util.Maybe.just(hydra.util.Either.<hydra.ext.java.syntax.ExpressionName, hydra.ext.java.syntax.Primary>left(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), id))),
                new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.setMethodName()),
                hydra.util.ConsList.of(rhs))))),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.sets.Member.apply(
                  name,
                  thunkedVars),
                () -> hydra.lib.eithers.Bind.apply(
                  hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                    jtype,
                    cx),
                  (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>>) (rt -> {
                    hydra.ext.java.syntax.LambdaBody lambdaBody = new hydra.ext.java.syntax.LambdaBody.Expression(rhs);
                    hydra.util.Lazy<hydra.ext.java.syntax.Expression> supplierLambda = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.Expression.Lambda(new hydra.ext.java.syntax.LambdaExpression(new hydra.ext.java.syntax.LambdaParameters.Tuple((hydra.util.ConsList<hydra.ext.java.syntax.LambdaParameters>) (hydra.util.ConsList.<hydra.ext.java.syntax.LambdaParameters>empty())), lambdaBody)));
                    hydra.ext.java.syntax.TypeArgumentsOrDiamond targs = hydra.ext.java.Coder.typeArgsOrDiamond(hydra.util.ConsList.of(new hydra.ext.java.syntax.TypeArgument.Reference(rt)));
                    hydra.util.Lazy<hydra.ext.java.syntax.Expression> lazyExpr = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaConstructorCall(
                      hydra.ext.java.Utils.javaConstructorName(
                        new hydra.ext.java.syntax.Identifier("hydra.util.Lazy"),
                        hydra.util.Maybe.just(targs)),
                      hydra.util.ConsList.of(supplierLambda.get()),
                      (hydra.util.Maybe<hydra.ext.java.syntax.ClassBody>) (hydra.util.Maybe.<hydra.ext.java.syntax.ClassBody>nothing())));
                    hydra.ext.java.syntax.Type lazyType = hydra.ext.java.Utils.javaRefType(
                      hydra.util.ConsList.of(rt),
                      hydra.ext.java.Names.hydraUtilPackageName(),
                      "Lazy");
                    return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>right(hydra.ext.java.Utils.variableDeclarationStatement(
                      aliasesExt,
                      lazyType,
                      id,
                      lazyExpr.get()));
                  })),
                () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.BlockStatement>right(hydra.ext.java.Utils.variableDeclarationStatement(
                  aliasesExt,
                  jtype,
                  id,
                  rhs))))));
        }))));
  }

  static hydra.util.Maybe<hydra.core.Type> tryInferFunctionType(hydra.core.Function fun) {
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Function instance) {
        return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Function.Lambda lam) {
        return hydra.lib.maybes.Bind.apply(
          (lam).value.domain,
          (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (dom -> {
            hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> mCod = new hydra.util.Lazy<>(() -> (lam).value.body.accept(new hydra.core.Term.PartialVisitor<>() {
              @Override
              public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
                return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
              }

              @Override
              public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Annotated at) {
                return hydra.lib.maybes.Bind.apply(
                  hydra.lib.maps.Lookup.apply(
                    hydra.Constants.key_type(),
                    (at).value.annotation),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Type>>) (typeTerm -> hydra.ext.java.Coder.decodeTypeFromTerm(typeTerm)));
              }

              @Override
              public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Function innerFun) {
                return hydra.ext.java.Coder.tryInferFunctionType((innerFun).value);
              }
            }));
            return hydra.lib.maybes.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (cod -> new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod))),
              mCod.get());
          }));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> typeAppFallbackCast(hydra.ext.java.environment.JavaEnvironment env, hydra.ext.java.environment.Aliases aliases, hydra.util.ConsList<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> anns, hydra.util.ConsList<hydra.ext.java.syntax.Type> tyapps, hydra.ext.java.syntax.Type jatyp, hydra.core.Term body, hydra.core.Type typ, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.core.Term annotatedBody = hydra.Annotations.setTermAnnotation(
      hydra.Constants.key_type(),
      hydra.util.Maybe.just(hydra.encode.Core.type(typ)),
      body);
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.java.Coder.encodeTermInternal(
        env,
        anns,
        hydra.lib.lists.Cons.apply(
          jatyp,
          tyapps),
        annotatedBody,
        cx,
        g),
      (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jbody -> hydra.lib.eithers.Bind.apply(
        hydra.ext.java.Coder.encodeType(
          aliases,
          (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
          typ,
          cx,
          g),
        (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jtype -> hydra.lib.eithers.Bind.apply(
          hydra.ext.java.Utils.javaTypeToJavaReferenceType(
            jtype,
            cx),
          (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.Utils.javaCastExpression(
            rt,
            hydra.ext.java.Utils.javaExpressionToJavaUnaryExpression(jbody))))))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> typeAppNullaryOrHoisted(hydra.ext.java.environment.JavaEnvironment env, hydra.ext.java.environment.Aliases aliases, hydra.util.ConsList<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> anns, hydra.util.ConsList<hydra.ext.java.syntax.Type> tyapps, hydra.ext.java.syntax.Type jatyp, hydra.core.Term body, hydra.core.Type correctedTyp, hydra.core.Name varName, hydra.ext.java.environment.JavaSymbolClass cls, hydra.util.ConsList<hydra.core.Type> allTypeArgs, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.module.QualifiedName qn = hydra.Names.qualifyName(varName);
    String localName = (qn).local;
    hydra.util.Maybe<hydra.module.Namespace> mns = (qn).namespace;
    return (cls).accept(new hydra.ext.java.environment.JavaSymbolClass.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> otherwise(hydra.ext.java.environment.JavaSymbolClass instance) {
        return hydra.ext.java.Coder.typeAppFallbackCast(
          env,
          aliases,
          anns,
          tyapps,
          jatyp,
          body,
          correctedTyp,
          cx,
          g);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.environment.JavaSymbolClass.NullaryFunction _u) {
        return hydra.lib.maybes.Cases.applyLazy(
          mns,
          () -> hydra.ext.java.Coder.typeAppFallbackCast(
            env,
            aliases,
            anns,
            tyapps,
            jatyp,
            body,
            correctedTyp,
            cx,
            g),
          (java.util.function.Function<hydra.module.Namespace, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (ns_ -> {
            hydra.ext.java.syntax.Identifier classId = hydra.ext.java.Utils.nameToJavaName(
              aliases,
              hydra.ext.java.Coder.elementsQualifiedName(ns_));
            hydra.ext.java.syntax.Identifier methodId = new hydra.ext.java.syntax.Identifier(hydra.ext.java.Utils.sanitizeJavaName(localName));
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.filterPhantomTypeArgs(
                varName,
                allTypeArgs,
                cx,
                g),
              (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (filteredTypeArgs -> hydra.lib.eithers.Bind.apply(
                hydra.lib.eithers.MapList.apply(
                  (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (t -> hydra.lib.eithers.Bind.apply(
                    hydra.ext.java.Coder.encodeType(
                      aliases,
                      (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                      t,
                      cx,
                      g),
                    (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (jt -> hydra.lib.eithers.Bind.apply(
                      hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                        jt,
                        cx),
                      (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>right(new hydra.ext.java.syntax.TypeArgument.Reference(rt))))))),
                  filteredTypeArgs),
                (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jTypeArgs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStaticWithTypeArgs(
                  classId,
                  methodId,
                  jTypeArgs,
                  (hydra.util.ConsList<hydra.ext.java.syntax.Expression>) (hydra.util.ConsList.<hydra.ext.java.syntax.Expression>empty()))))))));
          }));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression> visit(hydra.ext.java.environment.JavaSymbolClass.HoistedLambda arity) {
        return hydra.lib.maybes.Cases.applyLazy(
          mns,
          () -> hydra.ext.java.Coder.typeAppFallbackCast(
            env,
            aliases,
            anns,
            tyapps,
            jatyp,
            body,
            correctedTyp,
            cx,
            g),
          (java.util.function.Function<hydra.module.Namespace, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (ns_ -> {
            hydra.ext.java.syntax.Identifier classId = hydra.ext.java.Utils.nameToJavaName(
              aliases,
              hydra.ext.java.Coder.elementsQualifiedName(ns_));
            hydra.ext.java.syntax.Identifier methodId = new hydra.ext.java.syntax.Identifier(hydra.ext.java.Utils.sanitizeJavaName(localName));
            return hydra.lib.eithers.Bind.apply(
              hydra.ext.java.Coder.filterPhantomTypeArgs(
                varName,
                allTypeArgs,
                cx,
                g),
              (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (filteredTypeArgs -> hydra.lib.eithers.Bind.apply(
                hydra.lib.eithers.MapList.apply(
                  (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (t -> hydra.lib.eithers.Bind.apply(
                    hydra.ext.java.Coder.encodeType(
                      aliases,
                      (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                      t,
                      cx,
                      g),
                    (java.util.function.Function<hydra.ext.java.syntax.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (jt -> hydra.lib.eithers.Bind.apply(
                      hydra.ext.java.Utils.javaTypeToJavaReferenceType(
                        jt,
                        cx),
                      (java.util.function.Function<hydra.ext.java.syntax.ReferenceType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>>) (rt -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.TypeArgument>right(new hydra.ext.java.syntax.TypeArgument.Reference(rt))))))),
                  filteredTypeArgs),
                (java.util.function.Function<hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>>) (jTypeArgs -> {
                  hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> paramNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<Integer, hydra.core.Name>) (i -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                      "p",
                      hydra.lib.literals.ShowInt32.apply(i)))),
                    hydra.lib.math.Range.apply(
                      0,
                      hydra.lib.math.Sub.apply(
                        (arity).value,
                        1))));
                  hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.Expression>> paramExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.Name, hydra.ext.java.syntax.Expression>) (p -> hydra.ext.java.Utils.javaIdentifierToJavaExpression(hydra.ext.java.Utils.variableToJavaIdentifier(p))),
                    paramNames.get()));
                  hydra.ext.java.syntax.Expression call = hydra.ext.java.Utils.javaMethodInvocationToJavaExpression(hydra.ext.java.Utils.methodInvocationStaticWithTypeArgs(
                    classId,
                    methodId,
                    jTypeArgs,
                    paramExprs.get()));
                  return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.Expression>right(hydra.ext.java.Coder.buildCurriedLambda(
                    paramNames.get(),
                    call));
                }))));
          }));
      }
    });
  }

  static hydra.ext.java.syntax.TypeArgumentsOrDiamond typeArgsOrDiamond(hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument> args) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.ext.java.Coder.javaFeatures().supportsDiamondOperator,
      () -> new hydra.ext.java.syntax.TypeArgumentsOrDiamond.Diamond(),
      () -> new hydra.ext.java.syntax.TypeArgumentsOrDiamond.Arguments(args));
  }

  static Boolean typesMatch(hydra.core.Type a, hydra.core.Type b) {
    return (a).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Type.Variable va) {
        return (b).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Type instance) {
            return true;
          }

          @Override
          public Boolean visit(hydra.core.Type.Variable vb) {
            return hydra.lib.equality.Equal.apply(
              (va).value,
              (vb).value);
          }
        });
      }

      @Override
      public Boolean visit(hydra.core.Type.Wrap wa) {
        return (b).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Type instance) {
            return true;
          }

          @Override
          public Boolean visit(hydra.core.Type.Wrap wb) {
            return hydra.lib.equality.Equal.apply(
              (wa).value,
              (wb).value);
          }
        });
      }
    });
  }

  static hydra.core.Type unwrapReturnType(hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Function ft) {
        return hydra.ext.java.Coder.unwrapReturnType((ft).value.codomain);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Application at) {
        return hydra.ext.java.Coder.unwrapReturnType((at).value.argument);
      }
    });
  }

  static <T0> hydra.ext.java.syntax.ClassBodyDeclaration variantCompareToMethod(hydra.ext.java.environment.Aliases aliases, T0 tparams, hydra.core.Name parentName, hydra.core.Name variantName, hydra.util.ConsList<hydra.core.FieldType> fields) {
    hydra.util.ConsList<hydra.ext.java.syntax.Annotation> anns = hydra.util.ConsList.of(
      hydra.ext.java.Utils.overrideAnnotation(),
      hydra.ext.java.Utils.suppressWarningsUncheckedAnnotation());
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> tagDeclStmt = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.variableDeclarationStatement(
      aliases,
      hydra.ext.java.Utils.javaIntType(),
      hydra.ext.java.Utils.javaIdentifier("tagCmp"),
      hydra.ext.java.Coder.tagCompareExpr()));
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> tagReturnStmt = new hydra.util.Lazy<>(() -> new hydra.ext.java.syntax.BlockStatement.Statement(new hydra.ext.java.syntax.Statement.IfThen(new hydra.ext.java.syntax.IfThenStatement(hydra.ext.java.Coder.tagCmpNotZeroExpr(), hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.Utils.javaExpressionNameToJavaExpression(new hydra.ext.java.syntax.ExpressionName((hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName>) (hydra.util.Maybe.<hydra.ext.java.syntax.AmbiguousName>nothing()), hydra.ext.java.Utils.javaIdentifier("tagCmp")))))))));
    hydra.util.Lazy<hydra.ext.java.syntax.Expression> castOtherExpr = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.javaCastExpressionToJavaExpression(hydra.ext.java.Utils.javaCastExpression(
      hydra.ext.java.Utils.nameToJavaReferenceType(
        aliases,
        false,
        (hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeArgument>empty()),
        variantName,
        (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
      hydra.ext.java.Utils.javaIdentifierToJavaUnaryExpression(new hydra.ext.java.syntax.Identifier(hydra.ext.java.Names.otherInstanceName())))));
    String varTmpName = "o";
    hydra.ext.java.syntax.Type variantJavaType = hydra.ext.java.Utils.javaTypeFromTypeName(
      aliases,
      variantName);
    hydra.util.Lazy<hydra.ext.java.syntax.BlockStatement> castDeclStmt = new hydra.util.Lazy<>(() -> hydra.ext.java.Utils.variableDeclarationStatement(
      aliases,
      variantJavaType,
      hydra.ext.java.Utils.javaIdentifier(varTmpName),
      castOtherExpr.get()));
    hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement> emptyReturn = hydra.util.ConsList.of(new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(hydra.ext.java.Utils.javaIntExpression(new java.math.BigInteger("0"))))));
    hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> valueCompareStmt = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(fields),
      () -> emptyReturn,
      () -> hydra.lib.lists.Concat2.apply(
        hydra.util.ConsList.of(castDeclStmt.get()),
        hydra.ext.java.Coder.compareToBody(
          aliases,
          varTmpName,
          fields))));
    hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> body = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      hydra.util.ConsList.of(
        tagDeclStmt.get(),
        tagReturnStmt.get()),
      valueCompareStmt.get()));
    hydra.util.ConsList<hydra.ext.java.syntax.MethodModifier> mods = hydra.util.ConsList.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.Utils.javaTypeToJavaFormalParameter(
      hydra.ext.java.Utils.javaTypeFromTypeName(
        aliases,
        parentName),
      new hydra.core.Name(hydra.ext.java.Names.otherInstanceName()));
    hydra.ext.java.syntax.Result result = hydra.ext.java.Utils.javaTypeToJavaResult(hydra.ext.java.Utils.javaIntType());
    return hydra.ext.java.Utils.methodDeclaration(
      mods,
      (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()),
      anns,
      hydra.ext.java.Names.compareToMethodName(),
      hydra.util.ConsList.of(param),
      result,
      hydra.util.Maybe.just(body.get()));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> visitBranch(hydra.ext.java.environment.JavaEnvironment env, hydra.ext.java.environment.Aliases aliases, hydra.core.Type dom, hydra.core.Name tname, hydra.ext.java.syntax.Type jcod, hydra.util.ConsList<hydra.ext.java.syntax.TypeArgument> targs, hydra.core.Field field, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.util.ConsList<hydra.ext.java.syntax.Annotation> anns = hydra.util.ConsList.of(hydra.ext.java.Utils.overrideAnnotation());
    hydra.ext.java.syntax.Type jdom = new hydra.ext.java.syntax.Type.Reference(hydra.ext.java.Utils.nameToJavaReferenceType(
      aliases,
      true,
      targs,
      tname,
      hydra.util.Maybe.just(hydra.Formatting.capitalize((field).name.value))));
    hydra.util.ConsList<hydra.ext.java.syntax.MethodModifier> mods = hydra.util.ConsList.of(new hydra.ext.java.syntax.MethodModifier.Public());
    hydra.ext.java.syntax.Result result = new hydra.ext.java.syntax.Result.Type(new hydra.ext.java.syntax.UnannType(jcod));
    return hydra.Rewriting.deannotateTerm((field).term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          "visitBranch: field term is not a lambda: ",
          hydra.show.Core.term((field).term)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> otherwise(hydra.core.Function instance) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
              "visitBranch: field term is not a lambda: ",
              hydra.show.Core.term((field).term)))), cx)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments> visit(hydra.core.Function.Lambda lam) {
            return hydra.ext.java.Coder.withLambda(
              env,
              (lam).value,
              (java.util.function.Function<hydra.ext.java.environment.JavaEnvironment, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (env2 -> {
                hydra.core.Term body = (lam).value.body;
                hydra.core.Name lambdaParam = (lam).value.parameter;
                hydra.ext.java.environment.JavaEnvironment env3 = hydra.ext.java.Coder.insertBranchVar(
                  lambdaParam,
                  env2);
                return hydra.lib.eithers.Bind.apply(
                  hydra.ext.java.Coder.analyzeJavaFunction(
                    env3,
                    body,
                    cx,
                    g),
                  (java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (fs -> {
                    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.util.ConsList<hydra.core.Binding>>) (projected -> projected.bindings)).apply(fs));
                    hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env4 = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.ext.java.environment.JavaEnvironment>) (projected -> projected.environment)).apply(fs));
                    hydra.util.Lazy<hydra.core.Term> innerBody = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.typing.FunctionStructure<hydra.ext.java.environment.JavaEnvironment>, hydra.core.Term>) (projected -> projected.body)).apply(fs));
                    return hydra.lib.eithers.Bind.apply(
                      hydra.ext.java.Coder.bindingsToStatements(
                        env4.get(),
                        bindings.get(),
                        cx,
                        g),
                      (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>, hydra.ext.java.environment.JavaEnvironment>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (bindResult -> {
                        hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> bindingStmts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindResult));
                        hydra.util.Lazy<hydra.ext.java.environment.JavaEnvironment> env5 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindResult));
                        return hydra.lib.eithers.Bind.apply(
                          hydra.ext.java.Coder.encodeTerm(
                            env5.get(),
                            innerBody.get(),
                            cx,
                            g),
                          (java.util.function.Function<hydra.ext.java.syntax.Expression, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>>) (jret -> {
                            hydra.ext.java.syntax.BlockStatement returnStmt = new hydra.ext.java.syntax.BlockStatement.Statement(hydra.ext.java.Utils.javaReturnStatement(hydra.util.Maybe.just(jret)));
                            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.java.syntax.BlockStatement>> allStmts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                              bindingStmts.get(),
                              hydra.util.ConsList.of(returnStmt)));
                            hydra.ext.java.syntax.FormalParameter param = hydra.ext.java.Utils.javaTypeToJavaFormalParameter(
                              jdom,
                              lambdaParam);
                            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.ext.java.syntax.ClassBodyDeclarationWithComments>right(hydra.ext.java.Coder.noComment(hydra.ext.java.Utils.methodDeclaration(
                              mods,
                              (hydra.util.ConsList<hydra.ext.java.syntax.TypeParameter>) (hydra.util.ConsList.<hydra.ext.java.syntax.TypeParameter>empty()),
                              anns,
                              hydra.ext.java.Names.visitMethodName(),
                              hydra.util.ConsList.of(param),
                              result,
                              hydra.util.Maybe.just(allStmts.get()))));
                          }));
                      }));
                  }));
              }));
          }
        });
      }
    });
  }

  static <T0> T0 withLambda(hydra.ext.java.environment.JavaEnvironment env, hydra.core.Lambda lam, java.util.function.Function<hydra.ext.java.environment.JavaEnvironment, T0> k) {
    return hydra.Schemas.withLambdaContext(
      hydra.ext.java.Coder::javaEnvGetGraph,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.ext.java.environment.JavaEnvironment, hydra.ext.java.environment.JavaEnvironment>>) (p0 -> p1 -> hydra.ext.java.Coder.javaEnvSetGraph(
        p0,
        p1)),
      env,
      lam,
      (java.util.function.Function<hydra.ext.java.environment.JavaEnvironment, T0>) (env1 -> {
        hydra.ext.java.environment.Aliases aliases = (env1).aliases;
        hydra.util.Lazy<hydra.ext.java.environment.Aliases> aliases2 = new hydra.util.Lazy<>(() -> new hydra.ext.java.environment.Aliases((aliases).currentNamespace, (aliases).packages, (aliases).branchVars, (aliases).recursiveVars, (aliases).inScopeTypeParams, (aliases).polymorphicLocals, (aliases).inScopeJavaVars, (aliases).varRenames, hydra.lib.sets.Insert.apply(
          (lam).parameter,
          (aliases).lambdaVars), (aliases).typeVarSubst, (aliases).trustedTypeVars, (aliases).methodCodomain, (aliases).thunkedVars));
        hydra.ext.java.environment.JavaEnvironment env2 = new hydra.ext.java.environment.JavaEnvironment(aliases2.get(), (env1).graph);
        return (k).apply(env2);
      }));
  }

  static <T0> T0 withTypeLambda(hydra.ext.java.environment.JavaEnvironment v1, hydra.core.TypeLambda v2, java.util.function.Function<hydra.ext.java.environment.JavaEnvironment, T0> v3) {
    return hydra.Schemas.withTypeLambdaContext(
      hydra.ext.java.Coder::javaEnvGetGraph,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.ext.java.environment.JavaEnvironment, hydra.ext.java.environment.JavaEnvironment>>) (p0 -> p1 -> hydra.ext.java.Coder.javaEnvSetGraph(
        p0,
        p1)),
      v1,
      v2,
      v3);
  }

  static hydra.ext.java.syntax.Expression wrapInSupplierLambda(hydra.ext.java.syntax.Expression expr) {
    return new hydra.ext.java.syntax.Expression.Lambda(new hydra.ext.java.syntax.LambdaExpression(new hydra.ext.java.syntax.LambdaParameters.Tuple((hydra.util.ConsList<hydra.ext.java.syntax.LambdaParameters>) (hydra.util.ConsList.<hydra.ext.java.syntax.LambdaParameters>empty())), new hydra.ext.java.syntax.LambdaBody.Expression(expr)));
  }

  static hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>> wrapLazyArguments(hydra.core.Name name, hydra.util.ConsList<hydra.ext.java.syntax.Expression> args) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Equal.apply(
          name,
          new hydra.core.Name("hydra.lib.logic.ifElse")),
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(args),
          3)),
      () -> (hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>(hydra.util.ConsList.of(
        hydra.lib.lists.At.apply(
          0,
          args),
        hydra.ext.java.Coder.wrapInSupplierLambda(hydra.lib.lists.At.apply(
          1,
          args)),
        hydra.ext.java.Coder.wrapInSupplierLambda(hydra.lib.lists.At.apply(
          2,
          args))), hydra.util.Maybe.just("lazy")))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.And.apply(
          hydra.lib.equality.Equal.apply(
            name,
            new hydra.core.Name("hydra.lib.maybes.maybe")),
          hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(args),
            3)),
        () -> (hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>(hydra.util.ConsList.of(
          hydra.ext.java.Coder.wrapInSupplierLambda(hydra.lib.lists.At.apply(
            0,
            args)),
          hydra.lib.lists.At.apply(
            1,
            args),
          hydra.lib.lists.At.apply(
            2,
            args)), hydra.util.Maybe.just("applyLazy")))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.And.apply(
            hydra.lib.equality.Equal.apply(
              name,
              new hydra.core.Name("hydra.lib.maybes.cases")),
            hydra.lib.equality.Equal.apply(
              hydra.lib.lists.Length.apply(args),
              3)),
          () -> (hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>(hydra.util.ConsList.of(
            hydra.lib.lists.At.apply(
              0,
              args),
            hydra.ext.java.Coder.wrapInSupplierLambda(hydra.lib.lists.At.apply(
              1,
              args)),
            hydra.lib.lists.At.apply(
              2,
              args)), hydra.util.Maybe.just("applyLazy")))),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.logic.And.apply(
              hydra.lib.equality.Equal.apply(
                name,
                new hydra.core.Name("hydra.lib.maps.findWithDefault")),
              hydra.lib.equality.Equal.apply(
                hydra.lib.lists.Length.apply(args),
                3)),
            () -> (hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>(hydra.util.ConsList.of(
              hydra.ext.java.Coder.wrapInSupplierLambda(hydra.lib.lists.At.apply(
                0,
                args)),
              hydra.lib.lists.At.apply(
                1,
                args),
              hydra.lib.lists.At.apply(
                2,
                args)), hydra.util.Maybe.just("applyLazy")))),
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.And.apply(
                hydra.lib.logic.Or.apply(
                  hydra.lib.equality.Equal.apply(
                    name,
                    new hydra.core.Name("hydra.lib.maybes.fromMaybe")),
                  hydra.lib.logic.Or.apply(
                    hydra.lib.equality.Equal.apply(
                      name,
                      new hydra.core.Name("hydra.lib.eithers.fromLeft")),
                    hydra.lib.equality.Equal.apply(
                      name,
                      new hydra.core.Name("hydra.lib.eithers.fromRight")))),
                hydra.lib.equality.Equal.apply(
                  hydra.lib.lists.Length.apply(args),
                  2)),
              () -> (hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>(hydra.util.ConsList.of(
                hydra.ext.java.Coder.wrapInSupplierLambda(hydra.lib.lists.At.apply(
                  0,
                  args)),
                hydra.lib.lists.At.apply(
                  1,
                  args)), hydra.util.Maybe.just("applyLazy")))),
              () -> (hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.java.syntax.Expression>, hydra.util.Maybe<String>>(args, (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))))))));
  }
}
