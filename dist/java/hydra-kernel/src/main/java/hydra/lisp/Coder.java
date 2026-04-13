// Note: this is an automatically generated file. Do not edit.

package hydra.lisp;

/**
 * Lisp code generator: converts Hydra type and term modules to Lisp AST
 */
public interface Coder {
  static String dialectCadr(hydra.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.lisp.syntax.Dialect instance) {
        return "cadr";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return "second";
      }
    });
  }

  static String dialectCar(hydra.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.lisp.syntax.Dialect instance) {
        return "car";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return "first";
      }
    });
  }

  static String dialectConstructorPrefix(hydra.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.lisp.syntax.Dialect instance) {
        return "make-";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return "->";
      }
    });
  }

  static String dialectEqual(hydra.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.lisp.syntax.Dialect instance) {
        return "equal?";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return "=";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return "equal";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "equal";
      }
    });
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.lisp.syntax.Expression> encodeApplication(hydra.lisp.syntax.Dialect dialect, T0 cx, T1 g, hydra.core.Term rawFun, hydra.core.Term rawArg) {
    hydra.core.Term dFun = hydra.Strip.deannotateTerm(rawFun);
    java.util.function.Function<hydra.core.Term, hydra.util.Either<T2, hydra.lisp.syntax.Expression>> enc = (java.util.function.Function<hydra.core.Term, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (v1 -> hydra.lisp.Coder.<T0, T1, T2>encodeApplication_enc(
      cx,
      dialect,
      g,
      v1));
    hydra.util.Lazy<hydra.util.Either<T2, hydra.lisp.syntax.Expression>> normal = new hydra.util.Lazy<>(() -> hydra.lisp.Coder.<T0, T1, T2>encodeApplication_normal(
      cx,
      dialect,
      g,
      (java.util.function.Function<hydra.lisp.syntax.Expression, java.util.function.Function<java.util.List<hydra.lisp.syntax.Expression>, hydra.lisp.syntax.Expression>>) (p0 -> p1 -> hydra.lisp.Coder.lispApp(
        p0,
        p1)),
      rawArg,
      rawFun));
    return (dFun).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> otherwise(hydra.core.Term instance) {
        return normal.get();
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Application app2) {
        hydra.core.Term midFun = (app2).value.function;
        hydra.core.Term dMidFun = hydra.Strip.deannotateTerm(midFun);
        Boolean isLazy2 = hydra.lib.logic.Or.apply(
          hydra.lisp.Coder.isPrimitiveRef(
            "hydra.lib.eithers.fromLeft",
            dMidFun),
          hydra.lib.logic.Or.apply(
            hydra.lisp.Coder.isPrimitiveRef(
              "hydra.lib.eithers.fromRight",
              dMidFun),
            hydra.lisp.Coder.isPrimitiveRef(
              "hydra.lib.maybes.fromMaybe",
              dMidFun)));
        hydra.core.Term midArg = (app2).value.argument;
        return hydra.lib.logic.IfElse.lazy(
          isLazy2,
          () -> hydra.lib.eithers.Bind.apply(
            (enc).apply(midFun),
            (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (ePrim -> hydra.lib.eithers.Bind.apply(
              (enc).apply(midArg),
              (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (eDef -> hydra.lib.eithers.Bind.apply(
                (enc).apply(rawArg),
                (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (eArg -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispApp(
                  hydra.lisp.Coder.lispApp(
                    ePrim,
                    java.util.Arrays.asList(hydra.lisp.Coder.wrapInThunk(eDef))),
                  java.util.Arrays.asList(eArg))))))))),
          () -> (dMidFun).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.util.Either<T2, hydra.lisp.syntax.Expression> otherwise(hydra.core.Term instance) {
              return normal.get();
            }

            @Override
            public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Application app3) {
              hydra.core.Term innerFun = (app3).value.function;
              hydra.core.Term dInnerFun = hydra.Strip.deannotateTerm(innerFun);
              hydra.core.Term innerArg = (app3).value.argument;
              return hydra.lib.logic.IfElse.lazy(
                hydra.lisp.Coder.isPrimitiveRef(
                  "hydra.lib.logic.ifElse",
                  dInnerFun),
                () -> hydra.lib.eithers.Bind.apply(
                  (enc).apply(innerArg),
                  (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (eC -> hydra.lib.eithers.Bind.apply(
                    (enc).apply(midArg),
                    (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (eT -> hydra.lib.eithers.Bind.apply(
                      (enc).apply(rawArg),
                      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (eE -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(new hydra.lisp.syntax.Expression.If(new hydra.lisp.syntax.IfExpression(eC, eT, hydra.util.Maybe.just(eE)))))))))),
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.lisp.Coder.isPrimitiveRef(
                    "hydra.lib.maybes.maybe",
                    dInnerFun),
                  () -> hydra.lib.eithers.Bind.apply(
                    (enc).apply(innerFun),
                    (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (eP -> hydra.lib.eithers.Bind.apply(
                      (enc).apply(innerArg),
                      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (eDef -> hydra.lib.eithers.Bind.apply(
                        (enc).apply(midArg),
                        (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (eF -> hydra.lib.eithers.Bind.apply(
                          (enc).apply(rawArg),
                          (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (eM -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispApp(
                            hydra.lisp.Coder.lispApp(
                              hydra.lisp.Coder.lispApp(
                                eP,
                                java.util.Arrays.asList(hydra.lisp.Coder.wrapInThunk(eDef))),
                              java.util.Arrays.asList(eF)),
                            java.util.Arrays.asList(eM))))))))))),
                  () -> hydra.lib.logic.IfElse.lazy(
                    hydra.lisp.Coder.isPrimitiveRef(
                      "hydra.lib.maybes.cases",
                      dInnerFun),
                    () -> hydra.lib.eithers.Bind.apply(
                      (enc).apply(innerFun),
                      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (eP -> hydra.lib.eithers.Bind.apply(
                        (enc).apply(innerArg),
                        (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (eM -> hydra.lib.eithers.Bind.apply(
                          (enc).apply(midArg),
                          (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (eN -> hydra.lib.eithers.Bind.apply(
                            (enc).apply(rawArg),
                            (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (eJ -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispApp(
                              hydra.lisp.Coder.lispApp(
                                hydra.lisp.Coder.lispApp(
                                  eP,
                                  java.util.Arrays.asList(eM)),
                                java.util.Arrays.asList(hydra.lisp.Coder.wrapInThunk(eN))),
                              java.util.Arrays.asList(eJ))))))))))),
                    () -> normal.get())));
            }
          }));
      }
    });
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.lisp.syntax.Expression> encodeApplication_enc(T0 cx, hydra.lisp.syntax.Dialect dialect, T1 g, hydra.core.Term t) {
    return hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
      dialect,
      cx,
      g,
      t);
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.lisp.syntax.Expression> encodeApplication_normal(T0 cx, hydra.lisp.syntax.Dialect dialect, T1 g, java.util.function.Function<hydra.lisp.syntax.Expression, java.util.function.Function<java.util.List<hydra.lisp.syntax.Expression>, hydra.lisp.syntax.Expression>> hydra_lisp_coder_lispApp, hydra.core.Term rawArg, hydra.core.Term rawFun) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
        dialect,
        cx,
        g,
        rawFun),
      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (fun -> hydra.lib.eithers.Bind.apply(
        hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
          dialect,
          cx,
          g,
          rawArg),
        (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (arg -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right((hydra_lisp_coder_lispApp).apply(fun).apply(java.util.Arrays.asList(arg)))))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.lisp.syntax.Expression> encodeElimination(hydra.lisp.syntax.Dialect dialect, T0 cx, T1 g, hydra.core.Elimination elim, hydra.util.Maybe<hydra.core.Term> marg) {
    return (elim).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Elimination.Record proj) {
        String fname = hydra.Formatting.convertCaseCamelToLowerSnake((proj).value.field.value);
        String tname = hydra.lisp.Coder.qualifiedSnakeName((proj).value.typeName);
        return hydra.lib.maybes.Cases.applyLazy(
          marg,
          () -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispLambdaExpr(
            java.util.Arrays.asList("v"),
            new hydra.lisp.syntax.Expression.FieldAccess(new hydra.lisp.syntax.FieldAccess(new hydra.lisp.syntax.Symbol(tname), new hydra.lisp.syntax.Symbol(fname), hydra.lisp.Coder.lispVar("v"))))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (arg -> hydra.lib.eithers.Bind.apply(
            hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
              dialect,
              cx,
              g,
              arg),
            (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (sarg -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(new hydra.lisp.syntax.Expression.FieldAccess(new hydra.lisp.syntax.FieldAccess(new hydra.lisp.syntax.Symbol(tname), new hydra.lisp.syntax.Symbol(fname), sarg)))))));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Elimination.Union cs) {
        java.util.List<hydra.core.Field> caseFields = (cs).value.cases;
        hydra.util.Maybe<hydra.core.Term> defCase = (cs).value.default_;
        String tname = hydra.Names.localNameOf((cs).value.typeName);
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Field, hydra.util.Either<T2, hydra.lisp.syntax.CondClause>>) (cf -> {
              String cfname = hydra.Formatting.convertCaseCamelToLowerSnake((cf).name.value);
              hydra.core.Term cfterm = (cf).term;
              hydra.lisp.syntax.Expression condExpr = hydra.lisp.Coder.lispApp(
                hydra.lisp.Coder.lispVar(hydra.lisp.Coder.dialectEqual(dialect)),
                java.util.Arrays.asList(
                  hydra.lisp.Coder.lispApp(
                    hydra.lisp.Coder.lispVar(hydra.lisp.Coder.dialectCar(dialect)),
                    java.util.Arrays.asList(hydra.lisp.Coder.lispVar("match_target"))),
                  hydra.lisp.Coder.lispKeyword(cfname)));
              return hydra.lib.eithers.Bind.apply(
                hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
                  dialect,
                  cx,
                  g,
                  new hydra.core.Term.Application(new hydra.core.Application(cfterm, new hydra.core.Term.Variable(new hydra.core.Name("match_value"))))),
                (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.CondClause>>) (bodyExpr -> hydra.util.Either.<T2, hydra.lisp.syntax.CondClause>right(new hydra.lisp.syntax.CondClause(condExpr, bodyExpr))));
            }),
            caseFields),
          (java.util.function.Function<java.util.List<hydra.lisp.syntax.CondClause>, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (clauses -> hydra.lib.eithers.Bind.apply(
            hydra.lib.maybes.Cases.applyLazy(
              defCase,
              () -> hydra.util.Either.<T2, hydra.util.Maybe<hydra.lisp.syntax.Expression>>right((hydra.util.Maybe<hydra.lisp.syntax.Expression>) (hydra.util.Maybe.<hydra.lisp.syntax.Expression>nothing())),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<T2, hydra.util.Maybe<hydra.lisp.syntax.Expression>>>) (dt -> hydra.lib.eithers.Bind.apply(
                hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
                  dialect,
                  cx,
                  g,
                  dt),
                (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.util.Maybe<hydra.lisp.syntax.Expression>>>) (defBody -> hydra.util.Either.<T2, hydra.util.Maybe<hydra.lisp.syntax.Expression>>right(hydra.util.Maybe.just(defBody)))))),
            (java.util.function.Function<hydra.util.Maybe<hydra.lisp.syntax.Expression>, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (defExpr -> {
              hydra.lisp.syntax.Expression condExpr = new hydra.lisp.syntax.Expression.Cond(new hydra.lisp.syntax.CondExpression(clauses, defExpr));
              hydra.lisp.syntax.Expression innerExpr = hydra.lisp.Coder.lispApp(
                hydra.lisp.Coder.lispLambdaExpr(
                  java.util.Arrays.asList("match_value"),
                  condExpr),
                java.util.Arrays.asList(hydra.lisp.Coder.lispApp(
                  hydra.lisp.Coder.lispVar(hydra.lisp.Coder.dialectCadr(dialect)),
                  java.util.Arrays.asList(hydra.lisp.Coder.lispVar("match_target")))));
              return hydra.lib.maybes.Cases.applyLazy(
                marg,
                () -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispLambdaExpr(
                  java.util.Arrays.asList("match_target"),
                  innerExpr)),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (arg -> hydra.lib.eithers.Bind.apply(
                  hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
                    dialect,
                    cx,
                    g,
                    arg),
                  (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (sarg -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispApp(
                    hydra.lisp.Coder.lispLambdaExpr(
                      java.util.Arrays.asList("match_target"),
                      innerExpr),
                    java.util.Arrays.asList(sarg)))))));
            }))));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Elimination.Wrap name) {
        return hydra.lib.maybes.Cases.applyLazy(
          marg,
          () -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispLambdaExpr(
            java.util.Arrays.asList("v"),
            hydra.lisp.Coder.lispVar("v"))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (arg -> hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
            dialect,
            cx,
            g,
            arg)));
      }
    });
  }

  static hydra.lisp.syntax.FieldDefinition encodeFieldDef(hydra.core.FieldType ft) {
    String fname = (ft).name.value;
    return new hydra.lisp.syntax.FieldDefinition(new hydra.lisp.syntax.Symbol(hydra.Formatting.convertCaseCamelToLowerSnake(fname)), (hydra.util.Maybe<hydra.lisp.syntax.Expression>) (hydra.util.Maybe.<hydra.lisp.syntax.Expression>nothing()));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.lisp.syntax.Expression> encodeFunction(hydra.lisp.syntax.Dialect dialect, T0 cx, T1 g, hydra.core.Function fun) {
    return (fun).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Function.Lambda lam) {
        String param = hydra.Formatting.convertCaseCamelOrUnderscoreToLowerSnake(hydra.Formatting.sanitizeWithUnderscores(
          hydra.lisp.Language.lispReservedWords(),
          (lam).value.parameter.value));
        return hydra.lib.eithers.Bind.apply(
          hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
            dialect,
            cx,
            g,
            (lam).value.body),
          (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (body -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispLambdaExpr(
            java.util.Arrays.asList(param),
            body))));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Function.Elimination elim) {
        return hydra.lisp.Coder.<T0, T1, T2>encodeElimination(
          dialect,
          cx,
          g,
          (elim).value,
          (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()));
      }
    });
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.lisp.syntax.Expression> encodeLetAsLambdaApp(hydra.lisp.syntax.Dialect dialect, T0 cx, T1 g, java.util.List<hydra.core.Binding> bindings, hydra.core.Term body) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
        dialect,
        cx,
        g,
        body),
      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (bodyExpr -> hydra.lib.eithers.Foldl.apply(
        (java.util.function.Function<hydra.lisp.syntax.Expression, java.util.function.Function<hydra.core.Binding, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>>) (acc -> (java.util.function.Function<hydra.core.Binding, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (b -> {
          String bname = hydra.Formatting.convertCaseCamelOrUnderscoreToLowerSnake(hydra.Formatting.sanitizeWithUnderscores(
            hydra.lisp.Language.lispReservedWords(),
            (b).name.value));
          return hydra.lib.eithers.Bind.apply(
            hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
              dialect,
              cx,
              g,
              (b).term),
            (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (bval -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispApp(
              hydra.lisp.Coder.lispLambdaExpr(
                java.util.Arrays.asList(bname),
                acc),
              java.util.Arrays.asList(bval)))));
        })),
        bodyExpr,
        hydra.lib.lists.Reverse.apply(bindings))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.lisp.syntax.Expression> encodeLetAsNative(hydra.lisp.syntax.Dialect dialect, T0 cx, T1 g, java.util.List<hydra.core.Binding> bindings, hydra.core.Term body) {
    Boolean isClojureTop = (dialect).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.lisp.syntax.Dialect instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return true;
      }
    });
    return hydra.lib.eithers.Bind.apply(
      hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
        dialect,
        cx,
        g,
        body),
      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (bodyExpr -> {
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> sortedBindings = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          true,
          () -> ((java.util.function.Supplier<java.util.List<hydra.core.Binding>>) (() -> {
            hydra.util.Lazy<java.util.Set<hydra.core.Name>> allNames = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Binding, hydra.core.Name>) (b -> (b).name),
              bindings)));
            hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>>> adjList = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>>) (b -> (hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>((b).name, hydra.lib.sets.ToList.apply(hydra.lib.sets.Intersection.apply(
                allNames.get(),
                hydra.Variables.freeVariablesInTerm((b).term))))))),
              bindings));
            hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Binding>> nameToBinding = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, hydra.core.Binding>>) (b -> (hydra.util.Pair<hydra.core.Name, hydra.core.Binding>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Binding>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Binding>((b).name, b)))),
              bindings)));
            hydra.util.Lazy<hydra.util.Either<java.util.List<java.util.List<hydra.core.Name>>, java.util.List<hydra.core.Name>>> sortResult = new hydra.util.Lazy<>(() -> hydra.Sorting.topologicalSort(adjList.get()));
            return hydra.lib.eithers.Either.apply(
              (java.util.function.Function<java.util.List<java.util.List<hydra.core.Name>>, java.util.List<hydra.core.Binding>>) (ignored -> bindings),
              (java.util.function.Function<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Binding>>) (sorted -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Name, hydra.core.Binding>) (name -> hydra.lib.maybes.FromMaybe.applyLazy(
                  () -> hydra.lib.lists.Head.apply(bindings),
                  hydra.lib.maps.Lookup.apply(
                    name,
                    nameToBinding.get()))),
                sorted)),
              sortResult.get());
          })).get(),
          () -> bindings));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.util.Either<T2, hydra.util.Pair<String, hydra.lisp.syntax.Expression>>>) (b -> {
              String bname = hydra.Formatting.convertCaseCamelOrUnderscoreToLowerSnake(hydra.Formatting.sanitizeWithUnderscores(
                hydra.lisp.Language.lispReservedWords(),
                (b).name.value));
              Boolean isLambda = hydra.Strip.deannotateTerm((b).term).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public Boolean otherwise(hydra.core.Term instance) {
                  return false;
                }

                @Override
                public Boolean visit(hydra.core.Term.Function f) {
                  return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
                    @Override
                    public Boolean otherwise(hydra.core.Function instance) {
                      return false;
                    }

                    @Override
                    public Boolean visit(hydra.core.Function.Lambda ignored) {
                      return true;
                    }
                  });
                }
              });
              hydra.util.Lazy<Boolean> isSelfRef = new hydra.util.Lazy<>(() -> hydra.lib.sets.Member.apply(
                (b).name,
                hydra.Variables.freeVariablesInTerm((b).term)));
              return hydra.lib.eithers.Bind.apply(
                hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
                  dialect,
                  cx,
                  g,
                  (b).term),
                (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.util.Pair<String, hydra.lisp.syntax.Expression>>>) (bval -> {
                  Boolean isClojure = (dialect).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
                    @Override
                    public Boolean otherwise(hydra.lisp.syntax.Dialect instance) {
                      return false;
                    }

                    @Override
                    public Boolean visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
                      return true;
                    }
                  });
                  hydra.util.Lazy<hydra.lisp.syntax.Expression> wrappedVal = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                    isClojure,
                    () -> hydra.lib.logic.IfElse.lazy(
                      isSelfRef.get(),
                      () -> hydra.lib.logic.IfElse.lazy(
                        isLambda,
                        () -> (bval).accept(new hydra.lisp.syntax.Expression.PartialVisitor<>() {
                          @Override
                          public hydra.lisp.syntax.Expression otherwise(hydra.lisp.syntax.Expression instance) {
                            return bval;
                          }

                          @Override
                          public hydra.lisp.syntax.Expression visit(hydra.lisp.syntax.Expression.Lambda lam) {
                            return new hydra.lisp.syntax.Expression.Lambda(new hydra.lisp.syntax.Lambda(hydra.util.Maybe.just(new hydra.lisp.syntax.Symbol(bname)), (lam).value.params, (lam).value.restParam, (lam).value.body));
                          }
                        }),
                        () -> hydra.lisp.Coder.lispNamedLambdaExpr(
                          bname,
                          java.util.Arrays.asList("_arg"),
                          hydra.lisp.Coder.lispApp(
                            bval,
                            java.util.Arrays.asList(hydra.lisp.Coder.lispVar("_arg"))))),
                      () -> bval),
                    () -> hydra.lib.logic.IfElse.lazy(
                      hydra.lib.logic.And.apply(
                        isSelfRef.get(),
                        hydra.lib.logic.Not.apply(isLambda)),
                      () -> hydra.lisp.Coder.lispLambdaExpr(
                        java.util.Arrays.asList("_arg"),
                        hydra.lisp.Coder.lispApp(
                          bval,
                          java.util.Arrays.asList(hydra.lisp.Coder.lispVar("_arg")))),
                      () -> bval)));
                  return hydra.util.Either.<T2, hydra.util.Pair<String, hydra.lisp.syntax.Expression>>right((hydra.util.Pair<String, hydra.lisp.syntax.Expression>) ((hydra.util.Pair<String, hydra.lisp.syntax.Expression>) (new hydra.util.Pair<String, hydra.lisp.syntax.Expression>(bname, wrappedVal.get()))));
                }));
            }),
            sortedBindings.get()),
          (java.util.function.Function<java.util.List<hydra.util.Pair<String, hydra.lisp.syntax.Expression>>, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (encodedBindings -> {
            hydra.util.Lazy<java.util.Set<hydra.core.Name>> allBindingNames = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Binding, hydra.core.Name>) (b -> (b).name),
              bindings)));
            hydra.util.Lazy<Boolean> hasCrossRefs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Binding, Boolean>>) (acc -> (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.logic.Or.apply(
                acc,
                hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.lib.sets.Intersection.apply(
                  allBindingNames.get(),
                  hydra.Variables.freeVariablesInTerm((b).term))))))),
              false,
              bindings));
            hydra.util.Lazy<Boolean> hasSelfRef = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Binding, Boolean>>) (acc -> (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.logic.Or.apply(
                acc,
                hydra.lib.sets.Member.apply(
                  (b).name,
                  hydra.Variables.freeVariablesInTerm((b).term))))),
              false,
              bindings));
            Boolean isRecursive = hasSelfRef.get();
            hydra.util.Lazy<hydra.lisp.syntax.LetKind> letKind = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              isRecursive,
              () -> new hydra.lisp.syntax.LetKind.Recursive(),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.lists.Null.apply(hydra.lib.lists.Tail.apply(bindings)),
                () -> new hydra.lisp.syntax.LetKind.Parallel(),
                () -> new hydra.lisp.syntax.LetKind.Sequential())));
            hydra.util.Lazy<java.util.List<hydra.lisp.syntax.LetBinding>> lispBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<String, hydra.lisp.syntax.Expression>, hydra.lisp.syntax.LetBinding>) (eb -> new hydra.lisp.syntax.LetBinding.Simple(new hydra.lisp.syntax.SimpleBinding(new hydra.lisp.syntax.Symbol(hydra.lib.pairs.First.apply(eb)), hydra.lib.pairs.Second.apply(eb)))),
              encodedBindings));
            return hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(new hydra.lisp.syntax.Expression.Let(new hydra.lisp.syntax.LetExpression(letKind.get(), lispBindings.get(), java.util.Arrays.asList(bodyExpr))));
          }));
      }));
  }

  static hydra.lisp.syntax.Expression encodeLiteral(hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.lisp.syntax.Expression visit(hydra.core.Literal.Boolean_ b) {
        return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Boolean_((b).value));
      }

      @Override
      public hydra.lisp.syntax.Expression visit(hydra.core.Literal.String_ s) {
        return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.String_((s).value));
      }

      @Override
      public hydra.lisp.syntax.Expression visit(hydra.core.Literal.Float_ fv) {
        return (fv).value.accept(new hydra.core.FloatValue.PartialVisitor<>() {
          @Override
          public hydra.lisp.syntax.Expression visit(hydra.core.FloatValue.Float32 f) {
            return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Float_(new hydra.lisp.syntax.FloatLiteral(hydra.lib.literals.Float32ToBigfloat.apply((f).value), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
          }

          @Override
          public hydra.lisp.syntax.Expression visit(hydra.core.FloatValue.Float64 f) {
            return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Float_(new hydra.lisp.syntax.FloatLiteral(hydra.lib.literals.Float64ToBigfloat.apply((f).value), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
          }

          @Override
          public hydra.lisp.syntax.Expression visit(hydra.core.FloatValue.Bigfloat f) {
            return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Float_(new hydra.lisp.syntax.FloatLiteral((f).value, (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
          }
        });
      }

      @Override
      public hydra.lisp.syntax.Expression visit(hydra.core.Literal.Integer_ iv) {
        return (iv).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.lisp.syntax.Expression visit(hydra.core.IntegerValue.Int8 i) {
            return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Integer_(new hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.Int8ToBigint.apply((i).value), false)));
          }

          @Override
          public hydra.lisp.syntax.Expression visit(hydra.core.IntegerValue.Int16 i) {
            return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Integer_(new hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.Int16ToBigint.apply((i).value), false)));
          }

          @Override
          public hydra.lisp.syntax.Expression visit(hydra.core.IntegerValue.Int32 i) {
            return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Integer_(new hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.Int32ToBigint.apply((i).value), false)));
          }

          @Override
          public hydra.lisp.syntax.Expression visit(hydra.core.IntegerValue.Int64 i) {
            return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Integer_(new hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.Int64ToBigint.apply((i).value), false)));
          }

          @Override
          public hydra.lisp.syntax.Expression visit(hydra.core.IntegerValue.Uint8 i) {
            return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Integer_(new hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.Uint8ToBigint.apply((i).value), false)));
          }

          @Override
          public hydra.lisp.syntax.Expression visit(hydra.core.IntegerValue.Uint16 i) {
            return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Integer_(new hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.Uint16ToBigint.apply((i).value), false)));
          }

          @Override
          public hydra.lisp.syntax.Expression visit(hydra.core.IntegerValue.Uint32 i) {
            return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Integer_(new hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.Uint32ToBigint.apply((i).value), false)));
          }

          @Override
          public hydra.lisp.syntax.Expression visit(hydra.core.IntegerValue.Uint64 i) {
            return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Integer_(new hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.Uint64ToBigint.apply((i).value), false)));
          }

          @Override
          public hydra.lisp.syntax.Expression visit(hydra.core.IntegerValue.Bigint i) {
            return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Integer_(new hydra.lisp.syntax.IntegerLiteral((i).value, true)));
          }
        });
      }

      @Override
      public hydra.lisp.syntax.Expression visit(hydra.core.Literal.Binary b) {
        java.util.List<Integer> byteValues = hydra.lib.literals.BinaryToBytes.apply((b).value);
        return new hydra.lisp.syntax.Expression.Vector(new hydra.lisp.syntax.VectorLiteral(hydra.lib.lists.Map.apply(
          (java.util.function.Function<Integer, hydra.lisp.syntax.Expression>) (bv -> new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Integer_(new hydra.lisp.syntax.IntegerLiteral(hydra.lib.literals.Int32ToBigint.apply(bv), false)))),
          byteValues)));
      }
    });
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.lisp.syntax.Expression> encodeTerm(hydra.lisp.syntax.Dialect dialect, T0 cx, T1 g, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Annotated at) {
        return hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
          dialect,
          cx,
          g,
          (at).value.body);
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Application app) {
        hydra.core.Term rawArg = (app).value.argument;
        hydra.core.Term rawFun = (app).value.function;
        return hydra.lisp.Coder.<T0, T1, T2>encodeApplication(
          dialect,
          cx,
          g,
          rawFun,
          rawArg);
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (l -> hydra.lib.eithers.Bind.apply(
            hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
              dialect,
              cx,
              g,
              l),
            (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (sl -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispApp(
              hydra.lisp.Coder.lispVar("list"),
              java.util.Arrays.asList(
                hydra.lisp.Coder.lispKeyword("left"),
                sl)))))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (r -> hydra.lib.eithers.Bind.apply(
            hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
              dialect,
              cx,
              g,
              r),
            (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (sr -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispApp(
              hydra.lisp.Coder.lispVar("list"),
              java.util.Arrays.asList(
                hydra.lisp.Coder.lispKeyword("right"),
                sr)))))),
          (e).value);
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Function fun) {
        return hydra.lisp.Coder.<T0, T1, T2>encodeFunction(
          dialect,
          cx,
          g,
          (fun).value);
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = (lt).value.bindings;
        hydra.core.Term body = (lt).value.body;
        return hydra.lisp.Coder.<T0, T1, T2>encodeLetAsNative(
          dialect,
          cx,
          g,
          bindings,
          body);
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.List els) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (v1 -> hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
              dialect,
              cx,
              g,
              v1)),
            (els).value),
          (java.util.function.Function<java.util.List<hydra.lisp.syntax.Expression>, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (sels -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispListExpr(sels))));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Literal lit) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.encodeLiteral((lit).value));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Map m) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<T2, hydra.lisp.syntax.MapEntry>>) (entry -> hydra.lib.eithers.Bind.apply(
              hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
                dialect,
                cx,
                g,
                hydra.lib.pairs.First.apply(entry)),
              (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.MapEntry>>) (k -> hydra.lib.eithers.Bind.apply(
                hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
                  dialect,
                  cx,
                  g,
                  hydra.lib.pairs.Second.apply(entry)),
                (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.MapEntry>>) (v -> hydra.util.Either.<T2, hydra.lisp.syntax.MapEntry>right(new hydra.lisp.syntax.MapEntry(k, v))))))),
            hydra.lib.maps.ToList.apply((m).value)),
          (java.util.function.Function<java.util.List<hydra.lisp.syntax.MapEntry>, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (pairs -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(new hydra.lisp.syntax.Expression.Map(new hydra.lisp.syntax.MapLiteral(pairs)))));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Cases.applyLazy(
          (mt).value,
          () -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispApp(
            hydra.lisp.Coder.lispVar("list"),
            java.util.Arrays.asList(hydra.lisp.Coder.lispKeyword("nothing")))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (val -> hydra.lib.eithers.Bind.apply(
            hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
              dialect,
              cx,
              g,
              val),
            (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (sval -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispApp(
              hydra.lisp.Coder.lispVar("list"),
              java.util.Arrays.asList(
                hydra.lisp.Coder.lispKeyword("just"),
                sval)))))));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Pair p) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
            dialect,
            cx,
            g,
            hydra.lib.pairs.First.apply((p).value)),
          (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (f -> hydra.lib.eithers.Bind.apply(
            hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
              dialect,
              cx,
              g,
              hydra.lib.pairs.Second.apply((p).value)),
            (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (s -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispListExpr(java.util.Arrays.asList(
              f,
              s)))))));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Record rec) {
        java.util.List<hydra.core.Field> fields = (rec).value.fields;
        hydra.core.Name rname = (rec).value.typeName;
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Field, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (f -> hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
              dialect,
              cx,
              g,
              (f).term)),
            fields),
          (java.util.function.Function<java.util.List<hydra.lisp.syntax.Expression>, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (sfields -> {
            String constructorName = hydra.lib.strings.Cat2.apply(
              hydra.lisp.Coder.dialectConstructorPrefix(dialect),
              hydra.lisp.Coder.qualifiedSnakeName(rname));
            return hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispApp(
              hydra.lisp.Coder.lispVar(constructorName),
              sfields));
          }));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Set s) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (v1 -> hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
              dialect,
              cx,
              g,
              v1)),
            hydra.lib.sets.ToList.apply((s).value)),
          (java.util.function.Function<java.util.List<hydra.lisp.syntax.Expression>, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (sels -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(new hydra.lisp.syntax.Expression.Set(new hydra.lisp.syntax.SetLiteral(sels)))));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Union inj) {
        hydra.core.Field field = (inj).value.field;
        hydra.core.Term fterm = (field).term;
        hydra.core.Term dterm = hydra.Strip.deannotateTerm(fterm);
        String fname = (field).name.value;
        hydra.util.Lazy<Boolean> isUnit = new hydra.util.Lazy<>(() -> (dterm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Term instance) {
            return false;
          }

          @Override
          public Boolean visit(hydra.core.Term.Unit ignored) {
            return true;
          }

          @Override
          public Boolean visit(hydra.core.Term.Record rt) {
            return hydra.lib.lists.Null.apply((rt).value.fields);
          }
        }));
        String tname = hydra.Names.localNameOf((inj).value.typeName);
        return hydra.lib.logic.IfElse.lazy(
          isUnit.get(),
          () -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispApp(
            hydra.lisp.Coder.lispVar("list"),
            java.util.Arrays.asList(
              hydra.lisp.Coder.lispKeyword(hydra.Formatting.convertCaseCamelToLowerSnake(fname)),
              hydra.lisp.Coder.lispNilExpr()))),
          () -> hydra.lib.eithers.Bind.apply(
            hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
              dialect,
              cx,
              g,
              fterm),
            (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.Expression>>) (sval -> hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispApp(
              hydra.lisp.Coder.lispVar("list"),
              java.util.Arrays.asList(
                hydra.lisp.Coder.lispKeyword(hydra.Formatting.convertCaseCamelToLowerSnake(fname)),
                sval))))));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Unit ignored) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispNilExpr());
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Variable name) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.Expression>right(hydra.lisp.Coder.lispVar(hydra.Formatting.convertCaseCamelOrUnderscoreToLowerSnake(hydra.Formatting.sanitizeWithUnderscores(
          hydra.lisp.Language.lispReservedWords(),
          (name).value.value))));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
          dialect,
          cx,
          g,
          (ta).value.body);
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
          dialect,
          cx,
          g,
          (tl).value.body);
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.Expression> visit(hydra.core.Term.Wrap wt) {
        return hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
          dialect,
          cx,
          g,
          (wt).value.body);
      }
    });
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.lisp.syntax.TopLevelFormWithComments> encodeTermDefinition(hydra.lisp.syntax.Dialect dialect, T0 cx, T1 g, hydra.packaging.TermDefinition tdef) {
    hydra.core.Term term = (tdef).term;
    hydra.core.Term dterm = hydra.Strip.deannotateTerm(term);
    hydra.core.Name name = (tdef).name;
    String lname = hydra.lisp.Coder.qualifiedSnakeName(name);
    return (dterm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TopLevelFormWithComments> otherwise(hydra.core.Term instance) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
            dialect,
            cx,
            g,
            term),
          (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.TopLevelFormWithComments>>) (sterm -> hydra.util.Either.<T2, hydra.lisp.syntax.TopLevelFormWithComments>right(hydra.lisp.Coder.lispTopForm(new hydra.lisp.syntax.TopLevelForm.Variable(new hydra.lisp.syntax.VariableDefinition(new hydra.lisp.syntax.Symbol(lname), sterm, (hydra.util.Maybe<hydra.lisp.syntax.Docstring>) (hydra.util.Maybe.<hydra.lisp.syntax.Docstring>nothing())))))));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TopLevelFormWithComments> visit(hydra.core.Term.Function fun) {
        return (fun).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Either<T2, hydra.lisp.syntax.TopLevelFormWithComments> otherwise(hydra.core.Function instance) {
            return hydra.lib.eithers.Bind.apply(
              hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
                dialect,
                cx,
                g,
                term),
              (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.TopLevelFormWithComments>>) (sterm -> hydra.util.Either.<T2, hydra.lisp.syntax.TopLevelFormWithComments>right(hydra.lisp.Coder.lispTopForm(new hydra.lisp.syntax.TopLevelForm.Variable(new hydra.lisp.syntax.VariableDefinition(new hydra.lisp.syntax.Symbol(lname), sterm, (hydra.util.Maybe<hydra.lisp.syntax.Docstring>) (hydra.util.Maybe.<hydra.lisp.syntax.Docstring>nothing())))))));
          }

          @Override
          public hydra.util.Either<T2, hydra.lisp.syntax.TopLevelFormWithComments> visit(hydra.core.Function.Lambda lam) {
            return hydra.lib.eithers.Bind.apply(
              hydra.lisp.Coder.<T0, T1, T2>encodeTerm(
                dialect,
                cx,
                g,
                term),
              (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.util.Either<T2, hydra.lisp.syntax.TopLevelFormWithComments>>) (sterm -> hydra.util.Either.<T2, hydra.lisp.syntax.TopLevelFormWithComments>right(hydra.lisp.Coder.lispTopForm(new hydra.lisp.syntax.TopLevelForm.Variable(new hydra.lisp.syntax.VariableDefinition(new hydra.lisp.syntax.Symbol(lname), sterm, (hydra.util.Maybe<hydra.lisp.syntax.Docstring>) (hydra.util.Maybe.<hydra.lisp.syntax.Docstring>nothing())))))));
          }
        });
      }
    });
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> encodeType(T0 cx, T1 g, hydra.core.Type t) {
    hydra.core.Type typ = hydra.Strip.deannotateType(t);
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.TypeSpecifier>right(new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol("Any")));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Annotated at) {
        return hydra.lisp.Coder.<T0, T1, T2>encodeType(
          cx,
          g,
          (at).value.body);
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Application at) {
        return hydra.lisp.Coder.<T0, T1, T2>encodeType(
          cx,
          g,
          (at).value.function);
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Unit ignored) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.TypeSpecifier>right(new hydra.lisp.syntax.TypeSpecifier.Unit());
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Literal lt) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.TypeSpecifier>right((lt).value.accept(new hydra.core.LiteralType.PartialVisitor<>() {
          @Override
          public hydra.lisp.syntax.TypeSpecifier visit(hydra.core.LiteralType.Binary ignored) {
            return new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol("ByteArray"));
          }

          @Override
          public hydra.lisp.syntax.TypeSpecifier visit(hydra.core.LiteralType.Boolean_ ignored) {
            return new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol("Boolean"));
          }

          @Override
          public hydra.lisp.syntax.TypeSpecifier visit(hydra.core.LiteralType.Float_ ignored) {
            return new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol("Float"));
          }

          @Override
          public hydra.lisp.syntax.TypeSpecifier visit(hydra.core.LiteralType.Integer_ ignored) {
            return new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol("Integer"));
          }

          @Override
          public hydra.lisp.syntax.TypeSpecifier visit(hydra.core.LiteralType.String_ ignored) {
            return new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol("String"));
          }
        }));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.List inner) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.lisp.syntax.TypeSpecifier, hydra.lisp.syntax.TypeSpecifier>) (enc -> new hydra.lisp.syntax.TypeSpecifier.List(enc)),
          hydra.lisp.Coder.<T0, T1, T2>encodeType(
            cx,
            g,
            (inner).value));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Set inner) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.lisp.syntax.TypeSpecifier, hydra.lisp.syntax.TypeSpecifier>) (enc -> new hydra.lisp.syntax.TypeSpecifier.Set(enc)),
          hydra.lisp.Coder.<T0, T1, T2>encodeType(
            cx,
            g,
            (inner).value));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Map mt) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.TypeSpecifier>right(new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol("Map")));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Maybe inner) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.lisp.syntax.TypeSpecifier, hydra.lisp.syntax.TypeSpecifier>) (enc -> new hydra.lisp.syntax.TypeSpecifier.Maybe(enc)),
          hydra.lisp.Coder.<T0, T1, T2>encodeType(
            cx,
            g,
            (inner).value));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Either et) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.TypeSpecifier>right(new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol("Either")));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Pair pt) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.TypeSpecifier>right(new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol("Pair")));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Function ft) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.TypeSpecifier>right(new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol("Function")));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Record ignored) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.TypeSpecifier>right(new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol("Record")));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Union ignored) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.TypeSpecifier>right(new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol("Union")));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Wrap ignored) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.TypeSpecifier>right(new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol("Wrapper")));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Variable name) {
        return hydra.util.Either.<T2, hydra.lisp.syntax.TypeSpecifier>right(new hydra.lisp.syntax.TypeSpecifier.Named(new hydra.lisp.syntax.Symbol((name).value.value)));
      }

      @Override
      public hydra.util.Either<T2, hydra.lisp.syntax.TypeSpecifier> visit(hydra.core.Type.Forall fa) {
        return hydra.lisp.Coder.<T0, T1, T2>encodeType(
          cx,
          g,
          (fa).value.body);
      }
    });
  }

  static <T0> hydra.util.Either<T0, hydra.lisp.syntax.TopLevelFormWithComments> encodeTypeBody(String lname, hydra.core.Type origTyp, hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T0, hydra.lisp.syntax.TopLevelFormWithComments> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<T0, hydra.lisp.syntax.TopLevelFormWithComments>right(new hydra.lisp.syntax.TopLevelFormWithComments((hydra.util.Maybe<hydra.lisp.syntax.Docstring>) (hydra.util.Maybe.<hydra.lisp.syntax.Docstring>nothing()), hydra.util.Maybe.just(new hydra.lisp.syntax.Comment(new hydra.lisp.syntax.CommentStyle.Line(), hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            lname,
            " = "),
          hydra.show.Core.type(origTyp)))), new hydra.lisp.syntax.TopLevelForm.Expression(new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Nil()))));
      }

      @Override
      public hydra.util.Either<T0, hydra.lisp.syntax.TopLevelFormWithComments> visit(hydra.core.Type.Forall ft) {
        return hydra.lisp.Coder.<T0>encodeTypeBody(
          lname,
          origTyp,
          (ft).value.body);
      }

      @Override
      public hydra.util.Either<T0, hydra.lisp.syntax.TopLevelFormWithComments> visit(hydra.core.Type.Record rt) {
        hydra.util.Lazy<java.util.List<hydra.lisp.syntax.FieldDefinition>> fields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          hydra.lisp.Coder::encodeFieldDef,
          (rt).value));
        return hydra.util.Either.<T0, hydra.lisp.syntax.TopLevelFormWithComments>right(hydra.lisp.Coder.lispTopForm(new hydra.lisp.syntax.TopLevelForm.RecordType(new hydra.lisp.syntax.RecordTypeDefinition(new hydra.lisp.syntax.Symbol(lname), fields.get(), (hydra.util.Maybe<hydra.lisp.syntax.Docstring>) (hydra.util.Maybe.<hydra.lisp.syntax.Docstring>nothing())))));
      }

      @Override
      public hydra.util.Either<T0, hydra.lisp.syntax.TopLevelFormWithComments> visit(hydra.core.Type.Union rt) {
        hydra.util.Lazy<java.util.List<hydra.lisp.syntax.Expression>> variantNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, hydra.lisp.syntax.Expression>) (f -> new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Keyword(new hydra.lisp.syntax.Keyword(hydra.Formatting.convertCaseCamelToLowerSnake((f).name.value), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))))),
          (rt).value));
        return hydra.util.Either.<T0, hydra.lisp.syntax.TopLevelFormWithComments>right(hydra.lisp.Coder.lispTopForm(new hydra.lisp.syntax.TopLevelForm.Variable(new hydra.lisp.syntax.VariableDefinition(new hydra.lisp.syntax.Symbol(hydra.lib.strings.Cat2.apply(
          lname,
          "-variants")), hydra.lisp.Coder.lispListExpr(variantNames.get()), hydra.util.Maybe.just(new hydra.lisp.syntax.Docstring(hydra.lib.strings.Cat2.apply(
          "Variants of the ",
          lname)))))));
      }

      @Override
      public hydra.util.Either<T0, hydra.lisp.syntax.TopLevelFormWithComments> visit(hydra.core.Type.Wrap wt) {
        return hydra.util.Either.<T0, hydra.lisp.syntax.TopLevelFormWithComments>right(hydra.lisp.Coder.lispTopForm(new hydra.lisp.syntax.TopLevelForm.RecordType(new hydra.lisp.syntax.RecordTypeDefinition(new hydra.lisp.syntax.Symbol(lname), java.util.Arrays.asList(new hydra.lisp.syntax.FieldDefinition(new hydra.lisp.syntax.Symbol("value"), (hydra.util.Maybe<hydra.lisp.syntax.Expression>) (hydra.util.Maybe.<hydra.lisp.syntax.Expression>nothing()))), (hydra.util.Maybe<hydra.lisp.syntax.Docstring>) (hydra.util.Maybe.<hydra.lisp.syntax.Docstring>nothing())))));
      }
    });
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.lisp.syntax.TopLevelFormWithComments> encodeTypeDefinition(T0 cx, T1 g, hydra.packaging.TypeDefinition tdef) {
    hydra.core.Type typ = (tdef).type.type;
    hydra.core.Type dtyp = hydra.Strip.deannotateType(typ);
    hydra.core.Name name = (tdef).name;
    String lname = hydra.lisp.Coder.qualifiedSnakeName(name);
    return hydra.lisp.Coder.<T2>encodeTypeBody(
      lname,
      typ,
      dtyp);
  }

  static Boolean isCasesPrimitive(hydra.core.Name name) {
    return hydra.lib.equality.Equal.apply(
      name,
      new hydra.core.Name("hydra.lib.maybes.cases"));
  }

  static Boolean isLazy2ArgPrimitive(hydra.core.Name name) {
    return hydra.lib.logic.Or.apply(
      hydra.lib.equality.Equal.apply(
        name,
        new hydra.core.Name("hydra.lib.eithers.fromLeft")),
      hydra.lib.logic.Or.apply(
        hydra.lib.equality.Equal.apply(
          name,
          new hydra.core.Name("hydra.lib.eithers.fromRight")),
        hydra.lib.equality.Equal.apply(
          name,
          new hydra.core.Name("hydra.lib.maybes.fromMaybe"))));
  }

  static Boolean isLazy3ArgPrimitive(hydra.core.Name name) {
    return hydra.lib.equality.Equal.apply(
      name,
      new hydra.core.Name("hydra.lib.maybes.maybe"));
  }

  static Boolean isPrimitiveRef(String primName, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Term.Variable name) {
        return hydra.lib.equality.Equal.apply(
          (name).value.value,
          primName);
      }

      @Override
      public Boolean visit(hydra.core.Term.Annotated at) {
        return hydra.lisp.Coder.isPrimitiveRef(
          primName,
          (at).value.body);
      }

      @Override
      public Boolean visit(hydra.core.Term.TypeApplication ta) {
        return hydra.lisp.Coder.isPrimitiveRef(
          primName,
          (ta).value.body);
      }

      @Override
      public Boolean visit(hydra.core.Term.TypeLambda tl) {
        return hydra.lisp.Coder.isPrimitiveRef(
          primName,
          (tl).value.body);
      }
    });
  }

  static hydra.lisp.syntax.Expression lispApp(hydra.lisp.syntax.Expression fun, java.util.List<hydra.lisp.syntax.Expression> args) {
    return new hydra.lisp.syntax.Expression.Application(new hydra.lisp.syntax.Application(fun, args));
  }

  static hydra.lisp.syntax.Expression lispKeyword(String name) {
    return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Keyword(new hydra.lisp.syntax.Keyword(name, (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()))));
  }

  static hydra.lisp.syntax.Expression lispLambdaExpr(java.util.List<String> params, hydra.lisp.syntax.Expression body) {
    return new hydra.lisp.syntax.Expression.Lambda(new hydra.lisp.syntax.Lambda((hydra.util.Maybe<hydra.lisp.syntax.Symbol>) (hydra.util.Maybe.<hydra.lisp.syntax.Symbol>nothing()), hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, hydra.lisp.syntax.Symbol>) (p -> new hydra.lisp.syntax.Symbol(p)),
      params), (hydra.util.Maybe<hydra.lisp.syntax.Symbol>) (hydra.util.Maybe.<hydra.lisp.syntax.Symbol>nothing()), java.util.Arrays.asList(body)));
  }

  static hydra.lisp.syntax.Expression lispListExpr(java.util.List<hydra.lisp.syntax.Expression> elements) {
    return new hydra.lisp.syntax.Expression.List(new hydra.lisp.syntax.ListLiteral(elements, false));
  }

  static hydra.lisp.syntax.Expression lispLitExpr(hydra.lisp.syntax.Literal lit) {
    return new hydra.lisp.syntax.Expression.Literal(lit);
  }

  static hydra.lisp.syntax.Expression lispNamedLambdaExpr(String name, java.util.List<String> params, hydra.lisp.syntax.Expression body) {
    return new hydra.lisp.syntax.Expression.Lambda(new hydra.lisp.syntax.Lambda(hydra.util.Maybe.just(new hydra.lisp.syntax.Symbol(name)), hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, hydra.lisp.syntax.Symbol>) (p -> new hydra.lisp.syntax.Symbol(p)),
      params), (hydra.util.Maybe<hydra.lisp.syntax.Symbol>) (hydra.util.Maybe.<hydra.lisp.syntax.Symbol>nothing()), java.util.Arrays.asList(body)));
  }

  static hydra.lisp.syntax.Expression lispNilExpr() {
    return new hydra.lisp.syntax.Expression.Literal(new hydra.lisp.syntax.Literal.Nil());
  }

  static hydra.lisp.syntax.Symbol lispSymbol(String name) {
    return new hydra.lisp.syntax.Symbol(name);
  }

  static hydra.lisp.syntax.TopLevelFormWithComments lispTopForm(hydra.lisp.syntax.TopLevelForm form) {
    return new hydra.lisp.syntax.TopLevelFormWithComments((hydra.util.Maybe<hydra.lisp.syntax.Docstring>) (hydra.util.Maybe.<hydra.lisp.syntax.Docstring>nothing()), (hydra.util.Maybe<hydra.lisp.syntax.Comment>) (hydra.util.Maybe.<hydra.lisp.syntax.Comment>nothing()), form);
  }

  static hydra.lisp.syntax.TopLevelFormWithComments lispTopFormWithComments(hydra.util.Maybe<String> mdoc, hydra.lisp.syntax.TopLevelForm form) {
    return new hydra.lisp.syntax.TopLevelFormWithComments(hydra.lib.maybes.Map.apply(
      (java.util.function.Function<String, hydra.lisp.syntax.Docstring>) (d -> new hydra.lisp.syntax.Docstring(d)),
      mdoc), (hydra.util.Maybe<hydra.lisp.syntax.Comment>) (hydra.util.Maybe.<hydra.lisp.syntax.Comment>nothing()), form);
  }

  static hydra.lisp.syntax.Expression lispVar(String name) {
    return new hydra.lisp.syntax.Expression.Variable(new hydra.lisp.syntax.VariableReference(new hydra.lisp.syntax.Symbol(name), false));
  }

  static java.util.List<hydra.lisp.syntax.ExportDeclaration> moduleExports(java.util.List<hydra.lisp.syntax.TopLevelFormWithComments> forms) {
    hydra.util.Lazy<java.util.List<hydra.lisp.syntax.Symbol>> symbols = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.TopLevelFormWithComments, java.util.List<hydra.lisp.syntax.Symbol>>) (fwc -> {
        hydra.lisp.syntax.TopLevelForm form = (fwc).form;
        return (form).accept(new hydra.lisp.syntax.TopLevelForm.PartialVisitor<>() {
          @Override
          public java.util.List<hydra.lisp.syntax.Symbol> otherwise(hydra.lisp.syntax.TopLevelForm instance) {
            return (java.util.List<hydra.lisp.syntax.Symbol>) (java.util.Collections.<hydra.lisp.syntax.Symbol>emptyList());
          }

          @Override
          public java.util.List<hydra.lisp.syntax.Symbol> visit(hydra.lisp.syntax.TopLevelForm.Variable vd) {
            return java.util.Arrays.asList((vd).value.name);
          }

          @Override
          public java.util.List<hydra.lisp.syntax.Symbol> visit(hydra.lisp.syntax.TopLevelForm.RecordType rdef) {
            java.util.List<hydra.lisp.syntax.FieldDefinition> fields = (rdef).value.fields;
            String rname = (rdef).value.name.value;
            hydra.util.Lazy<java.util.List<hydra.lisp.syntax.Symbol>> fieldSyms = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.lisp.syntax.FieldDefinition, hydra.lisp.syntax.Symbol>) (f -> {
                String fn = (f).name.value;
                return new hydra.lisp.syntax.Symbol(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  rname,
                  "-",
                  fn)));
              }),
              fields));
            return hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              java.util.Arrays.asList(
                new hydra.lisp.syntax.Symbol(hydra.lib.strings.Cat2.apply(
                  "make-",
                  rname)),
                new hydra.lisp.syntax.Symbol(hydra.lib.strings.Cat2.apply(
                  rname,
                  "?"))),
              fieldSyms.get()));
          }
        });
      }),
      forms)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(symbols.get()),
      () -> (java.util.List<hydra.lisp.syntax.ExportDeclaration>) (java.util.Collections.<hydra.lisp.syntax.ExportDeclaration>emptyList()),
      () -> java.util.Arrays.asList(new hydra.lisp.syntax.ExportDeclaration(symbols.get())));
  }

  static java.util.List<hydra.lisp.syntax.ImportDeclaration> moduleImports(hydra.packaging.Namespace focusNs, java.util.List<hydra.packaging.Definition> defs) {
    hydra.util.Lazy<java.util.List<hydra.packaging.Namespace>> depNss = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(hydra.lib.sets.Delete.apply(
      focusNs,
      hydra.Analysis.definitionDependencyNamespaces(defs))));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Namespace, hydra.lisp.syntax.ImportDeclaration>) (ns -> new hydra.lisp.syntax.ImportDeclaration(new hydra.lisp.syntax.NamespaceName((ns).value), new hydra.lisp.syntax.ImportSpec.All())),
      depNss.get());
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.lisp.syntax.Program> moduleToLisp(hydra.lisp.syntax.Dialect dialect, hydra.packaging.Module mod, java.util.List<hydra.packaging.Definition> defs0, T0 cx, T1 g) {
    java.util.List<hydra.packaging.Definition> defs = hydra.Environment.reorderDefs(defs0);
    hydra.util.Pair<java.util.List<hydra.packaging.TypeDefinition>, java.util.List<hydra.packaging.TermDefinition>> partitioned = hydra.Environment.partitionDefinitions(defs);
    hydra.util.Lazy<java.util.List<hydra.packaging.TypeDefinition>> allTypeDefs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(partitioned));
    hydra.util.Lazy<java.util.List<hydra.packaging.TermDefinition>> termDefs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(partitioned));
    hydra.util.Lazy<java.util.List<hydra.packaging.TypeDefinition>> typeDefs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.packaging.TypeDefinition, Boolean>) (td -> hydra.Predicates.isNominalType((td).type.type)),
      allTypeDefs.get()));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.packaging.TypeDefinition, hydra.util.Either<T2, hydra.lisp.syntax.TopLevelFormWithComments>>) (v1 -> hydra.lisp.Coder.<T0, T1, T2>encodeTypeDefinition(
          cx,
          g,
          v1)),
        typeDefs.get()),
      (java.util.function.Function<java.util.List<hydra.lisp.syntax.TopLevelFormWithComments>, hydra.util.Either<T2, hydra.lisp.syntax.Program>>) (typeItems -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.packaging.TermDefinition, hydra.util.Either<T2, hydra.lisp.syntax.TopLevelFormWithComments>>) (v1 -> hydra.lisp.Coder.<T0, T1, T2>encodeTermDefinition(
            dialect,
            cx,
            g,
            v1)),
          termDefs.get()),
        (java.util.function.Function<java.util.List<hydra.lisp.syntax.TopLevelFormWithComments>, hydra.util.Either<T2, hydra.lisp.syntax.Program>>) (termItems -> {
          hydra.util.Lazy<java.util.List<hydra.lisp.syntax.TopLevelFormWithComments>> allItems = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
            typeItems,
            termItems));
          java.util.List<hydra.lisp.syntax.ExportDeclaration> exports = hydra.lisp.Coder.moduleExports(allItems.get());
          hydra.packaging.Namespace focusNs = (mod).namespace;
          java.util.List<hydra.lisp.syntax.ImportDeclaration> imports = hydra.lisp.Coder.moduleImports(
            focusNs,
            defs);
          String nsName = (mod).namespace.value;
          return hydra.util.Either.<T2, hydra.lisp.syntax.Program>right(new hydra.lisp.syntax.Program(dialect, hydra.util.Maybe.just(new hydra.lisp.syntax.ModuleDeclaration(new hydra.lisp.syntax.NamespaceName(nsName), (hydra.util.Maybe<hydra.lisp.syntax.Docstring>) (hydra.util.Maybe.<hydra.lisp.syntax.Docstring>nothing()))), imports, exports, allItems.get()));
        }))));
  }

  static String qualifiedSnakeName(hydra.core.Name name) {
    String raw = (name).value;
    java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      raw);
    hydra.util.Lazy<java.util.List<String>> snakeParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, String>) (p -> hydra.Formatting.convertCaseCamelOrUnderscoreToLowerSnake(p)),
      parts));
    String joined = hydra.lib.strings.Intercalate.apply(
      "_",
      snakeParts.get());
    return hydra.Formatting.sanitizeWithUnderscores(
      hydra.lisp.Language.lispReservedWords(),
      joined);
  }

  static String qualifiedTypeName(hydra.core.Name name) {
    return hydra.Formatting.capitalize(hydra.Names.localNameOf(name));
  }

  static hydra.lisp.syntax.Expression wrapInThunk(hydra.lisp.syntax.Expression expr) {
    return new hydra.lisp.syntax.Expression.Lambda(new hydra.lisp.syntax.Lambda((hydra.util.Maybe<hydra.lisp.syntax.Symbol>) (hydra.util.Maybe.<hydra.lisp.syntax.Symbol>nothing()), (java.util.List<hydra.lisp.syntax.Symbol>) (java.util.Collections.<hydra.lisp.syntax.Symbol>emptyList()), (hydra.util.Maybe<hydra.lisp.syntax.Symbol>) (hydra.util.Maybe.<hydra.lisp.syntax.Symbol>nothing()), java.util.Arrays.asList(expr)));
  }
}
