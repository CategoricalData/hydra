// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Functions for reducing terms and types, i.e. performing computations.
 */
public interface Reduction {
  static hydra.core.Term alphaConvert(hydra.core.Name vold, hydra.core.Name vnew, hydra.core.Term term) {
    return hydra.Rewriting.replaceFreeTermVariable(
      vold,
      new hydra.core.Term.Variable(vnew),
      term);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type> betaReduceType(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Type typ) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.ApplicationType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>> reduceApp = new java.util.concurrent.atomic.AtomicReference<>();
    reduceApp.set((java.util.function.Function<hydra.core.ApplicationType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (app -> {
      hydra.core.Type lhs = (app).function;
      hydra.core.Type rhs = (app).argument;
      return (lhs).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type> visit(hydra.core.Type.Annotated at) {
          return hydra.lib.eithers.Bind.apply(
            reduceApp.get().apply(new hydra.core.ApplicationType((at).value.body, rhs)),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (a -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right(new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(a, (at).value.annotation)))));
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type> visit(hydra.core.Type.Forall ft) {
          return hydra.Reduction.betaReduceType(
            cx,
            graph,
            hydra.Rewriting.replaceFreeTypeVariable(
              (ft).value.parameter,
              rhs,
              (ft).value.body));
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type> visit(hydra.core.Type.Variable name) {
          return hydra.lib.eithers.Bind.apply(
            hydra.Schemas.requireType(
              cx,
              graph,
              (name).value),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (t_ -> hydra.Reduction.betaReduceType(
              cx,
              graph,
              new hydra.core.Type.Application(new hydra.core.ApplicationType(t_, rhs)))));
        }
      });
    }));
    return hydra.Rewriting.rewriteTypeM(
      (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>>) (v1 -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (v2 -> hydra.Reduction.betaReduceType_mapExpr(
        reduceApp.get(),
        v1,
        v2))),
      typ);
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type> betaReduceType_mapExpr(java.util.function.Function<hydra.core.ApplicationType, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>> reduceApp, java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>> recurse, T0 t) {
    java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>> findApp = (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (r -> (r).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>right(r);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type> visit(hydra.core.Type.Application a) {
        return (reduceApp).apply((a).value);
      }
    }));
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply(t),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (r -> (findApp).apply(r)));
  }

  static hydra.core.Term contractTerm(hydra.core.Term term) {
    return hydra.Rewriting.rewriteTerm(
      (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v2 -> hydra.Reduction.contractTerm_rewrite(
        hydra.Rewriting::deannotateTerm,
        (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, Boolean>>) (p0 -> p1 -> hydra.Rewriting.isFreeVariableInTerm(
          p0,
          p1)),
        (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (p0 -> p1 -> p2 -> hydra.Rewriting.replaceFreeTermVariable(
          p0,
          p1,
          p2)),
        v1,
        v2))),
      term);
  }

  static <T0> hydra.core.Term contractTerm_rewrite(java.util.function.Function<hydra.core.Term, hydra.core.Term> hydra_rewriting_deannotateTerm2, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, Boolean>> hydra_rewriting_isFreeVariableInTerm2, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> hydra_rewriting_replaceFreeTermVariable2, java.util.function.Function<T0, hydra.core.Term> recurse, T0 t) {
    hydra.core.Term rec = (recurse).apply(t);
    return (rec).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return rec;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = (app).value.function;
        hydra.core.Term rhs = (app).value.argument;
        return (hydra_rewriting_deannotateTerm2).apply(lhs).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Term instance) {
            return rec;
          }

          @Override
          public hydra.core.Term visit(hydra.core.Term.Function f) {
            return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public hydra.core.Term otherwise(hydra.core.Function instance) {
                return rec;
              }

              @Override
              public hydra.core.Term visit(hydra.core.Function.Lambda l) {
                hydra.core.Term body = (l).value.body;
                hydra.core.Name v = (l).value.parameter;
                return hydra.lib.logic.IfElse.lazy(
                  (hydra_rewriting_isFreeVariableInTerm2).apply(v).apply(body),
                  () -> body,
                  () -> (hydra_rewriting_replaceFreeTermVariable2).apply(v).apply(rhs).apply(body));
              }
            });
          }
        });
      }
    });
  }

  static Boolean countPrimitiveInvocations() {
    return true;
  }

  static hydra.core.Term etaReduceTerm(hydra.core.Term term) {
    hydra.core.Term noChange = term;
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Lambda, hydra.core.Term>> reduceLambda = new java.util.concurrent.atomic.AtomicReference<>();
    reduceLambda.set((java.util.function.Function<hydra.core.Lambda, hydra.core.Term>) (l -> {
      hydra.core.Term body = (l).body;
      hydra.util.Maybe<hydra.core.Type> d = (l).domain;
      hydra.core.Name v = (l).parameter;
      return hydra.Reduction.etaReduceTerm(body).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return noChange;
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Annotated at) {
          return reduceLambda.get().apply(new hydra.core.Lambda(v, d, (at).value.body));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Application app) {
          hydra.core.Term lhs = (app).value.function;
          hydra.core.Term rhs = (app).value.argument;
          return hydra.Reduction.etaReduceTerm(rhs).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.core.Term otherwise(hydra.core.Term instance) {
              return noChange;
            }

            @Override
            public hydra.core.Term visit(hydra.core.Term.Annotated at) {
              return reduceLambda.get().apply(new hydra.core.Lambda(v, d, new hydra.core.Term.Application(new hydra.core.Application(lhs, (at).value.body))));
            }

            @Override
            public hydra.core.Term visit(hydra.core.Term.Variable v1) {
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.logic.And.apply(
                  hydra.lib.equality.Equal.apply(
                    (v).value,
                    (v1).value.value),
                  hydra.lib.logic.Not.apply(hydra.Rewriting.isFreeVariableInTerm(
                    v,
                    lhs))),
                () -> hydra.Reduction.etaReduceTerm(lhs),
                () -> noChange);
            }
          });
        }
      });
    }));
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return noChange;
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.Reduction.etaReduceTerm((at).value.body), (at).value.annotation));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return noChange;
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            return reduceLambda.get().apply((l).value);
          }
        });
      }
    });
  }

  static hydra.core.Term etaExpandTerm(hydra.graph.Graph graph, hydra.core.Term term) {
    java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> expand = (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (args -> (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (arity -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> {
      hydra.util.Lazy<hydra.core.Term> apps = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (lhs -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (arg -> new hydra.core.Term.Application(new hydra.core.Application(lhs, arg)))),
        t,
        args));
      hydra.util.Lazy<hydra.util.ConsList<Integer>> is = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Lte.apply(
          arity,
          hydra.lib.lists.Length.apply(args)),
        () -> (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()),
        () -> hydra.lib.math.Range.apply(
          1,
          hydra.lib.math.Sub.apply(
            arity,
            hydra.lib.lists.Length.apply(args)))));
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.ConsList<Integer>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> pad = new java.util.concurrent.atomic.AtomicReference<>();
      pad.set((java.util.function.Function<hydra.util.ConsList<Integer>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (indices -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t2 -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(indices),
        () -> t2,
        () -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name(hydra.lib.strings.Cat2.apply(
          "v",
          hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Head.apply(indices)))), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), pad.get().apply(hydra.lib.lists.Tail.apply(indices)).apply(new hydra.core.Term.Application(new hydra.core.Application(t2, new hydra.core.Term.Variable(new hydra.core.Name(hydra.lib.strings.Cat2.apply(
          "v",
          hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Head.apply(indices)))))))))))))));
      return pad.get().apply(is.get()).apply(apps.get());
    })));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>> rewrite = new java.util.concurrent.atomic.AtomicReference<>();
    rewrite.set((java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (args -> (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> {
      java.util.function.Function<hydra.core.Term, hydra.core.Term> afterRecursion = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> (expand).apply(args).apply(hydra.Reduction.etaExpansionArity(
        graph,
        term2)).apply(term2));
      hydra.core.Term t2 = hydra.Rewriting.detypeTerm(t);
      return (t2).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return (afterRecursion).apply((recurse).apply(t2));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Application app) {
          hydra.core.Term rhs = (app).value.argument;
          hydra.util.Lazy<hydra.core.Term> erhs = new hydra.util.Lazy<>(() -> rewrite.get().apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())).apply(recurse).apply(rhs));
          hydra.core.Term lhs = (app).value.function;
          return rewrite.get().apply(hydra.lib.lists.Cons.apply(
            erhs.get(),
            args)).apply(recurse).apply(lhs);
        }
      });
    }))));
    return hydra.Reduction.contractTerm(hydra.Rewriting.rewriteTerm(
      (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v2 -> rewrite.get().apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())).apply(v1).apply(v2))),
      term));
  }

  static hydra.core.Term etaExpandTermNew(hydra.graph.Graph tx0, hydra.core.Term term0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Integer, java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.ConsList<hydra.util.Maybe<hydra.core.Type>>>>> domainTypes = new java.util.concurrent.atomic.AtomicReference<>();
    domainTypes.set((java.util.function.Function<Integer, java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.ConsList<hydra.util.Maybe<hydra.core.Type>>>>) (n -> (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.ConsList<hydra.util.Maybe<hydra.core.Type>>>) (mt -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Lte.apply(
        n,
        0),
      () -> (hydra.util.ConsList<hydra.util.Maybe<hydra.core.Type>>) (hydra.util.ConsList.<hydra.util.Maybe<hydra.core.Type>>empty()),
      () -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<Integer, hydra.util.Maybe<hydra.core.Type>>) (ignored -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
          hydra.lib.math.Range.apply(
            1,
            n)),
        (java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.util.Maybe<hydra.core.Type>>>) (typ -> (typ).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.ConsList<hydra.util.Maybe<hydra.core.Type>> otherwise(hydra.core.Type instance) {
            return hydra.lib.lists.Map.apply(
              (java.util.function.Function<Integer, hydra.util.Maybe<hydra.core.Type>>) (ignored -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
              hydra.lib.math.Range.apply(
                1,
                n));
          }

          @Override
          public hydra.util.ConsList<hydra.util.Maybe<hydra.core.Type>> visit(hydra.core.Type.Function ftyp) {
            return hydra.lib.lists.Cons.apply(
              hydra.util.Maybe.just((ftyp).value.domain),
              domainTypes.get().apply(hydra.lib.math.Sub.apply(
                n,
                1)).apply(hydra.util.Maybe.just((ftyp).value.codomain)));
          }

          @Override
          public hydra.util.ConsList<hydra.util.Maybe<hydra.core.Type>> visit(hydra.core.Type.Annotated at) {
            return domainTypes.get().apply(n).apply(hydra.util.Maybe.just((at).value.body));
          }

          @Override
          public hydra.util.ConsList<hydra.util.Maybe<hydra.core.Type>> visit(hydra.core.Type.Application atyp) {
            return domainTypes.get().apply(n).apply(hydra.util.Maybe.just((atyp).value.function));
          }

          @Override
          public hydra.util.ConsList<hydra.util.Maybe<hydra.core.Type>> visit(hydra.core.Type.Forall ft) {
            return domainTypes.get().apply(n).apply(hydra.util.Maybe.just((ft).value.body));
          }
        })),
        mt)))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, java.util.function.Function<Integer, hydra.util.Maybe<hydra.core.Type>>>> peelFunctionDomains = new java.util.concurrent.atomic.AtomicReference<>();
    peelFunctionDomains.set((java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, java.util.function.Function<Integer, hydra.util.Maybe<hydra.core.Type>>>) (mtyp -> (java.util.function.Function<Integer, hydra.util.Maybe<hydra.core.Type>>) (n -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Lte.apply(
        n,
        0),
      () -> mtyp,
      () -> hydra.lib.maybes.Maybe.applyLazy(
        () -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()),
        (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (typ -> (typ).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Type instance) {
            return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Type.Function ftyp) {
            return peelFunctionDomains.get().apply(hydra.util.Maybe.just((ftyp).value.codomain)).apply(hydra.lib.math.Sub.apply(
              n,
              1));
          }

          @Override
          public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Type.Annotated at) {
            return peelFunctionDomains.get().apply(hydra.util.Maybe.just((at).value.body)).apply(n);
          }

          @Override
          public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Type.Application atyp) {
            return peelFunctionDomains.get().apply(hydra.util.Maybe.just((atyp).value.function)).apply(n);
          }

          @Override
          public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Type.Forall ft) {
            return peelFunctionDomains.get().apply(hydra.util.Maybe.just((ft).value.body)).apply(n);
          }
        })),
        mtyp)))));
    java.util.function.Function<Boolean, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<Integer, java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>>> expand = (java.util.function.Function<Boolean, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<Integer, java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>>>) (alwaysPad -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<Integer, java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>>) (args -> (java.util.function.Function<Integer, java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (arity -> (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (headTyp -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (head -> {
      hydra.util.Lazy<hydra.core.Term> applied = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (lhs -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (arg -> new hydra.core.Term.Application(new hydra.core.Application(lhs, arg)))),
        head,
        args));
      hydra.util.Lazy<Integer> numArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(args));
      Integer needed = hydra.lib.math.Sub.apply(
        arity,
        numArgs.get());
      return hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.And.apply(
          hydra.lib.equality.Gt.apply(
            needed,
            0),
          hydra.lib.logic.Or.apply(
            alwaysPad,
            hydra.lib.equality.Gt.apply(
              numArgs.get(),
              0))),
        () -> ((java.util.function.Supplier<hydra.core.Term>) (() -> {
          hydra.util.ConsList<Integer> indices = hydra.lib.math.Range.apply(
            1,
            needed);
          return ((java.util.function.Supplier<hydra.core.Term>) (() -> {
            hydra.util.Maybe<hydra.core.Type> remainingType = peelFunctionDomains.get().apply(headTyp).apply(numArgs.get());
            return ((java.util.function.Supplier<hydra.core.Term>) (() -> {
              hydra.util.ConsList<hydra.util.Maybe<hydra.core.Type>> domains = domainTypes.get().apply(needed).apply(remainingType);
              return ((java.util.function.Supplier<hydra.core.Term>) (() -> {
                hydra.util.Maybe<hydra.core.Type> codomainType = peelFunctionDomains.get().apply(remainingType).apply(needed);
                return ((java.util.function.Supplier<hydra.core.Term>) (() -> {
                  hydra.util.Lazy<hydra.core.Term> fullyAppliedRaw = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                    (java.util.function.Function<hydra.core.Term, java.util.function.Function<Integer, hydra.core.Term>>) (body -> (java.util.function.Function<Integer, hydra.core.Term>) (i -> {
                      hydra.core.Name vn = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                        "v",
                        hydra.lib.literals.ShowInt32.apply(i)));
                      return new hydra.core.Term.Application(new hydra.core.Application(body, new hydra.core.Term.Variable(vn)));
                    })),
                    applied.get(),
                    indices));
                  return ((java.util.function.Supplier<hydra.core.Term>) (() -> {
                    hydra.util.Lazy<hydra.core.Term> fullyApplied = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
                      () -> fullyAppliedRaw.get(),
                      (java.util.function.Function<hydra.core.Type, hydra.core.Term>) (ct -> new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(fullyAppliedRaw.get(), hydra.lib.maps.Singleton.apply(
                        new hydra.core.Name("type"),
                        hydra.encode.Core.type(ct))))),
                      codomainType));
                    return ((java.util.function.Supplier<hydra.core.Term>) (() -> {
                      hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<Integer, hydra.util.Maybe<hydra.core.Type>>>> indexedDomains = new hydra.util.Lazy<>(() -> hydra.lib.lists.Zip.apply(
                        indices,
                        domains));
                      return hydra.lib.lists.Foldl.apply(
                        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Pair<Integer, hydra.util.Maybe<hydra.core.Type>>, hydra.core.Term>>) (body -> (java.util.function.Function<hydra.util.Pair<Integer, hydra.util.Maybe<hydra.core.Type>>, hydra.core.Term>) (idPair -> {
                          hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> dom = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(idPair));
                          hydra.util.Lazy<Integer> i = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(idPair));
                          hydra.core.Name vn = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                            "v",
                            hydra.lib.literals.ShowInt32.apply(i.get())));
                          return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(vn, dom.get(), body)));
                        })),
                        fullyApplied.get(),
                        hydra.lib.lists.Reverse.apply(indexedDomains.get()));
                    })).get();
                  })).get();
                })).get();
              })).get();
            })).get();
          })).get();
        })).get(),
        () -> applied.get());
    })))));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>> primTypes = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.graph.Primitive, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (_gpt_p -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>((_gpt_p).name, (_gpt_p).type)))),
      hydra.lib.maps.Elems.apply((tx0).primitives))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, Integer>>> termArityWithContext = new java.util.concurrent.atomic.AtomicReference<>();
    termArityWithContext.set((java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, Integer>>) (tx -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Term instance) {
        return 0;
      }

      @Override
      public Integer visit(hydra.core.Term.Annotated at) {
        return termArityWithContext.get().apply(tx).apply((at).value.body);
      }

      @Override
      public Integer visit(hydra.core.Term.Application app) {
        return hydra.lib.math.Sub.apply(
          termArityWithContext.get().apply(tx).apply((app).value.function),
          1);
      }

      @Override
      public Integer visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public Integer visit(hydra.core.Function.Elimination ignored) {
            return 1;
          }

          @Override
          public Integer visit(hydra.core.Function.Lambda ignored) {
            return 0;
          }

          @Override
          public Integer visit(hydra.core.Function.Primitive name) {
            return hydra.lib.maybes.Maybe.applyLazy(
              () -> 0,
              hydra.Arity::typeSchemeArity,
              hydra.lib.maps.Lookup.apply(
                (name).value,
                primTypes.get()));
          }
        });
      }

      @Override
      public Integer visit(hydra.core.Term.Let l) {
        return termArityWithContext.get().apply(hydra.Rewriting.extendGraphForLet(
          (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
          tx,
          (l).value)).apply((l).value.body);
      }

      @Override
      public Integer visit(hydra.core.Term.TypeLambda tl) {
        return termArityWithContext.get().apply(hydra.Rewriting.extendGraphForTypeLambda(
          tx,
          (tl).value)).apply((tl).value.body);
      }

      @Override
      public Integer visit(hydra.core.Term.TypeApplication tat) {
        return termArityWithContext.get().apply(tx).apply((tat).value.body);
      }

      @Override
      public Integer visit(hydra.core.Term.Variable name) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> 0,
          hydra.Arity::typeArity,
          hydra.lib.maybes.Map.apply(
            hydra.Rewriting::typeSchemeToFType,
            hydra.lib.maps.Lookup.apply(
              (name).value,
              (tx).boundTypes)));
      }
    }))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>> rewriteWithArgs = new java.util.concurrent.atomic.AtomicReference<>();
    rewriteWithArgs.set((java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (args -> (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (tx -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> {
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Type>>>> termHeadType = new java.util.concurrent.atomic.AtomicReference<>();
      termHeadType.set((java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Type>>>) (tx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Type>>) (trm2 -> (trm2).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Term instance) {
          return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
        }

        @Override
        public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Annotated at2) {
          return termHeadType.get().apply(tx2).apply((at2).value.body);
        }

        @Override
        public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Function f2) {
          return (f2).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Function instance) {
              return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
            }

            @Override
            public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Function.Primitive pn2) {
              return hydra.lib.maybes.Map.apply(
                (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type>) (ts2 -> (ts2).type),
                hydra.lib.maps.Lookup.apply(
                  (pn2).value,
                  primTypes.get()));
            }
          });
        }

        @Override
        public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Let l2) {
          return termHeadType.get().apply(hydra.Rewriting.extendGraphForLet(
            (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
            tx2,
            (l2).value)).apply((l2).value.body);
        }

        @Override
        public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.TypeLambda tl2) {
          return termHeadType.get().apply(hydra.Rewriting.extendGraphForTypeLambda(
            tx2,
            (tl2).value)).apply((tl2).value.body);
        }

        @Override
        public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.TypeApplication tat2) {
          return hydra.lib.maybes.Bind.apply(
            termHeadType.get().apply(tx2).apply((tat2).value.body),
            (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (htyp2 -> (htyp2).accept(new hydra.core.Type.PartialVisitor<>() {
              @Override
              public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Type instance) {
                return hydra.util.Maybe.just(htyp2);
              }

              @Override
              public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Type.Forall ft2) {
                return hydra.util.Maybe.just(hydra.Rewriting.replaceFreeTypeVariable(
                  (ft2).value.parameter,
                  (tat2).value.type,
                  (ft2).value.body));
              }
            })));
        }

        @Override
        public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Term.Variable vn2) {
          return hydra.lib.maybes.Map.apply(
            hydra.Rewriting::typeSchemeToFType,
            hydra.lib.maps.Lookup.apply(
              (vn2).value,
              (tx2).boundTypes));
        }
      }))));
      java.util.function.Function<hydra.core.Term, hydra.core.Term> afterRecursion = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (trm -> {
        Integer arity = termArityWithContext.get().apply(tx).apply(trm);
        hydra.util.Maybe<hydra.core.Type> hType = termHeadType.get().apply(tx).apply(trm);
        return (expand).apply(false).apply(args).apply(arity).apply(hType).apply(trm);
      });
      java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.core.Term>> recurse = (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (tx1 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term1 -> rewriteWithArgs.get().apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())).apply(tx1).apply(term1)));
      java.util.function.Function<hydra.core.Field, hydra.core.Field> forCaseBranch = (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (f -> {
        hydra.core.Term branchBody = (recurse).apply(tx).apply((f).term);
        Integer arty = termArityWithContext.get().apply(tx).apply(branchBody);
        hydra.util.Maybe<hydra.core.Type> branchHType = termHeadType.get().apply(tx).apply(branchBody);
        return new hydra.core.Field((f).name, (expand).apply(true).apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())).apply(arty).apply(branchHType).apply(branchBody));
      });
      java.util.function.Function<hydra.core.Elimination, hydra.core.Elimination> forElimination = (java.util.function.Function<hydra.core.Elimination, hydra.core.Elimination>) (elm -> (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
        @Override
        public hydra.core.Elimination visit(hydra.core.Elimination.Record p) {
          return new hydra.core.Elimination.Record((p).value);
        }

        @Override
        public hydra.core.Elimination visit(hydra.core.Elimination.Union cs) {
          return new hydra.core.Elimination.Union(new hydra.core.CaseStatement((cs).value.typeName, hydra.lib.maybes.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t1 -> (recurse).apply(tx).apply(t1)),
            (cs).value.default_), hydra.lib.lists.Map.apply(
            forCaseBranch,
            (cs).value.cases)));
        }

        @Override
        public hydra.core.Elimination visit(hydra.core.Elimination.Wrap nm) {
          return new hydra.core.Elimination.Wrap((nm).value);
        }
      }));
      java.util.function.Function<hydra.core.Field, hydra.core.Field> forField = (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (f -> new hydra.core.Field((f).name, (recurse).apply(tx).apply((f).term)));
      java.util.function.Function<hydra.util.PersistentMap<hydra.core.Term, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Term, hydra.core.Term>> forMap = (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Term, hydra.core.Term>, hydra.util.PersistentMap<hydra.core.Term, hydra.core.Term>>) (mp -> {
        java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>> forPair = (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (pr -> (hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>((recurse).apply(tx).apply(hydra.lib.pairs.First.apply(pr)), (recurse).apply(tx).apply(hydra.lib.pairs.Second.apply(pr))))));
        return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          forPair,
          hydra.lib.maps.ToList.apply(mp)));
      });
      return (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term visit(hydra.core.Term.Annotated at) {
          return (afterRecursion).apply(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm((recurse).apply(tx).apply((at).value.body), (at).value.annotation)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Application app) {
          hydra.util.Lazy<hydra.core.Term> rhs = new hydra.util.Lazy<>(() -> rewriteWithArgs.get().apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())).apply(tx).apply((app).value.argument));
          return rewriteWithArgs.get().apply(hydra.lib.lists.Cons.apply(
            rhs.get(),
            args)).apply(tx).apply((app).value.function);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Either e) {
          return (afterRecursion).apply(new hydra.core.Term.Either(hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (l -> hydra.util.Either.<hydra.core.Term, hydra.core.Term>left((recurse).apply(tx).apply(l))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (r -> hydra.util.Either.<hydra.core.Term, hydra.core.Term>right((recurse).apply(tx).apply(r))),
            (e).value)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Function fn) {
          return (fn).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.core.Term visit(hydra.core.Function.Elimination elm) {
              hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> elimHeadType = new hydra.util.Lazy<>(() -> (elm).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                @Override
                public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Elimination instance) {
                  return (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing());
                }

                @Override
                public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Elimination.Union cs2) {
                  return hydra.util.Maybe.just(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable((cs2).value.typeName), new hydra.core.Type.Unit())));
                }
              }));
              hydra.core.Term elimTerm = new hydra.core.Term.Function(new hydra.core.Function.Elimination((forElimination).apply((elm).value)));
              Boolean padElim = (elm).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                @Override
                public Boolean visit(hydra.core.Elimination.Record ignored) {
                  return false;
                }

                @Override
                public Boolean visit(hydra.core.Elimination.Union ignored) {
                  return true;
                }

                @Override
                public Boolean visit(hydra.core.Elimination.Wrap ignored) {
                  return false;
                }
              });
              return (expand).apply(padElim).apply(args).apply(1).apply(elimHeadType.get()).apply(elimTerm);
            }

            @Override
            public hydra.core.Term visit(hydra.core.Function.Lambda lm) {
              hydra.graph.Graph tx1 = hydra.Rewriting.extendGraphForLambda(
                tx,
                (lm).value);
              hydra.util.Lazy<hydra.core.Term> body = new hydra.util.Lazy<>(() -> rewriteWithArgs.get().apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())).apply(tx1).apply((lm).value.body));
              hydra.core.Term result = new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((lm).value.parameter, (lm).value.domain, body.get())));
              Integer arty = termArityWithContext.get().apply(tx).apply(result);
              return (expand).apply(false).apply(args).apply(arty).apply((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())).apply(result);
            }

            @Override
            public hydra.core.Term visit(hydra.core.Function.Primitive pn) {
              Integer arty = termArityWithContext.get().apply(tx).apply(term);
              hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> primType = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
                (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type>) (ts -> (ts).type),
                hydra.lib.maps.Lookup.apply(
                  (pn).value,
                  primTypes.get())));
              return (expand).apply(false).apply(args).apply(arty).apply(primType.get()).apply(term);
            }
          });
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Let lt) {
          hydra.util.Lazy<hydra.graph.Graph> tx1 = new hydra.util.Lazy<>(() -> hydra.Rewriting.extendGraphForLet(
            (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
            tx,
            (lt).value));
          java.util.function.Function<hydra.core.Binding, hydra.core.Binding> mapBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, rewriteWithArgs.get().apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())).apply(tx1.get()).apply((b).term), (b).type));
          hydra.util.Lazy<hydra.core.Term> result = new hydra.util.Lazy<>(() -> new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
            mapBinding,
            (lt).value.bindings), rewriteWithArgs.get().apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())).apply(tx1.get()).apply((lt).value.body))));
          return (afterRecursion).apply(result.get());
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.List els) {
          return (afterRecursion).apply(new hydra.core.Term.List(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> (recurse).apply(tx).apply(el)),
            (els).value)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Literal v) {
          return new hydra.core.Term.Literal((v).value);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Map mp) {
          return (afterRecursion).apply(new hydra.core.Term.Map((forMap).apply((mp).value)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Maybe mb) {
          return (afterRecursion).apply(new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v -> (recurse).apply(tx).apply(v)),
            (mb).value)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Pair pr) {
          return (afterRecursion).apply(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>((recurse).apply(tx).apply(hydra.lib.pairs.First.apply((pr).value)), (recurse).apply(tx).apply(hydra.lib.pairs.Second.apply((pr).value)))))));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Record rc) {
          return (afterRecursion).apply(new hydra.core.Term.Record(new hydra.core.Record((rc).value.typeName, hydra.lib.lists.Map.apply(
            forField,
            (rc).value.fields))));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Set st) {
          return (afterRecursion).apply(new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> (recurse).apply(tx).apply(el)),
            hydra.lib.sets.ToList.apply((st).value)))));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
          return (afterRecursion).apply(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((recurse).apply(tx).apply((tt).value.body), (tt).value.type)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
          hydra.graph.Graph tx1 = hydra.Rewriting.extendGraphForTypeLambda(
            tx,
            (tl).value);
          hydra.util.Lazy<hydra.core.Term> result = new hydra.util.Lazy<>(() -> new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda((tl).value.parameter, rewriteWithArgs.get().apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())).apply(tx1).apply((tl).value.body))));
          return (afterRecursion).apply(result.get());
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Union inj) {
          return (afterRecursion).apply(new hydra.core.Term.Union(new hydra.core.Injection((inj).value.typeName, (forField).apply((inj).value.field))));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Unit ignored) {
          return new hydra.core.Term.Unit();
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Variable vn) {
          Integer arty = termArityWithContext.get().apply(tx).apply(term);
          hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> varType = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
            hydra.Rewriting::typeSchemeToFType,
            hydra.lib.maps.Lookup.apply(
              (vn).value,
              (tx).boundTypes)));
          return (expand).apply(false).apply(args).apply(arty).apply(varType.get()).apply(term);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Wrap wt) {
          return (afterRecursion).apply(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm((wt).value.typeName, (recurse).apply(tx).apply((wt).value.body))));
        }
      });
    }))));
    return hydra.Reduction.contractTerm(rewriteWithArgs.get().apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())).apply(tx0).apply(term0));
  }

  static Integer etaExpansionArity(hydra.graph.Graph graph, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Term instance) {
        return 0;
      }

      @Override
      public Integer visit(hydra.core.Term.Annotated at) {
        return hydra.Reduction.etaExpansionArity(
          graph,
          (at).value.body);
      }

      @Override
      public Integer visit(hydra.core.Term.Application app) {
        return hydra.lib.math.Sub.apply(
          hydra.Reduction.etaExpansionArity(
            graph,
            (app).value.function),
          1);
      }

      @Override
      public Integer visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public Integer visit(hydra.core.Function.Elimination ignored) {
            return 1;
          }

          @Override
          public Integer visit(hydra.core.Function.Lambda ignored) {
            return 0;
          }

          @Override
          public Integer visit(hydra.core.Function.Primitive name) {
            return hydra.Arity.primitiveArity(hydra.lib.maybes.FromJust.apply(hydra.Lexical.lookupPrimitive(
              graph,
              (name).value)));
          }
        });
      }

      @Override
      public Integer visit(hydra.core.Term.TypeLambda ta) {
        return hydra.Reduction.etaExpansionArity(
          graph,
          (ta).value.body);
      }

      @Override
      public Integer visit(hydra.core.Term.TypeApplication tt) {
        return hydra.Reduction.etaExpansionArity(
          graph,
          (tt).value.body);
      }

      @Override
      public Integer visit(hydra.core.Term.Variable name) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> 0,
          (java.util.function.Function<hydra.core.TypeScheme, Integer>) (ts -> hydra.Arity.typeArity((ts).type)),
          hydra.lib.maybes.Bind.apply(
            hydra.Lexical.lookupElement(
              graph,
              (name).value),
            (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.TypeScheme>>) (b -> (b).type)));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> etaExpandTypedTerm(hydra.context.Context cx, hydra.graph.Graph tx0, hydra.core.Term term0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>>>>>> rewrite = new java.util.concurrent.atomic.AtomicReference<>();
    rewrite.set((java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>>>>>) (topLevel -> (java.util.function.Function<Boolean, java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>>>>) (forced -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>>>) (typeArgs -> (java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>>) (recurse -> (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>) (tx -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (term -> {
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer>>>> arityOf = new java.util.concurrent.atomic.AtomicReference<>();
      arityOf.set((java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer>>>) (tx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer>>) (term2 -> {
        hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer>> dflt = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, Integer>) (_tc -> hydra.Arity.typeArity(hydra.lib.pairs.First.apply(_tc))),
          hydra.Checking.typeOf(
            cx,
            tx2,
            (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
            term2)));
        java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Function, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer>>> forFunction = (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Function, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer>>>) (tx3 -> (java.util.function.Function<hydra.core.Function, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer>>) (f -> (f).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer> visit(hydra.core.Function.Elimination ignored) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, Integer>right(1);
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer> visit(hydra.core.Function.Lambda l) {
            hydra.graph.Graph txl = hydra.Rewriting.extendGraphForLambda(
              tx3,
              (l).value);
            return arityOf.get().apply(txl).apply((l).value.body);
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer> visit(hydra.core.Function.Primitive name) {
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.TypeScheme, Integer>) (_ts -> hydra.Arity.typeSchemeArity(_ts)),
              hydra.Lexical.requirePrimitiveType(
                cx,
                tx3,
                (name).value));
          }
        })));
        return (term2).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer> otherwise(hydra.core.Term instance) {
            return dflt.get();
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer> visit(hydra.core.Term.Annotated at) {
            return arityOf.get().apply(tx2).apply((at).value.body);
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer> visit(hydra.core.Term.Function f) {
            return (forFunction).apply(tx2).apply((f).value);
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer> visit(hydra.core.Term.Let l) {
            hydra.util.Lazy<hydra.graph.Graph> txl = new hydra.util.Lazy<>(() -> hydra.Rewriting.extendGraphForLet(
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
              tx2,
              (l).value));
            return arityOf.get().apply(txl.get()).apply((l).value.body);
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer> visit(hydra.core.Term.TypeApplication tat) {
            return arityOf.get().apply(tx2).apply((tat).value.body);
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer> visit(hydra.core.Term.TypeLambda tl) {
            hydra.graph.Graph txt = hydra.Rewriting.extendGraphForTypeLambda(
              tx2,
              (tl).value);
            return arityOf.get().apply(txt).apply((tl).value.body);
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer> visit(hydra.core.Term.Variable name) {
            return hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.lib.eithers.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, Integer>) (_tc -> hydra.Arity.typeArity(hydra.lib.pairs.First.apply(_tc))),
                hydra.Checking.typeOf(
                  cx,
                  tx2,
                  (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
                  new hydra.core.Term.Variable((name).value))),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Integer>>) (t -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, Integer>right(hydra.Arity.typeArity(t))),
              hydra.lib.maybes.Map.apply(
                hydra.Rewriting::typeSchemeToFType,
                hydra.lib.maps.Lookup.apply(
                  (name).value,
                  (tx2).boundTypes)));
          }
        });
      })));
      java.util.function.Function<Integer, hydra.util.ConsList<hydra.core.Name>> extraVariables = (java.util.function.Function<Integer, hydra.util.ConsList<hydra.core.Name>>) (n -> hydra.lib.lists.Map.apply(
        (java.util.function.Function<Integer, hydra.core.Name>) (i -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
          "v",
          hydra.lib.literals.ShowInt32.apply(i)))),
        hydra.lib.math.Range.apply(
          1,
          n)));
      java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Field>> forCase = (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Field>>) (f -> hydra.lib.eithers.Bind.apply(
        rewrite.get().apply(false).apply(true).apply((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())).apply(recurse).apply(tx).apply((f).term),
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Field>>) (r -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Field>right(new hydra.core.Field((f).name, r)))));
      java.util.function.Function<hydra.core.CaseStatement, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>> forCaseStatement = (java.util.function.Function<hydra.core.CaseStatement, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (cs -> {
        hydra.util.ConsList<hydra.core.Field> cases = (cs).cases;
        hydra.util.Maybe<hydra.core.Term> dflt = (cs).default_;
        hydra.core.Name tname = (cs).typeName;
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapMaybe.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (v1 -> rewrite.get().apply(false).apply(false).apply((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())).apply(recurse).apply(tx).apply(v1)),
            dflt),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (rdflt -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              forCase,
              cases),
            (java.util.function.Function<hydra.util.ConsList<hydra.core.Field>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (rcases -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(tname, rdflt, rcases)))))))));
      });
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> pad = new java.util.concurrent.atomic.AtomicReference<>();
      pad.set((java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (vars -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (body -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(vars),
        () -> body,
        () -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(hydra.lib.lists.Head.apply(vars), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), pad.get().apply(hydra.lib.lists.Tail.apply(vars)).apply(new hydra.core.Term.Application(new hydra.core.Application(body, new hydra.core.Term.Variable(hydra.lib.lists.Head.apply(vars))))))))))));
      java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>> padn = (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (n -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (body -> pad.get().apply((extraVariables).apply(n)).apply(body)));
      java.util.function.Function<hydra.core.Term, hydra.core.Term> unwind = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Type, hydra.core.Term>>) (e -> (java.util.function.Function<hydra.core.Type, hydra.core.Term>) (t -> new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(e, t)))),
        term2,
        typeArgs));
      java.util.function.Function<hydra.core.Elimination, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>> forElimination = (java.util.function.Function<hydra.core.Elimination, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elm -> {
        java.util.function.Function<hydra.core.Elimination, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>> checkBase = (java.util.function.Function<hydra.core.Elimination, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elm2 -> (elm2).accept(new hydra.core.Elimination.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Elimination instance) {
            return (recurse).apply(tx).apply(term);
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Elimination.Union cs) {
            return (forCaseStatement).apply((cs).value);
          }
        }));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Map.apply(
            unwind,
            (checkBase).apply(elm)),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (base -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.logic.IfElse.lazy(
            hydra.lib.logic.Or.apply(
              topLevel,
              forced),
            () -> (padn).apply(1).apply(base),
            () -> base))));
      });
      java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>> forceExpansion = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (t -> hydra.lib.eithers.Bind.apply(
        hydra.Checking.typeOf(
          cx,
          tx,
          (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
          t),
        (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (typCx -> {
          hydra.util.Lazy<Integer> arity = new hydra.util.Lazy<>(() -> hydra.Arity.typeArity(hydra.lib.pairs.First.apply(typCx)));
          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right((padn).apply(arity.get()).apply((unwind).apply(t)));
        })));
      java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>> recurseOrForce = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (term2 -> hydra.lib.logic.IfElse.lazy(
        forced,
        () -> (forceExpansion).apply(term2),
        () -> (recurse).apply(tx).apply((unwind).apply(term2))));
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>> rewriteSpine = new java.util.concurrent.atomic.AtomicReference<>();
      rewriteSpine.set((java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (term2 -> (term2).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return rewrite.get().apply(false).apply(false).apply((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())).apply(recurse).apply(tx).apply(term2);
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Annotated at) {
          return hydra.lib.eithers.Bind.apply(
            rewriteSpine.get().apply((at).value.body),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (body -> {
              hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> ann = (at).value.annotation;
              return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(body, ann)));
            }));
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Application a) {
          hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> l = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
            false,
            () -> hydra.util.ConsList.of(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            () -> (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())));
          return hydra.lib.eithers.Bind.apply(
            rewriteSpine.get().apply((a).value.function),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (lhs -> hydra.lib.eithers.Bind.apply(
              rewrite.get().apply(true).apply(false).apply(l.get()).apply(recurse).apply(tx).apply((a).value.argument),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (rhs -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(lhs, rhs)))))));
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.TypeApplication tat) {
          return hydra.lib.eithers.Bind.apply(
            rewriteSpine.get().apply((tat).value.body),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (body -> {
              hydra.core.Type typ = (tat).value.type;
              return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(body, typ)));
            }));
        }
      })));
      return (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return (recurseOrForce).apply(term);
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Application a) {
          hydra.core.Term lhs = (a).value.function;
          hydra.core.Term rhs = (a).value.argument;
          return hydra.lib.eithers.Bind.apply(
            rewrite.get().apply(true).apply(false).apply((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())).apply(recurse).apply(tx).apply(rhs),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (rhs2 -> hydra.lib.eithers.Bind.apply(
              arityOf.get().apply(tx).apply(lhs),
              (java.util.function.Function<Integer, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (lhsarity -> hydra.lib.eithers.Bind.apply(
                rewriteSpine.get().apply(lhs),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (lhs2 -> {
                  hydra.core.Term a2 = new hydra.core.Term.Application(new hydra.core.Application(lhs2, rhs2));
                  return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.logic.IfElse.lazy(
                    hydra.lib.equality.Gt.apply(
                      lhsarity,
                      1),
                    () -> (padn).apply(hydra.lib.math.Sub.apply(
                      lhsarity,
                      1)).apply(a2),
                    () -> a2));
                }))))));
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Function f) {
          return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Function instance) {
              return (recurseOrForce).apply(term);
            }

            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Function.Elimination elm) {
              return (forElimination).apply((elm).value);
            }

            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Function.Lambda l) {
              hydra.graph.Graph txl = hydra.Rewriting.extendGraphForLambda(
                tx,
                (l).value);
              return hydra.lib.eithers.Map.apply(
                unwind,
                (recurse).apply(txl).apply(term));
            }
          });
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Let l) {
          hydra.util.Lazy<hydra.graph.Graph> txlt = new hydra.util.Lazy<>(() -> hydra.Rewriting.extendGraphForLet(
            (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
            tx,
            (l).value));
          return (recurse).apply(txlt.get()).apply(term);
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.TypeApplication tat) {
          return rewrite.get().apply(topLevel).apply(forced).apply(hydra.lib.lists.Cons.apply(
            (tat).value.type,
            typeArgs)).apply(recurse).apply(tx).apply((tat).value.body);
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.TypeLambda tl) {
          hydra.graph.Graph txt = hydra.Rewriting.extendGraphForTypeLambda(
            tx,
            (tl).value);
          return (recurse).apply(txt).apply(term);
        }
      });
    })))))));
    return hydra.Rewriting.rewriteTermWithContextM(
      (java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (v3 -> rewrite.get().apply(true).apply(false).apply((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty())).apply(v1).apply(v2).apply(v3)))),
      tx0,
      term0);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> reduceTerm(hydra.context.Context cx, hydra.graph.Graph graph, Boolean eager, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Elimination, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>> applyElimination = (java.util.function.Function<hydra.core.Elimination, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>) (elm -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (reducedArg -> (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Elimination.Record proj) {
        return hydra.lib.eithers.Bind.apply(
          hydra.extract.Core.record(
            cx,
            (proj).value.typeName,
            graph,
            hydra.Rewriting.deannotateTerm(reducedArg)),
          (java.util.function.Function<hydra.util.ConsList<hydra.core.Field>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (fields -> {
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Field>> matchingFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
              (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                (f).name,
                (proj).value.field)),
              fields));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(matchingFields.get()),
              () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
                "no such field: ",
                (proj).value.field.value,
                " in ",
                (proj).value.typeName.value,
                " record")))), cx))),
              () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.lists.Head.apply(matchingFields.get()).term));
          }));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Elimination.Union cs) {
        return hydra.lib.eithers.Bind.apply(
          hydra.extract.Core.injection(
            cx,
            (cs).value.typeName,
            graph,
            reducedArg),
          (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (field -> {
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Field>> matchingFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
              (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                (f).name,
                (field).name)),
              (cs).value.cases));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(matchingFields.get()),
              () -> hydra.lib.maybes.Maybe.applyLazy(
                () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
                  "no such field ",
                  (field).name.value,
                  " in ",
                  (cs).value.typeName.value,
                  " case statement")))), cx))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (x -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(x)),
                (cs).value.default_),
              () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(hydra.lib.lists.Head.apply(matchingFields.get()).term, (field).term))));
          }));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Elimination.Wrap name) {
        return hydra.extract.Core.wrap(
          cx,
          (name).value,
          graph,
          reducedArg);
      }
    })));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>>> applyToArguments = new java.util.concurrent.atomic.AtomicReference<>();
    applyToArguments.set((java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>>) (fun -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>) (args -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(args),
      () -> fun,
      () -> applyToArguments.get().apply(new hydra.core.Term.Application(new hydra.core.Application(fun, hydra.lib.lists.Head.apply(args)))).apply(hydra.lib.lists.Tail.apply(args))))));
    java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.context.InContext<hydra.errors.Error_>> mapErrorToString = (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.context.InContext<hydra.errors.Error_>>) (ic -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.show.Errors.error(((java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.errors.Error_>) (projected -> projected.object)).apply(ic)))), ((java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.context.Context>) (projected -> projected.context)).apply(ic))));
    java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>> reduce = (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>) (eager2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (v1 -> hydra.Reduction.reduceTerm(
      cx,
      graph,
      eager2,
      v1)));
    java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>> reduceArg = (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>) (eager2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (arg -> hydra.lib.logic.IfElse.lazy(
      eager2,
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(arg),
      () -> (reduce).apply(false).apply(arg))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>>> applyIfNullary = new java.util.concurrent.atomic.AtomicReference<>();
    applyIfNullary.set((java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>>) (eager2 -> (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>) (original -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (args -> {
      java.util.function.Function<hydra.core.Elimination, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>> forElimination = (java.util.function.Function<hydra.core.Elimination, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>) (elm -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (args2 -> {
        hydra.util.Lazy<hydra.core.Term> arg = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(args2));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> remainingArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(args2));
        return hydra.lib.eithers.Bind.apply(
          (reduceArg).apply(eager2).apply(hydra.Rewriting.deannotateTerm(arg.get())),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (reducedArg -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.Bind.apply(
              (applyElimination).apply(elm).apply(reducedArg),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (v1 -> (reduce).apply(eager2).apply(v1))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (reducedResult -> applyIfNullary.get().apply(eager2).apply(reducedResult).apply(remainingArgs.get())))));
      }));
      java.util.function.Function<hydra.core.Lambda, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>> forLambda = (java.util.function.Function<hydra.core.Lambda, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>) (l -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (args2 -> {
        hydra.util.Lazy<hydra.core.Term> arg = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(args2));
        hydra.core.Term body = (l).body;
        hydra.core.Name param = (l).parameter;
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> remainingArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(args2));
        return hydra.lib.eithers.Bind.apply(
          (reduce).apply(eager2).apply(hydra.Rewriting.deannotateTerm(arg.get())),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (reducedArg -> hydra.lib.eithers.Bind.apply(
            (reduce).apply(eager2).apply(hydra.Rewriting.replaceFreeTermVariable(
              param,
              reducedArg,
              body)),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (reducedResult -> applyIfNullary.get().apply(eager2).apply(reducedResult).apply(remainingArgs.get())))));
      }));
      java.util.function.Function<hydra.graph.Primitive, java.util.function.Function<Integer, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>> forPrimitive = (java.util.function.Function<hydra.graph.Primitive, java.util.function.Function<Integer, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>>) (prim -> (java.util.function.Function<Integer, java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>) (arity -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (args2 -> {
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> argList = new hydra.util.Lazy<>(() -> hydra.lib.lists.Take.apply(
          arity,
          args2));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> remainingArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Drop.apply(
          arity,
          args2));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (v1 -> (reduceArg).apply(eager2).apply(v1)),
            argList.get()),
          (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (reducedArgs -> {
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> strippedArgs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              hydra.Rewriting::deannotateTerm,
              reducedArgs));
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.Bimap.apply(
                mapErrorToString,
                (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> x),
                (prim).implementation.apply(cx).apply(graph).apply(strippedArgs.get())),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (primResult -> hydra.lib.eithers.Bind.apply(
                (reduce).apply(eager2).apply(primResult),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (reducedResult -> applyIfNullary.get().apply(eager2).apply(reducedResult).apply(remainingArgs.get())))));
          }));
      })));
      hydra.core.Term stripped = hydra.Rewriting.deannotateTerm(original);
      return (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(applyToArguments.get().apply(original).apply(args));
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Application app) {
          return applyIfNullary.get().apply(eager2).apply((app).value.function).apply(hydra.lib.lists.Cons.apply(
            (app).value.argument,
            args));
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Function v1) {
          return (v1).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Function.Elimination elm) {
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.lists.Null.apply(args),
                () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(original),
                () -> (forElimination).apply((elm).value).apply(args));
            }

            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Function.Lambda l) {
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.lists.Null.apply(args),
                () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(original),
                () -> (forLambda).apply((l).value).apply(args));
            }

            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Function.Primitive name) {
              return hydra.lib.eithers.Bind.apply(
                hydra.Lexical.requirePrimitive(
                  cx,
                  graph,
                  (name).value),
                (java.util.function.Function<hydra.graph.Primitive, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (prim -> {
                  Integer arity = hydra.Arity.primitiveArity(prim);
                  return hydra.lib.logic.IfElse.lazy(
                    hydra.lib.equality.Gt.apply(
                      arity,
                      hydra.lib.lists.Length.apply(args)),
                    () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(applyToArguments.get().apply(original).apply(args)),
                    () -> (forPrimitive).apply(prim).apply(arity).apply(args));
                }));
            }
          });
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Variable v) {
          hydra.util.Maybe<hydra.core.Binding> mBinding = hydra.Lexical.dereferenceElement(
            graph,
            (v).value);
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(applyToArguments.get().apply(original).apply(args)),
            (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (binding -> applyIfNullary.get().apply(eager2).apply((binding).term).apply(args)),
            mBinding);
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Let lt) {
          hydra.util.ConsList<hydra.core.Binding> bindings = (lt).value.bindings;
          hydra.core.Term body = (lt).value.body;
          java.util.function.Function<hydra.core.Binding, hydra.core.Term> letExpr = (java.util.function.Function<hydra.core.Binding, hydra.core.Term>) (b -> new hydra.core.Term.Let(new hydra.core.Let(hydra.util.ConsList.of(b), new hydra.core.Term.Variable((b).name))));
          java.util.function.Function<hydra.core.Binding, hydra.core.Binding> expandBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, hydra.Rewriting.replaceFreeTermVariable(
            (b).name,
            (letExpr).apply(b),
            (b).term), (b).type));
          hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> expandedBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            expandBinding,
            bindings));
          java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Binding, hydra.core.Term>> substituteBinding = (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Binding, hydra.core.Term>>) (term2 -> (java.util.function.Function<hydra.core.Binding, hydra.core.Term>) (b -> hydra.Rewriting.replaceFreeTermVariable(
            (b).name,
            (b).term,
            term2)));
          java.util.function.Function<hydra.util.ConsList<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> substituteAll = (java.util.function.Function<hydra.util.ConsList<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (bs -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> hydra.lib.lists.Foldl.apply(
            substituteBinding,
            term2,
            bs)));
          hydra.core.Term expandedBody = (substituteAll).apply(expandedBindings.get()).apply(body);
          return hydra.lib.eithers.Bind.apply(
            (reduce).apply(eager2).apply(expandedBody),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (reducedBody -> applyIfNullary.get().apply(eager2).apply(reducedBody).apply(args)));
        }
      });
    }))));
    java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>> doRecurse = (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (eager2 -> (java.util.function.Function<hydra.core.Term, Boolean>) (term2 -> {
      java.util.function.Function<hydra.core.Function, Boolean> isNonLambda = (java.util.function.Function<hydra.core.Function, Boolean>) (f -> (f).accept(new hydra.core.Function.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Function instance) {
          return true;
        }

        @Override
        public Boolean visit(hydra.core.Function.Lambda ignored) {
          return false;
        }
      }));
      Boolean isNonLambdaTerm = (term2).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Term instance) {
          return true;
        }

        @Override
        public Boolean visit(hydra.core.Term.Function f) {
          return (isNonLambda).apply((f).value);
        }

        @Override
        public Boolean visit(hydra.core.Term.Let ignored) {
          return false;
        }
      });
      return hydra.lib.logic.And.apply(
        eager2,
        isNonLambdaTerm);
    }));
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>> mapping = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (mid -> hydra.lib.eithers.Bind.apply(
      hydra.lib.logic.IfElse.lazy(
        (doRecurse).apply(eager).apply(mid),
        () -> (recurse).apply(mid),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(mid)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (inner -> applyIfNullary.get().apply(eager).apply(inner).apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty()))))));
    return hydra.Rewriting.rewriteTermM(
      mapping,
      term);
  }

  static Boolean termIsClosed(hydra.core.Term term) {
    return hydra.lib.sets.Null.apply(hydra.Rewriting.freeVariablesInTerm(term));
  }

  static Boolean termIsValue(hydra.core.Term term) {
    java.util.function.Function<hydra.core.Field, Boolean> checkField = (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.Reduction.termIsValue((f).term));
    java.util.function.Function<hydra.util.ConsList<hydra.core.Field>, Boolean> checkFields = (java.util.function.Function<hydra.util.ConsList<hydra.core.Field>, Boolean>) (fields -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Field, Boolean>>) (b -> (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.logic.And.apply(
        b,
        (checkField).apply(f)))),
      true,
      fields));
    java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, Boolean> forList = (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, Boolean>) (els -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (b -> (java.util.function.Function<hydra.core.Term, Boolean>) (t -> hydra.lib.logic.And.apply(
        b,
        hydra.Reduction.termIsValue(t)))),
      true,
      els));
    java.util.function.Function<hydra.core.Function, Boolean> functionIsValue = (java.util.function.Function<hydra.core.Function, Boolean>) (f -> (f).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public Boolean visit(hydra.core.Function.Elimination e) {
        return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
          @Override
          public Boolean visit(hydra.core.Elimination.Wrap ignored) {
            return true;
          }

          @Override
          public Boolean visit(hydra.core.Elimination.Record ignored) {
            return true;
          }

          @Override
          public Boolean visit(hydra.core.Elimination.Union cs) {
            return hydra.lib.logic.And.apply(
              (checkFields).apply((cs).value.cases),
              hydra.lib.maybes.Maybe.applyLazy(
                () -> true,
                hydra.Reduction::termIsValue,
                (cs).value.default_));
          }
        });
      }

      @Override
      public Boolean visit(hydra.core.Function.Lambda l) {
        return hydra.Reduction.termIsValue((l).value.body);
      }

      @Override
      public Boolean visit(hydra.core.Function.Primitive ignored) {
        return true;
      }
    }));
    return hydra.Rewriting.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Term.Application ignored) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, Boolean>) (l -> hydra.Reduction.termIsValue(l)),
          (java.util.function.Function<hydra.core.Term, Boolean>) (r -> hydra.Reduction.termIsValue(r)),
          (e).value);
      }

      @Override
      public Boolean visit(hydra.core.Term.Literal ignored) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Term.Function f) {
        return (functionIsValue).apply((f).value);
      }

      @Override
      public Boolean visit(hydra.core.Term.List els) {
        return (forList).apply((els).value);
      }

      @Override
      public Boolean visit(hydra.core.Term.Map m) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Boolean, java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, Boolean>>) (b -> (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, Boolean>) (kv -> hydra.lib.logic.And.apply(
            b,
            hydra.lib.logic.And.apply(
              hydra.Reduction.termIsValue(hydra.lib.pairs.First.apply(kv)),
              hydra.Reduction.termIsValue(hydra.lib.pairs.Second.apply(kv)))))),
          true,
          hydra.lib.maps.ToList.apply((m).value));
      }

      @Override
      public Boolean visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> true,
          hydra.Reduction::termIsValue,
          (m).value);
      }

      @Override
      public Boolean visit(hydra.core.Term.Record r) {
        return (checkFields).apply((r).value.fields);
      }

      @Override
      public Boolean visit(hydra.core.Term.Set s) {
        return (forList).apply(hydra.lib.sets.ToList.apply((s).value));
      }

      @Override
      public Boolean visit(hydra.core.Term.Union i) {
        return (checkField).apply((i).value.field);
      }

      @Override
      public Boolean visit(hydra.core.Term.Unit ignored) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Term.Variable ignored) {
        return false;
      }
    });
  }
}
