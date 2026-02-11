// Note: this is an automatically generated file. Do not edit.

package hydra.coderUtils;

/**
 * Common utilities for language coders, providing shared patterns for term decomposition and analysis.
 */
public interface CoderUtils {
  static String normalizeComment(String s) {
    String stripped = hydra.formatting.Formatting.stripLeadingAndTrailingWhitespace(s);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.strings.Null.apply(stripped),
      () -> "",
      () -> ((java.util.function.Supplier<String>) (() -> {
        Integer lastIdx = hydra.lib.math.Sub.apply(
          hydra.lib.strings.Length.apply(stripped),
          1);
        return ((java.util.function.Supplier<String>) (() -> {
          Integer lastChar = hydra.lib.strings.CharAt.apply(
            lastIdx,
            stripped);
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              lastChar,
              46),
            () -> stripped,
            () -> hydra.lib.strings.Cat2.apply(
              stripped,
              "."));
        })).get();
      })).get());
  }
  
  static hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term> gatherApplications(hydra.core.Term term) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term>>>) (args -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term>>) (t -> (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term>(args, t)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.core.Term> visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = ((app).value).function;
        hydra.core.Term rhs = ((app).value).argument;
        return ((go.get()).apply(hydra.lib.lists.Cons.apply(
          rhs,
          args))).apply(lhs);
      }
    }))));
    return ((go.get()).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))).apply(term);
  }
  
  static hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Term>> gatherArgs(hydra.core.Term term, java.util.List<hydra.core.Term> args) {
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Term>> otherwise(hydra.core.Term instance) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Term>>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Term>>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Term>>(term, args)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Term>> visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = ((app).value).function;
        hydra.core.Term rhs = ((app).value).argument;
        return hydra.coderUtils.CoderUtils.gatherArgs(
          lhs,
          hydra.lib.lists.Cons.apply(
            rhs,
            args));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Term>> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term body = ((tl).value).body;
        return hydra.coderUtils.CoderUtils.gatherArgs(
          body,
          args);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Term, java.util.List<hydra.core.Term>> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Term body = ((ta).value).body;
        return hydra.coderUtils.CoderUtils.gatherArgs(
          body,
          args);
      }
    });
  }
  
  static hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Type>>> gatherArgsWithTypeApps(hydra.core.Term term, java.util.List<hydra.core.Term> args, java.util.List<hydra.core.Type> tyArgs) {
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Type>>> otherwise(hydra.core.Term instance) {
        return (hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Type>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Type>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Type>>>(term, (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Type>>(args, tyArgs))))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Type>>> visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = ((app).value).function;
        hydra.core.Term rhs = ((app).value).argument;
        return hydra.coderUtils.CoderUtils.gatherArgsWithTypeApps(
          lhs,
          hydra.lib.lists.Cons.apply(
            rhs,
            args),
          tyArgs);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Type>>> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term body = ((tl).value).body;
        return hydra.coderUtils.CoderUtils.gatherArgsWithTypeApps(
          body,
          args,
          tyArgs);
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Type>>> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Term body = ((ta).value).body;
        hydra.core.Type typ = ((ta).value).type;
        return hydra.coderUtils.CoderUtils.gatherArgsWithTypeApps(
          body,
          args,
          hydra.lib.lists.Cons.apply(
            typ,
            tyArgs));
      }
    });
  }
  
  static Boolean isSimpleAssignment(hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        hydra.util.Lazy<hydra.core.Term> baseTerm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.coderUtils.CoderUtils.gatherArgs(
          term,
          (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))));
        return (baseTerm.get()).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Term instance) {
            return true;
          }
          
          @Override
          public Boolean visit(hydra.core.Term.Function f) {
            return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public Boolean otherwise(hydra.core.Function instance) {
                return true;
              }
              
              @Override
              public Boolean visit(hydra.core.Function.Elimination elim) {
                return ((elim).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
                  @Override
                  public Boolean otherwise(hydra.core.Elimination instance) {
                    return true;
                  }
                  
                  @Override
                  public Boolean visit(hydra.core.Elimination.Union ignored) {
                    return false;
                  }
                });
              }
            });
          }
        });
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Annotated at) {
        return hydra.coderUtils.CoderUtils.isSimpleAssignment(((at).value).body);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Function instance) {
            return true;
          }
          
          @Override
          public Boolean visit(hydra.core.Function.Lambda ignored) {
            return false;
          }
        });
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Let ignored) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.TypeLambda ignored) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.TypeApplication ta) {
        return hydra.coderUtils.CoderUtils.isSimpleAssignment(((ta).value).body);
      }
    });
  }
  
  static Boolean isComplexTerm(hydra.typing.TypeContext tc, hydra.core.Term t) {
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (b -> (java.util.function.Function<hydra.core.Term, Boolean>) (sub -> hydra.lib.logic.Or.apply(
            b,
            hydra.coderUtils.CoderUtils.isComplexTerm(
              tc,
              sub)))),
          false,
          hydra.rewriting.Rewriting.subterms(t));
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Let ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.TypeApplication ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.TypeLambda ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Variable name) {
        return hydra.coderUtils.CoderUtils.isComplexVariable(
          tc,
          (name).value);
      }
    });
  }
  
  static Boolean isComplexVariable(hydra.typing.TypeContext tc, hydra.core.Name name) {
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.Term>> metaLookup = new hydra.util.Lazy<>(() -> hydra.lib.maps.Lookup.apply(
      name,
      (tc).metadata));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maybes.IsJust.apply(metaLookup.get()),
      () -> true,
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.sets.Member.apply(
          name,
          (tc).lambdaVariables),
        () -> true,
        () -> ((java.util.function.Supplier<Boolean>) (() -> {
          hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> typeLookup = new hydra.util.Lazy<>(() -> hydra.lib.maps.Lookup.apply(
            name,
            (tc).types));
          return hydra.lib.logic.Not.apply(hydra.lib.maybes.IsJust.apply(typeLookup.get()));
        })).get()));
  }
  
  static Boolean isComplexBinding(hydra.typing.TypeContext tc, hydra.core.Binding b) {
    hydra.util.Maybe<hydra.core.TypeScheme> mts = (b).type;
    hydra.core.Term term = (b).term;
    return hydra.lib.maybes.Cases.apply(
      mts,
      hydra.coderUtils.CoderUtils.isComplexTerm(
        tc,
        term),
      (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> {
        Boolean isComplex = hydra.coderUtils.CoderUtils.isComplexTerm(
          tc,
          term);
        hydra.util.Lazy<Boolean> isNonNullary = new hydra.util.Lazy<>(() -> hydra.lib.equality.Gt.apply(
          hydra.arity.Arity.typeArity((ts).type),
          0));
        hydra.util.Lazy<Boolean> isPolymorphic = new hydra.util.Lazy<>(() -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply((ts).variables)));
        return hydra.lib.logic.Or.apply(
          hydra.lib.logic.Or.apply(
            isPolymorphic.get(),
            isNonNullary.get()),
          isComplex);
      }));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>> commentsFromElement(hydra.core.Binding b) {
    return hydra.annotations.Annotations.getTermDescription((b).term);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>> commentsFromFieldType(hydra.core.FieldType ft) {
    return hydra.annotations.Annotations.getTypeDescription((ft).type);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> tryTypeOf(String msg, hydra.typing.TypeContext tc, hydra.core.Term term) {
    return hydra.monads.Monads.withTrace(
      msg,
      hydra.checking.Checking.<T0>typeOf(
        tc,
        (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
        term));
  }
  
  static hydra.util.Maybe<hydra.core.Term> bindingMetadata(hydra.typing.TypeContext tc, hydra.core.Binding b) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.coderUtils.CoderUtils.isComplexBinding(
        tc,
        b),
      () -> hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))),
      () -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTermWith_finish(java.util.function.Function<T0, hydra.typing.TypeContext> getTC, T0 fEnv, java.util.List<hydra.core.Name> tparams, java.util.List<hydra.core.Name> args, java.util.List<hydra.core.Binding> bindings, java.util.List<hydra.core.Type> doms, java.util.List<hydra.core.Type> tapps, hydra.core.Term body) {
    hydra.util.Lazy<hydra.core.Term> bodyWithTapps = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Type, hydra.core.Term>>) (trm -> (java.util.function.Function<hydra.core.Type, hydra.core.Term>) (typ -> new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(trm, typ)))),
      body,
      tapps));
    return hydra.lib.flows.Bind.apply(
      hydra.coderUtils.CoderUtils.<T1>tryTypeOf(
        "analyzeFunctionTermWith",
        (getTC).apply(fEnv),
        bodyWithTapps.get()),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>>>) (typ -> hydra.lib.flows.Pure.apply((hydra.typing.FunctionStructure<T0>) (new hydra.typing.FunctionStructure<T0>(hydra.lib.lists.Reverse.apply(tparams), hydra.lib.lists.Reverse.apply(args), bindings, bodyWithTapps.get(), hydra.lib.lists.Reverse.apply(doms), hydra.util.Maybe.just(typ), fEnv)))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTermWith_gather(java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, java.util.function.Function<T0, hydra.typing.TypeContext> getTC, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T0, T0>> setTC, Boolean argMode, T0 gEnv, java.util.List<hydra.core.Name> tparams, java.util.List<hydra.core.Name> args, java.util.List<hydra.core.Binding> bindings, java.util.List<hydra.core.Type> doms, java.util.List<hydra.core.Type> tapps, hydra.core.Term t) {
    return (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> otherwise(hydra.core.Term instance) {
        return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_finish(
          getTC,
          gEnv,
          tparams,
          args,
          bindings,
          doms,
          tapps,
          t);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> otherwise(hydra.core.Function instance) {
            return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_finish(
              getTC,
              gEnv,
              tparams,
              args,
              bindings,
              doms,
              tapps,
              t);
          }
          
          @Override
          public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Function.Lambda lam) {
            return hydra.lib.logic.IfElse.lazy(
              argMode,
              () -> ((java.util.function.Supplier<hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>>>) (() -> {
                hydra.core.Name v = ((lam).value).parameter;
                return ((java.util.function.Supplier<hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>>>) (() -> {
                  hydra.util.Lazy<hydra.core.Type> dom = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
                    new hydra.core.Type.Variable(new hydra.core.Name("_")),
                    (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x_ -> x_),
                    ((lam).value).domain));
                  return ((java.util.function.Supplier<hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>>>) (() -> {
                    hydra.core.Term body = ((lam).value).body;
                    return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_gather(
                      forBinding,
                      getTC,
                      setTC,
                      argMode,
                      hydra.coderUtils.CoderUtils.analyzeFunctionTermWith_gather_newEnv(
                        gEnv,
                        getTC,
                        (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForLambda(
                          p0,
                          p1)),
                        (lam).value,
                        setTC),
                      tparams,
                      hydra.lib.lists.Cons.apply(
                        v,
                        args),
                      bindings,
                      hydra.lib.lists.Cons.apply(
                        dom.get(),
                        doms),
                      tapps,
                      body);
                  })).get();
                })).get();
              })).get(),
              () -> hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_finish(
                getTC,
                gEnv,
                tparams,
                args,
                bindings,
                doms,
                tapps,
                t));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.Let lt) {
        hydra.core.Term body = ((lt).value).body;
        java.util.List<hydra.core.Binding> newBindings = ((lt).value).bindings;
        return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_gather(
          forBinding,
          getTC,
          setTC,
          false,
          hydra.coderUtils.CoderUtils.analyzeFunctionTermWith_gather_newEnv2(
            forBinding,
            gEnv,
            getTC,
            (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>>) (p0 -> p1 -> p2 -> hydra.schemas.Schemas.extendTypeContextForLet(
              p0,
              p1,
              p2)),
            (lt).value,
            setTC),
          tparams,
          args,
          hydra.lib.lists.Concat2.apply(
            bindings,
            newBindings),
          doms,
          tapps,
          body);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Term taBody = ((ta).value).body;
        hydra.core.Type typ = ((ta).value).type;
        return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_gather(
          forBinding,
          getTC,
          setTC,
          argMode,
          gEnv,
          tparams,
          args,
          bindings,
          doms,
          hydra.lib.lists.Cons.apply(
            typ,
            tapps),
          taBody);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term tlBody = ((tl).value).body;
        hydra.core.Name tvar = ((tl).value).parameter;
        return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_gather(
          forBinding,
          getTC,
          setTC,
          argMode,
          hydra.coderUtils.CoderUtils.analyzeFunctionTermWith_gather_newEnv3(
            gEnv,
            getTC,
            (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForTypeLambda(
              p0,
              p1)),
            setTC,
            (tl).value),
          hydra.lib.lists.Cons.apply(
            tvar,
            tparams),
          args,
          bindings,
          doms,
          tapps,
          tlBody);
      }
    });
  }
  
  static <T0, T1, T2, T3, T4> T4 analyzeFunctionTermWith_gather_newEnv(T0 gEnv, java.util.function.Function<T0, T1> getTC, java.util.function.Function<T1, java.util.function.Function<T2, T3>> hydra_schemas_extendTypeContextForLambda2, T2 lam, java.util.function.Function<T3, java.util.function.Function<T0, T4>> setTC) {
    return ((setTC).apply(((hydra_schemas_extendTypeContextForLambda2).apply((getTC).apply(gEnv))).apply(lam))).apply(gEnv);
  }
  
  static <T0, T1, T2, T3, T4, T5> T5 analyzeFunctionTermWith_gather_newEnv2(T0 forBinding, T1 gEnv, java.util.function.Function<T1, T2> getTC, java.util.function.Function<T0, java.util.function.Function<T2, java.util.function.Function<T3, T4>>> hydra_schemas_extendTypeContextForLet2, T3 lt, java.util.function.Function<T4, java.util.function.Function<T1, T5>> setTC) {
    return ((setTC).apply((((hydra_schemas_extendTypeContextForLet2).apply(forBinding)).apply((getTC).apply(gEnv))).apply(lt))).apply(gEnv);
  }
  
  static <T0, T1, T2, T3, T4> T4 analyzeFunctionTermWith_gather_newEnv3(T0 gEnv, java.util.function.Function<T0, T1> getTC, java.util.function.Function<T1, java.util.function.Function<T2, T3>> hydra_schemas_extendTypeContextForTypeLambda2, java.util.function.Function<T3, java.util.function.Function<T0, T4>> setTC, T2 tl) {
    return ((setTC).apply(((hydra_schemas_extendTypeContextForTypeLambda2).apply((getTC).apply(gEnv))).apply(tl))).apply(gEnv);
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTermWith(java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, java.util.function.Function<T0, hydra.typing.TypeContext> getTC, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T0, T0>> setTC, T0 env, hydra.core.Term term) {
    return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_gather(
      forBinding,
      getTC,
      setTC,
      true,
      env,
      (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()),
      (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()),
      (java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of()),
      (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
      (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
      term);
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTermNoInferWith_finish(T0 fEnv, java.util.List<hydra.core.Name> tparams, java.util.List<hydra.core.Name> args, java.util.List<hydra.core.Binding> bindings, java.util.List<hydra.core.Type> doms, java.util.List<hydra.core.Type> tapps, hydra.core.Term body) {
    hydra.util.Lazy<hydra.core.Term> bodyWithTapps = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Type, hydra.core.Term>>) (trm -> (java.util.function.Function<hydra.core.Type, hydra.core.Term>) (typ -> new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(trm, typ)))),
      body,
      tapps));
    return hydra.lib.flows.Pure.apply((hydra.typing.FunctionStructure<T0>) (new hydra.typing.FunctionStructure<T0>(hydra.lib.lists.Reverse.apply(tparams), hydra.lib.lists.Reverse.apply(args), bindings, bodyWithTapps.get(), hydra.lib.lists.Reverse.apply(doms), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), fEnv)));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTermNoInferWith_gather(java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, java.util.function.Function<T0, hydra.typing.TypeContext> getTC, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T0, T0>> setTC, Boolean argMode, T0 gEnv, java.util.List<hydra.core.Name> tparams, java.util.List<hydra.core.Name> args, java.util.List<hydra.core.Binding> bindings, java.util.List<hydra.core.Type> doms, java.util.List<hydra.core.Type> tapps, hydra.core.Term t) {
    return (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> otherwise(hydra.core.Term instance) {
        return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermNoInferWith_finish(
          gEnv,
          tparams,
          args,
          bindings,
          doms,
          tapps,
          t);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> otherwise(hydra.core.Function instance) {
            return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermNoInferWith_finish(
              gEnv,
              tparams,
              args,
              bindings,
              doms,
              tapps,
              t);
          }
          
          @Override
          public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Function.Lambda lam) {
            return hydra.lib.logic.IfElse.lazy(
              argMode,
              () -> ((java.util.function.Supplier<hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>>>) (() -> {
                hydra.core.Name v = ((lam).value).parameter;
                return ((java.util.function.Supplier<hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>>>) (() -> {
                  hydra.util.Lazy<hydra.core.Type> dom = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
                    new hydra.core.Type.Variable(new hydra.core.Name("_")),
                    (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x_ -> x_),
                    ((lam).value).domain));
                  return ((java.util.function.Supplier<hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>>>) (() -> {
                    hydra.core.Term body = ((lam).value).body;
                    return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermNoInferWith_gather(
                      forBinding,
                      getTC,
                      setTC,
                      argMode,
                      hydra.coderUtils.CoderUtils.analyzeFunctionTermNoInferWith_gather_newEnv(
                        gEnv,
                        getTC,
                        (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForLambda(
                          p0,
                          p1)),
                        (lam).value,
                        setTC),
                      tparams,
                      hydra.lib.lists.Cons.apply(
                        v,
                        args),
                      bindings,
                      hydra.lib.lists.Cons.apply(
                        dom.get(),
                        doms),
                      tapps,
                      body);
                  })).get();
                })).get();
              })).get(),
              () -> hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermNoInferWith_finish(
                gEnv,
                tparams,
                args,
                bindings,
                doms,
                tapps,
                t));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.Let lt) {
        hydra.core.Term body = ((lt).value).body;
        java.util.List<hydra.core.Binding> newBindings = ((lt).value).bindings;
        return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermNoInferWith_gather(
          forBinding,
          getTC,
          setTC,
          false,
          hydra.coderUtils.CoderUtils.analyzeFunctionTermNoInferWith_gather_newEnv2(
            forBinding,
            gEnv,
            getTC,
            (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>>) (p0 -> p1 -> p2 -> hydra.schemas.Schemas.extendTypeContextForLet(
              p0,
              p1,
              p2)),
            (lt).value,
            setTC),
          tparams,
          args,
          hydra.lib.lists.Concat2.apply(
            bindings,
            newBindings),
          doms,
          tapps,
          body);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Term taBody = ((ta).value).body;
        hydra.core.Type typ = ((ta).value).type;
        return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermNoInferWith_gather(
          forBinding,
          getTC,
          setTC,
          argMode,
          gEnv,
          tparams,
          args,
          bindings,
          doms,
          hydra.lib.lists.Cons.apply(
            typ,
            tapps),
          taBody);
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term tlBody = ((tl).value).body;
        hydra.core.Name tvar = ((tl).value).parameter;
        return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermNoInferWith_gather(
          forBinding,
          getTC,
          setTC,
          argMode,
          hydra.coderUtils.CoderUtils.analyzeFunctionTermNoInferWith_gather_newEnv3(
            gEnv,
            getTC,
            (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForTypeLambda(
              p0,
              p1)),
            setTC,
            (tl).value),
          hydra.lib.lists.Cons.apply(
            tvar,
            tparams),
          args,
          bindings,
          doms,
          tapps,
          tlBody);
      }
    });
  }
  
  static <T0, T1, T2, T3, T4> T4 analyzeFunctionTermNoInferWith_gather_newEnv(T0 gEnv, java.util.function.Function<T0, T1> getTC, java.util.function.Function<T1, java.util.function.Function<T2, T3>> hydra_schemas_extendTypeContextForLambda2, T2 lam, java.util.function.Function<T3, java.util.function.Function<T0, T4>> setTC) {
    return ((setTC).apply(((hydra_schemas_extendTypeContextForLambda2).apply((getTC).apply(gEnv))).apply(lam))).apply(gEnv);
  }
  
  static <T0, T1, T2, T3, T4, T5> T5 analyzeFunctionTermNoInferWith_gather_newEnv2(T0 forBinding, T1 gEnv, java.util.function.Function<T1, T2> getTC, java.util.function.Function<T0, java.util.function.Function<T2, java.util.function.Function<T3, T4>>> hydra_schemas_extendTypeContextForLet2, T3 lt, java.util.function.Function<T4, java.util.function.Function<T1, T5>> setTC) {
    return ((setTC).apply((((hydra_schemas_extendTypeContextForLet2).apply(forBinding)).apply((getTC).apply(gEnv))).apply(lt))).apply(gEnv);
  }
  
  static <T0, T1, T2, T3, T4> T4 analyzeFunctionTermNoInferWith_gather_newEnv3(T0 gEnv, java.util.function.Function<T0, T1> getTC, java.util.function.Function<T1, java.util.function.Function<T2, T3>> hydra_schemas_extendTypeContextForTypeLambda2, java.util.function.Function<T3, java.util.function.Function<T0, T4>> setTC, T2 tl) {
    return ((setTC).apply(((hydra_schemas_extendTypeContextForTypeLambda2).apply((getTC).apply(gEnv))).apply(tl))).apply(gEnv);
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTermNoInferWith(java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, java.util.function.Function<T0, hydra.typing.TypeContext> getTC, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T0, T0>> setTC, T0 env, hydra.core.Term term) {
    return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermNoInferWith_gather(
      forBinding,
      getTC,
      setTC,
      true,
      env,
      (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()),
      (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()),
      (java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of()),
      (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
      (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
      term);
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTerm(java.util.function.Function<T0, hydra.typing.TypeContext> getTC, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T0, T0>> setTC, T0 env, hydra.core.Term term) {
    return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith(
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (p0 -> p1 -> hydra.coderUtils.CoderUtils.bindingMetadata(
        p0,
        p1)),
      getTC,
      setTC,
      env,
      term);
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTermInline(java.util.function.Function<T0, hydra.typing.TypeContext> getTC, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T0, T0>> setTC, T0 env, hydra.core.Term term) {
    return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith(
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
      getTC,
      setTC,
      env,
      term);
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTermNoInfer(java.util.function.Function<T0, hydra.typing.TypeContext> getTC, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T0, T0>> setTC, T0 env, hydra.core.Term term) {
    return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermNoInferWith(
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (p0 -> p1 -> hydra.coderUtils.CoderUtils.bindingMetadata(
        p0,
        p1)),
      getTC,
      setTC,
      env,
      term);
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T0, java.lang.Void> updateCoderMetadata(java.util.function.Function<T0, T1> getMeta, java.util.function.Function<T2, java.util.function.Function<T3, T0>> makeCoder, java.util.function.Function<T0, T2> getGraph, java.util.function.Function<T1, T3> f) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<T0>getState(),
      (java.util.function.Function<T0, hydra.compute.Flow<T0, java.lang.Void>>) (st -> hydra.monads.Monads.<T0>putState(((makeCoder).apply((getGraph).apply(st))).apply((f).apply((getMeta).apply(st))))));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T0, T3> withUpdatedCoderGraph(java.util.function.Function<T0, T1> getGraph, java.util.function.Function<T0, T2> getMeta, java.util.function.Function<T1, java.util.function.Function<T2, T0>> makeCoder, java.util.function.Function<T1, T1> f, hydra.compute.Flow<T0, T3> flow) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<T0>getState(),
      (java.util.function.Function<T0, hydra.compute.Flow<T0, T3>>) (st -> hydra.lib.flows.Bind.apply(
        hydra.monads.Monads.<T0>putState(((makeCoder).apply((f).apply((getGraph).apply(st)))).apply((getMeta).apply(st))),
        (java.util.function.Function<java.lang.Void, hydra.compute.Flow<T0, T3>>) (ignored -> hydra.lib.flows.Bind.apply(
          flow,
          (java.util.function.Function<T3, hydra.compute.Flow<T0, T3>>) (r -> hydra.lib.flows.Bind.apply(
            hydra.monads.Monads.<T0>getState(),
            (java.util.function.Function<T0, hydra.compute.Flow<T0, T3>>) (st2 -> hydra.lib.flows.Bind.apply(
              hydra.monads.Monads.<T0>putState(((makeCoder).apply((getGraph).apply(st))).apply((getMeta).apply(st2))),
              (java.util.function.Function<java.lang.Void, hydra.compute.Flow<T0, T3>>) (_2 -> hydra.lib.flows.Pure.apply(r)))))))))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T0, T2> withGraphBindings(java.util.function.Function<T0, hydra.graph.Graph> getGraph, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T1, T0>> makeCoder, java.util.function.Function<T0, T1> getMeta, java.util.List<hydra.core.Binding> bindings, hydra.compute.Flow<T0, T2> flow) {
    return hydra.coderUtils.CoderUtils.withUpdatedCoderGraph(
      getGraph,
      getMeta,
      makeCoder,
      (java.util.function.Function<hydra.graph.Graph, hydra.graph.Graph>) (v1 -> hydra.lexical.Lexical.extendGraphWithBindings(
        bindings,
        v1)),
      flow);
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T0, T3> inCoderGraphContext(java.util.function.Function<T0, T1> getGraph, java.util.function.Function<T0, T2> getMeta, java.util.function.Function<T1, java.util.function.Function<T2, T0>> makeCoder, hydra.compute.Flow<T1, T3> graphFlow) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<T0>getState(),
      (java.util.function.Function<T0, hydra.compute.Flow<T0, T3>>) (st -> hydra.lib.flows.Bind.apply(
        hydra.monads.Monads.<T1, hydra.util.Tuple.Tuple2<T3, T1>, T0>withState(
          (getGraph).apply(st),
          hydra.lib.flows.Bind.apply(
            graphFlow,
            (java.util.function.Function<T3, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T3, T1>>>) (ret -> hydra.lib.flows.Bind.apply(
              hydra.monads.Monads.<T1>getState(),
              (java.util.function.Function<T1, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<T3, T1>>>) (g2 -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T3, T1>) ((hydra.util.Tuple.Tuple2<T3, T1>) (new hydra.util.Tuple.Tuple2<T3, T1>(ret, g2))))))))),
        (java.util.function.Function<hydra.util.Tuple.Tuple2<T3, T1>, hydra.compute.Flow<T0, T3>>) (result -> hydra.lib.flows.Bind.apply(
          hydra.monads.Monads.<T0>putState(((makeCoder).apply(hydra.lib.pairs.Second.apply(result))).apply((getMeta).apply(st))),
          (java.util.function.Function<java.lang.Void, hydra.compute.Flow<T0, T3>>) (ignored -> hydra.lib.flows.Pure.apply(hydra.lib.pairs.First.apply(result))))))));
  }
}
