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
  
  static hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term> gatherApplications(hydra.core.Term term) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>>>) (args -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>>) (t -> (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>(args, t)));
      }
      
      @Override
      public hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term> visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = ((app).value).function;
        hydra.core.Term rhs = ((app).value).argument;
        return ((go.get()).apply(hydra.lib.lists.Cons.apply(
          rhs,
          args))).apply(lhs);
      }
    }))));
    return ((go.get()).apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty()))).apply(term);
  }
  
  static hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>> gatherArgs(hydra.core.Term term, hydra.util.ConsList<hydra.core.Term> args) {
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>> otherwise(hydra.core.Term instance) {
        return (hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>>) ((hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>>) (new hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>>(term, args)));
      }
      
      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>> visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = ((app).value).function;
        hydra.core.Term rhs = ((app).value).argument;
        return hydra.coderUtils.CoderUtils.gatherArgs(
          lhs,
          hydra.lib.lists.Cons.apply(
            rhs,
            args));
      }
      
      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term body = ((tl).value).body;
        return hydra.coderUtils.CoderUtils.gatherArgs(
          body,
          args);
      }
      
      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Term body = ((ta).value).body;
        return hydra.coderUtils.CoderUtils.gatherArgs(
          body,
          args);
      }
    });
  }
  
  static hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>> gatherArgsWithTypeApps(hydra.core.Term term, hydra.util.ConsList<hydra.core.Term> args, hydra.util.ConsList<hydra.core.Type> tyArgs) {
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>> otherwise(hydra.core.Term instance) {
        return (hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>>(term, (hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>(args, tyArgs))))));
      }
      
      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>> visit(hydra.core.Term.Application app) {
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
      public hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term body = ((tl).value).body;
        return hydra.coderUtils.CoderUtils.gatherArgsWithTypeApps(
          body,
          args,
          tyArgs);
      }
      
      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>> visit(hydra.core.Term.TypeApplication ta) {
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
          (hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty()))));
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
  
  static Boolean isComplexTerm(hydra.graph.Graph tc, hydra.core.Term t) {
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
  
  static Boolean isComplexVariable(hydra.graph.Graph tc, hydra.core.Name name) {
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
          hydra.util.Lazy<hydra.util.Maybe<hydra.core.TypeScheme>> typeLookup = new hydra.util.Lazy<>(() -> hydra.lib.maps.Lookup.apply(
            name,
            (tc).boundTypes));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> true,
            (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> hydra.lib.equality.Gt.apply(
              hydra.arity.Arity.typeSchemeArity(ts),
              0)),
            typeLookup.get());
        })).get()));
  }
  
  static Boolean isComplexBinding(hydra.graph.Graph tc, hydra.core.Binding b) {
    hydra.util.Maybe<hydra.core.TypeScheme> mts = (b).type;
    hydra.core.Term term = (b).term;
    return hydra.lib.maybes.Cases.applyLazy(
      mts,
      () -> hydra.coderUtils.CoderUtils.isComplexTerm(
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
  
  static Boolean isTrivialTerm(hydra.core.Term t) {
    return (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Literal ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Variable ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Unit ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Application app) {
        hydra.core.Term arg = ((app).value).argument;
        hydra.core.Term fun = ((app).value).function;
        return (fun).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Term instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.Term.Function f) {
            return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public Boolean otherwise(hydra.core.Function instance) {
                return false;
              }
              
              @Override
              public Boolean visit(hydra.core.Function.Elimination e) {
                return ((e).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
                  @Override
                  public Boolean otherwise(hydra.core.Elimination instance) {
                    return false;
                  }
                  
                  @Override
                  public Boolean visit(hydra.core.Elimination.Record ignored) {
                    return hydra.coderUtils.CoderUtils.isTrivialTerm(arg);
                  }
                  
                  @Override
                  public Boolean visit(hydra.core.Elimination.Wrap ignored) {
                    return hydra.coderUtils.CoderUtils.isTrivialTerm(arg);
                  }
                });
              }
            });
          }
        });
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Maybe opt) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> true,
          (java.util.function.Function<hydra.core.Term, Boolean>) (inner -> hydra.coderUtils.CoderUtils.isTrivialTerm(inner)),
          (opt).value);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Record rec) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Field, Boolean>>) (acc -> (java.util.function.Function<hydra.core.Field, Boolean>) (fld -> hydra.lib.logic.And.apply(
            acc,
            hydra.coderUtils.CoderUtils.isTrivialTerm((fld).term)))),
          true,
          ((rec).value).fields);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Wrap wt) {
        return hydra.coderUtils.CoderUtils.isTrivialTerm(((wt).value).body);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.TypeApplication ta) {
        return hydra.coderUtils.CoderUtils.isTrivialTerm(((ta).value).body);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.TypeLambda tl) {
        return hydra.coderUtils.CoderUtils.isTrivialTerm(((tl).value).body);
      }
    });
  }
  
  static Boolean isSelfTailRecursive(hydra.core.Name funcName, hydra.core.Term body) {
    Boolean callsSelf = hydra.lib.logic.Not.apply(hydra.rewriting.Rewriting.isFreeVariableInTerm(
      funcName,
      body));
    return hydra.lib.logic.IfElse.lazy(
      callsSelf,
      () -> hydra.coderUtils.CoderUtils.isTailRecursiveInTailPosition(
        funcName,
        body),
      () -> false);
  }
  
  static Boolean isTailRecursiveInTailPosition(hydra.core.Name funcName, hydra.core.Term term) {
    hydra.core.Term stripped = hydra.rewriting.Rewriting.deannotateAndDetypeTerm(term);
    return (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return hydra.rewriting.Rewriting.isFreeVariableInTerm(
          funcName,
          term);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Application app) {
        hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term> gathered = hydra.coderUtils.CoderUtils.gatherApplications(stripped);
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> gatherArgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered));
        hydra.util.Lazy<hydra.core.Term> gatherFun = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered));
        hydra.core.Term strippedFun = hydra.rewriting.Rewriting.deannotateAndDetypeTerm(gatherFun.get());
        return (strippedFun).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Term instance) {
            return hydra.rewriting.Rewriting.isFreeVariableInTerm(
              funcName,
              term);
          }
          
          @Override
          public Boolean visit(hydra.core.Term.Variable vname) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                (vname).value,
                funcName),
              () -> ((java.util.function.Supplier<Boolean>) (() -> {
                hydra.util.Lazy<Boolean> argsNoFunc = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                  (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (ok -> (java.util.function.Function<hydra.core.Term, Boolean>) (arg -> hydra.lib.logic.And.apply(
                    ok,
                    hydra.rewriting.Rewriting.isFreeVariableInTerm(
                      funcName,
                      arg)))),
                  true,
                  gatherArgs.get()));
                return ((java.util.function.Supplier<Boolean>) (() -> {
                  hydra.util.Lazy<Boolean> argsNoLambda = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                    (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (ok -> (java.util.function.Function<hydra.core.Term, Boolean>) (arg -> hydra.lib.logic.And.apply(
                      ok,
                      hydra.lib.logic.Not.apply(hydra.rewriting.Rewriting.foldOverTerm(
                        new hydra.coders.TraversalOrder.Pre(),
                        (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (found -> (java.util.function.Function<hydra.core.Term, Boolean>) (t -> hydra.lib.logic.Or.apply(
                          found,
                          (t).accept(new hydra.core.Term.PartialVisitor<>() {
                            @Override
                            public Boolean otherwise(hydra.core.Term instance) {
                              return false;
                            }
                            
                            @Override
                            public Boolean visit(hydra.core.Term.Function f2) {
                              return ((f2).value).accept(new hydra.core.Function.PartialVisitor<>() {
                                @Override
                                public Boolean otherwise(hydra.core.Function instance) {
                                  return false;
                                }
                                
                                @Override
                                public Boolean visit(hydra.core.Function.Lambda lam) {
                                  hydra.core.Term ignore = ((lam).value).body;
                                  return true;
                                }
                              });
                            }
                          })))),
                        false,
                        arg))))),
                    true,
                    gatherArgs.get()));
                  return hydra.lib.logic.And.apply(
                    argsNoFunc.get(),
                    argsNoLambda.get());
                })).get();
              })).get(),
              () -> hydra.rewriting.Rewriting.isFreeVariableInTerm(
                funcName,
                term));
          }
          
          @Override
          public Boolean visit(hydra.core.Term.Function f) {
            return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public Boolean otherwise(hydra.core.Function instance) {
                return hydra.rewriting.Rewriting.isFreeVariableInTerm(
                  funcName,
                  term);
              }
              
              @Override
              public Boolean visit(hydra.core.Function.Elimination e) {
                return ((e).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
                  @Override
                  public Boolean otherwise(hydra.core.Elimination instance) {
                    return hydra.rewriting.Rewriting.isFreeVariableInTerm(
                      funcName,
                      term);
                  }
                  
                  @Override
                  public Boolean visit(hydra.core.Elimination.Union cs) {
                    hydra.util.Lazy<Boolean> argsOk = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                      (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (ok -> (java.util.function.Function<hydra.core.Term, Boolean>) (arg -> hydra.lib.logic.And.apply(
                        ok,
                        hydra.rewriting.Rewriting.isFreeVariableInTerm(
                          funcName,
                          arg)))),
                      true,
                      gatherArgs.get()));
                    hydra.util.ConsList<hydra.core.Field> cases_ = ((cs).value).cases;
                    hydra.util.Lazy<Boolean> branchesOk = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                      (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Field, Boolean>>) (ok -> (java.util.function.Function<hydra.core.Field, Boolean>) (field -> hydra.lib.logic.And.apply(
                        ok,
                        hydra.coderUtils.CoderUtils.isTailRecursiveInTailPosition(
                          funcName,
                          (field).term)))),
                      true,
                      cases_));
                    hydra.util.Maybe<hydra.core.Term> dflt = ((cs).value).default_;
                    hydra.util.Lazy<Boolean> dfltOk = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
                      () -> true,
                      (java.util.function.Function<hydra.core.Term, Boolean>) (d -> hydra.coderUtils.CoderUtils.isTailRecursiveInTailPosition(
                        funcName,
                        d)),
                      dflt));
                    return hydra.lib.logic.And.apply(
                      hydra.lib.logic.And.apply(
                        branchesOk.get(),
                        dfltOk.get()),
                      argsOk.get());
                  }
                });
              }
            });
          }
        });
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Function instance) {
            return hydra.rewriting.Rewriting.isFreeVariableInTerm(
              funcName,
              term);
          }
          
          @Override
          public Boolean visit(hydra.core.Function.Lambda lam) {
            return hydra.coderUtils.CoderUtils.isTailRecursiveInTailPosition(
              funcName,
              ((lam).value).body);
          }
        });
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Let lt) {
        hydra.util.Lazy<Boolean> bindingsOk = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Binding, Boolean>>) (ok -> (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.logic.And.apply(
            ok,
            hydra.rewriting.Rewriting.isFreeVariableInTerm(
              funcName,
              (b).term)))),
          true,
          ((lt).value).bindings));
        return hydra.lib.logic.And.apply(
          bindingsOk.get(),
          hydra.coderUtils.CoderUtils.isTailRecursiveInTailPosition(
            funcName,
            ((lt).value).body));
      }
    });
  }
  
  static String nameToFilePath(hydra.util.CaseConvention nsConv, hydra.util.CaseConvention localConv, hydra.module.FileExtension ext, hydra.core.Name name) {
    hydra.module.QualifiedName qualName = hydra.names.Names.qualifyName(name);
    String local = (qualName).local;
    hydra.util.Maybe<hydra.module.Namespace> ns = (qualName).namespace;
    java.util.function.Function<hydra.module.Namespace, String> nsToFilePath = (java.util.function.Function<hydra.module.Namespace, String>) (ns2 -> hydra.lib.strings.Intercalate.apply(
      "/",
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<String, String>) (part -> hydra.formatting.Formatting.convertCase(
          new hydra.util.CaseConvention.Camel(),
          nsConv,
          part)),
        hydra.lib.strings.SplitOn.apply(
          ".",
          (ns2).value))));
    hydra.util.Lazy<String> prefix = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> "",
      (java.util.function.Function<hydra.module.Namespace, String>) (n -> hydra.lib.strings.Cat2.apply(
        (nsToFilePath).apply(n),
        "/")),
      ns));
    String suffix = hydra.formatting.Formatting.convertCase(
      new hydra.util.CaseConvention.Pascal(),
      localConv,
      local);
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      prefix.get(),
      suffix,
      ".",
      (ext).value));
  }
  
  static hydra.core.RowType unionTypeToRecordType(hydra.core.RowType rt) {
    java.util.function.Function<hydra.core.FieldType, hydra.core.FieldType> makeOptional = (java.util.function.Function<hydra.core.FieldType, hydra.core.FieldType>) (f -> {
      hydra.core.Name fn = (f).name;
      hydra.core.Type ft = (f).type;
      return new hydra.core.FieldType(fn, hydra.rewriting.Rewriting.mapBeneathTypeAnnotations(
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.Maybe(x)),
        ft));
    });
    return new hydra.core.RowType((rt).typeName, hydra.lib.lists.Map.apply(
      makeOptional,
      (rt).fields));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<String>> commentsFromElement(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Binding b) {
    return hydra.annotations.Annotations.getTermDescription(
      cx,
      g,
      (b).term);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<String>> commentsFromFieldType(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.FieldType ft) {
    return hydra.annotations.Annotations.getTypeDescription(
      cx,
      g,
      (ft).type);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> typeOfTerm(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.core.Type>) ((java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.core.Type>) (hydra.lib.pairs.First::apply)),
      hydra.checking.Checking.typeOf(
        cx,
        g,
        (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
        term));
  }
  
  static hydra.util.Maybe<hydra.core.Term> bindingMetadata(hydra.graph.Graph tc, hydra.core.Binding b) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.coderUtils.CoderUtils.isComplexBinding(
        tc,
        b),
      () -> hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))),
      () -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()));
  }
  
  static <T0, T1> hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTerm(hydra.context.Context cx, java.util.function.Function<T0, hydra.graph.Graph> getTC, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T0>> setTC, T0 env, hydra.core.Term term) {
    return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith(
      cx,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (p0 -> p1 -> hydra.coderUtils.CoderUtils.bindingMetadata(
        p0,
        p1)),
      getTC,
      setTC,
      env,
      term);
  }
  
  static <T0, T1> hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTermWith(hydra.context.Context cx, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, java.util.function.Function<T0, hydra.graph.Graph> getTC, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T0>> setTC, T0 env, hydra.core.Term term) {
    return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_gather(
      cx,
      forBinding,
      getTC,
      setTC,
      true,
      env,
      (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()),
      (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()),
      (hydra.util.ConsList<hydra.core.Binding>) (hydra.util.ConsList.<hydra.core.Binding>empty()),
      (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
      (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
      term);
  }
  
  static <T0, T1> hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTermWith_finish(hydra.context.Context cx, java.util.function.Function<T0, hydra.graph.Graph> getTC, T0 fEnv, hydra.util.ConsList<hydra.core.Name> tparams, hydra.util.ConsList<hydra.core.Name> args, hydra.util.ConsList<hydra.core.Binding> bindings, hydra.util.ConsList<hydra.core.Type> doms, hydra.util.ConsList<hydra.core.Type> tapps, hydra.core.Term body) {
    hydra.util.Lazy<hydra.core.Term> bodyWithTapps = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Type, hydra.core.Term>>) (trm -> (java.util.function.Function<hydra.core.Type, hydra.core.Term>) (typ -> new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(trm, typ)))),
      body,
      tapps));
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> mcod = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.Type>>) (ignored -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
      (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (c -> hydra.util.Maybe.just(c)),
      hydra.coderUtils.CoderUtils.typeOfTerm(
        cx,
        (getTC).apply(fEnv),
        bodyWithTapps.get())));
    return hydra.util.Either.<T1, hydra.typing.FunctionStructure<T0>>right((hydra.typing.FunctionStructure<T0>) (new hydra.typing.FunctionStructure<T0>(hydra.lib.lists.Reverse.apply(tparams), hydra.lib.lists.Reverse.apply(args), bindings, bodyWithTapps.get(), hydra.lib.lists.Reverse.apply(doms), mcod.get(), fEnv)));
  }
  
  static <T0, T1> hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTermWith_gather(hydra.context.Context cx, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, java.util.function.Function<T0, hydra.graph.Graph> getTC, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T0>> setTC, Boolean argMode, T0 gEnv, hydra.util.ConsList<hydra.core.Name> tparams, hydra.util.ConsList<hydra.core.Name> args, hydra.util.ConsList<hydra.core.Binding> bindings, hydra.util.ConsList<hydra.core.Type> doms, hydra.util.ConsList<hydra.core.Type> tapps, hydra.core.Term t) {
    return (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> otherwise(hydra.core.Term instance) {
        return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_finish(
          cx,
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
      public hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> otherwise(hydra.core.Function instance) {
            return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_finish(
              cx,
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
          public hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Function.Lambda lam) {
            return hydra.lib.logic.IfElse.lazy(
              argMode,
              () -> ((java.util.function.Supplier<hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>>>) (() -> {
                hydra.core.Name v = ((lam).value).parameter;
                return ((java.util.function.Supplier<hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>>>) (() -> {
                  hydra.util.Lazy<hydra.core.Type> dom = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
                    () -> new hydra.core.Type.Variable(new hydra.core.Name("_")),
                    (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x_ -> x_),
                    ((lam).value).domain));
                  return ((java.util.function.Supplier<hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>>>) (() -> {
                    hydra.core.Term body = ((lam).value).body;
                    return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_gather(
                      cx,
                      forBinding,
                      getTC,
                      setTC,
                      argMode,
                      hydra.coderUtils.CoderUtils.<T0>analyzeFunctionTermWith_gather_newEnv(
                        gEnv,
                        getTC,
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.schemas.Schemas.extendGraphForLambda(
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
                cx,
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
      public hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.Let lt) {
        hydra.core.Term body = ((lt).value).body;
        hydra.util.ConsList<hydra.core.Binding> newBindings = ((lt).value).bindings;
        return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_gather(
          cx,
          forBinding,
          getTC,
          setTC,
          false,
          hydra.coderUtils.CoderUtils.<T0>analyzeFunctionTermWith_gather_newEnv2(
            forBinding,
            gEnv,
            getTC,
            (java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>>) (p0 -> p1 -> p2 -> hydra.schemas.Schemas.extendGraphForLet(
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
      public hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Term taBody = ((ta).value).body;
        hydra.core.Type typ = ((ta).value).type;
        return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_gather(
          cx,
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
      public hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term tlBody = ((tl).value).body;
        hydra.core.Name tvar = ((tl).value).parameter;
        return hydra.coderUtils.CoderUtils.<T0, T1>analyzeFunctionTermWith_gather(
          cx,
          forBinding,
          getTC,
          setTC,
          argMode,
          hydra.coderUtils.CoderUtils.<T0>analyzeFunctionTermWith_gather_newEnv3(
            gEnv,
            getTC,
            (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.schemas.Schemas.extendGraphForTypeLambda(
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
  
  static <T0> T0 analyzeFunctionTermWith_gather_newEnv(T0 gEnv, java.util.function.Function<T0, hydra.graph.Graph> getTC, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>> hydra_schemas_extendGraphForLambda2, hydra.core.Lambda lam, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T0>> setTC) {
    return ((setTC).apply(((hydra_schemas_extendGraphForLambda2).apply((getTC).apply(gEnv))).apply(lam))).apply(gEnv);
  }
  
  static <T0> T0 analyzeFunctionTermWith_gather_newEnv2(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, T0 gEnv, java.util.function.Function<T0, hydra.graph.Graph> getTC, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>> hydra_schemas_extendGraphForLet2, hydra.core.Let lt, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T0>> setTC) {
    return ((setTC).apply((((hydra_schemas_extendGraphForLet2).apply(forBinding)).apply((getTC).apply(gEnv))).apply(lt))).apply(gEnv);
  }
  
  static <T0> T0 analyzeFunctionTermWith_gather_newEnv3(T0 gEnv, java.util.function.Function<T0, hydra.graph.Graph> getTC, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>> hydra_schemas_extendGraphForTypeLambda2, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T0>> setTC, hydra.core.TypeLambda tl) {
    return ((setTC).apply(((hydra_schemas_extendGraphForTypeLambda2).apply((getTC).apply(gEnv))).apply(tl))).apply(gEnv);
  }
}
