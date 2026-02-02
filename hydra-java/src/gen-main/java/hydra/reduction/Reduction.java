// Note: this is an automatically generated file. Do not edit.

package hydra.reduction;

/**
 * Functions for reducing terms and types, i.e. performing computations.
 */
public interface Reduction {
  static hydra.core.Term alphaConvert(hydra.core.Name vold, hydra.core.Name vnew, hydra.core.Term term) {
    return hydra.rewriting.Rewriting.replaceFreeTermVariable(
      (vold),
      new hydra.core.Term.Variable((vnew)),
      (term));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type> betaReduceType(hydra.core.Type typ) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.ApplicationType, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>> reduceApp = new java.util.concurrent.atomic.AtomicReference<>();
    reduceApp.set((java.util.function.Function<hydra.core.ApplicationType, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (app -> {
      hydra.core.Type lhs = ((app)).function;
      hydra.core.Type rhs = ((app)).argument;
      return ((lhs)).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type> visit(hydra.core.Type.Annotated at) {
          return hydra.lib.flows.Bind.apply(
            (reduceApp.get()).apply(new hydra.core.ApplicationType((((at)).value).body, (rhs))),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (a -> hydra.lib.flows.Pure.apply(new hydra.core.Type.Annotated(new hydra.core.AnnotatedType((a), (((at)).value).annotation)))));
        }
        
        @Override
        public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type> visit(hydra.core.Type.Forall ft) {
          return hydra.reduction.Reduction.betaReduceType(hydra.rewriting.Rewriting.replaceFreeTypeVariable(
            (((ft)).value).parameter,
            (rhs),
            (((ft)).value).body));
        }
        
        @Override
        public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type> visit(hydra.core.Type.Variable name) {
          return hydra.lib.flows.Bind.apply(
            hydra.schemas.Schemas.requireType(((name)).value),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (t_ -> hydra.reduction.Reduction.betaReduceType(new hydra.core.Type.Application(new hydra.core.ApplicationType((t_), (rhs))))));
        }
      });
    }));
    return hydra.rewriting.Rewriting.rewriteTypeM(
      (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>, java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>>) (v1 -> (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Type>>) (v2 -> hydra.reduction.Reduction.betaReduceType_mapExpr(
        reduceApp.get(),
        (v1),
        (v2)))),
      (typ));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, hydra.core.Type> betaReduceType_mapExpr(java.util.function.Function<hydra.core.ApplicationType, hydra.compute.Flow<T0, hydra.core.Type>> reduceApp, java.util.function.Function<T1, hydra.compute.Flow<T0, hydra.core.Type>> recurse, T1 t) {
    java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>> findApp = (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (r -> ((r)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.lib.flows.Pure.apply((r));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Type> visit(hydra.core.Type.Application a) {
        return ((reduceApp)).apply(((a)).value);
      }
    }));
    return hydra.lib.flows.Bind.apply(
      ((recurse)).apply((t)),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.core.Type>>) (r -> ((findApp)).apply((r))));
  }
  
  static hydra.core.Term contractTerm(hydra.core.Term term) {
    return hydra.rewriting.Rewriting.rewriteTerm(
      (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v2 -> hydra.reduction.Reduction.contractTerm_rewrite(
        (hydra.rewriting.Rewriting::deannotateTerm),
        (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, Boolean>>) (p0 -> p1 -> hydra.rewriting.Rewriting.isFreeVariableInTerm(
          (p0),
          (p1))),
        (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (p0 -> p1 -> p2 -> hydra.rewriting.Rewriting.replaceFreeTermVariable(
          (p0),
          (p1),
          (p2))),
        (v1),
        (v2)))),
      (term));
  }
  
  static <T0> hydra.core.Term contractTerm_rewrite(java.util.function.Function<hydra.core.Term, hydra.core.Term> hydra_rewriting_deannotateTerm2, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, Boolean>> hydra_rewriting_isFreeVariableInTerm2, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> hydra_rewriting_replaceFreeTermVariable2, java.util.function.Function<T0, hydra.core.Term> recurse, T0 t) {
    hydra.core.Term rec = ((recurse)).apply((t));
    return ((rec)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (rec);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = (((app)).value).function;
        hydra.core.Term rhs = (((app)).value).argument;
        return (((hydra_rewriting_deannotateTerm2)).apply((lhs))).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Term instance) {
            return (rec);
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Term.Function f) {
            return (((f)).value).accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public hydra.core.Term otherwise(hydra.core.Function instance) {
                return (rec);
              }
              
              @Override
              public hydra.core.Term visit(hydra.core.Function.Lambda l) {
                hydra.core.Term body = (((l)).value).body;
                hydra.core.Name v = (((l)).value).parameter;
                return hydra.lib.logic.IfElse.apply(
                  (((hydra_rewriting_isFreeVariableInTerm2)).apply((v))).apply((body)),
                  (body),
                  ((((hydra_rewriting_replaceFreeTermVariable2)).apply((v))).apply((rhs))).apply((body)));
              }
            });
          }
        });
      }
    });
  }
  
  Boolean countPrimitiveInvocations = true;
  
  static hydra.core.Term etaReduceTerm(hydra.core.Term term) {
    hydra.core.Term noChange = (term);
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Lambda, hydra.core.Term>> reduceLambda = new java.util.concurrent.atomic.AtomicReference<>();
    reduceLambda.set((java.util.function.Function<hydra.core.Lambda, hydra.core.Term>) (l -> {
      hydra.core.Term body = ((l)).body;
      hydra.util.Maybe<hydra.core.Type> d = ((l)).domain;
      hydra.core.Name v = ((l)).parameter;
      return (hydra.reduction.Reduction.etaReduceTerm((body))).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return (noChange);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Annotated at) {
          return (reduceLambda.get()).apply(new hydra.core.Lambda((v), (d), (((at)).value).body));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Application app) {
          hydra.core.Term lhs = (((app)).value).function;
          hydra.core.Term rhs = (((app)).value).argument;
          return (hydra.reduction.Reduction.etaReduceTerm((rhs))).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.core.Term otherwise(hydra.core.Term instance) {
              return (noChange);
            }
            
            @Override
            public hydra.core.Term visit(hydra.core.Term.Annotated at) {
              return (reduceLambda.get()).apply(new hydra.core.Lambda((v), (d), new hydra.core.Term.Application(new hydra.core.Application((lhs), (((at)).value).body))));
            }
            
            @Override
            public hydra.core.Term visit(hydra.core.Term.Variable v1) {
              return hydra.lib.logic.IfElse.apply(
                hydra.lib.logic.And.apply(
                  hydra.lib.equality.Equal.apply(
                    ((v)).value,
                    (((v1)).value).value),
                  hydra.lib.logic.Not.apply(hydra.rewriting.Rewriting.isFreeVariableInTerm(
                    (v),
                    (lhs)))),
                hydra.reduction.Reduction.etaReduceTerm((lhs)),
                (noChange));
            }
          });
        }
      });
    }));
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (noChange);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.reduction.Reduction.etaReduceTerm((((at)).value).body), (((at)).value).annotation));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return (((f)).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return (noChange);
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            return (reduceLambda.get()).apply(((l)).value);
          }
        });
      }
    });
  }
  
  static hydra.core.Term etaExpandTerm(hydra.graph.Graph graph, hydra.core.Term term) {
    java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> expand = (java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (args -> (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (arity -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> {
      hydra.core.Term apps = hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (lhs -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (arg -> new hydra.core.Term.Application(new hydra.core.Application((lhs), (arg))))),
        (t),
        (args));
      java.util.List<Integer> is = hydra.lib.logic.IfElse.apply(
        hydra.lib.equality.Lte.apply(
          (arity),
          hydra.lib.lists.Length.apply((args))),
        (java.util.List<Integer>) (java.util.List.<Integer>of()),
        hydra.lib.math.Range.apply(
          1,
          hydra.lib.math.Sub.apply(
            (arity),
            hydra.lib.lists.Length.apply((args)))));
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<Integer>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> pad = new java.util.concurrent.atomic.AtomicReference<>();
      pad.set((java.util.function.Function<java.util.List<Integer>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (indices -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t2 -> hydra.lib.logic.IfElse.apply(
        hydra.lib.lists.Null.apply((indices)),
        (t2),
        new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name(hydra.lib.strings.Cat2.apply(
          "v",
          hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Head.apply((indices))))), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), ((pad.get()).apply(hydra.lib.lists.Tail.apply((indices)))).apply(new hydra.core.Term.Application(new hydra.core.Application((t2), new hydra.core.Term.Variable(new hydra.core.Name(hydra.lib.strings.Cat2.apply(
          "v",
          hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Head.apply((indices))))))))))))))));
      return ((pad.get()).apply((is))).apply((apps));
    })));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>> rewrite = new java.util.concurrent.atomic.AtomicReference<>();
    rewrite.set((java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (args -> (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> {
      java.util.function.Function<hydra.core.Term, hydra.core.Term> afterRecursion = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> ((((expand)).apply((args))).apply(hydra.reduction.Reduction.etaExpansionArity(
        (graph),
        (term2)))).apply((term2)));
      hydra.core.Term t2 = hydra.rewriting.Rewriting.detypeTerm((t));
      return ((t2)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return ((afterRecursion)).apply(((recurse)).apply((t2)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Application app) {
          hydra.core.Term rhs = (((app)).value).argument;
          hydra.core.Term erhs = (((rewrite.get()).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))).apply((recurse))).apply((rhs));
          hydra.core.Term lhs = (((app)).value).function;
          return (((rewrite.get()).apply(hydra.lib.lists.Cons.apply(
            (erhs),
            (args)))).apply((recurse))).apply((lhs));
        }
      });
    }))));
    return hydra.reduction.Reduction.contractTerm(hydra.rewriting.Rewriting.rewriteTerm(
      (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v2 -> (((rewrite.get()).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))).apply((v1))).apply((v2)))),
      (term)));
  }
  
  static hydra.core.Term etaExpandTermNew(hydra.typing.TypeContext tx0, hydra.core.Term term0) {
    java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>> expand = (java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>>) (alwaysPad -> (java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (args -> (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (arity -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (head -> {
      hydra.core.Term applied = hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (lhs -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (arg -> new hydra.core.Term.Application(new hydra.core.Application((lhs), (arg))))),
        (head),
        (args));
      Integer numArgs = hydra.lib.lists.Length.apply((args));
      Integer needed = hydra.lib.math.Sub.apply(
        (arity),
        (numArgs));
      return hydra.lib.logic.IfElse.apply(
        hydra.lib.logic.And.apply(
          hydra.lib.equality.Gt.apply(
            (needed),
            0),
          hydra.lib.logic.Or.apply(
            (alwaysPad),
            hydra.lib.equality.Gt.apply(
              (numArgs),
              0))),
        ((java.util.function.Function<java.util.List<Integer>, hydra.core.Term>) (indices -> {
          hydra.core.Term fullyApplied = hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.core.Term, java.util.function.Function<Integer, hydra.core.Term>>) (body -> (java.util.function.Function<Integer, hydra.core.Term>) (i -> {
              hydra.core.Name vn = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                "v",
                hydra.lib.literals.ShowInt32.apply((i))));
              return new hydra.core.Term.Application(new hydra.core.Application((body), new hydra.core.Term.Variable((vn))));
            })),
            (applied),
            (indices));
          return hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.core.Term, java.util.function.Function<Integer, hydra.core.Term>>) (body -> (java.util.function.Function<Integer, hydra.core.Term>) (i -> {
              hydra.core.Name vn = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                "v",
                hydra.lib.literals.ShowInt32.apply((i))));
              return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((vn), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), (body))));
            })),
            (fullyApplied),
            hydra.lib.lists.Reverse.apply((indices)));
        })).apply(hydra.lib.math.Range.apply(
          1,
          (needed))),
        (applied));
    }))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, Integer>>> termArityWithContext = new java.util.concurrent.atomic.AtomicReference<>();
    termArityWithContext.set((java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, Integer>>) (tx -> (java.util.function.Function<hydra.core.Term, Integer>) (term -> ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Term instance) {
        return 0;
      }
      
      @Override
      public Integer visit(hydra.core.Term.Annotated at) {
        return ((termArityWithContext.get()).apply((tx))).apply((((at)).value).body);
      }
      
      @Override
      public Integer visit(hydra.core.Term.Application app) {
        return hydra.lib.math.Sub.apply(
          ((termArityWithContext.get()).apply((tx))).apply((((app)).value).function),
          1);
      }
      
      @Override
      public Integer visit(hydra.core.Term.Function f) {
        return (((f)).value).accept(new hydra.core.Function.PartialVisitor<>() {
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
            return hydra.lib.maybes.Maybe.apply(
              0,
              (hydra.arity.Arity::typeSchemeArity),
              hydra.lib.maps.Lookup.apply(
                ((name)).value,
                (((tx)).inferenceContext).primitiveTypes));
          }
        });
      }
      
      @Override
      public Integer visit(hydra.core.Term.Let l) {
        return ((termArityWithContext.get()).apply(hydra.schemas.Schemas.extendTypeContextForLet(
          (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
          (tx),
          ((l)).value))).apply((((l)).value).body);
      }
      
      @Override
      public Integer visit(hydra.core.Term.TypeLambda tl) {
        return ((termArityWithContext.get()).apply(hydra.schemas.Schemas.extendTypeContextForTypeLambda(
          (tx),
          ((tl)).value))).apply((((tl)).value).body);
      }
      
      @Override
      public Integer visit(hydra.core.Term.TypeApplication tat) {
        return ((termArityWithContext.get()).apply((tx))).apply((((tat)).value).body);
      }
      
      @Override
      public Integer visit(hydra.core.Term.Variable name) {
        return hydra.lib.maybes.Maybe.apply(
          0,
          (hydra.arity.Arity::typeArity),
          hydra.lib.maps.Lookup.apply(
            ((name)).value,
            ((tx)).types));
      }
    }))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>> rewriteWithArgs = new java.util.concurrent.atomic.AtomicReference<>();
    rewriteWithArgs.set((java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (args -> (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (tx -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> {
      java.util.function.Function<hydra.core.Term, hydra.core.Term> afterRecursion = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (trm -> {
        Integer arity = ((termArityWithContext.get()).apply((tx))).apply((trm));
        return (((((expand)).apply(false)).apply((args))).apply((arity))).apply((trm));
      });
      java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.core.Term>> recurse = (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (tx1 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term1 -> (((rewriteWithArgs.get()).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))).apply((tx1))).apply((term1))));
      java.util.function.Function<hydra.core.Field, hydra.core.Field> forCaseBranch = (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (f -> {
        hydra.core.Term branchBody = (((recurse)).apply((tx))).apply(((f)).term);
        Integer arty = ((termArityWithContext.get()).apply((tx))).apply((branchBody));
        return new hydra.core.Field(((f)).name, (((((expand)).apply(true)).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))).apply((arty))).apply((branchBody)));
      });
      java.util.function.Function<hydra.core.Elimination, hydra.core.Elimination> forElimination = (java.util.function.Function<hydra.core.Elimination, hydra.core.Elimination>) (elm -> ((elm)).accept(new hydra.core.Elimination.PartialVisitor<>() {
        @Override
        public hydra.core.Elimination visit(hydra.core.Elimination.Record p) {
          return new hydra.core.Elimination.Record(((p)).value);
        }
        
        @Override
        public hydra.core.Elimination visit(hydra.core.Elimination.Union cs) {
          return new hydra.core.Elimination.Union(new hydra.core.CaseStatement((((cs)).value).typeName, hydra.lib.maybes.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t1 -> (((recurse)).apply((tx))).apply((t1))),
            (((cs)).value).default_), hydra.lib.lists.Map.apply(
            (forCaseBranch),
            (((cs)).value).cases)));
        }
        
        @Override
        public hydra.core.Elimination visit(hydra.core.Elimination.Wrap nm) {
          return new hydra.core.Elimination.Wrap(((nm)).value);
        }
      }));
      java.util.function.Function<hydra.core.Field, hydra.core.Field> forField = (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (f -> new hydra.core.Field(((f)).name, (((recurse)).apply((tx))).apply(((f)).term)));
      java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>> forMap = (java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>>) (mp -> {
        java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>> forPair = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) (pr -> (hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>((((recurse)).apply((tx))).apply(hydra.lib.pairs.First.apply((pr))), (((recurse)).apply((tx))).apply(hydra.lib.pairs.Second.apply((pr)))))));
        return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (forPair),
          hydra.lib.maps.ToList.apply((mp))));
      });
      return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term visit(hydra.core.Term.Annotated at) {
          return ((afterRecursion)).apply(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm((((recurse)).apply((tx))).apply((((at)).value).body), (((at)).value).annotation)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Application app) {
          hydra.core.Term rhs = (((rewriteWithArgs.get()).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))).apply((tx))).apply((((app)).value).argument);
          return (((rewriteWithArgs.get()).apply(hydra.lib.lists.Cons.apply(
            (rhs),
            (args)))).apply((tx))).apply((((app)).value).function);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Either e) {
          return ((afterRecursion)).apply(new hydra.core.Term.Either(hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (l -> (hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left((((recurse)).apply((tx))).apply((l)))))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (r -> (hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right((((recurse)).apply((tx))).apply((r)))))),
            ((e)).value)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Function fn) {
          return (((fn)).value).accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.core.Term visit(hydra.core.Function.Elimination elm) {
              Boolean padElim = (((elm)).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
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
              return (((((expand)).apply((padElim))).apply((args))).apply(1)).apply(new hydra.core.Term.Function(new hydra.core.Function.Elimination(((forElimination)).apply(((elm)).value))));
            }
            
            @Override
            public hydra.core.Term visit(hydra.core.Function.Lambda lm) {
              hydra.typing.TypeContext tx1 = hydra.schemas.Schemas.extendTypeContextForLambda(
                (tx),
                ((lm)).value);
              hydra.core.Term body = (((rewriteWithArgs.get()).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))).apply((tx1))).apply((((lm)).value).body);
              hydra.core.Term result = new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((((lm)).value).parameter, (((lm)).value).domain, (body))));
              Integer arty = ((termArityWithContext.get()).apply((tx))).apply((result));
              return (((((expand)).apply(false)).apply((args))).apply((arty))).apply((result));
            }
            
            @Override
            public hydra.core.Term visit(hydra.core.Function.Primitive pn) {
              Integer arty = ((termArityWithContext.get()).apply((tx))).apply((term));
              return (((((expand)).apply(false)).apply((args))).apply((arty))).apply((term));
            }
          });
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Let lt) {
          hydra.typing.TypeContext tx1 = hydra.schemas.Schemas.extendTypeContextForLet(
            (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
            (tx),
            ((lt)).value);
          java.util.function.Function<hydra.core.Binding, hydra.core.Binding> mapBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding(((b)).name, (((rewriteWithArgs.get()).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))).apply((tx1))).apply(((b)).term), ((b)).type));
          hydra.core.Term result = new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
            (mapBinding),
            (((lt)).value).bindings), (((rewriteWithArgs.get()).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))).apply((tx1))).apply((((lt)).value).body)));
          return ((afterRecursion)).apply((result));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.List els) {
          return ((afterRecursion)).apply(new hydra.core.Term.List(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> (((recurse)).apply((tx))).apply((el))),
            ((els)).value)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Literal v) {
          return new hydra.core.Term.Literal(((v)).value);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Map mp) {
          return ((afterRecursion)).apply(new hydra.core.Term.Map(((forMap)).apply(((mp)).value)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Maybe mb) {
          return ((afterRecursion)).apply(new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v -> (((recurse)).apply((tx))).apply((v))),
            ((mb)).value)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Pair pr) {
          return ((afterRecursion)).apply(new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>((((recurse)).apply((tx))).apply(hydra.lib.pairs.First.apply(((pr)).value)), (((recurse)).apply((tx))).apply(hydra.lib.pairs.Second.apply(((pr)).value)))))));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Record rc) {
          return ((afterRecursion)).apply(new hydra.core.Term.Record(new hydra.core.Record((((rc)).value).typeName, hydra.lib.lists.Map.apply(
            (forField),
            (((rc)).value).fields))));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Set st) {
          return ((afterRecursion)).apply(new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> (((recurse)).apply((tx))).apply((el))),
            hydra.lib.sets.ToList.apply(((st)).value)))));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
          return ((afterRecursion)).apply(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((((recurse)).apply((tx))).apply((((tt)).value).body), (((tt)).value).type)));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
          hydra.typing.TypeContext tx1 = hydra.schemas.Schemas.extendTypeContextForTypeLambda(
            (tx),
            ((tl)).value);
          hydra.core.Term result = new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda((((tl)).value).parameter, (((rewriteWithArgs.get()).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))).apply((tx1))).apply((((tl)).value).body)));
          return ((afterRecursion)).apply((result));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Union inj) {
          return ((afterRecursion)).apply(new hydra.core.Term.Union(new hydra.core.Injection((((inj)).value).typeName, ((forField)).apply((((inj)).value).field))));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Unit ignored) {
          return new hydra.core.Term.Unit(true);
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Variable vn) {
          Integer arty = ((termArityWithContext.get()).apply((tx))).apply((term));
          return (((((expand)).apply(false)).apply((args))).apply((arty))).apply((term));
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Wrap wt) {
          return ((afterRecursion)).apply(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm((((wt)).value).typeName, (((recurse)).apply((tx))).apply((((wt)).value).body))));
        }
      });
    }))));
    return hydra.reduction.Reduction.contractTerm((((rewriteWithArgs.get()).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))).apply((tx0))).apply((term0)));
  }
  
  static Integer etaExpansionArity(hydra.graph.Graph graph, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Term instance) {
        return 0;
      }
      
      @Override
      public Integer visit(hydra.core.Term.Annotated at) {
        return hydra.reduction.Reduction.etaExpansionArity(
          (graph),
          (((at)).value).body);
      }
      
      @Override
      public Integer visit(hydra.core.Term.Application app) {
        return hydra.lib.math.Sub.apply(
          hydra.reduction.Reduction.etaExpansionArity(
            (graph),
            (((app)).value).function),
          1);
      }
      
      @Override
      public Integer visit(hydra.core.Term.Function f) {
        return (((f)).value).accept(new hydra.core.Function.PartialVisitor<>() {
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
            return hydra.arity.Arity.primitiveArity(hydra.lib.maybes.FromJust.apply(hydra.lexical.Lexical.lookupPrimitive(
              (graph),
              ((name)).value)));
          }
        });
      }
      
      @Override
      public Integer visit(hydra.core.Term.TypeLambda ta) {
        return hydra.reduction.Reduction.etaExpansionArity(
          (graph),
          (((ta)).value).body);
      }
      
      @Override
      public Integer visit(hydra.core.Term.TypeApplication tt) {
        return hydra.reduction.Reduction.etaExpansionArity(
          (graph),
          (((tt)).value).body);
      }
      
      @Override
      public Integer visit(hydra.core.Term.Variable name) {
        return hydra.lib.maybes.Maybe.apply(
          0,
          (java.util.function.Function<hydra.core.TypeScheme, Integer>) (ts -> hydra.arity.Arity.typeArity(((ts)).type)),
          hydra.lib.maybes.Bind.apply(
            hydra.lexical.Lexical.lookupElement(
              (graph),
              ((name)).value),
            (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.TypeScheme>>) (b -> ((b)).type)));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> etaExpandTypedTerm(hydra.typing.TypeContext tx0, hydra.core.Term term0) {
    return hydra.rewriting.Rewriting.rewriteTermWithContextM(
      ((((java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>>>>>>) (v1 -> (java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>>>>>) (v2 -> (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>>>>) (v3 -> (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>>>) (v4 -> (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>>) (v5 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.core.Term>>) (v6 -> hydra.reduction.Reduction.etaExpandTypedTerm_rewrite(
        (hydra.arity.Arity::typeArity),
        (hydra.arity.Arity::typeSchemeArity),
        (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForLambda(
          (p0),
          (p1))),
        (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>>) (p0 -> p1 -> p2 -> hydra.schemas.Schemas.extendTypeContextForLet(
          (p0),
          (p1),
          (p2))),
        (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForTypeLambda(
          (p0),
          (p1))),
        (v1),
        (v2),
        (v3),
        (v4),
        (v5),
        (v6))))))))).apply(true)).apply(false)).apply((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of())),
      (tx0),
      (term0));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T3, hydra.core.Term> etaExpandTypedTerm_rewrite(java.util.function.Function<hydra.core.Type, Integer> hydra_arity_typeArity2, java.util.function.Function<hydra.core.TypeScheme, Integer> hydra_arity_typeSchemeArity2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>> hydra_schemas_extendTypeContextForLambda2, java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<T2>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>> hydra_schemas_extendTypeContextForLet2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>> hydra_schemas_extendTypeContextForTypeLambda2, Boolean topLevel, Boolean forced, java.util.List<hydra.core.Type> typeArgs, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>> recurse, hydra.typing.TypeContext tx, hydra.core.Term term) {
    java.util.function.Function<Integer, java.util.List<hydra.core.Name>> extraVariables = (java.util.function.Function<Integer, java.util.List<hydra.core.Name>>) (n -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<Integer, hydra.core.Name>) (i -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
        "v",
        hydra.lib.literals.ShowInt32.apply((i))))),
      hydra.lib.math.Range.apply(
        1,
        (n))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> pad = new java.util.concurrent.atomic.AtomicReference<>();
    pad.set((java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (vars -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (body -> hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((vars)),
      (body),
      new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(hydra.lib.lists.Head.apply((vars)), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), ((pad.get()).apply(hydra.lib.lists.Tail.apply((vars)))).apply(new hydra.core.Term.Application(new hydra.core.Application((body), new hydra.core.Term.Variable(hydra.lib.lists.Head.apply((vars)))))))))))));
    java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>> padn = (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (n -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (body -> ((pad.get()).apply(((extraVariables)).apply((n)))).apply((body))));
    java.util.function.Function<hydra.core.Term, hydra.core.Term> unwind = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Type, hydra.core.Term>>) (e -> (java.util.function.Function<hydra.core.Type, hydra.core.Term>) (t -> new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((e), (t))))),
      (term2),
      (typeArgs)));
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T3, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.reduction.Reduction.etaExpandTypedTerm_recurseOrForce(
          (hydra_arity_typeArity2),
          (padn),
          (forced),
          (recurse),
          (tx),
          (unwind),
          (term));
      }
      
      @Override
      public hydra.compute.Flow<T3, hydra.core.Term> visit(hydra.core.Term.Application a) {
        hydra.core.Term lhs = (((a)).value).function;
        hydra.core.Term rhs = (((a)).value).argument;
        return hydra.lib.flows.Bind.apply(
          (((((((java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>>>) (v1 -> (java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>>) (v2 -> (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>) (v3 -> (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>) (v4 -> (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>) (v5 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>) (v6 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_rewrite(
            (hydra_arity_typeArity2),
            (hydra_arity_typeSchemeArity2),
            (hydra_schemas_extendTypeContextForLambda2),
            (hydra_schemas_extendTypeContextForLet2),
            (hydra_schemas_extendTypeContextForTypeLambda2),
            (v1),
            (v2),
            (v3),
            (v4),
            (v5),
            (v6))))))))).apply(true)).apply(false)).apply((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()))).apply((recurse))).apply((tx))).apply((rhs)),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>) (rhs2 -> hydra.lib.flows.Bind.apply(
            (((java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, Integer>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, Integer>>) (v2 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_arityOf(
              (hydra_arity_typeArity2),
              (hydra_arity_typeSchemeArity2),
              (hydra_schemas_extendTypeContextForLambda2),
              (hydra_schemas_extendTypeContextForLet2),
              (hydra_schemas_extendTypeContextForTypeLambda2),
              (v1),
              (v2))))).apply((tx))).apply((lhs)),
            (java.util.function.Function<Integer, hydra.compute.Flow<T3, hydra.core.Term>>) (lhsarity -> hydra.lib.flows.Bind.apply(
              hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_rewriteSpine(
                (hydra_arity_typeArity2),
                (hydra_arity_typeSchemeArity2),
                (hydra_schemas_extendTypeContextForLambda2),
                (hydra_schemas_extendTypeContextForLet2),
                (hydra_schemas_extendTypeContextForTypeLambda2),
                (recurse),
                (tx),
                (lhs)),
              (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>) (lhs2 -> {
                hydra.core.Term a2 = new hydra.core.Term.Application(new hydra.core.Application((lhs2), (rhs2)));
                return hydra.lib.flows.Pure.apply(hydra.lib.logic.IfElse.apply(
                  hydra.lib.equality.Gt.apply(
                    (lhsarity),
                    1),
                  (((padn)).apply(hydra.lib.math.Sub.apply(
                    (lhsarity),
                    1))).apply((a2)),
                  (a2)));
              }))))));
      }
      
      @Override
      public hydra.compute.Flow<T3, hydra.core.Term> visit(hydra.core.Term.Function f) {
        return (((f)).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T3, hydra.core.Term> otherwise(hydra.core.Function instance) {
            return hydra.reduction.Reduction.etaExpandTypedTerm_recurseOrForce(
              (hydra_arity_typeArity2),
              (padn),
              (forced),
              (recurse),
              (tx),
              (unwind),
              (term));
          }
          
          @Override
          public hydra.compute.Flow<T3, hydra.core.Term> visit(hydra.core.Function.Elimination elm) {
            return hydra.reduction.Reduction.etaExpandTypedTerm_forElimination(
              (java.util.function.Function<hydra.core.CaseStatement, hydra.compute.Flow<T3, hydra.core.Term>>) (v1 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_forCaseStatement(
                (hydra_arity_typeArity2),
                (hydra_arity_typeSchemeArity2),
                (hydra_schemas_extendTypeContextForLambda2),
                (hydra_schemas_extendTypeContextForLet2),
                (hydra_schemas_extendTypeContextForTypeLambda2),
                (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<T3, hydra.core.Field>>) (v12 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_forCase(
                  (hydra_arity_typeArity2),
                  (hydra_arity_typeSchemeArity2),
                  (hydra_schemas_extendTypeContextForLambda2),
                  (hydra_schemas_extendTypeContextForLet2),
                  (hydra_schemas_extendTypeContextForTypeLambda2),
                  (recurse),
                  (tx),
                  (v12))),
                (recurse),
                (tx),
                (v1))),
              (forced),
              (padn),
              (recurse),
              (term),
              (topLevel),
              (tx),
              (unwind),
              ((elm)).value);
          }
          
          @Override
          public hydra.compute.Flow<T3, hydra.core.Term> visit(hydra.core.Function.Lambda l) {
            hydra.typing.TypeContext txl = (((hydra_schemas_extendTypeContextForLambda2)).apply((tx))).apply(((l)).value);
            return hydra.lib.flows.Map.apply(
              (unwind),
              (((recurse)).apply((txl))).apply((term)));
          }
        });
      }
      
      @Override
      public hydra.compute.Flow<T3, hydra.core.Term> visit(hydra.core.Term.Let l) {
        hydra.typing.TypeContext txlt = ((((hydra_schemas_extendTypeContextForLet2)).apply((java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<T2>>>) (ignored -> (java.util.function.Function<T1, hydra.util.Maybe<T2>>) (_2 -> (hydra.util.Maybe<T2>) (hydra.util.Maybe.<T2>nothing()))))).apply((tx))).apply(((l)).value);
        return (((recurse)).apply((txlt))).apply((term));
      }
      
      @Override
      public hydra.compute.Flow<T3, hydra.core.Term> visit(hydra.core.Term.TypeApplication tat) {
        return (((((((java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>>>) (v1 -> (java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>>) (v2 -> (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>) (v3 -> (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>) (v4 -> (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>) (v5 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>) (v6 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_rewrite(
          (hydra_arity_typeArity2),
          (hydra_arity_typeSchemeArity2),
          (hydra_schemas_extendTypeContextForLambda2),
          (hydra_schemas_extendTypeContextForLet2),
          (hydra_schemas_extendTypeContextForTypeLambda2),
          (v1),
          (v2),
          (v3),
          (v4),
          (v5),
          (v6))))))))).apply((topLevel))).apply((forced))).apply(hydra.lib.lists.Cons.apply(
          (((tat)).value).type,
          (typeArgs)))).apply((recurse))).apply((tx))).apply((((tat)).value).body);
      }
      
      @Override
      public hydra.compute.Flow<T3, hydra.core.Term> visit(hydra.core.Term.TypeLambda tl) {
        hydra.typing.TypeContext txt = (((hydra_schemas_extendTypeContextForTypeLambda2)).apply((tx))).apply(((tl)).value);
        return (((recurse)).apply((txt))).apply((term));
      }
    });
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T3, hydra.core.Term> etaExpandTypedTerm_rewriteSpine(java.util.function.Function<hydra.core.Type, Integer> hydra_arity_typeArity2, java.util.function.Function<hydra.core.TypeScheme, Integer> hydra_arity_typeSchemeArity2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>> hydra_schemas_extendTypeContextForLambda2, java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<T2>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>> hydra_schemas_extendTypeContextForLet2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>> hydra_schemas_extendTypeContextForTypeLambda2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>> recurse, hydra.typing.TypeContext tx, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T3, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (((((((java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>>>) (v1 -> (java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>>) (v2 -> (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>) (v3 -> (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>) (v4 -> (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>) (v5 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>) (v6 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_rewrite(
          (hydra_arity_typeArity2),
          (hydra_arity_typeSchemeArity2),
          (hydra_schemas_extendTypeContextForLambda2),
          (hydra_schemas_extendTypeContextForLet2),
          (hydra_schemas_extendTypeContextForTypeLambda2),
          (v1),
          (v2),
          (v3),
          (v4),
          (v5),
          (v6))))))))).apply(false)).apply(false)).apply((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()))).apply((recurse))).apply((tx))).apply((term));
      }
      
      @Override
      public hydra.compute.Flow<T3, hydra.core.Term> visit(hydra.core.Term.Annotated at) {
        return hydra.lib.flows.Bind.apply(
          hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_rewriteSpine(
            (hydra_arity_typeArity2),
            (hydra_arity_typeSchemeArity2),
            (hydra_schemas_extendTypeContextForLambda2),
            (hydra_schemas_extendTypeContextForLet2),
            (hydra_schemas_extendTypeContextForTypeLambda2),
            (recurse),
            (tx),
            (((at)).value).body),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>) (body -> {
            java.util.Map<hydra.core.Name, hydra.core.Term> ann = (((at)).value).annotation;
            return hydra.lib.flows.Pure.apply(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm((body), (ann))));
          }));
      }
      
      @Override
      public hydra.compute.Flow<T3, hydra.core.Term> visit(hydra.core.Term.Application a) {
        java.util.List<hydra.core.Type> l = hydra.lib.logic.IfElse.apply(
          false,
          java.util.List.of(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_(true))),
          (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()));
        return hydra.lib.flows.Bind.apply(
          hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_rewriteSpine(
            (hydra_arity_typeArity2),
            (hydra_arity_typeSchemeArity2),
            (hydra_schemas_extendTypeContextForLambda2),
            (hydra_schemas_extendTypeContextForLet2),
            (hydra_schemas_extendTypeContextForTypeLambda2),
            (recurse),
            (tx),
            (((a)).value).function),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>) (lhs -> hydra.lib.flows.Bind.apply(
            (((((((java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>>>) (v1 -> (java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>>) (v2 -> (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>) (v3 -> (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>) (v4 -> (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>) (v5 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>) (v6 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_rewrite(
              (hydra_arity_typeArity2),
              (hydra_arity_typeSchemeArity2),
              (hydra_schemas_extendTypeContextForLambda2),
              (hydra_schemas_extendTypeContextForLet2),
              (hydra_schemas_extendTypeContextForTypeLambda2),
              (v1),
              (v2),
              (v3),
              (v4),
              (v5),
              (v6))))))))).apply(true)).apply(false)).apply((l))).apply((recurse))).apply((tx))).apply((((a)).value).argument),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>) (rhs -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application((lhs), (rhs))))))));
      }
      
      @Override
      public hydra.compute.Flow<T3, hydra.core.Term> visit(hydra.core.Term.TypeApplication tat) {
        return hydra.lib.flows.Bind.apply(
          hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_rewriteSpine(
            (hydra_arity_typeArity2),
            (hydra_arity_typeSchemeArity2),
            (hydra_schemas_extendTypeContextForLambda2),
            (hydra_schemas_extendTypeContextForLet2),
            (hydra_schemas_extendTypeContextForTypeLambda2),
            (recurse),
            (tx),
            (((tat)).value).body),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>) (body -> {
            hydra.core.Type typ = (((tat)).value).type;
            return hydra.lib.flows.Pure.apply(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((body), (typ))));
          }));
      }
    });
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T3, Integer> etaExpandTypedTerm_arityOf(java.util.function.Function<hydra.core.Type, Integer> hydra_arity_typeArity2, java.util.function.Function<hydra.core.TypeScheme, Integer> hydra_arity_typeSchemeArity2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>> hydra_schemas_extendTypeContextForLambda2, java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<T2>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>> hydra_schemas_extendTypeContextForLet2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>> hydra_schemas_extendTypeContextForTypeLambda2, hydra.typing.TypeContext tx, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T3, Integer> otherwise(hydra.core.Term instance) {
        return hydra.reduction.Reduction.<Integer, T3>etaExpandTypedTerm_dflt(
          (hydra_arity_typeArity2),
          (term),
          (tx));
      }
      
      @Override
      public hydra.compute.Flow<T3, Integer> visit(hydra.core.Term.Annotated at) {
        return (((java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, Integer>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, Integer>>) (v2 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_arityOf(
          (hydra_arity_typeArity2),
          (hydra_arity_typeSchemeArity2),
          (hydra_schemas_extendTypeContextForLambda2),
          (hydra_schemas_extendTypeContextForLet2),
          (hydra_schemas_extendTypeContextForTypeLambda2),
          (v1),
          (v2))))).apply((tx))).apply((((at)).value).body);
      }
      
      @Override
      public hydra.compute.Flow<T3, Integer> visit(hydra.core.Term.Function f) {
        return hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_forFunction(
          (hydra_arity_typeArity2),
          (hydra_schemas_extendTypeContextForLet2),
          (hydra_schemas_extendTypeContextForTypeLambda2),
          (hydra_arity_typeSchemeArity2),
          (hydra_schemas_extendTypeContextForLambda2),
          (tx),
          ((f)).value);
      }
      
      @Override
      public hydra.compute.Flow<T3, Integer> visit(hydra.core.Term.Let l) {
        hydra.typing.TypeContext txl = ((((hydra_schemas_extendTypeContextForLet2)).apply((java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<T2>>>) (ignored -> (java.util.function.Function<T1, hydra.util.Maybe<T2>>) (_2 -> (hydra.util.Maybe<T2>) (hydra.util.Maybe.<T2>nothing()))))).apply((tx))).apply(((l)).value);
        return (((java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, Integer>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, Integer>>) (v2 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_arityOf(
          (hydra_arity_typeArity2),
          (hydra_arity_typeSchemeArity2),
          (hydra_schemas_extendTypeContextForLambda2),
          (hydra_schemas_extendTypeContextForLet2),
          (hydra_schemas_extendTypeContextForTypeLambda2),
          (v1),
          (v2))))).apply((txl))).apply((((l)).value).body);
      }
      
      @Override
      public hydra.compute.Flow<T3, Integer> visit(hydra.core.Term.TypeApplication tat) {
        return (((java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, Integer>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, Integer>>) (v2 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_arityOf(
          (hydra_arity_typeArity2),
          (hydra_arity_typeSchemeArity2),
          (hydra_schemas_extendTypeContextForLambda2),
          (hydra_schemas_extendTypeContextForLet2),
          (hydra_schemas_extendTypeContextForTypeLambda2),
          (v1),
          (v2))))).apply((tx))).apply((((tat)).value).body);
      }
      
      @Override
      public hydra.compute.Flow<T3, Integer> visit(hydra.core.Term.TypeLambda tl) {
        hydra.typing.TypeContext txt = (((hydra_schemas_extendTypeContextForTypeLambda2)).apply((tx))).apply(((tl)).value);
        return (((java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, Integer>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, Integer>>) (v2 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_arityOf(
          (hydra_arity_typeArity2),
          (hydra_arity_typeSchemeArity2),
          (hydra_schemas_extendTypeContextForLambda2),
          (hydra_schemas_extendTypeContextForLet2),
          (hydra_schemas_extendTypeContextForTypeLambda2),
          (v1),
          (v2))))).apply((txt))).apply((((tl)).value).body);
      }
      
      @Override
      public hydra.compute.Flow<T3, Integer> visit(hydra.core.Term.Variable name) {
        return hydra.lib.maybes.Maybe.apply(
          hydra.lib.flows.Map.apply(
            (hydra_arity_typeArity2),
            hydra.checking.Checking.<T3>typeOf(
              (tx),
              (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
              new hydra.core.Term.Variable(((name)).value))),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T3, Integer>>) (t -> hydra.lib.flows.Pure.apply(((hydra_arity_typeArity2)).apply((t)))),
          hydra.lib.maps.Lookup.apply(
            ((name)).value,
            ((tx)).types));
      }
    });
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T3, T2> etaExpandTypedTerm_forceExpansion(java.util.function.Function<hydra.core.Type, T0> hydra_arity_typeArity2, java.util.function.Function<T0, java.util.function.Function<T1, T2>> padn, hydra.typing.TypeContext tx, java.util.function.Function<hydra.core.Term, T1> unwind, hydra.core.Term t) {
    return hydra.lib.flows.Bind.apply(
      hydra.checking.Checking.<T3>typeOf(
        (tx),
        (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
        (t)),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T3, T2>>) (typ -> {
        T0 arity = ((hydra_arity_typeArity2)).apply((typ));
        return hydra.lib.flows.Pure.apply((((padn)).apply((arity))).apply(((unwind)).apply((t))));
      }));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T3, T2> etaExpandTypedTerm_recurseOrForce(java.util.function.Function<hydra.core.Type, T0> hydra_arity_typeArity2, java.util.function.Function<T0, java.util.function.Function<T1, T2>> padn, Boolean forced, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T1, hydra.compute.Flow<T3, T2>>> recurse, hydra.typing.TypeContext tx, java.util.function.Function<hydra.core.Term, T1> unwind, hydra.core.Term term) {
    return hydra.lib.logic.IfElse.apply(
      (forced),
      ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, T2>>) (v1 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_forceExpansion(
        (hydra_arity_typeArity2),
        (padn),
        (tx),
        (unwind),
        (v1)))).apply((term)),
      (((recurse)).apply((tx))).apply(((unwind)).apply((term))));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T3, hydra.core.Field> etaExpandTypedTerm_forCase(java.util.function.Function<hydra.core.Type, Integer> hydra_arity_typeArity2, java.util.function.Function<hydra.core.TypeScheme, Integer> hydra_arity_typeSchemeArity2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>> hydra_schemas_extendTypeContextForLambda2, java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<T2>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>> hydra_schemas_extendTypeContextForLet2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>> hydra_schemas_extendTypeContextForTypeLambda2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>> recurse, hydra.typing.TypeContext tx, hydra.core.Field f) {
    return hydra.lib.flows.Bind.apply(
      (((((((java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>>>) (v1 -> (java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>>) (v2 -> (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>) (v3 -> (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>) (v4 -> (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>) (v5 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>) (v6 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_rewrite(
        (hydra_arity_typeArity2),
        (hydra_arity_typeSchemeArity2),
        (hydra_schemas_extendTypeContextForLambda2),
        (hydra_schemas_extendTypeContextForLet2),
        (hydra_schemas_extendTypeContextForTypeLambda2),
        (v1),
        (v2),
        (v3),
        (v4),
        (v5),
        (v6))))))))).apply(false)).apply(true)).apply((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()))).apply((recurse))).apply((tx))).apply(((f)).term),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Field>>) (r -> hydra.lib.flows.Pure.apply(new hydra.core.Field(((f)).name, (r)))));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T3, hydra.core.Term> etaExpandTypedTerm_forCaseStatement(java.util.function.Function<hydra.core.Type, Integer> hydra_arity_typeArity2, java.util.function.Function<hydra.core.TypeScheme, Integer> hydra_arity_typeSchemeArity2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>> hydra_schemas_extendTypeContextForLambda2, java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<T2>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>> hydra_schemas_extendTypeContextForLet2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>> hydra_schemas_extendTypeContextForTypeLambda2, java.util.function.Function<hydra.core.Field, hydra.compute.Flow<T3, hydra.core.Field>> forCase, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>> recurse, hydra.typing.TypeContext tx, hydra.core.CaseStatement cs) {
    java.util.List<hydra.core.Field> cases = ((cs)).cases;
    hydra.util.Maybe<hydra.core.Term> dflt = ((cs)).default_;
    hydra.core.Name tname = ((cs)).typeName;
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapMaybe.apply(
        ((((((java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>>>) (v1 -> (java.util.function.Function<Boolean, java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>>) (v2 -> (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>>) (v3 -> (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>>) (v4 -> (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>>) (v5 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, hydra.core.Term>>) (v6 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_rewrite(
          (hydra_arity_typeArity2),
          (hydra_arity_typeSchemeArity2),
          (hydra_schemas_extendTypeContextForLambda2),
          (hydra_schemas_extendTypeContextForLet2),
          (hydra_schemas_extendTypeContextForTypeLambda2),
          (v1),
          (v2),
          (v3),
          (v4),
          (v5),
          (v6))))))))).apply(false)).apply(false)).apply((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()))).apply((recurse))).apply((tx)),
        (dflt)),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.compute.Flow<T3, hydra.core.Term>>) (rdflt -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.MapList.apply(
          (forCase),
          (cases)),
        (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.compute.Flow<T3, hydra.core.Term>>) (rcases -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement((tname), (rdflt), (rcases))))))))));
  }
  
  static <T0, T1, T2, T3, T4> hydra.compute.Flow<T0, T2> etaExpandTypedTerm_forElimination(java.util.function.Function<hydra.core.CaseStatement, hydra.compute.Flow<T0, T1>> forCaseStatement, Boolean forced, java.util.function.Function<Integer, java.util.function.Function<T2, T2>> padn, java.util.function.Function<T3, java.util.function.Function<T4, hydra.compute.Flow<T0, T1>>> recurse, T4 term, Boolean topLevel, T3 tx, java.util.function.Function<T1, T2> unwind, hydra.core.Elimination elm) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.Map.apply(
        (unwind),
        hydra.reduction.Reduction.etaExpandTypedTerm_checkBase(
          (forCaseStatement),
          (recurse),
          (term),
          (tx),
          (elm))),
      (java.util.function.Function<T2, hydra.compute.Flow<T0, T2>>) (base -> hydra.lib.flows.Pure.apply(hydra.lib.logic.IfElse.apply(
        hydra.lib.logic.Or.apply(
          (topLevel),
          (forced)),
        (((padn)).apply(1)).apply((base)),
        (base)))));
  }
  
  static <T0, T1, T2> T0 etaExpandTypedTerm_checkBase(java.util.function.Function<hydra.core.CaseStatement, T0> forCaseStatement, java.util.function.Function<T1, java.util.function.Function<T2, T0>> recurse, T2 term, T1 tx, hydra.core.Elimination elm) {
    return ((elm)).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public T0 otherwise(hydra.core.Elimination instance) {
        return (((recurse)).apply((tx))).apply((term));
      }
      
      @Override
      public T0 visit(hydra.core.Elimination.Union cs) {
        return ((forCaseStatement)).apply(((cs)).value);
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T1, T0> etaExpandTypedTerm_dflt(java.util.function.Function<hydra.core.Type, T0> hydra_arity_typeArity2, hydra.core.Term term, hydra.typing.TypeContext tx) {
    return hydra.lib.flows.Map.apply(
      (hydra_arity_typeArity2),
      hydra.checking.Checking.<T1>typeOf(
        (tx),
        (java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()),
        (term)));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T3, Integer> etaExpandTypedTerm_forFunction(java.util.function.Function<hydra.core.Type, Integer> hydra_arity_typeArity2, java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<T2>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>> hydra_schemas_extendTypeContextForLet2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>> hydra_schemas_extendTypeContextForTypeLambda2, java.util.function.Function<hydra.core.TypeScheme, Integer> hydra_arity_typeSchemeArity2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>> hydra_schemas_extendTypeContextForLambda2, hydra.typing.TypeContext tx, hydra.core.Function f) {
    return ((f)).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T3, Integer> visit(hydra.core.Function.Elimination ignored) {
        return hydra.lib.flows.Pure.apply(1);
      }
      
      @Override
      public hydra.compute.Flow<T3, Integer> visit(hydra.core.Function.Lambda l) {
        hydra.typing.TypeContext txl = (((hydra_schemas_extendTypeContextForLambda2)).apply((tx))).apply(((l)).value);
        return (((java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, Integer>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T3, Integer>>) (v2 -> hydra.reduction.Reduction.<T0, T1, T2, T3>etaExpandTypedTerm_arityOf(
          (hydra_arity_typeArity2),
          (hydra_arity_typeSchemeArity2),
          (hydra_schemas_extendTypeContextForLambda2),
          (hydra_schemas_extendTypeContextForLet2),
          (hydra_schemas_extendTypeContextForTypeLambda2),
          (v1),
          (v2))))).apply((txl))).apply((((l)).value).body);
      }
      
      @Override
      public hydra.compute.Flow<T3, Integer> visit(hydra.core.Function.Primitive name) {
        return hydra.lib.flows.Map.apply(
          (hydra_arity_typeSchemeArity2),
          hydra.lexical.Lexical.<T3>requirePrimitiveType(
            (tx),
            ((name)).value));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> reduceTerm(Boolean eager, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Elimination, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>> applyElimination = (java.util.function.Function<hydra.core.Elimination, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) (elm -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (reducedArg -> ((elm)).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> visit(hydra.core.Elimination.Record proj) {
        return hydra.lib.flows.Bind.apply(
          hydra.extract.core.Core.record(
            (((proj)).value).typeName,
            hydra.rewriting.Rewriting.deannotateTerm((reducedArg))),
          (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (fields -> {
            java.util.List<hydra.core.Field> matchingFields = hydra.lib.lists.Filter.apply(
              (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                ((f)).name,
                (((proj)).value).field)),
              (fields));
            return hydra.lib.logic.IfElse.apply(
              hydra.lib.lists.Null.apply((matchingFields)),
              hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
                "no such field: ",
                ((((proj)).value).field).value,
                " in ",
                ((((proj)).value).typeName).value,
                " record"))),
              hydra.lib.flows.Pure.apply((hydra.lib.lists.Head.apply((matchingFields))).term));
          }));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> visit(hydra.core.Elimination.Union cs) {
        return hydra.lib.flows.Bind.apply(
          hydra.extract.core.Core.injection(
            (((cs)).value).typeName,
            (reducedArg)),
          (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (field -> {
            java.util.List<hydra.core.Field> matchingFields = hydra.lib.lists.Filter.apply(
              (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.equality.Equal.apply(
                ((f)).name,
                ((field)).name)),
              (((cs)).value).cases);
            return hydra.lib.logic.IfElse.apply(
              hydra.lib.lists.Null.apply((matchingFields)),
              hydra.lib.maybes.Maybe.apply(
                hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
                  "no such field ",
                  (((field)).name).value,
                  " in ",
                  ((((cs)).value).typeName).value,
                  " case statement"))),
                (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) ((hydra.lib.flows.Pure::apply))),
                (((cs)).value).default_),
              hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application((hydra.lib.lists.Head.apply((matchingFields))).term, ((field)).term))));
          }));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> visit(hydra.core.Elimination.Wrap name) {
        return hydra.extract.core.Core.wrap(
          ((name)).value,
          (reducedArg));
      }
    })));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>>> applyToArguments = new java.util.concurrent.atomic.AtomicReference<>();
    applyToArguments.set((java.util.function.Function<hydra.core.Term, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>>) (fun -> (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (args -> hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((args)),
      (fun),
      ((applyToArguments.get()).apply(new hydra.core.Term.Application(new hydra.core.Application((fun), hydra.lib.lists.Head.apply((args)))))).apply(hydra.lib.lists.Tail.apply((args)))))));
    java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>> reduce = (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) (eager2 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (v1 -> hydra.reduction.Reduction.reduceTerm(
      (eager2),
      (v1))));
    java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>> reduceArg = (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) (eager2 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (arg -> hydra.lib.logic.IfElse.apply(
      (eager2),
      hydra.lib.flows.Pure.apply((arg)),
      (((reduce)).apply(false)).apply((arg)))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>>> applyIfNullary = new java.util.concurrent.atomic.AtomicReference<>();
    applyIfNullary.set((java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>>) (eager2 -> (java.util.function.Function<hydra.core.Term, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) (original -> (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (args -> {
      java.util.function.Function<hydra.core.Elimination, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>> forElimination = (java.util.function.Function<hydra.core.Elimination, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) (elm -> (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (args2 -> {
        hydra.core.Term arg = hydra.lib.lists.Head.apply((args2));
        java.util.List<hydra.core.Term> remainingArgs = hydra.lib.lists.Tail.apply((args2));
        return hydra.lib.flows.Bind.apply(
          (((reduceArg)).apply((eager2))).apply(hydra.rewriting.Rewriting.deannotateTerm((arg))),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (reducedArg -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.Bind.apply(
              (((applyElimination)).apply((elm))).apply((reducedArg)),
              (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (v1 -> (((reduce)).apply((eager2))).apply((v1)))),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (reducedResult -> (((applyIfNullary.get()).apply((eager2))).apply((reducedResult))).apply((remainingArgs))))));
      }));
      java.util.function.Function<hydra.core.Lambda, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>> forLambda = (java.util.function.Function<hydra.core.Lambda, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) (l -> (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (args2 -> {
        hydra.core.Term arg = hydra.lib.lists.Head.apply((args2));
        hydra.core.Term body = ((l)).body;
        hydra.core.Name param = ((l)).parameter;
        java.util.List<hydra.core.Term> remainingArgs = hydra.lib.lists.Tail.apply((args2));
        return hydra.lib.flows.Bind.apply(
          (((reduce)).apply((eager2))).apply(hydra.rewriting.Rewriting.deannotateTerm((arg))),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (reducedArg -> hydra.lib.flows.Bind.apply(
            (((reduce)).apply((eager2))).apply(hydra.rewriting.Rewriting.replaceFreeTermVariable(
              (param),
              (reducedArg),
              (body))),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (reducedResult -> (((applyIfNullary.get()).apply((eager2))).apply((reducedResult))).apply((remainingArgs))))));
      }));
      java.util.function.Function<hydra.graph.Primitive, java.util.function.Function<Integer, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>> forPrimitive = (java.util.function.Function<hydra.graph.Primitive, java.util.function.Function<Integer, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>>) (prim -> (java.util.function.Function<Integer, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) (arity -> (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (args2 -> {
        java.util.List<hydra.core.Term> argList = hydra.lib.lists.Take.apply(
          (arity),
          (args2));
        java.util.List<hydra.core.Term> remainingArgs = hydra.lib.lists.Drop.apply(
          (arity),
          (args2));
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (v1 -> (((reduceArg)).apply((eager2))).apply((v1))),
            (argList)),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (reducedArgs -> {
            java.util.List<hydra.core.Term> strippedArgs = hydra.lib.lists.Map.apply(
              (hydra.rewriting.Rewriting::deannotateTerm),
              (reducedArgs));
            return hydra.lib.flows.Bind.apply(
              hydra.lib.flows.Bind.apply(
                (((prim)).implementation).apply((strippedArgs)),
                (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (v1 -> (((reduce)).apply((eager2))).apply((v1)))),
              (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (reducedResult -> (((applyIfNullary.get()).apply((eager2))).apply((reducedResult))).apply((remainingArgs))));
          }));
      })));
      hydra.core.Term stripped = hydra.rewriting.Rewriting.deannotateTerm((original));
      return ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return hydra.lib.flows.Pure.apply(((applyToArguments.get()).apply((original))).apply((args)));
        }
        
        @Override
        public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> visit(hydra.core.Term.Application app) {
          return (((applyIfNullary.get()).apply((eager2))).apply((((app)).value).function)).apply(hydra.lib.lists.Cons.apply(
            (((app)).value).argument,
            (args)));
        }
        
        @Override
        public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> visit(hydra.core.Term.Function v1) {
          return (((v1)).value).accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> visit(hydra.core.Function.Elimination elm) {
              return hydra.lib.logic.IfElse.apply(
                hydra.lib.lists.Null.apply((args)),
                hydra.lib.flows.Pure.apply((original)),
                (((forElimination)).apply(((elm)).value)).apply((args)));
            }
            
            @Override
            public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> visit(hydra.core.Function.Lambda l) {
              return hydra.lib.logic.IfElse.apply(
                hydra.lib.lists.Null.apply((args)),
                hydra.lib.flows.Pure.apply((original)),
                (((forLambda)).apply(((l)).value)).apply((args)));
            }
            
            @Override
            public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> visit(hydra.core.Function.Primitive name) {
              return hydra.lib.flows.Bind.apply(
                hydra.lexical.Lexical.requirePrimitive(((name)).value),
                (java.util.function.Function<hydra.graph.Primitive, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (prim -> {
                  Integer arity = hydra.arity.Arity.primitiveArity((prim));
                  return hydra.lib.logic.IfElse.apply(
                    hydra.lib.equality.Gt.apply(
                      (arity),
                      hydra.lib.lists.Length.apply((args))),
                    hydra.lib.flows.Pure.apply(((applyToArguments.get()).apply((original))).apply((args))),
                    ((((forPrimitive)).apply((prim))).apply((arity))).apply((args)));
                }));
            }
          });
        }
        
        @Override
        public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> visit(hydra.core.Term.Variable v) {
          return hydra.lib.flows.Bind.apply(
            hydra.lexical.Lexical.dereferenceElement(((v)).value),
            (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (mBinding -> hydra.lib.maybes.Maybe.apply(
              hydra.lib.flows.Pure.apply(((applyToArguments.get()).apply((original))).apply((args))),
              (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (binding -> (((applyIfNullary.get()).apply((eager2))).apply(((binding)).term)).apply((args))),
              (mBinding))));
        }
        
        @Override
        public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> visit(hydra.core.Term.Let lt) {
          java.util.List<hydra.core.Binding> bindings = (((lt)).value).bindings;
          hydra.core.Term body = (((lt)).value).body;
          java.util.function.Function<hydra.core.Binding, hydra.core.Term> letExpr = (java.util.function.Function<hydra.core.Binding, hydra.core.Term>) (b -> new hydra.core.Term.Let(new hydra.core.Let(java.util.List.of((b)), new hydra.core.Term.Variable(((b)).name))));
          java.util.function.Function<hydra.core.Binding, hydra.core.Binding> expandBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding(((b)).name, hydra.rewriting.Rewriting.replaceFreeTermVariable(
            ((b)).name,
            ((letExpr)).apply((b)),
            ((b)).term), ((b)).type));
          java.util.List<hydra.core.Binding> expandedBindings = hydra.lib.lists.Map.apply(
            (expandBinding),
            (bindings));
          java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Binding, hydra.core.Term>> substituteBinding = (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Binding, hydra.core.Term>>) (term2 -> (java.util.function.Function<hydra.core.Binding, hydra.core.Term>) (b -> hydra.rewriting.Rewriting.replaceFreeTermVariable(
            ((b)).name,
            ((b)).term,
            (term2))));
          java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> substituteAll = (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (bs -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> hydra.lib.lists.Foldl.apply(
            (substituteBinding),
            (term2),
            (bs))));
          hydra.core.Term expandedBody = (((substituteAll)).apply((expandedBindings))).apply((body));
          return hydra.lib.flows.Bind.apply(
            (((reduce)).apply((eager2))).apply((expandedBody)),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (reducedBody -> (((applyIfNullary.get()).apply((eager2))).apply((reducedBody))).apply((args))));
        }
      });
    }))));
    java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>> doRecurse = (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (eager2 -> (java.util.function.Function<hydra.core.Term, Boolean>) (term2 -> {
      java.util.function.Function<hydra.core.Function, Boolean> isNonLambda = (java.util.function.Function<hydra.core.Function, Boolean>) (f -> ((f)).accept(new hydra.core.Function.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Function instance) {
          return true;
        }
        
        @Override
        public Boolean visit(hydra.core.Function.Lambda ignored) {
          return false;
        }
      }));
      Boolean isNonLambdaTerm = ((term2)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Term instance) {
          return true;
        }
        
        @Override
        public Boolean visit(hydra.core.Term.Function f) {
          return ((isNonLambda)).apply(((f)).value);
        }
        
        @Override
        public Boolean visit(hydra.core.Term.Let ignored) {
          return false;
        }
      });
      return hydra.lib.logic.And.apply(
        (eager2),
        (isNonLambdaTerm));
    }));
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>> mapping = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (mid -> hydra.lib.flows.Bind.apply(
      hydra.lib.logic.IfElse.apply(
        (((doRecurse)).apply((eager))).apply((mid)),
        ((recurse)).apply((mid)),
        hydra.lib.flows.Pure.apply((mid))),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (inner -> (((applyIfNullary.get()).apply((eager))).apply((inner))).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))))));
    return hydra.rewriting.Rewriting.rewriteTermM(
      (mapping),
      (term));
  }
  
  static Boolean termIsClosed(hydra.core.Term term) {
    return hydra.lib.sets.Null.apply(hydra.rewriting.Rewriting.freeVariablesInTerm((term)));
  }
  
  static <T0> Boolean termIsValue(T0 g, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Field, Boolean> checkField = (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.reduction.Reduction.<T0>termIsValue(
      (g),
      ((f)).term));
    java.util.function.Function<java.util.List<hydra.core.Field>, Boolean> checkFields = (java.util.function.Function<java.util.List<hydra.core.Field>, Boolean>) (fields -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Field, Boolean>>) (b -> (java.util.function.Function<hydra.core.Field, Boolean>) (f -> hydra.lib.logic.And.apply(
        (b),
        ((checkField)).apply((f))))),
      true,
      (fields)));
    java.util.function.Function<java.util.List<hydra.core.Term>, Boolean> forList = (java.util.function.Function<java.util.List<hydra.core.Term>, Boolean>) (els -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (b -> (java.util.function.Function<hydra.core.Term, Boolean>) (t -> hydra.lib.logic.And.apply(
        (b),
        hydra.reduction.Reduction.<T0>termIsValue(
          (g),
          (t))))),
      true,
      (els)));
    java.util.function.Function<hydra.core.Function, Boolean> functionIsValue = (java.util.function.Function<hydra.core.Function, Boolean>) (f -> ((f)).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public Boolean visit(hydra.core.Function.Elimination e) {
        return (((e)).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
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
              ((checkFields)).apply((((cs)).value).cases),
              hydra.lib.maybes.Maybe.apply(
                true,
                (java.util.function.Function<hydra.core.Term, Boolean>) (v1 -> hydra.reduction.Reduction.<T0>termIsValue(
                  (g),
                  (v1))),
                (((cs)).value).default_));
          }
        });
      }
      
      @Override
      public Boolean visit(hydra.core.Function.Lambda l) {
        return hydra.reduction.Reduction.<T0>termIsValue(
          (g),
          (((l)).value).body);
      }
      
      @Override
      public Boolean visit(hydra.core.Function.Primitive ignored) {
        return true;
      }
    }));
    return (hydra.rewriting.Rewriting.deannotateTerm((term))).accept(new hydra.core.Term.PartialVisitor<>() {
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
          (java.util.function.Function<hydra.core.Term, Boolean>) (l -> hydra.reduction.Reduction.<T0>termIsValue(
            (g),
            (l))),
          (java.util.function.Function<hydra.core.Term, Boolean>) (r -> hydra.reduction.Reduction.<T0>termIsValue(
            (g),
            (r))),
          ((e)).value);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Literal ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Function f) {
        return ((functionIsValue)).apply(((f)).value);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.List els) {
        return ((forList)).apply(((els)).value);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Map m) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Boolean, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, Boolean>>) (b -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, Boolean>) (kv -> hydra.lib.logic.And.apply(
            (b),
            hydra.lib.logic.And.apply(
              hydra.reduction.Reduction.<T0>termIsValue(
                (g),
                hydra.lib.pairs.First.apply((kv))),
              hydra.reduction.Reduction.<T0>termIsValue(
                (g),
                hydra.lib.pairs.Second.apply((kv))))))),
          true,
          hydra.lib.maps.ToList.apply(((m)).value));
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Maybe.apply(
          true,
          (java.util.function.Function<hydra.core.Term, Boolean>) (v1 -> hydra.reduction.Reduction.<T0>termIsValue(
            (g),
            (v1))),
          ((m)).value);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Record r) {
        return ((checkFields)).apply((((r)).value).fields);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Set s) {
        return ((forList)).apply(hydra.lib.sets.ToList.apply(((s)).value));
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Union i) {
        return ((checkField)).apply((((i)).value).field);
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
