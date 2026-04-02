// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Free variable analysis, term-level substitution, and unshadowing
 */
public interface Variables {
  static hydra.util.PersistentSet<hydra.core.Name> freeTypeVariablesInTerm(hydra.core.Term term0) {
    java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>> tryType = (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>>) (tvars -> (java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>) (typ -> hydra.lib.sets.Difference.apply(
      hydra.Variables.freeVariablesInType(typ),
      tvars)));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.util.PersistentSet<hydra.core.Name>>>> getAll = new java.util.concurrent.atomic.AtomicReference<>();
    getAll.set((java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.util.PersistentSet<hydra.core.Name>>>) (vars -> (java.util.function.Function<hydra.core.Term, hydra.util.PersistentSet<hydra.core.Name>>) (term -> {
      java.util.function.Function<hydra.core.Term, hydra.util.PersistentSet<hydra.core.Name>> recurse = (java.util.function.Function<hydra.core.Term, hydra.util.PersistentSet<hydra.core.Name>>) (v1 -> getAll.get().apply(vars).apply(v1));
      hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> dflt = new hydra.util.Lazy<>(() -> hydra.Variables.freeTypeVariablesInTerm_allOf(hydra.lib.lists.Map.apply(
        recurse,
        hydra.Rewriting.subterms(term))));
      return (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.PersistentSet<hydra.core.Name> otherwise(hydra.core.Term instance) {
          return dflt.get();
        }

        @Override
        public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Term.Function f) {
          return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.util.PersistentSet<hydra.core.Name> otherwise(hydra.core.Function instance) {
              return dflt.get();
            }

            @Override
            public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Function.Elimination e) {
              return dflt.get();
            }

            @Override
            public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Function.Lambda l) {
              hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> domt = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
                () -> (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                (java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>) (v1 -> (tryType).apply(vars).apply(v1)),
                (l).value.domain));
              return hydra.lib.sets.Union.apply(
                domt.get(),
                (recurse).apply((l).value.body));
            }
          });
        }

        @Override
        public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Term.Let l) {
          java.util.function.Function<hydra.core.Binding, hydra.util.PersistentSet<hydra.core.Name>> forBinding = (java.util.function.Function<hydra.core.Binding, hydra.util.PersistentSet<hydra.core.Name>>) (b -> {
            hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> newVars = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
              () -> vars,
              (java.util.function.Function<hydra.core.TypeScheme, hydra.util.PersistentSet<hydra.core.Name>>) (ts -> hydra.lib.sets.Union.apply(
                vars,
                hydra.lib.sets.FromList.apply((ts).variables))),
              (b).type));
            return hydra.lib.sets.Union.apply(
              getAll.get().apply(newVars.get()).apply((b).term),
              hydra.lib.maybes.Maybe.applyLazy(
                () -> (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
                (java.util.function.Function<hydra.core.TypeScheme, hydra.util.PersistentSet<hydra.core.Name>>) (ts -> (tryType).apply(newVars.get()).apply((ts).type)),
                (b).type));
          });
          return hydra.lib.sets.Union.apply(
            hydra.Variables.freeTypeVariablesInTerm_allOf(hydra.lib.lists.Map.apply(
              forBinding,
              (l).value.bindings)),
            (recurse).apply((l).value.body));
        }

        @Override
        public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Term.TypeApplication tt) {
          return hydra.lib.sets.Union.apply(
            (tryType).apply(vars).apply((tt).value.type),
            (recurse).apply((tt).value.body));
        }

        @Override
        public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Term.TypeLambda tl) {
          return hydra.lib.sets.Union.apply(
            (tryType).apply(vars).apply(new hydra.core.Type.Variable((tl).value.parameter)),
            (recurse).apply((tl).value.body));
        }
      });
    })));
    return getAll.get().apply((hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())).apply(term0);
  }

  static <T0> hydra.util.PersistentSet<T0> freeTypeVariablesInTerm_allOf(hydra.util.ConsList<hydra.util.PersistentSet<T0>> sets) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.PersistentSet<T0>, java.util.function.Function<hydra.util.PersistentSet<T0>, hydra.util.PersistentSet<T0>>>) ((java.util.function.Function<hydra.util.PersistentSet<T0>, java.util.function.Function<hydra.util.PersistentSet<T0>, hydra.util.PersistentSet<T0>>>) (p0 -> p1 -> hydra.lib.sets.Union.apply(
        p0,
        p1))),
      (hydra.util.PersistentSet<T0>) (hydra.lib.sets.Empty.<T0>apply()),
      sets);
  }

  static hydra.util.PersistentSet<hydra.core.Name> freeVariablesInTerm(hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.PersistentSet<hydra.core.Name> otherwise(hydra.core.Term instance) {
        return hydra.Variables.freeVariablesInTerm_dfltVars(
          hydra.Rewriting::subterms,
          hydra.Variables::freeVariablesInTerm,
          term,
          null);
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Term.Function v1) {
        return (v1).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.PersistentSet<hydra.core.Name> otherwise(hydra.core.Function instance) {
            return hydra.Variables.freeVariablesInTerm_dfltVars(
              hydra.Rewriting::subterms,
              hydra.Variables::freeVariablesInTerm,
              term,
              null);
          }

          @Override
          public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Function.Lambda l) {
            return hydra.lib.sets.Delete.apply(
              (l).value.parameter,
              hydra.Variables.freeVariablesInTerm((l).value.body));
          }
        });
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Term.Let l) {
        return hydra.lib.sets.Difference.apply(
          hydra.Variables.freeVariablesInTerm_dfltVars(
            hydra.Rewriting::subterms,
            hydra.Variables::freeVariablesInTerm,
            term,
            null),
          hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
            projected -> projected.name,
            (l).value.bindings)));
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Term.Variable v) {
        return hydra.lib.sets.Singleton.apply((v).value);
      }
    });
  }

  static <T0> hydra.util.PersistentSet<hydra.core.Name> freeVariablesInTerm_dfltVars(java.util.function.Function<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>> hydra_rewriting_subterms2, java.util.function.Function<hydra.core.Term, hydra.util.PersistentSet<hydra.core.Name>> hydra_variables_freeVariablesInTerm2, hydra.core.Term term, T0 ignored) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.util.PersistentSet<hydra.core.Name>>>) (s -> (java.util.function.Function<hydra.core.Term, hydra.util.PersistentSet<hydra.core.Name>>) (t -> hydra.lib.sets.Union.apply(
        s,
        (hydra_variables_freeVariablesInTerm2).apply(t)))),
      (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      (hydra_rewriting_subterms2).apply(term));
  }

  static hydra.util.PersistentSet<hydra.core.Name> freeVariablesInType(hydra.core.Type typ) {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> dfltVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>>) (s -> (java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>) (t -> hydra.lib.sets.Union.apply(
        s,
        hydra.Variables.freeVariablesInType(t)))),
      (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      hydra.Rewriting.subtypes(typ)));
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.PersistentSet<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return dfltVars.get();
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Forall lt) {
        return hydra.lib.sets.Delete.apply(
          (lt).value.parameter,
          hydra.Variables.freeVariablesInType((lt).value.body));
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Variable v) {
        return hydra.lib.sets.Singleton.apply((v).value);
      }
    });
  }

  static hydra.util.ConsList<hydra.core.Name> freeVariablesInTypeOrdered(hydra.core.Type typ) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Name>>>> collectVars = new java.util.concurrent.atomic.AtomicReference<>();
    collectVars.set((java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Name>>>) (boundVars -> (java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Name>>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Name>>) (v1 -> collectVars.get().apply(boundVars).apply(v1)),
          hydra.Rewriting.subtypes(t)));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Variable v) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Member.apply(
            (v).value,
            boundVars),
          () -> (hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()),
          () -> hydra.util.ConsList.of((v).value));
      }

      @Override
      public hydra.util.ConsList<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return collectVars.get().apply(hydra.lib.sets.Insert.apply(
          (ft).value.parameter,
          boundVars)).apply((ft).value.body);
      }
    }))));
    return hydra.lib.lists.Nub.apply(collectVars.get().apply((hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())).apply(typ));
  }

  static hydra.util.PersistentSet<hydra.core.Name> freeVariablesInTypeScheme(hydra.core.TypeScheme ts) {
    hydra.core.Type t = (ts).type;
    hydra.util.ConsList<hydra.core.Name> vars = (ts).variables;
    return hydra.lib.sets.Difference.apply(
      hydra.Variables.freeVariablesInType(t),
      hydra.lib.sets.FromList.apply(vars));
  }

  static hydra.util.PersistentSet<hydra.core.Name> freeVariablesInTypeSchemeSimple(hydra.core.TypeScheme ts) {
    hydra.core.Type t = (ts).type;
    hydra.util.ConsList<hydra.core.Name> vars = (ts).variables;
    return hydra.lib.sets.Difference.apply(
      hydra.Variables.freeVariablesInTypeSimple(t),
      hydra.lib.sets.FromList.apply(vars));
  }

  static hydra.util.PersistentSet<hydra.core.Name> freeVariablesInTypeSimple(hydra.core.Type typ) {
    java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>> helper = (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>>) (types -> (java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>) (typ2 -> (typ2).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.PersistentSet<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return types;
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.core.Type.Variable v) {
        return hydra.lib.sets.Insert.apply(
          (v).value,
          types);
      }
    })));
    return hydra.Rewriting.foldOverType(
      new hydra.coders.TraversalOrder.Pre(),
      helper,
      (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      typ);
  }

  static Boolean isFreeVariableInTerm(hydra.core.Name v, hydra.core.Term term) {
    return hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
      v,
      hydra.Variables.freeVariablesInTerm(term)));
  }

  static hydra.core.Term normalizeTypeVariablesInTerm(hydra.core.Term term) {
    java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.Type>> substType = (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (subst -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> {
      java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ2 -> (typ2).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.core.Type otherwise(hydra.core.Type instance) {
          return (recurse).apply(typ2);
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Variable v) {
          return new hydra.core.Type.Variable(hydra.Variables.normalizeTypeVariablesInTerm_replaceName(
            subst,
            (v).value));
        }
      })));
      return hydra.Rewriting.rewriteType(
        rewrite,
        typ);
    }));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> rewriteWithSubst = new java.util.concurrent.atomic.AtomicReference<>();
    rewriteWithSubst.set((java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (state -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term0 -> {
      hydra.util.Lazy<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>> sb = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(state));
      hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> boundVars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(sb.get()));
      hydra.util.Lazy<Integer> next = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(state));
      hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> subst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(sb.get()));
      java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> (term2).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return (recurse).apply(term2);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Function v1) {
          return (v1).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.core.Term otherwise(hydra.core.Function instance) {
              return (recurse).apply(term2);
            }

            @Override
            public hydra.core.Term visit(hydra.core.Function.Elimination ignored) {
              return (recurse).apply(term2);
            }

            @Override
            public hydra.core.Term visit(hydra.core.Function.Lambda l) {
              hydra.util.Maybe<hydra.core.Type> domain = (l).value.domain;
              return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((l).value.parameter, hydra.lib.maybes.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (v12 -> (substType).apply(subst.get()).apply(v12)),
                domain), rewriteWithSubst.get().apply((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) ((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) (new hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>(subst.get(), boundVars.get()))), next.get())))).apply((l).value.body))));
            }
          });
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Let lt) {
          hydra.util.ConsList<hydra.core.Binding> bindings0 = (lt).value.bindings;
          java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.ConsList<hydra.core.Binding>, java.util.function.Function<hydra.util.ConsList<hydra.core.Binding>, hydra.util.ConsList<hydra.core.Binding>>>> step = new java.util.concurrent.atomic.AtomicReference<>();
          step.set((java.util.function.Function<hydra.util.ConsList<hydra.core.Binding>, java.util.function.Function<hydra.util.ConsList<hydra.core.Binding>, hydra.util.ConsList<hydra.core.Binding>>>) (acc -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Binding>, hydra.util.ConsList<hydra.core.Binding>>) (bs -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(bs),
            () -> hydra.lib.lists.Reverse.apply(acc),
            () -> ((java.util.function.Supplier<hydra.util.ConsList<hydra.core.Binding>>) (() -> {
              hydra.util.Lazy<hydra.core.Binding> b = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(bs));
              return ((java.util.function.Supplier<hydra.util.ConsList<hydra.core.Binding>>) (() -> {
                hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> tl = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(bs));
                return ((java.util.function.Supplier<hydra.util.ConsList<hydra.core.Binding>>) (() -> {
                  hydra.util.Lazy<hydra.core.Term> newVal = new hydra.util.Lazy<>(() -> rewriteWithSubst.get().apply((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) ((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) (new hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>(subst.get(), boundVars.get()))), next.get())))).apply(b.get().term));
                  hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> noType = new hydra.util.Lazy<>(() -> ((java.util.function.Supplier<hydra.util.ConsList<hydra.core.Binding>>) (() -> {
                    hydra.util.Lazy<hydra.core.Binding> b1 = new hydra.util.Lazy<>(() -> new hydra.core.Binding(b.get().name, newVal.get(), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
                    return step.get().apply(hydra.lib.lists.Cons.apply(
                      b1.get(),
                      acc)).apply(tl.get());
                  })).get());
                  return ((java.util.function.Supplier<hydra.util.ConsList<hydra.core.Binding>>) (() -> {
                    java.util.function.Function<hydra.core.TypeScheme, hydra.util.ConsList<hydra.core.Binding>> withType = (java.util.function.Function<hydra.core.TypeScheme, hydra.util.ConsList<hydra.core.Binding>>) (ts -> {
                      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Integer, java.util.function.Function<Integer, java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.ConsList<hydra.core.Name>>>>> gen = new java.util.concurrent.atomic.AtomicReference<>();
                      gen.set((java.util.function.Function<Integer, java.util.function.Function<Integer, java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.ConsList<hydra.core.Name>>>>) (i -> (java.util.function.Function<Integer, java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.ConsList<hydra.core.Name>>>) (rem -> (java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.ConsList<hydra.core.Name>>) (acc2 -> {
                        hydra.core.Name ti = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
                          "t",
                          hydra.lib.literals.ShowInt32.apply(hydra.lib.math.Add.apply(
                            next.get(),
                            i))));
                        return hydra.lib.logic.IfElse.lazy(
                          hydra.lib.equality.Equal.apply(
                            rem,
                            0),
                          () -> hydra.lib.lists.Reverse.apply(acc2),
                          () -> gen.get().apply(hydra.lib.math.Add.apply(
                            i,
                            1)).apply(hydra.lib.math.Sub.apply(
                            rem,
                            1)).apply(hydra.lib.lists.Cons.apply(
                            ti,
                            acc2)));
                      }))));
                      hydra.util.ConsList<hydra.core.Name> vars = (ts).variables;
                      hydra.util.Lazy<Integer> k = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(vars));
                      hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> newVars = new hydra.util.Lazy<>(() -> gen.get().apply(0).apply(k.get()).apply((hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty())));
                      hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> newSubst = new hydra.util.Lazy<>(() -> hydra.lib.maps.Union.apply(
                        hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
                          vars,
                          newVars.get())),
                        subst.get()));
                      hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>> oldConstraints = (ts).constraints;
                      hydra.util.Lazy<hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>> newConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
                        (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (v1 -> hydra.Variables.normalizeTypeVariablesInTerm_renameConstraintKeys(
                          newSubst.get(),
                          v1)),
                        oldConstraints));
                      hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> newBound = new hydra.util.Lazy<>(() -> hydra.lib.sets.Union.apply(
                        boundVars.get(),
                        hydra.lib.sets.FromList.apply(newVars.get())));
                      hydra.util.Lazy<hydra.core.Term> newVal2 = new hydra.util.Lazy<>(() -> rewriteWithSubst.get().apply((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) ((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) (new hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>(newSubst.get(), newBound.get()))), hydra.lib.math.Add.apply(
                        next.get(),
                        k.get()))))).apply(b.get().term));
                      hydra.core.Type typ = (ts).type;
                      hydra.core.Binding b1 = new hydra.core.Binding(b.get().name, newVal2.get(), hydra.util.Maybe.just(new hydra.core.TypeScheme(newVars.get(), (substType).apply(newSubst.get()).apply(typ), newConstraints.get())));
                      return step.get().apply(hydra.lib.lists.Cons.apply(
                        b1,
                        acc)).apply(tl.get());
                    });
                    return hydra.lib.maybes.Maybe.applyLazy(
                      () -> noType.get(),
                      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.ConsList<hydra.core.Binding>>) (ts -> (withType).apply(ts)),
                      b.get().type);
                  })).get();
                })).get();
              })).get();
            })).get()))));
          hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> bindings1 = new hydra.util.Lazy<>(() -> step.get().apply((hydra.util.ConsList<hydra.core.Binding>) (hydra.util.ConsList.<hydra.core.Binding>empty())).apply(bindings0));
          hydra.core.Term body0 = (lt).value.body;
          return new hydra.core.Term.Let(new hydra.core.Let(bindings1.get(), rewriteWithSubst.get().apply((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) ((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) (new hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>(subst.get(), boundVars.get()))), next.get())))).apply(body0)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
          return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(rewriteWithSubst.get().apply((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) ((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) (new hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>(subst.get(), boundVars.get()))), next.get())))).apply((tt).value.body), (substType).apply(subst.get()).apply((tt).value.type)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
          return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(hydra.Variables.normalizeTypeVariablesInTerm_replaceName(
            subst.get(),
            (ta).value.parameter), rewriteWithSubst.get().apply((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) ((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) (new hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>(subst.get(), boundVars.get()))), next.get())))).apply((ta).value.body)));
        }
      })));
      return hydra.Rewriting.rewriteTerm(
        rewrite,
        term0);
    })));
    return rewriteWithSubst.get().apply((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>, Integer>((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) ((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>) (new hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())), (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())))), 0)))).apply(term);
  }

  static <T0> T0 normalizeTypeVariablesInTerm_meta(hydra.util.Pair<hydra.core.Name, T0> p) {
    return hydra.lib.pairs.Second.apply(p);
  }

  static <T0> hydra.util.PersistentMap<hydra.core.Name, T0> normalizeTypeVariablesInTerm_renameConstraintKeys(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> newSubst, hydra.util.PersistentMap<hydra.core.Name, T0> constraintMap) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.core.Name, T0>, hydra.util.Pair<hydra.core.Name, T0>>) (p -> {
        hydra.util.Lazy<hydra.core.Name> oldName = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
        hydra.util.Lazy<hydra.core.Name> newName = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
          () -> oldName.get(),
          hydra.lib.maps.Lookup.apply(
            oldName.get(),
            newSubst)));
        return (hydra.util.Pair<hydra.core.Name, T0>) ((hydra.util.Pair<hydra.core.Name, T0>) (new hydra.util.Pair<hydra.core.Name, T0>(newName.get(), hydra.Variables.<T0>normalizeTypeVariablesInTerm_meta(p))));
      }),
      hydra.lib.maps.ToList.apply(constraintMap)));
  }

  static <T0> T0 normalizeTypeVariablesInTerm_replaceName(hydra.util.PersistentMap<T0, T0> subst, T0 v) {
    return hydra.lib.maybes.FromMaybe.applyLazy(
      () -> v,
      hydra.lib.maps.Lookup.apply(
        v,
        subst));
  }

  static hydra.core.Term replaceFreeTermVariable(hydra.core.Name vold, hydra.core.Term tnew, hydra.core.Term term) {
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (recurse).apply(t);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return (recurse).apply(t);
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            hydra.core.Name v = (l).value.parameter;
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                v,
                vold),
              () -> t,
              () -> (recurse).apply(t));
          }
        });
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable v) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (v).value,
            vold),
          () -> tnew,
          () -> new hydra.core.Term.Variable((v).value));
      }
    })));
    return hydra.Rewriting.rewriteTerm(
      rewrite,
      term);
  }

  static hydra.core.Type replaceFreeTypeVariable(hydra.core.Name v, hydra.core.Type rep, hydra.core.Type typ) {
    java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>> mapExpr = (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (recurse).apply(t);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            v,
            (ft).value.parameter),
          () -> t,
          () -> new hydra.core.Type.Forall(new hydra.core.ForallType((ft).value.parameter, (recurse).apply((ft).value.body))));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable v_) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            v,
            (v_).value),
          () -> rep,
          () -> t);
      }
    })));
    return hydra.Rewriting.rewriteType(
      mapExpr,
      typ);
  }

  static hydra.core.Type substituteTypeVariables(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> subst, hydra.core.Type typ) {
    java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>> replace = (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ2 -> (typ2).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (recurse).apply(typ2);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable n) {
        return new hydra.core.Type.Variable(hydra.lib.maybes.FromMaybe.applyLazy(
          () -> (n).value,
          hydra.lib.maps.Lookup.apply(
            (n).value,
            subst)));
      }
    })));
    return hydra.Rewriting.rewriteType(
      replace,
      typ);
  }

  static hydra.core.Term substituteTypeVariablesInTerm(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> subst, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Type, hydra.core.Type> st = (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (v1 -> hydra.Variables.substituteTypeVariables(
      subst,
      v1));
    java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>> stOpt = (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Maybe<hydra.core.Type>>) (mt -> hydra.lib.maybes.Map.apply(
      st,
      mt));
    java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme> stScheme = (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (ts -> new hydra.core.TypeScheme((ts).variables, (st).apply((ts).type), (ts).constraints));
    java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.util.Maybe<hydra.core.TypeScheme>> stSchemeOpt = (java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.util.Maybe<hydra.core.TypeScheme>>) (mts -> hydra.lib.maybes.Map.apply(
      stScheme,
      mts));
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> replace = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (recurse).apply(t);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function v1) {
        return (v1).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return (recurse).apply(t);
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((l).value.parameter, (stOpt).apply((l).value.domain), (recurse).apply((l).value.body))));
          }
        });
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        java.util.function.Function<hydra.core.Binding, hydra.core.Binding> mapBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, (recurse).apply((b).term), (stSchemeOpt).apply((b).type)));
        return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
          mapBinding,
          (lt).value.bindings), (recurse).apply((lt).value.body)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
        return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((recurse).apply((tt).value.body), (st).apply((tt).value.type)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(hydra.lib.maybes.FromMaybe.applyLazy(
          () -> (tl).value.parameter,
          hydra.lib.maps.Lookup.apply(
            (tl).value.parameter,
            subst)), (recurse).apply((tl).value.body)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm((recurse).apply((at).value.body), (at).value.annotation));
      }
    })));
    return hydra.Rewriting.rewriteTerm(
      replace,
      term);
  }

  static hydra.core.Term substituteVariable(hydra.core.Name from, hydra.core.Name to, hydra.core.Term term) {
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> replace = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> (term2).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (recurse).apply(term2);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable x) {
        return new hydra.core.Term.Variable(hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (x).value,
            from),
          () -> to,
          () -> (x).value));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function v1) {
        return (v1).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return (recurse).apply(term2);
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                (l).value.parameter,
                from),
              () -> term2,
              () -> (recurse).apply(term2));
          }
        });
      }
    })));
    return hydra.Rewriting.rewriteTerm(
      replace,
      term);
  }

  static hydra.core.Term substituteVariables(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> subst, hydra.core.Term term) {
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> replace = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term2 -> (term2).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (recurse).apply(term2);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable n) {
        return new hydra.core.Term.Variable(hydra.lib.maybes.FromMaybe.applyLazy(
          () -> (n).value,
          hydra.lib.maps.Lookup.apply(
            (n).value,
            subst)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function v1) {
        return (v1).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return (recurse).apply(term2);
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            return hydra.lib.maybes.Maybe.applyLazy(
              () -> (recurse).apply(term2),
              (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (ignored -> term2),
              hydra.lib.maps.Lookup.apply(
                (l).value.parameter,
                subst));
          }
        });
      }
    })));
    return hydra.Rewriting.rewriteTerm(
      replace,
      term);
  }

  static hydra.core.Term unshadowVariables(hydra.core.Term term0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>> f = new java.util.concurrent.atomic.AtomicReference<>();
    f.set((java.util.function.Function<java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>>) (recurse -> (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (m -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (recurse).apply(m).apply(term);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Function fn) {
        return (fn).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term otherwise(hydra.core.Function instance) {
            return (recurse).apply(m).apply(term);
          }

          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            hydra.core.Term body = (l).value.body;
            hydra.util.Maybe<hydra.core.Type> domain = (l).value.domain;
            hydra.core.Name v = (l).value.parameter;
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.maps.Member.apply(
                v,
                m),
              () -> ((java.util.function.Supplier<hydra.core.Term>) (() -> {
                hydra.util.Lazy<hydra.core.Name> v2 = new hydra.util.Lazy<>(() -> hydra.Variables.unshadowVariables_freshName(
                  v,
                  2,
                  m));
                return ((java.util.function.Supplier<hydra.core.Term>) (() -> {
                  hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> m2 = new hydra.util.Lazy<>(() -> hydra.lib.maps.Insert.apply(
                    v,
                    v2.get(),
                    hydra.lib.maps.Insert.apply(
                      v2.get(),
                      v2.get(),
                      m)));
                  return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(v2.get(), domain, f.get().apply(recurse).apply(m2.get()).apply(body))));
                })).get();
              })).get(),
              () -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(v, domain, f.get().apply(recurse).apply(hydra.lib.maps.Insert.apply(
                v,
                v,
                m)).apply(body)))));
          }
        });
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> m2 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Binding, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>>) (acc -> (java.util.function.Function<hydra.core.Binding, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (b -> {
            hydra.core.Name bname = (b).name;
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.maps.Member.apply(
                bname,
                acc),
              () -> acc,
              () -> hydra.lib.maps.Insert.apply(
                bname,
                bname,
                acc));
          })),
          m,
          (lt).value.bindings));
        return (recurse).apply(m2.get()).apply(term);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable v) {
        return new hydra.core.Term.Variable(hydra.lib.maybes.Maybe.applyLazy(
          () -> (v).value,
          (java.util.function.Function<hydra.core.Name, hydra.core.Name>) (renamed -> renamed),
          hydra.lib.maps.Lookup.apply(
            (v).value,
            m)));
      }
    })))));
    return hydra.Rewriting.rewriteTermWithContext(
      f.get(),
      (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
      term0);
  }

  static <T0> hydra.core.Name unshadowVariables_freshName(hydra.core.Name base, Integer i, hydra.util.PersistentMap<hydra.core.Name, T0> m) {
    hydra.core.Name candidate = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      (base).value,
      hydra.lib.literals.ShowInt32.apply(i)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Member.apply(
        candidate,
        m),
      () -> hydra.Variables.<T0>unshadowVariables_freshName(
        base,
        hydra.lib.math.Add.apply(
          i,
          1),
        m),
      () -> candidate);
  }
}
