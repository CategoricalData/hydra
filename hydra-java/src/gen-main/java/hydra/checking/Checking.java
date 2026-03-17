// Note: this is an automatically generated file. Do not edit.

package hydra.checking;

/**
 * Type checking and type reconstruction (type-of) for the results of Hydra unification and inference
 */
public interface Checking {
  static <T0> Boolean allEqual(hydra.util.ConsList<T0> els) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(els),
      () -> true,
      () -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<Boolean, java.util.function.Function<T0, Boolean>>) (b -> (java.util.function.Function<T0, Boolean>) (t -> hydra.lib.logic.And.apply(
          b,
          hydra.lib.equality.Equal.apply(
            t,
            hydra.lib.lists.Head.apply(els))))),
        true,
        hydra.lib.lists.Tail.apply(els)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> applyTypeArgumentsToType(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.Type t) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(typeArgs),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>right(t),
      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>>) (() -> {
        hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>> nonnull = new hydra.util.Lazy<>(() -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> otherwise(hydra.core.Type instance) {
            return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "not a forall type: ",
              hydra.show.core.Core.type(t),
              ". Trying to apply ",
              hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Length.apply(typeArgs)),
              " type args: ",
              hydra.formatting.Formatting.showList(
                hydra.show.core.Core::type,
                typeArgs),
              ". Context has vars: {",
              hydra.lib.strings.Intercalate.apply(
                ", ",
                hydra.lib.lists.Map.apply(
                  wrapped -> (wrapped).value,
                  hydra.lib.maps.Keys.apply((tx).boundTypes))),
              "}")))), cx)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> visit(hydra.core.Type.Forall ft) {
            hydra.core.Type tbody = (ft).value.body;
            hydra.core.Name v = (ft).value.parameter;
            return hydra.checking.Checking.applyTypeArgumentsToType(
              cx,
              tx,
              hydra.lib.lists.Tail.apply(typeArgs),
              hydra.substitution.Substitution.substInType(
                new hydra.typing.TypeSubst(hydra.lib.maps.Singleton.apply(
                  v,
                  hydra.lib.lists.Head.apply(typeArgs))),
                tbody));
          }
        }));
        return nonnull.get();
      })).get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> checkForUnboundTypeVariables(hydra.context.Context cx, hydra.graph.Graph tx, hydra.core.Term term0) {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> svars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((tx).schemaTypes)));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.util.ConsList<String>, java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>>>>> checkRecursive = new java.util.concurrent.atomic.AtomicReference<>();
    checkRecursive.set((java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.util.ConsList<String>, java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>>>>) (vars -> (java.util.function.Function<hydra.util.ConsList<String>, java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>>>) (trace -> (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>>) (lbinding -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>) (term -> {
      java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>> check = (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>) (typ -> {
        hydra.util.PersistentSet<hydra.core.Name> freevars = hydra.rewriting.Rewriting.freeVariablesInType(typ);
        hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> badvars = new hydra.util.Lazy<>(() -> hydra.lib.sets.Difference.apply(
          hydra.lib.sets.Difference.apply(
            freevars,
            vars),
          svars.get()));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.sets.Null.apply(badvars.get()),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>right(null),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.UnboundTypeVariables(new hydra.error.UnboundTypeVariablesError(badvars.get(), typ))), cx))));
      });
      java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>> checkOptional = (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>) (m -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapMaybe.apply(
          check,
          m),
        (java.util.function.Function<hydra.util.Maybe<java.lang.Void>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>) (ignored -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>right(null))));
      java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>> recurse = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>) (v1 -> checkRecursive.get().apply(vars).apply(trace).apply(lbinding).apply(v1));
      hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>> dflt = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          recurse,
          hydra.rewriting.Rewriting.subterms(term)),
        (java.util.function.Function<hydra.util.ConsList<java.lang.Void>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>) (ignored -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>right(null))));
      return (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> otherwise(hydra.core.Term instance) {
          return dflt.get();
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> visit(hydra.core.Term.Function f) {
          return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> otherwise(hydra.core.Function instance) {
              return dflt.get();
            }

            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> visit(hydra.core.Function.Elimination e) {
              return dflt.get();
            }

            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> visit(hydra.core.Function.Lambda l) {
              return hydra.lib.eithers.Bind.apply(
                (checkOptional).apply((l).value.domain),
                (java.util.function.Function<java.lang.Void, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>) (ignored -> (recurse).apply((l).value.body)));
            }
          });
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> visit(hydra.core.Term.Let l) {
          java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>> forBinding = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>) (b -> {
            hydra.core.Term bterm = (b).term;
            hydra.util.Lazy<hydra.util.ConsList<String>> newTrace = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
              (b).name.value,
              trace));
            hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> newVars = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
              () -> vars,
              (java.util.function.Function<hydra.core.TypeScheme, hydra.util.PersistentSet<hydra.core.Name>>) (ts -> hydra.lib.sets.Union.apply(
                vars,
                hydra.lib.sets.FromList.apply((ts).variables))),
              (b).type));
            return checkRecursive.get().apply(newVars.get()).apply(newTrace.get()).apply(hydra.util.Maybe.just(b)).apply(bterm);
          });
          return hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              forBinding,
              (l).value.bindings),
            (java.util.function.Function<hydra.util.ConsList<java.lang.Void>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>) (ignored -> (recurse).apply((l).value.body)));
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> visit(hydra.core.Term.TypeApplication tt) {
          return hydra.lib.eithers.Bind.apply(
            (check).apply((tt).value.type),
            (java.util.function.Function<java.lang.Void, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>) (ignored -> (recurse).apply((tt).value.body)));
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> visit(hydra.core.Term.TypeLambda tl) {
          return hydra.lib.eithers.Bind.apply(
            (check).apply(new hydra.core.Type.Variable((tl).value.parameter)),
            (java.util.function.Function<java.lang.Void, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>) (ignored -> (recurse).apply((tl).value.body)));
        }
      });
    })))));
    return checkRecursive.get().apply((hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())).apply(hydra.util.ConsList.of("top level")).apply((hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing())).apply(term0);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<java.lang.Void, hydra.context.Context>> checkNominalApplication(hydra.context.Context cx, hydra.graph.Graph tx, hydra.core.Name tname, hydra.util.ConsList<hydra.core.Type> typeArgs) {
    return hydra.lib.eithers.Bind.apply(
      hydra.schemas.Schemas.requireSchemaType(
        cx,
        (tx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<java.lang.Void, hydra.context.Context>>>) (result -> {
        hydra.util.Lazy<Integer> argslen = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(typeArgs));
        hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
        hydra.util.ConsList<hydra.core.Name> vars = schemaType.get().variables;
        hydra.util.Lazy<Integer> varslen = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(vars));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            varslen.get(),
            argslen.get()),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<java.lang.Void, hydra.context.Context>>right((hydra.util.Pair<java.lang.Void, hydra.context.Context>) ((hydra.util.Pair<java.lang.Void, hydra.context.Context>) (new hydra.util.Pair<java.lang.Void, hydra.context.Context>(null, cx2.get())))),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<java.lang.Void, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.TypeArityMismatch(new hydra.error.TypeArityMismatchError(new hydra.core.Type.Variable(tname), varslen.get(), argslen.get(), typeArgs))), cx2.get()))));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> checkSameType(hydra.context.Context cx, hydra.graph.Graph tx, String desc, hydra.util.ConsList<hydra.core.Type> types) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.checking.Checking.typesAllEffectivelyEqual(
        tx,
        types),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>right(hydra.lib.lists.Head.apply(types)),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.UnequalTypes(new hydra.error.UnequalTypesError(types, desc))), cx))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> checkType(hydra.context.Context cx, hydra.graph.Graph tx, hydra.core.Term term, hydra.core.Type typ) {
    hydra.util.PersistentSet<hydra.core.Name> vars = (tx).typeVariables;
    return hydra.lib.logic.IfElse.lazy(
      hydra.constants.Constants.debugInference(),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.core.Type>) (_p -> hydra.lib.pairs.First.apply(_p)),
          hydra.checking.Checking.typeOf(
            cx,
            tx,
            (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
            term)),
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>>) (t0 -> hydra.lib.logic.IfElse.lazy(
          hydra.checking.Checking.typesEffectivelyEqual(
            tx,
            t0,
            typ),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>right(null),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.TypeMismatch(new hydra.error.TypeMismatchError(typ, t0))), cx)))))),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>right(null));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.typing.TypeSubst> checkTypeSubst(hydra.context.Context cx, hydra.graph.Graph tx, hydra.typing.TypeSubst subst) {
    java.util.function.Function<hydra.core.TypeScheme, Boolean> isNominal = (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> hydra.rewriting.Rewriting.deannotateType((ts).type).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Type.Record ignored) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Type.Union ignored) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Type.Wrap ignored) {
        return true;
      }
    }));
    hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> s = (subst).value;
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> vars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply(s)));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> suspectVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.Intersection.apply(
      vars.get(),
      hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((tx).schemaTypes))));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> badVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.maybes.Maybe.applyLazy(
        () -> false,
        isNominal,
        hydra.lexical.Lexical.dereferenceSchemaType(
          v,
          (tx).schemaTypes))),
      hydra.lib.sets.ToList.apply(suspectVars.get()))));
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>> badPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, Boolean>) (p -> hydra.lib.sets.Member.apply(
        hydra.lib.pairs.First.apply(p),
        badVars.get())),
      hydra.lib.maps.ToList.apply(s)));
    java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String> printPair = (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.pairs.First.apply(p).value,
        " --> "),
      hydra.show.core.Core.type(hydra.lib.pairs.Second.apply(p))));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Null.apply(badVars.get()),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.typing.TypeSubst>right(subst),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.typing.TypeSubst>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.IncorrectUnification(new hydra.error.IncorrectUnificationError(subst))), cx))));
  }

  static <T0, T1> java.lang.Void checkTypeVariables(T0 _tx, T1 _typ) {
    return null;
  }

  static Boolean containsInScopeTypeVars(hydra.graph.Graph tx, hydra.core.Type t) {
    hydra.util.PersistentSet<hydra.core.Name> freeVars = hydra.rewriting.Rewriting.freeVariablesInTypeSimple(t);
    hydra.util.PersistentSet<hydra.core.Name> vars = (tx).typeVariables;
    return hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.lib.sets.Intersection.apply(
      vars,
      freeVars)));
  }

  static hydra.core.Type normalizeTypeFreeVars(hydra.core.Type typ) {
    java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>> collectVars = (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>>) (acc -> (java.util.function.Function<hydra.core.Type, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> otherwise(hydra.core.Type instance) {
        return acc;
      }

      @Override
      public hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name> visit(hydra.core.Type.Variable v) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.maps.Member.apply(
            (v).value,
            acc),
          () -> acc,
          () -> hydra.lib.maps.Insert.apply(
            (v).value,
            new hydra.core.Name(hydra.lib.strings.Cat2.apply(
              "_tv",
              hydra.lib.literals.ShowInt32.apply(hydra.lib.maps.Size.apply(acc)))),
            acc));
      }
    })));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> subst = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.foldOverType(
      new hydra.coders.TraversalOrder.Pre(),
      collectVars,
      (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())),
      typ));
    return hydra.rewriting.Rewriting.substituteTypeVariables(
      subst.get(),
      typ);
  }

  static hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> toFContext(hydra.graph.Graph cx) {
    return hydra.lib.maps.Map.apply(
      hydra.rewriting.Rewriting::typeSchemeToFType,
      (cx).boundTypes);
  }

  static Boolean typeListsEffectivelyEqual(hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> tlist1, hydra.util.ConsList<hydra.core.Type> tlist2) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(tlist1),
        hydra.lib.lists.Length.apply(tlist2)),
      () -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.And.apply(
          p0,
          p1)),
        true,
        hydra.lib.lists.ZipWith.apply(
          (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, Boolean>>) (v1 -> (java.util.function.Function<hydra.core.Type, Boolean>) (v2 -> hydra.checking.Checking.typesEffectivelyEqual(
            tx,
            v1,
            v2))),
          tlist1,
          tlist2)),
      () -> false);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOf(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.Term term) {
    hydra.util.Lazy<hydra.context.Context> cx1 = new hydra.util.Lazy<>(() -> new hydra.context.Context(hydra.lib.lists.Cons.apply(
      "typeOf",
      (cx).trace), (cx).messages, (cx).other));
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.UnsupportedTermVariant(new hydra.error.UnsupportedTermVariantError(hydra.reflect.Reflect.termVariant(term)))), cx1.get())));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Annotated v1) {
        return hydra.checking.Checking.typeOfAnnotatedTerm(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Application v1) {
        return hydra.checking.Checking.typeOfApplication(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Either v1) {
        return hydra.checking.Checking.typeOfEither(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Function.Elimination elm) {
            return (elm).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Elimination.Record v1) {
                return hydra.checking.Checking.typeOfProjection(
                  cx1.get(),
                  tx,
                  typeArgs,
                  (v1).value);
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Elimination.Union v1) {
                return hydra.checking.Checking.typeOfCaseStatement(
                  cx1.get(),
                  tx,
                  typeArgs,
                  (v1).value);
              }

              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Elimination.Wrap v1) {
                return hydra.checking.Checking.typeOfUnwrap(
                  cx1.get(),
                  tx,
                  typeArgs,
                  (v1).value);
              }
            });
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Function.Lambda v1) {
            return hydra.checking.Checking.typeOfLambda(
              cx1.get(),
              tx,
              typeArgs,
              (v1).value);
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Function.Primitive v1) {
            return hydra.checking.Checking.typeOfPrimitive(
              cx1.get(),
              tx,
              typeArgs,
              (v1).value);
          }
        });
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Let v1) {
        return hydra.checking.Checking.typeOfLet(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.List v1) {
        return hydra.checking.Checking.typeOfList(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Literal v1) {
        return hydra.checking.Checking.typeOfLiteral(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Map v1) {
        return hydra.checking.Checking.typeOfMap(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Maybe v1) {
        return hydra.checking.Checking.typeOfMaybe(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Pair v1) {
        return hydra.checking.Checking.typeOfPair(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Record v1) {
        return hydra.checking.Checking.typeOfRecord(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Set v1) {
        return hydra.checking.Checking.typeOfSet(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.TypeApplication v1) {
        return hydra.checking.Checking.typeOfTypeApplication(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.TypeLambda v1) {
        return hydra.checking.Checking.typeOfTypeLambda(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Union v1) {
        return hydra.checking.Checking.typeOfInjection(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Unit ignored) {
        return hydra.checking.Checking.typeOfUnit(
          cx1.get(),
          tx,
          typeArgs);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Variable v1) {
        return hydra.checking.Checking.typeOfVariable(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Term.Wrap v1) {
        return hydra.checking.Checking.typeOfWrappedTerm(
          cx1.get(),
          tx,
          typeArgs,
          (v1).value);
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfAnnotatedTerm(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.AnnotatedTerm at) {
    return hydra.checking.Checking.typeOf(
      cx,
      tx,
      typeArgs,
      (at).body);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfApplication(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.Application app) {
    hydra.core.Term arg = (app).argument;
    hydra.core.Term fun = (app).function;
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>>>> tryType = new java.util.concurrent.atomic.AtomicReference<>();
    tryType.set((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>>>) (cx0 -> (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>>) (tfun -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (targ -> (tfun).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.NotAFunctionType(new hydra.error.NotAFunctionTypeError(tfun))), cx0)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Type.Forall ft) {
        return tryType.get().apply(cx0).apply((ft).value.body).apply(targ);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Type.Function ft) {
        hydra.core.Type cod = (ft).value.codomain;
        hydra.core.Type dom = (ft).value.domain;
        return hydra.lib.logic.IfElse.lazy(
          hydra.checking.Checking.typesEffectivelyEqual(
            tx,
            dom,
            targ),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(cod, cx0)))),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.TypeMismatch(new hydra.error.TypeMismatchError(dom, targ))), cx0))));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> visit(hydra.core.Type.Variable v) {
        hydra.util.Pair<hydra.core.Name, hydra.context.Context> nameResult = hydra.schemas.Schemas.freshName(cx0);
        hydra.util.Lazy<hydra.context.Context> cx1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nameResult));
        hydra.util.Lazy<hydra.core.Name> freshN = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nameResult));
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.Variable(freshN.get()), cx1.get()))));
      }
    })))));
    return hydra.lib.eithers.Bind.apply(
      hydra.checking.Checking.typeOf(
        cx,
        tx,
        (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
        fun),
      (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (result1 -> {
        hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result1));
        hydra.util.Lazy<hydra.core.Type> tfun = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result1));
        return hydra.lib.eithers.Bind.apply(
          hydra.checking.Checking.typeOf(
            cx2.get(),
            tx,
            (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
            arg),
          (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (result2 -> {
            hydra.util.Lazy<hydra.context.Context> cx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result2));
            hydra.util.Lazy<hydra.core.Type> targ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result2));
            return hydra.lib.eithers.Bind.apply(
              tryType.get().apply(cx3.get()).apply(tfun.get()).apply(targ.get()),
              (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (result3 -> {
                hydra.util.Lazy<hydra.context.Context> cx4 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result3));
                hydra.util.Lazy<hydra.core.Type> t = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result3));
                return hydra.lib.eithers.Bind.apply(
                  hydra.checking.Checking.applyTypeArgumentsToType(
                    cx4.get(),
                    tx,
                    typeArgs,
                    t.get()),
                  (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (applied -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(applied, cx4.get()))))));
              }));
          }));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfCaseStatement(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.CaseStatement cs) {
    hydra.util.ConsList<hydra.core.Field> cases = (cs).cases;
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> cterms = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.term,
      cases));
    hydra.util.Maybe<hydra.core.Term> dflt = (cs).default_;
    hydra.core.Name tname = (cs).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapMaybe.apply(
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (e -> hydra.checking.Checking.typeOf(
          cx,
          tx,
          (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
          e)),
        dflt),
      (java.util.function.Function<hydra.util.Maybe<hydra.util.Pair<hydra.core.Type, hydra.context.Context>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (dfltResult -> {
        hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> cx,
          (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.context.Context>) ((java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.context.Context>) (hydra.lib.pairs.Second::apply)),
          dfltResult));
        hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>> foldResult = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (term -> hydra.lib.eithers.Bind.apply(
            acc,
            (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (accR -> {
              hydra.util.Lazy<hydra.context.Context> cxA = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(accR));
              hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(accR));
              return hydra.lib.eithers.Bind.apply(
                hydra.checking.Checking.typeOf(
                  cxA.get(),
                  tx,
                  (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
                  term),
                (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (tResult -> {
                  hydra.util.Lazy<hydra.context.Context> cxB = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tResult));
                  hydra.util.Lazy<hydra.core.Type> t = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tResult));
                  return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>(hydra.lib.lists.Concat2.apply(
                    types.get(),
                    hydra.lib.lists.Pure.apply(t.get())), cxB.get()))));
                }));
            })))),
          hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), cx2.get())))),
          cterms.get()));
        hydra.util.Lazy<hydra.util.Maybe<hydra.core.Type>> tdflt = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.core.Type>) ((java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.core.Type>) (hydra.lib.pairs.First::apply)),
          dfltResult));
        return hydra.lib.eithers.Bind.apply(
          foldResult.get(),
          (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (foldR -> {
            hydra.util.Lazy<hydra.context.Context> cx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(foldR));
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> tcterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(foldR));
            hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>> fcodsResult = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>>) (acc -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (t -> hydra.lib.eithers.Bind.apply(
                acc,
                (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (accR -> {
                  hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> cods = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(accR));
                  return hydra.lib.eithers.Bind.apply(
                    hydra.extract.core.Core.functionType(
                      cx3.get(),
                      t),
                    (java.util.function.Function<hydra.core.FunctionType, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (ft -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>(hydra.lib.lists.Concat2.apply(
                      cods.get(),
                      hydra.lib.lists.Pure.apply((ft).codomain)), cx3.get()))))));
                })))),
              hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), cx3.get())))),
              tcterms.get()));
            return hydra.lib.eithers.Bind.apply(
              fcodsResult.get(),
              (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (fcodsR -> {
                hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> fcods = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(fcodsR));
                hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> cods = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Cons.apply(
                  tdflt.get(),
                  hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (hydra.lib.maybes.Pure::apply),
                    fcods.get()))));
                return hydra.lib.eithers.Bind.apply(
                  hydra.checking.Checking.checkSameType(
                    cx3.get(),
                    tx,
                    "case branches",
                    cods.get()),
                  (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (cod -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.schemas.Schemas.nominalApplication(
                    tname,
                    typeArgs), cod)), cx3.get()))))));
              }));
          }));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfEither(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.util.Either<hydra.core.Term, hydra.core.Term> et) {
    hydra.util.Lazy<Integer> n = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(typeArgs));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        n.get(),
        2),
      () -> hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (leftTerm -> hydra.lib.eithers.Bind.apply(
          hydra.checking.Checking.typeOf(
            cx,
            tx,
            (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
            leftTerm),
          (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (result -> {
            hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
            hydra.util.Lazy<hydra.core.Type> leftType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
            return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.Either(new hydra.core.EitherType(leftType.get(), hydra.lib.lists.At.apply(
              1,
              typeArgs))), cx2.get()))));
          }))),
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (rightTerm -> hydra.lib.eithers.Bind.apply(
          hydra.checking.Checking.typeOf(
            cx,
            tx,
            (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
            rightTerm),
          (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (result -> {
            hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
            hydra.util.Lazy<hydra.core.Type> rightType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
            return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.Either(new hydra.core.EitherType(hydra.lib.lists.At.apply(
              0,
              typeArgs), rightType.get())), cx2.get()))));
          }))),
        et),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.TypeArityMismatch(new hydra.error.TypeArityMismatchError(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Unit(), new hydra.core.Type.Unit())), 2, n.get(), typeArgs))), cx))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfInjection(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.Injection injection) {
    hydra.core.Field field = (injection).field;
    hydra.core.Name fname = (field).name;
    hydra.core.Term fterm = (field).term;
    hydra.core.Name tname = (injection).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.schemas.Schemas.requireSchemaType(
        cx,
        (tx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (schemaResult -> {
        hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(schemaResult));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(schemaResult));
        hydra.core.Type sbody = schemaType.get().type;
        hydra.util.ConsList<hydra.core.Name> svars = schemaType.get().variables;
        return hydra.lib.eithers.Bind.apply(
          hydra.extract.core.Core.unionType(
            cx2.get(),
            tname,
            sbody),
          (java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (sfields -> hydra.lib.eithers.Bind.apply(
            hydra.schemas.Schemas.findFieldType(
              cx2.get(),
              fname,
              sfields),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (ftyp -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(hydra.schemas.Schemas.nominalApplication(
              tname,
              typeArgs), cx2.get()))))))));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfLambda(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.Lambda l) {
    hydra.core.Term body = (l).body;
    hydra.util.Maybe<hydra.core.Type> mdom = (l).domain;
    hydra.core.Name v = (l).parameter;
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.UntypedLambda(new hydra.error.UntypedLambdaError())), cx))),
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (dom -> {
          hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>> types2 = new hydra.util.Lazy<>(() -> hydra.lib.maps.Insert.apply(
            v,
            hydra.rewriting.Rewriting.fTypeToTypeScheme(dom),
            (tx).boundTypes));
          return hydra.lib.eithers.Bind.apply(
            hydra.checking.Checking.typeOf(
              cx,
              new hydra.graph.Graph((tx).boundTerms, types2.get(), (tx).classConstraints, (tx).lambdaVariables, (tx).metadata, (tx).primitives, (tx).schemaTypes, (tx).typeVariables),
              (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
              body),
            (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (codResult -> {
              hydra.util.Lazy<hydra.core.Type> cod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(codResult));
              hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(codResult));
              return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod.get())), cx2.get()))));
            }));
        }),
        mdom),
      (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (tbodyResult -> {
        hydra.util.Lazy<hydra.context.Context> cx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tbodyResult));
        hydra.util.Lazy<hydra.core.Type> tbody = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tbodyResult));
        return hydra.lib.eithers.Bind.apply(
          hydra.checking.Checking.applyTypeArgumentsToType(
            cx3.get(),
            tx,
            typeArgs,
            tbody.get()),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (applied -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(applied, cx3.get()))))));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfLet(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.Let letTerm) {
    java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>> bindingType = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>>) (b -> hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.UntypedLetBinding(new hydra.error.UntypedLetBindingError(b))), cx))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>>) (ts -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>right(hydra.rewriting.Rewriting.typeSchemeToFType(ts))),
      (b).type));
    hydra.util.ConsList<hydra.core.Binding> bs = (letTerm).bindings;
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> bnames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      bs));
    hydra.core.Term body = (letTerm).body;
    hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>>> btypesResult = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>>, java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>>>>) (acc -> (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>>>) (b -> hydra.lib.eithers.Bind.apply(
        acc,
        (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>>>) (accR -> {
          hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(accR));
          return hydra.lib.eithers.Bind.apply(
            (bindingType).apply(b),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>>>) (btype -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>(hydra.lib.lists.Concat2.apply(
              types.get(),
              hydra.lib.lists.Pure.apply(btype)), null))))));
        })))),
      hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), null)))),
      bs));
    return hydra.lib.eithers.Bind.apply(
      btypesResult.get(),
      (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, java.lang.Void>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (btypesR -> {
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> btypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(btypesR));
        hydra.util.Lazy<hydra.graph.Graph> tx2 = new hydra.util.Lazy<>(() -> new hydra.graph.Graph((tx).boundTerms, hydra.lib.maps.Union.apply(
          hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
            bnames.get(),
            hydra.lib.lists.Map.apply(
              hydra.rewriting.Rewriting::fTypeToTypeScheme,
              btypes.get()))),
          (tx).boundTypes), (tx).classConstraints, (tx).lambdaVariables, (tx).metadata, (tx).primitives, (tx).schemaTypes, (tx).typeVariables));
        return hydra.lib.eithers.Bind.apply(
          hydra.checking.Checking.typeOf(
            cx,
            tx2.get(),
            (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
            body),
          (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (tResult -> {
            hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tResult));
            hydra.util.Lazy<hydra.core.Type> t = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tResult));
            return hydra.lib.eithers.Bind.apply(
              hydra.checking.Checking.applyTypeArgumentsToType(
                cx2.get(),
                tx,
                typeArgs,
                t.get()),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (applied -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(applied, cx2.get()))))));
          }));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfList(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.util.ConsList<hydra.core.Term> els) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(els),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(typeArgs),
          1),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.List(hydra.lib.lists.Head.apply(typeArgs)), cx)))),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.TypeArityMismatch(new hydra.error.TypeArityMismatchError(new hydra.core.Type.List(new hydra.core.Type.Unit()), 1, hydra.lib.lists.Length.apply(typeArgs), typeArgs))), cx)))),
      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (() -> {
        hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>> foldResult = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (term -> hydra.lib.eithers.Bind.apply(
            acc,
            (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (accR -> {
              hydra.util.Lazy<hydra.context.Context> cxA = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(accR));
              hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(accR));
              return hydra.lib.eithers.Bind.apply(
                hydra.checking.Checking.typeOf(
                  cxA.get(),
                  tx,
                  (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
                  term),
                (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (tResult -> {
                  hydra.util.Lazy<hydra.context.Context> cxB = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tResult));
                  hydra.util.Lazy<hydra.core.Type> t = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tResult));
                  return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>(hydra.lib.lists.Concat2.apply(
                    types.get(),
                    hydra.lib.lists.Pure.apply(t.get())), cxB.get()))));
                }));
            })))),
          hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), cx)))),
          els));
        return hydra.lib.eithers.Bind.apply(
          foldResult.get(),
          (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (foldR -> {
            hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(foldR));
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> eltypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(foldR));
            return hydra.lib.eithers.Bind.apply(
              hydra.checking.Checking.checkSameType(
                cx2.get(),
                tx,
                "list elements",
                eltypes.get()),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (unifiedType -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.List(unifiedType), cx2.get()))))));
          }));
      })).get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfLiteral(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.Literal lit) {
    hydra.core.Type t = new hydra.core.Type.Literal(hydra.reflect.Reflect.literalType(lit));
    return hydra.lib.eithers.Bind.apply(
      hydra.checking.Checking.applyTypeArgumentsToType(
        cx,
        tx,
        typeArgs,
        t),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (applied -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(applied, cx))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfMap(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.util.PersistentMap<hydra.core.Term, hydra.core.Term> m) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply(m),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(typeArgs),
          2),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.Map(new hydra.core.MapType(hydra.lib.lists.At.apply(
          0,
          typeArgs), hydra.lib.lists.At.apply(
          1,
          typeArgs))), cx)))),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.TypeArityMismatch(new hydra.error.TypeArityMismatchError(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Unit(), new hydra.core.Type.Unit())), 2, hydra.lib.lists.Length.apply(typeArgs), typeArgs))), cx)))),
      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (() -> {
        hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>> pairs = new hydra.util.Lazy<>(() -> hydra.lib.maps.ToList.apply(m));
        return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (() -> {
          hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>> keyFoldResult = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>, java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>>) (acc -> (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (p -> hydra.lib.eithers.Bind.apply(
              acc,
              (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (accR -> {
                hydra.util.Lazy<hydra.context.Context> cxA = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(accR));
                hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(accR));
                return hydra.lib.eithers.Bind.apply(
                  hydra.checking.Checking.typeOf(
                    cxA.get(),
                    tx,
                    (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
                    hydra.lib.pairs.First.apply(p)),
                  (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (tResult -> {
                    hydra.util.Lazy<hydra.context.Context> cxB = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tResult));
                    hydra.util.Lazy<hydra.core.Type> t = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tResult));
                    return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>(hydra.lib.lists.Concat2.apply(
                      types.get(),
                      hydra.lib.lists.Pure.apply(t.get())), cxB.get()))));
                  }));
              })))),
            hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), cx)))),
            pairs.get()));
          return hydra.lib.eithers.Bind.apply(
            keyFoldResult.get(),
            (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (keyFoldR -> {
              hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(keyFoldR));
              hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> keyTypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(keyFoldR));
              return hydra.lib.eithers.Bind.apply(
                hydra.checking.Checking.checkSameType(
                  cx2.get(),
                  tx,
                  "map keys",
                  keyTypes.get()),
                (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (kt -> {
                  hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>> valFoldResult = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                    (java.util.function.Function<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>, java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>>) (acc -> (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (p -> hydra.lib.eithers.Bind.apply(
                      acc,
                      (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (accR -> {
                        hydra.util.Lazy<hydra.context.Context> cxA = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(accR));
                        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(accR));
                        return hydra.lib.eithers.Bind.apply(
                          hydra.checking.Checking.typeOf(
                            cxA.get(),
                            tx,
                            (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
                            hydra.lib.pairs.Second.apply(p)),
                          (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (tResult -> {
                            hydra.util.Lazy<hydra.context.Context> cxB = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tResult));
                            hydra.util.Lazy<hydra.core.Type> t = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tResult));
                            return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>(hydra.lib.lists.Concat2.apply(
                              types.get(),
                              hydra.lib.lists.Pure.apply(t.get())), cxB.get()))));
                          }));
                      })))),
                    hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), cx2.get())))),
                    pairs.get()));
                  return hydra.lib.eithers.Bind.apply(
                    valFoldResult.get(),
                    (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (valFoldR -> {
                      hydra.util.Lazy<hydra.context.Context> cx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(valFoldR));
                      hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> valTypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(valFoldR));
                      return hydra.lib.eithers.Bind.apply(
                        hydra.checking.Checking.checkSameType(
                          cx3.get(),
                          tx,
                          "map values",
                          valTypes.get()),
                        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (vt -> hydra.lib.eithers.Bind.apply(
                          hydra.checking.Checking.applyTypeArgumentsToType(
                            cx3.get(),
                            tx,
                            typeArgs,
                            new hydra.core.Type.Map(new hydra.core.MapType(kt, vt))),
                          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (applied -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(applied, cx3.get()))))))));
                    }));
                }));
            }));
        })).get();
      })).get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfMaybe(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.util.Maybe<hydra.core.Term> mt) {
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>> forJust = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (term -> hydra.lib.eithers.Bind.apply(
      hydra.checking.Checking.typeOf(
        cx,
        tx,
        (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
        term),
      (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (tResult -> {
        hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tResult));
        hydra.util.Lazy<hydra.core.Type> termType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tResult));
        hydra.core.Type t = new hydra.core.Type.Maybe(termType.get());
        return hydra.lib.eithers.Bind.apply(
          hydra.checking.Checking.applyTypeArgumentsToType(
            cx2.get(),
            tx,
            typeArgs,
            t),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (applied -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(applied, cx2.get()))))));
      })));
    hydra.util.Lazy<Integer> n = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(typeArgs));
    hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>> forNothing = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        n.get(),
        1),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.Maybe(hydra.lib.lists.Head.apply(typeArgs)), cx)))),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.TypeArityMismatch(new hydra.error.TypeArityMismatchError(new hydra.core.Type.Maybe(new hydra.core.Type.Unit()), 1, n.get(), typeArgs))), cx)))));
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> forNothing.get(),
      forJust,
      mt);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfPair(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.util.Pair<hydra.core.Term, hydra.core.Term> p) {
    hydra.util.Lazy<Integer> n = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(typeArgs));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        n.get(),
        2),
      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (() -> {
        hydra.util.Lazy<hydra.core.Term> pairFst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
        return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (() -> {
          hydra.util.Lazy<hydra.core.Term> pairSnd = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
          return hydra.lib.eithers.Bind.apply(
            hydra.checking.Checking.typeOf(
              cx,
              tx,
              (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
              pairFst.get()),
            (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (result1 -> {
              hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result1));
              hydra.util.Lazy<hydra.core.Type> firstType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result1));
              return hydra.lib.eithers.Bind.apply(
                hydra.checking.Checking.typeOf(
                  cx2.get(),
                  tx,
                  (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
                  pairSnd.get()),
                (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (result2 -> {
                  hydra.util.Lazy<hydra.context.Context> cx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result2));
                  hydra.util.Lazy<hydra.core.Type> secondType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result2));
                  return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.Pair(new hydra.core.PairType(firstType.get(), secondType.get())), cx3.get()))));
                }));
            }));
        })).get();
      })).get(),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.TypeArityMismatch(new hydra.error.TypeArityMismatchError(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Unit(), new hydra.core.Type.Unit())), 2, n.get(), typeArgs))), cx))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfPrimitive(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.Name name) {
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.TypeScheme>> rawTs = new hydra.util.Lazy<>(() -> hydra.lib.maps.Lookup.apply(
      name,
      hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.graph.Primitive, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (_gpt_p -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>((_gpt_p).name, (_gpt_p).type)))),
        hydra.lib.maps.Elems.apply((tx).primitives)))));
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.UndefinedTerm(new hydra.error.UndefinedTermError(name)), cx))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (tsRaw -> {
        hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context> instResult = hydra.schemas.Schemas.instantiateTypeScheme(
          cx,
          tsRaw);
        hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(instResult));
        hydra.util.Lazy<hydra.core.TypeScheme> ts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(instResult));
        hydra.core.Type t = hydra.rewriting.Rewriting.typeSchemeToFType(ts.get());
        return hydra.lib.eithers.Bind.apply(
          hydra.checking.Checking.applyTypeArgumentsToType(
            cx2.get(),
            tx,
            typeArgs,
            t),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (applied -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(applied, cx2.get()))))));
      }),
      rawTs.get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfProjection(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.Projection p) {
    hydra.core.Name fname = (p).field;
    hydra.core.Name tname = (p).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.schemas.Schemas.requireSchemaType(
        cx,
        (tx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (schemaResult -> {
        hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(schemaResult));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(schemaResult));
        hydra.core.Type sbody = schemaType.get().type;
        hydra.util.ConsList<hydra.core.Name> svars = schemaType.get().variables;
        return hydra.lib.eithers.Bind.apply(
          hydra.extract.core.Core.recordType(
            cx2.get(),
            tname,
            sbody),
          (java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (sfields -> hydra.lib.eithers.Bind.apply(
            hydra.schemas.Schemas.findFieldType(
              cx2.get(),
              fname,
              sfields),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (ftyp -> {
              hydra.util.Lazy<hydra.typing.TypeSubst> subst = new hydra.util.Lazy<>(() -> new hydra.typing.TypeSubst(hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
                svars,
                typeArgs))));
              hydra.core.Type sftyp = hydra.substitution.Substitution.substInType(
                subst.get(),
                ftyp);
              return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.schemas.Schemas.nominalApplication(
                tname,
                typeArgs), sftyp)), cx2.get()))));
            }))));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfRecord(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.Record record) {
    hydra.util.ConsList<hydra.core.Field> fields = (record).fields;
    hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>> foldResult = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (term -> hydra.lib.eithers.Bind.apply(
        acc,
        (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (accR -> {
          hydra.util.Lazy<hydra.context.Context> cxA = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(accR));
          hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(accR));
          return hydra.lib.eithers.Bind.apply(
            hydra.checking.Checking.typeOf(
              cxA.get(),
              tx,
              (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
              term),
            (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (tResult -> {
              hydra.util.Lazy<hydra.context.Context> cxB = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tResult));
              hydra.util.Lazy<hydra.core.Type> t = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tResult));
              return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>(hydra.lib.lists.Concat2.apply(
                types.get(),
                hydra.lib.lists.Pure.apply(t.get())), cxB.get()))));
            }));
        })))),
      hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), cx)))),
      hydra.lib.lists.Map.apply(
        projected -> projected.term,
        fields)));
    hydra.core.Name tname = (record).typeName;
    return hydra.lib.eithers.Bind.apply(
      foldResult.get(),
      (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (foldR -> {
        hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(foldR));
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(hydra.schemas.Schemas.nominalApplication(
          tname,
          typeArgs), cx2.get()))));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfSet(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.util.PersistentSet<hydra.core.Term> els) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Null.apply(els),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(typeArgs),
          1),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.Set(hydra.lib.lists.Head.apply(typeArgs)), cx)))),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Checking(new hydra.error.CheckingError.TypeArityMismatch(new hydra.error.TypeArityMismatchError(new hydra.core.Type.Set(new hydra.core.Type.Unit()), 1, hydra.lib.lists.Length.apply(typeArgs), typeArgs))), cx)))),
      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (() -> {
        hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>> foldResult = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (term -> hydra.lib.eithers.Bind.apply(
            acc,
            (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (accR -> {
              hydra.util.Lazy<hydra.context.Context> cxA = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(accR));
              hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(accR));
              return hydra.lib.eithers.Bind.apply(
                hydra.checking.Checking.typeOf(
                  cxA.get(),
                  tx,
                  (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
                  term),
                (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>>) (tResult -> {
                  hydra.util.Lazy<hydra.context.Context> cxB = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tResult));
                  hydra.util.Lazy<hydra.core.Type> t = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tResult));
                  return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>(hydra.lib.lists.Concat2.apply(
                    types.get(),
                    hydra.lib.lists.Pure.apply(t.get())), cxB.get()))));
                }));
            })))),
          hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>((hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()), cx)))),
          hydra.lib.sets.ToList.apply(els)));
        return hydra.lib.eithers.Bind.apply(
          foldResult.get(),
          (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Type>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (foldR -> {
            hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(foldR));
            hydra.util.Lazy<hydra.util.ConsList<hydra.core.Type>> eltypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(foldR));
            return hydra.lib.eithers.Bind.apply(
              hydra.checking.Checking.checkSameType(
                cx2.get(),
                tx,
                "set elements",
                eltypes.get()),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (unifiedType -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.Set(unifiedType), cx2.get()))))));
          }));
      })).get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfTypeApplication(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.TypeApplicationTerm tyapp) {
    hydra.core.Term body = (tyapp).body;
    hydra.core.Type t = (tyapp).type;
    return hydra.checking.Checking.typeOf(
      cx,
      tx,
      hydra.lib.lists.Cons.apply(
        t,
        typeArgs),
      body);
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfTypeLambda(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.TypeLambda tl) {
    hydra.core.Term body = (tl).body;
    hydra.core.Name v = (tl).parameter;
    hydra.util.PersistentSet<hydra.core.Name> vars = (tx).typeVariables;
    hydra.util.Lazy<hydra.graph.Graph> tx2 = new hydra.util.Lazy<>(() -> new hydra.graph.Graph((tx).boundTerms, (tx).boundTypes, (tx).classConstraints, (tx).lambdaVariables, (tx).metadata, (tx).primitives, (tx).schemaTypes, hydra.lib.sets.Insert.apply(
      v,
      vars)));
    return hydra.lib.eithers.Bind.apply(
      hydra.checking.Checking.typeOf(
        cx,
        tx2.get(),
        (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
        body),
      (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (result1 -> {
        hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result1));
        hydra.util.Lazy<hydra.core.Type> t1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result1));
        return hydra.lib.eithers.Bind.apply(
          hydra.checking.Checking.applyTypeArgumentsToType(
            cx2.get(),
            tx,
            typeArgs,
            new hydra.core.Type.Forall(new hydra.core.ForallType(v, t1.get()))),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (applied -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(applied, cx2.get()))))));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfUnit(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs) {
    return hydra.lib.eithers.Bind.apply(
      hydra.checking.Checking.applyTypeArgumentsToType(
        cx,
        tx,
        typeArgs,
        new hydra.core.Type.Unit()),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (applied -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(applied, cx))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfUnwrap(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.Name tname) {
    return hydra.lib.eithers.Bind.apply(
      hydra.schemas.Schemas.requireSchemaType(
        cx,
        (tx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (schemaResult -> {
        hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(schemaResult));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(schemaResult));
        hydra.core.Type sbody = schemaType.get().type;
        hydra.util.ConsList<hydra.core.Name> svars = schemaType.get().variables;
        return hydra.lib.eithers.Bind.apply(
          hydra.extract.core.Core.wrappedType(
            cx2.get(),
            tname,
            sbody),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (wrapped -> {
            hydra.util.Lazy<hydra.typing.TypeSubst> subst = new hydra.util.Lazy<>(() -> new hydra.typing.TypeSubst(hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
              svars,
              typeArgs))));
            hydra.core.Type swrapped = hydra.substitution.Substitution.substInType(
              subst.get(),
              wrapped);
            return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.schemas.Schemas.nominalApplication(
              tname,
              typeArgs), swrapped)), cx2.get()))));
          }));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfVariable(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.Name name) {
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.TypeScheme>> rawTypeScheme = new hydra.util.Lazy<>(() -> hydra.lib.maps.Lookup.apply(
      name,
      (tx).boundTypes));
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.UndefinedType(new hydra.error.UndefinedTypeError(name)), cx))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (ts -> {
        hydra.util.Lazy<hydra.util.Pair<hydra.core.Type, hydra.context.Context>> tResult = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(typeArgs),
          () -> hydra.schemas.Schemas.instantiateType(
            cx,
            hydra.rewriting.Rewriting.typeSchemeToFType(ts)),
          () -> (hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(hydra.rewriting.Rewriting.typeSchemeToFType(ts), cx)))));
        hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tResult.get()));
        hydra.util.Lazy<hydra.core.Type> t = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tResult.get()));
        return hydra.lib.eithers.Bind.apply(
          hydra.checking.Checking.applyTypeArgumentsToType(
            cx2.get(),
            tx,
            typeArgs,
            t.get()),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (applied -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(applied, cx2.get()))))));
      }),
      rawTypeScheme.get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>> typeOfWrappedTerm(hydra.context.Context cx, hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> typeArgs, hydra.core.WrappedTerm wt) {
    hydra.core.Term body = (wt).body;
    hydra.core.Name tname = (wt).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.checking.Checking.typeOf(
        cx,
        tx,
        (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
        body),
      (java.util.function.Function<hydra.util.Pair<hydra.core.Type, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>>) (result -> {
        hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Type, hydra.context.Context>>right((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(hydra.schemas.Schemas.nominalApplication(
          tname,
          typeArgs), cx2.get()))));
      }));
  }

  static Boolean typesAllEffectivelyEqual(hydra.graph.Graph tx, hydra.util.ConsList<hydra.core.Type> tlist) {
    hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme> types = (tx).schemaTypes;
    java.util.function.Function<hydra.core.Type, Boolean> containsFreeVar = (java.util.function.Function<hydra.core.Type, Boolean>) (t -> {
      hydra.util.PersistentSet<hydra.core.Name> allVars = hydra.rewriting.Rewriting.freeVariablesInTypeSimple(t);
      hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> schemaNames = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply(types)));
      return hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.lib.sets.Difference.apply(
        allVars,
        schemaNames.get())));
    });
    hydra.util.Lazy<Boolean> anyContainsFreeVar = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Type, Boolean>>) (acc -> (java.util.function.Function<hydra.core.Type, Boolean>) (t -> hydra.lib.logic.Or.apply(
        acc,
        (containsFreeVar).apply(t)))),
      false,
      tlist));
    return hydra.lib.logic.IfElse.lazy(
      anyContainsFreeVar.get(),
      () -> true,
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.checking.Checking.allEqual(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> hydra.checking.Checking.normalizeTypeFreeVars(t)),
          tlist)),
        () -> true,
        () -> hydra.checking.Checking.allEqual(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> hydra.checking.Checking.normalizeTypeFreeVars(hydra.rewriting.Rewriting.deannotateTypeRecursive(hydra.rewriting.Rewriting.replaceTypedefs(
            types,
            t)))),
          tlist))));
  }

  static Boolean typesEffectivelyEqual(hydra.graph.Graph tx, hydra.core.Type t1, hydra.core.Type t2) {
    return hydra.lib.logic.Or.apply(
      hydra.checking.Checking.containsInScopeTypeVars(
        tx,
        t1),
      hydra.lib.logic.Or.apply(
        hydra.checking.Checking.containsInScopeTypeVars(
          tx,
          t2),
        hydra.checking.Checking.typesAllEffectivelyEqual(
          tx,
          hydra.util.ConsList.of(
            hydra.schemas.Schemas.fullyStripAndNormalizeType(t1),
            hydra.schemas.Schemas.fullyStripAndNormalizeType(t2)))));
  }
}
