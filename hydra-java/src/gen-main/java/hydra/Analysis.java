// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Module dependency namespace analysis
 */
public interface Analysis {
  static <T0> hydra.packaging.Namespaces<T0> addNamesToNamespaces(java.util.function.Function<hydra.packaging.Namespace, T0> encodeNamespace, hydra.util.PersistentSet<hydra.core.Name> names, hydra.packaging.Namespaces<T0> ns0) {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.packaging.Namespace>> nss = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      hydra.Names::namespaceOf,
      hydra.lib.sets.ToList.apply(names)))));
    return (hydra.packaging.Namespaces<T0>) (new hydra.packaging.Namespaces<T0>(((java.util.function.Function<hydra.packaging.Namespaces<T0>, hydra.util.Pair<hydra.packaging.Namespace, T0>>) (projected -> projected.focus)).apply(ns0), hydra.lib.maps.Union.apply(
      ((java.util.function.Function<hydra.packaging.Namespaces<T0>, hydra.util.PersistentMap<hydra.packaging.Namespace, T0>>) (projected -> projected.mapping)).apply(ns0),
      hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.Namespace, hydra.util.Pair<hydra.packaging.Namespace, T0>>) (v1 -> hydra.Analysis.<T0>addNamesToNamespaces_toPair(
          encodeNamespace,
          v1)),
        hydra.lib.sets.ToList.apply(nss.get()))))));
  }

  static <T0> hydra.util.Pair<hydra.packaging.Namespace, T0> addNamesToNamespaces_toPair(java.util.function.Function<hydra.packaging.Namespace, T0> encodeNamespace, hydra.packaging.Namespace ns) {
    return (hydra.util.Pair<hydra.packaging.Namespace, T0>) ((hydra.util.Pair<hydra.packaging.Namespace, T0>) (new hydra.util.Pair<hydra.packaging.Namespace, T0>(ns, (encodeNamespace).apply(ns))));
  }

  static <T0, T1> hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTerm(hydra.context.Context cx, java.util.function.Function<T0, hydra.graph.Graph> getTC, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T0>> setTC, T0 env, hydra.core.Term term) {
    return hydra.Analysis.<T0, T1>analyzeFunctionTermWith(
      cx,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (g -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (b -> hydra.lib.logic.IfElse.lazy(
        hydra.Predicates.isComplexBinding(
          g,
          b),
        () -> hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))),
        () -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))),
      getTC,
      setTC,
      env,
      term);
  }

  static <T0, T1> hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTermWith(hydra.context.Context cx, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, java.util.function.Function<T0, hydra.graph.Graph> getTC, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T0>> setTC, T0 env, hydra.core.Term term) {
    return hydra.Analysis.<T0, T1>analyzeFunctionTermWith_gather(
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
      (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.Type>>) (ignored -> (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
      (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (c -> hydra.util.Maybe.just(c)),
      hydra.Checking.typeOfTerm(
        cx,
        (getTC).apply(fEnv),
        bodyWithTapps.get())));
    return hydra.util.Either.<T1, hydra.typing.FunctionStructure<T0>>right((hydra.typing.FunctionStructure<T0>) (new hydra.typing.FunctionStructure<T0>(hydra.lib.lists.Reverse.apply(tparams), hydra.lib.lists.Reverse.apply(args), bindings, bodyWithTapps.get(), hydra.lib.lists.Reverse.apply(doms), mcod.get(), fEnv)));
  }

  static <T0, T1> hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> analyzeFunctionTermWith_gather(hydra.context.Context cx, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, java.util.function.Function<T0, hydra.graph.Graph> getTC, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T0>> setTC, Boolean argMode, T0 gEnv, hydra.util.ConsList<hydra.core.Name> tparams, hydra.util.ConsList<hydra.core.Name> args, hydra.util.ConsList<hydra.core.Binding> bindings, hydra.util.ConsList<hydra.core.Type> doms, hydra.util.ConsList<hydra.core.Type> tapps, hydra.core.Term t) {
    return hydra.Strip.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> otherwise(hydra.core.Term instance) {
        return hydra.Analysis.<T0, T1>analyzeFunctionTermWith_finish(
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
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>> otherwise(hydra.core.Function instance) {
            return hydra.Analysis.<T0, T1>analyzeFunctionTermWith_finish(
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
                hydra.core.Name v = (lam).value.parameter;
                return ((java.util.function.Supplier<hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>>>) (() -> {
                  hydra.util.Lazy<hydra.core.Type> dom = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
                    () -> new hydra.core.Type.Variable(new hydra.core.Name("_")),
                    (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x_ -> x_),
                    (lam).value.domain));
                  return ((java.util.function.Supplier<hydra.util.Either<T1, hydra.typing.FunctionStructure<T0>>>) (() -> {
                    hydra.core.Term body = (lam).value.body;
                    return hydra.Analysis.<T0, T1>analyzeFunctionTermWith_gather(
                      cx,
                      forBinding,
                      getTC,
                      setTC,
                      argMode,
                      hydra.Analysis.<T0>analyzeFunctionTermWith_gather_newEnv(
                        gEnv,
                        getTC,
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.Scoping.extendGraphForLambda(
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
              () -> hydra.Analysis.<T0, T1>analyzeFunctionTermWith_finish(
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
        hydra.core.Term body = (lt).value.body;
        hydra.util.ConsList<hydra.core.Binding> newBindings = (lt).value.bindings;
        return hydra.Analysis.<T0, T1>analyzeFunctionTermWith_gather(
          cx,
          forBinding,
          getTC,
          setTC,
          false,
          hydra.Analysis.<T0>analyzeFunctionTermWith_gather_newEnv2(
            forBinding,
            gEnv,
            getTC,
            (java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>>) (p0 -> p1 -> p2 -> hydra.Scoping.extendGraphForLet(
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
        hydra.core.Term taBody = (ta).value.body;
        hydra.core.Type typ = (ta).value.type;
        return hydra.Analysis.<T0, T1>analyzeFunctionTermWith_gather(
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
        hydra.core.Term tlBody = (tl).value.body;
        hydra.core.Name tvar = (tl).value.parameter;
        return hydra.Analysis.<T0, T1>analyzeFunctionTermWith_gather(
          cx,
          forBinding,
          getTC,
          setTC,
          argMode,
          hydra.Analysis.<T0>analyzeFunctionTermWith_gather_newEnv3(
            gEnv,
            getTC,
            (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.Scoping.extendGraphForTypeLambda(
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

  static <T0> T0 analyzeFunctionTermWith_gather_newEnv(T0 gEnv, java.util.function.Function<T0, hydra.graph.Graph> getTC, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>> hydra_scoping_extendGraphForLambda, hydra.core.Lambda lam, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T0>> setTC) {
    return (setTC).apply((hydra_scoping_extendGraphForLambda).apply((getTC).apply(gEnv)).apply(lam)).apply(gEnv);
  }

  static <T0> T0 analyzeFunctionTermWith_gather_newEnv2(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, T0 gEnv, java.util.function.Function<T0, hydra.graph.Graph> getTC, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>> hydra_scoping_extendGraphForLet, hydra.core.Let lt, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T0>> setTC) {
    return (setTC).apply((hydra_scoping_extendGraphForLet).apply(forBinding).apply((getTC).apply(gEnv)).apply(lt)).apply(gEnv);
  }

  static <T0> T0 analyzeFunctionTermWith_gather_newEnv3(T0 gEnv, java.util.function.Function<T0, hydra.graph.Graph> getTC, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>> hydra_scoping_extendGraphForTypeLambda, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T0>> setTC, hydra.core.TypeLambda tl) {
    return (setTC).apply((hydra_scoping_extendGraphForTypeLambda).apply((getTC).apply(gEnv)).apply(tl)).apply(gEnv);
  }

  static hydra.util.PersistentSet<hydra.packaging.Namespace> definitionDependencyNamespaces(hydra.util.ConsList<hydra.packaging.Definition> defs) {
    java.util.function.Function<hydra.packaging.Definition, hydra.util.PersistentSet<hydra.core.Name>> defNames = (java.util.function.Function<hydra.packaging.Definition, hydra.util.PersistentSet<hydra.core.Name>>) (def -> (def).accept(new hydra.packaging.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.packaging.Definition.Type typeDef) {
        return hydra.Dependencies.typeDependencyNames(
          true,
          (typeDef).value.type.type);
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.packaging.Definition.Term termDef) {
        return hydra.Dependencies.termDependencyNames(
          true,
          true,
          true,
          (termDef).value.term);
      }
    }));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> allNames = new hydra.util.Lazy<>(() -> hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
      defNames,
      defs)));
    return hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      hydra.Names::namespaceOf,
      hydra.lib.sets.ToList.apply(allNames.get()))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentSet<hydra.packaging.Namespace>> dependencyNamespaces(hydra.context.Context cx, hydra.graph.Graph graph, Boolean binds, Boolean withPrims, Boolean withNoms, Boolean withSchema, hydra.util.ConsList<hydra.core.Binding> els) {
    java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentSet<hydra.core.Name>>> depNames = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentSet<hydra.core.Name>>>) (el -> {
      hydra.core.Term term = (el).term;
      hydra.util.PersistentSet<hydra.core.Name> dataNames = hydra.Dependencies.termDependencyNames(
        binds,
        withPrims,
        withNoms,
        term);
      hydra.core.Term deannotatedTerm = hydra.Strip.deannotateTerm(term);
      hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> schemaNames = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
        withSchema,
        () -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.PersistentSet<hydra.core.Name>>) (ts -> hydra.Dependencies.typeDependencyNames(
            true,
            (ts).type)),
          (el).type),
        () -> (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())));
      return hydra.lib.logic.IfElse.lazy(
        hydra.Predicates.isEncodedType(deannotatedTerm),
        () -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>) (typ -> hydra.lib.sets.Unions.apply(hydra.util.ConsList.of(
            dataNames,
            schemaNames.get(),
            hydra.Dependencies.typeDependencyNames(
              true,
              typ)))),
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<hydra.errors.Error_, hydra.context.InContext<hydra.errors.Error_>>) (_wc_e -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(_wc_e, new hydra.context.Context(hydra.lib.lists.Cons.apply(
              "dependency namespace (type)",
              (cx).trace), (cx).messages, (cx).other)))),
            (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_wc_a -> _wc_a),
            hydra.lib.eithers.Bimap.apply(
              (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Other(new hydra.errors.OtherError((_e).value))),
              (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_a -> _a),
              hydra.decode.Core.type(
                graph,
                term)))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.Predicates.isEncodedTerm(deannotatedTerm),
          () -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.PersistentSet<hydra.core.Name>>) (decodedTerm -> hydra.lib.sets.Unions.apply(hydra.util.ConsList.of(
              dataNames,
              schemaNames.get(),
              hydra.Dependencies.termDependencyNames(
                binds,
                withPrims,
                withNoms,
                decodedTerm)))),
            hydra.lib.eithers.Bimap.apply(
              (java.util.function.Function<hydra.errors.Error_, hydra.context.InContext<hydra.errors.Error_>>) (_wc_e -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(_wc_e, new hydra.context.Context(hydra.lib.lists.Cons.apply(
                "dependency namespace (term)",
                (cx).trace), (cx).messages, (cx).other)))),
              (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (_wc_a -> _wc_a),
              hydra.lib.eithers.Bimap.apply(
                (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Other(new hydra.errors.OtherError((_e).value))),
                (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (_a -> _a),
                hydra.decode.Core.term(
                  graph,
                  term)))),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentSet<hydra.core.Name>>right(hydra.lib.sets.Unions.apply(hydra.util.ConsList.of(
            dataNames,
            schemaNames.get())))));
    });
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.ConsList<hydra.util.PersistentSet<hydra.core.Name>>, hydra.util.PersistentSet<hydra.packaging.Namespace>>) (namesList -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        hydra.Names::namespaceOf,
        hydra.lib.sets.ToList.apply(hydra.lib.sets.Unions.apply(namesList)))))),
      hydra.lib.eithers.MapList.apply(
        depNames,
        els));
  }

  static hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term> gatherApplications(hydra.core.Term term) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>>>) (args -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>>) (t -> hydra.Strip.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term>(args, t)));
      }

      @Override
      public hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term> visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = (app).value.function;
        hydra.core.Term rhs = (app).value.argument;
        return go.get().apply(hydra.lib.lists.Cons.apply(
          rhs,
          args)).apply(lhs);
      }
    }))));
    return go.get().apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())).apply(term);
  }

  static hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>> gatherArgs(hydra.core.Term term, hydra.util.ConsList<hydra.core.Term> args) {
    return hydra.Strip.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>> otherwise(hydra.core.Term instance) {
        return (hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>>) ((hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>>) (new hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>>(term, args)));
      }

      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>> visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = (app).value.function;
        hydra.core.Term rhs = (app).value.argument;
        return hydra.Analysis.gatherArgs(
          lhs,
          hydra.lib.lists.Cons.apply(
            rhs,
            args));
      }

      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term body = (tl).value.body;
        return hydra.Analysis.gatherArgs(
          body,
          args);
      }

      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Term body = (ta).value.body;
        return hydra.Analysis.gatherArgs(
          body,
          args);
      }
    });
  }

  static hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>> gatherArgsWithTypeApps(hydra.core.Term term, hydra.util.ConsList<hydra.core.Term> args, hydra.util.ConsList<hydra.core.Type> tyArgs) {
    return hydra.Strip.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>> otherwise(hydra.core.Term instance) {
        return (hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>>(term, (hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>(args, tyArgs))))));
      }

      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>> visit(hydra.core.Term.Application app) {
        hydra.core.Term lhs = (app).value.function;
        hydra.core.Term rhs = (app).value.argument;
        return hydra.Analysis.gatherArgsWithTypeApps(
          lhs,
          hydra.lib.lists.Cons.apply(
            rhs,
            args),
          tyArgs);
      }

      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term body = (tl).value.body;
        return hydra.Analysis.gatherArgsWithTypeApps(
          body,
          args,
          tyArgs);
      }

      @Override
      public hydra.util.Pair<hydra.core.Term, hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.util.ConsList<hydra.core.Type>>> visit(hydra.core.Term.TypeApplication ta) {
        hydra.core.Term body = (ta).value.body;
        hydra.core.Type typ = (ta).value.type;
        return hydra.Analysis.gatherArgsWithTypeApps(
          body,
          args,
          hydra.lib.lists.Cons.apply(
            typ,
            tyArgs));
      }
    });
  }

  static Boolean isSelfTailRecursive(hydra.core.Name funcName, hydra.core.Term body) {
    Boolean callsSelf = hydra.lib.logic.Not.apply(hydra.Variables.isFreeVariableInTerm(
      funcName,
      body));
    return hydra.lib.logic.IfElse.lazy(
      callsSelf,
      () -> hydra.Analysis.isTailRecursiveInTailPosition(
        funcName,
        body),
      () -> false);
  }

  static Boolean isSimpleAssignment(hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        hydra.util.Lazy<hydra.core.Term> baseTerm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.Analysis.gatherArgs(
          term,
          (hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty()))));
        return baseTerm.get().accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Term instance) {
            return true;
          }

          @Override
          public Boolean visit(hydra.core.Term.Function f) {
            return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public Boolean otherwise(hydra.core.Function instance) {
                return true;
              }

              @Override
              public Boolean visit(hydra.core.Function.Elimination elim) {
                return (elim).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
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
        return hydra.Analysis.isSimpleAssignment((at).value.body);
      }

      @Override
      public Boolean visit(hydra.core.Term.Function f) {
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
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
        return hydra.Analysis.isSimpleAssignment((ta).value.body);
      }
    });
  }

  static Boolean isTailRecursiveInTailPosition(hydra.core.Name funcName, hydra.core.Term term) {
    hydra.core.Term stripped = hydra.Strip.deannotateAndDetypeTerm(term);
    return (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return hydra.Variables.isFreeVariableInTerm(
          funcName,
          term);
      }

      @Override
      public Boolean visit(hydra.core.Term.Application app) {
        hydra.util.Pair<hydra.util.ConsList<hydra.core.Term>, hydra.core.Term> gathered = hydra.Analysis.gatherApplications(stripped);
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> gatherArgs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(gathered));
        hydra.util.Lazy<hydra.core.Term> gatherFun = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(gathered));
        hydra.core.Term strippedFun = hydra.Strip.deannotateAndDetypeTerm(gatherFun.get());
        return (strippedFun).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Term instance) {
            return hydra.Variables.isFreeVariableInTerm(
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
                    hydra.Variables.isFreeVariableInTerm(
                      funcName,
                      arg)))),
                  true,
                  gatherArgs.get()));
                return ((java.util.function.Supplier<Boolean>) (() -> {
                  hydra.util.Lazy<Boolean> argsNoLambda = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                    (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (ok -> (java.util.function.Function<hydra.core.Term, Boolean>) (arg -> hydra.lib.logic.And.apply(
                      ok,
                      hydra.lib.logic.Not.apply(hydra.Rewriting.foldOverTerm(
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
                              return (f2).value.accept(new hydra.core.Function.PartialVisitor<>() {
                                @Override
                                public Boolean otherwise(hydra.core.Function instance) {
                                  return false;
                                }

                                @Override
                                public Boolean visit(hydra.core.Function.Lambda lam) {
                                  hydra.core.Term ignore = (lam).value.body;
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
              () -> hydra.Variables.isFreeVariableInTerm(
                funcName,
                term));
          }

          @Override
          public Boolean visit(hydra.core.Term.Function f) {
            return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
              @Override
              public Boolean otherwise(hydra.core.Function instance) {
                return hydra.Variables.isFreeVariableInTerm(
                  funcName,
                  term);
              }

              @Override
              public Boolean visit(hydra.core.Function.Elimination e) {
                return (e).value.accept(new hydra.core.Elimination.PartialVisitor<>() {
                  @Override
                  public Boolean otherwise(hydra.core.Elimination instance) {
                    return hydra.Variables.isFreeVariableInTerm(
                      funcName,
                      term);
                  }

                  @Override
                  public Boolean visit(hydra.core.Elimination.Union cs) {
                    hydra.util.Lazy<Boolean> argsOk = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                      (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (ok -> (java.util.function.Function<hydra.core.Term, Boolean>) (arg -> hydra.lib.logic.And.apply(
                        ok,
                        hydra.Variables.isFreeVariableInTerm(
                          funcName,
                          arg)))),
                      true,
                      gatherArgs.get()));
                    hydra.util.ConsList<hydra.core.Field> cases_ = (cs).value.cases;
                    hydra.util.Lazy<Boolean> branchesOk = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                      (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Field, Boolean>>) (ok -> (java.util.function.Function<hydra.core.Field, Boolean>) (field -> hydra.lib.logic.And.apply(
                        ok,
                        hydra.Analysis.isTailRecursiveInTailPosition(
                          funcName,
                          (field).term)))),
                      true,
                      cases_));
                    hydra.util.Maybe<hydra.core.Term> dflt = (cs).value.default_;
                    hydra.util.Lazy<Boolean> dfltOk = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
                      () -> true,
                      (java.util.function.Function<hydra.core.Term, Boolean>) (d -> hydra.Analysis.isTailRecursiveInTailPosition(
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
        return (f).value.accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Function instance) {
            return hydra.Variables.isFreeVariableInTerm(
              funcName,
              term);
          }

          @Override
          public Boolean visit(hydra.core.Function.Lambda lam) {
            return hydra.Analysis.isTailRecursiveInTailPosition(
              funcName,
              (lam).value.body);
          }
        });
      }

      @Override
      public Boolean visit(hydra.core.Term.Let lt) {
        hydra.util.Lazy<Boolean> bindingsOk = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Binding, Boolean>>) (ok -> (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.logic.And.apply(
            ok,
            hydra.Variables.isFreeVariableInTerm(
              funcName,
              (b).term)))),
          true,
          (lt).value.bindings));
        return hydra.lib.logic.And.apply(
          bindingsOk.get(),
          hydra.Analysis.isTailRecursiveInTailPosition(
            funcName,
            (lt).value.body));
      }
    });
  }

  static Boolean moduleContainsBinaryLiterals(hydra.packaging.Module mod) {
    java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>> checkTerm = (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (found -> (java.util.function.Function<hydra.core.Term, Boolean>) (term -> hydra.lib.logic.Or.apply(
      found,
      (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Term instance) {
          return false;
        }

        @Override
        public Boolean visit(hydra.core.Term.Literal lit) {
          return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
            @Override
            public Boolean otherwise(hydra.core.Literal instance) {
              return false;
            }

            @Override
            public Boolean visit(hydra.core.Literal.Binary ignored) {
              return true;
            }
          });
        }
      }))));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Term>> defTerms = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.core.Term>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Term> otherwise(hydra.packaging.Definition instance) {
          return (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing());
        }

        @Override
        public hydra.util.Maybe<hydra.core.Term> visit(hydra.packaging.Definition.Term td) {
          return hydra.util.Maybe.just((td).value.term);
        }
      })),
      (mod).definitions)));
    java.util.function.Function<hydra.core.Term, Boolean> termContainsBinary = (java.util.function.Function<hydra.core.Term, Boolean>) (term -> hydra.Rewriting.foldOverTerm(
      new hydra.coders.TraversalOrder.Pre(),
      checkTerm,
      false,
      term));
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (acc -> (java.util.function.Function<hydra.core.Term, Boolean>) (t -> hydra.lib.logic.Or.apply(
        acc,
        (termContainsBinary).apply(t)))),
      false,
      defTerms.get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentSet<hydra.packaging.Namespace>> moduleDependencyNamespaces(hydra.context.Context cx, hydra.graph.Graph graph, Boolean binds, Boolean withPrims, Boolean withNoms, Boolean withSchema, hydra.packaging.Module mod) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> allBindings = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.core.Binding>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.packaging.Definition instance) {
          return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
        }

        @Override
        public hydra.util.Maybe<hydra.core.Binding> visit(hydra.packaging.Definition.Type td) {
          return hydra.util.Maybe.just(((java.util.function.Supplier<hydra.core.Binding>) (() -> {
            hydra.core.Term schemaTerm = new hydra.core.Term.Variable(new hydra.core.Name("hydra.core.Type"));
            return ((java.util.function.Supplier<hydra.core.Binding>) (() -> {
              hydra.util.Lazy<hydra.core.Term> dataTerm = new hydra.util.Lazy<>(() -> hydra.Annotations.normalizeTermAnnotations(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.encode.Core.type((td).value.type.type), hydra.lib.maps.FromList.apply(hydra.util.ConsList.of((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>(hydra.Constants.key_type(), schemaTerm)))))))));
              return new hydra.core.Binding((td).value.name, dataTerm.get(), hydra.util.Maybe.just(new hydra.core.TypeScheme((hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), (hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))));
            })).get();
          })).get());
        }

        @Override
        public hydra.util.Maybe<hydra.core.Binding> visit(hydra.packaging.Definition.Term td) {
          return hydra.util.Maybe.just(new hydra.core.Binding((td).value.name, (td).value.term, (td).value.type));
        }
      })),
      (mod).definitions)));
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.PersistentSet<hydra.packaging.Namespace>, hydra.util.PersistentSet<hydra.packaging.Namespace>>) (deps -> hydra.lib.sets.Delete.apply(
        (mod).namespace,
        deps)),
      hydra.Analysis.dependencyNamespaces(
        cx,
        graph,
        binds,
        withPrims,
        withNoms,
        withSchema,
        allBindings.get()));
  }

  static <T0> hydra.packaging.Namespaces<T0> namespacesForDefinitions(java.util.function.Function<hydra.packaging.Namespace, T0> encodeNamespace, hydra.packaging.Namespace focusNs, hydra.util.ConsList<hydra.packaging.Definition> defs) {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.packaging.Namespace>> nss = new hydra.util.Lazy<>(() -> hydra.lib.sets.Delete.apply(
      focusNs,
      hydra.Analysis.definitionDependencyNamespaces(defs)));
    java.util.function.Function<hydra.packaging.Namespace, hydra.util.Pair<hydra.packaging.Namespace, T0>> toPair = (java.util.function.Function<hydra.packaging.Namespace, hydra.util.Pair<hydra.packaging.Namespace, T0>>) (v1 -> hydra.Analysis.<T0>namespacesForDefinitions_toPair(
      encodeNamespace,
      v1));
    return (hydra.packaging.Namespaces<T0>) (new hydra.packaging.Namespaces<T0>((toPair).apply(focusNs), hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      toPair,
      hydra.lib.sets.ToList.apply(nss.get())))));
  }

  static <T0> hydra.util.Pair<hydra.packaging.Namespace, T0> namespacesForDefinitions_toPair(java.util.function.Function<hydra.packaging.Namespace, T0> encodeNamespace, hydra.packaging.Namespace ns) {
    return (hydra.util.Pair<hydra.packaging.Namespace, T0>) ((hydra.util.Pair<hydra.packaging.Namespace, T0>) (new hydra.util.Pair<hydra.packaging.Namespace, T0>(ns, (encodeNamespace).apply(ns))));
  }
}
