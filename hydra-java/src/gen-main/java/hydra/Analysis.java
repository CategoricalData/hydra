// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Module dependency namespace analysis
 */
public interface Analysis {
  static <T0> hydra.module.Namespaces<T0> addNamesToNamespaces(java.util.function.Function<hydra.module.Namespace, T0> encodeNamespace, hydra.util.PersistentSet<hydra.core.Name> names, hydra.module.Namespaces<T0> ns0) {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.module.Namespace>> nss = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      hydra.Names::namespaceOf,
      hydra.lib.sets.ToList.apply(names)))));
    return (hydra.module.Namespaces<T0>) (new hydra.module.Namespaces<T0>(((java.util.function.Function<hydra.module.Namespaces<T0>, hydra.util.Pair<hydra.module.Namespace, T0>>) (projected -> projected.focus)).apply(ns0), hydra.lib.maps.Union.apply(
      ((java.util.function.Function<hydra.module.Namespaces<T0>, hydra.util.PersistentMap<hydra.module.Namespace, T0>>) (projected -> projected.mapping)).apply(ns0),
      hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Namespace, hydra.util.Pair<hydra.module.Namespace, T0>>) (v1 -> hydra.Analysis.<T0>addNamesToNamespaces_toPair(
          encodeNamespace,
          v1)),
        hydra.lib.sets.ToList.apply(nss.get()))))));
  }

  static <T0> hydra.util.Pair<hydra.module.Namespace, T0> addNamesToNamespaces_toPair(java.util.function.Function<hydra.module.Namespace, T0> encodeNamespace, hydra.module.Namespace ns) {
    return (hydra.util.Pair<hydra.module.Namespace, T0>) ((hydra.util.Pair<hydra.module.Namespace, T0>) (new hydra.util.Pair<hydra.module.Namespace, T0>(ns, (encodeNamespace).apply(ns))));
  }

  static hydra.util.PersistentSet<hydra.module.Namespace> definitionDependencyNamespaces(hydra.util.ConsList<hydra.module.Definition> defs) {
    java.util.function.Function<hydra.module.Definition, hydra.util.PersistentSet<hydra.core.Name>> defNames = (java.util.function.Function<hydra.module.Definition, hydra.util.PersistentSet<hydra.core.Name>>) (def -> (def).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.module.Definition.Type typeDef) {
        return hydra.Dependencies.typeDependencyNames(
          true,
          (typeDef).value.type);
      }

      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.module.Definition.Term termDef) {
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

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentSet<hydra.module.Namespace>> dependencyNamespaces(hydra.context.Context cx, hydra.graph.Graph graph, Boolean binds, Boolean withPrims, Boolean withNoms, Boolean withSchema, hydra.util.ConsList<hydra.core.Binding> els) {
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
      (java.util.function.Function<hydra.util.ConsList<hydra.util.PersistentSet<hydra.core.Name>>, hydra.util.PersistentSet<hydra.module.Namespace>>) (namesList -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        hydra.Names::namespaceOf,
        hydra.lib.sets.ToList.apply(hydra.lib.sets.Unions.apply(namesList)))))),
      hydra.lib.eithers.MapList.apply(
        depNames,
        els));
  }

  static Boolean moduleContainsBinaryLiterals(hydra.module.Module mod) {
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
      (java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.core.Term>>) (d -> (d).accept(new hydra.module.Definition.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Term> otherwise(hydra.module.Definition instance) {
          return (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing());
        }

        @Override
        public hydra.util.Maybe<hydra.core.Term> visit(hydra.module.Definition.Term td) {
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

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentSet<hydra.module.Namespace>> moduleDependencyNamespaces(hydra.context.Context cx, hydra.graph.Graph graph, Boolean binds, Boolean withPrims, Boolean withNoms, Boolean withSchema, hydra.module.Module mod) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> allBindings = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.core.Binding>>) (d -> (d).accept(new hydra.module.Definition.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.module.Definition instance) {
          return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
        }

        @Override
        public hydra.util.Maybe<hydra.core.Binding> visit(hydra.module.Definition.Type td) {
          return hydra.util.Maybe.just(hydra.Annotations.typeElement(
            (td).value.name,
            (td).value.type));
        }

        @Override
        public hydra.util.Maybe<hydra.core.Binding> visit(hydra.module.Definition.Term td) {
          return hydra.util.Maybe.just(new hydra.core.Binding((td).value.name, (td).value.term, (td).value.type));
        }
      })),
      (mod).definitions)));
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.PersistentSet<hydra.module.Namespace>>) (deps -> hydra.lib.sets.Delete.apply(
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

  static <T0> hydra.module.Namespaces<T0> namespacesForDefinitions(java.util.function.Function<hydra.module.Namespace, T0> encodeNamespace, hydra.module.Namespace focusNs, hydra.util.ConsList<hydra.module.Definition> defs) {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.module.Namespace>> nss = new hydra.util.Lazy<>(() -> hydra.lib.sets.Delete.apply(
      focusNs,
      hydra.Analysis.definitionDependencyNamespaces(defs)));
    java.util.function.Function<hydra.module.Namespace, hydra.util.Pair<hydra.module.Namespace, T0>> toPair = (java.util.function.Function<hydra.module.Namespace, hydra.util.Pair<hydra.module.Namespace, T0>>) (v1 -> hydra.Analysis.<T0>namespacesForDefinitions_toPair(
      encodeNamespace,
      v1));
    return (hydra.module.Namespaces<T0>) (new hydra.module.Namespaces<T0>((toPair).apply(focusNs), hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      toPair,
      hydra.lib.sets.ToList.apply(nss.get())))));
  }

  static <T0> hydra.util.Pair<hydra.module.Namespace, T0> namespacesForDefinitions_toPair(java.util.function.Function<hydra.module.Namespace, T0> encodeNamespace, hydra.module.Namespace ns) {
    return (hydra.util.Pair<hydra.module.Namespace, T0>) ((hydra.util.Pair<hydra.module.Namespace, T0>) (new hydra.util.Pair<hydra.module.Namespace, T0>(ns, (encodeNamespace).apply(ns))));
  }
}
