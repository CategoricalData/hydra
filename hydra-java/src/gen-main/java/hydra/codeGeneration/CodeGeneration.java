// Note: this is an automatically generated file. Do not edit.

package hydra.codeGeneration;

/**
 * Pure code generation pipeline for bootstrapping Hydra across languages.
 */
public interface CodeGeneration {
  static String namespaceToPath(hydra.module.Namespace ns) {
    return hydra.lib.strings.Intercalate.apply(
      "/",
      hydra.lib.strings.SplitOn.apply(
        ".",
        (ns).value));
  }

  static hydra.module.Module stripModuleTypeSchemes(hydra.module.Module m) {
    java.util.function.Function<hydra.core.Binding, hydra.core.Binding> stripIfTerm = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> hydra.lib.logic.IfElse.lazy(
      hydra.annotations.Annotations.isNativeType(b),
      () -> b,
      () -> new hydra.core.Binding((b).name, (b).term, (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))));
    return new hydra.module.Module((m).namespace, hydra.lib.lists.Map.apply(
      stripIfTerm,
      (m).elements), (m).termDependencies, (m).typeDependencies, (m).description);
  }

  static hydra.util.PersistentSet<hydra.module.Namespace> transitiveDeps(java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.module.Namespace>> getDeps, hydra.util.PersistentMap<hydra.module.Namespace, hydra.module.Module> nsMap, hydra.util.ConsList<hydra.module.Module> startMods) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.PersistentSet<hydra.module.Namespace>, java.util.function.Function<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.PersistentSet<hydra.module.Namespace>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<hydra.util.PersistentSet<hydra.module.Namespace>, java.util.function.Function<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.PersistentSet<hydra.module.Namespace>>>) (pending -> (java.util.function.Function<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.PersistentSet<hydra.module.Namespace>>) (visited -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Null.apply(pending),
      () -> visited,
      () -> ((java.util.function.Supplier<hydra.util.PersistentSet<hydra.module.Namespace>>) (() -> {
        hydra.util.Lazy<hydra.util.PersistentSet<hydra.module.Namespace>> newVisited = new hydra.util.Lazy<>(() -> hydra.lib.sets.Union.apply(
          visited,
          pending));
        return ((java.util.function.Supplier<hydra.util.PersistentSet<hydra.module.Namespace>>) (() -> {
          hydra.util.Lazy<hydra.util.PersistentSet<hydra.module.Namespace>> nextDeps = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.module.Namespace, hydra.util.ConsList<hydra.module.Namespace>>) (nsv -> hydra.lib.maybes.Maybe.applyLazy(
              () -> (hydra.util.ConsList<hydra.module.Namespace>) (hydra.util.ConsList.<hydra.module.Namespace>empty()),
              (java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.module.Namespace>>) (depMod -> (getDeps).apply(depMod)),
              hydra.lib.maps.Lookup.apply(
                nsv,
                nsMap))),
            hydra.lib.sets.ToList.apply(pending)))));
          return ((java.util.function.Supplier<hydra.util.PersistentSet<hydra.module.Namespace>>) (() -> {
            hydra.util.Lazy<hydra.util.PersistentSet<hydra.module.Namespace>> newPending = new hydra.util.Lazy<>(() -> hydra.lib.sets.Difference.apply(
              nextDeps.get(),
              newVisited.get()));
            return go.get().apply(newPending.get()).apply(newVisited.get());
          })).get();
        })).get();
      })).get()))));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.module.Namespace>> initialDeps = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.module.Namespace>>) (m -> hydra.lib.lists.Filter.apply(
        (java.util.function.Function<hydra.module.Namespace, Boolean>) (dep -> hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          dep,
          (m).namespace))),
        (getDeps).apply(m))),
      startMods))));
    return go.get().apply(initialDeps.get()).apply((hydra.util.PersistentSet<hydra.module.Namespace>) (hydra.lib.sets.Empty.<hydra.module.Namespace>apply()));
  }

  static hydra.util.ConsList<hydra.module.Module> moduleTermDepsTransitive(hydra.util.PersistentMap<hydra.module.Namespace, hydra.module.Module> nsMap, hydra.util.ConsList<hydra.module.Module> modules) {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.module.Namespace>> closure = new hydra.util.Lazy<>(() -> hydra.lib.sets.Union.apply(
      hydra.codeGeneration.CodeGeneration.transitiveDeps(
        (java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.module.Namespace>>) (m -> (m).termDependencies),
        nsMap,
        modules),
      hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, hydra.module.Namespace>) (m -> (m).namespace),
        modules))));
    return hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Namespace, hydra.util.Maybe<hydra.module.Module>>) (n -> hydra.lib.maps.Lookup.apply(
        n,
        nsMap)),
      hydra.lib.sets.ToList.apply(closure.get())));
  }

  static hydra.util.ConsList<hydra.module.Module> moduleTypeDepsTransitive(hydra.util.PersistentMap<hydra.module.Namespace, hydra.module.Module> nsMap, hydra.util.ConsList<hydra.module.Module> modules) {
    hydra.util.ConsList<hydra.module.Module> termMods = hydra.codeGeneration.CodeGeneration.moduleTermDepsTransitive(
      nsMap,
      modules);
    hydra.util.Lazy<hydra.util.ConsList<hydra.module.Namespace>> typeNamespaces = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(hydra.codeGeneration.CodeGeneration.transitiveDeps(
      (java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.module.Namespace>>) (m -> (m).typeDependencies),
      nsMap,
      termMods)));
    return hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Namespace, hydra.util.Maybe<hydra.module.Module>>) (n -> hydra.lib.maps.Lookup.apply(
        n,
        nsMap)),
      typeNamespaces.get()));
  }

  static hydra.graph.Graph modulesToGraph(hydra.graph.Graph bsGraph, hydra.util.ConsList<hydra.module.Module> universeModules, hydra.util.ConsList<hydra.module.Module> modules) {
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.module.Namespace, hydra.module.Module>> universe = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Module, hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>>) (m -> (hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>) ((hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>) (new hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>((m).namespace, m)))),
      hydra.lib.lists.Concat2.apply(
        universeModules,
        modules))));
    hydra.util.ConsList<hydra.module.Module> dataModules = hydra.codeGeneration.CodeGeneration.moduleTermDepsTransitive(
      universe.get(),
      modules);
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.lib.logic.Not.apply(hydra.annotations.Annotations.isNativeType(e))),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.core.Binding>>) (m -> (m).elements),
        dataModules))));
    hydra.util.ConsList<hydra.module.Module> schemaModules = hydra.codeGeneration.CodeGeneration.moduleTypeDepsTransitive(
      universe.get(),
      modules);
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> schemaElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.annotations.Annotations.isNativeType(e)),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.core.Binding>>) (m -> (m).elements),
        hydra.lib.lists.Concat2.apply(
          schemaModules,
          modules)))));
    hydra.util.Lazy<hydra.graph.Graph> schemaGraph = new hydra.util.Lazy<>(() -> hydra.lexical.Lexical.elementsToGraph(
      bsGraph,
      (hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())),
      schemaElements.get()));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>> schemaTypes = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>>) (ignored -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply()))),
      (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>>) (_r -> _r),
      hydra.schemas.Schemas.schemaGraphToTypingEnvironment(
        hydra.lexical.Lexical.emptyContext(),
        schemaGraph.get())));
    return hydra.lexical.Lexical.elementsToGraph(
      bsGraph,
      schemaTypes.get(),
      dataElements.get());
  }

  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>> generateSourceFiles(java.util.function.Function<hydra.module.Module, java.util.function.Function<hydra.util.ConsList<hydra.module.Definition>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<T0, T1>>>>>> printDefinitions, hydra.coders.Language lang, Boolean doInfer, Boolean doExpand, Boolean doHoistCaseStatements, Boolean doHoistPolymorphicLetBindings, hydra.graph.Graph bsGraph, hydra.util.ConsList<hydra.module.Module> universeModules, hydra.util.ConsList<hydra.module.Module> modsToGenerate, hydra.context.Context cx) {
    hydra.coders.LanguageConstraints constraints = (lang).constraints;
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.module.Namespace, hydra.module.Module>> namespaceMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Module, hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>>) (m -> (hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>) ((hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>) (new hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>((m).namespace, m)))),
      hydra.lib.lists.Concat2.apply(
        universeModules,
        modsToGenerate))));
    hydra.util.ConsList<hydra.module.Module> dataMods = hydra.codeGeneration.CodeGeneration.moduleTermDepsTransitive(
      namespaceMap.get(),
      modsToGenerate);
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.core.Binding>>) (m -> (m).elements),
      dataMods)));
    hydra.util.ConsList<hydra.module.Module> schemaMods = hydra.codeGeneration.CodeGeneration.moduleTypeDepsTransitive(
      namespaceMap.get(),
      modsToGenerate);
    java.util.function.Function<hydra.module.Module, Boolean> isTypeModule = (java.util.function.Function<hydra.module.Module, Boolean>) (mod -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.annotations.Annotations.isNativeType(e)),
      (mod).elements))));
    hydra.util.Lazy<hydra.util.Pair<hydra.util.ConsList<hydra.module.Module>, hydra.util.ConsList<hydra.module.Module>>> partitioned = new hydra.util.Lazy<>(() -> hydra.lib.lists.Partition.apply(
      isTypeModule,
      modsToGenerate));
    hydra.util.Lazy<hydra.util.ConsList<hydra.module.Module>> typeModulesToGenerate = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(partitioned.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> schemaElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.annotations.Annotations.isNativeType(e)),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.core.Binding>>) (m -> (m).elements),
        hydra.lib.lists.Concat2.apply(
          schemaMods,
          typeModulesToGenerate.get())))));
    hydra.util.Lazy<hydra.graph.Graph> schemaGraph = new hydra.util.Lazy<>(() -> hydra.lexical.Lexical.elementsToGraph(
      bsGraph,
      (hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())),
      schemaElements.get()));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>> schemaTypes2 = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>>) (ignored -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply()))),
      (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>>) (_r -> _r),
      hydra.schemas.Schemas.schemaGraphToTypingEnvironment(
        hydra.lexical.Lexical.emptyContext(),
        schemaGraph.get())));
    hydra.graph.Graph dataGraph = hydra.lexical.Lexical.elementsToGraph(
      bsGraph,
      schemaTypes2.get(),
      dataElements.get());
    hydra.util.Lazy<hydra.util.ConsList<hydra.module.Module>> termModulesToGenerate = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(partitioned.get()));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(typeModulesToGenerate.get()),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>right((hydra.util.ConsList<hydra.util.Pair<T0, T1>>) (hydra.util.ConsList.<hydra.util.Pair<T0, T1>>empty())),
        () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>>) (() -> {
          hydra.util.Lazy<hydra.util.ConsList<hydra.util.ConsList<hydra.core.Name>>> nameLists = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.core.Name>>) (m -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Binding, hydra.core.Name>) (e -> (e).name),
              hydra.lib.lists.Filter.apply(
                (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.annotations.Annotations.isNativeType(e)),
                (m).elements))),
            typeModulesToGenerate.get()));
          return hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.Bimap.apply(
              (java.util.function.Function<String, hydra.context.InContext<hydra.errors.Error_>>) (s -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(s)), cx))),
              (java.util.function.Function<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>, hydra.util.ConsList<hydra.util.ConsList<hydra.module.TypeDefinition>>>, hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>, hydra.util.ConsList<hydra.util.ConsList<hydra.module.TypeDefinition>>>>) (r -> r),
              hydra.adapt.Adapt.schemaGraphToDefinitions(
                constraints,
                schemaGraph.get(),
                nameLists.get(),
                cx)),
            (java.util.function.Function<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>, hydra.util.ConsList<hydra.util.ConsList<hydra.module.TypeDefinition>>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>>) (schemaResult -> {
              hydra.util.Lazy<hydra.util.ConsList<hydra.util.ConsList<hydra.module.TypeDefinition>>> defLists = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(schemaResult));
              hydra.graph.Graph schemaGraphWithTypes = new hydra.graph.Graph(schemaGraph.get().boundTerms, schemaGraph.get().boundTypes, schemaGraph.get().classConstraints, schemaGraph.get().lambdaVariables, schemaGraph.get().metadata, schemaGraph.get().primitives, schemaTypes2.get(), schemaGraph.get().typeVariables);
              return hydra.lib.eithers.Map.apply(
                (java.util.function.Function<hydra.util.ConsList<hydra.util.ConsList<hydra.util.Pair<T0, T1>>>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>) (xs -> hydra.lib.lists.Concat.apply(xs)),
                hydra.lib.eithers.MapList.apply(
                  (java.util.function.Function<hydra.util.Pair<hydra.module.Module, hydra.util.ConsList<hydra.module.TypeDefinition>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>>) (p -> {
                    hydra.util.Lazy<hydra.util.ConsList<hydra.module.TypeDefinition>> defs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
                    hydra.util.Lazy<hydra.module.Module> mod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
                    return hydra.lib.eithers.Map.apply(
                      (java.util.function.Function<hydra.util.PersistentMap<T0, T1>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>) (m -> hydra.lib.maps.ToList.apply(m)),
                      (printDefinitions).apply(mod.get()).apply(hydra.lib.lists.Map.apply(
                        (java.util.function.Function<hydra.module.TypeDefinition, hydra.module.Definition>) (d -> new hydra.module.Definition.Type(d)),
                        defs.get())).apply(cx).apply(schemaGraphWithTypes));
                  }),
                  hydra.lib.lists.Zip.apply(
                    typeModulesToGenerate.get(),
                    defLists.get())));
            }));
        })).get()),
      (java.util.function.Function<hydra.util.ConsList<hydra.util.Pair<T0, T1>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>>) (schemaFiles -> hydra.lib.eithers.Bind.apply(
        hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(termModulesToGenerate.get()),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>right((hydra.util.ConsList<hydra.util.Pair<T0, T1>>) (hydra.util.ConsList.<hydra.util.Pair<T0, T1>>empty())),
          () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>>) (() -> {
            hydra.util.Lazy<hydra.util.ConsList<hydra.module.Namespace>> namespaces = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.module.Module, hydra.module.Namespace>) (m -> (m).namespace),
              termModulesToGenerate.get()));
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.Bimap.apply(
                (java.util.function.Function<String, hydra.context.InContext<hydra.errors.Error_>>) (s -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(s)), cx))),
                (java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, hydra.util.ConsList<hydra.util.ConsList<hydra.module.TermDefinition>>>, hydra.util.Pair<hydra.graph.Graph, hydra.util.ConsList<hydra.util.ConsList<hydra.module.TermDefinition>>>>) (r -> r),
                hydra.adapt.Adapt.dataGraphToDefinitions(
                  constraints,
                  doInfer,
                  doExpand,
                  doHoistCaseStatements,
                  doHoistPolymorphicLetBindings,
                  dataElements.get(),
                  dataGraph,
                  namespaces.get(),
                  cx)),
              (java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, hydra.util.ConsList<hydra.util.ConsList<hydra.module.TermDefinition>>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>>) (dataResult -> {
                hydra.util.Lazy<hydra.graph.Graph> g1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(dataResult));
                hydra.util.ConsList<hydra.core.Binding> allBindings = hydra.lexical.Lexical.graphToBindings(g1.get());
                hydra.util.Lazy<hydra.util.ConsList<hydra.util.ConsList<hydra.module.TermDefinition>>> defLists = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(dataResult));
                java.util.function.Function<hydra.util.ConsList<hydra.core.Binding>, java.util.function.Function<hydra.module.Module, hydra.module.Module>> refreshModule = (java.util.function.Function<hydra.util.ConsList<hydra.core.Binding>, java.util.function.Function<hydra.module.Module, hydra.module.Module>>) (els -> (java.util.function.Function<hydra.module.Module, hydra.module.Module>) (m -> new hydra.module.Module((m).namespace, hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Binding>>) (e -> hydra.lib.lists.Find.apply(
                    (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
                      (b).name,
                      (e).name)),
                    els)),
                  (m).elements)), (m).termDependencies, (m).typeDependencies, (m).description)));
                hydra.util.Lazy<hydra.util.ConsList<hydra.module.Module>> refreshedMods = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.module.Module, hydra.module.Module>) (m -> (refreshModule).apply(allBindings).apply(m)),
                  termModulesToGenerate.get()));
                return hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<hydra.util.ConsList<hydra.util.ConsList<hydra.util.Pair<T0, T1>>>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>) (xs -> hydra.lib.lists.Concat.apply(xs)),
                  hydra.lib.eithers.MapList.apply(
                    (java.util.function.Function<hydra.util.Pair<hydra.module.Module, hydra.util.ConsList<hydra.module.TermDefinition>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>>) (p -> {
                      hydra.util.Lazy<hydra.util.ConsList<hydra.module.TermDefinition>> defs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
                      hydra.util.Lazy<hydra.module.Module> mod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
                      return hydra.lib.eithers.Map.apply(
                        (java.util.function.Function<hydra.util.PersistentMap<T0, T1>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>) (m -> hydra.lib.maps.ToList.apply(m)),
                        (printDefinitions).apply(mod.get()).apply(hydra.lib.lists.Map.apply(
                          (java.util.function.Function<hydra.module.TermDefinition, hydra.module.Definition>) (d -> new hydra.module.Definition.Term(d)),
                          defs.get())).apply(cx).apply(g1.get()));
                    }),
                    hydra.lib.lists.Zip.apply(
                      refreshedMods.get(),
                      defLists.get())));
              }));
          })).get()),
        (java.util.function.Function<hydra.util.ConsList<hydra.util.Pair<T0, T1>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>>) (termFiles -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.util.Pair<T0, T1>>>right(hydra.lib.lists.Concat2.apply(
          schemaFiles,
          termFiles))))));
  }

  static String formatTermBinding(hydra.core.Binding binding) {
    String name = (binding).name.value;
    hydra.util.Lazy<String> typeStr = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> "?",
      (java.util.function.Function<hydra.core.TypeScheme, String>) (scheme -> hydra.show.core.Core.typeScheme(scheme)),
      (binding).type));
    return hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "  ",
          name),
        " : "),
      typeStr.get());
  }

  static String formatPrimitive(hydra.graph.Primitive prim) {
    String name = (prim).name.value;
    String typeStr = hydra.show.core.Core.typeScheme((prim).type);
    return hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "  ",
          name),
        " : "),
      typeStr);
  }

  static hydra.util.Either<hydra.errors.DecodingError, String> formatTypeBinding(hydra.graph.Graph graph, hydra.core.Binding binding) {
    return hydra.lib.eithers.Bind.apply(
      hydra.decode.core.Core.type(
        graph,
        (binding).term),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, String>>) (typ -> hydra.util.Either.<hydra.errors.DecodingError, String>right(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "  ",
            (binding).name.value),
          " = "),
        hydra.show.core.Core.type(typ)))));
  }

  static hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> buildSchemaMap(hydra.graph.Graph g) {
    return hydra.lib.maps.Map.apply(
      (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type>) (ts -> hydra.rewriting.Rewriting.deannotateType((ts).type)),
      (g).schemaTypes);
  }

  static hydra.module.Module moduleToSourceModule(hydra.module.Module m) {
    hydra.module.Namespace modTypeNs = new hydra.module.Namespace("hydra.module");
    hydra.util.Lazy<hydra.module.Namespace> sourceNs = new hydra.util.Lazy<>(() -> new hydra.module.Namespace(hydra.lib.strings.Cat2.apply(
      "hydra.sources.",
      hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Drop.apply(
          1,
          hydra.lib.strings.SplitOn.apply(
            ".",
            (m).namespace.value))))));
    hydra.util.Lazy<hydra.core.Binding> moduleBinding = new hydra.util.Lazy<>(() -> new hydra.core.Binding(new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      sourceNs.get().value,
      ".module_")), hydra.encode.module.Module.module(m), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
    return new hydra.module.Module(sourceNs.get(), hydra.util.ConsList.of(moduleBinding.get()), hydra.util.ConsList.of(modTypeNs), hydra.util.ConsList.of(modTypeNs), hydra.util.Maybe.just(hydra.lib.strings.Cat2.apply(
      "Source module for ",
      (m).namespace.value)));
  }

  static hydra.util.Either<hydra.errors.DecodingError, String> generateLexicon(hydra.graph.Graph graph) {
    hydra.util.ConsList<hydra.core.Binding> bindings = hydra.lexical.Lexical.graphToBindings(graph);
    hydra.util.Lazy<hydra.util.Pair<hydra.util.ConsList<hydra.core.Binding>, hydra.util.ConsList<hydra.core.Binding>>> partitioned = new hydra.util.Lazy<>(() -> hydra.lib.lists.Partition.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.annotations.Annotations.isNativeType(b)),
      bindings));
    hydra.util.Lazy<hydra.util.ConsList<hydra.graph.Primitive>> primitives = new hydra.util.Lazy<>(() -> hydra.lib.maps.Elems.apply((graph).primitives));
    hydra.util.Lazy<hydra.util.ConsList<hydra.graph.Primitive>> sortedPrimitives = new hydra.util.Lazy<>(() -> hydra.lib.lists.SortOn.apply(
      (java.util.function.Function<hydra.graph.Primitive, hydra.core.Name>) (p -> (p).name),
      primitives.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> termBindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(partitioned.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> sortedTerms = new hydra.util.Lazy<>(() -> hydra.lib.lists.SortOn.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.core.Name>) (b -> (b).name),
      termBindings.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> typeBindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(partitioned.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> sortedTypes = new hydra.util.Lazy<>(() -> hydra.lib.lists.SortOn.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.core.Name>) (b -> (b).name),
      typeBindings.get()));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.DecodingError, String>>) (b -> hydra.codeGeneration.CodeGeneration.formatTypeBinding(
          graph,
          b)),
        sortedTypes.get()),
      (java.util.function.Function<hydra.util.ConsList<String>, hydra.util.Either<hydra.errors.DecodingError, String>>) (typeLines -> {
        hydra.util.Lazy<hydra.util.ConsList<String>> primitiveLines = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.graph.Primitive, String>) (p -> hydra.codeGeneration.CodeGeneration.formatPrimitive(p)),
          sortedPrimitives.get()));
        hydra.util.Lazy<hydra.util.ConsList<String>> termLines = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, String>) (b -> hydra.codeGeneration.CodeGeneration.formatTermBinding(b)),
          sortedTerms.get()));
        return hydra.util.Either.<hydra.errors.DecodingError, String>right(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                hydra.lib.strings.Cat2.apply(
                  "Primitives:\n",
                  hydra.lib.strings.Unlines.apply(primitiveLines.get())),
                "\nTypes:\n"),
              hydra.lib.strings.Unlines.apply(typeLines)),
            "\nTerms:\n"),
          hydra.lib.strings.Unlines.apply(termLines.get())));
      }));
  }

  static hydra.util.Either<String, String> moduleToJson(hydra.module.Module m) {
    hydra.core.Term term = hydra.encode.module.Module.module(m);
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.json.writer.Writer.printJson(json)),
      hydra.json.encode.Encode.toJson(term));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.module.Module>> inferModules(hydra.context.Context cx, hydra.graph.Graph bsGraph, hydra.util.ConsList<hydra.module.Module> universeMods, hydra.util.ConsList<hydra.module.Module> targetMods) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.lib.logic.Not.apply(hydra.annotations.Annotations.isNativeType(e))),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.core.Binding>>) (m -> (m).elements),
        universeMods))));
    hydra.graph.Graph g0 = hydra.codeGeneration.CodeGeneration.modulesToGraph(
      bsGraph,
      universeMods,
      universeMods);
    return hydra.lib.eithers.Bind.apply(
      hydra.inference.Inference.inferGraphTypes(
        cx,
        dataElements.get(),
        g0),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, hydra.util.ConsList<hydra.core.Binding>>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.module.Module>>>) (inferResultWithCx -> {
        hydra.util.Lazy<hydra.util.Pair<hydra.graph.Graph, hydra.util.ConsList<hydra.core.Binding>>> inferResult = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(inferResultWithCx));
        hydra.util.Lazy<hydra.graph.Graph> g1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(inferResult.get()));
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> inferredElements = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(inferResult.get()));
        java.util.function.Function<hydra.module.Module, Boolean> isTypeModule = (java.util.function.Function<hydra.module.Module, Boolean>) (mod -> hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.lib.logic.Not.apply(hydra.annotations.Annotations.isNativeType(e))),
          (mod).elements)));
        java.util.function.Function<hydra.module.Module, hydra.module.Module> refreshModule = (java.util.function.Function<hydra.module.Module, hydra.module.Module>) (m -> hydra.lib.logic.IfElse.lazy(
          (isTypeModule).apply(m),
          () -> m,
          () -> new hydra.module.Module((m).namespace, hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Binding>>) (e -> hydra.lib.lists.Find.apply(
              (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
                (b).name,
                (e).name)),
              inferredElements.get())),
            (m).elements)), (m).termDependencies, (m).typeDependencies, (m).description)));
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.ConsList<hydra.module.Module>>right(hydra.lib.lists.Map.apply(
          refreshModule,
          targetMods));
      }));
  }

  static <T0, T1, T2, T3> hydra.util.Either<T2, hydra.util.ConsList<T3>> generateCoderModules(java.util.function.Function<T0, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T1, hydra.util.Either<T2, hydra.util.Maybe<T3>>>>> codec, hydra.graph.Graph bsGraph, hydra.util.ConsList<hydra.module.Module> universeModules, hydra.util.ConsList<T1> typeModules, T0 cx) {
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.module.Namespace, hydra.module.Module>> universe = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Module, hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>>) (m -> (hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>) ((hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>) (new hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>((m).namespace, m)))),
      hydra.lib.lists.Concat2.apply(
        universeModules,
        universeModules))));
    hydra.util.ConsList<hydra.module.Module> dataModules = hydra.codeGeneration.CodeGeneration.moduleTermDepsTransitive(
      universe.get(),
      universeModules);
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.lib.logic.Not.apply(hydra.annotations.Annotations.isNativeType(e))),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.core.Binding>>) (m -> (m).elements),
        dataModules))));
    hydra.util.ConsList<hydra.module.Module> schemaModules = hydra.codeGeneration.CodeGeneration.moduleTypeDepsTransitive(
      universe.get(),
      universeModules);
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> schemaElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.annotations.Annotations.isNativeType(e)),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.core.Binding>>) (m -> (m).elements),
        hydra.lib.lists.Concat2.apply(
          schemaModules,
          universeModules)))));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> allElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      schemaElements.get(),
      dataElements.get()));
    hydra.util.Lazy<hydra.graph.Graph> schemaGraph = new hydra.util.Lazy<>(() -> hydra.lexical.Lexical.elementsToGraph(
      bsGraph,
      (hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())),
      schemaElements.get()));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>> schemaTypes = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>>) (ignored -> (hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply()))),
      (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>>) (_r -> _r),
      hydra.schemas.Schemas.schemaGraphToTypingEnvironment(
        hydra.lexical.Lexical.emptyContext(),
        schemaGraph.get())));
    hydra.graph.Graph graph = hydra.lexical.Lexical.elementsToGraph(
      bsGraph,
      schemaTypes.get(),
      allElements.get());
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.ConsList<hydra.util.Maybe<T3>>, hydra.util.ConsList<T3>>) (results -> hydra.lib.maybes.Cat.apply(results)),
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<T1, hydra.util.Either<T2, hydra.util.Maybe<T3>>>) (m -> (codec).apply(cx).apply(graph).apply(m)),
        typeModules));
  }

  static hydra.util.Either<String, String> inferAndGenerateLexicon(hydra.context.Context cx, hydra.graph.Graph bsGraph, hydra.util.ConsList<hydra.module.Module> kernelModules) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.lib.logic.Not.apply(hydra.annotations.Annotations.isNativeType(e))),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, hydra.util.ConsList<hydra.core.Binding>>) (m -> (m).elements),
        kernelModules))));
    hydra.graph.Graph g0 = hydra.codeGeneration.CodeGeneration.modulesToGraph(
      bsGraph,
      kernelModules,
      kernelModules);
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ic -> hydra.show.errors.Errors.error(((java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.errors.Error_>) (projected -> projected.object)).apply(ic))),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, hydra.util.ConsList<hydra.core.Binding>>, hydra.context.Context>, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, hydra.util.ConsList<hydra.core.Binding>>, hydra.context.Context>>) (x -> x),
        hydra.inference.Inference.inferGraphTypes(
          cx,
          dataElements.get(),
          g0)),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, hydra.util.ConsList<hydra.core.Binding>>, hydra.context.Context>, hydra.util.Either<String, String>>) (inferResultWithCx -> {
        hydra.util.Lazy<hydra.graph.Graph> g1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(inferResultWithCx)));
        return hydra.lib.eithers.Bimap.apply(
          wrapped -> (wrapped).value,
          (java.util.function.Function<String, String>) (x -> x),
          hydra.codeGeneration.CodeGeneration.generateLexicon(g1.get()));
      }));
  }

  static hydra.util.ConsList<Integer> escapeControlCharsInJson(hydra.util.ConsList<Integer> input) {
    java.util.function.Function<Integer, Integer> hexDigit = (java.util.function.Function<Integer, Integer>) (n -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Lt.apply(
        n,
        10),
      () -> hydra.lib.math.Add.apply(
        48,
        n),
      () -> hydra.lib.math.Add.apply(
        97,
        hydra.lib.math.Sub.apply(
          n,
          10))));
    java.util.function.Function<Integer, hydra.util.ConsList<Integer>> escapeToUnicode = (java.util.function.Function<Integer, hydra.util.ConsList<Integer>>) (b -> hydra.util.ConsList.of(
      92,
      117,
      48,
      48,
      (hexDigit).apply(hydra.lib.math.Div.apply(
        b,
        16)),
      (hexDigit).apply(hydra.lib.math.Mod.apply(
        b,
        16))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<hydra.util.ConsList<Integer>, hydra.util.ConsList<Integer>>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<hydra.util.ConsList<Integer>, hydra.util.ConsList<Integer>>>>) (inStr -> (java.util.function.Function<Boolean, java.util.function.Function<hydra.util.ConsList<Integer>, hydra.util.ConsList<Integer>>>) (esc -> (java.util.function.Function<hydra.util.ConsList<Integer>, hydra.util.ConsList<Integer>>) (bytes -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(bytes),
      () -> (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()),
      () -> ((java.util.function.Supplier<hydra.util.ConsList<Integer>>) (() -> {
        hydra.util.Lazy<Integer> b = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(bytes));
        return ((java.util.function.Supplier<hydra.util.ConsList<Integer>>) (() -> {
          hydra.util.Lazy<hydra.util.ConsList<Integer>> bs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(bytes));
          return hydra.lib.logic.IfElse.lazy(
            esc,
            () -> hydra.lib.lists.Cons.apply(
              b.get(),
              go.get().apply(inStr).apply(false).apply(bs.get())),
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.And.apply(
                hydra.lib.equality.Equal.apply(
                  b.get(),
                  92),
                inStr),
              () -> hydra.lib.lists.Cons.apply(
                b.get(),
                go.get().apply(inStr).apply(true).apply(bs.get())),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  b.get(),
                  34),
                () -> hydra.lib.lists.Cons.apply(
                  b.get(),
                  go.get().apply(hydra.lib.logic.Not.apply(inStr)).apply(false).apply(bs.get())),
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.logic.And.apply(
                    inStr,
                    hydra.lib.equality.Lt.apply(
                      b.get(),
                      32)),
                  () -> hydra.lib.lists.Concat2.apply(
                    (escapeToUnicode).apply(b.get()),
                    go.get().apply(inStr).apply(false).apply(bs.get())),
                  () -> hydra.lib.lists.Cons.apply(
                    b.get(),
                    go.get().apply(inStr).apply(false).apply(bs.get()))))));
        })).get();
      })).get())))));
    return go.get().apply(false).apply(false).apply(input);
  }

  static hydra.util.Either<String, hydra.module.Module> decodeModuleFromJson(hydra.graph.Graph bsGraph, hydra.util.ConsList<hydra.module.Module> universeModules, Boolean doStripTypeSchemes, hydra.json.model.Value jsonVal) {
    hydra.graph.Graph graph = hydra.codeGeneration.CodeGeneration.modulesToGraph(
      bsGraph,
      universeModules,
      universeModules);
    hydra.core.Type modType = new hydra.core.Type.Variable(new hydra.core.Name("hydra.module.Module"));
    hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> schemaMap = hydra.codeGeneration.CodeGeneration.buildSchemaMap(graph);
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<String, hydra.module.Module>>) (err -> hydra.util.Either.<String, hydra.module.Module>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.module.Module>>) (term -> hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<String, hydra.module.Module>>) (decErr -> hydra.util.Either.<String, hydra.module.Module>left((decErr).value)),
        (java.util.function.Function<hydra.module.Module, hydra.util.Either<String, hydra.module.Module>>) (mod -> hydra.util.Either.<String, hydra.module.Module>right(hydra.lib.logic.IfElse.lazy(
          doStripTypeSchemes,
          () -> hydra.codeGeneration.CodeGeneration.stripModuleTypeSchemes(mod),
          () -> mod))),
        hydra.decode.module.Module.module(
          graph,
          term))),
      hydra.json.decode.Decode.fromJson(
        schemaMap,
        new hydra.core.Name("hydra.module.Module"),
        modType,
        jsonVal));
  }
}
