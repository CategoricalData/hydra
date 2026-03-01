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
  
  static java.util.Set<hydra.module.Namespace> transitiveDeps(java.util.function.Function<hydra.module.Module, java.util.List<hydra.module.Namespace>> getDeps, java.util.Map<hydra.module.Namespace, hydra.module.Module> nsMap, java.util.List<hydra.module.Module> startMods) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.Set<hydra.module.Namespace>, java.util.function.Function<java.util.Set<hydra.module.Namespace>, java.util.Set<hydra.module.Namespace>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<java.util.Set<hydra.module.Namespace>, java.util.function.Function<java.util.Set<hydra.module.Namespace>, java.util.Set<hydra.module.Namespace>>>) (pending -> (java.util.function.Function<java.util.Set<hydra.module.Namespace>, java.util.Set<hydra.module.Namespace>>) (visited -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Null.apply(pending),
      () -> visited,
      () -> ((java.util.function.Supplier<java.util.Set<hydra.module.Namespace>>) (() -> {
        hydra.util.Lazy<java.util.Set<hydra.module.Namespace>> newVisited = new hydra.util.Lazy<>(() -> hydra.lib.sets.Union.apply(
          visited,
          pending));
        return ((java.util.function.Supplier<java.util.Set<hydra.module.Namespace>>) (() -> {
          hydra.util.Lazy<java.util.Set<hydra.module.Namespace>> nextDeps = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.module.Namespace, java.util.List<hydra.module.Namespace>>) (nsv -> hydra.lib.maybes.Maybe.apply(
              (java.util.List<hydra.module.Namespace>) (java.util.List.<hydra.module.Namespace>of()),
              (java.util.function.Function<hydra.module.Module, java.util.List<hydra.module.Namespace>>) (depMod -> (getDeps).apply(depMod)),
              hydra.lib.maps.Lookup.apply(
                nsv,
                nsMap))),
            hydra.lib.sets.ToList.apply(pending)))));
          return ((java.util.function.Supplier<java.util.Set<hydra.module.Namespace>>) (() -> {
            hydra.util.Lazy<java.util.Set<hydra.module.Namespace>> newPending = new hydra.util.Lazy<>(() -> hydra.lib.sets.Difference.apply(
              nextDeps.get(),
              newVisited.get()));
            return ((go.get()).apply(newPending.get())).apply(newVisited.get());
          })).get();
        })).get();
      })).get()))));
    hydra.util.Lazy<java.util.Set<hydra.module.Namespace>> initialDeps = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Module, java.util.List<hydra.module.Namespace>>) (m -> hydra.lib.lists.Filter.apply(
        (java.util.function.Function<hydra.module.Namespace, Boolean>) (dep -> hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          dep,
          (m).namespace))),
        (getDeps).apply(m))),
      startMods))));
    return ((go.get()).apply(initialDeps.get())).apply((java.util.Set<hydra.module.Namespace>) (hydra.lib.sets.Empty.<hydra.module.Namespace>apply()));
  }
  
  static java.util.List<hydra.module.Module> moduleTermDepsTransitive(java.util.Map<hydra.module.Namespace, hydra.module.Module> nsMap, java.util.List<hydra.module.Module> modules) {
    hydra.util.Lazy<java.util.Set<hydra.module.Namespace>> closure = new hydra.util.Lazy<>(() -> hydra.lib.sets.Union.apply(
      hydra.codeGeneration.CodeGeneration.transitiveDeps(
        (java.util.function.Function<hydra.module.Module, java.util.List<hydra.module.Namespace>>) (m -> (m).termDependencies),
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
  
  static java.util.List<hydra.module.Module> moduleTypeDepsTransitive(java.util.Map<hydra.module.Namespace, hydra.module.Module> nsMap, java.util.List<hydra.module.Module> modules) {
    java.util.List<hydra.module.Module> termMods = hydra.codeGeneration.CodeGeneration.moduleTermDepsTransitive(
      nsMap,
      modules);
    hydra.util.Lazy<java.util.List<hydra.module.Namespace>> typeNamespaces = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(hydra.codeGeneration.CodeGeneration.transitiveDeps(
      (java.util.function.Function<hydra.module.Module, java.util.List<hydra.module.Namespace>>) (m -> (m).typeDependencies),
      nsMap,
      termMods)));
    return hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Namespace, hydra.util.Maybe<hydra.module.Module>>) (n -> hydra.lib.maps.Lookup.apply(
        n,
        nsMap)),
      typeNamespaces.get()));
  }
  
  static hydra.graph.Graph modulesToGraph(hydra.graph.Graph bsGraph, java.util.List<hydra.module.Module> universeModules, java.util.List<hydra.module.Module> modules) {
    hydra.util.Lazy<java.util.Map<hydra.module.Namespace, hydra.module.Module>> universe = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Module, hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>>) (m -> (hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>) ((hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>) (new hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>((m).namespace, m)))),
      hydra.lib.lists.Concat2.apply(
        universeModules,
        modules))));
    java.util.List<hydra.module.Module> dataModules = hydra.codeGeneration.CodeGeneration.moduleTermDepsTransitive(
      universe.get(),
      modules);
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.lib.logic.Not.apply(hydra.annotations.Annotations.isNativeType(e))),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, java.util.List<hydra.core.Binding>>) (m -> (m).elements),
        dataModules))));
    java.util.List<hydra.module.Module> schemaModules = hydra.codeGeneration.CodeGeneration.moduleTypeDepsTransitive(
      universe.get(),
      modules);
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> schemaElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.annotations.Annotations.isNativeType(e)),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, java.util.List<hydra.core.Binding>>) (m -> (m).elements),
        hydra.lib.lists.Concat2.apply(
          schemaModules,
          modules)))));
    hydra.util.Lazy<hydra.graph.Graph> schemaGraph = new hydra.util.Lazy<>(() -> hydra.lexical.Lexical.elementsToGraph(
      bsGraph,
      (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())),
      schemaElements.get()));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> schemaTypes = new hydra.util.Lazy<>(() -> hydra.monads.Monads.fromFlow(
      (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())),
      schemaGraph.get(),
      hydra.schemas.Schemas.schemaGraphToTypingEnvironment(schemaGraph.get())));
    return hydra.lexical.Lexical.elementsToGraph(
      bsGraph,
      schemaTypes.get(),
      dataElements.get());
  }
  
  static <T0, T1> hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.util.Pair<T0, T1>>> generateSourceFiles(java.util.function.Function<hydra.module.Module, java.util.function.Function<java.util.List<hydra.module.Definition>, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<T0, T1>>>> printDefinitions, hydra.coders.Language lang, Boolean doInfer, Boolean doExpand, Boolean doHoistCaseStatements, Boolean doHoistPolymorphicLetBindings, hydra.graph.Graph bsGraph, java.util.List<hydra.module.Module> universeModules, java.util.List<hydra.module.Module> modsToGenerate) {
    hydra.coders.LanguageConstraints constraints = (lang).constraints;
    hydra.util.Lazy<java.util.Map<hydra.module.Namespace, hydra.module.Module>> namespaceMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Module, hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>>) (m -> (hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>) ((hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>) (new hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>((m).namespace, m)))),
      hydra.lib.lists.Concat2.apply(
        universeModules,
        modsToGenerate))));
    java.util.List<hydra.module.Module> dataMods = hydra.codeGeneration.CodeGeneration.moduleTermDepsTransitive(
      namespaceMap.get(),
      modsToGenerate);
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Module, java.util.List<hydra.core.Binding>>) (m -> (m).elements),
      dataMods)));
    java.util.List<hydra.module.Module> schemaMods = hydra.codeGeneration.CodeGeneration.moduleTypeDepsTransitive(
      namespaceMap.get(),
      modsToGenerate);
    java.util.function.Function<hydra.module.Module, Boolean> isTypeModule = (java.util.function.Function<hydra.module.Module, Boolean>) (mod -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.annotations.Annotations.isNativeType(e)),
      (mod).elements))));
    hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.module.Module>, java.util.List<hydra.module.Module>>> partitioned = new hydra.util.Lazy<>(() -> hydra.lib.lists.Partition.apply(
      isTypeModule,
      modsToGenerate));
    hydra.util.Lazy<java.util.List<hydra.module.Module>> typeModulesToGenerate = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(partitioned.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> schemaElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.annotations.Annotations.isNativeType(e)),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, java.util.List<hydra.core.Binding>>) (m -> (m).elements),
        hydra.lib.lists.Concat2.apply(
          schemaMods,
          typeModulesToGenerate.get())))));
    hydra.util.Lazy<hydra.graph.Graph> schemaGraph = new hydra.util.Lazy<>(() -> hydra.lexical.Lexical.elementsToGraph(
      bsGraph,
      (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())),
      schemaElements.get()));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> schemaTypes2 = new hydra.util.Lazy<>(() -> hydra.monads.Monads.fromFlow(
      (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())),
      schemaGraph.get(),
      hydra.schemas.Schemas.schemaGraphToTypingEnvironment(schemaGraph.get())));
    hydra.graph.Graph dataGraph = hydra.lexical.Lexical.elementsToGraph(
      bsGraph,
      schemaTypes2.get(),
      dataElements.get());
    hydra.util.Lazy<java.util.List<hydra.module.Module>> termModulesToGenerate = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(partitioned.get()));
    return hydra.lib.flows.Bind.apply(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(typeModulesToGenerate.get()),
        () -> hydra.lib.flows.Pure.apply((java.util.List<hydra.util.Pair<T0, T1>>) (java.util.List.<hydra.util.Pair<T0, T1>>of())),
        () -> hydra.monads.Monads.withTrace(
          "generate type modules",
          ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.util.Pair<T0, T1>>>>) (() -> {
            hydra.util.Lazy<java.util.List<java.util.List<hydra.core.Name>>> nameLists = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.module.Module, java.util.List<hydra.core.Name>>) (m -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Binding, hydra.core.Name>) (e -> (e).name),
                hydra.lib.lists.Filter.apply(
                  (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.annotations.Annotations.isNativeType(e)),
                  (m).elements))),
              typeModulesToGenerate.get()));
            return hydra.lib.flows.Bind.apply(
              hydra.adapt.simple.Simple.schemaGraphToDefinitions(
                constraints,
                schemaGraph.get(),
                nameLists.get()),
              (java.util.function.Function<hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.module.TypeDefinition>>>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.util.Pair<T0, T1>>>>) (schemaResult -> {
                hydra.util.Lazy<java.util.List<java.util.List<hydra.module.TypeDefinition>>> defLists = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(schemaResult));
                return hydra.monads.Monads.withState(
                  new hydra.graph.Graph((schemaGraph.get()).boundTerms, (schemaGraph.get()).boundTypes, (schemaGraph.get()).classConstraints, (schemaGraph.get()).lambdaVariables, (schemaGraph.get()).metadata, (schemaGraph.get()).primitives, schemaTypes2.get(), (schemaGraph.get()).typeVariables),
                  hydra.lib.flows.Map.apply(
                    (java.util.function.Function<java.util.List<java.util.List<hydra.util.Pair<T0, T1>>>, java.util.List<hydra.util.Pair<T0, T1>>>) (xs -> hydra.lib.lists.Concat.apply(xs)),
                    hydra.lib.flows.MapList.apply(
                      (java.util.function.Function<hydra.util.Pair<hydra.module.Module, java.util.List<hydra.module.TypeDefinition>>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.util.Pair<T0, T1>>>>) (p -> {
                        hydra.util.Lazy<java.util.List<hydra.module.TypeDefinition>> defs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
                        hydra.util.Lazy<hydra.module.Module> mod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
                        return hydra.monads.Monads.withTrace(
                          hydra.lib.strings.Cat2.apply(
                            "type module ",
                            ((mod.get()).namespace).value),
                          hydra.lib.flows.Map.apply(
                            (java.util.function.Function<java.util.Map<T0, T1>, java.util.List<hydra.util.Pair<T0, T1>>>) (m -> hydra.lib.maps.ToList.apply(m)),
                            ((printDefinitions).apply(mod.get())).apply(hydra.lib.lists.Map.apply(
                              (java.util.function.Function<hydra.module.TypeDefinition, hydra.module.Definition>) (d -> new hydra.module.Definition.Type(d)),
                              defs.get()))));
                      }),
                      hydra.lib.lists.Zip.apply(
                        typeModulesToGenerate.get(),
                        defLists.get()))));
              }));
          })).get())),
      (java.util.function.Function<java.util.List<hydra.util.Pair<T0, T1>>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.util.Pair<T0, T1>>>>) (schemaFiles -> hydra.lib.flows.Bind.apply(
        hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(termModulesToGenerate.get()),
          () -> hydra.lib.flows.Pure.apply((java.util.List<hydra.util.Pair<T0, T1>>) (java.util.List.<hydra.util.Pair<T0, T1>>of())),
          () -> hydra.monads.Monads.withTrace(
            "generate term modules",
            ((java.util.function.Supplier<hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.util.Pair<T0, T1>>>>) (() -> {
              hydra.util.Lazy<java.util.List<hydra.module.Namespace>> namespaces = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.module.Module, hydra.module.Namespace>) (m -> (m).namespace),
                termModulesToGenerate.get()));
              return hydra.lib.flows.Bind.apply(
                hydra.adapt.simple.Simple.dataGraphToDefinitions(
                  constraints,
                  doInfer,
                  doExpand,
                  doHoistCaseStatements,
                  doHoistPolymorphicLetBindings,
                  dataElements.get(),
                  dataGraph,
                  namespaces.get()),
                (java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, java.util.List<java.util.List<hydra.module.TermDefinition>>>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.util.Pair<T0, T1>>>>) (dataResult -> {
                  hydra.util.Lazy<java.util.List<java.util.List<hydra.module.TermDefinition>>> defLists = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(dataResult));
                  hydra.util.Lazy<hydra.graph.Graph> g1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(dataResult));
                  java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.module.Module, hydra.module.Module>> refreshModule = (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.module.Module, hydra.module.Module>>) (els -> (java.util.function.Function<hydra.module.Module, hydra.module.Module>) (m -> new hydra.module.Module((m).namespace, hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Binding>>) (e -> hydra.lib.lists.Find.apply(
                      (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
                        (b).name,
                        (e).name)),
                      els)),
                    (m).elements)), (m).termDependencies, (m).typeDependencies, (m).description)));
                  hydra.util.Lazy<java.util.List<hydra.module.Module>> refreshedMods = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.module.Module, hydra.module.Module>) (m -> ((refreshModule).apply(hydra.lexical.Lexical.graphToBindings(g1.get()))).apply(m)),
                    termModulesToGenerate.get()));
                  return hydra.monads.Monads.withState(
                    g1.get(),
                    hydra.lib.flows.Map.apply(
                      (java.util.function.Function<java.util.List<java.util.List<hydra.util.Pair<T0, T1>>>, java.util.List<hydra.util.Pair<T0, T1>>>) (xs -> hydra.lib.lists.Concat.apply(xs)),
                      hydra.lib.flows.MapList.apply(
                        (java.util.function.Function<hydra.util.Pair<hydra.module.Module, java.util.List<hydra.module.TermDefinition>>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.util.Pair<T0, T1>>>>) (p -> {
                          hydra.util.Lazy<java.util.List<hydra.module.TermDefinition>> defs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
                          hydra.util.Lazy<hydra.module.Module> mod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
                          return hydra.monads.Monads.withTrace(
                            hydra.lib.strings.Cat2.apply(
                              "term module ",
                              ((mod.get()).namespace).value),
                            hydra.lib.flows.Map.apply(
                              (java.util.function.Function<java.util.Map<T0, T1>, java.util.List<hydra.util.Pair<T0, T1>>>) (m -> hydra.lib.maps.ToList.apply(m)),
                              ((printDefinitions).apply(mod.get())).apply(hydra.lib.lists.Map.apply(
                                (java.util.function.Function<hydra.module.TermDefinition, hydra.module.Definition>) (d -> new hydra.module.Definition.Term(d)),
                                defs.get()))));
                        }),
                        hydra.lib.lists.Zip.apply(
                          refreshedMods.get(),
                          defLists.get()))));
                }));
            })).get())),
        (java.util.function.Function<java.util.List<hydra.util.Pair<T0, T1>>, hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.util.Pair<T0, T1>>>>) (termFiles -> hydra.lib.flows.Pure.apply(hydra.lib.lists.Concat2.apply(
          schemaFiles,
          termFiles))))));
  }
  
  static String formatTermBinding(hydra.core.Binding binding) {
    String name = ((binding).name).value;
    hydra.util.Lazy<String> typeStr = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
      "?",
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
    String name = ((prim).name).value;
    String typeStr = hydra.show.core.Core.typeScheme((prim).type);
    return hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "  ",
          name),
        " : "),
      typeStr);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, String> formatTypeBinding(hydra.core.Binding binding) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, String>>) (graph -> hydra.lib.flows.Bind.apply(
        hydra.monads.Monads.eitherToFlow(
          wrapped -> (wrapped).value,
          hydra.decode.core.Core.type(
            graph,
            (binding).term)),
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, String>>) (typ -> hydra.lib.flows.Pure.apply(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "  ",
              ((binding).name).value),
            " = "),
          hydra.show.core.Core.type(typ)))))));
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Type> buildSchemaMap(hydra.graph.Graph g) {
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
            ((m).namespace).value))))));
    hydra.util.Lazy<hydra.core.Binding> moduleBinding = new hydra.util.Lazy<>(() -> new hydra.core.Binding(new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      (sourceNs.get()).value,
      ".module_")), hydra.encode.module.Module.module(m), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
    return new hydra.module.Module(sourceNs.get(), java.util.List.of(moduleBinding.get()), java.util.List.of(modTypeNs), java.util.List.of(modTypeNs), hydra.util.Maybe.just(hydra.lib.strings.Cat2.apply(
      "Source module for ",
      ((m).namespace).value)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, String> generateLexicon(hydra.graph.Graph graph) {
    java.util.List<hydra.core.Binding> bindings = hydra.lexical.Lexical.graphToBindings(graph);
    hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>> partitioned = new hydra.util.Lazy<>(() -> hydra.lib.lists.Partition.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.annotations.Annotations.isNativeType(b)),
      bindings));
    hydra.util.Lazy<java.util.List<hydra.graph.Primitive>> primitives = new hydra.util.Lazy<>(() -> hydra.lib.maps.Elems.apply((graph).primitives));
    hydra.util.Lazy<java.util.List<hydra.graph.Primitive>> sortedPrimitives = new hydra.util.Lazy<>(() -> hydra.lib.lists.SortOn.apply(
      (java.util.function.Function<hydra.graph.Primitive, hydra.core.Name>) (p -> (p).name),
      primitives.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> termBindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(partitioned.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> sortedTerms = new hydra.util.Lazy<>(() -> hydra.lib.lists.SortOn.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.core.Name>) (b -> (b).name),
      termBindings.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> typeBindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(partitioned.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> sortedTypes = new hydra.util.Lazy<>(() -> hydra.lib.lists.SortOn.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.core.Name>) (b -> (b).name),
      typeBindings.get()));
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, String>>) (b -> hydra.codeGeneration.CodeGeneration.formatTypeBinding(b)),
        sortedTypes.get()),
      (java.util.function.Function<java.util.List<String>, hydra.compute.Flow<hydra.graph.Graph, String>>) (typeLines -> {
        hydra.util.Lazy<java.util.List<String>> primitiveLines = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.graph.Primitive, String>) (p -> hydra.codeGeneration.CodeGeneration.formatPrimitive(p)),
          sortedPrimitives.get()));
        hydra.util.Lazy<java.util.List<String>> termLines = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, String>) (b -> hydra.codeGeneration.CodeGeneration.formatTermBinding(b)),
          sortedTerms.get()));
        return hydra.lib.flows.Pure.apply(hydra.lib.strings.Cat2.apply(
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
  
  static <T0> hydra.compute.Flow<T0, java.util.List<hydra.module.Module>> inferModules(hydra.graph.Graph bsGraph, java.util.List<hydra.module.Module> universeMods, java.util.List<hydra.module.Module> targetMods) {
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.lib.logic.Not.apply(hydra.annotations.Annotations.isNativeType(e))),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, java.util.List<hydra.core.Binding>>) (m -> (m).elements),
        universeMods))));
    hydra.graph.Graph g0 = hydra.codeGeneration.CodeGeneration.modulesToGraph(
      bsGraph,
      universeMods,
      universeMods);
    return hydra.lib.flows.Bind.apply(
      hydra.inference.Inference.<T0>inferGraphTypes(
        dataElements.get(),
        g0),
      (java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.compute.Flow<T0, java.util.List<hydra.module.Module>>>) (inferResult -> {
        hydra.util.Lazy<hydra.graph.Graph> g1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(inferResult));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> inferredElements = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(inferResult));
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
        return hydra.lib.flows.Pure.apply(hydra.lib.lists.Map.apply(
          refreshModule,
          targetMods));
      }));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T2, java.util.List<T1>> generateCoderModules(java.util.function.Function<T0, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<T1>>> codec, hydra.graph.Graph bsGraph, java.util.List<hydra.module.Module> universeModules, java.util.List<T0> typeModules) {
    hydra.util.Lazy<java.util.Map<hydra.module.Namespace, hydra.module.Module>> universe = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.Module, hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>>) (m -> (hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>) ((hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>) (new hydra.util.Pair<hydra.module.Namespace, hydra.module.Module>((m).namespace, m)))),
      hydra.lib.lists.Concat2.apply(
        universeModules,
        universeModules))));
    java.util.List<hydra.module.Module> dataModules = hydra.codeGeneration.CodeGeneration.moduleTermDepsTransitive(
      universe.get(),
      universeModules);
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.lib.logic.Not.apply(hydra.annotations.Annotations.isNativeType(e))),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, java.util.List<hydra.core.Binding>>) (m -> (m).elements),
        dataModules))));
    java.util.List<hydra.module.Module> schemaModules = hydra.codeGeneration.CodeGeneration.moduleTypeDepsTransitive(
      universe.get(),
      universeModules);
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> schemaElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.annotations.Annotations.isNativeType(e)),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, java.util.List<hydra.core.Binding>>) (m -> (m).elements),
        hydra.lib.lists.Concat2.apply(
          schemaModules,
          universeModules)))));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> allElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      schemaElements.get(),
      dataElements.get()));
    hydra.util.Lazy<hydra.graph.Graph> schemaGraph = new hydra.util.Lazy<>(() -> hydra.lexical.Lexical.elementsToGraph(
      bsGraph,
      (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())),
      schemaElements.get()));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> schemaTypes = new hydra.util.Lazy<>(() -> hydra.monads.Monads.fromFlow(
      (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())),
      schemaGraph.get(),
      hydra.schemas.Schemas.schemaGraphToTypingEnvironment(schemaGraph.get())));
    hydra.graph.Graph graph = hydra.lexical.Lexical.elementsToGraph(
      bsGraph,
      schemaTypes.get(),
      allElements.get());
    return hydra.monads.Monads.withState(
      graph,
      hydra.lib.flows.Map.apply(
        (java.util.function.Function<java.util.List<hydra.util.Maybe<T1>>, java.util.List<T1>>) (results -> hydra.lib.maybes.Cat.apply(results)),
        hydra.lib.flows.MapList.apply(
          codec,
          typeModules)));
  }
  
  static <T0> hydra.compute.Flow<T0, String> inferAndGenerateLexicon(hydra.graph.Graph bsGraph, java.util.List<hydra.module.Module> kernelModules) {
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (e -> hydra.lib.logic.Not.apply(hydra.annotations.Annotations.isNativeType(e))),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Module, java.util.List<hydra.core.Binding>>) (m -> (m).elements),
        kernelModules))));
    hydra.graph.Graph g0 = hydra.codeGeneration.CodeGeneration.modulesToGraph(
      bsGraph,
      kernelModules,
      kernelModules);
    return hydra.lib.flows.Bind.apply(
      hydra.inference.Inference.<T0>inferGraphTypes(
        dataElements.get(),
        g0),
      (java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.compute.Flow<T0, String>>) (inferResult -> {
        hydra.util.Lazy<hydra.graph.Graph> g1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(inferResult));
        return hydra.monads.Monads.withState(
          g1.get(),
          hydra.codeGeneration.CodeGeneration.generateLexicon(g1.get()));
      }));
  }
  
  static java.util.List<Integer> escapeControlCharsInJson(java.util.List<Integer> input) {
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
    java.util.function.Function<Integer, java.util.List<Integer>> escapeToUnicode = (java.util.function.Function<Integer, java.util.List<Integer>>) (b -> java.util.List.of(
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
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<java.util.List<Integer>, java.util.List<Integer>>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<java.util.List<Integer>, java.util.List<Integer>>>>) (inStr -> (java.util.function.Function<Boolean, java.util.function.Function<java.util.List<Integer>, java.util.List<Integer>>>) (esc -> (java.util.function.Function<java.util.List<Integer>, java.util.List<Integer>>) (bytes -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(bytes),
      () -> (java.util.List<Integer>) (java.util.List.<Integer>of()),
      () -> ((java.util.function.Supplier<java.util.List<Integer>>) (() -> {
        hydra.util.Lazy<Integer> b = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(bytes));
        return ((java.util.function.Supplier<java.util.List<Integer>>) (() -> {
          hydra.util.Lazy<java.util.List<Integer>> bs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(bytes));
          return hydra.lib.logic.IfElse.lazy(
            esc,
            () -> hydra.lib.lists.Cons.apply(
              b.get(),
              (((go.get()).apply(inStr)).apply(false)).apply(bs.get())),
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.And.apply(
                hydra.lib.equality.Equal.apply(
                  b.get(),
                  92),
                inStr),
              () -> hydra.lib.lists.Cons.apply(
                b.get(),
                (((go.get()).apply(inStr)).apply(true)).apply(bs.get())),
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  b.get(),
                  34),
                () -> hydra.lib.lists.Cons.apply(
                  b.get(),
                  (((go.get()).apply(hydra.lib.logic.Not.apply(inStr))).apply(false)).apply(bs.get())),
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.logic.And.apply(
                    inStr,
                    hydra.lib.equality.Lt.apply(
                      b.get(),
                      32)),
                  () -> hydra.lib.lists.Concat2.apply(
                    (escapeToUnicode).apply(b.get()),
                    (((go.get()).apply(inStr)).apply(false)).apply(bs.get())),
                  () -> hydra.lib.lists.Cons.apply(
                    b.get(),
                    (((go.get()).apply(inStr)).apply(false)).apply(bs.get()))))));
        })).get();
      })).get())))));
    return (((go.get()).apply(false)).apply(false)).apply(input);
  }
  
  static hydra.util.Either<String, hydra.module.Module> decodeModuleFromJson(hydra.graph.Graph bsGraph, java.util.List<hydra.module.Module> universeModules, Boolean doStripTypeSchemes, hydra.json.model.Value jsonVal) {
    hydra.graph.Graph graph = hydra.codeGeneration.CodeGeneration.modulesToGraph(
      bsGraph,
      universeModules,
      universeModules);
    hydra.core.Type modType = new hydra.core.Type.Variable(new hydra.core.Name("hydra.module.Module"));
    java.util.Map<hydra.core.Name, hydra.core.Type> schemaMap = hydra.codeGeneration.CodeGeneration.buildSchemaMap(graph);
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<String, hydra.module.Module>>) (err -> (hydra.util.Either<String, hydra.module.Module>) ((hydra.util.Either<String, hydra.module.Module>) (hydra.util.Either.<String, hydra.module.Module>left(err)))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.module.Module>>) (term -> hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.util.DecodingError, hydra.util.Either<String, hydra.module.Module>>) (decErr -> (hydra.util.Either<String, hydra.module.Module>) ((hydra.util.Either<String, hydra.module.Module>) (hydra.util.Either.<String, hydra.module.Module>left((decErr).value)))),
        (java.util.function.Function<hydra.module.Module, hydra.util.Either<String, hydra.module.Module>>) (mod -> (hydra.util.Either<String, hydra.module.Module>) ((hydra.util.Either<String, hydra.module.Module>) (hydra.util.Either.<String, hydra.module.Module>right(hydra.lib.logic.IfElse.lazy(
          doStripTypeSchemes,
          () -> hydra.codeGeneration.CodeGeneration.stripModuleTypeSchemes(mod),
          () -> mod))))),
        hydra.decode.module.Module.module(
          graph,
          term))),
      hydra.json.decode.Decode.fromJson(
        schemaMap,
        modType,
        jsonVal));
  }
}
