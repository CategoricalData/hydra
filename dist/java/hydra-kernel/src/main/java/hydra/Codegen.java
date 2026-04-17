// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Pure code generation pipeline for bootstrapping Hydra across languages.
 */
public interface Codegen {
  static java.util.Map<hydra.core.Name, hydra.core.Type> buildSchemaMap(hydra.graph.Graph g) {
    return hydra.lib.maps.Map.apply(
      (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type>) (ts -> hydra.Strip.deannotateType((ts).type)),
      (g).schemaTypes);
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.packaging.Module> decodeModuleFromJson(hydra.graph.Graph bsGraph, java.util.List<hydra.packaging.Module> universeModules, hydra.json.model.Value jsonVal) {
    hydra.graph.Graph graph = hydra.Codegen.modulesToGraph(
      bsGraph,
      universeModules,
      universeModules);
    hydra.core.Type modType = new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.Module"));
    java.util.Map<hydra.core.Name, hydra.core.Type> schemaMap = hydra.Codegen.buildSchemaMap(graph);
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.Error_, hydra.packaging.Module>>) (err -> hydra.util.Either.<hydra.errors.Error_, hydra.packaging.Module>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError(err)))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.packaging.Module>>) (term -> hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.Error_, hydra.packaging.Module>>) (decErr -> hydra.util.Either.<hydra.errors.Error_, hydra.packaging.Module>left(new hydra.errors.Error_.Decoding(decErr))),
        (java.util.function.Function<hydra.packaging.Module, hydra.util.Either<hydra.errors.Error_, hydra.packaging.Module>>) (mod -> hydra.util.Either.<hydra.errors.Error_, hydra.packaging.Module>right(mod)),
        hydra.decode.Packaging.module(
          graph,
          term))),
      hydra.json.Decode.fromJson(
        schemaMap,
        new hydra.core.Name("hydra.packaging.Module"),
        modType,
        jsonVal));
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
    java.util.function.Function<Integer, java.util.List<Integer>> escapeToUnicode = (java.util.function.Function<Integer, java.util.List<Integer>>) (b -> java.util.Arrays.asList(
      92,
      117,
      48,
      48,
      (hexDigit).apply(hydra.lib.maybes.FromMaybe.applyLazy(
        () -> 0,
        hydra.lib.math.MaybeDiv.apply(
          b,
          16))),
      (hexDigit).apply(hydra.lib.maybes.FromMaybe.applyLazy(
        () -> 0,
        hydra.lib.math.MaybeMod.apply(
          b,
          16)))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<java.util.List<Integer>, java.util.List<Integer>>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<Boolean, java.util.function.Function<Boolean, java.util.function.Function<java.util.List<Integer>, java.util.List<Integer>>>>) (inStr -> (java.util.function.Function<Boolean, java.util.function.Function<java.util.List<Integer>, java.util.List<Integer>>>) (esc -> (java.util.function.Function<java.util.List<Integer>, java.util.List<Integer>>) (bytes -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()),
      (java.util.function.Function<hydra.util.Pair<Integer, java.util.List<Integer>>, java.util.List<Integer>>) (uc -> {
        hydra.util.Lazy<Integer> b = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(uc));
        hydra.util.Lazy<java.util.List<Integer>> bs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(uc));
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
      }),
      hydra.lib.lists.Uncons.apply(bytes))))));
    return go.get().apply(false).apply(false).apply(input);
  }

  static String formatPrimitive(hydra.graph.Primitive prim) {
    String name = (prim).name.value;
    String typeStr = hydra.show.Core.typeScheme((prim).type);
    return hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "  ",
          name),
        " : "),
      typeStr);
  }

  static String formatTermBinding(hydra.core.Binding binding) {
    String name = (binding).name.value;
    hydra.util.Lazy<String> typeStr = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> "?",
      (java.util.function.Function<hydra.core.TypeScheme, String>) (scheme -> hydra.show.Core.typeScheme(scheme)),
      (binding).type));
    return hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "  ",
          name),
        " : "),
      typeStr.get());
  }

  static hydra.util.Either<hydra.errors.Error_, String> formatTypeBinding(hydra.graph.Graph graph, hydra.core.Binding binding) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Decoding(_e)),
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_a -> _a),
        hydra.decode.Core.type(
          graph,
          (binding).term)),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, String>>) (typ -> hydra.util.Either.<hydra.errors.Error_, String>right(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "  ",
            (binding).name.value),
          " = "),
        hydra.show.Core.type(typ)))));
  }

  static <T0, T1, T2, T3> hydra.util.Either<T2, java.util.List<T3>> generateCoderModules(java.util.function.Function<T0, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T1, hydra.util.Either<T2, hydra.util.Maybe<T3>>>>> codec, hydra.graph.Graph bsGraph, java.util.List<hydra.packaging.Module> universeModules, java.util.List<T1> typeModules, T0 cx) {
    hydra.util.Lazy<java.util.Map<hydra.packaging.Namespace, hydra.packaging.Module>> universe = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, hydra.util.Pair<hydra.packaging.Namespace, hydra.packaging.Module>>) (m -> (hydra.util.Pair<hydra.packaging.Namespace, hydra.packaging.Module>) ((hydra.util.Pair<hydra.packaging.Namespace, hydra.packaging.Module>) (new hydra.util.Pair<hydra.packaging.Namespace, hydra.packaging.Module>((m).namespace, m)))),
      hydra.lib.lists.Concat2.apply(
        universeModules,
        universeModules))));
    java.util.List<hydra.packaging.Module> dataModules = hydra.Codegen.moduleTermDepsTransitive(
      universe.get(),
      universeModules);
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.core.Binding>>) (m -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.core.Binding>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.packaging.Definition instance) {
            return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.core.Binding> visit(hydra.packaging.Definition.Term td) {
            return hydra.util.Maybe.just(new hydra.core.Binding((td).value.name, (td).value.term, (td).value.type));
          }
        })),
        (m).definitions))),
      dataModules)));
    java.util.List<hydra.packaging.Module> schemaModules = hydra.Codegen.moduleTypeDepsTransitive(
      universe.get(),
      universeModules);
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> schemaElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.core.Binding>>) (m -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
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
                hydra.util.Lazy<hydra.core.Term> dataTerm = new hydra.util.Lazy<>(() -> hydra.Annotations.normalizeTermAnnotations(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.encode.Core.type((td).value.type.type), hydra.lib.maps.FromList.apply(java.util.Arrays.asList((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>(hydra.Constants.key_type(), schemaTerm)))))))));
                return new hydra.core.Binding((td).value.name, dataTerm.get(), hydra.util.Maybe.just(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))));
              })).get();
            })).get());
          }
        })),
        (m).definitions))),
      hydra.lib.lists.Concat2.apply(
        schemaModules,
        universeModules))));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> allElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      schemaElements.get(),
      dataElements.get()));
    hydra.util.Lazy<hydra.graph.Graph> schemaGraph = new hydra.util.Lazy<>(() -> hydra.Lexical.elementsToGraph(
      bsGraph,
      (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())),
      schemaElements.get()));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> schemaTypes = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>) (ignored -> (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply()))),
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>) (_r -> _r),
      hydra.Environment.schemaGraphToTypingEnvironment(schemaGraph.get())));
    hydra.graph.Graph graph = hydra.Lexical.elementsToGraph(
      bsGraph,
      schemaTypes.get(),
      allElements.get());
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.List<hydra.util.Maybe<T3>>, java.util.List<T3>>) (results -> hydra.lib.maybes.Cat.apply(results)),
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<T1, hydra.util.Either<T2, hydra.util.Maybe<T3>>>) (m -> (codec).apply(cx).apply(graph).apply(m)),
        typeModules));
  }

  static hydra.util.Either<hydra.errors.Error_, String> generateLexicon(hydra.graph.Graph graph) {
    java.util.List<hydra.core.Binding> bindings = hydra.Lexical.graphToBindings(graph);
    hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>> partitioned = new hydra.util.Lazy<>(() -> hydra.lib.lists.Partition.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.Annotations.isNativeType(b)),
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
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, String>>) (b -> hydra.Codegen.formatTypeBinding(
          graph,
          b)),
        sortedTypes.get()),
      (java.util.function.Function<java.util.List<String>, hydra.util.Either<hydra.errors.Error_, String>>) (typeLines -> {
        hydra.util.Lazy<java.util.List<String>> primitiveLines = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.graph.Primitive, String>) (p -> hydra.Codegen.formatPrimitive(p)),
          sortedPrimitives.get()));
        hydra.util.Lazy<java.util.List<String>> termLines = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, String>) (b -> hydra.Codegen.formatTermBinding(b)),
          sortedTerms.get()));
        return hydra.util.Either.<hydra.errors.Error_, String>right(hydra.lib.strings.Cat2.apply(
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

  static <T0, T1> hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.util.Pair<T0, T1>>> generateSourceFiles(java.util.function.Function<hydra.packaging.Module, java.util.function.Function<java.util.List<hydra.packaging.Definition>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<hydra.errors.Error_, java.util.Map<T0, T1>>>>>> printDefinitions, hydra.coders.Language lang, Boolean doInfer, Boolean doExpand, Boolean doHoistCaseStatements, Boolean doHoistPolymorphicLetBindings, hydra.graph.Graph bsGraph, java.util.List<hydra.packaging.Module> universeModules, java.util.List<hydra.packaging.Module> modsToGenerate, hydra.context.Context cx) {
    hydra.coders.LanguageConstraints constraints = (lang).constraints;
    hydra.util.Lazy<java.util.Map<hydra.packaging.Namespace, hydra.packaging.Module>> namespaceMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, hydra.util.Pair<hydra.packaging.Namespace, hydra.packaging.Module>>) (m -> (hydra.util.Pair<hydra.packaging.Namespace, hydra.packaging.Module>) ((hydra.util.Pair<hydra.packaging.Namespace, hydra.packaging.Module>) (new hydra.util.Pair<hydra.packaging.Namespace, hydra.packaging.Module>((m).namespace, m)))),
      hydra.lib.lists.Concat2.apply(
        universeModules,
        modsToGenerate))));
    java.util.List<hydra.packaging.Module> dataMods = hydra.Codegen.moduleTermDepsTransitive(
      namespaceMap.get(),
      modsToGenerate);
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.core.Binding>>) (m -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.core.Binding>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.packaging.Definition instance) {
            return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.core.Binding> visit(hydra.packaging.Definition.Term td) {
            return hydra.util.Maybe.just(new hydra.core.Binding((td).value.name, (td).value.term, (td).value.type));
          }
        })),
        (m).definitions))),
      dataMods)));
    java.util.List<hydra.packaging.Module> schemaMods = hydra.Codegen.moduleTypeDepsTransitive(
      namespaceMap.get(),
      modsToGenerate);
    hydra.util.Lazy<java.util.List<hydra.packaging.Module>> typeModulesToGenerate = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.packaging.Module, Boolean>) (mod -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
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
                hydra.util.Lazy<hydra.core.Term> dataTerm = new hydra.util.Lazy<>(() -> hydra.Annotations.normalizeTermAnnotations(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.encode.Core.type((td).value.type.type), hydra.lib.maps.FromList.apply(java.util.Arrays.asList((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>(hydra.Constants.key_type(), schemaTerm)))))))));
                return new hydra.core.Binding((td).value.name, dataTerm.get(), hydra.util.Maybe.just(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))));
              })).get();
            })).get());
          }
        })),
        (mod).definitions))))),
      modsToGenerate));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> schemaElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.core.Binding>>) (m -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
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
                hydra.util.Lazy<hydra.core.Term> dataTerm = new hydra.util.Lazy<>(() -> hydra.Annotations.normalizeTermAnnotations(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.encode.Core.type((td).value.type.type), hydra.lib.maps.FromList.apply(java.util.Arrays.asList((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>(hydra.Constants.key_type(), schemaTerm)))))))));
                return new hydra.core.Binding((td).value.name, dataTerm.get(), hydra.util.Maybe.just(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))));
              })).get();
            })).get());
          }
        })),
        (m).definitions))),
      hydra.lib.lists.Concat2.apply(
        schemaMods,
        typeModulesToGenerate.get()))));
    hydra.util.Lazy<hydra.graph.Graph> schemaGraph = new hydra.util.Lazy<>(() -> hydra.Lexical.elementsToGraph(
      bsGraph,
      (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())),
      schemaElements.get()));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> schemaTypes2 = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>) (ignored -> (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply()))),
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>) (_r -> _r),
      hydra.Environment.schemaGraphToTypingEnvironment(schemaGraph.get())));
    hydra.graph.Graph dataGraph = hydra.Lexical.elementsToGraph(
      bsGraph,
      schemaTypes2.get(),
      dataElements.get());
    hydra.util.Lazy<java.util.List<hydra.packaging.Module>> termModulesToGenerate = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.packaging.Module, Boolean>) (mod -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.core.Binding>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.packaging.Definition instance) {
            return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.core.Binding> visit(hydra.packaging.Definition.Term td) {
            return hydra.util.Maybe.just(new hydra.core.Binding((td).value.name, (td).value.term, (td).value.type));
          }
        })),
        (mod).definitions))))),
      modsToGenerate));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(typeModulesToGenerate.get()),
        () -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.util.Pair<T0, T1>>>right((java.util.List<hydra.util.Pair<T0, T1>>) (java.util.Collections.<hydra.util.Pair<T0, T1>>emptyList())),
        () -> ((java.util.function.Supplier<hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.util.Pair<T0, T1>>>>) (() -> {
          hydra.util.Lazy<java.util.List<java.util.List<hydra.core.Name>>> nameLists = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.core.Name>>) (m -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.core.Name>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
                @Override
                public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.packaging.Definition instance) {
                  return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
                }

                @Override
                public hydra.util.Maybe<hydra.core.Name> visit(hydra.packaging.Definition.Type td) {
                  return hydra.util.Maybe.just((td).value.name);
                }
              })),
              (m).definitions))),
            typeModulesToGenerate.get()));
          return hydra.lib.eithers.Bind.apply(
            hydra.Adapt.schemaGraphToDefinitions(
              constraints,
              schemaGraph.get(),
              nameLists.get(),
              cx),
            (java.util.function.Function<hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Type>, java.util.List<java.util.List<hydra.packaging.TypeDefinition>>>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.util.Pair<T0, T1>>>>) (schemaResult -> {
              hydra.util.Lazy<java.util.List<java.util.List<hydra.packaging.TypeDefinition>>> defLists = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(schemaResult));
              hydra.graph.Graph schemaGraphWithTypes = new hydra.graph.Graph(schemaGraph.get().boundTerms, schemaGraph.get().boundTypes, schemaGraph.get().classConstraints, schemaGraph.get().lambdaVariables, schemaGraph.get().metadata, schemaGraph.get().primitives, schemaTypes2.get(), schemaGraph.get().typeVariables);
              return hydra.lib.eithers.Map.apply(
                (java.util.function.Function<java.util.List<java.util.List<hydra.util.Pair<T0, T1>>>, java.util.List<hydra.util.Pair<T0, T1>>>) (xs -> hydra.lib.lists.Concat.apply(xs)),
                hydra.lib.eithers.MapList.apply(
                  (java.util.function.Function<hydra.util.Pair<hydra.packaging.Module, java.util.List<hydra.packaging.TypeDefinition>>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.util.Pair<T0, T1>>>>) (p -> {
                    hydra.util.Lazy<java.util.List<hydra.packaging.TypeDefinition>> defs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
                    hydra.util.Lazy<hydra.packaging.Module> mod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
                    return hydra.lib.eithers.Map.apply(
                      (java.util.function.Function<java.util.Map<T0, T1>, java.util.List<hydra.util.Pair<T0, T1>>>) (m -> hydra.lib.maps.ToList.apply(m)),
                      (printDefinitions).apply(mod.get()).apply(hydra.lib.lists.Map.apply(
                        (java.util.function.Function<hydra.packaging.TypeDefinition, hydra.packaging.Definition>) (d -> new hydra.packaging.Definition.Type(d)),
                        defs.get())).apply(cx).apply(schemaGraphWithTypes));
                  }),
                  hydra.lib.lists.Zip.apply(
                    typeModulesToGenerate.get(),
                    defLists.get())));
            }));
        })).get()),
      (java.util.function.Function<java.util.List<hydra.util.Pair<T0, T1>>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.util.Pair<T0, T1>>>>) (schemaFiles -> hydra.lib.eithers.Bind.apply(
        hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(termModulesToGenerate.get()),
          () -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.util.Pair<T0, T1>>>right((java.util.List<hydra.util.Pair<T0, T1>>) (java.util.Collections.<hydra.util.Pair<T0, T1>>emptyList())),
          () -> ((java.util.function.Supplier<hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.util.Pair<T0, T1>>>>) (() -> {
            hydra.util.Lazy<java.util.List<hydra.packaging.Namespace>> namespaces = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.packaging.Module, hydra.packaging.Namespace>) (m -> (m).namespace),
              termModulesToGenerate.get()));
            return hydra.lib.eithers.Bind.apply(
              hydra.Adapt.dataGraphToDefinitions(
                constraints,
                doInfer,
                doExpand,
                doHoistCaseStatements,
                doHoistPolymorphicLetBindings,
                dataElements.get(),
                dataGraph,
                namespaces.get(),
                cx),
              (java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, java.util.List<java.util.List<hydra.packaging.TermDefinition>>>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.util.Pair<T0, T1>>>>) (dataResult -> {
                hydra.util.Lazy<hydra.graph.Graph> g1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(dataResult));
                java.util.List<hydra.core.Binding> allBindings = hydra.Lexical.graphToBindings(g1.get());
                java.util.function.Function<java.util.List<hydra.packaging.TermDefinition>, java.util.List<hydra.packaging.TermDefinition>> dedupDefs = (java.util.function.Function<java.util.List<hydra.packaging.TermDefinition>, java.util.List<hydra.packaging.TermDefinition>>) (defs -> hydra.lib.maps.Elems.apply(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.packaging.TermDefinition, hydra.util.Pair<hydra.core.Name, hydra.packaging.TermDefinition>>) (d -> (hydra.util.Pair<hydra.core.Name, hydra.packaging.TermDefinition>) ((hydra.util.Pair<hydra.core.Name, hydra.packaging.TermDefinition>) (new hydra.util.Pair<hydra.core.Name, hydra.packaging.TermDefinition>((d).name, d)))),
                  defs))));
                hydra.util.Lazy<java.util.List<java.util.List<hydra.packaging.TermDefinition>>> defLists = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(dataResult));
                hydra.util.Lazy<java.util.List<java.util.List<hydra.packaging.TermDefinition>>> dedupedDefLists = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                  dedupDefs,
                  defLists.get()));
                java.util.function.Function<hydra.packaging.Definition, hydra.core.Name> defName = (java.util.function.Function<hydra.packaging.Definition, hydra.core.Name>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
                  @Override
                  public hydra.core.Name visit(hydra.packaging.Definition.Term td) {
                    return (td).value.name;
                  }

                  @Override
                  public hydra.core.Name visit(hydra.packaging.Definition.Type td) {
                    return (td).value.name;
                  }
                }));
                java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.packaging.Module, hydra.packaging.Module>> refreshModule = (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.packaging.Module, hydra.packaging.Module>>) (els -> (java.util.function.Function<hydra.packaging.Module, hydra.packaging.Module>) (m -> new hydra.packaging.Module((m).namespace, hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.packaging.Definition>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
                    @Override
                    public hydra.util.Maybe<hydra.packaging.Definition> visit(hydra.packaging.Definition.Type td) {
                      return hydra.util.Maybe.just(new hydra.packaging.Definition.Type((td).value));
                    }

                    @Override
                    public hydra.util.Maybe<hydra.packaging.Definition> visit(hydra.packaging.Definition.Term td) {
                      return hydra.lib.maybes.Map.apply(
                        (java.util.function.Function<hydra.core.Binding, hydra.packaging.Definition>) (b -> new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition((b).name, (b).term, (b).type))),
                        hydra.lib.lists.Find.apply(
                          (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
                            (b).name,
                            (td).value.name)),
                          els));
                    }
                  })),
                  (m).definitions)), (m).termDependencies, (m).typeDependencies, (m).description)));
                hydra.util.Lazy<java.util.List<hydra.packaging.Module>> refreshedMods = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.packaging.Module, hydra.packaging.Module>) (m -> (refreshModule).apply(allBindings).apply(m)),
                  termModulesToGenerate.get()));
                return hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<java.util.List<java.util.List<hydra.util.Pair<T0, T1>>>, java.util.List<hydra.util.Pair<T0, T1>>>) (xs -> hydra.lib.lists.Concat.apply(xs)),
                  hydra.lib.eithers.MapList.apply(
                    (java.util.function.Function<hydra.util.Pair<hydra.packaging.Module, java.util.List<hydra.packaging.TermDefinition>>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.util.Pair<T0, T1>>>>) (p -> {
                      hydra.util.Lazy<java.util.List<hydra.packaging.TermDefinition>> defs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
                      hydra.util.Lazy<hydra.packaging.Module> mod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
                      return hydra.lib.eithers.Map.apply(
                        (java.util.function.Function<java.util.Map<T0, T1>, java.util.List<hydra.util.Pair<T0, T1>>>) (m -> hydra.lib.maps.ToList.apply(m)),
                        (printDefinitions).apply(mod.get()).apply(hydra.lib.lists.Map.apply(
                          (java.util.function.Function<hydra.packaging.TermDefinition, hydra.packaging.Definition>) (d -> new hydra.packaging.Definition.Term(d)),
                          defs.get())).apply(cx).apply(g1.get()));
                    }),
                    hydra.lib.lists.Zip.apply(
                      refreshedMods.get(),
                      dedupedDefLists.get())));
              }));
          })).get()),
        (java.util.function.Function<java.util.List<hydra.util.Pair<T0, T1>>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.util.Pair<T0, T1>>>>) (termFiles -> hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.util.Pair<T0, T1>>>right(hydra.lib.lists.Concat2.apply(
          schemaFiles,
          termFiles))))));
  }

  static hydra.util.Either<hydra.errors.Error_, String> inferAndGenerateLexicon(hydra.context.Context cx, hydra.graph.Graph bsGraph, java.util.List<hydra.packaging.Module> kernelModules) {
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.core.Binding>>) (m -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.core.Binding>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.packaging.Definition instance) {
            return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.core.Binding> visit(hydra.packaging.Definition.Term td) {
            return hydra.util.Maybe.just(new hydra.core.Binding((td).value.name, (td).value.term, (td).value.type));
          }
        })),
        (m).definitions))),
      kernelModules)));
    hydra.graph.Graph g0 = hydra.Codegen.modulesToGraph(
      bsGraph,
      kernelModules,
      kernelModules);
    return hydra.lib.eithers.Bind.apply(
      hydra.Inference.inferGraphTypes(
        cx,
        dataElements.get(),
        g0),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, String>>) (inferResultWithCx -> {
        hydra.util.Lazy<hydra.graph.Graph> g1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(inferResultWithCx)));
        return hydra.Codegen.generateLexicon(g1.get());
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.packaging.Module>> inferModules(hydra.context.Context cx, hydra.graph.Graph bsGraph, java.util.List<hydra.packaging.Module> universeMods, java.util.List<hydra.packaging.Module> targetMods) {
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.core.Binding>>) (m -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.core.Binding>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.packaging.Definition instance) {
            return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.core.Binding> visit(hydra.packaging.Definition.Term td) {
            return hydra.util.Maybe.just(new hydra.core.Binding((td).value.name, (td).value.term, (td).value.type));
          }
        })),
        (m).definitions))),
      universeMods)));
    hydra.graph.Graph g0 = hydra.Codegen.modulesToGraph(
      bsGraph,
      universeMods,
      universeMods);
    return hydra.lib.eithers.Bind.apply(
      hydra.Inference.inferGraphTypes(
        cx,
        dataElements.get(),
        g0),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.packaging.Module>>>) (inferResultWithCx -> {
        hydra.util.Lazy<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>> inferResult = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(inferResultWithCx));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> inferredElements = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(inferResult.get()));
        return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.packaging.Module>>right(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.packaging.Module, hydra.packaging.Module>) (v1 -> hydra.Codegen.refreshModule(
            inferredElements.get(),
            v1)),
          targetMods));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.packaging.Module>> inferModulesGiven(hydra.context.Context cx, hydra.graph.Graph bsGraph, java.util.List<hydra.packaging.Module> universeMods, java.util.List<hydra.packaging.Module> targetMods) {
    hydra.graph.Graph g0 = hydra.Codegen.modulesToGraph(
      bsGraph,
      universeMods,
      universeMods);
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> targetBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.core.Binding>>) (m -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.core.Binding>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.packaging.Definition instance) {
            return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.core.Binding> visit(hydra.packaging.Definition.Term td) {
            return hydra.util.Maybe.just(new hydra.core.Binding((td).value.name, (td).value.term, (td).value.type));
          }
        })),
        (m).definitions))),
      targetMods)));
    return hydra.lib.eithers.Bind.apply(
      hydra.Inference.inferGraphTypes(
        cx,
        targetBindings.get(),
        g0),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.packaging.Module>>>) (inferResultWithCx -> {
        hydra.util.Lazy<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>> inferResult = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(inferResultWithCx));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> inferredElements = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(inferResult.get()));
        return hydra.util.Either.<hydra.errors.Error_, java.util.List<hydra.packaging.Module>>right(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.packaging.Module, hydra.packaging.Module>) (v1 -> hydra.Codegen.refreshModule(
            inferredElements.get(),
            v1)),
          targetMods));
      }));
  }

  static java.util.List<hydra.packaging.Module> moduleTermDepsTransitive(java.util.Map<hydra.packaging.Namespace, hydra.packaging.Module> nsMap, java.util.List<hydra.packaging.Module> modules) {
    hydra.util.Lazy<java.util.Set<hydra.packaging.Namespace>> closure = new hydra.util.Lazy<>(() -> hydra.lib.sets.Union.apply(
      hydra.Codegen.transitiveDeps(
        (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.packaging.Namespace>>) (m -> (m).termDependencies),
        nsMap,
        modules),
      hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.Module, hydra.packaging.Namespace>) (m -> (m).namespace),
        modules))));
    return hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Namespace, hydra.util.Maybe<hydra.packaging.Module>>) (n -> hydra.lib.maps.Lookup.apply(
        n,
        nsMap)),
      hydra.lib.sets.ToList.apply(closure.get())));
  }

  static hydra.util.Either<hydra.errors.Error_, String> moduleToJson(java.util.Map<hydra.core.Name, hydra.core.Type> schemaMap, hydra.packaging.Module m) {
    hydra.core.Type modType = new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.Module"));
    hydra.core.Term term = hydra.encode.Packaging.module(m);
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.json.Writer.printJson(json)),
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<String, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Other(new hydra.errors.OtherError(_e))),
        (java.util.function.Function<hydra.json.model.Value, hydra.json.model.Value>) (_a -> _a),
        hydra.json.Encode.toJson(
          schemaMap,
          new hydra.core.Name("hydra.packaging.Module"),
          modType,
          term)));
  }

  static hydra.packaging.Module moduleToSourceModule(hydra.packaging.Module m) {
    hydra.packaging.Namespace modTypeNs = new hydra.packaging.Namespace("hydra.packaging");
    hydra.util.Lazy<hydra.packaging.Namespace> sourceNs = new hydra.util.Lazy<>(() -> new hydra.packaging.Namespace(hydra.lib.strings.Cat2.apply(
      "hydra.sources.",
      hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Drop.apply(
          1,
          hydra.lib.strings.SplitOn.apply(
            ".",
            (m).namespace.value))))));
    hydra.util.Lazy<hydra.packaging.Definition> moduleDef = new hydra.util.Lazy<>(() -> new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition(new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      sourceNs.get().value,
      ".module_")), hydra.encode.Packaging.module(m), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))));
    return new hydra.packaging.Module(sourceNs.get(), java.util.Arrays.asList(moduleDef.get()), java.util.Arrays.asList(modTypeNs), java.util.Arrays.asList(modTypeNs), hydra.util.Maybe.just(hydra.lib.strings.Cat2.apply(
      "Source module for ",
      (m).namespace.value)));
  }

  static java.util.List<hydra.packaging.Module> moduleTypeDepsTransitive(java.util.Map<hydra.packaging.Namespace, hydra.packaging.Module> nsMap, java.util.List<hydra.packaging.Module> modules) {
    java.util.List<hydra.packaging.Module> termMods = hydra.Codegen.moduleTermDepsTransitive(
      nsMap,
      modules);
    hydra.util.Lazy<java.util.List<hydra.packaging.Namespace>> typeNamespaces = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(hydra.Codegen.transitiveDeps(
      (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.packaging.Namespace>>) (m -> (m).typeDependencies),
      nsMap,
      termMods)));
    return hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Namespace, hydra.util.Maybe<hydra.packaging.Module>>) (n -> hydra.lib.maps.Lookup.apply(
        n,
        nsMap)),
      typeNamespaces.get()));
  }

  static hydra.graph.Graph modulesToGraph(hydra.graph.Graph bsGraph, java.util.List<hydra.packaging.Module> universeModules, java.util.List<hydra.packaging.Module> modules) {
    hydra.util.Lazy<java.util.Map<hydra.packaging.Namespace, hydra.packaging.Module>> universe = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, hydra.util.Pair<hydra.packaging.Namespace, hydra.packaging.Module>>) (m -> (hydra.util.Pair<hydra.packaging.Namespace, hydra.packaging.Module>) ((hydra.util.Pair<hydra.packaging.Namespace, hydra.packaging.Module>) (new hydra.util.Pair<hydra.packaging.Namespace, hydra.packaging.Module>((m).namespace, m)))),
      hydra.lib.lists.Concat2.apply(
        universeModules,
        modules))));
    java.util.List<hydra.packaging.Module> dataModules = hydra.Codegen.moduleTermDepsTransitive(
      universe.get(),
      modules);
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> dataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.core.Binding>>) (m -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.core.Binding>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.packaging.Definition instance) {
            return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.core.Binding> visit(hydra.packaging.Definition.Term td) {
            return hydra.util.Maybe.just(new hydra.core.Binding((td).value.name, (td).value.term, (td).value.type));
          }
        })),
        (m).definitions))),
      dataModules)));
    java.util.List<hydra.packaging.Module> schemaModules = hydra.Codegen.moduleTypeDepsTransitive(
      universe.get(),
      modules);
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> schemaElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.core.Binding>>) (m -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
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
                hydra.util.Lazy<hydra.core.Term> dataTerm = new hydra.util.Lazy<>(() -> hydra.Annotations.normalizeTermAnnotations(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.encode.Core.type((td).value.type.type), hydra.lib.maps.FromList.apply(java.util.Arrays.asList((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>(hydra.Constants.key_type(), schemaTerm)))))))));
                return new hydra.core.Binding((td).value.name, dataTerm.get(), hydra.util.Maybe.just(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))));
              })).get();
            })).get());
          }
        })),
        (m).definitions))),
      hydra.lib.lists.Concat2.apply(
        schemaModules,
        modules))));
    hydra.util.Lazy<hydra.graph.Graph> schemaGraph = new hydra.util.Lazy<>(() -> hydra.Lexical.elementsToGraph(
      bsGraph,
      (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())),
      schemaElements.get()));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> schemaTypes = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>) (ignored -> (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply()))),
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>) (_r -> _r),
      hydra.Environment.schemaGraphToTypingEnvironment(schemaGraph.get())));
    hydra.graph.Graph baseGraph = hydra.Lexical.elementsToGraph(
      bsGraph,
      schemaTypes.get(),
      dataElements.get());
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> universeDataElements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.core.Binding>>) (m -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.core.Binding>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.packaging.Definition instance) {
            return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.core.Binding> visit(hydra.packaging.Definition.Term td) {
            return hydra.util.Maybe.just(new hydra.core.Binding((td).value.name, (td).value.term, (td).value.type));
          }
        })),
        (m).definitions))),
      universeModules)));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> universeBoundTypes = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>) (b -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (ts -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>((b).name, ts)))),
        (b).type)),
      universeDataElements.get()))));
    return new hydra.graph.Graph((baseGraph).boundTerms, universeBoundTypes.get(), (baseGraph).classConstraints, (baseGraph).lambdaVariables, (baseGraph).metadata, (baseGraph).primitives, (baseGraph).schemaTypes, (baseGraph).typeVariables);
  }

  static String namespaceToPath(hydra.packaging.Namespace ns) {
    return hydra.lib.strings.Intercalate.apply(
      "/",
      hydra.lib.strings.SplitOn.apply(
        ".",
        (ns).value));
  }

  static hydra.packaging.Module refreshModule(java.util.List<hydra.core.Binding> inferredElements, hydra.packaging.Module m) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.core.Binding>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.core.Binding> otherwise(hydra.packaging.Definition instance) {
            return (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing());
          }

          @Override
          public hydra.util.Maybe<hydra.core.Binding> visit(hydra.packaging.Definition.Term td) {
            return hydra.util.Maybe.just(new hydra.core.Binding((td).value.name, (td).value.term, (td).value.type));
          }
        })),
        (m).definitions))))),
      () -> m,
      () -> new hydra.packaging.Module((m).namespace, hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.packaging.Definition>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<hydra.packaging.Definition> visit(hydra.packaging.Definition.Type td) {
            return hydra.util.Maybe.just(new hydra.packaging.Definition.Type((td).value));
          }

          @Override
          public hydra.util.Maybe<hydra.packaging.Definition> visit(hydra.packaging.Definition.Term td) {
            return hydra.lib.maybes.Map.apply(
              (java.util.function.Function<hydra.core.Binding, hydra.packaging.Definition>) (b -> new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition((b).name, (b).term, (b).type))),
              hydra.lib.lists.Find.apply(
                (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
                  (b).name,
                  (td).value.name)),
                inferredElements));
          }
        })),
        (m).definitions)), (m).termDependencies, (m).typeDependencies, (m).description));
  }

  static java.util.Set<hydra.packaging.Namespace> transitiveDeps(java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.packaging.Namespace>> getDeps, java.util.Map<hydra.packaging.Namespace, hydra.packaging.Module> nsMap, java.util.List<hydra.packaging.Module> startMods) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.Set<hydra.packaging.Namespace>, java.util.function.Function<java.util.Set<hydra.packaging.Namespace>, java.util.Set<hydra.packaging.Namespace>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<java.util.Set<hydra.packaging.Namespace>, java.util.function.Function<java.util.Set<hydra.packaging.Namespace>, java.util.Set<hydra.packaging.Namespace>>>) (pending -> (java.util.function.Function<java.util.Set<hydra.packaging.Namespace>, java.util.Set<hydra.packaging.Namespace>>) (visited -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Null.apply(pending),
      () -> visited,
      () -> ((java.util.function.Supplier<java.util.Set<hydra.packaging.Namespace>>) (() -> {
        hydra.util.Lazy<java.util.Set<hydra.packaging.Namespace>> newVisited = new hydra.util.Lazy<>(() -> hydra.lib.sets.Union.apply(
          visited,
          pending));
        return ((java.util.function.Supplier<java.util.Set<hydra.packaging.Namespace>>) (() -> {
          hydra.util.Lazy<java.util.Set<hydra.packaging.Namespace>> nextDeps = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.packaging.Namespace, java.util.List<hydra.packaging.Namespace>>) (nsv -> hydra.lib.maybes.Maybe.applyLazy(
              () -> (java.util.List<hydra.packaging.Namespace>) (java.util.Collections.<hydra.packaging.Namespace>emptyList()),
              (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.packaging.Namespace>>) (depMod -> (getDeps).apply(depMod)),
              hydra.lib.maps.Lookup.apply(
                nsv,
                nsMap))),
            hydra.lib.sets.ToList.apply(pending)))));
          return ((java.util.function.Supplier<java.util.Set<hydra.packaging.Namespace>>) (() -> {
            hydra.util.Lazy<java.util.Set<hydra.packaging.Namespace>> newPending = new hydra.util.Lazy<>(() -> hydra.lib.sets.Difference.apply(
              nextDeps.get(),
              newVisited.get()));
            return go.get().apply(newPending.get()).apply(newVisited.get());
          })).get();
        })).get();
      })).get()))));
    hydra.util.Lazy<java.util.Set<hydra.packaging.Namespace>> initialDeps = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Module, java.util.List<hydra.packaging.Namespace>>) (m -> hydra.lib.lists.Filter.apply(
        (java.util.function.Function<hydra.packaging.Namespace, Boolean>) (dep -> hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          dep,
          (m).namespace))),
        (getDeps).apply(m))),
      startMods))));
    return go.get().apply(initialDeps.get()).apply((java.util.Set<hydra.packaging.Namespace>) (hydra.lib.sets.Empty.<hydra.packaging.Namespace>apply()));
  }
}
