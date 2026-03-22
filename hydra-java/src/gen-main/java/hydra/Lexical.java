// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * A module for lexical operations over graphs.
 */
public interface Lexical {
  static hydra.graph.Graph buildGraph(hydra.util.ConsList<hydra.core.Binding> elements, hydra.util.PersistentMap<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>> environment, hydra.util.PersistentMap<hydra.core.Name, hydra.graph.Primitive> primitives) {
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> elementTerms = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, hydra.core.Term>>) (b -> (hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>((b).name, (b).term)))),
      elements)));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>> elementTypes = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>) (b -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (ts -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>((b).name, ts)))),
        (b).type)),
      elements))));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> letTerms = new hydra.util.Lazy<>(() -> hydra.lib.maps.Map.apply(
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.core.Term>) (mt -> hydra.lib.maybes.FromJust.apply(mt)),
      hydra.lib.maps.Filter.apply(
        (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, Boolean>) (mt -> hydra.lib.maybes.IsJust.apply(mt)),
        environment)));
    return new hydra.graph.Graph(hydra.lib.maps.Union.apply(
      elementTerms.get(),
      letTerms.get()), elementTypes.get(), (hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply(hydra.lib.maps.Filter.apply(
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, Boolean>) (mt -> hydra.lib.maybes.IsNothing.apply(mt)),
      environment))), (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), primitives, (hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()));
  }

  static hydra.core.Name chooseUniqueName(hydra.util.PersistentSet<hydra.core.Name> reserved, hydra.core.Name name) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Integer, hydra.core.Name>> tryName = new java.util.concurrent.atomic.AtomicReference<>();
    tryName.set((java.util.function.Function<Integer, hydra.core.Name>) (index -> {
      hydra.util.Lazy<hydra.core.Name> candidate = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          index,
          1),
        () -> name,
        () -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
          (name).value,
          hydra.lib.literals.ShowInt32.apply(index)))));
      return hydra.lib.logic.IfElse.lazy(
        hydra.lib.sets.Member.apply(
          candidate.get(),
          reserved),
        () -> tryName.get().apply(hydra.lib.math.Add.apply(
          index,
          1)),
        () -> candidate.get());
    }));
    return tryName.get().apply(1);
  }

  static hydra.util.Maybe<hydra.core.Binding> dereferenceElement(hydra.graph.Graph graph, hydra.core.Name name) {
    return hydra.Lexical.lookupElement(
      graph,
      name);
  }

  static hydra.util.Maybe<hydra.core.TypeScheme> dereferenceSchemaType(hydra.core.Name name, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme> types) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>> forType = new java.util.concurrent.atomic.AtomicReference<>();
    forType.set((java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.TypeScheme> otherwise(hydra.core.Type instance) {
        return hydra.util.Maybe.just(new hydra.core.TypeScheme((hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()), t, (hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())));
      }

      @Override
      public hydra.util.Maybe<hydra.core.TypeScheme> visit(hydra.core.Type.Annotated at) {
        return forType.get().apply((at).value.body);
      }

      @Override
      public hydra.util.Maybe<hydra.core.TypeScheme> visit(hydra.core.Type.Forall ft) {
        return hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (ts -> new hydra.core.TypeScheme(hydra.lib.lists.Cons.apply(
            (ft).value.parameter,
            (ts).variables), (ts).type, (ts).constraints)),
          forType.get().apply((ft).value.body));
      }

      @Override
      public hydra.util.Maybe<hydra.core.TypeScheme> visit(hydra.core.Type.Variable v) {
        return hydra.Lexical.dereferenceSchemaType(
          (v).value,
          types);
      }
    })));
    return hydra.lib.maybes.Bind.apply(
      hydra.lib.maps.Lookup.apply(
        name,
        types),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Maybe<hydra.core.TypeScheme>>) (ts -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (ts2 -> new hydra.core.TypeScheme(hydra.lib.lists.Concat2.apply(
          (ts).variables,
          (ts2).variables), (ts2).type, (ts2).constraints)),
        forType.get().apply((ts).type))));
  }

  static hydra.util.Either<String, hydra.core.Binding> dereferenceVariable(hydra.graph.Graph graph, hydra.core.Name name) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<String, hydra.core.Binding>left(hydra.lib.strings.Cat2.apply(
        "no such element: ",
        (name).value)),
      (java.util.function.Function<hydra.core.Binding, hydra.util.Either<String, hydra.core.Binding>>) (right_ -> hydra.util.Either.<String, hydra.core.Binding>right(right_)),
      hydra.Lexical.lookupElement(
        graph,
        name));
  }

  static hydra.graph.Graph elementsToGraph(hydra.graph.Graph parent, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme> schemaTypes, hydra.util.ConsList<hydra.core.Binding> elements) {
    hydra.util.PersistentMap<hydra.core.Name, hydra.graph.Primitive> prims = (parent).primitives;
    hydra.util.Lazy<hydra.graph.Graph> g = new hydra.util.Lazy<>(() -> hydra.Lexical.buildGraph(
      elements,
      (hydra.util.PersistentMap<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())),
      prims));
    return new hydra.graph.Graph(g.get().boundTerms, g.get().boundTypes, g.get().classConstraints, g.get().lambdaVariables, g.get().metadata, g.get().primitives, schemaTypes, g.get().typeVariables);
  }

  static hydra.context.Context emptyContext() {
    return new hydra.context.Context((hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty()), (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty()), (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())));
  }

  static hydra.graph.Graph emptyGraph() {
    return new hydra.graph.Graph((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (hydra.util.PersistentMap<hydra.core.Name, hydra.graph.Primitive>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.graph.Primitive>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.graph.Primitive>apply())), (hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()));
  }

  static hydra.util.ConsList<hydra.core.Binding> graphToBindings(hydra.graph.Graph g) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, hydra.core.Binding>) (p -> {
        hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
        hydra.util.Lazy<hydra.core.Term> term = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
        return new hydra.core.Binding(name.get(), term.get(), hydra.lib.maps.Lookup.apply(
          name.get(),
          (g).boundTypes));
      }),
      hydra.lib.maps.ToList.apply((g).boundTerms));
  }

  static hydra.util.ConsList<hydra.core.FieldType> fieldsOf(hydra.core.Type t) {
    hydra.core.Type stripped = hydra.Rewriting.deannotateType(t);
    return (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.FieldType> otherwise(hydra.core.Type instance) {
        return (hydra.util.ConsList<hydra.core.FieldType>) (hydra.util.ConsList.<hydra.core.FieldType>empty());
      }

      @Override
      public hydra.util.ConsList<hydra.core.FieldType> visit(hydra.core.Type.Forall forallType) {
        return hydra.Lexical.fieldsOf((forallType).value.body);
      }

      @Override
      public hydra.util.ConsList<hydra.core.FieldType> visit(hydra.core.Type.Record rt) {
        return (rt).value;
      }

      @Override
      public hydra.util.ConsList<hydra.core.FieldType> visit(hydra.core.Type.Union rt) {
        return (rt).value;
      }
    });
  }

  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T1> getField(hydra.context.Context cx, hydra.util.PersistentMap<hydra.core.Name, T0> m, hydra.core.Name fname, java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T1>> decode) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, T1>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "expected field ",
          (fname).value),
        " not found"))), cx))),
      decode,
      hydra.lib.maps.Lookup.apply(
        fname,
        m));
  }

  static hydra.util.Maybe<hydra.core.Binding> lookupElement(hydra.graph.Graph graph, hydra.core.Name name) {
    return hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.core.Term, hydra.core.Binding>) (term -> new hydra.core.Binding(name, term, hydra.lib.maps.Lookup.apply(
        name,
        (graph).boundTypes))),
      hydra.lib.maps.Lookup.apply(
        name,
        (graph).boundTerms));
  }

  static hydra.util.Maybe<hydra.graph.Primitive> lookupPrimitive(hydra.graph.Graph graph, hydra.core.Name name) {
    return hydra.lib.maps.Lookup.apply(
      name,
      (graph).primitives);
  }

  static hydra.util.Maybe<hydra.core.Term> lookupTerm(hydra.graph.Graph graph, hydra.core.Name name) {
    return hydra.lib.maps.Lookup.apply(
      name,
      (graph).boundTerms);
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0> matchEnum(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Name tname, hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, T0>> pairs, hydra.core.Term v1) {
    return hydra.Lexical.<T0>matchUnion(
      cx,
      graph,
      tname,
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.Name, T0>, hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0>>>>) (pair -> hydra.Lexical.matchUnitField(
          hydra.lib.pairs.First.apply(pair),
          hydra.lib.pairs.Second.apply(pair))),
        pairs),
      v1);
  }

  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T1> matchRecord(hydra.context.Context cx, T0 graph, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T1>> decode, hydra.core.Term term) {
    hydra.core.Term stripped = hydra.Rewriting.deannotateAndDetypeTerm(term);
    return (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T1> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, T1>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          "expected a record, got ",
          hydra.show.Core.term(term)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T1> visit(hydra.core.Term.Record record) {
        return (decode).apply(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.core.Name, hydra.core.Term>>) (field -> (hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>((field).name, (field).term)))),
          (record).value.fields)));
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0> matchUnion(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Name tname, hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0>>>> pairs, hydra.core.Term term) {
    hydra.core.Term stripped = hydra.Rewriting.deannotateAndDetypeTerm(term);
    return (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, T0>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "expected inject(",
          (tname).value,
          ") with one of {",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0>>>, String>) (pair -> hydra.lib.pairs.First.apply(pair).value),
              pairs)),
          "}, got ",
          hydra.show.Core.term(stripped))))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0> visit(hydra.core.Term.Variable name) {
        return hydra.lib.eithers.Bind.apply(
          hydra.Lexical.requireElement(
            cx,
            graph,
            (name).value),
          (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0>>) (el -> hydra.Lexical.<T0>matchUnion(
            cx,
            graph,
            tname,
            pairs,
            (el).term)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0> visit(hydra.core.Term.Union injection) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (injection).value.typeName.value,
            (tname).value),
          () -> hydra.Lexical.<T0>matchUnion_exp(
            cx,
            (injection).value,
            hydra.Lexical.<T0>matchUnion_mapping(pairs),
            tname),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, T0>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected injection for type ",
                (tname).value),
              ", got "),
            hydra.show.Core.term(term)))), cx))));
      }
    });
  }

  static <T0> hydra.util.PersistentMap<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0>>> matchUnion_mapping(hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0>>>> pairs) {
    return hydra.lib.maps.FromList.apply(pairs);
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0> matchUnion_exp(hydra.context.Context cx, hydra.core.Injection injection, hydra.util.PersistentMap<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0>>> mapping, hydra.core.Name tname) {
    hydra.core.Name fname = (injection).field.name;
    hydra.core.Term val = (injection).field.term;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, T0>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "no matching case for field \"",
            (fname).value),
          "\" in union type "),
        (tname).value))), cx))),
      (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, T0>>) (f -> (f).apply(val)),
      hydra.lib.maps.Lookup.apply(
        fname,
        mapping));
  }

  static <T0, T1, T2, T3> hydra.util.Pair<T0, java.util.function.Function<T2, hydra.util.Either<T3, T1>>> matchUnitField(T0 fname, T1 x) {
    return (hydra.util.Pair<T0, java.util.function.Function<T2, hydra.util.Either<T3, T1>>>) ((hydra.util.Pair<T0, java.util.function.Function<T2, hydra.util.Either<T3, T1>>>) (new hydra.util.Pair<T0, java.util.function.Function<T2, hydra.util.Either<T3, T1>>>(fname, (java.util.function.Function<T2, hydra.util.Either<T3, T1>>) (ignored -> hydra.util.Either.<T3, T1>right(x)))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Binding> requireElement(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Name name) {
    Boolean showAll = false;
    java.util.function.Function<hydra.util.ConsList<String>, hydra.util.ConsList<String>> ellipsis = (java.util.function.Function<hydra.util.ConsList<String>, hydra.util.ConsList<String>>) (strings -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Gt.apply(
          hydra.lib.lists.Length.apply(strings),
          3),
        hydra.lib.logic.Not.apply(showAll)),
      () -> hydra.lib.lists.Concat2.apply(
        hydra.lib.lists.Take.apply(
          3,
          strings),
        hydra.util.ConsList.of("...")),
      () -> strings));
    hydra.util.Lazy<String> errMsg = new hydra.util.Lazy<>(() -> hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "no such element: ",
            (name).value),
          ". Available elements: {"),
        hydra.lib.strings.Intercalate.apply(
          ", ",
          (ellipsis).apply(hydra.lib.lists.Map.apply(
            wrapped -> (wrapped).value,
            hydra.lib.maps.Keys.apply((graph).boundTerms))))),
      "}"));
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Binding>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(errMsg.get())), cx))),
      (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Binding>>) (x -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Binding>right(x)),
      hydra.Lexical.dereferenceElement(
        graph,
        name));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.graph.Primitive> requirePrimitive(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Name name) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.graph.Primitive>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
        "no such primitive function: ",
        (name).value))), cx))),
      (java.util.function.Function<hydra.graph.Primitive, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.graph.Primitive>>) (x -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.graph.Primitive>right(x)),
      hydra.Lexical.lookupPrimitive(
        graph,
        name));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.TypeScheme> requirePrimitiveType(hydra.context.Context cx, hydra.graph.Graph tx, hydra.core.Name name) {
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.TypeScheme>> mts = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.graph.Primitive, hydra.core.TypeScheme>) (_p -> (_p).type),
      hydra.lib.maps.Lookup.apply(
        name,
        (tx).primitives)));
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.TypeScheme>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
        "no such primitive function: ",
        (name).value))), cx))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.TypeScheme>>) (ts -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.TypeScheme>right(ts)),
      mts.get());
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> requireTerm(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Name name) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
        "no such element: ",
        (name).value))), cx))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (x -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(x)),
      hydra.Lexical.resolveTerm(
        graph,
        name));
  }

  static hydra.util.Maybe<hydra.core.Term> resolveTerm(hydra.graph.Graph graph, hydra.core.Name name) {
    java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Term>> recurse = (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Term>>) (term -> {
      hydra.core.Term stripped = hydra.Rewriting.deannotateTerm(term);
      return (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Term> otherwise(hydra.core.Term instance) {
          return hydra.util.Maybe.just(term);
        }

        @Override
        public hydra.util.Maybe<hydra.core.Term> visit(hydra.core.Term.Variable name_) {
          return hydra.Lexical.resolveTerm(
            graph,
            (name_).value);
        }
      });
    });
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()),
      recurse,
      hydra.Lexical.lookupTerm(
        graph,
        name));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> stripAndDereferenceTerm(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term term) {
    hydra.core.Term stripped = hydra.Rewriting.deannotateAndDetypeTerm(term);
    return (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(stripped);
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Variable v) {
        return hydra.lib.eithers.Bind.apply(
          hydra.Lexical.requireTerm(
            cx,
            graph,
            (v).value),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (t -> hydra.Lexical.stripAndDereferenceTerm(
            cx,
            graph,
            t)));
      }
    });
  }

  static hydra.util.Either<String, hydra.core.Term> stripAndDereferenceTermEither(hydra.graph.Graph graph, hydra.core.Term term) {
    hydra.core.Term stripped = hydra.Rewriting.deannotateAndDetypeTerm(term);
    return (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<String, hydra.core.Term>right(stripped);
      }

      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Term.Variable v) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (left_ -> hydra.util.Either.<String, hydra.core.Term>left(left_)),
          (java.util.function.Function<hydra.core.Binding, hydra.util.Either<String, hydra.core.Term>>) (binding -> hydra.Lexical.stripAndDereferenceTermEither(
            graph,
            (binding).term)),
          hydra.Lexical.dereferenceVariable(
            graph,
            (v).value));
      }
    });
  }
}
