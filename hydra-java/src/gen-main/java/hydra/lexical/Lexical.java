// Note: this is an automatically generated file. Do not edit.

package hydra.lexical;

/**
 * A module for lexical operations over graphs.
 */
public interface Lexical {
  static hydra.core.Name chooseUniqueName(java.util.Set<hydra.core.Name> reserved, hydra.core.Name name) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Integer, hydra.core.Name>> tryName = new java.util.concurrent.atomic.AtomicReference<>();
    tryName.set((java.util.function.Function<Integer, hydra.core.Name>) (index -> {
      hydra.util.Lazy<hydra.core.Name> candidate = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          (index),
          1),
        () -> (name),
        () -> new hydra.core.Name(hydra.lib.strings.Cat2.apply(
          ((name)).value,
          hydra.lib.literals.ShowInt32.apply((index))))));
      return hydra.lib.logic.IfElse.lazy(
        hydra.lib.sets.Member.apply(
          candidate.get(),
          (reserved)),
        () -> (tryName.get()).apply(hydra.lib.math.Add.apply(
          (index),
          1)),
        () -> candidate.get());
    }));
    return (tryName.get()).apply(1);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Binding>> dereferenceElement(hydra.core.Name name) {
    return hydra.lib.flows.Map.apply(
      (java.util.function.Function<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Binding>>) (g -> hydra.lexical.Lexical.lookupElement(
        (g),
        (name))),
      hydra.monads.Monads.<hydra.graph.Graph>getState());
  }
  
  static hydra.util.Maybe<hydra.core.TypeScheme> dereferenceSchemaType(hydra.core.Name name, java.util.Map<hydra.core.Name, hydra.core.TypeScheme> types2) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>> forType = new java.util.concurrent.atomic.AtomicReference<>();
    forType.set((java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>) (t -> ((t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.TypeScheme> otherwise(hydra.core.Type instance) {
        return hydra.util.Maybe.just(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), (t), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())));
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.TypeScheme> visit(hydra.core.Type.Annotated at) {
        return (forType.get()).apply((((at)).value).body);
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.TypeScheme> visit(hydra.core.Type.Forall ft) {
        return hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (ts -> new hydra.core.TypeScheme(hydra.lib.lists.Cons.apply(
            (((ft)).value).parameter,
            ((ts)).variables), ((ts)).type, ((ts)).constraints)),
          (forType.get()).apply((((ft)).value).body));
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.TypeScheme> visit(hydra.core.Type.Variable v) {
        return hydra.lexical.Lexical.dereferenceSchemaType(
          ((v)).value,
          (types2));
      }
    })));
    return hydra.lib.maybes.Bind.apply(
      hydra.lib.maps.Lookup.apply(
        (name),
        (types2)),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Maybe<hydra.core.TypeScheme>>) (ts -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (ts2 -> new hydra.core.TypeScheme(hydra.lib.lists.Concat2.apply(
          ((ts)).variables,
          ((ts2)).variables), ((ts2)).type, ((ts2)).constraints)),
        (forType.get()).apply(((ts)).type))));
  }
  
  static hydra.util.Either<String, hydra.core.Binding> dereferenceVariable(hydra.graph.Graph g, hydra.core.Name name) {
    return hydra.lib.maybes.Maybe.apply(
      (hydra.util.Either<String, hydra.core.Binding>) ((hydra.util.Either<String, hydra.core.Binding>) (hydra.util.Either.<String, hydra.core.Binding>left(hydra.lib.strings.Cat2.apply(
        "no such element: ",
        ((name)).value)))),
      (java.util.function.Function<hydra.core.Binding, hydra.util.Either<String, hydra.core.Binding>>) (right_ -> (hydra.util.Either<String, hydra.core.Binding>) ((hydra.util.Either<String, hydra.core.Binding>) (hydra.util.Either.<String, hydra.core.Binding>right((right_))))),
      hydra.lexical.Lexical.lookupElement(
        (g),
        (name)));
  }
  
  static hydra.graph.Graph elementsToGraph(hydra.graph.Graph parent, hydra.util.Maybe<hydra.graph.Graph> schema, java.util.List<hydra.core.Binding> elements) {
    return new hydra.graph.Graph((elements), ((parent)).environment, ((parent)).types, ((parent)).body, ((parent)).primitives, (schema));
  }
  
  static hydra.graph.Graph emptyGraph() {
    return new hydra.graph.Graph((java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of()), (java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), new hydra.core.Term.Literal(new hydra.core.Literal.String_("empty graph")), (java.util.Map<hydra.core.Name, hydra.graph.Primitive>) ((java.util.Map<hydra.core.Name, hydra.graph.Primitive>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.graph.Primitive>apply())), (hydra.util.Maybe<hydra.graph.Graph>) (hydra.util.Maybe.<hydra.graph.Graph>nothing()));
  }
  
  static hydra.graph.Graph extendGraphWithBindings(java.util.List<hydra.core.Binding> bindings, hydra.graph.Graph g) {
    return new hydra.graph.Graph(hydra.lib.lists.Concat2.apply(
      (bindings),
      ((g)).elements), ((g)).environment, ((g)).types, ((g)).body, ((g)).primitives, ((g)).schema);
  }
  
  static java.util.List<hydra.core.FieldType> fieldsOf(hydra.core.Type t) {
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType((t));
    return ((stripped)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.FieldType> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.core.FieldType>) (java.util.List.<hydra.core.FieldType>of());
      }
      
      @Override
      public java.util.List<hydra.core.FieldType> visit(hydra.core.Type.Forall forallType) {
        return hydra.lexical.Lexical.fieldsOf((((forallType)).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.FieldType> visit(hydra.core.Type.Record rt) {
        return (((rt)).value).fields;
      }
      
      @Override
      public java.util.List<hydra.core.FieldType> visit(hydra.core.Type.Union rt) {
        return (((rt)).value).fields;
      }
    });
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T1, T2> getField(java.util.Map<hydra.core.Name, T0> m, hydra.core.Name fname, java.util.function.Function<T0, hydra.compute.Flow<T1, T2>> decode) {
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "expected field ",
          ((fname)).value),
        " not found")),
      (decode),
      hydra.lib.maps.Lookup.apply(
        (fname),
        (m)));
  }
  
  static hydra.util.Maybe<hydra.core.Binding> lookupElement(hydra.graph.Graph g, hydra.core.Name name) {
    return hydra.lib.lists.Find.apply(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
        ((b)).name,
        (name))),
      ((g)).elements);
  }
  
  static hydra.util.Maybe<hydra.graph.Primitive> lookupPrimitive(hydra.graph.Graph g, hydra.core.Name name) {
    return hydra.lib.maps.Lookup.apply(
      (name),
      ((g)).primitives);
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, T0> matchEnum(hydra.core.Name tname, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, T0>> pairs, hydra.core.Term v1) {
    return hydra.lexical.Lexical.<T0>matchUnion(
      (tname),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, T0>, hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T0>>>>) (pair -> hydra.lexical.Lexical.matchUnitField(
          hydra.lib.pairs.First.apply((pair)),
          hydra.lib.pairs.Second.apply((pair)))),
        (pairs)),
      (v1));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> matchRecord(java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, hydra.compute.Flow<T0, T1>> decode, hydra.core.Term term) {
    hydra.core.Term stripped = hydra.rewriting.Rewriting.deannotateAndDetypeTerm((term));
    return ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, T1> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.<T0, T1>unexpected(
          "record",
          hydra.show.core.Core.term((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, T1> visit(hydra.core.Term.Record record) {
        return ((decode)).apply(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>) (field -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>(((field)).name, ((field)).term)))),
          (((record)).value).fields)));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, T0> matchUnion(hydra.core.Name tname, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T0>>>> pairs, hydra.core.Term term) {
    hydra.core.Term stripped = hydra.rewriting.Rewriting.deannotateAndDetypeTerm((term));
    return ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, T0> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          hydra.lib.strings.Cat.apply(java.util.List.of(
            "inject(",
            ((tname)).value,
            ") with one of {",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, T0>>>, String>) (pair -> (hydra.lib.pairs.First.apply((pair))).value),
                (pairs))),
            "}")),
          hydra.show.core.Core.term((stripped)));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, T0> visit(hydra.core.Term.Variable name) {
        return hydra.lib.flows.Bind.apply(
          hydra.lexical.Lexical.requireElement(((name)).value),
          (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, T0>>) (el -> hydra.lexical.Lexical.<T0>matchUnion(
            (tname),
            (pairs),
            ((el)).term)));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, T0> visit(hydra.core.Term.Union injection) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            ((((injection)).value).typeName).value,
            ((tname)).value),
          () -> hydra.lexical.Lexical.matchUnion_exp(
            ((injection)).value,
            hydra.lexical.Lexical.matchUnion_mapping((pairs)),
            (tname)),
          () -> hydra.monads.Monads.unexpected(
            hydra.lib.strings.Cat2.apply(
              "injection for type ",
              ((tname)).value),
            hydra.show.core.Core.term((term))));
      }
    });
  }
  
  static <T0, T1> java.util.Map<T0, T1> matchUnion_mapping(java.util.List<hydra.util.Tuple.Tuple2<T0, T1>> pairs) {
    return hydra.lib.maps.FromList.apply((pairs));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> matchUnion_exp(hydra.core.Injection injection, java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T1>>> mapping, hydra.core.Name tname) {
    hydra.core.Name fname = (((injection)).field).name;
    hydra.core.Term val = (((injection)).field).term;
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "no matching case for field \"",
            ((fname)).value),
          "\" in union type "),
        ((tname)).value)),
      (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T1>>, hydra.compute.Flow<T0, T1>>) (f -> ((f)).apply((val))),
      hydra.lib.maps.Lookup.apply(
        (fname),
        (mapping)));
  }
  
  static <T0, T1, T2, T3> hydra.util.Tuple.Tuple2<T0, java.util.function.Function<T2, hydra.compute.Flow<T3, T1>>> matchUnitField(T0 fname, T1 x) {
    return (hydra.util.Tuple.Tuple2<T0, java.util.function.Function<T2, hydra.compute.Flow<T3, T1>>>) ((hydra.util.Tuple.Tuple2<T0, java.util.function.Function<T2, hydra.compute.Flow<T3, T1>>>) (new hydra.util.Tuple.Tuple2<T0, java.util.function.Function<T2, hydra.compute.Flow<T3, T1>>>((fname), (java.util.function.Function<T2, hydra.compute.Flow<T3, T1>>) (ignored -> hydra.lib.flows.Pure.apply((x))))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Binding> requireElement(hydra.core.Name name) {
    Boolean showAll = false;
    java.util.function.Function<java.util.List<String>, java.util.List<String>> ellipsis = (java.util.function.Function<java.util.List<String>, java.util.List<String>>) (strings -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Gt.apply(
          hydra.lib.lists.Length.apply((strings)),
          3),
        hydra.lib.logic.Not.apply((showAll))),
      () -> hydra.lib.lists.Concat2.apply(
        hydra.lib.lists.Take.apply(
          3,
          (strings)),
        java.util.List.of("...")),
      () -> (strings)));
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.dereferenceElement((name)),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Binding>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Binding>>) (mel -> hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Bind.apply(
          hydra.monads.Monads.<hydra.graph.Graph>getState(),
          (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Binding>>) (g -> ((java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Binding>>) (v1 -> hydra.lexical.Lexical.requireElement_err(
            (ellipsis),
            (name),
            (v1)))).apply((g)))),
        (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Binding>>) ((java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Binding>>) ((hydra.lib.flows.Pure::apply))),
        (mel))));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> requireElement_err(java.util.function.Function<java.util.List<String>, java.util.List<String>> ellipsis, hydra.core.Name name, hydra.graph.Graph g) {
    return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "no such element: ",
            ((name)).value),
          ". Available elements: {"),
        hydra.lib.strings.Intercalate.apply(
          ", ",
          ((ellipsis)).apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Binding, String>) (el -> (((el)).name).value),
            ((g)).elements)))),
      "}"));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Primitive> requirePrimitive(hydra.core.Name name) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Primitive>>) (g -> hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
          "no such primitive function: ",
          ((name)).value)),
        (java.util.function.Function<hydra.graph.Primitive, hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Primitive>>) ((java.util.function.Function<hydra.graph.Primitive, hydra.compute.Flow<hydra.graph.Graph, hydra.graph.Primitive>>) ((hydra.lib.flows.Pure::apply))),
        hydra.lexical.Lexical.lookupPrimitive(
          (g),
          (name)))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.TypeScheme> requirePrimitiveType(hydra.typing.TypeContext tx, hydra.core.Name name) {
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.TypeScheme>> mts = new hydra.util.Lazy<>(() -> hydra.lib.maps.Lookup.apply(
      (name),
      (((tx)).inferenceContext).primitiveTypes));
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        "no such primitive function: ",
        ((name)).value)),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.core.TypeScheme>>) (ts -> hydra.lib.flows.Pure.apply((ts))),
      mts.get());
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> requireTerm(hydra.core.Name name) {
    return hydra.lib.flows.Bind.apply(
      hydra.lexical.Lexical.resolveTerm((name)),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (mt -> hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
          "no such element: ",
          ((name)).value)),
        (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) ((hydra.lib.flows.Pure::apply))),
        (mt))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Term>> resolveTerm(hydra.core.Name name) {
    java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Term>>> recurse = (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Term>>>) (el -> {
      hydra.core.Term stripped = hydra.rewriting.Rewriting.deannotateTerm(((el)).term);
      return ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Term>> otherwise(hydra.core.Term instance) {
          return hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(((el)).term));
        }
        
        @Override
        public hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Term>> visit(hydra.core.Term.Variable name_) {
          return hydra.lexical.Lexical.resolveTerm(((name_)).value);
        }
      });
    });
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Term>>>) (g -> hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
        (recurse),
        hydra.lib.lists.Find.apply(
          (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
            ((b)).name,
            (name))),
          ((g)).elements))));
  }
  
  static hydra.graph.Graph schemaContext(hydra.graph.Graph g) {
    return hydra.lib.maybes.FromMaybe.apply(
      (g),
      ((g)).schema);
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> stripAndDereferenceTerm(hydra.core.Term term) {
    hydra.core.Term stripped = hydra.rewriting.Rewriting.deannotateAndDetypeTerm((term));
    return ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.lib.flows.Pure.apply((stripped));
      }
      
      @Override
      public hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> visit(hydra.core.Term.Variable v) {
        return hydra.lib.flows.Bind.apply(
          hydra.lexical.Lexical.requireTerm(((v)).value),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (t -> hydra.lexical.Lexical.stripAndDereferenceTerm((t))));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.core.Term> stripAndDereferenceTermEither(hydra.graph.Graph g, hydra.core.Term term) {
    hydra.core.Term stripped = hydra.rewriting.Rewriting.deannotateAndDetypeTerm((term));
    return ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>right((stripped))));
      }
      
      @Override
      public hydra.util.Either<String, hydra.core.Term> visit(hydra.core.Term.Variable v) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (left_ -> (hydra.util.Either<String, hydra.core.Term>) ((hydra.util.Either<String, hydra.core.Term>) (hydra.util.Either.<String, hydra.core.Term>left((left_))))),
          (java.util.function.Function<hydra.core.Binding, hydra.util.Either<String, hydra.core.Term>>) (binding -> hydra.lexical.Lexical.stripAndDereferenceTermEither(
            (g),
            ((binding)).term)),
          hydra.lexical.Lexical.dereferenceVariable(
            (g),
            ((v)).value));
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T1, T0> withEmptyGraph(hydra.compute.Flow<hydra.graph.Graph, T0> v1) {
    return hydra.monads.Monads.withState(
      hydra.lexical.Lexical.emptyGraph(),
      (v1));
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, T0> withSchemaContext(hydra.compute.Flow<hydra.graph.Graph, T0> f) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, T0>>) (g -> hydra.monads.Monads.withState(
        hydra.lexical.Lexical.schemaContext((g)),
        (f))));
  }
}
