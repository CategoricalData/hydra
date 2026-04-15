// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Functions for generating domain-specific DSL modules from type modules
 */
public interface Dsls {
  static java.util.List<hydra.core.Name> collectForallVars(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Annotated at) {
        return hydra.Dsls.collectForallVars((at).value.body);
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.lib.lists.Cons.apply(
          (ft).value.parameter,
          hydra.Dsls.collectForallVars((ft).value.body));
      }
    });
  }

  static java.util.List<hydra.core.Binding> deduplicateBindings(java.util.List<hydra.core.Binding> bindings) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>>) (acc -> (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (b -> {
        String n = (b).name.value;
        hydra.util.Lazy<java.util.List<String>> usedNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, String>) (a -> (a).name.value),
          acc));
        String uniqueName = hydra.Dsls.findUniqueName(
          n,
          usedNames.get());
        return hydra.lib.lists.Concat2.apply(
          acc,
          java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name(uniqueName), (b).term, (b).type)));
      })),
      (java.util.List<hydra.core.Binding>) (java.util.Collections.<hydra.core.Binding>emptyList()),
      bindings);
  }

  static hydra.core.Name dslBindingName(hydra.core.Name n) {
    java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      (n).value);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Tail.apply(parts))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Head.apply(parts),
          "hydra"),
        () -> new hydra.core.Name(hydra.lib.strings.Intercalate.apply(
          ".",
          hydra.lib.lists.Concat2.apply(
            java.util.Arrays.asList(
              "hydra",
              "dsl"),
            hydra.lib.lists.Concat2.apply(
              hydra.lib.lists.Tail.apply(hydra.lib.lists.Init.apply(parts)),
              java.util.Arrays.asList(hydra.Formatting.decapitalize(hydra.Names.localNameOf(n))))))),
        () -> new hydra.core.Name(hydra.lib.strings.Intercalate.apply(
          ".",
          hydra.lib.lists.Concat2.apply(
            java.util.Arrays.asList(
              "hydra",
              "dsl"),
            hydra.lib.lists.Concat2.apply(
              hydra.lib.lists.Init.apply(parts),
              java.util.Arrays.asList(hydra.Formatting.decapitalize(hydra.Names.localNameOf(n)))))))),
      () -> new hydra.core.Name(hydra.Formatting.decapitalize(hydra.Names.localNameOf(n))));
  }

  static hydra.core.Name dslDefinitionName(hydra.core.Name typeName, String localName) {
    java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      (typeName).value);
    hydra.util.Lazy<java.util.List<String>> nsParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Init.apply(parts));
    hydra.util.Lazy<java.util.List<String>> dslNsParts = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Head.apply(nsParts.get()),
        "hydra"),
      () -> hydra.lib.lists.Concat2.apply(
        java.util.Arrays.asList(
          "hydra",
          "dsl"),
        hydra.lib.lists.Tail.apply(nsParts.get())),
      () -> hydra.lib.lists.Concat2.apply(
        java.util.Arrays.asList(
          "hydra",
          "dsl"),
        nsParts.get())));
    return new hydra.core.Name(hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.lists.Concat2.apply(
        dslNsParts.get(),
        java.util.Arrays.asList(localName))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.packaging.Module>> dslModule(T0 cx, hydra.graph.Graph graph, hydra.packaging.Module mod) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Dsls.filterTypeBindings(
        cx,
        graph,
        hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
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
          (mod).definitions))),
      (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.packaging.Module>>>) (typeBindings -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(typeBindings),
        () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.packaging.Module>>right((hydra.util.Maybe<hydra.packaging.Module>) (hydra.util.Maybe.<hydra.packaging.Module>nothing())),
        () -> hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Binding>>>) (b -> hydra.lib.eithers.Bimap.apply(
              (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Decoding(_e)),
              (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>) (x -> x),
              hydra.Dsls.<T0>generateBindingsForType(
                cx,
                graph,
                b))),
            typeBindings),
          (java.util.function.Function<java.util.List<java.util.List<hydra.core.Binding>>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.packaging.Module>>>) (dslBindings -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.packaging.Module>>right(hydra.util.Maybe.just(new hydra.packaging.Module(hydra.Dsls.dslNamespace((mod).namespace), hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.packaging.Definition>) (b -> new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition((b).name, (b).term, (b).type))),
            hydra.Dsls.deduplicateBindings(hydra.lib.lists.Concat.apply(dslBindings))), hydra.lib.lists.Nub.apply(hydra.lib.lists.Map.apply(
            hydra.Dsls::dslNamespace,
            (mod).typeDependencies)), hydra.lib.lists.Nub.apply(hydra.lib.lists.Concat2.apply(
            java.util.Arrays.asList(
              (mod).namespace,
              new hydra.packaging.Namespace("hydra.phantoms")),
            (mod).typeDependencies)), hydra.util.Maybe.just(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "DSL functions for ",
            (mod).namespace.value)))))))))));
  }

  static hydra.packaging.Namespace dslNamespace(hydra.packaging.Namespace ns) {
    java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      (ns).value);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Head.apply(parts),
        "hydra"),
      () -> new hydra.packaging.Namespace(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        "hydra.dsl.",
        hydra.lib.strings.Intercalate.apply(
          ".",
          hydra.lib.lists.Tail.apply(parts))))),
      () -> new hydra.packaging.Namespace(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        "hydra.dsl.",
        (ns).value))));
  }

  static hydra.core.TypeScheme dslTypeScheme(hydra.core.Type origType, java.util.List<hydra.core.Type> paramTypes, hydra.core.Type resultType) {
    hydra.core.Type wrappedResult = new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.phantoms.TTerm")), resultType));
    hydra.util.Lazy<hydra.core.Type> funType = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldr.apply(
      (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (paramType -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (acc -> new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.phantoms.TTerm")), paramType)), acc)))),
      wrappedResult,
      paramTypes));
    java.util.List<hydra.core.Name> typeVars = hydra.Dsls.collectForallVars(origType);
    return new hydra.core.TypeScheme(typeVars, funType.get(), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()));
  }

  static <T0, T1, T2> hydra.util.Either<T2, java.util.List<hydra.core.Binding>> filterTypeBindings(T0 cx, T1 graph, java.util.List<hydra.core.Binding> bindings) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.core.Binding>>, java.util.List<hydra.core.Binding>>) (hydra.lib.maybes.Cat::apply),
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Either<T2, hydra.util.Maybe<hydra.core.Binding>>>) (v1 -> hydra.Dsls.<T0, T1, T2>isDslEligibleBinding(
          cx,
          graph,
          v1)),
        hydra.lib.lists.Filter.apply(
          hydra.Annotations::isNativeType,
          bindings)));
  }

  static String findUniqueName(String candidate, java.util.List<String> usedNames) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
        (java.util.function.Function<String, Boolean>) (v1 -> hydra.lib.equality.Equal.apply(
          candidate,
          v1)),
        usedNames)),
      () -> candidate,
      () -> hydra.Dsls.findUniqueName(
        hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          candidate,
          "_")),
        usedNames));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.core.Binding>> generateBindingsForType(T0 cx, hydra.graph.Graph graph, hydra.core.Binding b) {
    hydra.core.Name typeName = (b).name;
    return hydra.lib.eithers.Bind.apply(
      hydra.decode.Core.type(
        graph,
        (b).term),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.core.Binding>>>) (rawType -> {
        hydra.core.Type typ = hydra.Strip.deannotateTypeParameters(hydra.Strip.deannotateType(rawType));
        return hydra.util.Either.<hydra.errors.DecodingError, java.util.List<hydra.core.Binding>>right((typ).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public java.util.List<hydra.core.Binding> otherwise(hydra.core.Type instance) {
            return (java.util.List<hydra.core.Binding>) (java.util.Collections.<hydra.core.Binding>emptyList());
          }

          @Override
          public java.util.List<hydra.core.Binding> visit(hydra.core.Type.Record fts) {
            return hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              hydra.Dsls.generateRecordConstructor(
                rawType,
                typeName,
                (fts).value),
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.FieldType, hydra.core.Binding>) (v1 -> hydra.Dsls.generateRecordAccessor(
                  rawType,
                  typeName,
                  v1)),
                (fts).value),
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.FieldType, hydra.core.Binding>) (v1 -> hydra.Dsls.generateRecordWithUpdater(
                  rawType,
                  typeName,
                  (fts).value,
                  v1)),
                (fts).value)));
          }

          @Override
          public java.util.List<hydra.core.Binding> visit(hydra.core.Type.Union fts) {
            return hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.FieldType, hydra.core.Binding>) (v1 -> hydra.Dsls.generateUnionInjector(
                rawType,
                typeName,
                v1)),
              (fts).value);
          }

          @Override
          public java.util.List<hydra.core.Binding> visit(hydra.core.Type.Wrap innerType) {
            return hydra.Dsls.generateWrappedTypeAccessors(
              rawType,
              typeName,
              (innerType).value);
          }
        }));
      }));
  }

  static hydra.core.Binding generateRecordAccessor(hydra.core.Type origType, hydra.core.Name typeName, hydra.core.FieldType ft) {
    hydra.core.Name fieldName = (ft).name;
    hydra.util.Lazy<String> accessorLocalName = new hydra.util.Lazy<>(() -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      hydra.Formatting.decapitalize(hydra.Names.localNameOf(typeName)),
      hydra.lib.strings.Intercalate.apply(
        "",
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, String>) (s -> hydra.Formatting.capitalize(s)),
          hydra.lib.strings.SplitOn.apply(
            ".",
            (fieldName).value))))));
    hydra.core.Name accessorName = hydra.Dsls.dslDefinitionName(
      typeName,
      accessorLocalName.get());
    hydra.core.Type paramDomain = new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.phantoms.TTerm")), hydra.Dsls.nominalResultType(
      typeName,
      origType)));
    hydra.core.Term body = new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), hydra.util.Maybe.just(paramDomain), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.phantoms.TTerm"), new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("application"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Application"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("project"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Projection"), java.util.Arrays.asList(
        new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((typeName).value))))),
        new hydra.core.Field(new hydra.core.Name("field"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((fieldName).value)))))))))))),
      new hydra.core.Field(new hydra.core.Name("argument"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.phantoms.TTerm")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))))));
    hydra.core.TypeScheme ts = hydra.Dsls.dslTypeScheme(
      origType,
      java.util.Arrays.asList(hydra.Dsls.nominalResultType(
        typeName,
        origType)),
      (ft).type);
    return new hydra.core.Binding(accessorName, body, hydra.util.Maybe.just(ts));
  }

  static java.util.List<hydra.core.Binding> generateRecordConstructor(hydra.core.Type origType, hydra.core.Name typeName, java.util.List<hydra.core.FieldType> fieldTypes) {
    hydra.util.Lazy<java.util.List<hydra.util.Pair<String, hydra.core.Type>>> paramPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<String, hydra.core.Type>>) (ft -> (hydra.util.Pair<String, hydra.core.Type>) ((hydra.util.Pair<String, hydra.core.Type>) (new hydra.util.Pair<String, hydra.core.Type>(hydra.Formatting.decapitalize(hydra.Names.localNameOf((ft).name)), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.phantoms.TTerm")), (ft).type)))))),
      fieldTypes));
    hydra.util.Lazy<java.util.List<hydra.core.Term>> dFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.core.Term>) (ft -> new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Field"), java.util.Arrays.asList(
        new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((ft).name.value))))),
        new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.phantoms.TTerm")), new hydra.core.Term.Variable(new hydra.core.Name(hydra.Formatting.decapitalize(hydra.Names.localNameOf((ft).name))))))))))),
      fieldTypes));
    hydra.core.Term recordTerm = new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.phantoms.TTerm"), new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("record"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Record"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((typeName).value))))),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.List(dFields.get()))))))))));
    hydra.util.Lazy<hydra.core.Term> body = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Pair<String, hydra.core.Type>, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.util.Pair<String, hydra.core.Type>, hydra.core.Term>) (pp -> new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name(hydra.lib.pairs.First.apply(pp)), hydra.util.Maybe.just(hydra.lib.pairs.Second.apply(pp)), acc)))),
      recordTerm,
      hydra.lib.lists.Reverse.apply(paramPairs.get())));
    hydra.util.Lazy<java.util.List<hydra.core.Type>> paramTypes = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.core.Type>) (ft -> (ft).type),
      fieldTypes));
    hydra.core.Type resultType = hydra.Dsls.nominalResultType(
      typeName,
      origType);
    hydra.core.TypeScheme ts = hydra.Dsls.dslTypeScheme(
      origType,
      paramTypes.get(),
      resultType);
    return java.util.Arrays.asList(new hydra.core.Binding(hydra.Dsls.dslBindingName(typeName), body.get(), hydra.util.Maybe.just(ts)));
  }

  static hydra.core.Binding generateRecordWithUpdater(hydra.core.Type origType, hydra.core.Name typeName, java.util.List<hydra.core.FieldType> allFields, hydra.core.FieldType targetField) {
    hydra.core.Name targetFieldName = (targetField).name;
    hydra.util.Lazy<java.util.List<hydra.core.Term>> dFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.core.Term>) (ft -> new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Field"), java.util.Arrays.asList(
        new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((ft).name.value))))),
        new hydra.core.Field(new hydra.core.Name("term"), hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (ft).name.value,
            (targetFieldName).value),
          () -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.phantoms.TTerm")), new hydra.core.Term.Variable(new hydra.core.Name("newVal")))),
          () -> new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("application"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Application"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("function"), new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("project"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Projection"), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((typeName).value))))),
              new hydra.core.Field(new hydra.core.Name("field"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((ft).name.value)))))))))))),
            new hydra.core.Field(new hydra.core.Name("argument"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.phantoms.TTerm")), new hydra.core.Term.Variable(new hydra.core.Name("original"))))))))))))))))),
      allFields));
    hydra.core.Type fieldDomain = new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.phantoms.TTerm")), (targetField).type));
    hydra.core.Type recDomain = new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.phantoms.TTerm")), hydra.Dsls.nominalResultType(
      typeName,
      origType)));
    hydra.core.Term body = new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("original"), hydra.util.Maybe.just(recDomain), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("newVal"), hydra.util.Maybe.just(fieldDomain), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.phantoms.TTerm"), new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("record"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Record"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((typeName).value))))),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.List(dFields.get()))))))))))))));
    hydra.core.Type recType = hydra.Dsls.nominalResultType(
      typeName,
      origType);
    hydra.core.TypeScheme ts = hydra.Dsls.dslTypeScheme(
      origType,
      java.util.Arrays.asList(
        recType,
        (targetField).type),
      recType);
    hydra.util.Lazy<String> updaterLocalName = new hydra.util.Lazy<>(() -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      hydra.Formatting.decapitalize(hydra.Names.localNameOf(typeName)),
      "With",
      hydra.lib.strings.Intercalate.apply(
        "",
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, String>) (s -> hydra.Formatting.capitalize(s)),
          hydra.lib.strings.SplitOn.apply(
            ".",
            (targetFieldName).value))))));
    hydra.core.Name updaterName = hydra.Dsls.dslDefinitionName(
      typeName,
      updaterLocalName.get());
    return new hydra.core.Binding(updaterName, body, hydra.util.Maybe.just(ts));
  }

  static hydra.core.Binding generateUnionInjector(hydra.core.Type origType, hydra.core.Name typeName, hydra.core.FieldType ft) {
    hydra.core.Type fieldType = (ft).type;
    Boolean isUnit = hydra.Strip.deannotateType(fieldType).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Type.Unit ignored) {
        return true;
      }
    });
    hydra.util.Lazy<hydra.core.Term> dFieldValue = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      isUnit,
      () -> new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit()))),
      () -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.phantoms.TTerm")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))));
    hydra.core.Name fieldName = (ft).name;
    hydra.core.Term injectionTerm = new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.phantoms.TTerm"), new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("inject"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Injection"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((typeName).value))))),
      new hydra.core.Field(new hydra.core.Name("field"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Field"), java.util.Arrays.asList(
        new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((fieldName).value))))),
        new hydra.core.Field(new hydra.core.Name("term"), dFieldValue.get())))))))))))));
    hydra.core.Type variantDomain = new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.phantoms.TTerm")), (ft).type));
    hydra.util.Lazy<hydra.core.Term> body = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      isUnit,
      () -> injectionTerm,
      () -> new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), hydra.util.Maybe.just(variantDomain), injectionTerm))));
    hydra.util.Lazy<String> injectorLocalName = new hydra.util.Lazy<>(() -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      hydra.Formatting.decapitalize(hydra.Names.localNameOf(typeName)),
      hydra.lib.strings.Intercalate.apply(
        "",
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, String>) (s -> hydra.Formatting.capitalize(s)),
          hydra.lib.strings.SplitOn.apply(
            ".",
            (fieldName).value))))));
    hydra.core.Name injectorName = hydra.Dsls.dslDefinitionName(
      typeName,
      injectorLocalName.get());
    hydra.core.Type unionType = hydra.Dsls.nominalResultType(
      typeName,
      origType);
    hydra.util.Lazy<hydra.core.TypeScheme> ts = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      isUnit,
      () -> hydra.Dsls.dslTypeScheme(
        origType,
        (java.util.List<hydra.core.Type>) (java.util.Collections.<hydra.core.Type>emptyList()),
        unionType),
      () -> hydra.Dsls.dslTypeScheme(
        origType,
        java.util.Arrays.asList((ft).type),
        unionType)));
    return new hydra.core.Binding(injectorName, body.get(), hydra.util.Maybe.just(ts.get()));
  }

  static java.util.List<hydra.core.Binding> generateWrappedTypeAccessors(hydra.core.Type origType, hydra.core.Name typeName, hydra.core.Type innerType) {
    String localName = hydra.Names.localNameOf(typeName);
    hydra.core.Type wrapperType = hydra.Dsls.nominalResultType(
      typeName,
      origType);
    hydra.core.Type unwrapDomain = new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.phantoms.TTerm")), wrapperType));
    hydra.core.Term unwrapBody = new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), hydra.util.Maybe.just(unwrapDomain), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.phantoms.TTerm"), new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("application"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Application"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("unwrap"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((typeName).value)))))))),
      new hydra.core.Field(new hydra.core.Name("argument"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.phantoms.TTerm")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))))));
    String unwrapLocalName = hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "un",
      localName));
    hydra.core.Name unwrapName = hydra.Dsls.dslDefinitionName(
      typeName,
      unwrapLocalName);
    hydra.core.TypeScheme unwrapTs = hydra.Dsls.dslTypeScheme(
      origType,
      java.util.Arrays.asList(wrapperType),
      innerType);
    hydra.core.Type wrapDomain = new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.phantoms.TTerm")), innerType));
    hydra.core.Term wrapBody = new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), hydra.util.Maybe.just(wrapDomain), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.phantoms.TTerm"), new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("wrap"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.WrappedTerm"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((typeName).value))))),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.phantoms.TTerm")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))))));
    hydra.core.Name wrapName = hydra.Dsls.dslDefinitionName(
      typeName,
      hydra.Formatting.decapitalize(localName));
    hydra.core.TypeScheme wrapTs = hydra.Dsls.dslTypeScheme(
      origType,
      java.util.Arrays.asList(innerType),
      wrapperType);
    return java.util.Arrays.asList(
      new hydra.core.Binding(wrapName, wrapBody, hydra.util.Maybe.just(wrapTs)),
      new hydra.core.Binding(unwrapName, unwrapBody, hydra.util.Maybe.just(unwrapTs)));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.util.Maybe<hydra.core.Binding>> isDslEligibleBinding(T0 cx, T1 graph, hydra.core.Binding b) {
    hydra.util.Maybe<hydra.packaging.Namespace> ns = hydra.Names.namespaceOf((b).name);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.maybes.Maybe.applyLazy(
          () -> "",
          wrapped -> (wrapped).value,
          ns),
        "hydra.phantoms"),
      () -> hydra.util.Either.<T2, hydra.util.Maybe<hydra.core.Binding>>right((hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing())),
      () -> hydra.util.Either.<T2, hydra.util.Maybe<hydra.core.Binding>>right(hydra.util.Maybe.just(b)));
  }

  static hydra.core.Type nominalResultType(hydra.core.Name typeName, hydra.core.Type origType) {
    java.util.List<hydra.core.Name> vars = hydra.Dsls.collectForallVars(origType);
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Name, hydra.core.Type>>) (acc -> (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (v -> new hydra.core.Type.Application(new hydra.core.ApplicationType(acc, new hydra.core.Type.Variable(v))))),
      new hydra.core.Type.Variable(typeName),
      vars);
  }
}
