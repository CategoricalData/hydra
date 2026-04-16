// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Type dereference, lookup, requirements, and instantiation
 */
public interface Resolution {
  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Type>> dereferenceType(T0 cx, hydra.graph.Graph graph, hydra.core.Name name) {
    hydra.util.Maybe<hydra.core.Binding> mel = hydra.Lexical.lookupBinding(
      graph,
      name);
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Type>>right((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
      (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Type>>>) (el -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (hydra.lib.maybes.Pure::apply),
        hydra.lib.eithers.Bimap.apply(
          (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Resolution(new hydra.errors.ResolutionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("type", (_e).value)))),
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_a -> _a),
          hydra.decode.Core.type(
            graph,
            (el).term)))),
      mel);
  }

  static Boolean fTypeIsPolymorphic(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Type.Annotated at) {
        return hydra.Resolution.fTypeIsPolymorphic((at).value.body);
      }

      @Override
      public Boolean visit(hydra.core.Type.Forall ft) {
        return true;
      }
    });
  }

  static java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap(java.util.List<hydra.core.Field> fields) {
    java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.core.Name, hydra.core.Term>> toPair = (java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.core.Name, hydra.core.Term>>) (f -> (hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>((f).name, (f).term))));
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      toPair,
      fields));
  }

  static java.util.Map<hydra.core.Name, hydra.core.Type> fieldTypeMap(java.util.List<hydra.core.FieldType> fields) {
    java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.core.Name, hydra.core.Type>> toPair = (java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (f -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>((f).name, (f).type))));
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      toPair,
      fields));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.Type>> fieldTypes(T0 cx, hydra.graph.Graph graph, hydra.core.Type t) {
    java.util.function.Function<java.util.List<hydra.core.FieldType>, java.util.Map<hydra.core.Name, hydra.core.Type>> toMap = (java.util.function.Function<java.util.List<hydra.core.FieldType>, java.util.Map<hydra.core.Name, hydra.core.Type>>) (fields -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (ft -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>((ft).name, (ft).type)))),
      fields)));
    return hydra.Strip.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.Type>> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.Type>>left(new hydra.errors.Error_.Resolution(new hydra.errors.ResolutionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("record or union type", hydra.show.Core.type(t)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.Type>> visit(hydra.core.Type.Forall ft) {
        return hydra.Resolution.<T0>fieldTypes(
          cx,
          graph,
          (ft).value.body);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.Type>> visit(hydra.core.Type.Record rt) {
        return hydra.util.Either.<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.Type>>right((toMap).apply((rt).value));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.Type>> visit(hydra.core.Type.Union rt) {
        return hydra.util.Either.<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.Type>>right((toMap).apply((rt).value));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.Type>> visit(hydra.core.Type.Variable name) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.lib.eithers.Bind.apply(
            hydra.Lexical.requireBinding(
              graph,
              (name).value),
            (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (el -> hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.Bimap.apply(
                (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Resolution(new hydra.errors.ResolutionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("type", (_e).value)))),
                (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_a -> _a),
                hydra.decode.Core.type(
                  graph,
                  (el).term)),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (decodedType -> hydra.Resolution.<T0>fieldTypes(
                cx,
                graph,
                decodedType))))),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (ts -> hydra.Resolution.<T0>fieldTypes(
            cx,
            graph,
            (ts).type)),
          hydra.lib.maps.Lookup.apply(
            (name).value,
            (graph).schemaTypes));
      }
    });
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Type> findFieldType(T0 cx, hydra.core.Name fname, java.util.List<hydra.core.FieldType> fields) {
    hydra.util.Lazy<java.util.List<hydra.core.FieldType>> matchingFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.equality.Equal.apply(
        (ft).name.value,
        (fname).value)),
      fields));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(matchingFields.get()),
      () -> hydra.Resolution.findFieldType_noMatch(fname),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(matchingFields.get()),
          1),
        () -> hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.Resolution.findFieldType_noMatch(fname),
          (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>) (ft -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>right((ft).type)),
          hydra.lib.lists.MaybeHead.apply(matchingFields.get())),
        () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.MultipleFields(new hydra.errors.MultipleFieldsError(fname))))));
  }

  static <T1> hydra.util.Either<hydra.errors.Error_, T1> findFieldType_noMatch(hydra.core.Name fname) {
    return hydra.util.Either.<hydra.errors.Error_, T1>left(new hydra.errors.Error_.Resolution(new hydra.errors.ResolutionError.NoMatchingField(new hydra.errors.NoMatchingFieldError(fname))));
  }

  static hydra.core.Type fullyStripAndNormalizeType(hydra.core.Type typ) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Integer, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<Integer, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>>>>) (depth -> (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>>>) (subst -> (java.util.function.Function<hydra.core.Type, hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>>) (t -> hydra.Strip.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>) ((hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>) (new hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>(subst, t)));
      }

      @Override
      public hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type> visit(hydra.core.Type.Forall ft) {
        hydra.core.Name newVar = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
          "_",
          hydra.lib.literals.ShowInt32.apply(depth)));
        hydra.core.Name oldVar = (ft).value.parameter;
        return go.get().apply(hydra.lib.math.Add.apply(
          depth,
          1)).apply(hydra.lib.maps.Insert.apply(
          oldVar,
          newVar,
          subst)).apply((ft).value.body);
      }
    })))));
    hydra.util.Lazy<hydra.util.Pair<java.util.Map<hydra.core.Name, hydra.core.Name>, hydra.core.Type>> result = new hydra.util.Lazy<>(() -> go.get().apply(0).apply((java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply()))).apply(typ));
    hydra.util.Lazy<hydra.core.Type> body = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result.get()));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> subst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result.get()));
    return hydra.Variables.substituteTypeVariables(
      subst.get(),
      body.get());
  }

  static hydra.core.Type fullyStripType(hydra.core.Type typ) {
    return hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return typ;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return hydra.Resolution.fullyStripType((ft).value.body);
      }
    });
  }

  static hydra.util.Pair<hydra.core.Type, hydra.context.Context> instantiateType(hydra.context.Context cx, hydra.core.Type typ) {
    hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context> result = hydra.Resolution.instantiateTypeScheme(
      cx,
      hydra.Resolution.typeToTypeScheme(typ));
    return (hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.First.apply(result)), hydra.lib.pairs.Second.apply(result))));
  }

  static hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context> instantiateTypeScheme(hydra.context.Context cx, hydra.core.TypeScheme scheme) {
    java.util.List<hydra.core.Name> oldVars = (scheme).variables;
    hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context>> result = new hydra.util.Lazy<>(() -> hydra.Names.freshNames(
      hydra.lib.lists.Length.apply(oldVars),
      cx));
    hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Name>> newVars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result.get()));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Name>> nameSubst = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      oldVars,
      newVars.get())));
    hydra.util.Lazy<hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> renamedConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (oldConstraints -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (kv -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>(hydra.lib.maybes.FromMaybe.applyLazy(
          () -> hydra.lib.pairs.First.apply(kv),
          hydra.lib.maps.Lookup.apply(
            hydra.lib.pairs.First.apply(kv),
            nameSubst.get())), hydra.lib.pairs.Second.apply(kv))))),
        hydra.lib.maps.ToList.apply(oldConstraints)))),
      (scheme).constraints));
    hydra.util.Lazy<hydra.typing.TypeSubst> subst = new hydra.util.Lazy<>(() -> new hydra.typing.TypeSubst(hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      oldVars,
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
        newVars.get())))));
    return (hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>) ((hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>) (new hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>(new hydra.core.TypeScheme(newVars.get(), hydra.Substitution.substInType(
      subst.get(),
      (scheme).type), renamedConstraints.get()), cx2.get())));
  }

  static hydra.core.Type nominalApplication(hydra.core.Name tname, java.util.List<hydra.core.Type> args) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (t -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (a -> new hydra.core.Type.Application(new hydra.core.ApplicationType(t, a)))),
      new hydra.core.Type.Variable(tname),
      args);
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.FieldType>> requireRecordType(T0 cx, hydra.graph.Graph graph, hydra.core.Name name) {
    java.util.function.Function<hydra.core.Type, hydra.util.Maybe<java.util.List<hydra.core.FieldType>>> toRecord = (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<java.util.List<hydra.core.FieldType>>>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<java.util.List<hydra.core.FieldType>> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<java.util.List<hydra.core.FieldType>>) (hydra.util.Maybe.<java.util.List<hydra.core.FieldType>>nothing());
      }

      @Override
      public hydra.util.Maybe<java.util.List<hydra.core.FieldType>> visit(hydra.core.Type.Record rt) {
        return hydra.util.Maybe.just((rt).value);
      }
    }));
    return hydra.Resolution.requireRowType(
      cx,
      "record type",
      toRecord,
      graph,
      name);
  }

  static <T0, T1> hydra.util.Either<hydra.errors.Error_, T1> requireRowType(T0 cx, String label, java.util.function.Function<hydra.core.Type, hydra.util.Maybe<T1>> getter, hydra.graph.Graph graph, hydra.core.Name name) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, hydra.core.Type>> rawType = new java.util.concurrent.atomic.AtomicReference<>();
    rawType.set((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return rawType.get().apply((at).value.body);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return rawType.get().apply((ft).value.body);
      }
    })));
    return hydra.lib.eithers.Bind.apply(
      hydra.Resolution.<T0>requireType(
        cx,
        graph,
        name),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, T1>>) (t -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<hydra.errors.Error_, T1>left(new hydra.errors.Error_.Resolution(new hydra.errors.ResolutionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError(hydra.lib.strings.Cat2.apply(
          label,
          " type"), hydra.lib.strings.Cat2.apply(
          (name).value,
          hydra.lib.strings.Cat2.apply(
            ": ",
            hydra.show.Core.type(t))))))),
        (java.util.function.Function<T1, hydra.util.Either<hydra.errors.Error_, T1>>) (x -> hydra.util.Either.<hydra.errors.Error_, T1>right(x)),
        (getter).apply(rawType.get().apply(t)))));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>> requireSchemaType(hydra.context.Context cx, java.util.Map<hydra.core.Name, hydra.core.TypeScheme> types, hydra.core.Name tname) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>>left(new hydra.errors.Error_.Resolution(new hydra.errors.ResolutionError.NoSuchBinding(new hydra.errors.NoSuchBindingError(tname)))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>>>) (ts -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>>right(hydra.Resolution.instantiateTypeScheme(
        cx,
        hydra.Strip.deannotateTypeSchemeRecursive(ts)))),
      hydra.lib.maps.Lookup.apply(
        tname,
        types));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Type> requireType(T0 cx, hydra.graph.Graph graph, hydra.core.Name name) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>left(new hydra.errors.Error_.Resolution(new hydra.errors.ResolutionError.NoSuchBinding(new hydra.errors.NoSuchBindingError(name)))),
        (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>) (ts -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>right(hydra.Scoping.typeSchemeToFType(ts))),
        hydra.lib.maps.Lookup.apply(
          name,
          (graph).boundTypes)),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>) (ts -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>right(hydra.Scoping.typeSchemeToFType(ts))),
      hydra.lib.maps.Lookup.apply(
        name,
        (graph).schemaTypes));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Type> requireUnionField(T0 cx, hydra.graph.Graph graph, hydra.core.Name tname, hydra.core.Name fname) {
    java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>> withRowType = (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>) (rt -> hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.Resolution.requireUnionField_noMatchErr(fname),
      (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>) (ft -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Type>right((ft).type)),
      hydra.lib.lists.Find.apply(
        (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.equality.Equal.apply(
          (ft).name,
          fname)),
        rt)));
    return hydra.lib.eithers.Bind.apply(
      hydra.Resolution.<T0>requireUnionType(
        cx,
        graph,
        tname),
      withRowType);
  }

  static <T1> hydra.util.Either<hydra.errors.Error_, T1> requireUnionField_noMatchErr(hydra.core.Name fname) {
    return hydra.util.Either.<hydra.errors.Error_, T1>left(new hydra.errors.Error_.Resolution(new hydra.errors.ResolutionError.NoMatchingField(new hydra.errors.NoMatchingFieldError(fname))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.FieldType>> requireUnionType(T0 cx, hydra.graph.Graph graph, hydra.core.Name name) {
    java.util.function.Function<hydra.core.Type, hydra.util.Maybe<java.util.List<hydra.core.FieldType>>> toUnion = (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<java.util.List<hydra.core.FieldType>>>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<java.util.List<hydra.core.FieldType>> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<java.util.List<hydra.core.FieldType>>) (hydra.util.Maybe.<java.util.List<hydra.core.FieldType>>nothing());
      }

      @Override
      public hydra.util.Maybe<java.util.List<hydra.core.FieldType>> visit(hydra.core.Type.Union rt) {
        return hydra.util.Maybe.just((rt).value);
      }
    }));
    return hydra.Resolution.requireRowType(
      cx,
      "union",
      toUnion,
      graph,
      name);
  }

  static hydra.util.Maybe<hydra.core.Type> resolveType(hydra.graph.Graph graph, hydra.core.Type typ) {
    return hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Maybe.just(typ);
      }

      @Override
      public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Type.Variable name) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.lib.maybes.Map.apply(
            (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type>) (ts -> hydra.Scoping.typeSchemeToFType(ts)),
            hydra.lib.maps.Lookup.apply(
              (name).value,
              (graph).boundTypes)),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Maybe<hydra.core.Type>>) (ts -> hydra.util.Maybe.just(hydra.Scoping.typeSchemeToFType(ts))),
          hydra.lib.maps.Lookup.apply(
            (name).value,
            (graph).schemaTypes));
      }
    });
  }

  static hydra.core.TypeScheme typeToTypeScheme(hydra.core.Type t0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>> helper = new java.util.concurrent.atomic.AtomicReference<>();
    helper.set((java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>) (vars -> (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (t -> hydra.Strip.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.TypeScheme otherwise(hydra.core.Type instance) {
        return new hydra.core.TypeScheme(hydra.lib.lists.Reverse.apply(vars), t, (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()));
      }

      @Override
      public hydra.core.TypeScheme visit(hydra.core.Type.Forall ft) {
        return helper.get().apply(hydra.lib.lists.Cons.apply(
          (ft).value.parameter,
          vars)).apply((ft).value.body);
      }
    }))));
    return helper.get().apply((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList())).apply(t0);
  }
}
