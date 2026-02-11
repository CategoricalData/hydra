// Note: this is an automatically generated file. Do not edit.

package hydra.annotations;

/**
 * Utilities for reading and writing type and term annotations
 */
public interface Annotations {
  static <T0, T1, T2, T3> java.util.Map<T2, T3> aggregateAnnotations(java.util.function.Function<T0, hydra.util.Maybe<T1>> getValue, java.util.function.Function<T1, T0> getX, java.util.function.Function<T1, java.util.Map<T2, T3>> getAnns, T0 t) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Concat.apply(hydra.annotations.Annotations.<T1, T2, T3, T0>aggregateAnnotations_toPairs(
      getAnns,
      getValue,
      getX,
      (java.util.List<java.util.List<hydra.util.Tuple.Tuple2<T2, T3>>>) (java.util.List.<java.util.List<hydra.util.Tuple.Tuple2<T2, T3>>>of()),
      t)));
  }
  
  static <T0, T1, T2, T3> java.util.List<java.util.List<hydra.util.Tuple.Tuple2<T1, T2>>> aggregateAnnotations_toPairs(java.util.function.Function<T0, java.util.Map<T1, T2>> getAnns, java.util.function.Function<T3, hydra.util.Maybe<T0>> getValue, java.util.function.Function<T0, T3> getX, java.util.List<java.util.List<hydra.util.Tuple.Tuple2<T1, T2>>> rest, T3 t) {
    return hydra.lib.maybes.Maybe.apply(
      rest,
      (java.util.function.Function<T0, java.util.List<java.util.List<hydra.util.Tuple.Tuple2<T1, T2>>>>) (yy -> hydra.annotations.Annotations.<T0, T1, T2, T3>aggregateAnnotations_toPairs(
        getAnns,
        getValue,
        getX,
        hydra.lib.lists.Cons.apply(
          hydra.lib.maps.ToList.apply((getAnns).apply(yy)),
          rest),
        (getX).apply(yy))),
      (getValue).apply(t));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, java.lang.Void> debugIf(T0 debugId, String message) {
    return hydra.lib.flows.Bind.apply(
      hydra.annotations.Annotations.<T1>getDebugId(),
      (java.util.function.Function<hydra.util.Maybe<String>, hydra.compute.Flow<T1, java.lang.Void>>) (v1 -> hydra.annotations.Annotations.<T1>debugIf_checkAndFail(
        message,
        v1)));
  }
  
  static <T0> hydra.compute.Flow<T0, java.lang.Void> debugIf_checkAndFail(String message, hydra.util.Maybe<String> desc) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        desc,
        hydra.util.Maybe.just("debugId")),
      () -> hydra.lib.flows.Fail.apply(message),
      () -> hydra.lib.flows.Pure.apply(null));
  }
  
  static <T0> hydra.compute.Flow<T0, java.lang.Void> failOnFlag(hydra.core.Name flag, String msg) {
    return hydra.lib.flows.Bind.apply(
      hydra.annotations.Annotations.<T0>hasFlag(flag),
      (java.util.function.Function<Boolean, hydra.compute.Flow<T0, java.lang.Void>>) (val -> hydra.lib.logic.IfElse.lazy(
        val,
        () -> hydra.lib.flows.Fail.apply(msg),
        () -> hydra.lib.flows.Pure.apply(null))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.util.Maybe<String>> getDebugId() {
    return hydra.lexical.Lexical.<hydra.util.Maybe<String>, T0>withEmptyGraph(hydra.lib.flows.Bind.apply(
      hydra.annotations.Annotations.getAttr(hydra.constants.Constants.key_debugId()),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>>) (desc -> hydra.lib.flows.MapMaybe.apply(
        hydra.extract.core.Core::string,
        desc))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>> getAttr(hydra.core.Name key) {
    return (hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>) ((hydra.compute.Flow<T0, hydra.util.Maybe<hydra.core.Term>>) (new hydra.compute.Flow((java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, hydra.util.Maybe<hydra.core.Term>>>>) (s0 -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, hydra.util.Maybe<hydra.core.Term>>>) (t0 -> (hydra.compute.FlowState<T0, hydra.util.Maybe<hydra.core.Term>>) ((hydra.compute.FlowState<T0, hydra.util.Maybe<hydra.core.Term>>) (new hydra.compute.FlowState<T0, hydra.util.Maybe<hydra.core.Term>>(hydra.util.Maybe.just(hydra.lib.maps.Lookup.apply(
      key,
      (t0).other)), s0, t0))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> getAttrWithDefault(hydra.core.Name key, hydra.core.Term def) {
    return hydra.lib.flows.Map.apply(
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.core.Term>) (mval -> hydra.lib.maybes.FromMaybe.apply(
        def,
        mval)),
      hydra.annotations.Annotations.<T0>getAttr(key));
  }
  
  static <T0> hydra.compute.Flow<T0, Integer> getCount(hydra.core.Name key) {
    return hydra.lexical.Lexical.<Integer, T0>withEmptyGraph(hydra.lib.flows.Bind.apply(
      hydra.annotations.Annotations.getAttrWithDefault(
        key,
        new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))),
      hydra.extract.core.Core::int32));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>> getDescription(java.util.Map<hydra.core.Name, hydra.core.Term> anns) {
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Pure.apply((hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>>>) (term -> hydra.lib.flows.Map.apply(
        (java.util.function.Function<String, hydra.util.Maybe<String>>) (hydra.lib.maybes.Pure::apply),
        hydra.extract.core.Core.string(term))),
      hydra.lib.maps.Lookup.apply(
        new hydra.core.Name("description"),
        anns));
  }
  
  static hydra.util.Maybe<hydra.core.Term> getTermAnnotation(hydra.core.Name key, hydra.core.Term term) {
    return hydra.lib.maps.Lookup.apply(
      key,
      hydra.annotations.Annotations.termAnnotationInternal(term));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>> getTermDescription(hydra.core.Term term) {
    return hydra.annotations.Annotations.getDescription(hydra.annotations.Annotations.termAnnotationInternal(term));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>> getType(java.util.Map<hydra.core.Name, hydra.core.Term> anns) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>>>) (cx -> hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
        (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Type>>>) (dat -> hydra.lib.flows.Map.apply(
          (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (hydra.lib.maybes.Pure::apply),
          hydra.monads.Monads.withTrace(
            "get type",
            hydra.monads.Monads.eitherToFlow(
              wrapped -> (wrapped).value,
              hydra.decode.core.Core.type(
                cx,
                dat))))),
        hydra.lib.maps.Lookup.apply(
          hydra.constants.Constants.key_type(),
          anns))));
  }
  
  static hydra.util.Maybe<hydra.core.Term> getTypeAnnotation(hydra.core.Name key, hydra.core.Type typ) {
    return hydra.lib.maps.Lookup.apply(
      key,
      hydra.annotations.Annotations.typeAnnotationInternal(typ));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>> getTypeClasses(hydra.core.Term term) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>>>) (cx -> {
        java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.classes.TypeClass>> decodeClass = (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.classes.TypeClass>>) (term2 -> {
          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.classes.TypeClass>> byName = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.classes.TypeClass>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.classes.TypeClass>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.classes.TypeClass>(new hydra.core.Name("equality"), new hydra.classes.TypeClass.Equality()))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.classes.TypeClass>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.classes.TypeClass>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.classes.TypeClass>(new hydra.core.Name("ordering"), new hydra.classes.TypeClass.Ordering()))))));
          return hydra.lib.flows.Bind.apply(
            hydra.extract.core.Core.unitVariant(
              new hydra.core.Name("hydra.classes.TypeClass"),
              term2),
            (java.util.function.Function<hydra.core.Name, hydra.compute.Flow<hydra.graph.Graph, hydra.classes.TypeClass>>) (fn -> hydra.lib.maybes.Maybe.apply(
              hydra.monads.Monads.unexpected(
                "type class",
                hydra.show.core.Core.term(term2)),
              (java.util.function.Function<hydra.classes.TypeClass, hydra.compute.Flow<hydra.graph.Graph, hydra.classes.TypeClass>>) ((java.util.function.Function<hydra.classes.TypeClass, hydra.compute.Flow<hydra.graph.Graph, hydra.classes.TypeClass>>) (hydra.lib.flows.Pure::apply)),
              hydra.lib.maps.Lookup.apply(
                fn,
                byName.get()))));
        });
        return hydra.lib.maybes.Maybe.apply(
          hydra.lib.flows.Pure.apply((java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>) ((java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>) (hydra.lib.maps.Empty.<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>apply()))),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>>>) (term2 -> hydra.extract.core.Core.map(
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Name>>) (t -> hydra.monads.Monads.eitherToFlow(
              wrapped -> (wrapped).value,
              hydra.decode.core.Core.name(
                cx,
                t))),
            (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, java.util.Set<hydra.classes.TypeClass>>>) (v1 -> hydra.extract.core.Core.setOf(
              decodeClass,
              v1)),
            term2)),
          hydra.annotations.Annotations.getTermAnnotation(
            hydra.constants.Constants.key_classes(),
            term));
      }));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<String>> getTypeDescription(hydra.core.Type typ) {
    return hydra.annotations.Annotations.getDescription(hydra.annotations.Annotations.typeAnnotationInternal(typ));
  }
  
  static Boolean isNativeType(hydra.core.Binding el) {
    hydra.util.Lazy<Boolean> isFlaggedAsFirstClassType = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.apply(
      false,
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.Term, Boolean>) (ignored -> true),
        hydra.annotations.Annotations.getTermAnnotation(
          hydra.constants.Constants.key_firstClassType(),
          (el).term))));
    return hydra.lib.maybes.Maybe.apply(
      false,
      (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> hydra.lib.logic.And.apply(
        hydra.lib.equality.Equal.apply(
          ts,
          new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))),
        hydra.lib.logic.Not.apply(isFlaggedAsFirstClassType.get()))),
      (el).type);
  }
  
  static <T0> Boolean hasDescription(java.util.Map<hydra.core.Name, T0> anns) {
    return hydra.lib.maybes.IsJust.apply(hydra.lib.maps.Lookup.apply(
      hydra.constants.Constants.key_description(),
      anns));
  }
  
  static <T0> hydra.compute.Flow<T0, Boolean> hasFlag(hydra.core.Name flag) {
    return hydra.lexical.Lexical.<Boolean, T0>withEmptyGraph(hydra.lib.flows.Bind.apply(
      hydra.annotations.Annotations.getAttrWithDefault(
        flag,
        new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false))),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, Boolean>>) (term -> hydra.extract.core.Core.boolean_(term))));
  }
  
  static Boolean hasTypeDescription(hydra.core.Type typ) {
    return hydra.annotations.Annotations.hasDescription(hydra.annotations.Annotations.typeAnnotationInternal(typ));
  }
  
  static <T0> hydra.compute.Flow<T0, Integer> nextCount(hydra.core.Name key) {
    return hydra.lib.flows.Bind.apply(
      hydra.annotations.Annotations.<T0>getCount(key),
      (java.util.function.Function<Integer, hydra.compute.Flow<T0, Integer>>) (count -> hydra.lib.flows.Map.apply(
        (java.util.function.Function<java.lang.Void, Integer>) (ignored -> count),
        hydra.annotations.Annotations.<T0>putCount(
          key,
          hydra.lib.math.Add.apply(
            count,
            1)))));
  }
  
  static hydra.core.Term normalizeTermAnnotations(hydra.core.Term term) {
    java.util.Map<hydra.core.Name, hydra.core.Term> anns = hydra.annotations.Annotations.termAnnotationInternal(term);
    hydra.core.Term stripped = hydra.rewriting.Rewriting.deannotateTerm(term);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply(anns),
      () -> stripped,
      () -> new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(stripped, anns)));
  }
  
  static hydra.core.Type normalizeTypeAnnotations(hydra.core.Type typ) {
    java.util.Map<hydra.core.Name, hydra.core.Term> anns = hydra.annotations.Annotations.typeAnnotationInternal(typ);
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType(typ);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply(anns),
      () -> stripped,
      () -> new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(stripped, anns)));
  }
  
  static <T0> hydra.compute.Flow<T0, java.lang.Void> putAttr(hydra.core.Name key, hydra.core.Term val) {
    return (hydra.compute.Flow<T0, java.lang.Void>) ((hydra.compute.Flow<T0, java.lang.Void>) (new hydra.compute.Flow((java.util.function.Function<T0, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, java.lang.Void>>>) (s0 -> (java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<T0, java.lang.Void>>) (t0 -> (hydra.compute.FlowState<T0, java.lang.Void>) ((hydra.compute.FlowState<T0, java.lang.Void>) (new hydra.compute.FlowState<T0, java.lang.Void>(hydra.util.Maybe.just(null), s0, new hydra.compute.Trace((t0).stack, (t0).messages, hydra.lib.maps.Insert.apply(
      key,
      val,
      (t0).other))))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, java.lang.Void> putCount(hydra.core.Name key, Integer count) {
    return hydra.annotations.Annotations.<T0>putAttr(
      key,
      new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(count))));
  }
  
  static <T0> hydra.compute.Flow<T0, java.lang.Void> resetCount(hydra.core.Name key) {
    return hydra.annotations.Annotations.<T0>putAttr(
      key,
      new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))));
  }
  
  static <T0, T1> java.util.Map<T0, T1> setAnnotation(T0 key, hydra.util.Maybe<T1> val, java.util.Map<T0, T1> m) {
    return hydra.lib.maps.Alter.apply(
      (java.util.function.Function<hydra.util.Maybe<T1>, hydra.util.Maybe<T1>>) (ignored -> val),
      key,
      m);
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Term> setDescription(hydra.util.Maybe<String> d, java.util.Map<hydra.core.Name, hydra.core.Term> v1) {
    return hydra.annotations.Annotations.setAnnotation(
      hydra.constants.Constants.key_description(),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (arg_ -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(arg_))),
        d),
      v1);
  }
  
  static hydra.core.Term setTermAnnotation(hydra.core.Name key, hydra.util.Maybe<hydra.core.Term> val, hydra.core.Term term) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Term>> anns = new hydra.util.Lazy<>(() -> hydra.annotations.Annotations.setAnnotation(
      key,
      val,
      hydra.annotations.Annotations.termAnnotationInternal(term)));
    hydra.core.Term term_ = hydra.rewriting.Rewriting.deannotateTerm(term);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply(anns.get()),
      () -> term_,
      () -> new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(term_, anns.get())));
  }
  
  static hydra.core.Term setTermDescription(hydra.util.Maybe<String> d, hydra.core.Term v1) {
    return hydra.annotations.Annotations.setTermAnnotation(
      hydra.constants.Constants.key_description(),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (s -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(s))),
        d),
      v1);
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Term> setType(hydra.util.Maybe<hydra.core.Type> mt, java.util.Map<hydra.core.Name, hydra.core.Term> v1) {
    return hydra.annotations.Annotations.setAnnotation(
      hydra.constants.Constants.key_type(),
      hydra.lib.maybes.Map.apply(
        hydra.encode.core.Core::type,
        mt),
      v1);
  }
  
  static hydra.core.Type setTypeAnnotation(hydra.core.Name key, hydra.util.Maybe<hydra.core.Term> val, hydra.core.Type typ) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Term>> anns = new hydra.util.Lazy<>(() -> hydra.annotations.Annotations.setAnnotation(
      key,
      val,
      hydra.annotations.Annotations.typeAnnotationInternal(typ)));
    hydra.core.Type typ_ = hydra.rewriting.Rewriting.deannotateType(typ);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply(anns.get()),
      () -> typ_,
      () -> new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(typ_, anns.get())));
  }
  
  static hydra.core.Term setTypeClasses(java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>> m, hydra.core.Term term) {
    java.util.function.Function<hydra.classes.TypeClass, hydra.core.Term> encodeClass = (java.util.function.Function<hydra.classes.TypeClass, hydra.core.Term>) (tc -> (tc).accept(new hydra.classes.TypeClass.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.classes.TypeClass.Equality ignored) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.classes.TypeClass"), new hydra.core.Field(new hydra.core.Name("equality"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.classes.TypeClass.Ordering ignored) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.classes.TypeClass"), new hydra.core.Field(new hydra.core.Name("ordering"), new hydra.core.Term.Unit())));
      }
    }));
    java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>> encodePair = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>) (nameClasses -> {
      hydra.util.Lazy<java.util.Set<hydra.classes.TypeClass>> classes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nameClasses));
      hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nameClasses));
      return (hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(hydra.encode.core.Core.name(name.get()), new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
        encodeClass,
        hydra.lib.sets.ToList.apply(classes.get())))))));
    });
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.Term>> encoded = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply(m),
      () -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()),
      () -> hydra.util.Maybe.just(new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        encodePair,
        hydra.lib.maps.ToList.apply(m)))))));
    return hydra.annotations.Annotations.setTermAnnotation(
      hydra.constants.Constants.key_classes(),
      encoded.get(),
      term);
  }
  
  static hydra.core.Type setTypeDescription(hydra.util.Maybe<String> d, hydra.core.Type v1) {
    return hydra.annotations.Annotations.setTypeAnnotation(
      hydra.constants.Constants.key_description(),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (arg_ -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(arg_))),
        d),
      v1);
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Term> termAnnotationInternal(hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.AnnotatedTerm>> getAnn = (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.AnnotatedTerm>>) (t -> (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.AnnotatedTerm> otherwise(hydra.core.Term instance) {
        return (hydra.util.Maybe<hydra.core.AnnotatedTerm>) (hydra.util.Maybe.<hydra.core.AnnotatedTerm>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.AnnotatedTerm> visit(hydra.core.Term.Annotated a) {
        return hydra.util.Maybe.just((a).value);
      }
    }));
    return hydra.annotations.Annotations.aggregateAnnotations(
      getAnn,
      (java.util.function.Function<hydra.core.AnnotatedTerm, hydra.core.Term>) (at -> (at).body),
      (java.util.function.Function<hydra.core.AnnotatedTerm, java.util.Map<hydra.core.Name, hydra.core.Term>>) (at -> (at).annotation),
      term);
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Term> typeAnnotationInternal(hydra.core.Type typ) {
    java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.AnnotatedType>> getAnn = (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.AnnotatedType>>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.AnnotatedType> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<hydra.core.AnnotatedType>) (hydra.util.Maybe.<hydra.core.AnnotatedType>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.AnnotatedType> visit(hydra.core.Type.Annotated a) {
        return hydra.util.Maybe.just((a).value);
      }
    }));
    return hydra.annotations.Annotations.aggregateAnnotations(
      getAnn,
      (java.util.function.Function<hydra.core.AnnotatedType, hydra.core.Type>) (at -> (at).body),
      (java.util.function.Function<hydra.core.AnnotatedType, java.util.Map<hydra.core.Name, hydra.core.Term>>) (at -> (at).annotation),
      typ);
  }
  
  static hydra.core.Binding typeElement(hydra.core.Name name, hydra.core.Type typ) {
    hydra.core.Term schemaTerm = new hydra.core.Term.Variable(new hydra.core.Name("hydra.core.Type"));
    hydra.util.Lazy<hydra.core.Term> dataTerm = new hydra.util.Lazy<>(() -> hydra.annotations.Annotations.normalizeTermAnnotations(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.encode.core.Core.type(typ), hydra.lib.maps.FromList.apply(java.util.List.of((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>(hydra.constants.Constants.key_type(), schemaTerm)))))))));
    return new hydra.core.Binding(name, dataTerm.get(), hydra.util.Maybe.just(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> whenFlag(hydra.core.Name flag, hydra.compute.Flow<T0, T1> fthen, hydra.compute.Flow<T0, T1> felse) {
    return hydra.lib.flows.Bind.apply(
      hydra.annotations.Annotations.<T0>hasFlag(flag),
      (java.util.function.Function<Boolean, hydra.compute.Flow<T0, T1>>) (b -> hydra.lib.logic.IfElse.lazy(
        b,
        () -> fthen,
        () -> felse)));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> withDepth(hydra.core.Name key, java.util.function.Function<Integer, hydra.compute.Flow<T0, T1>> f) {
    return hydra.lib.flows.Bind.apply(
      hydra.annotations.Annotations.<T0>getCount(key),
      (java.util.function.Function<Integer, hydra.compute.Flow<T0, T1>>) (count -> {
        Integer inc = hydra.lib.math.Add.apply(
          count,
          1);
        return hydra.lib.flows.Bind.apply(
          hydra.annotations.Annotations.<T0>putCount(
            key,
            inc),
          (java.util.function.Function<java.lang.Void, hydra.compute.Flow<T0, T1>>) (ignored -> hydra.lib.flows.Bind.apply(
            (f).apply(inc),
            (java.util.function.Function<T1, hydra.compute.Flow<T0, T1>>) (r -> hydra.lib.flows.Bind.apply(
              hydra.annotations.Annotations.<T0>putCount(
                key,
                count),
              (java.util.function.Function<java.lang.Void, hydra.compute.Flow<T0, T1>>) (_2 -> hydra.lib.flows.Pure.apply(r)))))));
      }));
  }
}
