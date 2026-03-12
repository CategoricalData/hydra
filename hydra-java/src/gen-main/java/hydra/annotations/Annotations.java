// Note: this is an automatically generated file. Do not edit.

package hydra.annotations;

/**
 * Utilities for reading and writing type and term annotations
 */
public interface Annotations {
  static <T0, T1, T2, T3> java.util.Map<T2, T3> aggregateAnnotations(java.util.function.Function<T0, hydra.util.Maybe<T1>> getValue, java.util.function.Function<T1, T0> getX, java.util.function.Function<T1, java.util.Map<T2, T3>> getAnns, T0 t) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Concat.apply(hydra.annotations.Annotations.<T0, T1, T2, T3>aggregateAnnotations_toPairs(
      getAnns,
      getValue,
      getX,
      (java.util.List<java.util.List<hydra.util.Pair<T2, T3>>>) (java.util.List.<java.util.List<hydra.util.Pair<T2, T3>>>of()),
      t)));
  }
  
  static <T0, T1, T2, T3> java.util.List<java.util.List<hydra.util.Pair<T2, T3>>> aggregateAnnotations_toPairs(java.util.function.Function<T1, java.util.Map<T2, T3>> getAnns, java.util.function.Function<T0, hydra.util.Maybe<T1>> getValue, java.util.function.Function<T1, T0> getX, java.util.List<java.util.List<hydra.util.Pair<T2, T3>>> rest, T0 t) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> rest,
      (java.util.function.Function<T1, java.util.List<java.util.List<hydra.util.Pair<T2, T3>>>>) (yy -> hydra.annotations.Annotations.<T0, T1, T2, T3>aggregateAnnotations_toPairs(
        getAnns,
        getValue,
        getX,
        hydra.lib.lists.Cons.apply(
          hydra.lib.maps.ToList.apply((getAnns).apply(yy)),
          rest),
        (getX).apply(yy))),
      (getValue).apply(t));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, java.lang.Void> debugIf(hydra.context.Context cx, String debugId, String message) {
    return hydra.lib.eithers.Bind.apply(
      hydra.annotations.Annotations.getDebugId(cx),
      (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, java.lang.Void>>) (mid -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          mid,
          hydra.util.Maybe.just(debugId)),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, java.lang.Void>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError(message), cx))),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, java.lang.Void>right(null))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, java.lang.Void> failOnFlag(hydra.context.Context cx, hydra.core.Name flag, String msg) {
    return hydra.lib.eithers.Bind.apply(
      hydra.annotations.Annotations.hasFlag(
        cx,
        flag),
      (java.util.function.Function<Boolean, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, java.lang.Void>>) (val -> hydra.lib.logic.IfElse.lazy(
        val,
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, java.lang.Void>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError(msg), cx))),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, java.lang.Void>right(null))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<String>> getDebugId(hydra.context.Context cx) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<String>>right((hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<String>>>) (term -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<String, hydra.util.Maybe<String>>) (hydra.lib.maybes.Pure::apply),
        hydra.extract.core.Core.string(
          cx,
          new hydra.graph.Graph((java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Map<hydra.core.Name, hydra.graph.Primitive>) ((java.util.Map<hydra.core.Name, hydra.graph.Primitive>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.graph.Primitive>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())),
          term))),
      hydra.annotations.Annotations.getAttr(
        hydra.constants.Constants.key_debugId(),
        cx));
  }
  
  static hydra.util.Maybe<hydra.core.Term> getAttr(hydra.core.Name key, hydra.context.Context cx) {
    return hydra.lib.maps.Lookup.apply(
      key,
      (cx).other);
  }
  
  static hydra.core.Term getAttrWithDefault(hydra.core.Name key, hydra.core.Term def, hydra.context.Context cx) {
    return hydra.lib.maybes.FromMaybe.applyLazy(
      () -> def,
      hydra.annotations.Annotations.getAttr(
        key,
        cx));
  }
  
  static Integer getCount(hydra.core.Name key, hydra.context.Context cx) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> 0,
      (java.util.function.Function<hydra.core.Term, Integer>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public Integer otherwise(hydra.core.Term instance) {
          return 0;
        }
        
        @Override
        public Integer visit(hydra.core.Term.Literal lit) {
          return ((lit).value).accept(new hydra.core.Literal.PartialVisitor<>() {
            @Override
            public Integer otherwise(hydra.core.Literal instance) {
              return 0;
            }
            
            @Override
            public Integer visit(hydra.core.Literal.Integer_ iv) {
              return ((iv).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                @Override
                public Integer otherwise(hydra.core.IntegerValue instance) {
                  return 0;
                }
                
                @Override
                public Integer visit(hydra.core.IntegerValue.Int32 i) {
                  return (i).value;
                }
              });
            }
          });
        }
      })),
      hydra.lib.maps.Lookup.apply(
        key,
        (cx).other));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<String>> getDescription(hydra.context.Context cx, hydra.graph.Graph graph, java.util.Map<hydra.core.Name, hydra.core.Term> anns) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<String>>right((hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<String>>>) (term -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<String, hydra.util.Maybe<String>>) (hydra.lib.maybes.Pure::apply),
        hydra.extract.core.Core.string(
          cx,
          graph,
          term))),
      hydra.lib.maps.Lookup.apply(
        new hydra.core.Name("description"),
        anns));
  }
  
  static hydra.util.Maybe<hydra.core.Term> getTermAnnotation(hydra.core.Name key, hydra.core.Term term) {
    return hydra.lib.maps.Lookup.apply(
      key,
      hydra.annotations.Annotations.termAnnotationInternal(term));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<String>> getTermDescription(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term term) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.core.Term>> peel = new java.util.concurrent.atomic.AtomicReference<>();
    peel.set((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return t;
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return (peel.get()).apply(((tl).value).body);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication ta) {
        return (peel.get()).apply(((ta).value).body);
      }
    })));
    return hydra.annotations.Annotations.getDescription(
      cx,
      graph,
      hydra.annotations.Annotations.termAnnotationInternal((peel.get()).apply(term)));
  }
  
  static hydra.util.Either<hydra.error.DecodingError, hydra.util.Maybe<hydra.core.Type>> getType(hydra.graph.Graph graph, java.util.Map<hydra.core.Name, hydra.core.Term> anns) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.error.DecodingError, hydra.util.Maybe<hydra.core.Type>>right((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.util.Maybe<hydra.core.Type>>>) (dat -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (hydra.lib.maybes.Pure::apply),
        hydra.decode.core.Core.type(
          graph,
          dat))),
      hydra.lib.maps.Lookup.apply(
        hydra.constants.Constants.key_type(),
        anns));
  }
  
  static hydra.util.Maybe<hydra.core.Term> getTypeAnnotation(hydra.core.Name key, hydra.core.Type typ) {
    return hydra.lib.maps.Lookup.apply(
      key,
      hydra.annotations.Annotations.typeAnnotationInternal(typ));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>> getTypeClasses(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.classes.TypeClass>> decodeClass = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.classes.TypeClass>>) (term2 -> {
      hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.classes.TypeClass>> byName = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
        (hydra.util.Pair<hydra.core.Name, hydra.classes.TypeClass>) ((hydra.util.Pair<hydra.core.Name, hydra.classes.TypeClass>) (new hydra.util.Pair<hydra.core.Name, hydra.classes.TypeClass>(new hydra.core.Name("equality"), new hydra.classes.TypeClass.Equality()))),
        (hydra.util.Pair<hydra.core.Name, hydra.classes.TypeClass>) ((hydra.util.Pair<hydra.core.Name, hydra.classes.TypeClass>) (new hydra.util.Pair<hydra.core.Name, hydra.classes.TypeClass>(new hydra.core.Name("ordering"), new hydra.classes.TypeClass.Ordering()))))));
      return hydra.lib.eithers.Bind.apply(
        hydra.extract.core.Core.unitVariant(
          cx,
          new hydra.core.Name("hydra.classes.TypeClass"),
          graph,
          term2),
        (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.classes.TypeClass>>) (fn -> hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.classes.TypeClass>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            "unexpected: expected type class, got ",
            hydra.show.core.Core.term(term2))), cx))),
          (java.util.function.Function<hydra.classes.TypeClass, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.classes.TypeClass>>) (x -> hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.classes.TypeClass>right(x)),
          hydra.lib.maps.Lookup.apply(
            fn,
            byName.get()))));
    });
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>>right((java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>) ((java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>) (hydra.lib.maps.Empty.<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>apply()))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, java.util.Map<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>>>) (term2 -> hydra.extract.core.Core.map(
        cx,
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Name>>) (t -> hydra.lib.eithers.Bimap.apply(
          (java.util.function.Function<hydra.error.DecodingError, hydra.context.InContext<hydra.error.OtherError>>) (de -> (hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError((de).value), cx))),
          (java.util.function.Function<hydra.core.Name, hydra.core.Name>) (x -> x),
          hydra.decode.core.Core.name(
            graph,
            t))),
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, java.util.Set<hydra.classes.TypeClass>>>) (v1 -> hydra.extract.core.Core.setOf(
          cx,
          decodeClass,
          graph,
          v1)),
        graph,
        term2)),
      hydra.annotations.Annotations.getTermAnnotation(
        hydra.constants.Constants.key_classes(),
        term));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<String>> getTypeDescription(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Type typ) {
    return hydra.annotations.Annotations.getDescription(
      cx,
      graph,
      hydra.annotations.Annotations.typeAnnotationInternal(typ));
  }
  
  static Boolean isNativeType(hydra.core.Binding el) {
    hydra.util.Lazy<Boolean> isFlaggedAsFirstClassType = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
      () -> false,
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.Term, Boolean>) (ignored -> true),
        hydra.annotations.Annotations.getTermAnnotation(
          hydra.constants.Constants.key_firstClassType(),
          (el).term))));
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> false,
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
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, Boolean> hasFlag(hydra.context.Context cx, hydra.core.Name flag) {
    hydra.core.Term term = hydra.annotations.Annotations.getAttrWithDefault(
      flag,
      new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)),
      cx);
    return hydra.extract.core.Core.boolean_(
      cx,
      new hydra.graph.Graph((java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Map<hydra.core.Name, hydra.graph.Primitive>) ((java.util.Map<hydra.core.Name, hydra.graph.Primitive>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.graph.Primitive>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())),
      term);
  }
  
  static Boolean hasTypeDescription(hydra.core.Type typ) {
    return hydra.annotations.Annotations.hasDescription(hydra.annotations.Annotations.typeAnnotationInternal(typ));
  }
  
  static hydra.util.Pair<Integer, hydra.context.Context> nextCount(hydra.core.Name key, hydra.context.Context cx) {
    Integer count = hydra.annotations.Annotations.getCount(
      key,
      cx);
    return (hydra.util.Pair<Integer, hydra.context.Context>) ((hydra.util.Pair<Integer, hydra.context.Context>) (new hydra.util.Pair<Integer, hydra.context.Context>(count, hydra.annotations.Annotations.putCount(
      key,
      hydra.lib.math.Add.apply(
        count,
        1),
      cx))));
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
  
  static hydra.context.Context putAttr(hydra.core.Name key, hydra.core.Term val, hydra.context.Context cx) {
    return new hydra.context.Context((cx).trace, (cx).messages, hydra.lib.maps.Insert.apply(
      key,
      val,
      (cx).other));
  }
  
  static hydra.context.Context putCount(hydra.core.Name key, Integer count, hydra.context.Context cx) {
    return hydra.annotations.Annotations.putAttr(
      key,
      new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(count))),
      cx);
  }
  
  static hydra.context.Context resetCount(hydra.core.Name key, hydra.context.Context cx) {
    return hydra.annotations.Annotations.putAttr(
      key,
      new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))),
      cx);
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
    java.util.function.Function<hydra.util.Pair<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>> encodePair = (java.util.function.Function<hydra.util.Pair<hydra.core.Name, java.util.Set<hydra.classes.TypeClass>>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (nameClasses -> {
      hydra.util.Lazy<java.util.Set<hydra.classes.TypeClass>> classes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nameClasses));
      hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nameClasses));
      return (hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(hydra.encode.core.Core.name(name.get()), new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
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
    hydra.util.Lazy<hydra.core.Term> dataTerm = new hydra.util.Lazy<>(() -> hydra.annotations.Annotations.normalizeTermAnnotations(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.encode.core.Core.type(typ), hydra.lib.maps.FromList.apply(java.util.List.of((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>(hydra.constants.Constants.key_type(), schemaTerm)))))))));
    return new hydra.core.Binding(name, dataTerm.get(), hydra.util.Maybe.just(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T0> whenFlag(hydra.context.Context cx, hydra.core.Name flag, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T0> ethen, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T0> eelse) {
    return hydra.lib.eithers.Bind.apply(
      hydra.annotations.Annotations.hasFlag(
        cx,
        flag),
      (java.util.function.Function<Boolean, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T0>>) (b -> hydra.lib.logic.IfElse.lazy(
        b,
        () -> ethen,
        () -> eelse)));
  }
}
