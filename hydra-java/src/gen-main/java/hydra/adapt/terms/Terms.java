// Note: this is an automatically generated file. Do not edit.

package hydra.adapt.terms;

/**
 * Adapter framework for types and terms
 */
public interface Terms {
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>> fieldAdapter(hydra.coders.AdapterContext cx, hydra.core.FieldType ftyp) {
    return hydra.lib.eithers.Bind.apply(
      hydra.adapt.terms.Terms.termAdapter(
        cx,
        (ftyp).type),
      (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>) (ad -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>right((hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>) ((hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>) ((hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>) ((hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>) (new hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(ad), ftyp, new hydra.core.FieldType((ftyp).name, ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(ad)), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>>>>) (v1 -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>>>) (v2 -> (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>>) (v3 -> hydra.adapt.terms.Terms.fieldAdapter_encdec(
        ad,
        v1,
        v2,
        v3)))))))))))))));
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field> fieldAdapter_encdec(hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term> ad, hydra.coders.CoderDirection dir, hydra.context.Context cx, hydra.core.Field field) {
    hydra.core.Name name = (field).name;
    hydra.core.Term term = (field).term;
    return hydra.lib.eithers.Bind.apply(
      hydra.adapt.utils.Utils.encodeDecode(
        dir,
        ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(ad),
        cx,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>>) (newTerm -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>right(new hydra.core.Field(name, newTerm))))));
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> forTypeReference(hydra.coders.AdapterContext cx, hydra.core.Name name) {
    java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> adapters = (cx).adapters;
    java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<Boolean, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>, java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>> forMissingAdapter = (java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<Boolean, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>, java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>>) (cx2 -> (java.util.function.Function<Boolean, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>, java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (lossy -> (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>, java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (adapters0 -> (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (placeholder -> {
      hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> newAdapters = new hydra.util.Lazy<>(() -> hydra.lib.maps.Insert.apply(
        name,
        placeholder,
        adapters0));
      hydra.coders.AdapterContext newCx = new hydra.coders.AdapterContext((cx2).graph, (cx2).language, newAdapters.get());
      hydra.util.Maybe<hydra.core.Type> mt = hydra.schemas.Schemas.resolveType(
        (newCx).graph,
        new hydra.core.Type.Variable(name));
      return hydra.lib.maybes.Maybe.apply(
        (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(lossy, new hydra.core.Type.Variable(name), new hydra.core.Type.Variable(name), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (dir -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (_cx -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (term -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(term))))))))))))))),
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (v1 -> hydra.adapt.terms.Terms.forTypeReference_forType(
          (java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.termAdapter(
            p0,
            p1)),
          cx2,
          adapters0,
          v1)),
        mt);
    }))));
    Boolean lossy = false;
    hydra.util.Lazy<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> placeholder = new hydra.util.Lazy<>(() -> (hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(lossy, new hydra.core.Type.Variable(name), new hydra.core.Type.Variable(name), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v3 -> hydra.adapt.terms.Terms.forTypeReference_encdec(
      name,
      adapters,
      v1,
      v2,
      v3)))))))))));
    return hydra.lib.maybes.Maybe.apply(
      ((((forMissingAdapter).apply(cx)).apply(lossy)).apply(adapters)).apply(placeholder.get()),
      (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (x -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right(x)))),
      hydra.lib.maps.Lookup.apply(
        name,
        adapters));
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T2> forTypeReference_encdec(hydra.core.Name name, java.util.Map<hydra.core.Name, hydra.compute.Adapter<T0, T1, T2, T2>> adapters0, hydra.coders.CoderDirection dir, hydra.context.Context cx, T2 term) {
    return hydra.lib.maybes.Maybe.apply(
      (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T2>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T2>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, T2>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
        "no adapter for reference type ",
        (name).value)), cx))))),
      (java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T2>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T2>>) (ad -> hydra.adapt.utils.Utils.<T2>encodeDecode(
        dir,
        ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T2>, hydra.compute.Coder<T2, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T2>, hydra.compute.Coder<T2, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T2>, hydra.compute.Coder<T2, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T2>, hydra.compute.Coder<T2, T2>>) (projected -> projected.coder))))).apply(ad),
        cx,
        term)),
      hydra.lib.maps.Lookup.apply(
        name,
        adapters0));
  }
  
  static <T0> hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> forTypeReference_forType(java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>> hydra_adapt_terms_termAdapter2, hydra.coders.AdapterContext cx2, T0 adapters0, hydra.core.Type t) {
    return hydra.lib.eithers.Bind.apply(
      ((hydra_adapt_terms_termAdapter2).apply(cx2)).apply(t),
      (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (actual -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right(actual)))));
  }
  
  static hydra.core.Name functionProxyName() {
    return new hydra.core.Name("hydra.core.FunctionProxy");
  }
  
  static <T0> hydra.core.Type functionProxyType(T0 ignored) {
    return new hydra.core.Type.Union(new hydra.core.RowType(hydra.adapt.terms.Terms.functionProxyName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("wrap"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("record"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("union"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("lambda"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("primitive"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("variable"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))));
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> functionToUnion(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>> encTerm = (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (term -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (strippedTerm -> (strippedTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.Term.Function f) {
        return ((f).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.core.Term visit(hydra.core.Function.Elimination e) {
            return ((e).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
              @Override
              public hydra.core.Term visit(hydra.core.Elimination.Wrap name) {
                return new hydra.core.Term.Union(new hydra.core.Injection(hydra.adapt.terms.Terms.functionProxyName(), new hydra.core.Field(new hydra.core.Name("wrap"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((name).value).value)))));
              }
              
              @Override
              public hydra.core.Term visit(hydra.core.Elimination.Record r) {
                return new hydra.core.Term.Union(new hydra.core.Injection(hydra.adapt.terms.Terms.functionProxyName(), new hydra.core.Field(new hydra.core.Name("record"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.show.core.Core.term(term))))));
              }
              
              @Override
              public hydra.core.Term visit(hydra.core.Elimination.Union u) {
                return new hydra.core.Term.Union(new hydra.core.Injection(hydra.adapt.terms.Terms.functionProxyName(), new hydra.core.Field(new hydra.core.Name("union"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.show.core.Core.term(term))))));
              }
            });
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Function.Lambda l) {
            return new hydra.core.Term.Union(new hydra.core.Injection(hydra.adapt.terms.Terms.functionProxyName(), new hydra.core.Field(new hydra.core.Name("lambda"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.show.core.Core.term(term))))));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.Function.Primitive name) {
            return new hydra.core.Term.Union(new hydra.core.Injection(hydra.adapt.terms.Terms.functionProxyName(), new hydra.core.Field(new hydra.core.Name("primitive"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((name).value).value)))));
          }
        });
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable name) {
        return new hydra.core.Term.Union(new hydra.core.Injection(hydra.adapt.terms.Terms.functionProxyName(), new hydra.core.Field(new hydra.core.Name("variable"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((name).value).value)))));
      }
    })));
    java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>> readFromString = (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (cx2 -> (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (graph -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (term -> hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.string(
        cx2,
        graph,
        term),
      (java.util.function.Function<String, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (s -> hydra.lib.maybes.Maybe.apply(
        (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          "failed to parse term: ",
          s)), cx2))))),
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (x -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(x)))),
        hydra.show.core.Core.readTerm(s)))))));
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Function ft) {
        hydra.core.Type cod = ((ft).value).codomain;
        hydra.core.Type dom = ((ft).value).domain;
        hydra.util.Lazy<hydra.util.Either<String, hydra.core.Type>> unionType = new hydra.util.Lazy<>(() -> hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            dom),
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.core.Type>>) (domAd -> (hydra.util.Either<String, hydra.core.Type>) ((hydra.util.Either<String, hydra.core.Type>) (hydra.util.Either.<String, hydra.core.Type>right(new hydra.core.Type.Union(new hydra.core.RowType(hydra.adapt.terms.Terms.functionProxyName(), java.util.List.of(
            new hydra.core.FieldType(new hydra.core.Name("wrap"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            new hydra.core.FieldType(new hydra.core.Name("record"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            new hydra.core.FieldType(new hydra.core.Name("union"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            new hydra.core.FieldType(new hydra.core.Name("lambda"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            new hydra.core.FieldType(new hydra.core.Name("primitive"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            new hydra.core.FieldType(new hydra.core.Name("variable"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))))))));
        return hydra.lib.eithers.Bind.apply(
          unionType.get(),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ut -> hydra.lib.eithers.Bind.apply(
            hydra.adapt.terms.Terms.termAdapter(
              cx,
              ut),
            (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ad -> {
              hydra.graph.Graph graph = (cx).graph;
              return (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(ad), t, ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(ad), (hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) ((hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) (new hydra.compute.Coder<hydra.core.Term, hydra.core.Term>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v2 -> hydra.adapt.terms.Terms.functionToUnion_encode(
                encTerm,
                hydra.rewriting.Rewriting::deannotateTerm,
                ad,
                v1,
                v2))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v2 -> hydra.adapt.terms.Terms.functionToUnion_decode(
                hydra.adapt.terms.Terms.functionProxyName(),
                (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>>>>>) (p0 -> p1 -> p2 -> p3 -> hydra.extract.core.Core.injection(
                  p0,
                  p1,
                  p2,
                  p3)),
                (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, String>>>>) (p0 -> p1 -> p2 -> hydra.extract.core.Core.string(
                  p0,
                  p1,
                  p2)),
                readFromString,
                graph,
                ad,
                v1,
                v2))))))))))))));
            }))));
      }
    });
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T2> functionToUnion_encode(java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>> encTerm, java.util.function.Function<hydra.core.Term, hydra.core.Term> hydra_rewriting_deannotateTerm2, hydra.compute.Adapter<T0, T1, hydra.core.Term, T2> ad, hydra.context.Context cx, hydra.core.Term term) {
    hydra.core.Term strippedTerm = (hydra_rewriting_deannotateTerm2).apply(term);
    return ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T2>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T2>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) (projected -> projected.coder))))).apply(ad))).apply(cx)).apply(((encTerm).apply(term)).apply(strippedTerm));
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> functionToUnion_decode(hydra.core.Name hydra_adapt_terms_functionProxyName2, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>>>>> hydra_extract_core_injection2, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, String>>>> hydra_extract_core_string2, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>> readFromString, hydra.graph.Graph graph, hydra.compute.Adapter<T0, T1, hydra.core.Term, T2> ad, hydra.context.Context cx, T2 term) {
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>> forCases = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (fterm -> (((readFromString).apply(cx)).apply(graph)).apply(fterm));
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>> forLambda = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (fterm -> (((readFromString).apply(cx)).apply(graph)).apply(fterm));
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>> forPrimitive = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (fterm -> hydra.lib.eithers.Bind.apply(
      (((hydra_extract_core_string2).apply(cx)).apply(graph)).apply(fterm),
      (java.util.function.Function<String, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (s -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name(s)))))))));
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>> forProjection = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (fterm -> (((readFromString).apply(cx)).apply(graph)).apply(fterm));
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>> forVariable = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (fterm -> hydra.lib.eithers.Bind.apply(
      (((hydra_extract_core_string2).apply(cx)).apply(graph)).apply(fterm),
      (java.util.function.Function<String, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (s -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name(s))))))));
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>> forWrapped = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (fterm -> hydra.lib.eithers.Bind.apply(
      (((hydra_extract_core_string2).apply(cx)).apply(graph)).apply(fterm),
      (java.util.function.Function<String, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (s -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name(s))))))))));
    return hydra.lib.eithers.Bind.apply(
      ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T2, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T2, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (projected -> projected.decode))).apply(((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) (projected -> projected.coder))))).apply(ad))).apply(cx)).apply(term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (injTerm -> hydra.lib.eithers.Bind.apply(
        ((((hydra_extract_core_injection2).apply(cx)).apply(hydra_adapt_terms_functionProxyName2)).apply(graph)).apply(injTerm),
        (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (field -> {
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          return hydra.lib.maybes.FromMaybe.apply(
            hydra.adapt.terms.Terms.functionToUnion_notFound(
              cx,
              fname),
            hydra.lib.maps.Lookup.apply(
              fname,
              hydra.lib.maps.FromList.apply(java.util.List.of(
                (hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>(new hydra.core.Name("wrap"), (forWrapped).apply(fterm)))),
                (hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>(new hydra.core.Name("record"), (forProjection).apply(fterm)))),
                (hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>(new hydra.core.Name("union"), (forCases).apply(fterm)))),
                (hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>(new hydra.core.Name("lambda"), (forLambda).apply(fterm)))),
                (hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>(new hydra.core.Name("primitive"), (forPrimitive).apply(fterm)))),
                (hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>(new hydra.core.Name("variable"), (forVariable).apply(fterm))))))));
        }))));
  }
  
  static <T3> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T3> functionToUnion_notFound(hydra.context.Context cx, hydra.core.Name fname) {
    return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T3>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T3>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, T3>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
      "unexpected field: ",
      (fname).value)), cx)))));
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> lambdaToMonotype(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Forall ft) {
        hydra.core.Type body = ((ft).value).body;
        return hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            body),
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ad -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(ad), t, ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(ad), ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(ad)))))))))));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> maybeToList(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Maybe ot) {
        return hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            (ot).value),
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ad -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(false, t, new hydra.core.Type.List(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(ad)), (hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) ((hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) (new hydra.compute.Coder<hydra.core.Term, hydra.core.Term>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v2 -> hydra.adapt.terms.Terms.maybeToList_encode(
            ad,
            v1,
            v2))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v2 -> hydra.adapt.terms.Terms.maybeToList_decode(
            ad,
            v1,
            v2))))))))))))))));
      }
    });
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> maybeToList_encode(hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term> ad, hydra.context.Context cx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Maybe.apply(
          (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (r -> hydra.lib.eithers.Bind.apply(
            hydra.adapt.utils.Utils.encodeDecode(
              new hydra.coders.CoderDirection.Encode(),
              ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(ad),
              cx,
              r),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (encoded -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.List(java.util.List.of(encoded)))))))),
          (m).value);
      }
    });
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> maybeToList_decode(hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term> ad, hydra.context.Context cx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> visit(hydra.core.Term.List l) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.core.Term>) (x -> new hydra.core.Term.Maybe(x)),
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply((l).value),
            () -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<hydra.core.Term>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<hydra.core.Term>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<hydra.core.Term>>right((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))),
            () -> hydra.lib.eithers.Bind.apply(
              hydra.adapt.utils.Utils.encodeDecode(
                new hydra.coders.CoderDirection.Decode(),
                ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(ad),
                cx,
                hydra.lib.lists.Head.apply((l).value)),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<hydra.core.Term>>>) (decoded -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<hydra.core.Term>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<hydra.core.Term>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Maybe<hydra.core.Term>>right(hydra.util.Maybe.just(decoded))))))));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> passApplication(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    java.util.function.Function<hydra.core.ApplicationType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> forApplicationType = (java.util.function.Function<hydra.core.ApplicationType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (at -> {
      hydra.core.Type lhs = (at).function;
      hydra.core.Type rhs = (at).argument;
      return hydra.lib.eithers.Bind.apply(
        hydra.adapt.terms.Terms.termAdapter(
          cx,
          lhs),
        (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (lhsAd -> hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            rhs),
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (rhsAd -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(hydra.lib.logic.Or.apply(
            ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(lhsAd),
            ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(rhsAd)), t, new hydra.core.Type.Application(new hydra.core.ApplicationType(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(lhsAd), ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(rhsAd))), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (dir -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (term -> hydra.adapt.utils.Utils.encodeDecode(
            dir,
            ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(lhsAd),
            cx2,
            term)))))))))))))))));
    });
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Application at) {
        return (forApplicationType).apply((at).value);
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> passEither(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    java.util.function.Function<hydra.core.EitherType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> forEitherType = (java.util.function.Function<hydra.core.EitherType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (et -> {
      hydra.core.Type left = (et).left;
      hydra.core.Type right = (et).right;
      return hydra.lib.eithers.Bind.apply(
        hydra.adapt.terms.Terms.termAdapter(
          cx,
          left),
        (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (leftAd -> hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            right),
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (rightAd -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(hydra.lib.logic.Or.apply(
            ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(leftAd),
            ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(rightAd)), t, new hydra.core.Type.Either(new hydra.core.EitherType(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(leftAd), ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(rightAd))), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (dir -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (term -> hydra.adapt.utils.Utils.encodeDecode(
            dir,
            ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(leftAd),
            cx2,
            term)))))))))))))))));
    });
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Either et) {
        return (forEitherType).apply((et).value);
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> passFunction(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>>> toCaseAds = (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>>>) (dom -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>>) (cod -> (hydra.rewriting.Rewriting.deannotateType(dom)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>> otherwise(hydra.core.Type instance) {
        return (hydra.util.Either<String, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>) ((hydra.util.Either<String, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>) (hydra.util.Either.<String, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>right((java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>) ((java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>apply())))));
      }
      
      @Override
      public hydra.util.Either<String, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>> visit(hydra.core.Type.Union rt) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<String, hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>>) (f -> hydra.lib.eithers.Bind.apply(
              hydra.adapt.terms.Terms.fieldAdapter(
                cx,
                new hydra.core.FieldType((f).name, new hydra.core.Type.Function(new hydra.core.FunctionType((f).type, cod)))),
              (java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, hydra.util.Either<String, hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>>) (ad -> (hydra.util.Either<String, hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>) ((hydra.util.Either<String, hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>) (hydra.util.Either.<String, hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>right((hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>) ((hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>) (new hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>((f).name, ad))))))))),
            ((rt).value).fields),
          (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>, hydra.util.Either<String, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>>) (pairs -> (hydra.util.Either<String, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>) ((hydra.util.Either<String, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>) (hydra.util.Either.<String, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>right(hydra.lib.maps.FromList.apply(pairs))))));
      }
    })));
    java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.util.Maybe<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> toOptionAd = (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.util.Maybe<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (dom -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.util.Maybe<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (cod -> (hydra.rewriting.Rewriting.deannotateType(dom)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.util.Maybe<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> otherwise(hydra.core.Type instance) {
        return (hydra.util.Either<String, hydra.util.Maybe<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) ((hydra.util.Either<String, hydra.util.Maybe<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (hydra.util.Either.<String, hydra.util.Maybe<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>right((hydra.util.Maybe<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Maybe.<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>nothing()))));
      }
      
      @Override
      public hydra.util.Either<String, hydra.util.Maybe<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> visit(hydra.core.Type.Maybe ot) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Maybe<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (hydra.lib.maybes.Pure::apply),
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            new hydra.core.Type.Function(new hydra.core.FunctionType((ot).value, cod))));
      }
    })));
    java.util.function.Function<hydra.core.FunctionType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> forFunctionType = (java.util.function.Function<hydra.core.FunctionType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ft -> {
      hydra.core.Type cod = (ft).codomain;
      hydra.core.Type dom = (ft).domain;
      return hydra.lib.eithers.Bind.apply(
        hydra.adapt.terms.Terms.termAdapter(
          cx,
          dom),
        (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (domAd -> hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            cod),
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (codAd -> hydra.lib.eithers.Bind.apply(
            ((toCaseAds).apply(dom)).apply(cod),
            (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (caseAds -> hydra.lib.eithers.Bind.apply(
              ((toOptionAd).apply(dom)).apply(cod),
              (java.util.function.Function<hydra.util.Maybe<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (optionAd -> {
                hydra.util.Lazy<Boolean> lossy = new hydra.util.Lazy<>(() -> hydra.lib.logic.Or.apply(
                  ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(codAd),
                  hydra.lib.lists.Foldl.apply(
                    (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.Or.apply(
                      p0,
                      p1)),
                    false,
                    hydra.lib.lists.Map.apply(
                      (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>, Boolean>) (pair -> ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, Boolean>) (projected -> projected.isLossy))))).apply(hydra.lib.pairs.Second.apply(pair))),
                      hydra.lib.maps.ToList.apply(caseAds)))));
                hydra.util.Lazy<hydra.core.Type> target = new hydra.util.Lazy<>(() -> new hydra.core.Type.Function(new hydra.core.FunctionType(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(domAd), ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(codAd))));
                return (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(lossy.get(), t, target.get(), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v3 -> hydra.adapt.terms.Terms.passFunction_encdec(
                  hydra.rewriting.Rewriting::deannotateTerm,
                  codAd,
                  caseAds,
                  v1,
                  v2,
                  v3)))))))))))));
              }))))))));
    });
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Function ft) {
        return (forFunctionType).apply((ft).value);
      }
    });
  }
  
  static <T0, T1, T2, T3> hydra.compute.Coder<T3, T3> passFunction_getCoder(java.util.Map<T0, hydra.compute.Adapter<T1, T2, T3, T3>> caseAds, T0 fname) {
    return hydra.lib.maybes.Maybe.apply(
      hydra.adapt.utils.Utils.<T3>idCoder(),
      (java.util.function.Function<hydra.compute.Adapter<T1, T2, T3, T3>, hydra.compute.Coder<T3, T3>>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, T3, T3>, hydra.compute.Coder<T3, T3>>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, T3, T3>, hydra.compute.Coder<T3, T3>>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, T3, T3>, hydra.compute.Coder<T3, T3>>) (projected -> projected.coder)))),
      hydra.lib.maps.Lookup.apply(
        fname,
        caseAds));
  }
  
  static <T0, T1, T2, T3> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Elimination> passFunction_forElimination(hydra.coders.CoderDirection dir, hydra.context.Context cx, hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term> codAd, java.util.Map<hydra.core.Name, hydra.compute.Adapter<T2, T3, hydra.core.Field, hydra.core.Field>> caseAds, hydra.core.Elimination e) {
    return (e).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Elimination> visit(hydra.core.Elimination.Union cs) {
        java.util.List<hydra.core.Field> cases = ((cs).value).cases;
        hydra.util.Maybe<hydra.core.Term> def = ((cs).value).default_;
        hydra.core.Name n = ((cs).value).typeName;
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>>) (f -> hydra.adapt.utils.Utils.encodeDecode(
              dir,
              hydra.adapt.terms.Terms.passFunction_getCoder(
                caseAds,
                (f).name),
              cx,
              f)),
            cases),
          (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Elimination>>) (rcases -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapMaybe.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (d -> hydra.adapt.utils.Utils.encodeDecode(
                dir,
                ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(codAd),
                cx,
                d)),
              def),
            (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Elimination>>) (rdef -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Elimination>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Elimination>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Elimination>right(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(n, rdef, rcases)))))))));
      }
    });
  }
  
  static <T0, T1, T2, T3> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Function> passFunction_forFunction(hydra.coders.CoderDirection dir, hydra.context.Context cx, hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term> codAd, java.util.Map<hydra.core.Name, hydra.compute.Adapter<T2, T3, hydra.core.Field, hydra.core.Field>> caseAds, hydra.core.Function f) {
    return (f).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Function> visit(hydra.core.Function.Elimination e) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.core.Elimination, hydra.core.Function>) (x -> new hydra.core.Function.Elimination(x)),
          hydra.adapt.terms.Terms.<T0, T1, T2, T3>passFunction_forElimination(
            dir,
            cx,
            codAd,
            caseAds,
            (e).value));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Function> visit(hydra.core.Function.Lambda l) {
        hydra.core.Term body = ((l).value).body;
        hydra.util.Maybe<hydra.core.Type> d = ((l).value).domain;
        hydra.core.Name var = ((l).value).parameter;
        return hydra.lib.eithers.Bind.apply(
          hydra.adapt.utils.Utils.encodeDecode(
            dir,
            ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(codAd),
            cx,
            body),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Function>>) (newBody -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Function>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Function>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Function>right(new hydra.core.Function.Lambda(new hydra.core.Lambda(var, d, newBody)))))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Function> visit(hydra.core.Function.Primitive name) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Function>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Function>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Function>right(new hydra.core.Function.Primitive((name).value))));
      }
    });
  }
  
  static <T0, T1, T2, T3> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> passFunction_encdec(java.util.function.Function<hydra.core.Term, hydra.core.Term> hydra_rewriting_deannotateTerm2, hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term> codAd, java.util.Map<hydra.core.Name, hydra.compute.Adapter<T2, T3, hydra.core.Field, hydra.core.Field>> caseAds, hydra.coders.CoderDirection dir, hydra.context.Context cx, hydra.core.Term term) {
    return ((hydra_rewriting_deannotateTerm2).apply(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(term)));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> visit(hydra.core.Term.Function f) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.core.Function, hydra.core.Term>) (x -> new hydra.core.Term.Function(x)),
          hydra.adapt.terms.Terms.<T0, T1, T2, T3>passFunction_forFunction(
            dir,
            cx,
            codAd,
            caseAds,
            (f).value));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> passForall(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    java.util.function.Function<hydra.core.ForallType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> forForallType = (java.util.function.Function<hydra.core.ForallType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ft -> {
      hydra.core.Type body = (ft).body;
      hydra.core.Name v = (ft).parameter;
      return hydra.lib.eithers.Bind.apply(
        hydra.adapt.terms.Terms.termAdapter(
          cx,
          body),
        (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ad -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(ad), t, new hydra.core.Type.Forall(new hydra.core.ForallType(v, ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(ad))), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (dir -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (term -> hydra.adapt.utils.Utils.encodeDecode(
          dir,
          ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(ad),
          cx2,
          term)))))))))))))));
    });
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Forall ft) {
        return (forForallType).apply((ft).value);
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> passLiteral(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    java.util.function.Function<hydra.core.LiteralType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> forLiteral = (java.util.function.Function<hydra.core.LiteralType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (lt -> hydra.lib.eithers.Bind.apply(
      hydra.adapt.literals.Literals.literalAdapter(
        cx,
        lt),
      (java.util.function.Function<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ad -> {
        hydra.util.Lazy<hydra.compute.Coder<hydra.core.Term, hydra.core.Term>> step = new hydra.util.Lazy<>(() -> hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v3 -> hydra.adapt.terms.Terms.passLiteral_encdec(
          (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Literal>>>>) (p0 -> p1 -> p2 -> hydra.extract.core.Core.literal(
            p0,
            p1,
            p2)),
          (cx).graph,
          ad,
          v1,
          v2,
          v3))))));
        return (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>, Boolean>) (projected -> projected.isLossy))))).apply(ad), new hydra.core.Type.Literal(((java.util.function.Function<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>, hydra.core.LiteralType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>, hydra.core.LiteralType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>, hydra.core.LiteralType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>, hydra.core.LiteralType>) (projected -> projected.source))))).apply(ad)), new hydra.core.Type.Literal(((java.util.function.Function<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>, hydra.core.LiteralType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>, hydra.core.LiteralType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>, hydra.core.LiteralType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>, hydra.core.LiteralType>) (projected -> projected.target))))).apply(ad)), step.get()))))))));
      })));
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Literal lt) {
        return (forLiteral).apply((lt).value);
      }
    });
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> passLiteral_encdec(java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Literal>>>> hydra_extract_core_literal2, hydra.graph.Graph graph, hydra.compute.Adapter<T0, T1, hydra.core.Literal, hydra.core.Literal> ad, hydra.coders.CoderDirection dir, hydra.context.Context cx, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      (((hydra_extract_core_literal2).apply(cx)).apply(graph)).apply(term),
      (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.adapt.utils.Utils.encodeDecode(
          dir,
          ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Literal, hydra.core.Literal>, hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Literal, hydra.core.Literal>, hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Literal, hydra.core.Literal>, hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Literal, hydra.core.Literal>, hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>>) (projected -> projected.coder))))).apply(ad),
          cx,
          l),
        (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (l2 -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Literal(l2))))))));
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> passList(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> forListType = (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (lt -> hydra.lib.eithers.Bind.apply(
      hydra.adapt.terms.Terms.termAdapter(
        cx,
        lt),
      (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ad -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(ad), t, new hydra.core.Type.List(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(ad)), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v3 -> hydra.adapt.terms.Terms.passList_encdec(
        ad,
        v1,
        v2,
        v3))))))))))))))));
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.List lt) {
        return (forListType).apply((lt).value);
      }
    });
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> passList_encdec(hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term> ad, hydra.coders.CoderDirection dir, hydra.context.Context cx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> visit(hydra.core.Term.List terms) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v1 -> hydra.adapt.utils.Utils.encodeDecode(
              dir,
              ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(ad),
              cx,
              v1)),
            (terms).value),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (newTerms -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.List(newTerms))))));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> passMap(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    java.util.function.Function<hydra.core.MapType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> forMapType = (java.util.function.Function<hydra.core.MapType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (mt -> {
      hydra.core.Type kt = (mt).keys;
      hydra.core.Type vt = (mt).values;
      return hydra.lib.eithers.Bind.apply(
        hydra.adapt.terms.Terms.termAdapter(
          cx,
          kt),
        (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (kad -> hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            vt),
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (vad -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(hydra.lib.logic.Or.apply(
            ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(kad),
            ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(vad)), t, new hydra.core.Type.Map(new hydra.core.MapType(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(kad), ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(vad))), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v3 -> hydra.adapt.terms.Terms.passMap_encdec(
            kad,
            vad,
            v1,
            v2,
            v3)))))))))))))))));
    });
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Map mt) {
        return (forMapType).apply((mt).value);
      }
    });
  }
  
  static <T0, T1, T2, T3> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> passMap_encdec(hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term> kad, hydra.compute.Adapter<T2, T3, hydra.core.Term, hydra.core.Term> vad, hydra.coders.CoderDirection dir, hydra.context.Context cx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> visit(hydra.core.Term.Map m) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (pair -> {
              hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
              hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
              return hydra.lib.eithers.Bind.apply(
                hydra.adapt.utils.Utils.encodeDecode(
                  dir,
                  ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(kad),
                  cx,
                  k.get()),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (newK -> hydra.lib.eithers.Bind.apply(
                  hydra.adapt.utils.Utils.encodeDecode(
                    dir,
                    ((java.util.function.Function<hydra.compute.Adapter<T2, T3, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T2, T3, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T2, T3, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T2, T3, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(vad),
                    cx,
                    v.get()),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (newV -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>right((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(newK, newV))))))))));
            }),
            hydra.lib.maps.ToList.apply((m).value)),
          (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (newPairs -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(newPairs)))))));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> passOptional(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>>> mapTerm = (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>>>) (graph -> (java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>>) (coder -> (java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (dir -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (term -> hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.maybeTerm(
        cx2,
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (x -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(x)))),
        graph,
        term),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (opt -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapMaybe.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v1 -> hydra.adapt.utils.Utils.encodeDecode(
            dir,
            coder,
            cx2,
            v1)),
          opt),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (newOpt -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Maybe(newOpt)))))))))))));
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Maybe ot) {
        return hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            (ot).value),
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (adapter -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(adapter), t, new hydra.core.Type.Maybe(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(adapter)), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v3 -> (((((mapTerm).apply((cx).graph)).apply(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(adapter))).apply(v1)).apply(v2)).apply(v3)))))))))))))));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> passRecord(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    java.util.function.Function<hydra.core.RowType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> forRecordType = (java.util.function.Function<hydra.core.RowType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (rt -> hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>) (v1 -> hydra.adapt.terms.Terms.fieldAdapter(
          cx,
          v1)),
        (rt).fields),
      (java.util.function.Function<java.util.List<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (adapters -> {
        hydra.util.Lazy<Boolean> lossy = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.Or.apply(
            p0,
            p1)),
          false,
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, Boolean>) (projected -> projected.isLossy)))),
            adapters)));
        hydra.util.Lazy<java.util.List<hydra.core.FieldType>> sfields_ = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, hydra.core.FieldType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, hydra.core.FieldType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, hydra.core.FieldType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, hydra.core.FieldType>) (projected -> projected.target)))),
          adapters));
        return (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(lossy.get(), t, new hydra.core.Type.Record(new hydra.core.RowType((rt).typeName, sfields_.get())), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v3 -> hydra.adapt.terms.Terms.passRecord_encdec(
          rt,
          adapters,
          v1,
          v2,
          v3)))))))))))));
      })));
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Record rt) {
        return (forRecordType).apply((rt).value);
      }
    });
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> passRecord_encdec(hydra.core.RowType rt, java.util.List<hydra.compute.Adapter<T0, T1, hydra.core.Field, hydra.core.Field>> adapters, hydra.coders.CoderDirection dir, hydra.context.Context cx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> visit(hydra.core.Term.Record rec) {
        java.util.List<hydra.core.Field> dfields = ((rec).value).fields;
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.compute.Adapter<T0, T1, hydra.core.Field, hydra.core.Field>, hydra.core.Field>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>>) (p -> hydra.adapt.utils.Utils.encodeDecode(
              dir,
              ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Field, hydra.core.Field>, hydra.compute.Coder<hydra.core.Field, hydra.core.Field>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Field, hydra.core.Field>, hydra.compute.Coder<hydra.core.Field, hydra.core.Field>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Field, hydra.core.Field>, hydra.compute.Coder<hydra.core.Field, hydra.core.Field>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Field, hydra.core.Field>, hydra.compute.Coder<hydra.core.Field, hydra.core.Field>>) (projected -> projected.coder))))).apply(hydra.lib.pairs.First.apply(p)),
              cx,
              hydra.lib.pairs.Second.apply(p))),
            hydra.lib.lists.Zip.apply(
              adapters,
              dfields)),
          (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (newFields -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Record(new hydra.core.Record((rt).typeName, newFields)))))));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> passSet(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Set st) {
        return hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            (st).value),
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ad -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(ad), t, new hydra.core.Type.Set(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(ad)), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v3 -> hydra.adapt.terms.Terms.passSet_encdec(
            ad,
            v1,
            v2,
            v3)))))))))))))));
      }
    });
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> passSet_encdec(hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term> ad, hydra.coders.CoderDirection dir, hydra.context.Context cx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> visit(hydra.core.Term.Set terms) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v1 -> hydra.adapt.utils.Utils.encodeDecode(
              dir,
              ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(ad),
              cx,
              v1)),
            hydra.lib.sets.ToList.apply((terms).value)),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (newTerms -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(newTerms)))))));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> passUnion(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Union rt) {
        java.util.List<hydra.core.FieldType> sfields = ((rt).value).fields;
        hydra.core.Name tname = ((rt).value).typeName;
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<String, hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>>) (f -> hydra.lib.eithers.Bind.apply(
              hydra.adapt.terms.Terms.fieldAdapter(
                cx,
                f),
              (java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, hydra.util.Either<String, hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>>) (ad -> (hydra.util.Either<String, hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>) ((hydra.util.Either<String, hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>) (hydra.util.Either.<String, hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>right((hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>) ((hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>) (new hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>((f).name, ad))))))))),
            sfields),
          (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (adapters -> {
            hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>> adaptersMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(adapters));
            hydra.util.Lazy<Boolean> lossy = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
              (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.Or.apply(
                p0,
                p1)),
              false,
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>, Boolean>) (pair -> ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, Boolean>) (projected -> projected.isLossy))))).apply(hydra.lib.pairs.Second.apply(pair))),
                adapters)));
            hydra.util.Lazy<java.util.List<hydra.core.FieldType>> sfields_ = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>>, hydra.core.FieldType>) (pair -> ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, hydra.core.FieldType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, hydra.core.FieldType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, hydra.core.FieldType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FieldType, hydra.core.FieldType, hydra.core.Field, hydra.core.Field>, hydra.core.FieldType>) (projected -> projected.target))))).apply(hydra.lib.pairs.Second.apply(pair))),
              adapters));
            return (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(lossy.get(), t, new hydra.core.Type.Union(new hydra.core.RowType(tname, sfields_.get())), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (dir -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (_cx -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (term -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(term)))))))))))))));
          }));
      }
    });
  }
  
  static <T0, T1, T2> hydra.util.Either<T2, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> passUnit(T0 _cx, T1 ignored) {
    return (hydra.util.Either<T2, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<T2, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<T2, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(false, new hydra.core.Type.Unit(), new hydra.core.Type.Unit(), (hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) ((hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) (new hydra.compute.Coder<hydra.core.Term, hydra.core.Term>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (_cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (_2 -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Unit()))))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (_cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (_2 -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Unit()))))))))))))))));
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> passWrapped(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Wrap wt) {
        hydra.core.Name tname = ((wt).value).typeName;
        java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>>> mapTerm = (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>>>) (graph -> (java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>>) (coder -> (java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (dir -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (term -> hydra.lib.eithers.Bind.apply(
          hydra.extract.core.Core.wrap(
            cx2,
            tname,
            graph,
            term),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (unwrapped -> hydra.lib.eithers.Bind.apply(
            hydra.adapt.utils.Utils.encodeDecode(
              dir,
              coder,
              cx2,
              unwrapped),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (newTerm -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(tname, newTerm))))))))))))));
        hydra.core.Type ot = ((wt).value).body;
        return hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            ot),
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (adapter -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(adapter), t, new hydra.core.Type.Wrap(new hydra.core.WrappedType(tname, ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(adapter))), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v3 -> (((((mapTerm).apply((cx).graph)).apply(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(adapter))).apply(v1)).apply(v2)).apply(v3)))))))))))))));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> setToList(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> forSetType = (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (st -> hydra.lib.eithers.Bind.apply(
      hydra.adapt.terms.Terms.termAdapter(
        cx,
        new hydra.core.Type.List(st)),
      (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ad -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(ad), t, ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(ad), (hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) ((hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) (new hydra.compute.Coder<hydra.core.Term, hydra.core.Term>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v2 -> hydra.adapt.terms.Terms.setToList_encode(
        ad,
        v1,
        v2))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v2 -> hydra.adapt.terms.Terms.setToList_decode(
        ad,
        v1,
        v2)))))))))))))))));
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Set st) {
        return (forSetType).apply((st).value);
      }
    });
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> setToList_encode(hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term> ad, hydra.context.Context cx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> visit(hydra.core.Term.Set s) {
        return hydra.adapt.utils.Utils.encodeDecode(
          new hydra.coders.CoderDirection.Encode(),
          ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(ad),
          cx,
          new hydra.core.Term.List(hydra.lib.sets.ToList.apply((s).value)));
      }
    });
  }
  
  static <T0> hydra.util.Either<T0, hydra.core.Term> setToList_forListTerm(hydra.core.Term t) {
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T0, hydra.core.Term> visit(hydra.core.Term.List l) {
        return (hydra.util.Either<T0, hydra.core.Term>) ((hydra.util.Either<T0, hydra.core.Term>) (hydra.util.Either.<T0, hydra.core.Term>right(new hydra.core.Term.Set(hydra.lib.sets.FromList.apply((l).value)))));
      }
    });
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> setToList_decode(hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term> ad, hydra.context.Context cx, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.adapt.utils.Utils.encodeDecode(
        new hydra.coders.CoderDirection.Decode(),
        ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(ad),
        cx,
        term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (listTerm -> hydra.adapt.terms.Terms.setToList_forListTerm(listTerm)));
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> simplifyApplication(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    java.util.function.Function<hydra.core.ApplicationType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> forApplicationType = (java.util.function.Function<hydra.core.ApplicationType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (at -> {
      hydra.core.Type lhs = (at).function;
      return hydra.lib.eithers.Bind.apply(
        hydra.adapt.terms.Terms.termAdapter(
          cx,
          lhs),
        (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ad -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(false, t, ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(ad), hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v3 -> hydra.adapt.terms.Terms.simplifyApplication_encdec(
          ad,
          v1,
          v2,
          v3)))))))))))))));
    });
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Application at) {
        return (forApplicationType).apply((at).value);
      }
    });
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T2> simplifyApplication_encdec(hydra.compute.Adapter<T0, T1, T2, T2> ad, hydra.coders.CoderDirection dir, hydra.context.Context cx, T2 term) {
    return hydra.adapt.utils.Utils.<T2>encodeDecode(
      dir,
      ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T2>, hydra.compute.Coder<T2, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T2>, hydra.compute.Coder<T2, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T2>, hydra.compute.Coder<T2, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T2>, hydra.compute.Coder<T2, T2>>) (projected -> projected.coder))))).apply(ad),
      cx,
      term);
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> termAdapter(hydra.coders.AdapterContext cx, hydra.core.Type typ) {
    java.util.function.Function<hydra.core.Type, java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>> pass = (java.util.function.Function<hydra.core.Type, java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>>) (t -> (hydra.reflect.Reflect.typeVariant(hydra.rewriting.Rewriting.deannotateType(t))).accept(new hydra.variants.TypeVariant.PartialVisitor<>() {
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Annotated ignored) {
        return (java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (java.util.List.<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>of());
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Application ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.passApplication(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Either ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.passEither(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Forall ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.passForall(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Function ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.passFunction(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.List ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.passList(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Literal ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.passLiteral(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Map ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.passMap(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Maybe ignored) {
        return java.util.List.of(
          (java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.passOptional(
            p0,
            p1)),
          (java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.maybeToList(
            p0,
            p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Pair ignored) {
        return (java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (java.util.List.<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>of());
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Record ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.passRecord(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Set ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.passSet(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Union ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.passUnion(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Unit ignored) {
        return java.util.List.of(p0 -> p1 -> hydra.adapt.terms.Terms.<hydra.coders.AdapterContext, hydra.core.Type, String>passUnit(
          p0,
          p1));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Variable ignored) {
        return (java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (java.util.List.<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>of());
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Wrap ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.passWrapped(
          p0,
          p1)));
      }
    }));
    java.util.function.Function<hydra.coders.AdapterContext, hydra.coders.LanguageConstraints> constraints = (java.util.function.Function<hydra.coders.AdapterContext, hydra.coders.LanguageConstraints>) (cx2 -> ((cx2).language).constraints);
    java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, Boolean>> variantIsSupported = (java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, Boolean>>) (cx2 -> (java.util.function.Function<hydra.core.Type, Boolean>) (t -> hydra.lib.sets.Member.apply(
      hydra.reflect.Reflect.typeVariant(t),
      ((constraints).apply(cx2)).typeVariants)));
    java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, Boolean>> supportedAtTopLevel = (java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, Boolean>>) (cx2 -> (java.util.function.Function<hydra.core.Type, Boolean>) (t -> hydra.lib.logic.And.apply(
      ((variantIsSupported).apply(cx2)).apply(t),
      (((constraints).apply(cx2)).types).apply(t))));
    java.util.function.Function<hydra.core.Type, java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>> trySubstitution = (java.util.function.Function<hydra.core.Type, java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>>) (t -> (hydra.reflect.Reflect.typeVariant(t)).accept(new hydra.variants.TypeVariant.PartialVisitor<>() {
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Annotated ignored) {
        return (java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (java.util.List.<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>of());
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Application ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.simplifyApplication(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Either ignored) {
        return (java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (java.util.List.<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>of());
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Forall ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.lambdaToMonotype(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Function ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.functionToUnion(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.List ignored) {
        return (java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (java.util.List.<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>of());
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Literal ignored) {
        return (java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (java.util.List.<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>of());
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Map ignored) {
        return (java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (java.util.List.<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>of());
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Maybe ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.maybeToList(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Pair ignored) {
        return (java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (java.util.List.<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>of());
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Record ignored) {
        return (java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (java.util.List.<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>of());
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Set ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.setToList(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Union ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.unionToRecord(
          p0,
          p1)));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Unit ignored) {
        return java.util.List.of(p0 -> p1 -> hydra.adapt.terms.Terms.<hydra.coders.AdapterContext, hydra.core.Type, String>unitToRecord(
          p0,
          p1));
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Variable ignored) {
        return (java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (java.util.List.<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>of());
      }
      
      @Override
      public java.util.List<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> visit(hydra.variants.TypeVariant.Wrap ignored) {
        return java.util.List.of((java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (p0 -> p1 -> hydra.adapt.terms.Terms.wrapToUnwrapped(
          p0,
          p1)));
      }
    }));
    java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>> alts = (java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>>) (cx2 -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (t -> hydra.lib.eithers.MapList.apply(
      (java.util.function.Function<java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (c -> ((c).apply(cx2)).apply(t)),
      hydra.lib.logic.IfElse.lazy(
        ((supportedAtTopLevel).apply(cx2)).apply(t),
        () -> (pass).apply(t),
        () -> (trySubstitution).apply(t)))));
    java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, Boolean>> supported = (java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.Type, Boolean>>) (cx2 -> (java.util.function.Function<hydra.core.Type, Boolean>) (v1 -> hydra.adapt.utils.Utils.typeIsSupported(
      (constraints).apply(cx2),
      v1)));
    hydra.util.Lazy<hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> dflt = new hydra.util.Lazy<>(() -> (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> otherwise(hydra.core.Type instance) {
        return hydra.adapt.utils.Utils.chooseAdapter(
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>>) (v1 -> ((alts).apply(cx)).apply(v1)),
          (java.util.function.Function<hydra.core.Type, Boolean>) (v1 -> ((supported).apply(cx)).apply(v1)),
          hydra.show.core.Core::type,
          hydra.show.core.Core::type,
          typ);
      }
      
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Variable name) {
        return hydra.adapt.terms.Terms.forTypeReference(
          cx,
          (name).value);
      }
    }));
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> otherwise(hydra.core.Type instance) {
        return dflt.get();
      }
      
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Annotated at) {
        return hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            ((at).value).body),
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ad -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(ad), ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.source))))).apply(ad), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(ad), ((at).value).annotation)), ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(ad)))))))))));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> unionToRecord(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Field>> forField = (java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Field>>) (field -> {
      hydra.core.Name fn = (field).name;
      hydra.core.Term fterm = (field).term;
      return (fterm).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Maybe<hydra.core.Field> visit(hydra.core.Term.Maybe opt) {
          return hydra.lib.maybes.Bind.apply(
            (opt).value,
            (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Field>>) (t2 -> hydra.util.Maybe.just(new hydra.core.Field(fn, t2))));
        }
      });
    });
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Union rt) {
        hydra.core.Name nm = ((rt).value).typeName;
        java.util.List<hydra.core.FieldType> sfields = ((rt).value).fields;
        hydra.core.Type target = new hydra.core.Type.Record(hydra.adapt.terms.Terms.unionTypeToRecordType((rt).value));
        java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.FieldType, hydra.core.Field>>> toRecordField = (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.FieldType, hydra.core.Field>>>) (term -> (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.FieldType, hydra.core.Field>>) (fn -> (java.util.function.Function<hydra.core.FieldType, hydra.core.Field>) (f -> {
          hydra.core.Name fn_ = (f).name;
          return new hydra.core.Field(fn_, new hydra.core.Term.Maybe(hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              fn_,
              fn),
            () -> hydra.util.Maybe.just(term),
            () -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))));
        })));
        return hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            target),
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ad -> {
            hydra.graph.Graph graph = (cx).graph;
            return (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, Boolean>) (projected -> projected.isLossy))))).apply(ad), t, ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(ad), (hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) ((hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) (new hydra.compute.Coder<hydra.core.Term, hydra.core.Term>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (term_ -> hydra.lib.eithers.Bind.apply(
              hydra.extract.core.Core.injection(
                cx2,
                ((rt).value).typeName,
                graph,
                term_),
              (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (field -> {
                hydra.core.Name fn = (field).name;
                hydra.core.Term term = (field).term;
                return ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(ad))).apply(cx2)).apply(new hydra.core.Term.Record(new hydra.core.Record(nm, hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.FieldType, hydra.core.Field>) (v1 -> (((toRecordField).apply(term)).apply(fn)).apply(v1)),
                  sfields))));
              })))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (term -> hydra.lib.eithers.Bind.apply(
              ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (projected -> projected.decode))).apply(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(ad))).apply(cx2)).apply(term),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (recTerm -> hydra.adapt.terms.Terms.unionToRecord_forRecTerm(
                forField,
                hydra.show.core.Core::term,
                hydra.show.core.Core::type,
                t,
                cx2,
                nm,
                ad,
                term,
                recTerm))))))))))))))));
          }));
      }
    });
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field> unionToRecord_fromRecordFields(java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Field>> forField, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<hydra.core.Type, String> hydra_show_core_type2, hydra.core.Type t, hydra.context.Context cx, hydra.core.Term term, T0 term_, hydra.core.Type t_, java.util.List<hydra.core.Field> fields) {
    hydra.util.Lazy<java.util.List<hydra.core.Field>> matches = new hydra.util.Lazy<>(() -> hydra.lib.maybes.MapMaybe.apply(
      forField,
      fields));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(matches.get()),
      () -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
        "cannot convert term back to union: ",
        (hydra_show_core_term2).apply(term),
        " where type = ",
        (hydra_show_core_type2).apply(t),
        "    and target type = ",
        (hydra_show_core_type2).apply(t_)))), cx))))),
      () -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Field>right(hydra.lib.lists.Head.apply(matches.get())))));
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> unionToRecord_forRecTerm(java.util.function.Function<hydra.core.Field, hydra.util.Maybe<hydra.core.Field>> forField, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<hydra.core.Type, String> hydra_show_core_type2, hydra.core.Type t, hydra.context.Context cx, hydra.core.Name nm, hydra.compute.Adapter<T0, hydra.core.Type, T1, T2> ad, hydra.core.Term term, hydra.core.Term recTerm) {
    return (recTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> visit(hydra.core.Term.Record rec) {
        java.util.List<hydra.core.Field> fields = ((rec).value).fields;
        return hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.unionToRecord_fromRecordFields(
            forField,
            hydra_show_core_term2,
            hydra_show_core_type2,
            t,
            cx,
            term,
            new hydra.core.Term.Record(new hydra.core.Record(nm, fields)),
            ((java.util.function.Function<hydra.compute.Adapter<T0, hydra.core.Type, T1, T2>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<T0, hydra.core.Type, T1, T2>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<T0, hydra.core.Type, T1, T2>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<T0, hydra.core.Type, T1, T2>, hydra.core.Type>) (projected -> projected.target))))).apply(ad),
            fields),
          (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (resultField -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Union(new hydra.core.Injection(nm, resultField)))))));
      }
    });
  }
  
  static hydra.core.RowType unionTypeToRecordType(hydra.core.RowType rt) {
    java.util.function.Function<hydra.core.FieldType, hydra.core.FieldType> makeOptional = (java.util.function.Function<hydra.core.FieldType, hydra.core.FieldType>) (f -> {
      hydra.core.Name fn = (f).name;
      hydra.core.Type ft = (f).type;
      return new hydra.core.FieldType(fn, hydra.rewriting.Rewriting.mapBeneathTypeAnnotations(
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.Maybe(x)),
        ft));
    });
    return new hydra.core.RowType((rt).typeName, hydra.lib.lists.Map.apply(
      makeOptional,
      (rt).fields));
  }
  
  static <T0, T1, T2> hydra.util.Either<T2, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> unitToRecord(T0 _cx, T1 ignored) {
    return (hydra.util.Either<T2, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<T2, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<T2, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(false, new hydra.core.Type.Unit(), new hydra.core.Type.Record(new hydra.core.RowType(new hydra.core.Name("_Unit"), (java.util.List<hydra.core.FieldType>) (java.util.List.<hydra.core.FieldType>of()))), (hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) ((hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) (new hydra.compute.Coder<hydra.core.Term, hydra.core.Term>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (_cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (_2 -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("_Unit"), (java.util.List<hydra.core.Field>) (java.util.List.<hydra.core.Field>of())))))))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (_cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (_2 -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Unit()))))))))))))))));
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> wrapToUnwrapped(hydra.coders.AdapterContext cx, hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> visit(hydra.core.Type.Wrap wt) {
        hydra.core.Name tname = ((wt).value).typeName;
        hydra.core.Type typ = ((wt).value).body;
        return hydra.lib.eithers.Bind.apply(
          hydra.adapt.terms.Terms.termAdapter(
            cx,
            typ),
          (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>>) (ad -> (hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<String, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>right((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) ((hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>) (new hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>(false, t, ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(ad), (hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) ((hydra.compute.Coder<hydra.core.Term, hydra.core.Term>) (new hydra.compute.Coder<hydra.core.Term, hydra.core.Term>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v2 -> hydra.adapt.terms.Terms.wrapToUnwrapped_encode(
            (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>>) (p0 -> p1 -> p2 -> p3 -> hydra.extract.core.Core.wrap(
              p0,
              p1,
              p2,
              p3)),
            tname,
            (cx).graph,
            ad,
            v1,
            v2))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (v2 -> hydra.adapt.terms.Terms.wrapToUnwrapped_decode(
            tname,
            ad,
            v1,
            v2))))))))))))))));
      }
    });
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T2> wrapToUnwrapped_encode(java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>> hydra_extract_core_wrap2, hydra.core.Name tname, hydra.graph.Graph graph, hydra.compute.Adapter<T0, T1, hydra.core.Term, T2> ad, hydra.context.Context cx, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      ((((hydra_extract_core_wrap2).apply(cx)).apply(tname)).apply(graph)).apply(term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T2>>) (unwrapped -> ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T2>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T2>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) (projected -> projected.coder))))).apply(ad))).apply(cx)).apply(unwrapped)));
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> wrapToUnwrapped_decode(hydra.core.Name tname, hydra.compute.Adapter<T0, T1, hydra.core.Term, T2> ad, hydra.context.Context cx, T2 term) {
    return hydra.lib.eithers.Bind.apply(
      ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T2, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T2, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>>>) (projected -> projected.decode))).apply(((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, hydra.core.Term, T2>, hydra.compute.Coder<hydra.core.Term, T2>>) (projected -> projected.coder))))).apply(ad))).apply(cx)).apply(term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (decoded -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(tname, decoded)))))));
  }
}
