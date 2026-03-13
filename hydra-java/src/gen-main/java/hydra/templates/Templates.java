// Note: this is an automatically generated file. Do not edit.

package hydra.templates;

/**
 * A utility which instantiates a nonrecursive type with default values
 */
public interface Templates {
  static hydra.util.Either<hydra.context.InContext<hydra.error.DecodingError>, java.util.Map<hydra.core.Name, hydra.core.Type>> graphToSchema(hydra.context.Context cx, hydra.graph.Graph graph, java.util.List<hydra.core.Binding> els) {
    java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.DecodingError>, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>> toPair = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.DecodingError>, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>>) (el -> {
      hydra.core.Name name = (el).name;
      return hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.Bimap.apply(
          (java.util.function.Function<hydra.error.DecodingError, hydra.context.InContext<hydra.error.DecodingError>>) (_wc_e -> (hydra.context.InContext<hydra.error.DecodingError>) (new hydra.context.InContext<hydra.error.DecodingError>(_wc_e, cx))),
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_wc_a -> _wc_a),
          hydra.decode.core.Core.type(
            graph,
            (el).term)),
        (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.DecodingError>, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>>) (t -> hydra.util.Either.<hydra.context.InContext<hydra.error.DecodingError>, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>right((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(name, t))))));
    });
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        toPair,
        els),
      (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>, hydra.util.Either<hydra.context.InContext<hydra.error.DecodingError>, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (pairs -> hydra.util.Either.<hydra.context.InContext<hydra.error.DecodingError>, java.util.Map<hydra.core.Name, hydra.core.Type>>right(hydra.lib.maps.FromList.apply(pairs))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> instantiateTemplate(hydra.context.Context cx, Boolean minimal, java.util.Map<hydra.core.Name, hydra.core.Type> schema, hydra.core.Type t) {
    java.util.function.Function<hydra.core.FloatType, hydra.core.FloatValue> forFloat = (java.util.function.Function<hydra.core.FloatType, hydra.core.FloatValue>) (ft -> (ft).accept(new hydra.core.FloatType.PartialVisitor<>() {
      @Override
      public hydra.core.FloatValue visit(hydra.core.FloatType.Bigfloat ignored) {
        return new hydra.core.FloatValue.Bigfloat(new java.math.BigDecimal("0.0"));
      }
      
      @Override
      public hydra.core.FloatValue visit(hydra.core.FloatType.Float32 ignored) {
        return new hydra.core.FloatValue.Float32((float) (0.0));
      }
      
      @Override
      public hydra.core.FloatValue visit(hydra.core.FloatType.Float64 ignored) {
        return new hydra.core.FloatValue.Float64(0.0);
      }
    }));
    java.util.function.Function<hydra.core.IntegerType, hydra.core.IntegerValue> forInteger = (java.util.function.Function<hydra.core.IntegerType, hydra.core.IntegerValue>) (it -> (it).accept(new hydra.core.IntegerType.PartialVisitor<>() {
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Bigint ignored) {
        return new hydra.core.IntegerValue.Bigint(new java.math.BigInteger("0"));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int8 ignored) {
        return new hydra.core.IntegerValue.Int8((byte) (0));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int16 ignored) {
        return new hydra.core.IntegerValue.Int16((short) (0));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int32 ignored) {
        return new hydra.core.IntegerValue.Int32(0);
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int64 ignored) {
        return new hydra.core.IntegerValue.Int64((long) (0));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint8 ignored) {
        return new hydra.core.IntegerValue.Uint8((short) (0));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint16 ignored) {
        return new hydra.core.IntegerValue.Uint16('\u0000');
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint32 ignored) {
        return new hydra.core.IntegerValue.Uint32((long) (0));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint64 ignored) {
        return new hydra.core.IntegerValue.Uint64(new java.math.BigInteger("0"));
      }
    }));
    java.util.function.Function<hydra.core.LiteralType, hydra.core.Literal> forLiteral = (java.util.function.Function<hydra.core.LiteralType, hydra.core.Literal>) (lt -> (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.core.Literal visit(hydra.core.LiteralType.Binary ignored) {
        return new hydra.core.Literal.String_("");
      }
      
      @Override
      public hydra.core.Literal visit(hydra.core.LiteralType.Boolean_ ignored) {
        return new hydra.core.Literal.Boolean_(false);
      }
      
      @Override
      public hydra.core.Literal visit(hydra.core.LiteralType.Integer_ it) {
        return new hydra.core.Literal.Integer_((forInteger).apply((it).value));
      }
      
      @Override
      public hydra.core.Literal visit(hydra.core.LiteralType.Float_ ft) {
        return new hydra.core.Literal.Float_((forFloat).apply((ft).value));
      }
      
      @Override
      public hydra.core.Literal visit(hydra.core.LiteralType.String_ ignored) {
        return new hydra.core.Literal.String_("");
      }
    }));
    java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>> inst = (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (v1 -> hydra.templates.Templates.instantiateTemplate(
      cx,
      minimal,
      schema,
      v1));
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Type.Annotated at) {
        return (inst).apply(((at).value).body);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Type.Application ignored) {
        return hydra.templates.Templates.instantiateTemplate_noPoly(cx);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Type.Function ignored) {
        return hydra.templates.Templates.instantiateTemplate_noPoly(cx);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Type.Forall ignored) {
        return hydra.templates.Templates.instantiateTemplate_noPoly(cx);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Type.List et) {
        return hydra.lib.logic.IfElse.lazy(
          minimal,
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))),
          () -> hydra.lib.eithers.Bind.apply(
            (inst).apply((et).value),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (e -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.List(java.util.List.of(e))))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Type.Literal lt) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Literal((forLiteral).apply((lt).value)));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Type.Map mt) {
        hydra.core.Type kt = ((mt).value).keys;
        hydra.core.Type vt = ((mt).value).values;
        return hydra.lib.logic.IfElse.lazy(
          minimal,
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Map((java.util.Map<hydra.core.Term, hydra.core.Term>) ((java.util.Map<hydra.core.Term, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Term, hydra.core.Term>apply())))),
          () -> hydra.lib.eithers.Bind.apply(
            (inst).apply(kt),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (ke -> hydra.lib.eithers.Bind.apply(
              (inst).apply(vt),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (ve -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Map(hydra.lib.maps.Singleton.apply(
                ke,
                ve))))))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Type.Maybe ot) {
        return hydra.lib.logic.IfElse.lazy(
          minimal,
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
          () -> hydra.lib.eithers.Bind.apply(
            (inst).apply((ot).value),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (e -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Maybe(hydra.util.Maybe.just(e))))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Type.Record rt) {
        java.util.List<hydra.core.FieldType> fields = ((rt).value).fields;
        hydra.core.Name tname = ((rt).value).typeName;
        java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>> toField = (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>>) (ft -> hydra.lib.eithers.Bind.apply(
          (inst).apply((ft).type),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>>) (e -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>right(new hydra.core.Field((ft).name, e)))));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            toField,
            fields),
          (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (dfields -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Record(new hydra.core.Record(tname, dfields)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Type.Set et) {
        return hydra.lib.logic.IfElse.lazy(
          minimal,
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Set((java.util.Set<hydra.core.Term>) (hydra.lib.sets.Empty.<hydra.core.Term>apply()))),
          () -> hydra.lib.eithers.Bind.apply(
            (inst).apply((et).value),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (e -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(java.util.List.of(e)))))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Type.Variable tname) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            "Type variable ",
            hydra.lib.strings.Cat2.apply(
              hydra.show.core.Core.term(new hydra.core.Term.Variable((tname).value)),
              " not found in schema")))), cx))),
          inst,
          hydra.lib.maps.Lookup.apply(
            (tname).value,
            schema));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Type.Wrap wt) {
        hydra.core.Type t_ = ((wt).value).body;
        hydra.core.Name tname = ((wt).value).typeName;
        return hydra.lib.eithers.Bind.apply(
          (inst).apply(t_),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (e -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(tname, e)))));
      }
    });
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0> instantiateTemplate_noPoly(hydra.context.Context cx) {
    return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T0>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError("Polymorphic and function types are not currently supported")), cx)));
  }
}
