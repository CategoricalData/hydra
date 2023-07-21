package hydra.termEncoding;

/**
 * Implementation of LambdaGraph's sigma encoding, which represents terms as terms
 */
public interface TermEncoding {
  static <A> hydra.core.Term<A> sigmaEncodeAnnotated(hydra.core.Annotated<hydra.core.Term<A>, A> a) {
    return new hydra.core.Term.Annotated(new hydra.core.Annotated(hydra.termEncoding.TermEncoding.sigmaEncodeTerm(((a)).subject), ((a)).annotation));
  }

  static <A> hydra.core.Term<A> sigmaEncodeApplication(hydra.core.Application<A> app) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Application"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.FieldName("function"), hydra.termEncoding.TermEncoding.sigmaEncodeTerm(((app)).function)),
      new hydra.core.Field(new hydra.core.FieldName("argument"), hydra.termEncoding.TermEncoding.sigmaEncodeTerm(((app)).argument)))));
  }

  static <A> hydra.core.Term<A> sigmaEncodeCaseStatement(hydra.core.CaseStatement<A> cs) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.CaseStatement"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.FieldName("typeName"), hydra.termEncoding.TermEncoding.sigmaEncodeName(((cs)).typeName)),
      new hydra.core.Field(new hydra.core.FieldName("default"), new hydra.core.Term.Optional(hydra.lib.optionals.Map.apply(
        (x -> hydra.termEncoding.TermEncoding.sigmaEncodeTerm(x)),
        ((cs)).default_))),
      new hydra.core.Field(new hydra.core.FieldName("cases"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (x -> hydra.termEncoding.TermEncoding.sigmaEncodeField(x)),
        ((cs)).cases))))));
  }
  
  static <A> hydra.core.Term<A> sigmaEncodeElimination(hydra.core.Elimination<A> v1) {
    return ((v1)).accept(new hydra.core.Elimination.Visitor<>() {
      @Override
      public hydra.core.Term<A> visit(hydra.core.Elimination.List<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Elimination"), new hydra.core.Field(new hydra.core.FieldName("list"), hydra.termEncoding.TermEncoding.sigmaEncodeTerm((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Elimination.Optional<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Elimination"), new hydra.core.Field(new hydra.core.FieldName("optional"), hydra.termEncoding.TermEncoding.sigmaEncodeOptionalCases((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Elimination.Record<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Elimination"), new hydra.core.Field(new hydra.core.FieldName("record"), hydra.termEncoding.TermEncoding.sigmaEncodeProjection((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Elimination.Union<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Elimination"), new hydra.core.Field(new hydra.core.FieldName("union"), hydra.termEncoding.TermEncoding.sigmaEncodeCaseStatement((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Elimination.Wrap<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Elimination"), new hydra.core.Field(new hydra.core.FieldName("wrap"), hydra.termEncoding.TermEncoding.sigmaEncodeName((instance.value)))));
      }
    });
  }

  static <A> hydra.core.Term<A> sigmaEncodeField(hydra.core.Field<A> f) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Field"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.FieldName("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((((f)).name).value))),
      new hydra.core.Field(new hydra.core.FieldName("term"), hydra.termEncoding.TermEncoding.sigmaEncodeTerm(((f)).term)))));
  }

  static <A> hydra.core.Term<A> sigmaEncodeFieldName(hydra.core.FieldName fn) {
    return new hydra.core.Term.Wrap(new hydra.core.Nominal(new hydra.core.Name("hydra/core.FieldName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((fn)).value))));
  }

  static <A> hydra.core.Term<A> sigmaEncodeFloatValue(hydra.core.FloatValue v1) {
    return ((v1)).accept(new hydra.core.FloatValue.Visitor<>() {
      @Override
      public hydra.core.Term<A> visit(hydra.core.FloatValue.Bigfloat instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.FloatValue"), new hydra.core.Field(new hydra.core.FieldName("bigfloat"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat((instance.value)))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.FloatValue.Float32 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.FloatValue"), new hydra.core.Field(new hydra.core.FieldName("float32"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((instance.value)))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.FloatValue.Float64 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.FloatValue"), new hydra.core.Field(new hydra.core.FieldName("float64"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64((instance.value)))))));
      }
    });
  }

  static <A> hydra.core.Term<A> sigmaEncodeFunction(hydra.core.Function<A> v1) {
    return ((v1)).accept(new hydra.core.Function.Visitor<>() {
      @Override
      public hydra.core.Term<A> visit(hydra.core.Function.Elimination<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Function"), new hydra.core.Field(new hydra.core.FieldName("elimination"), hydra.termEncoding.TermEncoding.sigmaEncodeElimination((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Function.Lambda<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Function"), new hydra.core.Field(new hydra.core.FieldName("lambda"), hydra.termEncoding.TermEncoding.sigmaEncodeLambda((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Function.Primitive<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Function"), new hydra.core.Field(new hydra.core.FieldName("primitive"), hydra.termEncoding.TermEncoding.sigmaEncodeName((instance.value)))));
      }
    });
  }

  static <A> hydra.core.Term<A> sigmaEncodeInjection(hydra.core.Injection<A> i) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Injection"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.FieldName("typeName"), hydra.termEncoding.TermEncoding.sigmaEncodeName(((i)).typeName)),
      new hydra.core.Field(new hydra.core.FieldName("field"), hydra.termEncoding.TermEncoding.sigmaEncodeField(((i)).field)))));
  }

  static <A> hydra.core.Term<A> sigmaEncodeIntegerValue(hydra.core.IntegerValue v1) {
    return ((v1)).accept(new hydra.core.IntegerValue.Visitor<>() {
      @Override
      public hydra.core.Term<A> visit(hydra.core.IntegerValue.Bigint instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.FieldName("bigint"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Bigint((instance.value)))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.IntegerValue.Int8 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.FieldName("int8"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int8((instance.value)))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.IntegerValue.Int16 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.FieldName("int16"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int16((instance.value)))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.IntegerValue.Int32 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.FieldName("int32"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32((instance.value)))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.IntegerValue.Int64 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.FieldName("int64"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((instance.value)))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.IntegerValue.Uint8 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.FieldName("uint8"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint8((instance.value)))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.IntegerValue.Uint16 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.FieldName("uint16"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint16((instance.value)))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.IntegerValue.Uint32 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.FieldName("uint32"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint32((instance.value)))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.IntegerValue.Uint64 instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.IntegerValue"), new hydra.core.Field(new hydra.core.FieldName("uint64"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint64((instance.value)))))));
      }
    });
  }

  static <A> hydra.core.Term<A> sigmaEncodeLambda(hydra.core.Lambda<A> l) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Lambda"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.FieldName("parameter"), hydra.termEncoding.TermEncoding.sigmaEncodeName(((l)).parameter)),
      new hydra.core.Field(new hydra.core.FieldName("body"), hydra.termEncoding.TermEncoding.sigmaEncodeTerm(((l)).body)))));
  }

  static <A> hydra.core.Term<A> sigmaEncodeLiteral(hydra.core.Literal v1) {
    return ((v1)).accept(new hydra.core.Literal.Visitor<>() {
      @Override
      public hydra.core.Term<A> visit(hydra.core.Literal.Binary instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Literal"), new hydra.core.Field(new hydra.core.FieldName("binary"), new hydra.core.Term.Literal(new hydra.core.Literal.Binary((instance.value))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Literal.Boolean_ instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Literal"), new hydra.core.Field(new hydra.core.FieldName("boolean"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_((instance.value))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Literal.Float_ instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Literal"), new hydra.core.Field(new hydra.core.FieldName("float"), hydra.termEncoding.TermEncoding.sigmaEncodeFloatValue((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Literal.Integer_ instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Literal"), new hydra.core.Field(new hydra.core.FieldName("integer"), hydra.termEncoding.TermEncoding.sigmaEncodeIntegerValue((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Literal.String_ instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Literal"), new hydra.core.Field(new hydra.core.FieldName("string"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((instance.value))))));
      }
    });
  }

  static <A> hydra.core.Term<A> sigmaEncodeName(hydra.core.Name fn) {
    return new hydra.core.Term.Wrap(new hydra.core.Nominal(new hydra.core.Name("hydra/core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((fn)).value))));
  }

  static <A> hydra.core.Term<A> sigmaEncodeNominalTerm(hydra.core.Nominal<hydra.core.Term<A>> n) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Nominal"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.FieldName("typeName"), hydra.termEncoding.TermEncoding.sigmaEncodeName(((n)).typeName)),
      new hydra.core.Field(new hydra.core.FieldName("object"), hydra.termEncoding.TermEncoding.sigmaEncodeTerm(((n)).object)))));
  }

  static <A> hydra.core.Term<A> sigmaEncodeOptionalCases(hydra.core.OptionalCases<A> oc) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.OptionalCases"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.FieldName("nothing"), hydra.termEncoding.TermEncoding.sigmaEncodeTerm(((oc)).nothing)),
      new hydra.core.Field(new hydra.core.FieldName("just"), hydra.termEncoding.TermEncoding.sigmaEncodeTerm(((oc)).just)))));
  }

  static <A> hydra.core.Term<A> sigmaEncodeProjection(hydra.core.Projection p) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Projection"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.FieldName("typeName"), hydra.termEncoding.TermEncoding.sigmaEncodeName(((p)).typeName)),
      new hydra.core.Field(new hydra.core.FieldName("field"), hydra.termEncoding.TermEncoding.sigmaEncodeFieldName(((p)).field)))));
  }

  static <A> hydra.core.Term<A> sigmaEncodeRecord(hydra.core.Record<A> r) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Record"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.FieldName("typeName"), hydra.termEncoding.TermEncoding.sigmaEncodeName(((r)).typeName)),
      new hydra.core.Field(new hydra.core.FieldName("fields"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (x -> hydra.termEncoding.TermEncoding.sigmaEncodeField(x)),
        ((r)).fields))))));
  }

  static <A> hydra.core.Term<A> sigmaEncodeSum(hydra.core.Sum<A> s) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra/core.Sum"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.FieldName("index"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((s)).index)))),
      new hydra.core.Field(new hydra.core.FieldName("size"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((s)).size)))),
      new hydra.core.Field(new hydra.core.FieldName("term"), hydra.termEncoding.TermEncoding.sigmaEncodeTerm(((s)).term)))));
  }

  static <A> hydra.core.Term<A> sigmaEncodeTerm(hydra.core.Term<A> v1) {
    return ((v1)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term<A> otherwise(hydra.core.Term<A> instance) {
        return new hydra.core.Term.Literal(new hydra.core.Literal.String_("not implemented"));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Term.Annotated<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.FieldName("annotated"), hydra.termEncoding.TermEncoding.sigmaEncodeAnnotated((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Term.Application<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.FieldName("application"), hydra.termEncoding.TermEncoding.sigmaEncodeApplication((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Term.Function<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.FieldName("function"), hydra.termEncoding.TermEncoding.sigmaEncodeFunction((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Term.Literal<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.FieldName("literal"), hydra.termEncoding.TermEncoding.sigmaEncodeLiteral((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Term.List<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.FieldName("list"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (x -> hydra.termEncoding.TermEncoding.sigmaEncodeTerm(x)),
          (instance.value))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Term.Optional<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.FieldName("optional"), new hydra.core.Term.Optional(hydra.lib.optionals.Map.apply(
          (x -> hydra.termEncoding.TermEncoding.sigmaEncodeTerm(x)),
          (instance.value))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Term.Product<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.FieldName("product"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (x -> hydra.termEncoding.TermEncoding.sigmaEncodeTerm(x)),
          (instance.value))))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Term.Record<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.FieldName("record"), hydra.termEncoding.TermEncoding.sigmaEncodeRecord((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Term.Sum<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.FieldName("sum"), hydra.termEncoding.TermEncoding.sigmaEncodeSum((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Term.Union<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.FieldName("union"), hydra.termEncoding.TermEncoding.sigmaEncodeInjection((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Term.Variable<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.FieldName("variable"), hydra.termEncoding.TermEncoding.sigmaEncodeName((instance.value)))));
      }

      @Override
      public hydra.core.Term<A> visit(hydra.core.Term.Wrap<A> instance) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra/core.Term"), new hydra.core.Field(new hydra.core.FieldName("wrap"), hydra.termEncoding.TermEncoding.sigmaEncodeNominalTerm((instance.value)))));
      }
    });
  }
}