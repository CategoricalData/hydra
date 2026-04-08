// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Functions for generating term decoders from type modules
 */
public interface Decoding {
  static java.util.List<hydra.core.Name> collectForallVariables(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Annotated at) {
        return hydra.Decoding.collectForallVariables((at).value.body);
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.lib.lists.Cons.apply(
          (ft).value.parameter,
          hydra.Decoding.collectForallVariables((ft).value.body));
      }
    });
  }

  static java.util.List<hydra.core.Name> collectOrdConstrainedVariables(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Annotated at) {
        return hydra.Decoding.collectOrdConstrainedVariables((at).value.body);
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Application appType) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Decoding.collectOrdConstrainedVariables((appType).value.function),
          hydra.Decoding.collectOrdConstrainedVariables((appType).value.argument));
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Either et) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Decoding.collectOrdConstrainedVariables((et).value.left),
          hydra.Decoding.collectOrdConstrainedVariables((et).value.right));
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.Decoding.collectOrdConstrainedVariables((ft).value.body);
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.List elemType) {
        return hydra.Decoding.collectOrdConstrainedVariables((elemType).value);
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Map mt) {
        return hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          hydra.Decoding.collectTypeVariablesFromType((mt).value.keys),
          hydra.Decoding.collectOrdConstrainedVariables((mt).value.keys),
          hydra.Decoding.collectOrdConstrainedVariables((mt).value.values)));
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Maybe elemType) {
        return hydra.Decoding.collectOrdConstrainedVariables((elemType).value);
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Decoding.collectOrdConstrainedVariables((pt).value.first),
          hydra.Decoding.collectOrdConstrainedVariables((pt).value.second));
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Record rt) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, java.util.List<hydra.core.Name>>) (ft -> hydra.Decoding.collectOrdConstrainedVariables((ft).type)),
          (rt).value));
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Set elemType) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Decoding.collectTypeVariablesFromType((elemType).value),
          hydra.Decoding.collectOrdConstrainedVariables((elemType).value));
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Union rt) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, java.util.List<hydra.core.Name>>) (ft -> hydra.Decoding.collectOrdConstrainedVariables((ft).type)),
          (rt).value));
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Wrap wt) {
        return hydra.Decoding.collectOrdConstrainedVariables((wt).value);
      }
    });
  }

  static java.util.List<hydra.core.Name> collectTypeVariables(hydra.core.Type typ) {
    return hydra.Decoding.collectForallVariables(typ);
  }

  static java.util.List<hydra.core.Name> collectTypeVariablesFromType(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Annotated at) {
        return hydra.Decoding.collectTypeVariablesFromType((at).value.body);
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Application appType) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Decoding.collectTypeVariablesFromType((appType).value.function),
          hydra.Decoding.collectTypeVariablesFromType((appType).value.argument));
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Either et) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Decoding.collectTypeVariablesFromType((et).value.left),
          hydra.Decoding.collectTypeVariablesFromType((et).value.right));
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.Decoding.collectTypeVariablesFromType((ft).value.body);
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.List elemType) {
        return hydra.Decoding.collectTypeVariablesFromType((elemType).value);
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Map mt) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Decoding.collectTypeVariablesFromType((mt).value.keys),
          hydra.Decoding.collectTypeVariablesFromType((mt).value.values));
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Maybe elemType) {
        return hydra.Decoding.collectTypeVariablesFromType((elemType).value);
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.lists.Concat2.apply(
          hydra.Decoding.collectTypeVariablesFromType((pt).value.first),
          hydra.Decoding.collectTypeVariablesFromType((pt).value.second));
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Record rt) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, java.util.List<hydra.core.Name>>) (ft -> hydra.Decoding.collectTypeVariablesFromType((ft).type)),
          (rt).value));
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Set elemType) {
        return hydra.Decoding.collectTypeVariablesFromType((elemType).value);
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Union rt) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, java.util.List<hydra.core.Name>>) (ft -> hydra.Decoding.collectTypeVariablesFromType((ft).type)),
          (rt).value));
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Variable name) {
        return java.util.Arrays.asList((name).value);
      }

      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Wrap wt) {
        return hydra.Decoding.collectTypeVariablesFromType((wt).value);
      }
    });
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding> decodeBinding(T0 cx, hydra.graph.Graph graph, hydra.core.Binding b) {
    return hydra.lib.eithers.Bind.apply(
      hydra.decode.Core.type(
        graph,
        (b).term),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding>>) (typ -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Binding>right(new hydra.core.Binding(hydra.Decoding.decodeBindingName((b).name), hydra.Decoding.decodeTypeNamed(
        (b).name,
        typ), hydra.util.Maybe.just(hydra.Decoding.decoderTypeSchemeNamed(
        (b).name,
        typ))))));
  }

  static hydra.core.Name decodeBindingName(hydra.core.Name n) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Tail.apply(hydra.lib.strings.SplitOn.apply(
        ".",
        (n).value)))),
      () -> new hydra.core.Name(hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Concat2.apply(
          java.util.Arrays.asList(
            "hydra",
            "decode"),
          hydra.lib.lists.Concat2.apply(
            hydra.lib.lists.Tail.apply(hydra.lib.lists.Init.apply(hydra.lib.strings.SplitOn.apply(
              ".",
              (n).value))),
            java.util.Arrays.asList(hydra.Formatting.decapitalize(hydra.Names.localNameOf(n))))))),
      () -> new hydra.core.Name(hydra.Formatting.decapitalize(hydra.Names.localNameOf(n))));
  }

  static hydra.core.Term decodeEitherType(hydra.core.EitherType et) {
    hydra.core.Term leftDecoder = hydra.Decoding.decodeType((et).left);
    hydra.core.Term rightDecoder = hydra.Decoding.decodeType((et).right);
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.decodeEither")), leftDecoder)), rightDecoder));
  }

  static hydra.core.Term decodeForallType(hydra.core.ForallType ft) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(hydra.Decoding.decodeBindingName((ft).parameter), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), hydra.Decoding.decodeType((ft).body))));
  }

  static hydra.core.Term decodeListType(hydra.core.Type elemType) {
    hydra.core.Term elemDecoder = hydra.Decoding.decodeType(elemType);
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.decodeList")), elemDecoder));
  }

  static hydra.core.Term decodeLiteralType(hydra.core.LiteralType lt) {
    return (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Binary ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected binary literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("binary"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("b"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("b"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Boolean_ ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected boolean literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("boolean"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("b"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("b"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Float_ ft) {
        return (ft).value.accept(new hydra.core.FloatType.PartialVisitor<>() {
          @Override
          public hydra.core.Term visit(hydra.core.FloatType.Bigfloat ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "bigfloat",
              " literal"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.FloatValue"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "bigfloat",
              " value"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("bigfloat"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("f"))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }

          @Override
          public hydra.core.Term visit(hydra.core.FloatType.Float32 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "float32",
              " literal"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.FloatValue"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "float32",
              " value"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("float32"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("f"))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }

          @Override
          public hydra.core.Term visit(hydra.core.FloatType.Float64 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "float64",
              " literal"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.FloatValue"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "float64",
              " value"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("float64"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("f"))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
        });
      }

      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Integer_ it) {
        return (it).value.accept(new hydra.core.IntegerType.PartialVisitor<>() {
          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Bigint ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "bigint",
              " literal"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "bigint",
              " value"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("bigint"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }

          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Int8 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "int8",
              " literal"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "int8",
              " value"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("int8"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }

          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Int16 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "int16",
              " literal"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "int16",
              " value"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("int16"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }

          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Int32 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "int32",
              " literal"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "int32",
              " value"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("int32"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }

          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Int64 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "int64",
              " literal"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "int64",
              " value"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("int64"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }

          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Uint8 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "uint8",
              " literal"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "uint8",
              " value"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("uint8"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }

          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Uint16 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "uint16",
              " literal"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "uint16",
              " value"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("uint16"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }

          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Uint32 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "uint32",
              " literal"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "uint32",
              " value"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("uint32"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }

          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Uint64 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "uint64",
              " literal"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "expected ",
              "uint64",
              " value"))))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("uint64"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
        });
      }

      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.String_ ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected string literal"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("string"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("s"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
      }
    });
  }

  static hydra.core.Term decodeMapType(hydra.core.MapType mt) {
    hydra.core.Term keyDecoder = hydra.Decoding.decodeType((mt).keys);
    hydra.core.Term valDecoder = hydra.Decoding.decodeType((mt).values);
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.decodeMap")), keyDecoder)), valDecoder));
  }

  static hydra.core.Term decodeMaybeType(hydra.core.Type elemType) {
    hydra.core.Term elemDecoder = hydra.Decoding.decodeType(elemType);
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.decodeMaybe")), elemDecoder));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.packaging.Module>> decodeModule(hydra.context.Context cx, hydra.graph.Graph graph, hydra.packaging.Module mod) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Decoding.filterTypeBindings(
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
            (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.core.Binding>>) (b -> hydra.lib.eithers.Bimap.apply(
              (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Decoding(_e)),
              (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (x -> x),
              hydra.Decoding.decodeBinding(
                cx,
                graph,
                b))),
            typeBindings),
          (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.packaging.Module>>>) (decodedBindings -> {
            hydra.util.Lazy<java.util.List<hydra.packaging.Namespace>> decodedTermDeps = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              hydra.Decoding::decodeNamespace,
              (mod).termDependencies));
            hydra.util.Lazy<java.util.List<hydra.packaging.Namespace>> decodedTypeDeps = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              hydra.Decoding::decodeNamespace,
              (mod).typeDependencies));
            hydra.util.Lazy<java.util.List<hydra.packaging.Namespace>> allDecodedDeps = new hydra.util.Lazy<>(() -> hydra.lib.lists.Nub.apply(hydra.lib.lists.Concat2.apply(
              decodedTypeDeps.get(),
              decodedTermDeps.get())));
            return hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.packaging.Module>>right(hydra.util.Maybe.just(new hydra.packaging.Module(hydra.Decoding.decodeNamespace((mod).namespace), hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Binding, hydra.packaging.Definition>) (b -> new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition((b).name, (b).term, (b).type))),
              decodedBindings), hydra.lib.lists.Concat2.apply(
              java.util.Arrays.asList(
                new hydra.packaging.Namespace("hydra.extract.core"),
                new hydra.packaging.Namespace("hydra.lexical"),
                new hydra.packaging.Namespace("hydra.rewriting")),
              allDecodedDeps.get()), java.util.Arrays.asList(
              (mod).namespace,
              new hydra.packaging.Namespace("hydra.util")), hydra.util.Maybe.just(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "Term decoders for ",
              (mod).namespace.value))))));
          })))));
  }

  static hydra.packaging.Namespace decodeNamespace(hydra.packaging.Namespace ns) {
    return new hydra.packaging.Namespace(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "hydra.decode.",
      hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Tail.apply(hydra.lib.strings.SplitOn.apply(
          ".",
          (ns).value))))));
  }

  static hydra.core.Term decodePairType(hydra.core.PairType pt) {
    hydra.core.Term firstDecoder = hydra.Decoding.decodeType((pt).first);
    hydra.core.Term secondDecoder = hydra.Decoding.decodeType((pt).second);
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.decodePair")), firstDecoder)), secondDecoder));
  }

  static hydra.core.Term decodeRecordType(java.util.List<hydra.core.FieldType> rt) {
    return hydra.Decoding.decodeRecordTypeImpl(
      new hydra.core.Name("unknown"),
      rt);
  }

  static hydra.core.Term decodeRecordTypeImpl(hydra.core.Name tname, java.util.List<hydra.core.FieldType> rt) {
    java.util.function.Function<hydra.core.FieldType, hydra.core.Term> decodeFieldTerm = (java.util.function.Function<hydra.core.FieldType, hydra.core.Term>) (ft -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.requireField")), new hydra.core.Term.Literal(new hydra.core.Literal.String_((ft).name.value)))), hydra.Decoding.decodeType((ft).type))), new hydra.core.Term.Variable(new hydra.core.Name("fieldMap")))), new hydra.core.Term.Variable(new hydra.core.Name("cx")))));
    java.util.function.Function<hydra.core.FieldType, hydra.core.Name> localVarName = (java.util.function.Function<hydra.core.FieldType, hydra.core.Name>) (ft -> new hydra.core.Name(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "field_",
      (ft).name.value))));
    java.util.function.Function<hydra.core.FieldType, java.util.function.Function<hydra.core.Term, hydra.core.Term>> toFieldLambda = (java.util.function.Function<hydra.core.FieldType, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (ft -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (body -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((localVarName).apply(ft), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), body)))));
    hydra.util.Lazy<hydra.core.Term> decodeBody = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.FieldType, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.FieldType, hydra.core.Term>) (ft -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.bind")), (decodeFieldTerm).apply(ft))), (toFieldLambda).apply(ft).apply(acc))))),
      new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Record(new hydra.core.Record(tname, hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.core.Field>) (ft -> new hydra.core.Field((ft).name, new hydra.core.Term.Variable((localVarName).apply(ft)))),
        rt))))),
      hydra.lib.lists.Reverse.apply(rt)));
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected record"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("record"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("record"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("fieldMap"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.toFieldMap")), new hydra.core.Term.Variable(new hydra.core.Name("record")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), decodeBody.get()))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
  }

  static hydra.core.Term decodeRecordTypeNamed(hydra.core.Name ename, java.util.List<hydra.core.FieldType> rt) {
    return hydra.Decoding.decodeRecordTypeImpl(
      ename,
      rt);
  }

  static hydra.core.Term decodeSetType(hydra.core.Type elemType) {
    hydra.core.Term elemDecoder = hydra.Decoding.decodeType(elemType);
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.decodeSet")), elemDecoder));
  }

  static hydra.core.Term decodeType(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Type instance) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("t"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("unsupported type variant"))))))))))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Annotated at) {
        return hydra.Decoding.decodeType((at).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Application appType) {
        return new hydra.core.Term.Application(new hydra.core.Application(hydra.Decoding.decodeType((appType).value.function), hydra.Decoding.decodeType((appType).value.argument)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Either et) {
        return hydra.Decoding.decodeEitherType((et).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Forall ft) {
        return hydra.Decoding.decodeForallType((ft).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.List elemType) {
        return hydra.Decoding.decodeListType((elemType).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Literal lt) {
        return hydra.Decoding.decodeLiteralType((lt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Map mt) {
        return hydra.Decoding.decodeMapType((mt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Maybe elemType) {
        return hydra.Decoding.decodeMaybeType((elemType).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Pair pt) {
        return hydra.Decoding.decodePairType((pt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Record rt) {
        return hydra.Decoding.decodeRecordType((rt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Set elemType) {
        return hydra.Decoding.decodeSetType((elemType).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Union rt) {
        return hydra.Decoding.decodeUnionType((rt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Unit ignored) {
        return hydra.Decoding.decodeUnitType();
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Void_ ignored) {
        return hydra.Decoding.decodeUnitType();
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Wrap wt) {
        return hydra.Decoding.decodeWrappedType((wt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Variable typeName) {
        return new hydra.core.Term.Variable(hydra.Decoding.decodeBindingName((typeName).value));
      }
    });
  }

  static hydra.core.Term decodeTypeNamed(hydra.core.Name ename, hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Type instance) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("t"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("unsupported type variant"))))))))))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Annotated at) {
        return hydra.Decoding.decodeTypeNamed(
          ename,
          (at).value.body);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Application appType) {
        return new hydra.core.Term.Application(new hydra.core.Application(hydra.Decoding.decodeType((appType).value.function), hydra.Decoding.decodeType((appType).value.argument)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Either et) {
        return hydra.Decoding.decodeEitherType((et).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(hydra.Decoding.decodeBindingName((ft).value.parameter), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), hydra.Decoding.decodeTypeNamed(
          ename,
          (ft).value.body))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.List elemType) {
        return hydra.Decoding.decodeListType((elemType).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Literal lt) {
        return hydra.Decoding.decodeLiteralType((lt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Map mt) {
        return hydra.Decoding.decodeMapType((mt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Maybe elemType) {
        return hydra.Decoding.decodeMaybeType((elemType).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Pair pt) {
        return hydra.Decoding.decodePairType((pt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Record rt) {
        return hydra.Decoding.decodeRecordTypeNamed(
          ename,
          (rt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Set elemType) {
        return hydra.Decoding.decodeSetType((elemType).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Union rt) {
        return hydra.Decoding.decodeUnionTypeNamed(
          ename,
          (rt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Unit ignored) {
        return hydra.Decoding.decodeUnitType();
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Void_ ignored) {
        return hydra.Decoding.decodeUnitType();
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Wrap wt) {
        return hydra.Decoding.decodeWrappedTypeNamed(
          ename,
          (wt).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Type.Variable typeName) {
        return new hydra.core.Term.Variable(hydra.Decoding.decodeBindingName((typeName).value));
      }
    });
  }

  static hydra.core.Term decodeUnionType(java.util.List<hydra.core.FieldType> rt) {
    return hydra.Decoding.decodeUnionTypeNamed(
      new hydra.core.Name("unknown"),
      rt);
  }

  static hydra.core.Term decodeUnionTypeNamed(hydra.core.Name ename, java.util.List<hydra.core.FieldType> rt) {
    java.util.function.Function<hydra.core.FieldType, hydra.core.Term> toVariantPair = (java.util.function.Function<hydra.core.FieldType, hydra.core.Term>) (ft -> new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((ft).name.value)))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("input"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.map")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("t"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection(ename, new hydra.core.Field((ft).name, new hydra.core.Term.Variable(new hydra.core.Name("t")))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(hydra.Decoding.decodeType((ft).type), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("input"))))))))))))));
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected union"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("union"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("inj"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
      new hydra.core.Binding(new hydra.core.Name("field"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.core.Injection"), new hydra.core.Name("field"))))), new hydra.core.Term.Variable(new hydra.core.Name("inj")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
      new hydra.core.Binding(new hydra.core.Name("fname"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.core.Field"), new hydra.core.Name("name"))))), new hydra.core.Term.Variable(new hydra.core.Name("field")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
      new hydra.core.Binding(new hydra.core.Name("fterm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.core.Field"), new hydra.core.Name("term"))))), new hydra.core.Term.Variable(new hydra.core.Name("field")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
      new hydra.core.Binding(new hydra.core.Name("variantMap"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.fromList")), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        toVariantPair,
        rt)))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maybes.maybe")), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.strings.cat")), new hydra.core.Term.List(java.util.Arrays.asList(
      new hydra.core.Term.Literal(new hydra.core.Literal.String_("no such field ")),
      new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.core.Name")))), new hydra.core.Term.Variable(new hydra.core.Name("fname")))),
      new hydra.core.Term.Literal(new hydra.core.Literal.String_(" in union")))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("fterm"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.lookup")), new hydra.core.Term.Variable(new hydra.core.Name("fname")))), new hydra.core.Term.Variable(new hydra.core.Name("variantMap"))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
  }

  static hydra.core.Term decodeUnitType() {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("t"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.decodeUnit")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("t"))))))))));
  }

  static hydra.core.Term decodeWrappedType(hydra.core.Type wt) {
    return hydra.Decoding.decodeWrappedTypeNamed(
      new hydra.core.Name("unknown"),
      wt);
  }

  static hydra.core.Term decodeWrappedTypeNamed(hydra.core.Name ename, hydra.core.Type wt) {
    hydra.core.Term bodyDecoder = hydra.Decoding.decodeType(wt);
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.either")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected wrapped type"))))))), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("wrap"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("wrappedTerm"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.eithers.map")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("b"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(ename, new hydra.core.Term.Variable(new hydra.core.Name("b"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(bodyDecoder, new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.core.WrappedTerm"), new hydra.core.Name("body"))))), new hydra.core.Term.Variable(new hydra.core.Name("wrappedTerm"))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.core.stripWithDecodingError")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
  }

  static hydra.core.Type decoderFullResultType(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return hydra.Decoding.decoderFullResultType((at).value.body);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Application appType) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.Decoding.decoderFullResultType((appType).value.function), (appType).value.argument));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Either et) {
        return new hydra.core.Type.Either(new hydra.core.EitherType(hydra.Decoding.decoderFullResultType((et).value.left), hydra.Decoding.decoderFullResultType((et).value.right)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.Decoding.decoderFullResultType((ft).value.body), new hydra.core.Type.Variable((ft).value.parameter)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.List elemType) {
        return new hydra.core.Type.List(hydra.Decoding.decoderFullResultType((elemType).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Literal ignored) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Literal"));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Map mt) {
        return new hydra.core.Type.Map(new hydra.core.MapType(hydra.Decoding.decoderFullResultType((mt).value.keys), hydra.Decoding.decoderFullResultType((mt).value.values)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Maybe elemType) {
        return new hydra.core.Type.Maybe(hydra.Decoding.decoderFullResultType((elemType).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Pair pt) {
        return new hydra.core.Type.Pair(new hydra.core.PairType(hydra.Decoding.decoderFullResultType((pt).value.first), hydra.Decoding.decoderFullResultType((pt).value.second)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Record ignored) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Set elemType) {
        return new hydra.core.Type.Set(hydra.Decoding.decoderFullResultType((elemType).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Union ignored) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Unit ignored) {
        return new hydra.core.Type.Unit();
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable name) {
        return new hydra.core.Type.Variable((name).value);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Void_ ignored) {
        return new hydra.core.Type.Void_();
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Wrap ignored) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"));
      }
    });
  }

  static hydra.core.Type decoderFullResultTypeNamed(hydra.core.Name ename, hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return hydra.Decoding.decoderFullResultTypeNamed(
          ename,
          (at).value.body);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.Decoding.decoderFullResultTypeNamed(
          ename,
          (ft).value.body), new hydra.core.Type.Variable((ft).value.parameter)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Record ignored) {
        return new hydra.core.Type.Variable(ename);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Union ignored) {
        return new hydra.core.Type.Variable(ename);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Wrap ignored) {
        return new hydra.core.Type.Variable(ename);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Application appType) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.Decoding.decoderFullResultType((appType).value.function), (appType).value.argument));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Either et) {
        return new hydra.core.Type.Either(new hydra.core.EitherType(hydra.Decoding.decoderFullResultType((et).value.left), hydra.Decoding.decoderFullResultType((et).value.right)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.List elemType) {
        return new hydra.core.Type.List(hydra.Decoding.decoderFullResultType((elemType).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Literal ignored) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Literal"));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Map mt) {
        return new hydra.core.Type.Map(new hydra.core.MapType(hydra.Decoding.decoderFullResultType((mt).value.keys), hydra.Decoding.decoderFullResultType((mt).value.values)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Maybe elemType) {
        return new hydra.core.Type.Maybe(hydra.Decoding.decoderFullResultType((elemType).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Pair pt) {
        return new hydra.core.Type.Pair(new hydra.core.PairType(hydra.Decoding.decoderFullResultType((pt).value.first), hydra.Decoding.decoderFullResultType((pt).value.second)));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Set elemType) {
        return new hydra.core.Type.Set(hydra.Decoding.decoderFullResultType((elemType).value));
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Unit ignored) {
        return new hydra.core.Type.Unit();
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable name) {
        return new hydra.core.Type.Variable((name).value);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Void_ ignored) {
        return new hydra.core.Type.Void_();
      }
    });
  }

  static hydra.core.Name decoderResultType(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Name otherwise(hydra.core.Type instance) {
        return new hydra.core.Name("hydra.core.Term");
      }

      @Override
      public hydra.core.Name visit(hydra.core.Type.Annotated at) {
        return hydra.Decoding.decoderResultType((at).value.body);
      }

      @Override
      public hydra.core.Name visit(hydra.core.Type.Application appType) {
        return hydra.Decoding.decoderResultType((appType).value.function);
      }

      @Override
      public hydra.core.Name visit(hydra.core.Type.Forall ft) {
        return hydra.Decoding.decoderResultType((ft).value.body);
      }

      @Override
      public hydra.core.Name visit(hydra.core.Type.Literal ignored) {
        return new hydra.core.Name("hydra.core.Literal");
      }

      @Override
      public hydra.core.Name visit(hydra.core.Type.Record ignored) {
        return new hydra.core.Name("hydra.core.Term");
      }

      @Override
      public hydra.core.Name visit(hydra.core.Type.Union ignored) {
        return new hydra.core.Name("hydra.core.Term");
      }

      @Override
      public hydra.core.Name visit(hydra.core.Type.Wrap ignored) {
        return new hydra.core.Name("hydra.core.Term");
      }
    });
  }

  static hydra.core.Type decoderType(hydra.core.Type typ) {
    hydra.core.Type resultType = hydra.Decoding.decoderFullResultType(typ);
    hydra.core.Type baseType = new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.graph.Graph")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.errors.DecodingError")), resultType))))));
    return hydra.Decoding.prependForallDecoders(
      baseType,
      typ);
  }

  static hydra.core.Type decoderTypeNamed(hydra.core.Name ename, hydra.core.Type typ) {
    hydra.core.Type resultType = hydra.Decoding.decoderFullResultTypeNamed(
      ename,
      typ);
    hydra.core.Type baseType = new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.graph.Graph")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.errors.DecodingError")), resultType))))));
    return hydra.Decoding.prependForallDecoders(
      baseType,
      typ);
  }

  static hydra.core.TypeScheme decoderTypeScheme(hydra.core.Type typ) {
    java.util.List<hydra.core.Name> allOrdVars = hydra.Decoding.collectOrdConstrainedVariables(typ);
    java.util.List<hydra.core.Name> typeVars = hydra.Decoding.collectTypeVariables(typ);
    hydra.util.Lazy<java.util.List<hydra.core.Name>> ordVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.lists.Elem.apply(
        v,
        typeVars)),
      allOrdVars));
    hydra.util.Lazy<hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(ordVars.get()),
      () -> (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()),
      () -> hydra.util.Maybe.just(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (v -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>(v, new hydra.core.TypeVariableMetadata(hydra.lib.sets.Singleton.apply(new hydra.core.Name("ordering"))))))),
        ordVars.get())))));
    return new hydra.core.TypeScheme(typeVars, hydra.Decoding.decoderType(typ), constraints.get());
  }

  static hydra.core.TypeScheme decoderTypeSchemeNamed(hydra.core.Name ename, hydra.core.Type typ) {
    java.util.List<hydra.core.Name> allOrdVars = hydra.Decoding.collectOrdConstrainedVariables(typ);
    java.util.List<hydra.core.Name> typeVars = hydra.Decoding.collectTypeVariables(typ);
    hydra.util.Lazy<java.util.List<hydra.core.Name>> ordVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.lists.Elem.apply(
        v,
        typeVars)),
      allOrdVars));
    hydra.util.Lazy<hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(ordVars.get()),
      () -> (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()),
      () -> hydra.util.Maybe.just(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (v -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>(v, new hydra.core.TypeVariableMetadata(hydra.lib.sets.Singleton.apply(new hydra.core.Name("ordering"))))))),
        ordVars.get())))));
    return new hydra.core.TypeScheme(typeVars, hydra.Decoding.decoderTypeNamed(
      ename,
      typ), constraints.get());
  }

  static hydra.util.Either<hydra.errors.Error_, java.util.List<hydra.core.Binding>> filterTypeBindings(hydra.context.Context cx, hydra.graph.Graph graph, java.util.List<hydra.core.Binding> bindings) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.core.Binding>>, java.util.List<hydra.core.Binding>>) (hydra.lib.maybes.Cat::apply),
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Binding>>>) (v1 -> hydra.Decoding.isDecodableBinding(
          cx,
          graph,
          v1)),
        hydra.lib.lists.Filter.apply(
          hydra.Annotations::isNativeType,
          bindings)));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Binding>> isDecodableBinding(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Binding b) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Predicates.isSerializableByName(
        cx,
        graph,
        (b).name),
      (java.util.function.Function<Boolean, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Binding>>>) (serializable -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.core.Binding>>right(hydra.lib.logic.IfElse.lazy(
        serializable,
        () -> hydra.util.Maybe.just(b),
        () -> (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing())))));
  }

  static hydra.core.Type prependForallDecoders(hydra.core.Type baseType, hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return baseType;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return hydra.Decoding.prependForallDecoders(
          baseType,
          (at).value.body);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.graph.Graph")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.errors.DecodingError")), new hydra.core.Type.Variable((ft).value.parameter))))))), hydra.Decoding.prependForallDecoders(
          baseType,
          (ft).value.body)));
      }
    });
  }
}
