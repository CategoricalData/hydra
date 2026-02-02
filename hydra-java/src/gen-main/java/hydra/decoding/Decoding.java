// Note: this is an automatically generated file. Do not edit.

package hydra.decoding;

/**
 * Functions for generating term decoders from type modules
 */
public interface Decoding {
  static java.util.List<hydra.core.Name> collectForallVariables(hydra.core.Type typ) {
    return ((typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of());
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Annotated at) {
        return hydra.decoding.Decoding.collectForallVariables((((at)).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.lib.lists.Cons.apply(
          (((ft)).value).parameter,
          hydra.decoding.Decoding.collectForallVariables((((ft)).value).body));
      }
    });
  }
  
  static java.util.List<hydra.core.Name> collectOrdConstrainedVariables(hydra.core.Type typ) {
    return ((typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of());
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Annotated at) {
        return hydra.decoding.Decoding.collectOrdConstrainedVariables((((at)).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Application appType) {
        return hydra.lib.lists.Concat2.apply(
          hydra.decoding.Decoding.collectOrdConstrainedVariables((((appType)).value).function),
          hydra.decoding.Decoding.collectOrdConstrainedVariables((((appType)).value).argument));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Either et) {
        return hydra.lib.lists.Concat2.apply(
          hydra.decoding.Decoding.collectOrdConstrainedVariables((((et)).value).left),
          hydra.decoding.Decoding.collectOrdConstrainedVariables((((et)).value).right));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.decoding.Decoding.collectOrdConstrainedVariables((((ft)).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.List elemType) {
        return hydra.decoding.Decoding.collectOrdConstrainedVariables(((elemType)).value);
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Map mt) {
        return hydra.lib.lists.Concat2.apply(
          hydra.decoding.Decoding.collectOrdConstrainedVariables((((mt)).value).keys),
          hydra.decoding.Decoding.collectOrdConstrainedVariables((((mt)).value).values));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Maybe elemType) {
        return hydra.decoding.Decoding.collectOrdConstrainedVariables(((elemType)).value);
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.lists.Concat2.apply(
          hydra.decoding.Decoding.collectOrdConstrainedVariables((((pt)).value).first),
          hydra.decoding.Decoding.collectOrdConstrainedVariables((((pt)).value).second));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Record rt) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, java.util.List<hydra.core.Name>>) (ft -> hydra.decoding.Decoding.collectOrdConstrainedVariables(((ft)).type)),
          (((rt)).value).fields));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Set elemType) {
        return hydra.lib.lists.Concat2.apply(
          hydra.decoding.Decoding.collectTypeVariablesFromType(((elemType)).value),
          hydra.decoding.Decoding.collectOrdConstrainedVariables(((elemType)).value));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Union rt) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, java.util.List<hydra.core.Name>>) (ft -> hydra.decoding.Decoding.collectOrdConstrainedVariables(((ft)).type)),
          (((rt)).value).fields));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Wrap wt) {
        return hydra.decoding.Decoding.collectOrdConstrainedVariables((((wt)).value).body);
      }
    });
  }
  
  static java.util.List<hydra.core.Name> collectTypeVariables(hydra.core.Type typ) {
    return hydra.decoding.Decoding.collectForallVariables((typ));
  }
  
  static java.util.List<hydra.core.Name> collectTypeVariablesFromType(hydra.core.Type typ) {
    return ((typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of());
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Annotated at) {
        return hydra.decoding.Decoding.collectTypeVariablesFromType((((at)).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Application appType) {
        return hydra.lib.lists.Concat2.apply(
          hydra.decoding.Decoding.collectTypeVariablesFromType((((appType)).value).function),
          hydra.decoding.Decoding.collectTypeVariablesFromType((((appType)).value).argument));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Either et) {
        return hydra.lib.lists.Concat2.apply(
          hydra.decoding.Decoding.collectTypeVariablesFromType((((et)).value).left),
          hydra.decoding.Decoding.collectTypeVariablesFromType((((et)).value).right));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.decoding.Decoding.collectTypeVariablesFromType((((ft)).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.List elemType) {
        return hydra.decoding.Decoding.collectTypeVariablesFromType(((elemType)).value);
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Map mt) {
        return hydra.lib.lists.Concat2.apply(
          hydra.decoding.Decoding.collectTypeVariablesFromType((((mt)).value).keys),
          hydra.decoding.Decoding.collectTypeVariablesFromType((((mt)).value).values));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Maybe elemType) {
        return hydra.decoding.Decoding.collectTypeVariablesFromType(((elemType)).value);
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.lists.Concat2.apply(
          hydra.decoding.Decoding.collectTypeVariablesFromType((((pt)).value).first),
          hydra.decoding.Decoding.collectTypeVariablesFromType((((pt)).value).second));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Record rt) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, java.util.List<hydra.core.Name>>) (ft -> hydra.decoding.Decoding.collectTypeVariablesFromType(((ft)).type)),
          (((rt)).value).fields));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Set elemType) {
        return hydra.decoding.Decoding.collectTypeVariablesFromType(((elemType)).value);
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Union rt) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.FieldType, java.util.List<hydra.core.Name>>) (ft -> hydra.decoding.Decoding.collectTypeVariablesFromType(((ft)).type)),
          (((rt)).value).fields));
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Variable name) {
        return java.util.List.of(((name)).value);
      }
      
      @Override
      public java.util.List<hydra.core.Name> visit(hydra.core.Type.Wrap wt) {
        return hydra.decoding.Decoding.collectTypeVariablesFromType((((wt)).value).body);
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Binding> decodeBinding(hydra.core.Binding b) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Binding>>) (cx -> hydra.lib.flows.Bind.apply(
        hydra.monads.Monads.eitherToFlow(
          wrapped -> ((wrapped)).value,
          hydra.decode.core.Core.type(
            (cx),
            ((b)).term)),
        (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Binding>>) (typ -> hydra.lib.flows.Pure.apply(new hydra.core.Binding(hydra.decoding.Decoding.decodeBindingName(((b)).name), hydra.decoding.Decoding.decodeType((typ)), hydra.util.Maybe.just(hydra.decoding.Decoding.decoderTypeScheme((typ)))))))));
  }
  
  static hydra.core.Name decodeBindingName(hydra.core.Name n) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(hydra.lib.lists.Tail.apply(hydra.lib.strings.SplitOn.apply(
        ".",
        ((n)).value)))),
      new hydra.core.Name(hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Concat2.apply(
          java.util.List.of(
            "hydra",
            "decode"),
          hydra.lib.lists.Concat2.apply(
            hydra.lib.lists.Tail.apply(hydra.lib.lists.Init.apply(hydra.lib.strings.SplitOn.apply(
              ".",
              ((n)).value))),
            java.util.List.of(hydra.formatting.Formatting.decapitalize(hydra.names.Names.localNameOf((n)))))))),
      new hydra.core.Name(hydra.formatting.Formatting.decapitalize(hydra.names.Names.localNameOf((n)))));
  }
  
  static hydra.core.Term decodeEitherType(hydra.core.EitherType et) {
    hydra.core.Term leftDecoder = hydra.decoding.Decoding.decodeType(((et)).left);
    hydra.core.Term rightDecoder = hydra.decoding.Decoding.decodeType(((et)).right);
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.helpers.decodeEither")), (leftDecoder))), (rightDecoder)));
  }
  
  static hydra.core.Term decodeForallType(hydra.core.ForallType ft) {
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(hydra.decoding.Decoding.decodeBindingName(((ft)).parameter), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), hydra.decoding.Decoding.decodeType(((ft)).body))));
  }
  
  static hydra.core.Term decodeListType(hydra.core.Type elemType) {
    hydra.core.Term elemDecoder = hydra.decoding.Decoding.decodeType((elemType));
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.helpers.decodeList")), (elemDecoder)));
  }
  
  static hydra.core.Term decodeLiteralType(hydra.core.LiteralType lt) {
    return ((lt)).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Binary ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected binary literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("binary"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("b"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("b"))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Boolean_ ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected boolean literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("boolean"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("b"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("b"))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Float_ ft) {
        return (((ft)).value).accept(new hydra.core.FloatType.PartialVisitor<>() {
          @Override
          public hydra.core.Term visit(hydra.core.FloatType.Bigfloat ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "bigfloat",
              " literal"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.FloatValue"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "bigfloat",
              " value"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("bigfloat"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("f"))))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.FloatType.Float32 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "float32",
              " literal"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.FloatValue"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "float32",
              " value"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("float32"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("f"))))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.FloatType.Float64 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "float64",
              " literal"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("float"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.FloatValue"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "float64",
              " value"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("float64"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("f"))))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
        });
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.Integer_ it) {
        return (((it)).value).accept(new hydra.core.IntegerType.PartialVisitor<>() {
          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Bigint ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "bigint",
              " literal"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "bigint",
              " value"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("bigint"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Int8 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "int8",
              " literal"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "int8",
              " value"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("int8"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Int16 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "int16",
              " literal"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "int16",
              " value"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("int16"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Int32 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "int32",
              " literal"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "int32",
              " value"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("int32"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Int64 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "int64",
              " literal"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "int64",
              " value"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("int64"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Uint8 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "uint8",
              " literal"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "uint8",
              " value"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("uint8"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Uint16 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "uint16",
              " literal"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "uint16",
              " value"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("uint16"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Uint32 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "uint32",
              " literal"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "uint32",
              " value"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("uint32"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
          
          @Override
          public hydra.core.Term visit(hydra.core.IntegerType.Uint64 ignored) {
            return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "uint64",
              " literal"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("integer"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.IntegerValue"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
              "expected ",
              "uint64",
              " value"))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("uint64"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("i"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("i"))))))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
          }
        });
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.LiteralType.String_ ignored) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("literal"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Literal"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("expected string literal"))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("string"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("s"))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
      }
    });
  }
  
  static hydra.core.Term decodeMapType(hydra.core.MapType mt) {
    hydra.core.Term keyDecoder = hydra.decoding.Decoding.decodeType(((mt)).keys);
    hydra.core.Term valDecoder = hydra.decoding.Decoding.decodeType(((mt)).values);
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.helpers.decodeMap")), (keyDecoder))), (valDecoder)));
  }
  
  static hydra.core.Term decodeMaybeType(hydra.core.Type elemType) {
    hydra.core.Term elemDecoder = hydra.decoding.Decoding.decodeType((elemType));
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.helpers.decodeMaybe")), (elemDecoder)));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.module.Module>> decodeModule(hydra.module.Module mod) {
    return hydra.lib.flows.Bind.apply(
      hydra.decoding.Decoding.filterTypeBindings(((mod)).elements),
      (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.module.Module>>>) (typeBindings -> hydra.lib.logic.IfElse.apply(
        hydra.lib.lists.Null.apply((typeBindings)),
        hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.module.Module>) (hydra.util.Maybe.<hydra.module.Module>nothing())),
        hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (hydra.decoding.Decoding::decodeBinding),
            (typeBindings)),
          (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.module.Module>>>) (decodedBindings -> {
            java.util.List<hydra.module.Namespace> decodedTermDeps = hydra.lib.lists.Map.apply(
              (hydra.decoding.Decoding::decodeNamespace),
              ((mod)).termDependencies);
            java.util.List<hydra.module.Namespace> decodedTypeDeps = hydra.lib.lists.Map.apply(
              (hydra.decoding.Decoding::decodeNamespace),
              ((mod)).typeDependencies);
            java.util.List<hydra.module.Namespace> allDecodedDeps = hydra.lib.lists.Nub.apply(hydra.lib.lists.Concat2.apply(
              (decodedTypeDeps),
              (decodedTermDeps)));
            return hydra.lib.flows.Pure.apply(hydra.util.Maybe.just(new hydra.module.Module(hydra.decoding.Decoding.decodeNamespace(((mod)).namespace), (decodedBindings), hydra.lib.lists.Concat2.apply(
              java.util.List.of(
                new hydra.module.Namespace("hydra.extract.helpers"),
                new hydra.module.Namespace("hydra.lexical"),
                new hydra.module.Namespace("hydra.rewriting")),
              (allDecodedDeps)), java.util.List.of(
              ((mod)).namespace,
              new hydra.module.Namespace("hydra.util")), hydra.util.Maybe.just(hydra.lib.strings.Cat.apply(java.util.List.of(
              "Term decoders for ",
              (((mod)).namespace).value))))));
          })))));
  }
  
  static hydra.module.Namespace decodeNamespace(hydra.module.Namespace ns) {
    return new hydra.module.Namespace(hydra.lib.strings.Cat.apply(java.util.List.of(
      "hydra.decode.",
      hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Tail.apply(hydra.lib.strings.SplitOn.apply(
          ".",
          ((ns)).value))))));
  }
  
  static hydra.core.Term decodePairType(hydra.core.PairType pt) {
    hydra.core.Term firstDecoder = hydra.decoding.Decoding.decodeType(((pt)).first);
    hydra.core.Term secondDecoder = hydra.decoding.Decoding.decodeType(((pt)).second);
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.helpers.decodePair")), (firstDecoder))), (secondDecoder)));
  }
  
  static hydra.core.Term decodeRecordType(hydra.core.RowType rt) {
    java.util.function.Function<hydra.core.FieldType, hydra.core.Term> decodeFieldTerm = (java.util.function.Function<hydra.core.FieldType, hydra.core.Term>) (ft -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.helpers.requireField")), new hydra.core.Term.Literal(new hydra.core.Literal.String_((((ft)).name).value)))), hydra.decoding.Decoding.decodeType(((ft)).type))), new hydra.core.Term.Variable(new hydra.core.Name("fieldMap")))), new hydra.core.Term.Variable(new hydra.core.Name("cx")))));
    java.util.List<hydra.core.FieldType> fieldTypes = ((rt)).fields;
    java.util.function.Function<hydra.core.FieldType, hydra.core.Name> localVarName = (java.util.function.Function<hydra.core.FieldType, hydra.core.Name>) (ft -> new hydra.core.Name(hydra.lib.strings.Cat.apply(java.util.List.of(
      "field_",
      (((ft)).name).value))));
    java.util.function.Function<hydra.core.FieldType, java.util.function.Function<hydra.core.Term, hydra.core.Term>> toFieldLambda = (java.util.function.Function<hydra.core.FieldType, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (ft -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (body -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(((localVarName)).apply((ft)), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), (body))))));
    hydra.core.Name typeName = ((rt)).typeName;
    hydra.core.Term decodeBody = hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.FieldType, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.FieldType, hydra.core.Term>) (ft -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.bind"))), ((decodeFieldTerm)).apply((ft)))), (((toFieldLambda)).apply((ft))).apply((acc)))))),
      new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Record(new hydra.core.Record((typeName), hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.core.Field>) (ft -> new hydra.core.Field(((ft)).name, new hydra.core.Term.Variable(((localVarName)).apply((ft))))),
        (fieldTypes)))))))),
      hydra.lib.lists.Reverse.apply((fieldTypes)));
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
      "expected record of type ",
      ((typeName)).value))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("record"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("record"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Let(new hydra.core.Let(java.util.List.of(new hydra.core.Binding(new hydra.core.Name("fieldMap"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.helpers.toFieldMap")), new hydra.core.Term.Variable(new hydra.core.Name("record")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), (decodeBody)))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
  }
  
  static hydra.core.Term decodeSetType(hydra.core.Type elemType) {
    hydra.core.Term elemDecoder = hydra.decoding.Decoding.decodeType((elemType));
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.helpers.decodeSet")), (elemDecoder)));
  }
  
  static hydra.core.Term decodeType(hydra.core.Type typ) {
    return ((typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Type instance) {
        return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("t"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("unsupported type variant"))))))))))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Annotated at) {
        return hydra.decoding.Decoding.decodeType((((at)).value).body);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Application appType) {
        return new hydra.core.Term.Application(new hydra.core.Application(hydra.decoding.Decoding.decodeType((((appType)).value).function), hydra.decoding.Decoding.decodeType((((appType)).value).argument)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Either et) {
        return hydra.decoding.Decoding.decodeEitherType(((et)).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Forall ft) {
        return hydra.decoding.Decoding.decodeForallType(((ft)).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.List elemType) {
        return hydra.decoding.Decoding.decodeListType(((elemType)).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Literal lt) {
        return hydra.decoding.Decoding.decodeLiteralType(((lt)).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Map mt) {
        return hydra.decoding.Decoding.decodeMapType(((mt)).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Maybe elemType) {
        return hydra.decoding.Decoding.decodeMaybeType(((elemType)).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Pair pt) {
        return hydra.decoding.Decoding.decodePairType(((pt)).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Record rt) {
        return hydra.decoding.Decoding.decodeRecordType(((rt)).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Set elemType) {
        return hydra.decoding.Decoding.decodeSetType(((elemType)).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Union rt) {
        return hydra.decoding.Decoding.decodeUnionType(((rt)).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Unit ignored) {
        return (hydra.decoding.Decoding.decodeUnitType);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Wrap wt) {
        return hydra.decoding.Decoding.decodeWrappedType(((wt)).value);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Type.Variable typeName) {
        return new hydra.core.Term.Variable(hydra.decoding.Decoding.decodeBindingName(((typeName)).value));
      }
    });
  }
  
  hydra.core.Term decodeUnitType = new hydra.core.Term.Variable(new hydra.core.Name("hydra.extract.helpers.decodeUnit"));
  
  static hydra.core.Term decodeUnionType(hydra.core.RowType rt) {
    hydra.core.Name typeName = ((rt)).typeName;
    java.util.function.Function<hydra.core.FieldType, hydra.core.Term> toVariantPair = (java.util.function.Function<hydra.core.FieldType, hydra.core.Term>) (ft -> new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((((ft)).name).value)))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("input"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.map"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("t"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Union(new hydra.core.Injection((typeName), new hydra.core.Field(((ft)).name, new hydra.core.Term.Variable(new hydra.core.Name("t")))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(hydra.decoding.Decoding.decodeType(((ft)).type), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("input"))))))))))))));
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
      "expected union of type ",
      ((typeName)).value))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("union"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("inj"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Let(new hydra.core.Let(java.util.List.of(
      new hydra.core.Binding(new hydra.core.Name("tname"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.core.Injection"), new hydra.core.Name("typeName"))))), new hydra.core.Term.Variable(new hydra.core.Name("inj")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
      new hydra.core.Binding(new hydra.core.Name("field"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.core.Injection"), new hydra.core.Name("field"))))), new hydra.core.Term.Variable(new hydra.core.Name("inj")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
      new hydra.core.Binding(new hydra.core.Name("fname"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.core.Field"), new hydra.core.Name("name"))))), new hydra.core.Term.Variable(new hydra.core.Name("field")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
      new hydra.core.Binding(new hydra.core.Name("fterm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.core.Field"), new hydra.core.Name("term"))))), new hydra.core.Term.Variable(new hydra.core.Name("field")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
      new hydra.core.Binding(new hydra.core.Name("variantMap"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maps.fromList"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (toVariantPair),
        ((rt)).fields)))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.maybe"))), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.cat"))), new hydra.core.Term.List(java.util.List.of(
      new hydra.core.Term.Literal(new hydra.core.Literal.String_("no such field ")),
      new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.core.Name")))), new hydra.core.Term.Variable(new hydra.core.Name("fname")))),
      new hydra.core.Term.Literal(new hydra.core.Literal.String_(" in union type ")),
      new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.core.Name")))), new hydra.core.Term.Variable(new hydra.core.Name("tname")))))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("fterm"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maps.lookup"))), new hydra.core.Term.Variable(new hydra.core.Name("fname")))), new hydra.core.Term.Variable(new hydra.core.Name("variantMap"))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
  }
  
  static hydra.core.Term decodeWrappedType(hydra.core.WrappedType wt) {
    hydra.core.Term bodyDecoder = hydra.decoding.Decoding.decodeType(((wt)).body);
    return new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("cx"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("raw"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("stripped"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(new hydra.core.Name("hydra.core.Term"), hydra.util.Maybe.just(new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.util.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
      "expected wrapped type ",
      (((wt)).typeName).value))))))))))), java.util.List.of(new hydra.core.Field(new hydra.core.Name("wrap"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("wrappedTerm"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.map"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("b"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(((wt)).typeName, new hydra.core.Term.Variable(new hydra.core.Name("b"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application((bodyDecoder), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.core.WrappedTerm"), new hydra.core.Name("body"))))), new hydra.core.Term.Variable(new hydra.core.Name("wrappedTerm"))))))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("stripped"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lexical.stripAndDereferenceTermEither")), new hydra.core.Term.Variable(new hydra.core.Name("cx")))), new hydra.core.Term.Variable(new hydra.core.Name("raw"))))))))))));
  }
  
  static hydra.core.Type decoderFullResultType(hydra.core.Type typ) {
    return ((typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return hydra.decoding.Decoding.decoderFullResultType((((at)).value).body);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Application appType) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.decoding.Decoding.decoderFullResultType((((appType)).value).function), (((appType)).value).argument));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Either et) {
        return new hydra.core.Type.Either(new hydra.core.EitherType(hydra.decoding.Decoding.decoderFullResultType((((et)).value).left), hydra.decoding.Decoding.decoderFullResultType((((et)).value).right)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Type.Application(new hydra.core.ApplicationType(hydra.decoding.Decoding.decoderFullResultType((((ft)).value).body), new hydra.core.Type.Variable((((ft)).value).parameter)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.List elemType) {
        return new hydra.core.Type.List(hydra.decoding.Decoding.decoderFullResultType(((elemType)).value));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Literal ignored) {
        return new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Literal"));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Map mt) {
        return new hydra.core.Type.Map(new hydra.core.MapType(hydra.decoding.Decoding.decoderFullResultType((((mt)).value).keys), hydra.decoding.Decoding.decoderFullResultType((((mt)).value).values)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Maybe elemType) {
        return new hydra.core.Type.Maybe(hydra.decoding.Decoding.decoderFullResultType(((elemType)).value));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Pair pt) {
        return new hydra.core.Type.Pair(new hydra.core.PairType(hydra.decoding.Decoding.decoderFullResultType((((pt)).value).first), hydra.decoding.Decoding.decoderFullResultType((((pt)).value).second)));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Record rt) {
        return new hydra.core.Type.Variable((((rt)).value).typeName);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Set elemType) {
        return new hydra.core.Type.Set(hydra.decoding.Decoding.decoderFullResultType(((elemType)).value));
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Union rt) {
        return new hydra.core.Type.Variable((((rt)).value).typeName);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Unit ignored) {
        return new hydra.core.Type.Unit(true);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Variable name) {
        return new hydra.core.Type.Variable(((name)).value);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Wrap wt) {
        return new hydra.core.Type.Variable((((wt)).value).typeName);
      }
    });
  }
  
  static hydra.core.Name decoderResultType(hydra.core.Type typ) {
    return ((typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Name otherwise(hydra.core.Type instance) {
        return new hydra.core.Name("hydra.core.Term");
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.Type.Annotated at) {
        return hydra.decoding.Decoding.decoderResultType((((at)).value).body);
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.Type.Application appType) {
        return hydra.decoding.Decoding.decoderResultType((((appType)).value).function);
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.Type.Forall ft) {
        return hydra.decoding.Decoding.decoderResultType((((ft)).value).body);
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.Type.Literal ignored) {
        return new hydra.core.Name("hydra.core.Literal");
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.Type.Record rt) {
        return (((rt)).value).typeName;
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.Type.Union rt) {
        return (((rt)).value).typeName;
      }
      
      @Override
      public hydra.core.Name visit(hydra.core.Type.Wrap wt) {
        return (((wt)).value).typeName;
      }
    });
  }
  
  static hydra.core.Type decoderType(hydra.core.Type typ) {
    hydra.core.Type resultType = hydra.decoding.Decoding.decoderFullResultType((typ));
    hydra.core.Type baseType = new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.graph.Graph")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.util.DecodingError")), (resultType)))))));
    return hydra.decoding.Decoding.prependForallDecoders(
      (baseType),
      (typ));
  }
  
  static hydra.core.TypeScheme decoderTypeScheme(hydra.core.Type typ) {
    java.util.List<hydra.core.Name> allOrdVars = hydra.decoding.Decoding.collectOrdConstrainedVariables((typ));
    java.util.List<hydra.core.Name> typeVars = hydra.decoding.Decoding.collectTypeVariables((typ));
    java.util.List<hydra.core.Name> ordVars = hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.lists.Elem.apply(
        (v),
        (typeVars))),
      (allOrdVars));
    hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints = hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((ordVars)),
      (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()),
      hydra.util.Maybe.just(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (v -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>((v), new hydra.core.TypeVariableMetadata(hydra.lib.sets.Singleton.apply(new hydra.core.Name("ordering"))))))),
        (ordVars)))));
    return new hydra.core.TypeScheme((typeVars), hydra.decoding.Decoding.decoderType((typ)), (constraints));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, java.util.List<hydra.core.Binding>> filterTypeBindings(java.util.List<hydra.core.Binding> bindings) {
    return hydra.lib.flows.Map.apply(
      (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.core.Binding>>, java.util.List<hydra.core.Binding>>) ((hydra.lib.maybes.Cat::apply)),
      hydra.lib.flows.MapList.apply(
        (hydra.decoding.Decoding::isDecodableBinding),
        hydra.lib.lists.Filter.apply(
          (hydra.annotations.Annotations::isNativeType),
          (bindings))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Maybe<hydra.core.Binding>> isDecodableBinding(hydra.core.Binding b) {
    return hydra.lib.flows.Map.apply(
      (java.util.function.Function<Boolean, hydra.util.Maybe<hydra.core.Binding>>) (serializable -> hydra.lib.logic.IfElse.apply(
        (serializable),
        hydra.util.Maybe.just((b)),
        (hydra.util.Maybe<hydra.core.Binding>) (hydra.util.Maybe.<hydra.core.Binding>nothing()))),
      hydra.schemas.Schemas.isSerializableByName(((b)).name));
  }
  
  static hydra.core.Type prependForallDecoders(hydra.core.Type baseType, hydra.core.Type typ) {
    return ((typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (baseType);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return hydra.decoding.Decoding.prependForallDecoders(
          (baseType),
          (((at)).value).body);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.graph.Graph")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.util.DecodingError")), new hydra.core.Type.Variable((((ft)).value).parameter))))))), hydra.decoding.Decoding.prependForallDecoders(
          (baseType),
          (((ft)).value).body)));
      }
    });
  }
}
