// Note: this is an automatically generated file. Do not edit.

package hydra.decode.grammar;

/**
 * Term decoders for hydra.grammar
 */
public interface Grammar {
  static hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Constant> constant(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Constant>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Constant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Constant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Constant>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Constant>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Constant> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Constant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Constant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Constant>left(new hydra.util.DecodingError("expected wrapped type hydra.grammar.Constant"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Constant> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.grammar.Constant>) (b -> new hydra.grammar.Constant(b)),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError(err))))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                  return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                }
                
                @Override
                public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                  return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right((s).value)));
                    }
                  });
                }
              })),
              hydra.lexical.Lexical.stripAndDereferenceTermEither(
                cx,
                ((wrappedTerm).value).body)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Grammar> grammar(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Grammar>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Grammar>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Grammar>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Grammar>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Grammar>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Grammar> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Grammar>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Grammar>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Grammar>left(new hydra.util.DecodingError("expected wrapped type hydra.grammar.Grammar"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Grammar> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<hydra.grammar.Production>, hydra.grammar.Grammar>) (b -> new hydra.grammar.Grammar(b)),
            hydra.extract.helpers.Helpers.decodeList(
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production>>>) (p0 -> p1 -> hydra.decode.grammar.Grammar.production(
                p0,
                p1)),
              cx,
              ((wrappedTerm).value).body));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Label> label(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Label>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Label>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Label>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Label>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Label>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Label> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Label>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Label>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Label>left(new hydra.util.DecodingError("expected wrapped type hydra.grammar.Label"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Label> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.grammar.Label>) (b -> new hydra.grammar.Label(b)),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError(err))))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                  return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                }
                
                @Override
                public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                  return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right((s).value)));
                    }
                  });
                }
              })),
              hydra.lexical.Lexical.stripAndDereferenceTermEither(
                cx,
                ((wrappedTerm).value).body)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.grammar.LabeledPattern> labeledPattern(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.LabeledPattern>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.LabeledPattern>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.LabeledPattern>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.LabeledPattern>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.LabeledPattern>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.LabeledPattern> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.LabeledPattern>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.LabeledPattern>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.LabeledPattern>left(new hydra.util.DecodingError("expected record of type hydra.grammar.LabeledPattern"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.LabeledPattern> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "label",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Label>>>) (p0 -> p1 -> hydra.decode.grammar.Grammar.label(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.grammar.Label, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.LabeledPattern>>) (field_label -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "pattern",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (p0 -> p1 -> hydra.decode.grammar.Grammar.pattern(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.grammar.Pattern, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.LabeledPattern>>) (field_pattern -> (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.LabeledPattern>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.LabeledPattern>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.LabeledPattern>right(new hydra.grammar.LabeledPattern(field_label, field_pattern))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern> pattern(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Pattern>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Pattern>left(new hydra.util.DecodingError("expected union of type hydra.grammar.Pattern"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = ((inj).value).field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.core.Name tname = ((inj).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>(new hydra.core.Name("alternatives"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.grammar.Pattern>, hydra.grammar.Pattern>) (t -> new hydra.grammar.Pattern.Alternatives(t)),
              hydra.extract.helpers.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (p0 -> p1 -> hydra.decode.grammar.Grammar.pattern(
                  p0,
                  p1)),
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>(new hydra.core.Name("constant"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.grammar.Constant, hydra.grammar.Pattern>) (t -> new hydra.grammar.Pattern.Constant(t)),
              hydra.decode.grammar.Grammar.constant(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>(new hydra.core.Name("ignored"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.grammar.Pattern, hydra.grammar.Pattern>) (t -> new hydra.grammar.Pattern.Ignored(t)),
              hydra.decode.grammar.Grammar.pattern(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>(new hydra.core.Name("labeled"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.grammar.LabeledPattern, hydra.grammar.Pattern>) (t -> new hydra.grammar.Pattern.Labeled(t)),
              hydra.decode.grammar.Grammar.labeledPattern(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>(new hydra.core.Name("nil"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.grammar.Pattern>) (t -> new hydra.grammar.Pattern.Nil()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>(new hydra.core.Name("nonterminal"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.grammar.Symbol, hydra.grammar.Pattern>) (t -> new hydra.grammar.Pattern.Nonterminal(t)),
              hydra.decode.grammar.Grammar.symbol(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>(new hydra.core.Name("option"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.grammar.Pattern, hydra.grammar.Pattern>) (t -> new hydra.grammar.Pattern.Option(t)),
              hydra.decode.grammar.Grammar.pattern(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>(new hydra.core.Name("plus"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.grammar.Pattern, hydra.grammar.Pattern>) (t -> new hydra.grammar.Pattern.Plus(t)),
              hydra.decode.grammar.Grammar.pattern(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>(new hydra.core.Name("regex"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.grammar.Regex, hydra.grammar.Pattern>) (t -> new hydra.grammar.Pattern.Regex(t)),
              hydra.decode.grammar.Grammar.regex(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>(new hydra.core.Name("sequence"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.grammar.Pattern>, hydra.grammar.Pattern>) (t -> new hydra.grammar.Pattern.Sequence(t)),
              hydra.extract.helpers.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (p0 -> p1 -> hydra.decode.grammar.Grammar.pattern(
                  p0,
                  p1)),
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>(new hydra.core.Name("star"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.grammar.Pattern, hydra.grammar.Pattern>) (t -> new hydra.grammar.Pattern.Star(t)),
              hydra.decode.grammar.Grammar.pattern(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Pattern>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              (fname).value,
              " in union type ",
              (tname).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production> production(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Production>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Production>left(new hydra.util.DecodingError("expected record of type hydra.grammar.Production"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "symbol",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Symbol>>>) (p0 -> p1 -> hydra.decode.grammar.Grammar.symbol(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.grammar.Symbol, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production>>) (field_symbol -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "pattern",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Pattern>>>) (p0 -> p1 -> hydra.decode.grammar.Grammar.pattern(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.grammar.Pattern, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production>>) (field_pattern -> (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Production>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Production>right(new hydra.grammar.Production(field_symbol, field_pattern))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Regex> regex(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Regex>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Regex>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Regex>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Regex>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Regex>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Regex> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Regex>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Regex>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Regex>left(new hydra.util.DecodingError("expected wrapped type hydra.grammar.Regex"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Regex> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.grammar.Regex>) (b -> new hydra.grammar.Regex(b)),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError(err))))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                  return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                }
                
                @Override
                public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                  return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right((s).value)));
                    }
                  });
                }
              })),
              hydra.lexical.Lexical.stripAndDereferenceTermEither(
                cx,
                ((wrappedTerm).value).body)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Symbol> symbol(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Symbol>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Symbol>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Symbol>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Symbol>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Symbol>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Symbol> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Symbol>) ((hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Symbol>) (hydra.util.Either.<hydra.util.DecodingError, hydra.grammar.Symbol>left(new hydra.util.DecodingError("expected wrapped type hydra.grammar.Symbol"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.grammar.Symbol> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.grammar.Symbol>) (b -> new hydra.grammar.Symbol(b)),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError(err))))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                  return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                }
                
                @Override
                public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                  return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right((s).value)));
                    }
                  });
                }
              })),
              hydra.lexical.Lexical.stripAndDereferenceTermEither(
                cx,
                ((wrappedTerm).value).body)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
