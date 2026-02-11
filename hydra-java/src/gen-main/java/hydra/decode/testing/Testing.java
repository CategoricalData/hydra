// Note: this is an automatically generated file. Do not edit.

package hydra.decode.testing;

/**
 * Term decoders for hydra.testing
 */
public interface Testing {
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase> alphaConversionTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.AlphaConversionTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "term",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>>) (field_term -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "oldVariable",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>>) (field_oldVariable -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "newVariable",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>>) (field_newVariable -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.helpers.Helpers.requireField(
                    "result",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                      p0,
                      p1)),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>>) (field_result -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase>right(new hydra.testing.AlphaConversionTestCase(field_term, field_oldVariable, field_newVariable, field_result))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle> evaluationStyle(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.EvaluationStyle>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.EvaluationStyle>left(new hydra.util.DecodingError("expected union of type hydra.testing.EvaluationStyle"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = ((inj).value).field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.core.Name tname = ((inj).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>>(new hydra.core.Name("eager"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.testing.EvaluationStyle>) (t -> new hydra.testing.EvaluationStyle.Eager()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>>(new hydra.core.Name("lazy"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.testing.EvaluationStyle>) (t -> new hydra.testing.EvaluationStyle.Lazy()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.EvaluationStyle>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              (fname).value,
              " in union type ",
              (tname).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase> caseConversionTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.CaseConversionTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "fromConvention",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>) (p0 -> p1 -> hydra.decode.util.Util.caseConvention(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.util.CaseConvention, hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>>) (field_fromConvention -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "toConvention",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>) (p0 -> p1 -> hydra.decode.util.Util.caseConvention(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.CaseConvention, hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>>) (field_toConvention -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "fromString",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                      cx2,
                      raw2)))),
                  fieldMap,
                  cx),
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>>) (field_fromString -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.helpers.Helpers.requireField(
                    "toString",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                        cx2,
                        raw2)))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>>) (field_toString -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.CaseConversionTestCase>right(new hydra.testing.CaseConversionTestCase(field_fromConvention, field_toConvention, field_fromString, field_toString))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase> delegatedEvaluationTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.DelegatedEvaluationTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase>right(new hydra.testing.DelegatedEvaluationTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase> etaExpansionTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.EtaExpansionTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase>right(new hydra.testing.EtaExpansionTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase> deannotateTermTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.DeannotateTermTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase>right(new hydra.testing.DeannotateTermTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase> deannotateTypeTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.DeannotateTypeTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase>right(new hydra.testing.DeannotateTypeTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase> flattenLetTermsTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.FlattenLetTermsTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase>right(new hydra.testing.FlattenLetTermsTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation> foldOperation(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.FoldOperation>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.FoldOperation>left(new hydra.util.DecodingError("expected union of type hydra.testing.FoldOperation"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = ((inj).value).field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.core.Name tname = ((inj).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>>(new hydra.core.Name("sumInt32Literals"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.testing.FoldOperation>) (t -> new hydra.testing.FoldOperation.SumInt32Literals()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>>(new hydra.core.Name("collectListLengths"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.testing.FoldOperation>) (t -> new hydra.testing.FoldOperation.CollectListLengths()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>>(new hydra.core.Name("collectLabels"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.testing.FoldOperation>) (t -> new hydra.testing.FoldOperation.CollectLabels()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.FoldOperation>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              (fname).value,
              " in union type ",
              (tname).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase> foldOverTermTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.FoldOverTermTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "traversalOrder",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>>) (p0 -> p1 -> hydra.decode.coders.Coders.traversalOrder(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.coders.TraversalOrder, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>>) (field_traversalOrder -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "operation",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOperation>>>) (p0 -> p1 -> hydra.decode.testing.Testing.foldOperation(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.testing.FoldOperation, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>>) (field_operation -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.helpers.Helpers.requireField(
                    "output",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                      p0,
                      p1)),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase>right(new hydra.testing.FoldOverTermTestCase(field_input, field_traversalOrder, field_operation, field_output))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase> freeVariablesTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.FreeVariablesTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Set<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Set<hydra.core.Name>>>) (v2 -> hydra.extract.helpers.Helpers.decodeSet(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.Set<hydra.core.Name>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase>right(new hydra.testing.FreeVariablesTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate> hoistPredicate(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistPredicate>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistPredicate>left(new hydra.util.DecodingError("expected union of type hydra.testing.HoistPredicate"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = ((inj).value).field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.core.Name tname = ((inj).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>(new hydra.core.Name("caseStatements"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.testing.HoistPredicate>) (t -> new hydra.testing.HoistPredicate.CaseStatements()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>(new hydra.core.Name("applications"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.testing.HoistPredicate>) (t -> new hydra.testing.HoistPredicate.Applications()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>(new hydra.core.Name("lists"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.testing.HoistPredicate>) (t -> new hydra.testing.HoistPredicate.Lists()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>(new hydra.core.Name("nothing"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.testing.HoistPredicate>) (t -> new hydra.testing.HoistPredicate.Nothing()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistPredicate>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              (fname).value,
              " in union type ",
              (tname).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase> hoistLetBindingsTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.HoistLetBindingsTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Let>>>) (p0 -> p1 -> hydra.decode.core.Core.let(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Let, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Let>>>) (p0 -> p1 -> hydra.decode.core.Core.let(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Let, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase>right(new hydra.testing.HoistLetBindingsTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase> hoistPolymorphicLetBindingsTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.HoistPolymorphicLetBindingsTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Let>>>) (p0 -> p1 -> hydra.decode.core.Core.let(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Let, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Let>>>) (p0 -> p1 -> hydra.decode.core.Core.let(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Let, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase>right(new hydra.testing.HoistPolymorphicLetBindingsTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase> hoistSubtermsTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.HoistSubtermsTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "predicate",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistPredicate>>>) (p0 -> p1 -> hydra.decode.testing.Testing.hoistPredicate(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.testing.HoistPredicate, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>>) (field_predicate -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "input",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "output",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase>right(new hydra.testing.HoistSubtermsTestCase(field_predicate, field_input, field_output))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase> hoistCaseStatementsTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.HoistCaseStatementsTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase>right(new hydra.testing.HoistCaseStatementsTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter> termRewriter(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TermRewriter>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TermRewriter>left(new hydra.util.DecodingError("expected union of type hydra.testing.TermRewriter"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = ((inj).value).field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.core.Name tname = ((inj).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>>(new hydra.core.Name("replaceFooWithBar"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.testing.TermRewriter>) (t -> new hydra.testing.TermRewriter.ReplaceFooWithBar()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>>(new hydra.core.Name("replaceInt32WithInt64"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.testing.TermRewriter>) (t -> new hydra.testing.TermRewriter.ReplaceInt32WithInt64()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TermRewriter>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              (fname).value,
              " in union type ",
              (tname).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase> rewriteTermTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.RewriteTermTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "rewriter",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TermRewriter>>>) (p0 -> p1 -> hydra.decode.testing.Testing.termRewriter(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.testing.TermRewriter, hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>>) (field_rewriter -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "output",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.RewriteTermTestCase>right(new hydra.testing.RewriteTermTestCase(field_input, field_rewriter, field_output))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter> typeRewriter(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TypeRewriter>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TypeRewriter>left(new hydra.util.DecodingError("expected union of type hydra.testing.TypeRewriter"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = ((inj).value).field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.core.Name tname = ((inj).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>>>(new hydra.core.Name("replaceStringWithInt32"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>>) (input -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.lang.Void, hydra.testing.TypeRewriter>) (t -> new hydra.testing.TypeRewriter.ReplaceStringWithInt32()),
            hydra.extract.helpers.Helpers.decodeUnit(
              cx,
              input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TypeRewriter>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              (fname).value,
              " in union type ",
              (tname).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase> rewriteTypeTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.RewriteTypeTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "rewriter",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeRewriter>>>) (p0 -> p1 -> hydra.decode.testing.Testing.typeRewriter(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.testing.TypeRewriter, hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>>) (field_rewriter -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "output",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase>right(new hydra.testing.RewriteTypeTestCase(field_input, field_rewriter, field_output))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase> evaluationTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.EvaluationTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "evaluationStyle",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationStyle>>>) (p0 -> p1 -> hydra.decode.testing.Testing.evaluationStyle(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.testing.EvaluationStyle, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>>) (field_evaluationStyle -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "input",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "output",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.EvaluationTestCase>right(new hydra.testing.EvaluationTestCase(field_evaluationStyle, field_input, field_output))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase> inferenceFailureTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.InferenceFailureTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase>>) (field_input -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase>right(new hydra.testing.InferenceFailureTestCase(field_input))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceTestCase> inferenceTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.InferenceTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.InferenceTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.InferenceTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.TypeScheme>>>) (p0 -> p1 -> hydra.decode.core.Core.typeScheme(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.InferenceTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.InferenceTestCase>right(new hydra.testing.InferenceTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase> jsonCoderTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.JsonCoderTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "type",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>>) (field_type -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "term",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>>) (field_term -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "json",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) (p0 -> p1 -> hydra.decode.json.model.Model.value(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>>) (field_json -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JsonCoderTestCase>right(new hydra.testing.JsonCoderTestCase(field_type, field_term, field_json))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase> jsonDecodeTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.JsonDecodeTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "type",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>>) (field_type -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "json",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) (p0 -> p1 -> hydra.decode.json.model.Model.value(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>>) (field_json -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "expected",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<String, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<String, hydra.core.Term>>>) (v2 -> hydra.extract.helpers.Helpers.decodeEither(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                        cx2,
                        raw2)))),
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.Either<String, hydra.core.Term>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>>) (field_expected -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase>right(new hydra.testing.JsonDecodeTestCase(field_type, field_json, field_expected))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase> jsonEncodeTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.JsonEncodeTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "term",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase>>) (field_term -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "expected",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<String, hydra.json.model.Value>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<String, hydra.json.model.Value>>>) (v2 -> hydra.extract.helpers.Helpers.decodeEither(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                      cx2,
                      raw2)))),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) (p0 -> p1 -> hydra.decode.json.model.Model.value(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.Either<String, hydra.json.model.Value>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase>>) (field_expected -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase>right(new hydra.testing.JsonEncodeTestCase(field_term, field_expected))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<hydra.json.model.Value>> jsonParserTestCase(hydra.graph.Graph v1, hydra.core.Term v2) {
    return hydra.decode.testing.Testing.parserTestCase(
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) (p0 -> p1 -> hydra.decode.json.model.Model.value(
        p0,
        p1)),
      v1,
      v2);
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase> jsonRoundtripTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.JsonRoundtripTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "type",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase>>) (field_type -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "term",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase>>) (field_term -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase>right(new hydra.testing.JsonRoundtripTestCase(field_type, field_term))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase> liftLambdaAboveLetTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.LiftLambdaAboveLetTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase>right(new hydra.testing.LiftLambdaAboveLetTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<hydra.json.model.Value>> jsonWriterTestCase(hydra.graph.Graph v1, hydra.core.Term v2) {
    return hydra.decode.testing.Testing.writerTestCase(
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) (p0 -> p1 -> hydra.decode.json.model.Model.value(
        p0,
        p1)),
      v1,
      v2);
  }
  
  static <T0> hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>> parserTestCase(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> a, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>>left(new hydra.util.DecodingError("expected record of type hydra.testing.ParserTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                  cx2,
                  raw2)))),
              fieldMap,
              cx),
            (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>) (v2 -> hydra.decode.parsing.Parsing.<T0>parseResult(
                  a,
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.parsing.ParseResult<T0>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.ParserTestCase<T0>>right((hydra.testing.ParserTestCase<T0>) (new hydra.testing.ParserTestCase<T0>(field_input, field_output)))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.Tag> tag(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.Tag>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.Tag>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.Tag>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.Tag>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.Tag>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.Tag> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.Tag>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.Tag>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.Tag>left(new hydra.util.DecodingError("expected wrapped type hydra.testing.Tag"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.Tag> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.testing.Tag>) (b -> new hydra.testing.Tag(b)),
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
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase> testCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TestCase>left(new hydra.util.DecodingError("expected union of type hydra.testing.TestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = ((inj).value).field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.core.Name tname = ((inj).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("alphaConversion"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.AlphaConversionTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.AlphaConversion(t)),
              hydra.decode.testing.Testing.alphaConversionTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("caseConversion"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.CaseConversionTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.CaseConversion(t)),
              hydra.decode.testing.Testing.caseConversionTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("deannotateTerm"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.DeannotateTermTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.DeannotateTerm(t)),
              hydra.decode.testing.Testing.deannotateTermTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("deannotateType"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.DeannotateTypeTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.DeannotateType(t)),
              hydra.decode.testing.Testing.deannotateTypeTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("delegatedEvaluation"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.DelegatedEvaluationTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.DelegatedEvaluation(t)),
              hydra.decode.testing.Testing.delegatedEvaluationTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("etaExpansion"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.EtaExpansionTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.EtaExpansion(t)),
              hydra.decode.testing.Testing.etaExpansionTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("flattenLetTerms"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.FlattenLetTermsTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.FlattenLetTerms(t)),
              hydra.decode.testing.Testing.flattenLetTermsTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("freeVariables"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.FreeVariablesTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.FreeVariables(t)),
              hydra.decode.testing.Testing.freeVariablesTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("evaluation"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.EvaluationTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.Evaluation(t)),
              hydra.decode.testing.Testing.evaluationTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("inference"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.InferenceTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.Inference(t)),
              hydra.decode.testing.Testing.inferenceTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("inferenceFailure"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.InferenceFailureTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.InferenceFailure(t)),
              hydra.decode.testing.Testing.inferenceFailureTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("jsonCoder"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.JsonCoderTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.JsonCoder(t)),
              hydra.decode.testing.Testing.jsonCoderTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("jsonDecode"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.JsonDecodeTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.JsonDecode(t)),
              hydra.decode.testing.Testing.jsonDecodeTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("jsonEncode"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.JsonEncodeTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.JsonEncode(t)),
              hydra.decode.testing.Testing.jsonEncodeTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("jsonParser"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.ParserTestCase<hydra.json.model.Value>, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.JsonParser(t)),
              hydra.decode.testing.Testing.jsonParserTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("jsonRoundtrip"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.JsonRoundtripTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.JsonRoundtrip(t)),
              hydra.decode.testing.Testing.jsonRoundtripTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("jsonWriter"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.WriterTestCase<hydra.json.model.Value>, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.JsonWriter(t)),
              hydra.decode.testing.Testing.jsonWriterTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("liftLambdaAboveLet"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.LiftLambdaAboveLetTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.LiftLambdaAboveLet(t)),
              hydra.decode.testing.Testing.liftLambdaAboveLetTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("serialization"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.SerializationTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.Serialization(t)),
              hydra.decode.testing.Testing.serializationTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("simplifyTerm"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.SimplifyTermTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.SimplifyTerm(t)),
              hydra.decode.testing.Testing.simplifyTermTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("topologicalSort"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.TopologicalSortTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.TopologicalSort(t)),
              hydra.decode.testing.Testing.topologicalSortTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("topologicalSortBindings"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.TopologicalSortBindingsTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.TopologicalSortBindings(t)),
              hydra.decode.testing.Testing.topologicalSortBindingsTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("topologicalSortSCC"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.TopologicalSortSCCTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.TopologicalSortSCC(t)),
              hydra.decode.testing.Testing.topologicalSortSCCTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("typeChecking"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.TypeCheckingTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.TypeChecking(t)),
              hydra.decode.testing.Testing.typeCheckingTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("typeCheckingFailure"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.TypeCheckingFailureTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.TypeCheckingFailure(t)),
              hydra.decode.testing.Testing.typeCheckingFailureTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("typeReduction"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.TypeReductionTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.TypeReduction(t)),
              hydra.decode.testing.Testing.typeReductionTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("normalizeTypeVariables"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.NormalizeTypeVariablesTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.NormalizeTypeVariables(t)),
              hydra.decode.testing.Testing.normalizeTypeVariablesTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("foldOverTerm"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.FoldOverTermTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.FoldOverTerm(t)),
              hydra.decode.testing.Testing.foldOverTermTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("rewriteTerm"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.RewriteTermTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.RewriteTerm(t)),
              hydra.decode.testing.Testing.rewriteTermTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("rewriteType"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.RewriteTypeTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.RewriteType(t)),
              hydra.decode.testing.Testing.rewriteTypeTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("hoistSubterms"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.HoistSubtermsTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.HoistSubterms(t)),
              hydra.decode.testing.Testing.hoistSubtermsTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("hoistCaseStatements"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.HoistCaseStatementsTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.HoistCaseStatements(t)),
              hydra.decode.testing.Testing.hoistCaseStatementsTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("hoistLetBindings"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.HoistLetBindingsTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.HoistLetBindings(t)),
              hydra.decode.testing.Testing.hoistLetBindingsTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("hoistPolymorphicLetBindings"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.HoistPolymorphicLetBindingsTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.HoistPolymorphicLetBindings(t)),
              hydra.decode.testing.Testing.hoistPolymorphicLetBindingsTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("substInType"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.SubstInTypeTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.SubstInType(t)),
              hydra.decode.testing.Testing.substInTypeTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("variableOccursInType"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.VariableOccursInTypeTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.VariableOccursInType(t)),
              hydra.decode.testing.Testing.variableOccursInTypeTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("unifyTypes"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.UnifyTypesTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.UnifyTypes(t)),
              hydra.decode.testing.Testing.unifyTypesTestCase(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("joinTypes"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.testing.JoinTypesTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.JoinTypes(t)),
              hydra.decode.testing.Testing.joinTypesTestCase(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TestCase>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              (fname).value,
              " in union type ",
              (tname).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata> testCaseWithMetadata(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>left(new hydra.util.DecodingError("expected record of type hydra.testing.TestCaseWithMetadata"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                  cx2,
                  raw2)))),
              fieldMap,
              cx),
            (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "case",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCase>>>) (p0 -> p1 -> hydra.decode.testing.Testing.testCase(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.testing.TestCase, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>>) (field_case -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "description",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<String>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<String>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMaybe(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                        cx2,
                        raw2)))),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>>) (field_description -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.helpers.Helpers.requireField(
                    "tags",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.testing.Tag>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.testing.Tag>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.Tag>>>) (p0 -> p1 -> hydra.decode.testing.Testing.tag(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<java.util.List<hydra.testing.Tag>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>>) (field_tags -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>right(new hydra.testing.TestCaseWithMetadata(field_name, field_case, field_description, field_tags))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup> testGroup(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TestGroup>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TestGroup>left(new hydra.util.DecodingError("expected record of type hydra.testing.TestGroup"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                  cx2,
                  raw2)))),
              fieldMap,
              cx),
            (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "description",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<String>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<String>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMaybe(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                      cx2,
                      raw2)))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup>>) (field_description -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "subgroups",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.testing.TestGroup>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.testing.TestGroup>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup>>>) (p0 -> p1 -> hydra.decode.testing.Testing.testGroup(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<java.util.List<hydra.testing.TestGroup>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup>>) (field_subgroups -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.helpers.Helpers.requireField(
                    "cases",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.testing.TestCaseWithMetadata>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.testing.TestCaseWithMetadata>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata>>>) (p0 -> p1 -> hydra.decode.testing.Testing.testCaseWithMetadata(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<java.util.List<hydra.testing.TestCaseWithMetadata>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup>>) (field_cases -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TestGroup>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TestGroup>right(new hydra.testing.TestGroup(field_name, field_description, field_subgroups, field_cases))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase> typeCheckingTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.TypeCheckingTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "outputTerm",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>>) (field_outputTerm -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "outputType",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>>) (field_outputType -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase>right(new hydra.testing.TypeCheckingTestCase(field_input, field_outputTerm, field_outputType))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase> typeCheckingFailureTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.TypeCheckingFailureTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase>>) (field_input -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase>right(new hydra.testing.TypeCheckingFailureTestCase(field_input))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase> topologicalSortBindingsTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.TopologicalSortBindingsTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "bindings",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>) (v22 -> hydra.extract.helpers.Helpers.decodePair(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                    p0,
                    p1)),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                    p0,
                    p1)),
                  v12,
                  v22))),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase>>) (field_bindings -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "expected",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>>) (v22 -> hydra.extract.helpers.Helpers.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>>) (v13 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>) (v23 -> hydra.extract.helpers.Helpers.decodePair(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                        p0,
                        p1)),
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                        p0,
                        p1)),
                      v13,
                      v23))),
                    v12,
                    v22))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.List<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase>>) (field_expected -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase>right(new hydra.testing.TopologicalSortBindingsTestCase(field_bindings, field_expected))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase> topologicalSortTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.TopologicalSortTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "adjacencyList",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>>) (v22 -> hydra.extract.helpers.Helpers.decodePair(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
                    (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                        return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v13) {
                            return ((v13).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                              @Override
                              public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                                return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                              }
                              
                              @Override
                              public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                                return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right((i).value)));
                              }
                            });
                          }
                        });
                      }
                    })),
                    hydra.lexical.Lexical.stripAndDereferenceTermEither(
                      cx2,
                      raw2)))),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>>) (v13 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>) (v23 -> hydra.extract.helpers.Helpers.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
                      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
                      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                          return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                        }
                        
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                          return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                            @Override
                            public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                              return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                            }
                            
                            @Override
                            public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v14) {
                              return ((v14).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                                @Override
                                public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                                  return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                                }
                                
                                @Override
                                public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                                  return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right((i).value)));
                                }
                              });
                            }
                          });
                        }
                      })),
                      hydra.lexical.Lexical.stripAndDereferenceTermEither(
                        cx2,
                        raw2)))),
                    v13,
                    v23))),
                  v12,
                  v22))),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase>>) (field_adjacencyList -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "expected",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>>>) (v2 -> hydra.extract.helpers.Helpers.decodeEither(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<java.util.List<Integer>>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<java.util.List<Integer>>>>) (v22 -> hydra.extract.helpers.Helpers.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>>) (v13 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>) (v23 -> hydra.extract.helpers.Helpers.decodeList(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
                        (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
                        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                            return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                              @Override
                              public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                                return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                              }
                              
                              @Override
                              public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v14) {
                                return ((v14).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                                  @Override
                                  public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                                    return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                                  }
                                  
                                  @Override
                                  public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                                    return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right((i).value)));
                                  }
                                });
                              }
                            });
                          }
                        })),
                        hydra.lexical.Lexical.stripAndDereferenceTermEither(
                          cx2,
                          raw2)))),
                      v13,
                      v23))),
                    v12,
                    v22))),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>) (v22 -> hydra.extract.helpers.Helpers.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
                      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
                      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                          return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                        }
                        
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                          return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                            @Override
                            public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                              return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                            }
                            
                            @Override
                            public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v13) {
                              return ((v13).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                                @Override
                                public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                                  return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                                }
                                
                                @Override
                                public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                                  return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right((i).value)));
                                }
                              });
                            }
                          });
                        }
                      })),
                      hydra.lexical.Lexical.stripAndDereferenceTermEither(
                        cx2,
                        raw2)))),
                    v12,
                    v22))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase>>) (field_expected -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase>right(new hydra.testing.TopologicalSortTestCase(field_adjacencyList, field_expected))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase> topologicalSortSCCTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.TopologicalSortSCCTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "adjacencyList",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>>) (v22 -> hydra.extract.helpers.Helpers.decodePair(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
                    (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                        return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v13) {
                            return ((v13).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                              @Override
                              public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                                return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                              }
                              
                              @Override
                              public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                                return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right((i).value)));
                              }
                            });
                          }
                        });
                      }
                    })),
                    hydra.lexical.Lexical.stripAndDereferenceTermEither(
                      cx2,
                      raw2)))),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>>) (v13 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>) (v23 -> hydra.extract.helpers.Helpers.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
                      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
                      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                          return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                        }
                        
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                          return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                            @Override
                            public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                              return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                            }
                            
                            @Override
                            public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v14) {
                              return ((v14).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                                @Override
                                public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                                  return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                                }
                                
                                @Override
                                public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                                  return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right((i).value)));
                                }
                              });
                            }
                          });
                        }
                      })),
                      hydra.lexical.Lexical.stripAndDereferenceTermEither(
                        cx2,
                        raw2)))),
                    v13,
                    v23))),
                  v12,
                  v22))),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase>>) (field_adjacencyList -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "expected",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<java.util.List<Integer>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<java.util.List<Integer>>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>) (v22 -> hydra.extract.helpers.Helpers.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
                      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
                      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                          return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                        }
                        
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                          return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                            @Override
                            public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                              return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                            }
                            
                            @Override
                            public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v13) {
                              return ((v13).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                                @Override
                                public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                                  return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                                }
                                
                                @Override
                                public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                                  return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right((i).value)));
                                }
                              });
                            }
                          });
                        }
                      })),
                      hydra.lexical.Lexical.stripAndDereferenceTermEither(
                        cx2,
                        raw2)))),
                    v12,
                    v22))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.List<java.util.List<Integer>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase>>) (field_expected -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase>right(new hydra.testing.TopologicalSortSCCTestCase(field_adjacencyList, field_expected))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.SerializationTestCase> serializationTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.SerializationTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.SerializationTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.SerializationTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.SerializationTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.SerializationTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.SerializationTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.SerializationTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.SerializationTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.SerializationTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.SerializationTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.SerializationTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.ast.Expr>>>) (p0 -> p1 -> hydra.decode.ast.Ast.expr(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.ast.Expr, hydra.util.Either<hydra.util.DecodingError, hydra.testing.SerializationTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                    cx2,
                    raw2)))),
                fieldMap,
                cx),
              (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.SerializationTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.SerializationTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.SerializationTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.SerializationTestCase>right(new hydra.testing.SerializationTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase> simplifyTermTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.SimplifyTermTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase>right(new hydra.testing.SimplifyTermTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase> normalizeTypeVariablesTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.NormalizeTypeVariablesTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase>right(new hydra.testing.NormalizeTypeVariablesTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase> typeReductionTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.TypeReductionTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.TypeReductionTestCase>right(new hydra.testing.TypeReductionTestCase(field_input, field_output))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static <T0> hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>> writerTestCase(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> a, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>>left(new hydra.util.DecodingError("expected record of type hydra.testing.WriterTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "input",
              a,
              fieldMap,
              cx),
            (java.util.function.Function<T0, hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>>>) (field_input -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "output",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                    cx2,
                    raw2)))),
                fieldMap,
                cx),
              (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.WriterTestCase<T0>>right((hydra.testing.WriterTestCase<T0>) (new hydra.testing.WriterTestCase<T0>(field_input, field_output)))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase> substInTypeTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.SubstInTypeTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "substitution",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>>) (v22 -> hydra.extract.helpers.Helpers.decodePair(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                    p0,
                    p1)),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                    p0,
                    p1)),
                  v12,
                  v22))),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>>) (field_substitution -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "input",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>>) (field_input -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "output",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>>) (field_output -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.SubstInTypeTestCase>right(new hydra.testing.SubstInTypeTestCase(field_substitution, field_input, field_output))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase> variableOccursInTypeTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.VariableOccursInTypeTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "variable",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>>) (field_variable -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "type",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>>) (field_type -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "expected",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Boolean>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (raw2 -> hydra.lib.eithers.Either.apply(
                    (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError(err))))),
                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Boolean> otherwise(hydra.core.Term instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError("expected literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Boolean> visit(hydra.core.Term.Literal v) {
                        return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Boolean> otherwise(hydra.core.Literal instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError("expected boolean literal"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Boolean> visit(hydra.core.Literal.Boolean_ b) {
                            return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>right((b).value)));
                          }
                        });
                      }
                    })),
                    hydra.lexical.Lexical.stripAndDereferenceTermEither(
                      cx2,
                      raw2)))),
                  fieldMap,
                  cx),
                (java.util.function.Function<Boolean, hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>>) (field_expected -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.VariableOccursInTypeTestCase>right(new hydra.testing.VariableOccursInTypeTestCase(field_variable, field_type, field_expected))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase> unifyTypesTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.UnifyTypesTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "schemaTypes",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.core.Name>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>>) (field_schemaTypes -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "left",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>>) (field_left -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "right",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>>) (field_right -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.helpers.Helpers.requireField(
                    "expected",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<String, hydra.typing.TypeSubst>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<String, hydra.typing.TypeSubst>>>) (v2 -> hydra.extract.helpers.Helpers.decodeEither(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                          cx2,
                          raw2)))),
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeSubst>>>) (p0 -> p1 -> hydra.decode.typing.Typing.typeSubst(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.util.Either<String, hydra.typing.TypeSubst>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>>) (field_expected -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.UnifyTypesTestCase>right(new hydra.testing.UnifyTypesTestCase(field_schemaTypes, field_left, field_right, field_expected))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase> joinTypesTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>left(new hydra.util.DecodingError("expected record of type hydra.testing.JoinTypesTestCase"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "left",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>>) (field_left -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "right",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>>) (field_right -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "expected",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<java.lang.Void, java.util.List<hydra.typing.TypeConstraint>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<java.lang.Void, java.util.List<hydra.typing.TypeConstraint>>>>) (v2 -> hydra.extract.helpers.Helpers.decodeEither(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.lang.Void>>>) (p0 -> p1 -> hydra.extract.helpers.Helpers.decodeUnit(
                      p0,
                      p1)),
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.typing.TypeConstraint>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.typing.TypeConstraint>>>) (v22 -> hydra.extract.helpers.Helpers.decodeList(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint>>>) (p0 -> p1 -> hydra.decode.typing.Typing.typeConstraint(
                        p0,
                        p1)),
                      v12,
                      v22))),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.Either<java.lang.Void, java.util.List<hydra.typing.TypeConstraint>>, hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>>) (field_expected -> (hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>) ((hydra.util.Either<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>) (hydra.util.Either.<hydra.util.DecodingError, hydra.testing.JoinTypesTestCase>right(new hydra.testing.JoinTypesTestCase(field_left, field_right, field_expected))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
