// Note: this is an automatically generated file. Do not edit.

package hydra.decode.constraints;

/**
 * Term decoders for hydra.constraints
 */
public interface Constraints {
  static hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PathEquation> pathEquation(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PathEquation>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PathEquation>) ((hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PathEquation>) (hydra.util.Either.<hydra.util.DecodingError, hydra.constraints.PathEquation>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PathEquation>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PathEquation> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PathEquation>) ((hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PathEquation>) (hydra.util.Either.<hydra.util.DecodingError, hydra.constraints.PathEquation>left(new hydra.util.DecodingError("expected record of type hydra.constraints.PathEquation"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PathEquation> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "left",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.query.Path>>>) (p0 -> p1 -> hydra.decode.query.Query.path(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.query.Path, hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PathEquation>>) (field_left -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "right",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.query.Path>>>) (p0 -> p1 -> hydra.decode.query.Query.path(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.query.Path, hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PathEquation>>) (field_right -> (hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PathEquation>) ((hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PathEquation>) (hydra.util.Either.<hydra.util.DecodingError, hydra.constraints.PathEquation>right(new hydra.constraints.PathEquation(field_left, field_right))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PatternImplication> patternImplication(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PatternImplication>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PatternImplication>) ((hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PatternImplication>) (hydra.util.Either.<hydra.util.DecodingError, hydra.constraints.PatternImplication>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PatternImplication>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PatternImplication> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PatternImplication>) ((hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PatternImplication>) (hydra.util.Either.<hydra.util.DecodingError, hydra.constraints.PatternImplication>left(new hydra.util.DecodingError("expected record of type hydra.constraints.PatternImplication"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PatternImplication> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "antecedent",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.query.Pattern>>>) (p0 -> p1 -> hydra.decode.query.Query.pattern(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.query.Pattern, hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PatternImplication>>) (field_antecedent -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "consequent",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.query.Pattern>>>) (p0 -> p1 -> hydra.decode.query.Query.pattern(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.query.Pattern, hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PatternImplication>>) (field_consequent -> (hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PatternImplication>) ((hydra.util.Either<hydra.util.DecodingError, hydra.constraints.PatternImplication>) (hydra.util.Either.<hydra.util.DecodingError, hydra.constraints.PatternImplication>right(new hydra.constraints.PatternImplication(field_antecedent, field_consequent))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
