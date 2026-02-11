// Note: this is an automatically generated file. Do not edit.

package hydra.encode.constraints;

/**
 * Term encoders for hydra.constraints
 */
public interface Constraints {
  static hydra.core.Term pathEquation(hydra.constraints.PathEquation x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.constraints.PathEquation"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("left"), hydra.encode.query.Query.path((x).left)),
      new hydra.core.Field(new hydra.core.Name("right"), hydra.encode.query.Query.path((x).right)))));
  }
  
  static hydra.core.Term patternImplication(hydra.constraints.PatternImplication x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.constraints.PatternImplication"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("antecedent"), hydra.encode.query.Query.pattern((x).antecedent)),
      new hydra.core.Field(new hydra.core.Name("consequent"), hydra.encode.query.Query.pattern((x).consequent)))));
  }
}
