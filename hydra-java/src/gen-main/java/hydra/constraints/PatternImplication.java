// Note: this is an automatically generated file. Do not edit.

package hydra.constraints;

import java.io.Serializable;

/**
 * A pattern which, if it matches in a given graph, implies that another pattern must also match. Query variables are shared between the two patterns.
 */
public class PatternImplication implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/constraints.PatternImplication");
  
  public static final hydra.core.Name FIELD_NAME_ANTECEDENT = new hydra.core.Name("antecedent");
  
  public static final hydra.core.Name FIELD_NAME_CONSEQUENT = new hydra.core.Name("consequent");
  
  public final hydra.query.Pattern antecedent;
  
  public final hydra.query.Pattern consequent;
  
  public PatternImplication (hydra.query.Pattern antecedent, hydra.query.Pattern consequent) {
    java.util.Objects.requireNonNull((antecedent));
    java.util.Objects.requireNonNull((consequent));
    this.antecedent = antecedent;
    this.consequent = consequent;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternImplication)) {
      return false;
    }
    PatternImplication o = (PatternImplication) (other);
    return antecedent.equals(o.antecedent) && consequent.equals(o.consequent);
  }
  
  @Override
  public int hashCode() {
    return 2 * antecedent.hashCode() + 3 * consequent.hashCode();
  }
  
  public PatternImplication withAntecedent(hydra.query.Pattern antecedent) {
    java.util.Objects.requireNonNull((antecedent));
    return new PatternImplication(antecedent, consequent);
  }
  
  public PatternImplication withConsequent(hydra.query.Pattern consequent) {
    java.util.Objects.requireNonNull((consequent));
    return new PatternImplication(antecedent, consequent);
  }
}