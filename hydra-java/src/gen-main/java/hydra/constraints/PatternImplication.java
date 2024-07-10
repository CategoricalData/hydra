// Note: this is an automatically generated file. Do not edit.

package hydra.constraints;

import java.io.Serializable;

/**
 * A pattern which, if it matches in a given graph, implies that another pattern must also match. Query variables are shared between the two patterns.
 */
public class PatternImplication<A> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/constraints.PatternImplication");
  
  public final hydra.query.Pattern<A> antecedent;
  
  public final hydra.query.Pattern<A> consequent;
  
  public PatternImplication (hydra.query.Pattern<A> antecedent, hydra.query.Pattern<A> consequent) {
    if (antecedent == null) {
      throw new IllegalArgumentException("null value for 'antecedent' argument");
    }
    if (consequent == null) {
      throw new IllegalArgumentException("null value for 'consequent' argument");
    }
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
  
  public PatternImplication withAntecedent(hydra.query.Pattern<A> antecedent) {
    if (antecedent == null) {
      throw new IllegalArgumentException("null value for 'antecedent' argument");
    }
    return new PatternImplication(antecedent, consequent);
  }
  
  public PatternImplication withConsequent(hydra.query.Pattern<A> consequent) {
    if (consequent == null) {
      throw new IllegalArgumentException("null value for 'consequent' argument");
    }
    return new PatternImplication(antecedent, consequent);
  }
}