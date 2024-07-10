// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ListPredicateExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ListPredicateExpression");
  
  public final hydra.langs.cypher.openCypher.AddOrSubtractExpression value;
  
  public ListPredicateExpression (hydra.langs.cypher.openCypher.AddOrSubtractExpression value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListPredicateExpression)) {
      return false;
    }
    ListPredicateExpression o = (ListPredicateExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}