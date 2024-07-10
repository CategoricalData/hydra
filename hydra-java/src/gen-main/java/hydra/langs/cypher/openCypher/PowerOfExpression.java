// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PowerOfExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PowerOfExpression");
  
  public final java.util.List<hydra.langs.cypher.openCypher.UnaryAddOrSubtractExpression> value;
  
  public PowerOfExpression (java.util.List<hydra.langs.cypher.openCypher.UnaryAddOrSubtractExpression> value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PowerOfExpression)) {
      return false;
    }
    PowerOfExpression o = (PowerOfExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}