// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class PowerOfExpression implements Serializable, Comparable<PowerOfExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.PowerOfExpression");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.UnaryAddOrSubtractExpression> value;
  
  public PowerOfExpression (hydra.util.ConsList<hydra.ext.cypher.openCypher.UnaryAddOrSubtractExpression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PowerOfExpression)) {
      return false;
    }
    PowerOfExpression o = (PowerOfExpression) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PowerOfExpression other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
