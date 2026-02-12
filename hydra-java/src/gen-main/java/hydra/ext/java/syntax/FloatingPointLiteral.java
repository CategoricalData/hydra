// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

/**
 * Note: this is an approximation which ignores encoding
 */
public class FloatingPointLiteral implements Serializable, Comparable<FloatingPointLiteral> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.FloatingPointLiteral");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.math.BigDecimal value;
  
  public FloatingPointLiteral (java.math.BigDecimal value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FloatingPointLiteral)) {
      return false;
    }
    FloatingPointLiteral o = (FloatingPointLiteral) other;
    return this.value.compareTo(o.value) == 0;
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FloatingPointLiteral other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
