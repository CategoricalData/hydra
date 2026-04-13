// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class FloatLiteral implements Serializable, Comparable<FloatLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.FloatLiteral");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.math.BigDecimal value;

  public FloatLiteral (java.math.BigDecimal value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FloatLiteral)) {
      return false;
    }
    FloatLiteral o = (FloatLiteral) other;
    return this.value.compareTo(o.value) == 0;
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FloatLiteral other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
