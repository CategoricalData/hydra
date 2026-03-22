// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class BigDecimalValue implements Serializable, Comparable<BigDecimalValue> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.graphson.syntax.BigDecimalValue");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public BigDecimalValue (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BigDecimalValue)) {
      return false;
    }
    BigDecimalValue o = (BigDecimalValue) other;
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
  public int compareTo(BigDecimalValue other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
