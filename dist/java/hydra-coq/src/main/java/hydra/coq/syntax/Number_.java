// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Number_ implements Serializable, Comparable<Number_> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Number");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.math.BigDecimal value;

  public Number_ (java.math.BigDecimal value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Number_)) {
      return false;
    }
    Number_ o = (Number_) other;
    return this.value.compareTo(o.value) == 0;
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Number_ other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
