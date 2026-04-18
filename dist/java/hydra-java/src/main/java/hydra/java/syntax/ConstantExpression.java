// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class ConstantExpression implements Serializable, Comparable<ConstantExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ConstantExpression");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.java.syntax.Expression value;

  public ConstantExpression (hydra.java.syntax.Expression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstantExpression)) {
      return false;
    }
    ConstantExpression o = (ConstantExpression) other;
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
  public int compareTo(ConstantExpression other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
