// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class ParenthesizedExpression implements Serializable, Comparable<ParenthesizedExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.ParenthesizedExpression");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.cypher.openCypher.Expression value;

  public ParenthesizedExpression (hydra.cypher.openCypher.Expression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParenthesizedExpression)) {
      return false;
    }
    ParenthesizedExpression o = (ParenthesizedExpression) other;
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
  public int compareTo(ParenthesizedExpression other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
