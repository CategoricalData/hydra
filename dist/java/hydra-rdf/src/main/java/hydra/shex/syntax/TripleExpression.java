// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class TripleExpression implements Serializable, Comparable<TripleExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.TripleExpression");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.shex.syntax.OneOfTripleExpr value;

  public TripleExpression (hydra.shex.syntax.OneOfTripleExpr value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TripleExpression)) {
      return false;
    }
    TripleExpression o = (TripleExpression) other;
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
  public int compareTo(TripleExpression other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
