// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * An array type (T[])
 */
public class ArrayTypeExpression implements Serializable, Comparable<ArrayTypeExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ArrayTypeExpression");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.javaScript.syntax.TypeExpression value;

  public ArrayTypeExpression (hydra.javaScript.syntax.TypeExpression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayTypeExpression)) {
      return false;
    }
    ArrayTypeExpression o = (ArrayTypeExpression) other;
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
  public int compareTo(ArrayTypeExpression other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
