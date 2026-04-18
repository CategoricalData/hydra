// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A throw statement
 */
public class ThrowStatement implements Serializable, Comparable<ThrowStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ThrowStatement");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.javaScript.syntax.Expression value;

  public ThrowStatement (hydra.javaScript.syntax.Expression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ThrowStatement)) {
      return false;
    }
    ThrowStatement o = (ThrowStatement) other;
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
  public int compareTo(ThrowStatement other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
