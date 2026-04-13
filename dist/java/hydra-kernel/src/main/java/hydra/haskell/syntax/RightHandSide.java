// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A right-hand side of a binding
 */
public class RightHandSide implements Serializable, Comparable<RightHandSide> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.RightHandSide");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.haskell.syntax.Expression value;

  public RightHandSide (hydra.haskell.syntax.Expression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RightHandSide)) {
      return false;
    }
    RightHandSide o = (RightHandSide) other;
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
  public int compareTo(RightHandSide other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
