// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * The right-hand side of a pattern-matching alternative
 */
public class CaseRhs implements Serializable, Comparable<CaseRhs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.CaseRhs");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.haskell.syntax.Expression value;

  public CaseRhs (hydra.haskell.syntax.Expression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseRhs)) {
      return false;
    }
    CaseRhs o = (CaseRhs) other;
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
  public int compareTo(CaseRhs other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
