// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class IncludeSet implements Serializable, Comparable<IncludeSet> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.IncludeSet");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.ext.io.shex.syntax.ShapeExprLabel> value;

  public IncludeSet (java.util.List<hydra.ext.io.shex.syntax.ShapeExprLabel> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IncludeSet)) {
      return false;
    }
    IncludeSet o = (IncludeSet) other;
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
  public int compareTo(IncludeSet other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
