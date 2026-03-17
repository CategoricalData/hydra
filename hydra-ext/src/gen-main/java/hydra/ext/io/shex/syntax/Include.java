// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class Include implements Serializable, Comparable<Include> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.Include");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.ext.io.shex.syntax.TripleExprLabel value;

  public Include (hydra.ext.io.shex.syntax.TripleExprLabel value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Include)) {
      return false;
    }
    Include o = (Include) other;
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
  public int compareTo(Include other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
