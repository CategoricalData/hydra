// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class OrPattern implements Serializable, Comparable<OrPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.syntax.OrPattern");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.ConsList<hydra.ext.python.syntax.ClosedPattern> value;

  public OrPattern (hydra.util.ConsList<hydra.ext.python.syntax.ClosedPattern> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OrPattern)) {
      return false;
    }
    OrPattern o = (OrPattern) other;
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
  public int compareTo(OrPattern other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
