// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class Catches implements Serializable, Comparable<Catches> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.Catches");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.java.syntax.CatchClause> value;

  public Catches (java.util.List<hydra.java.syntax.CatchClause> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Catches)) {
      return false;
    }
    Catches o = (Catches) other;
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
  public int compareTo(Catches other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
