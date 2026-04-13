// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class Exclusion implements Serializable, Comparable<Exclusion> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.Exclusion");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.shex.syntax.Iri value;

  public Exclusion (hydra.shex.syntax.Iri value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Exclusion)) {
      return false;
    }
    Exclusion o = (Exclusion) other;
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
  public int compareTo(Exclusion other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
