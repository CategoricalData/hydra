// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Ident implements Serializable, Comparable<Ident> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Ident");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.coq.syntax.String_ value;

  public Ident (hydra.coq.syntax.String_ value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Ident)) {
      return false;
    }
    Ident o = (Ident) other;
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
  public int compareTo(Ident other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
