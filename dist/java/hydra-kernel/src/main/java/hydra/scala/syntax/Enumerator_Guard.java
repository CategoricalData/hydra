// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Enumerator_Guard implements Serializable, Comparable<Enumerator_Guard> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Enumerator_Guard");

  public static final hydra.core.Name COND = new hydra.core.Name("cond");

  public final hydra.scala.syntax.Data cond;

  public Enumerator_Guard (hydra.scala.syntax.Data cond) {
    this.cond = cond;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Enumerator_Guard)) {
      return false;
    }
    Enumerator_Guard o = (Enumerator_Guard) other;
    return java.util.Objects.equals(
      this.cond,
      o.cond);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(cond);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Enumerator_Guard other) {
    return hydra.util.Comparing.compare(
      cond,
      other.cond);
  }
}
