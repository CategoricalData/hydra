// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Enumerator_Val implements Serializable, Comparable<Enumerator_Val> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Enumerator_Val");

  public static final hydra.core.Name PAT = new hydra.core.Name("pat");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.ext.scala.syntax.Pat pat;

  public final hydra.ext.scala.syntax.Data rhs;

  public Enumerator_Val (hydra.ext.scala.syntax.Pat pat, hydra.ext.scala.syntax.Data rhs) {
    this.pat = pat;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Enumerator_Val)) {
      return false;
    }
    Enumerator_Val o = (Enumerator_Val) other;
    return java.util.Objects.equals(
      this.pat,
      o.pat) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pat) + 3 * java.util.Objects.hashCode(rhs);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Enumerator_Val other) {
    int cmp = 0;
    cmp = ((Comparable) pat).compareTo(other.pat);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }

  public Enumerator_Val withPat(hydra.ext.scala.syntax.Pat pat) {
    return new Enumerator_Val(pat, rhs);
  }

  public Enumerator_Val withRhs(hydra.ext.scala.syntax.Data rhs) {
    return new Enumerator_Val(pat, rhs);
  }
}
