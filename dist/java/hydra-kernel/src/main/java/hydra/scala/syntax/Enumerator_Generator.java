// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Enumerator_Generator implements Serializable, Comparable<Enumerator_Generator> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Enumerator_Generator");

  public static final hydra.core.Name PAT = new hydra.core.Name("pat");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.scala.syntax.Pat pat;

  public final hydra.scala.syntax.Data rhs;

  public Enumerator_Generator (hydra.scala.syntax.Pat pat, hydra.scala.syntax.Data rhs) {
    this.pat = pat;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Enumerator_Generator)) {
      return false;
    }
    Enumerator_Generator o = (Enumerator_Generator) other;
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
  public int compareTo(Enumerator_Generator other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pat,
      other.pat);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      rhs,
      other.rhs);
  }

  public Enumerator_Generator withPat(hydra.scala.syntax.Pat pat) {
    return new Enumerator_Generator(pat, rhs);
  }

  public Enumerator_Generator withRhs(hydra.scala.syntax.Data rhs) {
    return new Enumerator_Generator(pat, rhs);
  }
}
