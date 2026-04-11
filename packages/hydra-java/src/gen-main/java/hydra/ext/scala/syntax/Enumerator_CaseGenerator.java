// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Enumerator_CaseGenerator implements Serializable, Comparable<Enumerator_CaseGenerator> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Enumerator_CaseGenerator");

  public static final hydra.core.Name PAT = new hydra.core.Name("pat");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public final hydra.ext.scala.syntax.Pat pat;

  public final hydra.ext.scala.syntax.Data rhs;

  public Enumerator_CaseGenerator (hydra.ext.scala.syntax.Pat pat, hydra.ext.scala.syntax.Data rhs) {
    this.pat = pat;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Enumerator_CaseGenerator)) {
      return false;
    }
    Enumerator_CaseGenerator o = (Enumerator_CaseGenerator) other;
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
  public int compareTo(Enumerator_CaseGenerator other) {
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

  public Enumerator_CaseGenerator withPat(hydra.ext.scala.syntax.Pat pat) {
    return new Enumerator_CaseGenerator(pat, rhs);
  }

  public Enumerator_CaseGenerator withRhs(hydra.ext.scala.syntax.Data rhs) {
    return new Enumerator_CaseGenerator(pat, rhs);
  }
}
