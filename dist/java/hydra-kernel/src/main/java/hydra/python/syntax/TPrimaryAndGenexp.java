// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class TPrimaryAndGenexp implements Serializable, Comparable<TPrimaryAndGenexp> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.TPrimaryAndGenexp");

  public static final hydra.core.Name PRIMARY = new hydra.core.Name("primary");

  public static final hydra.core.Name GENEXP = new hydra.core.Name("genexp");

  public final hydra.python.syntax.TPrimary primary;

  public final hydra.python.syntax.Genexp genexp;

  public TPrimaryAndGenexp (hydra.python.syntax.TPrimary primary, hydra.python.syntax.Genexp genexp) {
    this.primary = primary;
    this.genexp = genexp;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TPrimaryAndGenexp)) {
      return false;
    }
    TPrimaryAndGenexp o = (TPrimaryAndGenexp) other;
    return java.util.Objects.equals(
      this.primary,
      o.primary) && java.util.Objects.equals(
      this.genexp,
      o.genexp);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(primary) + 3 * java.util.Objects.hashCode(genexp);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TPrimaryAndGenexp other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      primary,
      other.primary);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      genexp,
      other.genexp);
  }

  public TPrimaryAndGenexp withPrimary(hydra.python.syntax.TPrimary primary) {
    return new TPrimaryAndGenexp(primary, genexp);
  }

  public TPrimaryAndGenexp withGenexp(hydra.python.syntax.Genexp genexp) {
    return new TPrimaryAndGenexp(primary, genexp);
  }
}
