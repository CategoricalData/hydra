// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class AtpNameLn implements Serializable, Comparable<AtpNameLn> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.AtpNameLn");

  public static final hydra.core.Name PNAME_NS = new hydra.core.Name("PnameNs");

  public static final hydra.core.Name PN_LOCAL = new hydra.core.Name("PnLocal");

  public final hydra.shex.syntax.PnameNs PnameNs;

  public final hydra.shex.syntax.PnLocal PnLocal;

  public AtpNameLn (hydra.shex.syntax.PnameNs PnameNs, hydra.shex.syntax.PnLocal PnLocal) {
    this.PnameNs = PnameNs;
    this.PnLocal = PnLocal;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtpNameLn)) {
      return false;
    }
    AtpNameLn o = (AtpNameLn) other;
    return java.util.Objects.equals(
      this.PnameNs,
      o.PnameNs) && java.util.Objects.equals(
      this.PnLocal,
      o.PnLocal);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(PnameNs) + 3 * java.util.Objects.hashCode(PnLocal);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AtpNameLn other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      PnameNs,
      other.PnameNs);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      PnLocal,
      other.PnLocal);
  }

  public AtpNameLn withPnameNs(hydra.shex.syntax.PnameNs PnameNs) {
    return new AtpNameLn(PnameNs, PnLocal);
  }

  public AtpNameLn withPnLocal(hydra.shex.syntax.PnLocal PnLocal) {
    return new AtpNameLn(PnameNs, PnLocal);
  }
}
