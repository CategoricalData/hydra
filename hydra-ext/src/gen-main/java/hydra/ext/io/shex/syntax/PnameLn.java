// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class PnameLn implements Serializable, Comparable<PnameLn> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.PnameLn");

  public static final hydra.core.Name PNAME_NS = new hydra.core.Name("PnameNs");

  public static final hydra.core.Name PN_LOCAL = new hydra.core.Name("PnLocal");

  public final hydra.ext.io.shex.syntax.PnameNs PnameNs;

  public final hydra.ext.io.shex.syntax.PnLocal PnLocal;

  public PnameLn (hydra.ext.io.shex.syntax.PnameNs PnameNs, hydra.ext.io.shex.syntax.PnLocal PnLocal) {
    this.PnameNs = PnameNs;
    this.PnLocal = PnLocal;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnameLn)) {
      return false;
    }
    PnameLn o = (PnameLn) other;
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
  public int compareTo(PnameLn other) {
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

  public PnameLn withPnameNs(hydra.ext.io.shex.syntax.PnameNs PnameNs) {
    return new PnameLn(PnameNs, PnLocal);
  }

  public PnameLn withPnLocal(hydra.ext.io.shex.syntax.PnLocal PnLocal) {
    return new PnameLn(PnameNs, PnLocal);
  }
}
