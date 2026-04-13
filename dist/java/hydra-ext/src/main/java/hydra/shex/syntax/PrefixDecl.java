// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class PrefixDecl implements Serializable, Comparable<PrefixDecl> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.PrefixDecl");

  public static final hydra.core.Name PNAME_NS = new hydra.core.Name("PnameNs");

  public static final hydra.core.Name IRI_REF = new hydra.core.Name("IriRef");

  public final hydra.shex.syntax.PnameNs PnameNs;

  public final hydra.shex.syntax.IriRef IriRef;

  public PrefixDecl (hydra.shex.syntax.PnameNs PnameNs, hydra.shex.syntax.IriRef IriRef) {
    this.PnameNs = PnameNs;
    this.IriRef = IriRef;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrefixDecl)) {
      return false;
    }
    PrefixDecl o = (PrefixDecl) other;
    return java.util.Objects.equals(
      this.PnameNs,
      o.PnameNs) && java.util.Objects.equals(
      this.IriRef,
      o.IriRef);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(PnameNs) + 3 * java.util.Objects.hashCode(IriRef);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PrefixDecl other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      PnameNs,
      other.PnameNs);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      IriRef,
      other.IriRef);
  }

  public PrefixDecl withPnameNs(hydra.shex.syntax.PnameNs PnameNs) {
    return new PrefixDecl(PnameNs, IriRef);
  }

  public PrefixDecl withIriRef(hydra.shex.syntax.IriRef IriRef) {
    return new PrefixDecl(PnameNs, IriRef);
  }
}
