// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class PrefixDecl implements Serializable, Comparable<PrefixDecl> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.PrefixDecl");
  
  public static final hydra.core.Name PNAME_NS = new hydra.core.Name("PnameNs");
  
  public static final hydra.core.Name IRI_REF = new hydra.core.Name("IriRef");
  
  public final hydra.ext.io.shex.syntax.PnameNs PnameNs;
  
  public final hydra.ext.io.shex.syntax.IriRef IriRef;
  
  public PrefixDecl (hydra.ext.io.shex.syntax.PnameNs PnameNs, hydra.ext.io.shex.syntax.IriRef IriRef) {
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
    cmp = ((Comparable) PnameNs).compareTo(other.PnameNs);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) IriRef).compareTo(other.IriRef);
  }
  
  public PrefixDecl withPnameNs(hydra.ext.io.shex.syntax.PnameNs PnameNs) {
    return new PrefixDecl(PnameNs, IriRef);
  }
  
  public PrefixDecl withIriRef(hydra.ext.io.shex.syntax.IriRef IriRef) {
    return new PrefixDecl(PnameNs, IriRef);
  }
}
