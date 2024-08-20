// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class PrefixDecl implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.PrefixDecl");
  
  public static final hydra.core.Name FIELD_NAME_PNAME_NS = new hydra.core.Name("pnameNs");
  
  public static final hydra.core.Name FIELD_NAME_IRI_REF = new hydra.core.Name("iriRef");
  
  public final hydra.ext.shex.syntax.PnameNs pnameNs;
  
  public final hydra.ext.shex.syntax.IriRef iriRef;
  
  public PrefixDecl (hydra.ext.shex.syntax.PnameNs pnameNs, hydra.ext.shex.syntax.IriRef iriRef) {
    java.util.Objects.requireNonNull((pnameNs));
    java.util.Objects.requireNonNull((iriRef));
    this.pnameNs = pnameNs;
    this.iriRef = iriRef;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrefixDecl)) {
      return false;
    }
    PrefixDecl o = (PrefixDecl) (other);
    return pnameNs.equals(o.pnameNs) && iriRef.equals(o.iriRef);
  }
  
  @Override
  public int hashCode() {
    return 2 * pnameNs.hashCode() + 3 * iriRef.hashCode();
  }
  
  public PrefixDecl withPnameNs(hydra.ext.shex.syntax.PnameNs pnameNs) {
    java.util.Objects.requireNonNull((pnameNs));
    return new PrefixDecl(pnameNs, iriRef);
  }
  
  public PrefixDecl withIriRef(hydra.ext.shex.syntax.IriRef iriRef) {
    java.util.Objects.requireNonNull((iriRef));
    return new PrefixDecl(pnameNs, iriRef);
  }
}