package hydra.ext.shex.syntax;

public class PrefixDecl {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.PrefixDecl");
  
  public final hydra.ext.shex.syntax.PnameNs pnameNs;
  
  public final hydra.ext.shex.syntax.IriRef iriRef;
  
  public PrefixDecl (hydra.ext.shex.syntax.PnameNs pnameNs, hydra.ext.shex.syntax.IriRef iriRef) {
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
    return new PrefixDecl(pnameNs, iriRef);
  }
  
  public PrefixDecl withIriRef(hydra.ext.shex.syntax.IriRef iriRef) {
    return new PrefixDecl(pnameNs, iriRef);
  }
}