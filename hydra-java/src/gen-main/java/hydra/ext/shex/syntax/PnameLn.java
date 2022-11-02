package hydra.ext.shex.syntax;

public class PnameLn {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.PnameLn");
  
  public final hydra.ext.shex.syntax.PnameNs pnameNs;
  
  public final hydra.ext.shex.syntax.PnLocal pnLocal;
  
  public PnameLn (hydra.ext.shex.syntax.PnameNs pnameNs, hydra.ext.shex.syntax.PnLocal pnLocal) {
    this.pnameNs = pnameNs;
    this.pnLocal = pnLocal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnameLn)) {
      return false;
    }
    PnameLn o = (PnameLn) (other);
    return pnameNs.equals(o.pnameNs) && pnLocal.equals(o.pnLocal);
  }
  
  @Override
  public int hashCode() {
    return 2 * pnameNs.hashCode() + 3 * pnLocal.hashCode();
  }
  
  public PnameLn withPnameNs(hydra.ext.shex.syntax.PnameNs pnameNs) {
    return new PnameLn(pnameNs, pnLocal);
  }
  
  public PnameLn withPnLocal(hydra.ext.shex.syntax.PnLocal pnLocal) {
    return new PnameLn(pnameNs, pnLocal);
  }
}