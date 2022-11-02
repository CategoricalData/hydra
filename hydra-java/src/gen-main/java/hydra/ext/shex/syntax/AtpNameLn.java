package hydra.ext.shex.syntax;

public class AtpNameLn {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.AtpNameLn");
  
  public final hydra.ext.shex.syntax.PnameNs pnameNs;
  
  public final hydra.ext.shex.syntax.PnLocal pnLocal;
  
  public AtpNameLn (hydra.ext.shex.syntax.PnameNs pnameNs, hydra.ext.shex.syntax.PnLocal pnLocal) {
    this.pnameNs = pnameNs;
    this.pnLocal = pnLocal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtpNameLn)) {
      return false;
    }
    AtpNameLn o = (AtpNameLn) (other);
    return pnameNs.equals(o.pnameNs) && pnLocal.equals(o.pnLocal);
  }
  
  @Override
  public int hashCode() {
    return 2 * pnameNs.hashCode() + 3 * pnLocal.hashCode();
  }
  
  public AtpNameLn withPnameNs(hydra.ext.shex.syntax.PnameNs pnameNs) {
    return new AtpNameLn(pnameNs, pnLocal);
  }
  
  public AtpNameLn withPnLocal(hydra.ext.shex.syntax.PnLocal pnLocal) {
    return new AtpNameLn(pnameNs, pnLocal);
  }
}