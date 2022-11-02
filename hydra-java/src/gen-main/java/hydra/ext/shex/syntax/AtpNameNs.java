package hydra.ext.shex.syntax;

public class AtpNameNs {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.AtpNameNs");
  
  public final java.util.Optional<hydra.ext.shex.syntax.PnPrefix> pnPrefix;
  
  public AtpNameNs (java.util.Optional<hydra.ext.shex.syntax.PnPrefix> pnPrefix) {
    this.pnPrefix = pnPrefix;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtpNameNs)) {
      return false;
    }
    AtpNameNs o = (AtpNameNs) (other);
    return pnPrefix.equals(o.pnPrefix);
  }
  
  @Override
  public int hashCode() {
    return 2 * pnPrefix.hashCode();
  }
}