// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class PnameLn implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.PnameLn");
  
  public static final hydra.core.Name FIELD_NAME_PNAME_NS = new hydra.core.Name("pnameNs");
  
  public static final hydra.core.Name FIELD_NAME_PN_LOCAL = new hydra.core.Name("pnLocal");
  
  public final hydra.ext.shex.syntax.PnameNs pnameNs;
  
  public final hydra.ext.shex.syntax.PnLocal pnLocal;
  
  public PnameLn (hydra.ext.shex.syntax.PnameNs pnameNs, hydra.ext.shex.syntax.PnLocal pnLocal) {
    java.util.Objects.requireNonNull((pnameNs));
    java.util.Objects.requireNonNull((pnLocal));
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
    java.util.Objects.requireNonNull((pnameNs));
    return new PnameLn(pnameNs, pnLocal);
  }
  
  public PnameLn withPnLocal(hydra.ext.shex.syntax.PnLocal pnLocal) {
    java.util.Objects.requireNonNull((pnLocal));
    return new PnameLn(pnameNs, pnLocal);
  }
}