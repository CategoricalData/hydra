// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class PnPrefix_Sequence_Option implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.PnPrefix.Sequence.Option");
  
  public static final hydra.core.Name FIELD_NAME_ALTS = new hydra.core.Name("alts");
  
  public static final hydra.core.Name FIELD_NAME_PN_CHARS = new hydra.core.Name("pnChars");
  
  public final hydra.ext.shex.syntax.PnPrefix_Sequence_Option_Alts alts;
  
  public final hydra.ext.shex.syntax.PnChars pnChars;
  
  public PnPrefix_Sequence_Option (hydra.ext.shex.syntax.PnPrefix_Sequence_Option_Alts alts, hydra.ext.shex.syntax.PnChars pnChars) {
    java.util.Objects.requireNonNull((alts));
    java.util.Objects.requireNonNull((pnChars));
    this.alts = alts;
    this.pnChars = pnChars;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnPrefix_Sequence_Option)) {
      return false;
    }
    PnPrefix_Sequence_Option o = (PnPrefix_Sequence_Option) (other);
    return alts.equals(o.alts) && pnChars.equals(o.pnChars);
  }
  
  @Override
  public int hashCode() {
    return 2 * alts.hashCode() + 3 * pnChars.hashCode();
  }
  
  public PnPrefix_Sequence_Option withAlts(hydra.ext.shex.syntax.PnPrefix_Sequence_Option_Alts alts) {
    java.util.Objects.requireNonNull((alts));
    return new PnPrefix_Sequence_Option(alts, pnChars);
  }
  
  public PnPrefix_Sequence_Option withPnChars(hydra.ext.shex.syntax.PnChars pnChars) {
    java.util.Objects.requireNonNull((pnChars));
    return new PnPrefix_Sequence_Option(alts, pnChars);
  }
}