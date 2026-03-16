// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class PnLocal implements Serializable, Comparable<PnLocal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal");
  
  public static final hydra.core.Name ALTS = new hydra.core.Name("alts");
  
  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("Sequence");
  
  public final hydra.ext.io.shex.syntax.PnLocal_Alts alts;
  
  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.PnLocal_Sequence_Option> Sequence;
  
  public PnLocal (hydra.ext.io.shex.syntax.PnLocal_Alts alts, hydra.util.Maybe<hydra.ext.io.shex.syntax.PnLocal_Sequence_Option> Sequence) {
    this.alts = alts;
    this.Sequence = Sequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnLocal)) {
      return false;
    }
    PnLocal o = (PnLocal) other;
    return java.util.Objects.equals(
      this.alts,
      o.alts) && java.util.Objects.equals(
      this.Sequence,
      o.Sequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(alts) + 3 * java.util.Objects.hashCode(Sequence);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PnLocal other) {
    int cmp = 0;
    cmp = ((Comparable) alts).compareTo(other.alts);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) Sequence).compareTo(other.Sequence);
  }
  
  public PnLocal withAlts(hydra.ext.io.shex.syntax.PnLocal_Alts alts) {
    return new PnLocal(alts, Sequence);
  }
  
  public PnLocal withSequence(hydra.util.Maybe<hydra.ext.io.shex.syntax.PnLocal_Sequence_Option> Sequence) {
    return new PnLocal(alts, Sequence);
  }
}
