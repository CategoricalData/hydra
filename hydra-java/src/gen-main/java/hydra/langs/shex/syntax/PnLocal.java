// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class PnLocal implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.PnLocal");
  
  public final hydra.langs.shex.syntax.PnLocal_Alts alts;
  
  public final hydra.util.Opt<hydra.langs.shex.syntax.PnLocal_Sequence_Option> sequence;
  
  public PnLocal (hydra.langs.shex.syntax.PnLocal_Alts alts, hydra.util.Opt<hydra.langs.shex.syntax.PnLocal_Sequence_Option> sequence) {
    if (alts == null) {
      throw new IllegalArgumentException("null value for 'alts' argument");
    }
    if (sequence == null) {
      throw new IllegalArgumentException("null value for 'sequence' argument");
    }
    this.alts = alts;
    this.sequence = sequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnLocal)) {
      return false;
    }
    PnLocal o = (PnLocal) (other);
    return alts.equals(o.alts) && sequence.equals(o.sequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * alts.hashCode() + 3 * sequence.hashCode();
  }
  
  public PnLocal withAlts(hydra.langs.shex.syntax.PnLocal_Alts alts) {
    if (alts == null) {
      throw new IllegalArgumentException("null value for 'alts' argument");
    }
    return new PnLocal(alts, sequence);
  }
  
  public PnLocal withSequence(hydra.util.Opt<hydra.langs.shex.syntax.PnLocal_Sequence_Option> sequence) {
    if (sequence == null) {
      throw new IllegalArgumentException("null value for 'sequence' argument");
    }
    return new PnLocal(alts, sequence);
  }
}