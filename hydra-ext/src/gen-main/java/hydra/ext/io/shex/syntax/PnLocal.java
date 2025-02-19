// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class PnLocal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal");
  
  public static final hydra.core.Name FIELD_NAME_ALTS = new hydra.core.Name("alts");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  public final hydra.ext.io.shex.syntax.PnLocal_Alts alts;
  
  public final hydra.util.Opt<hydra.ext.io.shex.syntax.PnLocal_Sequence_Option> sequence;
  
  public PnLocal (hydra.ext.io.shex.syntax.PnLocal_Alts alts, hydra.util.Opt<hydra.ext.io.shex.syntax.PnLocal_Sequence_Option> sequence) {
    java.util.Objects.requireNonNull((alts));
    java.util.Objects.requireNonNull((sequence));
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
  
  public PnLocal withAlts(hydra.ext.io.shex.syntax.PnLocal_Alts alts) {
    java.util.Objects.requireNonNull((alts));
    return new PnLocal(alts, sequence);
  }
  
  public PnLocal withSequence(hydra.util.Opt<hydra.ext.io.shex.syntax.PnLocal_Sequence_Option> sequence) {
    java.util.Objects.requireNonNull((sequence));
    return new PnLocal(alts, sequence);
  }
}