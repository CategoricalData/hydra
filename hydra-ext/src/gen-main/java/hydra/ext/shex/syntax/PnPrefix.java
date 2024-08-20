// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class PnPrefix implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.PnPrefix");
  
  public static final hydra.core.Name FIELD_NAME_PN_CHARS_BASE = new hydra.core.Name("pnCharsBase");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  public final hydra.ext.shex.syntax.PnCharsBase pnCharsBase;
  
  public final hydra.util.Opt<hydra.ext.shex.syntax.PnPrefix_Sequence_Option> sequence;
  
  public PnPrefix (hydra.ext.shex.syntax.PnCharsBase pnCharsBase, hydra.util.Opt<hydra.ext.shex.syntax.PnPrefix_Sequence_Option> sequence) {
    java.util.Objects.requireNonNull((pnCharsBase));
    java.util.Objects.requireNonNull((sequence));
    this.pnCharsBase = pnCharsBase;
    this.sequence = sequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnPrefix)) {
      return false;
    }
    PnPrefix o = (PnPrefix) (other);
    return pnCharsBase.equals(o.pnCharsBase) && sequence.equals(o.sequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * pnCharsBase.hashCode() + 3 * sequence.hashCode();
  }
  
  public PnPrefix withPnCharsBase(hydra.ext.shex.syntax.PnCharsBase pnCharsBase) {
    java.util.Objects.requireNonNull((pnCharsBase));
    return new PnPrefix(pnCharsBase, sequence);
  }
  
  public PnPrefix withSequence(hydra.util.Opt<hydra.ext.shex.syntax.PnPrefix_Sequence_Option> sequence) {
    java.util.Objects.requireNonNull((sequence));
    return new PnPrefix(pnCharsBase, sequence);
  }
}
