// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class PnPrefix implements Serializable, Comparable<PnPrefix> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.PnPrefix");
  
  public static final hydra.core.Name PN_CHARS_BASE = new hydra.core.Name("PnCharsBase");
  
  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("Sequence");
  
  public final hydra.ext.io.shex.syntax.PnCharsBase PnCharsBase;
  
  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option> Sequence;
  
  public PnPrefix (hydra.ext.io.shex.syntax.PnCharsBase PnCharsBase, hydra.util.Maybe<hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option> Sequence) {
    this.PnCharsBase = PnCharsBase;
    this.Sequence = Sequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnPrefix)) {
      return false;
    }
    PnPrefix o = (PnPrefix) other;
    return java.util.Objects.equals(
      this.PnCharsBase,
      o.PnCharsBase) && java.util.Objects.equals(
      this.Sequence,
      o.Sequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(PnCharsBase) + 3 * java.util.Objects.hashCode(Sequence);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PnPrefix other) {
    int cmp = 0;
    cmp = ((Comparable) PnCharsBase).compareTo(other.PnCharsBase);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      Sequence.hashCode(),
      other.Sequence.hashCode());
  }
  
  public PnPrefix withPnCharsBase(hydra.ext.io.shex.syntax.PnCharsBase PnCharsBase) {
    return new PnPrefix(PnCharsBase, Sequence);
  }
  
  public PnPrefix withSequence(hydra.util.Maybe<hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option> Sequence) {
    return new PnPrefix(PnCharsBase, Sequence);
  }
}
