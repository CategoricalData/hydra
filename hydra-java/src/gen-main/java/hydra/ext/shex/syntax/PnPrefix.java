package hydra.ext.shex.syntax;

public class PnPrefix {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.PnPrefix");
  
  public final hydra.ext.shex.syntax.PnCharsBase pnCharsBase;
  
  public final java.util.Optional<hydra.ext.shex.syntax.PnPrefix_Sequence_Option> sequence;
  
  public PnPrefix (hydra.ext.shex.syntax.PnCharsBase pnCharsBase, java.util.Optional<hydra.ext.shex.syntax.PnPrefix_Sequence_Option> sequence) {
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
    return new PnPrefix(pnCharsBase, sequence);
  }
  
  public PnPrefix withSequence(java.util.Optional<hydra.ext.shex.syntax.PnPrefix_Sequence_Option> sequence) {
    return new PnPrefix(pnCharsBase, sequence);
  }
}