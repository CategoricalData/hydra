package hydra.ext.shex.syntax;

public class RepeatRange_Sequence_Option {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.RepeatRange.Sequence.Option");
  
  public final java.util.Optional<java.util.Optional<hydra.ext.shex.syntax.RepeatRange_Sequence_Option_Alts_Option_Option>> alts;
  
  public RepeatRange_Sequence_Option (java.util.Optional<java.util.Optional<hydra.ext.shex.syntax.RepeatRange_Sequence_Option_Alts_Option_Option>> alts) {
    this.alts = alts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RepeatRange_Sequence_Option)) {
      return false;
    }
    RepeatRange_Sequence_Option o = (RepeatRange_Sequence_Option) (other);
    return alts.equals(o.alts);
  }
  
  @Override
  public int hashCode() {
    return 2 * alts.hashCode();
  }
}