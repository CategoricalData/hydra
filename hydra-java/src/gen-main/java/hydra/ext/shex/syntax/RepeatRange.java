package hydra.ext.shex.syntax;

public class RepeatRange {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.RepeatRange");
  
  public final hydra.ext.shex.syntax.Integer_ integer;
  
  public final java.util.Optional<hydra.ext.shex.syntax.RepeatRange_Sequence_Option> sequence;
  
  public RepeatRange (hydra.ext.shex.syntax.Integer_ integer, java.util.Optional<hydra.ext.shex.syntax.RepeatRange_Sequence_Option> sequence) {
    this.integer = integer;
    this.sequence = sequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RepeatRange)) {
      return false;
    }
    RepeatRange o = (RepeatRange) (other);
    return integer.equals(o.integer) && sequence.equals(o.sequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * integer.hashCode() + 3 * sequence.hashCode();
  }
  
  public RepeatRange withInteger(hydra.ext.shex.syntax.Integer_ integer) {
    return new RepeatRange(integer, sequence);
  }
  
  public RepeatRange withSequence(java.util.Optional<hydra.ext.shex.syntax.RepeatRange_Sequence_Option> sequence) {
    return new RepeatRange(integer, sequence);
  }
}