// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class RepeatRange implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/shex/syntax.RepeatRange");
  
  public static final hydra.core.Name FIELD_NAME_INTEGER = new hydra.core.Name("integer");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  public final hydra.ext.io.shex.syntax.Integer_ integer;
  
  public final hydra.util.Opt<hydra.util.Opt<hydra.util.Opt<hydra.ext.io.shex.syntax.RepeatRange_Sequence_Option_Option_Option>>> sequence;
  
  public RepeatRange (hydra.ext.io.shex.syntax.Integer_ integer, hydra.util.Opt<hydra.util.Opt<hydra.util.Opt<hydra.ext.io.shex.syntax.RepeatRange_Sequence_Option_Option_Option>>> sequence) {
    java.util.Objects.requireNonNull((integer));
    java.util.Objects.requireNonNull((sequence));
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
  
  public RepeatRange withInteger(hydra.ext.io.shex.syntax.Integer_ integer) {
    java.util.Objects.requireNonNull((integer));
    return new RepeatRange(integer, sequence);
  }
  
  public RepeatRange withSequence(hydra.util.Opt<hydra.util.Opt<hydra.util.Opt<hydra.ext.io.shex.syntax.RepeatRange_Sequence_Option_Option_Option>>> sequence) {
    java.util.Objects.requireNonNull((sequence));
    return new RepeatRange(integer, sequence);
  }
}