// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class UnaryTripleExpr_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/shex/syntax.UnaryTripleExpr.Sequence");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  public static final hydra.core.Name FIELD_NAME_ALTS = new hydra.core.Name("alts");
  
  public final hydra.util.Opt<hydra.langs.shex.syntax.TripleExprLabel> sequence;
  
  public final hydra.langs.shex.syntax.UnaryTripleExpr_Sequence_Alts alts;
  
  public UnaryTripleExpr_Sequence (hydra.util.Opt<hydra.langs.shex.syntax.TripleExprLabel> sequence, hydra.langs.shex.syntax.UnaryTripleExpr_Sequence_Alts alts) {
    java.util.Objects.requireNonNull((sequence));
    java.util.Objects.requireNonNull((alts));
    this.sequence = sequence;
    this.alts = alts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnaryTripleExpr_Sequence)) {
      return false;
    }
    UnaryTripleExpr_Sequence o = (UnaryTripleExpr_Sequence) (other);
    return sequence.equals(o.sequence) && alts.equals(o.alts);
  }
  
  @Override
  public int hashCode() {
    return 2 * sequence.hashCode() + 3 * alts.hashCode();
  }
  
  public UnaryTripleExpr_Sequence withSequence(hydra.util.Opt<hydra.langs.shex.syntax.TripleExprLabel> sequence) {
    java.util.Objects.requireNonNull((sequence));
    return new UnaryTripleExpr_Sequence(sequence, alts);
  }
  
  public UnaryTripleExpr_Sequence withAlts(hydra.langs.shex.syntax.UnaryTripleExpr_Sequence_Alts alts) {
    java.util.Objects.requireNonNull((alts));
    return new UnaryTripleExpr_Sequence(sequence, alts);
  }
}