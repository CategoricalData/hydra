package hydra.ext.shex.syntax;

public class UnaryTripleExpr_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.UnaryTripleExpr.Sequence");
  
  public final java.util.Optional<hydra.ext.shex.syntax.TripleExprLabel> sequence;
  
  public final hydra.ext.shex.syntax.UnaryTripleExpr_Sequence_Alts alts;
  
  public UnaryTripleExpr_Sequence (java.util.Optional<hydra.ext.shex.syntax.TripleExprLabel> sequence, hydra.ext.shex.syntax.UnaryTripleExpr_Sequence_Alts alts) {
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
  
  public UnaryTripleExpr_Sequence withSequence(java.util.Optional<hydra.ext.shex.syntax.TripleExprLabel> sequence) {
    return new UnaryTripleExpr_Sequence(sequence, alts);
  }
  
  public UnaryTripleExpr_Sequence withAlts(hydra.ext.shex.syntax.UnaryTripleExpr_Sequence_Alts alts) {
    return new UnaryTripleExpr_Sequence(sequence, alts);
  }
}