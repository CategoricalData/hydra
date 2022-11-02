package hydra.ext.shex.syntax;

public class UnaryTripleExpr_Sequence_Sequence_Option {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.UnaryTripleExpr.Sequence.Sequence.Option");
  
  public final hydra.ext.shex.syntax.TripleExprLabel tripleExprLabel;
  
  public UnaryTripleExpr_Sequence_Sequence_Option (hydra.ext.shex.syntax.TripleExprLabel tripleExprLabel) {
    this.tripleExprLabel = tripleExprLabel;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnaryTripleExpr_Sequence_Sequence_Option)) {
      return false;
    }
    UnaryTripleExpr_Sequence_Sequence_Option o = (UnaryTripleExpr_Sequence_Sequence_Option) (other);
    return tripleExprLabel.equals(o.tripleExprLabel);
  }
  
  @Override
  public int hashCode() {
    return 2 * tripleExprLabel.hashCode();
  }
}