package hydra.ext.shex.syntax;

public class TripleExprLabel {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.TripleExprLabel");
  
  public final hydra.ext.shex.syntax.TripleExprLabel_Alts alts;
  
  public TripleExprLabel (hydra.ext.shex.syntax.TripleExprLabel_Alts alts) {
    this.alts = alts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TripleExprLabel)) {
      return false;
    }
    TripleExprLabel o = (TripleExprLabel) (other);
    return alts.equals(o.alts);
  }
  
  @Override
  public int hashCode() {
    return 2 * alts.hashCode();
  }
}