package hydra.ext.shex.syntax;

public class Include {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.Include");
  
  public final hydra.ext.shex.syntax.TripleExprLabel tripleExprLabel;
  
  public Include (hydra.ext.shex.syntax.TripleExprLabel tripleExprLabel) {
    this.tripleExprLabel = tripleExprLabel;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Include)) {
      return false;
    }
    Include o = (Include) (other);
    return tripleExprLabel.equals(o.tripleExprLabel);
  }
  
  @Override
  public int hashCode() {
    return 2 * tripleExprLabel.hashCode();
  }
}