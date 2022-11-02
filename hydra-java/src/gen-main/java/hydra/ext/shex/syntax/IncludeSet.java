package hydra.ext.shex.syntax;

public class IncludeSet {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.IncludeSet");
  
  public final java.util.List<hydra.ext.shex.syntax.ShapeExprLabel> listOfShapeExprLabel;
  
  public IncludeSet (java.util.List<hydra.ext.shex.syntax.ShapeExprLabel> listOfShapeExprLabel) {
    this.listOfShapeExprLabel = listOfShapeExprLabel;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IncludeSet)) {
      return false;
    }
    IncludeSet o = (IncludeSet) (other);
    return listOfShapeExprLabel.equals(o.listOfShapeExprLabel);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfShapeExprLabel.hashCode();
  }
}