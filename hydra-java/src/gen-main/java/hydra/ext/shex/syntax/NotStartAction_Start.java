package hydra.ext.shex.syntax;

public class NotStartAction_Start {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.NotStartAction.Start");
  
  public final hydra.ext.shex.syntax.ShapeExpression shapeExpression;
  
  public NotStartAction_Start (hydra.ext.shex.syntax.ShapeExpression shapeExpression) {
    this.shapeExpression = shapeExpression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NotStartAction_Start)) {
      return false;
    }
    NotStartAction_Start o = (NotStartAction_Start) (other);
    return shapeExpression.equals(o.shapeExpression);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeExpression.hashCode();
  }
}