package hydra.ext.java.syntax;

public class LambdaExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.LambdaExpression");
  
  public final hydra.ext.java.syntax.LambdaParameters parameters;
  
  public final hydra.ext.java.syntax.LambdaBody body;
  
  public LambdaExpression (hydra.ext.java.syntax.LambdaParameters parameters, hydra.ext.java.syntax.LambdaBody body) {
    this.parameters = parameters;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaExpression)) {
      return false;
    }
    LambdaExpression o = (LambdaExpression) (other);
    return parameters.equals(o.parameters) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameters.hashCode() + 3 * body.hashCode();
  }
  
  public LambdaExpression withParameters(hydra.ext.java.syntax.LambdaParameters parameters) {
    return new LambdaExpression(parameters, body);
  }
  
  public LambdaExpression withBody(hydra.ext.java.syntax.LambdaBody body) {
    return new LambdaExpression(parameters, body);
  }
}