package hydra.ext.java.syntax;

public class LambdaExpression {
  public final LambdaParameters parameters;
  
  public final LambdaBody body;
  
  public LambdaExpression (LambdaParameters parameters, LambdaBody body) {
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
  
  public LambdaExpression withParameters(LambdaParameters parameters) {
    return new LambdaExpression(parameters, body);
  }
  
  public LambdaExpression withBody(LambdaBody body) {
    return new LambdaExpression(parameters, body);
  }
}