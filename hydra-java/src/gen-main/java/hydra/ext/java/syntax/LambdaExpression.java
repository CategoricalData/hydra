// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class LambdaExpression implements Serializable, Comparable<LambdaExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.LambdaExpression");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
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
    LambdaExpression o = (LambdaExpression) other;
    return java.util.Objects.equals(
      this.parameters,
      o.parameters) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parameters) + 3 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LambdaExpression other) {
    int cmp = 0;
    cmp = ((Comparable) parameters).compareTo(other.parameters);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public LambdaExpression withParameters(hydra.ext.java.syntax.LambdaParameters parameters) {
    return new LambdaExpression(parameters, body);
  }
  
  public LambdaExpression withBody(hydra.ext.java.syntax.LambdaBody body) {
    return new LambdaExpression(parameters, body);
  }
}
