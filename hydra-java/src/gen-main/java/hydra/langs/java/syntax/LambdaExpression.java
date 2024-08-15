// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class LambdaExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.LambdaExpression");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.langs.java.syntax.LambdaParameters parameters;
  
  public final hydra.langs.java.syntax.LambdaBody body;
  
  public LambdaExpression (hydra.langs.java.syntax.LambdaParameters parameters, hydra.langs.java.syntax.LambdaBody body) {
    java.util.Objects.requireNonNull((parameters));
    java.util.Objects.requireNonNull((body));
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
  
  public LambdaExpression withParameters(hydra.langs.java.syntax.LambdaParameters parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new LambdaExpression(parameters, body);
  }
  
  public LambdaExpression withBody(hydra.langs.java.syntax.LambdaBody body) {
    java.util.Objects.requireNonNull((body));
    return new LambdaExpression(parameters, body);
  }
}