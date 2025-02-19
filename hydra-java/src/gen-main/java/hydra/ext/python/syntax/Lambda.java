// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Lambda implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Lambda");
  
  public static final hydra.core.Name FIELD_NAME_PARAMS = new hydra.core.Name("params");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.python.syntax.LambdaParameters params;
  
  public final hydra.ext.python.syntax.Expression body;
  
  public Lambda (hydra.ext.python.syntax.LambdaParameters params, hydra.ext.python.syntax.Expression body) {
    java.util.Objects.requireNonNull((params));
    java.util.Objects.requireNonNull((body));
    this.params = params;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Lambda)) {
      return false;
    }
    Lambda o = (Lambda) (other);
    return params.equals(o.params) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * params.hashCode() + 3 * body.hashCode();
  }
  
  public Lambda withParams(hydra.ext.python.syntax.LambdaParameters params) {
    java.util.Objects.requireNonNull((params));
    return new Lambda(params, body);
  }
  
  public Lambda withBody(hydra.ext.python.syntax.Expression body) {
    java.util.Objects.requireNonNull((body));
    return new Lambda(params, body);
  }
}