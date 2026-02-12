// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Lambda implements Serializable, Comparable<Lambda> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Lambda");
  
  public static final hydra.core.Name FIELD_NAME_PARAMS = new hydra.core.Name("params");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.python.syntax.LambdaParameters params;
  
  public final hydra.ext.python.syntax.Expression body;
  
  public Lambda (hydra.ext.python.syntax.LambdaParameters params, hydra.ext.python.syntax.Expression body) {
    this.params = params;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Lambda)) {
      return false;
    }
    Lambda o = (Lambda) other;
    return java.util.Objects.equals(
      this.params,
      o.params) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(params) + 3 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Lambda other) {
    int cmp = 0;
    cmp = ((Comparable) params).compareTo(other.params);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public Lambda withParams(hydra.ext.python.syntax.LambdaParameters params) {
    return new Lambda(params, body);
  }
  
  public Lambda withBody(hydra.ext.python.syntax.Expression body) {
    return new Lambda(params, body);
  }
}
