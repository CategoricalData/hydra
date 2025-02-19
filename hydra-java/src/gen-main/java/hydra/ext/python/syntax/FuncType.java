// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class FuncType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.FuncType");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.python.syntax.TypeExpression> type;
  
  public final hydra.ext.python.syntax.Expression body;
  
  public FuncType (java.util.List<hydra.ext.python.syntax.TypeExpression> type, hydra.ext.python.syntax.Expression body) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((body));
    this.type = type;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FuncType)) {
      return false;
    }
    FuncType o = (FuncType) (other);
    return type.equals(o.type) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * body.hashCode();
  }
  
  public FuncType withType(java.util.List<hydra.ext.python.syntax.TypeExpression> type) {
    java.util.Objects.requireNonNull((type));
    return new FuncType(type, body);
  }
  
  public FuncType withBody(hydra.ext.python.syntax.Expression body) {
    java.util.Objects.requireNonNull((body));
    return new FuncType(type, body);
  }
}