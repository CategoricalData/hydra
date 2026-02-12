// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class FuncType implements Serializable, Comparable<FuncType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.FuncType");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.python.syntax.TypeExpression> type;
  
  public final hydra.ext.python.syntax.Expression body;
  
  public FuncType (java.util.List<hydra.ext.python.syntax.TypeExpression> type, hydra.ext.python.syntax.Expression body) {
    this.type = type;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FuncType)) {
      return false;
    }
    FuncType o = (FuncType) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FuncType other) {
    int cmp = 0;
    cmp = Integer.compare(
      type.hashCode(),
      other.type.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public FuncType withType(java.util.List<hydra.ext.python.syntax.TypeExpression> type) {
    return new FuncType(type, body);
  }
  
  public FuncType withBody(hydra.ext.python.syntax.Expression body) {
    return new FuncType(type, body);
  }
}
