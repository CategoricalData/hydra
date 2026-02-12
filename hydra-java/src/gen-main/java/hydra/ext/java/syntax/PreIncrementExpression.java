// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class PreIncrementExpression implements Serializable, Comparable<PreIncrementExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.PreIncrementExpression");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.java.syntax.UnaryExpression value;
  
  public PreIncrementExpression (hydra.ext.java.syntax.UnaryExpression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PreIncrementExpression)) {
      return false;
    }
    PreIncrementExpression o = (PreIncrementExpression) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PreIncrementExpression other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
