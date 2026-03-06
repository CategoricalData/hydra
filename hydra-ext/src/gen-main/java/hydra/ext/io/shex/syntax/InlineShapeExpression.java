// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class InlineShapeExpression implements Serializable, Comparable<InlineShapeExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeExpression");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.io.shex.syntax.InlineShapeOr value;
  
  public InlineShapeExpression (hydra.ext.io.shex.syntax.InlineShapeOr value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeExpression)) {
      return false;
    }
    InlineShapeExpression o = (InlineShapeExpression) other;
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
  public int compareTo(InlineShapeExpression other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
