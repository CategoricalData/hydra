// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class PostIncrementExpression implements Serializable, Comparable<PostIncrementExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.PostIncrementExpression");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.java.syntax.PostfixExpression value;
  
  public PostIncrementExpression (hydra.ext.java.syntax.PostfixExpression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PostIncrementExpression)) {
      return false;
    }
    PostIncrementExpression o = (PostIncrementExpression) other;
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
  public int compareTo(PostIncrementExpression other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
