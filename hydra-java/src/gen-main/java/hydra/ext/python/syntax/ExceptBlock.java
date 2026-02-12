// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ExceptBlock implements Serializable, Comparable<ExceptBlock> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ExceptBlock");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.ExceptExpression> expression;
  
  public final hydra.ext.python.syntax.Block body;
  
  public ExceptBlock (hydra.util.Maybe<hydra.ext.python.syntax.ExceptExpression> expression, hydra.ext.python.syntax.Block body) {
    this.expression = expression;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExceptBlock)) {
      return false;
    }
    ExceptBlock o = (ExceptBlock) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExceptBlock other) {
    int cmp = 0;
    cmp = Integer.compare(
      expression.hashCode(),
      other.expression.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public ExceptBlock withExpression(hydra.util.Maybe<hydra.ext.python.syntax.ExceptExpression> expression) {
    return new ExceptBlock(expression, body);
  }
  
  public ExceptBlock withBody(hydra.ext.python.syntax.Block body) {
    return new ExceptBlock(expression, body);
  }
}
