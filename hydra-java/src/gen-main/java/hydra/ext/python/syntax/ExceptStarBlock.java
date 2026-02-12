// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ExceptStarBlock implements Serializable, Comparable<ExceptStarBlock> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ExceptStarBlock");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.python.syntax.Expression expression;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Name> as;
  
  public final hydra.ext.python.syntax.Block body;
  
  public ExceptStarBlock (hydra.ext.python.syntax.Expression expression, hydra.util.Maybe<hydra.ext.python.syntax.Name> as, hydra.ext.python.syntax.Block body) {
    this.expression = expression;
    this.as = as;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExceptStarBlock)) {
      return false;
    }
    ExceptStarBlock o = (ExceptStarBlock) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.as,
      o.as) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(as) + 5 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExceptStarBlock other) {
    int cmp = 0;
    cmp = ((Comparable) expression).compareTo(other.expression);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      as.hashCode(),
      other.as.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public ExceptStarBlock withExpression(hydra.ext.python.syntax.Expression expression) {
    return new ExceptStarBlock(expression, as, body);
  }
  
  public ExceptStarBlock withAs(hydra.util.Maybe<hydra.ext.python.syntax.Name> as) {
    return new ExceptStarBlock(expression, as, body);
  }
  
  public ExceptStarBlock withBody(hydra.ext.python.syntax.Block body) {
    return new ExceptStarBlock(expression, as, body);
  }
}
