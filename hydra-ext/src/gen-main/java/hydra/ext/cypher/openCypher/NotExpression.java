// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class NotExpression implements Serializable, Comparable<NotExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.NotExpression");
  
  public static final hydra.core.Name NOT = new hydra.core.Name("not");
  
  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");
  
  public final Boolean not;
  
  public final hydra.ext.cypher.openCypher.ComparisonExpression expression;
  
  public NotExpression (Boolean not, hydra.ext.cypher.openCypher.ComparisonExpression expression) {
    this.not = not;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NotExpression)) {
      return false;
    }
    NotExpression o = (NotExpression) other;
    return java.util.Objects.equals(
      this.not,
      o.not) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(not) + 3 * java.util.Objects.hashCode(expression);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NotExpression other) {
    int cmp = 0;
    cmp = ((Comparable) not).compareTo(other.not);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) expression).compareTo(other.expression);
  }
  
  public NotExpression withNot(Boolean not) {
    return new NotExpression(not, expression);
  }
  
  public NotExpression withExpression(hydra.ext.cypher.openCypher.ComparisonExpression expression) {
    return new NotExpression(not, expression);
  }
}
