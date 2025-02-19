// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class NotExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.NotExpression");
  
  public static final hydra.core.Name FIELD_NAME_NOT = new hydra.core.Name("not");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final Boolean not;
  
  public final hydra.ext.cypher.openCypher.ComparisonExpression expression;
  
  public NotExpression (Boolean not, hydra.ext.cypher.openCypher.ComparisonExpression expression) {
    java.util.Objects.requireNonNull((not));
    java.util.Objects.requireNonNull((expression));
    this.not = not;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NotExpression)) {
      return false;
    }
    NotExpression o = (NotExpression) (other);
    return not.equals(o.not) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * not.hashCode() + 3 * expression.hashCode();
  }
  
  public NotExpression withNot(Boolean not) {
    java.util.Objects.requireNonNull((not));
    return new NotExpression(not, expression);
  }
  
  public NotExpression withExpression(hydra.ext.cypher.openCypher.ComparisonExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new NotExpression(not, expression);
  }
}