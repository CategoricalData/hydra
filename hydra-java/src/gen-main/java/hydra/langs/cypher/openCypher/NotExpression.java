package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class NotExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.NotExpression");
  
  public final Boolean not;
  
  public final hydra.langs.cypher.openCypher.ComparisonExpression expression;
  
  public NotExpression (Boolean not, hydra.langs.cypher.openCypher.ComparisonExpression expression) {
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
    return new NotExpression(not, expression);
  }
  
  public NotExpression withExpression(hydra.langs.cypher.openCypher.ComparisonExpression expression) {
    return new NotExpression(not, expression);
  }
}