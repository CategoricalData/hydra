package hydra.langs.kusto.kql;

import java.io.Serializable;

public class BetweenExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.BetweenExpression");
  
  public final Boolean not;
  
  public final hydra.langs.kusto.kql.Expression expression;
  
  public final hydra.langs.kusto.kql.Expression lowerBound;
  
  public final hydra.langs.kusto.kql.Expression upperBound;
  
  public BetweenExpression (Boolean not, hydra.langs.kusto.kql.Expression expression, hydra.langs.kusto.kql.Expression lowerBound, hydra.langs.kusto.kql.Expression upperBound) {
    this.not = not;
    this.expression = expression;
    this.lowerBound = lowerBound;
    this.upperBound = upperBound;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BetweenExpression)) {
      return false;
    }
    BetweenExpression o = (BetweenExpression) (other);
    return not.equals(o.not) && expression.equals(o.expression) && lowerBound.equals(o.lowerBound) && upperBound.equals(o.upperBound);
  }
  
  @Override
  public int hashCode() {
    return 2 * not.hashCode() + 3 * expression.hashCode() + 5 * lowerBound.hashCode() + 7 * upperBound.hashCode();
  }
  
  public BetweenExpression withNot(Boolean not) {
    return new BetweenExpression(not, expression, lowerBound, upperBound);
  }
  
  public BetweenExpression withExpression(hydra.langs.kusto.kql.Expression expression) {
    return new BetweenExpression(not, expression, lowerBound, upperBound);
  }
  
  public BetweenExpression withLowerBound(hydra.langs.kusto.kql.Expression lowerBound) {
    return new BetweenExpression(not, expression, lowerBound, upperBound);
  }
  
  public BetweenExpression withUpperBound(hydra.langs.kusto.kql.Expression upperBound) {
    return new BetweenExpression(not, expression, lowerBound, upperBound);
  }
}