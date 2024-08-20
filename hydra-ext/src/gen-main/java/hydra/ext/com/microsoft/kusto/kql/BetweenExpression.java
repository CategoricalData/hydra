// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.kusto.kql;

import java.io.Serializable;

public class BetweenExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/kusto/kql.BetweenExpression");
  
  public static final hydra.core.Name FIELD_NAME_NOT = new hydra.core.Name("not");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_LOWER_BOUND = new hydra.core.Name("lowerBound");
  
  public static final hydra.core.Name FIELD_NAME_UPPER_BOUND = new hydra.core.Name("upperBound");
  
  public final Boolean not;
  
  public final hydra.ext.com.microsoft.kusto.kql.Expression expression;
  
  public final hydra.ext.com.microsoft.kusto.kql.Expression lowerBound;
  
  public final hydra.ext.com.microsoft.kusto.kql.Expression upperBound;
  
  public BetweenExpression (Boolean not, hydra.ext.com.microsoft.kusto.kql.Expression expression, hydra.ext.com.microsoft.kusto.kql.Expression lowerBound, hydra.ext.com.microsoft.kusto.kql.Expression upperBound) {
    java.util.Objects.requireNonNull((not));
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((lowerBound));
    java.util.Objects.requireNonNull((upperBound));
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
    java.util.Objects.requireNonNull((not));
    return new BetweenExpression(not, expression, lowerBound, upperBound);
  }
  
  public BetweenExpression withExpression(hydra.ext.com.microsoft.kusto.kql.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new BetweenExpression(not, expression, lowerBound, upperBound);
  }
  
  public BetweenExpression withLowerBound(hydra.ext.com.microsoft.kusto.kql.Expression lowerBound) {
    java.util.Objects.requireNonNull((lowerBound));
    return new BetweenExpression(not, expression, lowerBound, upperBound);
  }
  
  public BetweenExpression withUpperBound(hydra.ext.com.microsoft.kusto.kql.Expression upperBound) {
    java.util.Objects.requireNonNull((upperBound));
    return new BetweenExpression(not, expression, lowerBound, upperBound);
  }
}