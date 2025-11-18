// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import hydra.util.Maybe;

import java.io.Serializable;

public class SortItem implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.SortItem");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_ORDER = new hydra.core.Name("order");
  
  public final hydra.ext.cypher.openCypher.Expression expression;
  
  public final Maybe<SortOrder> order;
  
  public SortItem (hydra.ext.cypher.openCypher.Expression expression, Maybe<SortOrder> order) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((order));
    this.expression = expression;
    this.order = order;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SortItem)) {
      return false;
    }
    SortItem o = (SortItem) (other);
    return expression.equals(o.expression) && order.equals(o.order);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * order.hashCode();
  }
  
  public SortItem withExpression(hydra.ext.cypher.openCypher.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new SortItem(expression, order);
  }
  
  public SortItem withOrder(Maybe<SortOrder> order) {
    java.util.Objects.requireNonNull((order));
    return new SortItem(expression, order);
  }
}
