// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class SortItem implements Serializable, Comparable<SortItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.SortItem");
  
  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name ORDER = new hydra.core.Name("order");
  
  public final hydra.ext.cypher.openCypher.Expression expression;
  
  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.SortOrder> order;
  
  public SortItem (hydra.ext.cypher.openCypher.Expression expression, hydra.util.Maybe<hydra.ext.cypher.openCypher.SortOrder> order) {
    this.expression = expression;
    this.order = order;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SortItem)) {
      return false;
    }
    SortItem o = (SortItem) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.order,
      o.order);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(order);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SortItem other) {
    int cmp = 0;
    cmp = ((Comparable) expression).compareTo(other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) order).compareTo(other.order);
  }
  
  public SortItem withExpression(hydra.ext.cypher.openCypher.Expression expression) {
    return new SortItem(expression, order);
  }
  
  public SortItem withOrder(hydra.util.Maybe<hydra.ext.cypher.openCypher.SortOrder> order) {
    return new SortItem(expression, order);
  }
}
