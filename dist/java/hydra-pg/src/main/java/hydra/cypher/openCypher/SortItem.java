// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class SortItem implements Serializable, Comparable<SortItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.SortItem");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name ORDER = new hydra.core.Name("order");

  public final hydra.cypher.openCypher.Expression expression;

  public final hydra.util.Maybe<hydra.cypher.openCypher.SortOrder> order;

  public SortItem (hydra.cypher.openCypher.Expression expression, hydra.util.Maybe<hydra.cypher.openCypher.SortOrder> order) {
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
    cmp = hydra.util.Comparing.compare(
      expression,
      other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      order,
      other.order);
  }

  public SortItem withExpression(hydra.cypher.openCypher.Expression expression) {
    return new SortItem(expression, order);
  }

  public SortItem withOrder(hydra.util.Maybe<hydra.cypher.openCypher.SortOrder> order) {
    return new SortItem(expression, order);
  }
}
