// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class SortItem implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.SortItem");
  
  public final hydra.langs.cypher.openCypher.Expression expression;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.SortOrder> order;
  
  public SortItem (hydra.langs.cypher.openCypher.Expression expression, hydra.util.Opt<hydra.langs.cypher.openCypher.SortOrder> order) {
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
  
  public SortItem withExpression(hydra.langs.cypher.openCypher.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new SortItem(expression, order);
  }
  
  public SortItem withOrder(hydra.util.Opt<hydra.langs.cypher.openCypher.SortOrder> order) {
    java.util.Objects.requireNonNull((order));
    return new SortItem(expression, order);
  }
}