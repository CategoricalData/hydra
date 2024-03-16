package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Order implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Order");
  
  public final java.util.List<hydra.langs.cypher.openCypher.SortItem> value;
  
  public Order (java.util.List<hydra.langs.cypher.openCypher.SortItem> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Order)) {
      return false;
    }
    Order o = (Order) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}