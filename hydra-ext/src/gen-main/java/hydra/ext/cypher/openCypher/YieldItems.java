// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class YieldItems implements Serializable, Comparable<YieldItems> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.YieldItems");
  
  public static final hydra.core.Name ITEMS = new hydra.core.Name("items");
  
  public static final hydra.core.Name WHERE = new hydra.core.Name("where");
  
  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.YieldItem> items;
  
  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where;
  
  public YieldItems (hydra.util.ConsList<hydra.ext.cypher.openCypher.YieldItem> items, hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where) {
    this.items = items;
    this.where = where;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof YieldItems)) {
      return false;
    }
    YieldItems o = (YieldItems) other;
    return java.util.Objects.equals(
      this.items,
      o.items) && java.util.Objects.equals(
      this.where,
      o.where);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(items) + 3 * java.util.Objects.hashCode(where);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(YieldItems other) {
    int cmp = 0;
    cmp = ((Comparable) items).compareTo(other.items);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) where).compareTo(other.where);
  }
  
  public YieldItems withItems(hydra.util.ConsList<hydra.ext.cypher.openCypher.YieldItem> items) {
    return new YieldItems(items, where);
  }
  
  public YieldItems withWhere(hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where) {
    return new YieldItems(items, where);
  }
}
