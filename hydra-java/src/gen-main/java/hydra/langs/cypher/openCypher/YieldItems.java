// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class YieldItems implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.YieldItems");
  
  public final java.util.List<hydra.langs.cypher.openCypher.YieldItem> items;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where;
  
  public YieldItems (java.util.List<hydra.langs.cypher.openCypher.YieldItem> items, hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where) {
    if (items == null) {
      throw new IllegalArgumentException("null value for 'items' argument");
    }
    if (where == null) {
      throw new IllegalArgumentException("null value for 'where' argument");
    }
    this.items = items;
    this.where = where;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof YieldItems)) {
      return false;
    }
    YieldItems o = (YieldItems) (other);
    return items.equals(o.items) && where.equals(o.where);
  }
  
  @Override
  public int hashCode() {
    return 2 * items.hashCode() + 3 * where.hashCode();
  }
  
  public YieldItems withItems(java.util.List<hydra.langs.cypher.openCypher.YieldItem> items) {
    if (items == null) {
      throw new IllegalArgumentException("null value for 'items' argument");
    }
    return new YieldItems(items, where);
  }
  
  public YieldItems withWhere(hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where) {
    if (where == null) {
      throw new IllegalArgumentException("null value for 'where' argument");
    }
    return new YieldItems(items, where);
  }
}