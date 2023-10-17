package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class SortItem implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.SortItem");
  
  public final hydra.langs.cypher.openCypher.Expression sortBy;
  
  public final Boolean descending;
  
  public SortItem (hydra.langs.cypher.openCypher.Expression sortBy, Boolean descending) {
    this.sortBy = sortBy;
    this.descending = descending;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SortItem)) {
      return false;
    }
    SortItem o = (SortItem) (other);
    return sortBy.equals(o.sortBy) && descending.equals(o.descending);
  }
  
  @Override
  public int hashCode() {
    return 2 * sortBy.hashCode() + 3 * descending.hashCode();
  }
  
  public SortItem withSortBy(hydra.langs.cypher.openCypher.Expression sortBy) {
    return new SortItem(sortBy, descending);
  }
  
  public SortItem withDescending(Boolean descending) {
    return new SortItem(sortBy, descending);
  }
}