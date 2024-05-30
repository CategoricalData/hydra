package hydra.langs.kusto.kql;

import java.io.Serializable;

public class TopCommand implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.TopCommand");
  
  public final Integer count;
  
  public final java.util.List<hydra.langs.kusto.kql.SortBy> sort;
  
  public TopCommand (Integer count, java.util.List<hydra.langs.kusto.kql.SortBy> sort) {
    this.count = count;
    this.sort = sort;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TopCommand)) {
      return false;
    }
    TopCommand o = (TopCommand) (other);
    return count.equals(o.count) && sort.equals(o.sort);
  }
  
  @Override
  public int hashCode() {
    return 2 * count.hashCode() + 3 * sort.hashCode();
  }
  
  public TopCommand withCount(Integer count) {
    return new TopCommand(count, sort);
  }
  
  public TopCommand withSort(java.util.List<hydra.langs.kusto.kql.SortBy> sort) {
    return new TopCommand(count, sort);
  }
}