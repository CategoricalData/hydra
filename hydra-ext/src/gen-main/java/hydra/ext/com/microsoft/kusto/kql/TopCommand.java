// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.kusto.kql;

import java.io.Serializable;

public class TopCommand implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.kusto.kql.TopCommand");
  
  public static final hydra.core.Name FIELD_NAME_COUNT = new hydra.core.Name("count");
  
  public static final hydra.core.Name FIELD_NAME_SORT = new hydra.core.Name("sort");
  
  public final Integer count;
  
  public final java.util.List<hydra.ext.com.microsoft.kusto.kql.SortBy> sort;
  
  public TopCommand (Integer count, java.util.List<hydra.ext.com.microsoft.kusto.kql.SortBy> sort) {
    java.util.Objects.requireNonNull((count));
    java.util.Objects.requireNonNull((sort));
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
    java.util.Objects.requireNonNull((count));
    return new TopCommand(count, sort);
  }
  
  public TopCommand withSort(java.util.List<hydra.ext.com.microsoft.kusto.kql.SortBy> sort) {
    java.util.Objects.requireNonNull((sort));
    return new TopCommand(count, sort);
  }
}