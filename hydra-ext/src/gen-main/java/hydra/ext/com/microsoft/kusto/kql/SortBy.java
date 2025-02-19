// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.kusto.kql;

import java.io.Serializable;

public class SortBy implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.kusto.kql.SortBy");
  
  public static final hydra.core.Name FIELD_NAME_COLUMN = new hydra.core.Name("column");
  
  public static final hydra.core.Name FIELD_NAME_ORDER = new hydra.core.Name("order");
  
  public final hydra.ext.com.microsoft.kusto.kql.ColumnName column;
  
  public final hydra.util.Opt<hydra.ext.com.microsoft.kusto.kql.Order> order;
  
  public SortBy (hydra.ext.com.microsoft.kusto.kql.ColumnName column, hydra.util.Opt<hydra.ext.com.microsoft.kusto.kql.Order> order) {
    java.util.Objects.requireNonNull((column));
    java.util.Objects.requireNonNull((order));
    this.column = column;
    this.order = order;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SortBy)) {
      return false;
    }
    SortBy o = (SortBy) (other);
    return column.equals(o.column) && order.equals(o.order);
  }
  
  @Override
  public int hashCode() {
    return 2 * column.hashCode() + 3 * order.hashCode();
  }
  
  public SortBy withColumn(hydra.ext.com.microsoft.kusto.kql.ColumnName column) {
    java.util.Objects.requireNonNull((column));
    return new SortBy(column, order);
  }
  
  public SortBy withOrder(hydra.util.Opt<hydra.ext.com.microsoft.kusto.kql.Order> order) {
    java.util.Objects.requireNonNull((order));
    return new SortBy(column, order);
  }
}