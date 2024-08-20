// Note: this is an automatically generated file. Do not edit.

package hydra.ext.kusto.kql;

import java.io.Serializable;

public class SummarizeCommand implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/kusto/kql.SummarizeCommand");
  
  public static final hydra.core.Name FIELD_NAME_COLUMNS = new hydra.core.Name("columns");
  
  public static final hydra.core.Name FIELD_NAME_BY = new hydra.core.Name("by");
  
  public final java.util.List<hydra.ext.kusto.kql.ColumnAssignment> columns;
  
  public final java.util.List<hydra.ext.kusto.kql.ColumnName> by;
  
  public SummarizeCommand (java.util.List<hydra.ext.kusto.kql.ColumnAssignment> columns, java.util.List<hydra.ext.kusto.kql.ColumnName> by) {
    java.util.Objects.requireNonNull((columns));
    java.util.Objects.requireNonNull((by));
    this.columns = columns;
    this.by = by;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SummarizeCommand)) {
      return false;
    }
    SummarizeCommand o = (SummarizeCommand) (other);
    return columns.equals(o.columns) && by.equals(o.by);
  }
  
  @Override
  public int hashCode() {
    return 2 * columns.hashCode() + 3 * by.hashCode();
  }
  
  public SummarizeCommand withColumns(java.util.List<hydra.ext.kusto.kql.ColumnAssignment> columns) {
    java.util.Objects.requireNonNull((columns));
    return new SummarizeCommand(columns, by);
  }
  
  public SummarizeCommand withBy(java.util.List<hydra.ext.kusto.kql.ColumnName> by) {
    java.util.Objects.requireNonNull((by));
    return new SummarizeCommand(columns, by);
  }
}
