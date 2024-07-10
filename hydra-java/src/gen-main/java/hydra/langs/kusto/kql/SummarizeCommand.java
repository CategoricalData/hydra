// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class SummarizeCommand implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.SummarizeCommand");
  
  public final java.util.List<hydra.langs.kusto.kql.ColumnAssignment> columns;
  
  public final java.util.List<hydra.langs.kusto.kql.ColumnName> by;
  
  public SummarizeCommand (java.util.List<hydra.langs.kusto.kql.ColumnAssignment> columns, java.util.List<hydra.langs.kusto.kql.ColumnName> by) {
    if (columns == null) {
      throw new IllegalArgumentException("null value for 'columns' argument");
    }
    if (by == null) {
      throw new IllegalArgumentException("null value for 'by' argument");
    }
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
  
  public SummarizeCommand withColumns(java.util.List<hydra.langs.kusto.kql.ColumnAssignment> columns) {
    if (columns == null) {
      throw new IllegalArgumentException("null value for 'columns' argument");
    }
    return new SummarizeCommand(columns, by);
  }
  
  public SummarizeCommand withBy(java.util.List<hydra.langs.kusto.kql.ColumnName> by) {
    if (by == null) {
      throw new IllegalArgumentException("null value for 'by' argument");
    }
    return new SummarizeCommand(columns, by);
  }
}