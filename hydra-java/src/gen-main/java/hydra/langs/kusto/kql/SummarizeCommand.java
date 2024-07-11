// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class SummarizeCommand implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.SummarizeCommand");
  
  public final java.util.List<hydra.langs.kusto.kql.ColumnAssignment> columns;
  
  public final java.util.List<hydra.langs.kusto.kql.ColumnName> by;
  
  public SummarizeCommand (java.util.List<hydra.langs.kusto.kql.ColumnAssignment> columns, java.util.List<hydra.langs.kusto.kql.ColumnName> by) {
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
  
  public SummarizeCommand withColumns(java.util.List<hydra.langs.kusto.kql.ColumnAssignment> columns) {
    java.util.Objects.requireNonNull((columns));
    return new SummarizeCommand(columns, by);
  }
  
  public SummarizeCommand withBy(java.util.List<hydra.langs.kusto.kql.ColumnName> by) {
    java.util.Objects.requireNonNull((by));
    return new SummarizeCommand(columns, by);
  }
}