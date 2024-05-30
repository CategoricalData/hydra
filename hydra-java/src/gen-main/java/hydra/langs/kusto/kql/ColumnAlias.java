package hydra.langs.kusto.kql;

import java.io.Serializable;

public class ColumnAlias implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.ColumnAlias");
  
  public final hydra.langs.kusto.kql.ColumnName column;
  
  public final hydra.langs.kusto.kql.ColumnName alias;
  
  public ColumnAlias (hydra.langs.kusto.kql.ColumnName column, hydra.langs.kusto.kql.ColumnName alias) {
    this.column = column;
    this.alias = alias;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ColumnAlias)) {
      return false;
    }
    ColumnAlias o = (ColumnAlias) (other);
    return column.equals(o.column) && alias.equals(o.alias);
  }
  
  @Override
  public int hashCode() {
    return 2 * column.hashCode() + 3 * alias.hashCode();
  }
  
  public ColumnAlias withColumn(hydra.langs.kusto.kql.ColumnName column) {
    return new ColumnAlias(column, alias);
  }
  
  public ColumnAlias withAlias(hydra.langs.kusto.kql.ColumnName alias) {
    return new ColumnAlias(column, alias);
  }
}