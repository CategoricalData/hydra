// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class ParseCommand implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/kusto/kql.ParseCommand");
  
  public static final hydra.core.Name FIELD_NAME_COLUMN = new hydra.core.Name("column");
  
  public static final hydra.core.Name FIELD_NAME_PAIRS = new hydra.core.Name("pairs");
  
  public final hydra.langs.kusto.kql.ColumnName column;
  
  public final java.util.List<hydra.langs.kusto.kql.KeyValuePair> pairs;
  
  public ParseCommand (hydra.langs.kusto.kql.ColumnName column, java.util.List<hydra.langs.kusto.kql.KeyValuePair> pairs) {
    java.util.Objects.requireNonNull((column));
    java.util.Objects.requireNonNull((pairs));
    this.column = column;
    this.pairs = pairs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParseCommand)) {
      return false;
    }
    ParseCommand o = (ParseCommand) (other);
    return column.equals(o.column) && pairs.equals(o.pairs);
  }
  
  @Override
  public int hashCode() {
    return 2 * column.hashCode() + 3 * pairs.hashCode();
  }
  
  public ParseCommand withColumn(hydra.langs.kusto.kql.ColumnName column) {
    java.util.Objects.requireNonNull((column));
    return new ParseCommand(column, pairs);
  }
  
  public ParseCommand withPairs(java.util.List<hydra.langs.kusto.kql.KeyValuePair> pairs) {
    java.util.Objects.requireNonNull((pairs));
    return new ParseCommand(column, pairs);
  }
}