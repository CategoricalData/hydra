// Note: this is an automatically generated file. Do not edit.

package hydra.ext.kusto.kql;

import java.io.Serializable;

public class PrintCommand implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/kusto/kql.PrintCommand");
  
  public static final hydra.core.Name FIELD_NAME_COLUMN = new hydra.core.Name("column");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.util.Opt<hydra.ext.kusto.kql.ColumnName> column;
  
  public final hydra.ext.kusto.kql.Expression expression;
  
  public PrintCommand (hydra.util.Opt<hydra.ext.kusto.kql.ColumnName> column, hydra.ext.kusto.kql.Expression expression) {
    java.util.Objects.requireNonNull((column));
    java.util.Objects.requireNonNull((expression));
    this.column = column;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrintCommand)) {
      return false;
    }
    PrintCommand o = (PrintCommand) (other);
    return column.equals(o.column) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * column.hashCode() + 3 * expression.hashCode();
  }
  
  public PrintCommand withColumn(hydra.util.Opt<hydra.ext.kusto.kql.ColumnName> column) {
    java.util.Objects.requireNonNull((column));
    return new PrintCommand(column, expression);
  }
  
  public PrintCommand withExpression(hydra.ext.kusto.kql.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new PrintCommand(column, expression);
  }
}