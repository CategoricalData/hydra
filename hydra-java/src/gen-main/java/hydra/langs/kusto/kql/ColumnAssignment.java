// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class ColumnAssignment implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/kusto/kql.ColumnAssignment");
  
  public static final hydra.core.Name FIELD_NAME_COLUMN = new hydra.core.Name("column");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.langs.kusto.kql.ColumnName column;
  
  public final hydra.langs.kusto.kql.Expression expression;
  
  public ColumnAssignment (hydra.langs.kusto.kql.ColumnName column, hydra.langs.kusto.kql.Expression expression) {
    java.util.Objects.requireNonNull((column));
    java.util.Objects.requireNonNull((expression));
    this.column = column;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ColumnAssignment)) {
      return false;
    }
    ColumnAssignment o = (ColumnAssignment) (other);
    return column.equals(o.column) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * column.hashCode() + 3 * expression.hashCode();
  }
  
  public ColumnAssignment withColumn(hydra.langs.kusto.kql.ColumnName column) {
    java.util.Objects.requireNonNull((column));
    return new ColumnAssignment(column, expression);
  }
  
  public ColumnAssignment withExpression(hydra.langs.kusto.kql.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new ColumnAssignment(column, expression);
  }
}