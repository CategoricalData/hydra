// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class ColumnAssignment implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.ColumnAssignment");
  
  public final hydra.langs.kusto.kql.ColumnName column;
  
  public final hydra.langs.kusto.kql.Expression expression;
  
  public ColumnAssignment (hydra.langs.kusto.kql.ColumnName column, hydra.langs.kusto.kql.Expression expression) {
    if (column == null) {
      throw new IllegalArgumentException("null value for 'column' argument");
    }
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
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
    if (column == null) {
      throw new IllegalArgumentException("null value for 'column' argument");
    }
    return new ColumnAssignment(column, expression);
  }
  
  public ColumnAssignment withExpression(hydra.langs.kusto.kql.Expression expression) {
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    return new ColumnAssignment(column, expression);
  }
}