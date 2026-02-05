// Note: this is an automatically generated file. Do not edit.

package hydra.tabular;

import java.io.Serializable;

/**
 * A type definition for a table, including column names and types
 */
public class TableType implements Serializable, Comparable<TableType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.tabular.TableType");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_COLUMNS = new hydra.core.Name("columns");
  
  public final hydra.relational.RelationName name;
  
  public final java.util.List<hydra.tabular.ColumnType> columns;
  
  public TableType (hydra.relational.RelationName name, java.util.List<hydra.tabular.ColumnType> columns) {
    this.name = name;
    this.columns = columns;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TableType)) {
      return false;
    }
    TableType o = (TableType) (other);
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.columns,
      o.columns);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(columns);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TableType other) {
    int cmp = 0;
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      columns.hashCode(),
      other.columns.hashCode());
  }
  
  public TableType withName(hydra.relational.RelationName name) {
    return new TableType(name, columns);
  }
  
  public TableType withColumns(java.util.List<hydra.tabular.ColumnType> columns) {
    return new TableType(name, columns);
  }
}
