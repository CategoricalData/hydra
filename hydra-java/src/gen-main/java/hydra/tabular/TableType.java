// Note: this is an automatically generated file. Do not edit.

package hydra.tabular;

import java.io.Serializable;

/**
 * A type definition for a table, including column names and types
 */
public class TableType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.tabular.TableType");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_COLUMNS = new hydra.core.Name("columns");
  
  public final hydra.relational.RelationName name;
  
  public final java.util.List<hydra.tabular.ColumnType> columns;
  
  public TableType (hydra.relational.RelationName name, java.util.List<hydra.tabular.ColumnType> columns) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((columns));
    this.name = name;
    this.columns = columns;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TableType)) {
      return false;
    }
    TableType o = (TableType) (other);
    return name.equals(o.name) && columns.equals(o.columns);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * columns.hashCode();
  }
  
  public TableType withName(hydra.relational.RelationName name) {
    java.util.Objects.requireNonNull((name));
    return new TableType(name, columns);
  }
  
  public TableType withColumns(java.util.List<hydra.tabular.ColumnType> columns) {
    java.util.Objects.requireNonNull((columns));
    return new TableType(name, columns);
  }
}
