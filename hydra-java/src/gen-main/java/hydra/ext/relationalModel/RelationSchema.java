// Note: this is an automatically generated file. Do not edit.

package hydra.ext.relationalModel;

import java.io.Serializable;

/**
 * An abstract relation; the name and columns of a relation without its actual data
 */
public class RelationSchema<T> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.relationalModel.RelationSchema");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_COLUMNS = new hydra.core.Name("columns");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY_KEYS = new hydra.core.Name("primaryKeys");
  
  public static final hydra.core.Name FIELD_NAME_FOREIGN_KEYS = new hydra.core.Name("foreignKeys");
  
  /**
   * A unique name for the relation
   */
  public final hydra.ext.relationalModel.RelationName name;
  
  /**
   * A list of column specifications
   */
  public final java.util.List<hydra.ext.relationalModel.ColumnSchema<T>> columns;
  
  /**
   * Any number of primary keys for the relation, each of which must be valid for this relation
   */
  public final java.util.List<hydra.ext.relationalModel.PrimaryKey> primaryKeys;
  
  /**
   * Any number of foreign keys, each of which must be valid for both this relation and the target relation
   */
  public final java.util.List<hydra.ext.relationalModel.ForeignKey> foreignKeys;
  
  public RelationSchema (hydra.ext.relationalModel.RelationName name, java.util.List<hydra.ext.relationalModel.ColumnSchema<T>> columns, java.util.List<hydra.ext.relationalModel.PrimaryKey> primaryKeys, java.util.List<hydra.ext.relationalModel.ForeignKey> foreignKeys) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((columns));
    java.util.Objects.requireNonNull((primaryKeys));
    java.util.Objects.requireNonNull((foreignKeys));
    this.name = name;
    this.columns = columns;
    this.primaryKeys = primaryKeys;
    this.foreignKeys = foreignKeys;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationSchema)) {
      return false;
    }
    RelationSchema o = (RelationSchema) (other);
    return name.equals(o.name) && columns.equals(o.columns) && primaryKeys.equals(o.primaryKeys) && foreignKeys.equals(o.foreignKeys);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * columns.hashCode() + 5 * primaryKeys.hashCode() + 7 * foreignKeys.hashCode();
  }
  
  public RelationSchema withName(hydra.ext.relationalModel.RelationName name) {
    java.util.Objects.requireNonNull((name));
    return new RelationSchema(name, columns, primaryKeys, foreignKeys);
  }
  
  public RelationSchema withColumns(java.util.List<hydra.ext.relationalModel.ColumnSchema<T>> columns) {
    java.util.Objects.requireNonNull((columns));
    return new RelationSchema(name, columns, primaryKeys, foreignKeys);
  }
  
  public RelationSchema withPrimaryKeys(java.util.List<hydra.ext.relationalModel.PrimaryKey> primaryKeys) {
    java.util.Objects.requireNonNull((primaryKeys));
    return new RelationSchema(name, columns, primaryKeys, foreignKeys);
  }
  
  public RelationSchema withForeignKeys(java.util.List<hydra.ext.relationalModel.ForeignKey> foreignKeys) {
    java.util.Objects.requireNonNull((foreignKeys));
    return new RelationSchema(name, columns, primaryKeys, foreignKeys);
  }
}