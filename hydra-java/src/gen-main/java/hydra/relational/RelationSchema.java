// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

import java.io.Serializable;

/**
 * An abstract relation; the name and columns of a relation without its actual data
 */
public class RelationSchema<T> implements Serializable, Comparable<RelationSchema<T>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.relational.RelationSchema");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_COLUMNS = new hydra.core.Name("columns");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY_KEYS = new hydra.core.Name("primaryKeys");
  
  public static final hydra.core.Name FIELD_NAME_FOREIGN_KEYS = new hydra.core.Name("foreignKeys");
  
  /**
   * A unique name for the relation
   */
  public final hydra.relational.RelationName name;
  
  /**
   * A list of column specifications
   */
  public final java.util.List<hydra.relational.ColumnSchema<T>> columns;
  
  /**
   * Any number of primary keys for the relation, each of which must be valid for this relation
   */
  public final java.util.List<hydra.relational.PrimaryKey> primaryKeys;
  
  /**
   * Any number of foreign keys, each of which must be valid for both this relation and the target relation
   */
  public final java.util.List<hydra.relational.ForeignKey> foreignKeys;
  
  public RelationSchema (hydra.relational.RelationName name, java.util.List<hydra.relational.ColumnSchema<T>> columns, java.util.List<hydra.relational.PrimaryKey> primaryKeys, java.util.List<hydra.relational.ForeignKey> foreignKeys) {
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
    RelationSchema o = (RelationSchema) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.columns,
      o.columns) && java.util.Objects.equals(
      this.primaryKeys,
      o.primaryKeys) && java.util.Objects.equals(
      this.foreignKeys,
      o.foreignKeys);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(columns) + 5 * java.util.Objects.hashCode(primaryKeys) + 7 * java.util.Objects.hashCode(foreignKeys);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RelationSchema other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      columns.hashCode(),
      other.columns.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      primaryKeys.hashCode(),
      other.primaryKeys.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      foreignKeys.hashCode(),
      other.foreignKeys.hashCode());
  }
  
  public RelationSchema withName(hydra.relational.RelationName name) {
    return new RelationSchema(name, columns, primaryKeys, foreignKeys);
  }
  
  public RelationSchema withColumns(java.util.List<hydra.relational.ColumnSchema<T>> columns) {
    return new RelationSchema(name, columns, primaryKeys, foreignKeys);
  }
  
  public RelationSchema withPrimaryKeys(java.util.List<hydra.relational.PrimaryKey> primaryKeys) {
    return new RelationSchema(name, columns, primaryKeys, foreignKeys);
  }
  
  public RelationSchema withForeignKeys(java.util.List<hydra.relational.ForeignKey> foreignKeys) {
    return new RelationSchema(name, columns, primaryKeys, foreignKeys);
  }
}
