package hydra.ext.relationalModel;

/**
 * An abstract relation; the name and columns of a relation without its actual data
 */
public class RelationSchema<T> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/relationalModel.RelationSchema");
  
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
    return new RelationSchema(name, columns, primaryKeys, foreignKeys);
  }
  
  public RelationSchema withColumns(java.util.List<hydra.ext.relationalModel.ColumnSchema<T>> columns) {
    return new RelationSchema(name, columns, primaryKeys, foreignKeys);
  }
  
  public RelationSchema withPrimaryKeys(java.util.List<hydra.ext.relationalModel.PrimaryKey> primaryKeys) {
    return new RelationSchema(name, columns, primaryKeys, foreignKeys);
  }
  
  public RelationSchema withForeignKeys(java.util.List<hydra.ext.relationalModel.ForeignKey> foreignKeys) {
    return new RelationSchema(name, columns, primaryKeys, foreignKeys);
  }
}