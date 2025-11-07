// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

import java.io.Serializable;

/**
 * A mapping from certain columns of a source relation to primary key columns of a target relation
 */
public class ForeignKey implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.relational.ForeignKey");
  
  public static final hydra.core.Name FIELD_NAME_FOREIGN_RELATION = new hydra.core.Name("foreignRelation");
  
  public static final hydra.core.Name FIELD_NAME_KEYS = new hydra.core.Name("keys");
  
  /**
   * The name of the target relation
   */
  public final hydra.relational.RelationName foreignRelation;
  
  /**
   * The mapping of source column names to target column names. The target column names must together make up the primary key of the target relation.
   */
  public final java.util.Map<hydra.relational.ColumnName, hydra.relational.ColumnName> keys;
  
  public ForeignKey (hydra.relational.RelationName foreignRelation, java.util.Map<hydra.relational.ColumnName, hydra.relational.ColumnName> keys) {
    java.util.Objects.requireNonNull((foreignRelation));
    java.util.Objects.requireNonNull((keys));
    this.foreignRelation = foreignRelation;
    this.keys = keys;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForeignKey)) {
      return false;
    }
    ForeignKey o = (ForeignKey) (other);
    return foreignRelation.equals(o.foreignRelation) && keys.equals(o.keys);
  }
  
  @Override
  public int hashCode() {
    return 2 * foreignRelation.hashCode() + 3 * keys.hashCode();
  }
  
  public ForeignKey withForeignRelation(hydra.relational.RelationName foreignRelation) {
    java.util.Objects.requireNonNull((foreignRelation));
    return new ForeignKey(foreignRelation, keys);
  }
  
  public ForeignKey withKeys(java.util.Map<hydra.relational.ColumnName, hydra.relational.ColumnName> keys) {
    java.util.Objects.requireNonNull((keys));
    return new ForeignKey(foreignRelation, keys);
  }
}
