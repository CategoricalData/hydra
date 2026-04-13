// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

import java.io.Serializable;

/**
 * A mapping from certain columns of a source relation to primary key columns of a target relation
 */
public class ForeignKey implements Serializable, Comparable<ForeignKey> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.relational.ForeignKey");

  public static final hydra.core.Name FOREIGN_RELATION = new hydra.core.Name("foreignRelation");

  public static final hydra.core.Name KEYS = new hydra.core.Name("keys");

  /**
   * The name of the target relation
   */
  public final hydra.relational.RelationName foreignRelation;

  /**
   * The mapping of source column names to target column names. The target column names must together make up the primary key of the target relation.
   */
  public final java.util.Map<hydra.relational.ColumnName, hydra.relational.ColumnName> keys;

  public ForeignKey (hydra.relational.RelationName foreignRelation, java.util.Map<hydra.relational.ColumnName, hydra.relational.ColumnName> keys) {
    this.foreignRelation = foreignRelation;
    this.keys = keys;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForeignKey)) {
      return false;
    }
    ForeignKey o = (ForeignKey) other;
    return java.util.Objects.equals(
      this.foreignRelation,
      o.foreignRelation) && java.util.Objects.equals(
      this.keys,
      o.keys);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(foreignRelation) + 3 * java.util.Objects.hashCode(keys);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ForeignKey other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      foreignRelation,
      other.foreignRelation);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      keys,
      other.keys);
  }

  public ForeignKey withForeignRelation(hydra.relational.RelationName foreignRelation) {
    return new ForeignKey(foreignRelation, keys);
  }

  public ForeignKey withKeys(java.util.Map<hydra.relational.ColumnName, hydra.relational.ColumnName> keys) {
    return new ForeignKey(foreignRelation, keys);
  }
}
