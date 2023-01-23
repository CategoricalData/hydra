package hydra.ext.relationalModel;

/**
 * A mapping from certain columns of a source relation to primary key columns of a target relation
 */
public class ForeignKey {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/relationalModel.ForeignKey");
  
  /**
   * The name of the target relation
   */
  public final hydra.ext.relationalModel.RelationName foreignRelation;
  
  public final java.util.Map<hydra.ext.relationalModel.ColumnName, hydra.ext.relationalModel.ColumnName> keys;
  
  public ForeignKey (hydra.ext.relationalModel.RelationName foreignRelation, java.util.Map<hydra.ext.relationalModel.ColumnName, hydra.ext.relationalModel.ColumnName> keys) {
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
  
  public ForeignKey withForeignRelation(hydra.ext.relationalModel.RelationName foreignRelation) {
    return new ForeignKey(foreignRelation, keys);
  }
  
  public ForeignKey withKeys(java.util.Map<hydra.ext.relationalModel.ColumnName, hydra.ext.relationalModel.ColumnName> keys) {
    return new ForeignKey(foreignRelation, keys);
  }
}