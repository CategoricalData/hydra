package hydra.ext.relationalModel;

/**
 * A primary key of a relation, specified either as a single column, or as a list of columns
 */
public class PrimaryKey {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/relationalModel.PrimaryKey");
  
  /**
   * A primary key of a relation, specified either as a single column, or as a list of columns
   */
  public final java.util.List<hydra.ext.relationalModel.ColumnName> value;
  
  public PrimaryKey (java.util.List<hydra.ext.relationalModel.ColumnName> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrimaryKey)) {
      return false;
    }
    PrimaryKey o = (PrimaryKey) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}