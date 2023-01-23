package hydra.ext.relationalModel;

/**
 * A domain-unordered (string-indexed, rather than position-indexed) relation
 */
public class Relationship<V> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/relationalModel.Relationship");
  
  public final java.util.Set<java.util.Map<hydra.ext.relationalModel.ColumnName, V>> value;
  
  public Relationship (java.util.Set<java.util.Map<hydra.ext.relationalModel.ColumnName, V>> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Relationship)) {
      return false;
    }
    Relationship o = (Relationship) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}