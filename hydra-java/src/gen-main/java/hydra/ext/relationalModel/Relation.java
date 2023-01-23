package hydra.ext.relationalModel;

/**
 * A set of distinct n-tuples; a table
 */
public class Relation<V> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/relationalModel.Relation");
  
  public final java.util.Set<java.util.List<V>> value;
  
  public Relation (java.util.Set<java.util.List<V>> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Relation)) {
      return false;
    }
    Relation o = (Relation) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}