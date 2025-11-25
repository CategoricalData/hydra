// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

/**
 * A domain-unordered (string-indexed, rather than position-indexed) relation
 */
public class Relationship<V> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.relational.Relationship");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.Set<java.util.Map<hydra.relational.ColumnName, Object>> value;
  
  public Relationship (java.util.Set<java.util.Map<hydra.relational.ColumnName, Object>> value) {
    java.util.Objects.requireNonNull((value));
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
