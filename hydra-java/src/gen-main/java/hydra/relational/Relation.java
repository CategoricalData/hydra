// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

/**
 * A set of distinct n-tuples; a table
 */
public class Relation<V> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.relational.Relation");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.relational.Row<Object>> value;
  
  public Relation (java.util.List<hydra.relational.Row<Object>> value) {
    java.util.Objects.requireNonNull((value));
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
