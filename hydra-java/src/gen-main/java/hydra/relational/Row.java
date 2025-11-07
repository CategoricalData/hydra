// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

/**
 * An n-tuple which is an element of a given relation
 */
public class Row<V> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.relational.Row");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<V> value;
  
  public Row (java.util.List<V> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Row)) {
      return false;
    }
    Row o = (Row) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
