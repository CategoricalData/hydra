// Note: this is an automatically generated file. Do not edit.

package hydra.coders;

/**
 * A bidirectional encoder which maps between the same type and term languages on either side
 */
public class SymmetricAdapter<S, T, V> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.coders.SymmetricAdapter");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.compute.Adapter<Object, Object, Object, Object, Object, Object> value;
  
  public SymmetricAdapter (hydra.compute.Adapter<Object, Object, Object, Object, Object, Object> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SymmetricAdapter)) {
      return false;
    }
    SymmetricAdapter o = (SymmetricAdapter) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
