// Note: this is an automatically generated file. Do not edit.

package hydra.coders;

/**
 * A function which maps a Hydra type to a symmetric adapter between types and terms
 */
public class TypeAdapter {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.coders.TypeAdapter");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.coders.AdapterContext, hydra.coders.SymmetricAdapter<hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Term>>> value;
  
  public TypeAdapter (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<hydra.coders.AdapterContext, hydra.coders.SymmetricAdapter<hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Term>>> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeAdapter)) {
      return false;
    }
    TypeAdapter o = (TypeAdapter) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
