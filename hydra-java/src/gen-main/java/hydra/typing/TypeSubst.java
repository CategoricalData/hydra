// Note: this is an automatically generated file. Do not edit.

package hydra.typing;

import java.io.Serializable;

/**
 * A substitution of type variables for types
 */
public class TypeSubst implements Serializable, Comparable<TypeSubst> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.typing.TypeSubst");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> value;

  public TypeSubst (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeSubst)) {
      return false;
    }
    TypeSubst o = (TypeSubst) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeSubst other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
