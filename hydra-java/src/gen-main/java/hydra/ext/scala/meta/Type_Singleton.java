// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Singleton implements Serializable, Comparable<Type_Singleton> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Type_Singleton");

  public static final hydra.core.Name REF = new hydra.core.Name("ref");

  public final hydra.ext.scala.meta.Data_Ref ref;

  public Type_Singleton (hydra.ext.scala.meta.Data_Ref ref) {
    this.ref = ref;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Singleton)) {
      return false;
    }
    Type_Singleton o = (Type_Singleton) other;
    return java.util.Objects.equals(
      this.ref,
      o.ref);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ref);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Type_Singleton other) {
    return ((Comparable) ref).compareTo(other.ref);
  }
}
