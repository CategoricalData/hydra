// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Type_Singleton implements Serializable, Comparable<Type_Singleton> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Type_Singleton");

  public static final hydra.core.Name REF = new hydra.core.Name("ref");

  public final hydra.scala.syntax.Data_Ref ref;

  public Type_Singleton (hydra.scala.syntax.Data_Ref ref) {
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
    return hydra.util.Comparing.compare(
      ref,
      other.ref);
  }
}
