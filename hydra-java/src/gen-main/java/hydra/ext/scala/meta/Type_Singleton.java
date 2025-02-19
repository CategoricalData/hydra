// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Singleton implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Type_Singleton");
  
  public static final hydra.core.Name FIELD_NAME_REF = new hydra.core.Name("ref");
  
  public final hydra.ext.scala.meta.Data_Ref ref;
  
  public Type_Singleton (hydra.ext.scala.meta.Data_Ref ref) {
    java.util.Objects.requireNonNull((ref));
    this.ref = ref;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Singleton)) {
      return false;
    }
    Type_Singleton o = (Type_Singleton) (other);
    return ref.equals(o.ref);
  }
  
  @Override
  public int hashCode() {
    return 2 * ref.hashCode();
  }
}