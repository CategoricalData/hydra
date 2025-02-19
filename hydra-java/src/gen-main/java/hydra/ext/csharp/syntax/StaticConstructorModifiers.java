// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class StaticConstructorModifiers implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.StaticConstructorModifiers");
  
  public static final hydra.core.Name FIELD_NAME_EXTERN = new hydra.core.Name("extern");
  
  public static final hydra.core.Name FIELD_NAME_UNSAFE = new hydra.core.Name("unsafe");
  
  public final Boolean extern;
  
  public final Boolean unsafe;
  
  public StaticConstructorModifiers (Boolean extern, Boolean unsafe) {
    java.util.Objects.requireNonNull((extern));
    java.util.Objects.requireNonNull((unsafe));
    this.extern = extern;
    this.unsafe = unsafe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StaticConstructorModifiers)) {
      return false;
    }
    StaticConstructorModifiers o = (StaticConstructorModifiers) (other);
    return extern.equals(o.extern) && unsafe.equals(o.unsafe);
  }
  
  @Override
  public int hashCode() {
    return 2 * extern.hashCode() + 3 * unsafe.hashCode();
  }
  
  public StaticConstructorModifiers withExtern(Boolean extern) {
    java.util.Objects.requireNonNull((extern));
    return new StaticConstructorModifiers(extern, unsafe);
  }
  
  public StaticConstructorModifiers withUnsafe(Boolean unsafe) {
    java.util.Objects.requireNonNull((unsafe));
    return new StaticConstructorModifiers(extern, unsafe);
  }
}