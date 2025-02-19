// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class PointerElementAccess implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.PointerElementAccess");
  
  public static final hydra.core.Name FIELD_NAME_POINTER = new hydra.core.Name("pointer");
  
  public static final hydra.core.Name FIELD_NAME_INDEX = new hydra.core.Name("index");
  
  public final hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression pointer;
  
  public final hydra.ext.csharp.syntax.Expression index;
  
  public PointerElementAccess (hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression pointer, hydra.ext.csharp.syntax.Expression index) {
    java.util.Objects.requireNonNull((pointer));
    java.util.Objects.requireNonNull((index));
    this.pointer = pointer;
    this.index = index;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PointerElementAccess)) {
      return false;
    }
    PointerElementAccess o = (PointerElementAccess) (other);
    return pointer.equals(o.pointer) && index.equals(o.index);
  }
  
  @Override
  public int hashCode() {
    return 2 * pointer.hashCode() + 3 * index.hashCode();
  }
  
  public PointerElementAccess withPointer(hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression pointer) {
    java.util.Objects.requireNonNull((pointer));
    return new PointerElementAccess(pointer, index);
  }
  
  public PointerElementAccess withIndex(hydra.ext.csharp.syntax.Expression index) {
    java.util.Objects.requireNonNull((index));
    return new PointerElementAccess(pointer, index);
  }
}