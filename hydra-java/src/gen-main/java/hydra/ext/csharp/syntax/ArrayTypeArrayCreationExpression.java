// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ArrayTypeArrayCreationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ArrayTypeArrayCreationExpression");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_INITIALIZER = new hydra.core.Name("initializer");
  
  public final hydra.ext.csharp.syntax.ArrayType type;
  
  public final hydra.ext.csharp.syntax.ArrayInitializer initializer;
  
  public ArrayTypeArrayCreationExpression (hydra.ext.csharp.syntax.ArrayType type, hydra.ext.csharp.syntax.ArrayInitializer initializer) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((initializer));
    this.type = type;
    this.initializer = initializer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayTypeArrayCreationExpression)) {
      return false;
    }
    ArrayTypeArrayCreationExpression o = (ArrayTypeArrayCreationExpression) (other);
    return type.equals(o.type) && initializer.equals(o.initializer);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * initializer.hashCode();
  }
  
  public ArrayTypeArrayCreationExpression withType(hydra.ext.csharp.syntax.ArrayType type) {
    java.util.Objects.requireNonNull((type));
    return new ArrayTypeArrayCreationExpression(type, initializer);
  }
  
  public ArrayTypeArrayCreationExpression withInitializer(hydra.ext.csharp.syntax.ArrayInitializer initializer) {
    java.util.Objects.requireNonNull((initializer));
    return new ArrayTypeArrayCreationExpression(type, initializer);
  }
}