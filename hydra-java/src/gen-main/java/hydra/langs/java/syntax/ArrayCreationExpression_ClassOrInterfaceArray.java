// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ArrayCreationExpression_ClassOrInterfaceArray implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.ArrayCreationExpression.ClassOrInterfaceArray");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DIMS = new hydra.core.Name("dims");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY = new hydra.core.Name("array");
  
  public final hydra.langs.java.syntax.ClassOrInterfaceType type;
  
  public final java.util.List<hydra.langs.java.syntax.Dims> dims;
  
  public final hydra.langs.java.syntax.ArrayInitializer array;
  
  public ArrayCreationExpression_ClassOrInterfaceArray (hydra.langs.java.syntax.ClassOrInterfaceType type, java.util.List<hydra.langs.java.syntax.Dims> dims, hydra.langs.java.syntax.ArrayInitializer array) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((dims));
    java.util.Objects.requireNonNull((array));
    this.type = type;
    this.dims = dims;
    this.array = array;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayCreationExpression_ClassOrInterfaceArray)) {
      return false;
    }
    ArrayCreationExpression_ClassOrInterfaceArray o = (ArrayCreationExpression_ClassOrInterfaceArray) (other);
    return type.equals(o.type) && dims.equals(o.dims) && array.equals(o.array);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * dims.hashCode() + 5 * array.hashCode();
  }
  
  public ArrayCreationExpression_ClassOrInterfaceArray withType(hydra.langs.java.syntax.ClassOrInterfaceType type) {
    java.util.Objects.requireNonNull((type));
    return new ArrayCreationExpression_ClassOrInterfaceArray(type, dims, array);
  }
  
  public ArrayCreationExpression_ClassOrInterfaceArray withDims(java.util.List<hydra.langs.java.syntax.Dims> dims) {
    java.util.Objects.requireNonNull((dims));
    return new ArrayCreationExpression_ClassOrInterfaceArray(type, dims, array);
  }
  
  public ArrayCreationExpression_ClassOrInterfaceArray withArray(hydra.langs.java.syntax.ArrayInitializer array) {
    java.util.Objects.requireNonNull((array));
    return new ArrayCreationExpression_ClassOrInterfaceArray(type, dims, array);
  }
}