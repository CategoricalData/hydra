// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ArrayCreationExpression_PrimitiveArray implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DIMS = new hydra.core.Name("dims");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY = new hydra.core.Name("array");
  
  public final hydra.ext.java.syntax.PrimitiveTypeWithAnnotations type;
  
  public final java.util.List<hydra.ext.java.syntax.Dims> dims;
  
  public final hydra.ext.java.syntax.ArrayInitializer array;
  
  public ArrayCreationExpression_PrimitiveArray (hydra.ext.java.syntax.PrimitiveTypeWithAnnotations type, java.util.List<hydra.ext.java.syntax.Dims> dims, hydra.ext.java.syntax.ArrayInitializer array) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((dims));
    java.util.Objects.requireNonNull((array));
    this.type = type;
    this.dims = dims;
    this.array = array;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayCreationExpression_PrimitiveArray)) {
      return false;
    }
    ArrayCreationExpression_PrimitiveArray o = (ArrayCreationExpression_PrimitiveArray) (other);
    return type.equals(o.type) && dims.equals(o.dims) && array.equals(o.array);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * dims.hashCode() + 5 * array.hashCode();
  }
  
  public ArrayCreationExpression_PrimitiveArray withType(hydra.ext.java.syntax.PrimitiveTypeWithAnnotations type) {
    java.util.Objects.requireNonNull((type));
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }
  
  public ArrayCreationExpression_PrimitiveArray withDims(java.util.List<hydra.ext.java.syntax.Dims> dims) {
    java.util.Objects.requireNonNull((dims));
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }
  
  public ArrayCreationExpression_PrimitiveArray withArray(hydra.ext.java.syntax.ArrayInitializer array) {
    java.util.Objects.requireNonNull((array));
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }
}