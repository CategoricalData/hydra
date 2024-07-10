// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ArrayCreationExpression_PrimitiveArray implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.ArrayCreationExpression.PrimitiveArray");
  
  public final hydra.langs.java.syntax.PrimitiveTypeWithAnnotations type;
  
  public final java.util.List<hydra.langs.java.syntax.Dims> dims;
  
  public final hydra.langs.java.syntax.ArrayInitializer array;
  
  public ArrayCreationExpression_PrimitiveArray (hydra.langs.java.syntax.PrimitiveTypeWithAnnotations type, java.util.List<hydra.langs.java.syntax.Dims> dims, hydra.langs.java.syntax.ArrayInitializer array) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    if (dims == null) {
      throw new IllegalArgumentException("null value for 'dims' argument");
    }
    if (array == null) {
      throw new IllegalArgumentException("null value for 'array' argument");
    }
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
  
  public ArrayCreationExpression_PrimitiveArray withType(hydra.langs.java.syntax.PrimitiveTypeWithAnnotations type) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }
  
  public ArrayCreationExpression_PrimitiveArray withDims(java.util.List<hydra.langs.java.syntax.Dims> dims) {
    if (dims == null) {
      throw new IllegalArgumentException("null value for 'dims' argument");
    }
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }
  
  public ArrayCreationExpression_PrimitiveArray withArray(hydra.langs.java.syntax.ArrayInitializer array) {
    if (array == null) {
      throw new IllegalArgumentException("null value for 'array' argument");
    }
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }
}