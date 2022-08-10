package hydra.ext.java.syntax;

public class ArrayCreationExpression_PrimitiveArray {
  public final PrimitiveTypeWithAnnotations type;
  
  public final java.util.List<Dims> dims;
  
  public final ArrayInitializer array;
  
  public ArrayCreationExpression_PrimitiveArray (PrimitiveTypeWithAnnotations type, java.util.List<Dims> dims, ArrayInitializer array) {
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
  
  public ArrayCreationExpression_PrimitiveArray withType(PrimitiveTypeWithAnnotations type) {
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }
  
  public ArrayCreationExpression_PrimitiveArray withDims(java.util.List<Dims> dims) {
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }
  
  public ArrayCreationExpression_PrimitiveArray withArray(ArrayInitializer array) {
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }
}