package hydra.ext.java.syntax;

public class ArrayCreationExpression_ClassOrInterfaceArray {
  public final ClassOrInterfaceType type;
  
  /**
   * Note: list cannot be empty
   */
  public final java.util.List<Dims> dims;
  
  public final ArrayInitializer array;
  
  public ArrayCreationExpression_ClassOrInterfaceArray (ClassOrInterfaceType type, java.util.List<Dims> dims, ArrayInitializer array) {
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
  
  public ArrayCreationExpression_ClassOrInterfaceArray withType(ClassOrInterfaceType type) {
    return new ArrayCreationExpression_ClassOrInterfaceArray(type, dims, array);
  }
  
  public ArrayCreationExpression_ClassOrInterfaceArray withDims(java.util.List<Dims> dims) {
    return new ArrayCreationExpression_ClassOrInterfaceArray(type, dims, array);
  }
  
  public ArrayCreationExpression_ClassOrInterfaceArray withArray(ArrayInitializer array) {
    return new ArrayCreationExpression_ClassOrInterfaceArray(type, dims, array);
  }
}