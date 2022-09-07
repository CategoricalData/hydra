package hydra.ext.java.syntax;

public class ArrayCreationExpression_PrimitiveArray {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ArrayCreationExpression.PrimitiveArray");
  
  public final hydra.ext.java.syntax.PrimitiveTypeWithAnnotations type;
  
  public final java.util.List<hydra.ext.java.syntax.Dims> dims;
  
  public final hydra.ext.java.syntax.ArrayInitializer array;
  
  public ArrayCreationExpression_PrimitiveArray (hydra.ext.java.syntax.PrimitiveTypeWithAnnotations type, java.util.List<hydra.ext.java.syntax.Dims> dims, hydra.ext.java.syntax.ArrayInitializer array) {
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
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }
  
  public ArrayCreationExpression_PrimitiveArray withDims(java.util.List<hydra.ext.java.syntax.Dims> dims) {
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }
  
  public ArrayCreationExpression_PrimitiveArray withArray(hydra.ext.java.syntax.ArrayInitializer array) {
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }
}