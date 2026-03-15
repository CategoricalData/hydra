// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ArrayCreationExpression_ClassOrInterfaceArray implements Serializable, Comparable<ArrayCreationExpression_ClassOrInterfaceArray> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray");
  
  public static final hydra.core.Name TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name DIMS = new hydra.core.Name("dims");
  
  public static final hydra.core.Name ARRAY = new hydra.core.Name("array");
  
  public final hydra.ext.java.syntax.ClassOrInterfaceType type;
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.Dims> dims;
  
  public final hydra.ext.java.syntax.ArrayInitializer array;
  
  public ArrayCreationExpression_ClassOrInterfaceArray (hydra.ext.java.syntax.ClassOrInterfaceType type, hydra.util.ConsList<hydra.ext.java.syntax.Dims> dims, hydra.ext.java.syntax.ArrayInitializer array) {
    this.type = type;
    this.dims = dims;
    this.array = array;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayCreationExpression_ClassOrInterfaceArray)) {
      return false;
    }
    ArrayCreationExpression_ClassOrInterfaceArray o = (ArrayCreationExpression_ClassOrInterfaceArray) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.dims,
      o.dims) && java.util.Objects.equals(
      this.array,
      o.array);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(dims) + 5 * java.util.Objects.hashCode(array);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ArrayCreationExpression_ClassOrInterfaceArray other) {
    int cmp = 0;
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      dims.hashCode(),
      other.dims.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) array).compareTo(other.array);
  }
  
  public ArrayCreationExpression_ClassOrInterfaceArray withType(hydra.ext.java.syntax.ClassOrInterfaceType type) {
    return new ArrayCreationExpression_ClassOrInterfaceArray(type, dims, array);
  }
  
  public ArrayCreationExpression_ClassOrInterfaceArray withDims(hydra.util.ConsList<hydra.ext.java.syntax.Dims> dims) {
    return new ArrayCreationExpression_ClassOrInterfaceArray(type, dims, array);
  }
  
  public ArrayCreationExpression_ClassOrInterfaceArray withArray(hydra.ext.java.syntax.ArrayInitializer array) {
    return new ArrayCreationExpression_ClassOrInterfaceArray(type, dims, array);
  }
}
