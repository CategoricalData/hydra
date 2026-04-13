// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class ArrayCreationExpression_PrimitiveArray implements Serializable, Comparable<ArrayCreationExpression_PrimitiveArray> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ArrayCreationExpression_PrimitiveArray");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name DIMS = new hydra.core.Name("dims");

  public static final hydra.core.Name ARRAY = new hydra.core.Name("array");

  public final hydra.java.syntax.PrimitiveTypeWithAnnotations type;

  public final java.util.List<hydra.java.syntax.Dims> dims;

  public final hydra.java.syntax.ArrayInitializer array;

  public ArrayCreationExpression_PrimitiveArray (hydra.java.syntax.PrimitiveTypeWithAnnotations type, java.util.List<hydra.java.syntax.Dims> dims, hydra.java.syntax.ArrayInitializer array) {
    this.type = type;
    this.dims = dims;
    this.array = array;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayCreationExpression_PrimitiveArray)) {
      return false;
    }
    ArrayCreationExpression_PrimitiveArray o = (ArrayCreationExpression_PrimitiveArray) other;
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
  public int compareTo(ArrayCreationExpression_PrimitiveArray other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      dims,
      other.dims);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      array,
      other.array);
  }

  public ArrayCreationExpression_PrimitiveArray withType(hydra.java.syntax.PrimitiveTypeWithAnnotations type) {
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }

  public ArrayCreationExpression_PrimitiveArray withDims(java.util.List<hydra.java.syntax.Dims> dims) {
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }

  public ArrayCreationExpression_PrimitiveArray withArray(hydra.java.syntax.ArrayInitializer array) {
    return new ArrayCreationExpression_PrimitiveArray(type, dims, array);
  }
}
