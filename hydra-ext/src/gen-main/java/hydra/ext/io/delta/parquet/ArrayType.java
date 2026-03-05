// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.delta.parquet;

import java.io.Serializable;

/**
 * Represent array data type.
 */
public class ArrayType implements Serializable, Comparable<ArrayType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.delta.parquet.ArrayType");
  
  public static final hydra.core.Name ELEMENT_TYPE = new hydra.core.Name("elementType");
  
  public static final hydra.core.Name CONTAINS_NULL = new hydra.core.Name("containsNull");
  
  public final hydra.ext.io.delta.parquet.DataType elementType;
  
  public final Boolean containsNull;
  
  public ArrayType (hydra.ext.io.delta.parquet.DataType elementType, Boolean containsNull) {
    this.elementType = elementType;
    this.containsNull = containsNull;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayType)) {
      return false;
    }
    ArrayType o = (ArrayType) other;
    return java.util.Objects.equals(
      this.elementType,
      o.elementType) && java.util.Objects.equals(
      this.containsNull,
      o.containsNull);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(elementType) + 3 * java.util.Objects.hashCode(containsNull);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ArrayType other) {
    int cmp = 0;
    cmp = ((Comparable) elementType).compareTo(other.elementType);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) containsNull).compareTo(other.containsNull);
  }
  
  public ArrayType withElementType(hydra.ext.io.delta.parquet.DataType elementType) {
    return new ArrayType(elementType, containsNull);
  }
  
  public ArrayType withContainsNull(Boolean containsNull) {
    return new ArrayType(elementType, containsNull);
  }
}
