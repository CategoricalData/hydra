// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.delta;

import java.io.Serializable;

/**
 * Represent array data type.
 */
public class ArrayType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/parquet/delta.ArrayType");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENT_TYPE = new hydra.core.Name("elementType");
  
  public static final hydra.core.Name FIELD_NAME_CONTAINS_NULL = new hydra.core.Name("containsNull");
  
  public final hydra.langs.parquet.delta.DataType elementType;
  
  public final Boolean containsNull;
  
  public ArrayType (hydra.langs.parquet.delta.DataType elementType, Boolean containsNull) {
    java.util.Objects.requireNonNull((elementType));
    java.util.Objects.requireNonNull((containsNull));
    this.elementType = elementType;
    this.containsNull = containsNull;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayType)) {
      return false;
    }
    ArrayType o = (ArrayType) (other);
    return elementType.equals(o.elementType) && containsNull.equals(o.containsNull);
  }
  
  @Override
  public int hashCode() {
    return 2 * elementType.hashCode() + 3 * containsNull.hashCode();
  }
  
  public ArrayType withElementType(hydra.langs.parquet.delta.DataType elementType) {
    java.util.Objects.requireNonNull((elementType));
    return new ArrayType(elementType, containsNull);
  }
  
  public ArrayType withContainsNull(Boolean containsNull) {
    java.util.Objects.requireNonNull((containsNull));
    return new ArrayType(elementType, containsNull);
  }
}