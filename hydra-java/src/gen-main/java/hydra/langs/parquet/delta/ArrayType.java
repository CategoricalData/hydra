package hydra.langs.parquet.delta;

import java.io.Serializable;

public class ArrayType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/delta.ArrayType");
  
  public final hydra.langs.parquet.delta.DataType elementType;
  
  public final Boolean containsNull;
  
  public ArrayType (hydra.langs.parquet.delta.DataType elementType, Boolean containsNull) {
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
    return new ArrayType(elementType, containsNull);
  }
  
  public ArrayType withContainsNull(Boolean containsNull) {
    return new ArrayType(elementType, containsNull);
  }
}