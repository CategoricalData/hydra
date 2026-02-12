// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ArrayType implements Serializable, Comparable<ArrayType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ArrayType");
  
  public static final hydra.core.Name FIELD_NAME_DIMS = new hydra.core.Name("dims");
  
  public static final hydra.core.Name FIELD_NAME_VARIANT = new hydra.core.Name("variant");
  
  public final hydra.ext.java.syntax.Dims dims;
  
  public final hydra.ext.java.syntax.ArrayType_Variant variant;
  
  public ArrayType (hydra.ext.java.syntax.Dims dims, hydra.ext.java.syntax.ArrayType_Variant variant) {
    this.dims = dims;
    this.variant = variant;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayType)) {
      return false;
    }
    ArrayType o = (ArrayType) other;
    return java.util.Objects.equals(
      this.dims,
      o.dims) && java.util.Objects.equals(
      this.variant,
      o.variant);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(dims) + 3 * java.util.Objects.hashCode(variant);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ArrayType other) {
    int cmp = 0;
    cmp = ((Comparable) dims).compareTo(other.dims);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) variant).compareTo(other.variant);
  }
  
  public ArrayType withDims(hydra.ext.java.syntax.Dims dims) {
    return new ArrayType(dims, variant);
  }
  
  public ArrayType withVariant(hydra.ext.java.syntax.ArrayType_Variant variant) {
    return new ArrayType(dims, variant);
  }
}
