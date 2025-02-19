// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ArrayType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ArrayType");
  
  public static final hydra.core.Name FIELD_NAME_DIMS = new hydra.core.Name("dims");
  
  public static final hydra.core.Name FIELD_NAME_VARIANT = new hydra.core.Name("variant");
  
  public final hydra.ext.java.syntax.Dims dims;
  
  public final hydra.ext.java.syntax.ArrayType_Variant variant;
  
  public ArrayType (hydra.ext.java.syntax.Dims dims, hydra.ext.java.syntax.ArrayType_Variant variant) {
    java.util.Objects.requireNonNull((dims));
    java.util.Objects.requireNonNull((variant));
    this.dims = dims;
    this.variant = variant;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayType)) {
      return false;
    }
    ArrayType o = (ArrayType) (other);
    return dims.equals(o.dims) && variant.equals(o.variant);
  }
  
  @Override
  public int hashCode() {
    return 2 * dims.hashCode() + 3 * variant.hashCode();
  }
  
  public ArrayType withDims(hydra.ext.java.syntax.Dims dims) {
    java.util.Objects.requireNonNull((dims));
    return new ArrayType(dims, variant);
  }
  
  public ArrayType withVariant(hydra.ext.java.syntax.ArrayType_Variant variant) {
    java.util.Objects.requireNonNull((variant));
    return new ArrayType(dims, variant);
  }
}