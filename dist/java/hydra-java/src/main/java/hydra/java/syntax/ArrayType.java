// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class ArrayType implements Serializable, Comparable<ArrayType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ArrayType");

  public static final hydra.core.Name DIMS = new hydra.core.Name("dims");

  public static final hydra.core.Name VARIANT = new hydra.core.Name("variant");

  public final hydra.java.syntax.Dims dims;

  public final hydra.java.syntax.ArrayType_Variant variant;

  public ArrayType (hydra.java.syntax.Dims dims, hydra.java.syntax.ArrayType_Variant variant) {
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
    cmp = hydra.util.Comparing.compare(
      dims,
      other.dims);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      variant,
      other.variant);
  }

  public ArrayType withDims(hydra.java.syntax.Dims dims) {
    return new ArrayType(dims, variant);
  }

  public ArrayType withVariant(hydra.java.syntax.ArrayType_Variant variant) {
    return new ArrayType(dims, variant);
  }
}
