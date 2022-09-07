package hydra.ext.java.syntax;

public class ArrayType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ArrayType");
  
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
    ArrayType o = (ArrayType) (other);
    return dims.equals(o.dims) && variant.equals(o.variant);
  }
  
  @Override
  public int hashCode() {
    return 2 * dims.hashCode() + 3 * variant.hashCode();
  }
  
  public ArrayType withDims(hydra.ext.java.syntax.Dims dims) {
    return new ArrayType(dims, variant);
  }
  
  public ArrayType withVariant(hydra.ext.java.syntax.ArrayType_Variant variant) {
    return new ArrayType(dims, variant);
  }
}