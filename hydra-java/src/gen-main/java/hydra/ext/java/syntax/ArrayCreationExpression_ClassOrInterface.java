// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ArrayCreationExpression_ClassOrInterface implements Serializable, Comparable<ArrayCreationExpression_ClassOrInterface> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DIM_EXPRS = new hydra.core.Name("dimExprs");
  
  public static final hydra.core.Name FIELD_NAME_DIMS = new hydra.core.Name("dims");
  
  public final hydra.ext.java.syntax.ClassOrInterfaceType type;
  
  public final java.util.List<hydra.ext.java.syntax.DimExpr> dimExprs;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Dims> dims;
  
  public ArrayCreationExpression_ClassOrInterface (hydra.ext.java.syntax.ClassOrInterfaceType type, java.util.List<hydra.ext.java.syntax.DimExpr> dimExprs, hydra.util.Maybe<hydra.ext.java.syntax.Dims> dims) {
    this.type = type;
    this.dimExprs = dimExprs;
    this.dims = dims;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayCreationExpression_ClassOrInterface)) {
      return false;
    }
    ArrayCreationExpression_ClassOrInterface o = (ArrayCreationExpression_ClassOrInterface) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.dimExprs,
      o.dimExprs) && java.util.Objects.equals(
      this.dims,
      o.dims);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(dimExprs) + 5 * java.util.Objects.hashCode(dims);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ArrayCreationExpression_ClassOrInterface other) {
    int cmp = 0;
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      dimExprs.hashCode(),
      other.dimExprs.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      dims.hashCode(),
      other.dims.hashCode());
  }
  
  public ArrayCreationExpression_ClassOrInterface withType(hydra.ext.java.syntax.ClassOrInterfaceType type) {
    return new ArrayCreationExpression_ClassOrInterface(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_ClassOrInterface withDimExprs(java.util.List<hydra.ext.java.syntax.DimExpr> dimExprs) {
    return new ArrayCreationExpression_ClassOrInterface(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_ClassOrInterface withDims(hydra.util.Maybe<hydra.ext.java.syntax.Dims> dims) {
    return new ArrayCreationExpression_ClassOrInterface(type, dimExprs, dims);
  }
}
