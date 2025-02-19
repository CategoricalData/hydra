// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ArrayCreationExpression_ClassOrInterface implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DIM_EXPRS = new hydra.core.Name("dimExprs");
  
  public static final hydra.core.Name FIELD_NAME_DIMS = new hydra.core.Name("dims");
  
  public final hydra.ext.java.syntax.ClassOrInterfaceType type;
  
  public final java.util.List<hydra.ext.java.syntax.DimExpr> dimExprs;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.Dims> dims;
  
  public ArrayCreationExpression_ClassOrInterface (hydra.ext.java.syntax.ClassOrInterfaceType type, java.util.List<hydra.ext.java.syntax.DimExpr> dimExprs, hydra.util.Opt<hydra.ext.java.syntax.Dims> dims) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((dimExprs));
    java.util.Objects.requireNonNull((dims));
    this.type = type;
    this.dimExprs = dimExprs;
    this.dims = dims;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayCreationExpression_ClassOrInterface)) {
      return false;
    }
    ArrayCreationExpression_ClassOrInterface o = (ArrayCreationExpression_ClassOrInterface) (other);
    return type.equals(o.type) && dimExprs.equals(o.dimExprs) && dims.equals(o.dims);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * dimExprs.hashCode() + 5 * dims.hashCode();
  }
  
  public ArrayCreationExpression_ClassOrInterface withType(hydra.ext.java.syntax.ClassOrInterfaceType type) {
    java.util.Objects.requireNonNull((type));
    return new ArrayCreationExpression_ClassOrInterface(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_ClassOrInterface withDimExprs(java.util.List<hydra.ext.java.syntax.DimExpr> dimExprs) {
    java.util.Objects.requireNonNull((dimExprs));
    return new ArrayCreationExpression_ClassOrInterface(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_ClassOrInterface withDims(hydra.util.Opt<hydra.ext.java.syntax.Dims> dims) {
    java.util.Objects.requireNonNull((dims));
    return new ArrayCreationExpression_ClassOrInterface(type, dimExprs, dims);
  }
}