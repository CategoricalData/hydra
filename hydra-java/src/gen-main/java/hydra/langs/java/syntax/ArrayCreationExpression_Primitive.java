// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ArrayCreationExpression_Primitive implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.ArrayCreationExpression.Primitive");
  
  public final hydra.langs.java.syntax.PrimitiveTypeWithAnnotations type;
  
  public final java.util.List<hydra.langs.java.syntax.DimExpr> dimExprs;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.Dims> dims;
  
  public ArrayCreationExpression_Primitive (hydra.langs.java.syntax.PrimitiveTypeWithAnnotations type, java.util.List<hydra.langs.java.syntax.DimExpr> dimExprs, hydra.util.Opt<hydra.langs.java.syntax.Dims> dims) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((dimExprs));
    java.util.Objects.requireNonNull((dims));
    this.type = type;
    this.dimExprs = dimExprs;
    this.dims = dims;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayCreationExpression_Primitive)) {
      return false;
    }
    ArrayCreationExpression_Primitive o = (ArrayCreationExpression_Primitive) (other);
    return type.equals(o.type) && dimExprs.equals(o.dimExprs) && dims.equals(o.dims);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * dimExprs.hashCode() + 5 * dims.hashCode();
  }
  
  public ArrayCreationExpression_Primitive withType(hydra.langs.java.syntax.PrimitiveTypeWithAnnotations type) {
    java.util.Objects.requireNonNull((type));
    return new ArrayCreationExpression_Primitive(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_Primitive withDimExprs(java.util.List<hydra.langs.java.syntax.DimExpr> dimExprs) {
    java.util.Objects.requireNonNull((dimExprs));
    return new ArrayCreationExpression_Primitive(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_Primitive withDims(hydra.util.Opt<hydra.langs.java.syntax.Dims> dims) {
    java.util.Objects.requireNonNull((dims));
    return new ArrayCreationExpression_Primitive(type, dimExprs, dims);
  }
}