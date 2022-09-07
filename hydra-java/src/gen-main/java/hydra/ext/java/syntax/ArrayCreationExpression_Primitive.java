package hydra.ext.java.syntax;

public class ArrayCreationExpression_Primitive {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ArrayCreationExpression.Primitive");
  
  public final hydra.ext.java.syntax.PrimitiveTypeWithAnnotations type;
  
  public final java.util.List<hydra.ext.java.syntax.DimExpr> dimExprs;
  
  public final java.util.Optional<hydra.ext.java.syntax.Dims> dims;
  
  public ArrayCreationExpression_Primitive (hydra.ext.java.syntax.PrimitiveTypeWithAnnotations type, java.util.List<hydra.ext.java.syntax.DimExpr> dimExprs, java.util.Optional<hydra.ext.java.syntax.Dims> dims) {
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
  
  public ArrayCreationExpression_Primitive withType(hydra.ext.java.syntax.PrimitiveTypeWithAnnotations type) {
    return new ArrayCreationExpression_Primitive(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_Primitive withDimExprs(java.util.List<hydra.ext.java.syntax.DimExpr> dimExprs) {
    return new ArrayCreationExpression_Primitive(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_Primitive withDims(java.util.Optional<hydra.ext.java.syntax.Dims> dims) {
    return new ArrayCreationExpression_Primitive(type, dimExprs, dims);
  }
}