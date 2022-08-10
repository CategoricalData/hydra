package hydra.ext.java.syntax;

public class ArrayCreationExpression_Primitive {
  public final PrimitiveTypeWithAnnotations type;
  
  public final java.util.List<DimExpr> dimExprs;
  
  public final java.util.Optional<Dims> dims;
  
  public ArrayCreationExpression_Primitive (PrimitiveTypeWithAnnotations type, java.util.List<DimExpr> dimExprs, java.util.Optional<Dims> dims) {
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
  
  public ArrayCreationExpression_Primitive withType(PrimitiveTypeWithAnnotations type) {
    return new ArrayCreationExpression_Primitive(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_Primitive withDimExprs(java.util.List<DimExpr> dimExprs) {
    return new ArrayCreationExpression_Primitive(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_Primitive withDims(java.util.Optional<Dims> dims) {
    return new ArrayCreationExpression_Primitive(type, dimExprs, dims);
  }
}