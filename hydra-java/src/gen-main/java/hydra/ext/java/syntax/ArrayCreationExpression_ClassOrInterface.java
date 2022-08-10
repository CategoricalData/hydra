package hydra.ext.java.syntax;

public class ArrayCreationExpression_ClassOrInterface {
  public final ClassOrInterfaceType type;
  
  public final java.util.List<DimExpr> dimExprs;
  
  public final java.util.Optional<Dims> dims;
  
  public ArrayCreationExpression_ClassOrInterface (ClassOrInterfaceType type, java.util.List<DimExpr> dimExprs, java.util.Optional<Dims> dims) {
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
  
  public ArrayCreationExpression_ClassOrInterface withType(ClassOrInterfaceType type) {
    return new ArrayCreationExpression_ClassOrInterface(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_ClassOrInterface withDimExprs(java.util.List<DimExpr> dimExprs) {
    return new ArrayCreationExpression_ClassOrInterface(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_ClassOrInterface withDims(java.util.Optional<Dims> dims) {
    return new ArrayCreationExpression_ClassOrInterface(type, dimExprs, dims);
  }
}