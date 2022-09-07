package hydra.ext.java.syntax;

public class ArrayCreationExpression_ClassOrInterface {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ArrayCreationExpression.ClassOrInterface");
  
  public final hydra.ext.java.syntax.ClassOrInterfaceType type;
  
  public final java.util.List<hydra.ext.java.syntax.DimExpr> dimExprs;
  
  public final java.util.Optional<hydra.ext.java.syntax.Dims> dims;
  
  public ArrayCreationExpression_ClassOrInterface (hydra.ext.java.syntax.ClassOrInterfaceType type, java.util.List<hydra.ext.java.syntax.DimExpr> dimExprs, java.util.Optional<hydra.ext.java.syntax.Dims> dims) {
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
    return new ArrayCreationExpression_ClassOrInterface(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_ClassOrInterface withDimExprs(java.util.List<hydra.ext.java.syntax.DimExpr> dimExprs) {
    return new ArrayCreationExpression_ClassOrInterface(type, dimExprs, dims);
  }
  
  public ArrayCreationExpression_ClassOrInterface withDims(java.util.Optional<hydra.ext.java.syntax.Dims> dims) {
    return new ArrayCreationExpression_ClassOrInterface(type, dimExprs, dims);
  }
}