package hydra.ext.java.syntax;

public class DimExpr {
  public final java.util.List<Annotation> annotations;
  
  public final java.util.Optional<Expression> expression;
  
  public DimExpr (java.util.List<Annotation> annotations, java.util.Optional<Expression> expression) {
    this.annotations = annotations;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DimExpr)) {
      return false;
    }
    DimExpr o = (DimExpr) (other);
    return annotations.equals(o.annotations) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * expression.hashCode();
  }
  
  public DimExpr withAnnotations(java.util.List<Annotation> annotations) {
    return new DimExpr(annotations, expression);
  }
  
  public DimExpr withExpression(java.util.Optional<Expression> expression) {
    return new DimExpr(annotations, expression);
  }
}