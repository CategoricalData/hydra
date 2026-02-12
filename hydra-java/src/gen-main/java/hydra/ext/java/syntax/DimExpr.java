// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class DimExpr implements Serializable, Comparable<DimExpr> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.DimExpr");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Expression> expression;
  
  public DimExpr (java.util.List<hydra.ext.java.syntax.Annotation> annotations, hydra.util.Maybe<hydra.ext.java.syntax.Expression> expression) {
    this.annotations = annotations;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DimExpr)) {
      return false;
    }
    DimExpr o = (DimExpr) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(expression);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DimExpr other) {
    int cmp = 0;
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      expression.hashCode(),
      other.expression.hashCode());
  }
  
  public DimExpr withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    return new DimExpr(annotations, expression);
  }
  
  public DimExpr withExpression(hydra.util.Maybe<hydra.ext.java.syntax.Expression> expression) {
    return new DimExpr(annotations, expression);
  }
}
