// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class DimExpr implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.DimExpr");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final java.util.List<hydra.langs.java.syntax.Annotation> annotations;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.Expression> expression;
  
  public DimExpr (java.util.List<hydra.langs.java.syntax.Annotation> annotations, hydra.util.Opt<hydra.langs.java.syntax.Expression> expression) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((expression));
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
  
  public DimExpr withAnnotations(java.util.List<hydra.langs.java.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new DimExpr(annotations, expression);
  }
  
  public DimExpr withExpression(hydra.util.Opt<hydra.langs.java.syntax.Expression> expression) {
    java.util.Objects.requireNonNull((expression));
    return new DimExpr(annotations, expression);
  }
}