// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public class ContextuallyTypedRowValueExpressionList implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpressionList");
  
  public static final hydra.core.Name FIELD_NAME_FIRST = new hydra.core.Name("first");
  
  public static final hydra.core.Name FIELD_NAME_REST = new hydra.core.Name("rest");
  
  public final hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpression first;
  
  public final java.util.List<hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpression> rest;
  
  public ContextuallyTypedRowValueExpressionList (hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpression first, java.util.List<hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpression> rest) {
    java.util.Objects.requireNonNull((first));
    java.util.Objects.requireNonNull((rest));
    this.first = first;
    this.rest = rest;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ContextuallyTypedRowValueExpressionList)) {
      return false;
    }
    ContextuallyTypedRowValueExpressionList o = (ContextuallyTypedRowValueExpressionList) (other);
    return first.equals(o.first) && rest.equals(o.rest);
  }
  
  @Override
  public int hashCode() {
    return 2 * first.hashCode() + 3 * rest.hashCode();
  }
  
  public ContextuallyTypedRowValueExpressionList withFirst(hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpression first) {
    java.util.Objects.requireNonNull((first));
    return new ContextuallyTypedRowValueExpressionList(first, rest);
  }
  
  public ContextuallyTypedRowValueExpressionList withRest(java.util.List<hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpression> rest) {
    java.util.Objects.requireNonNull((rest));
    return new ContextuallyTypedRowValueExpressionList(first, rest);
  }
}