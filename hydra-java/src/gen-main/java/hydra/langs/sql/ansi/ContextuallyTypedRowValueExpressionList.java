// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class ContextuallyTypedRowValueExpressionList implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/sql/ansi.ContextuallyTypedRowValueExpressionList");
  
  public static final hydra.core.Name FIELD_NAME_FIRST = new hydra.core.Name("first");
  
  public static final hydra.core.Name FIELD_NAME_REST = new hydra.core.Name("rest");
  
  public final hydra.langs.sql.ansi.ContextuallyTypedRowValueExpression first;
  
  public final java.util.List<hydra.langs.sql.ansi.ContextuallyTypedRowValueExpression> rest;
  
  public ContextuallyTypedRowValueExpressionList (hydra.langs.sql.ansi.ContextuallyTypedRowValueExpression first, java.util.List<hydra.langs.sql.ansi.ContextuallyTypedRowValueExpression> rest) {
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
  
  public ContextuallyTypedRowValueExpressionList withFirst(hydra.langs.sql.ansi.ContextuallyTypedRowValueExpression first) {
    java.util.Objects.requireNonNull((first));
    return new ContextuallyTypedRowValueExpressionList(first, rest);
  }
  
  public ContextuallyTypedRowValueExpressionList withRest(java.util.List<hydra.langs.sql.ansi.ContextuallyTypedRowValueExpression> rest) {
    java.util.Objects.requireNonNull((rest));
    return new ContextuallyTypedRowValueExpressionList(first, rest);
  }
}