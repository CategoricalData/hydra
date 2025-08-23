// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.kusto.kql;

import java.io.Serializable;

public class LetExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.kusto.kql.LetExpression");
  
  public static final hydra.core.Name FIELD_NAME_BINDINGS = new hydra.core.Name("bindings");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final java.util.List<hydra.ext.com.microsoft.kusto.kql.LetBinding> bindings;
  
  public final hydra.ext.com.microsoft.kusto.kql.TabularExpression expression;
  
  public LetExpression (java.util.List<hydra.ext.com.microsoft.kusto.kql.LetBinding> bindings, hydra.ext.com.microsoft.kusto.kql.TabularExpression expression) {
    java.util.Objects.requireNonNull((bindings));
    java.util.Objects.requireNonNull((expression));
    this.bindings = bindings;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetExpression)) {
      return false;
    }
    LetExpression o = (LetExpression) (other);
    return bindings.equals(o.bindings) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * bindings.hashCode() + 3 * expression.hashCode();
  }
  
  public LetExpression withBindings(java.util.List<hydra.ext.com.microsoft.kusto.kql.LetBinding> bindings) {
    java.util.Objects.requireNonNull((bindings));
    return new LetExpression(bindings, expression);
  }
  
  public LetExpression withExpression(hydra.ext.com.microsoft.kusto.kql.TabularExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new LetExpression(bindings, expression);
  }
}
