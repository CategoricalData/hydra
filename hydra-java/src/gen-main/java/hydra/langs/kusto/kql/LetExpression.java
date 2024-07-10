// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class LetExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.LetExpression");
  
  public final java.util.List<hydra.langs.kusto.kql.LetBinding> bindings;
  
  public final hydra.langs.kusto.kql.TabularExpression expression;
  
  public LetExpression (java.util.List<hydra.langs.kusto.kql.LetBinding> bindings, hydra.langs.kusto.kql.TabularExpression expression) {
    if (bindings == null) {
      throw new IllegalArgumentException("null value for 'bindings' argument");
    }
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
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
  
  public LetExpression withBindings(java.util.List<hydra.langs.kusto.kql.LetBinding> bindings) {
    if (bindings == null) {
      throw new IllegalArgumentException("null value for 'bindings' argument");
    }
    return new LetExpression(bindings, expression);
  }
  
  public LetExpression withExpression(hydra.langs.kusto.kql.TabularExpression expression) {
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    return new LetExpression(bindings, expression);
  }
}