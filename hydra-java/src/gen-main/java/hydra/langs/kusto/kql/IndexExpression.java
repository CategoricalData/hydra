// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class IndexExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.IndexExpression");
  
  public final hydra.langs.kusto.kql.Expression expression;
  
  public final String index;
  
  public IndexExpression (hydra.langs.kusto.kql.Expression expression, String index) {
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    if (index == null) {
      throw new IllegalArgumentException("null value for 'index' argument");
    }
    this.expression = expression;
    this.index = index;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IndexExpression)) {
      return false;
    }
    IndexExpression o = (IndexExpression) (other);
    return expression.equals(o.expression) && index.equals(o.index);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * index.hashCode();
  }
  
  public IndexExpression withExpression(hydra.langs.kusto.kql.Expression expression) {
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    return new IndexExpression(expression, index);
  }
  
  public IndexExpression withIndex(String index) {
    if (index == null) {
      throw new IllegalArgumentException("null value for 'index' argument");
    }
    return new IndexExpression(expression, index);
  }
}