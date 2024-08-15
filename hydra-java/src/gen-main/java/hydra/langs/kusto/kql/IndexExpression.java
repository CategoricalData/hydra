// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class IndexExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/kusto/kql.IndexExpression");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_INDEX = new hydra.core.Name("index");
  
  public final hydra.langs.kusto.kql.Expression expression;
  
  public final String index;
  
  public IndexExpression (hydra.langs.kusto.kql.Expression expression, String index) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((index));
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
    java.util.Objects.requireNonNull((expression));
    return new IndexExpression(expression, index);
  }
  
  public IndexExpression withIndex(String index) {
    java.util.Objects.requireNonNull((index));
    return new IndexExpression(expression, index);
  }
}