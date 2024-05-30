package hydra.langs.kusto.kql;

import java.io.Serializable;

public class LetBinding implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.LetBinding");
  
  public final hydra.langs.kusto.kql.ColumnName name;
  
  public final hydra.langs.kusto.kql.Expression expression;
  
  public LetBinding (hydra.langs.kusto.kql.ColumnName name, hydra.langs.kusto.kql.Expression expression) {
    this.name = name;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetBinding)) {
      return false;
    }
    LetBinding o = (LetBinding) (other);
    return name.equals(o.name) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * expression.hashCode();
  }
  
  public LetBinding withName(hydra.langs.kusto.kql.ColumnName name) {
    return new LetBinding(name, expression);
  }
  
  public LetBinding withExpression(hydra.langs.kusto.kql.Expression expression) {
    return new LetBinding(name, expression);
  }
}