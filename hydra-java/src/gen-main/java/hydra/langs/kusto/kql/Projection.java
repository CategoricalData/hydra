// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class Projection implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.Projection");
  
  public final hydra.langs.kusto.kql.Expression expression;
  
  public final hydra.util.Opt<hydra.langs.kusto.kql.ColumnName> alias;
  
  public Projection (hydra.langs.kusto.kql.Expression expression, hydra.util.Opt<hydra.langs.kusto.kql.ColumnName> alias) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((alias));
    this.expression = expression;
    this.alias = alias;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Projection)) {
      return false;
    }
    Projection o = (Projection) (other);
    return expression.equals(o.expression) && alias.equals(o.alias);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * alias.hashCode();
  }
  
  public Projection withExpression(hydra.langs.kusto.kql.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new Projection(expression, alias);
  }
  
  public Projection withAlias(hydra.util.Opt<hydra.langs.kusto.kql.ColumnName> alias) {
    java.util.Objects.requireNonNull((alias));
    return new Projection(expression, alias);
  }
}