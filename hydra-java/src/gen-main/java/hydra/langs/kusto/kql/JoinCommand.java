// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class JoinCommand implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.JoinCommand");
  
  public final hydra.langs.kusto.kql.JoinKind kind;
  
  public final hydra.langs.kusto.kql.TableName expression;
  
  public final hydra.langs.kusto.kql.Expression on;
  
  public JoinCommand (hydra.langs.kusto.kql.JoinKind kind, hydra.langs.kusto.kql.TableName expression, hydra.langs.kusto.kql.Expression on) {
    if (kind == null) {
      throw new IllegalArgumentException("null value for 'kind' argument");
    }
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    if (on == null) {
      throw new IllegalArgumentException("null value for 'on' argument");
    }
    this.kind = kind;
    this.expression = expression;
    this.on = on;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JoinCommand)) {
      return false;
    }
    JoinCommand o = (JoinCommand) (other);
    return kind.equals(o.kind) && expression.equals(o.expression) && on.equals(o.on);
  }
  
  @Override
  public int hashCode() {
    return 2 * kind.hashCode() + 3 * expression.hashCode() + 5 * on.hashCode();
  }
  
  public JoinCommand withKind(hydra.langs.kusto.kql.JoinKind kind) {
    if (kind == null) {
      throw new IllegalArgumentException("null value for 'kind' argument");
    }
    return new JoinCommand(kind, expression, on);
  }
  
  public JoinCommand withExpression(hydra.langs.kusto.kql.TableName expression) {
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    return new JoinCommand(kind, expression, on);
  }
  
  public JoinCommand withOn(hydra.langs.kusto.kql.Expression on) {
    if (on == null) {
      throw new IllegalArgumentException("null value for 'on' argument");
    }
    return new JoinCommand(kind, expression, on);
  }
}