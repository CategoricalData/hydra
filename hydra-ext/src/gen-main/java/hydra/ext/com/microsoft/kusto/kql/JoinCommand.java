// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.kusto.kql;

import java.io.Serializable;

public class JoinCommand implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/kusto/kql.JoinCommand");
  
  public static final hydra.core.Name FIELD_NAME_KIND = new hydra.core.Name("kind");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_ON = new hydra.core.Name("on");
  
  public final hydra.ext.com.microsoft.kusto.kql.JoinKind kind;
  
  public final hydra.ext.com.microsoft.kusto.kql.TableName expression;
  
  public final hydra.ext.com.microsoft.kusto.kql.Expression on;
  
  public JoinCommand (hydra.ext.com.microsoft.kusto.kql.JoinKind kind, hydra.ext.com.microsoft.kusto.kql.TableName expression, hydra.ext.com.microsoft.kusto.kql.Expression on) {
    java.util.Objects.requireNonNull((kind));
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((on));
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
  
  public JoinCommand withKind(hydra.ext.com.microsoft.kusto.kql.JoinKind kind) {
    java.util.Objects.requireNonNull((kind));
    return new JoinCommand(kind, expression, on);
  }
  
  public JoinCommand withExpression(hydra.ext.com.microsoft.kusto.kql.TableName expression) {
    java.util.Objects.requireNonNull((expression));
    return new JoinCommand(kind, expression, on);
  }
  
  public JoinCommand withOn(hydra.ext.com.microsoft.kusto.kql.Expression on) {
    java.util.Objects.requireNonNull((on));
    return new JoinCommand(kind, expression, on);
  }
}