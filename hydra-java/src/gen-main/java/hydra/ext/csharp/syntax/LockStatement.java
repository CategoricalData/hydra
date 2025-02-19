// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class LockStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.LockStatement");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.csharp.syntax.Expression expression;
  
  public final hydra.ext.csharp.syntax.EmbeddedStatement body;
  
  public LockStatement (hydra.ext.csharp.syntax.Expression expression, hydra.ext.csharp.syntax.EmbeddedStatement body) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((body));
    this.expression = expression;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LockStatement)) {
      return false;
    }
    LockStatement o = (LockStatement) (other);
    return expression.equals(o.expression) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * body.hashCode();
  }
  
  public LockStatement withExpression(hydra.ext.csharp.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new LockStatement(expression, body);
  }
  
  public LockStatement withBody(hydra.ext.csharp.syntax.EmbeddedStatement body) {
    java.util.Objects.requireNonNull((body));
    return new LockStatement(expression, body);
  }
}