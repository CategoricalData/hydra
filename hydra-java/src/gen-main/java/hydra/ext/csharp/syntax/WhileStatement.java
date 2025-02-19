// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class WhileStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.WhileStatement");
  
  public static final hydra.core.Name FIELD_NAME_CONDITION = new hydra.core.Name("condition");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.csharp.syntax.BooleanExpression condition;
  
  public final hydra.ext.csharp.syntax.EmbeddedStatement body;
  
  public WhileStatement (hydra.ext.csharp.syntax.BooleanExpression condition, hydra.ext.csharp.syntax.EmbeddedStatement body) {
    java.util.Objects.requireNonNull((condition));
    java.util.Objects.requireNonNull((body));
    this.condition = condition;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WhileStatement)) {
      return false;
    }
    WhileStatement o = (WhileStatement) (other);
    return condition.equals(o.condition) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * condition.hashCode() + 3 * body.hashCode();
  }
  
  public WhileStatement withCondition(hydra.ext.csharp.syntax.BooleanExpression condition) {
    java.util.Objects.requireNonNull((condition));
    return new WhileStatement(condition, body);
  }
  
  public WhileStatement withBody(hydra.ext.csharp.syntax.EmbeddedStatement body) {
    java.util.Objects.requireNonNull((body));
    return new WhileStatement(condition, body);
  }
}