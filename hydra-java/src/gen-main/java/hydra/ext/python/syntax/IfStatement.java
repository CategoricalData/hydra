// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class IfStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.IfStatement");
  
  public static final hydra.core.Name FIELD_NAME_CONDITION = new hydra.core.Name("condition");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_CONTINUATION = new hydra.core.Name("continuation");
  
  public final hydra.ext.python.syntax.NamedExpression condition;
  
  public final hydra.ext.python.syntax.Block body;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.IfTail> continuation;
  
  public IfStatement (hydra.ext.python.syntax.NamedExpression condition, hydra.ext.python.syntax.Block body, hydra.util.Opt<hydra.ext.python.syntax.IfTail> continuation) {
    java.util.Objects.requireNonNull((condition));
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((continuation));
    this.condition = condition;
    this.body = body;
    this.continuation = continuation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IfStatement)) {
      return false;
    }
    IfStatement o = (IfStatement) (other);
    return condition.equals(o.condition) && body.equals(o.body) && continuation.equals(o.continuation);
  }
  
  @Override
  public int hashCode() {
    return 2 * condition.hashCode() + 3 * body.hashCode() + 5 * continuation.hashCode();
  }
  
  public IfStatement withCondition(hydra.ext.python.syntax.NamedExpression condition) {
    java.util.Objects.requireNonNull((condition));
    return new IfStatement(condition, body, continuation);
  }
  
  public IfStatement withBody(hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new IfStatement(condition, body, continuation);
  }
  
  public IfStatement withContinuation(hydra.util.Opt<hydra.ext.python.syntax.IfTail> continuation) {
    java.util.Objects.requireNonNull((continuation));
    return new IfStatement(condition, body, continuation);
  }
}