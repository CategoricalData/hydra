// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class IfStatement implements Serializable, Comparable<IfStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.IfStatement");
  
  public static final hydra.core.Name FIELD_NAME_CONDITION = new hydra.core.Name("condition");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_CONTINUATION = new hydra.core.Name("continuation");
  
  public final hydra.ext.python.syntax.NamedExpression condition;
  
  public final hydra.ext.python.syntax.Block body;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.IfTail> continuation;
  
  public IfStatement (hydra.ext.python.syntax.NamedExpression condition, hydra.ext.python.syntax.Block body, hydra.util.Maybe<hydra.ext.python.syntax.IfTail> continuation) {
    this.condition = condition;
    this.body = body;
    this.continuation = continuation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IfStatement)) {
      return false;
    }
    IfStatement o = (IfStatement) other;
    return java.util.Objects.equals(
      this.condition,
      o.condition) && java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.continuation,
      o.continuation);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(condition) + 3 * java.util.Objects.hashCode(body) + 5 * java.util.Objects.hashCode(continuation);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IfStatement other) {
    int cmp = 0;
    cmp = ((Comparable) condition).compareTo(other.condition);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) body).compareTo(other.body);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      continuation.hashCode(),
      other.continuation.hashCode());
  }
  
  public IfStatement withCondition(hydra.ext.python.syntax.NamedExpression condition) {
    return new IfStatement(condition, body, continuation);
  }
  
  public IfStatement withBody(hydra.ext.python.syntax.Block body) {
    return new IfStatement(condition, body, continuation);
  }
  
  public IfStatement withContinuation(hydra.util.Maybe<hydra.ext.python.syntax.IfTail> continuation) {
    return new IfStatement(condition, body, continuation);
  }
}
