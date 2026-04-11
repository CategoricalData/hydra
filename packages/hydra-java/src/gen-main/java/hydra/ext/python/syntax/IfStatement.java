// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class IfStatement implements Serializable, Comparable<IfStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.syntax.IfStatement");

  public static final hydra.core.Name CONDITION = new hydra.core.Name("condition");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name CONTINUATION = new hydra.core.Name("continuation");

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
    cmp = hydra.util.Comparing.compare(
      condition,
      other.condition);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      body,
      other.body);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      continuation,
      other.continuation);
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
