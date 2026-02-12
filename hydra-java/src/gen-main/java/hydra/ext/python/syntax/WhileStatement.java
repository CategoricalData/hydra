// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class WhileStatement implements Serializable, Comparable<WhileStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.WhileStatement");
  
  public static final hydra.core.Name FIELD_NAME_CONDITION = new hydra.core.Name("condition");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  public final hydra.ext.python.syntax.NamedExpression condition;
  
  public final hydra.ext.python.syntax.Block body;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Block> else_;
  
  public WhileStatement (hydra.ext.python.syntax.NamedExpression condition, hydra.ext.python.syntax.Block body, hydra.util.Maybe<hydra.ext.python.syntax.Block> else_) {
    this.condition = condition;
    this.body = body;
    this.else_ = else_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WhileStatement)) {
      return false;
    }
    WhileStatement o = (WhileStatement) other;
    return java.util.Objects.equals(
      this.condition,
      o.condition) && java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.else_,
      o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(condition) + 3 * java.util.Objects.hashCode(body) + 5 * java.util.Objects.hashCode(else_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(WhileStatement other) {
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
      else_.hashCode(),
      other.else_.hashCode());
  }
  
  public WhileStatement withCondition(hydra.ext.python.syntax.NamedExpression condition) {
    return new WhileStatement(condition, body, else_);
  }
  
  public WhileStatement withBody(hydra.ext.python.syntax.Block body) {
    return new WhileStatement(condition, body, else_);
  }
  
  public WhileStatement withElse(hydra.util.Maybe<hydra.ext.python.syntax.Block> else_) {
    return new WhileStatement(condition, body, else_);
  }
}
