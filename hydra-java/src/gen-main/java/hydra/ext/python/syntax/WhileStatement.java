// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class WhileStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.WhileStatement");
  
  public static final hydra.core.Name FIELD_NAME_CONDITION = new hydra.core.Name("condition");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  public final hydra.ext.python.syntax.NamedExpression condition;
  
  public final hydra.ext.python.syntax.Block body;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Block> else_;
  
  public WhileStatement (hydra.ext.python.syntax.NamedExpression condition, hydra.ext.python.syntax.Block body, hydra.util.Opt<hydra.ext.python.syntax.Block> else_) {
    java.util.Objects.requireNonNull((condition));
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((else_));
    this.condition = condition;
    this.body = body;
    this.else_ = else_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WhileStatement)) {
      return false;
    }
    WhileStatement o = (WhileStatement) (other);
    return condition.equals(o.condition) && body.equals(o.body) && else_.equals(o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * condition.hashCode() + 3 * body.hashCode() + 5 * else_.hashCode();
  }
  
  public WhileStatement withCondition(hydra.ext.python.syntax.NamedExpression condition) {
    java.util.Objects.requireNonNull((condition));
    return new WhileStatement(condition, body, else_);
  }
  
  public WhileStatement withBody(hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new WhileStatement(condition, body, else_);
  }
  
  public WhileStatement withElse(hydra.util.Opt<hydra.ext.python.syntax.Block> else_) {
    java.util.Objects.requireNonNull((else_));
    return new WhileStatement(condition, body, else_);
  }
}