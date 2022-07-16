package hydra.ext.haskell.ast;

/**
 * An 'if' expression
 */
public class Expression_If {
  public final Expression condition;
  
  public final Expression then;
  
  public final Expression else_;
  
  public Expression_If (Expression condition, Expression then, Expression else_) {
    this.condition = condition;
    this.then = then;
    this.else_ = else_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Expression_If)) {
      return false;
    }
    Expression_If o = (Expression_If) (other);
    return condition.equals(o.condition) && then.equals(o.then) && else_.equals(o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * condition.hashCode() + 3 * then.hashCode() + 5 * else_.hashCode();
  }
  
  public Expression_If withCondition(Expression condition) {
    return new Expression_If(condition, then, else_);
  }
  
  public Expression_If withThen(Expression then) {
    return new Expression_If(condition, then, else_);
  }
  
  public Expression_If withElse(Expression else_) {
    return new Expression_If(condition, then, else_);
  }
}