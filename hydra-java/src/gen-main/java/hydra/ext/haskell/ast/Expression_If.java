package hydra.ext.haskell.ast;

/**
 * An 'if' expression
 */
public class Expression_If {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Expression.If");
  
  public final hydra.ext.haskell.ast.Expression condition;
  
  public final hydra.ext.haskell.ast.Expression then;
  
  public final hydra.ext.haskell.ast.Expression else_;
  
  public Expression_If (hydra.ext.haskell.ast.Expression condition, hydra.ext.haskell.ast.Expression then, hydra.ext.haskell.ast.Expression else_) {
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
  
  public Expression_If withCondition(hydra.ext.haskell.ast.Expression condition) {
    return new Expression_If(condition, then, else_);
  }
  
  public Expression_If withThen(hydra.ext.haskell.ast.Expression then) {
    return new Expression_If(condition, then, else_);
  }
  
  public Expression_If withElse(hydra.ext.haskell.ast.Expression else_) {
    return new Expression_If(condition, then, else_);
  }
}