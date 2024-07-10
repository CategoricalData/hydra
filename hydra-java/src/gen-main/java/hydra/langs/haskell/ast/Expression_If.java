// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

/**
 * An 'if' expression
 */
public class Expression_If implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/haskell/ast.Expression.If");
  
  public final hydra.langs.haskell.ast.Expression condition;
  
  public final hydra.langs.haskell.ast.Expression then;
  
  public final hydra.langs.haskell.ast.Expression else_;
  
  public Expression_If (hydra.langs.haskell.ast.Expression condition, hydra.langs.haskell.ast.Expression then, hydra.langs.haskell.ast.Expression else_) {
    if (condition == null) {
      throw new IllegalArgumentException("null value for 'condition' argument");
    }
    if (then == null) {
      throw new IllegalArgumentException("null value for 'then' argument");
    }
    if (else_ == null) {
      throw new IllegalArgumentException("null value for 'else' argument");
    }
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
  
  public Expression_If withCondition(hydra.langs.haskell.ast.Expression condition) {
    if (condition == null) {
      throw new IllegalArgumentException("null value for 'condition' argument");
    }
    return new Expression_If(condition, then, else_);
  }
  
  public Expression_If withThen(hydra.langs.haskell.ast.Expression then) {
    if (then == null) {
      throw new IllegalArgumentException("null value for 'then' argument");
    }
    return new Expression_If(condition, then, else_);
  }
  
  public Expression_If withElse(hydra.langs.haskell.ast.Expression else_) {
    if (else_ == null) {
      throw new IllegalArgumentException("null value for 'else' argument");
    }
    return new Expression_If(condition, then, else_);
  }
}