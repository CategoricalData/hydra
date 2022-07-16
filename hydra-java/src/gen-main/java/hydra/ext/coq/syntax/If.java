package hydra.ext.coq.syntax;

/**
 * Pattern match on boolean values
 */
public class If {
  public final Term condition;
  
  public final java.util.Optional<ReturnAs> returnAs;
  
  public final Term then;
  
  public final Term else_;
  
  public If (Term condition, java.util.Optional<ReturnAs> returnAs, Term then, Term else_) {
    this.condition = condition;
    this.returnAs = returnAs;
    this.then = then;
    this.else_ = else_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof If)) {
      return false;
    }
    If o = (If) (other);
    return condition.equals(o.condition) && returnAs.equals(o.returnAs) && then.equals(o.then) && else_.equals(o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * condition.hashCode() + 3 * returnAs.hashCode() + 5 * then.hashCode() + 7 * else_.hashCode();
  }
  
  public If withCondition(Term condition) {
    return new If(condition, returnAs, then, else_);
  }
  
  public If withReturnAs(java.util.Optional<ReturnAs> returnAs) {
    return new If(condition, returnAs, then, else_);
  }
  
  public If withThen(Term then) {
    return new If(condition, returnAs, then, else_);
  }
  
  public If withElse(Term else_) {
    return new If(condition, returnAs, then, else_);
  }
}