package hydra.ext.coq.syntax;

/**
 * Pattern match on boolean values
 */
public class If {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.If");
  
  public final hydra.ext.coq.syntax.Term condition;
  
  public final java.util.Optional<hydra.ext.coq.syntax.ReturnAs> returnAs;
  
  public final hydra.ext.coq.syntax.Term then;
  
  public final hydra.ext.coq.syntax.Term else_;
  
  public If (hydra.ext.coq.syntax.Term condition, java.util.Optional<hydra.ext.coq.syntax.ReturnAs> returnAs, hydra.ext.coq.syntax.Term then, hydra.ext.coq.syntax.Term else_) {
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
  
  public If withCondition(hydra.ext.coq.syntax.Term condition) {
    return new If(condition, returnAs, then, else_);
  }
  
  public If withReturnAs(java.util.Optional<hydra.ext.coq.syntax.ReturnAs> returnAs) {
    return new If(condition, returnAs, then, else_);
  }
  
  public If withThen(hydra.ext.coq.syntax.Term then) {
    return new If(condition, returnAs, then, else_);
  }
  
  public If withElse(hydra.ext.coq.syntax.Term else_) {
    return new If(condition, returnAs, then, else_);
  }
}