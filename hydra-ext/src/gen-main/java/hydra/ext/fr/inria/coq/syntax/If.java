// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

/**
 * Pattern match on boolean values
 */
public class If implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.If");
  
  public static final hydra.core.Name FIELD_NAME_CONDITION = new hydra.core.Name("condition");
  
  public static final hydra.core.Name FIELD_NAME_RETURN_AS = new hydra.core.Name("returnAs");
  
  public static final hydra.core.Name FIELD_NAME_THEN = new hydra.core.Name("then");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  public final hydra.ext.fr.inria.coq.syntax.Term condition;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.ReturnAs> returnAs;
  
  public final hydra.ext.fr.inria.coq.syntax.Term then;
  
  public final hydra.ext.fr.inria.coq.syntax.Term else_;
  
  public If (hydra.ext.fr.inria.coq.syntax.Term condition, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.ReturnAs> returnAs, hydra.ext.fr.inria.coq.syntax.Term then, hydra.ext.fr.inria.coq.syntax.Term else_) {
    java.util.Objects.requireNonNull((condition));
    java.util.Objects.requireNonNull((returnAs));
    java.util.Objects.requireNonNull((then));
    java.util.Objects.requireNonNull((else_));
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
  
  public If withCondition(hydra.ext.fr.inria.coq.syntax.Term condition) {
    java.util.Objects.requireNonNull((condition));
    return new If(condition, returnAs, then, else_);
  }
  
  public If withReturnAs(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.ReturnAs> returnAs) {
    java.util.Objects.requireNonNull((returnAs));
    return new If(condition, returnAs, then, else_);
  }
  
  public If withThen(hydra.ext.fr.inria.coq.syntax.Term then) {
    java.util.Objects.requireNonNull((then));
    return new If(condition, returnAs, then, else_);
  }
  
  public If withElse(hydra.ext.fr.inria.coq.syntax.Term else_) {
    java.util.Objects.requireNonNull((else_));
    return new If(condition, returnAs, then, else_);
  }
}