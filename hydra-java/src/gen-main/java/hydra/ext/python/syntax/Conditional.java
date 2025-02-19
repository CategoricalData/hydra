// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Conditional implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Conditional");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_IF = new hydra.core.Name("if");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  public final hydra.ext.python.syntax.Disjunction body;
  
  public final hydra.ext.python.syntax.Disjunction if_;
  
  public final hydra.ext.python.syntax.Expression else_;
  
  public Conditional (hydra.ext.python.syntax.Disjunction body, hydra.ext.python.syntax.Disjunction if_, hydra.ext.python.syntax.Expression else_) {
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((if_));
    java.util.Objects.requireNonNull((else_));
    this.body = body;
    this.if_ = if_;
    this.else_ = else_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Conditional)) {
      return false;
    }
    Conditional o = (Conditional) (other);
    return body.equals(o.body) && if_.equals(o.if_) && else_.equals(o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * if_.hashCode() + 5 * else_.hashCode();
  }
  
  public Conditional withBody(hydra.ext.python.syntax.Disjunction body) {
    java.util.Objects.requireNonNull((body));
    return new Conditional(body, if_, else_);
  }
  
  public Conditional withIf(hydra.ext.python.syntax.Disjunction if_) {
    java.util.Objects.requireNonNull((if_));
    return new Conditional(body, if_, else_);
  }
  
  public Conditional withElse(hydra.ext.python.syntax.Expression else_) {
    java.util.Objects.requireNonNull((else_));
    return new Conditional(body, if_, else_);
  }
}