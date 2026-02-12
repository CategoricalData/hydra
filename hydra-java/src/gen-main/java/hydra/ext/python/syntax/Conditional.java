// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Conditional implements Serializable, Comparable<Conditional> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Conditional");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_IF = new hydra.core.Name("if");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  public final hydra.ext.python.syntax.Disjunction body;
  
  public final hydra.ext.python.syntax.Disjunction if_;
  
  public final hydra.ext.python.syntax.Expression else_;
  
  public Conditional (hydra.ext.python.syntax.Disjunction body, hydra.ext.python.syntax.Disjunction if_, hydra.ext.python.syntax.Expression else_) {
    this.body = body;
    this.if_ = if_;
    this.else_ = else_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Conditional)) {
      return false;
    }
    Conditional o = (Conditional) other;
    return java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.if_,
      o.if_) && java.util.Objects.equals(
      this.else_,
      o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body) + 3 * java.util.Objects.hashCode(if_) + 5 * java.util.Objects.hashCode(else_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Conditional other) {
    int cmp = 0;
    cmp = ((Comparable) body).compareTo(other.body);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) if_).compareTo(other.if_);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) else_).compareTo(other.else_);
  }
  
  public Conditional withBody(hydra.ext.python.syntax.Disjunction body) {
    return new Conditional(body, if_, else_);
  }
  
  public Conditional withIf(hydra.ext.python.syntax.Disjunction if_) {
    return new Conditional(body, if_, else_);
  }
  
  public Conditional withElse(hydra.ext.python.syntax.Expression else_) {
    return new Conditional(body, if_, else_);
  }
}
