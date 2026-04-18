// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * Pattern match on boolean values
 */
public class If implements Serializable, Comparable<If> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.If");

  public static final hydra.core.Name CONDITION = new hydra.core.Name("condition");

  public static final hydra.core.Name RETURN_AS = new hydra.core.Name("returnAs");

  public static final hydra.core.Name THEN = new hydra.core.Name("then");

  public static final hydra.core.Name ELSE = new hydra.core.Name("else");

  public final hydra.coq.syntax.Term condition;

  public final hydra.util.Maybe<hydra.coq.syntax.ReturnAs> returnAs;

  public final hydra.coq.syntax.Term then;

  public final hydra.coq.syntax.Term else_;

  public If (hydra.coq.syntax.Term condition, hydra.util.Maybe<hydra.coq.syntax.ReturnAs> returnAs, hydra.coq.syntax.Term then, hydra.coq.syntax.Term else_) {
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
    If o = (If) other;
    return java.util.Objects.equals(
      this.condition,
      o.condition) && java.util.Objects.equals(
      this.returnAs,
      o.returnAs) && java.util.Objects.equals(
      this.then,
      o.then) && java.util.Objects.equals(
      this.else_,
      o.else_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(condition) + 3 * java.util.Objects.hashCode(returnAs) + 5 * java.util.Objects.hashCode(then) + 7 * java.util.Objects.hashCode(else_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(If other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      condition,
      other.condition);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      returnAs,
      other.returnAs);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      then,
      other.then);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      else_,
      other.else_);
  }

  public If withCondition(hydra.coq.syntax.Term condition) {
    return new If(condition, returnAs, then, else_);
  }

  public If withReturnAs(hydra.util.Maybe<hydra.coq.syntax.ReturnAs> returnAs) {
    return new If(condition, returnAs, then, else_);
  }

  public If withThen(hydra.coq.syntax.Term then) {
    return new If(condition, returnAs, then, else_);
  }

  public If withElse(hydra.coq.syntax.Term else_) {
    return new If(condition, returnAs, then, else_);
  }
}
