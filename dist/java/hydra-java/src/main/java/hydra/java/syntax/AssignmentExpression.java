// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class AssignmentExpression implements Serializable, Comparable<AssignmentExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.AssignmentExpression");

  public static final hydra.core.Name CONDITIONAL = new hydra.core.Name("conditional");

  public static final hydra.core.Name ASSIGNMENT = new hydra.core.Name("assignment");

  private AssignmentExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Conditional instance) ;

    R visit(Assignment instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AssignmentExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Conditional instance) {
      return otherwise(instance);
    }

    default R visit(Assignment instance) {
      return otherwise(instance);
    }
  }

  public static final class Conditional extends hydra.java.syntax.AssignmentExpression implements Serializable {
    public final hydra.java.syntax.ConditionalExpression value;

    public Conditional (hydra.java.syntax.ConditionalExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Conditional)) {
        return false;
      }
      Conditional o = (Conditional) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Conditional o = (Conditional) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Assignment extends hydra.java.syntax.AssignmentExpression implements Serializable {
    public final hydra.java.syntax.Assignment value;

    public Assignment (hydra.java.syntax.Assignment value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Assignment)) {
        return false;
      }
      Assignment o = (Assignment) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Assignment o = (Assignment) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
