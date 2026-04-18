// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public abstract class NamedExpression implements Serializable, Comparable<NamedExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.NamedExpression");

  public static final hydra.core.Name ASSIGNMENT = new hydra.core.Name("assignment");

  public static final hydra.core.Name SIMPLE = new hydra.core.Name("simple");

  private NamedExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Assignment instance) ;

    R visit(Simple instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NamedExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Assignment instance) {
      return otherwise(instance);
    }

    default R visit(Simple instance) {
      return otherwise(instance);
    }
  }

  public static final class Assignment extends hydra.python.syntax.NamedExpression implements Serializable {
    public final hydra.python.syntax.AssignmentExpression value;

    public Assignment (hydra.python.syntax.AssignmentExpression value) {
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
    public int compareTo(NamedExpression other) {
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

  public static final class Simple extends hydra.python.syntax.NamedExpression implements Serializable {
    public final hydra.python.syntax.Expression value;

    public Simple (hydra.python.syntax.Expression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) other;
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
    public int compareTo(NamedExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Simple o = (Simple) other;
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
