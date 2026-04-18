// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public abstract class YieldExpression implements Serializable, Comparable<YieldExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.YieldExpression");

  public static final hydra.core.Name FROM = new hydra.core.Name("from");

  public static final hydra.core.Name SIMPLE = new hydra.core.Name("simple");

  private YieldExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(From instance) ;

    R visit(Simple instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(YieldExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(From instance) {
      return otherwise(instance);
    }

    default R visit(Simple instance) {
      return otherwise(instance);
    }
  }

  public static final class From extends hydra.python.syntax.YieldExpression implements Serializable {
    public final hydra.python.syntax.Expression value;

    public From (hydra.python.syntax.Expression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof From)) {
        return false;
      }
      From o = (From) other;
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
    public int compareTo(YieldExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      From o = (From) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Simple extends hydra.python.syntax.YieldExpression implements Serializable {
    public final java.util.List<hydra.python.syntax.StarExpression> value;

    public Simple (java.util.List<hydra.python.syntax.StarExpression> value) {
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
    public int compareTo(YieldExpression other) {
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
